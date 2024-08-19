\version "2.25.18"

#(use-modules (srfi srfi-2)
              (srfi srfi-26)
              (oop goops))

#(define-class <delay-dispatcher> ()
   (in-dispatcher #:init-thunk ly:make-dispatcher #:getter in)
   (out-dispatcher #:init-thunk ly:make-dispatcher #:getter out)
   (event-queue #:init-value '() #:accessor queue))

#(define-method (clear! (d <delay-dispatcher>))
   (set! (queue d) '()))

#(define-method (dump (d <delay-dispatcher>))
   "Broadcast all events in the queue to out-dispatcher in the order that
the events were received."
   (for-each (lambda (ev) (ly:broadcast (out d) ev))
             (reverse (queue d))))

#(define-method (initialize (d <delay-dispatcher>) initargs)
   "Events received by in-dispatcher are added to event-queue."
   (next-method)
   (ly:add-listener (lambda (ev)
                      (set! (queue d)
                            (cons ev (queue d))))
                    (in d)
                    'StreamEvent))

#(define (voice-id-or-staff c)
   "Map context ids to alist keys for collaParteDispatchers. Staff contexts map
to \"Staff\", default Bottom contexts with empty id map to \"default\"."
   (cond
    ((eq? c (ly:context-find c 'Staff)) "Staff")
    ((equal? "" (ly:context-id c)) "default")
    (else (ly:context-id c))))

#(define (find-colla-parte-relay ctx)
   "Search upward for parent of ctx with collaParte property set. If none is found,
return the Score context."
   (let ((dad (ly:context-parent ctx)))
     (ly:context-property-where-defined dad 'collaParte (ly:context-find dad 'Score))))

#(define (Colla_parte_source_translator ctx)
   "For this Staff context and any of its child contexts, broadcast a copy of
every music-event to a <delay-dispatcher> stored in the parent context of this
Staff. Add the property 'relay = #t to every copied event. Clear the events stored
in each <delay-dispatcher> in collaParteDispatchers at the end of each timestep."
   (let ((relay-ctx #f))

     (define (listen-and-relay c)
       (let* ((source (ly:context-event-source c))
              (id (voice-id-or-staff c))
              (relay-alist (ly:context-property relay-ctx 'collaParteRelays))
              (has-relay (assoc-get id relay-alist))
              (relay (or has-relay (ly:make-dispatcher))))
         (ly:add-listener (lambda (event)
                            (let ((ev-copy (ly:event-deep-copy event)))
                              (ly:event-set-property! ev-copy 'relay #t)
                              (ly:broadcast relay ev-copy)))
                          source
                          'music-event)
         (unless has-relay
           (ly:context-set-property! relay-ctx 'collaParteRelays
                                     (acons id relay relay-alist)))))

     (make-translator
      ((initialize translator)
       (set! relay-ctx (find-colla-parte-relay ctx))
       (listen-and-relay ctx))

      (listeners
       ((AnnounceNewContext translator event)
        (let ((baby (ly:event-property event 'context)))
          (when (eq? ctx (ly:context-parent baby))
            (listen-and-relay baby))))))))

#(define (Colla_parte_translator ctx)
   "Map this Staff context and each of its child contexts to receive events
from the <delay-dispatcher> with corresponding id stored in the parent context's
collaParteDispatchers. Wait until pre-process-music to dump events from the
<delay-dispatchers>. If a rhythmic-event happens within this Staff that wasn't
relayed via collaParteDispatchers, ignore events from collaParteDispatchers until
after the duration of the rhythmic-event.

Copy events from all child contexts spawned with empty context-id to the child
context with id \"default\"."
   (let ((relay-ctx #f)
         (disconnect-until #f)
         (default-dispatcher #f))

     (define (connect c)
       (let* ((source (ly:context-event-source c))
              (id (voice-id-or-staff c))
              (relay-alist (ly:context-property relay-ctx 'collaParteRelays))
              (has-relay (assoc-get id relay-alist))
              (relay (or has-relay (ly:make-dispatcher)))
              (delay-alist (ly:context-property ctx 'collaParteDispatchers))
              (has-delay (assoc-get id delay-alist))
              (delayer (or has-delay (make <delay-dispatcher>))))
         (ly:connect-dispatchers (in delayer) relay)
         (ly:connect-dispatchers source (out delayer))
         (unless has-relay
           (ly:context-set-property! relay-ctx 'collaParteRelays
                                     (acons id relay relay-alist)))
         (unless has-delay
           (ly:context-set-property! ctx 'collaParteDispatchers
                                     (acons id delayer delay-alist)))))

     (make-translator
      ((initialize translator)
       (set! relay-ctx (find-colla-parte-relay ctx))
       (set! default-dispatcher (ly:make-dispatcher))
       (connect ctx))

      ((start-translation-timestep translator)
       (when (ly:moment? disconnect-until)
         (unless (ly:moment<? (ly:context-current-moment ctx)
                              disconnect-until)
           (set! disconnect-until #f))))

      (listeners
       ((AnnounceNewContext translator event)
        (let* ((baby (ly:event-property event 'context))
               (id (ly:context-id baby)))
          (when (eq? ctx (ly:context-parent baby))
            (cond
             ((equal? id "")
              (ly:add-listener (cut ly:broadcast default-dispatcher <>)
                               (ly:context-event-source baby)
                               'music-event))
             ((equal? id "default")
              (ly:connect-dispatchers (ly:context-event-source baby)
                                      default-dispatcher)
              (connect baby))
             (else
              (connect baby))))))

       ((rhythmic-event translator event)
        (unless (or (ly:event-property event 'relay #f)
                    (ly:in-event-class? event 'skip-event))
          (let* ((len (ly:event-length event))
                 (now (ly:context-current-moment ctx))
                 (maybe-dc-until (ly:moment-add len now)))
            (unless (and disconnect-until
                         (ly:moment<? maybe-dc-until disconnect-until))
              (set! disconnect-until (ly:moment-add len now)))))))

      ((pre-process-music translator)
       (let ((delay-alist (ly:context-property ctx 'collaParteDispatchers)))
         (unless disconnect-until
             (for-each (compose dump cdr) delay-alist))
         (for-each (compose clear! cdr) delay-alist)))
      
      )))

#(set-object-property! 'collaParteDispatchers 'translation-type? alist?)
#(set-object-property! 'collaParteRelays 'translation-type? alist?)
#(set-object-property! 'collaParte 'translation-type? boolean-or-symbol?)

#(define (colla-parte-grob? grob)
   (let ((cause (event-cause grob)))
     (and cause
          (ly:event-property cause 'relay #f))))

#(define (remove-colla-parte-grobs axis)
   "Remove grobs that were relayed from a different Staff by Colla_parte_translator
from items-worth-living, so that this Staff will be treated as empty unless it has
musical material of its own."
   (and-let* ((items-worth-living (ly:grob-object axis 'items-worth-living #f))
              (worth-living-list (ly:grob-array->list items-worth-living))
              (really-worth-living (filter (compose not colla-parte-grob?)
                                           worth-living-list)))
     (ly:grob-set-object! axis 'items-worth-living
                          (ly:grob-list->grob-array really-worth-living))))

%{

  Make substitutions in 'property-operations:
  - (assign collaParte source) -> (consists Colla_parte_source_engraver)
      run procedure on 'element to find all Voice ids + their property-operations
      accumulate them

  - (assign collaParte client) -> (consists Colla_parte_engraver)
                                  (defaultchild Devnull)
      run procedure on 'element to wrap music with skips to keep alive every Voice
      id accumulated with appropriate property-operations
      use defaultchild from output def before substituting Devnull

%}

#(define (bottom-ctx-symbol? sym)
   (or (eq? sym 'Bottom)
       (not (ly:context-def-lookup
             (ly:output-def-lookup
              (ly:parser-lookup '$defaultlayout)
              sym) 'accepts #f))))

#(define (is-bottom-context? m)
   (and (music-is-of-type? m 'context-specification)
        (bottom-ctx-symbol? (ly:music-property m 'context-type))))

#(define (is-staff-context? m)
   (and-let* (((music-is-of-type? m 'context-specification))
              (context-def (ly:output-def-lookup
                            (ly:parser-lookup '$defaultlayout)
                            (ly:music-property m 'context-type)))
              (accepts (ly:context-def-lookup context-def 'accepts #f)))
     (every bottom-ctx-symbol? accepts)))

#(define (select-colla-parte role)
   "Return a function that checks for collaParte = role in 'property-operations of
a context-specced-music. If it finds it, return the 'property-operations list,
otherwise return #f."
   (lambda (staff)
     (let ((prop-ops (ly:music-property staff 'property-operations))
           (role-op (list 'assign 'collaParte role)))
       (and (member role-op prop-ops)
            prop-ops))))

#(define (accum-bottom-ctxs this-ctx id-ops-alist)
   "Use the 'context-id and 'property-operations of a context-specced-music
as the key and value to update an alist summarizing context ids and property-ops.
For use with fold."
   (let* ((this-id (ly:music-property this-ctx 'context-id))
          (this-ops (ly:music-property this-ctx 'property-operations))
          (same-id (assoc-get this-id id-ops-alist)))
     (cond
      ((not same-id) (acons this-id this-ops id-ops-alist))
      ((equal? same-id this-ops) id-ops-alist)
      (else (assoc-set! id-ops-alist this-id
                        (lset-union equal? this-ops same-id))))))

#(define (setup-source-staff staff)
   "Set up this staff to be the source of colla parte material for its group.
Parse its music expression to identify the id and mods of every bottom context,
so that contexts with appropriate ids can be kept alive in the client staves."
   (and-let* ((other-ops ((select-colla-parte 'source) staff))
              (ops-setup (cons `(consists ,Colla_parte_source_translator)
                               other-ops))
              (staff-music (ly:music-property staff 'element))
              (bottom-ctxs (extract-music staff-music is-bottom-context?)))
             (ly:music-set-property! staff 'property-operations ops-setup)
             bottom-ctxs))

#(define (voice-defaults id-ops-pair)
   "Accept an alist entry (context-id . property-ops). If the id is 1, 2, 3, or 4
prepend \\voiceOne \\voiceTwo \\voiceThree or \\voiceFour to property-ops and
return just the property-ops list. Otherwise simply return the property-ops list."
   (define (prepend-mods mod-music)
     (append (ly:get-context-mods (context-mod-from-music mod-music))
             (cdr id-ops-pair)))
   (cond
    ((equal? "1" (car id-ops-pair)) (prepend-mods voiceOne))
    ((equal? "2" (car id-ops-pair)) (prepend-mods voiceTwo))
    ((equal? "3" (car id-ops-pair)) (prepend-mods voiceThree))
    ((equal? "4" (car id-ops-pair)) (prepend-mods voiceFour))
    (else (cdr id-ops-pair))))

#(define (setup-client-staff staff bottom-info-alist)
   "Set up this staff to receive and print musical material from a colla parte source
staff whenever this staff does not have musical material of its own. Wrap the music
expression belowing to this staff with skips to keep alive all context ids used by the
colla parte source."
   (and-let* ((other-ops ((select-colla-parte 'client) staff))
              (ops-setup (cons* `(consists ,Colla_parte_translator)
                                `(push VerticalAxisGroup
                                       ,remove-colla-parte-grobs
                                       before-line-breaking)
                                '(default-child "Devnull")
                                other-ops))
              (default-voice (ly:context-def-lookup
                              (ly:output-def-lookup
                               (ly:parser-lookup '$defaultlayout)
                               (ly:music-property staff 'context-type))
                              'default-child))
              (staff-music (ly:music-property staff 'element))
              (skip-music (skip-of-length staff-music))
              (keep-alive-voice (lambda (id-ops-pair)
                                  (context-spec-music skip-music
                                                      default-voice
                                                      (car id-ops-pair)
                                                      (voice-defaults id-ops-pair))))
              (keep-alive-music (cons
                                 (context-spec-music staff-music
                                                     default-voice
                                                     "default")
                                 (map keep-alive-voice bottom-info-alist))))
    (ly:music-set-property! staff 'property-operations ops-setup)
    (ly:music-set-property! staff 'element
                           (make-simultaneous-music keep-alive-music))))

collaParte =
#(define-music-function (m) (ly:music?)
   (let* ((staves (extract-music m is-staff-context?))
          (source-voices (concatenate (filter-map setup-source-staff staves)))
          (bottom-info (fold accum-bottom-ctxs '() source-voices)))
     (for-each (cut setup-client-staff <> bottom-info) staves)
     m))

music = \relative {
  c'4 d e f g a b c
  %  \new Voice = "4" { g4 4 4 4 }
  \voices 1,2 <<
    {
      \resetRelativeOctave c''
      c4 d e f g a b c
      d4 c b a g f e d
      c1
    }
    \\
    {
      g1
    }
  >>
  <<
    {
      c,1
    }
    \context Staff = "foo" {
      c'2 2
    }
  >>
  \voices 1,2 <<
    {
      \resetRelativeOctave c'
      c8 e d f e g f a
      g b a c b d c e
      d4 c b a g f e d
      c1
    }
    \\
    {
      g1
    }
  >>
  f'1(\<
  g2 a)\f
}

global = {
  s1*15
}

\collaParte \new StaffGroup = "B" \with {
  collaParte = #'relay
} <<
  <<
    \new Staff = "Vln" \with {
    collaParte = #'source
  } << \global \music >>
  \new Staff  = "VlnB" \with {
    collaParte = #'client
%     \override VerticalAxisGroup.remove-empty = ##t
%     \override VerticalAxisGroup.remove-first = ##t
  } \global
  >>
  \new StaffGroup = "A" \new Staff  = "foo" \with {
    collaParte = #'client
%     \override VerticalAxisGroup.remove-empty = ##t
%     \override VerticalAxisGroup.remove-first = ##t
  } <<
    \global
    {
      s1*14
      e'2 d')\f
    }
  >>
>>