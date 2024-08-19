\version "2.25.18"

#(use-modules (srfi srfi-2)
              (srfi srfi-26)
              (ice-9 receive)
              (oop goops))

#(define-class <delay-dispatcher> ()
   (in-dispatcher #:init-thunk ly:make-dispatcher #:getter in)
   (out-dispatcher #:init-thunk ly:make-dispatcher #:getter out)
   (event-queue #:init-value '() #:accessor queue))

#(define-method (dump! (d <delay-dispatcher>))
   (for-each (lambda (ev) (ly:broadcast (out d) ev)) (queue d))
   (set! (queue d) '()))

#(define-method (clear! (d <delay-dispatcher>))
   (set! (queue d) '()))

#(define-method (initialize (d <delay-dispatcher>) initargs)
   (next-method)
   (ly:add-listener (lambda (ev)
                      (set! (queue d)
                            (cons ev (queue d))))
                    (in d)
                    'StreamEvent))

#(define (voice-id-or-staff c)
   (cond
    ((eq? c (ly:context-find c 'Staff)) "Staff")
    ((equal? "" (ly:context-id c)) "default")
    (else (ly:context-id c))))

#(define (Colla_parte_source_translator ctx)
   (let ()

     (define (listen-and-relay c)
       (let* ((source (ly:context-event-source c))
              (id (voice-id-or-staff c))
              (dad (ly:context-parent ctx))
              (relay-alist (ly:context-property dad 'collaParteDispatchers))
              (has-relay (assoc-get id relay-alist))
              (relay (or has-relay (make <delay-dispatcher>))))
         (if has-relay
             (ly:add-listener (lambda (event)
                                (let ((ev-copy (ly:event-deep-copy event)))
                                  (ly:event-set-property! ev-copy 'relay #t)
                                  (ly:broadcast (in relay) ev-copy)))
                              source
                              'music-event)
             (ly:context-set-property! dad 'collaParteDispatchers
                                       (acons id relay relay-alist)))))

     (make-translator
      ((initialize translator)
       (listen-and-relay ctx))

      (listeners
       ((AnnounceNewContext translator event)
        (let ((baby (ly:event-property event 'context)))
          (when (eq? ctx (ly:context-parent baby))
            (listen-and-relay baby))))))))

#(define (Colla_parte_translator ctx)
   (let ((disconnect-until #f)
         (default-dispatcher #f))

     (define (connect c)
       (let* ((source (ly:context-event-source c))
              (id (voice-id-or-staff c))
              (dad (ly:context-parent ctx))
              (relay-alist (ly:context-property dad 'collaParteDispatchers))
              (has-relay (assoc-get id relay-alist))
              (relay (or has-relay (make <delay-dispatcher>))))
         (ly:connect-dispatchers source (out relay))
         (unless has-relay
           (ly:context-set-property! dad 'collaParteDispatchers
                                     (acons id relay relay-alist)))))

     (make-translator
      ((initialize translator)
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
            (connect baby)))))

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
       (let* ((dad (ly:context-parent ctx))
              (relay-alist (ly:context-property dad 'collaParteDispatchers)))
         (if disconnect-until
             (for-each (compose clear! cdr) relay-alist)
             (for-each (compose dump! cdr) relay-alist))))

      )))

#(set-object-property! 'collaParteDispatchers 'translation-type? alist?)
% #(set-object-property! 'collaParte 'translation-type? boolean-or-symbol?)

#(define (colla-parte-grob? grob)
   (let ((cause (event-cause grob)))
     (and cause
          (ly:event-property cause 'relay #f))))

#(define (remove-colla-parte-grobs axis)
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

#(define (is-bottom-context? m)
   (and (music-is-of-type? m 'context-specification)
        (let ((type (ly:music-property m 'context-type)))
          (or (eq? type 'Bottom)
              (not (ly:context-def-lookup
                    (ly:output-def-lookup
                     (ly:parser-lookup '$defaultlayout)
                     type) 'accepts #f))))))

#(define (select-colla-parte role)
   (lambda (staff)
     (let ((prop-ops (ly:music-property staff 'property-operations))
           (role-op (list 'assign 'collaParte role)))
       (and (member role-op prop-ops)
            (delete role-op prop-ops)))))

#(define (accum-bottom-ctxs this-ctx id-ops-alist)
   (let* ((this-id (ly:music-property this-ctx 'context-id))
          (this-ops (ly:music-property this-ctx 'property-operations))
          (same-id (assoc-get this-id id-ops-alist)))
     (cond
      ((not same-id) (acons this-id this-ops id-ops-alist))
      ((equal? same-id this-ops) id-ops-alist)
      (else (assoc-set! id-ops-alist this-id
                        (lset-union equal? this-ops same-id))))))

#(define (setup-source-staff staff)
   (and-let* ((other-ops ((select-colla-parte 'source) staff))
              (ops-setup (cons `(consists ,Colla_parte_source_translator) other-ops))
              (staff-music (ly:music-property staff 'element))
              (bottom-ctxs (extract-music staff-music is-bottom-context?)))
             (ly:music-set-property! staff 'property-operations ops-setup)
             bottom-ctxs))

#(define (voice-defaults id-ops-pair)
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
   (and-let* ((other-ops ((select-colla-parte 'client) staff))
              (ops-setup (cons* `(consists ,Colla_parte_translator)
                                `(push VerticalAxisGroup ,remove-colla-parte-grobs before-line-breaking)
                                '(default-child "Devnull")
                                other-ops))
              (default-voice (ly:context-def-lookup
                              (ly:output-def-lookup
                               (ly:parser-lookup '$defaultlayout)
                               (ly:music-property staff 'context-type)) 'default-child))
              (staff-music (ly:music-property staff 'element))
              (skip-music (skip-of-length staff-music))
              (keep-alive-voice (lambda (id-ops-pair)
                                  (context-spec-music skip-music
                                                      default-voice
                                                      (car id-ops-pair)
                                                      (voice-defaults id-ops-pair))))
              (keep-alive-music (cons
                                 (context-spec-music staff-music default-voice "default")
                                 (map keep-alive-voice bottom-info-alist))))
     (ly:music-set-property! staff 'property-operations ops-setup)
     (ly:music-set-property! staff 'element (make-simultaneous-music keep-alive-music))))

collaParteGroup =
#(define-music-function (staves-simul-music) (ly:music?)
   (let* ((staves (ly:music-property staves-simul-music 'elements))
          (source-voices (concatenate (filter-map setup-source-staff staves)))
          (bottom-info (fold accum-bottom-ctxs '() source-voices)))
     (for-each (cut setup-client-staff <> bottom-info) staves)
     staves-simul-music))

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

\new StaffGroup \collaParteGroup <<
  \new Staff = "Vln" \with {
    collaParte = #'source
  } << \global \music >>
  \new Staff  = "foo" \with {
    collaParte = #'client
    \override VerticalAxisGroup.remove-empty = ##t
    \override VerticalAxisGroup.remove-first = ##t
  } <<
    \global
    {
      s1*14
      e'2 d')\f
    }
  >>
>>