\version "2.25.18"

#(use-modules (srfi srfi-2)
              (srfi srfi-26)
              (ice-9 receive)
              (oop goops))

#(ly:set-option 'debug-eval)

keepAliveVoices =
#(define-music-function (voice-ids music) ((list? '("" "1" "2")) ly:music?)
   (let ((skips (skip-of-length music)))
     (make-simultaneous-music (cons skips
                                    (map (cut context-spec-music skips 'Voice <>)
                                         voice-ids)))))

keepAliveBasicVoices =
#(define-music-function (music) (ly:music?)
   (let ((skips (skip-of-length music)))
     #{
       <<
         #music
         \context Voice ="1" \with { \voiceOne } { #skips }
         \context Voice = "2" \with { \voiceTwo } { #skips }
       >>
     #}))


#(define-class <delay-dispatcher> ()
   (in-dispatcher #:init-thunk ly:make-dispatcher #:getter in)
   (out-dispatcher #:init-thunk ly:make-dispatcher #:getter out)
   (event-queue #:init-value '() #:accessor queue))

#(define-method (dump (d <delay-dispatcher>))
   (for-each (lambda (ev) (ly:broadcast (out d) ev)) (queue d))
   (set! (queue d) '()))

#(define-method (clear (d <delay-dispatcher>))
   (set! (queue d) '()))

#(define-method (initialize (d <delay-dispatcher>) initargs)
   (next-method)
   (ly:add-listener (lambda (ev)
                      (set! (queue d)
                            (cons ev (queue d))))
                    (in d)
                    'StreamEvent))

#(define (voice-id-or-staff c)
   (if (eq? c (ly:context-find c 'Staff))
       "Staff"
       (ly:context-id c)))

#(define (canonical-relay canonical)
   (let ((canonical-source (ly:context-event-source canonical)))
     (lambda (event)
       (unless (ly:event-property event 'relay #f)
         (let ((ev-copy (ly:event-deep-copy event)))
           (ly:broadcast canonical-source ev-copy))))))

#(define (Colla_parte_source_engraver ctx)
   (let ()

     (define (listen-and-relay source-ctx)
       (let* ((source (ly:context-event-source source-ctx))
              (id (voice-id-or-staff source-ctx))
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

     (make-engraver
      ((initialize engraver)
       (listen-and-relay ctx))

      (listeners
       ((AnnounceNewContext engraver event)
        (let ((baby (ly:event-property event 'context)))
          (when (eq? ctx (ly:context-parent baby))
            (listen-and-relay baby))))))))


#(define (Colla_parte_engraver ctx)
   (let ((disconnect-until #f)
         (kill-list '())
         (canonical-voices '()))

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

     (define (find-canonical ctx)
       (let ((id (ly:context-id ctx)))
         (find (lambda (child)
                 (equal? id (ly:context-id child)))
               canonical-voices)))

     (make-engraver
      ((initialize engraver)
       (connect ctx))

      ((start-translation-timestep engraver)
       (when (ly:moment? disconnect-until)
         (unless (ly:moment<? (ly:context-current-moment ctx)
                              disconnect-until)
           (set! disconnect-until #f))))

      (listeners
       ((AnnounceNewContext engraver event)
        (let* ((baby (ly:event-property event 'context))
               (canonical (find-canonical baby)))
          (if canonical
              (ly:add-listener (canonical-relay canonical)
                               (ly:context-event-source baby)
                               'music-event)
              (begin
               (set! canonical-voices (cons baby canonical-voices))
               (connect baby)))))

       ((rhythmic-event engraver event)
        (unless (or disconnect-until
                    (ly:event-property event 'relay #f)
                    (ly:in-event-class? event 'skip-event))
          (let ((len (ly:event-length event))
                (now (ly:context-current-moment ctx)))
            (set! disconnect-until (ly:moment-add len now))))))

      ((pre-process-music engraver)
       (let* ((dad (ly:context-parent ctx))
              (relay-alist (ly:context-property dad 'collaParteDispatchers)))
         (if disconnect-until
             (for-each (compose clear cdr) relay-alist))
         (for-each (compose dump cdr) relay-alist)))

      (acknowledgers
       ((grob-interface engraver grob source-engraver)
        (let ((source-ctx (ly:translator-context source-engraver)))
          (unless (or (eq? source-ctx ctx)
                      (eq? source-ctx (find-canonical source-ctx)))
            (set! kill-list (cons grob kill-list))))))

      ((process-acknowledged engraver)
       (for-each ly:grob-suicide! kill-list)
       (set! kill-list '())))))


#(set-object-property! 'collaParteDispatchers 'translation-type? list?)

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

\new StaffGroup <<
  \new Staff = "Vln" \with {
    \consists #Colla_parte_source_engraver
  } << \global \music >>
  \new Staff  = "foo" \with {
    \consists #Colla_parte_engraver
  } <<
    \keepAliveBasicVoices \global
    {
      s1*14
      e'2 d')\f
    }
  >>
>>