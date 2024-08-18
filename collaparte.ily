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
         \context Voice = "default" { #music }
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
   (cond
    ((eq? c (ly:context-find c 'Staff)) "Staff")
    ((equal? "" (ly:context-id c)) "default")
    (else (ly:context-id c))))

#(define (Colla_parte_source_engraver ctx)
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

     (make-engraver
      ((initialize engraver)
       (set! default-dispatcher (ly:make-dispatcher))
       (connect ctx))

      ((start-translation-timestep engraver)
       (when (ly:moment? disconnect-until)
         (unless (ly:moment<? (ly:context-current-moment ctx)
                              disconnect-until)
           (set! disconnect-until #f))))

      (listeners
       ((AnnounceNewContext engraver event)
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

       ((rhythmic-event engraver event)
        (unless (or (ly:event-property event 'relay #f)
                    (ly:in-event-class? event 'skip-event))
          (let* ((len (ly:event-length event))
                 (now (ly:context-current-moment ctx))
                 (maybe-dc-until (ly:moment-add len now)))
            (unless (and disconnect-until
                         (ly:moment<? maybe-dc-until disconnect-until))
              (set! disconnect-until (ly:moment-add len now)))))))

      ((pre-process-music engraver)
       (let* ((dad (ly:context-parent ctx))
              (relay-alist (ly:context-property dad 'collaParteDispatchers)))
         (if disconnect-until
             (for-each (compose clear cdr) relay-alist)
             (for-each (compose dump cdr) relay-alist))))
      
      )))

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
    \defaultchild Devnull
  } <<
    \keepAliveBasicVoices \global
    {
      s1*14
      e'2 d')\f
    }
  >>
>>