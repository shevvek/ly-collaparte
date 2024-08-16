\version "2.25.18"

#(use-modules (srfi srfi-2)
              (srfi srfi-26))



#(define (find-child-with-id parent id)
   (find (lambda (child) (equal? id (ly:context-id child)))
         (ly:context-children parent)))

#(define (find-twin ctx)
   (let ((dad (ly:context-parent ctx))
         (id (ly:context-id ctx)))
     (find (lambda (child) 
             (and (equal? id (ly:context-id child))
                  (not (eq? child ctx))))
           (ly:context-children dad))))

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
       

#(define (voice-id-or-staff c)
  (if (eq? c (ly:context-find c 'Staff))
      "Staff"
      (ly:context-id c)))

#(define (Colla_parte_source_engraver ctx)
   (let ()
     
     (define (listen-and-relay source-ctx)
       (let* ((source (ly:context-event-source source-ctx))
              (id (voice-id-or-staff source-ctx))
              (dad (ly:context-parent ctx))
              (relay-alist (ly:context-property dad 'collaParteDispatchers))
              (has-relay (assoc-get id relay-alist))
              (relay (or has-relay (ly:make-dispatcher))))
         (ly:add-listener (lambda (event)
                            (let ((ev-copy (ly:event-deep-copy event)))
                              (ly:event-set-property! ev-copy 'relay id)
                              (ly:broadcast relay ev-copy)))
                          source
                          'music-event)
         (unless has-relay
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
         (kill-list '()))
     
     (define (connect c)
       (let* ((source (ly:context-event-source c))
              (id (voice-id-or-staff c))
              (dad (ly:context-parent ctx))
              (relay-alist (ly:context-property dad 'collaParteDispatchers))
              (has-relay (assoc-get id relay-alist))
              (relay (or has-relay (ly:make-dispatcher))))
           (ly:connect-dispatchers source relay)
           (unless has-relay
             (ly:context-set-property! dad 'collaParteDispatchers
                                   (acons id relay relay-alist)))))
     
     (define (disconnect c)
       (let* ((source (ly:context-event-source c))
              (id (voice-id-or-staff c))
              (dad (ly:context-parent ctx))
              (relay-alist (ly:context-property dad 'collaParteDispatchers))
              (has-relay (assoc-get id relay-alist)))
         (when has-relay
           (ly:message "Disconnecting ~a" id)
           (ly:disconnect-dispatchers source has-relay))))
              
     
     (make-engraver
      ((initialize engraver)
       (connect ctx))
      
      ((start-translation-timestep engraver)
       (when (ly:moment? disconnect-until)
         (unless (ly:moment<? (ly:context-current-moment ctx)
                              disconnect-until)
;            (connect ctx)
;            (for-each connect (ly:context-children ctx))
           (set! disconnect-until #f))))
      
      (listeners
       ((AnnounceNewContext engraver event)
        (let* ((baby (ly:event-property event 'context))
               (twin (find-twin baby)))
          (if twin
             (let ((baby-source (ly:context-event-source baby))
                   (twin-source (ly:context-event-source twin)))
                (ly:add-listener (lambda (event)
                                    (unless (ly:event-property event 'relay #f)
                                      (let ((ev-copy (ly:event-deep-copy event)))
                                        (ly:event-set-property! event 'relay #t)
                                        (ly:broadcast twin-source ev-copy))))
                            baby-source
                            'music-event))
            (unless disconnect-until
              (connect baby)))))
       
       ((rhythmic-event engraver event)
        (unless (or (ly:event-property event 'relay #f)
                    (ly:in-event-class? event 'skip-event))
;           (disconnect ctx)
;           (for-each disconnect (ly:context-children ctx))
          (let ((len (ly:event-length event))
                (now (ly:context-current-moment ctx)))
            (set! disconnect-until (ly:moment-add len now))))))
      
      (acknowledgers
       ((grob-interface engraver grob source-engraver)
        (when disconnect-until
          (let ((grob-cause (event-cause grob)))
            (when (and grob-cause
                       (ly:event-property grob-cause 'relay #f))
              (set! kill-list (cons grob kill-list)))))))
      
      ((process-acknowledged engraver)
       (for-each ly:grob-suicide! kill-list)
       (set! kill-list '()))
      
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
      c'1
    }
  >>
  \voices 1,2 << 
    {
      \resetRelativeOctave c'
      c4 d e f g a b c
      d4 c b a g f e d
      c1
    }
    \\
    {
      g1
    }
  >>
  f'1(\< <>\!
  g2 a)
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
    { s1*14 e'2 d' }
  >>
>>