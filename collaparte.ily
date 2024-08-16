\version "2.25.18"

#(use-modules (srfi srfi-2)
              (srfi srfi-26))



#(define (find-child-with-id parent id)
   (find (lambda (child) (equal? id (ly:context-id child)))
         (ly:context-children parent)))

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
       \context Voice { #skips }
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
;                               (ly:message "Relay via ~a at ~a"
;                                           (ly:context-current-moment ctx)
;                                           ev-copy)
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
   (let ((disconnect-until #f))
     
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
           (ly:disconnect-dispatchers source has-relay))))
              
     
     (make-engraver
      ((initialize engraver)
       (connect ctx))
      
      ((start-translation-timestep engraver)
       (when (ly:moment? disconnect-until)
         (unless (ly:moment<? (ly:context-current-moment ctx)
                              disconnect-until)
           (set! disconnect-until #f)
           (connect ctx)
           (for-each connect (ly:context-children ctx)))))
      
      (listeners
       ((AnnounceNewContext engraver event)
        (unless disconnect-until
          (let ((baby (ly:event-property event 'context)))
            (connect baby))))
       
       ((rhythmic-event engraver event)
        (unless (or (ly:event-property event 'relay #f)
                    (ly:in-event-class? event 'skip-event))
          (disconnect ctx)
          (for-each disconnect (ly:context-children ctx))
          (let ((len (ly:event-length event))
                (now (ly:context-current-moment ctx)))
            (set! disconnect-until (ly:moment-add len now)))))))))


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
    \context Staff = "foo" {
      c'1
    }
    {
      c,1
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
}

global = {
  s1*16
}

\new StaffGroup <<
  \new Staff = "Vln" \with {
    \consists #Colla_parte_source_engraver
  } << \global \music >>
  \new Staff  = "foo" \with {
    \consists #Colla_parte_engraver
  } \keepAliveBasicVoices \global
>>