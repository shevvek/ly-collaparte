\version "2.25.10"

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
              (dad (ly:context-parent ctx))
              (id (voice-id-or-staff source-ctx))
              (relay-alist (ly:context-property dad 'collaParteDispatchers))
              (has-relay (assoc-get id relay-alist))
              (relay (or has-relay (ly:make-dispatcher))))
         (ly:message "Relaying ~a" id)
         (ly:add-listener (lambda (event)
                            (ly:broadcast relay event))
                                       ;  (ly:event-deep-copy event)))
                          source
                          'music-event)
         ; not checking if duplicate context ids are spawned!
         (unless has-relay
             (ly:context-set-property! dad 'collaParteDispatchers
                                   (acons id relay relay-alist)))))
     
     (make-engraver
      ((initialize engraver)
       (listen-and-relay ctx))
      
      (listeners
       ((AnnounceNewContext engraver event)
        (let ((baby (ly:event-property event 'context)))
          (listen-and-relay baby)))))))

#(define (Colla_parte_engraver ctx)
   (let ()
     
     (define (connect c)
       (let* ((dad (ly:context-parent ctx))
              (id (voice-id-or-staff c))
              (relay-alist (ly:context-property dad 'collaParteDispatchers))
              (has-relay (assoc-get id relay-alist))
              (relay (or has-relay (ly:make-dispatcher)))
              (my-source (ly:context-event-source c)))
           (ly:connect-dispatchers my-source relay)
           (unless has-relay
             (ly:context-set-property! dad 'collaParteDispatchers
                                   (acons id relay relay-alist)))))
     
     (make-engraver
      ((initialize engraver)
       (connect ctx))
      
      (listeners
       ((AnnounceNewContext engraver event)
        (let ((baby (ly:event-property event 'context)))
          (connect baby)))))))

#(set-object-property! 'collaParteDispatchers 'translation-type? list?)

music = \relative {
  c'4 d e f g a b c
 %  \new Voice = "4" { g4 4 4 4 }
  \voices 1,2 << 
    {
      c'4 d e f g a b c
      d4 c b a g f e d
      c1
    }
    \\
    {
      g1
    }
  >>
  c1
  \voices 1,2 << 
    {
      c'4 d e f g a b c
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