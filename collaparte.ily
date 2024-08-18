\version "2.25.18"

#(use-modules (srfi srfi-2)
              (srfi srfi-26)
              (ice-9 receive))

#(ly:set-option 'debug-eval)

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
                              (ly:event-set-property! ev-copy 'relay #t)
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
         (kill-list '())
         (live-list '())
         (canonical-voices '())
         (trigger-event #f))
     
     (define (find-canonical ctx)
       (let ((id (ly:context-id ctx)))
         (find (lambda (child) 
                 (equal? id (ly:context-id child)))
               canonical-voices)))
     
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
     
     (define (canonical-relay canonical)
       (let ((canonical-source (ly:context-event-source canonical)))
         (lambda (event)
            (unless (ly:event-property event 'relay #f)
               (let ((ev-copy (ly:event-deep-copy event)))
                   ;(ly:message "~a" ev-copy)
                   ; (ly:event-set-property! event 'relay #t)
                   (ly:broadcast canonical-source ev-copy))))))

     (define (purify grob)
       (define (map-cadrs proc li)
         (map (lambda (el) `(,(car el)
                              ,(proc (cadr el)))) li))

       (define (remove-condemned prop)
         (receive (innocent condemned) (lset-diff+intersection eq? (cadr prop) kill-list)
           (when (pair? condemned)
             (ly:grob-set-object! grob (car prop) (ly:grob-list->grob-array innocent)))))

       (let* ((objects (append-map cadr (cadr (grob::all-objects grob))))
              (singles (filter (compose ly:grob? cadr) objects))
              (arrays (filter (compose ly:grob-array? cadr) objects))
              (lists (map-cadrs ly:grob-array->list arrays))
              (dead-singles (filter (compose (cut member <> kill-list eq?) cadr) singles)))

         (ly:message "~a" dead-singles)
         (for-each (compose (cut ly:grob-set-object! grob <> '()) car) dead-singles)
         (for-each remove-condemned lists)))
     
     (make-engraver
      ((initialize engraver)
       (connect ctx))
      
      ((start-translation-timestep engraver)
       (when (ly:moment? disconnect-until)
         (unless (ly:moment<? (ly:context-current-moment ctx)
                              disconnect-until)
            (connect ctx)
            (for-each connect canonical-voices)
           (set! disconnect-until #f))))
      
      (listeners
       ((AnnounceNewContext engraver event)
        (let* ((baby (ly:event-property event 'context))
               (canonical (find-canonical baby)))
          (if canonical
              (let ((relay (canonical-relay canonical)))
;                 (when (ly:stream-event? trigger-event)
;                   (ly:message "Relaying trigger ~a ~a"
;                               (ly:context-current-moment ctx)
;                               event)
;                   (relay trigger-event)
;                   (set! trigger-event #f))
                (ly:add-listener relay
                                 (ly:context-event-source baby)
                                 'music-event))
              ;; otherwise
              (unless (eq? baby canonical) ;; what if a context dies and gets created again?
                (set! canonical-voices (cons baby canonical-voices))
                (unless disconnect-until
                  (connect baby))))))

       ((rhythmic-event engraver event)
        (unless (or disconnect-until
                    (ly:event-property event 'relay #f)
                    (ly:in-event-class? event 'skip-event))
          (disconnect ctx)
          (for-each disconnect canonical-voices)
;           (set! trigger-event event)
          (let ((len (ly:event-length event))
                (now (ly:context-current-moment ctx)))
            (set! disconnect-until (ly:moment-add len now))))))
      
      (acknowledgers
       ((grob-interface engraver grob source-engraver)
          (let* ((grob-cause (event-cause grob))
                 (source-ctx (ly:translator-context source-engraver))
                 (legit-source? (or (eq? source-ctx ctx)
                                    (eq? source-ctx (find-canonical source-ctx)))))
;             (when (grob::has-interface grob 'stem-interface)
;               (ly:message "~a ~a ~a ~a"
;                           (ly:context-current-moment ctx)
;                           (not legit-source?)
;                           (and disconnect-until grob-cause (ly:event-property grob-cause 'relay #f))
;                           (append-map cadr (cadr (grob::all-objects grob)))))
            (if (or (not legit-source?)
                      (and disconnect-until
                           grob-cause
                           (ly:event-property grob-cause 'relay #f)))
              (set! kill-list (cons grob kill-list))
              (set! live-list (cons grob live-list))))))
      
;         (end-acknowledgers
;          ((slur-interface engraver grob source-engraver)
;           (ly:message "End ~a ~a" 
;                       (ly:spanner-bound grob RIGHT)
;                       (grob::all-objects grob))))
      
      ((process-acknowledged engraver)
       (map purify live-list)
;        (ly:message "~a ~a ~a" 
;                    (ly:context-current-moment ctx)
;                    kill-list
;                    (map (cut ly:grob-property <> 'duration-log) kill-list))
       (for-each ly:grob-suicide! kill-list)
       ; (ly:message "~a" kill-list)
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
%       \once\override Staff.Stem.before-line-breaking = #(lambda (g) (ly:message "Foo ~a"
%                                                                        (ly:grob-alist-chain g)
%                                                                       ))
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
  g2 a\f
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
%       \override Staff.NoteHead.before-line-breaking = #(lambda (g) (ly:message "Foo ~a ~a"
%                                                                              (ly:grob-object g 'stem)
%                                                                              (event-cause g)))
      e'2 d')\f
    }
  >>
>>