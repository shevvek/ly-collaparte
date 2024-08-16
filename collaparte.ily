\version "2.25.18"

#(use-modules (srfi srfi-2)
              (srfi srfi-26))

\layout {
  \context {
    \Voice
    \consists #(lambda (ctx)
                 (make-engraver
                  ((finalize engraver)
                   (ly:message "finalizing ~a ~a"
                               (ly:context-id ctx)
                               (ly:context-current-moment ctx)))))
  }
}

#(define (listen-and-relay destination source-ctx event-type)
   (let ((source (ly:context-event-source source-ctx)))
     ; (ly:message "setting relay ~a" source)
     (ly:add-listener (lambda (event)
                       ;  (ly:message "relaying ~a" event)
                        (ly:broadcast destination 
                                    (ly:event-deep-copy event)))
                      source
                      event-type)))

#(define (find-child-with-id parent id)
   (find (lambda (child) (equal? id (ly:context-id child)))
         (ly:context-children parent)))

#(define (Colla_parte_engraver ctx)
   (let ((source-ctx #f)
         (relay-dispatchers '()))
     
     (define (set-broadcaster voice-ctx)
       (let* ((new-id (ly:context-id voice-ctx))
              (old-dispatch (assoc-get new-id relay-dispatchers))
              (dispatch (or old-dispatch (ly:make-dispatcher))))
         (ly:message "setting up ~a" new-id)
         (listen-and-relay dispatch voice-ctx 'music-event)
         (unless old-dispatch
             (set! relay-dispatchers (acons new-id dispatch relay-dispatchers)))))

     (define (set-voice-relay create-event)
       (ly:message "top of relay")
       (let* ((new-id (ly:event-property create-event 'id))
              (new-ctx (find-child-with-id source-ctx new-id))
              (old-dispatch (assoc-get new-id relay-dispatchers))
              (dispatch (or old-dispatch (ly:make-dispatcher))))
         (ly:message "got foreign announcement of voice ~a" new-ctx)
         (unless (null? new-id)
           (unless old-dispatch
             (ly:message "setting ~a" new-id)
             (set! relay-dispatchers (acons new-id dispatch relay-dispatchers)))
           (listen-and-relay dispatch new-ctx 'music-event)
           (let ((twin-ctx (find-child-with-id ctx new-id)))
             (ly:message "Foo ~a ~a" twin-ctx new-id)
             (if twin-ctx
                 (begin 
                  (ly:message "bar")
                  (ly:connect-dispatchers (ly:context-event-source twin-ctx)
                                         dispatch))
                  (ly:broadcast (ly:context-event-source ctx)
                                (ly:event-deep-copy create-event)))
             )))
       (ly:message "end of relay"))
       ;; get id
       ;; make dispatcher and add to alist
       ;; add listener to broadcast to dispatcher
       ;; copy event and broadcast to this context
     
     (make-engraver
      
      ((initialize engraver)
       ;; worry about a UI to select the source dispatcher later
       (let* ((my-dad (ly:context-parent ctx))
              (my-sib (car (ly:context-children my-dad)))
              (my-source (ly:context-event-source ctx))
              (your-source (ly:context-event-source my-sib))
              (source-kids (ly:context-children my-sib)))
         (set! source-ctx my-sib)
         (ly:add-listener set-voice-relay
                          your-source
                          'CreateContext)
          (listen-and-relay my-source
                            source-ctx
                            'music-event)
          (for-each set-broadcaster source-kids))
       (ly:message "ending init ~a" (eq? (assoc-get "1" relay-dispatchers)
                                         (assoc-get "2" relay-dispatchers))))
      
       (listeners
;         ((skip-event engraver event)
;          (let ((my-kid-sources (map ly:context-event-source 
;                                     (ly:context-children ctx))))
;            (for-each (cut ly:broadcast <> event) my-kid-sources)))
        
        ((RemoveContext engraver event)
         (ly:message "Removing context ~a" (ly:event-property event 'id)))
        
        ((AnnounceNewContext engraver event)
         (let* ((created-ctx (ly:event-property event 'context))
                (created-id (ly:context-id created-ctx))
                (this-dispatch (assoc-get created-id relay-dispatchers)))
           (ly:message "got announcement here ~a" this-dispatch)
           (when this-dispatch
             (ly:message "connecting")
              (ly:connect-dispatchers (ly:context-event-source created-ctx)
                                      this-dispatch)
             )
           (ly:message "succeeded"))))
      
      ((start-translation-timestep engraver)
       (ly:message "got timestep ~a"
                   (ly:context-current-moment ctx)))
      
      ((stop-translation-timestep engraver)
       (ly:message "end timestep ~a"
                   (ly:context-current-moment ctx)))
      
      )))

#(define (Debug_events_translator ctx)
   (make-translator
    (listeners
     ((AnnounceNewContext translator event)
      (ly:message "~a" event)))))

channel = #(ly:make-dispatcher)

collaParte = \applyContext
#(lambda (ctx)
   (let* ((my-dad (ly:context-parent (ly:context-find ctx 'Staff)))
          (my-sib (car (ly:context-children my-dad)))
          (my-source (ly:context-event-source ctx))
          (your-source (ly:context-events-below my-sib)))
     (define (relay-events ev)
       (ly:broadcast channel (ly:event-deep-copy ev)))
     (ly:add-listener relay-events your-source 'music-event)
     (ly:connect-dispatchers my-source channel)
     (ly:connect-dispatchers my-source channel)
     ))

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

global = <<
  #(skip-of-length music)
  \context Voice = "1" {
    #(skip-of-length music)
  }
  \context Voice = "2" {
    #(skip-of-length music)
  }
>>

\new StaffGroup \with {
} <<
  \new Staff = "I" \with {
    instrumentName = "1"
  } << \global \music >>
 %  \new Staff = "Div" \with {
%   %   \consists #Debug_events_translator
%   } \new Voice \with {
%     \collaParte
%   } \global
  \new Staff \with {
    \consists #Debug_events_translator
    \consists #Colla_parte_engraver
  } \global
>>