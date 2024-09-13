%%  Add-on for GNU LilyPond: copy musical material from another Staff
%%  whenever this Staff is empty, for use primarily with string divisi.
%%
%%  Copyright (C) 2024 Saul James Tobin.
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program.  If not, see <https://www.gnu.org/licenses/>.

\version "2.25.18"
% -*- master: tests.ly;

#(use-modules (srfi srfi-2)
              (srfi srfi-26)
              (oop goops))

#(define (boolean-or-alist? x)
   (or (boolean? x)
       (alist? x)))

#(set-object-property! 'collaParteRelays 'translation-type? boolean-or-alist?)
#(set-object-property! 'collaParteEventClasses 'translation-type? symbol-list?)
#(set-object-property! 'collaParteExcludeEvents 'translation-type? symbol-list?)
#(set-object-property! 'collaParteLocalEvents 'translation-type? symbol-list?)

\layout {
  \context {
    \Score
    collaParteEventClasses =
    #'(Override 
       Revert
       music-event)
    collaParteExcludeEvents =
    #'(structural-event
       mark-event
       time-signature-event
       key-change-event
       caesura-event
       footnote-event
       partial-event
       label-event
       bar-check-event
       section-label-event
       ad-hoc-jump-event
       tempo-change-event
       spacing-section-event)
    collaParteLocalEvents =
    #'(Override 
       Revert
       UnsetProperty
       SetProperty
       music-event)
  }
}


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
   "Map context ids to alist keys for collaParteRelays. Staff contexts map
to \"Staff\". Bottom contexts with empty id map to \"default\"."
   (cond
    ((eq? c (ly:context-find c 'Staff)) "Staff")
    ((equal? "" (ly:context-id c)) "default")
    (else (ly:context-id c))))

#(define (find-colla-parte-relay ctx)
   "Search upward for parent of ctx with collaParteRelays property set.
If none is found, return the Score context."
   (let ((dad (ly:context-parent ctx)))
     (ly:context-property-where-defined dad 'collaParteRelays
                                        (ly:context-find dad 'Score))))


#(define (Colla_parte_source_translator ctx)
   "For this Staff context and any of its child contexts, broadcast a copy of
every event to a dispatcher stored in a parent context of this Staff. Filter
events based on collaParteEventClasses and collaParteExcludeEvents. Exclusions
take precedence. Add the property 'relay = #t to every copied event."
   (let ((relay-ctx #f))

     (define (listen-and-relay c)
       (let* ((source (ly:context-event-source c))
              (id (voice-id-or-staff c))
              (relay-alist (ly:context-property relay-ctx 'collaParteRelays))
              (has-relay (assoc-get id relay-alist))
              (relay (or has-relay (ly:make-dispatcher))))
         (ly:add-listener
          (lambda (ev)
            (and-let* ((allowed-events 
                        (ly:context-property ctx 'collaParteEventClasses))
                       ((any (cut ly:in-event-class? ev <>) allowed-events))
                       (banned-events 
                        (ly:context-property ctx 'collaParteExcludeEvents))
                       ((not (any (cut ly:in-event-class? ev <>) banned-events)))
                       (ev-copy (ly:event-deep-copy ev)))
              (ly:event-set-property! ev-copy 'relay #t)
              (ly:broadcast relay ev-copy)))
          source
          'StreamEvent)
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
from a <delay-dispatcher> stored in this context's collaParteRelays. Each
<delay-dispatcher> receives events from the dispatcher with corresponding id
stored in a parent context's collaParteRelays. Wait until pre-process-music to
send events from the <delay-dispatchers>, and only do so if this Staff has no
musical material of its own in this timestep. Then clear the events queue of
each <delay-dispatcher>.

Copy events from all child contexts spawned with empty context-id to the child
context with id \"default\", so that spanners are allowed to have one bound in
copied music and the other in the music belonging to this Staff."
   (let ((relay-ctx #f)
         (disconnect-until #f)
         (default-dispatcher #f))

     (define (connect c)
       (let* ((source (ly:context-event-source c))
              (id (voice-id-or-staff c))
              (group-alist (ly:context-property relay-ctx 'collaParteRelays))
              (has-group-relay (assoc-get id group-alist))
              (group-relay (or has-group-relay (ly:make-dispatcher)))
              (staff-alist (ly:context-property ctx 'collaParteRelays))
              (has-staff-relay (assoc-get id staff-alist))
              (staff-relay (or has-staff-relay (make <delay-dispatcher>))))
         (ly:connect-dispatchers (in staff-relay) group-relay)
         (ly:connect-dispatchers source (out staff-relay))
         (unless has-group-relay
           (ly:context-set-property! relay-ctx 'collaParteRelays
                                     (acons id group-relay group-alist)))
         (unless has-staff-relay
           (ly:context-set-property! ctx 'collaParteRelays
                                     (acons id staff-relay staff-alist)))))

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
              (ly:add-listener (lambda (ev)
                                 (when (any (cut ly:in-event-class? ev <>)
                                            (ly:context-property ctx 'collaParteLocalEvents))
                                 (ly:broadcast default-dispatcher ev)))
                               (ly:context-event-source baby)
                               'StreamEvent))
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
       (let ((staff-relay-alist (ly:context-property ctx 'collaParteRelays)))
         (unless disconnect-until
             (for-each (compose dump cdr) staff-relay-alist))
         (for-each (compose clear! cdr) staff-relay-alist))))))


#(define (colla-parte-grob? grob)
   (let ((cause (event-cause grob)))
     (and cause
          (ly:event-property cause 'relay #f))))

#(define (remove-colla-parte-grobs axis)
   "Remove grobs that were relayed from a different Staff by
Colla_parte_translator from items-worth-living, so that this Staff will be
treated as empty unless it has musical material of its own."
   (and-let* ((items-worth-living (ly:grob-object axis 'items-worth-living #f))
              (worth-living-list (ly:grob-array->list items-worth-living))
              (really-worth-living (filter (compose not colla-parte-grob?)
                                           worth-living-list)))
     (ly:grob-set-object! axis 'items-worth-living
                          (ly:grob-list->grob-array really-worth-living))))

#(define (bottom-ctx-symbol? sym)
   (or (eq? sym 'Bottom)
       (and-let* ((cdef (ly:output-def-lookup
                         (ly:parser-lookup '$defaultlayout)
                         sym #f)))
                 (not (ly:context-def-lookup cdef 'accepts #f)))))

#(define (is-bottom-context? m)
   (and (music-is-of-type? m 'context-specification)
        (bottom-ctx-symbol? (ly:music-property m 'context-type))))

#(define (staff-ctx-symbol? sym)
   (or (eq? sym 'Staff)
       (and-let* ((cdef (ly:output-def-lookup
                         (ly:parser-lookup '$defaultlayout)
                         sym #f))
                  ((member 'Staff
                          (ly:context-def-lookup cdef 'aliases))))
         (ly:context-def-lookup cdef 'accepts #f))))

#(define (is-staff-context? m)
   (and (music-is-of-type? m 'context-specification)
        (staff-ctx-symbol? (ly:music-property m 'context-type))))

#(define (accum-bottom-ctxs this-ctx id-ops-alist)
   "Use the 'context-id and 'property-operations of a context-specced-music
as the key and value to update an alist summarizing context ids and property-ops.
For use with fold."
   (let* ((this-id (ly:music-property this-ctx 'context-id))
          (this-ops (ly:music-property this-ctx 'property-operations))
          (same-id (assoc-get this-id id-ops-alist)))
     (cond
      ((null? this-id) id-ops-alist)
      ((equal? "" this-id) id-ops-alist)
      ((not same-id) (acons this-id this-ops id-ops-alist))
      ((equal? same-id this-ops) id-ops-alist)
      (else (assoc-set! id-ops-alist this-id
                        (lset-union equal? this-ops same-id))))))

#(define (setup-source-staff staff)
   "Set up this staff to be the source of colla parte material for its group.
Parse its music expression to identify the id and mods of every bottom context,
so that contexts with appropriate ids can be kept alive in the client staves."
   (and-let* ((ops (ly:music-property staff 'property-operations))
              ((member '(assign collaParteRelays #t) ops))
              (ops-setup (cons `(consists ,Colla_parte_source_translator)
                               ops))
              (staff-music (ly:music-property staff 'element))
              (bottom-ctxs (extract-music staff-music is-bottom-context?)))
     (ly:music-set-property! staff 'property-operations ops-setup)
     bottom-ctxs))

#(define (voice-defaults id-ops-pair)
   "Make sure contexts with numerical ids have corresponding voice settings."
   (let ((id-num (car id-ops-pair)))
     (if (string? id-num)
         (cons (ly:get-context-mods
                (context-mod-from-music
                 (make-voice-props-set (string->number id-num))))
               (cdr id-ops-pair))
         (cdr id-ops-pair))))

#(define ((setup-client-staff bottom-info-alist) staff)
   "Set up this staff to receive and print musical material from a colla parte
source staff whenever this staff does not have musical material of its own.
Wrap the music expression belonging to this staff with skips to keep alive all
context ids used by the colla parte source."
   (and-let* ((ops (ly:music-property staff 'property-operations))
              ((not (member '(assign collaParteRelays #t) ops)))
              ((not (member '(assign collaParteRelays #f) ops)))
              (ops-setup (cons* `(consists ,Colla_parte_translator)
                                ;; Note that the same context prop is used for
                                ;; the Staff level <delay-dispatcher> alist and
                                ;; the Group level dispatcher alist. So it needs
                                ;; to be initialized to '() at Staff level
                                '(assign collaParteRelays ())
                                `(push VerticalAxisGroup
                                       ,remove-colla-parte-grobs
                                       before-line-breaking)
                                ;; we want to send all events from implicitly
                                ;; created Voices instead to the canonical
                                ;; "default" Voice so that events belonging to
                                ;; this staff occur in the same Voice as events
                                ;; relayed from the colla parte source. setting
                                ;; default-child to Devnull for contexts that
                                ;; simply send their events onward to a different
                                ;; Voice avoids the need to kill grobs and makes
                                ;; spanner bounds work without warnings
                                '(default-child "Devnull")
                                ;; Put the existing context mods last so user
                                ;; can overwrite the defaults
                                ops))
              ;; before changing this context's default-child, check what it was
              ;; and use that context type to explicitly instantiate and keep
              ;; alive all the Voices with ids collected from source staff music
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
              ;; the default voice gets the original music expression while the
              ;; rest get skip, since we want to keep but not duplicate
              ;; global material such as time signatures and keys
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
   "Set up these staves to copy the music from a source staff whenever they do
not have musical material of their own. Copied music will be ignored when
deciding whether to hide empty staves. The source staff is designated by:
\\with {
  \\collaParteSource
}
Staves with collaParteRelays = ##f will be ignored by collaParte setup."
   (let* ((staves (extract-music m is-staff-context?))
          (source-voices (concatenate (filter-map setup-source-staff staves)))
          (bottom-info (fold accum-bottom-ctxs '() source-voices)))
     (when (music-is-of-type? m 'context-specification)
       ;; collaParte staves look for the first parent where collaParteRelays
       ;; is defined and use that context as the home for relay dispatchers
       (ly:music-set-property! m 'property-operations
                               (cons '(assign collaParteRelays ())
                                     (ly:music-property m 'property-operations))))
     (for-each (setup-client-staff bottom-info) staves)
     m))

collaParteSource = \with {
  collaParteRelays = ##t
}
