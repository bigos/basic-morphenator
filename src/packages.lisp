(defpackage #:basic-morphenator
  (:use #:cl)
  (:import-from :defclass-std defclass/std)
  (:import-from :boxes width height)
  (:local-nicknames (#:bm #:basic-morphenator))
  (:export
   ;; Special variables & model accessors
   #:*basic-morphenator-model*
   #:*environment*
   #:*window-size*
   #:the-model
   #:the-handler
   #:current-world
   #:morph-world
   #:morph-target-world
   #:greet-me

   ;; Morph classes & tree/layout/event helpers
   #:morph #:placed-morph #:boxed-morph #:hand-morph #:paste-up-morph
   #:world-morph #:layout-adjusting-morph #:window-edge-adjusting-morph
   #:layout-morph #:layout-spec #:label-morph #:pluggable-morph
   #:pluggable-button-morph #:pluggable-scroll-pane #:text-model-morph
   #:location #:extent #:basic-morphenator-model
   #:add-morph #:layout-submorphs #:morphs-at #:contains-point-p
   #:process-mouse-over #:handle-event-mouse-button
   #:update-text-drag-selection #:finish-text-selection
   #:render #:render-morph #:render-world #:resize #:widget-text
   #:x #:y #:move-text-cursor #:find-the-morph #:find-text-morph
   #:accepts-keyboard-focus-p #:world-keyboard-focus #:set-location-xy
   #:with-deferred-layout #:invalidate-text-metrics
   #:needs-redraw-p #:mark-needs-redraw #:clear-needs-redraw
   #:world-needs-redraw-p #:record-damage-rect #:record-morph-damage
   #:reset-damage-recorder #:padding-x #:padding-y #:tooltip

   ;; Text & input helpers
   #:printable-input-p #:active-text-morph
   #:delete-char-at-cursor #:delete-before-cursor
   #:clear-text-morph #:insert-at-cursor
   #:cursor-pos #:selection-start #:is-selecting #:scroll-x
   #:entered #:greeted #:world))
