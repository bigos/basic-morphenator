(defpackage #:basic-morphenator
  (:use #:cl)
  (:import-from :defclass-std
                defclass/std)
  (:import-from :boxes
                width
                height)
  (:local-nicknames (#:bm #:basic-morphenator))
  (:export
   ;; Entry points & top-level hooks
   #:main
   #:experiment
   #:draw-window
   #:process-event
   #:menu-bar
   #:handle-key-pressed
   #:handle-menu-action
   #:handle-window-resize
   #:dispatch-mouse-button

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
   #:morph
   #:placed-morph
   #:boxed-morph
   #:hand-morph
   #:paste-up-morph
   #:world-morph
   #:layout-adjusting-morph
   #:window-edge-adjusting-morph
   #:layout-morph
   #:layout-spec
   #:label-morph
   #:pluggable-morph
   #:pluggable-button-morph
   #:pluggable-scroll-pane
   #:text-model-morph
   #:location
   #:extent
   #:basic-morphenator-model
   #:basic-morphenator-window
   #:add-morph
   #:layout-submorphs
   #:morphs-at
   #:contains-point-p
   #:process-mouse-over
   #:handle-event-mouse-button
   #:update-text-drag-selection
   #:finish-text-selection
   #:render
   #:render-world
   #:resize
   #:widget-text
   #:x
   #:y
   #:move-text-cursor

   ;; Text & input helpers used in tests/code
   #:printable-input-p
   #:active-text-morph
   #:find-text-morph
   #:delete-char-at-cursor
   #:delete-before-cursor
   #:clear-text-morph
   #:insert-at-cursor
   #:cursor-pos
   #:selection-start
   #:is-selecting
   #:scroll-x
   #:entered
   #:greeted
   #:world))
