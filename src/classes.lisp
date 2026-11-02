(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defclass/std basic-morphenator-model (boxes:model)
  ((world :std (make-instance 'world-morph))))

(defclass/std location ()
  ((absolute-x)
   (absolute-y)
   (x)
   (y)))

(defclass/std extent ()
  ((x :documentation "x is the width of the widget")
   (y :documentation "y is the height of the widget")))

;;; kernel - objects -----------------------------------------------------------
(defclass/std message-send ()
  ((receiver)
   (selector)
   (arguments)))

;;; morphic - kernel -----------------------------------------------------------
(defclass/std drop-files-action ()
  ((drop-files-event)
   (selected-file-entry)
   (should-ask-to-stop)
   (stop-here-block)))

(defclass/std morph ()
  ((owner)
   (submorphs)
   (properties)
   (id)
   (private-display-bounds)
   (last-morph-id :allocation :class)))

(defclass/std placed-morph (morph)
  ((location)
   (layout-spec)))

(defclass/std boxed-morph (placed-morph)
  ((extent)                             ;width and height
   (color)
   (border-width)
   (border-color)))

(defclass/std hand-morph (boxed-morph)
  ((mouse-focus)
   (keyboard-focus)
   (mouse-click-state)
   (mouse-over-handler)
   (last-mouse-event)
   (has-changed)
   (saved-patch)
   (last-event-buffer)
   (last-key-down-value)
   (last-mouse-event-time)
   (prev-full-bounds)
   (grab-morph-data)
   (last-position)))

(defclass/std paste-up-morph (boxed-morph)
  ((background-image)
   (background-image-data)))

(defclass/std world-morph (paste-up-morph)
  ((active-hand)
   (canvas)
   (damage-recorder)
   (step-list)
   (last-cycle-time)
   (alarms)
   (last-alarm-time)
   (deferred-ui-messages)
   (drawing-failing-morphs)
   (wait-delay)
   (pause)
   (last-cycle-had-any-event)
   (taskbar)
   (scale-factor :allocation :class)))

;;; morphic - layouts ----------------------------------------------------------
(defclass/std layout-adjusting-morph (boxed-morph)
  ((hand)))

(defclass/std window-edge-adjusting-morph (layout-adjusting-morph)
  ((selector)
   (cursor-key)))

(defclass/std layout-morph (boxed-morph)
  ((direction)
   (separation)
   (axis-edge-weight)
   (do-adopt-widgets-colour)))

(defclass/std layout-spec ()
  ((morph)
   (fixed-width)
   (fixed-height)
   (off-axis-edge-weidth)
   (proportional-width)
   (proportional-height)))

;;; morphic - events -----------------------------------------------------------
(defclass/std morphic-alarm (message-send)
  ((schedule-time)
   (num-args)))

(defclass/std step-message (morphic-alarm)
  ((step-time)
   (last-evaluation-time)))

(defclass/std morphic-event ()
  ((timestamp)
   (source)))

(defclass/std drop-event (morphic-event)
  ((position2)
   (contents)
   (was-handled)
   (former-owner)
   (former-position)))

(defclass/std drop-files-event (morphic-event)
  ((position2)
   (was-handled)
   (number-of-files)))

(defclass/std user-input-event (morphic-event)
  ((type2)
   (buttons)
   (position2)
   (was-handled)))

(defclass/std keyboard-event (user-input-event)
  ((key-value)))

(defclass/std mouse-event (user-input-event)
  ())

(defclass/std mouse-button-event (mouse-event)
  ((which-button)
   (event-handler)))

(defclass/std mouse-move-event (mouse-event)
  ())

(defclass/std mouse-scroll-event (mouse-event)
  ((direction)
   (event-handler)))

(defclass/std window-event (morphic-event)
  ((action)
   (rectangle)))

(defclass/std mouse-over-handler ()
  ((mouse-over-morphs)
   (entered-morphs)
   (over-morphs)
   (left-morphs)))

;;; morphic - widgets ----------------------------------------------------------
(defclass/std label-morph (boxed-morph)
  ((font)
   (emphasis)
   (contents)))

(defclass/std pluggable-morph (boxed-morph)
  ((model)
   (menu-n-shortcuts-key)))

(defclass/std pluggable-button-morph (pluggable-morph)
  ((label)
   (font)
   (icon)
   (state-selector)
   (enable-selector)
   (action-selector)
   (is-pressed)
   (mouse-is-over)
   (magnified-icon)
   (act-when)
   (round-button-style)
   (icon-name)
   (secondary-action-selector)
   (state-argument)
   (action-argument)
   (mouse-over-color)))

(defclass/std pluggable-scroll-pane (pluggable-morph)
  ((scroll-pane)
   (scroller)
   (h-scroll-bar)
   (hides-scroll-bars)
   (draw-keyboard-focus-indicator)
   (scroller-scale)
   (scroller-radians)
   (the-scroller-extent)))

(defclass/std text-model-morph (pluggable-scroll-pane)
  ())

;;; morphic - composite widgets ------------------------------------------------
