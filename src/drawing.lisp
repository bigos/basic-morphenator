(declaim (optimize (speed 1) (safety 3) (debug 3)))
(in-package #:basic-morphenator)

(defmethod render-prepare-to-show-text ((morph placed-morph))
  (multiple-value-bind (family size slant weight color)
      (resolve-font-spec morph)
    (declare (ignore color))
    (cairo:select-font-face family slant weight)
    (cairo:set-font-size size)))

(defun text-baseline-y (morph &key (pad-y 4))
  "Compute y coordinate for text baseline inside MORPH using its resolved font size."
  (let* ((size (nth-value 1 (resolve-font-spec morph)))
         (font-size (if (and (numberp size) (> size 0))
                        size
                        *default-font-size*)))
    (+ pad-y (round (* font-size 0.9)))))

(defun draw-morph-label (morph text &key color)
  (let* ((ext (extent morph))
         (w (if ext (x ext) 0))
         (h (if ext (y ext) 0))
         (txt (or text ""))
         (pad-x 8)
         (pad-y 4)
         (resolved-color (or color
                             (nth-value 4 (resolve-font-spec morph))
                             *default-text-color*)))
    (cairo:save)
    (cairo:rectangle 0 0 w h)
    (cairo:clip)
    (render-prepare-to-show-text morph)
    (gui-color:set-rgba resolved-color)
    (cairo:move-to pad-x (text-baseline-y morph :pad-y pad-y))
    (cairo:show-text txt)
    (cairo:restore)))

(defun draw-box-rect (x y w h color &key fill-color border-color (border-width 1.0))
  "Draw a rectangle at (X, Y) with width W and height H in the active Cairo context."
  (let ((effective-fill (or fill-color color)))
    (cairo:save)
    (cairo:rectangle x y w h)
    (when effective-fill
      (gui-color:set-rgba effective-fill)
      (if border-color
          (cairo:fill-preserve)
          (cairo:fill-path)))
    (when border-color
      (gui-color:set-rgba border-color)
      (cairo:set-line-width border-width)
      (cairo:stroke))
    (cairo:restore)))

(defun draw-morph-box (morph &key color border-color (border-width 1.0))
  "Draw MORPH's bounding box using its extent and resolved color."
  (let* ((ext (extent morph))
         (w (if ext (x ext) 0))
         (h (if ext (y ext) 0))
         (box-color (or color (color morph))))
    (when (and box-color (> w 0) (> h 0))
      (draw-box-rect 0 0 w h box-color
                     :border-color border-color
                     :border-width border-width))))
