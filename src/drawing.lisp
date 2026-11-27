(declaim (optimize (speed 1) (safety 3) (debug 3)))
(in-package #:basic-morphenator)

(defun draw-box-rect (x y w h color)
  (gui-color:set-rgba (or color "yellow"))
  (cairo:rectangle x y w h)
  (cairo:fill-path))

(defun draw-morph-box (morph &optional color)
  (let ((ext (extent morph))
        (fill-color (if (and (null color) (slot-boundp morph 'color))
                        (color morph)
                        color)))
    (when (and ext fill-color)
      (draw-box-rect 0 0 (x ext) (y ext) fill-color))))

(defun draw-morph-label (morph text &key color)
  (render-prepare-to-show-text morph)
  (let ((resolved-color (or color (nth-value 4 (resolve-font-spec morph)))))
    (gui-color:set-rgba resolved-color)
    (cairo:show-text (or text ""))))

(defun make-layout-spec (&key fixed-width fixed-height
                           proportional-width proportional-height
                           (off-axis-edge-width 0))
  (make-instance 'layout-spec
                 :fixed-width fixed-width
                 :fixed-height fixed-height
                 :proportional-width proportional-width
                 :proportional-height proportional-height
                 :off-axis-edge-width off-axis-edge-width))
