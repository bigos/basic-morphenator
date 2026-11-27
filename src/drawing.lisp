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
  (multiple-value-bind (family size slant weight default-color)
      (resolve-font-spec morph)
    (declare (ignore family slant weight))
    (let* ((resolved-color (or color default-color))
           (str (or text ""))
           (ext (and (typep morph 'boxed-morph) (extent morph)))
           (text-w (if (and (boundp 'cairo:*context*) cairo:*context*)
                       (nth-value 4 (cairo:text-extents str))
                       0))
           (offset-x (if (and ext (plusp (x ext)))
                         (max 0 (/ (- (x ext) text-w) 2.0))
                         0)))
      (cairo:move-to offset-x (font-baseline-y size))
      (gui-color:set-rgba resolved-color)
      (cairo:show-text str))))

(defun make-layout-spec (&key fixed-width fixed-height
                           proportional-width proportional-height
                           (off-axis-edge-width 0))
  (make-instance 'layout-spec
                 :fixed-width fixed-width
                 :fixed-height fixed-height
                 :proportional-width proportional-width
                 :proportional-height proportional-height
                 :off-axis-edge-width off-axis-edge-width))
