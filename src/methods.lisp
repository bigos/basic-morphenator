(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defmethod init-gui ((model basic-morphenator-model))
  (let ((model-world (world model))
        (first-box (make-instance 'morph)))
    (add-submorph model-world first-box)))

(defmethod add-submorph ((morph morph) (submorph morph) )
  (setf (owner submorph) morph)
  (setf (submorphs morph) (if (submorphs morph)
                              ;; then add morph to submorph collection
                              (append (submorphs morph) (list submorph))
                              ;; else create a collection with one submorph
                              (list submorph))))

;;; ============= render =======================================================
(defmethod render ((morph world))
  ;; absolute coordinates

  ;; actual morph
  (gui-color:set-rgba "red")
  (cairo:rectangle   (x (coordinates-absolute node))
                     (y (coordinates-absolute node))
                     (width node)
                     (height node))
  (cairo:fill-path)

  (loop for m in (submorphs morph) do (render m)))
