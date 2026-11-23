(declaim (optimize (speed 1) (safety 3) (debug 3)))

;; (ql:quickload :basic-morphenator/tests)
(in-package #:basic-morphenator-test)
;; (fiveam:run-all-tests)
;; (run! 'basic-equalities)

;; (setf 5am:*on-error* :debug)

;;; =========================== fixtures =======================================
(def-fixture prepare-variables (n)
  (let ((a n)
        (b 2)
        (c 3)
        (expected 6))
    ;; body
    (&body)
    ;; teardown code
    (setf a nil
          b nil
          c nil)))

;;; ======================= tests ==============================================
(fiveam:test basic-equalities
  "basic equalities"
  (with-fixture prepare-variables (1)
    (is (eq a 1))
    (is (eq b 2))
    (is (eq c 3))
    (is (eq (+ a b c) expected))))
;;; ================================================= more tests ===============

(def-suite basic-morphenator-suite
  :description "Tests for basic-morphenator event and key handling.")

(in-suite basic-morphenator-suite)

;;; =========================== fixtures =======================================
(def-fixture with-test-window ()
  (let ((win (bm:main :testing t)))
    (unwind-protect
         (&body)
      (setf bm:*basic-morphenator-model* nil
            bm:*environment* nil))))

;;; ======================= unit tests =========================================
(test printable-input-validation
  "Verify printable-input-p filters control keys and non-printable characters."
  (is-true  (bm:printable-input-p "a" nil))
  (is-false (bm:printable-input-p "a" t))
  (is-false (bm:printable-input-p "" nil))
  (is-false (bm:printable-input-p nil nil))
  (is-false (bm:printable-input-p (string #\Escape) nil))
  (is-false (bm:printable-input-p (string #\Return) nil))
  (is-false (bm:printable-input-p (string #\Rubout) nil)))

(test text-morph-editing-helpers
  "Verify delete-char-at-cursor and clear-text-morph."
  (with-fixture with-test-window ()
    (let ((tm (bm:active-text-morph)))
      (when tm
        (setf (bm:widget-text tm) "abcd"
              (bm:cursor-pos tm) 1)
        (bm:delete-char-at-cursor tm)
        (is (string= "acd" (bm:widget-text tm)))
        (bm:clear-text-morph tm)
        (is (string= "" (bm:widget-text tm)))
        (is (= 0 (bm:cursor-pos tm)))))))

;;; ==================== integration / event tests =============================
(test process-event-key-and-shortcuts
  "Verify typing, Return greeting, and Alt+c clearing through process-event."
  (with-fixture with-test-window ()
    (let ((model (bm:the-model))
          (tm (bm:active-text-morph)))
      (process-event win :key-pressed '("H" "H" 0 nil))
      (process-event win :key-pressed '("i" "i" 0 nil))
      (if tm
          (is (string= "Hi" (bm:widget-text tm)))
          (is (string= "Hi" (bm:entered model))))

      ;; Trigger greeting via Return
      (process-event win :key-pressed '("" "Return" 0 nil))
      (is (string= (if tm (bm:widget-text tm) (bm:entered model))
                   (bm:greeted model)))

      ;; Clear via Alt+c
      (process-event win :key-pressed '("c" "c" 0 (:ALT)))
      (is (string= "" (bm:greeted model)))
      (if tm
          (is (string= "" (bm:widget-text tm)))
          (is (string= "" (bm:entered model)))))))

(test process-event-resize
  "Verify :resize updates window dimensions and *window-size*."
  (with-fixture with-test-window ()
    (process-event win :resize '(800 600))
    (is (equal '(800 . 600) bm:*window-size*))))

;;; ======================= layout & hit-testing tests =======================
(test layout-submorphs-horizontal-and-vertical
  "Verify horizontal and vertical layout sizing and gap placement without overflow."
  (let* ((container (make-instance 'bm:layout-morph
                                   :direction :horizontal
                                   :separation 10
                                   :extent (make-instance 'bm:extent :x 320 :y 40)))
         (left (make-instance 'bm:boxed-morph
                              :color "red"
                              :extent (make-instance 'bm:extent :x 60 :y 40)
                              :layout-spec (make-instance 'bm:layout-spec :fixed-width 60)))
         (right (make-instance 'bm:boxed-morph
                               :color "blue"
                               :extent (make-instance 'bm:extent :x 50 :y 40)
                               :layout-spec (make-instance 'bm:layout-spec :proportional-width 1.0))))
    (bm:add-morph container left)
    (bm:add-morph container right)
    (is (= 0 (bm:x (bm:location left))))
    (is (= 60 (bm:x (bm:extent left))))
    (is (= 70 (bm:x (bm:location right))))
    (is (= 250 (bm:x (bm:extent right))))
    (is (<= (+ (bm:x (bm:location right)) (bm:x (bm:extent right)))
            (bm:x (bm:extent container))))))

(test morphs-at-hit-testing-chain
  "Verify morphs-at returns outermost-to-innermost chain and respects bounds."
  (with-fixture with-test-window ()
    (let* ((world (bm:current-world))
           (chain-inside (bm:morphs-at world 15 15))
           (chain-outside (bm:morphs-at world 9999 9999)))
      (is-true chain-inside)
      (is (eq world (first chain-inside)))
      (is (null chain-outside)))))

;;; ======================= widget & cursor/selection tests ==================
(test button-press-triggers-greet-me
  "Pressing a pluggable-button-morph invokes its action-selector and updates greeted."
  (with-fixture with-test-window ()
    (let* ((model (bm:the-model))
           (clicked nil)
           (btn (make-instance 'bm:pluggable-button-morph
                               :color "green"
                               :extent (make-instance 'bm:extent :x 100 :y 30)
                               :label "Test"
                               :action-selector (lambda ()
                                                  (setf clicked t)
                                                  (bm:greet-me model)))))
      (setf (bm:entered model) "Ada")
      (bm:handle-event-mouse-button btn :pressed 1 10 10)
      (is-true clicked)
      (is (string= "Ada" (bm:greeted model))))))

(test text-cursor-selection-and-editing
  "Verify cursor movement, shift selection, insertion, and backspace deletion."
  (let ((tm (make-instance 'bm:text-model-morph :text "hello")))
    (setf (bm:cursor-pos tm) 5)
    (bm:move-text-cursor tm 2 t)
    (is (= 2 (bm:cursor-pos tm)))
    (is (= 5 (bm:selection-start tm)))
    (bm:insert-at-cursor tm "XY")
    (is (string= "heXY" (bm:widget-text tm)))
    (is (= 4 (bm:cursor-pos tm)))
    (is (null (bm:selection-start tm)))
    (bm:delete-before-cursor tm)
    (is (string= "heX" (bm:widget-text tm)))
    (is (= 3 (bm:cursor-pos tm)))))
