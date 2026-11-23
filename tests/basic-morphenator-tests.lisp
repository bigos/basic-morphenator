(declaim (optimize (speed 1) (safety 3) (debug 3)))

;; (ql:quickload :basic-morphenator/tests)
(in-package #:basic-morphenator-test)
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
      (setf bm::*basic-morphenator-model* nil
            bm::*environment* nil))))

;;; ======================= unit tests =========================================
(test printable-input-validation
  "Verify printable-input-p filters control keys and non-printable characters."
  (is-true  (bm::printable-input-p "a" nil))
  (is-false (bm::printable-input-p "a" t))
  (is-false (bm::printable-input-p "" nil))
  (is-false (bm::printable-input-p nil nil))
  (is-false (bm::printable-input-p (string #\Escape) nil))
  (is-false (bm::printable-input-p (string #\Return) nil))
  (is-false (bm::printable-input-p (string #\Rubout) nil)))

(test text-morph-editing-helpers
  "Verify delete-char-at-cursor and clear-text-morph."
  (with-fixture with-test-window ()
    (let ((tm (bm::active-text-morph)))
      (when tm
        (setf (bm::widget-text tm) "abcd"
              (bm::cursor-pos tm) 1)
        (bm::delete-char-at-cursor tm)
        (is (string= "acd" (bm::widget-text tm)))
        (bm::clear-text-morph tm)
        (is (string= "" (bm::widget-text tm)))
        (is (= 0 (bm::cursor-pos tm)))))))

;;; ==================== integration / event tests =============================
(test process-event-key-and-shortcuts
  "Verify typing, Return greeting, and Alt+c clearing through process-event."
  (with-fixture with-test-window ()
    (let ((model (bm::the-model))
          (tm (bm::active-text-morph)))
      (process-event win :key-pressed '("H" "H" 0 nil))
      (process-event win :key-pressed '("i" "i" 0 nil))
      (if tm
          (is (string= "Hi" (bm::widget-text tm)))
          (is (string= "Hi" (bm::entered model))))

      ;; Trigger greeting via Return
      (process-event win :key-pressed '("" "Return" 0 nil))
      (is (string= (if tm (bm::widget-text tm) (bm::entered model))
                   (bm::greeted model)))

      ;; Clear via Alt+c
      (process-event win :key-pressed '("c" "c" 0 (:ALT)))
      (is (string= "" (bm::greeted model)))
      (if tm
          (is (string= "" (bm::widget-text tm)))
          (is (string= "" (bm::entered model)))))))

(test process-event-resize
  "Verify :resize updates window dimensions and *window-size*."
  (with-fixture with-test-window ()
    (process-event win :resize '(800 600))
    (is (equal '(800 . 600) bm::*window-size*))))

;;; ======================= layout & scroll unit tests =========================
(test layout-horizontal-proportional-and-fixed
  "Verify layout-submorphs divides remaining space among proportional children."
  (let* ((row (make-instance 'bm::layout-morph
                             :direction :horizontal
                             :separation 10
                             :extent (make-instance 'bm::extent :x 320 :y 40)))
         (fixed-left (make-instance 'bm::label-morph
                                    :extent (make-instance 'bm::extent :x 60 :y 30)
                                    :layout-spec (bm::make-layout-spec :fixed-width 60
                                                                       :fixed-height 30)))
         (flex-mid (make-instance 'bm::text-model-morph
                                  :extent (make-instance 'bm::extent :x 10 :y 30)
                                  :layout-spec (bm::make-layout-spec :proportional-width 1.0
                                                                     :proportional-height 1.0)))
         (fixed-right (make-instance 'bm::pluggable-button-morph
                                     :extent (make-instance 'bm::extent :x 40 :y 30)
                                     :layout-spec (bm::make-layout-spec :fixed-width 40
                                                                        :fixed-height 30))))
    (bm::add-morph row fixed-left)
    (bm::add-morph row flex-mid)
    (bm::add-morph row fixed-right)
    ;; Available width = 320 - (2 gaps * 10) = 300; fixed = 60 + 40 = 100 -> flex = 200
    (is (= 60  (bm::x (bm::extent fixed-left))))
    (is (= 200 (bm::x (bm::extent flex-mid))))
    (is (= 40  (bm::x (bm::extent flex-right))))
    (is (= 40  (bm::y (bm::extent flex-mid))))
    ;; Check horizontal positions: 0, 70, 280
    (is (= 0   (bm::x (bm::location fixed-left))))
    (is (= 70  (bm::x (bm::location flex-mid))))
    (is (= 280 (bm::x (bm::location fixed-right))))))

(test cursor-scroll-visibility
  "Verify ensure-cursor-visible scrolls right when past inner-w and left when behind scroll-x."
  (let ((tm (make-instance 'bm::text-model-morph :scroll-x 0)))
    ;; Cursor at 150px inside a 100px inner viewport -> scroll-x becomes 50
    (bm::ensure-cursor-visible tm 150 100)
    (is (= 50 (bm::scroll-x tm)))
    ;; Cursor moves slightly left to 80px (still inside) -> scroll-x stays 50
    (bm::ensure-cursor-visible tm 80 100)
    (is (= 50 (bm::scroll-x tm)))
    ;; Cursor moves to 20px (< 50) -> scroll-x snaps back to 20
    (bm::ensure-cursor-visible tm 20 100)
    (is (= 20 (bm::scroll-x tm)))))

;;; ======================= text editing unit tests ============================
(test printable-input-validation
  "Verify printable-input-p filters control keys and non-printable characters."
  (is-true  (bm::printable-input-p "a" nil))
  (is-false (bm::printable-input-p "a" t))
  (is-false (bm::printable-input-p "" nil))
  (is-false (bm::printable-input-p nil nil))
  (is-false (bm::printable-input-p (string #\Escape) nil))
  (is-false (bm::printable-input-p (string #\Return) nil))
  (is-false (bm::printable-input-p (string #\Rubout) nil)))

(test text-morph-editing-helpers
  "Verify insert, selection deletion, delete-char-at-cursor, and clear-text-morph."
  (with-fixture with-test-window ()
    (let ((tm (bm::active-text-morph)))
      (is-true (not (null tm)))
      (bm::insert-at-cursor tm "abcdef")
      (is (string= "abcdef" (bm::widget-text tm)))
      (is (= 6 (bm::cursor-pos tm)))

      ;; Move cursor to index 1 and delete 'b'
      (setf (bm::cursor-pos tm) 1)
      (bm::delete-char-at-cursor tm)
      (is (string= "acdef" (bm::widget-text tm)))

      ;; Select "cd" (indices 1..3) and replace with "X"
      (setf (bm::selection-start tm) 1
            (bm::cursor-pos tm) 3)
      (bm::insert-at-cursor tm "X")
      (is (string= "aXef" (bm::widget-text tm)))
      (is (null (bm::selection-start tm)))

      ;; Clear resets text, cursor, and scroll
      (setf (bm::scroll-x tm) 42)
      (bm::clear-text-morph tm)
      (is (string= "" (bm::widget-text tm)))
      (is (= 0 (bm::cursor-pos tm)))
      (is (= 0 (bm::scroll-x tm))))))

;;; ==================== integration / event tests =============================
(test process-event-typing-greeting-and-shortcuts
  "Verify typing, Ctrl+A selection, Return greeting, and Alt+c clearing through process-event."
  (with-fixture with-test-window ()
    (let ((model (bm::the-model))
          (tm (bm::active-text-morph)))
      (process-event win :key-pressed '("J" "J" 0 nil))
      (process-event win :key-pressed '("a" "a" 0 nil))
      (process-event win :key-pressed '("c" "c" 0 nil))
      (process-event win :key-pressed '("e" "e" 0 nil))
      (process-event win :key-pressed '("k" "k" 0 nil))
      (is (string= "Jacek" (bm::widget-text tm)))

      ;; Ctrl+A selects all text
      (process-event win :key-pressed '("a" "a" 0 (:CONTROL)))
      (is (= 0 (bm::selection-start tm)))
      (is (= 5 (bm::cursor-pos tm)))

      ;; Return triggers greet-me
      (process-event win :key-pressed '("" "Return" 0 nil))
      (is (string= "Jacek" (bm::greeted model)))

      ;; Alt+c clears both greeted and text-morph
      (process-event win :key-pressed '("c" "c" 0 (:ALT)))
      (is (string= "" (bm::greeted model)))
      (is (string= "" (bm::widget-text tm))))))

(test process-event-resize-and-button-click
  "Verify :resize updates world dimensions and clicking Greet updates greeted."
  (with-fixture with-test-window ()
    (let ((model (bm::the-model))
          (tm (bm::active-text-morph)))
      (process-event win :resize '(800 500))
      (is (equal '(800 . 500) bm::*window-size*))
      (is (= 800 (bm::x (bm::extent (bm::current-world)))))
      (is (= 500 (bm::y (bm::extent (bm::current-world)))))

      ;; Type name and click the Greet button (located at right side of top row: x=760, y=15)
      (bm::insert-at-cursor tm "Lisp")
      (process-event win :pressed '(1 760 15))
      (process-event win :released '(1 760 15))
      (is (string= "Lisp" (bm::greeted model))))))
