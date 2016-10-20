(require 'evil-textobj-anyblock)

(define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)

(defmacro evil-textobj-anyblock-with (in &rest body)
  "This is `lispy-with' modified for evil-textobj-anyblock.
Note that | is considered to be \"on\" a character, meaning that it is included
in a visual selection. ~ on the other hand is not considered to be on a
character, so when it represents the region end, the character after it is not
considered as part of the region."
  (declare (indent 1))
  `(let ((temp-buffer (generate-new-buffer " *temp*")))
     (save-window-excursion
       (unwind-protect
           (progn
             (switch-to-buffer temp-buffer)
             (emacs-lisp-mode)
             (transient-mark-mode 1)
             (evil-mode)
             (insert ,in)
             (goto-char (point-min))
             (when (search-forward "~" nil t)
               (backward-delete-char 1)
               (set-mark (point)))
             (goto-char (point-max))
             (search-backward "|")
             (delete-char 1)
             (setq current-prefix-arg nil)
             ,@(mapcar (lambda (x)
                         (if (or (stringp x)
                                 (and (listp x)
                                      (eq (car x) 'kbd)))
                             `(evil-execute-macro 1 ,x)
                           x))
                       body)
             (insert "|")
             (when (region-active-p)
               (exchange-point-and-mark)
               ;; because not considering ~ as "on" like |
               (when (= (point) (region-end))
                 (forward-char))
               (insert "~"))
             (buffer-substring-no-properties
              (point-min)
              (point-max)))
         (and (buffer-name temp-buffer)
              (kill-buffer temp-buffer))))))

(ert-deftest evil-textobj-anyblock-inner-block ()
  (should (string= (evil-textobj-anyblock-with "(|foo) (bar)" "dib")
                   "(|) (bar)"))
  (should (string= (evil-textobj-anyblock-with "(|foo) (bar)" "vib")
                   "(~fo|o) (bar)"))
  ;; seeking
  (should (string= (evil-textobj-anyblock-with "|foo (bar)" "dib")
                   "foo (|)"))
  (should (string= (evil-textobj-anyblock-with "|foo (bar)" "vib")
                   "foo (~ba|r)"))
  ;; expansion
  (should (string= (evil-textobj-anyblock-with "\"foo ((a b) [|c d])\"" "vib")
                   "\"foo ((a b) [~c |d])\""))
  (should (string= (evil-textobj-anyblock-with "\"foo ((a b) [~c |d])\"" "ib")
                   "\"foo (~(a b) [c d|])\""))
  (should (string= (evil-textobj-anyblock-with "\"foo (~(a b) [c d|])\"" "ib")
                   "\"~foo ((a b) [c d]|)\"")))

(ert-deftest evil-textobj-anyblock-a-block ()
  (should (string= (evil-textobj-anyblock-with "(|foo) (bar)" "dab")
                   "| (bar)"))
  (should (string= (evil-textobj-anyblock-with "(|foo) (bar)" "vab")
                   "~(foo|) (bar)"))
  ;; seeking
  (should (string= (evil-textobj-anyblock-with "|foo (bar)" "dab")
                   "foo| "))             ; depends on evil-move-cursor-back t
  (should (string= (evil-textobj-anyblock-with "|foo (bar)" "vab")
                   "foo ~(bar|)"))
  ;; expansion
  (should (string= (evil-textobj-anyblock-with "\"foo ((a b) [|c d])\"" "vab")
                   "\"foo ((a b) ~[c d|])\""))
  (should (string= (evil-textobj-anyblock-with "\"foo ((a b) ~[c d|])\"" "ab")
                   "\"foo ~((a b) [c d]|)\""))
  (should (string= (evil-textobj-anyblock-with "\"foo ~((a b) [c d]|)\"" "ab")
                   "~\"foo ((a b) [c d])|\"")))
