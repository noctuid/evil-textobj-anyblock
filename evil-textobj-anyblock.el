;;; evil-textobj-anyblock.el --- Textobject for the closest user-defined blocks.

;; Author: Lit Wakefield <noct@openmailbox.org>
;; URL: https://github.com/noctuid/evil-textobj-anyblock
;; Keywords: evil
;; Package-Requires: ((cl-lib "0.5") (evil "1.1.0")
;; Version: 0.1

;;; Commentary:
;; This package is a port of vim-textobj-anyblock. It gives text objects for the
;; closest block of those defined in the evil-anyblock-blocks alist. By default
;; it includes (), {}, [], <>, '', "", ``, and “”. This is convenient for
;; operating on the closest block without having to choose between typing
;; something like i{ or i<. This package allows for the list of blocks to be
;; changed. They can be more complicated regexps. A simple expand-region like
;; functionality is also provided when in visual mode, though this is not a
;; primary focus of the plugin and does not exist in vim-textobj-anyblock.

;; The required version of evil is based on the last change I could find to
;; evil-select-paren, but the newest version of evil is probably preferable.

;; For more information see the README in the github repo.

;;; Code:
(require 'cl-lib)
(require 'evil)

(defgroup evil-anyblock nil
  "Gives text objects for selecting the closest block from any in a user-defined
alist."
  :group 'evil
  :prefix 'evil-anyblock-)

(defcustom evil-anyblock-blocks
  '(("(" . ")")
    ("{" . "}")
    ("\\[" . "\\]")
    ("<" . ">")
    ("'" . "'")
    ("\"" . "\"")
    ("`" . "`")
    ("“" . "”"))
  "Alist containing regexp blocks to look for."
  :group 'evil-anyblock
  :type '(alist
          :key-type regexp
          :value-type rexegp))

(defcustom evil-anyblock-boundary
  5000
  "Max range from point to search backwards for open block."
  :group 'evil-anyblock
  :type 'number)

(defun evil-anyblock--distance-from-point (open-block bound-pos)
  "If OPEN-BLOCK is matched within the BOUND-POS, returns a plist for
OPEN-BLOCK, the corresponding close block, and the distance to the match."
  (save-excursion
    (let ((case-fold-search nil)
          (match-pos
           (progn
             ;; potentially expand selection if more than one character selected
             (if (and
                  (equal evil-state 'visual)
                  (not (equal (marker-position evil-visual-beginning)
                              (- (marker-position evil-visual-end) 1))))
                 (progn
                   (goto-char evil-visual-beginning)
                   (evil-backward-char))
               (right-char))
             (save-excursion
               (re-search-backward open-block bound-pos t)))))
      (when match-pos
        (list ':open-block open-block
              ':close-block (cdr (assoc open-block evil-anyblock-blocks))
              ':distance (- match-pos (point)))))))

(defun evil-anyblock--find-closest-block ()
  "Finds the closest open block and converts the block to a character when
possible (so that evil-up-paren will be used instead of evil-up-block) and
returns a list containing the open block followed by the close block."
  (let* ((closest-block-info
          (car
           (sort
            (cl-loop for (open-block . _) in evil-anyblock-blocks
                     when (evil-anyblock--distance-from-point
                           open-block
                           (- (point) evil-anyblock-boundary))
                     collect it)
            ;; > since all distances negative
            (lambda (x y) (> (plist-get x ':distance)
                             (plist-get y ':distance))))))
         (open-block (plist-get closest-block-info ':open-block))
         (close-block (plist-get closest-block-info ':close-block)))
    ;; if in visual-mode, move outside of current selection
    (save-excursion
      (when (equal evil-state 'visual)
        (goto-char evil-visual-beginning)
        (evil-backward-char))
      (if (= 1 (length open-block) (length close-block))
          ;; evil-select-paren has undesirable behaviour for strings (evil-up-block)
          (list (string-to-char open-block) (string-to-char close-block))
        (list open-block close-block)))))

(defun evil-anyblock--make-textobj (outerp)
  "Helper function to prevent need for duplicating the same function for both
the inner and outer textobjects. If OUTERP is true, the text object will be an
outer text object. Otherwise it will be an inner one."
  (let* ((blocks (evil-anyblock--find-closest-block))
         (open-block (car blocks))
         (close-block (cadr blocks)))
    (if (or (equal open-block ?')
            (equal open-block ?\")
            (equal open-block ?`))
        (evil-select-quote open-block beg end type count outerp)
      (evil-select-paren open-block close-block beg end type count outerp))))

;; if fails with no surrounding delimeters found, seek forward
(evil-define-text-object evil-anyblock-inner-block (count &optional beg end type)
  "Select the closest inner anyblock block."
  (evil-anyblock--make-textobj nil))

(evil-define-text-object evil-anyblock-a-block (count &optional beg end type)
  "Select the closest outer anyblock block."
  (evil-anyblock--make-textobj t))

(provide 'evil-anyblock)
;;; evil-textobj-anyblock.el ends here
