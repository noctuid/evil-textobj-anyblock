# About [![MELPA](http://melpa.org/packages/evil-textobj-anyblock-badge.svg)](http://melpa.org/#/evil-textobj-anyblock)
Evil-textobj-anyblock is port of the vim plugin [vim-textobj-anyblock](https://github.com/rhysd/vim-textobj-anyblock). It gives a text object that will select the closest of `()`, `{}`, `[]`, `<>`, `''`, `""`, `` ` ` ``, or `“”` by default. This can be convenient for operating on the closest block without having to type its symbol.

In addition to the features provided by vim-textobj-anyblock, anyblock will seek forward in the case that there is not a surrounding match. Also, repeatedly using a text object in visual mode has an expand-region-like functionality by expanding the visual selection to the next block. This is not a primary feature of anyblock but may be nice in some simple cases and is given by evil for free.

# Configuration
Anyblock does not make any mappings by default. Here is some suggested configuration:
```
(define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)
```

The block alist can be modified by the user and contains regexps (though non-characters may not work well). The user may want to set this variable locally, for example for lisp modes by removing `'` as a potential block:
```
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq-local evil-textobj-anyblock-blocks
                        '(("(" . ")")
                          ("{" . "}")
                          ("\\[" . "\\]")
                          ("\"" . "\"")))))
```

# Creating More Specific Text Objects
This package can potentially be used to create text objects for more specific use cases. I'm sure there is already a package that does this, but as an example, you can use the following to create a text object that will select the closest quotation (single, double, smart, etc.):

```
(evil-define-text-object my-evil-textobj-anyblock-inner-quote
  (count &optional beg end type)
  "Select the closest outer quote."
  (let ((evil-textobj-anyblock-blocks
         '(("'" . "'")
           ("\"" . "\"")
           ("`" . "'")
           ("“" . "”"))))
    (evil-textobj-anyblock--make-textobj beg end type count nil)))

(evil-define-text-object my-evil-textobj-anyblock-a-quote
  (count &optional beg end type)
  "Select the closest outer quote."
  (let ((evil-textobj-anyblock-blocks
         '(("'" . "'")
           ("\"" . "\"")
           ("`" . "'")
           ("“" . "”"))))
    (evil-textobj-anyblock--make-textobj beg end type count t)))

(define-key evil-inner-text-objects-map "q" 'my-evil-textobj-anyblock-inner-quote)
(define-key evil-outer-text-objects-map "q" 'my-evil-textobj-anyblock-a-quote)
```

# Motions
This package also provides motions, which may or may not be useful. For example, if you want to override `b`:
```
(define-key evil-motion-state-map "b" 'evil-textobj-anyblock-forward-open-block-start)
```

# Similar
If you just want a text-object that operates on s-expressions, you may want to use the "form" text objects that [evil-cleverparens](https://github.com/luxbock/evil-cleverparens) provides. They are very nice and will work with parentheses and quotations.
