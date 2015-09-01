# About
Evil-textobj-anyblock is port of the vim plugin [vim-textobj-anyblock]([https://github.com/rhysd/vim-textobj-anyblock]). It gives a text object that will select the closest of `()`, `{}`, `[]`, `<>`, `''`, `""`, `` ` ` ``, or `“”` by default. This can be convenient for operating on the closest block without having to type its symbol.

In addition to the features provided by vim-textobj-anyblock, anyblock will seek forward in the case that there is not a surrounding match. Also, repeatedly using a text object in visual mode has an expand-region-like functionality by expanding the visual selection to the next block. This is not a primary feature of anyblock but may be nice in some simple cases and is given by evil for free.

# Configuration
Anyblock does not make any mappings by default. Here is some suggested configuration:
```
(define-key evil-inner-text-objects-map "b" 'evil-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-anyblock-a-block)
```

The block alist can be modified by the user and contains regexps (though non-characters may not work well). The user may want to set this variable locally, for example for lisp modes by removing ='= as a potential block:
```
(add-hook 'lisp-mode-hook
          '(setq-local evil-anyblock-blocks
                       '(("(" . ")")
                         ("{" . "}")
                         ("\\[" . "\\]")
                         ("\"" . "\""))))
```
