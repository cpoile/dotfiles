;;; jai-ts-mode.el --- Jai Lang Major Mode for Emacs -*- lexical-binding: t -*-

;; TODO: fix all this preamble
;; Author: Sampie159
;; URL: https://github.com/Sampie159/jai-ts-mode
;; Keywords: jai languages tree-sitter
;; Version 0.1.0
;; Package-Requires : ((emacs "29.1"))

;;; License:

;; MIT License
;;
;; Copyright (c) 2024 Sampie159
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Powered by Emacs >= 29 and tree-sitter this major mode provides
;; syntax highlighting, indentation and imenu support for Jai.
;; jai-ts-mode is built against the tree-sitter grammar locatated at
;; https://github.com/tree-sitter-grammars/tree-sitter-jai

;; Much of the structure of this code is based on the c3-ts-mode located at
;; https://github.com/c3lang/c3-ts-mode
;; and on jai-mode located at
;; https://github.com/mattt-b/jai-mode

;; Many thanks for Mickey Petersen for his article "Let's Write a Tree-Sitter Major mode"
;; which can be found at https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode
;; for helping me do this.

;;; Code:

(require 'treesit)
(require 'js)

(defgroup jai-ts nil
  "Major mode for editing jai files."
  :prefix "jai-ts-"
  :group 'languages)

(defcustom jai-ts-mode-hook nil
  "Hook run after entering `jai-ts-mode`."
  :version "29.1"
  :type 'symbol
  :group 'jai)

(defcustom jai-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `go-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'jai)

(defconst jai-ts-mode--syntax-table ;; shamelessly stolen directly from jai-mode
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; additional symbols
    (modify-syntax-entry ?_ "w" table)

    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ??  "." table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table)
  "Syntax table for `jai-ts-mode`.")

(defvar jai-ts-mode--indent-rules
  `((jai
     ;;((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ;;((parent-is "raw_string_literal") no-indent 0)
     ((parent-is "block") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "struct_literal") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "if_case_statement") parent-bol 0)  ;; Jai's style is no indent on cases?
     ((parent-is "if_statement") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "argument_list") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "communication_case") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "const_declaration") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "default_case") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "expression_case") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "selector_expression") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "expression_switch_statement") parent-bol 0)
     ;; ((parent-is "field_declaration_list") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "import_spec_list") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "interface_type") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "labeled_statement") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "literal_value") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "named_parameters") parent-bol jai-ts-mode-indent-offset)
     ;;((parent-is "assignment_parameters") parent-bol jai-ts-mode-indent-offset)
     ((match nil "assignment_parameters" nil 1 1) standalone-parent jai-ts-mode-indent-offset)
     ((match ")" "assignment_parameters" nil nil nil) standalone-parent 0)
     ((match nil "assignment_parameters" nil 2 nil) (nth-sibling 1) 0)
     ;; ((parent-is "select_statement") parent-bol 0)
     ;; ((parent-is "type_case") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "type_spec") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "type_switch_statement") parent-bol 0)
     ((parent-is "variable_declaration") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "struct_declaration") parent-bol jai-ts-mode-indent-offset)
     ((parent-is "enum_declaration") parent-bol jai-ts-mode-indent-offset)
     ;; ((match nil "variable_declaration" nil 1 4) standalone-parent jai-ts-mode-indent-offset)
     ;; ((match ")" "variable_declaration" nil nil nil) standalone-parent 0)
     ;; ((match nil "variable_declaration" nil 2 nil) (nth-sibling 1) 0)
     ((parent-is "const_declaration") parent-bol jai-ts-mode-indent-offset)
     (no-node parent-bol 0)))
  "Tree-sitter indent rules for `jai-ts-mode'.")

(defconst jai-ts-mode--includes
  '("import" "package")
  "Includes used in `jai-ts-mode`.")

(defconst jai-ts-mode--storage-classes
  '("distinct" "dynamic")
  "Storage classes used in `jai-ts-mode`.")

(defconst jai-ts-mode--operators
  '(":=" "=" "+" "-" "*" "/" "%" "%%" ">" ">=" "<" "<=" "==" "!=" "~="
    "|" "~" "&" "&~" "<<" ">>" "||" "&&" "!" "^" ".." "+=" "-=" "*="
    "/=" "%=" "&=" "|=" "^=" "<<=" ">>=" "||=" "&&=" "&~=" "..=" "..<" "?")
  "Operators used in `jai-ts-mode`.")

(defconst jai-ts-mode--keywords
  '("foreign" "or_else"
    "in" "not_in"
    "defer" "return" "proc"
    "struct" "union" "enum" "bit_field" "bit_set" "map"
    "using" "or_return" "or_continue" "or_break")
  "Keywords used in the Jai language.")

(defconst jai-ts-mode--conditionals
  '("if" "else" "when" "switch" "case" "where" "break")
  "Conditionals used in `jai-ts-mode`.")

(defconst jai-ts-mode--repeats
  '("for" "do" "continue")
  "Repeats used in `jai-ts-mode`.")

(defvar jai-ts-mode--typenames
  '("int" "u64" "u32" "u16" "u8"
    "s64" "s32" "s16" "s8" "float"
    "float32" "float64" "string"
    "bool"))

(defvar jai-ts-mode--preprocessor
  '("#system_library" "#run")
  "Jai preprocessor keywords for tree-sitter font-locking.")

(defvar jai-ts-mode--font-lock-rules
  (treesit-font-lock-rules
    :language 'jai
    :feature 'comment
    '([(comment) (block_comment)] @font-lock-comment-face
      (if_statement "#if" condition: (boolean "false") consequence: (statement (block (statement) @font-lock-comment-face)))
      (if_statement "#if" condition: (boolean "true") consequence: (_) alternative: (else_clause (statement (block (statement) @font-lock-comment-face)))))

    :language 'jai
    :feature 'string
    '((string) @font-lock-string-face)

    :language 'jai
    :feature 'type
    `((types (identifier) @font-lock-type-face)
      ([,@jai-ts-mode--typenames] @font-lock-type-face)
      (struct_declaration name: (identifier) @font-lock-type-face)
      (struct_literal type: (identifier) @font-lock-type-face)
      (const_declaration name: (identifier) @font-lock-type-face)
      (enum_declaration name: (identifier) @font-lock-type-face))

    :language 'jai
    :feature 'preprocessor
    '((compiler_declaration [("#") (identifier)] @font-lock-preprocessor-face)
      (load ("#load") @font-lock-preprocessor-face)
      (import ("#import") @font-lock-preprocessor-face)
      (run_statement ("#run") @font-lock-preprocessor-face)
      (run_expression ("#run") @font-lock-preprocessor-face)
      (type_literal ("#type") @font-lock-preprocessor-face))

    :language 'jai
    :feature 'keyword
    '((return_statement ("return") @font-lock-keyword-face)
      (continue_statement ("continue") @font-lock-keyword-face)
      (if_statement [("if") ("#if") ("then") ("else")] @font-lock-keyword-face)
      (if_expression [("ifx") ("then") ("else")] @font-lock-keyword-face)
      (if_case_statement ("if") @font-lock-keyword-face)
      (for_statement ("for") @font-lock-keyword-face)
      (while_statement ("while") @font-lock-keyword-face)
      (using_statement ("using") @font-lock-keyword-face)
      (break_statement ("break") @font-lock-keyword-face)
      (defer_statement ("defer") @font-lock-keyword-face)
      (switch_case ("case") @font-lock-keyword-face)
      (else_clause ("else") @font-lock-keyword-face)
      (cast_expression ["xx" "cast"] @font-lock-keyword-face)
      (struct_declaration ("struct") @font-lock-keyword-face)
      (procedure_declaration modifier: ("inline") @font-lock-keyword-face)
      (enum_declaration [("enum_flags") ("enum")] @font-lock-keyword-face))

    :language 'jai
    :feature 'function
    '((procedure_declaration name: (identifier) @font-lock-function-name-face)
     (call_expression function: (identifier) @font-lock-function-call-face))

    :language 'jai
    :feature 'pointer-operator
    '((pointer_type ("*") @font-lock-operator-face)
      (address ("*") @font-lock-operator-face)
      (pointer_type ("*") @font-lock-operator-face)
      (pointer_expression operator: ("<<") @font-lock-operator-face))

    :language 'jai
    :feature 'number
    '([(float)
       (integer)] @font-lock-number-face)

    :language 'jai
    :feature 'constant
    '(([(boolean) (null)] @font-lock-constant-face))
   ))

(defconst jai-ts-mode--defun-function-type-list
  '("procedure_declaration"
    "struct_declaration"
    "enum_declaration"))

(defun jai-ts-mode--narrow-to-defun ()
  "Narrow to the function/method definition at point using treesit."
  (let ((node (treesit-node-at (point))))
    (when-let ((defun-node (treesit-parent-until
                           node
                           (lambda (n)
                             (member (treesit-node-type n)
                                    jai-ts-mode--defun-function-type-list)))))
      (narrow-to-region (treesit-node-start defun-node)
                       (treesit-node-end defun-node)))))

(defun jai-ts-mode--beginning-of-defun ()
  "Move to beginning of defun using treesit."
  (when-let* ((node (treesit-node-at (point)))
              (defun-node (treesit-parent-until
                          node
                          (lambda (n)
                            (member (treesit-node-type n)
                                   jai-ts-mode--defun-function-type-list)))))
    (goto-char (treesit-node-start defun-node))))

(defun jai-ts-mode--end-of-defun ()
  "Move to end of defun using treesit."
  (when-let* ((node (treesit-node-at (point)))
              (defun-node (treesit-parent-until
                          node
                          (lambda (n)
                            (member (treesit-node-type n)
                                   jai-ts-mode--defun-function-type-list)))))
    (goto-char (treesit-node-end defun-node))))

(defun jai-ts-mode--get-defun-bounds ()
  "Get bounds of defun using treesit."
  (when-let* ((node (treesit-node-at (point)))
              (defun-node (treesit-parent-until
                          node
                          (lambda (n)
                            (member (treesit-node-type n)
                                   jai-ts-mode--defun-function-type-list)))))
    (cons (treesit-node-start defun-node)
          (treesit-node-end defun-node))))

;;;###autoload
(define-derived-mode jai-ts-mode prog-mode "jai"
  "Major mode for editing jai files, powered by tree-sitter."
  :group 'jai
  :syntax-table jai-ts-mode--syntax-table

  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'jai)
    (treesit-parser-create 'jai)

    ;; Comments.
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "//" (* (syntax whitespace))))

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("function_declaration"
                              ;;"type_declaration"
                              )))
    ;; this breaks which-fun:
    ;;(setq-local treesit-defun-name-function #'jai-ts-mode--defun-name)
    (setq-local treesit-defun-name-function nil)

    (setq-local narrow-to-defun-function #'jai-ts-mode--narrow-to-defun)
    (setq-local beginning-of-defun-function #'jai-ts-mode--beginning-of-defun)
    (setq-local end-of-defun-function #'jai-ts-mode--end-of-defun)

    ;; TODO: this overides other major modes? fixme. nocommit
    (put 'defun 'bounds-of-thing-at-point 'jai-ts-mode--get-defun-bounds)
    ;;(get 'defun 'bounds-of-thing-at-point) ;; maybe store the original and reput?

        ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("proc" "\\`procedure_declaration\\'" nil jai-ts-mode--defun-name)
                  ("struct" "\\`struct_declaration\\'" nil jai-ts-mode--defun-name)
                  ("enum" "\\`enum_declaration\\'" nil jai-ts-mode--defun-name)
                  ;;("const" "\\`const_declaration\\'" nil nil)
                  ;; ("Method" "\\`method_declaration\\'" nil nil)
                  ;; ("Interface" "\\`type_declaration\\'" go-ts-mode--interface-node-p nil)
                  ;; ("Type" "\\`type_declaration\\'" go-ts-mode--other-type-node-p nil)
                  ;; ("Alias" "\\`type_declaration\\'" go-ts-mode--alias-node-p nil)
                  ))

    ;; Indent.
    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules jai-ts-mode--indent-rules
                which-func-functions nil)

    ;; Electric
    (setq-local electric-indent-chars
                (append "{}()" electric-indent-chars))

      ;; Font-lock
    (setq-local treesit-font-lock-settings jai-ts-mode--font-lock-rules)
    (setq-local treesit-font-lock-feature-list
                '((comment string)
                  (type keyword number constant pointer-operator)
                  (preprocessor function operator)))

    (treesit-major-mode-setup)))

;; to reset to nothing:
;;(unload-feature 'jai-ts-mode t)

(defun jai-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("procedure_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name
       node "name")
      t))
    ("struct_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name
       node "name")
      t))
    ("enum_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name
       node "name")
      t))
    ))


;; NOTE: not used yet
(defun jai-ts-mode--struct-node-p (node)
  "Return t if NODE is a struct."
  (and
   (string-equal "type_declaration" (treesit-node-type node))
   (treesit-search-subtree node "struct_type" nil nil 2)))

;;;###autoload
(when (treesit-ready-p 'jai)
  (add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-ts-mode)))

(defconst jai--error-regexp
  "^\\([^ \n:]+.*\.jai\\):\\([0-9]+\\),\\([0-9]+\\):")
(push `(jai ,jai--error-regexp 1 2 3 2) compilation-error-regexp-alist-alist)
(push 'jai compilation-error-regexp-alist)

(provide 'jai-ts-mode)

;;; jai-ts-mode.el ends here
