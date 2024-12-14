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
     ((parent-is "struct_declaration") parent-bol jai-ts-mode-indent-offset)
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
     ((parent-is "assignment_parameters") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "select_statement") parent-bol 0)
     ;; ((parent-is "type_case") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "type_spec") parent-bol jai-ts-mode-indent-offset)
     ;; ((parent-is "type_switch_statement") parent-bol 0)
     ((parent-is "variable_declaration") parent-bol jai-ts-mode-indent-offset)
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
  '("#import" "#load" "#system_library" "#run")
  "Jai preprocessor keywords for tree-sitter font-locking.")

(defvar jai-ts-mode--font-lock-rules
  (treesit-font-lock-rules
    :language 'jai
    :feature 'comment
    '([(comment) (block_comment)] @font-lock-comment-face)

    :language 'jai
    :feature 'string
    '((string) @font-lock-string-face)

    :language 'jai
    :feature 'type
    `((types (identifier) @font-lock-type-face)
      ([,@jai-ts-mode--typenames] @font-lock-type-face))

    :language 'jai
    :feature 'preprocessor
    '((compiler_declaration [("#") (identifier)] @font-lock-preprocessor-face))

    :language 'jai
    :feature 'keyword
    '((if_statement ("if") @font-lock-keyword-face)
     (cast_expression ["xx" "cast"] @font-lock-keyword-face)
     ((boolean) @font-lock-keyword-face))

    :language 'jai
    :feature 'function
    '((procedure_declaration name: (identifier) @font-lock-function-name-face)
     (call_expression function: (identifier) @font-lock-function-call-face))

    :language 'jai
    :feature 'operator
    '((pointer_type ("*") @font-lock-operator-face)
      (address ("*") @font-lock-operator-face)
      (unary_expression operator: ("&") @font-lock-operator-face))

   ))

(defvar jai-ts-mode--font-lock-rules-old
  '(:language 'jai
   :override t
   :feature 'variable
   '((identifier) @font-lock-variable-name-face
     (member_expression (identifier) @font-lock-variable-name-face))

   :language 'jai
   :override t
   :feature 'namespace
   '((package_declaration (identifier) @font-lock-constant-face)
     (import_declaration alias: (identifier) @font-lock-constant-face)
     (foreign_block (identifier) @font-lock-constant-face)
     (using_statement (identifier) @font-lock-constant-face))


   :language 'jai
   :override t
   :feature 'literal
   '((number) @font-lock-number-face
     (float) @font-lock-number-face
     (character) @font-lock-constant-face
     (boolean) @font-lock-constant-face
     [(uninitialized) (nil)] @font-lock-constant-face)

   :language 'jai
   :override t
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'jai
   :override t
   :feature 'escape-sequence
   '((escape_sequence) @font-lock-escape-face)

   :language 'jai
   :override t
   :feature 'preproc
   '((attribute (identifier) @font-lock-preprocessor-face)
     [(calling_convention) (tag)] @font-lock-preprocessor-face)

   :language 'jai
   :override t
   :feature 'keyword
   `([,@jai-ts-mode--keywords] @font-lock-keyword-face
     [,@jai-ts-mode--includes] @font-lock-keyword-face
     [,@jai-ts-mode--storage-classes] @font-lock-keyword-face
     [,@jai-ts-mode--conditionals (fallthrough_statement)] @font-lock-keyword-face
     [,@jai-ts-mode--repeats] @font-lock-keyword-face)

   :language 'jai
   :override t
   :feature 'builtin
   '(["auto_cast" "cast" "transmute"] @font-lock-builtin-face)

   :language 'jai
   :override t
   :feature 'function
   '((procedure_declaration (identifier) @font-lock-function-name-face)
     (call_expression function: (identifier) @font-lock-function-call-face)
     (overloaded_procedure_declaration (identifier) @font-lock-function-name-face))

   :language 'jai
   :override t
   :feature 'type
   `((struct_declaration (identifier) @font-lock-type-face)
     (const_declaration (identifier) @font-lock-type-face)
     (type (identifier) @font-lock-type-face)
     (enum_declaration (identifier) @font-lock-type-face)
     (union_declaration (identifier) @font-lock-type-face)
     (bit_field_declaration (identifier) @font-lock-type-face))

   :language 'jai
   :override t
   :feature 'punctuation
   `([,@jai-ts-mode--operators] @font-lock-punctuation-face
     ["{" "}" "(" ")" "[" "]"] @font-lock-punctuation-face
     ["::" "->" "." "," ":" ";"] @font-lock-punctuation-face
     ["@" "$"] @font-lock-punctuation-face)

   :language 'jai
   :override t
   :feature 'error
   '((ERROR) @font-lock-warning-face)
   ))

(defvar jai-ts-mode--font-lock-feature-list-old
  '((comment string)
    (keyword type)
    (builtin preproc escape-sequence literal constant function)
    (operator punctuation variable namespace property))
  "Feature list used by `jai-ts-mode`.")


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
    ))

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
    (setq-local treesit-defun-name-function #'jai-ts-mode--defun-name)

        ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("procedure" "\\`procedure_declaration\\'" nil nil)
                  ("struct" "\\`struct_declaration\\'" nil nil)
                  ;; ("Method" "\\`method_declaration\\'" nil nil)
                  ;; ("Interface" "\\`type_declaration\\'" go-ts-mode--interface-node-p nil)
                  ;; ("Type" "\\`type_declaration\\'" go-ts-mode--other-type-node-p nil)
                  ;; ("Alias" "\\`type_declaration\\'" go-ts-mode--alias-node-p nil)
                  ))

    ;; Indent.
    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules jai-ts-mode--indent-rules)

    ;; Electric
    (setq-local electric-indent-chars
                (append "{}()" electric-indent-chars))

      ;; Font-lock
    (setq-local treesit-font-lock-settings jai-ts-mode--font-lock-rules)
    (setq-local treesit-font-lock-feature-list
                '((comment string)
                  (type keyword)
                  (preprocessor function operator)))

    (treesit-major-mode-setup)))

;; to reset to nothing:
;;(unload-feature 'jai-ts-mode t)

;;;###autoload
(when (treesit-ready-p 'jai)
  (add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-ts-mode)))

(provide 'jai-ts-mode)

;;; jai-ts-mode.el ends here
