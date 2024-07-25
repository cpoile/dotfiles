;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Christopher Poile"
      user-mail-address "cpoile@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Meslo LG S" :size 12 :weight 'normal)
;;     doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;(setq doom-font (font-spec :family "Meslo LG S" :size 12 :weight 'normal))

(if (string-equal system-type "darwin")
    (progn
      (setq doom-font (font-spec :family "JetBrains Mono" :size 16 :weight 'Light))))

(if (string-equal system-type "gnu/linux")
    (progn
      (setq doom-font (font-spec :family "JetBrains Mono" :size 21))))

(setq line-spacing 0)

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-zenburn)

(setq doom-theme 'forestbones)

(set-face-attribute 'line-number-current-line nil :inherit nil)
(after! whitespace
  (set-face-attribute 'whitespace-tab nil :background "#242D34"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-load-path! "lisp")

(if (string-equal system-type "gnu/linux")
    (progn
      (setq x-alt-keysym 'meta)))

(setq!
        mac-command-modifier 'meta
        mac-option-modifier  'super
        scroll-margin        10
        backup-by-copying t      ; don't clobber symlinks
        delete-old-versions t
        kept-new-versions 50
        version-control t       ; use versioned backups
        vc-make-backup-files t
        auto-save-interval 20
        auto-save-timeout 5
        global-hl-line-modes nil)

;; backup dirrectory is common for all files:
(setq backup-directory-alist
      `((".*" . "~/to-sync/.backups")))
;; create the backups dir if necessary, since emacs won't.
(make-directory "~/to-sync/.backups/" t)

;; but keep the autosave directory local to the file so that it can
;; recover from crashes:
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/to-sync/.autosaves/" t)
(setq auto-save-file-name-transforms
      `((".*" "~/to-sync/.autosaves/\\2" t)))

;; always backup.
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;; autosave code block edit buffers
(setq org-edit-src-auto-save-idle-delay 0)  ;; don't use this
(setq org-edit-src-turn-on-auto-save t)     ;; use this
;; and now change the auto-save name so it saves with the rest of the auto-save files:
(defadvice org-edit-src-code (after rename-org-src-buffer activate)
  (if org-src-mode
      (progn
        (setq buffer-file-name
              (concat default-directory buffer-auto-save-file-name))
        (setq buffer-auto-save-file-name (make-auto-save-file-name)))))

(use-package backup-walker)

;; better scrolling
(use-package golden-ratio-scroll-screen)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

;; are we in a terminal?
;; (unless (display-graphic-p)
;;   (getenv-internal "TERM" initial-environment))

;;
;; PROGRAMMING
;;

(use-package! smart-comment
  :bind ("M-;" . smart-comment))

(global-set-key (kbd "C-c ]") 'git-gutter:next-hunk)
(global-set-key (kbd "C-c [") 'git-gutter:previous-hunk)

(define-key prog-mode-map (kbd "C-q") 'lsp-ui-doc-show)
;;
;; Jai setup
;;

(load! "jai-mode.el")
(use-package jai-mode
  :bind (:map jai-mode-map
      ;;("C-c C-r" . 'jai-run-project)
      ;;("C-c C-c" . 'jai-build-project)
      ("C-c C-r" . 'recompile)
      ("C-c C-c" . 'compile)))

(defun jai-previous-defun ()
  "Go to previous proc."
  (interactive)
  (beginning-of-line)
  (re-search-backward jai--defun-rx)
  (beginning-of-line))

(defun jai-next-defun ()
  "Go to next proc."
  (interactive)
  (forward-line)
  (re-search-forward jai--defun-rx)
  (beginning-of-line))

(map! :map jai-mode-map
      "C-M-e" #'jai-next-defun
      "C-M-a" #'jai-previous-defun)


;;
;; Odin setup
;;
(load! "odin-mode.el")
;;(package-vc-install "https://git.sr.ht/~mgmarlow/odin-mode")
(use-package odin-mode
  :bind (:map odin-mode-map
      ;;("C-c C-r" . 'odin-run-project)
      ;;("C-c C-c" . 'odin-build-project)
      ("C-c C-r" . 'recompile)
      ("C-c C-c" . 'compile)
      ("C-c C-t" . 'odin-test-project)
      ))





(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(odin-mode . "odin"))
  (let ((ols-exec (if (string-equal system-type "darwin") "~/bin/ols" "~/git/ols/ols.exe")))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection ols-exec)
                      :major-modes '(odin-mode)
                      :server-id 'ols
                      :multi-root t)))) ;; This is just so lsp-mode sends the "workspaceFolders" param to the server.
(add-hook 'odin-mode-hook #'lsp)
;; (after! compile
;;   (add-to-list 'compilation-error-regexp-alist-alist '(odin "^\\([A-Za-z0-9\\._/-]+\\)(\\([0-9]+\\):\\([0-9]+\\))" 1 2 3))
;;   (add-to-list 'compilation-error-regexp-alist 'odin))

(add-hook 'odin-mode-hook (lambda ()
                            (setq comment-start "//"
                                  comment-end   "")
                            ;; (setq indent-tabs-mode nil)
                            ;; (setq indent-line-function 'relative-line-indent)
                            (setq lsp-ui-doc-max-height 40)
                            (setq lsp-ui-doc-max-width 150)
                            ))

;; Should be in an after! macro, but we're definitely loading it above, so:
(defun odin-previous-defun ()
  "Go to previous proc."
  (interactive)
  (beginning-of-line)
  (re-search-backward odin--defun-rx)
  (beginning-of-line))

(defun odin-next-defun ()
  "Go to next proc."
  (interactive)
  (forward-line)
  (re-search-forward odin--defun-rx)
  (beginning-of-line))

(map! :map odin-mode-map
      "C-M-e" #'odin-next-defun
      "C-M-a" #'odin-previous-defun
      "C-M-l" #'lsp-format-buffer)


;;
;; Misc settings
;;
(back-button-mode 1)
(setq doom-modeline-persp-name t)
(setq doom-modeline-display-default-persp-name t)
(setq! mark-even-if-inactive nil)
(whole-line-or-region-global-mode)

;; Magit
(after! magit
  (setq!
   magit-diff-refine-hunk 'all
   git-commit-summary-max-length 68))
;; (use-package magit-delta
;;   :hook (magit-mode . magit-delta-mode))

;; Add to a programming language map as needed (see rust below)
;; Not added because it doesn't seem to work in terminal mode
(defun cp/lsp-doc-window-or-focus ()
  "Bring up the lsp doc window, or focus the lsp doc window"
  (interactive)
  (if (lsp-ui-doc--visible-p)
      (lsp-ui-doc-focus-frame)
    (lsp-ui-doc-show)))

;;
;; Rust mode
;;

(map! :map rust-mode-map
      ;"C-q" #'cp/lsp-doc-window-or-focus
      "C-q" #'lsp-ui-doc-glance
      "C-M-l" #'lsp-format-buffer)
(setq! lsp-ui-doc-max-height 40)
(setq! lsp-ui-doc-max-width 150)
(set-popup-rules!
  '(("^\\*cargo-run" :size 0.4 :slot -1 :ttl t)))
;;(setq! lsp-ui-doc-enable nil)

(after! rust-mode
  (modify-syntax-entry ?_ "w" rust-mode-syntax-table))

(setq! lsp-idle-delay 0.2)
(setq! lsp-rust-analyzer-server-display-inlay-hints t)
(setq! lsp-rust-analyzer-display-chaining-hints t)
(setq! lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
(setq! lsp-rust-analyzer-display-closure-return-type-hints t)
(setq! lsp-rust-analyzer-display-parameter-hints nil)
(setq! lsp-rust-analyzer-display-reborrow-hints nil)
(setq! lsp-eldoc-render-all nil)
(setq! eldoc-idle-delay 0.2)
(setq! lsp-signature-auto-activate)

;;;
;;; lsp-mode with Tramp doesn't seem to work:
;;;
;; ;; (defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
;; ;;   "Start a program in a subprocess.  Return the process object for it.
;; ;; Similar to `start-process-shell-command', but calls `start-file-process'."
;; ;;   ;; On remote hosts, the local `shell-file-name' might be useless.
;; ;;   (let ((command (mapconcat 'identity args " ")))
;; ;;     (funcall start-file-process-shell-command name buffer command)))
;;
;; ;; (advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)
;;
;; (with-eval-after-load "lsp-rust"
;;  (lsp-register-client
;;   (make-lsp-client
;;    :new-connection (lsp-stdio-connection
;;                     (lambda ()
;;                       `(,(or (executable-find
;;                               (cl-first lsp-rust-analyzer-server-command))
;;                              (lsp-package-path 'rust-analyzer)
;;                              "rust-analyzer")
;;                         ,@(cl-rest lsp-rust-analyzer-server-args))))
;;    :remote? t
;;    :major-modes '(rust-mode rustic-mode)
;;    :initialization-options 'lsp-rust-analyzer--make-init-options
;;    :notification-handlers (ht<-alist lsp-rust-notification-handlers)
;;    :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
;;    :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
;;    :after-open-fn (lambda ()
;;                     (when lsp-rust-analyzer-server-display-inlay-hints
;;                       (lsp-rust-analyzer-inlay-hints-mode)))
;;    :ignore-messages nil
;;    :server-id 'rust-analyzer-remote)))

;; Also doesn't work:
;;
;; (defun rustic-lsp-conn-command ()
;;   (cons
;;    (executable-find "rust-analyzer" t)
;;    (cl-rest lsp-rust-analyzer-server-args)))
;;
;; (with-eval-after-load "lsp-rust"
;;   (defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
;;     "Start a program in a subprocess.  Return the process object
;; for it. Similar to `start-process-shell-command', but calls
;; `start-file-process'."
;;     ;; On remote hosts, the local `shell-file-name' might be useless.
;;     (let ((command (mapconcat 'identity args " ")))
;;       (funcall start-file-process-shell-command name buffer command)))
;;
;;   (advice-add 'start-file-process-shell-command
;;               :around #'start-file-process-shell-command@around)
;;
;;  (lsp-register-client
;;   (make-lsp-client
;;    :new-connection (lsp-tramp-connection #'rustic-lsp-conn-command)
;;    :remote? t
;;    :major-modes '(rust-mode rustic-mode)
;;    :initialization-options 'lsp-rust-analyzer--make-init-options
;;    :notification-handlers (ht<-alist lsp-rust-notification-handlers)
;;    :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
;;    :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
;;    :after-open-fn (lambda ()
;;                     (when lsp-rust-analyzer-server-display-inlay-hints
;;                       (lsp-rust-analyzer-inlay-hints-mode)))
;;    :ignore-messages nil
;;    :server-id 'rust-analyzer-remote)))
;;
;;
;; (defun expand-file-name-remote (file)
;;   "A tramp-friendly version of expand-file-name.  Expand file
;; relative to the remote part of default-directory."
;;   (let ((file (expand-file-name file))
;;         (dir-remote (file-remote-p default-directory)))
;;     (if (file-remote-p file)
;;         ;; if file is already remote, return it
;;         file
;;       ;; otherwise prepend the remote part (if any) of default-directory
;;       (concat dir-remote file))))
;;
;; (defun around-ad-executable-find (orig-fn cmd &optional is-remote)
;;   (let ((remote-cmd (if is-remote (expand-file-name-remote cmd) cmd)))
;;     (if (file-executable-p remote-cmd)
;;         cmd
;;       (funcall orig-fn cmd is-remote))))
;; (advice-add 'executable-find :around 'around-ad-executable-find)
;;
;; (require 'lsp-rust)
;;
;; (require 'tramp)
;; (after! tramp
;;   (setq tramp-verbose 10)
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;
;; C-mode
;;
(map! :map c-mode-map
      "C-q" #'+lookup/documentation
      "C-M-l" #'lsp-format-buffer
      "C-c C-c" #'compile
      "C-c C-r" #'recompile)

;; TODO: make it so that using C-u shows all buffers (except that's used... so, something else)
(map! "C-x C-b" #'(lambda (arg)
                    (interactive "P")
                    (with-persp-buffer-list () (ibuffer arg))))

;;
;; Customize zenburn
;;

;; (set-face-attribute 'region nil :background "#5F5F5F")
;; (set-face-attribute 'default nil :foreground "#D6D6C6" :background "#303030")
;; (after! vertico
;;   (set-face-attribute 'vertico-current nil :background "#494949"))
;; (after! lsp-mode
;;   (set-face-attribute 'lsp-face-highlight-textual nil :foreground "#D6D6C6" :background "#464C43"))

;;
;; Custom functions
;;
(defun isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))
(add-hook 'isearch-mode-hook #'isearch-with-region)

;; testing why yanking is weird:
;; (ad-deactivate 'yank)

;;
;; Borrowed from crux -- consider loading it, if it doesn't clobber keybindings
;; Fixed: if we are regioning onto a new line (point is current-column 0),
;; then back up -- we probably don't intend to duplicate that final line
(defun crux-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (if mark-active
        (if (and (= (current-column) 0) (> (point) (mark)))
            (backward-char)))
    (setq end (line-end-position))
    (cons beg end)))


;; This fixes the duplicate lines bug, should be fixed eventually:
;; https://github.com/bbatsov/crux/pull/96
(defun crux-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun crux-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

;;
;; Customize for writing, and org-mode
;;

;; I   use single spaces between paragraphs, so I will have to put up with
;; "forward-sentence" picking up abbreviations and titles as sentence endings.
(setq sentence-end-double-space nil)

;; ;; I tried but didn't like the following:
;; don't leave two or more whitespaces after killing a word
;; (defadvice kill-word (after kill-word-and-spaces (arg) activate compile)
;;   "Kill the word, and the spaces around it."
;;   (interactive "p")
;;    (if (eq (char-after) ? ) (just-one-space)))
;;(ad-unadvise 'kill-word)

(global-visual-line-mode t)

;; this is bound in the keybindings section later.
(defun cp/backward-kill-visual-line ()
  (interactive)
  (if (boundp visual-line-mode)
      (kill-visual-line 0)
    (kill-line 0))
  (indent-according-to-mode))

;;
;; Org-mode
;;

(setq org-startup-folded 'content)
(setq org-use-speed-commands t)

;; Move up and down org-trees with C-M-n and C-M-p
(defun cp/org-next-visible-any-item-or-heading ()
  "Move to the beginning of the next item child, item uncle, or heading, if it is visible."
  (interactive)
  (let ((item (org-in-item-p)))
    (cond
     ((not item)
      (or (cp/goto-next-subtree-item) (outline-next-visible-heading 1)))
     ;; we are at an item.
     ;; see if we have children to move down.
     (t (let* ((struct (org-list-struct))
               (child (org-list-has-child-p item struct)))
          (cond
           (child (goto-char child))
           (t (cp/org-next-visible-item-skip-children))))))))

(defun cp/get-visible-list-in-context-area (&optional direction)
  "Returns position of list item, or nil if there is no visible list in context area.
If direction is 'whole or nil, use the whole context. Search downwards from point if direction is 'next, search up from point if direction is 'prev"
  (interactive)
  ;; if we're going up, if we are on an outline heading, use context
  ;; of previous heading.
  (fset 'search-fn (if (eq direction 'prev) 're-search-backward 're-search-forward))
  (let* ((context (if (eq direction 'prev) (cp/get-prev-context) (org-list-context)))
         (contxbeg (car context))
         (contxend (car (cdr context)))
         (lim-end (if (eq direction 'prev) contxbeg contxend)))
    (save-excursion
      (let* ((listpos (search-fn (org-item-beginning-re) lim-end t))
             (listposbeg (beginning-of-line)))
        ;; if we found nothing, listpos is nil here.
        (and listpos (cp/item-visible listposbeg))))))

(defun cp/goto-next-subtree-item (&optional arg)
  "arg 'next: search downwards
arg 'prev: search above"
  ;; we are not in an item right now. check if our subtree
  ;; context has more items.
  (let ((nextitem (cp/get-visible-list-in-context-area (or arg 'next))))
    (if nextitem (goto-char nextitem) nil)))

(defun cp/goto-prev-subtree-item ()
  (cp/goto-next-subtree-item 'prev))

(defun cp/org-prev-visible-any-item-or-heading ()
  "Move to the beginning of the prev item child, item uncle, or heading, if it is visible."
  (interactive)
  (or (cp/goto-prev-subtree-item) (outline-previous-visible-heading 1)))

(defun cp/org-next-visible-item-skip-children ()
  "Assumes we are in an item. Go to next item, or if none go back to parent and try again without
going through children."
  (interactive)
  (let*
      ((struct (org-list-struct))
       (item (org-in-item-p))
       (prevs (org-list-prevs-alist struct))
       (next-item (org-list-get-next-item item struct prevs)))
    (if next-item (goto-char next-item)
      (let*
          ((parents (org-list-parents-alist struct))
           (prev-parent (org-list-get-parent item struct parents)))
        (if prev-parent
            (progn
              (goto-char prev-parent)
              (cp/org-next-visible-item-skip-children))
          ;; no parent
          (outline-next-visible-heading 1))))))

(defun cp/get-prev-context ()
  (interactive)
  (save-excursion
    (outline-previous-visible-heading 1)
    (org-list-context)))


(defun cp/item-visible (&optional pos)
  "returns point if it's visible, or nil if invisible"
  (if (outline-invisible-p (or pos (point)))
      nil
    (or pos (point))))

;; the next functions help us when we are in a list. C-M-u in a list
;; would have brought us up to the /parent/ of the current
;; org-heading, when what you probably want is to go up to the current
;; heading (the parent of the list you are currently in). That would
;; be org-prev-visible-heading.
(defun cp/org-up-immediate-heading ()
  "If in a list, go to the previous org-heading. If not in a list, go to the parent
   heading as usual."
  ;; if in an item,
  ;; or go to previous heading is an item,
  ;; up heading should actually be org-prev-heading
  (interactive)
  (if (outline-on-heading-p)
      ;; if we're on a heading already, just move up.
      (outline-up-heading 1 t)
    (let ((prev-item (cp/get-visible-list-in-context-area 'prev)))
      (if prev-item
          (outline-previous-heading)
        (outline-up-heading 1 t)))))

;; Add the TODO keyword to org-mode's font keywords
(defun cp/set-inline-todo-keyword ()
  (add-to-list 'org-font-lock-extra-keywords
               '("<TODO:[^>]+>" . 'org-todo) t))

(add-hook 'org-font-lock-set-keywords-hook 'cp/set-inline-todo-keyword)

(defun cp/copy-buffer-to-other-window ()
  "Move the current buffer to the other window"
  (interactive)
  (switch-to-buffer-other-window (buffer-name)))

(defun cp/move-buffer-to-other-window ()
  "Move the current buffer to the other window"
  (interactive)
  (let ((current-buffer (buffer-name)))
    (bury-buffer)
    (switch-to-buffer-other-window current-buffer)))

;; Misc fixes


;; turn off smartparens mode in orgmode (revisit if I ever start doing literate programming)
;; don't use the insert-tab indent mode that text-mode uses (it seems to override org, so I guess it runs too?)
;; move to the start of an org header, not the start of the line
(after! org
  (add-hook 'org-mode-hook #'turn-off-smartparens-mode)
  (add-hook 'org-mode-hook (lambda ()
                             (setq indent-line-function 'indent-relative)
                             (setq org-insert-heading-respect-content nil)
                             (setq org-M-RET-may-split-line t)
                             (setq org-special-ctrl-a/e t))))

;; turn of company mode by default. If you want to turn it on in some modes,
;; see: https://emacs.stackexchange.com/questions/48871/how-to-enable-company-mode-for-some-buffers-only
(after! company
  (global-company-mode -1)
  (setq company-global-modes '(not org-mode)))


;; and make txt files use paragraph indents as the paragraph breaks.
;; TODO: this changes for everyone -- need to fix
;; (add-hook 'text-mode-hook (lambda ()
;;                        (setq-local paragraph-start "[ 	\n\f]\\|\f\\|[ 	]*$")
;;                        (setq-local indent-line-function 'insert-tab)))

(defun cp/backward-paragraph (&optional arg)
  "Move backwards by paragraph, but in text-mode normally this would
 send you to the start of the current paragraph. I want to go to the
 start of the previous one."
  (interactive "^p")
  (or arg (setq arg 3))
  (forward-char)
  (forward-paragraph (- arg 3)))

(defun volatile-kill-buffer ()
   "Kill current buffer unconditionally."
   (interactive)
   (let ((buffer-modified-p nil))
     (kill-buffer (current-buffer))))

(defun cp/frame-resize-init (&optional arg)
  "set the frame size for editing using emacs chrome (it always starts too small)"
  (when (display-graphic-p arg)
    (set-frame-size arg 80 24)))

(add-hook 'after-make-frame-functions 'cp/frame-resize-init)

;;
;; Keybindings
;;
(map! :map text-mode-map "C-M-p" #'cp/backward-paragraph)
(map! :map text-mode-map "M-p" #'cp/backward-paragraph)

(map! "C-M-n" #'forward-paragraph)
(map! "C-M-p" #'backward-paragraph)
(map! "M-n" #'forward-paragraph)
(map! "M-p" #'backward-paragraph)
(map! :after smartparens
      :map smartparens-mode-map
      "C-M-n" nil
      "C-M-p" nil
      "C-M-a" nil
      "C-M-e" nil
      "C-M-u" #'sp-backward-up-sexp)

;; (define-key prog-mode-map (kbd "M-/") 'company-capf)
;; (map! "M-<left>" #'back-button-global-backward)
;; (map! "M-<right>" #'back-button-global-forward)
(map! "C-M-[" #'back-button-global-backward)
(map! "C-M-]" #'back-button-global-forward)
(map! "M-{" #'back-button-local-backward)
(map! "M-}" #'back-button-local-forward)

;; because those conflict in org-mode, backup
(map! "C-c d" #'crux-duplicate-current-line-or-region)
(map! "C-M-j" #'crux-top-join-line)

(map! "C-x C-o" #'cp/copy-buffer-to-other-window)
(map! "C-x O" #'cp/move-buffer-to-other-window)

;; TODO: convert the following to map! macros?
;;
;; backward kill sentence
(global-set-key (kbd "M-K") 'backward-kill-sentence)
;; backward kill line (but only the visual line)
(global-set-key (kbd "C-S-k") 'cp/backward-kill-visual-line)
(global-set-key (kbd "C-k") 'kill-visual-line)
(global-set-key (kbd "M-Z") 'zap-to-char)
(global-set-key (kbd "M-z") 'zap-up-to-char)
;;(global-set-key (kbd "M-<return>") '+default/newline-above)
;; (global-set-key (kbd "C-o") '+default/newline-above)
;; (global-set-key (kbd "C-S-o") 'open-line)

;; Because pressing esc key was bringing up the esc map, and esc again was global-back
(global-set-key (kbd "<escape>") 'doom/escape)
;; Unconditionally kill unmodified buffers.
(global-set-key (kbd "C-x k") 'volatile-kill-buffer)

(after! org
  (map! :map org-mode-map "C-M-n" #'cp/org-next-visible-any-item-or-heading)
  (map! :map org-mode-map "C-M-p" #'cp/org-prev-visible-any-item-or-heading)
  (map! :map org-mode-map "C-M-u" #'outline-up-heading)
  (map! :map org-mode-map "C-M-u" #'outline-up-heading)
  (map! :map org-mode-map "C-M-f" #'org-forward-heading-same-level)
  (map! :map org-mode-map "C-M-b" #'org-backward-heading-same-level)
  (map! :map org-mode-map "C-M-u" #'cp/org-up-immediate-heading)
  (map! :map org-mode-map "M-}" #'back-button-local-forward)
  (map! :map org-mode-map "M-{" #'back-button-local-backward)
  ;;(map! :map org-mode-map "C-o" #'+org/insert-item-above)

  ;; (map! :map org-mode-map "M-n" #'cp/org-next-visible-any-item-or-heading)
  ;; (map! :map org-mode-map "M-p" #'cp/org-prev-visible-any-item-or-heading)
  (org-remap org-mode-map 'backward-paragraph 'backward-paragraph)
  (org-remap org-mode-map 'forward-paragraph 'forward-paragraph)
  (map! :map org-mode-map "M-n" #'forward-paragraph)
  (map! :map org-mode-map "M-p" #'backward-paragraph)

  ;; these were conflicting with global and local back-button
  ;; (map! :map org-mode-map "M-]" #'org-metaright)
  ;; (map! :map org-mode-map "M-[" #'org-metaleft)
  (map! :map org-mode-map "C-e" #'end-of-visual-line))


;;
;; Multiple cursors
;;
(use-package! multiple-cursors
  :ensure   t
  :bind (("C-M-SPC" . set-rectangular-region-anchor)
         ("C-M->" . mc/mark-next-like-this)
         ("C-M-<" . mc/mark-previous-like-this)
         ("C-c C-." . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)))

;; (use-package! multiple-cursors
;;   :ensure t
;;   :config
;;   (lambda ()
;;     (defvar mc/mark-next-like-this)
;;     (defvar mc/mark-previous-like-this)
;;     (defvar mc/mark-all-like-this)
;;     (map! "C-M->" 'mc/mark-next-like-this)
;;     (map! "C-M-<" 'mc/mark-previous-like-this)
;;     (map! "C-c C-," 'mc/mark-all-like-this)))

;; Now C-x SPC will be rectangle mark mode
(after! back-button
  (map! :map back-button-mode-map "C-x SPC" nil))


;;
;; Highlight variable under cursor
;;
(use-package! idle-highlight-mode
  :config (progn
           (setq
            idle-highlight-idle-time 0.3
            idle-highlight-ignore-modes (list 'org-mode))
           (set-face-attribute 'idle-highlight nil :background (doom-lighten "#1E262B" 0.10)))
  :hook ((prog-mode text-mode) . idle-highlight-mode))

;;
;; Popup documentation
;;
(use-package! company-quickhelp
  :config (progn
            (setq
             company-quickhelp-delay nil)
            (set-face-attribute 'company-tooltip-selection nil :background (doom-lighten "#1E262B" 0.20)))
  :hook (prog-mode . company-quickhelp-mode))

(after! company-quickhelp
  (define-key company-active-map (kbd "C-q") #'company-quickhelp-manual-begin))

;;
;; Dumb-jump for jai instead of using an ols
;;
(use-package! dumb-jump
  :ensure t
  :custom
  (dumb-jump-prefer-searcher 'rg)
  ;; (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'consult-xref)
  (dumb-jump-rg-search-args "--pcre2 --type-add 'jai:*.{jai}'")

  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
;;(setq dumb-jump-debug t)

;;
;; Org-novelist
;;
(load! "org-novelist.el")
(after! org-novelist
  (setq org-novelist-language-tag "en-GB")  ; The interface language for Org Novelist to use. It defaults to 'en-GB' when not set
  (setq org-novelist-author "C.R. Poile")  ; The default author name to use when exporting a story. Each story can also override this setting
  (setq org-novelist-author-email "cpoile@gmail.com")  ; The default author contact email to use when exporting a story. Each story can also override this setting
  (setq org-novelist-automatic-referencing-p nil))


;;
;; So pdflatex can be found
;;
(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
;; call function now

(set-exec-path-from-shell-PATH)


;; Clippety
(unless (display-graphic-p)
  (use-package! clipetty
    ;; if you omit :defer, :hook, :commands, or :after, then the package is loaded
    ;; immediately. By using :hook here, the `hl-todo` package won't be loaded
    ;; until prog-mode-hook is triggered (by activating a major mode derived from
    ;; it, e.g. python-mode)
    :ensure t
    :bind ("M-w" . clipetty-kill-ring-save)
    :config
    ;; code here will run after the package is loaded
    (setq clipetty-assume-nested-mux t)))

;; Start almost full screen on mac
(if (string-equal system-type "darwin")
    (progn
      (setq frame-inhibit-implied-resize t)  ;; prevent resize window on startup
      (set-frame-height (window-frame) 65)
      (set-frame-width (window-frame) 220)))

;;
;; xterm crap
;;
;; xterm with the resource ?.VT100.modifyOtherKeys: 1
;; GNU Emacs >=24.4 sets xterm in this mode and define
;; some of the escape sequences but not all of them.
(defun character-apply-modifiers (c &rest modifiers)
  "Apply modifiers to the character C.
MODIFIERS must be a list of symbols amongst (meta control shift).
Return an event vector."
  (if (memq 'control modifiers) (setq c (if (or (and (<= ?@ c) (<= c ?_))
                                                (and (<= ?a c) (<= c ?z)))
                                            (logand c ?\x1f)
                                          (logior (lsh 1 26) c))))
  (if (memq 'meta modifiers) (setq c (logior (lsh 1 27) c)))
  (if (memq 'shift modifiers) (setq c (logior (lsh 1 25) c)))
  (vector c))

(defun my-eval-after-load-xterm ()
  (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
    (let ((c 32))
      (while (<= c 126)
        (mapc (lambda (x)
                (define-key xterm-function-map (format (car x) c)
                  (apply 'character-apply-modifiers c (cdr x))))
              '(;; with ?.VT100.formatOtherKeys: 0
                ("\e\[27;3;%d~" meta)
                ("\e\[27;5;%d~" control)
                ("\e\[27;6;%d~" control shift)
                ("\e\[27;7;%d~" control meta)
                ("\e\[27;8;%d~" control meta shift)
                ;; with ?.VT100.formatOtherKeys: 1
                ("\e\[%d;3u" meta)
                ("\e\[%d;5u" control)
                ("\e\[%d;6u" control shift)
                ("\e\[%d;7u" control meta)
                ("\e\[%d;8u" control meta shift)))
        (setq c (1+ c))))))

(unless (display-graphic-p)
 (eval-after-load "xterm" '(my-eval-after-load-xterm))

 ;; to fix tmux (until this is fixed: https://github.com/tmux/tmux/issues/3721)
 (map! "C-*" 'undo-redo)
 ;; I've set S-backspace to send Esc+[7;5- because I keep pressing it by accident
 ;; See: https://www.vinc17.net/unix/ctrl-backspace.en.html
 (define-key global-map "\C-[[7;5~" 'backward-delete-char))


;;
;; Tree-sitter
;;
;; Should use:
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
;; ;;at least once per installation or while changing this list
;; (setq treesit-language-source-alist
;;       '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;         (cmake "https://github.com/uyha/tree-sitter-cmake")
;;         (css "https://github.com/tree-sitter/tree-sitter-css")
;;         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;         (go "https://github.com/tree-sitter/tree-sitter-go")
;;         (html "https://github.com/tree-sitter/tree-sitter-html")
;;         (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;         (json "https://github.com/tree-sitter/tree-sitter-json")
;;         (make "https://github.com/alemuller/tree-sitter-make")
;;         (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;         (python "https://github.com/tree-sitter/tree-sitter-python")
;;         (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;         (yaml "https://github.com/ikatyang/tree-sitter-yaml")
;;         (c "https://github.com/tree-sitter/tree-sitter-c")
;;         (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;         (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;         (odin "https://github.com/ap29600/tree-sitter-odin")))

 ;; (major-mode-remap-alist
 ;;  '((elixir-mode . elixir-ts-mode)))

;; (add-to-list 'treesit-load-name-override-list '(c "libtree-sitter-c" "tree_sitter_c"))
;; (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

;; ;; When you want to change a major mode -> its ts mode, do this:
;; (add-to-list 'major-mode '(rustic-mode . rust-ts-mode))

;; ;; When you want to keep your mode hooks:
;; (defun rust-ts-mode-call-hook () (run-hooks 'rust-mode-hook))
;; (add-hook 'rust-ts-mode-hook #'rust-ts-mode-call-hook)

(defun c-ts-mode-call-hook () (run-hooks 'c-mode-hook))
(add-hook 'c-ts-mode-hook #'c-ts-mode-call-hook)

;; is it enabled?
(defun treesit-enabled-p ()
  "Checks if the current buffer has a treesit parser."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-at (point))))

; to set up emacs with treesitter:
;   https://github.com/d12frosted/homebrew-emacs-plus
; to set up treesitter:
;; git clone --depth=1 https://github.com/tree-sitter/tree-sitter.git
;;   make
;;
;; (instead of using the list command above)
;; git clone --depth=1 https://github.com/casouri/tree-sitter-module
;;   ./batch.sh
;;   mv dist/* /usr/local/lib
;;(require 'treesit)
;;(treesit-language-available-p 'go)

;;
;; go
;;
;; (after! go-mode
;;   (after! lsp-mode
;;     (add-hook 'go-mode-hook 'lsp)))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :init
  ;;(setq compilation-read-command nil)
  :bind (;; ("M-." . godef-jump)
         ("C-." . +lookup/references))
  :hook ((go-mode . lsp-deferred)))

;; ->
;; don't need this bc we're just not going to use treesit for now
;;
;;(defun go-mode-run-hooks () (run-hooks 'go-mode-hook))
;;(add-hook 'go-ts-mode-hook #'go-mode-run-hooks)

(after! go-ts-mode
  (setq auto-mode-alist (delete '("\\.go\\'" . go-ts-mode) auto-mode-alist)))


;;
;; flash on copy
;;

(defun my/pulse-current-region (&rest _)
  "Pulse the current implicit or active region."
  (if mark-active
      (pulse-momentary-highlight-region (region-beginning) (region-end))
    (pulse-momentary-highlight-one-line)))

(advice-add #'kill-ring-save :before #'my/pulse-current-region)


;; Edit text areas in chrome with Ctrl-.
(add-load-path! "~/.doom.d/websocket/")
(add-load-path! "~/.doom.d/atomic-chrome/")
(require 'atomic-chrome)
(setq-default atomic-chrome-buffer-open-style 'frame)
(setq-default atomic-chrome-auto-remove-file t)
(setq atomic-chrome-buffer-frame-width 50)
(setq atomic-chrome-buffer-frame-height 100)
(setq-default atomic-chrome-url-major-mode-alist
              '(("ramdajs.com" . js-ts-mode)
                ("github.com" . gfm-mode)
                ("gitlab.com" . gfm-mode)
                ("leetcode.com" . typescript-ts-mode)
                ("codesandbox.io" . js-ts-mode)
                ("typescriptlang.org" . typescript-ts-mode)
                ("jsfiddle.net" . js-ts-mode)
                ("w3schools.com" . js-ts-mode)))
(add-to-list 'atomic-chrome-create-file-strategy
             '("~/repos/ts-scratch/src/" :extension
               ("js" "ts" "tsx" "jsx" "cjs" "mjs")))
(atomic-chrome-start-server)

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

 ;; (setq default-frame-alist '((width . 50)
 ;;                             (height . 100)
 ;;                             (menu-bar-lines . 0)
 ;;                             (tool-bar-lines . 0)
 ;;                             (right-divider-width . 1)
 ;;                             (bottom-divider-width . 1)
 ;;                             (vertical-scroll-bars)
 ;;                             (left-fringe . 8)
 ;;                             (right-fringe . 8)))

;; Works at the end?
(after! org
  (set-face-attribute 'org-block nil :background "#2A3339"))

(if (string-equal system-type "windows-nt")
    (progn
      (setq doom-font (font-spec :family "JetBrainsMono NF" :size 25)
            nerd-icons-font-family "JetBrainsMono NF")
      ;; Remember to run doom env from a windows cmd, not from mingw
      (doom-load-envvars-file "~/.emacs.d/.local/env")))
