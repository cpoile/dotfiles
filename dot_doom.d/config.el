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
      (setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'Light))))

(if (string-equal system-type "gnu/linux")
    (progn
      (setq doom-font (font-spec :family "JetBrains Mono" :size 21))))

(if (string-equal system-type "windows-nt")
    (progn
      (setq doom-font (font-spec :family "JetBrainsMono NF Regular" :size 21 :weight 'regular)
            nerd-icons-font-family "JetBrainsMono NF Regular")
      ;;(setq doom-font (font-spec :family "Iosevka" :size 22 :weight 'normal))
      ;; Remember to run doom env from a windows cmd, not from mingw
      (doom-load-envvars-file "~/.emacs.d/.local/env")))

(setq line-spacing 0)
(setq isearch-lazy-count t)
(after! ligature
  (global-ligature-mode 0))

;;
;; If you or (Emacs can't find your font, use 'M-x describe-font' to look them
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
;;(setq display-line-numbers-type 't)
(setq display-line-numbers-type nil)

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
(setq! golden-ratio-scroll-highlight-flag nil)
(add-hook 'golden-ratio-scroll-screen-down-hook 'pulse-momentary-highlight-one-line)
(add-hook 'golden-ratio-scroll-screen-up-hook 'pulse-momentary-highlight-one-line)

;; are we in a terminal?
;; (unless (display-graphic-p)
;;   (getenv-internal "TERM" initial-environment))

;;
;; PROGRAMMING
;;

(use-package! smart-comment
  :bind ("M-;" . smart-comment))

(global-set-key (kbd "C-c ]") 'diff-hl-show-hunk-next)
(global-set-key (kbd "C-c [") 'diff-hl-show-hunk-previous)

(defun cp/find-file (&optional arg)
  "if arg, use regular find file. e.g., C-u C-x C-f"
  (interactive "P")
  (if (or arg (not (doom-project-p)))
          (call-interactively #'find-file)
    (projectile-find-file arg)))

(map! :map global-map
      "C-x C-f" #'cp/find-file)

;;
;; Jai setup
;;

;;(load! "jai-mode.el")
(load! "jai-ts-mode.el")
;; using prog-mode-map for all keybindings
(use-package jai-ts-mode)

(defconst jai-identifier-rx "[[:word:][:multibyte:]_]+")
(defconst jai--defun-rx "\(.*\).*\{")  ;; original
(defconst jai-proc-rx (concat "\\(\\_<" jai-identifier-rx "\\_>\\)\\s *::\\s *\\(?:inline\\)?\\(?:struct\\)?\\s *[({]"))

(defun jai-previous-defun ()
  "Go to previous proc."
  (interactive)
  (beginning-of-line)
  (re-search-backward jai-proc-rx nil t)
  (beginning-of-line))

(defun jai-next-defun ()
  "Go to next proc."
  (interactive)
  (end-of-line)
  (re-search-forward jai-proc-rx nil t)
  (beginning-of-line))

(map! :map jai-ts-mode-map
      "C-M-e" #'jai-next-defun
      "C-M-a" #'jai-previous-defun
      "C-M-l" #'align-regexp)

;; don't add this to jai-ts-mode-hook -- it's already in the mode
(add-hook 'jai-mode-hook (lambda ()
                           (setq-local indent-tabs-mode nil
                                       tab-width 4
                                       indent-line-function 'js-indent-line
                                       electric-indent-chars '(10 127)
                                       block-comment-start "//"
                                       block-comment-end "")))

;; (remove-hook 'jai-mode-hook (lambda ()
;;                               ))

;;
;; Odin setup
;;

(load! "odin-mode.el")  ;; for reference
(load! "odin-ts-mode.el")
;;(package-vc-install "https://git.sr.ht/~mgmarlow/odin-mode")
(use-package odin-ts-mode
  :bind (:map odin-ts-mode-map
      ;;("C-c C-r" . 'odin-run-project)
      ;;("C-c C-c" . 'odin-build-project)
      ;;("C-c C-r" . 'cp/compile)
      ;;("C-c C-c" . 'compile)
      ;;("C-c r t" . 'odin-test-project)
      ))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(odin-ts-mode . "odin"))
  (let ((ols-exec (if (string-equal system-type "darwin") "~/bin/ols" "~/git/ols/ols.exe")))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection ols-exec)
                      :major-modes '(odin-ts-mode)
                      :server-id 'ols
                      :multi-root t)))) ;; This is just so lsp-mode sends the "workspaceFolders" param to the server.
(add-hook 'odin-ts-mode-hook #'lsp)
;;(remove-hook 'odin-ts-mode-hook #'lsp)
(after! compile
  (add-to-list 'compilation-error-regexp-alist '("^\\(.*?\\)(\\([0-9]+\\):\\([0-9]+\\).*" 1 2 3))

  ;; looks like these two did nothing?
  ;(add-to-list 'compilation-error-regexp-alist-alist '(odin "\\[\\([A-Za-z_]+\\.odin\\):\\([0-9]+\\):" 1 2 3))
  ;(add-to-list 'compilation-error-regexp-alist 'odin)
  )

;; Remove annoying lsp crap:
;(setq lsp-ui-sideline-show-diagnostics t)
;(setq lsp-diagnostics-provider :auto)


;; treat _ as part of the word
;; (defvar my-odin-mode-syntax-table
;;   (let ((table (copy-syntax-table (odin-ts-mode--syntax-table))))
;;     (modify-syntax-entry ?_ "w" table)
;;     table))

(add-hook 'odin-ts-mode-hook (lambda ()
                            (setq comment-start "//"
                                  comment-end   "")
                            ;; (setq indent-tabs-mode nil)
                            ;; (setq indent-line-function 'relative-line-indent)
                            (setq lsp-ui-doc-max-height 40)
                            (setq lsp-ui-doc-max-width 150)
                            (lsp)
                            (lsp-completion-mode -1)
                            (setq-local
                             flycheck-check-syntax-automatically '(mode-enabled save)
                             block-comment-start "//"
                             block-comment-end ""
                             )
                            (modify-syntax-entry ?_ "w" odin-ts-mode--syntax-table)
                            ))

;(remove-hook 'odin-ts-mode-hook (car odin-ts-mode-hook))


;; Should be in an after! macro, but we're definitely loading it above, so:
;; TODO: move over to using ts queries like in c-ts-mode below
(defconst odin-identifier-rx "[[:word:][:multibyte:]_]+")
(defun odin-directives-rx (directives)
  (odin-wrap-directive-rx (regexp-opt directives t)))
(defconst odin-proc-directives
  '("#force_inline"
    "#force_no_inline"
    "#type")
  "Directives that can appear before a proc declaration")
(defun odin-wrap-directive-rx (s)
  (concat "\\_<" s "\\>"))
(defconst odin-proc-or-struct-rx (concat "\\(\\_<" odin-identifier-rx "\\_>\\)\\s *::\\s *\\(" (odin-directives-rx odin-proc-directives) "\\)?\\s *\\(\\_<proc\\_>\\|\\_<struct\\_>\\)"))

(defun odin-previous-defun-or-struct ()
  "Go to previous proc."
  (interactive)
  (beginning-of-line)
  (re-search-backward odin-proc-or-struct-rx nil t)
  (beginning-of-line))


(defun odin-next-defun-or-struct ()
  "Go to next proc."
  (interactive)
  (end-of-line)
  (re-search-forward odin-proc-or-struct-rx nil t)
  (beginning-of-line))

(map! :map odin-ts-mode-map
      "C-M-e" #'odin-next-defun-or-struct
      "C-M-a" #'odin-previous-defun-or-struct
      "C-M-l" #'lsp-format-buffer)

;;
;; Programming remapping
;;

;; remove c-i = tab
(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-\S-i] [C-S-i])
;; replace with expand region
(global-set-key (kbd "<C-i>") 'expreg-expand)
(global-set-key (kbd "<C-S-i>") 'expreg-contract)

(map! :map global-map
      "C-x f" #'+workspace/switch-to)

;;
;; Bookmarking with C-S-1..9  and jumping with C-1..0
;;
(defun cp/bmk-set-mark (num)
  "Set mark num"
  (interactive)
  (bookmark-set num))
(defun cp/bmk-jump-mark (num)
  "Jump to mark num"
  (interactive)
  (push-mark)
  (bookmark-jump num))

(global-set-key (kbd "C-!") (lambda () (interactive)(cp/bmk-set-mark "1")))
(global-unset-key (kbd "C-1"))
(global-set-key (kbd "C-1") (lambda () (interactive)(cp/bmk-jump-mark "1")))
(global-unset-key (kbd "C-@"))
(global-set-key (kbd "C-@") (lambda () (interactive)(cp/bmk-set-mark "2")))
(global-unset-key (kbd "C-2"))
(global-set-key (kbd "C-2") (lambda () (interactive)(cp/bmk-jump-mark "2")))
(global-set-key (kbd "C-#") (lambda () (interactive)(cp/bmk-set-mark "3")))
(global-unset-key (kbd "C-3"))
(global-set-key (kbd "C-3") (lambda () (interactive)(cp/bmk-jump-mark "3")))
(global-set-key (kbd "C-$") (lambda () (interactive)(cp/bmk-set-mark "4")))
(global-unset-key (kbd "C-4"))
(global-set-key (kbd "C-4") (lambda () (interactive)(cp/bmk-jump-mark "4")))
(global-set-key (kbd "C-%") (lambda () (interactive)(cp/bmk-set-mark "5")))
(global-unset-key (kbd "C-5"))
(global-set-key (kbd "C-5") (lambda () (interactive)(cp/bmk-jump-mark "5")))
(global-set-key (kbd "C-^") (lambda () (interactive)(cp/bmk-set-mark "6")))
(global-unset-key (kbd "C-6"))
(global-set-key (kbd "C-6") (lambda () (interactive)(cp/bmk-jump-mark "6")))
(global-set-key (kbd "C-&") (lambda () (interactive)(cp/bmk-set-mark "7")))
(global-unset-key (kbd "C-7"))
(global-set-key (kbd "C-7") (lambda () (interactive)(cp/bmk-jump-mark "7")))
(global-set-key (kbd "C-*") (lambda () (interactive)(cp/bmk-set-mark "8")))
(global-unset-key (kbd "C-8"))
(global-set-key (kbd "C-8") (lambda () (interactive)(cp/bmk-jump-mark "8")))
(global-set-key (kbd "C-(") (lambda () (interactive)(cp/bmk-set-mark "9")))
(global-unset-key (kbd "C-9"))
(global-set-key (kbd "C-9") (lambda () (interactive)(cp/bmk-jump-mark "9")))
(global-set-key (kbd "C-)") (lambda () (interactive)(cp/bmk-set-mark "0")))
(global-unset-key (kbd "C-0"))
(global-set-key (kbd "C-0") (lambda () (interactive)(cp/bmk-jump-mark "0")))

;;
;; Perspective -- put most recently used persp on top -- doesn't work, abandoned.
;;

;; (defun cp/switch-first-two-persp (names)
;;   (if (null (cl-rest names))
;;       names
;;     (let* ((first (nth 0 names))
;;            (second (nth 1 names))
;;            (rest (nthcdr 2 names)))
;;       (pushnew! rest first second))))

;; (cp/switch-first-two-persp (list "first" "second" "third" "forth"))
;; (cp/switch-first-two-persp (list "first"))

;; (after! persp-mode
;;   (add-hook persp-before-switch-functions '))

;;
;; Perspective mode -- autoload dotfiles
;;

;;(after! persp-mode
;;  (add-hook 'persp-mode-hook
;;          (lambda ()
;;              (interactive)
;;              (doom/quickload-session t)
;;              (+workspace/switch-to "dotfiles"))))

;;
;; Misc settings
;;
(back-button-mode 1)
(setq doom-modeline-persp-name t)
(setq doom-modeline-display-default-persp-name t)
(setq! mark-even-if-inactive nil)
(whole-line-or-region-global-mode)
(setq global-mark-ring-max 32)
(setq mark-ring-max 32)

;; Magit
(after! magit
  (setq!
   magit-diff-refine-hunk 'all
   git-commit-summary-max-length 68
   magit-ediff-dwim-show-on-hunks t
   magit-diff-hide-trailing-cr-characters t
   ))
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
(set-popup-rules!
  '(("^\\*cargo-run" :size 0.4 :slot -1 :ttl t)))
;;(setq! lsp-ui-doc-enable nil)

(after! rust-mode
  (modify-syntax-entry ?_ "w" rust-mode-syntax-table))

(after! lsp-ui-doc
  (setq! lsp-ui-doc-max-height 40)
  (setq! lsp-ui-doc-max-width 150))

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

(defun cp/c-next-defun ()
  (interactive)
  (c-beginning-of-defun -1))

(map! :map c-mode-map
      "C-q" #'+lookup/documentation
      "C-M-l" #'lsp-format-buffer
      "C-c C-c" #'compile
      "C-c C-r" #'recompile
      "C-M-e" #'cp/c-next-defun)

;;
;; c-ts-mode
;;

(defun cp/c-ts-next-defun ()
  (interactive)
  (if (treesit-beginning-of-defun -1)
      (forward-line)))

(defun cp/c-ts-previous-defun ()
  (interactive)
  (let ((prev (lambda (numtimes)
                (interactive)
                (if (treesit-beginning-of-defun numtimes)
                    (forward-line))))
        (cur (point)))
    (if (= cur (progn
                 (funcall prev 1)
                 (point)))
        (funcall prev 2))))

(map! :map c-ts-mode-map
      ;"C-q" #'+lookup/documentation
      "C-M-l" #'lsp-format-buffer
      "C-c C-c" #'compile
      "C-c C-r" #'recompile
      "C-M-a" #'cp/c-ts-previous-defun
      "C-M-e" #'cp/c-ts-next-defun)

;; TODO: make it so that using C-u shows all buffers (except that's used... so, something else)
(map! "C-x C-b" #'(lambda (arg)
                    (interactive "P")
                    (with-persp-buffer-list () (ibuffer arg))))

(remove-hook 'c-mode-common-hook 'rainbow-delimiters-mode nil)

(after! c-ts-mode
  (add-hook 'c-ts-mode-hook 'lsp-deferred)
  )

;; (defun c-ts-mode-call-hook () (run-hooks 'c-mode-hook))
;; (add-hook 'c-ts-mode-hook #'c-ts-mode-call-hook)

(setq next-error-message-highlight t)

(setq c-ts-mode-indent-offset 4)

;;
;; LSP mode
;;

(with-eval-after-load 'lsp-mode
  (setq lsp-enable-symbol-highlighting nil))

;;
;; Find clangd
;;
;;(executable-find "clangd")

(setq lsp-clients-clangd-args '("--log=verbose" "-j=16" "--header-insertion-decorators=0"))
;;(setq lsp-clients-clangd-args '("-j=16" "--header-insertion-decorators=0"))

;;
;; Fix clangd -- wow, this took awhile.
;; Putting it here to survive rebuilds.
;; Put it into lsp-mode.el after rebuilding (because there's a macro in there that doesn't work out here).
;; Replace lsp--locations-to-xref-items, and add the other two above it.
;; Then run M-x emacs-lisp-byte-compile to compile it.

;; (defun lsp--lowercase-loc (loc)
;;   "See: https://github.com/clangd/clangd/issues/2195 ,
;; https://github.com/clangd/vscode-clangd/issues/687#issuecomment-2365439284"
;;   (-let [(filename . matches) loc]
;;     (cons
;;      (concat (downcase (substring filename 0 1)) (substring filename 1))
;;      matches)))

;; (defun lsp--is-dup (left right)
;;   "Same filename?"
;;   (string= (car left) (car right)))

;; (defun lsp--locations-to-xref-items (locations)
;;   "Return a list of `xref-item' given LOCATIONS, which can be of
;; type Location, LocationLink, Location[] or LocationLink[]."
;;   (setq locations
;;         (pcase locations
;;           ((seq (or (Location)
;;                     (LocationLink)))
;;            (append locations nil))
;;           ((or (Location)
;;                (LocationLink))
;;            (list locations))))

;;   (cl-labels ((get-xrefs-in-file
;;                 (file-locs)
;;                 (-let [(filename . matches) file-locs]
;;                   (condition-case err
;;                       (let ((visiting (find-buffer-visiting filename))
;;                             (fn (lambda (loc)
;;                                   (lsp-with-filename filename
;;                                     (lsp--xref-make-item filename
;;                                                          (lsp--location-range loc))))))
;;                         (if visiting
;;                             (with-current-buffer visiting
;;                               (seq-map fn matches))
;;                           (when (file-readable-p filename)
;;                             (with-temp-buffer
;;                               (insert-file-contents-literally filename)
;;                               (seq-map fn matches)))))
;;                     (error (lsp-warn "Failed to process xref entry for filename '%s': %s"
;;                                      filename (error-message-string err)))
;;                     (file-error (lsp-warn "Failed to process xref entry, file-error, '%s': %s"
;;                                           filename (error-message-string err)))))))

;;     (-as-> locations locs
;;          (seq-sort #'lsp--location-before-p locs)
;;          (seq-group-by (-compose #'lsp--uri-to-path #'lsp--location-uri) locs)
;;          ;; so that we display the nicer relative location, rather than the full path
;;          (seq-map #'lsp--lowercase-loc locs)
;;          (cl-remove-duplicates locs :test #'lsp--is-dup)
;;          (seq-map #'get-xrefs-in-file locs)
;;          (apply #'nconc locs))))

;;
;; RemedyBg controls
;;

(defun remedy-run ()
  (interactive)
  (shell-command "remedybg.exe bring-debugger-to-foreground")
  (async-shell-command "remedybg.exe start-debugging")
  ;;(make-process "*remedy-out*" "*remedy-out*" "cmd.exe" "-c" "remedybg.exe start-debugging")
  )

(defun remedy-run-to-cursor ()
  (interactive)
  (let* ((name (buffer-file-name))
         (linenum (line-number-at-pos))
         (command (format "remedybg.exe run-to-cursor %s %d &" name linenum)))
    (progn
      (message (format "running: %s" command))
      (shell-command command)
      (shell-command "remedybg.exe bring-debugger-to-foreground"))))

(defun raddbg-run-to-cursor ()
  (interactive)
  (let* ((name (buffer-file-name))
         (linenum (line-number-at-pos))
         (command (format "raddbg --ipc run_to_line %s:%d &" name linenum)))
    (progn
      (message (format "running: %s" command))
      (shell-command command))))

(defun remedy-open-to-cursor ()
  (interactive)
  (let* ((name (buffer-file-name))
         (linenum (line-number-at-pos))
         (command (format "remedybg.exe open-file %s %d &" name linenum))
         (prev-cmd compile-command))
    (progn
      (shell-command command)
      (shell-command "remedybg.exe bring-debugger-to-foreground")
      (setq-local compile-command prev-cmd))))

(defun remedy-add-breakpoint ()
  (interactive)
  (let* ((name (buffer-file-name))
         (linenum (line-number-at-pos))
         (command (format "remedybg.exe add-breakpoint-at-file %s %d &" name linenum))
         (prev-cmd compile-command))
    (progn
      (shell-command command)
      (setq-local compile-command prev-cmd))))

(defun remedy-remove-all-breakpoints ()
  (interactive)
  (let* ((name (buffer-file-name))
         (linenum (line-number-at-pos))
         (command (format "remedybg.exe remove-all-breakpoints &" name linenum))
         (prev-cmd compile-command))
    (progn
      (shell-command command)
      (setq-local compile-command prev-cmd))))


;;
;; .dir-locals.el helpers  (from https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables)

(defun cp/reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun cp/reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (cp/reload-dir-locals-for-current-buffer))))))

(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook 'after-save-hook
                        'cp/reload-dir-locals-for-all-buffer-in-this-directory
                        nil t))))

;; add to your project's .dir-locals.el, eg.
;; ((jai-ts-mode . ((custom-compile-cmd . "jai c:/Users/Chris/git/jai/handmade_jai/build_handmade.jai")
                 ;; (custom-run-all-cmd . "c:/Users/Chris/git/jai/handmade_jai/build/handmade.exe")
                 ;; (custom-run-test-cmd . "c:/Users/Chris/git/jai/handmade_jai/build/handmade.exe test")
                 ;; (file-to-compile-cmd . (("win32_handmade.jai" . (compile "jai c:/Users/Chris/git/jai/handmade_jai/build_win32_handmade.jai")))))))

(defun cp/compile ()
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (file-to-cmd-alist (if (boundp 'file-to-compile-cmd)
                                file-to-compile-cmd
                              nil))
         (default-cmd (if (boundp 'custom-compile-cmd)
                          '(compile custom-compile-cmd)
                        '(call-interactively 'compile)))
         (cmd (or (cdr (assoc file-name file-to-cmd-alist))
                         default-cmd)))
    (eval cmd)))

(defun cp/run-all ()
  (interactive)
  (if (boundp 'custom-run-all-cmd)
      (compile custom-run-all-cmd)
    (call-interactively 'compile)))

(defun cp/compile-all ()
  (interactive)
  (if (boundp 'compile-all-cmd)
      (compile compile-all-cmd)
    (call-interactively 'compile)))

;; (defun cp/run-tests ()
;;   (interactive)
;;   (if (and (boundp 'custom-run-test-cmd) (boundp 'custom-compile-cmd))
;;       (progn
;;         (compile custom-compile-cmd)
;;         (compilation-start custom-run-test-cmd nil nil nil t))
;;     (if (boundp 'custom-run-test-cmd)
;;         (compile custom-run-test-cmd))
;;     (call-interactively 'compile)))

(defun cp/run-tests ()
  (interactive)
    (if (boundp 'custom-run-test-cmd)
        (compile custom-run-test-cmd)
    (call-interactively 'compile)))

;;
;; To go to matching brace with a single key
;;

(defun cp/matching-brace ()
  (interactive)
  (push-mark)
  (if (= (char-before) ?\})
      (sp-backward-sexp)
    (if (= (char-after) ?\{)
        (sp-forward-sexp)
      (if (= (char-before) ?\{)
          (progn (backward-char)
                 (sp-forward-sexp))
        (sp-backward-up-sexp)))))

(defun cp/go-to-def-or-ref ()
  (interactive)
  (let ((cur (line-number-at-pos)))
    (if (= cur (progn
                 (call-interactively '+lookup/definition)
                 (line-number-at-pos)))
        (call-interactively '+lookup/references))))

(defun cp/tab-dwim ()
  "Call dabbrev-expand if point is within a word,
    otherwise call normal tab command."
  (interactive)
  (if (looking-back "[[:word:]_\\.]" (1- (point)))
      ;;(dabbrev-expand nil)
      (+company/complete)
    (indent-for-tab-command)))

;; tab was indent-for-tab-command
;;(global-set-key (kbd "TAB") 'cp/tab-dwim)

;;
;; Format enclosing region
;;
(defun cp/align-brace-content ()
  "Execute OPERATION on the content between braces, excluding brace lines."
  (interactive)
  (save-excursion
    (cp/matching-brace)
    (forward-line 1)
    (let ((begin (point)))
      (cp/matching-brace)
      (cp/matching-brace)
      (beginning-of-line)
      (set-mark (point))
      (goto-char begin)
      (align-regexp (region-beginning)
                    (region-end)
                    (concat "\\(\\s-*\\)" (read-string "Align regexp: " ":" nil ":"))))))

(defun cp/remove-hat-M ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\^M$" nil t)
      (replace-match "" nil nil))))

(advice-add 'diff-hl-revert-hunk :after #'cp/remove-hat-M)

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

(after! org
  (setq org-startup-folded 'content)
  (setq org-use-speed-commands t))

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
  (let ((current-buffer (buffer-name))
        (pos (point)))
    (bury-buffer)
    (switch-to-buffer-other-window current-buffer)
    (goto-char pos)))

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
  (setq company-global-modes '(not org-mode))
  (setq company-idle-delay nil)
  (map! :map company-active-map "<tab>" #'company-complete-selection)
  )


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

;; (defun cp/frame-resize-init (&optional arg)
;;   "set the frame size for editing using emacs chrome (it always starts too small)"
;;   (when (display-graphic-p arg)
;;     (set-frame-size arg 80 24)))

;; (add-hook 'after-make-frame-functions 'cp/frame-resize-init)

(defun cp/frame-scroll-bar (&optional arg)
  "turn on the scroll bar"
  (when (display-graphic-p)
    (set-scroll-bar-mode t)))
(add-hook 'after-make-frame-functions 'cp/frame-scroll-bar)
(set-scroll-bar-mode t)

;;
;; Treemacs
;;
;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay        0.5
;;           treemacs-directory-name-transformer      #'identity
;;           treemacs-display-in-side-window          t
;;           treemacs-eldoc-display                   'simple
;;           treemacs-file-event-delay                2000
;;           treemacs-file-extension-regex            treemacs-last-period-regex-value
;;           treemacs-file-follow-delay               0.2
;;           treemacs-file-name-transformer           #'identity
;;           treemacs-follow-after-init               t
;;           treemacs-expand-after-init               t
;;           treemacs-find-workspace-method           'find-for-file-or-pick-first
;;           treemacs-git-command-pipe                ""
;;           treemacs-goto-tag-strategy               'refetch-index
;;           treemacs-header-scroll-indicators        '(nil . "^^^^^^")
;;           treemacs-hide-dot-git-directory          t
;;           treemacs-indentation                     2
;;           treemacs-indentation-string              " "
;;           treemacs-is-never-other-window           t
;;           treemacs-max-git-entries                 5000
;;           treemacs-missing-project-action          'ask
;;           treemacs-move-files-by-mouse-dragging    t
;;           treemacs-move-forward-on-expand          nil
;;           treemacs-no-png-images                   nil
;;           treemacs-no-delete-other-windows         t
;;           treemacs-project-follow-cleanup          nil
;;           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                        'left
;;           treemacs-read-string-input               'from-child-frame
;;           treemacs-recenter-distance               0.1
;;           treemacs-recenter-after-file-follow      nil
;;           treemacs-recenter-after-tag-follow       nil
;;           treemacs-recenter-after-project-jump     'always
;;           treemacs-recenter-after-project-expand   'on-distance
;;           treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
;;           treemacs-project-follow-into-home        nil
;;           treemacs-show-cursor                     nil
;;           treemacs-show-hidden-files               t
;;           treemacs-silent-filewatch                nil
;;           treemacs-silent-refresh                  nil
;;           treemacs-sorting                         'alphabetic-asc
;;           treemacs-select-when-already-in-treemacs 'move-back
;;           treemacs-space-between-root-nodes        t
;;           treemacs-tag-follow-cleanup              t
;;           treemacs-tag-follow-delay                1
;;           treemacs-text-scale                      -1
;;           treemacs-user-mode-line-format           nil
;;           treemacs-user-header-line-format         nil
;;           treemacs-wide-toggle-width               70
;;           treemacs-width                           22
;;           treemacs-width-increment                 1
;;           treemacs-width-is-initially-locked       t
;;           treemacs-workspace-switch-cleanup        nil)

;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)

;;     (treemacs-follow-mode t)
;;     (treemacs-project-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode 'always)
;;     (when treemacs-python-executable
;;       (treemacs-git-commit-diff-mode t))

;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple)))

;;     (treemacs-hide-gitignored-files-mode nil))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ;("C-x t 1"   . treemacs-delete-other-windows)
;;         ("s-1"   . treemacs)
;;         ("C-x t d"   . treemacs-select-directory)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;;   :ensure t)

;; (use-package treemacs-magit
;;   :after (treemacs magit)
;;   :ensure t)


(after! treemacs
  (map! :map global-map "s-1" #'treemacs)
  (treemacs-create-theme "simple"
    :config
    (progn
      (treemacs-create-icon :icon "- " :extensions (root-open) :fallback 'same-as-icon)
      (treemacs-create-icon :icon "+ " :extensions (root-closed) :fallback 'same-as-icon)
      (treemacs-create-icon :icon "- " :extensions (dir-open) :fallback 'same-as-icon)
      (treemacs-create-icon :icon "+ " :extensions (dir-closed) :fallback 'same-as-icon)
      (treemacs-create-icon :icon "  " :extensions (fallback) :fallback 'same-as-icon)))
  (treemacs-load-theme "simple"))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-text-scale    0
          treemacs-width         20
          treemacs-no-png-images t )
    (treemacs-resize-icons 18))
    :bind
  (:map global-map
        ;("M-0"       . treemacs-select-window)
        ;("C-x t 1"   . treemacs-delete-other-windows)
        ("s-1"   . treemacs)
        ;("C-x t d"   . treemacs-select-directory)
        ;("C-x t B"   . treemacs-bookmark)
        ;("C-x t C-t" . treemacs-find-file)
        ;("C-x t M-t" . treemacs-find-tag)
        ))

;;
;; Keybindings
;;

;; move windows using S-up, S-down, S-left, S-right
(after! windmove
  (windmove-default-keybindings))
(map! "C-c <right>" #'windmove-swap-states-right)
(map! "C-c <left>" #'windmove-swap-states-left)
(map! "C-c <up>" #'windmove-swap-states-up)
(map! "C-c <down>" #'windmove-swap-states-down)

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
;; (map! "C-M-[" #'back-button-global-backward)
;; (map! "C-M-]" #'back-button-global-forward)
(map! "M-[" #'back-button-global-backward)
(map! "M-]" #'back-button-global-forward)
(map! "M-{" #'back-button-local-backward)
(map! "M-}" #'back-button-local-forward)

(after! nav-flash
  (setq nav-flash-delay 0))

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
;; (global-set-key (kbd "C-o") '+default/newline-above)
;; (global-set-key (kbd "C-S-o") 'open-line)

;; this was getting pressed by accident when hitting C-x o quickly: ...?
;;(global-set-key (kbd "C-x M-o") 'other-window)

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

(map! :map undo-fu-mode-map "M-_")

;;
;; Multiple cursors
;;

(defun mc-mark-next-like-this-symbol-then-cycle-forward (arg)
  "Mark next like this then cycle forward, take interactive ARG."
  (interactive "p")
  (call-interactively 'mc/mark-next-like-this-symbol)
  (call-interactively 'mc/cycle-forward))

(defun mc-skip-to-next-like-this-then-cycle-forward (arg)
  "Skip to next like this then cycle forward, take interactive ARG."
  (interactive "p")
  (call-interactively 'mc/cycle-backward)
  (call-interactively 'mc/skip-to-next-like-this)
  (call-interactively 'mc/cycle-forward))

(defun mc-mark-previous-like-this-then-cycle-backward (arg)
  "Mark previous like this then cycle backward take interactive ARG."
  (interactive "p")
  (call-interactively 'mc/mark-previous-like-this)
  (call-interactively 'mc/cycle-backward))

(defun mc-skip-to-previous-like-this-then-cycle-backward (arg)
  "Skip to previous like this then cycle backward take interactive ARG."
  (interactive "p")
  (call-interactively 'mc/cycle-forward)
  (call-interactively 'mc/skip-to-previous-like-this)
  (call-interactively 'mc/cycle-backward))

(defun mc-unmark-current (arg)
  (interactive "p")
  (call-interactively 'mc/cycle-backward)
  (call-interactively 'mc/unmark-next-like-this)
  (call-interactively 'mc/skip-to-next-like-this))

(use-package! multiple-cursors
  :ensure   t
  :bind (("C-M-SPC" . set-rectangular-region-anchor)
         ("C-M->" . mc/mark-next-like-this)
         ("C-M-<" . mc/mark-previous-like-this)
         ("C-c C-." . mc/mark-all-like-this-in-defun)
         ("C-c C-SPC" . mc/edit-lines)
         ("M-j" . mc-mark-next-like-this-symbol-then-cycle-forward)
         ("M-J" .  mc-unmark-current)
         ("M-C-S-j" . mc-skip-to-next-like-this-then-cycle-forward)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

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
(load! "dumb-jump.el")
(after! dumb-jump
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-force-searcher 'rg
        dumb-jump-rg-search-args "--pcre2 --type-add \"jai:*.jai\"")
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;(setq dumb-jump-debug t)
;;(setq dumb-jump-debug nil)
;;(setq dumb-jump-force-searcher nil)

;;
;; NOTE: this is so that if we goto next-error and the resulting error is displayed in our current window,
;;       but the other window already had that file open, move the buffer the other window.
;;       This is kind of hacky. It would probably be better to change the compilation-goto-locus itself.

(defun get-next-non-temp-buffer-window ()
  "Return the next window not displaying a temporary buffer."
  (seq-find
   (lambda (win)
     (and (not (eq win (selected-window)))
          (not (string-match "^[ *]" (buffer-name (window-buffer win))))))
   (window-list nil 'f)))

(defun cp/move-to-already-opened (func &rest args)
  (interactive)
  "If we searched and found a ref in the file that is already opened, move it to that window.
Unless we started with the file opened in both windows, then move
other window to new position and move current window to original
position. -- why? -- only for xref, not for next-error... we want
next-error to be in current window, right?
 TODO: use excursion,or transient?"
    ;; for debugging:
    ;; (message (format "after! other buf: %s  cur buf: %s  window-list: %s"
    ;;                  (buffer-file-name (window-buffer (get-next-non-temp-buffer-window)))
    ;;                  (buffer-file-name (current-buffer))
    ;;                  (window-list nil 'f)))
  (let* ((orig-cur-buffer-name (buffer-file-name (current-buffer)))
         (the-other-window (get-next-non-temp-buffer-window))
         (the-other-buffer (if the-other-window
                               (window-buffer (get-next-non-temp-buffer-window))
                             nil))
         (other-buffer-name (if the-other-window
                                (buffer-file-name the-other-buffer)
                              nil))
         (started-same (string= orig-cur-buffer-name other-buffer-name))
         (orig-point (point)))
    (apply func args)
    (let ((ended-same (string= (buffer-file-name (current-buffer))
                               other-buffer-name)))
      (if (and started-same ended-same)
          (let ((new-point (point)))
            (goto-char orig-point)
            (select-window the-other-window)
            (goto-char new-point))
            ;; (save-selected-window
            ;;   (select-window the-other-window)
            ;;   (goto-line line-num))
            ;; (goto-char orig-point))
        (if ended-same (cp/move-buffer-to-other-window))))))

(defun cp/move-to-already-opened-for-next-error (func &rest args)
  (interactive)
  "If we searched and found a ref in the file that is already opened, move it to that window.
Unless we started with the file opened in both windows, then move
the current window only. TODO: use excursion,or transient?"
  (let* ((orig-cur-buffer-name (buffer-file-name (current-buffer)))
         (the-other-window (get-next-non-temp-buffer-window))
         (the-other-buffer (if the-other-window
                               (window-buffer (get-next-non-temp-buffer-window))
                             nil))
         (other-buffer-name (if the-other-window
                                (buffer-file-name the-other-buffer)
                              nil))
         (started-same (string= orig-cur-buffer-name other-buffer-name))
         (orig-point (point)))
    (apply func args)
    (let ((ended-same (string= (buffer-file-name (current-buffer))
                               other-buffer-name)))
      (if (not (and started-same ended-same))
          (if ended-same (cp/move-buffer-to-other-window))))))

(defun cp/next-error ()
  "for some reaton, when we adive-add'ed compilation-goto-locus with :around cp/move-to-already-opened,
the current-buffer was nil. So use this wrapper.  This is bound to M-g M-n."
  (interactive)
  (cp/move-to-already-opened-for-next-error #'next-error))


;; Don't need it anymore, after cp/next-errror:
;;(advice-add 'compilation-goto-locus :around #'cp/move-to-already-opened)
;;
;; clean up during debugging:
;;(advice-remove 'compilation-goto-locus #'cp/move-to-already-opened)
;;

;;
;; XRef jumps should recenter nearer to the top of the window so we can see more of the function
;;
;; only for new buffers:
;; (defun cp/xref-recenter-in-new-buffer (func &rest args)
;;   (let ((original-buf (current-buffer)))
;;     (apply func args)
;;     (unless (eq (current-buffer) original-buf)
;;       (recenter-top-bottom 20))))

;; all buffers:
(defun cp/jump-recenter (func &optional &rest args)
  ;; TODO: only recenter if we moved far away?
  (interactive)
  (let ((cur (line-number-at-pos)))
    (if (not (= cur (progn
                      (apply func args)
                      (line-number-at-pos))))
        (recenter-top-bottom 24))))

;; turn off if it causes you to jump when you're looking at the destination
(advice-add 'cp/go-to-def-or-ref :around 'cp/jump-recenter)
(advice-add 'cp/go-to-def-or-ref :around 'cp/move-to-already-opened)
(advice-add 'better-jumper-jump-backward :around 'cp/jump-recenter)

;;(advice-remove 'cp/go-to-def-or-ref 'cp/move-to-already-opened)
;;(advice-remove 'cp/go-to-def-or-ref 'cp/jump-recenter)

;;
;; Do the same for consult-imenu
;;
(advice-add 'consult-imenu :around 'cp/jump-recenter)


(map! :map prog-mode-map
      "C-c C-c"    #'compile
      "C-c C-r"    #'cp/compile
      ;;"C-c r a"    #'cp/run-all
      "C-c r a"    #'cp/compile-all
      "C-c r t"    #'cp/run-tests
      "C-c r r"    #'remedy-run
      "C-c r c"    #'remedy-run-to-cursor
      ;;"C-c r c"    #'raddbg-run-to-cursor
      "C-c r o"    #'remedy-open-to-cursor
      "C-c r b"    #'remedy-add-breakpoint
      "C-c r B"    #'remedy-remove-all-breakpoints
      "C-q"        #'lsp-ui-doc-show
      "M-s"        #'+default/search-project
      "M-_"        #'+fold/close-all
      "M-+"        #'+fold/open-all
      "M--"        #'+fold/close
      "M-="        #'+fold/open
      "M-m"        #'cp/matching-brace
      "M-."        #'cp/go-to-def-or-ref
      "M-<return>" #'+default/newline-above
      "<tab>"      #'cp/tab-dwim
      "M-i"        #'consult-imenu
      "C-M-S-L"    #'cp/align-brace-content
      "M-g M-n"    #'cp/next-error
      )


;;
;; Show which-function-mode in the modeline
;;
(which-function-mode)
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))
;; (setq mode-line-misc-info
;;             ;; We remove Which Function Mode from the mode line, because it's mostly
;;             ;; invisible here anyway.
;;             (assq-delete-all 'which-func-mode mode-line-misc-info))

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
;; (defun set-exec-path-from-shell-PATH ()
;;   "Sets the exec-path to the same value used by the user shell"
;;   (let ((path-from-shell
;;          (replace-regexp-in-string
;;           "[[:space:]\n]*$" ""
;;           (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))
;; ;; call function now

;; (set-exec-path-from-shell-PATH)

;;
;; To make TODO: highlight the whole line:
;;
(after! hl-todo
  (defun hl-todo--setup-regexp ()
    "Setup keyword regular expression.
     See the function `hl-todo--regexp'."
    (when-let ((bomb (assoc "???" hl-todo-keyword-faces)))
      ;; If the user customized this variable before we started to
      ;; treat the strings as regexps, then the string "???" might
      ;; still be present.  We have to remove it because it results
      ;; in the regexp search taking forever.
      (setq hl-todo-keyword-faces (delete bomb hl-todo-keyword-faces)))
    (setq hl-todo--regexp
          (concat "\\(\\<"
                  "\\(" (mapconcat #'car hl-todo-keyword-faces "\\|") "\\)"
                  "\\>"
                  "[^\"\n]*\\)")))
  (setq hl-todo--keywords
        `((,(lambda (bound) (hl-todo--search nil bound))
     (1 (hl-todo--get-face) prepend t)))))

;; TODO: this is a test
(after! hl-todo
  (defface my-TODO-face
    '((t :background "#52442E" :foreground "#D3C6AA" :inherit (hl-todo)))
    "Face for highlighting the TODO keyword."
    :group 'hl-todo)

  ;; remove existing todo:
  (setq hl-todo-keyword-faces (cl-remove "TODO" hl-todo-keyword-faces :test 'equal :key 'car))
  (add-to-list 'hl-todo-keyword-faces '("TODO" . my-TODO-face)))


;; Clippety
(unless (display-graphic-p)
  (use-package! clipetty
    ;; if you omit :defer, :hook, :commands, or :after, then the package is loaded
    ;; immediately. By using :hook here, the `hl-todo` package won't be loaded
    ;; until prog-mode-hook is triggered (by activating a major mode derived from
    ;; it, e.g. python-mode)
    :ensure t
    ;;:bind ("M-w" . clipetty-kill-ring-save)
    :config
    ;; code here will run after the package is loaded
    (setq clipetty-assume-nested-mux t)))

;; was using this, but windows had `whole-line-or-region-kill-ring-save' -- shouldn't that be enough?
;; copy whole line if none is selected
;; (defun my/copy-line ()
;;   (interactive)
;;     (if mark-active
;;         (kill-ring-save (region-beginning) (region-end))
;;       (kill-ring-save (pos-bol) (pos-eol)))
;;   (message "1 line copied"))

;; (map! "M-w" #'my/copy-line)

;; Start almost full screen on mac
(if (string-equal system-type "darwin")
    (progn
      (setq frame-inhibit-implied-resize t)  ;; prevent resize window on startup
      (set-frame-height (window-frame) 65)
      (set-frame-width (window-frame) 220)))

(if (string-equal system-type "windows-nt")
    (progn
      (toggle-frame-maximized (window-frame))
      (setq default-frame-alist '((scroll-bar-width . 6)))))

;;(set-frame-parameter (selected-frame) 'scroll-bar-width 6)

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
;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
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

;; to install for c and c++, go only:
;;
;; (setq treesit-language-source-alist
;;       '((c "https://github.com/tree-sitter/tree-sitter-c")
;;         (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;         (go "https://github.com/tree-sitter/tree-sitter-go")))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

;; to install for jai:
;;
;; (setq treesit-language-source-alist
;;       '((jai "https://github.com/constantitus/tree-sitter-jai")))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

;; to install for Odin:
;; (setq treesit-language-source-alist
;;       '((odin "https://github.com/tree-sitter-grammars/tree-sitter-odin")))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(odin-mode . odin-ts-mode))
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
(add-to-list 'major-mode-remap-alist '(jai-mode . jai-ts-mode))  ;; jai-ts-mode does this already?

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

;; is it enabled?
(defun treesit-enabled-p ()
  "Checks if the current buffer has a treesit parser."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-at (point))))


(add-load-path! "~/.doom.d/combobulate/")
(require 'combobulate)


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
(require 'pulse)
(after! pulse
  (defun my/pulse-current-region (&rest _)
    "Pulse the current implicit or active region."
    (if mark-active
        (pulse-momentary-highlight-region (region-beginning) (region-end))
      (pulse-momentary-highlight-one-line)))
  (setq pulse-flag 't)
  (advice-add #'kill-ring-save :before #'my/pulse-current-region))

(if (string-equal system-type "gnu/linux")
    (progn
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
      (atomic-chrome-start-server)))

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter) ;; doesn't work?

;(setq compilation-max-output-line-length nil)

;; (defun colorize-compilation-buffer ()
;;   (ansi-color-apply-on-region compilation-filter-start (point)))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;;(remove-hook 'compilation-filter-hook 'colorize-compilation-buffer)

 ;; (setq default-frame-alist '((width . 50)
 ;;                             (height . 100)
 ;;                             (menu-bar-lines . 0)
 ;;                             (tool-bar-lines . 0)
 ;;                             (right-divider-width . 1)
 ;;                             (bottom-divider-width . 1)
 ;;                             (vertical-scroll-bars)
 ;;                             (left-fringe . 8)
 ;;                             (right-fringe . 8)))

;;
;; Olivetti mode for writing
;; does it need to be required?
;;(require 'olivetti)


;;
;; ediff region with clipboard
;;

(defun cp/ediff-region-with-clipboard ()
  "Compare the selected region with the contents of the system clipboard using ediff."
  (interactive)
  (unless (region-active-p)
    (error "No region selected"))

  (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (clipboard-text (current-kill 0))
         (buf1 (generate-new-buffer "*Region*"))
         (buf2 (generate-new-buffer "*Clipboard*")))

    (with-current-buffer buf1
      (insert region-text)
      (visual-line-mode 1))

    (with-current-buffer buf2
      (insert clipboard-text)
      (visual-line-mode 1))

    (ediff-buffers buf1 buf2)))

(defun cp/kill-ediff-buffers ()
  (let ((bufA (buffer-name ediff-buffer-A))
        (bufB (buffer-name ediff-buffer-B))
        (bufC (buffer-name ediff-buffer-C))
        (reg "^\\*.*\\*$"))
    (if (string-match reg bufA)
        (kill-buffer ediff-buffer-A))
    (if (string-match reg bufB)
        (kill-buffer ediff-buffer-B))
    (if (string-match reg bufC)
        (kill-buffer ediff-buffer-C))))

;; adding the hook after init, so that it's last. See at the end of this file.

(global-set-key (kbd "C-c C-d") 'cp/ediff-region-with-clipboard)

;(remove-hook 'ediff-quit-hook (car ediff-quit-hook))
;(remove-hook 'ediff-quit-hook 'cp/kill-ediff-buffers)

(after! dired
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode -1))))


;;(global-hl-line-mode)

;;
;; RUN AFTER EVERYTHING (e.g., customize theme, orgmode, settings that get overwritten)
;;
(add-hook 'emacs-startup-hook
          (lambda ()
            (progn
              ;;(set-face-attribute 'font-lock-comment-face nil :foreground "#586D36")
              ;;(set-face-attribute 'font-lock-comment-face nil :foreground "#637B3D")
              ;;(set-face-attribute 'font-lock-comment-face nil :foreground "#6E8943")
              (set-face-attribute 'font-lock-comment-face nil :foreground "#6E7B85")
                                        ;(set-face-attribute 'isearch nil :background "#76875a" :foreground "#F7ECD2")
                                        ;(set-face-attribute 'isearch nil :background "#726C5D" :foreground "#E7DCC4")
              (set-face-attribute 'isearch nil :background "#9E5179" :foreground "#E7DCC4")
                                        ;(set-face-attribute 'lazy-highlight nil :background "#92a375" :foreground "#F7ECD2")
              (set-face-attribute 'lazy-highlight nil :background "#76875a" :foreground "#F7ECD2")

              (set-face-attribute 'font-lock-preprocessor-face nil :foreground "#A9C181" :weight 'bold) ;; was red: #E67C7F, then blue: #7FBCB4
              (set-face-attribute 'font-lock-keyword-face nil :foreground "#A9C181") ;; was: ...forget green I think, then B5C49E
              (set-face-attribute 'font-lock-type-face nil :foreground "#7FBCB4") ;; was: #DDBD7F A7C080
              (set-face-attribute 'font-lock-function-name-face nil :foreground "#EDC77A") ;; was: #E7DCC4 EDC77A
              (set-face-attribute 'font-lock-function-call-face nil :foreground "#C4CD9F") ;; was: #E7DCC4 EDC77A #f1d396 #CACFA5
              (set-face-attribute 'font-lock-variable-name-face nil :foreground "#D3C6AA") ;; was: #efcadb D3C6AA
              (set-face-attribute 'font-lock-operator-face nil :foreground "#E69875")
              (set-face-attribute 'font-lock-number-face nil :foreground "#D699B6")

              (set-face-attribute 'window-divider nil :foreground "gray30")
              (set-face-attribute 'next-error-message nil :background "#3E482D" :foreground "#D3C6AA")
              (set-face-attribute 'default nil :foreground "#D3C6AA") ;; was: #efcadb D3C6AA e2d5b8 E7DCC4
              (after! hl-line
                (set-face-attribute 'hl-line nil :background "#20282D")) ;; was: #1E262B
              (set-face-attribute 'scroll-bar nil :foreground "#6E7B85" :background "#242D34")  ;; doesn't work in windows
              (set-face-attribute 'show-paren-match nil :background nil)
              (after! diff-mode
                (set-face-attribute 'diff-added nil :background "#3E482D" :foreground nil) ;;#643839 #724041
                (set-face-attribute 'diff-refine-added nil :background "#E7DCC4" :foreground "#3E482D")
                (set-face-attribute 'diff-removed nil :background "#311b1c" :foreground "#E7DCC4")
                ;(set-face-attribute 'diff-removed-higlight nil :background "#311b1c" :foreground "#E7DCC4")
                (set-face-attribute 'diff-refine-removed nil :background "#E7DCC4" :foreground "#643839") ;;#643839 #724041
                ;;(set-face-attribute 'diff-refine-changed nil :background "#E7DCC4" :foreground "#304946")
                )
              (after! magit
                (set-face-attribute 'magit-diff-added-highlight nil :background "#404830" :foreground nil :bold nil) ;;#643839 #724041 #304946
                (set-face-attribute 'magit-diff-removed-highlight nil :background "#412E2E" :foreground nil :bold nil) ;;#643839 #724041
                )
              (after! markdown-mode
                (set-face-attribute 'markdown-inline-code-face nil :background nil)
                (set-face-attribute 'markdown-code-face nil :background nil))
              (add-to-list 'display-buffer-alist
                           (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))
              (set-face-attribute 'org-block nil :background "#2A3339")
              (add-hook 'ediff-quit-hook 'cp/kill-ediff-buffers)
              ;;(remove-hook 'ediff-quit-hook 'cp/kill-ediff-buffers)
              (message "finished after init-setup")
              )
            ))

(require 'server)
(unless (eq (server-running-p) 't)
  (server-start))
