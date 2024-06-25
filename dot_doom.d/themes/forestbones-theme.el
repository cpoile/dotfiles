;;; forestbones-theme.el --- inspired by zenbones' forestbones https://github.com/mcchrish/zenbones.nvim -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: Dec 03, 2023
;; Author: Christopher Poile <https://github.com/cpoile>
;; Maintainer: Christopher Poile <https://github.com/cpoile>
;; Source: https://github.com/mcchrish/zenbones.nvim
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup forestbones-theme nil
  "Options for the `forestbones' theme."
  :group 'doom-themes)

(defcustom forestbones-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'forestbones-theme
  :type 'boolean)

(defcustom forestbones-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'forestbones-theme
  :type 'boolean)

(defcustom forestbones-comment-bg forestbones-brighter-comments
  "If non-nil, comments will have a subtle highlight to enhance their
legibility."
  :group 'forestbones-theme
  :type 'boolean)

(defcustom forestbones-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'forestbones-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme forestbones
  "A dark theme inspired by Atom One Dark."

  ;; name        default   256           16
  ((bg         '("#242D34" "black"       "black"  ))
   (fg         '("#E7DCC4" "#bfbfbf"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#1E262B" "black"       "black"        ))
   (fg-alt     '("#736E62" "#2d2d2d"     "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#1C2329" "black"       "black"        ))
   (base1      '("#2e3437" "#1e1e1e"     "brightblack"  ))
   (base2      '("#414545" "#2e2e2e"     "brightblack"  ))
   (base3      '("#535553" "#262626"     "brightblack"  ))
   (base4      '("#656661" "#3f3f3f"     "brightblack"  ))
   (base5      '("#8a877e" "#525252"     "brightblack"  ))
   (base6      '("#aea99a" "#6b6b6b"     "brightblack"  ))
   (base7      '("#d3cab6" "#979797"     "brightblack"  ))
   (base8      '("#F7ECD2" "#dfdfdf"     "white"        ))

   ;; These are terminal colors
   (grey       base4)
   (red        '("#E67C7F" "#ff6655" "red"          ))
   (orange     '("#ED9294" "#dd8844" "brightred"    ))
   (green      '("#A9C181" "#99bb66" "green"        ))
   (teal       '("#B0CE7B" "#44b9b1" "brightgreen"  ))
   (yellow     '("#DDBD7F" "#ECBE7B" "yellow"       ))
   (blue       '("#7AC9C0" "#51afef" "brightblue"   ))
   (dark-blue  '("#7FBCB4" "#2257A0" "blue"         ))
   (magenta    '("#E5A7C4" "#c678dd" "brightmagenta"))
   (violet     '("#D69AB7" "#a9a1e1" "magenta"      ))
   (cyan       '("#7DD093" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#83C193" "#5699AF" "cyan"         ))

   ;; syntax colors
   (dark-fg '("#ADA28B" "#99bb66" "green")) ;; first is themed, second two are default
   (dark-green '("#a7c080" "#99bb66" "green")) ;; first is themed, second two are default
   (comment '("#6E7B85" "#7FBCB4" "dark-blue")) ;; first is themed, second two are default

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      green)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        dark-green)
   (comments       (if forestbones-brighter-comments base5 comment))
   (doc-comments   (doom-lighten (if forestbones-brighter-comments base5 comment) 0.25))
   (constants      dark-fg)
   (functions      fg)
   (keywords       green)
   (methods        cyan)
   (operators      red)
   (type           yellow)
   (strings        dark-fg)
   (variables      (doom-lighten magenta 0.4))
   (numbers        fg)
   ;;(region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (region         '("#59544A"))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if forestbones-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if forestbones-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when forestbones-padded-modeline
      (if (integerp forestbones-padded-modeline) forestbones-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground base4 :inherit nil)
   ((font-lock-comment-face &override)
    :background (if forestbones-comment-bg (doom-lighten bg 0.05) 'unspecified)
    :slant (if doom-themes-enable-italic 'italic 'unspecified))
   ((font-lock-string-face &override)
    :slant (if doom-themes-enable-italic 'italic 'unspecified))
   ((font-lock-number-face &override)
    :weight 'normal
    :slant (if doom-themes-enable-italic 'italic 'unspecified))
   ((highlight-numbers-number &override)
    :weight 'normal
    :slant (if doom-themes-enable-italic 'italic 'unspecified))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if forestbones-brighter-modeline base8 highlight))


   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if forestbones-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))
    ;;;; company-mode
    ;;(company-tooltip-selection :background (doom-lighten green 0.2)))
  ;;;; Base theme variable overrides-
  )())

;;; forestbones-theme.el ends here
