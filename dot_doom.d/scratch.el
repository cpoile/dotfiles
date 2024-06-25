;;; scratch.el -*- lexical-binding: t; -*-

(def-doom-theme doom-zenburn
  "An implementation of the popular Zenburn theme."

  ;; name        default   256       16
  ((bg         '("#3F3F3F" nil       nil            )) ;; zenburn-bg
   (bg-alt     '("#383838" nil       nil            )) ;; zenburn-bg-05
   (base0      '("#000000" "black"   "black"        )) ;; zenburn-bg-2
   (base1      '("#2B2B2B" "#1e1e1e" "brightblack"  )) ;; zenburn-bg-1
   (base2      '("#303030" "#2e2e2e" "brightblack"  )) ;; zenburn-bg-08
   (base3      '("#383838" "#262626" "brightblack"  )) ;; zenburn-bg-05
   (base4      '("#494949" "#3f3f3f" "brightblack"  )) ;; zenburn-bg+05
   (base5      '("#4F4F4F" "#525252" "brightblack"  )) ;; zenburn-bg+1
   (base6      '("#5F5F5F" "#6b6b6b" "brightblack"  )) ;; zenburn-bg+2
   (base7      '("#6F6F6F" "#979797" "brightblack"  )) ;; zenburn-bg+3
   (base8      '("#FFFFEF" "#dfdfdf" "white"        )) ;; zenburn-fg+1
   (fg         '("#DCDCDC" "#bfbfbf" "brightwhite"  )) ;; zenburn-fg
   (fg-alt     '("#989890" "#2d2d2d" "white"        )) ;; zenburn-fg-05

   (grey       base4)
   (red        '("#CC9393" "#ff6655" "red"          )) ;; zenburn-red
   (orange     '("#DFAF8F" "#dd8844" "brightred"    )) ;; zenburn-orange
   (green      '("#7F9F7F" "#99bb66" "green"        )) ;; zenburn-green
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  )) ;; zenburn-??
   (yellow     '("#F0DFAF" "#ECBE7B" "yellow"       )) ;; zenburn-yellow
   (blue       '("#8CD0D3" "#51afef" "brightblue"   )) ;; zenburn-blue
   (dark-blue  '("#2257A0" "#2257A0" "blue"         )) ;; zenburn-??
   (magenta    '("#DC8CC3" "#c678dd" "brightmagenta")) ;; zenburn-magenta
   (violet     '("#a9a1e1" "#a9a1e1" "magenta"      )) ;; zendurn-??
   (cyan       '("#93E0E3" "#46D9FF" "brightcyan"   )) ;; zenburn-cyan
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         )) ;; zenburn-??

   ;; Extra zenburn colors
   (fg-1       '("#656555"))
   (fg+2       '("#FFFFFD"))
   (red-4      '("#8C5353"))
   (red-1      '("#BC8383"))
   (red+1      '("#DCA3A3"))
   (yellow-2   '("#D0BF8F"))
   (yellow-1   '("#E0CF9F"))
   (green-2    '("#5F7F5F"))
   (green+1    '("#8FB28F"))
   (green+2    '("#9FC59F"))
   (green+3    '("#AFD8AF"))
   (green+4    '("#BFEBBF"))
   (blue+1     '("#94BFF3"))
   (blue-1     '("#7CB8BB"))
   (blue-2     '("#6CA0A3"))
   (blue-3     '("#5C888B"))
   (blue-4     '("#4C7073"))
   (blue-5     '("#366060"))
))
