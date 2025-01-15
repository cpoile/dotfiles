(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((t (:extend t :background "#3f575a"))))
 '(ediff-current-diff-B ((t (:inherit ediff-current-diff-A :background "#304946"))))
 '(ediff-even-diff-B ((t (:inherit ediff-even-diff-A :background "#2e4542"))))
 '(ediff-fine-diff-A ((t (:extend t :background "#304946" :weight bold)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((default-compile-cmd
      . "jai c:/Users/Chris/git/jai/handmade_jai/build_handmade.jai")
     (defualt-compile-cmd
      . "jai c:/Users/Chris/git/jai/handmade_jai/build_handmade.jai")
     (compile-all-cmd . "c:/Users/Chris/git/jai/handmade_jai/make.bat")
     (file-to-compile-cmd
      ("win32_handmade.jai" compile
       "jai c:/Users/Chris/git/jai/handmade_jai/build_win32_handmade.jai"))
     (custom-compile-cmd
      . "jai c:/Users/Chris/git/jai/handmade_jai/build_handmade.jai")
     (custom-run-test-cmd
      . "c:/Users/Chris/git/jai/scratch/build/scratch.exe test")
     (custom-run-all-cmd . "c:/Users/Chris/git/jai/scratch/build/scratch.exe")
     (custom-compile-cmd . "c:/Users/Chris/git/jai/scratch/make.bat")
     (custom-run-test-cmd
      . "odin test c:/Users/Chris/git/odin/writing_an_interpreter/src/ -all-packages -define:ODIN_TEST_FANCY=false -debug -o:minimal -out:c:/Users/Chris/git/odin/writing_an_interpreter/build/test.exe")
     (custom-run-all-cmd
      . "c:/Users/Chris/git/odin/writing_an_interpreter/build/interpreter.exe")
     (custom-compile-cmd
      . "c:/Users/Chris/git/odin/writing_an_interpreter/make.bat")
     (custom-run-test-cmd
      . "odin test c:/Users/Chris/git/odin/handmade_hero/src/ -all-packages -define:ODIN_TEST_FANCY=false -debug -o:minimal -out:c:/Users/Chris/git/odin/handmade_hero/build/test.exe")
     (custom-run-all-cmd
      . "c:/Users/Chris/git/odin/handmade_hero/build/handmade.exe")
     (custom-compile-cmd . "c:/Users/Chris/git/odin/handmade_hero/make.bat")
     (custom-run-test-cmd
      . "c:/Users/Chris/git/jai/handmade_jai/build/handmade.exe test")
     (custom-run-all-cmd
      . "c:/Users/Chris/git/jai/handmade_jai/build/handmade.exe")
     (custom-compile-cmd . "c:/Users/Chris/git/jai/handmade_jai/make.bat")
     (custom-run-test-cmd
      . "c:/Users/Chris/git/jai/writing-an-interpreter/build/main.exe test")
     (custom-run-all-cmd
      . "c:/Users/Chris/git/jai/writing-an-interpreter/build/main.exe")
     (custom-compile-cmd
      . "c:/Users/Chris/git/jai/writing-an-interpreter/make.bat")
     (mangle-whitespace . t))))
