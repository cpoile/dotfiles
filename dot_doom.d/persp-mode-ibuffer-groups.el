;;; persp-mode-ibuffer-groups.el -*- lexical-binding: t; -*-

;; from: https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-mode-ibuffer-groups-el
;; Simplified variant. Add only current perspective group.

(with-eval-after-load "ibuffer"

  (require 'ibuf-ext)

  (define-ibuffer-filter persp
      "Toggle current view to buffers of current perspective."
    (:description "persp-mode"
     :reader (persp-prompt nil nil (safe-persp-name (get-frame-persp)) t))
    (find buf (safe-persp-buffers (persp-get-by-name qualifier))))

  (defun persp-add-ibuffer-group ()
      (let ((perspslist (list
                         (list (safe-persp-name (get-frame-persp))
                               (cons 'persp (safe-persp-name (get-frame-persp)))))))
        (setq ibuffer-saved-filter-groups
              (delete* "persp-mode" ibuffer-saved-filter-groups
                       :test 'string= :key 'car))
        (push
         (cons "persp-mode" perspslist)
         ibuffer-saved-filter-groups)))

  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (persp-add-ibuffer-group)
                (ibuffer-switch-to-saved-filter-groups "persp-mode"))))


;; Shows groups for all perspectives. But can't show same buffer in multiple groups.

(with-eval-after-load "ibuffer"

  (require 'ibuf-ext)

  (define-ibuffer-filter persp
      "Toggle current view to buffers of current perspective."
    (:description "persp-mode"
     :reader (persp-prompt nil nil (safe-persp-name (get-frame-persp)) t))
    (find buf (safe-persp-buffers (persp-get-by-name qualifier))))

  (defun persp-add-ibuffer-group ()
    (let ((perspslist (mapcar #'(lambda (pn)
                                  (list pn (cons 'persp pn)))
                              (nconc
                               (delete* persp-nil-name
                                        (persp-names-current-frame-fast-ordered)
                                        :test 'string=)
                               (list persp-nil-name)))))
      (setq ibuffer-saved-filter-groups
            (delete* "persp-mode" ibuffer-saved-filter-groups
                     :test 'string= :key 'car))
      (push
       (cons "persp-mode" perspslist)
       ibuffer-saved-filter-groups)))

  (defun persp-ibuffer-visit-buffer ()
    (let ((buf (ibuffer-current-buffer t))
          (persp-name (get-text-property
                       (line-beginning-position) 'ibuffer-filter-group)))
      (persp-switch persp-name)
      (switch-to-buffer buf)))

  (define-key ibuffer-mode-map (kbd "RET") 'persp-ibuffer-visit-buffer)

  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (persp-add-ibuffer-group)
                (ibuffer-switch-to-saved-filter-groups "persp-mode"))))
