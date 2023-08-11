(defvar-local small-space (format "%s" (make-string (round (* 0.01 (frame-width))) ?\s)))
(defvar-local medium-space (format "%s" (make-string (round (* 0.02 (frame-width))) ?\s)))
(defvar-local large-space (format "%s" (make-string (round (* 0.2 (frame-width))) ?\s)))

(defface bob-modeline/face-blue
  '((t (:bold t :foreground "green" :background nil)))
  "Blue face for mode-line.")

(defun mode--line-element-width (mode-line-element)
  (length (substring-no-properties (format-mode-line mode-line-element))))


(defvar-local bob-modeline/buffer-name
    '(:eval (format "%s  %s "
                    (propertize ""
                                'face '(bob-modeline/face-blue))
                    (propertize (buffer-name)
                                'face '(modus-themes-bold)))))

(defvar-local bob-modeline/major-mode
    '(:eval (major-mode-to-icon-property)))

(defun major-mode-to-icon-property ()
  (cond ((eq major-mode 'js2-mode)
         (bobs-modeline--propertize-png "js-icon"
                                        0.035))
        ((eq major-mode 'typescript-mode)
         (bobs-modeline--propertize-png "ts-icon"))
        ((eq major-mode 'emacs-lisp-mode)
         (bobs-modeline--propertize-png "emacs-icon"
                                        0.02))
        (t (propertize " "
                       'face '(bob-modeline/face-blue bold)))))

(defun bobs-modeline--propertize-png (file-name-minus-suffix &optional scale)
  (propertize " "
              'display (create-image (format "/Users/bob/source/bobs-modeline/assets/%s.png" file-name-minus-suffix)
                                     'png nil :ascent 'center :scale (or scale 1.0))))

(defvar-local bob-modeline/buffer-modify-state
    '(:eval (if (buffer-modified-p)
                (propertize ""
                            'face '(modus-themes-fg-green-faint bold))
              (propertize ""
                          'face '(modus-themes-fg-green-faint bold)))))

(defvar-local bob-modeline/vc-mode
    '(:eval (when vc-mode
              (format "%s %s"
                      (propertize (vc-state-symbol)
                                  'face '(bob-modeline/face-blue))
                      (propertize (substring vc-mode 1)))
              "")))

;; TODO:
;; vc-mode should change icon according to (vc-state (buffer-file-name))

(defun vc-state-symbol ()
  (cond ((equal (vc-state (buffer-file-name)) 'up-to-date)
         (propertize ""
                     'face '(modus-themes-fg-blue-intense bold)))
        (t (propertize ""
                       'face '(modus-themes-fg-blue-intense bold)))))

(defvar-local bob-modeline/project-name
    '(:eval (when (project-current)
              (format "%s %s"
                      (propertize ""
                                  'face '(bob-modeline/face-blue))
                      (propertize (capitalize (project-name (project-current)))
                                  'face '(modus-themes-bold))))))

(defvar-local bob-modeline/battery-status
    '(:eval (format "%s  %s%s"
                    (propertize ""
                                'face '(modus-themes-fg-yellow bold))
                    (propertize (cdr (nth 1 (funcall battery-status-function)))
                                'face '(modus-themes-bold))
                    (propertize "%%"
                                'face '(modus-themes-bold)))))
(setq bob-modeline/battery-status-length (mode--line-element-width bob-modeline/battery-status))

(defvar-local bob-modeline/time
    '(:eval (format "%s"
                    (propertize (format-time-string "%R" (current-time))
                                'face '(:foreground "green")))))

(setq bob-modeline/time-length (mode--line-element-width bob-modeline/time))
(setq-default mode-line-format
              '(" "
                (:eval bob-modeline/buffer-modify-state)
                medium-space
                (:eval bob-modeline/major-mode)
                small-space
                (:eval bob-modeline/buffer-name)
                small-space
                (:eval bob-modeline/project-name)
                small-space
                (:eval bob-modeline/vc-mode)
                small-space
                (:eval
                 (propertize
                  " "
                  'display
                  `((space :align-to (- (+ right right-margin)
                                        ,(+ 1 bob-modeline/time-length))))))
                small-space
                (:eval bob-modeline/time)))

(set-face-background 'mode-line nil)
(set-face-foreground 'mode-line nil)
(set-face-attribute 'mode-line nil :box nil)

(provide 'bobs-modeline)
