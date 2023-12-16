(defvar-local small-space (format "%s" (make-string (round (* 0.01 (frame-width))) ?\s)))
(defvar-local medium-space (format "%s" (make-string (round (* 0.02 (frame-width))) ?\s)))
(defvar-local large-space (format "%s" (make-string (round (* 0.2 (frame-width))) ?\s)))


(defface bob-modeline/face-blue
  '((t (:bold t :foreground "blue" :background nil)))
  "Blue face for mode-line.")

(defface bob-modeline/face-green
  '((t (:bold t :foreground "green" :background nil)))
  "Green face for mode-line.")

(defface bob-modeline/face-yellow
  '((t (:bold t :foreground "yellow" :background nil)))
  "Yellow face for mode-line.")

(defface bob-modeline/face-orange
  '((t (:bold t :foreground "orange" :background nil)))
  "Orange face for mode-line.")

(defun mode--line-element-width (mode-line-element)
  (length (substring-no-properties (format-mode-line mode-line-element))))


(defvar-local bob-modeline/buffer-name
    '(:eval (format "%s: %s"
                    (propertize "Buffer:"
                                  'face '(bob-modeline/face-green))
                    (propertize (buffer-name)
                                'face '(modus-themes-bold)
                                'help-echo "Buffer Name"))))

(defvar-local bob-modeline/major-mode
    '(:eval (major-mode-to-icon-property)))

(defun major-mode-to-icon-property ()
  (cond ((eq major-mode 'js2-mode)
         (bobs-modeline--propertize-png "js-icon"
                                        0.035))
        ((eq major-mode 'typescript-mode)
         (bobs-modeline--propertize-png "ts-icon"))
        ((derived-mode-p 'emacs-lisp-mode)
         (bobs-modeline--propertize-png "emacs-icon"
                                        0.025))
        ((derived-mode-p 'magit-mode)
         (bobs-modeline--propertize-png "magit-icon" 0.08))
        ((derived-mode-p 'dired-mode)
         (bobs-modeline--propertize-png "dired-icon" 0.08))
        ((derived-mode-p 'web-mode)
         (bobs-modeline--propertize-png "vue-icon" 0.7))
        ((derived-mode-p 'rust-mode)
         (bobs-modeline--propertize-png "rust-icon" 0.08))
        (t (propertize "ℵ "
                       'face '(bob-modeline/face-green bold)))))

(defun bobs-modeline--propertize-png (file-name-minus-suffix &optional scale)
  (propertize " "
              'display (create-image (format "/Users/bob/source/bobs-modeline/assets/%s.png" file-name-minus-suffix)
                                     'png nil :ascent 'center :scale (or scale 1.0))
              'help-echo "Major Mode"))

(defvar-local bob-modeline/buffer-modify-state
    '(:eval (if (buffer-modified-p)
                (propertize ""
                            'face '(modus-themes-fg-green-faint bold)
                            'help-echo "Buffer modified.")
              (propertize ""
                          'face '(modus-themes-fg-green-faint bold)
                          'help-echo "Buffer not modified."))))

(defun should-calc-vc-mode ()
  (and (not (and (buffer-file-name)
                 (file-remote-p (buffer-file-name))))
       vc-mode))

(defvar-local bob-modeline/vc-mode
    '(:eval (when (should-calc-vc-mode)
              (format "%s %s"
                      (vc-state-symbol)
                      (propertize (substring vc-mode 1))))))

;; TODO:
;; vc-mode should change icon according to (vc-state (buffer-file-name))

(defun vc-state-symbol ()
  (cond ((equal (vc-state (buffer-file-name)) 'up-to-date)
         (propertize ""
                     'face '(bob-modeline/face-green bold)
                     'help-echo "VC State: Up to date"))
        ((equal (vc-state (buffer-file-name)) 'edited)
         (propertize ""
                     'face '(bob-modeline/face-yellow bold)
                     'help-echo "VC State: Edited"))
        ((equal (vc-state (buffer-file-name)) 'added)
         (propertize ""
                     'face '(bob-modeline/face-orange bold)
                     'help-echo "VC State: Added"))
        (t (propertize ""
                       'face '(bob-modeline/face-green bold)
                       'help-echo "VC State"))))

(defun should-calc-project-name ()
  (and (buffer-file-name)
       (not (file-remote-p (buffer-file-name)))))

(defvar-local bob-modeline/project-name
    '(:eval (when (and (should-calc-project-name)
                       (project-current))
              (format "%s %s"
                      (propertize "Project:"
                                  'face '(bob-modeline/face-green))
                      (propertize (capitalize (project-name (project-current)))
                                  'face '(modus-themes-bold)
                                  'help-echo "Project Name")))))

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
                medium-space
                (:eval bob-modeline/project-name)
                medium-space
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

(let ((subtle (face-foreground 'shadow)))
    (custom-set-faces
     `(mode-line ((t :background unspecified :box unspecified)))
     `(mode-line-active ((t :inherit mode-line :box unspecified :overline ,subtle)))
     `(mode-line-inactive ((t :background unspecified :foreground ,subtle :box unspecified)))))

(provide 'bobs-modeline)
