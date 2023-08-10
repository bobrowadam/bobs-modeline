(defvar-local small-space (format "%s" (make-string (round (* 0.01 (frame-width))) ?\s)))
(defvar-local medium-space (format "%s" (make-string (round (* 0.02 (frame-width))) ?\s)))
(defvar-local large-space (format "%s" (make-string (round (* 0.2 (frame-width))) ?\s)))


(setq bob-modeline/time-length (mode--line-element-width bob-modeline/time))
(setq bob-modeline/battery-status-length (mode--line-element-width bob-modeline/battery-status))

(setq-default mode-line-format
              '((:eval bob-modeline/buffer-modify-state)
                medium-space
                (:eval bob-modeline/buffer-name)
                small-space
                (:eval bob-modeline/major-mode)
                large-space
                (:eval bob-modeline/project-name)
                small-space
                (:eval bob-modeline/vc-mode)
                (:eval
                 (propertize
                  " "
                  'display
                  `((space :align-to (- (+ right right-margin)
                                        ,(+ 1 bob-modeline/time-length))))))
                small-space
                (:eval bob-modeline/time)))

(defvar-local bob-modeline/buffer-name
    '(:eval (format "%s  %s "
                    (propertize ""
                                'face '(modus-themes-fg-blue-intense bold))
                    (propertize (buffer-name)
                                'face '(modus-themes-bold)))))

(defvar-local bob-modeline/major-mode
    '(:eval (format "%s %s "
                    (propertize "λ"
                                'face '(modus-themes-fg-blue-intense bold))
                    (format-mode-line mode-name))))

(defvar-local bob-modeline/buffer-modify-state
    '(:eval (if (buffer-modified-p)
                (propertize ""
                            'face '(modus-themes-fg-green-faint bold))
              (propertize ""
                          'face '(modus-themes-fg-green-faint bold)))))

(defvar-local bob-modeline/vc-mode
    '(:eval (when vc-mode
              (format "%s %s"
                      (propertize ""
                                  'face '(modus-themes-fg-blue-intense bold))   
                      (propertize (substring vc-mode 1))))))

(defvar-local bob-modeline/project-name
    '(:eval (when (project-current)
             (format "%s %s"
                     (propertize ""
                                 'face '(modus-themes-fg-blue-intense bold))
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

(defvar-local bob-modeline/time
    '(:eval (format "%s"
                    (propertize (format-time-string "%R" (current-time))
                                'face '(modus-themes-fg-green-cooler)))))

(defun mode--line-element-width (mode-line-element)
  (length (substring-no-properties (format-mode-line mode-line-element))))

(provide 'bobs-modeline)



