(defvar-local small-space (format "%s" (make-string (round (* 0.01 (frame-width))) ?\s)))
(defvar-local medium-space (format "%s" (make-string (round (* 0.1 (frame-width))) ?\s)))
(defvar-local large-space (format "%s" (make-string (round (* 0.3 (frame-width))) ?\s)))

(setq-default mode-line-format
              '("%e"
                small-space
                (:eval bob-modeline/buffer-modify-state)
                medium-space
                (:eval bob-modeline/buffer-name)
                medium-space
                (:eval bob-modeline/major-mode)
                medium-space
                bob-modeline/project-name
                small-space
                (:eval bob-modeline/vc-mode)))

(defvar-local bob-modeline/buffer-name
    '(:eval (format "%s  %s "
                    (propertize ""
                                'face '(modus-themes-fg-yellow bold))
                    (propertize (buffer-name)
                                'face '(modus-themes-bold)))))

(defvar-local bob-modeline/major-mode
    '(:eval (format "%s %s "
                    (propertize "λ"
                                'face '(modus-themes-fg-yellow bold))
                    (propertize (capitalize (symbol-name major-mode))
                                   'face '(modus-themes-bold)))))

(defvar-local bob-modeline/buffer-modify-state
    '(:eval (if (buffer-modified-p)
                (propertize
                 " "
                 'display
                 (list 'image :type 'svg :file
                       (format "%s/source/bobs-modeline/%s" user-home-dir "assets/buffer-modified.svg")
                       :ascent 'center))
              (propertize
               " "
               'display
               (list 'image :type 'svg :file
                     (format "%s/source/bobs-modeline/%s" user-home-dir "assets/buffer-saved.svg")
                     :ascent 'center :foreground "green")))))

(defvar-local bob-modeline/vc-mode
    '(:eval (format "%s %s"
                    (propertize ""
                                'face '(modus-themes-fg-yellow bold))
                    (propertize (substring vc-mode 1)
                                'face '(modus-themes-bold)))))

(defvar-local bob-modeline/project-name
    '(:eval (format "%s %s"
                    (propertize ""
                                'face '(modus-themes-fg-yellow bold))
                    (propertize (capitalize (project-name (project-current))
                                            'face '(modus-themes-bold))))))

(provide 'bobs-modeline)
