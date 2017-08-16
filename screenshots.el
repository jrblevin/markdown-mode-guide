;; Support code for generating screenshots on macOS from within Emacs
;;
;; Jason Blevins <jrblevin@sdf.org>
;;
;; screencapture arguments:
;;    -o      no shadow
;;    -x      no sounds
;;    -tpng   generate a PNG file
;;    -T1     one second delay
;;    -W      capture window
;;    -l      window id

;; Set up a consistent look for screenshots.
(setq-default line-spacing 0.25)
(set-face-attribute 'default nil :family "Operator Mono" :weight 'light :height 150)
(set-face-attribute 'fixed-pitch nil :family "Courier Prime")
(set-face-attribute 'variable-pitch nil :family "Fira Sans")
(dolist (theme custom-enabled-themes)
  (disable-theme theme))
(load-theme 'sanityinc-tomorrow-day)
(set-frame-size (selected-frame) 70 35)
(set-face-attribute 'default nil :background "#f8f8f8")
(set-face-attribute 'fringe nil :background nil)
(setq frame-title-format "%b")
(powerline-reset)

;; Load markdown-mode
(load-library "markdown-mode.el")

(defconst markdown-guide-images-dir "~/work/markdown-mode-guide/manuscript/images/")

(defun markdown-guide-screenshot (&optional filename)
  (interactive)
  (unless filename
    (setq filename (format-time-string
                    (concat markdown-guide-images-dir
                            "screenshot-%Y-%02m-%02dT%02H.%02M.%02S.png")
                    (current-time))))
  (let ((window-id
         (do-applescript "tell app \"Emacs\" to id of window 1")))
    (shell-command (format "screencapture -o -x -tpng -T1 -W -l%d %s" window-id filename))))

(global-set-key (kbd "C-c s") #'markdown-guide-screenshot)
