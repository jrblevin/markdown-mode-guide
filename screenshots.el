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
