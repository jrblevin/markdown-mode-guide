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

;; Set the load path
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Disable scroll bar, tool bar, and menu bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode -1)
(display-time-mode 0)

;; Modifier keys
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Load `package.el'
(eval-when-compile (require 'package))
(setq package-enable-at-startup nil
      package-user-dir (format "%selpa-%d.%d" user-emacs-directory
                               emacs-major-version emacs-minor-version)
      package-menu-async t
      package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
(eval-when-compile (package-initialize))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;; Color themes
(setq custom-theme-directory "~/.emacs.d/themes")

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
(global-hl-line-mode 0)
(blink-cursor-mode 0)

;; Load markdown-mode
(use-package markdown-mode
  :bind (("<f7>" . markdown-mode))
  :commands (markdown-mode gfm-mode)
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :init
  (setq markdown-header-scaling t
        markdown-hide-urls t
        markdown-fontify-code-blocks-natively t)
  :config
  (setq markdown-command "multimarkdown --snippet --smart --notes"
        markdown-open-command "mark"))

;; Load powerline
(use-package powerline
  :config
  (setq powerline-display-hud nil
        powerline-display-buffer-size nil
        powerline-display-mule-info nil
        ;; powerline-gui-use-vcs-glyph t
        powerline-height 24
        powerline-default-separator 'slant)
  :init (powerline-default-theme))

;; Screenshots
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
