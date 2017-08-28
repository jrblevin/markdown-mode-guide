;; Support code for generating screenshots on macOS from within Emacs
;;
;; Jason Blevins <jblevins@xbeta.org>
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

;; Set frame geometry according to display resolution.
(setq default-frame-alist
      `((top . 1)
        (left . 1)
        (width . 70)
        (height . 35)
        (vertical-scroll-bars . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-splash-screen t
      initial-scratch-message nil
      select-enable-clipboard t
      column-number-mode 0
      line-number-mode 0
      mode-line-position nil)

;; Remove Git branch info from mode line
(delete '(vc-mode vc-mode) mode-line-format)

;; Set up a consistent look for screenshots.
(setq-default line-spacing 0.25)
(set-face-attribute 'default nil :family "Operator Mono" :weight 'light :height 150)
(set-face-attribute 'fixed-pitch nil :family "Courier Prime")
(set-face-attribute 'variable-pitch nil :family "Fira Sans")
(set-face-attribute 'default nil :background "#f8f8f8")
(set-face-attribute 'fringe nil :background nil)
(setq frame-title-format "%b")
(global-hl-line-mode 0)
(blink-cursor-mode 0)

;; Load markdown-mode
(use-package markdown-mode
  :bind (("<f7>" . markdown-mode))
  :commands (markdown-mode gfm-mode)
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown --snippet --smart --notes"
        markdown-open-command "mark"))

;; Deft
(use-package deft
  :init
  (setq deft-directory "~/Documents/Deft/demo/"
	deft-auto-save-interval 0
	deft-recursive nil
	deft-extensions '("txt" "text" "tex" "org")
	deft-use-filter-string-for-filename t
	deft-markdown-mode-title-level 1
        deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ; blank
                "\\|^#.*$" ; Markdown headings
                "\\)")
	deft-file-naming-rules '((noslash . "-")
				 (nospace . "-")
				 (case-fn . downcase))))

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
