(add-to-list 'load-path "~/.emacs.d/lisp/")

;; disable aquamacs pester mode/customizations.el
(setq aquamacs-save-options-on-quit nil)
(setq custom-file "~/.emacs.d/custom.el")
(load-file "~/.emacs.d/custom.el")

;; enable mouse terminal support
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)
(setq mouse-wheel-mode t)
(setq backup-directory-alist `(("." . "~/.saves")))

;; copy and paste using the OS X clipboard regardless of where i'm running emacs
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; disable copy-on-mouse-select
(defun x-select-text (text))
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary nil)
(setq mouse-drag-copy-region nil)

; something to do with tabs and spaces
(setq-default indent-tabs-mode nil)

;; use fn as meta
(setq mac-option-modifier 'none)
(setq ns-function-modifier 'meta)

;; shut the fuck up
(setq ring-bell-function 'ignore)

;; scroll with mouse in terminal (this causes garbage to be sprayed into the terminal window if you scroll too high, for some reason)
(global-set-key [mouse-4] 'previous-line)
(global-set-key [mouse-5] 'next-line)

;; uhhh, i think this disables the annoying toolbar in Aquamacs
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(custom-set-variables '(tool-bar-mode nil))

;; melpa
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; fix stupid Aquamacs defaults which break custom theme
(assq-delete-all 'foreground-color default-frame-alist)
(assq-delete-all 'background-color default-frame-alist)
(assq-delete-all 'cursor-color default-frame-alist)

;; configure theme
(require 'vimspectr)
(add-to-list 'custom-theme-load-path  "~/.emacs.d/vimspectr-themes")
(setq custom-theme-directory "~/.emacs.d/themes")

(load-theme 'vimspectr210wcurve-light t)
(load-theme 'vimspectr210wcurve-dark t)
(load-theme 'smart-mode-line-light-powerline t)
(load-theme 'smart-mode-line-powerline t)

(defun turn-on-daytime-theme ()
  (interactive)
  (disable-theme 'smart-mode-line-powerline)
  (enable-theme 'smart-mode-line-light-powerline)
  (disable-theme 'vimspectr210wcurve-dark)
  (enable-theme 'vimspectr210wcurve-light))
(turn-on-daytime-theme)

(defun turn-on-nighttime-theme ()
  (interactive)
  (disable-theme 'smart-mode-line-light-powerline)
  (enable-theme 'smart-mode-line-powerline)
  (disable-theme 'vimspectr210wcurve-light)
  (enable-theme 'vimspectr210wcurve-dark))

(defun toggle-day-night ()
  (interactive)
  (if (memq 'vimspectr210wcurve-dark custom-enabled-themes)
      (turn-on-daytime-theme)
    (turn-on-nighttime-theme)))
(global-set-key (kbd "C-*") 'toggle-day-night)

;; smart mode line
(setq sml/theme 'light-powerline)
(sml/setup)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; ido-mode
(ido-mode t)

;; tabbar-mode is kill
(tabbar-mode 0)
(define-key osx-key-mode-map (kbd "A-t") nil)

;; global-linum-mode
(global-linum-mode 1)

;; allow selection by clicking in the left margin
;; from http://stackoverflow.com/questions/8103111/can-i-select-text-by-clicking-on-the-linum-column-in-emacs

(defvar *linum-mdown-line* nil)

(defun line-at-click ()
  (save-excursion
    (let ((click-y (cdr (cdr (mouse-position))))
          (line-move-visual-store line-move-visual))
      (setq line-move-visual t)
      (goto-char (window-start))
      (next-line (1- click-y))
      (setq line-move-visual line-move-visual-store)
      ;; If you are using tabbar substitute the next line with
      ;; (line-number-at-pos))))
      (1+ (line-number-at-pos)))))

(defun md-select-linum ()
  (interactive)
  (goto-line (line-at-click))
  (set-mark (point))
  (setq *linum-mdown-line* (line-number-at-pos)))

(defun mu-select-linum ()
  (interactive)
  (when *linum-mdown-line*
    (let (mu-line)
      (setq mu-line (line-at-click))
      (if (> mu-line *linum-mdown-line*)
          (progn
            (goto-line *linum-mdown-line*)
            (set-mark (point))
            (goto-line mu-line)
            (end-of-line))
          (progn
            (goto-line *linum-mdown-line*)
            (set-mark (line-end-position))
            (goto-line mu-line)
            (beginning-of-line)))
      (setq *linum-mdown* nil))))

(global-set-key (kbd "<left-margin> <down-mouse-1>") 'md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'mu-select-linum)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'mu-select-linum)

;; don't highlight long lines in whitespace-mode
(whitespace-mode)
(setq whitespace-style (remove 'lines whitespace-style))

;; configure whitespace-mode to automatically start with makefile-mode
(add-hook 'makefile-mode-hook 'whitespace-mode)

;; nxml-mode customizations
(nxml-mode)
(setq nxml-slash-auto-complete-flag t)
(add-to-list 'rng-schema-locating-files "schemas.xml")
(add-to-list 'rng-schema-locating-files "~/.emacs.d/schemas.xml")

;; enable rgb(...) colours rendering in less-css-mode
(rainbow-mode)
(add-to-list 'rainbow-html-colors-major-mode-list 'less-css-mode)

;; auto-fill-mode is kill
(turn-off-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'nxml-mode-hook 'turn-off-auto-fill)
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)

;; utility functions
(defun title-case (b e)
  (interactive "r")
  (shell-command-on-region b e "titlecase" (current-buffer) t))

;; add smart-quotes mode
(require 'smart-quotes)
(setq smart-quotes-reverse-quotes nil)
(setq smart-quotes-left-context "^\\|[^[:word:]“‘.,;]") ; fixes for my idiosyncratic typing style, and also for all unicode non-word chars

;; de-irritate markdown-mode
(markdown-mode)
(define-key markdown-mode-map (kbd "<M-right>") nil)
(define-key markdown-mode-map (kbd "<M-left>") nil)

;; set Cmd-Opt-brackets to cycle buffers
(global-set-key (kbd "A-“") 'previous-buffer)
(global-set-key (kbd "A-‘") 'next-buffer)
;; and Cmd-Shift too (@@ decide on just one of these, this is borken)
(define-key osx-key-mode-map (kbd "A-{") 'previous-buffer)
(define-key osx-key-mode-map (kbd "A-}") 'next-buffer)

;; set Cmd-W to kill buffer and Cmd-Opt-W to close window
(defun kill-current-buffer ()
  "Kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))
(define-key osx-key-mode-map (kbd "A-w") 'kill-current-buffer)
(define-key osx-key-mode-map (kbd "A-∑") 'close-window)

;; select line
(defun select-current-line ()
  "Select the current line, including the newline character at the end"
  (interactive)
  (next-line)
  (beginning-of-line)
  (let ((lbgp (line-beginning-position)))
    (previous-line)
    (set-mark lbgp)))
(define-key osx-key-mode-map (kbd "A-l") 'select-current-line)
(define-key osx-key-mode-map (kbd "A-j") 'goto-line)

;; start the server
(server-start)

(if (file-exists-p "~/.emacs.d/init.private.el")
    (load-file "~/.emacs.d/init.private.el"))
