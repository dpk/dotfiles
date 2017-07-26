(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-mode t)
 '(nxml-slash-auto-complete-flag t)
 '(rng-schema-locating-files
   (quote
    ("schemas.xml")))) ;; @@ add more here (OS-specific)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "Hello-world -- this is my init.el file loading!")
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

;; smart mode line
(setq sml/theme 'light-powerline)
(sml/setup)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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

;; auto-fill-mode is kill
(turn-off-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'nxml-mode-hook 'turn-off-auto-fill)

;; start the server
(server-start)
