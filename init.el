;; ---------------------
;; -- Global Settings --
;; ---------------------
(add-to-list 'load-path "/Users/mehulmandania/.emacs.d")
(setq default-directory "/Users/mehulmandania/Dropbox/sandbox/")

(require 'cl)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'linum)
(require 'smooth-scrolling)
(require 'whitespace)
(require 'dired-x)
(require 'compile)
(ido-mode t) ;; auto-completes file-names etc, ido comes with new emacs now
(setq ido-enable-flex-matching t) ;; enables fuzzy matching
;; (menu-bar-mode -1)
(normal-erase-is-backspace-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)

;; http://superuser.com/questions/639460/how-do-i-remove-the-emacs-toolbar-in-a-maximized-window-on-os-x
(tool-bar-mode 0)

;;  substitute ‘y-or-n-p’ for ‘yes-or-no-p’ everywhere to save one or two characters
(defalias 'yes-or-no-p 'y-or-n-p)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit autoface-default :strike-through nil :underline nil :slant normal :weight normal :height 120 :width normal :family "monaco"))))
 '(column-marker-1 ((t (:background "red"))) t)
 '(diff-added ((t (:foreground "cyan"))))
 '(flymake-errline ((((class color) (background light)) (:background "Red"))))
 '(font-lock-comment-face ((((class color) (min-colors 8) (background light)) (:foreground "red"))))
 '(fundamental-mode-default ((t (:inherit default))) t)
 '(highlight ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
 '(isearch ((((class color) (min-colors 8)) (:background "yellow" :foreground "black"))))
 '(linum ((t (:foreground "black" :weight bold))))
 '(region ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
 '(secondary-selection ((((class color) (min-colors 8)) (:background "gray" :foreground "cyan"))))
 '(show-paren-match ((((class color) (background light)) (:background "black"))))
 '(vertical-border ((t nil))))

;; -------------------------------
;; --- JS SHORTCUT DEFINITIONS ---
;; -------------------------------

;; inserts js/clj log call and puts cursor between brackets
(defun insert-print ()
  (interactive)
  (insert "console.log();")
  (backward-char)(backward-char))

;; inserts js function declaration and moves cursor to body
(defun js-insert-function ()
  (interactive)
  (insert "function() {}")
  (backward-char))

;; inserts js throw if error in callback
(defun js-throw-error ()
  (interactive)
  (insert "if (err) throw err;"))

;; inserts blank React component
(defun insert-react-component ()
  (interactive)
  (insert "var <COMPONENT NAME> = React.createClass({")(newline)
  (insert "render: function() {")(newline)
  (insert "return (")(newline)
  (insert "<div className=''>")(newline)
  (insert "<h1>Hello beti</h1>")(newline)
  (insert "</div>")(newline)
  (insert ");")(newline)
  (insert "}")(newline)
  (insert "});"))(newline)
  

;; ------------
;; -- Macros --
;; ------------
(load "defuns-config.el")
(fset 'align-equals "\C-[xalign-regex\C-m=\C-m")
(global-set-key "\M-=" 'align-equals)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c;" 'comment-or-uncomment-region)
(global-set-key "\M-n" 'next5)
(global-set-key "\M-p" 'prev5)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-i" 'back-window)
(global-set-key "\C-z" 'zap-to-char)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\M-d" 'delete-word)
(global-set-key "\M-h" 'backward-delete-word)
(global-set-key "\M-u" 'zap-to-char)

;; http://www.emacswiki.org/emacs/BackspaceKey
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "C-x g") 'goto-line)

(global-set-key [delete] 'delete-char)
(global-set-key [M-delete] 'kill-word)

(global-set-key (kbd "C-c <right>") 'hs-show-block)
(global-set-key (kbd "C-c <left>")  'hs-hide-block)
(global-set-key (kbd "C-c <up>")    'hs-hide-all)
(global-set-key (kbd "C-c <down>")  'hs-show-all)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (read-kbd-macro "C-x l") 'insert-print)
(global-set-key (read-kbd-macro "C-x f") 'js-insert-function)
(global-set-key (read-kbd-macro "C-x e") 'js-throw-error)
(global-set-key (read-kbd-macro "C-x r") 'insert-react-component)

;; ELPA (emacs lisp package archive)
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; cycling through buffers in one keychord
(global-set-key (kbd "C-S-b") 'previous-buffer) ; TRY ace-jump
(global-set-key (kbd "C-S-f") 'next-buffer) ; TRY ace-jump

;; SET TRANSPARENCY
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(95 50))
(add-to-list 'default-frame-alist '(alpha 95 50))

;; Tab complete in ansi term
(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))

;; http://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Backspace wasn't deleting previously
(normal-erase-is-backspace-mode 0)

;; disable the bell entirely
(setq ring-bell-function 'ignore)

;; http://emacs-fu.blogspot.co.uk/2009/12/scrolling.html
(set-frame-parameter (selected-frame) 'scroll-bar-width nil)

(set-scroll-bar-mode 'nil)
(setq
 scroll-margin 0                  
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

;; Automatic and manual symbol highlighting for Emacs
(require 'highlight-symbol)
(global-set-key [(control f5)] 'highlight-symbol-at-point)
(global-set-key [f5] 'highlight-symbol-next)
(global-set-key [(shift f5)] 'highlight-symbol-prev)
(global-set-key [(meta f5)] 'highlight-symbol-query-replace)

;; ---------------------------
;; -- JS Mode configuration --
;; ---------------------------
(load "js-config.el")
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20141103.105/dict")

;; ---------------------------
;; -- Load MELPA Packages --
;; ---------------------------

;; “M-y immediately pulls up the kill ring” behavior
(require 'browse-kill-ring)
 (browse-kill-ring-default-keybindings)

(require 'yasnippet)
(yas-global-mode 1)(require 'magit)

;; ---------------------------------------------------------
;; ------------------- Paren highlighting ------------------
;; ---------------------------------------------------------

(defadvice show-paren-function
      (after show-matching-paren-offscreen activate)
      "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
      (interactive)
      (let* ((cb (char-before (point)))
             (matching-text (and cb
                                 (char-equal (char-syntax cb) ?\) )
                                 (blink-matching-open))))
        (when matching-text (message matching-text))))

(show-paren-mode 1)

;; ---------------------------------------------------------
;; --------------------------- SMEX ------------------------
;; ---------------------------------------------------------

;; Smex is a M-x enhancement for Emacs. Built on top of Ido, it provides a
;; convenient interface to your recently and most frequently used commands.

(require 'smex)
(smex-initialize)

;; ---------------------------
;; -- Colour themes --
;; ---------------------------

(add-to-list 'custom-theme-load-path "/Users/mehulmandania/.emacs.d/themes")
(load-theme 'solarized-dark t)

;; -----------------------------------
;; -------------------- Navigate to the beginning of the line ---------------
;; -- http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
;; -----------------------------------

(defun smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; ---------------------------------------------------------
;; ------ http://www.emacswiki.org/emacs/move-text.el ------
;; ---------------------------------------------------------
(require 'move-text)
(move-text-default-bindings)

;; permanently enable Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;; ---------------------------------------------------------
;; ---------------  Code folding ---------------------------
;; ---------------------------------------------------------

(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)))


;; Remove grey vertical borders on edges of Emacs screen
(set-window-fringes nil 0 0) 

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat "/Users/mehulmandania/.emacs/tmp"))

;; ---------------------------------------------------------
;; --------------- http://web-mode.org/  -------------------
;; ---------------------------------------------------------

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(blink-cursor-mode 0)

(require 'clojure-mode-extra-font-locking)

;; Disable backups and autosaves
(setq backup-inhibited t)
(setq auto-save-default nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" default)))
 '(fci-rule-color "#383838")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))


(set-variable (quote scheme-program-name) "scm")

;; http://www.emacswiki.org/emacs/NeoTree 
(global-set-key [f8] 'neotree-toggle)
(setq projectile-switch-project-action 'neotree-projectile-action)
