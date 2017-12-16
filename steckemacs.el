;;; steckemacs.el --- steckemacs emacs configuration

;; Copyright 2017, Steckerhalter

;; Author: steckerhalter
;; Keywords: emacs configuration init
;; URL: https://github.com/steckerhalter/steckemacs.el

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs configuration that tries to fetch and compile everything necessary with my package manager `quelpa' directly from the source repos
;; To make it easier to navigate the init file, `outshine' is used for org-mode-like folding: use `C-M-i' to cycle

;;; Requirements:

;; Emacs 24.4

;;; Code:

;;; initialization
;; disable the GNU ELPA
(setq package-archives nil)
;; initialize the package system
(package-initialize)
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))
;; install use-package and the quelpa handler
(quelpa '(quelpa-use-package :fetcher github :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)

;;; key bindings
(use-package bind-key
  ;; A simple way to manage personal keybindings, provided by `use-package'
  :init
  ;; If non-nil, extract docstrings from lambdas, closures and keymaps if possible.
  (setq bind-key-describe-special-forms t)
  ;; free C-t and C-u they can be used as prefix keys
  (global-unset-key (kbd "C-t"))
  (global-unset-key (kbd "C-u"))

  (defun my-keyboard-translations (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      ;; translate C-i to H-i so it can be used apart from TAB
      (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
      ;; use C-h as backspace
      (define-key input-decode-map (kbd "C-h") (kbd "<backspace>"))
      (define-key input-decode-map (kbd "M-h") (kbd "<M-backspace>"))))
  (add-to-list 'after-make-frame-functions 'my-keyboard-translations)

  ;; minor mode to override bindings
  (defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;;;; personal functions
  (defun my-switch-to-scratch () (interactive)
         (switch-to-buffer "*scratch*"))

  (defun my-kill-buffer () (interactive)
         (kill-buffer (buffer-name)))

  (defun my-select-prev-window () (interactive)
         (select-window (previous-window)))

  (defun my-select-next-window () (interactive)
         (select-window (next-window)))

  (defun my-indent-whole-buffer () (interactive)
         (indent-region (point-min) (point-max)))

  (defun my-split-window()
    "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
    (interactive)
    (if (eq last-command 'my-split-window)
        (progn
          (jump-to-register :my-split-window)
          (setq this-command 'my-unsplit-window))
      (window-configuration-to-register :my-split-window)
      (switch-to-buffer-other-window nil)))

  (defmacro my-package-desc (info-type library)
    `(,(intern (format "package-desc-%s" info-type))
      (with-temp-buffer
        (insert-file-contents-literally (find-library-name (format "%s" ,library)))
        (package-buffer-info))))

  (defun my-insert-package-desc-summary ()
    (interactive)
    (let* ((name (thing-at-point 'symbol t))
           (summary (my-package-desc summary name)))
      (back-to-indentation)
      (open-line 1)
      (insert (format ";; %s" summary))))

  (defun my-show-file-name ()
    "Show the full path file name in the minibuffer."
    (interactive)
    (message (buffer-file-name))
    (kill-new (file-truename buffer-file-name)))

  (defun my-toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  (defun my-url-insert-file-contents (url)
    "Prompt for URL and insert file contents at point."
    (interactive "sURL: ")
    (url-insert-file-contents url))

  (defun my-sudo-edit (&optional arg)
    "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:"
                           (ido-read-file-name "Find file(as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  (defun my-timestamp ()
    (interactive)
    (let ((timestring (if current-prefix-arg
                          "%H:%M"
                        "%d.%m.%y")))
      (insert (format-time-string timestring))))

  (defun my-xdg-open-dir ()
    "Open directory in default external program."
    (interactive)
    (shell-command
     (concat "xdg-open " (shell-quote-argument
                          (expand-file-name default-directory)))))

;;;; global key bindings
  :bind
  (;; general
   ("<f1> <f1>" . universal-argument) 	;remap what was C-u
   ("H-i x" . kill-emacs)
   ("C-S-l" . package-list-packages)
   ("C-c d" . ispell-change-dictionary)
   ("C-t f" . flyspell-buffer)
   ("C-t C-f" . flyspell-mode)
   ("H-i C-p" . find-file)
   ("C-x C-r" . my-sudo-edit)
   ("C-c m" . menu-bar-mode)
   ("C-x C-u" . my-url-insert-file-contents)
   ("C-u C-e" . eww)
   ("M-1" . my-xdg-open-dir)
   ;; editing
   ("C-z" . undo-only)
   ("M-W" . delete-region)
   ("C-c q" . auto-fill-mode)
   ("C-c w" . whitespace-cleanup)
   ("H-i C-v" . visual-line-mode)
   ("H-i t" . my-timestamp)
   ("M-k" . kill-line)
   ("M-K" . kill-sentence)
   ;; source
   ("H-i C-0" . edebug-defun)
   ("H-i C-b" . eval-buffer)
   ("H-i C-e" . toggle-debug-on-error)
   ("C-t C-s" . my-insert-package-desc-summary)
   ;; buffers
   ("H-i H-i" . save-buffer)
   ("C-c r" . revert-buffer)
   ("<f6>" . my-kill-buffer)
   ("C-." . my-switch-to-scratch)
   ("H-i TAB" . my-indent-whole-buffer)
   ("C-c n" . my-show-file-name)
   ("H-i 0" . text-scale-adjust)
   ;; windows
   :map my-keys-minor-mode-map
   ;; minor mode is used so modes do not override
   (("M-n" . my-select-next-window)
    ("M-p" . my-select-prev-window))
   ("<f7>" . my-toggle-window-split)
   ("C-8" . my-split-window)
   ("<f2>" . split-window-vertically)
   ("<f3>" . split-window-horizontally)
   ("<f4>" . delete-window)
   ("<f5>" . delete-other-windows)
   ;; find/grep
   ("H-i G" . grep-find)
   ("C-S-u C-S-g" . find-grep-dired)
   ("H-i o" . helm-projectile-grep)
   ("H-i g" . helm-do-grep-ag)
   ("H-i O" . occur)))

;;; settings
(use-package steckemacs-settings
  :init
  ;;global flags
  (setq
   inhibit-startup-message t
   backup-directory-alist `((".*" . ,temporary-file-directory)) ;don't clutter my fs and put backups into tmp
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   require-final-newline t                ;auto add newline at the end of file
   column-number-mode t                   ;show the column number
   default-major-mode 'text-mode          ;use text mode per default
   mouse-yank-at-point t                  ;middle click with the mouse yanks at point
   history-length 250                     ;default is 30
   locale-coding-system 'utf-8            ;utf-8 is default
   tab-always-indent 'complete            ;try to complete before identing
   confirm-nonexistent-file-or-buffer nil ;don't ask to create a buffer
   vc-follow-symlinks t                   ;follow symlinks automatically
   recentf-max-saved-items 5000           ;save up to 5000 recent files
   eval-expression-print-length nil       ;do not truncate printed expressions
   eval-expression-print-level nil        ;print nested expressions
   send-mail-function 'sendmail-send-it
   kill-ring-max 5000                     ;truncate kill ring after 5000 entries
   mark-ring-max 5000                     ;truncate mark ring after 5000 entries
   mouse-autoselect-window -.1            ;window focus follows the mouse pointer
   mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))) ;make mouse scrolling smooth
   indicate-buffer-boundaries 'left       ;fringe markers (on the left side)
   split-height-threshold 110             ;more readily split horziontally
   enable-recursive-minibuffers t         ;whatever...
   show-paren-delay 0                     ;show the paren immediately
   load-prefer-newer t                    ;prefer newer .el instead of the .elc
   split-width-threshold 160              ;split horizontally only if less than 160 columns
   gc-cons-percentage 0.3                 ;increase garbage collection limit
   safe-local-variable-values '((engine . django))
   switch-to-buffer-preserve-window-point t ;this allows operating on the same buffer in diff. positions
   custom-file "/tmp/custom-file.el") ;don't pollute the init file and don't `load' the customs
                                        ;but keep them for reference...

  ;; default flags
  (setq-default
   tab-width 4
   indent-tabs-mode nil                   ;use spaces instead of tabs
   c-basic-offset 4                       ;"tab" with in c-related modes
   c-hungry-delete-key t)                 ;delete more than one space

  ;; disable full `yes' or `no' answers, `y' and `n' suffices
  (defalias 'yes-or-no-p 'y-or-n-p)

  (provide 'steckemacs-settings)

  :config

  ;; load custom user code
  (when (file-readable-p "~/.user.el")
    (load "~/.user.el"))

  ;; display the time in the mode-line
  (setq display-time-day-and-date t)
  (display-time)

  ;; encoding
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)

  ;; disable some global modes
  (blink-cursor-mode -1)       ;no cursor blinking
  (tool-bar-mode -1)           ;disable the awful toolbar
  (menu-bar-mode -1)           ;no menu, you can toggle it with C-c m
  (scroll-bar-mode -1)         ;disable the sroll bar

  ;;narrow to region should be enabled by default
  (put 'narrow-to-region 'disabled nil)

  ;; don't ask to kill buffers
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

  ;; default font
  (defvar my-font-attributes '(default nil :family "DejaVu Sans Mono" :height 90))
  ;; (defvar my-font-attributes '(default nil :family "Anonymous Pro" :height 89))
  (apply 'set-face-attribute  my-font-attributes))

;;; core packages
;;;; advice
;; An overloading mechanism for Emacs Lisp functions
(use-package advice
  :config
  (defadvice kill-region (before slick-cut activate compile)
    "When called interactively with no active region, kill a single
line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))

  (defadvice kill-buffer (around kill-buffer-around-advice activate)
    "Don't really kill *scratch* but only bury it."
    (let ((buffer-to-kill (ad-get-arg 0)))
      (if (equal buffer-to-kill "*scratch*")
          (bury-buffer)
        ad-do-it))))

;;;; appt
;; appointment notification functions
(use-package appt
  :init (setq
         appt-message-warning-time 30
         appt-display-interval 15
         appt-display-mode-line t             ;show in the modeline
         appt-display-format 'window)
  :config (appt-activate 1))

;;;; autorevert
;; revert buffers when files on disk change
(use-package autorevert
  :config
  ;; auto revert buffers when changed on disk
  (global-auto-revert-mode 1))

;;;; browse-url
(use-package browse-url
  :preface
  (defun my-browse-url-file (&optional file)
    (interactive)
    (cl-letf (((symbol-function 'browse-url) 'browse-url-firefox))
      (browse-url-of-file file)))
  :bind ("C-t C-b" . my-browse-url-file))

;;;; custom
;; tools for declaring and initializing options
(use-package custom
  :init
  (setq
   custom-unlispify-menu-entries nil ;M-x customize should not cripple menu entries
   custom-unlispify-tag-names nil) ;M-x customize should not cripple tags
  :bind ("C-S-g" . customize-group))

;;;; dired
;; directory-browsing commands
(use-package dired
  :demand
  :init
  (setq dired-auto-revert-buffer t)
  (setq dired-no-confirm
        '(byte-compile chgrp chmod chown copy delete load move symlink))
  (setq dired-deletion-confirmer (lambda (x) t))
  :bind (:map dired-mode-map ("`" . dired-toggle-read-only))
  :config
  ;; make rename use ido and not helm
  (put 'dired-do-rename 'ido 'find-file)
  ;; make copy use ido and not helm
  (put 'dired-do-copy 'ido 'find-file)

  (use-package wdired
    :init
    ;; allow changing of file permissions
    (setq wdired-allow-to-change-permissions t))

  (use-package dired+
    ;; dired+ adds some features to standard dired (like reusing buffers)
    :quelpa (dired+ :fetcher url :url "https://github.com/emacsmirror/emacswiki.org/raw/master/dired+.el")
    :bind ("C-]" . dired-jump)
    :defer 1
    :init
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-hide-details-propagate-flag nil)

    :config
    (diredp-toggle-find-file-reuse-dir 1)))

;;;; eldoc
;; display information about a function or variable in the the echo area
(use-package eldoc
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode))

;;;; elec-pair
;; Automatic parenthesis pairing
(use-package elec-pair
  :config
  ;;auto pair brackets, parens etc.
  (electric-pair-mode 1))

;;;; eww
;; Emacs Web Wowser (web browser) settings
(use-package eww
  :bind ("C-t C-u" . my-eww-browse-dwim)
  :config
  (setq eww-search-prefix "https://startpage.com/do/m/mobilesearch?query=")

  (defun my-eww-browse-dwim ()
    "`eww' browse \"do what I mean\".
 Browse the url at point if there is one. Otherwise use the last
 kill-ring item and provide that to `eww'. If it is an url `eww'
 will browse it, if not `eww' will search for it using a search
 engine."
    (interactive)
    (let ((arg (or
                (url-get-url-at-point)
                (current-kill 0 t))))
      (eww arg))))

;;;; flyspell
;;On-the-fly spell checker
(use-package flyspell
  :init (setq flyspell-auto-correct-binding [nil]))

;;;; frame
;; multi-frame management independent of window systems
(use-package frame
  :config
  ;; maximize emacs
  (modify-all-frames-parameters '((fullscreen . maximized)))

  (defun my-after-make-frame (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      ;; use symbola font for emoticons
      (when (find-font (font-spec :name "Symbola") frame)
        (dolist (range '((#x2600 . #x26ff)
                         (#x1f300 . #x1f5ff)
                         (#x1f600 . #x1f640)
                         (#x1f680 . #x1f6ff)))
          (set-fontset-font "fontset-default" range "Symbola")))))
  (add-to-list 'after-make-frame-functions 'my-after-make-frame)

  ;; better frame title
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b")))))

;;;; erc
;; Emacs ERC client settings
(use-package erc
  :quelpa (erc-hl-nicks :fetcher github :repo "leathekd/erc-hl-nicks")
  :config
  (add-hook 'erc-mode-hook (lambda ()
                             (erc-truncate-mode t)
                             (erc-fill-disable)
                             (set (make-local-variable 'scroll-conservatively) 1000)
                             (visual-line-mode)))
  (setq erc-timestamp-format "%H:%M "
        erc-fill-prefix "      "
        erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (setq erc-interpret-mirc-color t)
  (setq erc-kill-buffer-on-part t)
  (setq erc-kill-server-buffer-on-quit t)
  (setq erc-kill-queries-on-quit t)
  (setq erc-kill-server-buffer-on-quit t)
  (setq erc-server-send-ping-interval 45)
  (setq erc-server-send-ping-timeout 180)
  (setq erc-server-reconnect-timeout 60)
  (erc-track-mode t)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

  ;; ------ template for .user.el
  ;; (setq erc-prompt-for-nickserv-password nil)
  ;; (setq erc-server "hostname"
  ;;       erc-port 7000
  ;;       erc-nick "user"
  ;;       erc-user-full-name "user"
  ;;       erc-email-userid "user"
  ;;       erc-password "user:pw"
  ;;       )

  (defun my-erc-connect ()
    "Connect with ERC or open the last active buffer."
    (interactive)
    (if (erc-buffer-list)
        (erc-track-switch-buffer 1)
      (erc-tls :server erc-server :port erc-port :nick erc-nick :full-name erc-user-full-name :password erc-password)))
  :bind ("C-u e" . my-erc-connect))

;;;; ibuffer
(use-package ibuffer
  :init (setq ibuffer-default-display-maybe-show-predicates t)
  :bind  ("C-x b" . ibuffer))

;;;; ido
;; selection framework (used for file opening `C-x C-f' by me)
(use-package ido
  :demand
  :init
  (setq ido-enable-flex-matching t
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-everywhere t
        ido-default-buffer-method 'selected-window
        ido-max-prospects 32
        ido-use-filename-at-point 'guess
        ido-use-faces nil)

  :bind ("C-x C-b" . ido-switch-buffer)

  :config
  (ido-mode 1)

  (use-package flx-ido
    ;; flx integration for ido
    :quelpa (flx-ido :repo "lewang/flx" :fetcher github :files ("flx-ido.el"))
    :config
    (flx-ido-mode 1)))

;;;; ielm
;; interactively evaluate Emacs Lisp expressions
(use-package ielm
  :config
  (add-hook 'inferior-emacs-lisp-mode-hook
            (lambda ()
              (turn-on-eldoc-mode))))

;;;; man
;; browse UNIX manual pages
(use-package man
  :bind ("H-i m" . man)
  :init (setq Man-notify-method 'aggressive))

;;;; org
;;  "Outline-based notes management and organizer"
(use-package org
  :quelpa (org :url "git://orgmode.org/org-mode.git" :fetcher git
               :files ("lisp/*.el" "contrib/lisp/*.el" "doc/dir" "doc/*.texi"))
  :init
  (setq org-startup-indented t)
  (setq org-startup-with-inline-images t)
  (setq org-startup-truncated t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  (setq org-confirm-babel-evaluate nil)
  (setq org-use-speed-commands t)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-html-postamble nil)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-speed-commands-user '(("S" . org-schedule)))
  :config
  (use-package org-bullets
    :quelpa (org-bullets :fetcher github :repo "emacsorphanage/org-bullets")
    :config (add-hook 'org-mode-hook 'org-bullets-mode)))

;;;; paren
;; highlight matching paren
(use-package paren
  :config
  ;;visualize ( and )
  (show-paren-mode t))

;;;; prog-mode
;; Generic major mode for programming
(use-package prog-mode
  :config
  (defun my-prog-mode-hook ()
    (setq show-trailing-whitespace 1))
  (add-hook 'prog-mode-hook 'my-prog-mode-hook))

;;;; recentf
;; setup a menu of recently opened files
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "~/.recentf"))
  (recentf-mode 1))

;;;; savehist
;; Save minibuffer history
(use-package savehist
  :config
  (setq savehist-additional-variables
        '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history))
  (savehist-mode 1))

;;;; saveplace
;; automatically save place in files
(use-package saveplace
  :config (setq-default save-place t))

;;;; sgml
;; SGML- and HTML-editing modes
(use-package sgml-mode
  :config
  (setq sgml-basic-offset 4)
  (add-hook 'sgml-mode-hook 'sgml-electric-tag-pair-mode))

;;;; skeleton
;; Lisp language extension for writing statement skeletons
(use-package skeleton
  :bind (("C-t q" . liquid-quote)
         ("C-t l" . liquid-tag))
  :config
  (define-skeleton liquid-tag
    "Inserts a liquid tag"
    "tag: "
    "{% " str " " _ " %}" \n
    "{% end" str " %}")
  (define-skeleton liquid-quote
    "Inserts a liquid quote tag"
    "tag: "
    "{% quote " _ " %}" \n
    "{% endquote %}"))

;;;; term
;; general command interpreter in a window stuff
(use-package term
  :config
  (defun my-term-setup ()
    (interactive)
    (define-key term-raw-map (kbd "C-y") 'term-send-raw)
    (define-key term-raw-map (kbd "C-p") 'term-send-raw)
    (define-key term-raw-map (kbd "C-n") 'term-send-raw)
    (define-key term-raw-map (kbd "C-s") 'term-send-raw)
    (define-key term-raw-map (kbd "C-r") 'term-send-raw)
    (define-key term-raw-map (kbd "M-d") (lambda () (interactive) (term-send-raw-string "\ed")))
    (define-key term-raw-map (kbd "<C-backspace>") (lambda () (interactive) (term-send-raw-string "\e\C-?")))
    (define-key term-raw-map (kbd "M-p") (lambda () (interactive) (term-send-raw-string "\ep")))
    (define-key term-raw-map (kbd "M-n") (lambda () (interactive) (term-send-raw-string "\en")))
    (define-key term-raw-map (kbd "M-,") 'term-send-input)
    (define-key term-raw-map (kbd "C-c y") 'term-paste)
    (define-key term-raw-map (kbd "C-S-y") 'term-paste)
    (define-key term-raw-map (kbd "M-x") nil) ;unbind M-x
    (define-key term-raw-map (kbd "C-]") nil))
  (add-hook 'term-mode-hook 'my-term-setup t))

;;; external packages
;;;; add recipes that are required by some packages
(add-to-list
 'quelpa-melpa-recipe-stores
 '(;; eval-sexp-fu
   (highlight :fetcher github :repo "emacsmirror/highlight")
   ;; flycheck + magit
   (let-alist :fetcher url
              :url "http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/let-alist.el"
              :version original)
   ;; cider
   (queue :fetcher github :repo "emacsmirror/queue")
   (seq :fetcher github :repo "NicolasPetton/seq.el")
   (spinner :fetcher github :repo "Malabarba/spinner.el")))

;;;; anaconda-mode
;; Code navigation, documentation lookup and completion for Python
(use-package anaconda-mode
  :quelpa (anaconda-mode
           :fetcher github
           :repo "proofit404/anaconda-mode"
           :files ("*.el" "*.py" "vendor/jedi/jedi" ("jsonrpc" "vendor/jsonrpc/jsonrpc/*.py")))
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode)

  (use-package pip-requirements
    ;; A major mode for editing pip requirements files
    :quelpa (pip-requirements :repo "Wilfred/pip-requirements.el" :fetcher github))

  (use-package pyenv-mode
    :quelpa (pyenv-mode :fetcher github :repo "proofit404/pyenv-mode")
    :init

    (defun projectile-pyenv-mode-set ()
      "Set pyenv version matching project name.
       Version must be already installed."
      (let ((name (projectile-project-name)))
        (if (member name (pyenv-mode-versions))
            (pyenv-mode-set name)
          (pyenv-mode-set "system"))))

    (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

    :config (pyenv-mode 1)))

;;;; ag
;; A front-end for ag ('the silver searcher'), the C ack replacement.
(use-package ag
  :quelpa (ag :repo "Wilfred/ag.el" :fetcher github)
  :bind ("H-i C-g" . ag-project))

;;;; apache-mode
;; major mode for editing Apache configuration files
(use-package apache-mode
  :quelpa (apache-mode :fetcher github :repo "emacsmirror/apache-mode"))

;;;; ansible-doc
(use-package ansible-doc
  :quelpa (ansible-doc :repo "lunaryorn/ansible-doc.el" :fetcher github)
  :config (add-hook 'yaml-mode-hook #'ansible-doc-mode))

;;;; auctex
;; enhanced LaTeX mode
(setq TeX-PDF-mode t)
(setq TeX-parse-self t)
(setq TeX-auto-save t)
(setq TeX-save-query nil)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'TeX-mode-hook
          '(lambda ()
             (define-key TeX-mode-map (kbd "<C-f8>")
               (lambda ()
                 (interactive)
                 (TeX-command-menu "LaTeX")))))

;;;; back-button
;; Visual navigation through mark rings
(use-package back-button
  :quelpa (back-button :repo "rolandwalker/back-button" :fetcher github)
  :bind (("C-3" . back-button-local-backward)
         ("C-4" . back-button-local-forward))
  :config
  (setq back-button-local-keystrokes nil) ;don't overwrite C-x SPC binding
  (back-button-mode 1))

;;;; cider
;; Clojure Interactive Development Environment that Rocks
(use-package cider
  :quelpa (cider
           :fetcher github
           :repo "clojure-emacs/cider"
           :files ("*.el" (:exclude ".dir-locals.el"))
           :old-names (nrepl))
  :init
  (setq cider-popup-stacktraces nil)
  (setq cider-repl-popup-stacktraces nil)
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-repl-use-clojure-font-lock t))

;;;; company
;; Modular text completion framework
(use-package company
  :quelpa (company :repo "company-mode/company-mode" :fetcher github)
  :diminish company-mode

  :init
  (setq company-idle-delay 0.3)
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 2)
  (setq company-echo-delay 0)
  (setq company-auto-complete nil)

  :config
  (global-company-mode 1)
  (add-to-list 'company-backends 'company-dabbrev t)
  (add-to-list 'company-backends 'company-ispell t)
  (add-to-list 'company-backends 'company-files t)
  (add-to-list 'company-begin-commands 'outshine-self-insert-command)
  (setq company-backends (remove 'company-ropemacs company-backends))

  (defun my-company-elisp-setup ()
    (set (make-local-variable 'company-backends)
         '((company-capf :with company-dabbrev-code))))

  (use-package company-statistics
    ;; Usage based completion sorting
    :quelpa (company-statistics :repo "company-mode/company-statistics" :fetcher github)
    :hook ((emacs-lisp-mode lisp-interaction-mode) . my-company-elisp-setup)
    :config (company-statistics-mode)))

;;;; company-anaconda
;; Anaconda backend for company-mode
(use-package company-anaconda
  :quelpa (company-anaconda
           :fetcher github
           :repo "proofit404/company-anaconda")
  :config (add-to-list 'company-backends 'company-anaconda))

;;;; company-ansible
(use-package company-ansible
  :quelpa (company-ansible :repo "krzysztof-magosa/company-ansible" :fetcher github)
  :config (add-to-list 'company-backends 'company-ansible))

;;;; company-dict
(use-package company-dict
  :quelpa (company-dict :repo "hlissner/emacs-company-dict" :fetcher github)
  :config (add-to-list 'company-backends 'company-dict))

;;;; company-flx
(use-package company-flx
  :quelpa (company-flx :repo "PythonNut/company-flx" :fetcher github)
  :config (company-flx-mode +1))

;;;; company-quickhelp
;; Popup documentation for completion candidates
(use-package company-quickhelp
  :quelpa (company-quickhelp :fetcher github :repo "expez/company-quickhelp")
  :init (setq company-quickhelp-delay 1)
  :config (company-quickhelp-mode 1))

;;;; company-web
;; Company version of ac-html, complete for web,html,emmet,jade,slim modes
(use-package company-web
  :quelpa (company-web :repo "osv/company-web" :fetcher github)
  :config
  (defun my-company-web ()
    (set (make-local-variable 'company-backends) '(company-web-html))
    (company-mode t))
  :hook (web-mode . my-company-web))

;;;; deft
;; quickly browse, filter, and edit plain text notes
(use-package deft
  :quelpa (deft :url "https://jblevins.org/git/deft.git" :fetcher git)
  :bind (("C-," . deft)
         :map deft-mode-map
         ("<f6>" . quit-window)
         ("C-g" . deft-filter-clear)
         ("C-c C-c" . deft-refresh)
         ("<M-return>" . deft-new-file))
  :commands (deft)
  :config
  ;; display filter in mode-line instead of header
  (defun deft-print-header () (deft-set-mode-name))
  (defun deft-set-mode-name ()
    "Set the mode line text based on search mode and add the filter."
    (let* ((name (if deft-incremental-search "Deft" "Deft/R"))
           (filter (deft-whole-filter-regexp))
           (sep (unless (string= "" filter) "/")))
      (setq mode-name (concat name sep filter))))
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
                "\\|^# .*$" ;; md titles
                "\\)"))
  (setq deft-separator " ")
  (setq deft-directory "~/Sync/notes")
  (setq deft-file-naming-rules nil)
  (setq deft-recursive t)
  (setq deft-default-extension "org")
  (setq deft-extensions '("org"))
  (setq deft-auto-save-interval 0))

;;;; diatheke
(use-package diatheke
  :quelpa (diatheke :fetcher github :repo "steckerhalter/diatheke.el")
  :init
  (setq diatheke-bible "GerLut1912")
  (setq diatheke-locale "de")
  :hook (text-mode . diatheke-mode))

;;;; diff-hl
;; Highlight uncommitted changes
(use-package diff-hl
  :demand
  :quelpa (diff-hl :fetcher github :repo "dgutov/diff-hl")
  :bind ("C-u C-r" . diff-hl-revert-hunk)
  :config
  (global-diff-hl-mode 1)
  (eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;;;; discover-my-major
;; discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :quelpa (discover-my-major :fetcher github :repo "steckerhalter/discover-my-major")
  :bind ("H-i C-m" . discover-my-major))

;;;; dokuwiki-mode
(use-package dokuwiki-mode
  :quelpa (dokuwiki-mode :fetcher github :repo "kai2nenobu/emacs-dokuwiki-mode")
  :mode "\\.dokuwiki$")

;;;; easy-kill
;; make marking and killing easier
(use-package easy-kill
  :quelpa (easy-kill :fetcher github :repo "leoliu/easy-kill")
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;;;; elisp-slime-nav
;; jump to elisp definition (function, symbol etc.) and back, show doc
(use-package elisp-slime-nav
  :demand
  :quelpa (elisp-slime-nav :repo "purcell/elisp-slime-nav" :fetcher github)
  :bind
  ("H-i C-." . elisp-slime-nav-find-elisp-thing-at-point)
  ("H-i C-d" . my-show-help)
  ("H-i C-," . elisp-slime-nav-describe-elisp-thing-at-point)
  :diminish elisp-slime-nav-mode
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . elisp-slime-nav-mode))

;;;; eval-sexp-fu
;; flash the region that is evaluated (visual feedback) in elisp
(use-package eval-sexp-fu
  :quelpa (eval-sexp-fu  :fetcher github :repo "hchbaw/eval-sexp-fu.el")
  :bind
  (:map
   lisp-interaction-mode-map
   ("C-c C-c" . eval-sexp-fu-eval-sexp-inner-list)
   ("C-c C-e" . eval-sexp-fu-eval-sexp-inner-sexp)
   :map
   emacs-lisp-mode-map
   ("C-c C-c" . eval-sexp-fu-eval-sexp-inner-list)
   ("C-c C-e" . eval-sexp-fu-eval-sexp-inner-sexp))
  :init
  (setq eval-sexp-fu-flash-duration 0.4)
  :config
  (turn-on-eval-sexp-fu-flash-mode))

;;;; fasd
;; find previous files/dirs quickly (uses `fasd' shell script)
(use-package fasd
  :quelpa (fasd :repo "steckerhalter/emacs-fasd" :fetcher github)
  :bind ("H-i C-f" . fasd-find-file)
  :config
  (setq fasd-completing-read-function 'helm--completing-read-default)
  (global-fasd-mode 1))

;;;; flycheck
;; on-the-fly source code syntax checks
(use-package flycheck
  :requires let-alist
  :quelpa (flycheck :repo "flycheck/flycheck" :fetcher github)
  :hook ((php-mode sh-mode json-mode nxml-mode python-mode emacs-lisp-mode lisp-interaction-mode) . flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;disable the annoying doc checker
  (setq flycheck-indication-mode 'right-fringe))

;;;; git-modes
;; Emacs major modes for various Git configuration files
(use-package git-modes
  :defer t
  :quelpa (git-modes :fetcher github :repo "magit/git-modes"))

;;;; git-timemachine
;; step through historic versions of git controlled file
(use-package git-timemachine
  :quelpa (git-timemachine :fetcher github :repo "pidu/git-timemachine"))
;;;; go-mode
(use-package git-timemachine
  :quelpa (go-mode :repo "dominikh/go-mode.el" :fetcher github :files ("go-mode.el")))

;;;; google-translate
;; Emacs interface to Google's translation service
(use-package google-translate
  :quelpa (google-translate :fetcher github :repo "atykhonov/google-translate")
  :bind (("H-i r" . google-translate-query-translate)
         ("H-i C-r" . google-translate-query-translate-reverse))
  :init
  (setq google-translate-default-source-language "de")
  (setq google-translate-default-target-language "en"))

;;;; grandshell-theme
;; Grand Shell color theme for Emacs > 24
(use-package grandshell-theme
  :quelpa (grandshell-theme :repo "steckerhalter/grandshell-theme" :fetcher github)
  :config (load-theme 'grandshell t))

;;;; helm
;; fancy candidate selection framework
(use-package helm
  :unless (setq async-bytecomp-allowed-packages nil) ;disable async bytecomp
  :quelpa (helm :repo "emacs-helm/helm" :fetcher github :files ("*.el" "emacs-helm.sh"))
  :diminish helm-mode
  :commands helm-mini

  :init
  (setq helm-idle-delay 0.1)
  (setq helm-input-idle-delay 0.1)
  (setq helm-buffer-max-length 50)
  (setq helm-M-x-always-save-history t)
  (setq helm-buffer-details-flag nil)
  (setq helm-mode-handle-completion-in-region nil) ;don't use helm for `completion-at-point'

  :bind
  (("M-x" . helm-M-x)
   ("H-i ," . helm-apropos)
   ("H-i ." . helm-info-emacs)
   ("H-i 4" . helm-info-elisp)
   ("H-i 3" . helm-locate-library)
   ("H-i C-SPC" . helm-show-kill-ring)
   ("H-i SPC" . helm-all-mark-rings)
   ("H-i C-l" . helm-locate)
   ("H-i w" . helm-wikipedia-suggest)
   ("H-i i" . helm-imenu)
   :map my-keys-minor-mode-map ("C-;" . helm-mini))

  :config
  (require 'helm-config)
  (helm-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(dired-create-directory))
  (add-to-list 'helm-boring-buffer-regexp-list ":.*")

  (use-package helm-descbinds
    ;; Yet Another `describe-bindings' with `helm'.
    :quelpa (helm-descbinds :repo "emacs-helm/helm-descbinds" :fetcher github)
    :config (helm-descbinds-mode))

  (use-package helm-gtags
    ;; GNU GLOBAL helm interface
    :quelpa (helm-gtags :repo "syohex/emacs-helm-gtags" :fetcher github :files ("helm-gtags.el"))
    :diminish helm-gtags-mode
    :config (helm-gtags-mode 1))

  (use-package helm-projectile
    ;; Helm integration for Projectile
    :quelpa (helm-projectile :repo "bbatsov/helm-projectile" :fetcher github)
    :bind ("H-i h" . helm-projectile))

  (use-package helm-google
    ;; Emacs Helm Interface for quick Google searches
    :quelpa (helm-google :fetcher github :repo "steckerhalter/helm-google")
    :bind (("H-i C-o" . helm-google)
           ("H-i C-c" . helm-google-suggest)))

  (use-package helm-tramp
    :quelpa (helm-tramp :repo "masasam/emacs-helm-tramp" :fetcher github)
    :bind ("H-i C-t" . helm-tramp))

  (use-package helm-swoop
    ;; Efficiently hopping squeezed lines powered by helm interface
    :quelpa
    :bind (
           ("M-I" . helm-multi-swoop)
           :map my-keys-minor-mode-map ("M-i" . helm-swoop)
           :map isearch-mode-map ("M-i" . helm-swoop-from-isearch)
           :map helm-swoop-map ("M-i" . helm-multi-swoop-all-from-helm-swoop))))

;;;; highlight-parentheses
(use-package highlight-parentheses
  :quelpa (highlight-parentheses :repo "nschum/highlight-parentheses.el"
                                 :fetcher github)
  :hook (prog-mode . highlight-parentheses-mode)
  :init
  (setq hl-paren-delay 0.2)
  (setq hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4"))
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

;;;; highlight-symbol
;; automatic and manual symbol highlighting
(use-package highlight-symbol
  :quelpa (highlight-symbol :fetcher github :repo "nschum/highlight-symbol.el")
  :diminish highlight-symbol-mode
  :bind (("M-2" . highlight-symbol-occur)
         ("M-3" . highlight-symbol-prev)
         ("M-4" . highlight-symbol-next))
  :hook (prog-mode . highlight-symbol-mode)
  :init
  (setq highlight-symbol-on-navigation-p t))

;;;; iedit
;; change multiple occurences of word-at-point (compress display to show all of them)
(use-package iedit
  :quelpa (iedit :repo "victorhge/iedit" :fetcher github)
  :bind ("C-u H-i" . iedit-mode)
  :init
  (setq iedit-unmatched-lines-invisible t)
  (setq iedit-toggle-key-default nil))

;;;; iflipb
;; interactively flip between recently visited buffers
(use-package iflipb
  :quelpa (iflipb :repo "jrosdahl/iflipb" :fetcher github)

  :init
  ;; wrap around list
  (setq iflipb-wrap-around t)
  ;; don't ignore buffers starting with * (like magit etc.)
  (setq iflipb-ignore-buffers '("*helm.**"
                                "*magit-diff: .*"
                                "*Quail Completions*"
                                "*Completions*"
                                "*anaconda-mode*"
                                "*Deft*"
                                ":.*"))

  :bind (("<f8>" . iflipb-next-buffer)
         ("<f9>" . iflipb-previous-buffer)))

;;;; ipretty
;; pretty-print the result elisp expressions
(use-package ipretty
  :quelpa (ipretty :fetcher github :repo "steckerhalter/ipretty")
  :config (ipretty-mode t)
  :bind (("H-i C-j" . ipretty-last-sexp)
         ("H-i C-k" . ipretty-last-sexp-other-buffer)))

;;;; js2-mode
;; extended javascript mode
(use-package js2-mode
  :quelpa (js2-mode :repo "mooz/js2-mode" :fetcher github)
  :mode "\\.js$"
  :hook (js2-mode . flycheck-mode))

;;;; json-mode
;; syntax highlighting for `json'
(use-package json-mode
  :quelpa (json-mode :fetcher github :repo "joshwnj/json-mode")
  :mode "\\.json\\'")

;;;; magit
;; Emacs interface to git
(use-package magit
  :quelpa
  :bind (("C-u C-u" . magit-status)
         ("C-u l" . magit-log)
         ("C-u b" . magit-blame))
  :init
  (setq magit-push-always-verify nil)
  (setq git-commit-finish-query-functions nil)
  (setq magit-save-some-buffers nil) ;don't ask to save buffers
  (setq magit-set-upstream-on-push t) ;ask to set upstream
  (setq magit-diff-refine-hunk t) ;show word-based diff for current hunk
  (setq magit-default-tracking-name-function
        'magit-default-tracking-name-branch-only) ;don't track with origin-*

  :config
  (use-package git-auto-commit-mode
    :quelpa (git-auto-commit-mode :fetcher github
                                  :repo "ryuslash/git-auto-commit-mode")
    :commands (gac-commit gac)
    :config
    (defun gac ()
      (interactive)
      (gac-commit))))

;;;; markdown-mode
;; Emacs Major mode for Markdown-formatted text files
(use-package markdown-mode
  :quelpa (markdown-mode :fetcher github :repo "jrblevin/markdown-mode")
  :bind (:map markdown-mode-map
              ("C-M-i" . markdown-shifttab)
              ("<backtab>" . markdown-promote)
              ("<S-iso-lefttab>" . markdown-promote)
              ("<S-tab>" . markdown-promote)
              ("<C-tab>" . markdown-shifttab)
              ("<M-S-return>" . my-markdown-checkbox)
              ("<C-up>" . markdown-move-up)
              ("<C-down>" . markdown-move-down))
  :mode
  ("\\.markdown\\'" . gfm-mode)
  ("\\.md\\'" . gfm-mode)
  ("\\.lr\\'" . gfm-mode)

  :init
  (defun my-markdown-checkbox ()
    "Shortcut for list item with checkbox."
    (interactive)
    (call-interactively 'markdown-insert-list-item)
    (insert "[ ] "))

  (defun my-markdown-convert-wiki-link (func name)
    "Fix naming of files for wiki links."
    (let ((markdown-link-space-sub-char " ")
          (major-mode 'markdown-mode))
      (funcall func name)))

  (setq markdown-asymmetric-header t)
  (setq markdown-header-scaling t)
  (setq markdown-enable-wiki-links t)
  (setq markdown-list-indent-width 2)
  (setq markdown-enable-wiki-links t)
  (setq markdown-footnote-location 'immediately)
  (setq markdown-wiki-link-fontify-missing t)
  (setq markdown-wiki-link-alias-first nil)
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (setq markdown-command "pandoc -c http://benjam.info/panam/styling.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")

  :hook (markdown-mode . visual-line-mode)
  :config
  (advice-add 'markdown-convert-wiki-link-to-filename
              :around 'my-markdown-convert-wiki-link))

;;;; multiple-cursors
;; allow editing with multiple cursors
(use-package multiple-cursors
  :quelpa (multiple-cursors :fetcher github :repo "magnars/multiple-cursors.el")
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-?" . mc/mark-all-like-this)))

;;;; open-junk-file
;; Open a junk (memo) file to try-and-error
(use-package open-junk-file
  :quelpa (open-junk-file :repo "rubikitch/open-junk-file" :fetcher github)
  :bind ("H-i j" . open-junk-file)
  :init (setq open-junk-file-format "~/junk/%Y/%m/%d-%H%M%S."))

;;;; outshine
;; outline with outshine outshines outline
(use-package outshine
  :quelpa (outshine :fetcher github :repo "tj64/outshine")
  :bind ("M-# 3" . outshine-insert-heading)
  :diminish outline-minor-mode
  :commands outshine-hook-function
  :hook ((outline-minor-mode . outshine-hook-function)
         (emacs-lisp-mode . outline-minor-mode))
  :init
  (setq outshine-imenu-show-headlines-p nil))

;;;; page-break-lines
;; Display ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :quelpa (page-break-lines :fetcher github :repo "purcell/page-break-lines")
  :config (global-page-break-lines-mode))

;;;; package-lint
;; A linting library for elisp package authors
(use-package package-lint
  :quelpa (package-lint :fetcher github :repo "purcell/package-lint")
  :bind ("C-t C-l" . package-lint-current-buffer))

;;;; php
;; Major mode for editing PHP code
(use-package php-mode
  :quelpa (php-mode
           :repo "ejmr/php-mode"
           :fetcher github
           :files ("php-mode.el" "skeleton/*.el"))
  :mode "\\.module\\'"
  :init
  (setq php-mode-coding-style "Symfony2")
  (setq php-template-compatibility nil)

  (defun my-var_dump-die (start end)
    "Die me some var_dump quickly."
    (interactive "r")
    (if mark-active
        (progn
          (goto-char end)
          (insert "));")
          (goto-char start)
          (insert "die(var_dump("))
      (insert "die(var_dump());")))

  :config
  (use-package company-php
    :quelpa (company-php :repo "xcwen/ac-php" :fetcher github :files ("company-php.el"))
    :config (add-to-list 'company-backends 'company-ac-php-backend)
    :bind (:map php-mode-map
                ("M-." . ac-php-find-symbol-at-point)
                ("M-," . ac-php-location-stack-back)
                ("M-S-," . ac-php-location-stack-forward)
                ("H-i d" . my-var_dump-die)))

  (use-package php-eldoc
    :quelpa (php-eldoc :repo "sabof/php-eldoc" :fetcher github :files ("*.el" "*.php")))

  (defun setup-php-mode ()
    (php-eldoc-enable))

  :hook (php-mode . setup-php-mode))

;;;; pos-tip
;; Show tooltip at point
(use-package pos-tip
  :quelpa (pos-tip :repo "syohex/pos-tip" :fetcher github :files ("pos-tip.el"))
  :config
  (defun my-show-help ()
    "Show docs for symbol at point or at beginning of list if not on a symbol.
Pass symbol-name to the function DOC-FUNCTION."
    (interactive)
    (let* ((symbol (save-excursion
                     (or (symbol-at-point)
                         (progn (backward-up-list)
                                (forward-char)
                                (symbol-at-point)))))
           (doc-string (if (fboundp symbol)
                           (documentation symbol t)
                         (documentation-property
                          symbol 'variable-documentation t))))
      (if doc-string
          (pos-tip-show doc-string 'popup-tip-face (point) nil -1 60)
        (message "No documentation for %s" symbol))))
  ;; define key to show help in lisp-modes
  (define-key lisp-mode-shared-map (kbd "C-c C-d")
    (lambda ()
      (interactive)
      (my-show-help))))

;;;; projectile
;; Manage and navigate projects in Emacs easily
(use-package projectile
  :quelpa (projectile
           :repo "bbatsov/projectile"
           :fetcher github
           :files ("projectile.el"))
  :bind
  (("H-i C-u" . projectile-find-file)
   ("H-i C-y" . projectile-find-dir)
   ("H-i G" . projectile-grep)
   ("H-i z" . projectile-ack)
   ("H-i C-p" . projectile-switch-project))

  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line '(:eval (format " <%s>" (projectile-project-name))))

  :config
  (projectile-global-mode 1))

;;;; quelpa
;; Emacs Lisp packages built directly from source
(use-package quelpa
  :bind ("C-t C-e" . quelpa-expand-recipe))

;;;; rainbow-mode
;; Colorize color names in buffers
(use-package rainbow-mode
  :quelpa (rainbow-mode :fetcher github :repo "emacsmirror/rainbow-mode")
  :diminish rainbow-mode
  :hook (css-mode html-mode js-mode emacs-lisp-mode text-mode))

;;;; robe
;; Code navigation, documentation lookup and completion for Ruby
(use-package robe
  :quelpa (robe
           :repo "dgutov/robe"
           :fetcher github
           :files ("robe*.el" "company-robe.el" "lib"))
  :config
  (push 'company-robe company-backends)
  :hook (ruby-mode . robe-mode))

;;;; smart-mode-line
;; A color coded smart mode-line.
(use-package smart-mode-line
  :quelpa (smart-mode-line :repo "Bruce-Connor/smart-mode-line" :fetcher github)

  :init
  (setq sml/vc-mode-show-backend t)
  (setq sml/no-confirm-load-theme t)

  :config
  (sml/setup)
  (sml/apply-theme 'automatic))

;;;; sqlformat
;; Use sqlformat to make SQL readable in Emacs
(use-package sqlformat
  :quelpa (sqlformat :fetcher github :repo "steckerhalter/sqlformat.el"))

;;;; stylus-mode
;; Major mode for editing .jade files
(use-package stylus-mode
  :quelpa (stylus-mode :fetcher github :repo "brianc/jade-mode" :files ("stylus-mode.el")))

;;;; systemd
(use-package systemd
  :quelpa (systemd :fetcher github :repo "holomorph/systemd-mode" :files (:defaults "*.txt")))

;;;; term+
(use-package term+mux
  :quelpa (term+mux :repo "tarao/term-plus-mux-el" :fetcher github)
  :bind (("C-'" . term+mux-other-window)
         ("C-\"" . term+mux-new))
  :config
  (define-key term-raw-map (kbd "M-8") 'term+mux-new)
  (define-key term-raw-map (kbd "M-9") 'tab-group:next)
  (define-key term-raw-map (kbd "M-0") 'tab-group:prev))

;;;; toml-mode
;; Major mode for editing toml files
(use-package toml-mode
  :quelpa (toml-mode :fetcher github :repo "dryman/toml-mode.el"))

;;;; yaml-mode
;; Major mode for editing YAML files
(use-package yaml-mode
  :quelpa (yaml-mode :repo "yoshiki/yaml-mode" :fetcher github))

;;;; visual-regexp
(use-package use-package
  :quelpa (visual-regexp :repo "benma/visual-regexp.el" :fetcher github)
  :bind ("C-u v" . vr/replace))

;;;; vlf
;; View Large Files
(use-package vlf
  :quelpa (vlf :repo "m00natic/vlfi" :fetcher github :old-names (vlfi))
  :init
  (setq vlf-application 'dont-ask)   ;just do it
  (setq vlf-batch-size 8192))        ;a bit more text per batch please

;;;; web-mode
;; major mode for editing web templates
(use-package web-mode
  :quelpa (web-mode :repo "fxbois/web-mode" :fetcher github)
  :mode ("\\.html?\\'"
         "\\.ejs?\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-engine-detection t)
  (defun setup-web-mode ()
    (set (make-local-variable 'electric-pair-mode) nil)) ;disable electric-pairing in web-mode
  :hook (web-mode . setup-web-mode))

;;;; my-keys-minor-mode (must be last)
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t nil 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

;;; steckemacs.el ends here
