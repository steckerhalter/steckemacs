;;; steckemacs.el --- steckemacs emacs configuration

;; Copyright 2018, Steckerhalter

;; Author: steckerhalter
;; Keywords: emacs configuration init
;; URL: https://framagit.org/steckerhalter/steckemacs.el

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
(unless (and (boundp 'package--initialized) 
	     package--initialized)
  (package-initialize))
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))
;; install use-package and the quelpa handler
(quelpa '(quelpa-use-package :fetcher github :repo "quelpa/quelpa-use-package"))
(setq use-package-expand-minimally t)
(require 'quelpa-use-package)

;;; diminish
;; Diminished modes are minor modes with no modeline display
(use-package diminish
  :quelpa (diminish :fetcher github :repo myrjola/diminish.el))

;;; settings
(use-package steckemacs-settings
  :init
  ;; personal variables
  (defvar my-todo "~/Sync/notes/todo.org")

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
   custom-file (expand-file-name "custom-file.el" temporary-file-directory) ;don't pollute the init file and don't `load' the customs but keep them for reference...
   initial-buffer-choice my-todo)

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
  (setq display-time-24hr-format t)
  (setq display-time-default-load-average nil)
  (setq display-time-use-mail-icon t)
  (display-time)

  ;; encoding
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)

  ;; disable some global modes
  (blink-cursor-mode -1)       ;no cursor blinking
  (menu-bar-mode -1)           ;no menu, you can toggle it with C-c m
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; narrow to region should be enabled by default
  (put 'narrow-to-region 'disabled nil)

  ;; don't ask to kill buffers
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

  ;; default font
  (defvar my-font-attributes '(default nil :family "fixed" :width semi-condensed :height 120))
  ;; (defvar my-font-attributes '(default nil :family "DejaVu Sans Mono" :height 90))
  ;; (defvar my-font-attributes '(default nil :family "Anonymous Pro" :height 90))
  (apply 'set-face-attribute  my-font-attributes))

;;; key bindings
(use-package bind-key
  ;; A simple way to manage personal keybindings, provided by `use-package'
  :init
  ;; If non-nil, extract docstrings from lambdas, closures and keymaps if possible.
  (setq bind-key-describe-special-forms t)

  (defun my-keyboard-translations (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      (define-key input-decode-map (kbd "C-h") (kbd "<backspace>"))
      (define-key input-decode-map (kbd "M-h") (kbd "<M-backspace>"))))
  (add-to-list 'after-make-frame-functions 'my-keyboard-translations)

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
                        "%a %d.%m.%y %H:%M")))
      (insert (format-time-string timestring))))

  (defun my-xdg-open-dir ()
    "Open directory in default external program."
    (interactive)
    (shell-command
     (concat "xdg-open " (shell-quote-argument
                          (expand-file-name default-directory)))))

  (defun my-browse-url-dwim ()
    "Browse url \"do what I mean\".
 Browse the url at point if there is one. Otherwise use the last
 kill-ring item if it is an url. Browse with desktop browser by
 default. With prefix argument use `eww', with `digit-argument' 0
 use `xwidget'."
    (interactive)
    (require 'ffap)
    (let* ((input (or
                   (shr-url-at-point nil)
                   (url-get-url-at-point)
                   (when (eq major-mode 'org-mode)
                     (org-element-property
                      :raw-link (org-element-lineage (org-element-context) '(link) t)))
                   (current-kill 0 t)))
           (url (if (string-match-p ffap-url-regexp input)
                    input
                  (read-string "URL: ")))
           (prefix (prefix-numeric-value current-prefix-arg)))
      (cond ((= prefix 0) (xwidget-webkit-browse-url url))
            ((= prefix 4) (eww url))
            (t (browse-url url)))))

;;;; global key bindings
  :bind
  ("<f6>" . my-kill-buffer)
  ("<f7>" . my-toggle-window-split)
  ("C-8" . my-split-window)
  ("<f2>" . split-window-vertically)
  ("S-<f2>" . make-frame-command)
  ("<f3>" . split-window-horizontally)
  ("<f4>" . delete-window)
  ("S-<f4>" . delete-frame)
  ("<f5>" . delete-other-windows)
  ("S-<f5>" . delete-other-frames)
  ("C-c c" . (org-capture nil "s")))

;; Make bindings that stick around.
(use-package hydra
  :quelpa (hydra :repo "abo-abo/hydra" :fetcher github)
  :bind
  ("M-SPC" . !/body)
  ("<menu>" . !/body)
  :config
  (defun kbds (keys)
    "Simulate keyboard input.
KEYS should be provided as with `kbd'."
    (execute-kbd-macro (kbd keys)))

  (defmacro hydra-resume (fn &rest args)
    "Execute FN and resume the current hydra."
    `(progn (if (commandp ',fn)
                (call-interactively ',fn)
              (,fn ,@args))
            (funcall hydra-curr-body-fn)))

  (defmacro hydra-arg (fn &rest plist)
    "Execute FN or with arg the function given in the PLIST.
PLIST are pairs of the numerical argument and function, for example to call `find-file' with C-u, use: 4 find-file"
    ;; Example:
    ;; ("SPC f" (hydra-arg ff-helm-places 4 hydra-ff) :exit t)
    ;; (defhydra hydra-ff (:color blue :pre (setq hydra-is-helpful t) :post (!/body))
    ;;   "Firefox"
    ;;   ("f" ff-helm-places "History")
    ;;   ("b" ff-helm-bookmarks "Bookmarks")
    ;;   ("u" ff-paste-current-url "Yank current url")
    ;;   ("q" nil "quit"))
    `(let* ((prefix (prefix-numeric-value current-prefix-arg))
            (fnp (plist-get ',plist prefix)))
       (funcall (or fnp ',fn))))

  (defun !/state (&optional exit)
    ;; TODO: add state to mode-line
    (setq hydra-is-helpful exit)
    (if exit
        (progn
          (setq which-key-show-docstrings 'docstring-only)
          (custom-theme-recalc-face 'cursor))
      (set-face-background 'cursor "#ff5f87")
      (setq which-key-show-docstrings nil)))

  (defhydra ! (:color pink
                      :pre (!/state)
                      :post (!/state t))
    ;; edit
    ("a" (kbds "C-a"))
    ("A" (kbds "M-m"))
    ("f" (kbds "C-e"))
    ("o" (kbds "C-p"))
    ("i" (kbds "C-n"))
    ("j" (kbds "C-b"))
    (";" (kbds "C-f"))
    ("d" (kbds "C-v"))
    ("s" (kbds "M-v"))
    ("h" (kbds "C-d"))
    ("H" (kbds "M-d"))
    ("D" (kbds "M->"))
    ("F" (hydra-resume helm-find-files) :exit t)
    ("M-f" (hydra-resume project-find-file) :exit t)
    ("k" (kbds "C-k"))
    ("I" back-button-local-forward)
    ("M-i" highlight-symbol-next)
    ("S" (kbds "M-<"))
    ("p" pophint:do-flexibly)
    ("O" back-button-local-backward)
    ("M-o" highlight-symbol-prev)
    ("x" helm-M-x)
    ("/" undo)
    ("c" (org-capture nil "s"))
    ;; mark
    ("w" (kbds "C-SPC"))
    ("M-w" easy-mark-sexp)
    ("M-W" mark-whole-buffer)
    ("W" (kbds "M-w"))
    ("n" (kbds "C-w"))
    ("N" org-cut-special)
    ("y" yank)
    ("Y" yank-pop)
    ;; buffers
    ("l" recenter-top-bottom)
    ("<backtab>" outshine-cycle-buffer)
    ;; windows
    ("=" default-text-scale-increase)
    ("+" default-text-scale-decrease)
    ("," my-select-prev-window)
    ("." my-select-next-window)
    ;; misc
    (">" mc/mark-next-like-this)
    ("<" mc/mark-previous-like-this)
    ("[" helm-swoop)
    ("]" (hydra-resume isearch-forward) :exit t)
    ("-" shell-switcher-switch-buffer :exit t)
    ("_" shell-switcher-new-shell :exit t)
    ("b" helm-mini)
    ("B" (switch-to-buffer nil))
    ("M-b" my-switch-to-scratch)
    ("9" eval-sexp-fu-eval-sexp-inner-list)
    ("M-9" eval-sexp-fu-eval-sexp-inner-sexp)
    ("0" eval-last-sexp)
    ;; commands
    ("SPC SPC" save-buffer)
    ("SPC ." elisp-slime-nav-find-elisp-thing-at-point)
    ("SPC ," pop-tag-mark)
    ("SPC %" (insert "¯\\_(ツ)_/¯"))
    ("SPC ;" (find-file my-todo))
    ("SPC /" helm-rg)
    ("SPC 4" mu4e :exit t)
    ("SPC $ l" org-mu4e-store-and-capture)
    ("SPC $ c" mu4e-compose-new)
    ("SPC a e" my-erc-connect)
    ("SPC a f" elfeed)
    ("SPC a M" mastodon)
    ("SPC a m" mastodon-toot)
    ("SPC a h" hackernews)
    ("SPC a d" daemons)
    ("SPC b r" revert-buffer)
    ("SPC b s" my-sudo-edit)
    ("SPC b i" (hydra-resume iedit-mode) :exit t)
    ("SPC c" customize-group)
    ("SPC C" zenity-cp-color-at-point-dwim)
    ("SPC d" (hydra-resume deft) :exit t)
    ("SPC e e" edebug-defun)
    ("SPC e b" eval-buffer)
    ("SPC e d" toggle-debug-on-error)
    ("SPC f" ff-helm-places)
    ("SPC F b" ff-helm-bookmarks)
    ("SPC F u" ff-paste-current-url)
    ("SPC g" magit-status :exit t)
    ("SPC G l" magit-log-all)
    ("SPC G b" magit-blame)
    ("SPC G r" diff-hl-revert-hunk)
    ("SPC G p" diff-hl-previous-hunk)
    ("SPC G n" diff-hl-next-hunk)
    ("SPC h a" helm-apropos)
    ("SPC h d" elisp-slime-nav-describe-elisp-thing-at-point)
    ("SPC h s" info-lookup-symbol)
    ("SPC h i" info)
    ("SPC h I" helm-info)
    ("SPC h r" info-emacs-manual)
    ("SPC h R" helm-info-emacs)
    ("SPC h p" helm-info-at-point)
    ("SPC i" ipretty-last-sexp)
    ("SPC I" ipretty-last-sexp-other-buffer)
    ("SPC j" dired-jump)
    ("SPC k" kill-emacs)
    ("SPC l" (rename-file (org-latex-export-to-pdf nil t)
                          (concat "~/ownCloud/chords/"
                                  (car (split-string (org-entry-get nil "ITEM") "-" t split-string-default-separators))
                                  ".pdf")
                          t))
    ("SPC m" man)
    ("SPC n" my-org-agenda)
    ("SPC o o" org-open-at-point)
    ("SPC o T" (org-insert-time-stamp (current-time) t t))
    ("SPC o t" (org-set-tags-command))
    ("SPC o i" org-toggle-inline-images)
    ("SPC p l" list-packages)
    ("SPC p s" helm-system-packages)
    ("SPC p d" my-insert-package-desc-summary)
    ("SPC p p" my-package-def)
    ("SPC p p" helm-locate-library)
    ("SPC q" quelpa)
    ("SPC Q" quelpa-expand-recipe)
    ("SPC r" helm-show-kill-ring)
    ("SPC R" helm-all-mark-rings)
    ("SPC s" helm-google-searx)
    ("SPC S" helm-google-google)
    ("SPC u" my-browse-url-dwim)
    ("SPC v" visual-line-mode)
    ("SPC w" (lambda ()
               (interactive)
               (let ((which-key-show-docstrings 'docstring-only))
                 (which-key-show-major-mode))))
    ("SPC z" zoom-window-zoom)
    ("M-SPC" nil :color blue)
    ("<menu>" nil :color blue)))

;;; packages
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
    (setq show-trailing-whitespace 1)
    (prettify-symbols-mode 1))
  :hook (prog-mode . my-prog-mode-hook))

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
        '(kill-ring
          mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history))
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

;;;; shr
;; Simple HTML Renderer
(use-package shr
  ;; don't use proportional fonts
  :config (setq shr-use-fonts nil))

;;;; skeleton
;; Lisp language extension for writing statement skeletons
(use-package skeleton
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
    "{% endquote %}")
  (define-skeleton my-package-def
    "Inserts use-package definition"
    "pkg: "
    "(use-package " str \n
    ":quelpa " (format "%s" (quelpa-expand-recipe str)) ")"))

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
    (define-key term-raw-map (kbd "C-S-y") 'term-paste)
    (define-key term-raw-map (kbd "M-x") nil) ;unbind M-x
    (define-key term-raw-map (kbd "C-]") nil))
  (add-hook 'term-mode-hook 'my-term-setup t))

;;;; xwidget
;; api functions for xwidgets
(use-package x-widget
  :bind (:map xwidget-webkit-mode-map
              ("<mouse-4>" . xwidget-webkit-scroll-down)
              ("<mouse-5>" . xwidget-webkit-scroll-up)
              ("<up>" . xwidget-webkit-scroll-down)
              ("<down>" . xwidget-webkit-scroll-up)
              ("M-w" . xwidget-webkit-copy-selection-as-kill))
  :hook (window-configuration-change . (lambda ()
                                         (when (equal major-mode 'xwidget-webkit-mode)
                                           (xwidget-webkit-adjust-size-dispatch)))))

;;;; add recipes that are required by some packages
(add-to-list
 'quelpa-melpa-recipe-stores
 '(;; eval-sexp-fu
   (highlight :fetcher github :repo "steckerhalter/highlight.el")
   ;; magit
   (let-alist :fetcher url
              :url "http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/let-alist.el"
              :version original)
   ;; cider
   (queue :fetcher github :repo "emacsmirror/queue")
   (seq :fetcher github :repo "NicolasPetton/seq.el")
   (spinner :fetcher github :repo "Malabarba/spinner.el")))

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

;;;; ag
;; A front-end for ag ('the silver searcher'), the C ack replacement.
(use-package ag
  :quelpa (ag :repo "Wilfred/ag.el" :fetcher github))

;;;; aggressive-indent
(use-package aggressive-indent
  :quelpa (aggressive-indent :repo "Malabarba/aggressive-indent-mode" :fetcher github)
  :config (global-aggressive-indent-mode 1))

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

  ;; A major mode for editing pip requirements files
  (use-package pip-requirements
    :quelpa (pip-requirements :repo "Wilfred/pip-requirements.el" :fetcher github))

  ;; Integrate pyenv with python-mode
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

;;;; ansible-doc
;; Ansible documentation Minor Mode
(use-package ansible-doc
  :quelpa (ansible-doc :repo "lunaryorn/ansible-doc.el" :fetcher github)
  :config (add-hook 'yaml-mode-hook #'ansible-doc-mode))

;;;; apache-mode
;; major mode for editing Apache configuration files
(use-package apache-mode
  :quelpa (apache-mode :fetcher github :repo "emacsmirror/apache-mode"))

;;;; appt
;; appointment notification functions
(use-package appt
  :init (setq
         appt-message-warning-time 30
         appt-display-interval 15
         appt-display-mode-line t             ;show in the modeline
         appt-display-format 'window)
  :config (appt-activate 1))

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

;;;; autorevert
;; revert buffers when files on disk change
(use-package autorevert
  :diminish auto-revert-mode
  :config
  ;; auto revert buffers when changed on disk
  (global-auto-revert-mode 1))

;;;; back-button
;; Visual navigation through mark rings
(use-package back-button
  :quelpa (back-button :repo "rolandwalker/back-button" :fetcher github)
  :diminish
  :demand
  :config
  (setq back-button-local-keystrokes nil) ;don't overwrite C-x SPC binding
  (back-button-mode 1))

;;;; browse-url
(use-package browse-url
  :preface
  (defun my-browse-url-file (&optional file)
    (interactive)
    (cl-letf (((symbol-function 'browse-url) 'browse-url-firefox))
      (browse-url-of-file file))))

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
  :diminish

  :init
  (setq company-idle-delay nil)
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 2)

  :config
  (company-tng-configure-default)       ;TAB selects candidates

  ;; use TAB to trigger completion ----------------------------------------------
  ;; https://github.com/company-mode/company-mode/issues/94#issuecomment-40884387
  (define-key company-mode-map [remap indent-for-tab-command]
    'company-indent-for-tab-command)

  (setq tab-always-indent 'complete)

  (defvar completion-at-point-functions-saved nil)

  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common)))
  ;; ----------------------------------------------------------------------------

  (global-company-mode 1)
  (add-to-list 'company-backends 'company-dabbrev t)
  (add-to-list 'company-backends 'company-ispell t)
  (add-to-list 'company-backends 'company-files t)
  (add-to-list 'company-begin-commands 'outshine-self-insert-command)
  (setq company-backends (remove 'company-ropemacs company-backends))

  (defun my-company-elisp-setup ()
    (set (make-local-variable 'company-backends)
         '((company-capf :with company-dabbrev-code))))

  ;; Usage based completion sorting
  (use-package company-statistics
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

;;;; company-dict
;; A backend that emulates ac-source-dictionary
(use-package company-dict
  :quelpa (company-dict :repo "hlissner/emacs-company-dict" :fetcher github)
  :config (add-to-list 'company-backends 'company-dict))

;;;; company-quickhelp
;; Popup documentation for completion candidates
(use-package company-quickhelp
  :quelpa (company-quickhelp :fetcher github :repo "expez/company-quickhelp")
  :init
  (setq company-quickhelp-use-propertized-text t)
  (setq company-quickhelp-delay 1)
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

;;;; custom
;; tools for declaring and initializing options
(use-package custom
  :init
  (setq
   ;; M-x customize should not cripple menu entries
   custom-unlispify-menu-entries nil
   ;;M-x customize should not cripple tags
   custom-unlispify-tag-names nil))

;;;; darkroom
(use-package darkroom
  :quelpa (darkroom :fetcher github :repo "joaotavora/darkroom")
  :bind ("S-<f11>" . darkroom-tentative-mode)
  :custom
  (darkroom-text-scale-increase 3)
  (darkroom-margins-if-failed-guess 0.1))

;;;; default-text-scale
(use-package default-text-scale
  :quelpa (default-text-scale :fetcher github :repo "purcell/default-text-scale"))

;;;; deft
;; quickly browse, filter, and edit plain text notes
(use-package deft
  :quelpa (deft :url "https://jblevins.org/git/deft.git" :fetcher git)
  :bind  (:map deft-mode-map
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

;;;; diff-hl
;; Highlight uncommitted changes
(use-package diff-hl
  :demand
  :quelpa (diff-hl :fetcher github :repo "dgutov/diff-hl")
  :config
  (global-diff-hl-mode 1)
  (eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;;;; dired
;; directory-browsing commands
(use-package dired
  :demand
  :init
  (defun my-find-name-dired (pattern)
    "Find files in `default-directory' using `rg' if available.
PREFIX forces the use of `find'."
    (interactive "sFind-name (filename wildcard): ")
    (if (and (not current-prefix-arg) (executable-find "rg"))
        (let ((find-program (concat "rg -g " (shell-quote-argument pattern) " --files"))
              (find-ls-option (cons "" "-dilsb")))
          (find-dired default-directory ""))
      (find-dired
       default-directory
       (concat find-name-arg " " (shell-quote-argument pattern)))))

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

  ;; Rename files editing their names in dired buffers
  (use-package wdired
    :init
    ;; allow changing of file permissions
    (setq wdired-allow-to-change-permissions t))

  ;; dired+ adds some features to standard dired (like reusing buffers)
  (use-package dired+
    :quelpa (dired+ :fetcher url :url "https://www.emacswiki.org/emacs/download/dired+.el")
    :defer 1
    :init
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-hide-details-propagate-flag nil)

    :config
    (diredp-toggle-find-file-reuse-dir 1)))

;;;; discover-my-major
;; discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :quelpa (discover-my-major :fetcher git :url "https://framagit.org/steckerhalter/discover-my-major.git"))

;;;; easy-kill
;; make marking and killing easier
(use-package easy-kill
  :quelpa (easy-kill :fetcher github :repo "leoliu/easy-kill")
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;;;; eldoc
;; display information about a function or variable in the the echo area
(use-package eldoc
  :diminish
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode))

;;;; elec-pair
;; Automatic parenthesis pairing
(use-package elec-pair
  :config
  ;;auto pair brackets, parens etc.
  (electric-pair-mode 1))

;;;; elisp-slime-nav
;; jump to elisp definition (function, symbol etc.) and back, show doc
(use-package elisp-slime-nav
  :demand
  :quelpa (elisp-slime-nav :repo "purcell/elisp-slime-nav" :fetcher github)
  :bind
  ("<f1> <f1>" . elisp-slime-nav-describe-elisp-thing-at-point)
  :diminish
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . elisp-slime-nav-mode))

;;;; emms
;; Playlist mode for Emms
(use-package emms
  :quelpa (emms :url "https://git.savannah.gnu.org/git/emms.git"
                :fetcher git :files ("lisp/*.el" "doc/emms.texinfo"))
  :bind
  ("<XF86AudioPlay>" . emms-pause)
  ("<XF86AudioPrev>" . emms-previous)
  ("<XF86AudioNext>" . emms-next)
  ("S-<XF86AudioPause>" . emms-toggle-repeat-track)
  ("S-<XF86AudioPrev>" . emms-seek-backward)
  ("S-<XF86AudioNext>" . emms-seek-forward)
  :config
  (use-package emms-setup
    :custom (emms-player-list '(emms-player-mpv))
    :config
    (emms-standard)))

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
  (setq erc-join-buffer 'window-noselect)
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
        (switch-to-buffer (car (erc-buffer-list)))
      (erc-tls :server erc-server :port erc-port :nick erc-nick :full-name erc-user-full-name :password erc-password))))

;;;; eshell
(use-package eshell
  :demand
  :hook ((eshell-mode . my-eshell-setup)
         (eshell-mode . eldoc-mode))
  :init
  (setq eshell-save-history-on-exit t)
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-hist-ignoredups t)
  (setq eshell-history-size 10000)

  (defun eshell/g (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))   ;; The echo command suppresses output

  (defun my-eshell-setup ()
    (bind-key "C-c p" 'helm-eshell-prompts eshell-mode-map)
    (bind-key "M-r" 'helm-eshell-history eshell-mode-map)
    (setq-local eldoc-idle-delay 3)
    (setenv "PAGER" "cat")
    (setenv "EDITOR" "emacsclient"))

  :config

  (use-package em-alias
    :config
    (eshell/alias "cs" "apt search $1")
    (eshell/alias "e" "find-file $1")
    (eshell/alias "eo" "find-file-other-window $1")
    (eshell/alias "gd" "magit-diff-unstaged")
    (eshell/alias "gds" "magit-diff-staged")
    (eshell/alias "d" "dired $1")
    (eshell/alias "ll" "ls -l")
    (eshell/alias "la" "ls -A")
    (eshell/alias "l" "ls -CF"))

  (use-package esh-help
    :quelpa (esh-help :repo tom-tan/esh-help :fetcher github)
    :config (setup-esh-help-eldoc))

  ;; BASH completion for the shell buffer
  (use-package bash-completion
    :quelpa (bash-completion :repo szermatt/emacs-bash-completion :fetcher github)
    :config
    (defun eshell-bash-completion ()
      (setq-local bash-completion-nospace t)
      (while (pcomplete-here
              (nth 2 (bash-completion-dynamic-complete-nocomint
                      (save-excursion (eshell-bol) (point)) (point))))))
    (setq eshell-default-completion-function 'eshell-bash-completion))
  (use-package em-smart
    :hook (eshell-mode . eshell-smart-initialize)))

;;;; eval-sexp-fu
;; flash the region that is evaluated (visual feedback) in elisp
(use-package eval-sexp-fu
  :quelpa (eval-sexp-fu  :fetcher github :repo "hchbaw/eval-sexp-fu.el")
  :bind
  (:map lisp-interaction-mode-map
        ("C-c C-c" . eval-sexp-fu-eval-sexp-inner-list)
        ("C-c C-e" . eval-sexp-fu-eval-sexp-inner-sexp)
        :map emacs-lisp-mode-map
        ("C-c C-c" . eval-sexp-fu-eval-sexp-inner-list)
        ("C-c C-e" . eval-sexp-fu-eval-sexp-inner-sexp))
  :init
  (setq eval-sexp-fu-flash-duration 0.4)
  :config
  (turn-on-eval-sexp-fu-flash-mode))

;;;; eww
;; Emacs Web Wowser (web browser) settings
(use-package eww
  :config
  (setq eww-search-prefix "https://startpage.com/do/m/mobilesearch?query="))

;;;; flymake
(use-package flymake
  :diminish
  :hook ((php-mode sh-mode json-mode nxml-mode python-mode emacs-lisp-mode lisp-interaction-mode) . flymake-mode-on)
  :config (flymake-mode-on))

;;;; flyspell
;;On-the-fly spell checker
(use-package flyspell
  :init (setq flyspell-auto-correct-binding [nil]))

;;;; frame
;; multi-frame management independent of window systems
(use-package frame
  :config
  ;; maximize emacs
  (modify-all-frames-parameters '((fullscreen . fullboth)))

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

;;;; git-modes
;; Emacs major modes for various Git configuration files
(use-package git-modes
  :defer t
  :quelpa (git-modes :fetcher github :repo "magit/git-modes"))

;;;; google-translate
;; Emacs interface to Google's translation service
(use-package google-translate
  :quelpa (google-translate :fetcher github :repo "atykhonov/google-translate")
  :init
  (setq google-translate-default-source-language "de")
  (setq google-translate-default-target-language "en"))

;;;; grandshell-theme
;; Grand Shell color theme for Emacs > 24
(use-package grandshell-theme
  :quelpa (grandshell-theme :url "https://framagit.org/steckerhalter/grandshell-theme.git" :fetcher git)
  :config (load-theme 'grandshell t))

;;;; helm
;; fancy candidate selection framework
(use-package helm
  :unless (setq async-bytecomp-allowed-packages nil) ;disable async bytecomp
  :quelpa (helm :repo "emacs-helm/helm" :fetcher github :files ("*.el" "emacs-helm.sh"))
  :diminish
  :commands helm-mini

  :init
  (setq helm-idle-delay 0.1)
  (setq helm-input-idle-delay 0.1)
  (setq helm-buffer-max-length 50)
  (setq helm-M-x-always-save-history t)
  (setq helm-buffer-details-flag nil)
  (setq helm-mode-handle-completion-in-region nil) ;don't use helm for `completion-at-point'
  (setq helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")

  :bind ("M-x" . helm-M-x)

  :config
  (require 'helm-config)
  (helm-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(dired-create-directory))
  (add-to-list 'helm-boring-buffer-regexp-list ":.*")

  ;; Yet Another `describe-bindings' with `helm'.
  (use-package helm-descbinds
    :quelpa (helm-descbinds :repo "emacs-helm/helm-descbinds" :fetcher github)
    :config (helm-descbinds-mode))

  ;; GNU GLOBAL helm interface
  (use-package helm-gtags
    :quelpa (helm-gtags :repo "syohex/emacs-helm-gtags" :fetcher github :files ("helm-gtags.el"))
    :diminish
    :config (helm-gtags-mode 1))

  ;; Helm integration for Projectile
  (use-package helm-projectile
    :quelpa (helm-projectile :repo "bbatsov/helm-projectile" :fetcher github))

  ;; Helm UI wrapper for system package managers.
  (use-package helm-system-packages
    :quelpa (helm-system-packages :repo "emacs-helm/helm-system-packages" :fetcher github))

  (use-package helm-rg
    :quelpa (helm-rg :fetcher github :repo "cosmicexplorer/helm-rg"))

  ;; Efficiently hopping squeezed lines powered by helm interface
  (use-package helm-swoop
    :quelpa
    :bind (:map
           isearch-mode-map ("M-i" . helm-swoop-from-isearch)
           :map
           helm-swoop-map ("M-i" . helm-multi-swoop-all-from-helm-swoop))
    :init (setq helm-swoop-speed-or-color t)))

;;;; highlight-parentheses
;; highlight surrounding parentheses
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
  :diminish
  :hook (prog-mode . highlight-symbol-mode)
  :init
  (setq highlight-symbol-on-navigation-p t))

;;;; ibuffer
(use-package ibuffer
  :init (setq ibuffer-default-display-maybe-show-predicates t)
  :bind  ("C-x b" . ibuffer))

;;;; iedit
;; change multiple occurences of word-at-point (compress display to show all of them)
(use-package iedit
  :quelpa (iedit :repo "victorhge/iedit" :fetcher github)
  :init
  (setq iedit-unmatched-lines-invisible t)
  (setq iedit-toggle-key-default nil))

;;;; ielm
;; interactively evaluate Emacs Lisp expressions
(use-package ielm
  :config
  (add-hook 'inferior-emacs-lisp-mode-hook
            (lambda ()
              (turn-on-eldoc-mode))))

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
  :quelpa (ipretty :fetcher git :url "https://framagit.org/steckerhalter/ipretty.git")
  :config (ipretty-mode t))

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

;;;; logview
;; Major mode for viewing log files
(use-package logview
  :quelpa (logview :repo "doublep/logview" :fetcher github)
  :mode ("\\.log" . logview-mode)
  :config (setq logview-auto-revert-mode t))

;;;; magit
;; Emacs interface to git
(use-package magit
  :quelpa
  :demand
  :diminish magit-wip-after-apply-mode
  :init
  (setq magit-push-always-verify nil)
  (setq git-commit-finish-query-functions nil)
  (setq magit-save-some-buffers nil) ;don't ask to save buffers
  (setq magit-set-upstream-on-push t) ;ask to set upstream
  (setq magit-diff-refine-hunk 'all) ;show word-based diff for all hunks
  (setq magit-default-tracking-name-function
        'magit-default-tracking-name-branch-only) ;don't track with origin-*

  :config
  (setq magit-wip-after-save-mode 1)
  (setq magit-wip-after-apply-mode 1)
  ;; Emacs Minor mode to automatically commit and push
  (use-package git-auto-commit-mode
    :quelpa (git-auto-commit-mode :fetcher github
                                  :repo "ryuslash/git-auto-commit-mode")
    :commands (gac-commit gac)
    :config
    (defun gac ()
      (interactive)
      (gac-commit))))

;;;; man
;; browse UNIX manual pages
(use-package man
  :init (setq Man-notify-method 'aggressive))

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

;;;; monky
(use-package monky
  :quelpa (monky :fetcher github :repo ananthakumaran/monky :files ("*.el" "*.info" "style"))
  :init (setq monky-process-type 'cmdserver))

;;;; multiple-cursors
;; allow editing with multiple cursors
(use-package multiple-cursors
  :quelpa (multiple-cursors :fetcher github :repo "magnars/multiple-cursors.el")
  :init
  (setq mc/always-repeat-command t)
  (setq mc/always-run-for-all t)
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-?" . mc/mark-all-like-this)))

;;;; open-junk-file
;; Open a junk (memo) file to try-and-error
(use-package open-junk-file
  :quelpa (open-junk-file :repo "rubikitch/open-junk-file" :fetcher github)
  :init (setq open-junk-file-format "~/junk/%Y/%m/%d-%H%M%S."))

;;;; org
;;  "Outline-based notes management and organizer"
(use-package org
  :hook (org-mode . (lambda () (setq-local company-idle-delay 0.3)))
  :custom
  (org-capture-templates
   '(("t" "Task" entry (file "") "* TODO %?\n %a\n" :prepend t)
     ("s" "Simple Task" entry (file "") "* TODO %?\n" :prepend t)
     ("l" "Link" entry (file "") "* TODO %a %T\n" :prepend t)))
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-startup-truncated t)
  (org-startup-folded nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)
  (org-use-speed-commands t)
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-html-postamble nil)
  (org-enforce-todo-checkbox-dependencies t)
  (org-speed-commands-user '(("S" . org-schedule)))
  (org-directory "~/Sync/notes")
  (org-default-notes-file my-todo)
  (org-image-actual-width nil)
  (org-log-repeat nil)
  (org-blank-before-new-entry
   '((heading . nil) (plain-list-item . nil)))

  :config
  (add-to-list 'org-file-apps '("\\(?:ogg\\|mp3\\|m4a\\)" . "mpv --player-operation-mode=pseudo-gui -- %s"))
  (add-to-list 'org-structure-template-alist
               '("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist
               '("S" "#+BEGIN_SRC shell-script\n?\n#+END_SRC\n"))

;;;;; org-protocol
  (use-package org-protocol
    :demand
    :config
    (add-to-list 'org-capture-templates
                 '("p" "Protocol" entry (file "")
                   "* TODO %?[[%:link][%:description]] %U\n%i\n" :prepend t :immediate-finish t))
    (add-to-list 'org-capture-templates
                 '("L" "Protocol Link" entry (file "")
                   "* TODO %?[[%:link][%:description]] %U\n" :prepend t :immediate-finish t)))

;;;;; org-protocol-capture-html
  (use-package org-protocol-capture-html
    :quelpa (org-protocol-capture-html :fetcher github :repo "alphapapa/org-protocol-capture-html")
    :config (add-to-list 'org-capture-templates
                         '("w" "Web site" entry (file "")
                           "* %a :website:\n\n%U %?\n\n%:initial" :immediate-finish t)))

;;;;; org-agenda
  (use-package org-agenda
    :init
    (defun my-org-agenda () (interactive) (org-agenda nil "n"))
    (setq org-agenda-start-with-log-mode t)
    (setq org-agenda-todo-ignore-scheduled 'future) ;don't show future scheduled
    (setq org-agenda-todo-ignore-deadlines 'far)    ;show only near deadlines
    (setq org-agenda-dim-blocked-tasks t)
    (setq org-agenda-todo-ignore-scheduled 'all) ;hide scheduled TODOs
    (setq org-agenda-dim-blocked-tasks t)
    (setq org-agenda-show-all-dates nil)
    (setq org-agenda-prefix-format "%?-12t% s")

    :config
    ;; add state to the sorting strategy of todo
    (setcdr (assq 'todo org-agenda-sorting-strategy) '(todo-state-up priority-down category-keep))

    ;; create the file for the agendas if it doesn't exist
    (let ((agendas "~/.agenda_files"))
      (unless (file-readable-p agendas)
        (with-temp-file agendas nil))
      (setq org-agenda-files agendas))

    ;; display the agenda first
    (setq org-agenda-custom-commands
          '(("n" "Agenda and all TODO's"
             ((tags-todo "+PRIORITY=\"A\"-DEADLINE>\"<now>\"|DEADLINE<=\"<now>\"" ((org-agenda-overriding-header "today")))
              (tags-todo "+PRIORITY=\"B\"-DEADLINE={.}" ((org-agenda-overriding-header "inbox")))
              (tags-todo "+PRIORITY=\"C\"-DEADLINE={.}" ((org-agenda-overriding-header "backlog")))
              (tags "+reminder-TODO"  ((org-agenda-overriding-header "reminders")))
              (agenda "agenda")))))

    ;; add new appointments when saving the org buffer, use 'refresh argument to do it properly
    (defun my-org-agenda-to-appt-refresh () (org-agenda-to-appt 'refresh))
    (defun my-org-mode-hook ()
      (add-hook 'after-save-hook 'my-org-agenda-to-appt-refresh nil 'make-it-local))
    (add-hook 'org-mode-hook 'my-org-mode-hook))

  (use-package notifications
    :config
    (defun my-appt-disp-window-function (min-to-app new-time msg)
      (notifications-notify :title (format "Appointment in %s min" min-to-app) :body msg))
    (setq appt-disp-window-function 'my-appt-disp-window-function)
    (setq appt-delete-window-function (lambda (&rest args))))

;;;;; org-bullets
  ;; Show bullets in org-mode as UTF-8 characters
  (use-package org-bullets
    :quelpa (org-bullets :fetcher github :repo "emacsorphanage/org-bullets")
    :config (add-hook 'org-mode-hook 'org-bullets-mode))

;;;;; org-emms
  (use-package org-emms
    :after org
    :quelpa (org-emms :fetcher github :repo "jagrg/org-emms")))

;;;; outshine
;; outline with outshine outshines outline
(use-package outshine
  :quelpa (outshine :fetcher github :repo "alphapapa/outshine")
  :diminish outline-minor-mode
  :commands outshine-hook-function
  :hook ((outline-minor-mode . outshine-mode)
         (emacs-lisp-mode . outline-minor-mode))
  :init
  (setq outshine-imenu-show-headlines-p nil))

;;;; page-break-lines
;; Display ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :quelpa (page-break-lines :fetcher github :repo "purcell/page-break-lines")
  :diminish
  :config (global-page-break-lines-mode))

;;;; pdf-tools
(use-package pdf-tools
  :quelpa (pdf-tools
           :fetcher github
           :repo "politza/pdf-tools"
           :files ("lisp/*.el"
                   "README"
                   ("build" "Makefile")
                   ("build" "server")
                   (:exclude "lisp/tablist.el" "lisp/tablist-filter.el")))
  :hook (doc-view-mode . (pdf-tools-install pdf-tools-enable-minor-modes))
  :magic ("%PDF" . pdf-view-mode))

;;;; php
;; Major mode for editing PHP code
(use-package php-mode
  :quelpa (php-mode
           :repo "ejmr/php-mode"
           :fetcher github
           :files ("php-mode.el" "skeleton/*.el"))
  :mode "\\.module\\'"
  :init
  (setq php-mode-coding-style 'symfony2)
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

  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line '(:eval (format " <%s>" (projectile-project-name))))

  :config
  (projectile-global-mode 1))

;;;; rainbow-mode
;; Colorize color names in buffers
(use-package rainbow-mode
  :quelpa (rainbow-mode :fetcher github :repo "emacsmirror/rainbow-mode")
  :diminish
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

;;;; shell-switcher
(use-package shell-switcher
  :quelpa (shell-switcher :fetcher github
                          :repo "DamienCassou/shell-switcher"
                          :files ("rswitcher.el" "shell-switcher.el"))
  :demand
  :config (shell-switcher-mode 1))

;;;; shr
(use-package shr
  :custom
  ;; increase contrast between similar colors
  (shr-color-visible-luminance-min 60))

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

;;;; stylus-mode
;; Major mode for editing .jade files
(use-package stylus-mode
  :quelpa (stylus-mode :fetcher github :repo "brianc/jade-mode" :files ("stylus-mode.el")))

;;;; systemd
;; Major mode for editing systemd units
(use-package systemd
  :quelpa (systemd :fetcher github :repo "holomorph/systemd-mode" :files (:defaults "*.txt")))

;;;; toml-mode
;; Major mode for editing toml files
(use-package toml-mode
  :quelpa (toml-mode :fetcher github :repo "dryman/toml-mode.el"))

;;;; tldr
(use-package tldr
  :quelpa (tldr :fetcher github :repo "kuanyui/tldr.el"))

;;;; yaml-mode
;; Major mode for editing YAML files
(use-package yaml-mode
  :quelpa (yaml-mode :repo "yoshiki/yaml-mode" :fetcher github))

;;;; visual-regexp
;; A regexp/replace command for Emacs with interactive visual feedback
(use-package visual-regexp
  :quelpa (visual-regexp :repo "benma/visual-regexp.el" :fetcher github))

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

;;;; which-key
(use-package which-key
  :quelpa (which-key :repo justbur/emacs-which-key :fetcher github)
  :diminish
  :custom
  (which-key-show-docstrings 'docstring-only)
  (which-key-max-description-length nil)
  (which-key-side-window-max-height 0.75)
  :config (which-key-mode))

;;;; zenity-color-picker
(use-package zenity-color-picker
  :quelpa (zenity-color-picker :fetcher git :url "https://bitbucket.org/Soft/zenity-color-picker.el.git"))

;; Zoom window like tmux
(use-package zoom-window
  :quelpa (zoom-window :fetcher github :repo "syohex/emacs-zoom-window"))

;;; steckemacs.el ends here
