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
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(package-install 'use-package)
(use-package use-package-ensure
  :config  (setq use-package-always-ensure t))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;;; diminish
;; Diminished modes are minor modes with no modeline display
(use-package diminish)

;;; settings
(use-package steckemacs-settings
  :ensure nil
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
   enable-recursive-minibuffers t         ;whatever...
   show-paren-delay 0                     ;show the paren immediately
   load-prefer-newer t                    ;prefer newer .el instead of the .elc
   split-height-threshold 140             ;more readily split horziontally
   split-width-threshold 140              ;split horizontally only if less than 160 columns
   gc-cons-percentage 0.3                 ;increase garbage collection limit
   safe-local-variable-values '((engine . django))
   switch-to-buffer-preserve-window-point t ;this allows operating on the same buffer in diff. positions
   custom-file (expand-file-name "custom-file.el" user-emacs-directory) ;don't pollute the init file and don't `load' the customs but keep them for reference...
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

  (defun my-json-format ()
    (interactive)
    (switch-to-buffer (get-buffer-create "output.json"))
    (yank)
    (json-reformat-region (point-min) (point-max))
    (json-mode))

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

  (defun my-capture ()
    (interactive)
    (if current-prefix-arg
        (org-capture nil (cadr org-capture-default))
      (org-capture nil (car org-capture-default))))

  (defun my-songs ()
    (interactive)
    (let ((songs (org-map-entries
                  (lambda () (substring
                         (org-element-property
                          :title (org-element-at-point)) 0 -13))
                  nil
                  'region-start-level)))
      (switch-to-buffer (get-buffer-create "Reto's Songs"))
      (erase-buffer)
      (insert "# Reto's Songs\n\n")
      (dolist (line songs)
        (insert line)
        (insert "  ")
        (newline))
      (markdown-mode)
      (markdown-preview)))

  (defun retonom ()
    (interactive)
    (org-publish "web")
    (when current-prefix-arg
      (sync-retonom)))

  (defun sync-retonom (&optional dir)
    (interactive)
    (let ((auth (let ((netrc-file "~/.netrc"))
                  (netrc-credentials "ftp.legtux.org"))))
      (shell-command
       (concat "lftp -e \"open ftp.legtux.org; user " (car auth) " '" (cadr auth) "';mirror --no-symlinks --reverse --continue --delete --verbose ~/retonom/" dir " /retonom/" dir "; bye\""))))

  (defun my-after-save-hook ()
    (let ((matches '("songs.org"
                     "demos.org"
                     "raw.org"
                     "setup.org"
                     "index.org")))
      (when (and buffer-file-name
                 current-prefix-arg
                 (member (file-name-nondirectory buffer-file-name) matches))
        (publish-music))))
  (add-hook 'after-save-hook 'my-after-save-hook)

  (defun my-org-insert-time-stamp (&optional heading)
    (interactive)
    (when heading
      (org-insert-heading))
    (let ((time (current-time))
          (with-hm (not current-prefix-arg)))
      (org-insert-time-stamp time with-hm t)
      (insert " ")))


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
  ("C-c c" . my-capture)
  ("C-c m" . menu-bar-mode)
  ("C-c n" . my-org-agenda)
  ("C-c a" . org-agenda)
  ("C-c g" . magit-status)
  ("C-c d" . ispell-change-dictionary)
  ("C-z" . undo)
  ("C-c s" . web-search)
  ("C-c w" . wdired-mode))

;; Make bindings that stick around.
(use-package hydra
  :bind
  ("S-SPC" . !/body)
  ("<menu>" . !/body)
  ("<apps>" . !/body)
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
       (call-interactively (or fnp ',fn))))

  (defun !/state (&optional exit)
    ;; TODO: add state to mode-line
    (setq hydra-is-helpful exit)
    (if exit
        (progn
          (setq which-key-show-docstrings 'docstring-only)
          (custom-theme-recalc-face 'cursor))
      (set-face-background 'cursor "#ff5f87")
      (setq which-key-show-docstrings nil)))

  (defhydra ! (:color pink :pre (!/state) :post (!/state t) :hint nil)
    "
  _a_  BOL           _h_  delete         _l_  recenter        _r_  kill ring        _x_  M-x             _C-f_  flyspell
  _f_  EOL           _H_  kill word      _=_  scale up        _R_  mark rings       _b_  helm-mini   C-u _C-f_  flyspell buf
  _A_  to indent     _S_  beg of buffer  _+_  scale down      _t_  timestamp
  _o_  up            _D_  end of buffer  _v_  visual line     _T_  journal entry    _._  find thing      _C C_  customize group
  _i_  down          _w_  prev window    ^^                   _c_  capture          _,_  pop mark        _C v_  customize var
  _j_  back          _e_  next window    _>_  mc next
  _;_  forward       _k_  kill line      _<_  mc prev         _B_  switch to buf  _q q_  rectangle mode  _p p_  projectile switch
  _ö_  forward       _n_  kill region    ^^^^                                     _q k_  kill rectangle  _p f_  projectile ff
  _d_  page down     _N_  cut subtree    _'_  shell switch    _F_  find files     _q y_  yank rectangle  _p u_  projectile switch to buffer
  _s_  page up       _y_  yank           _\"_  new shell     _M-b_  scratch        _q c_  copy rect       _p a_  projectile ag
  ^^                 _Y_  yank-pop       ^^                 _C-b_  revert buffer
  _O_  last pos      _/_  undo           _6_  edebug defun  ^^                    _u u_  agenda          _u s_  songs.org
  _I_  next pos      ^^                  _7_  toggle edebug _g p_  prev hunk      _u m_  demos.org       _u W_  work.org
_M-o_  prev symbol _M-m_  mark-sexp      _9_  eval list     _g n_  next hunk      _u j_  journal.org     _u w_  work agenda
_M-i_  next symbol _M-M_  mark buf   C-u _9_  eval sexp     _g g_  magit          _u o_  todo.org
  _[_  swoop         _M_  mark line      _0_  eval l. sexp  _g l_  magit log      _u d_  deft
  _]_  isearch       _m_  mark           _8_  eval buffer   _g r_  revert hunk    _u a_  org archive done
  "
    ("M-SPC" (setq hydra-is-helpful t))
    ("a" (kbds "C-a"))
    ("A" (kbds "M-m"))
    ("b" helm-mini)
    ("B" (switch-to-buffer nil))
    ("M-b" my-switch-to-scratch)
    ("C-b" revert-buffer)
    ("c" my-capture :exit t)
    ("C C" customize-group)
    ("C v" customize-variable)
    ("d" (kbds "C-v"))
    ("e" my-select-next-window)
    ("D" (kbds "M->"))
    ("f" (kbds "C-e"))
    ("F" find-file)
    ("C-f" (hydra-arg flyspell-mode
                      4 flyspell-buffer))
    ("g g" magit-status :exit t)
    ("g l" magit-log-all)
    ("g b" magit-blame)
    ("g r" diff-hl-revert-hunk)
    ("g p" diff-hl-previous-hunk)
    ("g n" diff-hl-next-hunk)
    ("g c" (customize-group 'magit t))
    ("h" (kbds "C-d"))
    ("H" (kbds "M-d"))
    ("i" (kbds "C-n"))
    ("M-i" highlight-symbol-next)
    ("I" back-button-local-forward)
    ("j" (kbds "C-b"))
    ("C-j" my-json-format)
    ("k" (kbds "C-k"))
    ("l" recenter-top-bottom)
    ("C-l" (rename-file (org-latex-export-to-pdf nil t nil nil '(:with-toc nil))
                        (concat "~/ownCloud/chords/"
                                (car (split-string (org-entry-get nil "ITEM") "-" t split-string-default-separators))
                                ".pdf")
                        t))
    ("m" (kbds "C-SPC"))
    ("M-m" easy-mark-sexp)
    ("M-M" mark-whole-buffer)
    ("M" (kbds "M-w"))
    ("n" (kbds "C-w"))
    ("N" org-cut-special)
    ("o" (kbds "C-p"))
    ("O" back-button-local-backward)
    ("M-o" highlight-symbol-prev)
    ("ö" (kbds "C-f"))
    ("p p" projectile-switch-project)
    ("p f" (hydra-resume project-find-file) :exit t)
    ("p u" projectile-switch-to-buffer)
    ("p a" (hydra-resume projectile-ag) :exit t)
    ("q q" rectangle-mark-mode)
    ("q k" kill-rectangle)
    ("q y" yank-rectangle)
    ("q c" copy-rectangle-as-kill)
    ("r" helm-show-kill-ring)
    ("R" helm-all-mark-rings)
    ("s" (kbds "M-v"))
    ("S" (kbds "M-<"))
    ("t" my-org-insert-time-stamp)
    ("T" (my-org-insert-time-stamp t) :exit t)
    ("u u" (my-org-agenda "n"))
    ("u w" (my-org-agenda "w"))
    ("u W" (find-file (expand-file-name "./notes/work.org" my-work-folder)))
    ("u o" (find-file my-todo))
    ("u m" (find-file "~/Sync/music/demos.org"))
    ("u s" (find-file "~/Sync/music/songs.org"))
    ("u j" (find-file (expand-file-name "journal.org" deft-directory)))
    ("u d" deft)
    ("u a" org-archive-done-tasks)
    ("ü" helm-swoop)
    ("C-ü" web-search)
    ("v" visual-line-mode)
    ("w" my-select-prev-window)
    ("x" execute-extended-command)
    ("y" yank)
    ("Y" yank-pop)
    (";" (kbds "C-f"))
    ("/" undo)
    ("-" undo)
    ("<backtab>" outshine-cycle-buffer)
    ("=" default-text-scale-increase)
    ("+" default-text-scale-decrease)
    (">" (kbds "C->"))
    ("<" (kbds "C-<"))
    ("[" helm-swoop)
    ("]" isearch-forward :exit t)
    ("'" shell-switcher-switch-buffer :exit t)
    ("\"" shell-switcher-new-shell :exit t)
    ("9" (hydra-arg eval-sexp-fu-eval-sexp-inner-list 4 eval-sexp-fu-eval-sexp-inner-sexp))
    ("0" eval-last-sexp)
    ("SPC" save-buffer)
    ("." elisp-slime-nav-find-elisp-thing-at-point)
    ("," pop-tag-mark)
    ("6" edebug-defun)
    ("8" eval-buffer)
    ("7" toggle-debug-on-error)
    ("<f12> %" (insert "¯\\_(ツ)_/¯"))
    ("<f12> /" helm-rg)
    ("<f12> 4" mu4e :exit t)
    ("<f12> $ l" org-mu4e-store-and-capture)
    ("<f12> $ c" mu4e-compose-new)
    ("<f12> a e" my-erc-connect)
    ("<f12> a f" elfeed)
    ("<f12> a M" mastodon)
    ("<f12> a m" mastodon-toot)
    ("<f12> a h" hackernews)
    ("<f12> a d" daemons)
    ("<f12> b s" my-sudo-edit)
    ("<f12> b i" (hydra-resume iedit-mode) :exit t)
    ("<f12> c" customize-group)
    ("<f12> C" zenity-cp-color-at-point-dwim)
    ("<f12> d" (hydra-resume deft) :exit t)
    ("<f12> h a" helm-apropos)
    ("<f12> h d" elisp-slime-nav-describe-elisp-thing-at-point)
    ("<f12> h s" info-lookup-symbol)
    ("<f12> h i" info)
    ("<f12> h I" helm-info)
    ("<f12> h r" info-emacs-manual)
    ("<f12> h R" helm-info-emacs)
    ("<f12> h p" helm-info-at-point)
    ("<f12> i" ipretty-last-sexp)
    ("<f12> I" ipretty-last-sexp-other-buffer)
    ("<f12> j" dired-jump)
    ("<f12> k" kill-emacs)
    ("<f12> m" man)
    ("<f12> o o" org-open-at-point)
    ("<f12> o T" my-org-insert-timestamp)
    ("<f12> o t" (org-set-tags-command))
    ("<f12> o i" org-toggle-inline-images)
    ("<f12> p l" list-packages)
    ("<f12> p s" helm-system-packages)
    ("<f12> p d" my-insert-package-desc-summary)
    ("<f12> p p" my-package-def)
    ("<f12> p p" helm-locate-library)
    ("<f12> q" quelpa)
    ("<f12> Q" quelpa-expand-recipe)
    ("<f12> u" my-browse-url-dwim)
    ("<f12> w" (lambda ()
                 (interactive)
                 (let ((which-key-show-docstrings 'docstring-only))
                   (which-key-show-major-mode))))
    ("<f12> z" zoom-window-zoom)
    ("S-SPC" nil :color blue)
    ("<apps>" nil :color blue)
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
  :ensure nil
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
  (define-skeleton audio
    "Insert html audio"
    "file: "
    "#+BEGIN_EXPORT html" \n
    "<audio controls=\"controls\" preload=\"none\">" \n
    "<source src=\"" str "\" type=\"audio/ogg\">" \n
    "<a href=\"" str "\">" str "</a>" \n
    "</audio>" \n
    "#+END_EXPORT" \n))

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
  :ensure nil
  :bind (:map xwidget-webkit-mode-map
              ("<mouse-4>" . xwidget-webkit-scroll-down)
              ("<mouse-5>" . xwidget-webkit-scroll-up)
              ("<up>" . xwidget-webkit-scroll-down)
              ("<down>" . xwidget-webkit-scroll-up)
              ("M-w" . xwidget-webkit-copy-selection-as-kill))
  :hook (window-configuration-change . (lambda ()
                                         (when (equal major-mode 'xwidget-webkit-mode)
                                           (xwidget-webkit-adjust-size-dispatch)))))

;;;; advice
;; An overloading mechanism for Emacs Lisp functions
(use-package advice
  :ensure nil
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
(use-package ag)

;;;; aggressive-indent
(use-package aggressive-indent
  :config (global-aggressive-indent-mode 1))

;;;; anaconda-mode
;; Code navigation, documentation lookup and completion for Python
(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode)

  ;; A major mode for editing pip requirements files
  (use-package pip-requirements)

  ;; Integrate pyenv with python-mode
  (use-package pyenv-mode
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
  :config (add-hook 'yaml-mode-hook #'ansible-doc-mode))

;;;; apache-mode
;; major mode for editing Apache configuration files
(use-package apache-mode)

;;;; appt
;; appointment notification functions
(use-package appt
  :ensure nil
  :init (setq
         appt-message-warning-time 30
         appt-display-interval 15
         appt-display-mode-line t             ;show in the modeline
         appt-display-format 'window)
  :config (appt-activate 1))

;;;; auctex
;; enhanced LaTeX mode
(use-package tex
  :ensure auctex
  :config
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
                   (TeX-command-menu "LaTeX"))))))

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
  :diminish
  :demand
  :config
  (setq back-button-local-keystrokes nil) ;don't overwrite C-x SPC binding
  (back-button-mode 1))

;;;; balanced-windows
(use-package balanced-windows
  :config
  (balanced-windows-mode))

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
  :init
  (setq cider-popup-stacktraces nil)
  (setq cider-repl-popup-stacktraces nil)
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-repl-use-clojure-font-lock t))

;;;; company
;; Modular text completion framework
(use-package company
  :diminish

  :init
  (setq company-idle-delay 0.3)
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 2)

  :config
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
    :hook ((emacs-lisp-mode lisp-interaction-mode) . my-company-elisp-setup)
    :config (company-statistics-mode)))

;;;; company-anaconda
;; Anaconda backend for company-mode
(use-package company-anaconda
  :config (add-to-list 'company-backends 'company-anaconda))

;;;; company-dict
;; A backend that emulates ac-source-dictionary
(use-package company-dict
  :config (add-to-list 'company-backends 'company-dict))

;;;; company-quickhelp
;; Popup documentation for completion candidates
(use-package company-quickhelp
  :init
  (setq company-quickhelp-use-propertized-text t)
  (setq company-quickhelp-delay 1)
  :config (company-quickhelp-mode 1))

;;;; company-web
;; Company version of ac-html, complete for web,html,emmet,jade,slim modes
(use-package company-web
  :config
  (defun my-company-web ()
    (set (make-local-variable 'company-backends) '(company-web-html))
    (company-mode t))
  :hook (web-mode . my-company-web))

;;;; custom
;; tools for declaring and initializing options
(use-package custom
  :ensure nil
  :init
  (setq
   ;; M-x customize should not cripple menu entries
   custom-unlispify-menu-entries nil
   ;;M-x customize should not cripple tags
   custom-unlispify-tag-names nil))

;;;; darkroom
(use-package darkroom
  :bind ("S-<f11>" . darkroom-tentative-mode)
  :custom
  (darkroom-text-scale-increase 3)
  (darkroom-margins-if-failed-guess 0.1))

;;;; default-text-scale
(use-package default-text-scale)

;;;; deft
;; quickly browse, filter, and edit plain text notes
(use-package deft
  :demand
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
  :config
  (global-diff-hl-mode 1)
  (eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;;;; dired
;; directory-browsing commands
(use-package dired
  :ensure nil
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

  ;; Rename files editing their names in dired buffers
  (use-package wdired
    :init
    ;; allow changing of file permissions
    (setq wdired-allow-to-change-permissions t))

  ;; dired+ adds some features to standard dired (like reusing buffers)
  (use-package dired+
    :ensure nil
    :quelpa (dired+ :fetcher url :url "https://www.emacswiki.org/emacs/download/dired+.el")
    :defer 1
    :init
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-hide-details-propagate-flag nil)

    :config
    (diredp-toggle-find-file-reuse-dir 1)))

;;;; discover-my-major
;; discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major)

;;;; easy-kill
;; make marking and killing easier
(use-package easy-kill
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
  :bind
  ("<f1> <f1>" . elisp-slime-nav-describe-elisp-thing-at-point)
  :diminish
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . elisp-slime-nav-mode))

;;;; emms
;; Playlist mode for Emms
(use-package emms
  :bind
  ("<XF86AudioPlay>" . emms-pause)
  ("<XF86AudioPrev>" . emms-previous)
  ("<XF86AudioNext>" . emms-next)
  ("S-<XF86AudioPause>" . emms-toggle-repeat-track)
  ("S-<XF86AudioPrev>" . emms-seek-backward)
  ("S-<XF86AudioNext>" . emms-seek-forward)
  :config
  (use-package emms-setup
    :ensure nil
    :custom (emms-player-list '(emms-player-mpv))
    :config
    (emms-standard)))

;;;; erc
;; Emacs ERC client settings
(use-package erc
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
    :ensure nil
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
    :config (setup-esh-help-eldoc))

  ;; BASH completion for the shell buffer
  (use-package bash-completion
    :config
    (defun eshell-bash-completion ()
      (setq-local bash-completion-nospace t)
      (while (pcomplete-here
              (nth 2 (bash-completion-dynamic-complete-nocomint
                      (save-excursion (eshell-bol) (point)) (point))))))
    (setq eshell-default-completion-function 'eshell-bash-completion))
  (use-package em-smart
    :ensure nil
    :hook (eshell-mode . eshell-smart-initialize)))

;;;; eval-sexp-fu
;; flash the region that is evaluated (visual feedback) in elisp
(use-package eval-sexp-fu
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
  :ensure nil
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

;;;; git-modes
;; Emacs major modes for various Git configuration files
(use-package git-modes
  :after magit
  :ensure nil
  :defer t)

;;;; google-translate
;; Emacs interface to Google's translation service
(use-package google-translate
  :init
  (setq google-translate-default-source-language "de")
  (setq google-translate-default-target-language "en"))

;;;; grandshell-theme
;; Grand Shell color theme for Emacs > 24
(use-package grandshell-theme
  :config (load-theme 'grandshell t))

;;;; helm
;; fancy candidate selection framework
(use-package helm
  :unless (setq async-bytecomp-allowed-packages nil) ;disable async bytecomp
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

  :config
  (require 'helm-config)

  ;; Yet Another `describe-bindings' with `helm'.
  (use-package helm-descbinds
    :config (helm-descbinds-mode))

  ;; GNU GLOBAL helm interface
  (use-package helm-gtags
    :diminish
    :config (helm-gtags-mode 1))

  ;; Helm integration for Projectile
  (use-package helm-projectile)

  ;; Helm UI wrapper for system package managers.
  (use-package helm-system-packages)

  (use-package helm-rg)

  ;; Efficiently hopping squeezed lines powered by helm interface
  (use-package helm-swoop
    :bind (:map
           isearch-mode-map ("M-i" . helm-swoop-from-isearch)
           :map
           helm-swoop-map ("M-i" . helm-multi-swoop-all-from-helm-swoop))
    :init
    (setq helm-swoop-speed-or-color t)
    (setq helm-swoop-pre-input-function (lambda ()))))

;;;; highlight-parentheses
;; highlight surrounding parentheses
(use-package highlight-parentheses
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
  :diminish
  :hook (prog-mode . highlight-symbol-mode)
  :init
  (setq highlight-symbol-on-navigation-p t))

;;;; ibuffer
(use-package ibuffer
  :init (setq ibuffer-default-display-maybe-show-predicates t)
  :bind  ("C-x b" . ibuffer))

;;;; ido
(use-package ido
  :init
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-default-buffer-method 'selected-window)
  :config
  (ido-mode 1)
  (put 'ido-exit-minibuffer 'disabled nil))

;;;; ido-completing-read+
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1)
  (use-package amx
    :config (amx-mode 1)))

;;;; iedit
;; change multiple occurences of word-at-point (compress display to show all of them)
(use-package iedit
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
  :config (ipretty-mode t))

;;;; json-mode
;; syntax highlighting for `json'
(use-package json-mode
  :mode "\\.json\\'")

;;;; logview
;; Major mode for viewing log files
(use-package logview
  :mode ("\\.log" . logview-mode)
  :config (setq logview-auto-revert-mode t))

;;;; magit
;; Emacs interface to git
(use-package magit
  :demand
  :diminish magit-wip-after-apply-mode
  :init
  (setq magit-no-confirm '(stage-all-changes))
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
  (setq markdown-command "pandoc -c http://benjam.info/panam/styling.css --from gfm -t html5 --mathjax --highlight-style pygments --metadata title=' ' --standalone")

  :hook (markdown-mode . visual-line-mode)
  :config
  (advice-add 'markdown-convert-wiki-link-to-filename
              :around 'my-markdown-convert-wiki-link))

;;;; monky
(use-package monky
  :init (setq monky-process-type 'cmdserver))

;;;; multiple-cursors
;; allow editing with multiple cursors
(use-package multiple-cursors
  :init
  (setq mc/always-repeat-command t)
  (setq mc/always-run-for-all t)
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-?" . mc/mark-all-like-this)))

;;;; open-junk-file
;; Open a junk (memo) file to try-and-error
(use-package open-junk-file
  :init (setq open-junk-file-format "~/junk/%Y/%m/%d-%H%M%S."))

;;;; org
;;  "Outline-based notes management and organizer"
(use-package org
  :hook (org-mode . (lambda () (setq-local company-idle-delay 0.3)))
  :bind (:map org-mode-map
              ("C-c M-RET" . org-insert-heading-after-current)
              ("C-c t" . (lambda () (interactive) (org-todo 'done))))
  :init
  (defvar org-capture-default '("s" "w") "default capture template to be used.
       Override it in `.user.el': (setq org-capture-default '(\"w\" \"s\"))")
  (defvar my-work-folder "~/work")

  (setq org-publish-project-alist
        '(("web" :components ("web-org" "web-files"))
          ("web-org"
           :publishing-function org-html-publish-to-html
           :base-directory "~/Sync/web"
           :publishing-directory "~/retonom"
           :recursive t
           :exclude "setup.org"
           :with-title nil
           :with-toc t
           :section-numbers nil)
          ("web-files"
           :publishing-function org-publish-attachment
           :base-extension "ogg\\|mp3\\|m4a\\|mp4\\|aac\\|png\\|jpg\\|jpeg\\|css\\|html\\|js"
           :include (".htaccess")
           :base-directory "~/Sync/web"
           :publishing-directory "~/retonom"
           :recursive t)))

  (setq org-capture-templates
        `(("t" "Task" entry (file "") "* TODO %?\n %a\n" :prepend t)
          ("s" "home" entry (file "todo.org") "* TODO %?\n")
          ("w" "work" entry (file ,(expand-file-name "./notes/work.org" my-work-folder)) "* TODO %?\n")
          ("l" "Link" entry (file "") "* TODO %a %T\n" :prepend t)))
  (setq org-todo-keywords '((sequence "TODO(t)" "PICK(p)" "WAIT(w!)" "DONE(d)")))
  (setq org-todo-keyword-faces '(("WAIT" . org-footnote)
                                 ("PICK" . org-warning)))
  (setq org-startup-indented t)
  (setq org-archive-mark-done t)
  (setq org-startup-with-inline-images t)
  (setq org-startup-truncated t)
  (setq org-startup-folded nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  (setq org-confirm-babel-evaluate nil)
  (setq org-use-speed-commands t)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-html-postamble nil)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-speed-commands-user '(("S" . org-schedule)))
  (setq org-directory "~/Sync/notes")
  (setq org-default-notes-file my-todo)
  (setq org-image-actual-width nil)
  (setq org-log-repeat nil)
  (setq org-tags-exclude-from-inheritance '("song"))
  (setq org-blank-before-new-entry
        '((heading . nil) (plain-list-item . nil)))

  :config
  (add-to-list 'org-file-apps '("\\(?:ogg\\|mp3\\|m4a\\)" . "mpv --player-operation-mode=pseudo-gui -- %s"))
  (add-to-list 'org-structure-template-alist
               '("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist
               '("S" "#+BEGIN_SRC shell-script\n?\n#+END_SRC\n"))

;;;;; org-protocol
  (use-package org-protocol
    :ensure nil
    :demand
    :config
    (add-to-list 'org-capture-templates
                 '("p" "Protocol" entry (file "")
                   "* TODO %?[[%:link][%:description]]\n%i\n" :immediate-finish t))
    (add-to-list 'org-capture-templates
                 '("L" "Protocol Link" entry (file "")
                   "* TODO %?[[%:link][%:description]]\n" :immediate-finish t)))

;;;;; org-agenda
  (use-package org-agenda
    :ensure nil
    :bind (:map org-agenda-mode-map
                ("C-c t" . (lambda () (interactive)
                             (org-agenda-todo 'done)
                             (org-agenda-redo-all))))
    :init
    (defun my-org-agenda (key) (interactive) (org-agenda nil key))
    (setq org-agenda-start-with-log-mode t)
    (setq org-agenda-todo-ignore-scheduled 'future) ;don't show future scheduled
    (setq org-agenda-todo-ignore-deadlines 'far)    ;show only near deadlines
    (setq org-agenda-dim-blocked-tasks t)
    (setq org-agenda-todo-ignore-scheduled 'all) ;hide scheduled TODOs
    (setq org-agenda-dim-blocked-tasks t)
    (setq org-agenda-show-all-dates nil)
    (setq org-agenda-prefix-format "%?-12t% s")
    (setq org-agenda-confirm-kill nil)
    (setq org-tags-match-list-sublevels nil)

    (defun org-archive-done-tasks ()
      (interactive)
      (org-map-entries
       (lambda ()
         (org-archive-subtree)
         (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
       "/DONE" 'file))

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
          '(("n" "@home"
             ((tags-todo "-@work+DEADLINE<=\"<now>\"" ((org-agenda-overriding-header "now")))
              (tags-todo "-@work-DEADLINE={.}+TODO={TODO}" ((org-agenda-overriding-header "todo")))
              (tags "-@work+reminder+DEADLINE>\"<now>\|-@work+reminder-DEADLINE<=\"<now>\""  ((org-agenda-overriding-header "reminders")))
              (tags-todo "-@work+TODO={PICK}+DEADLINE>\"<now>\"|-@work+TODO={PICK}-DEADLINE={.}" ((org-agenda-overriding-header "pick")))
              (tags-todo "-@work+DEADLINE>=\"<now>\"" ((org-agenda-overriding-header "scheduled")))))
            ("w" "@work"
             ((tags-todo "@work+DEADLINE<=\"<now>\"" ((org-agenda-overriding-header "now")))
              (tags-todo "@work-DEADLINE={.}+TODO={TODO}" ((org-agenda-overriding-header "todo")))
              (tags "@work+reminder+DEADLINE>\"<now>\|@work+reminder-DEADLINE<=\"<now>\""  ((org-agenda-overriding-header "reminders")))
              (tags-todo "@work+TODO={PICK}+DEADLINE>\"<now>\"|@work+TODO={PICK}-DEADLINE={.}" ((org-agenda-overriding-header "pick")))
              (tags-todo "@work+DEADLINE>=\"<now>\"" ((org-agenda-overriding-header "scheduled")))))))

    ;; add new appointments when saving the org buffer, use 'refresh argument to do it properly
    (defun my-org-agenda-to-appt-refresh () (org-agenda-to-appt 'refresh))
    (defun my-org-mode-hook ()
      (add-hook 'after-save-hook 'my-org-agenda-to-appt-refresh nil 'make-it-local))
    (add-hook 'org-mode-hook 'my-org-mode-hook))

  (use-package notifications
    :config
    (defun my-appt-disp-window-function (min-to-app new-time msg)
      (if (string-equal system-type "windows-nt")
          (shell-command (format "msg /server:localhost rhu \"In %s min: %s\"" min-to-app msg))
        (notifications-notify :title (format "In %s min" min-to-app) :body msg)))
    (setq appt-disp-window-function 'my-appt-disp-window-function)
    (setq appt-delete-window-function (lambda (&rest args))))

;;;;; org-bullets
  ;; Show bullets in org-mode as UTF-8 characters
  (use-package org-bullets
    :config (add-hook 'org-mode-hook 'org-bullets-mode)))

;;;; outshine
;; outline with outshine outshines outline
(use-package outshine
  :diminish outline-minor-mode
  :commands outshine-hook-function
  :hook ((outline-minor-mode . outshine-mode)
         (emacs-lisp-mode . outline-minor-mode))
  :init
  (setq outshine-imenu-show-headlines-p nil))

;;;; page-break-lines
;; Display ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :config (global-page-break-lines-mode))

;;;; pdf-tools
(use-package pdf-tools
  :hook (doc-view-mode . (pdf-tools-install pdf-tools-enable-minor-modes))
  :magic ("%PDF" . pdf-view-mode))

;;;; php
;; Major mode for editing PHP code
(use-package php-mode
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
    :config (add-to-list 'company-backends 'company-ac-php-backend)
    :bind (:map php-mode-map
                ("M-." . ac-php-find-symbol-at-point)
                ("M-," . ac-php-location-stack-back)
                ("M-S-," . ac-php-location-stack-forward)
                ("H-i d" . my-var_dump-die)))

  (use-package php-eldoc)

  (defun setup-php-mode ()
    (php-eldoc-enable))

  :hook (php-mode . setup-php-mode))

;;;; pos-tip
;; Show tooltip at point
(use-package pos-tip
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

  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ido)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line '(:eval (format " <%s>" (projectile-project-name))))

  :config
  (projectile-global-mode 1))

;;;; rainbow-mode
;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish
  :hook (css-mode html-mode js-mode emacs-lisp-mode text-mode))

;;;; restclient
(use-package ob-restclient)
;;;; robe
;; Code navigation, documentation lookup and completion for Ruby
(use-package robe
  :config
  (push 'company-robe company-backends)
  :hook (ruby-mode . robe-mode))

;;;; shell-switcher
(use-package shell-switcher
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

  :init
  (setq sml/vc-mode-show-backend t)
  (setq sml/no-confirm-load-theme t)

  :config
  (sml/setup)
  (sml/apply-theme 'automatic))

;;;; stylus-mode
;; Major mode for editing .jade files
(use-package stylus-mode)

;;;; systemd
;; Major mode for editing systemd units
(use-package systemd)

;;;; toml-mode
;; Major mode for editing toml files
(use-package toml-mode)

;;;; tldr
(use-package tldr)

;;;; yaml-mode
;; Major mode for editing YAML files
(use-package yaml-mode)

;;;; visual-regexp
;; A regexp/replace command for Emacs with interactive visual feedback
(use-package visual-regexp)

;;;; vlf
;; View Large Files
(use-package vlf
  :init
  (setq vlf-application 'dont-ask)   ;just do it
  (setq vlf-batch-size 8192))        ;a bit more text per batch please

;;;; web-mode
;; major mode for editing web templates
(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.ejs?\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-engine-detection t)
  (defun setup-web-mode ()
    (set (make-local-variable 'electric-pair-mode) nil)) ;disable electric-pairing in web-mode
  :hook (web-mode . setup-web-mode))

;;;; web-search
(use-package web-search
  :config
  (push '("Qwant" "https://www.qwant.com/?q=%s" "Search") web-search-providers)
  (setq web-search-default-provider "Qwant"))

;;;; writeroom-mode
(use-package writeroom-mode)

;;;; which-key
(use-package which-key
  :diminish
  :custom
  (which-key-show-docstrings 'docstring-only)
  (which-key-max-description-length nil)
  (which-key-side-window-max-height 0.75)
  :config (which-key-mode))

;;;; zenity-color-picker
(use-package zenity-color-picker)

;; Zoom window like tmux
(use-package zoom-window)

;;; steckemacs.el ends here
