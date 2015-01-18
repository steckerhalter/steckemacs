;;; steckemacs.el --- steckemacs emacs configuration

;; Copyright 2015, Steckerhalter

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

;; Emacs configuration that tries to fetch everything necessary from
;; MELPA on startup. Instead of splitting everything up I try to keep
;; everything in one file. My theme called `grandshell` is loaded from
;; MELPA too.

;;; Requirements:

;; Emacs 24.4

;;; Code:

;;; general settings
;;;; maximize emacs
(modify-all-frames-parameters '((fullscreen . maximized)))

;;;; `elisp' load path
;; if `~/.emacs.d/elisp/' exists add it to the load path
(let ((default-directory "~/.emacs.d/elisp/"))
  (unless (file-exists-p default-directory)
    (make-directory default-directory))
  (add-to-list 'load-path default-directory))

;;;; quelpa
;; disable the GNU ELPA
(setq package-archives nil)
(if (require 'quelpa nil t)
    (quelpa '(quelpa :repo "quelpa/quelpa" :fetcher github) :upgrade t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;;;; defvars
;; define minor mode to override bindings
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
;; set outline prefix
(defvar outline-minor-mode-prefix "\M-#")

;;;; load custom user code
(when (file-readable-p "~/.user.el") (load "~/.user.el"))

;;;; encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;;;; global flags
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
 recentf-max-saved-items 5000           ;same up to 5000 recent files
 eval-expression-print-length nil       ;do not truncate printed expressions
 eval-expression-print-level nil        ;print nested expressions
 send-mail-function 'sendmail-send-it
 kill-ring-max 5000                     ;truncate kill ring after 5000 entries
 mark-ring-max 5000                     ;truncate mark ring after 5000 entries
 mouse-autoselect-window -.1            ;window focus follows the mouse pointer
 mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))) ;make mouse scrolling smooth
 indicate-buffer-boundaries 'left       ;fringe markers
 split-height-threshold 110             ;more readily split horziontally
 enable-recursive-minibuffers t
 custom-unlispify-menu-entries nil      ;M-x customize should not cripple menu entries
 custom-unlispify-tag-names nil         ;M-x customize should not cripple tags
 show-paren-delay 0
 load-prefer-newer t                    ;prefer newer .el instead of the .elc
 )

;;;; enable narrowing
(put 'narrow-to-region 'disabled nil)   ;narrow to region should be enabled by default

;;;; default flags
(setq-default
 tab-width 4
 indent-tabs-mode nil                   ;use spaces instead of tabs
 c-basic-offset 4                       ;"tab" with in c-related modes
 c-hungry-delete-key t                  ;delete more than one space
 )

;;;; enabled global modes
(global-auto-revert-mode 1)  ;auto revert buffers when changed on disk
(show-paren-mode t)          ;visualize()

;;;; disabled global modes
(blink-cursor-mode -1)       ;no cursor blinking
(tool-bar-mode -1)           ;disable the awful toolbar
(menu-bar-mode -1)           ;no menu, you can toggle it with C-c m
(scroll-bar-mode -1)         ;disable the sroll bar

;;;; disable full `yes' or `no' answers
(defalias 'yes-or-no-p 'y-or-n-p)

;;;; don't ask to kill buffers
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;;; load my theme
(quelpa '(grandshell-theme :repo "steckerhalter/grandshell-theme" :fetcher github))
(load-theme 'grandshell t)

;;;; use symbola font for emoticons
(defun my-after-make-frame (frame)
  (when (find-font (font-spec :name "Symbola") frame)
    (dolist (range '((#x2600 . #x26ff)
                     (#x1f300 . #x1f5ff)
                     (#x1f600 . #x1f640)
                     (#x1f680 . #x1f6ff)))
      (set-fontset-font "fontset-default" range "Symbola"))))
(add-to-list 'after-make-frame-functions 'my-after-make-frame)

;;;; better frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; key bindings
;;;; key-chord mode
(quelpa '(key-chord :fetcher wiki))
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.03)

;;;; `bind' macro to define keys
(defmacro bind (key fn)
  (let ((method (if (string-match "^[[:alnum:]]\\{2\\}$" (format "%s" key))
                          'key-chord-define-global
                        'global-set-key)))
  `(,method (kbd ,key) ,(if (listp fn) `(lambda () (interactive) ,fn) `',fn))))

;;;; global keys
;; unset C-t so we can use it
(global-unset-key (kbd "C-t"))
;;;;; general
(bind "C-h x" kill-emacs)
(bind "C-S-l" package-list-packages)
(bind "C-c n" my-show-file-name)
(bind "C-c d" ispell-change-dictionary)
(bind "C-c C-f" flyspell-mode)
(bind "M-x" helm-M-x)
(bind "C-h C-h" helm-M-x)
(bind "C-h h" helm-projectile)
(bind "C-h ," helm-apropos)
(bind "C-h ." helm-info-emacs)
(bind "C-h 4" helm-info-elisp)
(bind "C-h 3" helm-locate-library)
(bind "C-h C-p" find-file)
(bind "cg" customize-group)
(bind "C-c m" menu-bar-mode)
(bind "ln" linum-mode)
(bind "C-x C-u" my-url-insert-file-contents)
(bind "C-c C-w" browse-url-at-point)
;;;;; editing
(bind "C-z" undo-only)
(bind "<M-f10>" move-text-up)
(bind "<M-f9>" move-text-down)
(bind "C-S-c C-S-c" mc/edit-lines)
(bind "C-<" mc/mark-previous-like-this)
(bind "C->" mc/mark-next-like-this)
(bind "C-*" mc/mark-all-like-this)
(bind "vr" vr/replace)
(bind "i9" electric-indent-mode)
(bind "ac" align-current)
(bind "C-8" er/expand-region)
(bind "M-8" er/contract-region)
(bind "M-W" delete-region)
(bind "fc" flycheck-mode)
(bind "C-c q" auto-fill-mode)
(bind "C-c w" whitespace-cleanup)
(bind "C-h C-v" visual-line-mode)
(bind "C-h TAB" my-indent-whole-buffer)
;;;;; templates
(bind "C-t q" liquid-quote)
(bind "C-t l" liquid-tag)
;;;;; source
(bind "C-h C-0" edebug-defun)
(bind "C-h C-b" eval-buffer)
(bind "C-h C-e" toggle-debug-on-error)
(bind "C-h C-j" ipretty-last-sexp)
(bind "C-h C-k" ipretty-last-sexp-other-buffer)
(bind "C-h N" diff-hl-revert-hunk)
;;;;; directories
(bind "C-h C-u" dired-jump)
(bind "C-h C-f" fasd-find-file)
;;;;; buffers
(bind "C-h C-s" save-buffer)
(bind "C-c r" revert-buffer)
(bind "C-x C-b" ido-switch-buffer)
(bind "<f6>" (kill-buffer (buffer-name)))
(bind "<f8>" (switch-to-buffer nil))
(bind "jn" (switch-to-buffer nil))
(bind "fv" (kill-buffer (buffer-name)))
(bind "sv" save-buffer)
(bind "sc" (switch-to-buffer "*scratch*"))
(bind "<f9>" my-split-window)
;;;;; history
(bind "C-h C-SPC" helm-show-kill-ring)
(bind "C-h SPC" helm-all-mark-rings)
(bind "C-3" back-button-local-backward)
(bind "C-4" back-button-local-forward)
;;;;; occur
(bind "M-2" highlight-symbol-occur)
(bind "M-3" (highlight-symbol-jump -1))
(bind "M-4" (highlight-symbol-jump 1))
(bind "34" helm-imenu)
(bind "M-i" helm-swoop)
(bind "M-I" helm-swoop-back-to-last-point)
(bind "ok" projectile-multi-occur)
;;;;; windows
(bind "C-0" (select-window (previous-window)))
(bind "C-9" (select-window (next-window)))
(bind "<f2>" split-window-vertically)
(bind "<f3>" split-window-horizontally)
(bind "<f4>" delete-window)
(bind "<f5>" delete-other-windows)
(bind "<f7>" my-toggle-window-split)
(bind "M-9" my-switch-to-minibuffer-window)
(bind "<M-up>" buf-move-up)
(bind "<M-down>" buf-move-down)
(bind "<M-left>" buf-move-left)
(bind "<M-right>" buf-move-right)
(bind "C-h C-8" dedicated-mode)
;;;;; find/grep
(bind "vg" vc-git-grep)
(bind "C-h C-." elisp-slime-nav-find-elisp-thing-at-point)
(bind "C-h g" grep-find)
(bind "C-S-h C-S-g" find-grep-dired)
(bind "C-h C-o" occur)
(bind "C-h C-g" ag-project)
(bind "C-h C-l" helm-locate)
(bind "C-h C-y" projectile-find-file)
(bind "C-h G" projectile-grep)
(bind "C-h z" projectile-ack)
;;;;; version control
(bind "C-c g" magit-status)
(bind "C-c l" magit-log)
(bind "bm" magit-blame-mode)
;;;;; open/start stuff
(bind "C-c e" my-erc-connect)
(bind "C-h C-m" discover-my-major)
(bind "C-h C-<return>" eww)
(bind "C-h M-RET" my-eww-browse-dwim)
(bind "C-h C--" helm-google)
(bind "C-h r" google-translate-query-translate)
(bind "C-h C-r" google-translate-query-translate-reverse)
(bind "C-h C-c" helm-google-suggest)
(bind "C-S-h C-c" helm-wikipedia-suggest)
(bind "C-\"" shell-switcher-new-shell)
;;;;; org
(bind "C-h o" helm-info-org)
(bind "C-h C-n" (org-agenda nil "n"))
(bind "C-h t" (org-capture nil "s"))
(bind "C-h T" org-capture)
(bind "C-c i" org-clock-in-last)
(bind "C-c o" org-clock-out)
(bind "C-S-g" org-clock-goto)
(bind "C-c C-9" org-insert-subheading)
(bind "C-c C-0" org-insert-todo-subheading)
(bind "C-h C-w" org-cut-subtree)
;;;;; php
(bind "C-c v" var_dump-die)
(bind "C-c V" var_dump)

;;;; use C-return to invoke `helm-mini'
(define-key my-keys-minor-mode-map (kbd "<C-return>") 'helm-mini)

;;; `my' functions and advices
;;;; my-indent-whole-buffer
(defun my-indent-whole-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;;;; my-isearch-goto-match-beginning
(defun my-isearch-goto-match-beginning ()
  (when (and isearch-forward (not isearch-mode-end-hook-quit)) (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook 'my-isearch-goto-match-beginning)

;;;; my-show-file-name
(defun my-show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

;;;; my-show-help
(quelpa '(pos-tip :repo "syohex/pos-tip" :fetcher github :files ("pos-tip.el")))
(require 'pos-tip)
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
(define-key lisp-mode-shared-map (kbd "C-c C-d")
  (lambda ()
    (interactive)
    (my-show-help)))

;;;; my-split-window
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

;;;; my-switch-to-minibuffer-window
(defun my-switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;;;; my-toggle-window-split
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

;;;; my-url-insert-file-contents
(defun my-url-insert-file-contents (url)
  "Prompt for URL and insert file contents at point."
  (interactive "sURL: ")
  (url-insert-file-contents url))

;;;; advice to kill single line if there is no region
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
            (line-beginning-position 2)))))

;;;; don't really kill *scratch*
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))
;;; modes
;;;; anaconda-mode
(quelpa '(anaconda-mode :fetcher github :repo "proofit404/anaconda-mode" :files ("*.el" "*.py" "vendor/jedi/jedi" ("jsonrpc" "vendor/jsonrpc/jsonrpc/*.py"))))
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)

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

;;;; ag
(quelpa '(ag :repo "Wilfred/ag.el" :fetcher github))

;;;; apache-mode
(quelpa '(apache-mode :fetcher wiki))

;;;; back-button
(quelpa '(back-button :repo "rolandwalker/back-button" :fetcher github))
(setq back-button-local-keystrokes nil) ;don't overwrite C-x SPC binding
(require 'back-button)
(back-button-mode 1)

;;;; buffer-move
(quelpa '(buffer-move :fetcher wiki))

;;;; cider
;; cider needs queue which is in the ELPA repo but I have disabled that
(quelpa '(queue :url "http://www.dr-qubit.org/download.php?file=predictive/queue.el" :fetcher url :version original))
(quelpa '(cider :fetcher github :repo "clojure-emacs/cider" :old-names (nrepl)))
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces nil)
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-repl-use-clojure-font-lock t)

;;;; company
(quelpa '(company :repo "company-mode/company-mode" :fetcher github))
(require 'company)
(setq company-idle-delay 0.3)
(setq company-tooltip-limit 20)
(setq company-minimum-prefix-length 2)
(setq company-echo-delay 0)
(setq company-auto-complete nil)
(global-company-mode 1)
(add-to-list 'company-backends 'company-dabbrev t)
(add-to-list 'company-backends 'company-ispell t)
(add-to-list 'company-backends 'company-files t)
(add-to-list 'company-begin-commands 'outshine-self-insert-command)
(setq company-backends (remove 'company-ropemacs company-backends))

(defun my-pcomplete-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
(add-hook 'org-mode-hook #'my-pcomplete-capf)

(defun my-company-elisp-setup ()
  (set (make-local-variable 'company-backends)
       '((company-capf :with company-dabbrev-code))))

(dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook))
  (add-hook hook 'my-company-elisp-setup))

;;;; dedicated
(quelpa '(dedicated :fetcher github :repo "emacsmirror/dedicated"))
(require 'dedicated)

;;;; diff-hl
(quelpa '(diff-hl :fetcher github :repo "dgutov/diff-hl"))
(global-diff-hl-mode)

;;;; dired+
(quelpa '(dired+ :fetcher wiki))
(setq dired-auto-revert-buffer t)
(setq dired-no-confirm '(byte-compile chgrp chmod chown copy delete load move symlink))
(setq dired-deletion-confirmer (lambda (x) t))
(setq wdired-allow-to-change-permissions t) ; allow changing of file permissions
(toggle-diredp-find-file-reuse-dir 1)
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)

;;;; discover-my-major
(quelpa '(discover-my-major :fetcher github :repo "steckerhalter/discover-my-major"))

;;;; easy-kill
(quelpa '(easy-kill :fetcher github :repo "leoliu/easy-kill"))
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;;;; elisp-slime-nav
(quelpa '(elisp-slime-nav :repo "purcell/elisp-slime-nav" :fetcher github))
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
(define-key elisp-slime-nav-mode-map (kbd "C-c C-d") 'my-show-help)
(define-key elisp-slime-nav-mode-map (kbd "C-c d") 'elisp-slime-nav-describe-elisp-thing-at-point)

;;;; eval-sexp-fu
(quelpa '(eval-sexp-fu :fetcher wiki :files ("eval-sexp-fu.el")))
(require 'eval-sexp-fu)
(setq eval-sexp-fu-flash-duration 0.4)
(turn-on-eval-sexp-fu-flash-mode)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") 'eval-sexp-fu-eval-sexp-inner-list)
(define-key lisp-interaction-mode-map (kbd "C-c C-e") 'eval-sexp-fu-eval-sexp-inner-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-sexp-fu-eval-sexp-inner-list)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-sexp-fu-eval-sexp-inner-sexp)

;;;; erc
(quelpa '(erc-hl-nicks :fetcher github :repo "leathekd/erc-hl-nicks"))
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

;;;; eww
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
    (eww arg)))

;;;; expand-region
(quelpa '(expand-region :repo "magnars/expand-region.el" :fetcher github))

;;;; fasd
(quelpa '(fasd :repo "steckerhalter/emacs-fasd" :fetcher github))
(global-fasd-mode 1)

;;;; flycheck
;; let-alist would be in GNU ELPA but I have disabled that, so I need to fetch it before flycheck (which demands that):
(quelpa '(let-alist :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/let-alist/let-alist.el" :fetcher url :version original))
(quelpa '(flycheck :repo "flycheck/flycheck" :fetcher github))
(add-hook 'php-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook 'flycheck-mode)
(add-hook 'json-mode-hook 'flycheck-mode)
(add-hook 'nxml-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'lisp-interaction-mode-hook 'flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;disable the annoying doc checker
(setq flycheck-indication-mode 'right-fringe)

;;;; grizzl
(quelpa '(grizzl :repo "d11wtq/grizzl" :fetcher github))
(setq *grizzl-read-max-results* 30)

;;;; google-translate
(quelpa '(google-translate :fetcher github :repo "atykhonov/google-translate"))
(setq google-translate-default-source-language "de")
(setq google-translate-default-target-language "en")

;;;; haskell-mode
(quelpa '(haskell-mode :repo "haskell/haskell-mode" :fetcher github :files ("*.el" "haskell-mode.texi" "NEWS" "logo.svg")))
(require 'haskell-mode)
(setq haskell-indent-thenelse 3)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;;; helm
(quelpa '(helm :repo "emacs-helm/helm" :fetcher github :files ("*.el" "emacs-helm.sh")))
(quelpa '(helm-descbinds :repo "emacs-helm/helm-descbinds" :fetcher github))
(quelpa '(helm-gtags :repo "syohex/emacs-helm-gtags" :fetcher github :files ("helm-gtags.el")))
(quelpa '(helm-projectile :repo "bbatsov/projectile" :fetcher github :files ("helm-projectile.el")))
(require 'helm-config)
(setq helm-mode-handle-completion-in-region nil) ; don't use helm for `completion-at-point'
(helm-mode 1)
(helm-gtags-mode 1)
(helm-descbinds-mode)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(setq helm-buffer-max-length 50)
(setq helm-M-x-always-save-history t)
(setq helm-buffer-details-flag nil)
(add-to-list 'helm-completing-read-handlers-alist '(org-refile)) ; helm-mode does not do org-refile well
(add-to-list 'helm-completing-read-handlers-alist '(org-agenda-refile)) ; same goes for org-agenda-refile

(quelpa '(helm-google :fetcher github :repo "steckerhalter/helm-google"))
(setq helm-google-use-regexp-parsing t)

(quelpa '(helm-swoop :repo "ShingoFukuyama/helm-swoop" :fetcher github))
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;;;; highlight-symbol
(quelpa '(highlight-symbol :fetcher github :repo "nschum/highlight-symbol.el"))
(setq highlight-symbol-on-navigation-p t)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;;;; ido
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-everywhere t
      ido-default-buffer-method 'selected-window
      ido-max-prospects 32
      ido-use-filename-at-point 'guess
      )
(ido-mode 1)
(quelpa '(flx-ido :repo "lewang/flx" :fetcher github :files ("flx-ido.el")))
(flx-ido-mode 1)
(setq ido-use-faces nil)

;;;; iedit
(quelpa '(iedit :repo "victorhge/iedit" :fetcher github))
(require 'iedit)
(setq iedit-unmatched-lines-invisible-default t)

;;;; ielm
(eval-after-load 'ielm
  '(progn
     (add-hook 'inferior-emacs-lisp-mode-hook
               (lambda ()
                 (turn-on-eldoc-mode)))))

;;;; ipretty
(quelpa '(ipretty :fetcher github :repo "steckerhalter/ipretty"))
(ipretty-mode t)

;;;; js2-mode
(quelpa '(js2-mode :repo "mooz/js2-mode" :fetcher github))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'flycheck-mode)

;;;; json-mode
(quelpa '(json-mode :fetcher github :repo "joshwnj/json-mode"))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;;;; livedown
(quelpa '(livedown :fetcher github :repo "shime/emacs-livedown"))
(require 'livedown)

;;;; magit
(quelpa '(magit :fetcher github
                :repo "magit/magit"
                :files ("magit.el" "magit-bisect.el" "magit-blame.el" "magit-key-mode.el" "magit-popup.el" "magit-wip.el" "magit.texi" "AUTHORS.md" "README.md")))
(when (fboundp 'file-notify-add-watch)
  (quelpa '(magit-filenotify :fetcher github :repo "magit/magit-filenotify"))
  (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))
(setq magit-save-some-buffers nil) ;don't ask to save buffers
(setq magit-set-upstream-on-push t) ;ask to set upstream
(setq magit-diff-refine-hunk t) ;show word-based diff for current hunk
(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only) ;don't track with origin-*

;;;; markdown-mode
(quelpa '(markdown-mode :url "git://jblevins.org/git/markdown-mode.git" :fetcher git))
(require 'markdown-mode)
(setq gfm-liquid-font-lock-keywords
      (append
       gfm-font-lock-keywords
       '(("{%\\|%}\\|{{\\|}}" . font-lock-comment-face)
         ("{%\s*\\(quote\\|endquote\\|blockquote\\|endblockquote\\|codeblock\\|endcodeblock\\|pullquote\\|endpullquote\\|img\\|link\\|rawblock\\|endrawblock\\)" (1 font-lock-keyword-face))
         ("{%\s*\\(?:quote\\|blockquote\\|codeblock\\|pullquote\\|img\\|link\\|rawblock\\)\s+\\(\\(?:\\w\\|\\.\\|_\\)+\\)" (1 font-lock-variable-name-face))
         ("{{\s*\\(\\(?:\\w\\|\\.\\)+\\)" (1 font-lock-variable-name-face))
         ("\s+|\s+" . font-lock-comment-face))))
(define-derived-mode gfm-liquid-mode gfm-mode
  "Major mode for editing GitHub Flavored Markdown files with Liquid highlighting."
  (set (make-local-variable 'font-lock-defaults)
       '(gfm-liquid-font-lock-keywords)))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-liquid-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-liquid-mode))

;;;;; liquid-tag skeleton
(define-skeleton liquid-tag
  "Inserts a liquid tag"
  "tag: "
  "{% " str " " _ " %}" \n
  "{% end" str " %}")

;;;;; liquid-quote skeleton
(define-skeleton liquid-quote
  "Inserts a liquid tag"
  "tag: "
  "{% quote " _ " %}" \n
  "{% endquote %}")

;;;; move-text
(quelpa '(move-text :fetcher wiki))
(require 'move-text)

;;;; mu4e
(when (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (autoload 'mu4e "mu4e" "Mail client based on mu (maildir-utils)." t)
  (require 'org-mu4e)
  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (setq mu4e-html2text-command "html2text -utf8 -width 72")
  (setq mu4e-update-interval 60)
  (setq mu4e-auto-retrieve-keys t)
  (setq mu4e-headers-leave-behavior 'apply)
  (setq mu4e-headers-visible-lines 20)
  (setq mu4e-hide-index-messages t)

  (add-hook 'mu4e-headers-mode-hook (lambda () (local-set-key (kbd "X") (lambda () (interactive) (mu4e-mark-execute-all t)))))
  (add-hook 'mu4e-view-mode-hook (lambda () (local-set-key (kbd "X") (lambda () (interactive) (mu4e-mark-execute-all t)))))

  (defun mu4e-headers-mark-all-unread-read ()
    (interactive)
    (mu4e~headers-mark-for-each-if
     (cons 'read nil)
     (lambda (msg param)
       (memq 'unread (mu4e-msg-field msg :flags)))))

  (defun mu4e-flag-all-read ()
    (interactive)
    (mu4e-headers-mark-all-unread-read)
    (mu4e-mark-execute-all t))

  (setq message-kill-buffer-on-exit t))

;;;; multiple-cursors
(quelpa '(multiple-cursors :fetcher github :repo "magnars/multiple-cursors.el"))

;;;; ob-php
(quelpa '(ob-php :fetcher github :repo "steckerhalter/ob-php"))
(add-to-list 'org-babel-load-languages '(php . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;;;; org
;; we get `org' with contrib, so if the included `htmlize' is not available we need to force an upgrade
(let ((quelpa-upgrade-p (not (require 'htmlize nil t))))
  (quelpa '(org :url "git://orgmode.org/org-mode.git" :fetcher git
                :files ("lisp/*.el" "contrib/lisp/*.el" "doc/dir" "doc/*.texi"))))
(require 'org)
(require 'ox-org)
(require 'ox-md)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-startup-folded t)
(setq org-startup-indented nil)
(setq org-startup-with-inline-images t)
(setq org-startup-truncated t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)
(setq org-confirm-babel-evaluate nil)
(setq org-use-speed-commands t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-dim-blocked-tasks t)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

;; Don't use the same TODO state as the current heading for new heading
(defun my-org-insert-todo-heading () (interactive) (org-insert-todo-heading t))
(define-key org-mode-map (kbd "<M-S-return>") 'my-org-insert-todo-heading)

;;;;; agenda
;; create the file for the agendas if it doesn't exist
(let ((agendas "~/.agenda_files"))
  (unless (file-readable-p agendas)
    (with-temp-file agendas nil))
  (setq org-agenda-files agendas))

;; display the agenda first
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODO's"
         ((alltodo "")
          (agenda "")))))

(defun my-initial-buffer-choice ()
  (org-agenda nil "n")
  (delete-other-windows)
  (current-buffer))
(setq initial-buffer-choice #'my-initial-buffer-choice)

(setq org-agenda-start-with-log-mode t)
(setq org-agenda-todo-ignore-scheduled 'future) ; don't show future scheduled
(setq org-agenda-todo-ignore-deadlines 'far)    ; show only near deadlines

(setq
 appt-message-warning-time 30
 appt-display-interval 15
 appt-display-mode-line t      ; show in the modeline
 appt-display-format 'window)
(appt-activate 1)              ; activate appt (appointment notification)

(org-agenda-to-appt)           ; add appointments on startup

;; add new appointments when saving the org buffer, use 'refresh argument to do it properly
(defun my-org-agenda-to-appt-refresh () (org-agenda-to-appt 'refresh))
(defun my-org-mode-hook ()
  (add-hook 'after-save-hook 'my-org-agenda-to-appt-refresh nil 'make-it-local))
(add-hook 'org-mode-hook 'my-org-mode-hook)

(require 'notifications)
(defun my-appt-disp-window-function (min-to-app new-time msg)
  (notifications-notify :title (format "Appointment in %s min" min-to-app) :body msg))
(setq appt-disp-window-function 'my-appt-disp-window-function)
(setq appt-delete-window-function (lambda (&rest args)))

;; add state to the sorting strategy of todo
(setcdr (assq 'todo org-agenda-sorting-strategy) '(todo-state-up priority-down category-keep))

;;;;; templates
(setq org-capture-templates
      '(
        ("t" "Task" entry (file "") "* TODO %?\n %a")
        ("s" "Simple Task" entry (file "") "* TODO %?\n")
        ))

(add-to-list 'org-structure-template-alist '("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("S" "#+BEGIN_SRC shell-script\n?\n#+END_SRC\n"))

;;;;; todo
(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "WAITING(w)"
         "SCHEDULED(s)"
         "FUTURE(f)"
         "|"
         "DONE(d)"
         )))
(setq org-todo-keyword-faces
      '(
        ("SCHEDULED" . warning)
        ("WAITING" . font-lock-doc-face)
        ("FUTURE" . "white")
        ))
(setq org-log-into-drawer t) ; don't clutter files with state logs

;;;;; clocking
(setq org-clock-idle-time 15)
(setq org-clock-in-resume t)
(setq org-clock-persist t)
(setq org-clock-persist-query-resume nil)
(when (executable-find "xprintidle")
  (setq org-x11idle-exists-p t)
  (setq org-clock-x11idle-program-name "xprintidle"))
(org-clock-persistence-insinuate)
(setq org-clock-frame-title-format (append '((t org-mode-line-string)) '(" ") frame-title-format))
(setq org-clock-clocked-in-display 'both)

;;;;; org-journal
(quelpa '(org-journal :repo "bastibe/org-journal" :fetcher github))
(let ((dir "~/Dropbox/journal/"))
  (when (file-exists-p dir)
    (setq org-journal-dir dir)))

;;;;; org-mobile-sync
(when (and (boundp 'org-mobile-directory) (version<= "24.3.50" emacs-version))
  (quelpa '(org-mobile-sync :repo "steckerhalter/org-mobile-sync" :fetcher github))
  (setq org-mobile-inbox-for-pull (concat org-directory "/notes.org"))
  (org-mobile-sync-mode 1))

;;;; latex
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;;; outshine
(quelpa '(outshine :fetcher github :repo "tj64/outshine" :files ("outshine.el")))
(require 'outshine)
(add-hook 'outline-minor-mode-hook 'outshine-hook-function)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
;;;; php
(quelpa '(geben :fetcher svn :url "http://geben-on-emacs.googlecode.com/svn/trunk/"))
(quelpa '(php-align :fetcher github :repo "tetsujin/emacs-php-align"))
(quelpa '(php-boris :repo "tomterl/php-boris" :fetcher github))
(quelpa '(php-boris-minor-mode :fetcher github :repo "steckerhalter/php-boris-minor-mode"))
(quelpa '(php-eldoc :repo "sabof/php-eldoc" :fetcher github :files ("*.el" "*.php")))
(quelpa '(php-mode :repo "ejmr/php-mode" :fetcher github))

(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
(setq php-mode-coding-style "Symfony2")
(setq php-template-compatibility nil)

(let ((manual "/usr/share/doc/php-doc/html/"))
  (when (file-readable-p manual)
    (setq php-manual-path manual)))

(defun my-php-completion-at-point ()
  "Provide php completions for completion-at-point.
Relies on functions of `php-mode'."
  (let ((pattern (php-get-pattern)))
    (when pattern
      (list (- (point) (length pattern))
            (point)
            (or php-completion-table
                (php-completion-table))
            :exclusive 'no))))

(defun setup-php-mode ()
  (require 'php-align nil t)
  (add-hook 'completion-at-point-functions 'my-php-completion-at-point nil t)
  (set (make-local-variable 'company-backends)
       '((company-capf :with company-dabbrev-code)))
  (php-align-setup)
  (set (make-local-variable 'electric-indent-mode) nil)
  (php-eldoc-enable))
(add-hook 'php-mode-hook 'setup-php-mode)

(defun var_dump-die ()
  (interactive)
  (let ((expression (if (region-active-p)
                        (buffer-substring (region-beginning) (region-end))
                      (sexp-at-point)))
        (line (thing-at-point 'line))
        (pre "die(var_dump(")
        (post "));"))
    (if expression
        (progn
          (beginning-of-line)
          (if (string-match "return" line)
              (progn
                (newline)
                (previous-line))
            (next-line)
            (newline)
            (previous-line))
          (insert pre)
          (insert (format "%s" expression))
          (insert post))
      ()
      (insert pre)
      (insert post)
      (backward-char (length post)))))

(defun var_dump ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (insert ");")
        (goto-char (region-beginning))
        (insert "var_dump("))
    (insert "var_dump();")
    (backward-char 3)))

;;;; prog
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;;;; projectile
(quelpa '(projectile :repo "bbatsov/projectile" :fetcher github :files ("projectile.el")))
(require 'projectile nil t)
(setq projectile-completion-system 'grizzl)

;;;; rainbow-mode
(quelpa '(rainbow-mode :fetcher url :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/rainbow-mode/rainbow-mode.el"))

(dolist (hook '(css-mode-hook
                html-mode-hook
                js-mode-hook
                emacs-lisp-mode-hook
                org-mode-hook
                text-mode-hook
                ))
  (add-hook hook 'rainbow-mode))

;;;; recentf
(setq recentf-save-file (expand-file-name "~/.recentf"))
(recentf-mode 1)

;;;; robe
(quelpa '(robe :repo "dgutov/robe" :fetcher github :files ("robe*.el" "company-robe.el" "lib")))
(push 'company-robe company-backends)
(add-hook 'ruby-mode-hook 'robe-mode)

;;;; saveplace
(require 'saveplace)
(setq-default save-place t)

(setq savehist-additional-variables '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history))
(savehist-mode 1)

;;;; shell-switcher
(quelpa '(shell-switcher :fetcher github :repo "DamienCassou/shell-switcher" :files ("rswitcher.el" "shell-switcher.el")))
(setq shell-switcher-new-shell-function 'shell-switcher-make-ansi-term)
(setq shell-switcher-mode t)
(require 'shell-switcher)

;;;; skewer-mode
(quelpa '(skewer-mode :repo "skeeto/skewer-mode" :fetcher github :files ("*.html" "*.js" "*.el")))
(skewer-setup)

;;;; smart-mode-line
(quelpa '(smart-mode-line :repo "Bruce-Connor/smart-mode-line" :fetcher github))
(setq sml/vc-mode-show-backend t)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'respectful)

;;;; smartparens
(quelpa '(smartparens :fetcher github :repo "Fuco1/smartparens"))
(require 'smartparens-config)
(smartparens-global-mode t)
;; "fix"" highlight issue in scratch buffer
(custom-set-faces '(sp-pair-overlay-face ((t ()))))
(define-key sp-keymap (kbd "C--") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-=") 'sp-backward-sexp)
(define-key sp-keymap (kbd "C-.") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-,") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-e") 'sp-end-of-sexp)
(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
(define-key sp-keymap (kbd "C-S-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-S-w") 'sp-copy-sexp)
(define-key sp-keymap (kbd "M-S-<backspace>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
(define-key sp-keymap (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-}") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-(") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-{") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-S-f") 'sp-select-next-thing)
(define-key sp-keymap (kbd "C-S-b") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-\\") 'sp-select-previous-thing-exchange)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)
(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)
(define-key sp-keymap (kbd "M-S") 'sp-split-sexp)
(define-key sp-keymap (kbd "M-r") 'sp-splice-sexp-killing-around)
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :wrap "C-M-9"))

(setq sgml-basic-offset 4)
(add-hook 'sgml-mode-hook 'sgml-electric-tag-pair-mode)

(when (file-exists-p "~/quicklisp/slime-helper.el") (load "~/quicklisp/slime-helper.el"))

;;;; stylus-mode
(quelpa '(stylus-mode :fetcher github :repo "brianc/jade-mode" :files ("stylus-mode.el")))

;;;; term
(defun my-term-toggle-char-line-mode ()
  "Toggle between `term-char-mode' and `term-line-mode'."
  (interactive)
  (when (equal major-mode 'term-mode)
    (if (term-in-line-mode)
        (term-char-mode)
      (term-line-mode))))

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
  (define-key term-raw-map (kbd "C-h") nil) ; unbind C-h
  (define-key term-raw-map (kbd "M-x") nil) ; unbind M-x
  (define-key term-raw-map (kbd "C-7") 'my-term-toggle-char-line-mode)
  (define-key term-mode-map (kbd "C-7") 'my-term-toggle-char-line-mode))
(add-hook 'term-mode-hook 'my-term-setup t)

;;;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-min-dir-content 2)

;;;; yaml-mode
(quelpa '(yaml-mode :repo "yoshiki/yaml-mode" :fetcher github))

;;;; vlf
(quelpa '(vlf :repo "m00natic/vlfi" :fetcher github :old-names (vlfi)))
(setq vlf-application 'dont-ask)        ; just do it
(setq vlf-batch-size 8192)              ; a bit more text per batch please
(require 'vlf-setup)

;;;; web-mode
(quelpa '(web-mode :repo "fxbois/web-mode" :fetcher github))
(setq web-mode-markup-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs?\\'" . web-mode))

;;;; w3m
(when (require 'w3m nil t)
  (setq
   w3m-use-favicon nil
   w3m-default-display-inline-images t
   w3m-search-word-at-point nil
   w3m-use-cookies t
   w3m-home-page "http://en.wikipedia.org/"
   w3m-cookie-accept-bad-cookies t
   w3m-session-crash-recovery nil)
  (add-hook 'w3m-mode-hook
            (function (lambda ()
                        (set-face-foreground 'w3m-anchor-face "LightSalmon")
                        (set-face-foreground 'w3m-arrived-anchor-face "LightGoldenrod")
                        ;;(set-face-background 'w3m-image-anchor "black")
                        (load "w3m-lnum")
                        (defun w3m-go-to-linknum ()
                          "Turn on link numbers and ask for one to go to."
                          (interactive)
                          (let ((active w3m-lnum-mode))
                            (when (not active) (w3m-lnum-mode))
                            (unwind-protect
                                (w3m-move-numbered-anchor (read-number "Anchor number: "))
                              (when (not active) (w3m-lnum-mode))))
                          (w3m-view-this-url)
                          )
                        (define-key w3m-mode-map "f" 'w3m-go-to-linknum)
                        (define-key w3m-mode-map "L" 'w3m-lnum-mode)
                        (define-key w3m-mode-map "o" 'w3m-previous-anchor)
                        (define-key w3m-mode-map "i" 'w3m-next-anchor)
                        (define-key w3m-mode-map "w" 'w3m-search-new-session)
                        (define-key w3m-mode-map "p" 'w3m-previous-buffer)
                        (define-key w3m-mode-map "n" 'w3m-next-buffer)
                        (define-key w3m-mode-map "z" 'w3m-delete-buffer)
                        (define-key w3m-mode-map "O" 'w3m-goto-new-session-url)
                        )))
  )

;;;; my-keys-minor-mode (must be last)
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " K" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

;;; steckemacs.el ends here
