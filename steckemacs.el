;;; steckemacs.el --- steckemacs emacs configuration  -*- lexical-binding: t; -*-

;; Copyright 2026, Reto Hubmann

;; Author: Reto Hubmann (@steckerhalter)
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

;; Emacs configuration that fetches and compiles packages directly from source repos via `quelpa2'.
;; To make it easier to navigate the init file, `outshine' is used for org-mode-like folding: use `C-M-i' to cycle

;;; Requirements:

;; Emacs 30.1

;;; Code:

;;; initialization
(require 'package)
(package-initialize)

;; Bootstrap and load quelpa2 directly
(unless (package-installed-p 'quelpa2)
  (package-vc-install '(quelpa2 :url "https://github.com/quelpa/quelpa2")))
(require 'quelpa2)

;;; settings
;; load custom user code
(when (file-readable-p "~/.user.el")
  (load "~/.user.el"))

;;;; startup & core Behavior
(defvar my-todo "~/Sync/notes/todo.org")
(setq inhibit-startup-message t)
(setq initial-buffer-choice my-todo)
(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
;; Automatically handle file-local variables without prompting
(setq enable-local-variables :safe)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;;;; files, backups & auto-saving
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq recentf-max-saved-items 5000)
(setq confirm-nonexistent-file-or-buffer nil)
(setq vc-follow-symlinks t)
(setq delete-by-moving-to-trash t)
;; don't ask to kill buffers
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;;; editing, indentation & completion
(setq require-final-newline t)
(setq default-major-mode 'text-mode)
(setq tab-always-indent 'complete)
(setq safe-local-variable-values '((engine . django)))
(setq-default
 tab-width 4
 indent-tabs-mode nil
 c-basic-offset 4
 c-hungry-delete-key t)

;;;; ui, windows & mouse behavior
(setq column-number-mode t)
(setq mouse-yank-at-point t)
(setq mouse-autoselect-window -.1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
(setq indicate-buffer-boundaries 'left)
(setq show-paren-delay 0)
(setq split-height-threshold 140)
(setq split-width-threshold 140)
(setq switch-to-buffer-preserve-window-point t)
;; disable full yes or no answers, y and n suffices
(defalias 'yes-or-no-p 'y-or-n-p)
;; display time in mode-line
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(setq display-time-use-mail-icon t)
(display-time)
(blink-cursor-mode -1)
;; Disable UI elements globally for all future frames (ideal for daemon mode)
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(defvar my-font-attributes '(default nil :family "fixed" :width semi-condensed :height 120))
(apply 'set-face-attribute my-font-attributes)

;;;; history, rings & undo limits
(setq history-length 250)
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)
(setq kill-ring-max 5000)
(setq mark-ring-max 5000)
(setq undo-limit 67108864)
(setq undo-strong-limit 100663296)
(setq undo-outer-limit 1006632960)

;;;; system, environment & performance
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq send-mail-function 'sendmail-send-it)
(setq enable-recursive-minibuffers t)
(setq load-prefer-newer t)
(setq gc-cons-percentage 0.3)
(setq long-line-threshold nil)
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")))

;;; key bindings
;;;; free keys for usage
(dolist (n (number-sequence 0 9))
  (global-unset-key (kbd (format "M-%d" n)))
  (global-unset-key (kbd (format "C-%d" n))))

;;;; umlauts on en layout
(global-set-key (kbd "s-;") (lambda () (interactive) (insert "ö")))
(global-set-key (kbd "s-'") (lambda () (interactive) (insert "ä")))
(global-set-key (kbd "s-[") (lambda () (interactive) (insert "ü")))
(global-set-key (kbd "s-:") (lambda () (interactive) (insert "Ö")))
(global-set-key (kbd "s-\"") (lambda () (interactive) (insert "Ä")))
(global-set-key (kbd "s-{") (lambda () (interactive) (insert "Ü")))

;;;; movement & navigation
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 1 t)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 1 t)))
(global-set-key (kbd "M-o") 'back-button-local-backward)
(global-set-key (kbd "M-i") 'back-button-local-forward)

;;;; window management
(global-set-key (kbd "<f2>") 'split-window-vertically)
(global-set-key (kbd "<f3>") 'split-window-horizontally)
(global-set-key (kbd "<f4>") 'delete-window)
(global-set-key (kbd "<f5>") 'delete-other-windows)
(global-set-key (kbd "<f7>") (lambda () (interactive) (switch-to-buffer nil)))
(global-set-key (kbd "M-<f7>") 'my-toggle-window-split)
(global-set-key (kbd "S-<f7>") 'my-split-window)
(global-set-key (kbd "M-9") 'other-window)

;;;; buffer, file & session management
(global-set-key (kbd "M-0") 'save-buffer)
(global-set-key (kbd "M-7") 'iflipb-next-buffer)
(global-set-key (kbd "M-8") 'iflipb-previous-buffer)
(global-set-key (kbd "<f6>") 'my-kill-buffer)
(global-set-key (kbd "C-c k") 'my-kill-buffer)
(global-set-key (kbd "C-c 0") 'erase-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c K") 'kill-emacs)

;;;; helm & search
(global-set-key (kbd "M-1") 'helm-projectile-ag)
(global-set-key (kbd "M-2") 'helm-show-kill-ring)
(global-set-key (kbd "M-S-2") 'helm-all-mark-rings)
(global-set-key (kbd "M-3") 'helm-occur)
(global-set-key (kbd "C-4") 'helm-mini)
(global-set-key (kbd "C-0") 'highlight-symbol-prev)
(global-set-key (kbd "C-9") 'highlight-symbol-next)
(global-set-key (kbd "C-c w") 'web-search)

;;;; org-mode, notes & agenda
(global-set-key (kbd "C-1") 'my-capture)
(global-set-key (kbd "C-2") (lambda () (interactive) (my-capture t)))
(global-set-key (kbd "C-3") (lambda () (interactive) (org-agenda nil "p")))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<f8>") 'journal)
(global-set-key (kbd "S-<f8>") (lambda () (interactive) (jump-to-id "kinder")))

;;;; git & version control
(global-set-key (kbd "C-5") 'magit-status)
(global-set-key (kbd "C-c h") 'diff-hl-show-hunk)
(global-set-key (kbd "C-c H") 'diff-hl-revert-hunk)

;;;; custom shortcuts, utilities & modes (`C-c` Prefixes)
(global-set-key (kbd "C-c s") 'my-switch-to-scratch)
(global-set-key (kbd "C-c t") 'my-todo-buffer)
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/steckemacs.el/steckemacs.el")))
(global-set-key (kbd "C-c f") 'flyspell-mode)
(global-set-key (kbd "C-c c") 'kde-connect-scratchpad)
(global-set-key (kbd "C-c S") 'shell-switcher-switch-buffer)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c D") 'ispell-change-dictionary)
(global-set-key (kbd "C-c m") 'menu-bar-mode)

;;; functions
(defun export-song () (interactive)
       (rename-file (org-latex-export-to-pdf nil t nil nil '(:with-toc nil :with-tags nil))
                    (concat "~/Nextcloud/chords/"
                            (car (split-string (org-entry-get nil "ITEM") "-" t split-string-default-separators))
                            ".pdf")
                    t))
(defun my-todo-buffer () (interactive)
       (find-file my-todo))
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
  "Split window to see recent buffer."
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
  "Show full path in minibuffer."
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
             (this-win-2nd (not (and (<= (car this-win-edges) (car next-win-edges))
                                     (<= (cadr this-win-edges) (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges) (car (window-edges (next-window))))
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
  (interactive "sURL: ")
  (url-insert-file-contents url))

(defun my-sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun my-timestamp ()
  (interactive)
  (let ((timestring (if current-prefix-arg "%H:%M" "%a %d.%m.%y %H:%M")))
    (insert (format-time-string timestring))))

(defun my-xdg-open-dir ()
  (interactive)
  (shell-command (concat "xdg-open " (shell-quote-argument (expand-file-name default-directory)))))

(defun my-json-format ()
  (interactive)
  (switch-to-buffer (get-buffer-create "output.json"))
  (yank)
  (json-reformat-region (point-min) (point-max))
  (json-mode))

(defun my-browse-url-dwim ()
  (interactive)
  (require 'ffap)
  (let* ((input (or (shr-url-at-point nil)
                    (url-get-url-at-point)
                    (when (eq major-mode 'org-mode)
                      (org-element-property :raw-link (org-element-lineage (org-element-context) '(link) t)))
                    (current-kill 0 t)))
         (url (if (string-match-p ffap-url-regexp input) input (read-string "URL: ")))
         (prefix (prefix-numeric-value current-prefix-arg)))
    (cond ((= prefix 0) (xwidget-webkit-browse-url url))
          ((= prefix 4) (eww url))
          (t (browse-url url)))))

(defun my-capture (&optional playlist)
  (interactive)
  (raise-frame)
  (if playlist (org-capture nil "P") (org-capture nil "s")))

(defun my-songs ()
  (interactive)
  (let ((songs (org-map-entries
                (lambda () (substring (org-element-property :title (org-element-at-point)) 0 -13))
                nil 'region-start-level)))
    (switch-to-buffer (get-buffer-create "Reto's Songs"))
    (erase-buffer)
    (insert "# Reto's Songs\n\n")
    (dolist (line songs)
      (insert line)
      (insert "  ")
      (newline))
    (markdown-mode)
    (markdown-preview)))

(defun notes-publish ()
  (interactive)
  (org-publish-project "notes")
  (let ((auth (nth 0 (auth-source-search :host "legtux.org"))))
    (shell-command
     (concat "lftp -e \"open legtux.org; user " (plist-get auth :user) " '" (auth-info-password auth) "';mirror --no-symlinks --reverse --continue --exclude-glob=.git/* --verbose ~/web/notes /retonom/notes; bye\""))))

(defun retonom ()
  (interactive)
  (org-publish "web")
  (when current-prefix-arg (sync-retonom)))

(defun sync-retonom (&optional dir)
  (interactive)
  (let ((auth (let ((netrc-file "~/.netrc")) (netrc-credentials "ftp.legtux.org"))))
    (shell-command
     (concat "lftp -e \"open ftp.legtux.org; user " (car auth) " '" (cadr auth) "';mirror --no-symlinks --reverse --continue --delete --verbose ~/retonom/" dir " /retonom/" dir "; bye\""))))

(defun kkadw-sync (&optional dir)
  (interactive)
  (let ((auth (let ((netrc-file "~/.netrc")) (netrc-credentials "legtux.org"))))
    (shell-command
     (concat "lftp -e \"open legtux.org; user " (car auth) " '" (cadr auth) "';mirror --no-symlinks --reverse --continue --delete --exclude-glob=.git/* --verbose ~/projects/linktree" dir " /kkadw" dir "; bye\""))))

(defun my-after-save-hook ()
  (let ((matches '("songs.org" "demos.org" "raw.org" "setup.org" "index.org")))
    (when (and buffer-file-name current-prefix-arg (member (file-name-nondirectory buffer-file-name) matches))
      (publish-music))))
(add-hook 'after-save-hook 'my-after-save-hook)

(defun jump-to-id (id)
  (org-link-open-from-string (format "[[#%s]]" id)))

(defun journal (&optional arg)
  (interactive "p")
  (jump-to-id "tagebuch")
  (org-end-of-meta-data t)
  (when (equal arg 4) (org-insert-heading nil nil 2)))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Don't really kill *scratch* but only bury it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;;; package configurations
;;;; aggressive-indent
(quelpa2 '(aggressive-indent :url "https://github.com/Malabarba/aggressive-indent-mode"))
(require 'aggressive-indent)
(global-aggressive-indent-mode 1)

;;;; apache-mode
(quelpa2 '(apache-mode :url "https://github.com/emacsmirror/apache-mode"))
(require 'apache-mode)

;;;; appt
(setq appt-message-warning-time 30
      appt-display-interval 15
      appt-display-mode-line t
      appt-display-format 'window)
(appt-activate 1)

;;;; auctex
(package-install 'auctex)
(require 'tex)
(setq TeX-PDF-mode t
      TeX-parse-self t
      TeX-auto-save t
      TeX-save-query nil)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'TeX-mode-hook
          (lambda ()
            (define-key TeX-mode-map (kbd "<C-f8>")
                        (lambda () (interactive) (TeX-command-menu "LaTeX")))))

;;;; autorevert
(require 'autorevert)
(setq auto-revert-interval 1)
(global-auto-revert-mode 1)

;;;; back-button
(quelpa2 '(smartrep :url "https://github.com/myuhe/smartrep.el"))
(quelpa2 '(pcache :url "https://github.com/sigma/pcache"))
(quelpa2 '(list-utils :url "https://github.com/rolandwalker/list-utils"))
(quelpa2 '(nav-flash :url "https://github.com/rolandwalker/nav-flash"))
(quelpa2 '(persistent-soft :url "https://github.com/rolandwalker/persistent-soft"))
(quelpa2 '(back-button :url "https://github.com/rolandwalker/back-button"))
(require 'back-button)
(setq back-button-local-keystrokes nil)
(back-button-mode 1)

;;;; browse-url
(require 'browse-url)
(defun my-browse-url-file (&optional file)
  (interactive)
  (cl-letf (((symbol-function 'browse-url) 'browse-url-firefox))
    (browse-url-of-file file)))

;;;; cider
(quelpa2 '(cider :url "https://github.com/clojure-emacs/cider"))
(setq cider-popup-stacktraces nil
      cider-repl-popup-stacktraces nil
      cider-repl-pop-to-buffer-on-connect t
      cider-repl-use-clojure-font-lock t)

;;;; company
(quelpa2 '(company :url "https://github.com/company-mode/company-mode"))
(require 'company)
(setq company-idle-delay 0.3
      company-tooltip-limit 20
      company-minimum-prefix-length 2)
(setq tab-always-indent 'complete)
(defvar completion-at-point-functions-saved nil)

(defun company-indent-for-tab-command (&optional arg)
  (interactive "P")
  (let ((completion-at-point-functions completion-at-point-functions)
        (completion-at-point-functions '(company-complete-common-wrapper)))
    (indent-for-tab-command arg)))

(defun company-complete-common-wrapper ()
  (let ((completion-at-point-functions completion-at-point-functions-saved))
    (company-complete-common)))

(global-company-mode 1)
(add-to-list 'company-backends 'company-dabbrev t)
(add-to-list 'company-backends 'company-ispell t)
(add-to-list 'company-backends 'company-files t)
(add-to-list 'company-begin-commands 'outshine-self-insert-command)
(setq company-backends (remove 'company-ropemacs company-backends))

(defun my-company-elisp-setup ()
  (set (make-local-variable 'company-backends)
       '((company-capf :with company-dabbrev-code))))

(quelpa2 '(company-statistics :url "https://github.com/company-mode/company-statistics"))
(require 'company-statistics)
(add-hook 'emacs-lisp-mode-hook 'my-company-elisp-setup)
(add-hook 'lisp-interaction-mode-hook 'my-company-elisp-setup)
(company-statistics-mode 1)

;;;; company backends & quickhelp
(quelpa2 '(pos-tip :url "https://github.com/pitkali/pos-tip"))
(quelpa2 '(company-quickhelp :url "https://github.com/company-mode/company-quickhelp"))
(require 'company-quickhelp)
(setq company-quickhelp-use-propertized-text t
	  company-quickhelp-delay 1)
(company-quickhelp-mode 1)

;;;; custom
(setq custom-unlispify-menu-entries nil
      custom-unlispify-tag-names nil)

;;;; deft
(quelpa2 '(deft :url "https://github.com/jrblevin/deft"))
(require 'deft)
(define-key deft-mode-map (kbd "<f6>") 'quit-window)
(define-key deft-mode-map (kbd "C-g") 'deft-filter-clear)
(define-key deft-mode-map (kbd "C-c C-c") 'deft-refresh)
(define-key deft-mode-map (kbd "<M-return>") 'deft-new-file)

(defun deft-print-header () (deft-set-mode-name))
(defun deft-set-mode-name ()
  (let* ((name (if deft-incremental-search "Deft" "Deft/R"))
         (filter (deft-whole-filter-regexp))
         (sep (unless (string= "" filter) "/")))
    (setq mode-name (concat name sep filter))))
(setq deft-strip-summary-regexp
      (concat "\\("
              "[\n\t]"
              "\\|^#\\+[[:upper:]_]+:.*$"
              "\\|^# .*$"
              "\\)"))
(setq deft-separator " "
      deft-directory "~/Sync"
      deft-file-naming-rules nil
      deft-recursive t
      deft-default-extension "org"
      deft-extensions '("org")
      deft-auto-save-interval 0)

;;;; diff-hl
(quelpa2 '(diff-hl :url "https://github.com/dgutov/diff-hl"))
(require 'diff-hl)
(global-diff-hl-mode 1)
(eval-after-load 'magit
  '(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;;; dired & wdired
(require 'dired)
(defun my-find-name-dired (pattern)
  (interactive "sFind-name (filename wildcard): ")
  (if (and (not current-prefix-arg) (executable-find "rg"))
      (let ((find-program (concat "rg -g " (shell-quote-argument pattern) " --files"))
            (find-ls-option (cons "" "-dilsb")))
        (find-dired default-directory ""))
    (find-dired default-directory (concat find-name-arg " " (shell-quote-argument pattern)))))

(defun my-dired-ediff-marked-files ()
  (interactive)
  (let ((files (dired-get-marked-files nil nil)))
    (if (= (length files) 2)
        (ediff-files (nth 0 files) (nth 1 files))
      (user-error "Please mark exactly 2 files"))))

(define-key dired-mode-map (kbd "E") #'my-dired-ediff-marked-files)
(define-key dired-mode-map (kbd "`") #'dired-toggle-read-only)

(setq dired-auto-revert-buffer t
      dired-no-confirm '(byte-compile chgrp chmod chown copy delete load move symlink)
      dired-deletion-confirmer (lambda (x) t))

(require 'wdired)
(setq wdired-allow-to-change-permissions t)

;;;; discover-my-major
(quelpa2 '(makey :url "https://github.com/mickeynp/makey"))
(quelpa2 '(discover-my-major :url "https://framagit.org/steckerhalter/discover-my-major.git"))
(require 'discover-my-major)

;;;; easylist-mode
(quelpa2 '(easylist-mode :url "https://github.com/steckerhalter/easylist-mode"))
(require 'easylist-mode)

;;;; easy-kill
(quelpa2 '(easy-kill :url "https://github.com/leoliu/easy-kill"))
(require 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;;;; eldoc
(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)

;;;; elec-pair
(electric-pair-mode 1)

;;;; elisp-slime-nav
(quelpa2 '(elisp-slime-nav :url "https://github.com/purcell/elisp-slime-nav"))
(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'ielm-mode-hook 'elisp-slime-nav-mode)
(add-hook 'lisp-interaction-mode-hook 'elisp-slime-nav-mode)

;;;; emms
(quelpa2 '(emms :url "https://github.com/emacs-straight/emms"))
(require 'emms)
(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
(global-set-key (kbd "<XF86AudioNext>") 'emms-next)
(global-set-key (kbd "S-<XF86AudioPause>") 'emms-toggle-repeat-track)
(global-set-key (kbd "S-<XF86AudioPrev>") 'emms-seek-backward)
(global-set-key (kbd "S-<XF86AudioNext>") 'emms-seek-forward)
(require 'emms-setup)
(setq emms-player-list '(emms-player-mpv))
(emms-standard)

;;;; erc
(require 'erc)
(add-hook 'erc-mode-hook (lambda ()
                           (erc-truncate-mode t)
                           (erc-fill-disable)
                           (set (make-local-variable 'scroll-conservatively) 1000)
                           (visual-line-mode)))
(setq erc-timestamp-format "%H:%M "
      erc-fill-prefix "      "
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-interpret-mirc-color t
      erc-kill-buffer-on-part t
      erc-kill-server-buffer-on-quit t
      erc-kill-queries-on-quit t
      erc-server-send-ping-interval 45
      erc-server-send-ping-timeout 180
      erc-server-reconnect-timeout 60
      erc-join-buffer 'window-noselect)
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun my-erc-connect ()
  (interactive)
  (if (erc-buffer-list)
      (switch-to-buffer (car (erc-buffer-list)))
    (erc-tls :server erc-server :port erc-port :nick erc-nick :full-name erc-user-full-name :password erc-password)))

;;;; eshell
(require 'eshell)
(add-hook 'eshell-mode-hook (lambda ()
                              (my-eshell-setup)
                              (eldoc-mode 1)))
(setq eshell-save-history-on-exit t
      eshell-destroy-buffer-when-process-dies t
      eshell-where-to-jump 'begin
      eshell-hist-ignoredups t
      eshell-history-size 10000)

(defun eshell/g (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo))

(defun my-eshell-setup ()
  (define-key eshell-mode-map (kbd "C-c p") 'helm-eshell-prompts)
  (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)
  (setq-local eldoc-idle-delay 3)
  (setenv "PAGER" "cat")
  (setenv "EDITOR" "emacsclient"))

(quelpa2 '(bash-completion :url "https://github.com/szermatt/emacs-bash-completion"))
(require 'bash-completion)
(defun eshell-bash-completion ()
  (setq-local bash-completion-nospace t)
  (while (pcomplete-here
          (nth 2 (bash-completion-dynamic-complete-nocomint
                  (save-excursion (eshell-bol) (point)) (point))))))
(setq eshell-default-completion-function 'eshell-bash-completion)

;;;; eww
(require 'eww)
(setq eww-search-prefix "https://startpage.com/do/m/mobilesearch?query=")

;;;; flymake
(require 'flymake)
(add-hook 'php-mode-hook 'flymake-mode-on)
(add-hook 'sh-mode-hook 'flymake-mode-on)
(add-hook 'nxml-mode-hook 'flymake-mode-on)
(add-hook 'python-mode-hook 'flymake-mode-on)
(add-hook 'emacs-lisp-mode-hook 'flymake-mode-on)
(add-hook 'lisp-interaction-mode-hook 'flymake-mode-on)
(flymake-mode-on)

;;;; flyspell
(require 'flyspell)
(setq flyspell-auto-correct-binding [nil])

;;;; frame
(require 'frame)
(modify-all-frames-parameters '((fullscreen . maximized)))

(defun my-after-make-frame (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (when (find-font (font-spec :name "Symbola") frame)
      (dolist (range '((#x2600 . #x26ff)
                       (#x1f300 . #x1f5ff)
                       (#x1f600 . #x1f640)
                       (#x1f680 . #x1f6ff)))
        (set-fontset-font "fontset-default" range "Symbola")))))
(add-to-list 'after-make-frame-functions 'my-after-make-frame)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;;; git-modes
(quelpa2 '(git-modes :url "https://github.com/magit/git-modes"))
(eval-after-load 'magit '(require 'git-modes))

;;;; grandshell-theme
(quelpa2 '(grandshell-theme :url "https://framagit.org/steckerhalter/grandshell-theme"))
(load-theme 'grandshell t)

;;;; helm
(quelpa2 '(helm :url "https://github.com/emacs-helm/helm"))
(require 'helm)
(setq async-bytecomp-allowed-packages nil)
(setq helm-idle-delay 0.1
      helm-input-idle-delay 0.1
      helm-buffer-max-length 50
      helm-M-x-always-save-history t
      helm-buffer-details-flag nil
      helm-mode-handle-completion-in-region nil
      helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")

(quelpa2 '(helm-projectile :url "https://github.com/bbatsov/helm-projectile"))
(require 'helm-projectile)

(quelpa2 '(helm-system-packages :url "https://github.com/emacsmirror/helm-system-packages"))
(require 'helm-system-packages)

;;;; highlight-symbol
(quelpa2 '(highlight-symbol :url "https://github.com/nschum/highlight-symbol.el"))
(require 'highlight-symbol)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(setq highlight-symbol-on-navigation-p t)

;;;; ibuffer
(require 'ibuffer)
(setq ibuffer-default-display-maybe-show-predicates t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;; ido
(require 'ido)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-default-buffer-method 'selected-window)
(ido-mode 1)
(put 'ido-exit-minibuffer 'disabled nil)

;;;; ido-completing-read+
(quelpa2 '(ido-completing-read+ :url "https://github.com/emacsmirror/ido-completing-read-plus"))
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(quelpa2 '(amx :url "https://github.com/DarwinAwardWinner/amx"))
(require 'amx)
(amx-mode 1)

;;;; iedit
(quelpa2 '(iedit :url "https://github.com/victorhge/iedit"))
(require 'iedit)
(setq iedit-unmatched-lines-invisible t
      iedit-toggle-key-default nil)

;;;; ielm
(add-hook 'inferior-emacs-lisp-mode-hook (lambda () (turn-on-eldoc-mode)))

;;;; iflipb
(quelpa2 '(iflipb :url "https://github.com/jrosdahl/iflipb"))
(require 'iflipb)
(setq iflipb-wrap-around t
      iflipb-ignore-buffers '("*helm.**"
                              "*magit-diff: .*"
                              "*Quail Completions*"
                              "*Completions*"
                              "*anaconda-mode*"
                              "*Deft*"
                              ":.*"))

;;;; ipretty
(quelpa2 '(ipretty :url "https://github.com/emacsmirror/ipretty"))
(require 'ipretty)
(ipretty-mode t)

;;;; kde-connect-mode
(quelpa2 '(kde-connect-mode :url "https://github.com/steckerhalter/kde-connect-mode"))
(require 'kde-connect-mode)

;;;; magit
(quelpa2 '(magit :url "https://github.com/magit/magit"))
(require 'magit)
(setq magit-no-confirm '(stage-all-changes)
      magit-push-always-verify nil
      git-commit-finish-query-functions nil
      magit-save-some-buffers nil
      magit-set-upstream-on-push t
      magit-diff-refine-hunk 'all
      magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
(setq magit-wip-after-save-mode 1
      magit-wip-after-apply-mode 1)

(define-key magit-mode-map (kbd "M-1") nil)
(define-key magit-mode-map (kbd "M-2") nil)
(define-key magit-mode-map (kbd "M-3") nil)
(define-key magit-mode-map (kbd "M-4") nil)

(defun magit-push-after-commit ()
  (magit-push-current-to-upstream nil))
(add-hook 'git-commit-post-finish-hook #'magit-push-after-commit)

;;;; man
(require 'man)
(setq Man-notify-method 'aggressive)

;;;; markdown-mode
(quelpa2 '(markdown-mode :url "https://github.com/jrblevin/markdown-mode"))
(require 'markdown-mode)
(define-key markdown-mode-map (kbd "C-M-i") 'markdown-shifttab)
(define-key markdown-mode-map (kbd "<backtab>") 'markdown-promote)
(define-key markdown-mode-map (kbd "<S-iso-lefttab>") 'markdown-promote)
(define-key markdown-mode-map (kbd "<S-tab>") 'markdown-promote)
(define-key markdown-mode-map (kbd "<C-tab>") 'markdown-shifttab)
(define-key markdown-mode-map (kbd "<M-S-return>") 'my-markdown-checkbox)
(define-key markdown-mode-map (kbd "<C-up>") 'markdown-move-up)
(define-key markdown-mode-map (kbd "<C-down>") 'markdown-move-down)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.lr\\'" . gfm-mode))

(defun my-markdown-checkbox ()
  (interactive)
  (call-interactively 'markdown-insert-list-item)
  (insert "[ ] "))

(defun my-markdown-convert-wiki-link (func name)
  (let ((markdown-link-space-sub-char " ")
        (major-mode 'markdown-mode))
    (funcall func name)))

(setq markdown-asymmetric-header t
      markdown-header-scaling t
      markdown-enable-wiki-links t
      markdown-list-indent-width 2
      markdown-footnote-location 'immediately
      markdown-wiki-link-fontify-missing t
      markdown-wiki-link-alias-first nil
      markdown-indent-on-enter 'indent-and-new-item
      markdown-command "pandoc -c http://benjam.info/panam/styling.css --from gfm -t html5 --mathjax --highlight-style pygments --metadata title=' ' --standalone")

(add-hook 'markdown-mode-hook 'visual-line-mode)
(advice-add 'markdown-convert-wiki-link-to-filename :around 'my-markdown-convert-wiki-link)

;;;; monky
(quelpa2 '(monky :url "https://github.com/ananthakumaran/monky"))
(require 'monky)
(setq monky-process-type 'cmdserver)

;;;; multiple-cursors
(quelpa2 '(multiple-cursors :url "https://github.com/magnars/multiple-cursors.el"))
(require 'multiple-cursors)
(setq mc/always-repeat-command t
      mc/always-run-for-all t)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-?") 'mc/mark-all-like-this)

;;;; open-junk-file
(quelpa2 '(open-junk-file :url "https://github.com/emacsmirror/open-junk-file"))
(require 'open-junk-file)
(setq open-junk-file-format "~/junk/%Y/%m/%d-%H%M%S.")

;;;; org
(require 'org)
(add-hook 'org-mode-hook (lambda ()
                           (setq-local company-idle-delay 0.3)
                           (setq-local electric-pair-mode nil)
                           (visual-line-mode)))
(define-key org-mode-map (kbd "C-c M-RET") 'org-insert-heading-after-current)
(define-key org-mode-map (kbd "C-c !") 'my-org-insert-time-stamp)
(define-key org-mode-map (kbd "C-c 1") 'my-org-insert-time-stamp)
(define-key org-mode-map (kbd "C-c d") 'org-archive-done-tasks)
(define-key org-mode-map (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "C-c c") 'org-copy-subtree)
(define-key org-mode-map (kbd "M-4") (lambda () (interactive) (org-todo 'done)))

(defvar org-capture-default '("s" "w"))
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
         :recursive t)
        ("notes" :components ("notes-org" "notes-files"))
        ("notes-org"
         :publishing-function org-html-publish-to-html
         :base-directory "~/Sync/notes"
         :publishing-directory "~/web/notes"
         :with-title nil
         :with-toc t
         :html-head-extra "<style>#content { max-width: 100% !important; margin: 5% !important; img { width: 100%; height: auto; }}</style>"
         :section-numbers nil)
        ("notes-files"
         :publishing-function org-publish-attachment
         :base-extension "ogg\\|mp3\\|m4a\\|mp4\\|aac\\|png\\|jpg\\|jpeg\\|css\\|html\\|js"
         :include (".htaccess")
         :base-directory "~/Sync/notes"
         :publishing-directory "~/web/notes"
         :recursive t)))

(setq org-capture-templates
      `(("P" "Playlist" entry (file "todo.org")"* TODO %^{Task Name}\nSCHEDULED: %(my/org-capture-get-hour-timestamp)\n" :immediate-finish t :prepend t)
        ("s" "home" entry (file "todo.org") "* TODO %?\n" :prepend t)
        ("w" "work" entry (file ,(expand-file-name "./notes/work.org" my-work-folder)) "* TODO %?\n")
        ("l" "Link" entry (file "") "* TODO %a %T\n" :prepend t)))
(setq org-todo-keywords '((sequence "TODO(t)" "DONE(d)")))
(setq org-tag-alist '(("einkauf" . ?e)
                      ("brocki" . ?b)
                      ("alarm" . ?a)
                      ("amazon" . ?z)
                      ("backlog" . ?l)
                      ("song" . ?s)
                      ("god" . ?g)))
(setq org-use-fast-todo-selection 'auto
      org-startup-indented t
      org-archive-mark-done t
      org-startup-with-inline-images t
      org-startup-truncated t
      org-startup-folded nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0
      org-confirm-babel-evaluate nil
      org-use-speed-commands t
      org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-html-postamble nil
      org-enforce-todo-checkbox-dependencies t
      org-directory "~/Sync/notes"
      org-default-notes-file my-todo
      org-image-actual-width nil
      org-log-repeat nil
      org-clock-idle-time 10
      org-clock-x11idle-program-name "xprintidle"
      org-tags-exclude-from-inheritance '("song")
      org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
      org-plain-list-ordered-item-terminator ?\)
      org-cycle-hide-drawers t
      org-reverse-note-order t
      org-refile-use-outline-path t)

(defun my-org-insert-time-stamp (&optional heading)
  (interactive)
  (when heading (org-insert-heading))
  (let ((time (current-time))
        (with-hm (not current-prefix-arg)))
    (if (equal current-prefix-arg '(16))
        (insert (format-time-string "%I:%M " time))
      (org-insert-time-stamp time with-hm t nil " "))))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

(defun my/org-process-time-input (input &optional brackets)
  (let* ((trimmed (org-trim (or input "")))
         (hour-str (cond
                    ((string-empty-p trimmed) "")
                    ((string-match "^\\([0-9]\\{1,2\\}\\)$" trimmed)
                     (format "%02d:00" (string-to-number trimmed)))
                    ((string-match "^\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)$" trimmed)
                     (concat (substring trimmed 0 2) ":" (substring trimmed 2 4)))
                    (t trimmed)))
         (time-str (format-time-string (concat "%Y-%m-%d %a " hour-str))))
    (if brackets (concat "<" time-str ">") time-str)))

(defun my/org-playlist-toggle (arg)
  (interactive "P")
  (let* ((marker (or (get-text-property (point) 'org-marker)
                     (get-text-property (point) 'org-hd-marker)))
         (buffer (if marker (marker-buffer marker) (current-buffer)))
         (pos (if marker (marker-position marker) (point))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (let ((is-scheduled (org-get-scheduled-time (point))))
          (cond
           (arg
            (let ((hour (read-string "Today at (14, 1430, 14:45): ")))
              (org-schedule nil (my/org-process-time-input hour))
              (message "Scheduled for today.")))
           (is-scheduled
            (org-schedule '(4))
            (message "Cleared -> Backlog."))
           (t
            (let ((hour (read-string "Today at (14, 1430, 14:45) [Enter=Today]: ")))
              (org-schedule nil (my/org-process-time-input hour))
              (message "Added to Playlist for today.")))))))
    (when (derived-mode-p 'org-agenda-mode)
      (org-agenda-redo))))

(defun my/org-capture-get-hour-timestamp ()
  (let ((input (read-string "Hour (14, 1430, 14:45) [Enter=Now]: ")))
    (my/org-process-time-input input t)))

(add-to-list 'org-file-apps '("\\(?:ogg\\|mp3\\|m4a\\)" . "mpv --player-operation-mode=pseudo-gui -- %s"))
(let ((list '(("T" . my/org-playlist-toggle)
              ("S" . org-schedule))))
  (if (boundp 'org-speed-commands)
      (setq org-speed-commands (append list org-speed-commands))
    (setq org-speed-commands-user list)))

;;;; org-tempo
(require 'org-tempo)

;;;; org-protocol
(require 'org-protocol)
(add-to-list 'org-capture-templates
             '("p" "Protocol" entry (file "")
               "* TODO %?[[%:link][%:description]]\n%i\n" :prepend t :immediate-finish t))
(add-to-list 'org-capture-templates
             '("L" "Protocol Link" entry (file "")
               "* TODO %?[[%:link][%:description]]\n" :prepend t :immediate-finish t))

;;;; org-agenda
(require 'org-agenda)
(define-key org-agenda-mode-map (kbd "M-4") (lambda () (interactive)
                                              (org-agenda-todo 'done)
                                              (org-agenda-redo-all)))
(define-key org-agenda-mode-map (kbd "T") 'my/org-playlist-toggle)
(defun my-org-agenda (key) (interactive) (org-agenda nil key))
(setq org-agenda-start-with-log-mode t
      org-agenda-todo-ignore-scheduled 'all
      org-agenda-todo-ignore-deadlines 'far
      org-agenda-dim-blocked-tasks t
      org-agenda-show-all-dates nil
      org-agenda-prefix-format "%?-12t% s"
      org-agenda-confirm-kill nil
      org-tags-match-list-sublevels nil
      org-agenda-span 2
      org-deadline-warning-days 1)

(setcdr (assq 'todo org-agenda-sorting-strategy) '(todo-state-up priority-down category-keep))

(let ((agendas "~/.agenda_files"))
  (unless (file-readable-p agendas)
    (with-temp-file agendas nil))
  (setq org-agenda-files agendas))

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
          (tags-todo "@work+DEADLINE>=\"<now>\"" ((org-agenda-overriding-header "scheduled")))))
        ("p" "Daily Planner"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
          (tags-todo "SCHEDULED=\"\"&DEADLINE=\"\"-backlog/!TODO"
                     ((org-agenda-overriding-header "")))
          (tags "SCHEDULED=\"\"&DEADLINE=\"\"+backlog/!TODO"
                ((org-agenda-overriding-header "")))))))

(defun my-org-agenda-to-appt-refresh () (org-agenda-to-appt 'refresh))
(defun my-org-mode-hook ()
  (add-hook 'after-save-hook 'my-org-agenda-to-appt-refresh nil 'make-it-local))
(add-hook 'org-mode-hook 'my-org-mode-hook)

;;;; notifications
(require 'notifications)
(defun my-appt-disp-window-function (min-to-app new-time msg)
  (notifications-notify :title (format "In %s min" min-to-app) :body msg))
(setq appt-disp-window-function 'my-appt-disp-window-function)
(setq appt-delete-window-function (lambda (&rest args)))

;;;; org-bullets
(quelpa2 '(org-bullets :url "https://github.com/sabof/org-bullets"))
(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;;;; ox-epub
(quelpa2 '(ox-epub :url "https://github.com/ofosos/ox-epub"))
(require 'ox-epub)
(defun org-epub-to-azw3-export-to-file (&optional async scope command void)
  (interactive)
  (let* ((epub-file (org-epub-export-to-epub))
         (output-file-base (file-name-sans-extension (buffer-file-name)))
         (azw3-file (concat output-file-base ".azw3"))
         (kindle-path "/run/user/1000/gvfs/mtp:host=Amazon_Kindle_Paperwhite_GN433X11525600D1/Internal Storage/documents/")
         (convert-command (format "ebook-convert %s %s --linearize-tables"
                                  (shell-quote-argument epub-file)
                                  (shell-quote-argument azw3-file))))
    (if (file-exists-p epub-file)
        (progn
          (message "Converting EPUB to AZW3...")
          (shell-command convert-command)
          (message "Exported to AZW3 file: %s" azw3-file)
          (shell-command (format "gio copy %s %s"
                                 (shell-quote-argument azw3-file)
                                 (shell-quote-argument kindle-path)))
          (message "Copied file to kindle at: %s" kindle-path))
      (error "EPUB export failed, cannot convert to AZW3"))))

(org-export-define-derived-backend 'azw3 'epub
  :menu-entry '(?a "Export to AZW3" org-epub-to-azw3-export-to-file))

;;;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

;;;; outshine
(quelpa2 '(outshine :url "https://github.com/alphapapa/outshine"))
(require 'outshine)
(add-hook 'outline-minor-mode-hook #'outshine-mode)
(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(setq outshine-imenu-show-headlines-p nil)

;;;; page-break-lines
(quelpa2 '(page-break-lines :url "https://github.com/purcell/page-break-lines"))
(require 'page-break-lines)
(global-page-break-lines-mode)

;;;; paren
(require 'paren)
(show-paren-mode t)

;;;; pdf-tools
(quelpa2 '(pdf-tools :url "https://github.com/vedang/pdf-tools"))
(require 'pdf-tools)
(add-hook 'doc-view-mode-hook (lambda () (pdf-tools-install) (pdf-tools-enable-minor-modes)))
(add-to-list 'magic-fallback-mode-alist '("%PDF" . pdf-view-mode))

;;;; php-mode
(quelpa2 '(php-mode :url "https://github.com/emacs-php/php-mode"))
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
(setq php-mode-coding-style 'symfony2
      php-template-compatibility nil)

(defun my-var_dump-die (start end)
  (interactive "r")
  (if mark-active
      (progn
        (goto-char end)
        (insert "));")
        (goto-char start)
        (insert "die(var_dump("))
    (insert "die(var_dump());")))

(quelpa2 '(ac-php :url "https://github.com/xcwen/ac-php"))
(require 'company-php)
(add-to-list 'company-backends 'company-ac-php-backend)
(define-key php-mode-map (kbd "M-.") 'ac-php-find-symbol-at-point)
(define-key php-mode-map (kbd "M-,") 'ac-php-location-stack-back)
(define-key php-mode-map (kbd "M-S-,") 'ac-php-location-stack-forward)
(define-key php-mode-map (kbd "H-i d") 'my-var_dump-die)

;;;; pos-tip
(quelpa2 '(pos-tip :url "https://github.com/emacsmirror/pos-tip"))
(require 'pos-tip)
(defun my-show-help ()
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

;;;; prog-mode
(require 'prog-mode)
(defun my-prog-mode-hook ()
  (setq show-trailing-whitespace 1)
  (prettify-symbols-mode 1))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;;;; projectile
(quelpa2 '(projectile :url "https://github.com/bbatsov/projectile"))
(require 'projectile)
(setq projectile-switch-project-action 'projectile-dired
      projectile-completion-system 'ido
      projectile-enable-caching t
      projectile-mode-line '(:eval (format " <%s>" (projectile-project-name))))

;;;; python
(quelpa2 '(pip-requirements :url "https://github.com/Wilfred/pip-requirements.el"))
(quelpa2 '(pyenv-mode :url "https://github.com/pythonic-emacs/pyenv-mode"))

(add-hook 'python-mode-hook 'eldoc-mode)

(require 'pip-requirements)
(require 'pyenv-mode)
(defun projectile-pyenv-mode-set ()
  (let ((name (projectile-project-name)))
    (if (member name (pyenv-mode-versions))
        (pyenv-mode-set name)
      (pyenv-mode-set "system"))))
(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

;;;; rainbow-mode
(quelpa2 '(rainbow-mode :url "https://github.com/emacsmirror/rainbow-mode"))
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'js-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'text-mode-hook 'rainbow-mode)

;;;; recentf
(require 'recentf)
(setq recentf-save-file (expand-file-name "~/.recentf"))
(recentf-mode 1)

;;;; restclient
(quelpa2 '(restclient :url "https://github.com/pashky/restclient.el"))

;;;; savehist
(require 'savehist)
(setq savehist-additional-variables
      '(kill-ring
        mark-ring
        global-mark-ring
        search-ring
        regexp-search-ring
        extended-command-history))
(savehist-mode 1)

;;;; saveplace
(require 'saveplace)
(setq-default save-place t)

;;;; sgml
(require 'sgml-mode)
(setq sgml-basic-offset 4)
(add-hook 'sgml-mode-hook 'sgml-electric-tag-pair-mode)

;;;; shr
(require 'shr)
(setq shr-use-fonts nil
      shr-color-visible-luminance-min 60)

;;;; skeleton
(require 'skeleton)
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
  "<audio controls=\"controls\" preload=\"none\" src=\"" str "\">" \n
  "<a href=\"" str "\">" str "</a>" \n
  "</audio>" \n
  "#+END_EXPORT" \n)

;;;; shell-switcher
(quelpa2 '(shell-switcher :url "https://github.com/DamienCassou/shell-switcher"))
(require 'shell-switcher)
(shell-switcher-mode 1)

;;;; smart-mode-line
(quelpa2 '(smart-mode-line :url "https://github.com/malabarba/smart-mode-line"))
(require 'smart-mode-line)
(setq sml/vc-mode-show-backend t
      sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'automatic)

;;;; systemd
(quelpa2 '(systemd :url "https://github.com/holomorph/systemd-mode"))
(require 'systemd)

;;;; term
(require 'term)
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
  (define-key term-raw-map (kbd "M-x") nil)
  (define-key term-raw-map (kbd "C-]") nil))
(add-hook 'term-mode-hook 'my-term-setup t)

;;;; tldr
(quelpa2 '(tldr :url "https://github.com/kuanyui/tldr.el"))
(require 'tldr)

;;;; yaml-mode
(quelpa2 '(yaml-mode :url "https://github.com/yoshiki/yaml-mode"))
(require 'yaml-mode)

;;;; visual-regexp
(quelpa2 '(visual-regexp :url "https://github.com/benma/visual-regexp.el"))
(require 'visual-regexp)

;;;; web-mode
(quelpa2 '(web-mode :url "https://github.com/fxbois/web-mode"))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-enable-engine-detection t)
(add-hook 'web-mode-hook (lambda () (set (make-local-variable 'electric-pair-mode) nil)))

;;;; writeroom-mode
(quelpa2 '(writeroom-mode :url "https://github.com/joostkremers/writeroom-mode"))
(require 'writeroom-mode)

;;;; which-key
(which-key-setup-minibuffer)
(which-key-mode 1)

;;;; xwidget
(require 'xwidget)
(define-key xwidget-webkit-mode-map (kbd "<mouse-4>") 'xwidget-webkit-scroll-down)
(define-key xwidget-webkit-mode-map (kbd "<mouse-5>") 'xwidget-webkit-scroll-up)
(define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-down)
(define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-up)
(define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
(add-hook 'window-configuration-change-hook (lambda ()
                                              (when (equal major-mode 'xwidget-webkit-mode)
                                                (xwidget-webkit-adjust-size-dispatch))))

(provide 'steckemacs)
;;; steckemacs.el ends here
