;; add melpa as source
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defun my/compile-cfg ()
  "Compile config if config is saved."
  (when (equal (buffer-file-name)
	       (expand-file-name (concat user-emacs-directory "init.el")))
    (byte-compile-file (concat user-emacs-directory "init.el"))))

(add-hook 'after-save-hook 'my/compile-cfg)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use emacs as server
(server-start)

(require 'use-package)

;; PACKAGE MANAGEMENT
(use-package package-utils
  :ensure t)

;; vim emulation
(use-package evil
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "g f") 'imenu)
  (evil-mode 1))

(use-package evil-mc
    :ensure t
    :config
    (global-evil-mc-mode 1))

(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "fd")
  (setq-default evil-escape-delay 0.2)
  (evil-escape-mode))

(use-package evil-leader
      :ensure t
      :config
      (global-evil-leader-mode)
      (evil-leader/set-leader "<SPC>")
      (evil-leader/set-key
		"k"  'kill-this-buffer
		"e" 'find-file
		"b" 'switch-to-buffer))

(use-package evil-org
      :ensure t
      :after org
      :config
      (add-hook 'org-mode-hook 'evil-org-mode)
      (add-hook 'evil-org-mode-hook
		(lambda ()
		  (evil-org-set-key-theme))))

(use-package evil-surround
      :ensure t
      :config
      (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
      :ensure t
      :config
      (evil-leader/set-key
	"ci" 'evilnc-comment-or-uncomment-lines
	"cl" 'evilnc-quick-comment-or-uncomment-to-the-line
	"ll" 'evilnc-quick-comment-or-uncomment-to-the-line
	"cc" 'evilnc-copy-and-comment-lines
	"cp" 'evilnc-comment-or-uncomment-paragraphs
	"cr" 'comment-or-uncomment-region))

(use-package evil-avy
      :ensure t
      :config
      (evil-avy-mode))

(use-package evil-magit
      :ensure t
      :config
      (add-to-list 'evil-insert-state-modes 'magit-log-edit-mode)
      (evil-leader/set-key
	"s" 'magit-status))

(use-package evil-exchange
      :ensure t
      :config
      (evil-exchange-install))

(use-package evil-lion
      :ensure t
      :config
      (evil-lion-mode))

;; file to mode mapping
(add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.psgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t\\'"  . cperl-mode))
(add-to-list 'auto-mode-alist '("cpanfile" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.*tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.*tml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.*tt\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.*tt2\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.nqp\\'" . perl6-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

;; anser with "y" and "n" instead of "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; save autosaves into another directory
(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/"))

(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

;; some usefull defaults
(setq indent-tabs-mode nil
      auto-revert-interval 1
      inhibit-startup-message t
      initial-scratch-message nil 
      ring-bell-function 'ignore
      set-language-environment "UTF-8"
      abbrev-file-name "~/.emacs.d/.abbrev")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

;; show possible keybindings
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;; LAYOUT

(dolist (mode
	 '(tool-bar-mode                ; No toolbars, more room for text
	   scroll-bar-mode              ; No scroll bars either
	   blink-cursor-mode            ; The blinking cursor gets old
	   menu-bar-mode))              ; No menu-bar
  (funcall mode 0))

(use-package atom-dark-theme
  :ensure t
  :config
  (load-theme 'atom-dark 'NO-CONFIRM))

;; PROJECT MANAGEMENT
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (evil-leader/set-key
    "pf" 'counsel-projectile-find-file
    "pp" 'counsel-projectile-switch-project
    "ps" 'ag-project
    "pc" 'counsel-projectile-compile-project
    "pt" 'counsel-projectile-test-project))

;; MINIBUFFER STUFF
(use-package ivy
  :ensure t
  ;; :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t
	ivy-height 30
	recentf-max-saved-items 100  ; Show more recent files
	ivy-initial-inputs-alist nil ; no regexp by default
	magit-completing-read-function 'ivy-completing-read
	completion-in-region-function 'ivy-completion-in-region
	ivy-re-builders-alist '((t   . ivy--regex-ignore-order))))

(use-package ivy-rich
    :ensure t
    :config
    (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
    (setq ivy-virtual-abbreviate 'full
	  ivy-rich-switch-buffer-align-virtual-buffer t))

(use-package swiper
  :ensure t
  :config
  :bind (("M-o" . swiper)))

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (setq smex-completion-method 'ivy))

(use-package counsel
  :ensure t
  :bind (("C-c h" . counsel-descbinds)
	 ("M-x" . counsel-M-x)
	 ("M-y" . counsel-yank-pop)
	 ("M-p" . ag-project)
	 ("C-c l" . counsel-locate)
	 ("C-x C-f" . counsel-find-file))
  :config
  (evil-leader/set-key
	"l"  'counsel-locate)
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-on)))

;; COMPLETION
(use-package company-flx
    :ensure t
    :config
    (with-eval-after-load 'company
      (add-hook 'company-mode-hook (lambda ()
				     (add-to-list 'company-backends 'company-capf)))
      (company-flx-mode +1)))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
	company-echo-delay 0
	company-dabbrev-downcase nil
	company-minimum-prefix-length 1
	company-selection-wrap-around t
	company-transformers '(company-sort-by-occurrence
			       company-sort-by-backend-importance))
  (add-to-list 'company-backends 'company-ispell)
  (global-company-mode)

  ;; adapt completion buffer to theme
  (require 'color)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package ycmd
  :ensure t
  :config
  (set-variable 'ycmd-global-config "/usr/share/vim/vimfiles/third_party/ycmd/ycmd/default_settings.json")
  (set-variable 'ycmd-server-command '("python2" "/usr/share/vim/vimfiles/third_party/ycmd/ycmd"))
  (add-hook 'after-init-hook #'global-ycmd-mode)

  (evil-define-key 'normal python-mode-map (kbd "g d") 'ycmd-goto)
  (evil-define-key 'normal python-mode-map (kbd "g h") 'ycmd-show-documentation)

  ;; (evil-define-key 'normal js2-mode-map (kbd "g d") 'ycmd-goto)
  (evil-define-key 'normal js2-mode-map (kbd "g h") 'ycmd-show-documentation)
  (evil-leader/set-key-for-mode 'js2-mode "r" 'ycmd-refactor-rename)

  (use-package flycheck-ycmd
    :ensure t
    :config
    (flycheck-ycmd-setup))

  (use-package company-ycmd
    :ensure t
    :config
    (company-ycmd-setup)))

;; LINTING
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;; spell checking
(use-package flyspell
  :ensure t
  :config
  (setq flyspell-issue-message-flag nil)) ; performance

(use-package langtool
  :ensure t
  :config
  (setq langtool-java-classpath
	"/usr/share/languagetool:/usr/share/java/languagetool/*"))

;; CUSTOM FUNCTIONS
(defun my/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))

(defun my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer) (buffer-list))))

(defun my/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

(defun my/random-sort-lines (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
	  ((inhibit-field-text-motion t))
	(sort-subr nil 'forward-line 'end-of-line nil nil
		   (lambda (s1 s2) (eq (random 2) 0)))))))

;; JAVASCRIPT
(use-package skewer-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

(use-package js2-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package company-tern
  :ensure t
  :config
  ;; (setq tern-command '("node" "/usr/lib/node_modules/tern/bin/tern"))
  (evil-define-key 'normal js2-mode-map (kbd "g d") 'tern-find-definition)
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook (lambda () (tern-mode))))

(use-package nodejs-repl
  :ensure t
  :config
  (add-hook 'js-mode-hook
          (lambda ()
            (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
            (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
            (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
            (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))))

;; WEB HTML CSS
(use-package web-mode
  :ensure t
  :defer t
  :config
  (setq-default web-mode-enable-auto-pairing t
		web-mode-enable-auto-opening t
		web-mode-enable-auto-indentation t
		web-mode-enable-block-face t
		web-mode-enable-part-face t
		web-mode-enable-comment-keywords t
		web-mode-enable-css-colorization t
		web-mode-enable-current-element-highlight t
		web-mode-enable-heredoc-fontification t
		web-mode-enable-engine-detection t
		web-mode-markup-indent-offset 2
		web-mode-css-indent-offset 2
		web-mode-code-indent-offset 2
		web-mode-style-padding 2
		web-mode-script-padding 2
		web-mode-block-padding 0
		web-mode-comment-style 2)
  (custom-set-faces
   '(web-mode-html-tag-face
     ((t (:foreground "#729fcf"))))
   '(web-mode-html-tag-bracket-face
     ((t (:foreground "#FFE84B"))))
   '(web-mode-current-element-highlight-face
     ((t (:foreground "#FF8A4B"))))
   '(web-mode-current-element-highlight-face
     ((t (:background "#000000"
		      :foreground "#FF8A4B")))))
  (define-key helm-map (kbd "C-c C-e") 'web-mode-element-close)
  (add-hook 'web-mode-hook
	    '(lambda ()
	       (local-set-key (kbd "RET") 'newline-and-indent)
	       (setq smartparens-mode nil)))
  (add-hook 'web-mode-before-auto-complete-hooks
	    '(lambda ()
	       (let ((web-mode-cur-language
		      (web-mode-language-at-pos)))
		 (if (string= web-mode-cur-language "php")
		     (yas-activate-extra-mode 'php-mode)
		   (yas-deactivate-extra-mode 'php-mode))
		 (if (string= web-mode-cur-language "css")
		     (setq emmet-use-css-transform t)
		   (setq emmet-use-css-transform nil))))))

(use-package company-web
    :ensure t
    :defer t
    :config
    (add-to-list 'company-backends '(company-web-html))
    (add-to-list 'company-backends '(company-web-jade)))

;; JSON
(use-package json-mode
  :ensure t)

;; TYPESCRIPT
(defun setup-tide-mode ()
  (tide-setup)
  (tide-hl-identifier-mode +1))

(use-package tide
  :ensure t
  :config
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

;; PHP
(use-package php-mode
  :ensure t)

;; PERL
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy-sweet -q" nil t)))

(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
		  (perltidy-region)))

(defalias 'perl-mode 'cperl-mode)
(use-package cperl-mode
  :ensure t
  :bind (("C-c p" . perltidy-region)
	 ("C-c f" . perltidy-defun))
  :config
  (custom-set-faces
   '(cperl-hash-face  ((t)))
   '(cperl-array-face ((t))))
  (setq cperl-close-paren-offset -4
	cperl-continued-statement-offset 4
	cperl-indent-level 4
	cperl-tab-always-indent t
	cperl-indent-parens-as-block t))

;; checker that also integrates carton modules
(flycheck-define-checker my-perl
  "A Perl syntax checker using the Perl interpreter."
  :command ("/usr/bin/perl" "-w" "-c"
	    (eval (let ((options '()))
		    (when (projectile-project-p)
		      (push (concat "-I" (projectile-project-root)) options)
		      (push (concat "-I" (projectile-expand-root "lib")) options)
		      (when (projectile-verify-file "cpanfile")
			(push (concat "-I" (projectile-expand-root "local/lib/perl5")) options))
		      options)))
	    source)
  :error-patterns ((error line-start (minimal-match (message)) " at " (file-name) " line " line (or "." (and ", " (zero-or-more not-newline))) line-end))
  :modes (perl-mode cperl-mode)
  :next-checkers (my-perl-perlcritic))

(flycheck-define-checker my-perl-perlcritic
  "A Perl syntax checker using Perl::Critic."
  :command ("perlcritic" "--no-color" "--verbose" "%f:%l:%c:%s:%m (%e)\n"
	    (option "--severity" flycheck-perlcritic-verbosity flycheck-option-int)
	    source-original)
  :error-patterns ((info    line-start (file-name) ":" line ":" column ":" (any "1")   ":" (message) line-end)
		   (warning line-start (file-name) ":" line ":" column ":" (any "234") ":" (message) line-end)
		   (error   line-start (file-name) ":" line ":" column ":" (any "5")   ":" (message) line-end))
  :modes (cperl-mode perl-mode))

(add-to-list 'flycheck-checkers 'my-perl)
(add-to-list 'flycheck-checkers 'my-perl-perlcritic)

(defun my-cperl-mode-hook ()
  "Hook function for `cperl-mode'."
  (helm-perldoc:carton-setup)
  (flycheck-select-checker 'my-perl))
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

(use-package helm-perldoc
  :ensure t)

;; PERL6

(use-package perl6-mode
  :ensure t)

;; (use-package flycheck-perl6
;;   :ensure t)

;; SEARCHING AND NAVIGATION
(use-package ag
  :ensure t
  :bind (("M-s" . counsel-projectile-ag)))

(use-package ace-window
  :bind(("C-x o" . ace-window))
  :ensure t
  :config
  (evil-leader/set-key "TAB" 'ace-window))

(use-package dumb-jump
  :ensure t
  :config
  (eval-after-load 'evil
    (eval-after-load 'dumb-jump
      (defadvice dumb-jump-go (before dotemacs activate)
	(evil--jumps-push))))
  (define-key evil-normal-state-map (kbd "g D") 'dumb-jump-go))

;; LATEX
(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(setq latex-run-command "pdflatex")
(TeX-PDF-mode t)
(evil-leader/set-key-for-mode 'latex-mode "x" 'TeX-command-master)

;; C/C++
(setq c-basic-offset 4)

(use-package rtags
  :ensure t
  :config
  (evil-define-key 'normal c-mode-map (kbd "g d") 'rtags-find-symbol-at-point)
  (evil-define-key 'normal c++-mode-map (kbd "g d") 'rtags-find-symbol-at-point)
  (evil-define-key 'normal c-mode-map (kbd "g t") 'rtags-symbol-type)
  (evil-define-key 'normal c++-mode-map (kbd "g t") 'rtags-symbol-type)
  (evil-define-key 'normal c-mode-map (kbd "g r") 'rtags-find-references-at-point)
  (evil-define-key 'normal c++-mode-map (kbd "g r") 'rtags-find-references-at-point)
  (evil-leader/set-key-for-mode 'c++-mode "r" 'rtags-rename-symbol)
  (evil-leader/set-key-for-mode 'c++-mode "a" 'rtags-find-references-at-point)
  (use-package flycheck-rtags
    :ensure t)
  (require 'flycheck-rtags))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode))

(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package cmake-mode
  :ensure t)

(use-package cmake-ide
  :ensure t
  :config
  (require 'rtags)
  (cmake-ide-setup))

(defun my/set-cmake-ide-build-dir()
  "Set build dir with CompileCommands.json"
  (interactive)
  (let ((dir (read-directory-name "Build dir:")))
    (setq cmake-ide-build-dir dir)))

(use-package disaster
  :ensure t
  :config
  (define-key c-mode-base-map (kbd "C-c d") 'disaster))

(use-package clang-format
  :ensure t
  :config)

;; GO
(use-package go-mode
  :ensure t
  :config
  (evil-define-key 'normal go-mode-map (kbd "g d") 'godef-jump))

;; ;; completion
;; (use-package company-go
;;   :ensure t
;;   :config
;;   (add-hook 'go-mode-hook
;; 	    (lambda ()
;; 	      (set (make-local-variable 'company-backends) '(company-go)))))

;; ORG
(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :config
  (evil-leader/set-key-for-mode 'org-mode
    "s" 'org-schedule
    "r" 'org-toggle-latex-fragment
    "d" 'org-deadline
    "D" 'my/org-todo-with-date
    "$" 'org-archive-subtree
    "c" 'org-set-category-property
    "P" 'org-set-property
    "t" 'org-set-tags)

  (evil-leader/set-key
    "oa" 'org-agenda
    "oc" 'org-capture)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t)
     ;; other languages..
     ))

  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (setq org-modules (append org-modules '(org-habit)))
  (setq org-habit-show-habits-only-for-today t)
  (setq org-habit-graph-column 80)
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (setq org-todo-keyword-faces
	(quote (("TODO" :foreground "red" :weight bold)
		("NEXT" :foreground "blue" :weight bold)
		("DONE" :foreground "forest green" :weight bold)
		("WAITING" :foreground "orange" :weight bold)
		("HOLD" :foreground "magenta" :weight bold)
		("CANCELLED" :foreground "forest green" :weight bold)
		("MEETING" :foreground "forest green" :weight bold)
		("PHONE" :foreground "forest green" :weight bold))))
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-agenda-files '("~/Dropbox/org"))
  (setq org-default-notes-file (concat org-directory "~/Dropbox/org/notes.org"))
  (setq org-src-tab-acts-natively t)
  (setq org-agenda-custom-commands
	'(
	  ("h" "Daily habits"
	   ((agenda ""))
	   ((org-agenda-show-log t)
	    (org-agenda-ndays 7)
	    (org-agenda-log-mode-items '(state))
	    (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
	  ("c" . "My Custom Agendas")
	  ("cu" "Unscheduled TODO"
	   ((todo ""
		  ((org-agenda-overriding-header "\nUnscheduled TODO")
		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
	   nil
	   nil)))
  (setq org-capture-templates
	'(("t" "Todo" entry (file "~/Dropbox/org/index.org" "")
	   "* TODO %?\n"))))

(defun my/org-todo-with-date (&optional arg)
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
	     (my-current-time (org-read-date t t nil "when:" nil nil nil))
	     ((symbol-function #'org-current-effective-time)
	      #'(lambda () my-current-time)))
    (org-todo arg)))

(use-package org-bullets
  :ensure t
  :config
  (setq org-ellipsis " …")
  (setq org-bullets-bullet-list '("•"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-projectile
  :bind (("C-c n p" . org-projectile:project-todo-completing-read)
	 ("C-c c" . org-capture))
  :ensure t
  :config
  (org-projectile:per-repo)
  (setq org-projectile:per-repo-filename "TODO.org")
  (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files))))

(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
     background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
	   (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
	org-html-head-extra
	(format
	 "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
	 my-pre-bg my-pre-fg))))))
(add-hook 'my/org-export-before-processing-hook #'my/org-inline-css-hook)

;; VCS
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-paint-whitespace t)
  (evil-leader/set-key
    "ms" 'magit-status
    "mb" 'magit-log-buffer-file
    "ml" 'magit-log))

;; (use-package magithub
;;   :after magit
;;   :ensure t
;;   :config (magithub-feature-autoinject t))

(use-package git-messenger
  :ensure t
  :config
  (evil-leader/set-key
    "mp" 'git-messenger:popup-message)
  (setq git-messenger:show-detail t)
  (setq git-messenger:use-magit-popup t))

(use-package git-gutter+
  :ensure t)

;; ADDITIONAL BINDINGS
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(global-set-key (kbd "s-m") 'toggle-maximize-buffer)

;; SCALA
(use-package sbt-mode
  :ensure t)

(use-package scala-mode
  :ensure t
  :config
  (setq scala-indent:step 4))

(use-package ensime
  :ensure t)

;; YAML
(use-package yaml-mode
  :ensure t)

;; not in repo
(load-file "~/.emacs.d/private.el")

(defun my/xah-run-current-file ()
  (interactive)
  (let (
	(-suffix-map
	 `(
	   ("php" . "php")
	   ("pl6" . "perl6")
	   ("pl" . "/home/ben/perl5/perlbrew/perls/perl-5.24.0/bin/perl")
	   ("pm" . "/home/ben/perl5/perlbrew/perls/perl-5.24.0/bin/perl")
	   ("py" . "python3")
	   ("rb" . "ruby")
	   ("go" . "go run")
	   ("js" . "node") ; node.js
	   ("sh" . "bash")
	   ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
	   ("rkt" . "racket")
	   ("ml" . "ocaml")
	   ("vbs" . "cscript")
	   ("tex" . "pdflatex")
	   ("latex" . "pdflatex")
	   ("java" . "javac")))
	-fname
	-fSuffix
	-prog-name
	-cmd-str)

    (when (null (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))

    (setq -fname (buffer-file-name))
    (setq -fSuffix (file-name-extension -fname))
    (setq -prog-name (cdr (assoc -fSuffix -suffix-map)))
    (setq -cmd-str (concat -prog-name " \""   -fname "\""))

    (cond
     ((string-equal -fSuffix "el") (load -fname))
     ((string-equal -fSuffix "java")
      (progn
	(shell-command -cmd-str "*xah-run-current-file output*" )
	(shell-command
	 (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname))))))
     (t (if -prog-name
	    (progn
	      (message "Running…")
	      (shell-command -cmd-str "*xah-run-current-file output*" ))
	  (message "No recognized program file suffix for this file."))))))

(global-set-key (kbd "<f5>") 'my/xah-run-current-file)

;; RUST
(use-package rust-mode
  :ensure t
  :config
  (evil-define-key 'normal rust-mode-map (kbd "g d") 'racer-find-definition)
  (add-hook 'rust-mode-hook #'racer-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :ensure t)

;; EDITORCONFIG
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; jumping around in files


;; tags
(use-package ggtags
  :ensure t)

;; irc
(use-package erc
  :ensure t
  :config
  (require 'erc-services)
  (erc-services-mode 1)
  (add-to-list 'erc-modules 'notifications)
  (setq erc-prompt-for-nickserv-password t))


;; highlighting current variable
(use-package highlight-symbol
  :ensure t)

;; smart auto parens for lisp
(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (paredit-mode t))))

;; some tramp stuff
(setq tramp-default-method "ssh")

(use-package helm-tramp
  :ensure t)

(defun my/shell-set-hook ()
  (when (file-remote-p (buffer-file-name))
    (let ((vec (tramp-dissect-file-name (buffer-file-name))))
     ;; Please change "some-hostname" to your remote hostname
      (when (string-match-p "some-hostname" (tramp-file-name-host vec))
        (setq-local shell-file-name "/usr/local/bin/bash")))))

(add-hook 'find-file-hook #'my/shell-set-hook)

;; MARKDOWN
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; DOCUMENTATION LOOKUP
(use-package counsel-dash
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("/home/ben/Dropbox/org/habbits.org" "/home/ben/Dropbox/org/index.org" "~/workspace/perl6/Projective/TODO.org" "~/workspace/elisp/telmacs/TODO.org" "~/workspace/perl5/EMP/TODO.org" "~/workspace/uni/inside_my_iphone/TODO.org")))
 '(package-selected-packages
   (quote
    (xref-js2 js2-refactor restclient nodejs-repl js-auto-beautify flymake-lua company-lua lua-mode calfw-ical calfw-org flycheck-ycmd company-ycmd ycmd www-synonyms paste-of-code gitter tree-mode ob-ipython julia-shell flycheck-julia magithub package-lint test-simple erlang swift-mode clojure-mode slack request evil-exchange langtool evil-mc ess company-quickhelp elpy comapny-jedi company-jedi evil-avy ivy-hydra hydra evil-org fireplace evil-lion ztree yaml-mode which-key web-mode use-package tide smex skewer-mode racer python-mode php-mode perl6-mode package-utils ox-reveal org-projectile org-evil org-bullets mu4e-alert latex-preview-pane json-mode ivy-rich highlight-symbol helm-perldoc google-translate git-messenger git-gutter+ ggtags flycheck-rust flycheck-rtags flycheck-perl6 expand-region evil-surround evil-snipe evil-smartparens evil-nerd-commenter evil-mu4e evil-magit evil-leader evil-escape evil-cleverparens ensime elfeed-org elfeed-goodies editorconfig dumb-jump disaster counsel-projectile company-web company-irony company-go company-flx company-auctex company-anaconda cmake-mode cmake-ide clang-format calfw atom-dark-theme ag ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#34383c"))))
 '(company-scrollbar-fg ((t (:background "#282b2e"))))
 '(company-tooltip ((t (:inherit default :background "#212426"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(cperl-array-face ((t)))
 '(cperl-hash-face ((t)))
 '(web-mode-current-element-highlight-face ((t (:foreground "#FF8A4B"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "#FFE84B"))))
 '(web-mode-html-tag-face ((t (:foreground "#729fcf")))))
