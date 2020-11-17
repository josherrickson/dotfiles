;;; Early Initialization
;==============================

;; Turn off mouse interface early to speed up launching time
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode
                              tooltip-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Display start-up time for debugging.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs init time was %s."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time))) )))

;;; Package Management
;==============================

;; Access to alternative packages
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))

;; Better organization of packages, and enables auto-installation
;; https://github.com/jwiegley/use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Older version protection
(when (< emacs-major-version 27)
  (package-initialize) ;; initialize packages (needed to load packages installed by M-x package-install; only needed in pre 27)
  (use-package package)) ;; This may not be needed.

;; Load color theme
(load-theme 'tsdh-dark t)  ;; The 't' says not to security check

;;; General Key Bindings
;==============================

;; Adjust bindgs for killing
(global-set-key (kbd "C-c M-w") 'kill-region)
(global-set-key (kbd "C-w")     'backward-kill-word)

;; Change from zap-to-char to zap-up-to-char (don't delete the char itself)
(autoload 'zap-up-to-char "misc" 'interactive)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Rotates between just-one-space, no-space, original spacing.
(bind-key "M-SPC" 'cycle-spacing)

;;; Settings
;==============================

;;;; Variables
;; Variables which are `buffer-local` (check with 5th line of C-h v <varname>) need setq-default, otherwise setq is fine.
(setq-default
 tab-width 2                          ;; default tab width is 2 spaces
 indent-tabs-mode nil                 ;; don't allow tabs (spaces instead)
 indicate-empty-lines t               ;; show end of file
 fill-column 70                       ;; column default width
)
(setq
 tab-always-indent 'complete          ;; some sort of smart-tabbing thing
 inhibit-startup-message t            ;; Don't show start-up message...
 initial-scratch-message nil          ;; ... or *scratch* message
 visible-bell t                       ;; no beeps on errors ...
 scroll-error-top-bottom t            ;; ... and don't error too soon
 ring-bell-function 'ignore           ;; no bells at all
 backup-inhibited t                   ;; disable backups ...
 make-backup-files nil                ;; ... and more backups ...
 auto-save-default nil                ;; ... and autosave ...
 auto-save-list-file-prefix nil       ;; ... and recovery
 sentence-end-double-space nil        ;; single space follows a period
 vc-follow-symlinks t                 ;; open symlinks to version controlled files
 echo-keystrokes 0.01                 ;; show commands instantly in minibuffer
 scroll-conservatively 5              ;; only scroll a bit when moving cursor
 read-buffer-completion-ignore-case t ;; don't worry about case in minibuffer
 read-file-name-completion-ignore-case t
 electric-pair-mode nil               ;; Ensure that electric-pairing isn't activated
)

;; Remove trailing whtiespace and lines upon saving
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(setq delete-trailing-lines t)

;; Needed when installing aspell by homebrew (may work without it if you install to /usr/bin/aspell)
(when (equal system-type 'darwin)
  (defvar ispell-program-name)
  (defvar ispell-extra-args)
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq ispell-extra-args '("--sug-mode=ultra"))) ;; faster but less accurate

;;;; Modes
(global-auto-revert-mode          t ) ;; revert buffers when changed
(transient-mark-mode              t ) ;; visual highlighting
(delete-selection-mode            t ) ;; typing replaces selected text
(size-indication-mode             t ) ;; include file size on mode line
(line-number-mode                 t ) ;; cursor position line ...
(column-number-mode               t ) ;; ... and column


;;;; Miscellaneous
;; Use a wider fill-column for text-only modes (e.g. not likely to be run side-by-side with terminal/output.
(add-hook 'emacs-lisp-mode-hook (lambda () (set-fill-column 150)))
(add-hook 'markdown-mode-hook   (lambda () (set-fill-column 150)))
(add-hook 'LaTeX-mode-hook      (lambda () (set-fill-column 150)))
(add-hook 'TeX-mode-hook        (lambda () (set-fill-column 150)))

(fset 'yes-or-no-p 'y-or-n-p)         ;; 'y or n' instead of 'yes or no'

;;; Functions
;==============================

;; Select the current word. http://xahlee.org/emacs/elisp_examples.html
(defun my/select-current-word ()
  "Select the word under cursor. 'word' here is considered any alphanumeric sequence with '_' or '-'."
  (interactive)
  (let (pt)
    (skip-chars-backward "-_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "-_A-Za-z0-9")
    (set-mark pt)))
(global-set-key (kbd "M-~") 'my/select-current-word)

;; Make C-a smarter. http://www.cs.utah.edu/~aek/code/init.el.html
(defun my/beginning-of-line-dynamic ()
  "Jumps to the beginning of text on line. If already there, goes to the true beginning of the line (before space.)"
  (interactive)
  (let ((cur (point)))
    (beginning-of-line-text)
    (when (= cur (point))
      (beginning-of-line))))
(global-set-key (kbd "C-a") 'my/beginning-of-line-dynamic)

;; GUI Emacs on Mac doesn't respect system PATH, this syncs them on launch.
;; http://stackoverflow.com/questions/2266905/emacs-is-ignoring-my-path-when-it-runs-a-compile-command
(defun my/set-exec-path-from-shell-PATH ()
  "Sets the exec-path to PATH from the system."
  (let ((path-from-shell
         (replace-regexp-in-string "[[:space:]\n]*$" ""
                                   (shell-command-to-string
                                    "$SHELL -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (my/set-exec-path-from-shell-PATH))

;; Does align-regexp over ALL entries in the line instead of just the first http://www.emacswiki.org/emacs/AlignCommands
(defun align-all (start end regexp)
  "Aligns on the same regexp as often as it appears in a line."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

;; Makes yanked text available in the os clipboard. The sources also has a function to paste from clipboard, but Cmd-V works fine for that and I don't
;; want to lose my yank queue.  https://gist.github.com/the-kenny/267162
(defun my/copy-to-clipboard (text &optional push)
  "Copy the selection to OS X clipboard."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(when (equal system-type 'darwin)
  (setq interprogram-cut-function 'my/copy-to-clipboard))

;; https://stackoverflow.com/a/25792294
(defun my/new-empty-frame ()
  "Open a new frame with a buffer named untitled<N>. The buffer is not associated with a file."
  (interactive)
  (switch-to-buffer-other-frame (generate-new-buffer "untitled")))
(global-set-key (kbd "C-C n") 'my/new-empty-frame)

;; Inserts stata do chunk
(defun my/insert-stata-dyndoc-chucnk ()
  "Inserts the tags for a Stata dyndoc chunk."
  (interactive)
  (insert "~~~~\n<<dd_do>>\n")
  (save-excursion
    (insert "\n<</dd_do>>\n~~~~")))
(add-hook 'markdown-mode-hook
          (lambda () (local-set-key (kbd "C-c C-s d") 'my/insert-stata-dyndoc-chucnk)))

;; The inverse of fill-paragraph, http://pages.sachachua.com/.emacs.d/Sacha.html#org3dd06d8
(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(bind-key "M-Q" 'my/unfill-paragraph)


;;; External Packages
;==============================

;;;; diminish
;; Using :diminish in use-packge hides minor-modes from mode line
(use-package diminish
  :ensure t)

;;;; avy
;; Jump to a specified location. Replaces ace-jump-mode
(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1))
  :config
  (setq avy-background t)) ;; gray text other then matches


;;;; multiple-cursors
;; Selecting and editing repeated words
(use-package multiple-cursors
  :ensure t
  :bind (;; ("C-c M-c" . mc/edit-lines)
         ("M-!" . mc/mark-next-like-this)
         ("M-@" . mc/mark-previous-like-this)
         ("M-#" . mc/mark-all-like-this)))

;;;; emacs speaks statistics
;; For R and R files
;; ess-stata has been removed (https://github.com/emacs-ess/ESS/issues/1033)
(use-package ess
  :ensure t
  :defer t
  :config
  (setq ess-ask-for-ess-directory nil        ;; just run R wherever the file lives
        ess-history-file nil                 ;; don't save history
        ess-eval-visibly-p nil               ;; when running R, don't show code, just output (greatly speeds running)
        inferior-R-args
          "--no-restore --no-save --quiet"   ;; R startup conditions
        ess-style 'RStudio                   ;; better indenting
        comint-scroll-to-bottom-on-input   t ;; force ESS to scroll R to the bottom after running code
        comintq-scroll-to-bottom-on-output t
        comint-scroll-show-maximum-output  t
        comint-move-point-for-output       t))

;; poly-mode for R
(use-package poly-R
  :ensure t
  :defer t
  :config
  (setq markdown-enable-math t)) ;; Highlight latex math snippets

;;;; Auctex
(use-package auctex
  :ensure t
  :defer t
  :mode ("\\.Rnw$" . LaTeX-mode)
  :init
  (add-hook 'LaTeX-mode-hook (lambda () ;; add latexmk to C-c C-c list
                               (push
                                '("Latexmk" "latexmk -pdf -pvc %s" TeX-run-command nil t
                                  :help "Run Latexmk on file")
                                TeX-command-list)))
  :config
  (setq
   Tex-engine 'default                       ;; XeTeX causes issues with tikz
   ;TeX-engine 'xetex                        ;; use XeTeX
   ;TeX-engine 'pdflatex
   TeX-PDF-mode t                            ;; PDF instead of dvi
   TeX-newline-function 'newline-and-indent) ;; autoindent in TeX-mode

  ;; For multi-file documents, use `C-c _` to add master information to the file.  Second command removes automation
  (setq TeX-master nil
        TeX-one-master "<none>")

  ;; OS X use `open`
  (setq TeX-view-program-selection '((output-pdf "open")))
  (setq TeX-view-program-list '(("open" "open %o"))))

;;;; ivy, counsel, swiper
;; Enables better matching in minibuffer (e.g. find file, switch buffer)
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  :diminish ivy-mode
  :demand t
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  ;; Ignore order of space-separated items
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  :bind (("C-c C-r" . ivy-resume)))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line) ;; suggested in ivy-rich documentation
  (setq ivy-rich-path-style 'abbrev)) ;; show full path (with ~ abbrev) instead of relative

;; Enables ivy in more locations
(use-package counsel
  :ensure t
  :init
  (counsel-mode 1)
  :diminish counsel-mode)

;; Uses ivy for searching
(use-package swiper
  :ensure t
  :config
  (setq swiper-action-recenter t) ;; after returning, center at line
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;;;; magit
;; Git access inside emacs
(use-package magit
  :ensure t
  :init
  ;; Binding this in init and using defer ensures that a) magit is only loaded on demand (which
  ;;   speeds up emacs startup) and b) makes C-x g work even if magit-status isn't loaded (so
  ;;    calling C-x g loads magit if needed).
  (bind-key "C-x g" 'magit-status)
  :defer t
  :config
  (setq magit-refresh-verbose t)) ;; Give timing information in *Messages* for debugging slowness

;;;; fish-mode
;; Mode for fishshell scripts
(use-package fish-mode
  :ensure t
  :defer t)

;;; Internal Packages
;==============================

;;;; recentf
(use-package recentf
  :init
  (recentf-mode t)
  :bind (("C-x M-f" . set-fill-column) ;; to make room
         ("C-x f" . recentf-open-files))
  :config
  (setq recentf-exclude '("\\cookies\\'"            ;; don't list these files in recentf
                          "\\archive-contents\\'"
                          "\\.ido.last\\'")))
;;;; tramp
(use-package tramp
  :defer t
  :config
  (setq password-cache-expiry 3600)) ;; cache passwords in tramp for 1 hr

;;;; org
(use-package org
  :defer t
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-hide-leading-stars t)) ;; In subsections (e.g. ***) hides all but the last *

;;;; dired
(use-package dired
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t                            ;; On OSX, ls isn't gnu-ls so causes some issues.
          insert-directory-program "/usr/local/bin/gls")) ;; Install `coreutils` via homebrew, this enables it
  ;; dired creates a new buffer for each directory. This encourages dired to reuse the same buffer.
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)                         ;; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))) ;; was dired-up-directory
  (put 'dired-find-alternate-file 'disabled nil)
  :custom
  (dired-listing-switches "-AFBhl")) ;; switches passed to ls

;;;; display-line-numbers
(use-package display-line-numbers
  :init
  (global-display-line-numbers-mode t)
  :config
  (setq display-line-numbers-grow-only t)) ;; don't shrink line-number space when looking at fewer digits

;;;; paren
(use-package paren
  :init
  (show-paren-mode t)
  :config
  (setq show-paren-delay 0)) ;; don't delay showing parens

;;; Custom file
;==============================

;; Rather that letting emacs stick custom-set-variables in here, place it in a different file that is
;; not under version control.
;; https://old.reddit.com/r/emacs/comments/67pzh5/using_customsetvariables_programmatically/dgsxvm3/

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; This page break is to ensure no local variables are set
;; https://stackoverflow.com/questions/18099531/how-to-ignore-a-local-variables-list-in-text

