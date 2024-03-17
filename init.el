;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Early Initialization ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                              (time-subtract after-init-time
                                             before-init-time))) )))

;; Don't bother trying to dynamically resize
;; https://tony-zorman.com/posts/2022-10-22-emacs-potpourri.html
(setq frame-inhibit-implied-resize t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Package Management ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Access to alternative packages
(require 'package)
(add-to-list 'package-archives '("gnu"   .
                                 "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" .
                                 "https://melpa.org/packages/"))

;; Better organization of packages, and enables auto-installation
;; https://github.com/jwiegley/use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Older version protection
(when (< emacs-major-version 27)
  (package-initialize) ;; initialize packages (needed to load packages
                       ;; installed by M-x package-install; only
                       ;; needed in pre 27)
  (use-package package)) ;; This may not be needed.

;;;;;;;;;;;;;;;;;
;;;;; Theme ;;;;;
;;;;;;;;;;;;;;;;;

;;;; This allows switching between light-mode and dark-mode. Load any themes
;;;; libraries needed, and then update definitions of `my-light-theme` and
;;;; `my-dark-theme`.
;;;;
;;;; On Mac, for GUI emacs, follow Dark mode status and change as appropriate.
;;;; For terminal, at launch, choose theme based upon current Dark Mode status.
;;;; You can rerun `M-x reapply-theme` to have it reevaluate (e.g. check for
;;;; Dark Mode).

(use-package solarized-theme
  :ensure t)

(setq my-light-theme 'solarized-light)
(setq my-dark-theme 'solarized-dark)

;;;; Do not change anything below here, just update the two variables above.


;; If not on a mac, just load a theme
(when (not (eq system-type 'darwin))
    (load-theme my-dark-theme t))

;; On a Mac
(when (eq system-type 'darwin)
  ;; Check if in terminal or GUI
  (if window-system
      ;; GUI Emacs
      (progn
        (defun my/apply-theme (appearance)
          "Load theme, taking current system APPEARANCE into consideration."
          (mapc #'disable-theme custom-enabled-themes)
          (pcase appearance
            ('light (load-theme my-light-theme t))
            ('dark  (load-theme my-dark-theme  t))))
        (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))
    ;; Terminal Emacs
    (progn
      (defun reapply-theme ()
        "Check status of Dark Mode and apply appropriate pre-defined theme."
        (interactive)

        (let ((cmd-output (shell-command-to-string "defaults read -g AppleInterfaceStyle")))
          (if (string-match-p "Dark" cmd-output)
              (load-theme my-dark-theme t)
            (load-theme my-light-theme t))))
      (reapply-theme))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Internal Packages ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; emacs
;; Anything which is defined in C Source Code, or most built-in
;; packges belong here
(use-package emacs
  ;; adjust bindings for killing
  :bind (("C-c M-w" . kill-region)
         ("C-w"     . backward-kill-word))
  ;; Move from zap-to-char to zap-up-to-char (don't delete char itself)
  :bind ("M-z"      . zap-up-to-char)
  ;; Rotates between just-one-space, no-space, original spacing.
  :bind ("M-SPC"    . cycle-spacing) ;; default in 29.1
  ;; Switch capitalization to dwim (Do What I mean). If a region is
  ;; selected, call e.g. upcase-region, otherwise call upcase-word.
  :bind (("M-u"     . upcase-dwim)
         ("M-c"     . capitalize-dwim)
         ("M-l"     . downcase-dwim))
  ;; Run a make command. This replaces send mail.
  ;; The first runs `compile-command` without interaction (make -k), the
  ;; second prompts for the command.
  :bind (("C-c C-m" . (lambda () (interactive)
                        (compile compile-command)))
         ("C-x C-m C-m" . compile))
  :bind ("C-c c" . comment-or-uncomment-region)
  :bind ("C-c n" . scratch-buffer) ;; switch to or create a scratch buffer
  :bind ("C-x <up>" . windmove-up)
  :bind ("C-x <down>" . windmove-down)
  :bind ("C-x <left>" . windmove-left)
  :bind ("C-x <right>" . windmove-right)
  :config
;; Variables which are `buffer-local` (check with C-h v <varname>) need
;; setq-default, otherwise setq is fine.
  (setq-default
   tab-width 2                           ;; tab width to 2 spaces
   indent-tabs-mode nil                  ;; spaces instead of tabs
   indicate-empty-lines t                ;; show end of file
   fill-column 80)                       ;; column default width
  (setq
    inhibit-startup-message t            ;; No start-up message...
    initial-scratch-message nil          ;; ... or *scratch* message
    tab-always-indent 'complete          ;; indent if possible,
                                         ;; otherwise complete-at-point
    visible-bell t                       ;; no beeps on errors ...
    scroll-error-top-bottom t            ;; ... and no error on scroll
    ring-bell-function 'ignore           ;; no bells at all
    backup-inhibited t                   ;; disable backups ...
    make-backup-files nil                ;; ... and more backups ...
    auto-save-default nil                ;; ... and autosave ...
    auto-save-list-file-prefix nil       ;; ... and recovery
    sentence-end-double-space nil        ;; single space after period
    vc-follow-symlinks t                 ;; open symlinks to vc files
    echo-keystrokes 0.01                 ;; instant typing in
                                         ;; minibuffer
    scroll-conservatively 100            ;; one line scrolling
    read-buffer-completion-ignore-case t ;; don't worry about case in
                                         ;; minibuffer
    ;package-install-upgrade-built-in t   ;; upgrade built-in packages
    read-file-name-completion-ignore-case t
    electric-pair-mode nil               ;; Don't auto-add closing
                                         ;; parens
    compilation-scroll-output t)         ;; compilation buffer scrolls
  ;; Remove trailing whtiespace and lines upon saving
  (add-hook 'before-save-hook (lambda ()
                                (delete-trailing-whitespace)))
  (setq delete-trailing-lines t)

  ;; Modes
  (global-auto-revert-mode          t ) ;; revert buffers when changed
  (transient-mark-mode              t ) ;; visual highlighting
  (delete-selection-mode            t ) ;; overwrite selected text
  (size-indication-mode             t ) ;; file size on mode line
  (column-number-mode               t ) ;; cusor column position

  ;; Add a vertical line at fill-column
  (global-display-fill-column-indicator-mode t)
  ;; Control how visible it is
  (set-face-attribute 'fill-column-indicator nil :foreground "grey50")

  ;; 'y or n' instead of 'yes or no'
  (fset 'yes-or-no-p 'y-or-n-p)
)

;; Provides access to recently opened files
(use-package recentf
  :init
  (recentf-mode t)
  :bind (("C-x M-f" . set-fill-column) ;; to make room
         ("C-x f" . recentf-open-files))
  :config
  ;; don't list these files in recentf
  (setq recentf-exclude '("\\cookies\\'"
                          "\\archive-contents\\'"
                          "\\.ido.last\\'")))
;; Remote editing files via SSH
(use-package tramp
  :defer t
  :config
  ;; cache passwords in tramp for 1 hr
  (setq password-cache-expiry 3600))

(use-package org
  :pin gnu
  :defer t
  :mode (("\\.org$" . org-mode))
  :config
  ;; In subsections (e.g. ***) hides all but the last *
  (setq org-hide-leading-stars t))

;; File listings
(use-package dired
  :config
  (setq dired-recursive-deletes 'always) ;; Prompt only once for dired delete
  ;; On OSX, ls isn't gnu-ls so causes some issues. Install
  ;; `coreutils` via homebrew first.
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/opt/homebrew/bin/gls"))
  ;; .cbz files are just renamed .zip files
  (add-hook 'dired-mode-hook
            (lambda ()
              (require 'dired-aux) ;; defines dired-compress-files-alist
              (add-to-list 'dired-compress-files-alist
                           '("\\.cbz\\'" . "zip %o -r --filesync %i"))))
  ;; dired creates a new buffer for each directory. This encourages
  ;; dired to reuse the same buffer.
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda ()
                                         (interactive)
                                         (find-alternate-file "..")))
  ;; Switch from dired-do-find-regexp-and-replace to
  ;; dired-do-query-replace-regexp
  (define-key dired-mode-map (kbd "Q") 'dired-do-query-replace-regexp)
  (put 'dired-find-alternate-file 'disabled nil)
  ;; Open files in dired mode using 'open'
  (eval-after-load "dired"
    '(progn
       (define-key dired-mode-map (kbd "z")
         (lambda () (interactive)
           (let ((fn (dired-get-file-for-visit)))
             (start-process "default-app" nil "open" fn))))))
  :custom
  (dired-listing-switches "-AFBhl")) ;; switches passed to ls

;; Display line numbers in left buffer
(use-package display-line-numbers
  :init
  (global-display-line-numbers-mode t)
  :config
  ;; don't shrink line-number space when looking at fewer digits
  (setq display-line-numbers-grow-only t)
  ;; Start with width-3 by default, only grow for 4 or more digits
  (setq-default display-line-numbers-width 3))

;; Show matching parantheses
(use-package paren
  :init
  (show-paren-mode t)
  :config
  (setq show-paren-delay 0)) ;; don't delay showing parens

(use-package ispell
  :config
  ;; Needed when installing aspell by homebrew (may work without it if
  ;; you install to /usr/bin/aspell)
  (when (equal system-type 'darwin)
    (defvar ispell-program-name)
    (defvar ispell-extra-args)
    (setq ispell-program-name "/opt/homebrew/bin/aspell")
    ;; ultra is faster but less accurate
    (setq ispell-extra-args '("--sug-mode=ultra")))
  ;; This redefines the function to place the corrections below instead of above.
  ;; Doing this on emacs 29.1, if emacs updates, this may need to change. Do by
  ;; running `C-h f ispell-display-buffer` and copying the code.
  (defun ispell-display-buffer (buffer)
    "Show BUFFER in new window belowselected one.
     Also position fit window to BUFFER and select it."
    (let* ((unsplittable
	          (cdr (assq 'unsplittable (frame-parameters (selected-frame)))))
	         (window
	          (or (get-buffer-window buffer)
	              (and unsplittable
		                 ;; If frame is unsplittable, temporarily disable that...
		                 (let ((frame (selected-frame)))
		                   (modify-frame-parameters frame '((unsplittable . nil)))
		                   (prog1
			                     (condition-case nil
			                         (split-window
                                ;; Chose the last of a window group, since
                                ;; otherwise, the lowering of another window's
                                ;; TL corner would cause the logical order of
                                ;; the windows to be changed.
			                          (car (last (selected-window-group)))
                                (- ispell-choices-win-default-height) 'below)
			                       (error nil))
		                     (modify-frame-parameters frame '((unsplittable . t))))))
	              (and (not unsplittable)
		                 (condition-case nil
		                     (split-window
                          ;; See comment above.
			                    (car (last (selected-window-group)))
                          (- ispell-choices-win-default-height) 'below)
		                   (error nil)))
	              (display-buffer buffer))))
      (if (not window)
	        (error "Couldn't make window for *Choices*")
        (select-window window)
        (set-window-buffer window buffer)
        (set-window-point window (point-min))
        (fit-window-to-buffer window nil nil nil nil t))))
  )


;;;;;;;;;;;;;;;;;;;;;
;;;;; Functions ;;;;;
;;;;;;;;;;;;;;;;;;;;;

;;;;; Non-interactive functions

;; Makes yanked text available in the os clipboard. The sources also
;; has a function to paste from clipboard, but Cmd-V works fine for
;; that and I don't want to lose my yank queue.
;; https://gist.github.com/the-kenny/267162
(defun my/copy-to-clipboard (text &optional push)
  "Copy the selection to OS X clipboard."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(when (equal system-type 'darwin)
  (setq interprogram-cut-function 'my/copy-to-clipboard))

;; https://www.reddit.com/r/emacs/comments/u2lf9t/weekly_tips_tricks_c_thread/i4n9aoa/?context=1
;; Greys out any files in .gitignore
(defun dired-dim-git-ignores ()
  "Dim out .gitignore contents"
  (when-let ((_ (require 'vc)) (ignores
                                (vc-default-ignore-completion-table
                                 'git ".gitignore"))
             (exts (make-local-variable
                    'completion-ignored-extensions)))
    (dolist (item ignores) (add-to-list exts item))))
(add-hook 'dired-mode-hook #'dired-dim-git-ignores)

;;;;; Interactive functions

;; Select the current word.
;; http://xahlee.org/emacs/elisp_examples.html
(defun my/select-current-word ()
  "Select the word under cursor. 'word' here is considered any
alphanumeric sequence with '_' or '-'."
  (interactive)
  (let (pt)
    (skip-chars-backward "-_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "-_A-Za-z0-9")
    (set-mark pt)))
(global-set-key (kbd "M-~") 'my/select-current-word)

;; Make C-a smarter.
;; http://www.cs.utah.edu/~aek/code/init.el.html
(defun my/beginning-of-line-dynamic ()
  "Jumps to the beginning of text on line. If already there, goes
to the true beginning of the line (before space.)"
  (interactive)
  (let ((cur (point)))
    (beginning-of-line-text)
    (when (= cur (point))
      (beginning-of-line))))
(global-set-key (kbd "C-a") 'my/beginning-of-line-dynamic)

;; Does align-regexp over ALL entries in the line instead of just the
;; first http://www.emacswiki.org/emacs/AlignCommands
(defun align-all (start end regexp)
  "Aligns on the same regexp as often as it appears in a line."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

;; Inserts R chunk
(defun my/insert-R-chunk ()
  "Inserts the tags for a R dyndoc chunk."
  (interactive)
  (insert "```{r}\n")
  (save-excursion
    (insert "\n```")))
(add-hook 'markdown-mode-hook
          (lambda () (local-set-key (kbd "C-c C-s r")
                                    'my/insert-R-chunk)))

;; Inserts stata do chunk
(defun my/insert-stata-dyndoc-chunk ()
  "Inserts the tags for a Stata dyndoc chunk."
  (interactive)
  (insert "~~~~\n<<dd_do>>\n")
  (save-excursion
    (insert "\n<</dd_do>>\n~~~~")))
(add-hook 'markdown-mode-hook
          (lambda () (local-set-key (kbd "C-c C-s d")
                                    'my/insert-stata-dyndoc-chunk)))

;; When given a list of numbers like 1, 2, 3, ..., 10, 11, 12, ..., 100, 101,
;; this will pad the single digits with 00 and the double digits with 0
(defun my/pad_numeric_zeroes ()
  "Pads a region with up to 2 \"0\"s to ensure a total length of 3"
  (interactive)
  (setq-local len (- (region-end) (region-beginning)))
  (goto-char (region-beginning))
  (cond ((eq len 2) (insert "0"))
        ((eq len 1) (insert "00"))))

;; The inverse of fill-paragraph,
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org3dd06d8
(defun my/unfill-paragraph (&optional region)
  "Make a multi-line paragraph into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(bind-key "M-Q" 'my/unfill-paragraph)

;; https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Opens a file which has the current buffer name, but a different extension
(defun my/open-file-alternate-extension (extension)
  "Opens the file represented by the current buffer with the new extension.

Useful on md/Rmd files to open their compiled pdf or html versions."
  (interactive)
  ;; If `extension` has a proceeding period, remove it.
  (replace-regexp-in-string "$\." "$" extension)
  (shell-command
   (concat "open " (file-name-sans-extension (buffer-file-name)) "." extension)))

(defun my/open-markdown-output-html ()
  (interactive)
  (my/open-file-alternate-extension "html"))
(bind-key "C-c o h" 'my/open-markdown-output-html)
(defun my/open-markdown-output-pdf ()
  (interactive)
  (my/open-file-alternate-extension "pdf"))
(bind-key "C-c o p" 'my/open-markdown-output-pdf)

;; Tweak how paragraphs are defined so fill-paragraph (M-q) beahves more as
;; desired.
;; https://emacs.stackexchange.com/a/78562
(defun my-in-dd-display ()
  "Don't break lines in Inline Stata Dynamic Tags."
  (save-excursion
    (save-match-data
      (and (re-search-backward ">>\\|\\(<<dd_display:\\)"
                               (line-beginning-position) t)
       (match-beginning 1)))))
(add-hook 'fill-nobreak-predicate #'my-in-dd-display)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; <span> functions ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/wrap-text-with-span (val)
  "Wrap the selected text with <span class=\"val\">...</span>.
Place the cursor on the '>' in '</span>'."
  (interactive)
  (if (region-active-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (goto-char end)
      (insert "</span>")
      (save-excursion
        (goto-char beg)
        (insert (format "<span class=\"%s\">" val))))))

(add-hook 'mhtml-mode-hook
       (lambda ()
         (local-set-key (kbd "C-c C-g n") (lambda () (interactive) (my/wrap-text-with-span "numeric")))
         (local-set-key (kbd "C-c C-g k") (lambda () (interactive) (my/wrap-text-with-span "keyword")))
         (local-set-key (kbd "C-c C-g o") (lambda () (interactive) (my/wrap-text-with-span "operator")))
         (local-set-key (kbd "C-c C-g c") (lambda () (interactive) (my/wrap-text-with-span "comment")))
         (local-set-key (kbd "C-c C-g s") (lambda () (interactive) (my/wrap-text-with-span "string")))))

(add-hook 'web-mode-hook
       (lambda ()
         (local-set-key (kbd "C-c C-g n") (lambda () (interactive) (my/wrap-text-with-span "numeric")))
         (local-set-key (kbd "C-c C-g k") (lambda () (interactive) (my/wrap-text-with-span "keyword")))
         (local-set-key (kbd "C-c C-g o") (lambda () (interactive) (my/wrap-text-with-span "operator")))
         (local-set-key (kbd "C-c C-g c") (lambda () (interactive) (my/wrap-text-with-span "comment")))
         (local-set-key (kbd "C-c C-g s") (lambda () (interactive) (my/wrap-text-with-span "string")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; External Packages ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Using :diminish in use-package hides minor-modes from mode line
(use-package diminish
  :ensure t)

;; Jump to a specified location. Replaces ace-jump-mode
(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1)) ;; enter two characters, not necessarily at
                                    ;; start of work
  :config
  (setq avy-background t) ;; gray text other then matches
  ;; numbers only in hints
  (setq avy-keys (nconc (number-sequence ?1 ?9) '(?0))))


;; Selecting and editing repeated words
(use-package multiple-cursors
  :ensure t
  :bind (;; ("C-c M-c" . mc/edit-lines)
         ("M-!" . mc/mark-next-like-this)
         ("M-@" . mc/mark-previous-like-this)
         ("M-#" . mc/mark-all-like-this)))

;; Gives R,Rmd,etc modes. Also provides interface to R.
;; ess-stata has been removed
;; (https://github.com/emacs-ess/ESS/issues/1033)
(use-package ess
  :ensure t
  :defer t
  :init
  ;; Fixes a bug where tidyverse output (often errors) (I think coming from
  ;; package https://github.com/r-lib/cli), causes the R window prints only grey
  ;; text. This fix is from https://github.com/emacs-ess/ESS/issues/1193,
  ;; specifically commit
  ;; https://github.com/Fuco1/.emacs.d/commit/ff6bec53c5c61262c32f43c238171c599f747e55.
  ;; This should be removed once this functionality is folded into ESS.
  (defun my-inferior-ess-init ()
    (setq-local ansi-color-for-comint-mode 'filter))
  (add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)
  :config
  (setq ess-ask-for-ess-directory nil        ;; just run R wherever
                                             ;; the file lives
        ess-history-file nil                 ;; don't save history
        ess-eval-visibly-p nil               ;; when running R, don't
                                             ;; show code, just output
                                             ;; (greatly speeds
                                             ;; running)
        inferior-R-args
          "--no-restore --no-save --quiet"   ;; R startup conditions
        ess-style 'RStudio                   ;; better indenting
        comint-scroll-to-bottom-on-input   t ;; force ESS to scroll R
                                             ;; to the bottom after
                                             ;; running code
        comintq-scroll-to-bottom-on-output t
        comint-scroll-show-maximum-output  t
        comint-move-point-for-output       t))

;; Mixing of markdown and R in Rmarkdown files
(use-package poly-markdown
  :ensure t
  :defer t
  :init
  :mode ("\\.md$" . poly-markdown-mode)
  :mode ("\\.Rmd$" . poly-markdown-mode)
  :mode ("\\.dyndoc$" . poly-markdown-mode)
  :config
  ;; Highlight latex math snippets
  (setq markdown-enable-math t))

(use-package quarto-mode
  :ensure t
  :defer t
  :init
  :mode ("\\.qmd$" . poly-quarto-mode))

;; Latex mode and compilation
(use-package auctex
  :ensure t
  :defer t
  :mode ("\\.Rnw$" . LaTeX-mode)
  :init
  (add-hook 'LaTeX-mode-hook (lambda () ;; add latexmk to C-c C-c list
                               (push
                                '("Latexmk" "latexmk -pdf -pvc %s"
                                  TeX-run-command nil t
                                  :help "Run Latexmk on file")
                                TeX-command-list)))
  :config
  (setq
   Tex-engine 'default
   TeX-PDF-mode t                            ;; PDF instead of dvi
   TeX-newline-function 'newline-and-indent) ;; autoindent in TeX-mode

  ;; For multi-file documents, use `C-c _` to add master information
  ;; to the file. Second command removes automation
  (setq TeX-master nil
        TeX-one-master "<none>")

  ;; OS X use `open`
  (setq TeX-view-program-selection '((output-pdf "open")))
  (setq TeX-view-program-list '(("open" "open %o"))))

;; Better matching in minibuffer (e.g. find file, switch buffer)
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

;; more niceties for ivy, including buffer showing path to file.
(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1)
  :config
  ;; suggested in ivy-rich documentation
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  ;; show full path (with ~ abbrev) instead of relative
  (setq ivy-rich-path-style 'abbrev))

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

;; GUI Emacs doesn't inherit from terminal environment variables (e.g.
;; PATH). This fixes it.
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; when switching buffers or scrolling, point a beacon at the cursor
(use-package beacon
  :ensure t
  :init
  (beacon-mode 1)
  :config
  (setq beacon-blink-duration 1
        beacon-blink-delay .5
        beacon-color 0))

;; Browsing yaml files
(use-package yaml-mode
  :ensure t
  :defer t)

;; These packages are all dependencies for helpful below, but need to be
;; manually installed
(use-package f
  :ensure t
  :defer t)
(use-package s
  :ensure t
  :defer t)
(use-package dash
  :ensure t
  :defer t)
(use-package elisp-refs
  :ensure t
  :defer t)

;; Better help files
(use-package helpful
  :ensure t
  :defer t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;; html, js, and CSS files
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.js?\\'" . web-mode))
         ;("\\.css?\\'" . web-mode)) ;; I kinda like css-mode for css better
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 2)
  :bind (("C-c ]" . web-mode-element-close)
         ("C-c /" . web-mode-element-close)))

; Highlight colors (e.g. #d10000)
(use-package rainbow-mode
  :hook (emacs-lisp-mode text-mode web-mode))

;;;;;;;;;;;;;;;
;;;;; lsp ;;;;;
;;;;;;;;;;;;;;;

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Custom file ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Rather that letting emacs stick custom-set-variables in here, place
;; it in a different file that is not under version control.
;; https://old.reddit.com/r/emacs/comments/67pzh5/using_customsetvariables_programmatically/dgsxvm3/

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; This page break is to ensure no local variables are set
;; https://stackoverflow.com/questions/18099531/how-to-ignore-a-local-variables-list-in-text

