;; Turn off mouse interface early to speed up launching time
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode
                              tooltip-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-message t)       ;; Don't show start-up message

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Package Management ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use M-x list-packages or package-list-packages
(require 'package)

;; Two additional, better repos.
(add-to-list 'package-archives '("marmalade"
                                 . "http://marmalade-repo.org/packages/")
             t)
(add-to-list 'package-archives '("melpa"
                                 . "http://melpa.milkbox.net/packages/")
             t)

(package-initialize) ;; initialize packages (needed to load packages installed by M-x package-install)

(custom-set-variables
 '(package-selected-packages
   (quote
    (polymode ssh multiple-cursors markdown-mode ess buffer-move auctex ace-jump-mode))))

(require 'ace-jump-mode)    ;; quicker way to jump around in repetitious code
(require 'multiple-cursors) ;; load at start to avoid issues with first usage

;; I occasionally fork bbatsov's excellent zenburn-theme, so I install this via git instead of via the packages.  To use the default, add
;; zenburn-theme to (defvar my-packages...) and remove the next line.
(add-to-list 'custom-theme-load-path "~/repositories/zenburn-emacs/")
(load-theme 'zenburn t)  ;; The 't' says not to security check

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Key Bindings ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Move this to allow deleting a whole word
(global-set-key (kbd "C-c M-w") 'kill-region)
(global-set-key (kbd "C-w")     'backward-kill-word)

;; Move this to allow listing recent files
(global-set-key (kbd "C-x M-f") 'set-fill-column)
(global-set-key (kbd "C-x f")   'recentf-open-files)

;; Use ibuffer instead of regular buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; AceJumpMode
(global-set-key (kbd "M-s")     'ace-jump-mode)
(global-set-key (kbd "M-r")     'ace-jump-line-mode)
;; selects symbol under cursor and search
(global-set-key (kbd "C-c M-s") 'isearch-forward-symbol-at-point)

;; Multiple-cursors
;(global-set-key (kbd "C-c M-c") 'mc/edit-lines)
(global-set-key (kbd "M-!") 'mc/mark-next-like-this)
(global-set-key (kbd "M-@") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-#") 'mc/mark-all-like-this)

;; Change from zap-to-char to zap-up-to-char (don't delete the char itself)
(autoload 'zap-up-to-char "misc" 'interactive)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;;;;;;;;;;;;;;;;;;;;
;;;;; Settings ;;;;;
;;;;;;;;;;;;;;;;;;;;


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
 vc-follow-symlinks t                 ;; open symlinks to version controlled files
 echo-keystrokes 0.01                 ;; show commands instantly in minibuffer
 scroll-conservatively 5              ;; only scroll a bit when moving cursor
 read-buffer-completion-ignore-case t ;; don't worry about case in minibuffer
 read-file-name-completion-ignore-case t
 electric-pair-mode nil               ;; Ensure that electric-pairing isn't activated
)

(fset 'yes-or-no-p 'y-or-n-p)         ;; 'y or n' instead of 'yes or no'
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
                                      ;; trailing whitespace is deleted
(setq delete-trailing-lines t)        ;; delete-trailing-whitespace will also do this

;; Needed when installing aspell by homebrew (may work without it if you install to /usr/bin/aspell)
(when (equal system-type 'darwin)
  (defvar ispell-program-name)
  (defvar ispell-extra-args)
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq ispell-extra-args '("--sug-mode=ultra"))) ;; faster but less accurate

;; When using q to quit ibuffer, kill *Ibuffer*
(defadvice ibuffer-quit (after kill-ibuffer activate)
  "Kill the ibuffer buffer on exit."
  (kill-buffer "*Ibuffer*"))


;; mode loadings
(global-auto-revert-mode  t   ) ;; revert buffers when changed
(transient-mark-mode      t   ) ;; visual highlighting
(delete-selection-mode    t   ) ;; typing replaces selected text
(size-indication-mode     t   ) ;; include file size on toolbar
(line-number-mode         t   ) ;; cursor position line ...
(column-number-mode       t   ) ;; ... and column
(setq electric-pair-mode  nil ) ;; Ensure that electric-pairing isn't activated

(show-paren-mode          t   ) ;; show matching parens
(setq show-paren-delay    0   ) ;; don't delay showing parens

(recentf-mode             t   )        ;; recent file mode:
                                       ;; recentf-open-files
(setq recentf-save-file "~/.recentf"   ;; Put it somewhere not synced to avoid issues on multiple machines
      recentf-exclude '("\\cookies\\'" ;; don't list these files in recentf
                        "\\archive-contents\\'"
                        "\\.ido.last\\'"))

(global-linum-mode        t  ) ;; show row numbers
(setq linum-format             ;; add space after row number, right align
      (lambda (line)
        (propertize (format
                     (let ((w (length (number-to-string
                                       (count-lines (point-min)
                                                    (point-max))))))
                       (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

;;;;;;;;;;;;;;;;;;;;;
;;;;; Functions ;;;;;
;;;;;;;;;;;;;;;;;;;;;

;; Select the current word. http://xahlee.org/emacs/elisp_examples.html
(defun select-current-word ()
  "Select the word under cursor. 'word' here is considered any alphanumeric sequence with '_' or '-'."
  (interactive)
  (let (pt)
    (skip-chars-backward "-_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "-_A-Za-z0-9")
    (set-mark pt)))
(global-set-key (kbd "M-~") 'select-current-word)

;; Make C-a smarter. http://www.cs.utah.edu/~aek/code/init.el.html
(defun beginning-of-line-dynamic ()
  "Jumps to the beginning of text on line. If already there, goes to the true beginning of the line (before space.)"
  (interactive)
  (let ((cur (point)))
    (beginning-of-line-text)
    (when (= cur (point))
      (beginning-of-line))))
(global-set-key (kbd "C-a") 'beginning-of-line-dynamic)

;; GUI Emacs on Mac doesn't respect system PATH, this syncs them on launch.
;; http://stackoverflow.com/questions/2266905/emacs-is-ignoring-my-path-when-it-runs-a-compile-command
(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to PATH from the system."
  (let ((path-from-shell
         (replace-regexp-in-string "[[:space:]\n]*$" ""
                                   (shell-command-to-string
                                    "$SHELL -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

;; Run over a section to increment all numbers in the region http://jmdavisblog.blogspot.com/2013/08/a-handful-of-emacs-utilities.html
(defun inc-num-region (p m)
  "Increments the numbers in a given region."
  (interactive "r")
  (save-restriction
    (save-excursion
      (narrow-to-region p m)
      (goto-char (point-min))
      (forward-line)
      (let ((counter 1))
        (while (not (eq (point)
                        (point-max)))
          (goto-char (point-at-eol))
          (search-backward-regexp "[0-9]+" (point-at-bol) t)
          (let* ((this-num (string-to-number (match-string 0)))
                 (new-num-str (number-to-string (+ this-num
                                                   counter))))
            (replace-match new-num-str)
            (incf counter)
            (forward-line)))))))

;; Does align-regexp over ALL entries in the line instead of just the first http://www.emacswiki.org/emacs/AlignCommands
(defun align-all (start end regexp)
  "Aligns on the same regexp as often as it appears in a line."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

;; Makes yanked text available in the os clipboard. The sources also has a function to paste from clipboard, but Cmd-V works fine for that and I don't
;; want to lose my yank queue.  https://gist.github.com/the-kenny/267162
(defun copy-to-clipboard (text &optional push)
  "Copy the selection to OS X clipboard."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'copy-to-clipboard)

;; Launch R, split into side-by-side buffers, and make the left buffer an R-mode file to edit. If R is already running, don't restart it.
(defun rr ()
  (interactive)
  (if (get-buffer "*R*")
      (switch-to-buffer "*R*")
    (R))
  (split-window-right)
  (switch-to-buffer "rscript")
  (r-mode))

;;;;;;;;;;;;;;;
;;;;; ESS ;;;;;
;;;;;;;;;;;;;;;

;; load ESS only on demand - if opening a .R file, or calling r-mode
(add-to-list 'auto-mode-alist '("\\.R$" . r-mode))
(autoload 'R        "ess-site" nil t) ;; Only load ess if starting R,
(autoload 'r-mode   "ess-site" nil t) ;; or opening an R file,

(setq ess-ask-for-ess-directory nil      ;; just run R wherever the file lives
      ess-history-file nil               ;; don't save history
      ess-eval-visibly-p nil             ;; when running R, don't show code, just output (greatly speeds running)
      inferior-R-args
      "--no-restore --no-save --quiet") ;; R startup conditions

(setq ess-default-style 'RStudio) ;; better indenting

;; force ESS to scroll R to the bottom after running code
(setq
 comint-scroll-to-bottom-on-input  t
 comint-scroll-to-bottom-on-output t
 comint-scroll-show-maximum-output t
 comint-move-point-for-output      t)

;; TRAMP
(setq password-cache-expiry 3600) ;; cache passwords in tramp for 1 hr

;;;;;;;;;;;;;;;;;;
;;;;; Auctex ;;;;;
;;;;;;;;;;;;;;;;;;

(autoload 'LaTeX-mode "auctex" nil t) ;; Load auctex when entering tex-mode

(setq
 ;TeX-engine 'default                      ;; XeTeX causes issues with tikz
 TeX-engine 'xetex                         ;; use XeTeX
 TeX-PDF-mode t                            ;; PDF instead of dvi
 TeX-newline-function 'newline-and-indent) ;; autoindent in TeX-mode

(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("Latexmk" "latexmk -pdf -pvc %s" TeX-run-command nil t
      :help "Run Latexmk on file")
    TeX-command-list)))

;; For multi-file documents, use `C-c _` to add master information to the file.  Second command removes automation
(setq TeX-master nil
      TeX-one-master "<none>")

;; OS X use `open`
(setq TeX-view-program-selection '((output-pdf "open")))
(setq TeX-view-program-list '(("open" "open %o")))

;; use auto-fill always on tex files
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Markdown-mode ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(setq markdown-enable-math t) ;; Highlight latex math snippets

;;;;;;;;;;;;;;;;;;;;
;;;;; Org-mode ;;;;;
;;;;;;;;;;;;;;;;;;;;

(setq org-hide-leading-stars t)

;;;;;;;;;;;;;;;;;;;;
;;;;; Ido-mode ;;;;;
;;;;;;;;;;;;;;;;;;;;

(ido-mode t)                                               ;; enable ido-mode by default
(setq ido-enable-flex-matching t                           ;; fuzzy matching
      ido-case-fold t                                      ;; ignore case
      ido-save-directory-list-file "~/.emacs.d/.ido.last") ;; move save file

(add-to-list 'ido-ignore-files '("\.DS_Store", "\.pyc")) ;; Don't list these files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; fill-column per mode ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use a wider fill-column for text-only modes (e.g. not likely to be run side-by-side with terminal/output.

(add-hook 'markdown-mode-hook
          (lambda ()
            (set-fill-column 150)))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (set-fill-column 150)))

(add-hook 'TeX-mode-hook
          (lambda ()
            (set-fill-column 150)))


;; This page break is to ensure no local variables are set
;; https://stackoverflow.com/questions/18099531/how-to-ignore-a-local-variables-list-in-text

