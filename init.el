;; Turn off mouse interface early to speed up launching time
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode tooltip-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-message t)            ;; Don't show start-up message

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Package Management ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use M-x list-packages or package-list-packages
(require 'package)

;; Two additional, better repos.
(add-to-list 'package-archives '("marmalade"
                                 . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa"
                                 . "http://melpa.milkbox.net/packages/") t)

(package-initialize) ;; initialize packages (needed to load packages installed
                     ;; by M-x package-install)

; Automatically install important packages on a new system.
;; (defvar my-packages '(ace-jump-mode auctex buffer-move ess multiple-cursors ssh))
;; (dolist (p my-packages)
;;  (when (not (package-installed-p p))
;;    (package-install p)))

(require 'uniquify)         ;; for unique buffer names
(require 'ace-jump-mode)    ;; quicker way to jump around in repetitious code
(require 'multiple-cursors) ;; load at start to avoid issues with first usage

;; I occasionally fork bbatsov's excellent zenburn-theme, so I install this via git instead of
;; via the packages.
;; To use the default, add zenburn-theme to (defvar my-packages...) and remove the next line.
(add-to-list 'custom-theme-load-path "~/repositories/zenburn-emacs/")
(load-theme 'zenburn t)  ;; The 't' says not to security check

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Key Bindings ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c M-w") 'kill-region)        ;; Move this to allow ...
(global-set-key (kbd "C-w")     'backward-kill-word) ;; ... delete a whole word
(global-set-key (kbd "C-x C-b") 'ibuffer)            ;; use ibuffer
(global-set-key (kbd "C-x M-f") 'set-fill-column)    ;; Move this to allow ...
(global-set-key (kbd "C-x f")   'recentf-open-files) ;; ... list recent files
(global-set-key (kbd "RET")     'newline-and-indent) ;; auto-indent on "enter"

;; AceJumpMode
(global-set-key (kbd "M-s")     'ace-jump-mode)      ;; search by ace-jump
(global-set-key (kbd "M-r")     'ace-jump-line-mode) ;; ace-jump to lines

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

(setq-default tab-width 2             ;; default tab width is 2 spaces
              indent-tabs-mode nil    ;; don't allow tabs (spaces instead)
              indicate-empty-lines t  ;; show end of file
              fill-column 115)         ;; column width to 115 by default
(setq
 tab-always-indent 'complete          ;; some sort of smart-tabbing thing
 inhibit-startup-message t            ;; Don't show start-up message...
 initial-scratch-message nil          ;; ... or *scratch* message
 uniquify-buffer-name-style 'post-forward
                                      ;; uniqify as file|dir
 visible-bell t                       ;; no beeps on errors ...
 scroll-error-top-bottom t            ;; ... and don't error too soon
 ring-bell-function 'ignore           ;; no bells at all
 backup-inhibited t                   ;; disable backups ...
 make-backup-files nil                ;; ... and more backups ...
 auto-save-default nil                ;; ... and autosave ...
 auto-save-list-file-prefix nil       ;; ... and recovery
 vc-follow-symlinks t                 ;; open symlinks to version controlled files
 ns-pop-up-frames nil                 ;; OS X specific: new buffer, not file
 echo-keystrokes 0.01                 ;; show commands instantly in minibuffer
 scroll-conservatively 5              ;; only scroll a bit when moving cursor
 show-paren-delay 0                   ;; don't delay showing parens
 read-buffer-completion-ignore-case t ;; don't worry about case in minibuffer
 read-file-name-completion-ignore-case t
)

(fset 'yes-or-no-p 'y-or-n-p)         ;; 'y or n' instead of 'yes or no'
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
                                      ;; trailing whitespace is deleted
(setq delete-trailing-lines t)        ;; delete-trailing-whitespace will also do this

;; Needed when installing aspell by homebrew (may work without it if you install
;; to /usr/bin/aspell)
(when (equal system-type 'darwin)
  (defvar ispell-program-name)
  (defvar ispell-extra-args)
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq ispell-extra-args '("--sug-mode=ultra"))) ;; faster but less accurate

;; When using q to quit ibuffer, kill *Ibuffer*
(defadvice ibuffer-quit (after kill-ibuffer activate)
      "Kill the ibuffer buffer on exit."
      (kill-buffer "*Ibuffer*"))

;; don't list these files in recentf
(setq recentf-exclude '("\\cookies\\'"
                        "\\archive-contents\\'"
                        "\\.ido.last\\'"))

;; mode loadings
(show-paren-mode          t ) ;; show matching parens
(global-auto-revert-mode  t ) ;; revert buffers when changed
(transient-mark-mode      t ) ;; visual highlighting
(delete-selection-mode    t ) ;; typing replaces selected text
(size-indication-mode     t ) ;; include file size on toolbar
(line-number-mode         t ) ;; cursor position line ...
(column-number-mode       t ) ;; ... and column

(recentf-mode             t )         ;; recent file mode: recentf-open-files
(setq recentf-save-file "~/.recentf") ;; Put it somewhere not synced to avoid issues on multiple machines

(global-linum-mode t)         ;; show row numbers
(setq linum-format            ;; add space after row number, right align
      (lambda (line)
        (propertize (format
                     (let ((w (length (number-to-string
                                       (count-lines (point-min) (point-max))))))
                       (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

;;;;;;;;;;;;;;;;;;;;;
;;;;; Functions ;;;;;
;;;;;;;;;;;;;;;;;;;;;

;; Select the current word.
;; http://xahlee.org/emacs/elisp_examples.html
(defun select-current-word ()
  "Select the word under cursor. 'word' here is considered any
alphanumeric sequence with '_' or '-'."
  (interactive)
  (let (pt)
    (skip-chars-backward "-_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "-_A-Za-z0-9")
    (set-mark pt)))
(global-set-key (kbd "M-~") 'select-current-word)

;; Make C-a smarter
;; http://www.cs.utah.edu/~aek/code/init.el.html
(defun beginning-of-line-dynamic ()
  "Jumps to the beginning of text on line.  If already there,
goes to the true beginning of the line (before space.)"
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
                                   (shell-command-to-string "$SHELL -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

;; Run over a section to increment all numbers in the region
;; http://jmdavisblog.blogspot.com/2013/08/a-handful-of-emacs-utilities.html
(defun inc-num-region (p m)
  "Increments the numbers in a given region"
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

;;;;;;;;;;;;;;;
;;;;; ESS ;;;;;
;;;;;;;;;;;;;;;

;; load ESS only on demand - if opening a .R file, or calling r-mode
(add-to-list 'auto-mode-alist '("\\.R$" . r-mode))
(autoload 'R        "ess-site" nil t) ;; Only load ess if starting R,
(autoload 'r-mode   "ess-site" nil t) ;; or opening an R file,
(autoload 'Rnw-mode "ess-site" nil t) ;; or opening a Sweave file.

(setq ess-ask-for-ess-directory nil      ;; just run R wherever the file lives
      ess-history-file nil               ;; don't save history
      ess-eval-visibly-p nil             ;; when running R, don't show code,
      ;; just output (greatly speeds running)
      inferior-R-args
      "--no-restore --no-save --quiet") ;; R startup conditions

(setq ess-default-style 'GNU) ;; better indenting
(setq ess-fancy-comments nil) ;; don't distinguish #, ##, ###


;; force ESS to scroll R to the bottom after running code
(setq
 comint-scroll-to-bottom-on-input  t
 comint-scroll-to-bottom-on-output t
 comint-scroll-show-maximum-output t
 comint-move-point-for-output      t)

;;;;;;;;;;;;;;;;;;
;;;;; Auctex ;;;;;
;;;;;;;;;;;;;;;;;;


(autoload 'tex-mode "auctex" nil t) ;; Load auctgex when entering tex-mode

;; Synctex
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode) ;; Auto enter synctex mode
(setq TeX-source-correlate-start-server t)

(setq
 TeX-engine 'default                       ;; XeTeX causes issues with tikz
 ;TeX-engine 'xetex                        ;; use XeTeX
 TeX-PDF-mode t                            ;; PDF instead of dvi
 TeX-newline-function 'newline-and-indent) ;; autoindent in TeX-mode

;; Set list of programs to open output (Linux or Mac)
(setq TeX-view-program-list '(("open" "open %o")
                              ("evince" "evince %o")
                              ("zathura" "zathura %o")))

;; use open to open files on OS X
(when (equal system-type 'darwin)
  (setq TeX-view-program-selection
        (quote ((
                (output-dvistyle-pstricks) "dvips andgv")
                (output-dvi "open")
                (output-pdf "open")
                (output-html "open")))))

;; use specific programs in Linux
(when (equal system-type 'gnu/linux)
  (setq TeX-view-program-selection
        (quote ((
                (output-dvistyle-pstricks) "dvips andgv")
                (output-dvi "evince")
                (output-pdf "zathura")
                (output-html "firefox")))))
;; use auto-fill always on tex files
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;;;;;;;;;;;;;;;;;;
;;;;; Sweave ;;;;;
;;;;;;;;;;;;;;;;;;

;; from https://sites.google.com/site/andreaskiermeier/essmaterials

(defvar TeX-file-extensions)
(setq TeX-file-extensions
      '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))
(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
(add-to-list 'auto-mode-alist '("\\.Snw\\'" . Snw-mode))

;; Define these now to avoid warnings upon compiliation
;; http://stackoverflow.com/questions/12432093/get-rid-of-reference-to-free-variable-byte-compilation-warnings
(defvar TeX-expand-list)
(defvar TeX-command-list)
(defvar TeX-command-default)
(add-hook 'Rnw-mode-hook
          (lambda ()
            (add-to-list 'TeX-expand-list '("%rnw" file "Rnw" t) t)
            (add-to-list 'TeX-command-list
                         '("Sweave" "R CMD Sweave %rnw"
                           TeX-run-command nil (latex-mode) :help "Run Sweave") t)
            (add-to-list 'TeX-command-list
                         '("LatexSweave" "%l %(mode) %s"
                           TeX-run-TeX nil (latex-mode) :help "Run Latex after Sweave") t)
            (setq TeX-command-default "Sweave")))

;;;;;;;;;;;;;;;;;;;;
;;;;; Org-mode ;;;;;
;;;;;;;;;;;;;;;;;;;;

;; (global-set-key (kbd "C-c a") 'org-agenda)  ;; access agenda (personal calendar)
;; (setq
;;  org-agenda-files '("~/.emacs.d/org")       ;; location of .org agenda files
;;  org-agenda-start-on-weekday 0              ;; start on sunday, not monday
;;  org-agenda-timegrid-use-ampm 1             ;; 12-hour instead of 24-hour in agenda view
;;  org-agenda-time-grid '((daily today)       ;; only change from default is show time-grid every day regardless of events
;;                         #("----------------" 0 16 (org-heading t))
;;                         (800 1000 1200 1400 1600 1800 2000)))

;; ;; Declare this now since org isn't loaded until asked for
;; ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Declaring-Functions.html
;; (declare-function org-agenda-goto-today "org-agenda" nil)

;; ;; Jump to the beginning of "now - - ..." in the agenda buffer
;; (defun org-agenda-goto-now ()
;;   "Go to the 'now' entry on today."
;;   (interactive)
;;   (org-agenda-goto-today)
;;   (re-search-forward "now - - - - - - - -")
;;   (goto-char (match-beginning 0)))

;; ;; Jump to "now - - ...", or jump between "now" and "today"
;; (defun org-agenda-goto-now-dynamic ()
;;   "Go to the 'now' entry on today. If already there, jump to 'today'."
;;   (interactive)
;;   (let ((cur (point)))
;;     (org-agenda-goto-now)
;;     (when (= cur (point))
;;       (org-agenda-goto-today))))

;; ;; Replace '.' with dynamic goto-now in org-agenda-mode
;; (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd ".") 'org-agenda-goto-now-dynamic)))


;; ;; Color entries from each file separately
;; ;; http://stackoverflow.com/questions/17066580/color-code-agenda-view-per-file
;; (add-hook 'org-finalize-agenda-hook
;;           (lambda ()
;;             (save-excursion
;;               (color-org-header "Personal:"  "green")
;;               (color-org-header "Birthdays:" "gold")
;;               (color-org-header "Holidays:"  "khaki")
;;               (color-org-header "Class:"     "blue")
;;               (color-org-header "Work:"      "orange")
;;               (color-org-header "Wedding:"   "color-135")
;;               (color-org-header "Research:"  "OrangeRed"))))

;; (defun color-org-header (tag col)
;;   "Colors org-agenda entries of category 'tag' with color 'col'."
;;   (interactive)
;;   (goto-char (point-min))
;;   (while (re-search-forward tag nil t)
;;     (add-text-properties (match-beginning 0) (point-at-eol)
;;                          `(face (:foreground ,col)))))

;;;;;;;;;;;;;;;;;;;;
;;;;; Ido-mode ;;;;;
;;;;;;;;;;;;;;;;;;;;

(ido-mode t)                     ;; enable ido-mode by default
(setq ido-enable-flex-matching t ;; fuzzy matching
      ido-case-fold t            ;; ignore case
      ido-save-directory-list-file "~/.emacs.d/.ido.last") ;; move save file
;; Don't list these files
(add-to-list 'ido-ignore-files '("\.DS_Store", "\.pyc"))
