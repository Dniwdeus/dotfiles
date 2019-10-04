(setq-default indent-tabs-mode nil)

;; delete the selection with a keypress
(delete-selection-mode 1)

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent

;; default to 2 visible spaces to display a tab
(setq-default tab-width 2)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

(setq auto-save-visited-mode t)
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; let buffer update when content_files changes
(global-auto-revert-mode t)

;; enable global line numbers
(display-line-numbers-mode)
;; show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; where emacs looks for credentials

(setq auth-sources
   (quote
    ("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"
     (:source "(password-store)"
              (:host t)))))
