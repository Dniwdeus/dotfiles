;;; * setup package manager for emacs
(require 'package)
(setq package-enable-at-startup t)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;;; * bootstrap use package [fn:11]


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;; * for now keep usepackage verbose.
(setq use-package-verbose t
      use-package-expand-minimally nil
      use-package-compute-statistics t)

;;; * load global configs for emacs
(load-file "~/.emacs.d/elisp/global-settings.el")

;;; * install org plus contrib with either package.el[fn:12]

(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load
(unless (package-installed-p 'org-plus-contrib)  ;; Make sure the Org package is
  (package-install 'org-plus-contrib))           ;; installed, install it if not
(package-initialize)                ;; Initialize & Install Package


;;; * my custom functions

(defun my/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
;;; * my global keymaps


(global-set-key (kbd "\C-c gi") 'my/find-user-init-file)

(defun load-directory (dir)
  (let ((load-it (lambda (f) (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;;; * load restclient
(use-package restclient
  :ensure t
)
(use-package ob-restclient
  :ensure t
)

;;; * configure magit

(use-package magit
  :commands (magit-status magit-blame magit-mode magit-log-buffer-file)
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c C-g l" . magit-log-buffer-file)
         ("C-c C-g c" . magit-commit)
         ("C-c C-g f" . vc-git-grep))
  :config
  (progn
    ;; Set `magit-status' fullscreen
    (setq magit-post-display-buffer-hook
          #'(lambda ()
              (when (derived-mode-p 'magit-status-mode)
                (delete-other-windows))))

    (setenv "GIT_PAGER" "")
    (add-hook 'magit-log-edit-mode-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)
                 (set-fill-column 80))))
  (use-package magit-blame
    :bind ("C-c C-g b" . magit-blame))
  (use-package magit-popup
    :ensure t)
  )


(use-package ghub
  :ensure t
  :config
  ;; FIXME https://github.com/magit/ghub/issues/81
  (setq ghub-use-workaround-for-emacs-bug nil)

  (require 'auth-source-pass)
  (defvar my-ghub-token-cache nil)

  (advice-add
   'ghub--token :around
   #'(lambda (orig-func host username package &optional nocreate forge)
       (or my-ghub-token-cache
           (setq my-ghub-token-cache
                 (funcall orig-func host username package nocreate forge))))))

(use-package forge
  :after (ghub magit)

  :ensure t
  :config
  (add-to-list 'forge-alist '("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository))
  (load-directory "~/sensitive/config/emacs/")
  )

;;; * git timemachine
(use-package git-timemachine
  :bind ("C-c C-g h" . git-timemachine)

  :diminish
  :ensure t)

(use-package password-store
  :defer 1
  :ensure t)

;;; * load org-board into emacs for archiving entire websites

(use-package org-board
  :bind (
         ("C-c bo" . obo)
         ("C-c ba" . oba))
  :init
  (defalias 'obo #'org-board-open)
  (defalias 'oba #'org-board-archive)
  :ensure t)

;;; * configure which-key to remember key-bindings

(use-package which-key
  :defer 5
  :diminish which-key-mode
  :bind (
         ("C-h ,m" . which-key-show-major-mode)
         ("C-h ,t" . which-key-show-top-level)
         ("C-h ,n" . which-key-show-next-page)
         )
  :config

  (which-key-setup-side-window-right-bottom)
  ;; another is to make the local map keys appear in bold
  ;; the page information along with the prefix.
  (setq which-key-show-prefix 'top)
  (setq which-key-is-verbose 1)
  (setq which-key-keymap-history t)

  ;; Set to t to show the count of keys shown vs. total keys in the mode line.
  (setq which-key-show-remaining-keys t)
  (which-key-add-key-based-replacements
    "C-c T" "Toggle prefix"
    "C-c w" "Web prefix"
    "C-h" "Help prefix"
    "M-s" "Find prefix"
    "s-x" "Launch prefix"
    "C-c !" "Flycheck prefix"
    "C-c &" "Snippets prefix"

    "C-x n" "Narrow prefix"

    "C-x a" "Abbreviate prefix"

    "C-x X" "Edebug prefix"
    )
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (setq which-key-highlighted-command-list (quote ("helm" "dired" "magit" "compose-mail" "prefix")))
  ;; Replacements for how KEY is replaced when which-key displays
  ;;   KEY → FUNCTION
  ;; Eg: After "C-c", display "right → winner-redo" as "▶ → winner-redo"
  (setq which-key-key-replacement-alist
        '(
          ("left"                  . "◀")
          ("right"                 . "▶")
          ("up"                    . "▲")
          ("down"                  . "▼")
          ("TAB"                   . "↹")
          ("RET"                   . "⏎")
          ("DEL"                   . "⇤")
          ("SPC"                   . "␣")
          ("<\\([[:alnum:]-]+\\)>" . "\\1")
          ;; Underlines commands to emphasize some functions:
          which-key-highlighted-command-list
          '("\\(rectangle-\\)\\|\\(-rectangle\\)"
            "\\`org-")))

  (setq which-key-idle-delay 1)
  (setq which-key-sort-order 'which-key-prefix-then-key-order)

  (which-key-mode)
  :ensure t)

(use-package org
  :demand t
  :commands (org-mode)
  :ensure t
  :diminish org-mode
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   ;;("C-c C-w" . org-refile)
   ;;("C-c [" . org-agenda-file-to-front)
   ;;("C-c !" . org-time-stamp-inactive)
   ;;("C-c r" . org-sort)
   ;;
   ;; ("C-c +" . mby-org-agenda-toggle-list-sublevels)
   ;; ("C-k" . org-cut-subtree)
   ;;
   ;;
   ;;("C-c b" . org-iswitchb)
   ;; ("C-c j" . org-clock-goto) ;;; jump to current task from anywhere
   ;; ("C-c C-w" . org-refile)
   ("C-c d" . org-refile-to-datetree)
   ("C-c M-," . org-insert-structure-template)
   ;; ("C-c i s" . my-org-screenshot)
   ;; ("C-c o c" . org-contacts)
   ;; ("C-c ," . org-cycle-agenda-files)
   )
  :mode ("\\.org'" . org-mode)
  :interpreter "org"
  :init

  (org-babel-do-load-languages

   'org-babel-load-languages
   '((R . t)
     (emacs-lisp . t)
     (python . t)
     (haskell . t)
     (latex . t)
     (gnuplot . t)
     (C . t)
     (sql . t)
     (ditaa . t)
     (shell . t)
     (css . t)
     (ditaa . t)
     (dot .t)
     (js . t)
     (latex . t)
     (ledger . t)
     (makefile . t)
     (org . t)
     (python . t)
     (sass . t)
     ))
  :config
  (progn
    (add-hook 'org-mode-hook
              (lambda ()
                (let ((filename (buffer-file-name (current-buffer))))
                  (when (and filename (string= "trello" (file-name-extension filename)))
                    (org-trello-mode)))))

    (setq org-directory "~/org/")
    (setq org-default-notes-file (concat org-directory "/org.org"))
    (add-to-list 'auto-mode-alist '("\\.trello\\'" . org-mode))



    (setq org-agenda-custom-commands
          '(

            ("@" "Contexts"
             ((tags-todo "@email"
                         ((org-agenda-overriding-header "Emails")))
              (tags-todo "@phone"
                         ((org-agenda-overriding-header "Phone")))))

            ("AZ" "Master Agenda"
             ((agenda #1="")
              (alltodo #1#))
             ((org-agenda-todo-list-sublevels 'indented)
              (org-tags-match-list-sublevels 'indented)
              (org-agenda-sorting-strategy '(priority-down)))

             )

            ("w" "work" agenda "")
            ("k" "knowledge" alltodo "")
            ))
    (setq org-agenda-files (list org-directory))




    (setq org-log-done t)
    (setq org-log-into-drawer "LOGBOOK")
    (setq org-yank-adjusted-subtrees t)
    (setq org-fontify-whole-heading-line t)

    (setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))
    (setq org-latex-remove-logfiles t)
    (setq org-export-backends (quote (ascii html icalendar latex md odt)))
                                                ;;; Switches off use of time-stamps when publishing. I would prefer to publish everything every time
    (setq org-publish-use-timestamps-flag nil)

    (setq-default org-html-head "body{margin:40px auto ;max-width:920px;
line-height:1.6; font-size:18px; color:#444; padding:0 10px}
h1,h2,h3{line-height:1.2}")

    ))

(use-package org-capture
  :init
  (defalias 'orca #'org-capture)
  :config
  (add-to-list 'org-capture-templates
               '("k" "klocking"))
  (add-to-list 'org-capture-templates
               '("kc" "Item to Current Clocked Task" item
                 (clock)
                 "%i%?" :empty-lines 1))
  (add-to-list 'org-capture-templates
               '("kC" "Contents to Current Clocked Task" plain
                 (clock)
                 "%i" :immediate-finish t :empty-lines 1))
  (add-to-list 'org-capture-templates

               '("kK" "Kill-ring to Current Clocked Task" plain
                 (clock)
                 "%c" :immediate-finish t :empty-lines 1))

  (defun region-to-clocked-task (start end)
    "Copies the selected text to the currently clocked in org-mode task."
    (interactive "r")
    (org-capture-string (buffer-substring-no-properties start end) "C"))

  (global-set-key (kbd "C-<F12>") 'region-to-clocked-task)

  )
(use-package org-trello
:defer 1
  :mode ("\\.trello\\'" . org-mode)
  :interpreter "org"
  :ensure t
  :functions org-trello
  :config
  (message "now `org-trello' is loaded")
  :commands (org-trello-mode))


(use-package notmuch
  :commands notmuch notmuch-search-reply-to-thread-sender
  :defer 8
  :config

  (define-key notmuch-hello-mode-map "g"
    'notmuch-poll-and-refresh-this-buffer)

  (define-key notmuch-hello-mode-map "f"
    (lambda ()
      (interactive)
      (notmuch-hello-search "tag:followup")))

  (define-key notmuch-hello-mode-map "i"
    (lambda ()
      (interactive)
      (notmuch-hello-search "tag:inbox")))

  (define-key notmuch-hello-mode-map "u"
    (lambda ()
      (interactive)
      (notmuch-hello-search "tag:unread")))

  (define-key notmuch-hello-mode-map "a"
    (lambda ()
      (interactive)
      (notmuch-hello-search "tag:archive")))

  (define-key notmuch-search-mode-map "g"
    'notmuch-poll-and-refresh-this-buffer)

  (define-key notmuch-search-mode-map "K"
    (lambda ()
      "sKip: toggle deleted tag for thread"
      (interactive)
      (if (member "skipped" (notmuch-search-get-tags))
          (notmuch-search-tag '("-skipped"))
        (notmuch-search-tag '("+skipped" "-inbox" "-unread")))))


  (define-key notmuch-search-mode-map "D"
    (lambda ()
      "Loose: toggle deleted tag for thread"
      (interactive)
      (if (member "deleted" (notmuch-search-get-tags))
          (notmuch-search-tag '("-deleted"))
        (notmuch-search-tag '("+deleted" "-inbox" "-unread")))))

  (define-key notmuch-search-mode-map "F"
    (lambda ()
      "toggle followup tag for thread"
      (interactive)
      (if (member "followup" (notmuch-search-get-tags))
          (notmuch-search-tag '("-followup"))
        (notmuch-search-tag '("+followup")))))

  (define-key notmuch-search-mode-map "!"
    (lambda ()
      "toggle unread tag for thread"
      (interactive)
      (if (member "unread" (notmuch-search-get-tags))
          (notmuch-search-tag '("-unread"))
        (notmuch-search-tag '("+unread")))))

  (define-key notmuch-search-mode-map "a"
    (lambda ()
      "toggle archive"
      (interactive)
      (if (member "archive" (notmuch-search-get-tags))
          (notmuch-search-tag '("-archive"))
        (notmuch-search-tag '("+archive" "-inbox" "-unread")))))

  :ensure t)

(use-package org-notmuch


  :after (org notmuch))

(use-package helm-notmuch
  :defer 8
  :after notmuch helm
  :bind
  (:map helm-command-map
        ("m" . helm-notmuch))
  :config
  (define-key notmuch-hello-mode-map "S" 'helm-notmuch)
  (define-key notmuch-show-mode-map "S" 'helm-notmuch)
  (define-key notmuch-search-mode-map "S" 'helm-notmuch)
  :ensure t)

(use-package notmuch-show
  :defer t
  :config
  (define-key notmuch-show-mode-map "\C-c\C-o" 'browse-url-at-point)
  (define-key notmuch-show-mode-map "K"
    (lambda ()
      "mark message as SKIPPED to be considered for filtering to ignored"
      (interactive)
      (notmuch-show-tag (list "+SKIPPED" "-inbox"))))


  (define-key notmuch-show-mode-map "D"
    (lambda ()
      "Loose: toggle deleted tag for message"
      (interactive)
      (if (member "deleted" (notmuch-show-get-tags))
          (notmuch-show-tag '("-deleted"))
        (notmuch-show-tag '("+deleted" "-inbox" "-unread")))))


  (define-key notmuch-show-mode-map "F"
    (lambda ()
      "toggle followup"
      (interactive)
      (if (member "followup" (notmuch-show-get-tags))
          (notmuch-show-tag '("-followup"))
        (notmuch-show-tag '("+followup" "-inbox" "-unread")))))

  (define-key notmuch-show-mode-map "A"
    (lambda ()
      "toggle archive"
      (interactive)
      (if (member "archive" (notmuch-show-get-tags))
          (notmuch-show-tag '("-archive"))
        (notmuch-show-tag '("+archive" "-inbox" "-unread"))))))

(use-package message-mode
  :commands message-mode
  :disabled nil
  :no-require t
  :defer t
  :config
  (message-default-mail-headers "
Cc:
X-Message-SMTP-Method: sendmail
")

  ;; This is needed to allow msmtp to do its magic:
  (setq-default message-sendmail-f-is-evil 't)

  ;;need to tell msmtp which account we're using
  (setq-default message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq mail-specify-envelope-from t)
  (setq-default mail-envelope-from 'header)
  (setq-default message-sendmail-envelope-from 'header)
  (setq-default message-send-mail-function 'message-send-mail-with-sendmail)

  ;;use msmtp instead of sendmail
  (setq-default sendmail-program "/usr/bin/msmtp")

  (setq message-kill-buffer-on-exit t)


  (add-hook 'message-mode-hook 'turn-on-flyspell 'append))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode "\\.ya?ml'"
  :init
  (defun yaml-mode-syntax-propertize-function (beg end)
  ;;; properly highlight comments in yaml
    (save-excursion
      (goto-char beg)
      (while (search-forward "#" end t)
        (save-excursion
          (forward-char -1)
          (if (bolp)
              (put-text-property (point) (1+ (point))
                                 'syntax-table (string-to-syntax "<"))
            (forward-char -1)
            (when (looking-at "[ \t]")
              (forward-char 1)
              (put-text-property (point) (1+ (point))
                                 'syntax-table (string-to-syntax "<")))))))))



(use-package helm

  :commands (helm-mini helm-M-x helm-all-mark-rings helm-show-kill-ring)
  :demand t
  :delight (helm-mode)
  :ensure t
  :init
  (setq helm-command-prefix-key "C-c h")

  (require 'helm-config)
  :bind (("M-x" . helm-M-x)
         ("C-c h"   . helm-command-prefix)
         ("C-c C-SPC" . helm-all-mark-rings) ; I modified the keybinding
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . bookmark-jump)
         ("C-h )" . helm-execute-kmacro)
         ("C-x C-b" . helm-mini) ;; (helm-bind-helm-mini)

         :map helm-command-map
         ("o" . helm-occur)
         (")" . helm-execute-kmacro)
         ;; ("f" . helm-find-files)
         ;; ("b" . helm-mini)
         ;;                ("g" . helm-google-suggest)
         ;; ("l" . helm-buffers-list)
         ;; ("p" . nil)
         ;; ("i" . helm-imenu)
         ;; ("r" . helm-register)    ; C-x r SPC and C-x r j
         ;; ("M-:" . helm-eval-expression-with-eldoc)

         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
         ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
         ("C-z" . helm-select-action) ; list actions using C-z

         :map minibuffer-local-map
         ("C-c C-l" . helm-minibuffer-history))


  :config

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))


  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t
        helm-locate-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t)

  (helm-mode 1))



(use-package helm-descbinds                        ; (helm-descbinds-setup)
  :ensure t
  :defer 8
  :after helm
  :bind
  (:map helm-command-map
        ("hk" . helm-descbinds))
  :init
  (fset 'describe-bindings 'helm-descbinds)
  )

(use-package helm-describe-modes
  :after helm
  :defer 8
  :ensure t
  :bind
  (:map helm-command-map
        ("hmm" . helm-describe-modes))
  )


(use-package helm-org
  :ensure t
  :defer 8
  :after (org helm)
  :defer t

  )

(use-package helm-org-rifle
  :ensure t
  :defer 8
  :after (org helm helm-org)
  :defer t
  :bind (:map helm-command-map
              ("R" . helm-org-rifle))
  )

(use-package helm-swoop
  :defer 8
  :ensure t
  :bind
  (("C-S-s" . helm-swoop)
   ("M-i" . helm-swoop)
   ("M-s s" . helm-swoop)
   ("M-s M-s" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   )
  :after helm
  :config
  (progn
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)))


(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups '(
                                  (:name "Important"
                                         :priority "A")

                                  (:name "Alltodo Items"
                                         :time-grid nil
                                         :todo "TODO")
                                  (:name "Next Items"
                                         :time-grid t
                                         :tag ("NEXT" "outbox"))
                                  (:name "Today"
                                         :time-grid t
                                         :scheduled today)

                                  (:priority<= "B"
                                               :order 1)
                                  (:name "Due today"
                                         :deadline today)

                                  (:name "Overdue"
                                         :deadline past)
                                  (:name "Due soon"
                                         :deadline future)
                                  (:name "Waiting"
                                         :todo "WAIT")
                                  (:order-multi (1 (:name "Done today"
                                                          :and (:regexp "State \"DONE\""
                                                                        :log t))
                                                   (:name "Clocked today"
                                                          :log t)))) ))

(use-package rainbow-mode
  :ensure t
  )
(use-package dired
  :commands (dired-jump-other-window dired dired-undo dired-jump)
  :defer 1
  :bind
  (:map dired-mode-map
        ("C-c C-w" . 'browse-url-of-dired-file)
        ("*." . 'dired-mark-extension))

  :init
  (defun my-dired-mode-setup ()
    "to be run as hook for `dired-mode'."
    (dired-hide-details-mode 1))
  :config

  (setq image-dired-thumb-height 400
        image-dired-thumb-size 400
        image-dired-thumb-width 400)

  (setq
   dired-dwim-target t            ;; (dired-dwim-target)
   dired-recursive-copies 'always         ; "always" means no asking
   dired-recursive-deletes 'top           ; "top" means ask once for top level directory
   dired-listing-switches "-lha"          ; human-readable listing
   )

  (add-hook 'dired-mode-hook 'my-dired-mode-setup)
  (add-hook 'dired-mode-hook 'auto-revert-mode) ;; (dired-auto-revert-mode)

  ;; if it is not Windows, use the following listing switches
  (when (not (eq system-type 'windows-nt))
    (setq dired-listing-switches "-lha --group-directories-first"))


  (define-key ctl-x-4-map "\C-j" 'dired-jump-other-window))


(use-package outshine
  :defer 1
  :disabled t
  :config

  (defun -add-font-lock-kwds (FONT-LOCK-ALIST)
    (font-lock-add-keywords
     nil (--map (-let (((rgx uni-point) it))
                  `(,rgx (0 (progn
                              (compose-region (match-beginning 1) (match-end 1)
                                              ,(concat "\t" (list uni-point)))
                              nil))))
                FONT-LOCK-ALIST)))

  (defmacro add-font-locks (FONT-LOCK-HOOKS-ALIST)
    `(--each ,FONT-LOCK-HOOKS-ALIST
       (-let (((font-locks . mode-hooks) it))
         (--each mode-hooks
           (add-hook it (-partial '-add-font-lock-kwds
                                  (symbol-value font-locks)))))))

  (defconst emacs-outlines-font-lock-alist
    ;; Outlines
    '(("\\(^;;;\\) "          ?■)
      ("\\(^;;;;\\) "         ?○)
      ("\\(^;;;;;\\) "        ?✸)
      ("\\(^;;;;;;\\) "       ?✿)))

  (defconst lisp-outlines-font-lock-alist
    ;; Outlines
    '(("\\(^;; \\*\\) "          ?■)
      ("\\(^;; \\*\\*\\) "       ?○)
      ("\\(^;; \\*\\*\\*\\) "    ?✸)
      ("\\(^;; \\*\\*\\*\\*\\) " ?✿)))

  (defconst python-outlines-font-lock-alist
    '(("\\(^# \\*\\) "          ?■)
      ("\\(^# \\*\\*\\) "       ?○)
      ("\\(^# \\*\\*\\*\\) "    ?✸)
      ("\\(^# \\*\\*\\*\\*\\) " ?✿)))

  (add-font-locks
   '((emacs-outlines-font-lock-alist emacs-lisp-mode-hook)
     (lisp-outlines-font-lock-alist clojure-mode-hook hy-mode-hook)
     (python-outlines-font-lock-alist python-mode-hook)))

  (setq outshine-use-speed-commands t)

  ;; Easier navigation for source files, especially this one.

  :bind (:map outshine-mode-map
              ("<S-iso-lefttab>" . outshine-cycle-buffer)
              ("<backtab>" . outshine-cycle-buffer)
              )
  :hook (emacs-lisp-mode . outshine-mode)
  :ensure t
  )

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-c m c") 'mc/edit-lines))

(use-package aweshell
  :commands (aweshell-new
             aweshell-next
             aweshell-prev
             aweshell-clear-buffer
             aweshell-sudo-toggle
             aweshell-switch-buffer
             aweshell-dedicated-toggle
             aweshell-dedicated-open
             aweshell-dedicated-close)
  :bind
  (("M-e" . aweshell-toggle)
   ("M-e" . aweshell-dedicated-toggle)
   :map eshell-mode
   ("C-S-l" . aweshell-sudo-toggle)
   ("M-'" . aweshell-search-history)
   ("C-l" . aweshell-clear-buffer))
  )
(use-package helm-projectile
  :ensure t
  :defer 1
  :after (helm projectile)
  :bind ( ("C-c h Pf" . helm-projectile-find-file)
          ("C-c h Pd" . helm-projectile-find-dir)
          ("C-c h Pp" . helm-projectile)
          :map helm-command-map
          ("P" . nil)
          ("Pf" . helm-projectile-find-file)
          ("Pd" . helm-projectile-find-dir)
          ("Pp" . helm-projectile))
  :config
  (helm-projectile-on))

(use-package org-projectile-helm
  :ensure t
  :defer 1
  :after (org-projectile helm-projectile)
  :bind (:map helm-command-map
              ("Pp" . org-projectile-helm-template-or-project)))

(use-package org-projectile ;;
  :bind (("C-c P" . juicepie)
         :map helm-command-map
         ("Pc" . juicepie))
  :defer
  :after (projectile org)
  :commands (org-projectile-project-todo-completing-read)
  :init
  (defalias 'juicepie #'org-projectile-project-todo-completing-read)
  :config
  (progn
    (setq org-projectile-projects-file "~/org/campaigns.org")
    ;;    (add-to-list 'org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    )
  :ensure t)

(use-package projectile
  :ensure t
  :after helm
  :defer 1
  :commands (gopro projectile-switch-project projectile-open-project projectile-find-file)
  :bind (("C-c g p o" . chops)
         ("C-c g p g". gopro)
         ("C-c g p f" . projectile-find-file)
         :map helm-command-map
         ("P" . nil)
         ("Po" . chops)
         ("Pg". gopro))
  :diminish projectile-mode
  :delight '(:eval
             (propertize (concat " " (projectile-project-name))
                         'face '( :weight bold  :foreground "#FD9711" :background "#111")))
  :init
  (defalias 'gopro #'projectile-switch-project)
  (defalias 'chops #'projectile-switch-open-project)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line
        '(:eval
          (format " Proj[%s]"
                  (projectile-project-name)))))

(use-package ansible
  :ensure t
  :init
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

(use-package ansible-doc
  :ensure t
  :after ansible
  :init
  (add-hook 'yaml-mode-hook #'ansible-doc-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode t)
 '(inhibit-startup-screen t)
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "archive2017" :query "date:2017")
     (:name "tomymovieguru" :query "to:\"c.avgulas@mymovie.guru\""))))
 '(org-agenda-files
   (quote
    ("/home/dniwdeus/org/org.org" "/home/dniwdeus/org/appguru.org" "/home/dniwdeus/org/quicklog.org")))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   (quote
    (ansible-doc ansible org-projectile-helm helm-projectile projectile multiple-cursors password-store forge org-trello org-super-agenda helm-swoop helm-org-rifle helm-org helm-describe-modes helm-descbinds org-notmuch yaml-mode helm-notmuch notmuch org-plus-contrib which-key magit use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f413f" :foreground "#d5dcd5" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 198 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "brightyellow" :weight bold :height 1.2))))
 '(custom-variable-tag ((t (:foreground "cyan" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "deep sky blue"))))
 '(font-lock-keyword-face ((t (:foreground "color-141"))))
 '(font-lock-string-face ((t (:foreground "color-207"))))
 '(helm-selection ((t (:background "dark orange" :distant-foreground "black"))))
 '(highlight ((t (:background "color-53"))))
 '(link ((t (:foreground "brightcyan" :underline t))))
 '(link-visited ((t (:inherit link :foreground "brightmagenta"))))
 '(magit-bisect-bad ((t (:foreground "red"))))
 '(minibuffer-prompt ((t (:foreground "brightwhite" :height 1.5)))))
