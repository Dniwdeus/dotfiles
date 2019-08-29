;; setup package manager for emacs
(require 'package)
(setq package-enable-at-startup t)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; bootstrap use package [fn:11]


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; for now keep usepackage verbose.
(setq use-package-verbose t
      use-package-expand-minimally nil
      use-package-compute-statistics t)

;; load global configs for emacs
(load-file "~/.emacs.d/elisp/global-settings.el")

;; install org plus contrib with either package.el[fn:12]

(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load
(unless (package-installed-p 'org-plus-contrib)  ;; Make sure the Org package is
  (package-install 'org-plus-contrib))           ;; installed, install it if not
(package-initialize)                ;; Initialize & Install Package


;; load restclient
(use-package restclient
  :ensure t
  :defer t)
(use-package ob-restclient
  :ensure t
  :defer t)

;; configure magit

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

;; git timemachine
(use-package git-timemachine
  :bind ("C-c C-g h" . git-timemachine)
  :defer 1
  :diminish
  :ensure t)


;; load ecloud into emacs
(use-package s
  :ensure t)
(use-package f
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/packages/asoc/")
(require 'asoc)

(add-to-list 'load-path "~/.emacs.d/packages/ecloud/")
(require 'ecloud)

;; load org-board into emacs

(use-package org-board
  :bind (
         ("C-c bo" . obo)
         ("C-c ba" . oba))
  :init
  (defalias 'obo #'org-board-open)
  (defalias 'oba #'org-board-archive)
  :ensure t)

;; configure which-key to remember key-bindings

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



    ;; (setq org-agenda-custom-commands
    ;;       '(("@" "Contexts"
    ;;          ((tags-todo "@email"
    ;;                      ((org-agenda-overriding-header "Emails")))
    ;;           (tags-todo "@phone"
    ;;                      ((org-agenda-overriding-header "Phone")))))))
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
  :config (load-file "~/.emacs.d/notmuch-config.el")
  :disabled t
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (org-plus-contrib which-key magit use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
