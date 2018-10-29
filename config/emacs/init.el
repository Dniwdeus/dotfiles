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

;; configure magit

(use-package magit
  :commands (magit-status magit-blame magit-mode)
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c C-g c" . magit-commit)
         ("C-c C-g f" . magit-grep))
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
    :bind ("C-c C-g b" . magit-blame-mode))
 )

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



(setq-default indent-tabs-mode nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (deeper-blue)))
 '(package-selected-packages (quote (which-key magit use-package yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq backup-directory-alist `((".*" . ,temporary-file-directory))) (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(delete-selection-mode 1)
