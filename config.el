;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; NOTE(Felix): Todos left
;;  - [X] Highlight todos in code
;;  - [X] Fragtog delay super high
;;  - [X] Magit
;;  - [X] Fixed Dir locals such that nice headers in vault
;;  - [X] Org bullets for non vault org
;;  - [X] More packages for latex preview
;;  - [X] bind C-o to embark-export in vertico mode
;;  - [X] latex scale to small
;;  - [X] roam find not listing tags
;;  - [X] make C-c f p work
;;  - [X] startup opening neotree correctly
;;  - [X] intall biblio
;;  - [X] org roam capture templates
;;  - [X] C-y (yank) can now insert images straigt into org buffers
;;  - [X] org insert screenshot from clipboard (Windows)
;;  - [X] knowledge base export -> wiki website
;;  - [X] ws-butler (?) keeps messing up whitespace on save -> should only delete end of line whitespace (not on current line though)
;;  - [X] indent pasted images the same as the cursor
;;  - [X] never export :toc: sections? -> like noexport
;;  - [X] Autocomplete popup temporarily destroys inline images under the popup
;;  - [X] undo can't be undone
;;  - [X] fixed latex header for previews (use #+latex_header_extra: to exclude from previews)
;;  - [X] C-j does not work in org
;;  - [X] knowledge base export -> .bib file
;;  - [X] org-fill-paragraph should not remove latex preview

;;  - [ ] org insert screenshot from clipboard (Linux)
;;  - [ ] C-s (consult-line) does not work with search text that spans multiple lines


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name    "Felix Brendel"
      user-mail-address "felix@brendel.io")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)

;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;; (setq doom-font         (font-spec :family "randy-gg" :size 13.5))
(setq doom-font         (font-spec :family "noto sans mono" :size 12.0))

(setq doom-modeline-major-mode-icon t
      doom-unicode-font        doom-font
      doom-variable-pitch-font doom-font)

(prefer-coding-system 'utf-8)

(set-selection-coding-system
 (if (eq system-type 'windows-nt)
     'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
   'utf-8))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-nova)
(setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'frappe) ;; or 'latte, 'macchiato, or 'mocha
;; (catppuccin-reload)

(setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'")

(use-package! sqlite)
(use-package! sly)
(use-package! rg)
(use-package! flycheck)

(use-package! neotree
  :custom
  (doom-themes-neotree-enable-variable-pitch nil)
  (doom-themes-neotree-file-icons 'fancy)
  (neo-hidden-regexp-list
      '("^\\.\\(?:git\\|hg\\|svn\\)$"
        "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
        "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
        "^\\.\\(?:sync\\|export\\|attach\\)$"
        "~$"
        "^#.*#$"
        "^\\.archives$"
        "\\.\\(?:db\\|db-shm\\|db-wal\\|log\\|orgids\\|projectile\\|dir-locals.el\\)$"))
  (neo-show-hidden-files nil))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory                (or (getenv "ORG-DIR")          "~/org/")
      org-exported-vault-directory (or (getenv "ORG-VAULT-EXPORT") "~/knowledge_base/"))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! citar-org-roam
  :after  (citar org-roam)
  :config (citar-org-roam-mode))

(use-package! company-posframe
  :config
  (company-posframe-mode 1))
(use-package! ssh-agency :defer t)
(use-package! valign)
; (use-package! org-preview)

(use-package! form-feed
  :hook (prog-mode . form-feed-mode)
        (org-mode . form-feed-mode))

(use-package! biblio)

(use-package! org-roam-timestamps
  :hook (org-mode . org-roam-timestamps-mode))

(use-package! yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


(use-package! orderless
  :init
  (setq completion-styles '(substring orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package! ob-sql-mode :ensure t)
(require 'ob-sql-mode)

(use-package! dap-mode
  :config
  (require 'dap-mode)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  (dap-mode 1)
  ;; The modes below are optional
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1))

(use-package! multiple-cursors
  :config
  (after! multiple-cursors-core
    (setq mc/list-file (concat doom-private-dir "mc-settings.el"))))
(use-package org-fragtog
  :custom (org-fragtog-preview-delay 99999999999999999)
  :bind   (:map org-mode-map ("<f2>" . my-org-f2))
  :hook   (org-mode . org-fragtog-mode))

(use-package! org-roam
  :init
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.25)
                 (window-parameters . ((no-delete-other-windows . t)))))
  (setq org-roam-v2-ack t)
  :custom
  ;; NOTE(Felix): dir locals for vault
  ;; ((org-mode . ((eval . (progn
  ;;                         (face-remap-add-relative 'org-document-title '(:height 2.4 :family "Times New Roman" :weight bold))
  ;;                         (face-remap-add-relative 'org-level-1        '(:height 2.0 :family "Times New Roman"))
  ;;                         (face-remap-add-relative 'org-level-2        '(:height 1.8 :family "Times New Roman"))
  ;;                         (face-remap-add-relative 'org-level-3        '(:height 1.6 :family "Times New Roman"))
  ;;                         (face-remap-add-relative 'org-level-4        '(:height 1.4 :family "Times New Roman"))
  ;;                         (face-remap-add-relative 'org-level-5        '(:height 1.2 :family "Times New Roman"))
  ;;                         (setq-local org-bullets-bullet-list '(" ")))))))
  (org-roam-directory (concat org-directory "vault/"))
  (org-roam-db-location (concat org-directory "vault/.roam.db"))
  (org-roam-capture-templates
   '(("d" "General Notes" plain "%?" :target
      (file+head "Default/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n")
      :unnarrowed t)
     ("m" "Music" plain "%?" :target
      (file+head "Music/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+setupfile: setupfile.org\n#+filetags: :music:\n")
      :unnarrowed t)
     ("k" "Korean" plain "%?" :target
      (file+head "Korean/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+filetags: :korean:\n")
      :unnarrowed t)
     ("p" "Notes for Programming things")
     ("pw" "Win32 API" plain "%?" :target
      (file+head "Programming/win32/${slug}.org" "#+title: [win32] ${title}\n#+date: %<%F>\n#+filetags: :win32:\n")
      :unnarrowed t)
     ("pv" "Vulkan API" plain "%?" :target
      (file+head "Programming/vulkan/${slug}.org" "#+title: [vk] ${title}\n#+date: %<%F>\n#+filetags: :vulkan:\n")
      :unnarrowed t)
     ("pg" "OpenGL" plain "%?" :target
      (file+head "Programming/opengl/${slug}.org" "#+title: [gl] ${title}\n#+date: %<%F>\n#+filetags: :opengl:\n")
      :unnarrowed t)
     ("pj" "jai" plain "%?" :target
      (file+head "Programming/jai/${slug}.org" "#+title: [jai] ${title}\n#+date: %<%F>\n#+filetags: :jai:\n")
      :unnarrowed t)
     ("pp" "general" plain "%?" :target
      (file+head "Programming/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+filetags: :programming:\n")
      :unnarrowed t)

     ("u" "Notes for Uni")
     ("uc" "Uni/Computer Architecture" plain "%?" :target
      (file+head "Uni/Computer Architecture/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+filetags: :computer-architecture:\n")
      :unnarrowed t)
     ("ud" "Uni/Databases" plain "%?" :target
      (file+head "Uni/Databases/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+filetags: :databases:\n")
      :unnarrowed t)
     ("ug" "Uni/Graphics" plain "%?" :target
      (file+head "Uni/Graphics/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+filetags: :computer-graphics:\n")
      :unnarrowed t)
     ("uq" "Uni/Quantum Computing" plain "%?" :target
      (file+head "Uni/Quantum Computing/${slug}.org" "#+latex_header: \\usepackage{qcircuit}\n#+latex_header: \\usepackage{braket}\n#+title: ${title}\n#+date: %<%F>\n#+filetags: :quantum-computing:\n")
      :unnarrowed t)
     ("ul" "Uni/Deep Learning" plain "%?" :target
      (file+head "Uni/Deep Learning/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+filetags: :deep-learning:\n")
      :unnarrowed t)
     ("um" "Uni Math")
     ("umm" "Uni Math General" plain "%?" :target
      (file+head "Uni/Math/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+filetags: :math:\n")
      :unnarrowed t)
     ("umc" "Uni Math Calculus" plain "%?" :target
      (file+head "Uni/Math/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+filetags: :math:calculus:\n")
      :unnarrowed t)
     ("ump" "Uni Math Probability" plain "%?" :target
      (file+head "Uni/Math/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+filetags: :math:probability:\n")
      :unnarrowed t)
     ("uml" "Uni Math Linear Algebra" plain "%?" :target
      (file+head "Uni/Math/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+filetags: :math:linear-algebra:\n")
      :unnarrowed t)
     ("uu" "Uni General" plain "%?" :target
      (file+head "Uni/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+filetags: :chem:\n")
      :unnarrowed t)

     ("c" "Uni/Chemie" plain "%?" :target
      (file+head "Uni/Chemie/${slug}.org" "#+title: ${title}\n#+date: %<%F>\n#+setupfile: setupfile.org\n")
      :unnarrowed t)
     ))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         ("<f5>"  . org-redisplay-inline-images))
  :config
  ;; NOTE(Felix): hopefullly teporary fix
;;   (progn
;;     (require 'emacsql-sqlite3)
;;     (defun org-roam-db ()
;;       "Entrypoint to the Org-roam sqlite database.
;; Initializes and stores the database, and the database connection.
;; Performs a database upgrade when required."
;;       (unless (and (org-roam-db--get-connection)
;;                    (emacsql-live-p (org-roam-db--get-connection)))
;;         (let ((init-db (not (file-exists-p org-roam-db-location))))
;;           (make-directory (file-name-directory org-roam-db-location) t)
;;           ;; (let ((conn (emacsql-sqlite org-roam-db-location)))
;;           (let ((conn (emacsql-sqlite3 org-roam-db-location)))
;;             (emacsql conn [:pragma (= foreign_keys ON)])
;;             (set-process-query-on-exit-flag (emacsql-process conn) nil)
;;             (puthash (expand-file-name org-roam-directory)
;;                      conn
;;                      org-roam-db--connection)
;;             (when init-db
;;               (org-roam-db--init conn))
;;             (let* ((version (caar (emacsql conn "PRAGMA user_version")))
;;                    (version (org-roam-db--upgrade-maybe conn version)))
;;               (cond
;;                ((> version org-roam-db-version)
;;                 (emacsql-close conn)
;;                 (user-error
;;                  "The Org-roam database was created with a newer Org-roam version.  "
;;                  "You need to update the Org-roam package"))
;;                ((< version org-roam-db-version)
;;                 (emacsql-close conn)
;;                 (error "BUG: The Org-roam database scheme changed %s"
;;                        "and there is no upgrade path")))))))
;;       (org-roam-db--get-connection))
;;     (defun org-roam-db--init (db)
;;       "Initialize database DB with the correct schema and user version."
;;       (emacsql-with-transaction db
;;         ;; (emacsql db "PRAGMA foreign_keys = ON")
;;         (emacsql db [:pragma (= foreign_keys ON)])
;;         (pcase-dolist (`(,table ,schema) org-roam-db--table-schemata)
;;           (emacsql db [:create-table $i1 $S2] table schema))
;;         (pcase-dolist (`(,index-name ,table ,columns) org-roam-db--table-indices)
;;           (emacsql db [:create-index $i1 :on $i2 $S3] index-name table columns))
;;         (emacsql db (format "PRAGMA user_version = %s" org-roam-db-version))))
;;     )

  (org-roam-setup))

(use-package! websocket :after org-roam)
;; (use-package! org-roam-ui :after org-roam
;;     :config (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line.
   source: https://stackoverflow.com/a/998472"
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(defun lookup-docs-for-symbol-at-point ()
  (interactive)
    (+lookup/online (thing-at-point 'symbol) "DevDocs.io"))

(defun lookup/definition-other-window ()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (my-lookup/definition)
  (other-window 1))

(defun my-lookup/definition ()
  (interactive)
  ;; (if (eq major-mode 'jai-mode)
      ;; (let ((root-dir   (projectile-project-root))
            ;; (identifier (doom-thing-at-point-or-region)))

        ;; (if (null identifier) (user-error "Nothing under point"))

        ;; ;; (projectile-ripgrep (concat "(?:(\\s+)|\\()(" identifier ")(?:\\s*:)") t)
        ;; ;; (projectile-ripgrep (concat "" identifier "(?:\\s*:)") t)
        ;; (projectile-ripgrep (concat "(" identifier "\\s*:)") t)
        ;; )
      ;; if not jai mode
      (when (call-interactively #'+lookup/definition)
        (recenter-top-bottom 3))
      ;; )
  )

(defun mark-word-or-next-word-like-this ()
  "if there is no active region the word under
   the point will be marked, otherwise the next word is selected."
  (interactive)
  (if (region-active-p)
      ;; then
      (progn
        (mc/mark-next-like-this 1)
        (mc/cycle-forward)
        (mc/maybe-multiple-cursors-mode))
    ;; else
    (mc--mark-symbol-at-point)))

(defmacro code-region (name &rest body)
  ;; NOTE(Felix): after first arg there is body and should be indented as body
  (declare (indent 1))
  (cons 'progn body))




(code-region "Compiling"
    (load-file (concat doom-private-dir "compilation.el")))

(map! :map LaTeX-mode-map
      "C-c C-e" (lambda () (interactive) (TeX-command "LaTeX" 'TeX-master-file)))

(map! :map vertico-map
      "C-o" 'embark-export)

(map! "C-s"         'consult-line
      "C-j"         'join-line
      "C-d"         'mark-word-or-next-word-like-this
      "C-S-d"       'duplicate-line
      "C-S-c C-S-c" 'mc/edit-lines
      "<f1>"        '+doom-dashboard/open
      "C-r"         'org-roam-node-insert
      "C-o"         'org-roam-node-find
      "C-#"         'comment-line
      "M-SPC"       'change-lang
      "M-d"         'lookup-docs-for-symbol-at-point
      "M-o"         'mc/vertical-align-with-space
      :leader "e"   'find-build-script-and-compile
      )

;; NOTE(Felix): make C-c f p not throw errors by rebinding it
(unbind-key "p" doom-leader-file-map)
(unbind-key "C-c f p" global-map)
(unbind-key "f p" mode-specific-map)
(map!  "C-c f p"     'doom/open-private-config)
(map! :map global-map
      "C-c f p" 'doom/open-private-config
      "M-." 'lookup/definition-other-window)


(setq +doom-dashboard-menu-sections
      '(;; ("Track Work for Bernd"
        ;;  :icon (nerd-icons-mdicon "nf-md-bag_checked" :face 'doom-dashboard-menu-title)
        ;;  :action open-bernd-work)
        ("Find node in vault"
         :icon (nerd-icons-mdicon "nf-md-magnify" :face 'doom-dashboard-menu-title)
         :action org-roam-node-find)
        ("Open random node from vault"
         :icon (nerd-icons-faicon "nf-fa-briefcase" :face 'doom-dashboard-menu-title)
         :action org-roam-node-random)
        ("Show knowledge graph"
         :icon (nerd-icons-mdicon "nf-md-graph_outline" :face 'doom-dashboard-menu-title)
         :action org-roam-ui-mode)
        ("Open Calendar"
         :icon (nerd-icons-codicon "nf-cod-calendar" :face 'doom-dashboard-menu-title)
         :action open-my-calendar)
        ("Open private configuration"
         :icon (nerd-icons-mdicon "nf-md-tools" :face 'doom-dashboard-menu-title)
         :when (file-directory-p doom-private-dir)
         :action doom/open-private-config)))

(code-region "Org Screenshots"
  (defun org-take-and-insert-screenshot ()
    "Take a screenshot into a time stamped unique-named file in ab `images'
   directory, next to the org-buffer and insert a link to this file."
    (interactive)
    (unless (derived-mode-p 'org-mode)
      (error "Not in org mode buffer"))
    (setq filename
          (concat "images/"
                  (file-name-nondirectory (buffer-file-name))
                  " -- "
                  (format-time-string "%d-%m-%Y_%H-%M-%S")
                  ".png"))

    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))

                                        ; take screenshot
    (when (eq system-type 'windows-nt)
      (shell-command "snippingtool /clip")
      (shell-command (concat "powershell -command "
                             "\"Add-Type -AssemblyName System.Windows.Forms;"
                             "if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {"
                             "    $image = [System.Windows.Forms.Clipboard]::GetImage();"
                             "    [System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png);"
                             "    Write-Output 'clipboard content saved as file'"
                             "} else {"
                             "    Write-Output 'clipboard does not contain image data'"
                             "}\"")))
    (when (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))

                                        ; insert into file if correctly taken
    (when (file-exists-p filename)
      (let ((col (current-column)))
        (insert (concat
                 "#+attr_org: :width 500\n"
                 (string-pad "" col)
                 "[[file:" filename "]]\n"))
        (org-redisplay-inline-images))))

  (defun system-clipboard-contains-image-p ()
    (interactive)
    (cond
      ((eq system-type 'windows-nt)  (let ((clip (w32-selection-targets 'CLIPBOARD)))
                                       (if (and (eq 'DataObject (aref clip 0))
                                                (eq 'BITMAP (aref clip 2)))
                                           t
                                         )))
      ((eq system-type 'gnu/linux)    nil
       ;; (error "TODO: implement this for linux")
       )))

  (defun org-paste-screenshot-from-clipboard ()
    (interactive)

    ;; should only work in org
    (unless (derived-mode-p 'org-mode)
      (error "Not in org mode buffer"))

    ;; make some file name
    (setq filename
          (concat "images/"
                  (file-name-nondirectory (buffer-file-name))
                  " -- "
                  (format-time-string "%d-%m-%Y_%H-%M-%S")
                  ".png"))

    ;; make sure the directory structure exists
    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))

    ;; write the image file to disk
    (when (eq system-type 'windows-nt)
      (shell-command (concat "powershell -command "
                             "\"Add-Type -AssemblyName System.Windows.Forms;"
                             "if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {"
                             "    $image = [System.Windows.Forms.Clipboard]::GetImage();"
                             "    [System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png);"
                             "    Write-Output 'clipboard content saved as file'"
                             "} else {"
                             "    Write-Output 'clipboard does not contain image data'"
                             "}\"")))

    (when (eq system-type 'gnu/linux)
      (error "TODO: implement this for linux"))

    ;; insert link into the buffer
    (when (file-exists-p filename)
      (let ((col (current-column)))
        (insert (concat
                 "#+attr_org: :width 500\n"
                 (string-pad "" col)
                 "[[file:" filename "]]\n"))
        (org-redisplay-inline-images)))
    )
  )


(code-region "c indentation"
  (c-add-style "FELIX"
               '("gnu"
                 (c-basic-offset . 4)     ; Guessed value
                 (c-offsets-alist
                  (arglist-cont . 0)      ; Guessed value
                  (arglist-intro . +)     ; Guessed value
                  (block-close . 0)       ; Guessed value
                  (brace-entry-open . 0)  ; Guessed value
                  (brace-list-close . 0)  ; Guessed value
                  (brace-list-entry . 0)  ; Guessed value
                  (brace-list-intro . +)  ; Guessed value
                  (defun-block-intro . +) ; Guessed value
                  (defun-close . 0)       ; Guessed value
                  (defun-open . 0)        ; Guessed value
                  (else-clause . 0)       ; Guessed value
                  (func-decl-cont . +)    ; Guessed value
                  (inline-close . 0)      ; Guessed value
                  (innamespace . +)       ; Guessed value
                  (namespace-close . 0)   ; Guessed value
                  (statement . 0)         ; Guessed value
                  (statement-block-intro . +) ; Guessed value
                  (statement-cont . +)    ; Guessed value
                  (substatement . +)      ; Guessed value
                  (substatement-open . 0) ; Guessed value
                  (topmost-intro . 0)     ; Guessed value
                  (access-label . -)
                  (annotation-top-cont . 0)
                  (annotation-var-cont . +)
                  (arglist-close . c-lineup-close-paren)
                  (arglist-cont-nonempty . c-lineup-arglist)
                  (block-open . 0)
                  (brace-list-open . +)
                  (c . c-lineup-C-comments)
                  (case-label . +)
                  (catch-clause . 0)
                  (class-close . 0)
                  (class-open . 0)
                  (comment-intro . c-lineup-comment)
                  (composition-close . 0)
                  (composition-open . 0)
                  (cpp-define-intro c-lineup-cpp-define +)
                  (cpp-macro . -1000)
                  (cpp-macro-cont . +)
                  (do-while-closure . 0)
                  (extern-lang-close . 0)
                  (extern-lang-open . 0)
                  (friend . 0)
                  (inclass . +)
                  (incomposition . +)
                  (inexpr-class . +)
                  (inexpr-statement . +)
                  (inextern-lang . +)
                  (inher-cont . c-lineup-multi-inher)
                  (inher-intro . +)
                  (inlambda . 0)
                  (inline-open . 0)
                  (inmodule . +)
                  (knr-argdecl . 0)
                  (knr-argdecl-intro . 5)
                  (label . 0)
                  (lambda-intro-cont . +)
                  (member-init-cont . c-lineup-multi-inher)
                  (member-init-intro . +)
                  (module-close . 0)
                  (module-open . 0)
                  (namespace-open . 0)
                  (objc-method-args-cont . c-lineup-ObjC-method-args)
                  (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                  (objc-method-intro .
                   [0])
                  (statement-case-intro . +)
                  (statement-case-open . +)
                  (stream-op . c-lineup-streamop)
                  (string . -1000)
                  (substatement-label . 0)
                  (template-args-cont c-lineup-template-args +)
                  (topmost-intro-cont first c-lineup-topmost-intro-cont c-lineup-gnu-DEFUN-intro-cont))))
  (setq c-default-style "FELIX")

  (defun my-c-mode-hook ()
    (lsp)
    (bind-key* "C-d" #'mark-word-or-next-word-like-this)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'case-label '+)
    (c-set-offset 'brace-list-intro '+)
    (c-set-offset 'brace-list-close 0))

  (add-hook 'prog-mode-hook 'which-function-mode)
  (add-hook 'java-mode-hook 'my-c-mode-hook)
  (add-hook 'c-mode-hook    'my-c-mode-hook)
  (add-hook 'c++-mode-hook  'my-c-mode-hook)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'compilation-mode (lambda () (toggle-truncate-lines -1)))
  (add-hook 'compilation-minor-mode-hook (lambda () (toggle-truncate-lines -1)))
  (add-hook 'comint-mode-hook (lambda () (toggle-truncate-lines -1)))
  ;; (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  (add-hook 'org-mode-hook #'hl-todo-mode)

  (font-lock-add-keywords
   'c++-mode
   '(("\\<\\(defer\\|if_debug\\|panic_if\\|panic\\)\\>" . font-lock-keyword-face)))
  )

(code-region "remedybg"
  (defun get-pid-of-process-name (pids name)
    (when pids
      (if (string= (alist-get 'comm (process-attributes (car pids)))
                   name)
          (car pids)
        (get-pid-of-process-name (cdr pids) name))))

  (defun system-process-running-p (name)
    (let ((pids (list-system-processes)))
      (and (get-pid-of-process-name pids name) t)))

  (defun rbd-break-here ()
    (interactive)
    (when (buffer-file-name)
      (unless (system-process-running-p "remedybg.exe")
        (async-shell-command "remedybg.exe" "rmd open")
        (sleep-for 1))

      (shell-command (concat "remedybg.exe open-file "
                                   (buffer-file-name)
                                   " "
                                   (format "%d" (line-number-at-pos))))

      (shell-command (concat "remedybg.exe add-breakpoint-at-file "
                                   (buffer-file-name)
                                   " "
                                   (format "%d" (line-number-at-pos)))
                           "rmd openfile")
      ))
  )

(code-region "lsp stuff"
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
                 '(jai-mode . "jai"))

    ;; (lsp-register-client
     ;; (make-lsp-client :new-connection (lsp-stdio-connection "jails")
                      ;; :activation-fn (lsp-activate-on "jai")
                      ;; :server-id 'jails))

  ;; (add-hook 'jai-mode-hook (lambda () (lsp)))
  ))

(code-region "Org babel"

  (code-region "jupyter"
    (let ((jupyter-present (not (not (executable-find "jupyter")))))
      (org-babel-do-load-languages
       'org-babel-load-languages
       `((emacs-lisp . t) ;; Other languages
         (shell      . t)
         (sqlite     . t)
         (python     . t)
         (jupyter    . ,jupyter-present)))

      (when jupyter-present
        (use-package! jupyter)
        (require 'jupyter)
        (require 'ob-jupyter)
        (org-babel-jupyter-override-src-block "python")
        (setq ob-async-no-async-languages-alist '("python" "jupyter-python"))
        (setq org-babel-default-header-args:python '((:async . "yes")
                                                     (:kernel . "python3")
                                                     (:session . "python-default-session")))))
    )


  (defun execute-command-and-insert-as-org-result (org-buffer exe-file on-done)
    (lexical-let ((previous-compilation-hooks compilation-finish-functions)
                  (buffer    org-buffer)
                  (on-done   on-done))

      (setq compilation-finish-functions
            (lambda (comp-buffer result)
              (compilation-minor-mode 1)
              (setq compilation-finish-functions previous-compilation-hooks)
              (let* ((comp-result-str (buffer-substring-no-properties
                                       (point-min) (point-max)))
                     (stripped-comp-result-str
                      (string-join (butlast (nthcdr 4 (split-string comp-result-str "\n")) 2)
                                   "\n")))

                (switch-to-buffer buffer)
                (org-babel-insert-result stripped-comp-result-str)
                (funcall on-done)))))

    (compilation-start exe-file t)
    (other-window 1))


  (defun run-pascal-org-src-bloc ()
    (interactive)

    (lexical-let ((code (cadr (org-babel-get-src-block-info t)))
                  (used-register 2))
      (window-configuration-to-register used-register)
      (when code
        (lexical-let* ((hash            (abs (sxhash code)))
                       (pascal-file-dir (temporary-file-directory))
                       (curr-dir        default-directory)
                       (curr-buffer     (current-buffer))
                       (pascal-file     (format "%spascal_%d.pp"  pascal-file-dir hash))
                       (exe-file        (format "%spascal_%d.exe" pascal-file-dir hash)))

          (with-temp-file pascal-file
            (insert code))

          (lexical-let ((previous-compilation-hooks compilation-finish-functions))
            (setq compilation-finish-functions
                  (lambda (comp-buffer result)
                    (setq compilation-finish-functions previous-compilation-hooks)
                    (when (string= result "finished\n")
                      ;;(switch-to-buffer comp-buffer)
                      (quit-window nil (get-buffer-window comp-buffer))
                      (execute-command-and-insert-as-org-result curr-buffer
                                                                exe-file
                                                                (lambda () (jump-to-register used-register)))))))

          (cd pascal-file-dir)
          (+org-clear-babel-results-h)
          (compilation-start (format "fpc pascal_%d" hash) t)
          (cd curr-dir)))))

  (defun my-org-babel-execute-src-block ()
    (interactive)
    (let ((code-info (org-babel-get-src-block-info t)))
      (if code-info
          (if (string= (car code-info) "pascal")
              (run-pascal-org-src-bloc)
            (org-babel-execute-src-block))
        (org-ctrl-c-ctrl-c)
        )))

  (bind-key "C-c C-c" #'my-org-babel-execute-src-block org-mode-map)

  ;; to correctly render the colored error messages
  (defun display-ansi-colors ()
    (ansi-color-apply-on-region (point-min) (point-max)))
  (add-hook 'org-babel-after-execute-hook #'display-ansi-colors)
  )

(code-region "Org config"
  (use-package citar
    :no-require
    :custom
    (org-cite-global-bibliography
     (list (expand-file-name (concat org-directory "bib.bib"))
           (expand-file-name (concat org-directory "vault.bib"))))
    (org-cite-insert-processor 'citar)
    (org-cite-follow-processor 'citar)
    (org-cite-activate-processor 'citar)
    (citar-bibliography org-cite-global-bibliography)

    (use-package org-capture
      :config
      (setq org-capture-templates
            '(("t" "Todo Today" checkitem (file+headline (concat org-directory "todo.org") "Today")
               "- [ ] %?\n" :unnarrowed t)
              ("b" "To buy" entry (file+headline (concat org-directory "todo.org") "Einkaufsliste")
               "* TOBUY %?\n" :unnarrowed t)
              ("j" "Journal" entry (file+datetree (concat org-directory "journal.org"))
               "* %?\nEntered on %U\n  %i\n  %a"
               :unnarrowed t))))


    ;; optional: org-cite-insert is also bound to C-c C-x C-@
    :bind
    (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
    )


  (setq citar-symbols
      `((file ,(nerd-icons-faicon "nf-fae-file_export" :face 'nerd-icons-green  :v-adjust -0.1) . " ")
        (note ,(nerd-icons-mdicon "nf-md-pencil"       :face 'nerd-icons-blue   :v-adjust -0.3) . " ")
        (link ,(nerd-icons-mdicon "nf-md-link"         :face 'nerd-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator "  ")
  (setq citar-org-roam-mode t)
  (setq citar-notes-source 'citar-org-roam)
  (setq citar-templates
   '((main . "${title:48}     ${date year issued:4}     ${author editor:30}")
     (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.
")
     (note . "Notes on ${author editor}, ${title}")))

  (defun my-org-f2 ()
    (interactive)
    (org-toggle-latex-fragment))

  (defun my-org-fill-paragraph ()
    (interactive)
    (org-fill-paragraph)
    (save-excursion
      (when org-startup-with-latex-preview
        (mark-paragraph)
        (org-toggle-latex-fragment)
        )))

  (defun my-org-yank (&optional arg)
    "If the clipboard contains an image then write it to disk
in a 'images' folder and insert a link to it in the org buffer."
    (interactive "P")
    (if (region-active-p)
        (delete-region (region-beginning) (region-end)))
    (if (system-clipboard-contains-image-p)
        (org-paste-screenshot-from-clipboard)
      (call-interactively #'org-yank)))

  (add-hook 'org-mode-hook #'valign-mode)
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  (add-hook 'org-mode-hook (lambda () (setq fill-column 75)))

  ;; NOTE(Felix): This is a fix for org mode hanging indefinetly when saving the
  ;;   buffer because doom emacs adds a hook to org-encrypt-entries on save which
  ;;   seems to hang indefinitely (org mode bug maybe)
  ;;   https://github.com/hlissner/doom-emacs/issues/5924
  (add-hook 'org-mode-hook (lambda () (remove-hook 'before-save-hook 'org-encrypt-entries t)) 100)


  ;; NOTE(Felix): we gotta set org-format-latex-options here after requiring org,
  ;;   otherwise if we use with-eval-after-load, it would only get loaded after we
  ;;   open the first org file, which at load time would already try to latex
  ;;   compile the fragments, before executing the `setq org-format-latex-options'
  ;;   it seems ...
  (require 'org)
  (require 'ox-latex)
  (add-to-list 'org-latex-classes
               '("scrreprt" "\\documentclass{scrreprt}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("scrbook" "\\documentclass{scrbook}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; (setq org-cite-global-bibliography )
  ;; (setq citar-bibliography org-cite-global-bibliography)

  (add-to-list 'org-export-exclude-tags "toc")
  (add-to-list 'org-latex-packages-alist '("" "tikz" t) t)
  (add-to-list 'org-latex-packages-alist '("" "pgfplots" t) t)

  (setq org-format-latex-options
        '(:foreground default :background default :scale 2 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
          ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (setq org-preview-latex-image-directory "./images/latex/")

  (add-hook 'org-mode-hook
            (lambda ()
              (if (member "music" (org-get-tags))
                  (setq org-preview-latex-default-process 'dvisvgmmusic))))
  (setq org-export-babel-evaluate nil
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-emphasis-markers nil
        org-highlight-latex-and-related '(latex)
        org-html-with-latex               'dvisvgm
        org-preview-latex-default-process 'dvisvgm

        org-latex-caption-above '(table src-block)
        ;; org-latex-listings t
        org-latex-src-block-backend 'minted
        org-latex-listings-options '(("captionpos" "t"))
        org-startup-with-inline-images t
        org-startup-with-latex-preview t
        org-startup-indented t

        org-preview-latex-process-alist
        `((dvipng :programs
           ("latex" "dvipng")
           :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
           (1.0 . 1.0)
           :latex-compiler
           ("latex -interaction nonstopmode -output-directory %o %f")
           :image-converter
           ("dvipng -D %D -T tight -o %O %f")
           :transparent-image-converter
           ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
          (dvisvgm :programs
           ("latex" "dvisvgm")
           :description "dvi > svg"
           :message "you need to install the programs: latex and dvisvgm."
           :image-input-type "dvi"
           :image-output-type "svg"
           :image-size-adjust (1.7 . 1.5)
           :latex-compiler
           ("latex -interaction nonstopmode -output-directory %o %f")
           :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
          (dvisvgmmusic :programs
           ("latex" "dvisvgm")
           :description "dvi > svg via musixflx"
           :message "you need to install the programs: latex and dvisvgm."
           :image-input-type "dvi"
           :image-output-type "svg"
           :image-size-adjust (1.7 . 1.5)
           :latex-compiler
           ("latex -interaction nonstopmode -output-directory %o %f"
            "musixflx %f"
            "latex -interaction nonstopmode -output-directory %o %f")
           :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
          (imagemagick :programs
           ("latex" "convert")
           :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
           (1.0 . 1.0)
           :latex-compiler
           ("pdflatex -interaction nonstopmode -output-directory %o %f")
           :image-converter
           ("convert -density %D -trim -antialias %f -quality 100 %O"))))

  (with-eval-after-load 'ox-html
    (setq org-html-head
          (replace-regexp-in-string
           ".org-svg { width: 90%; }"
           ".org-svg { width: auto; }"
           org-html-style-default)))

  (with-eval-after-load 'org
    (require 'org-tempo)

    )
    ;;(add-to-list 'org-latex-packages-alist '("" "tikz" t))
    ;;(add-to-list 'org-latex-packages-alist '("" "pgfplots" t))


  (map! :map org-mode-map
        "C-c C-o" 'my/org-open-at-point
        "C-c C-o" 'org-open-at-point
        "C-r"  'org-roam-node-insert
        "C-j"  'join-line
        "C-y"  'my-org-yank
        "M-q"  'my-org-fill-paragraph
        "C-#"  'comment-line)

  (code-region "color links"
    ;; work like this:
    ;;   - This is [[color:green][green text]]
    ;;   - This is [[color:red][red]]

    (require 's)

    (defun color-comp (&optional arg)
      "Completion function for color links."
      (let ((color-data (prog2
                            (save-selected-window
                              (list-colors-display))
                            (with-current-buffer (get-buffer "*Colors*")
                              (mapcar (lambda (line)
                                        (append (list line)
                                                (s-split " " line t)))
                                      (s-split "\n" (buffer-string))))
                          (kill-buffer "*Colors*"))))
        (format "color:%s"
                (s-trim (cadr (assoc (completing-read "Color: " color-data) color-data))))))


    (defun color-link-face (path)
      "Face function for color links."
      ;; (message path)
      ;; (or 1 (cdr (assoc path org-link-colors))
      `(:underline ,nil :foreground ,path))
    ;; )


    (defun color-link-export (path description backend)
      "Export function for color links."
      (cond
       ((eq backend 'latex)                          ; added by TL
        (format "{\\color{%s}%s}" path description)) ; added by TL
       ((or (eq backend 'html) (eq backend 'md) (eq backend 'hugo))
        (let ((rgb (assoc (downcase path) color-name-rgb-alist))
              r g b)
          (setq r (* 255 (/ (nth 1 rgb) 65535.0))
                g (* 255 (/ (nth 2 rgb) 65535.0))
                b (* 255 (/ (nth 3 rgb) 65535.0)))
          (format "<span style=\"color: rgb(%s,%s,%s)\">%s</span>"
                  (truncate r) (truncate g) (truncate b)
                  (or description path))))))

    (with-eval-after-load 'org
      (org-link-set-parameters "color"
                               :face     'color-link-face
                               :complete 'color-comp
                               :export   'color-link-export))

    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (code-region "Org Agenda"
    (with-eval-after-load 'org
      ;; Allow multiple line Org emphasis markup. Like bold
      ;; http://emacs.stackexchange.com/a/13828/115
      (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
      ;; Below is needed to apply the modified `org-emphasis-regexp-components'
      ;; settings from above.
      (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

    (defun open-my-calendar ()
      (interactive)
      (org-agenda nil "o"))

    (map! "<f2>" 'open-my-calendar)

    (setq! calendar-week-start-day 0
           calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                                              "Donnerstag" "Freitag" "Samstag"]
           calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                               "Juni" "Juli" "August" "September"
                                               "Oktober" "November" "Dezember"])

    (setq solar-n-hemi-seasons
          '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang"))

    (setq holiday-general-holidays
          '((holiday-fixed 1 1 "Neujahr")
            (holiday-fixed 5 1 "1. Mai")
            (holiday-fixed 10 3 "Tag der Deutschen Einheit")))

    ;; Feiertage für Bayern, weitere auskommentiert
    (setq holiday-christian-holidays
          '((holiday-float 12 0 -4 "1. Advent" 24)
            (holiday-float 12 0 -3 "2. Advent" 24)
            (holiday-float 12 0 -2 "3. Advent" 24)
            (holiday-float 12 0 -1 "4. Advent" 24)
            (holiday-fixed 12 25 "1. Weihnachtstag")
            (holiday-fixed 12 26 "2. Weihnachtstag")
            (holiday-fixed 1 6 "Heilige Drei Könige")
            (holiday-easter-etc -48 "Rosenmontag")
            ;; (holiday-easter-etc -3 "Gründonnerstag")
            (holiday-easter-etc  -2 "Karfreitag")
            (holiday-easter-etc   0 "Ostersonntag")
            (holiday-easter-etc  +1 "Ostermontag")
            (holiday-easter-etc +39 "Christi Himmelfahrt")
            (holiday-easter-etc +49 "Pfingstsonntag")
            (holiday-easter-etc +50 "Pfingstmontag")
            (holiday-easter-etc +60 "Fronleichnam")
            (holiday-fixed 8 15 "Mariae Himmelfahrt")
            (holiday-fixed 11 1 "Allerheiligen")
            ;; (holiday-float 11 3 1 "Buss- und Bettag" 16)
            (holiday-float 11 0 1 "Totensonntag" 20)))



    (setq org-tags-column -90)
    (setq org-log-done t)

    ;; NOTE(Felix): for org clocktables dont show number of days, keep counting
    ;;   hours even they are more than 24
    (setq org-duration-format '(("h") (special . h:mm)))
    (setq org-agenda-category-icon-alist
          `(("pizza"       ,(list (nerd-icons-faicon  "nf-fae-pizza"))        nil nil)
            ("coffee"      ,(list (nerd-icons-faicon  "nf-fa-coffee"))        nil nil)
            ("events"      ,(list (nerd-icons-codicon "nf-cod-calendar"))     nil nil)
            ("geburtstage" ,(list (nerd-icons-faicon  "nf-fa-birthday_cake")) nil nil)
            ("todo"        ,(list (nerd-icons-octicon "nf-oct-checkbox"))     nil nil)
            ("uni"         ,(list (nerd-icons-faicon "nf-fa-graduation_cap")) nil nil)
            ("sport"       ,(list "🏓")            nil nil)
            ("teaching"    ,(list "\xf130")       nil nil :ascent center)))

    (setq org-agenda-block-separator (string-to-char " "))
    (setq org-agenda-format-date 'my-org-agenda-format-date-aligned)
    (setq org-agenda-dim-blocked-tasks 'invisible)

    (setq org-archive-location (concat org-directory ".archives/%s_archive::"))

    (defun my-org-agenda-format-date-aligned (date)
      "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
      (require 'cal-iso)
      (let* ((dayname (calendar-day-name date 1 nil))
             (day (cadr date))
             (month (car date))
             (monthname (elt calendar-month-name-array (- month 1))))
        (format "  %-2s. %2d %s"
                dayname day monthname)))

    (defun format-lectures ()
      (concat "[ " (nth 0 (org-get-outline-path)) " ]"))

    (defun format-deadlines ()
      (concat (format-lectures)
              " <"
              (org-format-time-string "%d.%m.%Y" (org-get-deadline-time (point)))
              ">"))


    (defun my/org-skip-function (part)
      "Partitions things to decide if they should go into the agenda '(agenda future-scheduled done)"
      (let* ((skip (save-excursion (org-entry-end-position)))
             (dont-skip nil)
             (deadline (org-get-deadline-time (point)))
             (deadline-seconds (time-convert deadline 'integer))
             (time-now (time-convert (current-time) 'integer))
             (result
              (or (and deadline
                       (< time-now (+ deadline-seconds 86400))
                       ;; 86400 == seconds per day -- basically check if the
                       ;; deadline time (which maybe was midnight) is today or in
                       ;; the future
                       'agenda)
                  'done)))                 ; Everything else should go in the agenda
        (if (eq result part) dont-skip skip)))

    (setq org-agenda-custom-commands
          '(("o" "Plan"
             ((tags-todo
               "today"
               ((org-agenda-remove-tags t)
                (org-agenda-overriding-header "Other todos today:\n")))
              ;; (tags "today"
               ;;((org-agenda-overriding-header "Lectures still to watch:\n")
                ;;(org-agenda-files '("~/org/uni.org"))
                ;;(org-agenda-prefix-format   "  %-2i  %-10(format-lectures) ")
                ;;(org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'timestamp))))
              ;; (tags  "assignments"
              ;;  ((org-agenda-skip-function '(my/org-skip-function 'agenda))
              ;;   (org-agenda-prefix-format   "  %-2i  %-23(format-deadlines) ")
              ;;   (org-agenda-sorting-strategy '(deadline-up))
              ;;   (org-agenda-files '("~/org/uni.org"))
              ;;   (org-agenda-overriding-header "Due assignments:\n")
              ;;   (org-agenda-remove-tags t)))
              (agenda ""
               ((org-agenda-start-day "-1d")
                (org-agenda-span 15)
                (org-agenda-overriding-header "Agenda:\n")
                (org-agenda-repeating-timestamp-show-all nil)
                (org-agenda-remove-tags t)
                (org-agenda-prefix-format   "    %-2i  %-20b %-11t  %s")
                (org-agenda-todo-keyword-format "%-1s")
                (org-agenda-current-time-string "<┈┈┈┈┈┈┈┈┈┈ now ┈┈┈┈┈┈┈┈┈┈>")
                (org-agenda-deadline-leaders '("Deadline:  " "In %3d t:  " "Vor %2d t: "))
                (org-agenda-scheduled-leaders '("Scheduled: " "Overdue: "))
                (org-agenda-time-grid '((daily today remove-match)
                                        (0800 0900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
                                        " . . ." "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈"))))))))))

(code-region "Org gardentangle"


  ;; (defun my-org-tangle-this-file ()
    ;; (interactive)

    ;; (let ((this-file (expand-file-name (buffer-file-name))))
      ;; (my-org-babel-tangle-publish nil this-file
                                   ;; (expand-file-name "~/org/"))
      ;; ))

  (setq tangle-bib-file (concat org-directory "vault.bib"))
  (defun my-org-babel-tangle-append (&optional arg target-file lang-re)
    "Hard copy of `org-babel-tangle' with the difference-of
appending to the tangled file instead of overwriting it. Before
hard-copying this here, I tried to flet the delete-file, which
worked but still write-region does delete first if an optional
arg is not set, and I don't know how I can make the original
`org-babel-tangle' pass that arg from outside.. So here we are"
    (interactive "P")
    (run-hooks 'org-babel-pre-tangle-hook)
    ;; Possibly Restrict the buffer to the current code block
    (save-restriction
      (save-excursion
        (when (equal arg '(4))
          (let ((head (org-babel-where-is-src-block-head)))
            (if head
                (goto-char head)
              (user-error "Point is not in a source code block"))))
        (let ((block-counter 0)
              (org-babel-default-header-args
               (if target-file
                   (org-babel-merge-params org-babel-default-header-args
                                           (list (cons :tangle target-file)))
                 org-babel-default-header-args))
              (tangle-file
               (when (equal arg '(16))
                 (or (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light))))
                     (user-error "Point is not in a source code block"))))
              path-collector)
          (mapc ;; map over file-names
           (lambda (by-fn)
             (let ((file-name (car by-fn)))
               (when file-name
                 (let ((lspecs (cdr by-fn))
                       (fnd (file-name-directory file-name))
                       modes make-dir she-banged lang)
                   ;; drop source-blocks to file
                   ;; We avoid append-to-file as it does not work with tramp.
                   (with-temp-buffer
                     (mapc
                      (lambda (lspec)
                        (let* ((block-lang (car lspec))
                               (spec (cdr lspec))
                               (get-spec (lambda (name) (cdr (assq name (nth 4 spec)))))
                               (she-bang (let ((sheb (funcall get-spec :shebang)))
                                           (when (> (length sheb) 0) sheb)))
                               (tangle-mode (funcall get-spec :tangle-mode)))
                          (unless (string-equal block-lang lang)
                            (setq lang block-lang)
                            (let ((lang-f (org-src-get-lang-mode lang)))
                              (when (fboundp lang-f) (ignore-errors (funcall lang-f)))))
                          ;; if file contains she-bangs, then make it executable
                          (when she-bang
                            (unless tangle-mode (setq tangle-mode #o755)))
                          (when tangle-mode
                            (add-to-list 'modes (org-babel-interpret-file-mode tangle-mode)))
                          ;; Possibly create the parent directories for file.
                          (let ((m (funcall get-spec :mkdirp)))
                            (and m fnd (not (string= m "no"))
                                 (setq make-dir t)))
                          ;; Handle :padlines unless first line in file
                          (unless (or (string= "no" (funcall get-spec :padline))
                                      (= (point) (point-min)))
                            (insert "\n"))
                          (when (and she-bang (not she-banged))
                            (insert (concat she-bang "\n"))
                            (setq she-banged t))
                          (org-babel-spec-to-string spec)
                          (setq block-counter (+ 1 block-counter))))
                      lspecs)
                     (when make-dir
                       (make-directory fnd 'parents))
                     ;; erase previous file
                     ;; (when (file-exists-p file-name)
                     ;; (delete-file file-name))
                     (write-region nil nil file-name t)
                     (mapc (lambda (mode) (set-file-modes file-name mode)) modes)
                     (push file-name path-collector))))))
           (if (equal arg '(4))
               (org-babel-tangle-single-block 1 t)
             (org-babel-tangle-collect-blocks lang-re tangle-file)))
          (message "Tangled %d code block%s from %s" block-counter
                   (if (= block-counter 1) "" "s")
                   (file-name-nondirectory
                    (buffer-file-name
                     (or (buffer-base-buffer)
                         (current-buffer)
                         (and (org-src-edit-buffer-p)
                              (org-src-source-buffer))))))
          ;; run `org-babel-post-tangle-hook' in all tangled files
          (when org-babel-post-tangle-hook
            (mapc
             (lambda (file)
               (org-babel-with-temp-filebuffer file
                 (run-hooks 'org-babel-post-tangle-hook)))
             path-collector))
          path-collector))))

  (defun my-org-babel-tangle-publish (_ filename pub-dir)
    (let* ((visited (find-buffer-visiting filename))
           (buffer (or visited (find-file-noselect filename))))
      (prog1
          (with-current-buffer buffer
            (org-with-wide-buffer
             (my-org-babel-tangle-append nil (expand-file-name tangle-bib-file) "bibtex"))))
      (unless visited (kill-buffer buffer))))


  (defun publish-garden-bib (&optional FORCE)
    (interactive)
    (setq org-publish-project-alist
          `(("garden-bib"
             :base-directory ,org-roam-directory
             :base-extension "org"
             :publishing-directory ,(expand-file-name org-directory)
             :recursive t
             :publishing-function my-org-babel-tangle-publish
             :auto-preamble t
             )))


    (when (file-exists-p tangle-bib-file)
      (delete-file tangle-bib-file))

    (let ((old-org-startup-with-latex-preview org-startup-with-latex-preview))
      (setq org-startup-with-latex-preview nil)

      ;; do
      (org-publish-project "garden-bib" FORCE)

      ;; restore
      (setq org-startup-with-latex-preview old-org-startup-with-latex-preview))))

(code-region "Org publish stuff for garden"
  (require 'ox-publish)
  (use-package! ox-twbs)
  (use-package! ox-hugo
    :custom
    (org-hugo-auto-set-lastmod t))

  (with-eval-after-load 'org-roam
    (cl-defmethod org-roam-node-my-title ((node org-roam-node))
      (let ((title (org-roam-node-title node)))
        (concat (string-pad title (- (/ (frame-width) 2) 1) nil t) "  ")))

    (setq org-roam-node-display-template
          ;; (concat "${title:*}"  (propertize "${tags}" 'face 'org-tag))
          (concat "${my-title}" (propertize "${tags}" 'face 'org-tag)))

    (defun org-hugo-publish (plist filename pub-dir)
      ;; (print plist)
      ;; (print filename)
      ;; (print pub-dir)
      ;;
      ;; (:base-directory "~/../../org/vault/" :base-extension "org" :publishing-directory "~/public_html/" :recursive t :publishing-function org-hugo-publish :headline-levels 4 :auto-preamble t)
      ;; "d:/Programme/org-roam/org/vault/Uni/Deep Learning/voxnet.org"
      ;; "d:/Programme/org-roam/emacs/home/public_html/Uni/Deep Learning/"

      (unless (string= "setupfile" (file-name-base filename))
        (with-current-buffer (find-file-noselect filename)
          (save-buffer)
          (let* ((abs-path-vault     (expand-file-name (plist-get plist :base-directory)))
                 (abs-path-vault-len (length abs-path-vault))
                 (abs-path-org-file  (file-name-directory filename))
                 (added-path         (substring abs-path-org-file abs-path-vault-len)))

            ;;(print abs-path-org-file)
            ;;(print added-path)
            ;;
            ;;"d:/Programme/org-roam/org/vault/Uni/Graphics/"
            ;;"Uni/Graphics/"

            (let ((org-hugo-section added-path))
              (org-hugo-export-wim-to-md :all-subtrees nil nil nil))))))


    (defun publish-garden (&optional FORCE)
      (interactive "P")
      (save-excursion
        (unless (file-directory-p org-exported-vault-directory)
          (unless (and (executable-find "hugo")
                       (executable-find "git"))
            (error "git and hugo need to be installed to be able to export the garden"))

          (make-directory org-exported-vault-directory t)
          (let ((current-pwd default-directory))
            (cd (concat org-exported-vault-directory "../"))
            (compilation-start
             (concat "hugo new site knowledge_base && "
                     "cd " org-exported-vault-directory " && "
                     "git init && "
                     "git submodule add https://github.com/bphenriques/explorer-hugo-theme themes/explorer"))
            (write-region "theme = 'explorer'\n[markup.goldmark.renderer]\nunsafe= true" nil (concat org-exported-vault-directory "hugo.toml") 'append)))

        (let ((old-org-startup-with-latex-preview org-startup-with-latex-preview)
              (old-org-preview-latex-image-directory org-preview-latex-image-directory)
              (old-org-format-latex-options org-format-latex-options))

          ;; config
          (setq org-element-use-cache nil)
          (setq org-hugo-base-dir (concat org-exported-vault-directory))
          (setq org-format-latex-options
                '(:foreground default :background default :scale 2 :html-foreground "White" :html-background "Transparent" :html-scale 1.0 :matchers
                              ("begin" "$1" "$" "$$" "\\(" "\\[")))
          (setq org-preview-latex-image-directory (concat (temporary-file-directory) "/ox-hugo-latex/"))

          (if (file-directory-p org-preview-latex-image-directory)
              (delete-directory org-preview-latex-image-directory t))

          (setq org-publish-project-alist
                `(("garden-org"
                   :base-directory ,org-roam-directory
                   :base-extension "org"
                   :publishing-directory ,org-hugo-base-dir
                   :recursive t
                   :publishing-function org-hugo-publish
                   :headline-levels 4             ; Just the default for this project.
                   :auto-preamble t
                   )
                  ("garden-static"
                   :base-directory ,org-roam-directory
                   :base-extension "png\\|jpg\\|svg"
                   :publishing-directory ,org-hugo-base-dir
                   :recursive t
                   :publishing-function org-publish-attachment
                   )
                  ("garden" :components ("garden-org"
                                         ;;"garden-static"
                                         ))
                  ))


          ;; do
          (setq org-startup-with-latex-preview nil)
          (org-roam-update-org-id-locations)
          (org-publish-project "garden" FORCE)

          ;; publish if linux and upload script exists
          (if (not is-windows)
            (let ((upload-script-path (concat org-hugo-base-dir "upload.sh")))
              (if (file-exists-p upload-script-path)
                  (progn
                    (compilation-start upload-script-path t))
                (message "No upload script found."))))

          ;; restore
          (delete-directory org-preview-latex-image-directory t)
          (setq org-preview-latex-image-directory old-org-preview-latex-image-directory)
          (setq org-startup-with-latex-preview old-org-startup-with-latex-preview)
          (setq org-format-latex-options old-org-format-latex-options)
          (message "Publish Done!"))))))

(defun search-jai-modules ()
  (interactive)
  (compilation-start (concat "grep \""
                             "^[[:space:]]*"
                             (or (when (symbol-at-point)
                                     (symbol-name (symbol-at-point)))
                                 (read-string "What to search in the modules? "))
                             "[[:space:]]\\+:: \" -Rn "
                             "/home/felix/jai/jai/modules/"
                             " | sort -u")
                     t))

(defun search-jai-how-to ()
  (interactive)
    (compilation-start (concat "grep \""
                               (read-string "What to look for in the how-to? ")
                               "\" -Rn "
                               "/home/felix/jai/jai/how_to/"
                               " | sort -u")
                       t))

;; (map! :map jai-mode-map
;;       "M-." 'search-jai-modules)

(defun delete-trailing-whitespace-except-current-line ()
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) begin)
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)))
      (when (> (point-max) end)
        (save-restriction
          (narrow-to-region (1+ end) (point-max))
          (delete-trailing-whitespace))))))
(add-hook 'before-save-hook 'delete-trailing-whitespace-except-current-line)

;; NOTE(Felix): by doom-default enter in comment will insert the comment
;;   starting characters into the buffer on the next line. So I revert that
;;   behavior.
(advice-remove 'newline-and-indent #'+default--newline-indent-and-continue-comments-a)


(code-region "Korean input"
  (defvar *my-input-method* :de)

  (defun change-lang ()
    (interactive)
    ;; (keyboard-translate ?z ?y)
    (if (eq *my-input-method* :de)
        (progn
          (keyboard-translate ?y ?z)  ; For german keyboards
          (keyboard-translate ?z ?y)
          (set-input-method 'korean-hangul)
          (setq *my-input-method* :kr))
      (toggle-input-method nil)
      (keyboard-translate ?y ?y)  ; For german keyboards
      (keyboard-translate ?z ?z)
      (setq *my-input-method* :de)))
  )

(defun show-in-explorer ()
  (interactive)
  (if is-windows
      (shell-command "explorer .")
    (shell-command "thunar .")))

(push '(organization . organization) citeproc-blt-to-csl-standard-alist)

(require 'jai-mode)
;; (require 'org-preview)

(setq jai--error-regexp "^\\([^\n]*[^ :]+\\):\\([0-9]+\\),\\([0-9]+\\): \\(?:Error\\|\\(Info\\|Warning\\)\\)")
(push `(jai ,jai--error-regexp 1 2 3 (4)) compilation-error-regexp-alist-alist)
;;
;; (push `(jai
;;         "\\([A-Za-z0-9\\/.]*\\):\\([0-9]+\\),\\([0-9]+\\): \\(?:Error\\|\\(Info\\|Warning\\)\\)"
;;         1 2 3 (4))
;;       compilation-error-regexp-alist-alist)



(push 'jai compilation-error-regexp-alist)


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; if you use typescript-mode
(add-hook 'typescript-mode-hook #'setup-tide-mode)
;; if you use treesitter based typescript-ts-mode (emacs 29+)
(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)


(defun copy-rectangle-as-one-line (start end)
  "Like `copy-rectangle-as-kill', but make just one string out of it."
  (interactive "r")
  (call-interactively #'copy-rectangle-as-kill)
  (with-temp-buffer
    (yank-rectangle)
    (delete-trailing-whitespace)
    (kill-region (point-min) (point-max))))

(setq org-prettify-symbols-alist
      '(("lambda" . 955)))

;; (add-hook 'org-mode-hook (lambda ()
   ;; "Beautify Org Checkbox Symbol"
   ;; (setq-local prettify-symbols-alist org-prettify-symbols-alist)
   ;; (push '("[ ]" . "")  prettify-symbols-alist)
   ;; (push '("[X]" . "" ) prettify-symbols-alist)
   ;; (push '("[-]" . "" ) prettify-symbols-alist)
   ;; (prettify-symbols-mode)
   ;; ))

;; (add-hook 'emacs-startup-hook (lambda () (doom/reload-font)))
