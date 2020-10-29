;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name    "Felix Brendel"
      user-mail-address "felixbrendel@airmail.cc")

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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)
;; (setq doom-theme 'material)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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

(setq projectile-generic-command
 (concat "rg -0 --files --follow --color=never --hidden"
         (if IS-WINDOWS " --path-separator /")))

(setq counsel-rg-base-command '("rg" "-M" "240" "--with-filename" "--no-heading" "--line-number" "--color" "never" "%s" "--path-separator" "/" "."))

(setq +doom-dashboard-menu-sections
  '(("Reload last session"
     :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
     :when (cond ((require 'persp-mode nil t)
                  (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                 ((require 'desktop nil t)
                  (file-exists-p (desktop-full-file-name))))
     :face (:inherit (doom-dashboard-menu-title bold))
     :action doom/quickload-session)
    ("Open Calendar"
     :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
     :when (fboundp 'org-agenda)
     :action open-my-calendar)
    ("Recently opened files"
     :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
     :action recentf-open-files)
    ("Open project"
     :icon (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
     :action projectile-switch-project)
    ("Jump to bookmark"
     :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
     :action bookmark-jump)
    ("Open private configuration"
     :icon (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
     :when (file-directory-p doom-private-dir)
     :action doom/open-private-config)
    ("Open documentation"
     :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
     :action doom/help)))


(defun duplicate-line()
  (interactive)
  (let ((inhibit-message 1))
  (save-excursion
    (move-beginning-of-line 1)
    (set-mark (point-marker))
    (move-end-of-line 1)
    (copy-region-as-kill (region-beginning) (region-end))
    (deactivate-mark)
    (insert "\n")
    (yank)
    (pop kill-ring))
  (call-interactively 'next-line)))

(cond ((string= system-type "windows-nt")
       ;; if windows
       (setq build-script-name "build.bat"))
      (t ;; if linux
       (setq build-script-name "build.sh")))


(defun save-and-find-build-script-and-compile ()
  "Walks upward the directory tree until a buildscript is found"
  (interactive)

  (when (and buffer-file-name (buffer-modified-p)) (save-buffer))
  (let ((build-script-path (locate-dominating-file (expand-file-name default-directory) build-script-name)))
    (unless build-script-path
      (error (concat "The default buildscript '" build-script-name "' cannot be found")))
    ;; NOTE(Felix): because we will set compilation to comint mode,
    ;; after the compilation process finishes, we will set it back to
    ;; normal compilation mode, so that we can press g for recompile or
    ;; q for closing the window. As opposoed to typing in these chars as
    ;; we would if we would not switch off the comint mode.
    (setq compilation-finish-functions
          (list (lambda (&rest _) (compilation-minor-mode 1))))
    (compile (concat build-script-path build-script-name) t)))

(defun lookup-docs-for-symbol-at-point ()
  (interactive)
  (+lookup/online (thing-at-point 'symbol) "DevDocs.io"))

(defun mark-word-or-next-word-like-this ()
  "if there is no active region the word under
   the point will be marked, otherwise the next word is selected."
  (interactive)
  (if (region-active-p)
      ;; then
      (progn
        (mc/mark-next-like-this 1)
        (mc/maybe-multiple-cursors-mode))
    ;; else
    (mc--mark-symbol-at-point)))

(defun org-in-kanban-p ()
  (save-match-data
    (let ((case-fold-search t)
	        (lim-up (save-excursion (outline-previous-heading)))
	        (lim-down (save-excursion (outline-next-heading))))
	    (when (org-between-regexps-p
		         "^[ \t]*#\\+begin: kanban"
		         "^[ \t]*#\\+end"
		         lim-up lim-down)
	      t))))

(defun org-maybe-shift-right ()
  (interactive)
  (if (org-in-kanban-p)
      (org-kanban/next)
    (org-shiftright)))

(defun org-maybe-shift-left ()
  (interactive)
  (if (org-in-kanban-p)
      (org-kanban/prev)
    (org-shiftleft)))

(defun open-my-calendar ()
  (interactive)
  (org-agenda nil "o"))

(map! :map org-mode-map
      "S-<right>" 'org-maybe-shift-right
      "S-<left>"  'org-maybe-shift-left)

;; (map! :map c-mode-map
      ;; "C-d"         'mark-word-or-next-word-like-this)

(map! :mode  prog-mode-map  "C-d"         'mark-word-or-next-word-like-this)
(map! :after prog-mode      "C-d"         'mark-word-or-next-word-like-this)

(map! "C-s"         'swiper
      "C-x b"       '+ivy/switch-buffer
      "C-x 4 b"     '+ivy/switch-buffer-other-window
      "C-j"         'join-line
      "C-d"         'mark-word-or-next-word-like-this
      "C-S-d"       'duplicate-line
      "C-S-c C-S-c" 'mc/edit-lines
      "C-#"         'comment-line
      "M-d"         'lookup-docs-for-symbol-at-point
      "<f1>"        '+doom-dashboard/open
      "<f2>"        'open-my-calendar
      "<f5>"        'save-and-find-build-script-and-compile
      :leader "e" 'save-and-find-build-script-and-compile)

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(add-hook 'bat-mode-hook (lambda () (set (make-local-variable 'comment-start) ":: ")))

(use-package! multiple-cursors)
(use-package! ripgrep)
(use-package! ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
      '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(use-package! company-posframe
  :config
  (company-posframe-mode 1))

(use-package! ox-twbs)
(use-package! org-kanban)

(setq doom-font (font-spec :family "Noto Sans Mono" :foundry "outline" :height 113)
      doom-modeline-major-mode-icon t)

(put 'projectile-ripgrep 'disabled nil)
(put 'irony-additional-clang-options 'safe-local-variable #'listp)

(push '("^Comint \\(finished\\).*"
        (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
        (1 compilation-info-face))
      compilation-mode-font-lock-keywords)

(push '("^Comint \\(exited abnormally\\|interrupt\\|killed\\|terminated\\|segmentation fault\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
        (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
        (1 compilation-error-face)
        (2 compilation-error-face nil t))
      compilation-mode-font-lock-keywords)


(font-lock-add-keywords
 'c++-mode
 '(("\\<\\(defer\\|proc\\)\\>" . font-lock-keyword-face)))



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


(setq org-agenda-category-icon-alist
      `(("teaching" ,(list (all-the-icons-material "school")) nil nil :ascent center)
        ("events"   ,(list (all-the-icons-material "event")) nil nil :ascent center)
        ("todo"     ,(list (all-the-icons-material "check")) nil nil :ascent center)
        ("exams"    ,(list (all-the-icons-material "create")) nil nil :ascent center)
        ))

(setq org-agenda-block-separator (string-to-char " "))
(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)
(setq org-agenda-dim-blocked-tasks 'invisible)

(setq org-archive-location "~/org/archives/%s_archive::")

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

(setq org-agenda-custom-commands
      '(("o" "Plan"
         ((agenda "" (
                      (org-agenda-start-day "-2d")
                      (org-agenda-span 15)
                      (org-agenda-overriding-header "Schedule:\n")
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format   "    %-2i  %-15b %t   %s")
                      (org-agenda-todo-keyword-format "%-1s")
                      (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
                      (org-agenda-deadline-leaders '("Deadline: " "In %3d t:  " "Vor %2d t: "))
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-time-grid (quote ((daily today remove-match)
                                                    (0800 0900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
                                                    "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))
                      ))))))

(setq org-capture-templates
     '(("t" "Todo" entry (file+headline "todo.org" "todos") "* [ ] %?
%i
%a" :prepend t) ("l" "Teching" entry (file+headline "teaching.org" "Nachhilfe") "* Nachhilfe <Name>%?

am %T" :prepend t) ("e" "Events" entry (file+headline "events.org" "Events") "* %?
%T" :prepend t) ("p" "Templates for projects") ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?
%i
%a" :prepend t) ("pn" "Project-local notes" entry (file+headline +org-capture-project-notes-file "Inbox") "* %U %?
%i
%a" :prepend t) ("pc" "Project-local changelog" entry (file+headline +org-capture-project-changelog-file "Unreleased") "* %U %?
%i
%a" :prepend t) ("o" "Centralized templates for projects") ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?
 %i
 %a" :heading "Tasks" :prepend nil) ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?
 %i
 %a" :heading "Notes" :prepend t) ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?
 %i
 %a" :heading "Changelog" :prepend t)))

(setq org-export-babel-evaluate nil)


(require 'epa-file)
(epa-file-enable)
