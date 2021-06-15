;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
;

;; (set-language-environment "Korean")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(setq doom-font (font-spec :family "noto sans mono" :slant 'normal :weight 'normal :height 127 :width 'normal)
      doom-modeline-major-mode-icon t)

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
(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup. Like bold
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

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
        (mc/cycle-forward)
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

(defun open-my-drill-calendar ()
  (interactive)
  (org-agenda nil "d"))


(map! :map org-mode-map
      "S-<right>" 'org-maybe-shift-right
      "S-<left>"  'org-maybe-shift-left)

(map! "C-s"         'swiper
      "C-x b"       '+ivy/switch-buffer
      "C-x 4 b"     '+ivy/switch-buffer-other-window
      "C-j"         'join-line
      "C-d"         'mark-word-or-next-word-like-this
      "C-S-d"       'duplicate-line
      "C-S-c C-S-c" 'mc/edit-lines
      "C-#"         'comment-line
      "M-d"         'lookup-docs-for-symbol-at-point
      "<f1>"        'open-my-drill-calendar
      "<f2>"        'open-my-calendar
      "<f5>"        'save-and-find-build-script-and-compile
      "M-SPC"       'change-lang
      :leader "e" 'save-and-find-build-script-and-compile)


;; (add-hook 'bat-mode-hook (lambda () (set (make-local-variable 'comment-start) "REM ")))
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; (use-package! irony)
;; (use-package! irony-eldoc)
;; (use-package! modern-cpp-font-lock)

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

(use-package! ox-hugo
  :after ox)

(use-package! valign
  :config
  (add-hook 'org-mode-hook #'valign-mode))

(setq magit-todos-nice nil)

(use-package! zig-mode)
(use-package! multiple-cursors)
(use-package! glsl-mode)
(use-package! ripgrep)
(use-package! ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
      '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(use-package! company-posframe
  :config
  (company-posframe-mode 1))

(use-package! ox-reveal)
(use-package! ox-twbs)
(use-package! org-kanban)
(use-package! org-fragtog)
(use-package! org-drill)
(use-package! org-table)

(add-hook 'org-mode-hook 'org-fragtog-mode)

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

(defun my-c-mode-hook ()
  (bind-key* "C-d" #'mark-word-or-next-word-like-this)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'brace-list-intro '+)
  (c-set-offset 'brace-list-close 0))

(add-hook 'java-mode-hook   'my-c-mode-hook)
(add-hook 'c-mode-hook   'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
(add-hook 'org-mode-hook #'hl-todo-mode)

;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'irony-mode-hook #'irony-eldoc)

(font-lock-add-keywords
 'c++-mode
 '(("\\<\\(defer\\|proc\\|panic\\)\\>" . font-lock-keyword-face)))

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
(setq org-agenda-category-icon-alist
      `(("teaching" ,(list "\xf130")       nil nil :ascent center)
        ("pizza"    ,(list (all-the-icons-material "local_pizza"))           nil nil)
        ("coffee"   ,(list (all-the-icons-faicon "coffee"))           nil nil)
        ("events"   ,(list (all-the-icons-faicon "calendar-check-o")) nil nil)
        ("todo"     ,(list (all-the-icons-faicon "check"))            nil nil)
        ("pprog"    ,(list (all-the-icons-faicon "align-left"))       nil nil)
        ("dbsys"    ,(list (all-the-icons-faicon "table"))            nil nil)
        ("gemji"    ,(list (all-the-icons-faicon "gamepad"))          nil nil)
        ("mocap"    ,(list (all-the-icons-faicon "eye"))              nil nil)
        ("uni"      ,(list (all-the-icons-faicon "graduation-cap"))   nil nil)))

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
          (todo ""
                ((org-agenda-overriding-header "Lectures still to watch:\n")
                 (org-agenda-files '("~/org/uni.org"))
                 (org-agenda-prefix-format   "  %-2i  %-10(format-lectures) ")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'timestamp))))
          (tags  "assignments"
                 ((org-agenda-skip-function '(my/org-skip-function 'agenda))
                  (org-agenda-prefix-format   "  %-2i  %-23(format-deadlines) ")
                  (org-agenda-sorting-strategy '(deadline-up))
                  (org-agenda-files '("~/org/uni.org"))
                  (org-agenda-overriding-header "Due assignments:\n")
                  (org-agenda-remove-tags t)))
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
                                           " . . ." "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈"))))))))

;; (defun org-agenda-delete-empty-blocks ()
;;   "Remove empty agenda blocks.
;;   A block is identified as empty if there are fewer than 2
;;   non-empty lines in the block (excluding the line with
;;   `org-agenda-block-separator' characters)."
;;   (when org-agenda-compact-blocks
;;     (user-error "Cannot delete empty compact blocks"))
;;   (setq buffer-read-only nil)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let* ((blank-line-re "^\\s-*$")
;;            (content-line-count (if (looking-at-p blank-line-re) 0 1))
;;            (start-pos (point))
;;            (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
;;       (while (and (not (eobp)) (forward-line))
;;         (cond
;;          ((looking-at-p block-re)
;;           (when (< content-line-count 2)
;;             (delete-region start-pos (1+ (point-at-bol))))
;;           (setq start-pos (point))
;;           (forward-line)
;;           (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
;;          ((not (looking-at-p blank-line-re))
;;           (setq content-line-count (1+ content-line-count)))))
;;       (when (< content-line-count 2)
;;         (delete-region start-pos (point-max)))
;;       (goto-char (point-min))
;;       ;; The above strategy can leave a separator line at the beginning
;;       ;; of the buffer.
;;       (when (looking-at-p block-re)
;;         (delete-region (point) (1+ (point-at-eol))))))
;;   (setq buffer-read-only t))
;; (add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks)

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

(add-hook 'org-mode-hook 'org-bullets-mode)


(setq lang :de)
(defun change-lang ()
  (interactive)
  ;; (keyboard-translate ?z ?y)
  (if (eq lang :de)
      (progn
        (keyboard-translate ?y ?z)  ; For german keyboards
        (keyboard-translate ?z ?y)
        (set-input-method 'korean-hangul)
        (setq lang :kr))
    (toggle-input-method nil)
    (keyboard-translate ?y ?y)  ; For german keyboards
    (keyboard-translate ?z ?z)
    (setq lang :de)))

(open-my-calendar)
