;; -*- lexical-binding: t -*-
(setq is-windows (string= system-type "windows-nt"))

(defun append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR.
source: https://stackoverflow.com/questions/24356401/how-to-append-multiple-elements-to-a-list-in-emacs-lisp"
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(defun find-c-compiler ()
  (cond ((executable-find "clang") "clang %s -g -o %s")
        ((executable-find "gcc")   "gcc %s -g -o %s")
        ((executable-find "cl")    "cl %s /Z7 /Fe: %s")))

(defun find-c++-compiler ()
  (cond ((executable-find "clang++") "clang++ %s -g -o %s")
        ((executable-find "g++")     "g++ %s -g -o %s")
        ((executable-find "cl")      "cl %s /Z7 /Fe: %s")))

(defun get-c-compile-cmd-line (c-file executable-name)
  (let ((format (find-c-compiler)))
    (when format
      (format format c-file executable-name))))

(defun get-c++-compile-cmd-line (c++-file executable-name)
  (let ((format (find-c++-compiler)))
    (when format
      (format format c++-file executable-name))))

(defun make-c-compile-and-run-cmd (c-file executable-name)
  (lambda (_)
    (concat (get-c-compile-cmd-line
             c-file
             (concat executable-name (if is-windows ".exe")))
            " && " executable-name)))

(defun make-c++-compile-and-run-cmd (c++-file executable-name)
  (lambda (_)
    (concat (get-c++-compile-cmd-line
             c++-file
             (concat executable-name (if is-windows ".exe")))
            " && " executable-name)))

(setq build-script-names (list
                          (if is-windows (cons "build.ps1"  "powershell.exe -ExecutionPolicy Unrestricted -File build.ps1"))
                          (if is-windows "build.bat"         "./build.sh")
                          (if is-windows "build-windows.bat" "./build-linux.sh")
                          (if is-windows "bob.exe"           "./bob")

                          (cons "bob.c"   (make-c-compile-and-run-cmd "bob.c" "bob"))
                          (cons "bob.cpp" (make-c++-compile-and-run-cmd "bob.cpp" "bob"))

                          (cons "CMakeLists.txt"  "cmake -S . -B __build__ && cmake --build __build__")
                          (if is-windows '("build.sh" . "wsl bash -ic ./build.sh"))
                          (if is-windows
                              '("Makefile" . "wsl make")
                            '("Makefile"       . "make"))
                          ;; (cons "main.c"    (make-c-compile-and-run-cmd "main.c" "main"))
                          ;; (cons "main.cpp"  (make-c++-compile-and-run-cmd "main.cpp" "main"))
                          ))

(setq secondary-build-script-names
      (list
       (cons "main.c"    (make-c-compile-and-run-cmd "main.c" "main"))
       (cons "main.cpp"  (make-c++-compile-and-run-cmd "main.cpp" "main"))))

(if is-windows
    (append-to-list 'secondary-build-script-names
                    (list '("build.jai"      . "jai build.jai -quiet -exe app && app")
                          '("main.jai"       . "jai main.jai -quiet -exe app && app")
                          '("first.jai"      . "jai first.jai -quiet -exe app && app")))
  (append-to-list 'secondary-build-script-names
                  (list '("build.jai"      . "time jai build.jai -quiet -exe app && time ./app")
                        '("main.jai"       . "time jai main.jai -quiet -exe app && time ./app")
                        '("first.jai"      . "time jai first.jai -quiet -exe app && time ./app"))))



(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'msbuild-error)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(msbuild-error
                 "^\\(.*?\\)(\\([0-9]+\\),\\([0-9]+\\)): \\(?:\\(fatal \\)?error\\|\\(warning\\)\\|\\(message\\)\\) .*?:" 1 2 3 (4))))


(defun first-non-nil (l)
  (unless (null l)
    (if (car l)
        (car l)
      (first-non-nil (cdr l)))))

(defun filter-non-nil (l)
  (unless (null l)
    (if (car l)
        (cons (car l) (filter-non-nil (cdr l)))
      (filter-non-nil (cdr l)))))

(cl-defun longest-car-string (l &optional (max-so-far (cons nil nil)) (max-so-far-len 0))
  (if l (let* ((element (caar l))
               (others  (cdr l))
               (strlen  (length element)))
          (if (> strlen max-so-far-len)
              (longest-car-string others (car l) strlen)
            (longest-car-string others max-so-far max-so-far-len)))
    max-so-far))

(defun find-closest-build-script (build-script-list)
  (let* ((potential-paths (mapcar (lambda (build-script-name)
                                    (let ((name (if (consp build-script-name) (car build-script-name) build-script-name)))
                                      (when name ;; NOTE(Felix): name could be
                                        ;; nil since this kind of
                                        ;; build system is not
                                        ;; available on this
                                        ;; architecture
                                        (let ((path (locate-dominating-file (expand-file-name default-directory) name)))
                                          (when path
                                            (cons path name))))))
                                  build-script-list))
         (existing-paths (filter-non-nil potential-paths)))

    (if existing-paths
        (longest-car-string existing-paths)
      (cons nil nil))))

(cl-defun translate-build-file-to-command (file &optional (build-scrips build-script-names))
  (unless build-scrips
    (error "unkown build script"))
  (let* ((first          (car build-scrips))
         (iter-file-name (if (consp first) (car first) first)))
    (if (string= file iter-file-name)
        (if (consp first) (cdr first) first)
      (translate-build-file-to-command file (cdr build-scrips)))))

(setq compilation-finish-functions
      (list (lambda (&rest _)
              (compilation-minor-mode 1))))


(defun get-build-cmd-one-list (build-script-list)
  (let* ((path-and-script (find-closest-build-script build-script-list))
         (path            (car path-and-script))
         (script          (cdr path-and-script)))
    (when path
      (list (translate-build-file-to-command script build-script-list)
            script
            path))))

(defun get-build-cmd ()
  (let ((cmd (get-build-cmd-one-list build-script-names)))
    (if cmd
        cmd
      (let ((cmd2 (get-build-cmd-one-list secondary-build-script-names)))
        (if cmd2
            cmd2
          (error "no build files found"))))))

(defun find-build-script-and-compile ()
  (interactive)
  (let* ((command-script-and-path (get-build-cmd))
         (command (first  command-script-and-path))
         (script  (second command-script-and-path))
         (path    (third  command-script-and-path))
         (curr-dir default-directory))

      (let ((actual-command-str (cond ((stringp   command) command)
                                      ((functionp command) (funcall command script))
                                      (t (error "unsupported command type")))))

        (cd path)
        (compilation-start actual-command-str t)
        (cd curr-dir))))


(push '("^Comint \\(finished\\).*"
        (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
        (1 compilation-info-face))
      compilation-mode-font-lock-keywords)

(push '("^Comint \\(exited abnormally\\|interrupt\\|killed\\|terminated\\|segmentation fault\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
        (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
        (1 compilation-error-face)
        (2 compilation-error-face nil t))
      compilation-mode-font-lock-keywords)
