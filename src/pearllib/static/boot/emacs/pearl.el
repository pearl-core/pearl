; This file is used for loading the config.el defined in the Pearl packages.

(defun source-config-file (pkg-name)
  (setq pkg-short-name (replace-regexp-in-string "^.*\/" "" pkg-name))
  (setq repo-name (replace-regexp-in-string "\/.*$" "" pkg-name))

  (setq config-file
    (concat pearl-home "packages/" pkg-name "/pearl-config/config.el")
  )
  ; To verify that this variable are visible use (getenv "VARNAME") from scratch buffer
  (setenv "PEARL_PKGDIR" (concat pearl-home "packages/" pkg-name))
  (setenv "PEARL_PKGVARDIR" (concat pearl-home "var/" pkg-name))
  (setenv "PEARL_PKGNAME" pkg-short-name)
  (setenv "PEARL_PKGREPONAME" repo-name)
  (if (file-exists-p config-file)
    (progn
      (if (getenv "PEARL_DEBUG")
        ; To check these lines go to Messages buffer (C-x C-b)
        (print (concat "Running " config-file "..."))
      )
      (load-file config-file)
    )
  )
  (setenv "PEARL_PKGDIR" nil)
  (setenv "PEARL_PKGVARDIR" nil)
  (setenv "PEARL_PKGNAME" nil)
  (setenv "PEARL_PKGREPONAME" nil)
)

(defun pearl-main ()
    ; This appends a slash at the end of the variable
    (setq pearl-home (file-name-as-directory (getenv "PEARL_HOME")))

    (setq pearl-pkg-names
      (split-string
        (shell-command-to-string "pearl list --dependency-tree --package-only --installed-only")
        "\n"
      )
    )

    (dolist (pkg-name pearl-pkg-names)
      (source-config-file pkg-name)
    )
)

(if (and (getenv "PEARL_HOME") (file-readable-p (getenv "PEARL_HOME")) (executable-find "pearl") )
  (pearl-main)
  (message "Pearl error: Could not load pearl package config files. `pearl` executable not found. Please update the PATH variable first.")
)
