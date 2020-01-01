; This file is used for loading the config.el defined in the Pearl packages.

(defun source-config-file (config-file)
  (setq pkg-name (replace-regexp-in-string "^.*\/packages\/" "" (replace-regexp-in-string "\/pearl-config\/.*$" "" config-file)))
  (setq pkg-short-name (replace-regexp-in-string "^.*\/" "" pkg-name))
  (setq repo-name (replace-regexp-in-string "\/.*$" "" pkg-name))

  ; To verify that this variable are visible use (getenv "VARNAME") from scratch buffer
  (setenv "PEARL_PKGDIR" (concat pearl-home "packages/" pkg-name))
  (setenv "PEARL_PKGVARDIR" (concat pearl-home "var/" pkg-name))
  (setenv "PEARL_PKGNAME" pkg-short-name)
  (setenv "PEARL_PKGREPONAME" repo-name)
  (load-file config-file)
  (setenv "PEARL_PKGDIR" nil)
  (setenv "PEARL_PKGVARDIR" nil)
  (setenv "PEARL_PKGNAME" nil)
  (setenv "PEARL_PKGREPONAME" nil)
)

(defun pearl-main ()
    (setq pearl-home (file-name-as-directory (getenv "PEARL_HOME")))

    (setq pearl-config-files
      (file-expand-wildcards
        (concat pearl-home
                "packages/*/*/pearl-config/config.el"
        )
      )
    )

    (dolist (config-file pearl-config-files)
      (source-config-file config-file)
    )
)

(if (and (getenv "PEARL_HOME") (file-readable-p (getenv "PEARL_HOME"))) (pearl-main) )
