; This file is used for loading the config.el defined in the Pearl packages.

(defun source-config-file (config-file)
  (setq pkg-name (replace-regexp-in-string "^.*\/packages\/" "" (replace-regexp-in-string "\/pearl-metadata\/.*$" "" (replace-regexp-in-string "\/pearl-config\/.*$" "" config-file))))

  (setenv "PEARL_PKGDIR" (concat pearl-home "packages/" pkg-name))
  (setenv "PEARL_PKGVARDIR" (concat pearl-home "var/" pkg-name))
  (load-file config-file)
  (setenv "PEARL_PKGDIR" nil)
  (setenv "PEARL_PKGVARDIR" nil)
)

(defun pearl-main ()
    (setq pearl-home (file-name-as-directory (getenv "PEARL_HOME")))

    ; TODO pearl-metadata directory is meant to be deprecated in the future versions
    (setq pearl-config-files
      (file-expand-wildcards
        (concat pearl-home
                "packages/*/*/pearl-metadata/config.el"
        )
      )
    )

    (dolist (config-file pearl-config-files)
      (source-config-file config-file)
    )

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
