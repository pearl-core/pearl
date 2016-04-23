; This file is used for loading the config.el defined in the Pearl packages.

(setq pearl-config-files
  (file-expand-wildcards
    (concat (file-name-as-directory (getenv "PEARL_HOME"))
            "packages/*/*/pearl-metadata/config.el"
    )
  )
)

(defun source-config-file (config-file)
  (setenv "PEARL_PKGDIR" (replace-regexp-in-string "\/pearl-metadata\/.*$" "" config-file))
  (load-file config-file)
)

(dolist (config-file pearl-config-files)
  (source-config-file config-file)
)
