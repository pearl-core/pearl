; This file is used for loading the config.el defined in the Pearl packages.

(setq pearl-config-files
  (file-expand-wildcards
    (concat (file-name-as-directory (getenv "PEARL_HOME"))
            "packages/*/*/pearl-metadata/config.el"
    )
  )
)

(dolist (config-file pearl-config-files)
  (load-file config-file)
)
