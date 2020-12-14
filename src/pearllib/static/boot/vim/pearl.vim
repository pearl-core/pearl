
if isdirectory($PEARL_HOME)
    """"""""""""""""""
    " Package loader
    """"""""""""""""""
    if executable("pearl")
        let package_list = system('pearl list --dependency-tree --package-only --installed-only')
        for pkg_name in split(package_list, "\n")
            let config_path = $PEARL_HOME."/packages/".pkg_name."/pearl-config/config.vim"
            if filereadable(config_path)
                if $PEARL_DEBUG
                  echo "Running".config_path."..."
                endif
                let pkg_short_name = substitute(pkg_name, "^.*\/", "", "")
                let repo_name = substitute(pkg_name, "\/.*$", "", "")
                let $PEARL_PKGVARDIR = $PEARL_HOME.'/var/'.pkg_name
                let $PEARL_PKGDIR = $PEARL_HOME.'/packages/'.pkg_name
                let $PEARL_PKGNAME = pkg_short_name
                let $PEARL_PKGREPONAME = repo_name
                :exec ":source ".config_path
            endif
        endfor
    else
        echo "Pearl error: Could not load pearl package config files. `pearl` executable not found. Please update the PATH variable first."
    endif
endif
