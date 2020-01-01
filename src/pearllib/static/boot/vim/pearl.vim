
if isdirectory($PEARL_ROOT) && isdirectory($PEARL_HOME)
    """"""""""""""""""
    " Package loader
    """"""""""""""""""
    " TODO pearl-metadata directory is meant to be deprecated in the future versions
    for config_path in split(globpath($PEARL_HOME."/packages/*/*/pearl-metadata", 'config.vim'), "\n")
        let pkg_name = substitute(substitute(config_path, "^.*\/packages\/", "", ""), "\/pearl-metadata\/config\.vim$", "", "")
        if filereadable(config_path)
            let $PEARL_PKGVARDIR = $PEARL_HOME.'/var/'.pkg_name
            let $PEARL_PKGDIR = $PEARL_HOME.'/packages/'.pkg_name
            :exec ":source ".config_path
        endif
    endfor

    for config_path in split(globpath($PEARL_HOME."/packages/*/*/pearl-config", 'config.vim'), "\n")
        if filereadable(config_path)
            let pkg_name = substitute(substitute(config_path, "^.*\/packages\/", "", ""), "\/pearl-config\/config\.vim$", "", "")
            let pkg_short_name = substitute(pkg_name, "^.*\/", "", "")
            let repo_name = substitute(pkg_name, "\/.*$", "", "")
            let $PEARL_PKGVARDIR = $PEARL_HOME.'/var/'.pkg_name
            let $PEARL_PKGDIR = $PEARL_HOME.'/packages/'.pkg_name
            let $PEARL_PKGNAME = pkg_short_name
            let $PEARL_PKGREPONAME = repo_name
            :exec ":source ".config_path
        endif
    endfor
endif
