
if isdirectory($PEARL_HOME)
    """"""""""""""""""
    " Package loader
    """"""""""""""""""
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
