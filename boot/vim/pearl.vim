
if isdirectory($PEARL_ROOT) && isdirectory($PEARL_HOME)
    """"""""""""""""""""""""""""""""""""""""""""""""""""
    " Pathogen (https://github.com/tpope/vim-pathogen)
    """"""""""""""""""""""""""""""""""""""""""""""""""""
    source $PEARL_ROOT/boot/vim/vim.d/plugins/pathogen/autoload/pathogen.vim
    filetype off
    call pathogen#infect($PEARL_HOME.'/packages/*/*/{}')
    call pathogen#helptags()
    filetype plugin indent on
    syntax on


    """"""""""""""""""
    " Package loader
    """"""""""""""""""
    for config_path in split(globpath($PEARL_HOME."/packages/*/*/pearl-metadata", 'config.vim'), "\n")
        let pkg_name = substitute(substitute(config_path, "^.*\/packages\/", "", ""), "\/pearl-metadata\/config\.vim$", "", "")
        if filereadable(config_path)
            let $PEARL_PKGVARDIR = $PEARL_HOME.'/var/'.pkg_name
            let $PEARL_PKGDIR = $PEARL_HOME.'/packages/'.pkg_name
            :exec ":source ".config_path
        endif
    endfor
endif
