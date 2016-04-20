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
    if filereadable(config_path)
        :exec ":source ".config_path
    endif
endfor
