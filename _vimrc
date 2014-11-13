
" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible
 
set encoding=utf-8

" Set filetype off for vundle, it will be turned back on later
filetype off

set rtp+=~/.vim/bundle/vundle/
"call vundle#rc
let path='~/.vim/bundle'
call vundle#begin(path)

Bundle 'gmarik/vundle'

" My Bundles here:
" original repos on github
Bundle 'majutsushi/tagbar'
Bundle 'klen/python-mode'
"Bundle 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim'} "Installed powerline w/ pip...
Bundle 'tpope/vim-fugitive'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/nerdcommenter'
Bundle 'davidhalter/jedi-vim'
Bundle 'jmcantrell/vim-virtualenv'
"Bundle 'ivanov/vim-ipython'

"Color schemes
"Bundle 'altercation/vim-colors-solarized'
Bundle 'tomasr/molokai'
"Bundle 'tpope/vim-vividchalk'
Bundle 'morhetz/gruvbox'

" Github repos of the user 'vim-scripts'
" => can omit the username part
Bundle 'L9'
Bundle 'FuzzyFinder'

" non github repos
Bundle 'git://git.wincent.com/command-t.git'
" ...
"
" Required, plugins available after.
call vundle#end()
filetype plugin indent on

"source $VIMRUNTIME/mswin.vim
"behave mswin
"behave xterm

" Use default python due to str byte conversion
"let g:powerline_pycmd='python'
set rtp+=C:/tools/python/Lib/site-packages/powerline/bindings/vim
python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup

set autochdir
set autoindent
set backspace=indent,eol,start
set colorcolumn=80
set directory=~/.vim/swap
set expandtab
set foldlevel=99
set foldmethod=indent
set incsearch 
set laststatus=2
set listchars=eol:$,tab:>-,trail:~,extends:>,precedes:<
set mouse=a
set nobackup
set noshowmode
set noundofile
set ruler
set shiftround
set shiftwidth=4
set shortmess=aIostT
set showcmd
set showtabline=2
set smartcase
set softtabstop=4
set splitbelow
set splitright
set t_vb=
set tabstop=4
set textwidth=80
set history=50		" keep 50 lines of command line history

" Wildmenus
set wildchar=<Tab>
set wildmenu
set wildmode=full
set wildcharm=<C-Z>
nnoremap <F10> :b <C-Z>

syntax enable

if has ('gui_running')
    set guifont=DejaVu\ Sans\ Mono\ for\ Powerline:h10
    highlight Pmenu guibg=#cccccc gui=bold

    set lines=999
    set columns=999
endif

if !empty($CONEMUBUILD) && !has('gui_running')
    " ConEmu setup
    silent echom "Running in conemu"
    " xterm settings
    set term=xterm
    "set termencoding=utf8
    set t_Co=256
    "set notimeout		" don't timeout on mappings
    "set ttimeout		" do timeout on terminal key codes
    "set timeoutlen=150	" timeout after 150 msec
    set t_RV=xterm-256color
    let &t_AB="\e[48;5;%dm"
    let &t_AF="\e[38;5;%dm"
    set ttyfast

    " termcap codes for cursor shape changes on entry and exit to
    " /from insert mode
    " doesn't work
    "let &t_ti="\e[1 q"
    "let &t_SI="\e[5 q"
    "let &t_EI="\e[1 q"
    "let &t_te="\e[0 q"
    " termcap options
    "set t_ks=
    "set t_ke=
endif

colorscheme molokai
set background=dark
highlight LineNr term=bold cterm=NONE ctermfg=Grey ctermbg=Blue gui=NONE guifg=Grey guibg=DodgerBlue4
"highlight SignColumn ctermbg=blue guibg=blue
set cursorline
"set cursorcolumn
"hi Cursor ctermbg=Grey
hi CursorLine term=none cterm=none ctermfg=none guibg=black
"hi CursorColumn term=none cterm=none ctermbg=none ctermbg=none guibg=Grey95

" Powerline setup
"set guifont=Consolas\ for\ Powerline\ FixedD:h10
"set guifont=Sauce\ Code\ Powerline:h10
"let g:Powerline_symbols="fancy"
"more subtle popup colors

" Only do this part when compiled with support for autocommands.
if has("autocmd")

    " Enable file type detection.
    " Use the default filetype settings, so that mail gets 'tw' set to 72,
    " 'cindent' is on in C files, etc.
    " Also load indent files, to automatically do language-dependent indenting.
    filetype plugin indent on

    augroup vimrc_autocmds
        autocmd!
        "autocmd FileType python nnoremap <buffer> <C-F10> :w<CR>:!python %<CR>

        " When editing a file, always jump to the last known cursor position.
        " Don't do it when the position is invalid or when inside an event handler
        " (happens when dropping a file on gvim).
        " Also don't do it when the mark is in the first line, that is the default
        " position when opening a file.
        autocmd BufReadPost *
            \ if line("'\"") > 1 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

        autocmd FocusGained,BufEnter,InsertLeave * :set number relativenumber
        autocmd BufLeave,FocusLost,InsertEnter * :set number norelativenumber
        "au InsertEnter * hi CursorLine term=none cterm=underline
        "au InsertLeave * hi CursorLine term=none cterm=none ctermbg=none
        "au InsertEnter * hi CursorColumn term=none ctermbg=darkblue
        "au InsertLeave * hi CursorColumn term=none cterm=none ctermbg=none
    augroup END

endif " has("autocmd")

"let g:ipy_perform_mappings != 0

let g:jedi#auto_initialization=1
let g:jedi#use_tabs_not_buffers=0

let g:virtualenv_directory=$WORKON_HOME
let g:virtualenv_stl_format = '[%n]'

let g:pymode=1
let g:pymode_breakpoint = 1
let g:pymode_breakpoint_bind = '<leader>b'
let g:pymode_doc = 0
let g:pymode_folding=1
let g:pymode_indent=1
let g:pymode_lint=1
let g:pymode_lint_on_fly=0
let g:pymode_lint_on_write=1
let g:pymode_lint_sort = ['E', 'C', 'I']
let g:pymode_motion=1
let g:pymode_options=1
let g:pymode_options_colorcolumn=1
let g:pymode_options_max_line_length=80
let g:pymode_python='python'
let g:pymode_rope=0
let g:pymode_run=1
let g:pymode_run_bind='<C-F10>'
let g:pymode_trim_whitespaces=1
"let g:pymode_virtualenv=1

let g:NERDTreeShowHidden=1

" jk mapped to esc in Insert mode 
map! jk <Esc>

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" Don't use Ex mode, use Q for formatting
map Q gq

" Toggles
map <F2> :NERDTreeToggle<CR>
nnoremap <silent><leader>n :set relativenumber!<CR>
nmap <leader>1 :set list!<CR>

" Help for word under cursor
map <F3> "zyiw:exe "h ".@z.""<CR> 

" vimrc commands
map <F9> :vsp $HOME/_vimrc<CR>
map <F6> :so $HOME/_vimrc<CR>

"Ctrl-[hjkl] to move within splits
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

"Alt-[jk] to move lines up/down
nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv

map <Space> <PageDown>
map <S-Space> <PageUp>
"map <Space> <PageDown>
"map <S-Space> <PageUp>

" Center screen on searches...
nmap n nzz
nmap N Nzz

" Play macro recorded to q
nnoremap <leader>q @q

" Remove search highlight
map <leader>f :noh<CR>

" Show messages
map <leader>m :messages<CR>

":vnoremap <f5> :!python<CR>
"nmap <buffer> <F5> :w<esc>mwG:r!py %<CR>'.

function! MyDiff()
  let opt = '-a --binary '
  if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
  if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
  let arg1 = v:fname_in
  if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
  let arg2 = v:fname_new
  if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
  let arg3 = v:fname_out
  if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
  let eq = ''
  if $VIMRUNTIME =~ ' '
    if &sh =~ '\<cmd'
      let cmd = '"' . $VIMRUNTIME . '\diff"'
      let eq = '""'
    else
      let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
    endif
  else
    let cmd = $VIMRUNTIME . '\diff'
  endif
  silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3 . eq
endfunction
set diffexpr=MyDiff()

" Toggle logging to file
function! ToggleVerbose()
    if !&verbose
        set verbosefile=c:/tmp/log/vim/verbose.log
        set verbose=15
    else
        set verbose=0
        set verbosefile=
    endif
endfunction
nmap <leader>v :call ToggleVerbose()<CR>
nmap <leader>l :vsp c:/tmp/log/vim/verbose.log<CR>:e!<CR>

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

" Add the virtualenv's site-packages to vim path
"py << EOF
"import os.path
"import sys
"import vim
"if 'VIRTUAL_ENV' in os.environ:
    "project_base_dir = os.environ['VIRTUAL_ENV']
    "sys.path.insert(0, project_base_dir)
    "activate_this = os.path.join(project_base_dir, 'scripts/activate_this.py')
    "execfile(activate_this, dict(__file__=activate_this))
"EOF

