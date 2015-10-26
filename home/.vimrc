
" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible
 
set encoding=utf-8

let s:has_windows = has("win32")
let s:has_unix = has("unix")
let s:has_gui = has("gui_running")

" Set filetype off for vundle, it will be turned back on later
filetype off

set rtp+=~/.vim
set rtp+=~/.vim/bundle/vundle.vim
"call vundle#rc
let path='~/.vim/bundle'
call vundle#begin(path)

Plugin 'gmarik/Vundle.vim'

" My Plugins here:
" original repos on github
Plugin 'majutsushi/tagbar'
Plugin 'klen/python-mode'
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'davidhalter/jedi-vim'
"Plugin 'jmcantrell/vim-virtualenv'
Plugin 'PProvost/vim-ps1'
Plugin 'kchmck/vim-coffee-script'
"Plugin 'ivanov/vim-ipython'
Plugin 'fatih/vim-go'
Plugin 'sukima/xmledit'
Plugin 'vim-scripts/slimv.vim'
Plugin 'chrisbra/csv.vim'

"Color schemes
"Plugin 'altercation/vim-colors-solarized'
Plugin 'tomasr/molokai'
"Plugin 'tpope/vim-vividchalk'
Plugin 'morhetz/gruvbox'

" Github repos of the user 'vim-scripts'
" => can omit the username part
Plugin 'L9'
Plugin 'FuzzyFinder'

" non github repos
Plugin 'git://git.wincent.com/command-t.git'
" ...
"
" Required, plugins available after.

" Windows only plugins here
if s:has_windows && !s:has_gui
    "Plugin 'Lokaltog/vim-powerline'
endif
call vundle#end()

filetype plugin indent on

"source $VIMRUNTIME/mswin.vim
"behave mswin
"behave xterm

set autochdir
set autoindent
set backspace=indent,eol,start
set colorcolumn=80
set directory=~/.vim/swap
set noerrorbells visualbell t_vb=
set expandtab
set fillchars+=stl:\ ,stlnc:\
set foldlevel=99
set foldmethod=indent
set history=50		" keep 50 lines of command line history
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
set tabstop=4
set textwidth=120
set ttyfast

" Wildmenus
set wildchar=<Tab>
set wildmenu
set wildmode=full
set wildcharm=<C-Z>
nnoremap <F10> :b <C-Z>

syntax enable

if s:has_gui
    set guifont=DejaVu\ Sans\ Mono\ for\ Powerline:h10
    highlight Pmenu guibg=#cccccc gui=bold

    set lines=999
    set columns=999
endif

if s:has_gui || s:has_unix || s:has_windows
    " Use default python due to str byte conversion
    "let g:powerline_pycmd='python'
    "set rtp+=C:/tools/python/Lib/site-packages/powerline/bindings/vim
    python from powerline.vim import setup as powerline_setup
    python powerline_setup()
    python del powerline_setup
endif

if !empty($CONEMUBUILD) && !s:has_gui
    "let g:Powerline_symbols = 'fancy'
    " ConEmu setup
    silent echom "Running in conemu"
    " xterm settings
    set term=xterm
    "set termencoding=utf-8
    set t_Co=256
    set t_RV=xterm-256color
    let &t_AB="\e[48;5;%dm"
    let &t_AF="\e[38;5;%dm"

    " mouse wheel in conemu
    inoremap <Esc>[62~ <C-X><C-E>
    inoremap <Esc>[63~ <C-X><C-Y>
    nnoremap <Esc>[62~ <C-E>
    nnoremap <Esc>[63~ <C-Y>

    "set notimeout		" don't timeout on mappings
    "set ttimeout		" do timeout on terminal key codes
    "set timeoutlen=150	" timeout after 150 msec

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

" Only do this part when compiled with support for autocommands.
if has("autocmd")

    " Enable file type detection.
    " Use the default filetype settings, so that mail gets 'tw' set to 72,
    " 'cindent' is on in C files, etc.
    " Also load indent files, to automatically do language-dependent indenting.
    filetype plugin indent on

    augroup vimrc_autocmds
        autocmd!
        autocmd GUIEnter * set visualbell t_vb=
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

if !empty($WORKON_HOME) | let g:virtualenv_directory=$WORKON_HOME | endif
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

map Q gq|                          "Don't use Ex mode, use Q for formatting

" Toggles
map <F2> :NERDTreeToggle<CR>
nnoremap <silent><leader>n :set relativenumber!<CR>
nmap <leader>1 :set list!<CR>
nmap <leader>w :set wrap!<CR>

map <F3> "zyiw:exe "h ".@z.""<CR>| "Help for word under cursor


" vimrc commands
map <F6> :so $HOME/_vimrc<CR>
map <F9> :vsp $HOME/_vimrc<CR>

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

" Center screen on searches...
nmap n nzz
nmap N Nzz

nnoremap <leader>q @q|             "Play macro recorded to q
map <leader>f :noh<CR>|            "Remove search highlight
map <leader>m :messages<CR>|       "Show messages


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
        set verbosefile=c:/temp/log/vim/verbose.log
        set verbose=15
    else
        set verbose=0
        set verbosefile=
    endif
endfunction
nmap <leader>v :call ToggleVerbose()<CR>
nmap <leader>l :vsp c:/temp/log/vim/verbose.log<CR>:e!<CR>

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

function! Dos2unix()
    update
    e ++ff=dos
    setlocal ff=unix
    w
endfunction
command! -nargs=0 D2u :call Dos2unix()

function! Unix2dos()
    update
    e ++ff=dos
    w
endfunction
command! -nargs=0 U2d :call Unix2dos()


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
"
" Highlight a column in csv text.
" :Csv 1    " highlight first column
" :Csv 12   " highlight twelfth column
" :Csv 0    " switch off highlight
"function! CSVH(colnr)
  "if a:colnr > 1
    "let n = a:colnr - 1
    "execute 'match Keyword /^\([^,]*,\)\{'.n.'}\zs[^,]*/'
    "execute 'normal! 0'.n.'f,'
  "elseif a:colnr == 1
    "match Keyword /^[^,]*/
    "normal! 0
  "else
    "match
  "endif
"endfunction
"command! -nargs=1 Csv :call CSVH(<args>)

