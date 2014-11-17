#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias vi=vim
alias aura='sudo aura'
alias sshx='ssh -XC'
alias ls='ls -v --color=auto'
alias links='rlwrap links'

export EDITOR='emacs -nw'

HISTCONTROL=erasedups

# Tell Java that XMonad is nonreparenting
export _JAVA_AWT_WM_NONREPARENTING=1

#Colorful prompt, shows current git branch                                                                
#TODO: Use a separate color for every host. Hash or dispatch manually.
PS1='\[\033[01;32m\]\u\[\033[00m\]@\[\033[01;34m\]\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\[\033[0;36m\]`git branch 2>/dev/null|cut -f2 -d\* -s`\[\033[00m\]\$ '
