# .bash_aliases
#
# Note: this file is meant to be sourced in bash and zsh

export VISUAL=vim
export EDITOR=$VISUAL
export SYSTEMD_PAGER=  # disable systemctl's auto-paging
export AWS_PAGER=      # disable awscli auto-paging

# PATH manipulation
if ! [[ "$MANPATH" =~ "$HOME/.local/share/man" ]] ; then
	export MANPATH="$MANPATH:$HOME/.local/share/man"
fi

addpath() {
	if ! [[ "$PATH" =~ "$1" ]] ; then
		export PATH="$1:$PATH"
	fi
}

addpath "$HOME/.local/bin"
addpath "$HOME/bin"

# aliases
alias ll='ls -l'
alias la='ls -la'
alias l='ls -lah'
alias g='git'
alias cdd='cd $HOME/Downloads'
alias cdg='cd $HOME/git'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias emac='emacsclient -c'
alias _='sudo'
alias k='kubectl'
alias kc='kubectx'
alias kn='kubens'

# handle compressed packages
extract() {
	if [[ -f $1 ]] ; then
		case $1 in
			*.tar.bz2) tar xvjf $1 ;;
			*.tar.gz) tar xvzf $1 ;;
			*.tar.xz) tar xf $1 ;;
			*.bz2) bunzip2 $1 ;;
			*.gz) gunzip $1 ;;
			*.tar) tar xvf $1 ;;
			*.tbz2) tar xvjf $1 ;;
			*.tgz) tar xvzf $1 ;;
			*.zip) unzip $1 ;;
			*.Z) uncompress $1 ;;
			*.7z) 7za x $1 ;;
			*.rar) unrar $1 ;;
			*) echo "'$1' not a recognized file format" ;;
		esac
	else
		echo "'$1' is not a valid file"
	fi
}
