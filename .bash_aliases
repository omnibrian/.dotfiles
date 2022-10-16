# .bash_aliases
#
# Note: this file is meant to be sourced in bash and zsh


# ================ exports ============================================
export VISUAL=${VISUAL:-vim}
export EDITOR=${EDITOR:-vim}
export PAGER=${PAGER:-less -R}
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

# ls colors
COLORS=

if [[ -e "$HOME/.dircolors.256color" ]] &&
		[[ "$(tty -s && tput colors 2>/dev/null)" = "256" ]] ; then
	COLORS="$HOME/.dircolors.256color"
elif [[ -e "$HOME/.dircolors" ]] ; then
	COLORS="$HOME/.dircolors"
fi

if [[ -n "$COLORS" ]] ; then
	eval "$(dircolors --sh $COLORS)"
fi
# ================ exports ============================================


# ================ aliases ============================================
alias ls='ls --color=auto'
alias ll='ls -lB'
alias la='ls -la'
alias l='ls -lah'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias g='git'
alias cdd='cd $HOME/Downloads'
alias cdg='cd $HOME/git'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias pip='python -m pip'
alias _='sudo'
alias emac='emacsclient -c'
alias k='kubectl'
alias kc='kubectx'
alias kn='kubens'
# ================ aliases ============================================


# ================ utilities ==========================================
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

shrink-path() {
	echo ${PWD/#$HOME/\~} | awk -F/ '{ for(i=1; i<=NF-1; i++) { printf substr($i,1,1) "/" } printf $NF }'
}

git-branch() {
	type git &>/dev/null || return

	local git_branch
	git_branch=$(git describe --contains --all HEAD 2>/dev/null)
	[[ -z "$git_branch" ]] && return

	echo "[${git_branch}]"
}

git-dirty() {
	type git &>/dev/null || return
	[[ -n "$(git status --porcelain 2>/dev/null)" ]] || return
	echo '*'
}
# ================ utilities ==========================================
