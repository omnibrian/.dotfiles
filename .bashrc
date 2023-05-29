# .bashrc

# ================ common-files =======================================
sourceif() {
	if [[ -f $1 ]] ; then
		source $1
	fi
}

sourceif /etc/bashrc
sourceif $HOME/.env
sourceif $HOME/.bash_aliases
sourceif $HOME/.fzf.bash
sourceif $HOME/.docker/init-bash.sh || true
# ================ common-files =======================================


# ================ completion =========================================
completeif() {
	if [[ -f $1 ]] ; then
		complete -C $1 $2
	fi
}

completeif /usr/local/bin/aws_completer aws
# ================ completion =========================================


# ================ prompt =============================================
bash-prompt() {
	local rc="$?"
	PS1="\[\e[39m\][\A]\[\e[36m\][$(shrink-path)]\[\e[34m\]$(git-branch)\[\e[32m\]$([[ $rc -eq 0 ]] || echo '\[\e[31m\]')\$\[\e[31m\]$(git-dirty)\[\e[0m\] "
}

PROMPT_COMMAND=bash-prompt
# ================ prompt =============================================
