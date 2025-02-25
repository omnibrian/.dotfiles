# -*- mode: sh; indent-tabs-mode: t; -*-
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
	PSTIME="\[\e[39m\][\A]\[\e[0m\]"
	PSPATH="\[\e[36m\][$(shrink-path)]\[\e[0m\]"
	PSGITBRANCH="\[\e[34m\]$(git-branch)\[\e[0m\]"
	PSGITSTASHED="\[\e[33m\]$(git-stashed)\[\e[0m\]"
	PSGITDIRTY="\[\e[31m\]$(git-dirty)\[\e[0m\]"
	PSAWSPROFILE="\[\e[35m\]$(aws-profile)\[\e[0m\]"
	PSPROMPT="\[\e[32m\]$([[ ${rc} -eq 0 ]] || echo '\[\e[31m\]')\$\[\e[0m\]"
	PS1="${PSTIME}${PSAWSPROFILE}${PSPATH}${PSGITBRANCH}${PSGITSTASHED}${PSGITDIRTY}${PSPROMPT} "
}

PROMPT_COMMAND=bash-prompt
# ================ prompt =============================================
