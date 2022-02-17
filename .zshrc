# .zshrc

# ================ zsh-options ========================================
HISTFILE=~/.zsh_history
HISTSIZE=128000
SAVEHIST=128000

# dont auto cd if command is a folder name
unsetopt auto_cd

# automatically use menus for selection
setopt auto_menu

# push old dir onto dir stack on cd
setopt auto_pushd

# append history list to the history file
setopt append_history

# dont beep on error
unsetopt beep

# not just at the end
setopt completeinword

# dont try to correct spelling of all args in a line
unsetopt correct_all

# save each command's start time and duration
setopt extended_history

# in order to use #, ~, and ^ for filename generation grep word
setopt extended_glob

# dont have * match .dotfiles
setopt glob_dots

# ensure the entire command path is hashed first when completion is
# attempted
setopt hash_list_all

# skip adding to history when first char is a space
setopt histignorespace

# dont send sighup to background processes when shell exits
unsetopt hup

# display PID when suspending processes as well
setopt longlistjobs

# error if not filename matches
setopt nomatch

# report the status of background jobs immediately
setopt notify

# enable parameter expansion and command substitution in prompt
setopt prompt_subst

# dont push the same dir again
setopt pushd_ignore_dups

# dont error when unset parameters are used
setopt unset

expand-or-complete-with-dots() {
	COMPLETION_WAITING_DOTS="%F{red}…%f"
	# print prompt-expanded sequence with line wrapping off
	printf '\e[?7l%s\e[?7h' "${(%)COMPLETION_WAITING_DOTS}"

	zle expand-or-complete
	zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey -M emacs "^I" expand-or-complete-with-dots

# emacs keybindings
bindkey -e

export WORDCHARS="*?[]~=;!#$%^(){}<>"
# ================ zsh-newuser-install ================================


# ================ common-files =======================================
sourceif() {
	if [[ -f $1 ]] ; then
		source $1
	fi
}

sourceif /etc/zshrc
sourceif $HOME/.env
sourceif $HOME/.bash_aliases
sourceif $HOME/.fzf.zsh
# ================ common-files =======================================


# ================ completion =========================================
# allow one error every three characters typed in approximate completer
zstyle ':completion:*:approximate:' max-errors 'reply=( $((($#PREFIX+$#SUFFIX)/3 )) numeric )'

# start menu completion only if it could find no unambiguous initial string
zstyle ':completion:*:correct:*'   insert-unambiguous true
zstyle ':completion:*:corrections' format $'%{\e[0;31m%} -- %d (errors: %e) --%{\e[0m%}'
zstyle ':completion:*:correct:*'   original true

# activate color-completion
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# format on completion
zstyle ':completion:*:descriptions' format $'%{\e[0;32m%} -- %d --%{\e[0m%}'

# expand based on path suffix
zstyle ':completion:*' expand suffix

# insert all expansions for expand completer
zstyle ':completion:*:expand:*'      tag-order all-expansions
zstyle ':completion:*:history-words' list false

# activate menu
zstyle ':completion:*:history-words' menu yes

# ignore duplicate entries
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' stop yes

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'

# separate matches into groups
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*'         group-name ''

# use menu when matches dont fit on screen
zstyle ':completion:*' menu yes=long select

zstyle ':completion:*:messages' format ' -- %d --'
zstyle ':completion:*:options'  auto-description '%d'

# describe options in full
zstyle ':completion:*:options' description 'yes'

# complete all user processes
zstyle ':completion:*:processes' command 'ps -au ${USER}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# provide verbose completion information
zstyle ':completion:*' verbose true

# disable trying to provide descriptions for commands because slow
zstyle ':completion:*:-command-:*:' verbose false

# set format for warnings
zstyle ':completion:*:warnings' format $'%{\e[0;33m%} -- no matches --%{\e[0m%}'

# ignore completion functions for commands that dont exist
zstyle ':completion::(^approximate*):*:functions' ignored-patterns '_*'

# provide more processes in completion of programs like killall
zstyle ':completion:*:processes-names' command 'ps -c -u ${USER} -o command | uniq'

# complete manual by their section
zstyle ':completion:*:manuals'   separate-sections true
zstyle ':completion:*:manuals.*' insert-sections true
zstyle ':completion:*:man:*'     menu yes select

# search path for sudo completion
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin \
                                           /usr/local/bin  \
					   /usr/sbin       \
					   /usr/bin        \
					   /sbin           \
					   /bin

# provide .. as a completion
#zstyle ':completion:*' special-dirs ..

# treat multiple slashes as a single slash
zstyle ':completion:*' squeeze-slashes true

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories

# attempt to complete with multiple partial pathname components
zstyle ':completion:*' list-suffixes true

# set completer functions
zstyle ':completion:*' completer _oldlist _expand _complete _ignored _correct _approximate _files

# cache completions
zstyle ':completion:*'            use-cache yes
zstyle ':completion:*:complete:*' cache-path $HOME/.cache/zshcompletion

# start up completion
autoload -Uz compinit
compinit

# history on up/down arrows
autoload -Uz up-line-or-beginning-search
autoload -Uz down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search
bindkey "^[[B" down-line-or-beginning-search

# bash completion
autoload -Uz bashcompinit
bashcompinit

# awscli autocompletion
if command -v aws_completer &>/dev/null ; then
	complete -C aws_completer aws
fi
# ================ completion =========================================


# ================ prompt =============================================
# run functions based on a hook
autoload -Uz add-zsh-hook

# update guake title but in a background job since the guake call is
# slow but also stuff it into a subprocess with hidden output to avoid
# background job logs showing up in foreground
guake-retitle() {
	if [[ -n "${GUAKE_TAB_UUID}" ]] ; then
		if [[ -n "$1" ]] && ! [[ "$1" =~ "^cd" ]] ; then
			(&>/dev/null guake -r "$1" &)
		else
			(&>/dev/null guake -r "$(shrink-path)" &)
		fi
	fi
}
# retitle to running command
add-zsh-hook preexec guake-retitle
guake-retitle  # retitle to pwd on start

# retitle to pwd when printing prompt
precmd() {
	guake-retitle
}

# load colors for prompt
autoload -Uz colors
colors

# secondary prompt, used when shell needs more info for completion
PS2='[%_]> '

# selection prompt used within a select loop
PS3='#? '

# execution trace prompt (setopt xtrace) [default: '+%N:%i>']
PS4='+%N:%i>'

# right-side prompt
RPS1='%{$fg[red]%}$(git-dirty)%{$fg[blue]%}$(git-branch)%{$fg[white]%}[%D{%H:%M}]%{%b%}'

# main prompt
PROMPT='%{$fg[yellow]%}[$(shrink-path)% ]%(?.%{$fg[green]%}.%{$fg[red]%})%B%(!.#.$)%b '
# ================ prompt =============================================
