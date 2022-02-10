# .bashrc

sourceif() {
	if [[ -f $1 ]] ; then
		source $1
	fi
}

sourceif /etc/bashrc
sourceif $HOME/.bash_aliases

completeif() {
	if [[ -f $1 ]] ; then
		complete -C $1 $2
	fi
}

completeif /usr/local/bin/aws_completer aws
