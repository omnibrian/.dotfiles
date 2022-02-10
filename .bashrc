# .bashrc

sourceif() {
	if [[ -f $1 ]] ; then
		source $1
	fi
}

sourceif /etc/bashrc
sourceif $HOME/.bash_aliases
