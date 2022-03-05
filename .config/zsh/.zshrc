# export ZSH="/home/ryan/.oh-my-zsh"

# # Which plugins would you like to load?
# # Standard plugins can be found in $ZSH/plugins/
# # Custom plugins may be added to $ZSH_CUSTOM/plugins/
# # Example format: plugins=(rails git textmate ruby lighthouse)
# # Add wisely, as too many plugins slow down shell startup.
# plugins=(git colored-man-pages zsh-fzf-history-search zsh-syntax-highlighting) 

# source $ZSH/oh-my-zsh.sh

###############################################################################
# General
autoload -U colors && colors

setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus
setopt multios
setopt prompt_subst

bindkey -e

# History
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt share_history

bindkey ' ' magic-space

# Jobs
setopt long_list_jobs

# Prompt
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

setopt interactivecomments

# Redefine Commands
alias ls='exa --group-directories-first'
alias diff='diff --color'
alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
alias egrep='egrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
alias fgrep='fgrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'

# User configuration
source $ZDOTDIR/alias.zsh

export DEFAULT_USER=ryan
set -o emacs

# Colors
eval $( dircolors -b $HOME/.config/shell/colors/dircolors.ansi-universal )

# Functions
lfcd () {
	tmp="$(mktemp)"
	lf -last-dir-path="$tmp" "$@"
	if [-f "$tmp" ]; then
		dir="$(cat "$tmp")"
		rm -f "$tmp" >/dev/null
		[ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
	fi
}
bindkey -s '^o' 'lfcd\n'

# Path Extensions
export PATH="$HOME/.poetry/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"

# Plugins
for plugin in $(find $ZDOTDIR/ -type f -name '*.plugin.zsh'); do
	source $plugin
done

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
# [[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh
eval "$(starship init zsh)"
