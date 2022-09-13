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

# Directories
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_minus

bindkey -e

# History
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/history"
HISTSIZE=1000000
SAVEHIST=1000000
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
autoload -Uz compinit && compinit
zstyle ':completion:*' menu select 
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zmodload zsh/complist
_comp_options+=(globdots)

autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

setopt prompt_subst
setopt interactive_comments
setopt multios

# Redefine Commands
alias diff='diff --color'
alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
alias egrep='egrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
alias fgrep='fgrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
alias lf='lfenv'
alias ls='exa --group-directories-first'

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
	if [ -f "$tmp" ]; then
		dir="$(cat "$tmp")"
		rm -f "$tmp" >/dev/null
		[ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
	fi
}
bindkey -s '^o' 'lfcd\n'
bindkey '^[[P' delete-char

# Path Extensions
export PATH="$HOME/.local/bin/statusbar:$PATH"
export PATH="$HOME/.poetry/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"

# Plugins
for plugin in $(find -L $ZDOTDIR/ -type f -name '*.plugin.zsh'); do
	source $plugin
done

[ -f "/home/ryan/.ghcup/env" ] && source "/home/ryan/.ghcup/env" # ghcup-env

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
# [[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh
eval "$(starship init zsh)"
