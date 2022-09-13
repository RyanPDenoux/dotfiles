alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'

alias S='systemctl'
alias J='journalctl'
alias s='systemctl --user'
alias j='journalctl --user'

alias emacs='emacsclient -nc'

alias g='git'
alias gst='git status'
alias ga='git add'
alias gcsm='git commit -s -m'
alias gcb='git checkout -b'
alias gdf='git diff'
alias glg='git log --stat'
alias glgp='git log --stat -p'
alias glo='git log --oneline --decorate'
alias glol="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset'"
alias ggpush='git push origin "$(git_current_branch)"'
alias grbm='git rebase $(git_main_branch)'

alias config='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
alias cst='config status'
alias ca='config add'
alias ccsm='config commit -s -m'
alias ccb='config checkout -b'
alias cdf='config diff'
alias clg='config log --stat'
alias clgp='config log --stat -p'
alias clo='config log --oneline --decorate'
alias clol="config log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset'"
alias cgpush='config push origin "$(git_current_branch)"'
alias crbm='config rebase $(git_main_branch)'
