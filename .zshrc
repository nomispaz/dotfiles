# import plugins
source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh-history-substring-search/zsh-history-substring-search.zsh

# --------------------------------------------------
# Options
# --------------------------------------------------

setopt autocd
setopt nobeep                # No beep
setopt appendhistory         # Immediately append history instead of overwriting
setopt histignorealldups     # If a new command is a duplicate, remove the older one
setopt inc_append_history    # save commands are added to the history immediately, otherwise only when shell exits.
setopt histignorespace       # Don't save commands that start with space
setopt correct               # Auto correct mistakes
setopt extendedglob          # Extended globbing. Allows using regular expressions with *
setopt nocaseglob            # Case insensitive globbing
setopt hist_ignore_space


HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

# Speed up completions
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

#unsetopt beep
#setopt appendhistory
#setopt sharehistory
#setopt hist_ignore_dups
#
#autoload functions
autoload -Uz compinit && compinit
autoload -Uz promptinit && promptinit


# hit tab twice to show list of options and select option
zstyle ':completion:*' menu select
#hit tab once to show list of options or if completion is not ambigous, complete
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'       # Case insensitive tab completion 

bindkey '^[OA' history-substring-search-up
bindkey '^[OB' history-substring-search-down
# Navigate words with ctrl+arrow keys
bindkey '^[[1;5D' backward-word                                 # STRG + Arrow left
bindkey '^[[1;5C' forward-word                                  # STRG + ARROW right


setopt PROMPT_SUBST
# Fish-style prompt: time, user@host, path, signal, newline >
PROMPT='%F{white}[%*]%f %F{blue}%n@%m%f %F{green}%d%f
%F{white}>%f '

# Colour man pages
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-R

# File and Dir colours for ls and other outputs
export LS_OPTIONS='--color=auto'
eval "$(dircolors -b)"
alias ls='ls $LS_OPTIONS'

# ----------------------------------------------
# Aliases and functions
# ----------------------------------------------

alias config="git --git-dir=$HOME/git_repos/dotfiles/ --work-tree=$HOME $argv"
