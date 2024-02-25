### EXPORT ###
# Supresses fish's intro message
set fish_greeting

# set aliases
alias emergesync='sudo emerge --sync'
alias emergeupdate='sudo emerge -avugDN @world'
#alias emergeclean='sudo emerge --ask --depclean'
function emergeclean
    sudo emerge --ask --depclean
    sudo eclean distfiles
end

#set new global path for npm packages
set NPM_PACKAGES "$HOME/npm-packages"
set PATH $PATH $NPM_PACKAGES/bin
set MANPATH $NPM_PACKAGES/share/man $MANPATH  

if status is-interactive
    # Commands to run in interactive sessions can go here
end
