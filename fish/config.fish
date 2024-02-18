### EXPORT ###
# Supresses fish's intro message
set fish_greeting

# set aliases
alias emergesync='sudo emerge --sync'
alias emergeupdate='sudo emerge -avuDN @world'
#alias emergeclean='sudo emerge --ask --depclean'
function emergeclean
    sudo emerge --ask --depclean
    sudo eclean distfiles
end

if status is-interactive
    # Commands to run in interactive sessions can go here
end
