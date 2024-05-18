### EXPORT ###
# Supresses fish's intro message
set fish_greeting

# set aliases
# gentoo
alias emergesync='sudo emerge --sync'
function emergeinstall
    sudo snapper -c root create --description "install package $argv"
    sudo emerge -ag $argv
end
function emergeupdate
    sudo snapper -c root create --description "system update"
    sudo emerge -avugDN @world
end
function emergeclean
    sudo emerge --ask --depclean
    sudo eclean distfiles
end

#arch
#function pacupdate
#    sudo snapper -c root create --description "system update"
#    sudo pacman -Syu
#end
#function pacinstall
#    sudo snapper -c root create --description "install package $argv"
#    sudo pacman -Syu $argv
#end
#function pacremove
#    sudo snapper -c root create --description "removed package $argv"
#    # R : remove package
#    # s : remove dependencies (recursively) that are not needed by other packages 
#    # n : remove configs
#    sudo pacman -Rsn
#end
alias pacupdate='sudo pacman -Syu'
alias pacinstall='sudo pacman -Syu $argv'
alias pacremove='sudo pacman -Rsn $argv'
function pacclean
    # see https://wiki.archlinux.org/title/system_maintenance
    # check if services are not starting
    systemctl --failed

    echo "Search and remove orphaned packages"
    sudo pacman -Qdtq | sudo pacman -Rns -

    echo "cleanup the package cache (keep the last version)"
    sudo paccache -rk1

    echo "remove all uninstaled packages from the package cache"
    sudo paccache -ruk0
end

#tumbleweed
alias zypperrefresh='sudo zypper refresh'
function zypperdup
    sudo zypper refresh
    sudo zypper dup --no-recommends
end
alias zypperclean='sudo zypper clean'

#set new global path for npm packages
set NPM_PACKAGES "$HOME/npm-packages"
set PATH $PATH $NPM_PACKAGES/bin
set MANPATH $NPM_PACKAGES/share/man $MANPATH  

if status is-interactive
    # Commands to run in interactive sessions can go here
end
