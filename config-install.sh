# read the branch (main if non is given)
set -e
BRANCH=${1:-${BRANCH:-main}}

echo "Installing dotfiles from branch: $BRANCH"

# clone repository into a bare git repository
git clone --bare https://github.com/nomispaz/dotfiles.git $HOME/git_repos/dotfiles
# predefine config as alias for git with set git- and work-tree
function config {
   git --git-dir=$HOME/git_repos/dotfiles/ --work-tree=$HOME $@
}

# checkout the dotfiles
config checkout
if [ $? = 0 ]; then
  echo "Checked out config."
else
  echo "Backing up pre-existing dotfiles..."

  mkdir -p $HOME/.config-backup

  # Find all conflicting files and move them safely
  config checkout 2>&1 | grep -E "^\s+\." | awk '{print $1}' | while read -r file; do
    # Ensure the parent directory exists in backup
    mkdir -p "$(dirname ".config-backup/$file")"
    mv "$file" ".config-backup/$file"
  done

  echo "Retrying checkout..."
  config checkout
fi

# don't show untracked files since all files in $HOME would be shown
config config status.showUntrackedFiles no

config switch $BRANCH
