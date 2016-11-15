#!/bin/bash -e

if [ "$HOME"/.emacs ]; then
  echo "Backing up previous '.emacs' file to '.emacs-backup'"
  mv "$HOME"/.emacs "$HOME"/.emacs-backup
fi

if [ "$HOME"/.emacs.d ]; then
  echo "Backing up previous '.emacs.d' directory to '.emacs.d.backup'"
  mv "$HOME"/.emacs.d/ "$HOME"/.emacs.d.backup/
fi

echo "Cloning git repository into ~/emacs"
git=$(which git)
$git clone https://github.com/paullucas/emacs.git "$HOME"/emacs

echo "Creating '.emacs.d' directory"
mkdir "$HOME"/.emacs.d
ln -s "$HOME/emacs/init.el" "$HOME/.emacs.d/init.el"

echo "Creating '.emacs' file"
ln -s "$HOME/emacs/init.el" "$HOME/.emacs"

echo "Launching Emacs"
emacs=$(which emacs)
$emacs
