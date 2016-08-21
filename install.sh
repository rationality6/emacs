#!/bin/bash -e

if [ "$HOME"/.emacs ]; then
  echo "Backing up previous '.emacs' file to '.emacs-backup'"
  mv "$HOME"/.emacs "$HOME"/.emacs-backup
fi

if [ "$HOME"/.emacs.d ]; then
  echo "Backing up previous '.emacs.d' directory to '.emacs.d.backup'"
  mv "$HOME"/.emacs.d/ "$HOME"/.emacs.d.backup/
fi

echo "Cloning git repository"
/usr/bin/git clone https://github.com/paullucas/emacs.git "$HOME"/.emacs.d

echo "Creating '.emacs' file"
ln -s "$HOME/.emacs.d/init.el" "$HOME/.emacs"

echo "Launching Emacs"
EMACS=which emacs
$EMACS
