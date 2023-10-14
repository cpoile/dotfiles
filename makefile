all:
	stow --verbose --target=$$HOME --restow */

delete:
	stow --verbose --target=$$HOME --delete */

ubuntu:
	sudo apt-get update
	sudo apt-get -y install man-db emacs-nox stow zsh ripgrep fd-find
