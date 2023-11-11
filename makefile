ARCH := $(shell uname -m)

all:
	stow --verbose --target=$$HOME --restow */

delete:
	stow --verbose --target=$$HOME --delete */

ubuntu:
	sudo apt-get -y install software-properties-common
	sudo add-apt-repository -y ppa:neovim-ppa/unstable
	sudo apt-get -y update 
	sudo apt-get -y install man-db stow zsh ripgrep fd-find markdown shellcheck build-essential gcc g++ clang ninja-build autoconf automake libtool tree exa clangd wget rsync valgrind glibc-source gh zoxide fzf libelf-dev libexpat-dev libgmp-dev neovim unzip
	sudo apt remove -y software-properties-common
	sudo apt autoremove -y
	# Do we want emacs? Not anymore on remote machines.
	# sudo apt-get -y install emacs-nox
	sudo ln -sf $(which fdfind) /usr/local/bin/fd
	sudo mkdir -p /usr/include/src && sudo tar -xf /usr/src/glibc/glibc-2.38.tar.xz --strip-components=1 -C /usr/include/src
	wget -qO- "https://github.com/Kitware/CMake/releases/download/v3.26.5/cmake-3.26.5-linux-$(ARCH).tar.gz" | sudo tar --strip-components=1 -xz -C /usr
	mkdir -p ~/gdb
	wget -qO- "https://ftp.gnu.org/gnu/gdb/gdb-13.1.tar.xz" | tar --strip-components=1 -xJ -C ~/gdb
	# cd ~/gdb && ./configure && make && sudo make install
	# these should exist already (else how are you running this?)
	#mkdir -p ~/git
	#git clone https://github.com/cpoile/dotfiles.git ~/git/dotfiles
	curl -sSf https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh | sh -s -- -y
	rm ~/.zshrc
	echo "If you receive any 'existing target' conflicts, consider rm-ing those and running ~/git/dotfiles/make again"
	cd ~/git/dotfiles && make
	git config --global commit.gpgsign false
	git clone --depth=1 https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
	git clone --depth=1 https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
	git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ~/.oh-my-zsh/custom/themes/powerlevel10k
	# For emacs:
	# git clone --depth=1 https://github.com/hlissner/doom-emacs ~/.emacs.d
	# ~/.emacs.d/bin/doom install
	# ~/.emacs.d/bin/doom sync
	# add doom to path
	# ~/.emacs.d/bin/doom doctor
	curl https://sh.rustup.rs -sSf | sh -s -- -y
	rustup component add rust-src
	git clone --depth=1 https://github.com/rust-analyzer/rust-analyzer.git -b release ~/git/rust-analyzer
	cd ~/git/rust-analyzer && cargo xtask install --server
	tic -x -o ~/.terminfo ~/git/dotfiles/xterm-24bit.src
	## need to put in the steps from https://github.com/helix-editor/helix
	$(eval LAZYGIT_VERSION := $(curl -s "https://api.github.com/repos/jesseduffield/lazygit/releases/latest" | grep -Po '"tag_name": "v\K[^"]*'))
	curl -Lo lazygit.tar.gz "https://github.com/jesseduffield/lazygit/releases/latest/download/lazygit_${LAZYGIT_VERSION}_Linux_x86_64.tar.gz"
	tar xf lazygit.tar.gz lazygit
	sudo install lazygit /usr/local/bin
	sudo update-alternatives --install /usr/bin/vi vi /usr/bin/nvim 60
	sudo update-alternatives --config vi
	sudo update-alternatives --install /usr/bin/vim vim /usr/bin/nvim 60
	sudo update-alternatives --config vim
	sudo update-alternatives --install /usr/bin/editor editor /usr/bin/nvim 60
	sudo update-alternatives --config editor
	sudo chsh -s "$(command -v zsh)" "${USER}"
	echo "Now restart the shell!\n"
