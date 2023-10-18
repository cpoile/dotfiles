all:
	stow --verbose --target=$$HOME --restow */

delete:
	stow --verbose --target=$$HOME --delete */

ubuntu:
	sudo apt-get update
	sudo apt-get -y install man-db emacs-nox stow zsh ripgrep fd-find markdown shellcheck build-essential gcc g++ clang ninja-build autoconf automake libtool tree exa clangd wget rsync valgrind glibc-source gh
	sudo ln -s $(which fdfind) /usr/local/bin/fd
	sudo mkdir /usr/include/src && sudo tar -xf /usr/src/glibc/glibc-2.38.tar.xz --strip-components=1 -C /usr/include/src
	wget -qO- "https://github.com/Kitware/CMake/releases/download/v3.26.5/cmake-3.26.5-linux-aarch64.tar.gz" | sudo tar --strip-components=1 -xz -C /usr
	mdir ~/gdb
	wget -qO- "https://ftp.gnu.org/gnu/gdb/gdb-13.1.tar.xz" | tar --strip-components=1 -xJ -C ~/gdb
	cd gdb
	./configure
	make
	sudo make install
	cd ~
	mkdir ~/git
	git clone https://github.com/cpoile/dotfiles.git ~/git/dotfiles
	echo "If you receive any 'existing target' conflicts, consider rm-ing those and running ~/git/dotfiles/make again"
	cd ~/git/dotfiles
	make
	git config --global commit.gpgsign false
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
	git clone --depth=1 https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
	git clone --depth=1 https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
	git clone --depth=1 https://github.com/wting/autojump.git ~/git/autojump
	cd ~/git/autojump
	./install.py
	git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
	git clone --depth=1 https://github.com/hlissner/doom-emacs ~/.emacs.d
	~/.emacs.d/bin/doom install
	~/.emacs.d/bin/doom sync
add doom to path
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
	source "$HOME/.cargo/env"
	rustup component add rust-src
	git clone --depth=1 https://github.com/rust-analyzer/rust-analyzer.git -b release ~/git/rust-analyzer
	cd ~/git/rust-analyzer
	cargo xtask install --server
	~/.emacs.d/bin/doom doctor
	tic -x -o ~/.terminfo ~/git/dotfiles/xterm-24bit.src
	export TERM=xterm-24bit
