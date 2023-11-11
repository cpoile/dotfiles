## ChrisP's Dotfiles

(Hat tip to https://github.com/venthur/dotfiles)

This repo uses [stow][] to manage the symlinks, but you don't **have** to --
you can also just individually symlink the contents of each directory directly
into your home.

If this is a new Ubuntu box and you want the full ~~emacs~~ neovim experience: install git, clone this repo into `~/git/dotfiles` (or wherever you keep your git repos) and

``` sh
make ubuntu-aarch64
```
or
``` sh
make ubuntu-amd64
```

If you want to use stow, clone this repository into `~/git/dotfiles` and do

```sh
cd ~/git/dotfiles
make
```

NOTE: Make sure to personalize the `.gitconfig`.

stow will automatically symlink the contents of each "package" into the parent
directory from where it is invoked (i.e. your home directory), that's why it
is important to clone this repository directly into your home directory. If
you cloned the repository somewhere else, you can use the `--target`
parameter.

```sh
cd ~/path/to/dotfiles
stow --target=$HOME --restow */
```

[stow]: https://www.gnu.org/software/stow/
