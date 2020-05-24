# .dotfiles

This dotfiles repository uses a [bare git repository](https://www.atlassian.com/git/tutorials/dotfiles) technique.

## Cloning dotfiles

```shell
$ git clone --bare git@github.com:moxheit/.dotfiles /
  alias config="/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME" /
  config config --local status.showUntrackedFiles no /
  config checkout
```