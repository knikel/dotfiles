#!/bin/bash

[ -f ~/.env ] && source ~/.env

fpath=(/usr/local/share/zsh-completions $fpath)
autoload -U compinit && compinit

