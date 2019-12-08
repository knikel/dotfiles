#!/bin/bash

# Idle, if not interactive
case $- in
	*i*) ;;
	*) return;;
esac

# * ~/.path can be used to extend `$PATH`
# * ~/.extra can be used for other settings you don't want to commit
# * ~/.env_* are used for language/context specific stuff

for file in ~/.{aliases,path,extra,exports,env_*}; do
    if [[ -r "$file" ]] && [[ -f "$file" ]]; then
        source "$file"
    fi
done
unset file


