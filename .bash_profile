#!/bin/bash 

# * ~/.path can be used to extend `$PATH`
# * ~/.extra can be used for other settings you don't want to commit

for file in ~/.{bashrc,bash_prompt,aliases,functions,path,extra,exports}; do
    if [[ -r "$file" ]] && [[ -f "$file" ]]; then
        # shellcheck source=/dev/null
        source "$file"
    fi
done
unset file

# Match filenames in a case-insensitive fashion when performing pathname expansion
shopt -s nocaseglob

# The history list is appended rather than overwritten
shopt -s histappend

# Correct minor errors in the spelling of a directory component in a `cd`
shopt -s cdspell

# Enable some Bash 4 features when possible:
# * `autocd`, e.g. `**/qux` will enter `./foo/bar/baz/qux`
# * Recursive globbing, e.g. `echo **/*.txt`
for option in autocd globstar; do
	shopt -s "$option" 2> /dev/null
done

# Add bash-completion
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# NVM/Node
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion