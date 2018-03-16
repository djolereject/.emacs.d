#!/bin/sh

find . -maxdepth 1 -not \( -name '.' -or -name 'reset.sh' -or -name '.git*' -or -name 'README*' -or -name 'init.el' -or -name 'settings.org' \) -exec rm -r {} +
echo $PWD " is set to initial state "
