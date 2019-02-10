#!/bin/bash

# Idle, if not interactive
case $- in
	*i*) ;;
	*) return;;
esac

# If  set,  bash checks the window size after each command
# and, if necessary, updates the values of LINES and  COL-
# UMNS.
shopt -s checkwinsize