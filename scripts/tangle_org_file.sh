#!/bin/bash
emacs --batch -l org --eval "(org-babel-tangle-file \"$1\")"
