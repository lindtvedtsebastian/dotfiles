#!/bin/bash

if [[ "$OSTYPE" == "darwin"* ]] && [[ $(uname -m) == "arm64" ]];
then
    eval "$(/opt/homebrew/bin/brew shellenv)"
    export PATH="/opt/homebrew/opt/postgresql@17/bin:$PATH"
fi
