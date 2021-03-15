#!/bin/sh

PATH=~/.roswell/bin:$PATH make org-gen

if [ -z "$(git status doc/Code --porcelain)" ]; then
  exit 0
else
  git diff doc/Code
  exit 1
fi
