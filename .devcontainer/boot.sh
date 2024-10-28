#!/bin/bash

# Build and Install
./configure --prefix=/usr/
make
make install

# Set up pre-commit hook
cp .devcontainer/term_settings/pre-commit .git/hooks/pre-commit

# Set up ~/.bashrc
cat .devcontainer/term_settings/extra_bashrc.sh >> ~/.bashrc
