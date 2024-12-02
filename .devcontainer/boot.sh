#!/bin/bash

# Build and Install
./configure --prefix=/usr/
make
make install

# Set up ~/.bashrc
cat .devcontainer/term_settings/extra_bashrc.sh >> ~/.bashrc
