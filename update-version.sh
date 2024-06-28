#!/bin/bash

tmpfile=$(mktemp)
old_version=$1
new_version=$2

if [ "$old_version" = "" ] || [ "$new_version" = "" ]; then
    echo "Error: Specify both the old version number and the new version number" > /dev/stderr
    exit 1
fi

function update_versions_in_file()
{
    cat $1 | sed -e "s/$old_version/$new_version/g" > $tmpfile
    cp $tmpfile $1
}

# replace version strings
update_versions_in_file configure.ac
update_versions_in_file README.md
update_versions_in_file README_JP.md
update_versions_in_file libcobj/app/build.gradle.kts
update_versions_in_file libcobj/app/src/main/java/jp/osscons/opensourcecobol/libcobj/Const.java
update_versions_in_file tests/command-line-options.src/info-java-dir.at
update_versions_in_file libcobj/app/src/main/java/jp/osscons/opensourcecobol/libcobj/user_util/cobj_api/ApiFilesOptions.java
update_versions_in_file win/config.h

# Rebuild
autoreconf
./configure --prefix=/usr/
make
