#!/bin/sh

BASE_DIR=$(pwd)

javac -encoding UTF-8 \
    -classpath $1 \
    ${BASE_DIR}/libcobj/src/jp/osscons/opensourcecobol/libcobj/*.java \
    ${BASE_DIR}/libcobj/src/jp/osscons/opensourcecobol/libcobj/data/*.java \
    ${BASE_DIR}/libcobj/src/jp/osscons/opensourcecobol/libcobj/common/*.java \
    ${BASE_DIR}/libcobj/src/jp/osscons/opensourcecobol/libcobj/exceptions/*.java \
    ${BASE_DIR}/libcobj/src/jp/osscons/opensourcecobol/libcobj/termio/*.java \
    ${BASE_DIR}/libcobj/src/jp/osscons/opensourcecobol/libcobj/call/*.java \
    ${BASE_DIR}/libcobj/src/jp/osscons/opensourcecobol/libcobj/file/*.java \
    ${BASE_DIR}/libcobj/src/*.java \
    -d ${BASE_DIR}/libcobj/bin

echo ${BASE_DIR}
