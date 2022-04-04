SRC_BASE_PATH=src/jp/osscons/opensourcecobol/libcobj/

javac -encoding UTF8 -d build \
    ${SRC_BASE_PATH}/*.java \
    ${SRC_BASE_PATH}/call/*.java \
    ${SRC_BASE_PATH}/common/*.java \
    ${SRC_BASE_PATH}/data/*.java \
    ${SRC_BASE_PATH}/exceptions/*.java \
    ${SRC_BASE_PATH}/file/*.java \
    ${SRC_BASE_PATH}/termio/*.java

cd build && jar cf libcobj.jar jp
