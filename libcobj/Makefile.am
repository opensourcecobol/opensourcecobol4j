TARGET_JAR = app/build/libs/libcobj.jar
COBJ_IDX = bin/cobj-idx
COBJ_API = bin/cobj-api

SRC_FILES = \
	settings.gradle.kts \
	./app/build.gradle.kts \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/Const.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/call/CobolResolve.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/call/CobolRunnable.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/call/CobolSystemRoutine.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/common/CobolCallParams.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/common/CobolConstant.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/common/CobolExternal.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/common/CobolInspect.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/common/CobolModule.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/common/CobolUtil.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/common/GetInt.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/common/CobolCheck.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/common/CobolControl.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/common/CobolIntrinsic.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/common/CobolString.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/common/GetAbstractCobolField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/AbstractCobolField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolAlphanumericField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolFieldAttribute.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolNationalAllField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolNumericBinaryField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolNumericField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolAlphanumericAllField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolDataStorage.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolFieldFactory.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolNationalEditedField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolNumericDoubleField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolNumericPackedField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolAlphanumericEditedField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolDecimal.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolGroupField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolNationalField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/data/CobolNumericEditedField.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/exceptions/CobolExceptionId.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/exceptions/CobolGoBackException.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/exceptions/CobolStopRunException.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/exceptions/CobolExceptionInfo.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/exceptions/CobolExceptionTabCode.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/exceptions/CobolRuntimeException.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/CobolFile.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/CobolFileSort.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/CobolLineSequentialFile.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/CobolSort.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/IndexedCursor.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/Linage.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/CobolFileFactory.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/CobolIndexedFile.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/CobolRelativeFile.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/FileIO.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/IndexedFile.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/MemoryStruct.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/CobolFileKey.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/CobolItem.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/CobolSequentialFile.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/FileStruct.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/KeyComponent.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/termio/CobolTerminal.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/ui/CobolResultSetException.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/ui/CobolResultSet.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/ui/CobolCallResult.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/ui/CobolResultInt.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/ui/CobolResultString.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/ui/CobolResultDouble.java \ 
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/user_util/indexed_file/IndexedFileUtilMain.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/user_util/indexed_file/ErrorLib.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/user_util/indexed_file/LoadResult.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/user_util/indexed_file/UserDataFormat.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/user_util/indexed_file/RecordReader.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/user_util/indexed_file/RecordWriter.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/user_util/indexed_file/StdinRecordReader.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/user_util/indexed_file/FileSeqRecordReader.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/user_util/indexed_file/FileLineSeqRecordReader.java\
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/user_util/cobj_api/ApiFiles.java \
	./app/src/main/java/jp/osscons/opensourcecobol/libcobj/user_util/cobj_api/ApiFilesOptions.java

TEST_FILE = \
	./app/src/test/java/jp/osscons/opensourcecobol/libcobj/user_util/cobj_api/ApiFilesTest.java

all: $(TARGET_JAR)

$(TARGET_JAR): $(SRC_FILES)
	./gradlew shadowJar

clean:
	rm -rf $(TARGET_JAR)

install: $(TARGET_JAR) $(COBJ_IDX) $(COBJ_API)
	mkdir -p $(DESTDIR)$(prefix)/lib/opensourcecobol4j/
	install $(TARGET_JAR) $(DESTDIR)$(prefix)/lib/opensourcecobol4j/
	install $(COBJ_IDX) $(DESTDIR)$(prefix)/bin/
	install $(COBJ_API) $(DESTDIR)$(prefix)/bin/

test:
	./gradlew test
	rm -f ./app/testController.java ./app/testRecord.java
