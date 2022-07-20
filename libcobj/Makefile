# Copyright (C) 2021-2022 TOKYO SYSTEM HOUSE Co., Ltd.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 3.0,
# or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; see the file COPYING.LIB.  If
# not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
# Boston, MA 02110-1301 USA

SRC_FILES=\
	./src/jp/osscons/opensourcecobol/libcobj/Const.java \
	./src/jp/osscons/opensourcecobol/libcobj/call/CobolCallDataContent.java \
	./src/jp/osscons/opensourcecobol/libcobj/call/CobolResolve.java \
	./src/jp/osscons/opensourcecobol/libcobj/call/CobolRunnable.java \
	./src/jp/osscons/opensourcecobol/libcobj/call/CobolSystemRoutine.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/CobolCallParams.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/CobolConstant.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/CobolExternal.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/CobolInspect.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/CobolModule.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/CobolUtil.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/GetInt.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/CobolCheck.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/CobolControl.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/CobolFrame.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/CobolIntrinsic.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/CobolString.java \
	./src/jp/osscons/opensourcecobol/libcobj/common/GetAbstractCobolField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/AbstractCobolField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolAlphanumericField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolFieldAttribute.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolNationalAllField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolNumericBinaryField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolNumericField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolAlphanumericAllField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolDataStorage.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolFieldFactory.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolNationalEditedField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolNumericDoubleField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolNumericPackedField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolAlphanumericEditedField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolDecimal.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolGroupField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolNationalField.java \
	./src/jp/osscons/opensourcecobol/libcobj/data/CobolNumericEditedField.java \
	./src/jp/osscons/opensourcecobol/libcobj/exceptions/CobolCallException.java \
	./src/jp/osscons/opensourcecobol/libcobj/exceptions/CobolExceptionId.java \
	./src/jp/osscons/opensourcecobol/libcobj/exceptions/CobolGoBackException.java \
	./src/jp/osscons/opensourcecobol/libcobj/exceptions/CobolStopRunException.java \
	./src/jp/osscons/opensourcecobol/libcobj/exceptions/RuntimeErrorHandler.java \
	./src/jp/osscons/opensourcecobol/libcobj/exceptions/CobolException.java \
	./src/jp/osscons/opensourcecobol/libcobj/exceptions/CobolExceptionTabCode.java \
	./src/jp/osscons/opensourcecobol/libcobj/exceptions/CobolRuntimeException.java \
	./src/jp/osscons/opensourcecobol/libcobj/exceptions/CobolUndefinedException.java \
	./src/jp/osscons/opensourcecobol/libcobj/exceptions/RuntimeExitHandler.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/CobolFile.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/CobolFileSort.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/CobolLineSequentialFile.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/CobolSort.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/IndexedCursor.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/Linage.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/CobolFileFactory.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/CobolIndexedFile.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/CobolRelativeFile.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/FileIO.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/IndexedFile.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/MemoryStruct.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/CobolFileKey.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/CobolItem.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/CobolSequentialFile.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/FileStruct.java \
	./src/jp/osscons/opensourcecobol/libcobj/file/KeyComponent.java \
	./src/jp/osscons/opensourcecobol/libcobj/termio/CobolTerminal.java

all: build/libcobj.jar

build/libcobj.jar: $(SRC_FILES)
	javac -encoding UTF8 -d build $^
	cd build && jar cf libcobj.jar jp

clean:
	rm -rf build/

install:

.PHONY: clean all install