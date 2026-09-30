/*
 * Copyright (C) 2021-2022 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3.0,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */
package jp.osscons.opensourcecobol.libcobj.file;

import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;

/**
 * JvmFileLockRegistryTestが別のJVMとして起動する検査用プログラム。<br>
 * 引数: FILE POSITION。指定した位置の1バイトに排他ロックを試み、拒否されれば終了コード0、 取得できれば終了コード3で終了する。
 */
public final class LockProbe {
    private LockProbe() {}

    public static void main(String[] args) throws Exception {
        try (FileChannel ch =
                FileChannel.open(
                        Paths.get(args[0]), StandardOpenOption.READ, StandardOpenOption.WRITE)) {
            FileLock lock = ch.tryLock(Long.parseLong(args[1]), 1, false);
            if (lock == null) {
                System.exit(0);
            }
            lock.release();
            System.exit(3);
        }
    }
}
