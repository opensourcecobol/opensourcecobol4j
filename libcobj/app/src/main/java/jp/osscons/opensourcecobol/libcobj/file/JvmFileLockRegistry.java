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
 * License along with this library; see the file COPYING.LIB; if
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */
package jp.osscons.opensourcecobol.libcobj.file;

import java.io.Closeable;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.NonWritableChannelException;
import java.nio.channels.OverlappingFileLockException;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * 順編成・行順編成・相対編成ファイルの、実行単位(スレッド)間およびプロセス間のロックを管理する台帳。
 *
 * <p>OSのファイルロック({@link FileChannel#tryLock})はJVM単位で管理されており、 同一JVM内で既にロックされている範囲を別のチャネルでロックしようとすると
 * (共有ロック同士でも){@link OverlappingFileLockException}がスローされる。 そこでOSのロックはファイルごとにこの台帳が1つだけ保持し、
 * 同一JVM内のスレッド間の競合は台帳の保持者一覧で、別プロセスとの競合はOSのロックで、 それぞれ別プロセスからのOPENと同じ規則(共有同士は共存、排他が絡めばFILE STATUS
 * 61)で判定する。
 *
 * <p>OSのロックは、この台帳が専用に開いたチャネル上の、ファイル終端よりはるか先の小さな範囲
 * ({@link #MAIN_POSITION})に取得する。範囲が実データと重ならないため、ロックが必須(mandatory)である
 * Windowsでも保持者自身の読み書きを一切妨げず、また保持者がファイルをクローズしてもロックは
 * 台帳のチャネル上で生き続けるので、ロックの引き継ぎ(解放と取り直しの隙間)が存在しない。
 * C版opensource COBOLのfcntlロック(offset 0から無限大)や旧実装の全域ロック([0, Long.MAX_VALUE))は
 * この範囲とも重なるため、それらとの相互検知は保たれる。
 *
 * <p>多くのOS(Linuxのfcntlロックなど)では、あるプロセスがファイルに対して持つロックは、そのプロセスが
 * <b>同じファイルを指すどのディスクリプタを閉じても</b>すべて解放される({@link java.nio.channels.FileLock}の
 * Javadocにも明記されている)。このため、ロックを保持している間は、このJVMの誰もそのファイルのチャネルを
 * 閉じてはならない。保持者のCLOSEや、FILE STATUS 61で拒否されたOPENが手放すチャネルは、
 * {@link #release(Lease, Closeable)}と{@link #closeFor(String, Closeable)}を通じて台帳が預かり、
 * 最後の保持者が解放してOSのロックを手放すときにまとめて閉じる。 保持者が絶えず入れ替わって最後の保持者が
 * 現れない場合でも預かるチャネルが増え続けないよう、このJVMでのファイルのオープンは
 * {@link #openChannel}と{@link #openRandomAccessFile}を通して行い、預かっているチャネルのうち
 * 同じモードで開いたものがあれば新たに開かずに再利用する。これにより、預かるチャネルの数は
 * そのファイルを同時にオープンしていた実行単位の数を超えない。
 *
 * <p>ロックのモード変更(同一実行単位がINPUTとOUTPUTを重ねてオープンした場合の昇格・降格)は、
 * 隣接するもう1つの範囲({@link #BRIDGE_POSITION})を橋渡しに使う: 目的のモードでブリッジ範囲を
 * ロックしてから主範囲を取り直す。この台帳の全操作はブリッジ範囲を先にロックする規約なので、
 * 主範囲が一瞬解放されている間も、他のJVMや全域ロックを使うプロセスは必ずブリッジ範囲に衝突し、
 * 割り込むことができない。
 */
final class JvmFileLockRegistry {

    private JvmFileLockRegistry() {}

    /** OSロックを取得する主範囲の開始位置。実データと重ならないよう、ファイル終端よりはるか先に置く。 */
    static final long MAIN_POSITION = Long.MAX_VALUE - 2;

    /** ロックのモード変更時に橋渡しとして使う範囲の開始位置。 */
    private static final long BRIDGE_POSITION = Long.MAX_VALUE - 1;

    /** ロックする範囲の長さ。 */
    private static final long LOCK_SIZE = 1;

    /** ロックの保持者に渡す票。クローズ時に{@link #release(Lease, Closeable)}へ渡す。 */
    static final class Lease {
        private final String key;

        /** この票を取得した実行単位(スレッド) */
        private final Thread owner = Thread.currentThread();

        /** 排他ロックとして要求されたかどうか */
        private final boolean exclusiveRequested;

        private Lease(String key, boolean exclusiveRequested) {
            this.key = key;
            this.exclusiveRequested = exclusiveRequested;
        }
    }

    /** ファイルごとのロックの保持状況 */
    private static final class Entry {
        /** OSのロックを保持するための、台帳が所有する専用チャネル */
        final FileChannel channel;

        /** 主範囲に取得したOSのロック */
        FileLock osLock;

        /** 現在のOSロックが排他かどうか */
        boolean exclusive;

        /** このJVM内での保持者 */
        final List<Lease> holders = new ArrayList<>();

        /** OSのロックを失わないよう、ロックの解放まで閉じずに預かっているチャネル */
        final List<Closeable> parked = new ArrayList<>();

        Entry(FileChannel channel, FileLock osLock, boolean exclusive) {
            this.channel = channel;
            this.osLock = osLock;
            this.exclusive = exclusive;
        }
    }

    /** 正規化したパスをキーとするロックの台帳。このクラスのモニタで保護する。 */
    private static final Map<String, Entry> entries = new HashMap<>();

    /** 再利用できる形で開かれたファイル。預かったチャネルを次のオープンで使い回すために記録する。 */
    private static final class Reusable {
        /** 開いたときのモード。同じモードのオープンにだけ再利用する */
        final String mode;

        /** 呼び出し元に返すオブジェクト(FileChannelまたはRandomAccessFile) */
        final Object handle;

        Reusable(String mode, Object handle) {
            this.mode = mode;
            this.handle = handle;
        }
    }

    /**
     * {@link #openChannel}と{@link #openRandomAccessFile}で開いたチャネルと、その開き方の対応。
     * チャネルを閉じるときに取り除く。このクラスのモニタで保護する。
     */
    private static final Map<Closeable, Reusable> reusables = new IdentityHashMap<>();

    private static String keyOf(String filename) {
        Path path = Paths.get(filename).toAbsolutePath();
        try {
            return path.toRealPath().toString();
        } catch (IOException e) {
            return path.normalize().toString();
        }
    }

    /**
     * ファイルのロックを取得する。
     *
     * @param filename ロックするファイルのパス
     * @param shared 共有ロックならtrue、排他ロックならfalse
     * @return 取得したロックの票。同一JVM内の他の実行単位またはほかのプロセスと競合して取得できない場合はnull
     * @throws NonWritableChannelException 書き込みが許可されていないファイルに排他ロックを取得しようとした場合
     * @throws IOException チャネルのオープンまたはロックに失敗した場合
     */
    static synchronized Lease acquire(String filename, boolean shared) throws IOException {
        String key = keyOf(filename);
        Entry entry = entries.get(key);
        if (entry != null) {
            boolean sameRunUnit = true;
            for (Lease holder : entry.holders) {
                if (holder.owner != Thread.currentThread()) {
                    sameRunUnit = false;
                    break;
                }
            }
            if (!sameRunUnit && (entry.exclusive || !shared)) {
                return null;
            }
            // 同じ実行単位からの再オープンでは競合させない(1つのプロセス内のロックが
            // 互いに競合しないのと同じ規則)。排他での再オープンならOSロックを昇格する
            if (!shared && !entry.exclusive) {
                if (!changeMode(entry, false)) {
                    return null;
                }
                entry.exclusive = true;
            }
            Lease lease = new Lease(key, !shared);
            entry.holders.add(lease);
            return lease;
        }

        FileChannel channel = openLockChannel(filename);
        FileLock osLock;
        try {
            osLock = lockMainFenced(channel, shared);
        } catch (RuntimeException e) {
            closeQuietly(channel);
            throw e;
        }
        if (osLock == null) {
            closeQuietly(channel);
            return null;
        }
        entry = new Entry(channel, osLock, !shared);
        Lease lease = new Lease(key, !shared);
        entry.holders.add(lease);
        entries.put(key, entry);
        return lease;
    }

    /**
     * ロックを解放し、保持者が使っていたチャネルを閉じる。<br>
     * このJVM内の最後の保持者が解放したときは、OSのロックを解放してから、預かっていたチャネルと あわせて閉じる。他の保持者が残っている場合は、チャネルを閉じるとOSのロックまで失われるため、
     * 閉じずに預かる。 排他を要求していた保持者が解放して共有の保持者だけが残った場合は、OSのロックを共有へ降格する。
     *
     * @param lease {@link #acquire}で取得した票。nullの場合はresourceを閉じるだけ
     * @param resource 保持者がファイルのI/Oに使っていたチャネル。nullでもよい
     */
    static synchronized void release(Lease lease, Closeable resource) {
        Entry entry = lease == null ? null : entries.get(lease.key);
        if (entry == null) {
            closeQuietly(resource);
            return;
        }
        entry.holders.remove(lease);
        if (entry.holders.isEmpty()) {
            if (entry.osLock != null) {
                try {
                    entry.osLock.release();
                } catch (IOException e) {
                    System.err.println("Failed to release the lock of " + lease.key);
                }
            }
            for (Closeable parked : entry.parked) {
                closeQuietly(parked);
            }
            closeQuietly(resource);
            closeQuietly(entry.channel);
            entries.remove(lease.key);
            return;
        }
        park(entry, resource);
        boolean exclusive = false;
        for (Lease other : entry.holders) {
            exclusive |= other.exclusiveRequested;
        }
        if (entry.exclusive && !exclusive) {
            // 降格に失敗した場合は排他のまま維持する(過剰な保護に倒す)。失敗しうるのは、
            // 他のJVMがまさにブリッジ範囲をロックしている一瞬に重なった場合だけである
            if (changeMode(entry, true)) {
                entry.exclusive = false;
            }
        } else {
            entry.exclusive = exclusive;
        }
    }

    /**
     * ファイルをFileChannelとして開く。そのファイルについて預かっているチャネルのうち、同じオプションで
     * 開いたものがあれば、新たに開かずにそれを先頭の位置に戻して返す。
     *
     * @param filename 開くファイルのパス
     * @param options {@link FileChannel#open(Path, OpenOption...)}に渡すオプション
     * @return 開いたチャネル
     * @throws IOException ファイルを開けなかった場合
     */
    static synchronized FileChannel openChannel(String filename, OpenOption... options)
            throws IOException {
        String mode = "channel:" + optionsKey(options);
        Object reused = takeParked(filename, mode);
        if (reused != null) {
            FileChannel channel = (FileChannel) reused;
            channel.position(0L);
            return channel;
        }
        FileChannel channel = FileChannel.open(Paths.get(filename), options);
        reusables.put(channel, new Reusable(mode, channel));
        return channel;
    }

    /**
     * ファイルをRandomAccessFileとして開く。そのファイルについて預かっているもののうち、同じモードで
     * 開いたものがあれば、新たに開かずにそれを先頭の位置に戻して返す。預けるときは{@link
     * RandomAccessFile#getChannel()}のチャネルを渡すこと。
     *
     * @param filename 開くファイルのパス
     * @param mode {@link RandomAccessFile}のモード("r"や"rw")
     * @return 開いたファイル
     * @throws IOException ファイルを開けなかった場合
     */
    static synchronized RandomAccessFile openRandomAccessFile(String filename, String mode)
            throws IOException {
        String key = "raf:" + mode;
        Object reused = takeParked(filename, key);
        if (reused != null) {
            RandomAccessFile raf = (RandomAccessFile) reused;
            raf.seek(0L);
            return raf;
        }
        RandomAccessFile raf = new RandomAccessFile(filename, mode);
        reusables.put(raf.getChannel(), new Reusable(key, raf));
        return raf;
    }

    /** 預かっているチャネルから、指定したモードで開いたものを取り出す。なければnull。 */
    private static Object takeParked(String filename, String mode) {
        Entry entry = entries.get(keyOf(filename));
        if (entry == null) {
            return null;
        }
        Iterator<Closeable> it = entry.parked.iterator();
        while (it.hasNext()) {
            Closeable parked = it.next();
            Reusable reusable = reusables.get(parked);
            if (reusable != null
                    && reusable.mode.equals(mode)
                    && parked instanceof FileChannel
                    && ((FileChannel) parked).isOpen()) {
                it.remove();
                return reusable.handle;
            }
        }
        return null;
    }

    private static String optionsKey(OpenOption... options) {
        List<String> names = new ArrayList<>();
        for (OpenOption option : options) {
            names.add(option.toString());
        }
        Collections.sort(names);
        return String.join(",", names);
    }

    /**
     * ロックを取得していないチャネル(FILE STATUS 61で拒否されたOPENのチャネルなど)を閉じる。<br>
     * このJVMがそのファイルのロックを保持している間は、閉じるとOSのロックが失われるため、 ロックの解放まで閉じずに預かる。
     *
     * @param filename チャネルが指すファイルのパス
     * @param resource 閉じるチャネル。nullの場合は何もしない
     */
    static synchronized void closeFor(String filename, Closeable resource) {
        if (resource == null) {
            return;
        }
        Entry entry = entries.get(keyOf(filename));
        if (entry == null) {
            closeQuietly(resource);
        } else {
            park(entry, resource);
        }
    }

    private static void park(Entry entry, Closeable resource) {
        if (resource != null) {
            entry.parked.add(resource);
        }
    }

    /**
     * 指定したファイルについて預かっているチャネルの数を返す(テスト用)。
     *
     * @param filename ファイルのパス
     * @return 預かっているチャネルの数。ロックされていない場合は0
     */
    static synchronized int parkedCount(String filename) {
        Entry entry = entries.get(keyOf(filename));
        return entry == null ? 0 : entry.parked.size();
    }

    /**
     * エントリのOSロックのモードを変更する(昇格・降格)。<br>
     * ブリッジ範囲を目的のモードでロックしてから主範囲を取り直すため、変更の間も 他のプロセスが割り込む隙間はない。
     *
     * @param entry 対象のエントリ
     * @param shared 変更後のモード。共有ならtrue
     * @return 変更できた場合はtrue。他のプロセスと競合して変更できない場合はfalse(元のロックは維持される)
     */
    private static boolean changeMode(Entry entry, boolean shared) {
        FileLock bridge = tryLockRange(entry.channel, BRIDGE_POSITION, shared);
        if (bridge == null) {
            return false;
        }
        try {
            if (entry.osLock != null) {
                try {
                    entry.osLock.release();
                } catch (IOException e) {
                    return false;
                }
                entry.osLock = null;
            }
            // ブリッジ範囲を保持している間は、この台帳の規約に従う他のJVMも、
            // 全域をロックするプロセスも主範囲を取得できないため、ここで失敗することはない
            FileLock newLock = tryLockRange(entry.channel, MAIN_POSITION, shared);
            if (newLock == null) {
                // 防御的な回復: 元のモードで取り直す
                entry.osLock = tryLockRange(entry.channel, MAIN_POSITION, !shared);
                if (entry.osLock == null) {
                    System.err.println("Lost the OS lock while changing its mode");
                }
                return false;
            }
            entry.osLock = newLock;
            return true;
        } finally {
            try {
                bridge.release();
            } catch (IOException e) {
                System.err.println("Failed to release the bridge lock");
            }
        }
    }

    /**
     * ブリッジ範囲を先にロックしてから主範囲をロックする。
     *
     * @param channel 台帳が所有するチャネル
     * @param shared 共有ロックならtrue
     * @return 主範囲のロック。競合して取得できない場合はnull
     */
    private static FileLock lockMainFenced(FileChannel channel, boolean shared) {
        FileLock bridge = tryLockRange(channel, BRIDGE_POSITION, shared);
        if (bridge == null) {
            return null;
        }
        try {
            return tryLockRange(channel, MAIN_POSITION, shared);
        } finally {
            try {
                bridge.release();
            } catch (IOException e) {
                System.err.println("Failed to release the bridge lock");
            }
        }
    }

    /**
     * 指定した範囲のロックを試みる。
     *
     * @param channel 対象のチャネル
     * @param position ロックする範囲の開始位置
     * @param shared 共有ロックならtrue
     * @return 取得したロック。競合して取得できない場合はnull
     * @throws NonWritableChannelException 読み取り専用のチャネルに排他ロックを取得しようとした場合
     */
    private static FileLock tryLockRange(FileChannel channel, long position, boolean shared) {
        try {
            FileLock lock = channel.tryLock(position, LOCK_SIZE, shared);
            if (lock != null && lock.isValid()) {
                return lock;
            }
            return null;
        } catch (OverlappingFileLockException | IOException e) {
            // OverlappingFileLockExceptionは、このJVM内で台帳を介さずに取得された
            // ロックと重なった場合にスローされる。どちらも競合として扱う
            return null;
        }
    }

    /**
     * ロック保持用のチャネルを開く。共有ロックには読み取り、排他ロックには書き込みの許可が要るため、
     * 読み書き両用で開き、許可がなければ読み取り専用、書き込み専用の順に代替する。
     *
     * @param filename 対象ファイルのパス
     * @return 開いたチャネル
     * @throws IOException どのモードでも開けなかった場合
     */
    private static FileChannel openLockChannel(String filename) throws IOException {
        Path path = Paths.get(filename);
        StandardOpenOption[][] optionsToTry = {
            {StandardOpenOption.READ, StandardOpenOption.WRITE},
            {StandardOpenOption.READ},
            {StandardOpenOption.WRITE},
        };
        IOException failure = null;
        for (StandardOpenOption[] options : optionsToTry) {
            try {
                return FileChannel.open(path, options);
            } catch (IOException e) {
                failure = e;
            }
        }
        throw failure;
    }

    private static void closeQuietly(Closeable resource) {
        if (resource == null) {
            return;
        }
        reusables.remove(resource);
        try {
            resource.close();
        } catch (IOException e) {
            System.err.println("Failed to close a file channel: " + e.getMessage());
        }
    }

    /**
     * 指定したファイルがこのJVM内でロックされているかどうかを返す(テスト用)。
     *
     * @param filename ファイルのパス
     * @return ロックされていればtrue
     */
    static synchronized boolean isLocked(String filename) {
        return entries.containsKey(keyOf(filename));
    }
}
