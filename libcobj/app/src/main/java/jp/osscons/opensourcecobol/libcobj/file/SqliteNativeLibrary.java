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

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFileAttributes;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sqlite.SQLiteJDBCLoader;
import org.sqlite.util.OSInfo;

/**
 * INDEXEDファイルのバックエンドであるsqlite-jdbcのネイティブライブラリを、プロセス間で共有する1つのファイルに展開しておくためのユーティリティ。
 *
 * <p>sqlite-jdbcは既定では、最初の接続時にネイティブライブラリを{@code
 * java.io.tmpdir}へ{@code sqlite-<バージョン>-<UUID>-<ライブラリ名>}という名前で展開する。 このとき、展開に先立って同じ一時ディレクトリを走査し、{@code
 * .lck}ファイルを持たない同名パターンのファイルを「すでに終了したプロセスの残骸」とみなして削除する。
 *
 * <p>この一時ディレクトリはマシン上の全プロセスで共有されるため、INDEXEDファイルを扱うCOBOLプログラムを同時に複数起動すると、
 * 2つのプロセスが同じ残骸を削除しようとして競合する。削除できなかった側は、実行結果には何の影響もないにもかかわらず、{@code Failed to delete old native
 * lib}というエラーとスタックトレースを標準エラー出力へ書き出す。
 *
 * <p>そこでSQLiteへ最初に接続する前に、ネイティブライブラリをバージョンごとに1つだけ決まった場所へ展開し、その場所を{@code
 * org.sqlite.lib.path}と{@code org.sqlite.lib.name}でsqlite-jdbcに指示する。 こうするとsqlite-jdbc自身は共有一時ディレクトリへ展開しなくなるので、
 * 新たな残骸が増えない。残骸が無ければ削除するものも無く、競合は起こらない。
 * 展開したファイルはバージョンが同じ限り再利用するので、実行のたびに一時ファイルが増えることもない。
 *
 * <p>ただし、後始末そのものを止められるわけではない点に注意が必要である。sqlite-jdbcは{@code
 * org.sqlite.lib.path}を見るより前に共有一時ディレクトリの走査を行うため、そこに残骸があれば、
 * このクラスを使っていても削除しに行き、競合すればエラーを出力する。 残骸を作るのはsqlite-jdbcを使う他のアプリケーションや、この修正より前のlibcobjである。
 * それらが無くなり、走査の対象が空になった時点で出力も止まる。
 *
 * <p>準備に失敗した場合は何も設定しない。その場合sqlite-jdbcは従来どおり自身でライブラリを展開するため、動作に支障はない。
 */
public final class SqliteNativeLibrary {
    /**
     * ネイティブライブラリを置いたディレクトリをsqlite-jdbcに伝えるためのシステムプロパティ。
     *
     * <p>sqlite-jdbcはライブラリを読み込むとき、まずこのプロパティが指す場所を見に行き、そこにあればそれを使って終わる。
     * 自前で展開する処理はその後ろにあるので、ここで見つけさせれば展開も後始末も行われない。
     */
    private static final String LIB_PATH_PROPERTY = "org.sqlite.lib.path";

    /**
     * ネイティブライブラリのファイル名をsqlite-jdbcに伝えるためのシステムプロパティ。
     *
     * <p>指定しなくても、sqlite-jdbcは{@code System.mapLibraryName}で同じ名前を組み立てるため、
     * 現在の依存バージョンでは省略しても動く。それでも明示するのは、名前の決め方が将来変わったときに、
     * こちらが置いたファイルと探しに行く名前が食い違うのを防ぐためである。
     */
    private static final String LIB_NAME_PROPERTY = "org.sqlite.lib.name";

    /**
     * sqlite-jdbcがネイティブライブラリの置き場所を決めるときに参照するシステムプロパティ。
     *
     * <p>sqlite-jdbcはこの指定があればそちらを、無ければ{@code java.io.tmpdir}を使う。
     * こちらも同じ順序で場所を決める必要がある。{@code /tmp}が{@code
     * noexec}であるなどの理由でこのプロパティが指定されている場合に、 {@code
     * java.io.tmpdir}へ置いてしまうと、そこから読み込めずにsqlite-jdbcがエラーを出力することになる。
     */
    private static final String SQLITE_TMPDIR_PROPERTY = "org.sqlite.tmpdir";

    private static final Logger logger = LoggerFactory.getLogger(SqliteNativeLibrary.class);

    /** 準備処理を直列化するためのロック。クラスの内部ロックを外部に晒さないよう専用のオブジェクトを使う。 */
    private static final Object LOCK = new Object();

    private static boolean prepared = false;

    private SqliteNativeLibrary() {}

    /**
     * sqlite-jdbcのネイティブライブラリを共有の場所へ展開し、その場所をシステムプロパティでsqlite-jdbcに指示する。
     *
     * <p>SQLiteへの最初の接続より前に呼び出す必要がある。2回目以降の呼び出しは何もしない。 利用者が{@code
     * org.sqlite.lib.path}または{@code org.sqlite.lib.name}を明示的に指定している場合は、その指定を尊重して何もしない。
     */
    public static void prepare() {
        // 準備が終わるまでロックを保持する。先に接続へ進んだスレッドが、
        // まだ設定されていないシステムプロパティを読んでしまうことを防ぐ。
        synchronized (LOCK) {
            if (prepared) {
                return;
            }
            // 失敗しても再試行しない。用意できない環境では毎回同じ結果になるだけなので、
            // 接続のたびに繰り返す意味がない。
            prepared = true;

            // 利用者がライブラリの場所か名前を明示している場合は、その指定を尊重して何もしない。
            // 名前も見るのは、手順4で両方を設定するためである。片方だけ指定している利用者の設定を、
            // もう片方を上書きすることで壊してしまわないようにする。
            if (System.getProperty(LIB_PATH_PROPERTY) != null
                    || System.getProperty(LIB_NAME_PROPERTY) != null) {
                return;
            }

            try {
                // ------------------------------------------------------------
                // 手順1: jarの中のどのファイルを取り出すのかを決める
                // ------------------------------------------------------------

                // sqlite-jdbcが自身のネイティブライブラリを探すときと同じ規則で名前と場所を組み立てる。
                // ここがずれると手順3で取り出せず、結局sqlite-jdbc自身の展開に任せることになる。
                //
                // resourceが指すのはjarの中の位置であって、ファイルシステム上の場所ではない。
                // ネイティブライブラリはlibcobj.jarに同梱されている。
                // 例: Linux x86_64 なら "/org/sqlite/native/Linux/x86_64/libsqlitejdbc.so"
                String libraryName = System.mapLibraryName("sqlitejdbc");
                String nativeFolder = OSInfo.getNativeLibFolderPathForCurrentOS();
                String resource = "/org/sqlite/native/" + nativeFolder + "/" + libraryName;

                // ------------------------------------------------------------
                // 手順2: 置き場所となるディレクトリを用意する
                // ------------------------------------------------------------

                // 名前にバージョンとプラットフォームを含める。sqlite-jdbcを更新したときに
                // 古いライブラリを使い続けてしまわないようにするためと、
                // 一時ディレクトリが複数の環境から共有される場合に取り違えないようにするため。
                // 例: "opensourcecobol4j-sqlitejdbc-3.51.0.0-Linux-x86_64"
                Path directory =
                        cacheDirectory(
                                SQLiteJDBCLoader.getVersion()
                                        + "-"
                                        + nativeFolder.replace('/', '-'));
                // 安全に使えるディレクトリを用意できなかった場合(詳細はcacheDirectoryを参照)。
                // 何も設定せずに戻れば、sqlite-jdbcが従来どおり自身で展開する。
                if (directory == null) {
                    return;
                }

                // ------------------------------------------------------------
                // 手順3: まだ無ければ取り出す
                // ------------------------------------------------------------

                Path library = directory.resolve(libraryName);

                // jarの中のライブラリの大きさ。取り出したファイルが壊れていないかの確認に使う。
                long expectedSize = resourceSize(resource);

                // 過去の実行、または今まさに動いている別のプロセスが、すでに置いてくれていることがある。
                // その場合は取り出す必要がない。バージョンごとに1つあれば足りるので使い回す。
                //
                // ただし「ある」だけでは足りず、中身が揃っていることまで確かめる。決まった場所に置いて
                // 使い回す以上、一度でも壊れたファイルが残ると、以降このマシンの全プロセスが
                // 読み込みに失敗し続けることになるためである。書き出しの途中で電源が落ちるなどして、
                // 長さの足りないファイルが残ることがありうる。
                if (!isComplete(library, expectedSize)) {
                    extract(resource, directory, library, libraryName);
                }

                // ------------------------------------------------------------
                // 手順4: 置き場所をsqlite-jdbcに伝える
                // ------------------------------------------------------------

                // 手順3で取り出せなかった場合(jar内の配置が想定と違うなど)は、ここでファイルが無いままになる。
                // 揃っていることを確かめてから設定するのは、読み込めないファイルを指してしまうと、
                // sqlite-jdbcがそれを読もうとして失敗し、エラーを出力することになるため。
                // 何も設定しなければ、sqlite-jdbcは自身で展開する従来の動作に戻るだけで済む。
                //
                // なお、ここで設定するのはこのプロセスのシステムプロパティであって、
                // 後続のプロセスに引き継がれるものではない。後続のプロセスが得をするのは、
                // 手順3で取り出したファイルが残っていて、取り出しを省けるという点である。
                if (isComplete(library, expectedSize)) {
                    System.setProperty(LIB_PATH_PROPERTY, directory.toString());
                    System.setProperty(LIB_NAME_PROPERTY, libraryName);
                } else {
                    logger.debug(
                            "The shared sqlite-jdbc native library is not usable: {}", library);
                }
            } catch (Exception e) {
                // 準備できなければsqlite-jdbc自身の展開に任せるので、実行には支障がない。
                // 既定では出力されないdebugレベルにとどめ、調べたいときだけ表示できるようにする。
                logger.debug("Failed to prepare the shared sqlite-jdbc native library", e);
            }
        }
    }

    /**
     * 展開先のディレクトリを返す。使えるディレクトリを用意できなければ{@code null}を返す。
     *
     * <p>sqlite-jdbcの既定の展開先はUUIDを含む名前で、他人には狙って作れない。 対してこのクラスは決まった名前のファイルを読み込むので、
     * 名前が事前に分かってしまう。 POSIX環境の{@code /tmp}のように一時ディレクトリが利用者間で共有される場合、
     * 別の利用者が先回りしてそこに細工したライブラリを置けてしまっては困る。 そこで所有者だけが書き込めるディレクトリを作り、
     * すでに存在する場合は所有者と許可属性を確認してから使う。 確認できないディレクトリは使わず、{@code null}を返して呼び出し元に諦めさせる。
     *
     * @param tag ディレクトリ名に含める、バージョンとプラットフォームの識別子
     * @return 使えるディレクトリ。用意できなければ{@code null}
     */
    private static Path cacheDirectory(String tag) throws IOException {
        // 置き場所の決め方はsqlite-jdbcに合わせる。sqlite-jdbcはorg.sqlite.tmpdirがあればそちらを、
        // 無ければjava.io.tmpdirを使う。ここで食い違うと、読み込めない場所へ置くことになりかねない。
        String base =
                System.getProperty(SQLITE_TMPDIR_PROPERTY, System.getProperty("java.io.tmpdir"));
        Path directory = Paths.get(base).resolve("opensourcecobol4j-sqlitejdbc-" + tag);

        // 許可属性の考え方はOSによって異なる。POSIXの許可属性を扱えるかどうかで処理を分ける。
        // Windowsの一時ディレクトリは利用者ごとに分かれているため、この確認は行わない。
        boolean posix = directory.getFileSystem().supportedFileAttributeViews().contains("posix");

        // --- すでにある場合: 素性を確かめてから使う ---
        // シンボリックリンクをたどらずに確認する。たどってしまうと、他の利用者がこの名前で
        // 自分のディレクトリへのリンクを置いておくだけで、以降の確認をすべて通してしまう。
        if (Files.exists(directory, LinkOption.NOFOLLOW_LINKS)) {
            return isUsableDirectory(directory, posix) ? directory : null;
        }

        // --- 無い場合: 作る ---
        try {
            if (posix) {
                // 作成と同時に許可属性を与える。作ってから変更するのでは、その隙に
                // 他の利用者がファイルを置ける時間ができてしまう。
                Files.createDirectories(
                        directory,
                        PosixFilePermissions.asFileAttribute(
                                PosixFilePermissions.fromString("rwx------")));
            } else {
                Files.createDirectories(directory);
            }
        } catch (FileAlreadyExistsException e) {
            // 存在の確認と作成の間に、別のプロセスが先に作った。
            // 作ったのが同じ利用者とは限らないので、すでにある場合と同じように素性を確かめる。
            return isUsableDirectory(directory, posix) ? directory : null;
        }
        return directory;
    }

    /**
     * すでにある置き場所を、そのまま使ってよいかどうか確かめる。
     *
     * <p>POSIX環境では、ディレクトリであること、所有者が自分であること、グループとその他に書き込み権限が無いことを求める。
     * どれか1つでも欠けると、中のファイルを別の利用者に差し替えられる余地が残る。
     * シンボリックリンクをたどらないので、リンクが置かれていた場合はディレクトリではないとみなして拒否する。
     *
     * <p>使えないと判断した場合はdebugレベルで記録する。 呼び出し元はこの場合何も設定せず、sqlite-jdbc本来の動作に任せることになるが、
     * その判断が黙って行われると、並列実行時の出力が減らない理由が分からなくなるためである。
     */
    private static boolean isUsableDirectory(Path directory, boolean posix) throws IOException {
        if (!posix) {
            if (Files.isDirectory(directory, LinkOption.NOFOLLOW_LINKS)) {
                return true;
            }
            logger.debug("Not a directory: {}", directory);
            return false;
        }

        PosixFileAttributes attributes =
                Files.readAttributes(
                        directory, PosixFileAttributes.class, LinkOption.NOFOLLOW_LINKS);
        if (!attributes.isDirectory()) {
            logger.debug("Not a directory: {}", directory);
            return false;
        }
        // 所有者の照合は名前で行う。利用者名を持たない環境(コンテナで数値のuidだけを指定した場合など)では
        // 一致せず、共有を諦めることになる。動作に支障はないが、その場合はこのログで分かるようにしておく。
        if (!attributes.owner().getName().equals(System.getProperty("user.name"))) {
            logger.debug("Not owned by the current user: {}", directory);
            return false;
        }
        Set<PosixFilePermission> permissions = attributes.permissions();
        if (permissions.contains(PosixFilePermission.GROUP_WRITE)
                || permissions.contains(PosixFilePermission.OTHERS_WRITE)) {
            logger.debug("Writable by others: {}", directory);
            return false;
        }
        return true;
    }

    /**
     * 取り出し済みのファイルが、jarの中のライブラリと同じ大きさで揃っているかどうかを確かめる。
     *
     * <p>jarの中の大きさが分からなかった場合({@code expectedSize}が負)は、存在することだけを確かめる。
     *
     * @param library 確かめる対象のファイル
     * @param expectedSize jarの中のライブラリの大きさ。分からない場合は負の値
     */
    private static boolean isComplete(Path library, long expectedSize) {
        try {
            if (!Files.isRegularFile(library)) {
                return false;
            }
            return expectedSize < 0 || Files.size(library) == expectedSize;
        } catch (IOException e) {
            return false;
        }
    }

    /**
     * jarの中のネイティブライブラリの大きさを返す。分からない場合は負の値を返す。
     *
     * <p>中身を読まずにjarの持つ情報から大きさだけを得る。
     */
    private static long resourceSize(String resource) {
        URL url = SqliteNativeLibrary.class.getResource(resource);
        if (url == null) {
            return -1;
        }
        try {
            return url.openConnection().getContentLengthLong();
        } catch (IOException e) {
            return -1;
        }
    }

    /**
     * jarの中のネイティブライブラリを展開する。
     *
     * <p>いったん一時的な名前で書き出してから最終的な名前へ移動する。 最終的な名前のファイルは移動によって現れるので、
     * 書き出し途中の中途半端な内容が読み込まれることはない。 複数のプロセスが同時にここへ来ても、それぞれ別の一時的な名前へ書き出すので互いを壊さず、
     * 移動が成功した1つだけが残る。
     *
     * <p>「展開」といっても、どこかに置かれているものを探してくるわけではない。 ネイティブライブラリはjarの中に同梱されており、それをファイルとして
     * 取り出すことを指す。ネイティブライブラリはJavaのクラスと違ってjarの中のままでは読み込めないため、 誰かが必ず一度ファイルへ取り出す必要がある。
     *
     * @param resource jarの中のネイティブライブラリの位置
     * @param directory 取り出し先のディレクトリ
     * @param library 最終的なファイルの位置
     * @param libraryName ネイティブライブラリのファイル名。一時的な名前の元にする
     */
    private static void extract(String resource, Path directory, Path library, String libraryName)
            throws IOException {
        // ここで開くのは「取り出し元」であって、展開先ではない。
        // libcobj.jarにはsqlite-jdbcの内容が同梱されており、その中の
        // org/sqlite/native/<プラットフォーム>/<ライブラリ名> を読み出している。
        // sqlite-jdbc自身も、同じjarの同じ位置から読み出している。
        //
        // 既定の「展開先」はこれとは別で、java.io.tmpdirである。
        // そこへ sqlite-<バージョン>-<UUID>-<ライブラリ名> という名前で書き出すのが本来の動作で、
        // 全プロセスがそこを共有するために後始末が競合していた。
        // このクラスは、同じものを読み出した上で、書き出す先だけを共有の一時ディレクトリから
        // 専用のディレクトリへ移している。
        try (InputStream input = SqliteNativeLibrary.class.getResourceAsStream(resource)) {
            if (input == null) {
                // jar内の配置が想定と違う。sqlite-jdbcの更新で配置が変わるとここに来る。
                // 何も置かずに戻れば、呼び出し元はプロパティを設定せず、
                // sqlite-jdbc自身の展開に任せることになる(つまり修正前の動作に戻るだけ)。
                return;
            }
            // 他のプロセスと名前がぶつからないよう、一時的な名前を作ってもらう
            Path work = Files.createTempFile(directory, libraryName, ".tmp");
            try {
                Files.copy(input, work, StandardCopyOption.REPLACE_EXISTING);
                // ATOMIC_MOVEを指定して、他のプロセスから見て「無い」か「完全にある」かのどちらかにする
                Files.move(work, library, StandardCopyOption.ATOMIC_MOVE);
            } catch (IOException | UnsupportedOperationException e) {
                // POSIXでは、移動先にすでにファイルがあっても置き換えられるので、
                // 別のプロセスと同時に取り出しただけならここには来ない。
                // ここに来るのは、書き込む余地が無い、権限が足りない、
                // Windowsで移動先のファイルが読み込み中で置き換えられない、といった場合である。
                //
                // いずれの場合も、何も設定せずsqlite-jdbc本来の動作に任せれば実行はできる。
                // ただ原因が分からないままになるのは避けたいので、書きかけを片付けた上で記録しておく。
                logger.debug("Failed to place the shared sqlite-jdbc native library", e);
                Files.deleteIfExists(work);
            }
        }
    }
}
