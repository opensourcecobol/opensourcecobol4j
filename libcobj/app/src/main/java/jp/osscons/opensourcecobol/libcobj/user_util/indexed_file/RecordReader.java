package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

import java.util.Optional;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * 入力ソース(ファイル/標準入力)から1レコードずつ読み込むためのインタフェース。<br>
 * {@code cobj-idx load}コマンドで、入力データをインデックスファイルへ書き込むために使用される。
 * 実装は入力データの形式(SEQUENTIAL / LINE SEQUENTIAL)および入力ソースによって切り替わる。
 */
interface RecordReader {
    /** 入力ソースをオープンし、読み込み可能な状態にする。 */
    void open();

    /**
     * 入力ソースから1レコードを読み込み、{@code record}に格納する。
     *
     * @param record 読み込んだレコードの格納先
     * @return 読み込み結果を表す{@link LoadResult}
     */
    LoadResult read(CobolDataStorage record);

    /** 入力ソースをクローズし、保持しているリソースを解放する。 */
    void close();

    /**
     * 指定された入力データ形式および入力ソースに対応する{@code RecordReader}の実装を返す。<br>
     * {@code filePath}が指定されている場合はファイルから、指定されていない場合は標準入力から読み込む実装を返す。
     *
     * @param userDataFormat 入力データの形式
     * @param recordSize 1レコードのバイト数
     * @param filePath 読み込み対象のファイルのパス。標準入力から読み込む場合は空の{@link Optional}
     * @return 入力データ形式と入力ソースに対応する{@code RecordReader}の実装
     */
    static RecordReader getInstance(
            UserDataFormat userDataFormat, int recordSize, Optional<String> filePath) {
        if (filePath.isPresent()) {
            switch (userDataFormat) {
                case LINE_SEQUENTIAL:
                    return new FileLineSeqRecordReader(recordSize, filePath.get());
                case SEQUENTIAL:
                    return new FileSeqRecordReader(recordSize, filePath.get());
                default:
                    return null;
            }
        } else {
            return StdinRecordReader.getInstance(userDataFormat, recordSize);
        }
    }
}
