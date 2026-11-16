package jp.osscons.opensourcecobol.libcobj.sql;

import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.Charset;
import java.sql.ParameterMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.text.SimpleDateFormat;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;

/**
 * COBOL ホスト変数のストレージと Java/JDBC 型との間で相互変換を行う。
 *
 * <p>{@link #setParam}・{@link #getValueFromResultSet}・{@link #stringToCobol} は、
 * {@link CobolEsqlBackendInterface} 実装（別パッケージで提供されるバックエンドを含む）が利用できる
 * 公開 API。それ以外のメンバはパッケージ内部用である。
 */
public final class CobolDataConverter {

    /** ユーティリティクラスのインスタンス化を防ぐための private コンストラクタ。 */
    private CobolDataConverter() {}

    /**
     * COBOL ホスト変数の内部分類。CobolFieldAttribute から resolveHvarType で
     * 解決され、本クラス内のディスパッチにのみ使われる (公開しない)。
     */
    private enum HvarType {
        /** 符号なし数値 DISPLAY (USAGE DISPLAY、符号なし)。 */
        UNSIGNED_NUMERIC,
        /** 末尾に分離符号 (separate sign) 文字を持つ符号付き数値。 */
        SIGNED_TRAILING_SEPARATE,
        /** 末尾に結合符号 (overpunch) を持つ符号付き数値。 */
        SIGNED_TRAILING_COMBINED,
        /** 先頭に分離符号 (separate sign) 文字を持つ符号付き数値。 */
        SIGNED_LEADING_SEPARATE,
        /** 先頭に結合符号 (overpunch) を持つ符号付き数値。 */
        SIGNED_LEADING_COMBINED,
        /** 符号なし packed-decimal (COMP-3、符号ニブルなし)。 */
        UNSIGNED_PACKED,
        /** 符号付き packed-decimal (COMP-3)。 */
        SIGNED_PACKED,
        /** 符号なしネイティブ binary (COMP-5、ビッグエンディアン)。 */
        UNSIGNED_BINARY_NATIVE,
        /** 符号付きネイティブ binary (COMP-5、ビッグエンディアン)。 */
        SIGNED_BINARY_NATIVE,
        /** 英字 (PIC A)。 */
        ALPHABETIC,
        /** 集団項目 (英数字として扱う)。 */
        GROUP,
        /** 倍精度浮動小数点 (COMP-2)。 */
        FLOAT,
        /** 各国文字 (PIC N)。 */
        NATIONAL,
        /** 英数字の可変長 (varying) 文字列。 */
        ALPHANUMERIC_VARYING,
        /** 日本語 (DBCS) の可変長 (varying) 文字列。 */
        JAPANESE_VARYING
    }

    private static final Charset SHIFT_JIS = Charset.forName("SHIFT-JIS");
    private static final int OCDB_VARCHAR_HEADER_BYTE = 4;

    /**
     * Shift-JIS の全角 (表意文字) スペース。National (PIC N) 項目および日本語 VARYING 項目の
     * パディングに使われ、読み取り時にも末尾のこのバイト列を取り除く。
     */
    private static final byte[] SJIS_FULLWIDTH_SPACE = {(byte) 0x81, (byte) 0x40};

    /**
     * AbstractCobolField の属性から内部用の HvarType を解決する。
     *
     * @param field COBOL フィールド
     * @return 内部ディスパッチ用の HvarType
     */
    private static HvarType resolveHvarType(AbstractCobolField field) {
        CobolFieldAttribute attr = field.getAttribute();
        int type = attr.getType();
        boolean hasSign = attr.isFlagHaveSign();
        boolean signSep = attr.isFlagSignSeparate();
        boolean signLead = attr.isFlagSignLeading();

        switch (type) {
            case CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY:
                if (!hasSign) {
                    return HvarType.UNSIGNED_NUMERIC;
                }
                if (signSep && signLead) {
                    return HvarType.SIGNED_LEADING_SEPARATE;
                }
                if (signSep) {
                    return HvarType.SIGNED_TRAILING_SEPARATE;
                }
                if (signLead) {
                    return HvarType.SIGNED_LEADING_COMBINED;
                }
                return HvarType.SIGNED_TRAILING_COMBINED;
            case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
                return hasSign ? HvarType.SIGNED_PACKED : HvarType.UNSIGNED_PACKED;
            case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
                return hasSign ? HvarType.SIGNED_BINARY_NATIVE : HvarType.UNSIGNED_BINARY_NATIVE;
            case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
            case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
                return HvarType.FLOAT;
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
                return HvarType.NATIONAL;
            case CobolFieldAttribute.COB_TYPE_GROUP:
                if (attr.isFlagVarying()) {
                    if ((attr.getFlags() & CobolFieldAttribute.COB_FLAG_NATIONAL_VARYING) != 0) {
                        return HvarType.JAPANESE_VARYING;
                    }
                    return HvarType.ALPHANUMERIC_VARYING;
                }
                return HvarType.GROUP;
            default:
                return HvarType.ALPHABETIC;
        }
    }

    /**
     * COBOL ホスト変数を SQL バインド用の文字列表現に変換する。
     *
     * @param field COBOL ホスト変数フィールド
     * @return 文字列表現。field が null の場合は空文字列
     */
    static String cobolToString(AbstractCobolField field) {
        if (field == null || field.getDataStorage() == null) {
            return "";
        }
        int length = field.getSize();
        // V99 → getScale()=2 → scale=-2; PP → getScale()=-2 → scale=0 (packed 以外の場合)
        int scale = Math.min(0, -field.getAttribute().getScale());
        CobolDataStorage storage = field.getDataStorage();
        HvarType hvarType = resolveHvarType(field);

        // VARYING の場合、length = ARR サイズ (全体 - 4 バイトのヘッダ)
        if (hvarType == HvarType.ALPHANUMERIC_VARYING || hvarType == HvarType.JAPANESE_VARYING) {
            length = field.getSize() - OCDB_VARCHAR_HEADER_BYTE;
        }

        // packed decimal の場合、バイトサイズではなく桁数を使う。
        // PP の桁 (負の scale) は物理的に格納されないため除外する。
        int rawScale = field.getAttribute().getScale();
        int packedLength = field.getAttribute().getDigits();
        int ppCount = 0;
        if (rawScale < 0) {
            ppCount = -rawScale;
            packedLength += rawScale; // PP の桁を減算する (rawScale は負の値)
        }

        switch (hvarType) {
            case UNSIGNED_NUMERIC:
                return readNumericDisplay(length, scale, storage, DisplaySign.UNSIGNED);
            case SIGNED_TRAILING_SEPARATE:
                return readNumericDisplay(length, scale, storage, DisplaySign.TRAILING_SEPARATE);
            case SIGNED_TRAILING_COMBINED:
                return readNumericDisplay(length, scale, storage, DisplaySign.TRAILING_COMBINED);
            case SIGNED_LEADING_COMBINED:
                return readNumericDisplay(length, scale, storage, DisplaySign.LEADING_COMBINED);
            case SIGNED_LEADING_SEPARATE:
                return readNumericDisplay(length, scale, storage, DisplaySign.LEADING_SEPARATE);
            case UNSIGNED_PACKED:
                return appendTrailingZeros(
                        readUnsignedPacked(packedLength, scale, storage), ppCount);
            case SIGNED_PACKED:
                return appendTrailingZeros(readSignedPacked(packedLength, scale, storage), ppCount);
            case UNSIGNED_BINARY_NATIVE:
                return readUnsignedBinaryNative(length, scale, storage);
            case SIGNED_BINARY_NATIVE:
                return readSignedBinaryNative(length, scale, storage);
            case ALPHABETIC:
            case GROUP:
                return readAlphanumeric(length, scale, storage);
            case FLOAT:
                return readFloat(length, scale, storage);
            case NATIONAL:
                return readNational(length, scale, storage);
            case ALPHANUMERIC_VARYING:
                return readAlphanumericVarying(length, scale, storage);
            case JAPANESE_VARYING:
                return readJapaneseVarying(length, scale, storage);
            default:
                return readAlphanumeric(length, scale, storage);
        }
    }

    /** PIC 9 DISPLAY (ゾーン10進 zoned decimal) 項目の符号表現。 */
    private enum DisplaySign {
        /** 演算符号 (operational sign) なし。 */
        UNSIGNED,
        /** 先頭桁に overpunch された符号。 */
        LEADING_COMBINED,
        /** 末尾桁に overpunch された符号。 */
        TRAILING_COMBINED,
        /** 先頭の分離符号バイト ('+' / '-')。 */
        LEADING_SEPARATE,
        /** 末尾の分離符号バイト ('+' / '-')。 */
        TRAILING_SEPARATE
    }

    /**
     * PIC 9 DISPLAY (ゾーン10進 zoned decimal) の値を読み取る。5 つのバリアント (UNSIGNED、LEADING /
     * TRAILING COMBINED、LEADING / TRAILING SEPARATE) は演算符号 (operational sign) の置かれる
     * 位置だけが異なり、符号と桁バイトの並びが特定できれば、同じフォーマット処理パスを共有する。
     */
    private static String readNumericDisplay(
            int length, int scale, CobolDataStorage storage, DisplaySign sign) {
        // length は分離符号バイトも含むフィールド全体のサイズである
        // (writeNumericDisplay と対称: 先頭/末尾の分離符号は length の内側に置かれる)。
        // ここでは (getByteArrayRef ではなく) コピーが必要である。結合符号 (combined sign) のケースでは
        // overpunch を取り除くために `data` 内の桁バイトを上書きするが、これが COBOL ストレージを
        // 変更してはならないためである。
        byte[] data = storage.getByteArray(0, length);
        boolean negative = false;
        int digitStart = 0;
        int digitCount = length;

        switch (sign) {
            case UNSIGNED:
                break;
            case LEADING_COMBINED:
                negative = isNegativeOverpunch(data[0]);
                if (negative) {
                    data[0] = clearOverpunch(data[0]);
                }
                break;
            case TRAILING_COMBINED:
                negative = isNegativeOverpunch(data[length - 1]);
                if (negative) {
                    data[length - 1] = clearOverpunch(data[length - 1]);
                }
                break;
            case LEADING_SEPARATE:
                negative = data[0] == (byte) '-';
                digitStart = 1; // 先頭バイトが分離符号である
                digitCount = length - 1; // 符号バイトの分だけ桁数を減らす
                break;
            case TRAILING_SEPARATE:
                negative = data[length - 1] == (byte) '-';
                digitCount = length - 1; // 末尾バイトが分離符号である
                break;
            default:
                break;
        }

        String digits = formatDisplayDigits(data, digitStart, digitCount, scale);
        if (negative && !"0".equals(digits)) {
            return "-" + digits;
        }
        return digits;
    }

    /**
     * {@code data} 内の {@code digitStart} から始まる {@code digitCount} 個の桁バイトから、
     * (符号なしの) 10進文字列を構築する。作業バッファをゼロ埋めし、{@code scale < 0} のときに
     * 小数点を挿入し、先頭のゼロを取り除く。
     *
     * <p>fully-fractional な値 (小数桁数 &ge; 桁数。例: {@code PIC SV9(6)}) でも先頭の {@code "0."}
     * を正しく表現できるよう、先頭に 1 バイトのパディングを確保する。それ以外のケースでは、これは
     * 単なる先頭のゼロであり、{@link #removeLeadingZeros} によって取り除かれる。
     */
    private static String formatDisplayDigits(
            byte[] data, int digitStart, int digitCount, int scale) {
        int realDataLength = (scale < 0) ? digitCount + 2 : digitCount + scale + 1;
        byte[] realData = new byte[realDataLength];
        java.util.Arrays.fill(realData, (byte) '0');
        System.arraycopy(data, digitStart, realData, 1, digitCount);

        if (scale < 0) {
            int pointIndex = realDataLength + scale - 1;
            if (pointIndex > 0 && pointIndex < realDataLength) {
                for (int i = realDataLength - 1; i > pointIndex; i--) {
                    realData[i] = realData[i - 1];
                }
                realData[pointIndex] = (byte) '.';
            }
        }

        return removeLeadingZeros(realData, false);
    }

    /** ゾーン10進 (zoned decimal) のバイトが負の overpunch 符号 (0x70-0x79) を持つ場合に true。 */
    private static boolean isNegativeOverpunch(byte b) {
        int v = b & 0xFF;
        return v >= 0x70 && v <= 0x79;
    }

    /** ゾーン10進 (zoned decimal) のバイトから overpunch 符号を取り除き、通常の桁に戻す。 */
    private static byte clearOverpunch(byte b) {
        return (byte) ((b & 0xFF) - 0x40);
    }

    private static String readUnsignedPacked(int length, int scale, CobolDataStorage storage) {
        int len = (length / 2) + 1;
        byte[] digits = unpackBcd(storage, length, len);
        return formatPackedResult(digits, length, scale, false);
    }

    private static String readSignedPacked(int length, int scale, CobolDataStorage storage) {
        int len = (length / 2) + 1;
        byte lastByte = storage.getByte(len - 1);
        boolean isNegative = (lastByte & 0x0F) == 0x0D;
        byte[] digits = unpackBcd(storage, length, len);
        return formatPackedResult(digits, length, scale, isNegative);
    }

    private static String appendTrailingZeros(String value, int count) {
        if (count <= 0) {
            return value;
        }
        // 末尾にゼロを追加する（負の場合は符号を復元する）
        boolean neg = value.startsWith("-");
        String abs = neg ? value.substring(1) : value;
        StringBuilder sb = new StringBuilder(abs);
        for (int i = 0; i < count; i++) {
            sb.append('0');
        }
        return neg ? "-" + sb.toString() : sb.toString();
    }

    private static byte[] unpackBcd(CobolDataStorage storage, int length, int len) {
        byte[] digits = new byte[length];
        for (int i = 0; i < len; i++) {
            int b = storage.getByte(i) & 0xFF;
            byte a0 = (byte) (((b & 0xF0) >> 4) + '0');
            byte a1 = (byte) ((b & 0x0F) + '0');
            if (length % 2 == 0) {
                if (i == 0) {
                    digits[0] = a1;
                } else if (i == len - 1) {
                    digits[length - 1] = a0;
                } else {
                    digits[2 * i - 1] = a0;
                    digits[2 * i] = a1;
                }
            } else {
                if (i == len - 1) {
                    digits[length - 1] = a0;
                } else {
                    digits[2 * i] = a0;
                    digits[2 * i + 1] = a1;
                }
            }
        }
        return digits;
    }

    private static String formatPackedResult(
            byte[] digits, int length, int scale, boolean isNegative) {
        byte[] tmpData;
        int tmpDataLen;

        if (-scale > length) {
            tmpDataLen = -scale + 2;
            tmpData = new byte[tmpDataLen];
            java.util.Arrays.fill(tmpData, (byte) '0');
            tmpData[1] = (byte) '.';
            System.arraycopy(digits, 0, tmpData, tmpDataLen - digits.length, digits.length);
        } else if (-scale > 0) {
            tmpDataLen = length + 1;
            tmpData = new byte[tmpDataLen];
            int intPartLen = tmpDataLen + scale - 1;
            System.arraycopy(digits, 0, tmpData, 0, intPartLen);
            tmpData[intPartLen] = (byte) '.';
            for (int i = 0; i < -scale; i++) {
                tmpData[intPartLen + 1 + i] = digits[digits.length + scale + i];
            }
            String noZeros = removeLeadingZeros(tmpData, false);
            if (isNegative && !"0".equals(noZeros)) {
                return "-" + noZeros;
            }
            return noZeros;
        } else {
            tmpDataLen = length + scale;
            tmpData = new byte[tmpDataLen];
            java.util.Arrays.fill(tmpData, (byte) '0');
            System.arraycopy(digits, 0, tmpData, 0, length);
        }

        String result = removeLeadingZeros(tmpData, false);
        if (isNegative && !"0".equals(result)) {
            return "-" + result;
        }
        return result;
    }

    private static String readUnsignedBinaryNative(
            int length, int scale, CobolDataStorage storage) {
        ByteBuffer bb = bigEndianRef(storage, 0, length);
        long value;
        switch (length) {
            case 1:
                value = bb.get() & 0xFFL;
                break;
            case 2:
                value = bb.getShort() & 0xFFFFL;
                break;
            case 4:
                value = bb.getInt() & 0xFFFFFFFFL;
                break;
            case 8:
                value = bb.getLong();
                break;
            default:
                value = 0;
                break;
        }
        return applyBinaryScale(value, scale);
    }

    /**
     * ネイティブバイナリ (COMP-5) の整数値に scale を適用して 10 進文字列にする。scale &lt; 0 の
     * フィールド (例: PIC 9(4)V99) は格納された整数の途中に小数点を挿入する。符号付き・符号なしの
     * 両方で同じ処理を使う。
     */
    private static String applyBinaryScale(long value, int scale) {
        String str = Long.toString(value);
        if (scale >= 0) {
            return str;
        }
        boolean neg = value < 0;
        String abs = neg ? str.substring(1) : str;
        String result;
        if (abs.length() <= -scale) {
            StringBuilder sb = new StringBuilder("0.");
            for (int i = 0; i < -scale - abs.length(); i++) {
                sb.append('0');
            }
            sb.append(abs);
            result = sb.toString();
        } else {
            int pointPos = abs.length() + scale;
            result = abs.substring(0, pointPos) + "." + abs.substring(pointPos);
        }
        return neg ? "-" + result : result;
    }

    private static String readSignedBinaryNative(int length, int scale, CobolDataStorage storage) {
        ByteBuffer bb = bigEndianRef(storage, 0, length);
        long value;
        switch (length) {
            case 1:
                value = bb.get();
                break;
            case 2:
                value = bb.getShort();
                break;
            case 4:
                value = bb.getInt();
                break;
            case 8:
                value = bb.getLong();
                break;
            default:
                value = 0;
                break;
        }
        return applyBinaryScale(value, scale);
    }

    /**
     * ストレージの内部配列を (コピーせずに) ビッグエンディアンの {@link ByteBuffer} としてラップし、
     * {@code index} から始まる {@code length} バイトの範囲に位置付ける。
     */
    private static ByteBuffer bigEndianRef(CobolDataStorage storage, int index, int length) {
        ByteBuffer bb =
                ByteBuffer.wrap(
                        storage.getByteArrayRef(index, length), storage.getIndex() + index, length);
        bb.order(ByteOrder.BIG_ENDIAN);
        return bb;
    }

    /**
     * ストレージの内部配列の {@code index} から始まる {@code length} バイトを、スライスをコピーせずに
     * Shift-JIS としてデコードする。
     */
    private static String shiftJisRef(CobolDataStorage storage, int index, int length) {
        return new String(
                storage.getByteArrayRef(index, length),
                storage.getIndex() + index,
                length,
                SHIFT_JIS);
    }

    private static String readAlphanumeric(int length, int scale, CobolDataStorage storage) {
        int end = length;
        while (end > 0 && (storage.getByte(end - 1) & 0xFF) == 0x20) {
            end--;
        }
        return shiftJisRef(storage, 0, end);
    }

    private static String readFloat(int length, int scale, CobolDataStorage storage) {
        return Double.toString(bigEndianRef(storage, 0, 8).getDouble());
    }

    private static String readNational(int length, int scale, CobolDataStorage storage) {
        byte[] data = storage.getByteArrayRef(0, length);
        int start = storage.getIndex();
        int end = start + length;
        // National 項目は全角 (表意文字) スペースでパディングされ、これは Shift-JIS で 0x81 0x40 の
        // 2 バイトとして符号化される。パディングをデコードしないよう、バイトレベルで取り除く。
        while (end - start >= 2
                && data[end - 2] == SJIS_FULLWIDTH_SPACE[0]
                && data[end - 1] == SJIS_FULLWIDTH_SPACE[1]) {
            end -= 2;
        }
        return new String(data, start, end - start, SHIFT_JIS);
    }

    /** VARYING ヘッダ (先頭 4 バイト) に格納されたビッグエンディアンの長さを読み取る。 */
    private static int readVaryingHeaderLength(CobolDataStorage storage) {
        return bigEndianRef(storage, 0, OCDB_VARCHAR_HEADER_BYTE).getInt();
    }

    private static String readAlphanumericVarying(int length, int scale, CobolDataStorage storage) {
        int lenSize = readVaryingHeaderLength(storage);
        return shiftJisRef(storage, OCDB_VARCHAR_HEADER_BYTE, lenSize);
    }

    private static String readJapaneseVarying(int length, int scale, CobolDataStorage storage) {
        int charCount = readVaryingHeaderLength(storage);
        return shiftJisRef(storage, OCDB_VARCHAR_HEADER_BYTE, charCount * 2);
    }

    private static String removeLeadingZeros(byte[] data, boolean hasLeadingSign) {
        int start = 0;
        if (hasLeadingSign && data.length > 0 && (data[0] == (byte) '-' || data[0] == (byte) '+')) {
            start = 1;
        }

        int i = start;
        while (i < data.length && data[i] == (byte) '0') {
            i++;
        }

        String digits;
        if (i == data.length) {
            digits = "0";
        } else if (data[i] == (byte) '.') {
            digits = "0" + new String(data, i, data.length - i);
        } else {
            digits = new String(data, i, data.length - i);
        }

        if (hasLeadingSign && data.length > 0 && data[0] == (byte) '-' && !"0".equals(digits)) {
            return "-" + digits;
        }
        return digits;
    }

    // -------------------------------------------------------
    // stringToCobol: SQL の結果データを COBOL ストレージへ書き戻す
    // -------------------------------------------------------
    /**
     * SQL の結果データを COBOL ホスト変数のストレージへ書き戻す。
     *
     * @param field 書き込み先の COBOL ホスト変数フィールド
     * @param resultData SQL 結果から得られた生のバイトデータ
     */
    /**
     * SQL の結果データを特定の COBOL ストレージ位置へ書き込む。
     * 行ごとにストレージのオフセットが変わる OCCURS 配列で使用する。
     *
     * @param field COBOL フィールド（型解決用）
     * @param storage 書き込み先のストレージ位置
     * @param length フィールドのバイト長
     * @param resultData バイト列としての SQL 結果データ
     */
    static void stringToCobolRaw(
            AbstractCobolField field, CobolDataStorage storage, int length, byte[] resultData) {
        if (field == null || storage == null || resultData == null) {
            return;
        }
        int scale = Math.min(0, -field.getAttribute().getScale());
        HvarType hvarType = resolveHvarType(field);
        // VARYING の場合、length = ARR サイズ
        if (hvarType == HvarType.ALPHANUMERIC_VARYING || hvarType == HvarType.JAPANESE_VARYING) {
            length = field.getSize() - OCDB_VARCHAR_HEADER_BYTE;
        }
        // packed decimal の場合、バイトサイズではなく (PP を除いた) 桁数を使う
        if (hvarType == HvarType.UNSIGNED_PACKED || hvarType == HvarType.SIGNED_PACKED) {
            int rawScale = field.getAttribute().getScale();
            length = field.getAttribute().getDigits();
            if (rawScale < 0) {
                length += rawScale;
                // PP: SQL 結果から末尾のゼロを取り除き、位置合わせには正の scale を使う
                resultData = stripTrailingDigits(resultData, -rawScale);
            }
        }
        stringToCobolInternal(hvarType, length, scale, storage, resultData);
    }

    /**
     * SQL の結果データを COBOL ホスト変数フィールドへ書き戻す。
     *
     * @param field 書き込み先の COBOL フィールド
     * @param resultData バイト列としての SQL 結果データ
     */
    public static void stringToCobol(AbstractCobolField field, byte[] resultData) {
        if (field == null || field.getDataStorage() == null || resultData == null) {
            return;
        }
        HvarType hvarType = resolveHvarType(field);
        int length = field.getSize();
        // packed decimal の場合、バイトサイズではなく (PP を除いた) 桁数を使う
        if (hvarType == HvarType.UNSIGNED_PACKED || hvarType == HvarType.SIGNED_PACKED) {
            int rawScale = field.getAttribute().getScale();
            length = field.getAttribute().getDigits();
            if (rawScale < 0) {
                length += rawScale;
                // PP: SQL 結果から末尾のゼロを取り除く
                resultData = stripTrailingDigits(resultData, -rawScale);
            }
        }
        // VARYING の場合、length = ARR サイズ (全体 - 4 バイトのヘッダ)
        if (hvarType == HvarType.ALPHANUMERIC_VARYING || hvarType == HvarType.JAPANESE_VARYING) {
            length = field.getSize() - OCDB_VARCHAR_HEADER_BYTE;
        }
        stringToCobolInternal(
                hvarType,
                length,
                -field.getAttribute().getScale(),
                field.getDataStorage(),
                resultData);
    }

    private static void stringToCobolInternal(
            HvarType hvarType, int length, int scale, CobolDataStorage storage, byte[] resultData) {
        switch (hvarType) {
            case UNSIGNED_NUMERIC:
                writeNumericDisplay(length, scale, storage, resultData, DisplaySign.UNSIGNED);
                break;
            case SIGNED_TRAILING_COMBINED:
                writeNumericDisplay(
                        length, scale, storage, resultData, DisplaySign.TRAILING_COMBINED);
                break;
            case SIGNED_TRAILING_SEPARATE:
                writeNumericDisplay(
                        length, scale, storage, resultData, DisplaySign.TRAILING_SEPARATE);
                break;
            case SIGNED_LEADING_SEPARATE:
                writeNumericDisplay(
                        length, scale, storage, resultData, DisplaySign.LEADING_SEPARATE);
                break;
            case SIGNED_LEADING_COMBINED:
                writeNumericDisplay(
                        length, scale, storage, resultData, DisplaySign.LEADING_COMBINED);
                break;
            case UNSIGNED_PACKED:
                writeUnsignedPacked(length, scale, storage, resultData);
                break;
            case SIGNED_PACKED:
                writeSignedPacked(length, scale, storage, resultData);
                break;
            case ALPHABETIC:
            case GROUP:
                writeAlphanumeric(length, scale, storage, resultData);
                break;
            case NATIONAL:
                writeNational(length, scale, storage, resultData);
                break;
            case ALPHANUMERIC_VARYING:
                writeAlphanumericVarying(length, scale, storage, resultData);
                break;
            case JAPANESE_VARYING:
                writeJapaneseVarying(length, scale, storage, resultData);
                break;
            case UNSIGNED_BINARY_NATIVE:
            case SIGNED_BINARY_NATIVE:
                writeAlphanumeric(length, scale, storage, resultData);
                break;
            case FLOAT:
                writeAlphanumeric(length, scale, storage, resultData);
                break;
            default:
                writeAlphanumeric(length, scale, storage, resultData);
                break;
        }
    }

    /**
     * 数値 (ASCII 文字列として表現され、必要に応じて先頭の '-' と '.' を伴う) を
     * PIC 9 DISPLAY (ゾーン10進 zoned decimal) フィールドへ書き込む。5 つの符号バリアントは
     * この処理パスを共有し、桁領域の位置と符号の適用方法だけが異なる。
     */
    private static void writeNumericDisplay(
            int length, int scale, CobolDataStorage storage, byte[] str, DisplaySign sign) {
        byte[] finalBuf = new byte[length];
        java.util.Arrays.fill(finalBuf, (byte) '0');
        boolean isNegative = str.length > 0 && str[0] == (byte) '-';
        int valueFirstIndex = isNegative ? 1 : 0;
        int indexOfDecimalPoint = indexOf(str, (byte) '.');
        if (indexOfDecimalPoint < 0) {
            indexOfDecimalPoint = str.length;
        }

        // finalBuf 内の桁領域。分離符号バイトはこの領域のすぐ外側に置かれる
        // (leading -> インデックス 0、trailing -> 最後のインデックス)。
        int regionStart = (sign == DisplaySign.LEADING_SEPARATE) ? 1 : 0;
        int regionEnd = (sign == DisplaySign.TRAILING_SEPARATE) ? length - 1 : length;

        placeDisplayDigits(
                finalBuf, str, valueFirstIndex, indexOfDecimalPoint, scale, regionStart, regionEnd);
        applyWriteSign(finalBuf, sign, isNegative);
        storage.memcpy(finalBuf, finalBuf.length);
    }

    /**
     * {@code str} の (符号を取り除いた) 桁を {@code finalBuf} の桁領域 [{@code regionStart},
     * {@code regionEnd}) へ配置する。{@code scale} が暗黙の小数位置を与えるよう位置合わせする
     * (scale &lt; 0 は {@code -scale} 個の小数桁を意味し、scale &ge; 0 は整数 / 末尾 P (trailing-P)
     * の値である)。
     */
    private static void placeDisplayDigits(
            byte[] finalBuf,
            byte[] str,
            int valueFirstIndex,
            int indexOfDecimalPoint,
            int scale,
            int regionStart,
            int regionEnd) {
        if (scale >= 0) {
            // 整数桁を暗黙の小数点位置へ右詰めでコピーする (finalBuf[i + delta] = str[i])。
            // src/dest 双方を桁領域 [regionStart, regionEnd) に収まる範囲へクランプする。
            int delta = regionEnd - (indexOfDecimalPoint + scale);
            int srcStart = Math.max(valueFirstIndex, regionStart - delta);
            int srcEnd = Math.min(indexOfDecimalPoint, regionEnd - delta);
            int len = srcEnd - srcStart;
            if (len > 0) {
                System.arraycopy(str, srcStart, finalBuf, srcStart + delta, len);
            }
        } else {
            // 整数桁を小数点直前まで後方詰めでコピーする。
            int intLen =
                    Math.max(
                            0,
                            Math.min(
                                    regionEnd + scale - regionStart,
                                    indexOfDecimalPoint - valueFirstIndex));
            if (intLen > 0) {
                System.arraycopy(
                        str,
                        indexOfDecimalPoint - intLen,
                        finalBuf,
                        regionEnd + scale - intLen,
                        intLen);
            }
            // 小数桁 (小数点の次から) を桁領域の末尾へコピーする。
            int fracLen = Math.max(0, Math.min(-scale, str.length - indexOfDecimalPoint - 1));
            if (fracLen > 0) {
                System.arraycopy(
                        str, indexOfDecimalPoint + 1, finalBuf, regionEnd + scale, fracLen);
            }
        }
    }

    /** すでにフォーマット済みのゾーン10進 (zoned decimal) バッファに、演算符号 (operational sign) をその場で適用する。 */
    private static void applyWriteSign(byte[] finalBuf, DisplaySign sign, boolean isNegative) {
        switch (sign) {
            case UNSIGNED:
                break;
            case LEADING_COMBINED:
                if (isNegative) {
                    finalBuf[0] = (byte) ((finalBuf[0] & 0xFF) + 0x40);
                }
                break;
            case TRAILING_COMBINED:
                if (isNegative) {
                    int last = finalBuf.length - 1;
                    finalBuf[last] = (byte) ((finalBuf[last] & 0xFF) + 0x40);
                }
                break;
            case LEADING_SEPARATE:
                finalBuf[0] = isNegative ? (byte) '-' : (byte) '+';
                break;
            case TRAILING_SEPARATE:
                finalBuf[finalBuf.length - 1] = isNegative ? (byte) '-' : (byte) '+';
                break;
            default:
                break;
        }
    }

    private static void writeUnsignedPacked(
            int length, int scale, CobolDataStorage storage, byte[] str) {
        writePackedDecimal(length, scale, storage, str, false);
    }

    private static void writeSignedPacked(
            int length, int scale, CobolDataStorage storage, byte[] str) {
        writePackedDecimal(length, scale, storage, str, true);
    }

    /**
     * 数値文字列を packed-decimal (COMP-3) ストレージへ書き込む。符号なし (unsigned) と
     * 符号付き (signed) の唯一の違いは末尾バイトの符号ニブルである: unsigned は 0x0F、
     * signed は 正 = 0x0C / 負 = 0x0D。桁の配置処理は両者で共通である。
     */
    private static void writePackedDecimal(
            int length, int scale, CobolDataStorage storage, byte[] str, boolean signed) {
        int strStartIndex = 0;
        boolean negative = false;
        if (str.length > 0 && (str[0] == (byte) '+' || str[0] == (byte) '-')) {
            strStartIndex = 1;
            negative = str[0] == (byte) '-';
        }
        int strPointIndex = indexOf(str, (byte) '.');
        if (strPointIndex < 0) {
            strPointIndex = str.length;
        }
        int dataPointIndex = length + scale;
        int realDataLength = (length / 2) + 1;

        storage.memset((byte) 0, realDataLength);
        byte signNibble;
        if (!signed) {
            signNibble = (byte) 0x0F;
        } else {
            signNibble = negative ? (byte) 0x0D : (byte) 0x0C;
        }
        storage.setByte(realDataLength - 1, signNibble);

        for (int i = 0; i < length; i++) {
            int strIndex = i - dataPointIndex + strPointIndex;
            if (strIndex >= strPointIndex) {
                strIndex += 1;
            }
            byte digit;
            if (strIndex >= strStartIndex && strIndex < str.length) {
                digit = str[strIndex];
            } else {
                digit = (byte) '0';
            }
            putPackedDigit(storage, length, i, digit);
        }
    }

    /**
     * BCD 桁 {@code digit} ('0'-'9') を packed-decimal ストレージの該当バイトへ OR 書き込みする。
     * {@code dataLen} と {@code index} の偶奇で書き込み先バイトと上位/下位ニブルが決まる。
     * 反復ごとの配列割り当てを避けるため、結果を返さずその場で書き込む。
     */
    private static void putPackedDigit(
            CobolDataStorage storage, int dataLen, int index, byte digit) {
        int d = (digit & 0xFF) - '0';
        int byteIndex;
        int nibble;
        if (dataLen % 2 == 0) {
            byteIndex = (index + 1) / 2;
            nibble = (index % 2 == 0) ? d : (d << 4);
        } else {
            byteIndex = index / 2;
            nibble = (index % 2 == 0) ? (d << 4) : d;
        }
        byte b = storage.getByte(byteIndex);
        storage.setByte(byteIndex, (byte) (b | nibble));
    }

    private static void writeAlphanumeric(
            int length, int scale, CobolDataStorage storage, byte[] str) {
        if (str.length >= length) {
            storage.memcpy(str, length);
        } else {
            storage.memset((byte) ' ', length);
            storage.memcpy(str, str.length);
        }
    }

    private static void writeNational(int length, int scale, CobolDataStorage storage, byte[] str) {
        // length はすでにバイトサイズである (例: PIC N(5) なら 10)。National 項目は全角
        // (表意文字) スペース (Shift-JIS 0x81 0x40) でパディングする。readNational が末尾の
        // 全角スペースを取り除くため、これで読み書きが対称になる。
        // SQL の結果は既に Shift-JIS。COBOL の MOVE 時と異なり半角→全角変換
        // (CobolNationalField.han2zen) は適用しない (Open COBOL ESQL 4J の実装に準拠)。
        for (int j = 0; j + 1 < length; j += 2) {
            storage.memcpy(j, SJIS_FULLWIDTH_SPACE, 2);
        }
        int copyLen = Math.min(length, str.length);
        storage.memcpy(str, copyLen);
    }

    private static void writeAlphanumericVarying(
            int length, int scale, CobolDataStorage storage, byte[] str) {
        if (length <= 0) {
            return;
        }
        byte[] lengthBytes = new byte[OCDB_VARCHAR_HEADER_BYTE];
        if (str.length >= length) {
            ByteBuffer.wrap(lengthBytes).putInt(length);
            storage.memcpy(0, lengthBytes, OCDB_VARCHAR_HEADER_BYTE);
            storage.memcpy(OCDB_VARCHAR_HEADER_BYTE, str, length);
        } else {
            ByteBuffer.wrap(lengthBytes).putInt(str.length);
            storage.memset(OCDB_VARCHAR_HEADER_BYTE, (byte) ' ', length);
            storage.memcpy(0, lengthBytes, OCDB_VARCHAR_HEADER_BYTE);
            storage.memcpy(OCDB_VARCHAR_HEADER_BYTE, str, str.length);
        }
    }

    private static void writeJapaneseVarying(
            int length, int scale, CobolDataStorage storage, byte[] str) {
        // SQL の結果は既に Shift-JIS。COBOL の MOVE 時と異なり半角→全角変換
        // (CobolNationalField.han2zen) は適用しない (Open COBOL ESQL 4J の実装に準拠)。
        byte[] lengthBytes = new byte[OCDB_VARCHAR_HEADER_BYTE];
        if (str.length >= length) {
            ByteBuffer.wrap(lengthBytes).putInt(length / 2);
            storage.memcpy(0, lengthBytes, OCDB_VARCHAR_HEADER_BYTE);
            storage.memcpy(OCDB_VARCHAR_HEADER_BYTE, str, length);
        } else {
            // 全角 (表意文字) スペースでデータ領域をパディングする。
            for (int i = OCDB_VARCHAR_HEADER_BYTE;
                    i < OCDB_VARCHAR_HEADER_BYTE + length - 1;
                    i += 2) {
                storage.memcpy(i, SJIS_FULLWIDTH_SPACE, 2);
            }
            ByteBuffer.wrap(lengthBytes).putInt(str.length / 2);
            storage.memcpy(0, lengthBytes, OCDB_VARCHAR_HEADER_BYTE);
            storage.memcpy(OCDB_VARCHAR_HEADER_BYTE, str, str.length);
        }
    }

    /**
     * PP (scaling position) フィールド向けに、数値文字列から末尾の桁を取り除く。例えば、
     * count=2 のとき "999900" は "9999" になる。先頭の符号 (任意) も処理する。
     */
    private static byte[] stripTrailingDigits(byte[] data, int count) {
        if (count <= 0 || data.length == 0) {
            return data;
        }
        // 数値部分の末尾を探す (先頭に符号があればスキップする)
        int start = 0;
        if (data[0] == (byte) '-' || data[0] == (byte) '+') {
            start = 1;
        }
        // 小数点の位置を探す
        int dotPos = indexOf(data, (byte) '.');
        int endPos = dotPos >= 0 ? dotPos : data.length;
        // 整数部 (小数点より前) から 'count' 個の桁を取り除く
        int intDigits = endPos - start;
        if (count >= intDigits) {
            // すべての整数桁が取り除かれてしまう場合、符号付きで "0" を返す
            if (start > 0) {
                return new byte[] {data[0], (byte) '0'};
            }
            return new byte[] {(byte) '0'};
        }
        int newEnd = endPos - count;
        byte[] result = new byte[start + (newEnd - start) + (data.length - endPos)];
        if (start > 0) {
            result[0] = data[0];
        }
        System.arraycopy(data, start, result, start, newEnd - start);
        if (dotPos >= 0) {
            System.arraycopy(data, endPos, result, newEnd, data.length - endPos);
        }
        return result;
    }

    private static int indexOf(byte[] arr, byte target) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == target) {
                return i;
            }
        }
        return -1;
    }

    // -------------------------------------------------------
    // setParam: COBOL の値を PreparedStatement にバインドする
    // -------------------------------------------------------
    /**
     * COBOL ホスト変数の値を JDBC PreparedStatement のパラメータにバインドする。
     *
     * @param stmt JDBC の PreparedStatement
     * @param index 1 始まりのパラメータインデックス
     * @param metaData 型推論用のパラメータメタデータ (null の場合がある)
     * @param field バインド対象の COBOL ホスト変数フィールド
     * @throws SQLException JDBC エラーが発生した場合
     */
    public static void setParam(
            PreparedStatement stmt, int index, ParameterMetaData metaData, AbstractCobolField field)
            throws SQLException {
        String str = cobolToString(field);
        if (metaData == null) {
            stmt.setString(index, str);
            return;
        }
        int paramType;
        try {
            paramType = metaData.getParameterType(index);
        } catch (SQLException e) {
            stmt.setString(index, str);
            return;
        }

        // 数値/日付への変換 (parseInt / BigDecimal / Date.valueOf 等) は失敗時に非チェック例外を
        // 投げる。呼び出し側は SQLException しか捕捉しないため、ここで SQLState "42804"
        // (ECPG_DATA_FORMAT_ERROR) の SQLException に変換し、SQLCA 経由でエラー報告させる。
        try {
            switch (paramType) {
                case Types.CHAR:
                case Types.VARCHAR:
                case Types.NCHAR:
                case Types.NVARCHAR:
                case Types.LONGVARCHAR:
                case Types.LONGNVARCHAR:
                case Types.CLOB:
                case Types.BLOB:
                case Types.ARRAY:
                case Types.OTHER:
                case Types.LONGVARBINARY:
                case Types.JAVA_OBJECT:
                    stmt.setString(index, str);
                    break;
                case Types.INTEGER:
                case Types.SMALLINT:
                case Types.TINYINT:
                    stmt.setInt(index, Integer.parseInt(str));
                    break;
                case Types.BIGINT:
                case Types.DECIMAL:
                case Types.NUMERIC:
                    // NUMERIC/DECIMAL は BigDecimal で正確にバインドする。
                    // setDouble だと 16 桁超の精度や正確な小数で丸め誤差が生じる。
                    stmt.setBigDecimal(index, new BigDecimal(str));
                    break;
                case Types.FLOAT:
                    stmt.setFloat(index, Float.parseFloat(str));
                    break;
                case Types.DOUBLE:
                case Types.REAL:
                    stmt.setDouble(index, Double.parseDouble(str));
                    break;
                case Types.DATE:
                    stmt.setDate(index, java.sql.Date.valueOf(str));
                    break;
                case Types.TIME:
                case Types.TIME_WITH_TIMEZONE:
                    stmt.setTime(index, java.sql.Time.valueOf(str));
                    break;
                case Types.TIMESTAMP:
                case Types.TIMESTAMP_WITH_TIMEZONE:
                    stmt.setTimestamp(index, java.sql.Timestamp.valueOf(str));
                    break;
                case Types.BOOLEAN:
                case Types.BIT:
                    stmt.setBoolean(index, Boolean.valueOf(str));
                    break;
                default:
                    stmt.setString(index, str);
                    break;
            }
        } catch (IllegalArgumentException e) {
            // NumberFormatException は IllegalArgumentException のサブクラス。
            // Date/Time/Timestamp.valueOf の解析失敗も IllegalArgumentException を投げる。
            throw new SQLException(
                    "Data format error converting host variable value \""
                            + str
                            + "\" for SQL type "
                            + paramType
                            + ": "
                            + e.getMessage(),
                    "42804",
                    e);
        }
    }

    // -------------------------------------------------------
    // getValueFromResultSet: カラムの値を SHIFT-JIS のバイト列として取り出す
    // -------------------------------------------------------
    private static final String DATE_FORMAT_PATTERN = "yyyy-MM-dd HH:mm:ss";

    // SimpleDateFormat はスレッドセーフでないため ThreadLocal でスレッドごとに 1 個を再利用する。
    private static final ThreadLocal<SimpleDateFormat> DATE_FORMAT =
            ThreadLocal.withInitial(() -> new SimpleDateFormat(DATE_FORMAT_PATTERN));

    /**
     * ResultSet からカラムの値を SHIFT-JIS でエンコードしたバイト列として取り出す。
     *
     * @param rs 現在の行に位置付けられた JDBC result set
     * @param columnIndex 1 始まりのカラムインデックス
     * @return カラムの値をバイト配列として返す。値が SQL NULL の場合は null
     * @throws SQLException 列値の取得・変換に失敗した場合 (実エラーを NULL に潰さず呼び出し元へ伝播する)
     */
    public static byte[] getValueFromResultSet(ResultSet rs, int columnIndex) throws SQLException {
        int colType = rs.getMetaData().getColumnType(columnIndex);
        String strValue;
        switch (colType) {
            case Types.CHAR:
            case Types.VARCHAR:
            case Types.NCHAR:
            case Types.NVARCHAR:
            case Types.LONGVARCHAR:
            case Types.LONGNVARCHAR:
                {
                    String s = rs.getString(columnIndex);
                    if (s == null) {
                        return null;
                    }
                    return s.getBytes(SHIFT_JIS);
                }
            case Types.DECIMAL:
            case Types.NUMERIC:
                {
                    BigDecimal bd = rs.getBigDecimal(columnIndex);
                    if (bd == null) {
                        return null;
                    }
                    return bd.toString().getBytes();
                }
            case Types.TIMESTAMP:
                {
                    java.sql.Timestamp ts = rs.getTimestamp(columnIndex);
                    if (ts == null) {
                        return null;
                    }
                    return DATE_FORMAT.get().format(ts).getBytes();
                }
            case Types.DATE:
                {
                    java.sql.Date d = rs.getDate(columnIndex);
                    if (d == null) {
                        return null;
                    }
                    return DATE_FORMAT.get().format(d).getBytes();
                }
            case Types.TIME:
                {
                    java.sql.Time t = rs.getTime(columnIndex);
                    if (t == null) {
                        return null;
                    }
                    return t.toString().getBytes();
                }
            case Types.TINYINT:
                strValue = Byte.toString(rs.getByte(columnIndex));
                break;
            case Types.SMALLINT:
                strValue = Short.toString(rs.getShort(columnIndex));
                break;
            case Types.INTEGER:
                strValue = Integer.toString(rs.getInt(columnIndex));
                break;
            case Types.BIGINT:
                strValue = Long.toString(rs.getLong(columnIndex));
                break;
            case Types.BOOLEAN:
                strValue = Boolean.toString(rs.getBoolean(columnIndex));
                break;
            case Types.FLOAT:
            case Types.DOUBLE:
            case Types.REAL:
                strValue = Double.toString(rs.getDouble(columnIndex));
                break;
            case Types.ROWID:
                {
                    java.sql.RowId rowId = rs.getRowId(columnIndex);
                    if (rowId == null) {
                        return null;
                    }
                    return rowId.toString().getBytes();
                }
            default:
                {
                    String s = rs.getString(columnIndex);
                    if (s == null) {
                        return null;
                    }
                    return s.getBytes(SHIFT_JIS);
                }
        }
        if (rs.wasNull()) {
            return null;
        }
        return strValue.getBytes();
    }
}
