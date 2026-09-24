#!/bin/sh
#
# Copyright (C) 2001-2009 Keisuke Nishida
# Copyright (C) 2007-2009 Roger While
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this software; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
# Boston, MA 02111-1307 USA

# Run the NIST COBOL85 test programs of one module directory and write
# the results to report.txt.
#
# Usage: sh report.sh [extra-cobj-options]
#
# All COBOL sources are first compiled with a single cobj invocation so
# that cobj runs javac only once, then every program is executed in turn.

opt="$*"
compile="cobj -jar -std=cobol85${opt:+ $opt}"

export COB_SWITCH_1=ON
export COB_SWITCH_2=OFF

# The classpath separator of the JVM differs between Windows and other
# platforms.
case "$(uname -s 2>/dev/null)" in
MINGW* | MSYS* | CYGWIN*) classpath_sep=';' ;;
*) classpath_sep=':' ;;
esac

is_skipped() {
    case "$1" in
    SQ207M | SG102A | SG103A | SG201A | SG202A | SG203A | \
        OBNC1M | OBNC2M | NC127A | NC219A | \
        IC222A | IC223A | IC224A | IC225A | IC226A | IC227A | IC228A | \
        IC233A | IC234A | IC235A | IC237A | IC401M)
        return 0
        ;;
    ??[34]0*)
        return 0
        ;;
    esac
    return 1
}

num_progs=0
test_skipped=0
compile_error=0
execute_error=0

total_all=0
total_pass=0
total_fail=0
total_deleted=0
total_inspect=0
total_ok=0

# Sources of the module in the execution order of the original test
# driver: *.CBL and *.SUB merged and sorted by name.
sources=$(ls ./*.CBL ./*.SUB 2>/dev/null | sed 's|^\./||' | grep -v tmp | LC_ALL=C sort)

# Names of the programs whose compilation failed, separated and
# surrounded by spaces.
compile_failed=""

# Compile all given sources with a single cobj invocation.  If the batch
# fails, cobj stops at the first faulty source, so fall back to
# compiling one by one to find out which sources are broken.
# $1: extra compiler flags (may be empty), $2...: source files
batch_compile() {
    flags="$1"
    shift
    [ $# -eq 0 ] && return 0
    echo "$compile $flags $*"
    if $compile $flags "$@"; then
        return 0
    fi
    echo "batch compilation failed - compiling one by one"
    for source in "$@"; do
        if ! $compile $flags "$source"; then
            failed=$(basename "$source" | sed 's/\.CBL$//; s/\.SUB$//')
            compile_failed="$compile_failed $failed "
        fi
    done
}

# Build the list of the sources to compile: the library subroutines
# first, then every non-skipped program.  SM206A.CBL is excluded here
# because it needs the -fdebugging-line option.
copy_flag=""
set --
for lib_source in lib/*.CBL; do
    [ -f "$lib_source" ] && set -- "$@" "$lib_source"
done
for source in $sources; do
    exe=$(echo "$source" | sed 's/\.CBL$//; s/\.SUB$//')
    is_skipped "$exe" && continue
    case "$exe" in
    SM206A) continue ;;
    SM*) copy_flag="-I ../copy" ;;
    esac
    set -- "$@" "$source"
done

batch_compile "$copy_flag" "$@"

if [ -f SM206A.CBL ] && ! is_skipped SM206A; then
    batch_compile "-fdebugging-line -I ../copy" SM206A.CBL
fi

exec 3>report.txt
echo "Filename    total pass fail deleted inspect" >&3
echo "--------    ----- ---- ---- ------- -------" >&3

for source in $sources; do
    exe=$(echo "$source" | sed 's/\.CBL$//; s/\.SUB$//')

    if is_skipped "$exe"; then
        test_skipped=$((test_skipped + 1))
        printf '%-12s  ----- test skipped -----\n' "$source" >&3
        continue
    fi

    num_progs=$((num_progs + 1))

    case " $compile_failed " in
    *" $exe "*)
        compile_error=$((compile_error + 1))
        printf '%-12s  ===== compile error =====\n' "$source" >&3
        continue
        ;;
    esac

    # The file I-O tests leave their work files XXXXX* behind; remove
    # them before running the next program.
    case "$source" in
    *.CBL)
        rm -rf XXXXX*
        if [ -n "${DB_HOME:-}" ]; then
            rm -rf "$DB_HOME"/XXXXX*
        fi
        ;;
    esac

    if [ -f "$exe.DAT" ]; then
        echo "java -cp \"\$CLASSPATH$classpath_sep./*\" $exe < $exe.DAT"
        java -cp "$CLASSPATH$classpath_sep./*" "$exe" <"$exe.DAT" >"$exe.out"
        exec_result=$?
    else
        echo "java -cp \"\$CLASSPATH$classpath_sep./*\" $exe"
        java -cp "$CLASSPATH$classpath_sep./*" "$exe" >"$exe.out"
        exec_result=$?
    fi

    if [ "$exec_result" -ne 0 ]; then
        execute_error=$((execute_error + 1))
        printf '%-12s  ***** execute error *****\n' "$source" >&3
        continue
    fi

    if [ -f report.log ]; then
        set -- $(awk '
            { sub(/\r$/, "") }
            $1 ~ /^[0-9]+$/ && $2 == "OF" && $4 == "TESTS" && $5 == "WERE" {
                total += $3; pass += $1
            }
            $1 ~ /^[0-9NO]+$/ && $2 == "TEST(S)" {
                n = ($1 == "NO") ? 0 : $1 + 0
                if ($3 == "FAILED") fail += n
                else if ($3 == "DELETED") deleted += n
                else if ($3 == "REQUIRE") inspect += n
            }
            END { printf "%d %d %d %d %d", total, pass, fail, deleted, inspect }
        ' report.log)
    else
        set -- 0 0 0 0 0
    fi
    prog_total=$1
    prog_pass=$2
    prog_fail=$3
    prog_deleted=$4
    prog_inspect=$5

    if [ "$prog_fail" -eq 0 ]; then
        ok="OK"
        total_ok=$((total_ok + 1))
    else
        ok=""
    fi
    printf '%-12s%5s %4s %4s %7s %7s %s\n' "$source" \
        "$prog_total" "$prog_pass" "$prog_fail" \
        "$prog_deleted" "$prog_inspect" "$ok" >&3

    total_all=$((total_all + prog_total))
    total_pass=$((total_pass + prog_pass))
    total_fail=$((total_fail + prog_fail))
    total_deleted=$((total_deleted + prog_deleted))
    total_inspect=$((total_inspect + prog_inspect))

    [ -f report.log ] && mv report.log "$exe.log"
    [ -s "$exe.out" ] || rm -f "$exe.out"
done

echo "--------    ----- ---- ---- ------- -------" >&3
printf 'Total       %5s %4s %4s %7s %7s\n' \
    "$total_all" "$total_pass" "$total_fail" \
    "$total_deleted" "$total_inspect" >&3
awk -v all="$total_all" -v pass="$total_pass" -v fail="$total_fail" \
    -v deleted="$total_deleted" -v inspect="$total_inspect" \
    -v progs="$num_progs" -v ok="$total_ok" \
    -v cerr="$compile_error" -v xerr="$execute_error" '
    BEGIN {
        if (all) {
            printf "%%           100.0 %4.1f %4.1f    %4.1f    %4.1f\n\n",
                100 * pass / all, 100 * fail / all,
                100 * deleted / all, 100 * inspect / all
        }
        printf "Number of programs:    %2s\n", progs
        printf "Successfully executed: %2s (%5.2f%%)\n", ok, progs ? 100 * ok / progs : 0
        printf "Compile error:         %2s (%5.2f%%)\n", cerr, progs ? 100 * cerr / progs : 0
        printf "Execute error:         %2s (%5.2f%%)\n", xerr, progs ? 100 * xerr / progs : 0
    }' >&3
exec 3>&-
