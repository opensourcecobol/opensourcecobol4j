#!/bin/sh
#
# Copyright (C) 2002-2009 Keisuke Nishida
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
# the Free Software Foundation, 51 Franklin Street, Fifth Floor
# Boston, MA 02110-1301 USA

# Print a summary table of the NIST COBOL85 test results.
#
# Usage: sh summary.sh MODULE...

echo "------ Directory Information -------   --- Total Tests Information ---"
echo "Module Programs Executed Error Crash   Pass Fail Deleted Inspect Total"
echo "------ -------- -------- ----- -----  ----- ---- ------- ------- -----"

total_progs=0
total_executed=0
total_error=0
total_crash=0
total_pass=0
total_fail=0
total_del=0
total_insp=0
total_total=0

for module in "$@"; do
    if [ ! -f "$module/report.txt" ]; then
        echo "cannot open $module/report.txt" >&2
        exit 1
    fi
    set -- $(awk '
        { sub(/\r$/, "") }
        $1 == "Total" && $2 ~ /^[0-9]+$/ {
            test = $2; pass = $3; fail = $4; deleted = $5; inspect = $6
        }
        /^Number of programs:/ { progs = $4 }
        /^Successfully executed:/ { executed = $3 }
        /^Compile error:/ { error = $3 }
        /^Execute error:/ { crash = $3 }
        END {
            printf "%d %d %d %d %d %d %d %d %d",
                progs, executed, error, crash,
                pass, fail, deleted, inspect, test
        }
    ' "$module/report.txt")
    printf '%-6s %8d %8d %5d %5d   %4d %4d %7d %7d %5d\n' \
        "$module" "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9"
    total_progs=$((total_progs + $1))
    total_executed=$((total_executed + $2))
    total_error=$((total_error + $3))
    total_crash=$((total_crash + $4))
    total_pass=$((total_pass + $5))
    total_fail=$((total_fail + $6))
    total_del=$((total_del + $7))
    total_insp=$((total_insp + $8))
    total_total=$((total_total + $9))
done

echo "------ -------- -------- ----- -----  ----- ---- ------- ------- -----"
printf 'Total  %8d %8d %5d %5d  %5d %4d %7d %7d %5d\n' \
    "$total_progs" "$total_executed" "$total_error" "$total_crash" \
    "$total_pass" "$total_fail" "$total_del" "$total_insp" "$total_total"
