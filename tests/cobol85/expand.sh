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
# the Free Software Foundation, 51 Franklin Street, Fifth Floor
# Boston, MA 02110-1301 USA

# Expand the NIST COBOL85 test suite archive (newcob.val) into the
# per-module test directories.
#
# Usage: sh expand.sh newcob.val

if [ $# -lt 1 ]; then
    echo "usage: $0 newcob.val" >&2
    exit 1
fi
if [ ! -f "$1" ]; then
    echo "cannot open $1" >&2
    exit 1
fi

awk '
function trim_word(s) {
    sub(/ .*/, "", s)
    return s
}
{
    line = $0
    sub(/\r$/, "", line)
}
# Copy the lines of the current section into its output file.
out != "" || skipping {
    if (line ~ /^      \*END/) {
        if (out != "") {
            close(out)
        }
        out = ""
        skipping = 0
        next
    }
    if (skipping) {
        next
    }
    if (type == "DATA*" && length($0) + 1 >= 80) {
        # Long data lines form fixed-length records: write them without
        # a line terminator.
        body = line
        sub(/REC-FILLER/, "FILLER    ", body)
        printf "%s", body > out
    } else {
        body = $0
        sub(/REC-FILLER/, "FILLER    ", body)
        print body > out
    }
    next
}
line ~ /^      \*HEADER,/ {
    n = split(substr(line, index(line, ",") + 1), field, ",")
    type = field[1]
    prog = trim_word(field[2])
    subt = (n >= 3) ? trim_word(field[3]) : ""
    subr = (n >= 4) ? trim_word(field[4]) : ""
    module = substr(prog, 1, 2)
    name = ""
    if (subt != "") {
        if (subt == "SUBPRG") {
            name = subr ".SUB"
        } else if (subt == "SUBRTN") {
            name = "lib/" subr ".CBL"
            system("mkdir -p " module "/lib")
        }
    } else if (type == "COBOL") {
        name = prog ".CBL"
    } else if (type == "DATA*") {
        name = prog ".DAT"
    } else if (type == "CLBRY") {
        if (prog == "ALTL1") {
            module = "copyalt"
            name = "ALTLB"
        } else {
            module = "copy"
            name = prog
        }
    }
    if (name != "") {
        system("mkdir -p " module)
        out = module "/" name
        printf "" > out
    } else {
        skipping = 1
    }
}
' "$1"
