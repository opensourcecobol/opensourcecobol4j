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

open (IN, shift) or die;

while (<IN>) {
  s/\x0d\x0a|\x0d|\x0a//g;
  if (/^      \*HEADER,([^,]*),([^, ]*)(,([^,]*),([^, ]*))?/) {
    my ($type, $prog, $subt, $subr) = ($1, $2, $4, $5);
    my $module = substr($prog, 0, 2);
    my $name = '';
    if ($subt) {
      if ($subt eq "SUBPRG") {
	$name = "$subr.SUB";
      } elsif ($subt eq "SUBRTN") {
	$name = "lib/$subr.CBL";
	mkdir "$module/lib",0755 unless (-e "$module/lib");
      }
    } elsif ($type eq "COBOL") {
      $name = "$prog.CBL";
    } elsif ($type eq "DATA*") {
      $name = "$prog.DAT";
    } elsif ($type eq "CLBRY") {
      if ($prog eq "ALTL1") {
        $module = "copyalt";
        $name = "ALTLB";
      } else {
        $module = "copy";
        $name = "$prog";
      }
    }
    if ($name) {
      mkdir $module,0755 unless (-e $module);
      open (OUT, "> $module/$name") or die;
      while (<IN>) {
	last if /^      \*END/;
	s/\x0d\x0a|\x0d|\x0a//g if ($type eq "DATA*" and length >= 80);
	s/REC-FILLER/FILLER    /;
	print OUT;
      }
    } else {
      while (<IN>) {
	last if /^      \*END/;
      }
    }
  }
}
