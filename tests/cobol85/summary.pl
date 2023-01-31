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

my $total_progs = 0;
my $total_executed = 0;
my $total_error = 0;
my $total_crash = 0;
my $total_pass = 0;
my $total_fail = 0;
my $total_del = 0;
my $total_insp = 0;
my $total_total = 0;

print ("------ Directory Information -------   --- Total Tests Information ---\n");
print ("Module Programs Executed Error Crash   Pass Fail Deleted Inspect Total\n");
print ("------ -------- -------- ----- -----  ----- ---- ------- ------- -----\n");

while ($module = shift) {
  open(IN, "$module/report.txt") or die;
  while (<IN>) {
    if (/^Total *(\d+) *(\d+) *(\d+) *(\d+) *(\d+)/) {
      ($test, $pass, $fail, $delete, $inspect) = ($1, $2, $3, $4, $5);
    } elsif (/^Number of programs: *(\d+)/) {
      $progs = $1;
    } elsif (/^Successfully executed: *(\d+)/) {
      $executed = $1;
    } elsif (/^Compile error: *(\d+)/) {
      $error = $1;
    } elsif (/^Execute error: *(\d+)/) {
      $crash = $1;
    }
  }
  printf "%-6s %8d %8d %5d %5d   %4d %4d %7d %7d %5d\n",
	  $module, $progs, $executed, $error, $crash,
	  $pass, $fail, $delete, $inspect, $test;
  $total_progs += $progs;
  $total_executed += $executed;
  $total_error += $error;
  $total_crash += $crash;
  $total_pass += $pass;
  $total_fail += $fail;
  $total_del += $delete;
  $total_insp += $inspect;
  $total_total += $test;
}

print ("------ -------- -------- ----- -----  ----- ---- ------- ------- -----\n");
printf "Total  %8d %8d %5d %5d  %5d %4d %7d %7d %5d\n",
	$total_progs, $total_executed, $total_error, $total_crash,
	$total_pass, $total_fail, $total_del, $total_insp, $total_total;
