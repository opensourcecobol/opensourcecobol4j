#!/usr/bin/env python3
"""Checks the responses collected from concurrent requests to /calc.

Usage: check-responses.py RESPONSES-FILE [EXPECTED-COUNT]

RESPONSES-FILE holds one response line per request, in arbitrary order
(the requests were sent concurrently). A response line looks like

  num=000042 square=000000001764 upper=BANANA     a=03 calls=000007 subcalls=000007 thread=http-nio-18080-exec-3

where every value before "thread=" was produced by the COBOL program
"calc" (and its CALLed subprogram "subcount") running on the Tomcat
request thread named after "thread=".

The checks below prove, piece by piece, that the COBOL programs behaved
correctly while many request threads ran them at the same time. The
script prints one line per violation and exits non-zero if there was any.
"""
import collections
import re
import sys

pattern = re.compile(
    r"num=(\d+) square=(\d+) upper=(\S+)\s+a=(\d+) calls=(\d+) subcalls=(\d+) thread=(\S+)"
)

bad = 0  # number of violations found
total = 0  # number of response lines seen
# thread name -> list of the "calls" counter values that this thread reported
per_thread = collections.defaultdict(list)

for line in open(sys.argv[1], encoding="ascii", errors="replace"):
    line = line.strip()
    if not line:
        continue
    total += 1

    # Check 1: every line has the expected shape. A truncated or garbled
    # line would mean the response of one request was mixed with another.
    m = pattern.search(line)
    if not m:
        bad += 1
        print("unexpected response:", line)
        continue
    num, square, upper, a, calls, subcalls, thread = m.groups()

    # Remember which thread produced which "calls" value for check 4.
    per_thread[thread].append(int(calls))

    # Check 2: the response belongs to its own request. Every request asks
    # for a different "num", and the response must carry the square of
    # exactly that number. A wrong square would mean two threads swapped
    # their inputs or outputs.
    #
    # Check 3: the computed values are right. "upper" and "a" come from
    # FUNCTION UPPER-CASE and INSPECT TALLYING over the request text
    # ("banana" in every request, so always BANANA and 3). "calls" must
    # equal "subcalls": the counter of the main program and the counter of
    # the CALLed subprogram live in separate WORKING-STORAGE, but both are
    # incremented once per request on the same thread, so they can only
    # diverge if another thread's call leaked into one of them.
    if int(square) != int(num) ** 2 or upper != "BANANA" or a != "03" or calls != subcalls:
        bad += 1
        print("wrong response:", line)

# Check 4: the "calls" counter of each thread forms a contiguous sequence
# (for example 3,4,5,...,18 in any order). The counter lives in the
# WORKING-STORAGE of the thread's own program instance and is incremented
# by one on every request the thread serves, so:
#   - a gap would mean another thread stole one of the increments,
#   - a duplicate would mean two threads shared the same instance.
# The sequence does not have to start at 1: requests sent before the
# measurement (the readiness probe) may have consumed the first values.
for thread, counters in per_thread.items():
    counters.sort()
    if counters != list(range(counters[0], counters[0] + len(counters))):
        bad += 1
        print("counter of", thread, "is not contiguous:", counters)

# Check 5: no response was lost. curl -sf drops the output of a failed
# request silently, so compare against the number of requests sent.
expected = int(sys.argv[2]) if len(sys.argv) > 2 else None
if expected is not None and total != expected:
    bad += 1
    print("expected", expected, "responses but got", total)

print("responses", total, "threads", len(per_thread), "bad", bad)
sys.exit(1 if bad or total == 0 else 0)
