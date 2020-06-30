mv newcob.tmp newcob.tmp2
sed -e 's/^061200.*IF1194.*/rogerw     COMPUTE WS-NUM = FUNCTION MAX (4 0 5 3 7)\./
s/^050300.*IF1204.*/rogerw     COMPUTE WS-NUM = FUNCTION MEAN (4 0 5 3 7)\./
s/^050000.*IF1214.*/rogerw     COMPUTE WS-NUM = FUNCTION MEDIAN (4 0 5 3 7)\./
s/^052200.*IF1224.*/rogerw     COMPUTE WS-NUM = FUNCTION MIDRANGE (4 0 5 3 7)\./
s/^061500.*IF1234.*/rogerw     COMPUTE WS-NUM = FUNCTION MIN (4 0 5 3 7)\./
s/^054900.*IF1284.*/rogerw     COMPUTE WS-INT = FUNCTION ORD-MAX (4 0 5 3 7)\./
s/^055200.*IF1294.*/rogerw     COMPUTE WS-INT = FUNCTION ORD-MIN (4 0 5 3 7)\./
s/^049800.*IF1324.*/rogerw     COMPUTE WS-NUM = FUNCTION RANGE (4 0 5 3 7)\./
s/^053100.*IF1374.*/rogerw     COMPUTE WS-NUM = FUNCTION STANDARD-DEVIATION (4 0 5 3 7)\./
s/^049800.*IF1384.*/rogerw     COMPUTE WS-NUM = FUNCTION SUM (4 0 5 3 7)\./
s/^012250.*IX1104.*/rogerw 01  STATUS-TEST-10          PIC 9 VALUE ZERO\./
s/^000200.*K2SEA4.*/rogerw     GO TO PARA-4\./
s/^052500.*IF1414.*/rogerw     COMPUTE WS-NUM = FUNCTION VARIANCE (4 0 5 3 7)\./' <newcob.tmp2 >newcob.tmp
rm -f newcob.tmp2
#
# Following no longer necessary
# s/^065100.*IF1194.*/rogerw     COMPUTE WS-NUM = FUNCTION MAX((A * B) ((C + 1) \/ 2) (3 + 4))\./
# s/^065400.*IF1234.*/rogerw     COMPUTE WS-NUM = FUNCTION MIN((A * B) ((3 + 1) \/ 2) (3 + 4))\./
