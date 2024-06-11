cd app/src/main/java
javadoc $(find . -name '*.java') -Xdoclint:missing 2> javadoc.log
cat javadoc.log
test "$(grep '[1-9][0-9]* warning' javadoc.log)" = ""
