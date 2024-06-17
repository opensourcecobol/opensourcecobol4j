cd app/src/main/java
javadoc $(find . -name '*.java') -Xdoclint:missing 2> javadoc.log
cat javadoc.log
test "$(grep 'warning: no comment' javadoc.log)" = ""
