# Connection settings for the cluster win/start-test-postgresql.ps1 brings up.
# DB_PORT is not 5432 on purpose, and DB_NAME / DB_PORT / DB_USER have to stay
# in step with that script's defaults; see its comment header for both reasons.
# Database Settings ----------
DB_NAME=testdb
DB_HOST=localhost
DB_PORT=55432
DB_USER=main_user
DB_PASSWORD=password
# ----------------------------

TEMP_FILE=$(mktemp)

cat $1 |
sed -e "s/<|DB_NAME|>/${DB_NAME}/g" |
sed -e "s/<|DB_HOST|>/${DB_HOST}/g" |
sed -e "s/<|DB_PORT|>/${DB_PORT}/g" |
sed -e "s/<|DB_USER|>/${DB_USER}/g" |
sed -e "s/<|DB_PASSWORD|>/${DB_PASSWORD}/g" > ${TEMP_FILE}

mv ${TEMP_FILE} $1
