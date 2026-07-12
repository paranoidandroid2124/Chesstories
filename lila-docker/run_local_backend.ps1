$env:CHESSTORY_DOMAIN="localhost:9663"
$env:CHESSTORY_URL="http://localhost:9663"
$env:MOCK_EMAIL="true"
$env:ENABLE_MONITORING="false"
$env:ENABLE_RATE_LIMITING="false"
$env:LILA_REDIS_URI="redis://localhost:6379"
$env:LILA_DB_DUAL_URL="mongodb://127.0.0.1:27017?directConnection=true"
$env:HTTP_PORT="9663"

# Important Java options from the local app runner
$env:JAVA_OPTS="-Dreactivemongo.api.bson.document.strict=false -Dplay.http.parser.allowEmptyFiles=false"

Write-Host "Starting Chesstory backend locally (sbt run)..."
cd repos/lila

# Use cmd /c specifically to ensure sbt.bat is picked up if on Windows path
# Or just sbt if it is an exe.
# sbt usually installs sbt.bat on Windows.
sbt run
