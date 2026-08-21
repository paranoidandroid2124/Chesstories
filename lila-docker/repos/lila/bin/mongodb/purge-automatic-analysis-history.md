# Durable ordinary-analysis data purge

Use this one-time operation after deploying the application change that stops
automatic analysis and imported-game history writes and removes the dormant
server-analysis repositories. It deletes documents from exactly these four
MongoDB collections:

- `analysis_import_account`
- `analysis_import_history`
- `analysis_requester`
- `analysis2`

It does not touch the separate `study` or `study_chapter_flat` collections that
hold explicitly saved Study content, nor does it touch user accounts or
authentication sessions. `analysis2` contains detached results from a retired
server-analysis subsystem; the current Study create, display, edit, and export
paths do not read it. The operation uses `deleteMany`, so collection indexes
remain in place, and rerunning it is safe.

Before executing, verify that the deployed application version no longer wires
these repositories and that no out-of-repository worker still reads or writes
any of the four collections.

## 1. Run the required dry run

Choose the database explicitly in both the `mongosh` database argument and
`CHESSTORY_PURGE_TARGET_DB`. The script aborts if they differ. For the local
Docker environment:

```sh
docker compose exec \
  -e CHESSTORY_PURGE_TARGET_DB=lichess \
  mongodb mongosh --quiet lichess \
  /lila/bin/mongodb/purge-automatic-analysis-history.js
```

The dry run reports document counts without changing data and prints the exact,
database-bound confirmation token required for execution. Review the selected
database and all four counts before continuing.

For a non-Docker environment, pass a MongoDB URI that explicitly selects the
intended database and set the same database name in the environment:

```sh
CHESSTORY_PURGE_TARGET_DB=your_database \
  mongosh --quiet 'mongodb://host/your_database' \
  bin/mongodb/purge-automatic-analysis-history.js
```

## 2. Execute the purge

Stop or deploy away all writers to these four collections first. Then add the
execution flag and copy the confirmation token from the dry-run output. For the
local `lichess` database, the command is:

```sh
docker compose exec \
  -e CHESSTORY_PURGE_TARGET_DB=lichess \
  -e CHESSTORY_PURGE_EXECUTE=1 \
  -e CHESSTORY_PURGE_CONFIRM=DELETE_DURABLE_ORDINARY_ANALYSIS_DATA_FROM_lichess \
  mongodb mongosh --quiet lichess \
  /lila/bin/mongodb/purge-automatic-analysis-history.js
```

The script uses majority-acknowledged deletes and then verifies that all four
collections contain zero documents. It exits with an error if verification finds
new or remaining documents; stop the remaining writer and rerun the same command.

## 3. Verify independently

Rerun the dry-run command from step 1. All four reported counts must be zero. A
syntax-only check that does not connect to MongoDB is also available:

```sh
node --check bin/mongodb/purge-automatic-analysis-history.js
```
