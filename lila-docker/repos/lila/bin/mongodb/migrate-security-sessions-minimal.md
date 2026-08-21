# Minimal security-session migration

Use this operation after deploying the session-store change that writes only
`_id`, `user`, and `date`. It changes only the `security` collection:

- deletes legacy inactive documents selected by exactly `{ up: false }`;
- removes `ip`, `ua`, `api`, `proxy`, `pwned`, `fp`, and the redundant `up`
  field from every remaining active session;
- preserves `_id`, `user`, and `date`;
- creates or updates a single-field `date` TTL index to 2,678,400 seconds
  (31 days);
- optionally drops only the exact legacy `ip_1` / `{ ip: 1 }` and `fp_1` /
  `{ fp: 1 }` index matches.

The script never explicitly deletes an active session. MongoDB's TTL monitor
will asynchronously delete sessions whose `date` is at least 31 days old,
including any currently active legacy session. The dry run reports how many
active sessions are already TTL-eligible so this effect can be reviewed first.

## 1. Run the required dry run

Select the database explicitly in both the `mongosh` database argument and
`CHESSTORY_SESSION_MIGRATION_TARGET_DB`. The script aborts if they differ. From
the local Docker checkout:

```sh
docker compose exec \
  -e CHESSTORY_SESSION_MIGRATION_TARGET_DB=lichess \
  mongodb mongosh --quiet lichess \
  /lila/bin/mongodb/migrate-security-sessions-minimal.js
```

Review the document counts, the number of active sessions already older than 31
days, and the TTL-index plan. The dry run prints the exact database-bound
confirmation token needed for execution.

For a non-Docker environment, use a URI that explicitly selects the intended
database and bind the same name in the environment:

```sh
CHESSTORY_SESSION_MIGRATION_TARGET_DB=your_database \
  mongosh --quiet 'mongodb://host/your_database' \
  bin/mongodb/migrate-security-sessions-minimal.js
```

## 2. Execute the migration

First deploy away legacy writers and take any backup required by the deployment
policy. Then add the execution flag and copy the confirmation token printed by
the dry run:

```sh
docker compose exec \
  -e CHESSTORY_SESSION_MIGRATION_TARGET_DB=lichess \
  -e CHESSTORY_SESSION_MIGRATION_EXECUTE=1 \
  -e CHESSTORY_SESSION_MIGRATION_CONFIRM=MIGRATE_SECURITY_SESSIONS_IN_lichess \
  mongodb mongosh --quiet lichess \
  /lila/bin/mongodb/migrate-security-sessions-minimal.js
```

The data operations use majority write concern. The migration is idempotent, so
it can be rerun if a legacy writer races with the first execution.

### Optional exact legacy-index removal

By default, the migration leaves obsolete indexes in place. To remove them, add:

```sh
-e CHESSTORY_SESSION_DROP_OBSOLETE_INDEXES=1
```

The script drops an index only when both its name and key are the known legacy
pair: `ip_1` with `{ ip: 1 }`, or `fp_1` with `{ fp: 1 }`. Custom names, compound
indexes, and name/key mismatches are left untouched; a conflicting known name
causes execution to abort before any writes.

## 3. Verify independently

Rerun the dry-run command. It must report zero inactive documents, zero active
documents requiring minimization, and an already-correct 31-day TTL index. TTL
deletion itself is asynchronous, so old session documents may disappear shortly
after index creation rather than during this script.

Check JavaScript syntax without connecting to MongoDB:

```sh
node --check bin/mongodb/migrate-security-sessions-minimal.js
```
