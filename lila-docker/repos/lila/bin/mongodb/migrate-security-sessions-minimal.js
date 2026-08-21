// Minimize existing session records and apply a bounded session lifetime.
//
// This script is deliberately a dry run unless CHESSTORY_SESSION_MIGRATION_EXECUTE=1
// and CHESSTORY_SESSION_MIGRATION_CONFIRM matches the database-bound token that
// the dry run prints. The database selected by mongosh must also exactly match
// CHESSTORY_SESSION_MIGRATION_TARGET_DB.

(() => {
  const collectionName = 'security';
  const ttlSeconds = 31 * 24 * 60 * 60;
  const legacyFields = Object.freeze(['ip', 'ua', 'api', 'proxy', 'pwned', 'fp', 'up']);
  const obsoleteIndexes = Object.freeze([
    Object.freeze({ name: 'ip_1', field: 'ip' }),
    Object.freeze({ name: 'fp_1', field: 'fp' }),
  ]);
  const environment = process.env;
  const targetDb = environment.CHESSTORY_SESSION_MIGRATION_TARGET_DB;
  const executeSetting = environment.CHESSTORY_SESSION_MIGRATION_EXECUTE || '0';
  const dropIndexSetting = environment.CHESSTORY_SESSION_DROP_OBSOLETE_INDEXES || '0';
  const connectedDb = db.getName();

  if (!targetDb) {
    throw new Error(
      'CHESSTORY_SESSION_MIGRATION_TARGET_DB is required and must name the database selected by mongosh.',
    );
  }
  if (targetDb !== connectedDb) {
    throw new Error(
      `Refusing to continue: target database "${targetDb}" does not match connected database "${connectedDb}".`,
    );
  }
  if (['admin', 'config', 'local'].includes(connectedDb)) {
    throw new Error(`Refusing to migrate MongoDB system database "${connectedDb}".`);
  }
  if (!['0', '1'].includes(executeSetting)) {
    throw new Error('CHESSTORY_SESSION_MIGRATION_EXECUTE must be either 0 (dry run) or 1 (execute).');
  }
  if (!['0', '1'].includes(dropIndexSetting)) {
    throw new Error('CHESSTORY_SESSION_DROP_OBSOLETE_INDEXES must be either 0 or 1.');
  }

  const collection = db.getCollection(collectionName);
  const collectionExists = db.getCollectionInfos({ name: collectionName }).length === 1;
  const indexes = collectionExists ? collection.getIndexes() : [];
  const confirmation = `MIGRATE_SECURITY_SESSIONS_IN_${connectedDb}`;
  const fieldExistsSelector = {
    $or: legacyFields.map(field => ({ [field]: { $exists: true } })),
  };
  const activeCleanupSelector = {
    up: { $ne: false },
    ...fieldExistsSelector,
  };
  const indexHasExactKey = (index, field) => {
    const entries = Object.entries(index.key);
    return entries.length === 1 && entries[0][0] === field && Number(entries[0][1]) === 1;
  };
  const countDocuments = selector => collection.countDocuments(selector, { maxTimeMS: 30000 });

  const dateIndexes = indexes.filter(index => indexHasExactKey(index, 'date'));
  if (dateIndexes.length > 1) {
    throw new Error('Refusing to continue: more than one single-field date index was found.');
  }
  const dateIndex = dateIndexes[0];
  const dateNameConflict = indexes.find(index => index.name === 'date_1' && index !== dateIndex);
  if (!dateIndex && dateNameConflict) {
    throw new Error('Refusing to create date_1 because that name belongs to a different index key.');
  }
  if (
    dateIndex &&
    (dateIndex.unique === true ||
      dateIndex.sparse === true ||
      dateIndex.partialFilterExpression !== undefined)
  ) {
    throw new Error(
      `Refusing to alter nonstandard date index "${dateIndex.name}"; remove its unique, sparse, or partial option manually first.`,
    );
  }

  const obsoleteIndexState = obsoleteIndexes.map(spec => {
    const sameName = indexes.find(index => index.name === spec.name);
    return {
      spec,
      exactMatch: sameName !== undefined && indexHasExactKey(sameName, spec.field),
      nameConflict: sameName !== undefined && !indexHasExactKey(sameName, spec.field),
    };
  });
  if (dropIndexSetting === '1') {
    const conflict = obsoleteIndexState.find(state => state.nameConflict);
    if (conflict) {
      throw new Error(
        `Refusing to drop index "${conflict.spec.name}" because its key is not exactly { ${conflict.spec.field}: 1 }.`,
      );
    }
  }

  const cutoff = new Date(Date.now() - ttlSeconds * 1000);
  const before = {
    total: countDocuments({}),
    inactiveToDelete: countDocuments({ up: false }),
    activeToMinimize: countDocuments(activeCleanupSelector),
    activeAlreadyTtlEligible: countDocuments({ up: { $ne: false }, date: { $lte: cutoff } }),
  };
  const ttlState = !dateIndex
    ? 'missing; will create date_1'
    : Number(dateIndex.expireAfterSeconds) === ttlSeconds
      ? `${dateIndex.name} already expires after ${ttlSeconds} seconds`
      : `${dateIndex.name} will be changed to expire after ${ttlSeconds} seconds`;

  print(`Database: ${connectedDb}`);
  print(`Collection: ${collectionName}`);
  print(`Session document plan: ${JSON.stringify(before)}`);
  print(`TTL index: ${ttlState}`);
  obsoleteIndexState.forEach(state => {
    const status = state.exactMatch
      ? dropIndexSetting === '1'
        ? 'exact legacy match; will drop'
        : 'exact legacy match; retained unless CHESSTORY_SESSION_DROP_OBSOLETE_INDEXES=1'
      : state.nameConflict
        ? 'same name has a different key; will not drop'
        : 'exact legacy index not present';
    print(`Obsolete index ${state.spec.name}: ${status}`);
  });
  print(
    `TTL warning: ${before.activeAlreadyTtlEligible} active session(s) are already at least 31 days old and become eligible for MongoDB TTL deletion.`,
  );

  if (executeSetting === '0') {
    print('DRY RUN: no documents or indexes were changed.');
    print('To execute, set CHESSTORY_SESSION_MIGRATION_EXECUTE=1 and:');
    print(`CHESSTORY_SESSION_MIGRATION_CONFIRM=${confirmation}`);
    return;
  }

  if (environment.CHESSTORY_SESSION_MIGRATION_CONFIRM !== confirmation) {
    throw new Error(
      `Refusing to migrate: CHESSTORY_SESSION_MIGRATION_CONFIRM must exactly equal "${confirmation}".`,
    );
  }

  const inactiveResult = collection.deleteMany(
    { up: false },
    {
      comment: 'chesstory-delete-inactive-legacy-sessions',
      writeConcern: { w: 'majority', wtimeout: 30000 },
    },
  );
  const minimizeResult = collection.updateMany(
    activeCleanupSelector,
    { $unset: Object.fromEntries(legacyFields.map(field => [field, ''])) },
    {
      comment: 'chesstory-minimize-active-session-fields',
      writeConcern: { w: 'majority', wtimeout: 30000 },
    },
  );

  if (!dateIndex) {
    collection.createIndex({ date: 1 }, { name: 'date_1', expireAfterSeconds: ttlSeconds });
  } else if (Number(dateIndex.expireAfterSeconds) !== ttlSeconds) {
    const collModResult = db.runCommand({
      collMod: collectionName,
      index: { name: dateIndex.name, expireAfterSeconds: ttlSeconds },
    });
    if (collModResult.ok !== 1) {
      throw new Error(`Failed to configure the TTL index: ${JSON.stringify(collModResult)}`);
    }
  }

  const droppedIndexes = [];
  if (dropIndexSetting === '1') {
    obsoleteIndexState
      .filter(state => state.exactMatch)
      .forEach(state => {
        collection.dropIndex(state.spec.name);
        droppedIndexes.push(state.spec.name);
      });
  }

  const remainingInactive = countDocuments({ up: false });
  const remainingLegacyFields = countDocuments(fieldExistsSelector);
  const resultingIndexes = collection.getIndexes();
  const resultingDateIndexes = resultingIndexes.filter(index => indexHasExactKey(index, 'date'));
  const resultingDateIndex = resultingDateIndexes[0];
  const validTtl =
    resultingDateIndexes.length === 1 &&
    Number(resultingDateIndex.expireAfterSeconds) === ttlSeconds &&
    resultingDateIndex.unique !== true &&
    resultingDateIndex.sparse !== true &&
    resultingDateIndex.partialFilterExpression === undefined;
  const obsoleteStillPresent =
    dropIndexSetting === '1'
      ? obsoleteIndexes.filter(spec =>
          resultingIndexes.some(index => index.name === spec.name && indexHasExactKey(index, spec.field)),
        )
      : [];

  print(`Inactive sessions deleted: ${inactiveResult.deletedCount}`);
  print(`Active sessions minimized: ${minimizeResult.modifiedCount}`);
  print(`Obsolete indexes dropped: ${droppedIndexes.join(', ') || 'none'}`);
  print(`Verification: ${JSON.stringify({ remainingInactive, remainingLegacyFields, validTtl })}`);

  if (
    remainingInactive !== 0 ||
    remainingLegacyFields !== 0 ||
    !validTtl ||
    obsoleteStillPresent.length !== 0
  ) {
    throw new Error(
      'Migration verification failed. Stop any legacy session writer, inspect the reported index state, and rerun this idempotent script.',
    );
  }

  print('Session minimization complete. Only _id, user, date, and any unrelated fields were preserved.');
})();
