// Purge legacy durable ordinary-analysis data that the current product no longer reads or writes.
//
// This script is deliberately a dry run unless CHESSTORY_PURGE_EXECUTE=1 and
// CHESSTORY_PURGE_CONFIRM matches the database-bound confirmation token that
// the dry run prints. The database selected by mongosh must also exactly match
// CHESSTORY_PURGE_TARGET_DB.

(() => {
  const collections = Object.freeze([
    'analysis_import_account',
    'analysis_import_history',
    'analysis_requester',
    'analysis2',
  ]);
  const environment = process.env;
  const targetDb = environment.CHESSTORY_PURGE_TARGET_DB;
  const executeSetting = environment.CHESSTORY_PURGE_EXECUTE || '0';
  const connectedDb = db.getName();

  if (!targetDb) {
    throw new Error('CHESSTORY_PURGE_TARGET_DB is required and must name the database selected by mongosh.');
  }
  if (targetDb !== connectedDb) {
    throw new Error(
      `Refusing to continue: target database "${targetDb}" does not match connected database "${connectedDb}".`,
    );
  }
  if (['admin', 'config', 'local'].includes(connectedDb)) {
    throw new Error(`Refusing to purge MongoDB system database "${connectedDb}".`);
  }
  if (!['0', '1'].includes(executeSetting)) {
    throw new Error('CHESSTORY_PURGE_EXECUTE must be either 0 (dry run) or 1 (execute).');
  }

  const confirmation = `DELETE_DURABLE_ORDINARY_ANALYSIS_DATA_FROM_${connectedDb}`;
  const countDocuments = collection => db.getCollection(collection).countDocuments({}, { maxTimeMS: 30000 });
  const before = Object.fromEntries(collections.map(collection => [collection, countDocuments(collection)]));

  print(`Database: ${connectedDb}`);
  print(`Collections: ${collections.join(', ')}`);
  print(`Documents before purge: ${JSON.stringify(before)}`);

  if (executeSetting === '0') {
    print('DRY RUN: no documents were deleted.');
    print('To execute, set CHESSTORY_PURGE_EXECUTE=1 and:');
    print(`CHESSTORY_PURGE_CONFIRM=${confirmation}`);
    return;
  }

  if (environment.CHESSTORY_PURGE_CONFIRM !== confirmation) {
    throw new Error(`Refusing to delete: CHESSTORY_PURGE_CONFIRM must exactly equal "${confirmation}".`);
  }

  const deleted = Object.fromEntries(
    collections.map(collection => {
      const result = db.getCollection(collection).deleteMany(
        {},
        {
          comment: 'chesstory-purge-durable-ordinary-analysis-data',
          writeConcern: { w: 'majority', wtimeout: 30000 },
        },
      );
      return [collection, result.deletedCount];
    }),
  );
  const after = Object.fromEntries(collections.map(collection => [collection, countDocuments(collection)]));

  print(`Documents deleted: ${JSON.stringify(deleted)}`);
  print(`Documents after purge: ${JSON.stringify(after)}`);

  const remaining = Object.values(after).reduce((total, count) => total + count, 0);
  if (remaining !== 0) {
    throw new Error(
      'Purge completed but documents remain. Stop any remaining writers, then run this idempotent script again.',
    );
  }

  print('Purge complete. All four legacy ordinary-analysis collections are empty.');
})();
