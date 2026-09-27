import { initMiniBoards, toggleBoxInit } from 'lib/view';
import { prefersLightThemeQuery } from 'lib/device';
import { scrollToInnerSelector, requestIdleCallback } from 'lib';
import { dispatchChessgroundResize } from 'lib/chessgroundResize';
import { addDomHandlers } from './domHandlers';
import { updateTimeAgo, renderTimeAgo } from './renderTimeAgo';
import { pubsub } from 'lib/pubsub';
import { addExceptionListeners } from './unhandledError';
import { clearLegacyClientStorage } from 'lib/legacyStorageCleanup';
import { applyAppearance } from './appearance';
import { initLanguage } from './language';

const retireDormantServiceWorkers = () => {
  if (!('serviceWorker' in navigator)) return;

  void navigator.serviceWorker
    .getRegistrations()
    .then(registrations => Promise.all(registrations.map(registration => registration.unregister())));
};

export function boot() {
  initLanguage();
  void clearLegacyClientStorage();
  if (!site.blindMode) applyAppearance(document.documentElement.dataset.appearance || document.body.dataset.theme || 'dark');
  addExceptionListeners();
  const setBlind = location.hash === '#blind';
  const showDebug = location.hash.startsWith('#debug');

  requestAnimationFrame(() => {
    initMiniBoards();
    pubsub.on('content-loaded', initMiniBoards);
    updateTimeAgo(1000);
    pubsub.on('content-loaded', renderTimeAgo);
    pubsub.on('content-loaded', toggleBoxInit);
  });
  requestIdleCallback(() => {
    retireDormantServiceWorkers();

    $('.subnav__inner').each(function (this: HTMLElement) {
      scrollToInnerSelector(this, '.active', true);
    });

    addDomHandlers();

    toggleBoxInit();

    window.addEventListener('resize', dispatchChessgroundResize);

    if (setBlind && !site.blindMode) setTimeout(() => $('#blind-mode button').trigger('click'), 1500);

    if (showDebug) site.asset.loadEsm('bits.diagnosticDialog');

    console.info('Chesstory is open source.');

    const mql = prefersLightThemeQuery();
    if (typeof mql.addEventListener === 'function')
      mql.addEventListener('change', () => {
        if (document.body.dataset.theme === 'system') applyAppearance('system');
      });
  }, 800);
}
