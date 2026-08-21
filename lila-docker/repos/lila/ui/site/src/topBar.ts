import { isBrandV3ShellEnabled, readShellSelectors } from 'lib/shell';

import { isTouchDevice } from 'lib/device';

export default function () {
  if (!isBrandV3ShellEnabled()) return;
  const { headerId, navId, navToggleId } = readShellSelectors();
  const top = document.getElementById(headerId);
  if (!top) return;
  const navToggle = document.getElementById(navToggleId) as HTMLInputElement | null;
  const nav = document.getElementById(navId);
  const navButton = top.querySelector<HTMLButtonElement>('.js-topnav-toggle');
  const compactNav = window.matchMedia('(max-width: 1019.29px)');
  let lastNavFocus: HTMLElement | null = null;
  let closeNavTimer: number | undefined;

  const navFocusable = () =>
    [
      navButton,
      ...Array.from(
        nav?.querySelectorAll<HTMLElement>(
          'a[href], button:not([disabled]), [tabindex]:not([tabindex="-1"])',
        ) || [],
      ),
    ].filter(
      (element): element is HTMLElement =>
        !!element && !element.hasAttribute('disabled') && element.getAttribute('aria-hidden') !== 'true',
    );

  const syncNavState = (open = navToggle?.checked ?? false) => {
    const isCompact = compactNav.matches;
    const navOpen = isCompact && open;
    navButton?.setAttribute('aria-expanded', navOpen ? 'true' : 'false');
    navButton?.setAttribute('aria-label', navOpen ? 'Close navigation' : 'Open navigation');
    if (!nav) return;
    if (isCompact) {
      nav.setAttribute('aria-hidden', navOpen ? 'false' : 'true');
      (nav as HTMLElement & { inert?: boolean }).inert = !navOpen;
    } else {
      nav.removeAttribute('aria-hidden');
      (nav as HTMLElement & { inert?: boolean }).inert = false;
    }
  };

  const closeNav = (restoreFocus = true) => {
    if (!compactNav.matches || !navToggle?.checked) return;
    navToggle.checked = false;
    navToggle.dispatchEvent(new Event('change', { bubbles: true }));
    if (restoreFocus) (lastNavFocus?.isConnected ? lastNavFocus : navButton)?.focus();
  };

  const focusFirstNavTarget = () => {
    const [_, ...targets] = navFocusable();
    (targets[0] || navButton)?.focus();
  };

  const clearNavClose = () => {
    if (closeNavTimer !== undefined) {
      window.clearTimeout(closeNavTimer);
      closeNavTimer = undefined;
    }
    navToggle?.classList.remove('opened');
  };

  const handleNavKeydown = (event: KeyboardEvent) => {
    if (!compactNav.matches || !navToggle?.checked) return;
    if (event.key === 'Escape') {
      event.preventDefault();
      closeNav();
      return;
    }
    if (event.key !== 'Tab') return;
    const focusables = navFocusable();
    if (!focusables.length) return;
    const currentIndex = focusables.indexOf(document.activeElement as HTMLElement);
    const nextIndex = event.shiftKey
      ? currentIndex <= 0
        ? focusables.length - 1
        : currentIndex - 1
      : currentIndex === -1 || currentIndex >= focusables.length - 1
        ? 0
        : currentIndex + 1;
    event.preventDefault();
    focusables[nextIndex]?.focus();
  };

  syncNavState();
  document.addEventListener('keydown', handleNavKeydown);

  const blockBodyScroll = (e: Event) => {
    // on iOS, overflow: hidden isn't sufficient
    if (!nav?.contains(e.target as HTMLElement)) e.preventDefault();
  };

  $(`#${navToggleId}`).on('change', e => {
    const input = e.target as HTMLInputElement;
    const menuOpen = compactNav.matches && input.checked;
    if (!compactNav.matches) input.checked = false;
    const focusWasInNav = !menuOpen && !!nav?.contains(document.activeElement);
    if (focusWasInNav) (lastNavFocus?.isConnected ? lastNavFocus : navButton)?.focus();
    syncNavState(menuOpen);
    if (menuOpen) {
      clearNavClose();
      lastNavFocus = document.activeElement instanceof HTMLElement ? document.activeElement : null;
      document.body.addEventListener('touchmove', blockBodyScroll, { passive: false });
      $(e.target).addClass('opened');
      requestAnimationFrame(() => {
        if (compactNav.matches && navToggle?.checked) focusFirstNavTarget();
      });
    } else {
      document.body.removeEventListener('touchmove', blockBodyScroll);
      if (closeNavTimer !== undefined) window.clearTimeout(closeNavTimer);
      closeNavTimer = window.setTimeout(() => {
        if (!navToggle?.checked) navToggle?.classList.remove('opened');
        closeNavTimer = undefined;
      }, 200);
    }
    document.body.classList.toggle('masked', menuOpen);
  });

  compactNav.addEventListener('change', () => {
    if (compactNav.matches && nav?.contains(document.activeElement)) navButton?.focus();
    navToggle && (navToggle.checked = false);
    clearNavClose();
    document.body.removeEventListener('touchmove', blockBodyScroll);
    document.body.classList.remove('masked');
    if (!compactNav.matches) {
      lastNavFocus = null;
    }
    syncNavState();
  });

  navButton?.addEventListener('click', () => {
    if (!navToggle || !compactNav.matches) return;
    navToggle.checked = !navToggle.checked;
    navToggle.dispatchEvent(new Event('change', { bubbles: true }));
  });

  nav?.addEventListener('click', event => {
    const target = event.target as HTMLElement | null;
    if (compactNav.matches && target?.closest('a[href]')) closeNav(false);
  });

  {
    // stick top bar
    if (window.scrollY > 0) top.classList.add('scrolled');

    window.addEventListener(
      'scroll',
      () => {
        top.classList.toggle('scrolled', window.scrollY > 0);
      },
      { passive: true },
    );

    if (!isTouchDevice() || site.blindMode || !document.querySelector('main.analyse')) return;

    // double tap to align top of board with viewport
    document.querySelector<HTMLElement>('.main-board')?.addEventListener(
      'dblclick',
      e => {
        window.scrollTo({
          top: parseInt(window.getComputedStyle(document.body).getPropertyValue('---site-header-height')),
          behavior: 'instant',
        });
        e.preventDefault();
      },
      { passive: true },
    );
  }
}
