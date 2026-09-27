import * as licon from 'lib/licon';
import { icons } from 'lib/icons';
import { writeTextClipboard } from 'lib/xhr';
import { chooseAppearance } from './appearance';
import topBar from './topBar';

export function addWindowHandlers() {
  let animFrame: number;

  window.addEventListener('resize', () => {
    cancelAnimationFrame(animFrame);
    animFrame = requestAnimationFrame(setViewportHeight);
  });

  // ios safari vh correction
  function setViewportHeight() {
    document.body.style.setProperty('---viewport-height', `${window.innerHeight}px`);
  }
}

export function addDomHandlers() {
  topBar();
  document.querySelectorAll<HTMLButtonElement>('.auth-password-toggle').forEach(button => {
    button.addEventListener('click', () => {
      const input = document.getElementById(button.getAttribute('aria-controls')!) as HTMLInputElement;
      const visible = input.type === 'password';
      input.type = visible ? 'text' : 'password';
      button.setAttribute('aria-pressed', String(visible));
      button.textContent = visible ? 'Hide password' : 'Show password';
    });
  });
  const headings = [...document.querySelectorAll<HTMLElement>('.legal-content > .legal-section > h2')];
  if (headings.length >= 6) {
    const contents = document.createElement('details');
    contents.className = 'legal-contents';
    contents.open = matchMedia('(min-width: 800px)').matches;
    const summary = document.createElement('summary');
    summary.textContent = 'On this page';
    const nav = document.createElement('nav');
    nav.setAttribute('aria-label', 'On this page');
    const links = headings.map((heading, index) => {
      heading.id ||= 'section-' + (index + 1);
      const link = document.createElement('a');
      link.href = '#' + heading.id;
      nav.appendChild(link);
      return link;
    });
    const update = () => links.forEach((link, index) => { link.textContent = headings[index].textContent; });
    update();
    window.addEventListener('chesstory:language', update);
    contents.append(summary, nav);
    headings[0].parentElement!.before(contents);
  }

  const appearance = document.querySelector<HTMLDetailsElement>('.site-appearance');
  document.addEventListener('click', event => {
    if (appearance?.open && !appearance.contains(event.target as Node)) appearance.open = false;
  });
  document.addEventListener('keydown', event => {
    if (event.key === 'Escape' && appearance?.open) {
      appearance.open = false;
      appearance.querySelector('summary')?.focus();
    }
  });

  $('#main-wrap').on('click', '.copy-me__button', function (this: HTMLElement) {
    const showCheckmark = () => {
      const path = this.querySelector('svg path');
      if (path) {
        path.setAttribute('d', icons.checkmark);
        setTimeout(() => path.setAttribute('d', icons.clipboard), 1000);
        return;
      }
      $(this).attr('data-icon', licon.toDataIcon(licon.Checkmark)).removeClass('button-metal');
      setTimeout(() => $(this).attr('data-icon', licon.toDataIcon(licon.Clipboard)).addClass('button-metal'), 1000);
    };
    const fetchContent = $(this).parent().hasClass('fetch-content');
    $(this.parentElement!.firstElementChild!).each(function (this: any) {
      try {
        if (fetchContent) writeTextClipboard(this.href, showCheckmark);
        else navigator.clipboard.writeText(this.value || this.href).then(showCheckmark);
      } catch (e) {
        console.error(e);
      }
    });
    return false;
  });

  $('body').on('click', '.js-theme-choice', function (this: HTMLElement, e: Event) {
    e.preventDefault();
    const choice = this.getAttribute('data-theme-choice');
    if (appearance) appearance.open = false;
    if (!choice || this.getAttribute('aria-pressed') === 'true') return false;
    chooseAppearance(choice);
    return false;
  });
}
