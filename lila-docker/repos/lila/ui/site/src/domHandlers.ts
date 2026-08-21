import * as licon from 'lib/licon';
import { writeTextClipboard, text as xhrText } from 'lib/xhr';
import topBar from './topBar';

function submitThemeChoice(choice: string, trigger?: HTMLElement): void {
  trigger?.setAttribute('aria-busy', 'true');
  xhrText(`/pref/bg?v=${encodeURIComponent(choice)}`, { method: 'post' })
    .then(() => {
      window.location.reload();
    })
    .catch(err => {
      console.error(err);
      trigger?.removeAttribute('aria-busy');
    });
}

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

  $('#main-wrap').on('click', '.copy-me__button', function (this: HTMLElement) {
    const showCheckmark = () => {
      $(this).attr('data-icon', licon.Checkmark).removeClass('button-metal');
      setTimeout(() => $(this).attr('data-icon', licon.Clipboard).addClass('button-metal'), 1000);
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
    if (!choice || this.getAttribute('aria-pressed') === 'true') return false;
    submitThemeChoice(choice, this);
    return false;
  });
}
