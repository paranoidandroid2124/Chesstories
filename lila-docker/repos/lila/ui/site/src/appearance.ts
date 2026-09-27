import { text as xhrText } from 'lib/xhr';

export function applyAppearance(choice: string): void {
  if (!['light', 'dark', 'system'].includes(choice)) choice = 'dark';
  const root = document.documentElement;
  const light = choice === 'light' || (choice === 'system' && matchMedia('(prefers-color-scheme: light)').matches);
  root.dataset.appearance = document.body.dataset.theme = choice;
  root.classList.add('pro');
  root.classList.toggle('light', light);
  root.classList.toggle('dark', !light);
  document.querySelectorAll<HTMLElement>('.js-theme-choice').forEach(button =>
    button.setAttribute('aria-pressed', String(button.dataset.themeChoice === choice)),
  );
  document.querySelector('meta[name="theme-color"]')?.setAttribute('content', light ? '#f4f5f6' : '#0e1419');
}

export function chooseAppearance(choice: string): void {
  if (!['light', 'dark', 'system'].includes(choice)) return;
  applyAppearance(choice);
  try { localStorage.setItem('chesstory.appearance', choice); } catch { /* This visit still uses the choice. */ }
  if (document.body.dataset.user)
    void xhrText(`/pref/bg?v=${choice}`, { method: 'post' }).catch(console.error);
}
