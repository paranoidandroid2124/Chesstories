import { wsDestroy } from 'lib/socket';

let redirectInProgress = false;

export const redirect = (opts: RedirectTo) => {
  const url = typeof opts === 'string' ? opts : opts.url;
  const href = '//' + location.host + '/' + url.replace(/^\//, '');
  redirectInProgress = true;
  location.href = href;
};

export const unload = {
  expected: false,
};

export const reload = (err?: any) => {
  if (err) console.warn(err);
  if (redirectInProgress) return;
  unload.expected = true;
  wsDestroy();
  if (location.hash) location.reload();
  else location.assign(location.href);
};
