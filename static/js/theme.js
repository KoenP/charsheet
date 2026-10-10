(function () {
  var btn = document.getElementById('theme-toggle');
  if (!btn) return;

  var STORAGE_KEY = 'charsheet-theme';
  var DARK_THEME = 'rose-pine';

  function updateButton() {
    var isDark = document.documentElement.getAttribute('data-theme') === DARK_THEME;
    btn.textContent = isDark ? '\u2600\uFE0F' : '\uD83C\uDF19';
    btn.setAttribute('aria-pressed', isDark ? 'true' : 'false');
    btn.setAttribute('aria-label', isDark ? 'Switch to light theme' : 'Switch to dark theme');
    btn.setAttribute('title', isDark ? 'Switch to light theme' : 'Switch to dark theme');
  }

  function setTheme(dark) {
    if (dark) {
      document.documentElement.setAttribute('data-theme', DARK_THEME);
    } else {
      document.documentElement.removeAttribute('data-theme');
    }
    try {
      localStorage.setItem(STORAGE_KEY, dark ? DARK_THEME : 'rose-pine-dawn');
    } catch (e) {}
    updateButton();
  }

  btn.addEventListener('click', function () {
    setTheme(document.documentElement.getAttribute('data-theme') !== DARK_THEME);
  });

  updateButton();
})();
