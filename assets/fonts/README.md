# Шрифты книги

Все — SIL Open Font License 1.1: свободны для личного и коммерческого
использования, включая встраивание в PDF и публикацию в вебе.

| Шрифт | Роль | Источник |
|---|---|---|
| Source Sans 3 | основной текст: PDF (`.otf`) и HTML (`.woff2`, `fonts.css`) | adobe-fonts/source-sans, ветка release |
| Source Code Pro | код: PDF (`.otf`) и HTML (`.woff2`) | adobe-fonts/source-code-pro, ветка release |
| Montserrat Bold | заголовки: PDF (`.ttf`, через sectsty) и HTML (`.woff2`, `fonts.css`) | JulietaUla/Montserrat |
| Golos Text | картинки (генераторы в `figures/`) | googlefonts/golos-text (Paratype) |

Лежат в репозитории, чтобы книга и `figures/*.R` собирались на любой машине
без установки шрифтов в систему: PDF подключает файлы через fontspec
(`_quarto.yml`), HTML — через `@font-face` в `fonts.css`, картинки — через
`systemfonts::register_font()`.
