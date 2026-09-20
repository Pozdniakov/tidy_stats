# Генератор картинки для 210-desc_stats: эксцесс = тяжесть хвостов
# Четыре распределения с одинаковыми средним (0), дисперсией (1) и асимметрией (0),
# но разным эксцессом; нижняя панель — «лупа» на дальний правый хвост.
#
# Результат: images/210-desc_stats_kurtosis_tails.png
# Запускать из корня репозитория книги: Rscript figures/210-desc_stats_kurtosis_tails.R

library(tidyverse)
library(ggforce)   # facet_zoom()
library(ggtext)    # geom_richtext(): смешанное начертание внутри одной подписи

# ── Шрифт ────────────────────────────────────────────────────────────────────
# Golos Text — шрифт картинок книги (решение 18.07). Если он не установлен
# в систему, регистрируем файлы напрямую из assets/.
#
# ВАЖНО: register_font() видят только systemfonts-девайсы (ragg). Если в RStudio
# графический бэкенд не AGG (Tools → Global Options → General → Graphics →
# Backend: AGG), предпросмотр в панели Plots упадёт с «invalid font type» —
# сам ggsave() ниже всё равно отработает. Радикальное лечение: поставить
# assets/fonts/golos-text/*.ttf в систему через Font Book.
fam <- "Golos Text"
if (!fam %in% systemfonts::system_fonts()$family) {
  systemfonts::register_font(
    fam,
    plain = "assets/fonts/golos-text/golos-text-v7-cyrillic_latin-regular.ttf",
    bold  = "assets/fonts/golos-text/golos-text-v7-cyrillic_latin-700.ttf"
  )
}

out <- "images/210-desc_stats_kurtosis_tails.png"

# ── Данные ───────────────────────────────────────────────────────────────────
b  <- 1 / sqrt(2)   # масштаб Лапласа при sd = 1
s5 <- sqrt(5 / 3)   # sd t-распределения с df = 5

x <- seq(-4, 9, by = 0.002)
df <- tibble(
  x = x,
  unif = dunif(x, -sqrt(3), sqrt(3)),
  norm = dnorm(x),
  laplace = exp(-abs(x) / b) / (2 * b),
  t = dt(x * s5, 5) * s5
) %>%
  pivot_longer(cols = !x, names_to = "dist", values_to = "y") %>%
  mutate(dist = factor(dist, levels = unique(dist)))  # порядок колонок,
                                                      # иначе палитра ляжет по алфавиту

# ── Цвета ────────────────────────────────────────────────────────────────────
pal      <- c("#2B83BA", "#5E5E5E", "#FDAE61", "#D7191C")  # минус — синий, ноль — серый, плюс — тёплые
pal_soft <- c("#7FAECF", "#9A9A9A", "#F3C68F", "#E08B80")  # приглушённые: вторые строки легенды
zoom_fill <- "#EFEBF7"; zoom_line <- "#9B8CC9"; zoom_text <- "#7361AE"  # сиреневый зума

# ── Подписи верхней панели (замена легенды): z = FALSE ───────────────────────
top_title <- tibble(x = 4.35, y = 0.705, label = "**Распределения:**", z = FALSE)

top_ann <- tibble(
  x = 4.75,                                # левый край текстов
  y = c(0.615, 0.50, 0.385, 0.27),         # центры блоков (шаг 0.115)
  label = c(
    sprintf("**Платикуртическое**<br><span style='font-size:8.5pt;color:%s'>(эксцесс = −1.2)</span>", pal_soft[1]),
    sprintf("**Мезокуртическое**<br><span style='font-size:8.5pt;color:%s'>(эксцесс = 0)</span>",     pal_soft[2]),
    sprintf("**Лептокуртическое**<br><span style='font-size:8.5pt;color:%s'>(эксцесс = 3)</span>",    pal_soft[3]),
    sprintf("**Лептокуртическое**<br><span style='font-size:8.5pt;color:%s'>(эксцесс = 6)</span>",    pal_soft[4])
  ),
  col = pal, z = FALSE
)

top_key <- tibble(                          # чёрточки-ключи слева от текстов
  x = 4.35, xend = 4.62,
  y = top_ann$y, yend = top_ann$y,
  col = pal, z = FALSE
)

top_note <- tibble(                         # мостик к лупе; x = центр окна зума (4..9)
  x = 6.5, y = 0.105,
  label = "Здесь хвосты уже почти неразличимы:\nвнизу ↓ эти же хвосты под увеличением",
  z = FALSE
)

# ── Подписи зум-панели: порядок как в легенде, слева направо; z = TRUE ───────
ann <- tibble(
  x = c(4.45, 5.7, 6.5, 7.0),
  y = c(0.00252, 0.00170, 0.00110, 0.00060),   # y = центр многострочного блока
  label = c(
    "Равномерное распределение вообще не имеет хвостов\n(оно резко обрубается), эксцесс = −1.2",
    "Хвост нормального распределения\nпочти исчез, эксцесс = 0",
    "Тяжелый хвост, эксцесс = 3",
    "Самый тяжелый хвост, эксцесс = 6"
  ),
  col = pal, z = TRUE
)

seg <- tibble(
  x    = c(4.45, 5.68, 6.48, 6.98),            # синяя: x == xend => строгая вертикаль
  xend = c(4.45, 4.62, 5.60, 6.13),
  y    = c(0.00220, 0.00169, 0.00109, 0.00057),
  yend = c(0.00005, 0.00006, 0.00032, 0.00024),  # кончики чуть НЕ доходят до линий
  col = pal, z = TRUE
)

# ── График ───────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(x, y, colour = dist)) +
  geom_line(linewidth = 0.9, show.legend = FALSE) +
  geom_richtext(
    data = top_title, aes(x, y, label = label), colour = "grey20",
    inherit.aes = FALSE, size = 3.8, hjust = 0, family = fam,
    fill = NA, label.colour = NA, show.legend = FALSE
  ) +
  geom_richtext(
    data = top_ann, aes(x, y, label = label, colour = I(col)),
    inherit.aes = FALSE, size = 3.6, hjust = 0, lineheight = 1.2, family = fam,
    fill = NA, label.colour = NA, show.legend = FALSE
  ) +
  geom_segment(
    data = top_key, aes(x = x, xend = xend, y = y, yend = yend, colour = I(col)),
    inherit.aes = FALSE, linewidth = 0.9, show.legend = FALSE
  ) +
  geom_text(
    data = top_note, aes(x, y, label = label), colour = zoom_text,
    inherit.aes = FALSE, size = 3.3, hjust = 0.5, lineheight = 1.2, family = fam,
    show.legend = FALSE
  ) +
  geom_text(
    data = ann, aes(x, y, label = label, colour = I(col)),
    inherit.aes = FALSE, size = 3.7, fontface = "bold",
    hjust = 0, lineheight = 1.05, family = fam, show.legend = FALSE
  ) +
  geom_segment(
    data = seg, aes(x = x, xend = xend, y = y, yend = yend, colour = I(col)),
    inherit.aes = FALSE, linewidth = 0.4, show.legend = FALSE,
    arrow = arrow(length = unit(5, "pt"), type = "closed")
  ) +
  facet_zoom(
    xlim = c(4, 9), ylim = c(0, 0.003),   # окно лупы: x-диапазон И растяжка по y
    zoom.size = 0.8, horizontal = FALSE,  # панель-лупа снизу
    zoom.data = z                         # z: TRUE — лупа, FALSE — верх, нет колонки — обе
  ) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(breaks = -4:9) +     # одинаковые breaks => грид совпадает между панелями
  labs(
    title = "Одинаковые среднее, дисперсия и асимметрия — разный эксцесс",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13, base_family = fam) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.3),
    zoom.x = element_rect(fill = zoom_fill, colour = zoom_line, linewidth = 0.3),
    zoom.y = element_rect(fill = NA, colour = NA),  # иначе линия вдоль нуля сверху
    validate = FALSE,                               # разрешить ggforce-элементы zoom.* в theme()
    legend.position = "none",
    plot.margin = margin(6, 8, 6, 14)               # запас слева: «0.003» лупы не режется краем
  )

ggsave(out, p, width = 8.4, height = 6, dpi = 300, device = ragg::agg_png)

# Предпросмотр в панели Plots (требует AGG-бэкенда, см. блок «Шрифт» выше)
if (interactive()) print(p)
