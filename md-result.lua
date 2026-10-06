-- Рамка «Получаем» для живого рендера Markdown-примеров (глава 250).
-- HTML и EPUB оформляют класс .md-result через md-result.css;
-- для PDF div оборачивается в окружение tcolorbox `mdresult` (определено в _quarto.yml).
function Div(el)
  if not el.classes:includes("md-result") then
    return nil
  end
  if quarto.doc.is_format("pdf") then
    local out = pandoc.List({ pandoc.RawBlock("latex", "\\begin{mdresult}") })
    out:extend(el.content)
    out:insert(pandoc.RawBlock("latex", "\\end{mdresult}"))
    return out
  end
  return nil
end
