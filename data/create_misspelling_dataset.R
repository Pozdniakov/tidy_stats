# this is dataset from this article https://pudding.cool/2019/02/gyllenhaal/
library(tidyverse)
library(jsonlite)
df <- jsonlite::read_json("https://pudding.cool/2019/02/sankey-data/data-all.json")
df <- df$data$names

misspellings <- map_dfr(1:15, function(i){
  tibble(correct = names(df)[i],
         spelling = map_chr(df[[i]], "name"),
         count = map_int(df[[i]], "count"))
})

write_csv(misspellings, "data/misspelling_dataset.csv")
