library(glue)
library(dplyr)
library(stringr)
source("natural_effect_definitions.R")
effects = c(natural_defs(),
            MS1_defs(),
            MS2_defs())

replacements = c(
  "NIE12" = "NIE_{12}",
  "NIE1" = "NIE_{1}",
  "NIE2" = "NIE_{2}",
  "MS1" = "MS_{1}",
  "MS2" = "MS_{2}",
  "M1" = "M_{1}",
  "M2" = "M_{2}"
)

create_po <-
  function(po)
    as.character(glue("Y({po[1]}, M_1({po[2]}), M_2({po[3]}, M_1({po[4]}))"))



new_effects = list()
for (ef in effects) {
  for (repl_pat in names(replacements)) {
    ef$label = str_replace(ef$label,
                           pattern = fixed(repl_pat),
                           replacement = replacements[[repl_pat]])
  }
  ef$level = NA
  if (str_detect(ef$label, regex("N.*"))) {
    level = str_extract(ef$label, regex("(?<=-)[01]{3}$"))
    level = sum(as.numeric(str_split(level, "", simplify = T)))
    ef$level = level
  }
  
  lpo = create_po(ef$left_po)
  rpo = create_po(ef$right_po)
  
  ef$contrast = glue("E[{lpo} - {rpo}]")
  
  new_effects = c(new_effects, list(
    list(
      label = glue::glue("${ef$label}$"),
      level = glue::glue("${ef$level}$"),
      contrast = glue::glue("${ef$contrast}$")
    )
  ))
}

new_effects = bind_rows(new_effects)

ms_effects = new_effects %>% filter(!str_starts(label, fixed("$N")))
nat_effects = new_effects %>% filter(str_starts(label, fixed("$N")))

knitr::kable(
  ms_effects %>% select(-level),
  longtable = T,
  format = "latex",
  escape = F,
  caption = "Mediator specific effect definitions",
  align =  c("l","c")
) %>%
  kableExtra::kable_styling(latex_options = "repeat_header")


knitr::kable(
  nat_effects,
  longtable = T,
  format = "latex",
  escape = F,
  caption = "Natural effect definitions",
  align =  c("l","c", "l")
) %>% 
  kableExtra::kable_styling(latex_options = "repeat_header")