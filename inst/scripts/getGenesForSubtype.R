message('Running gene list extraction.')

packages <- c("mesenchymalSubsetAnalysis", "tidyverse")
purrr::walk(packages, library, character.only = TRUE)
rm(packages)

load('data/KSres.rda')

geneList <- processKStest(fc, classes, 0.05) %>%
    filter(sigBool) %>% 
    group_by(gene) %>%
    mutate(n = n()) %>%
    filter(n == 1) %>%
    select(-n, -sigBool) %>%
    ungroup()

save(geneList, file = "data/geneList.rda")