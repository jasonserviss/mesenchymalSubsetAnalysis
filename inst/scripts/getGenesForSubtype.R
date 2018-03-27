message('Running gene list extraction.')

packages <- c("mesenchymalSubsetAnalysis", "tidyverse")
purrr::walk(packages, library, character.only = TRUE)
rm(packages)

load("/home/mesenchymalSubsetAnalysis/data/KSres.rda")

geneList <- processKStest(KSres, sampleClasses$class, 0.05) %>%
    filter(sigBool) %>% 
    group_by(gene) %>%
    filter(n() == 1) %>%
    select(-n, -sigBool) %>%
    ungroup()

save(geneList, file = "/home/mesenchymalSubsetAnalysis/geneList.rda")
message('Done with gene list extraction.')
