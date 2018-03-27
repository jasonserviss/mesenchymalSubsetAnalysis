#needs to be run from the package root
library(mesenchymalSubsetAnalysis)
c <- c("all", "mergedMes458", "mergedMes4578", "endoSuper", "mesenSuper")
ksTestCombos(cores = 12, outFilePath = './inst/KStests', combos = c)

load('./inst/KStests/ksTests_full.rda')
names(geneLists) <- c
names(ksTestResults) <- c

geneListsDF <- dplyr::bind_rows(geneLists, .id = "combination")

