message('Running KStest analysis.')

packages <- c("mesenchymalSubsetAnalysis", "dplyr", "tidyr")
suppressPackageStartupMessages(purrr::walk(packages, library, character.only = TRUE))
rm(packages)

#import data
message('Importing data')
counts <- counts.nodups
counts.norm <- countsNorm(counts)
rm(counts)
classSub <- subset(sampleClasses, !class %in% c("UndefinedMesenchymal"))

#Pre-filter
message('Pre-filtering genes')
select <- foldChangePerClass(counts.norm, sampleClasses) %>%
matrix_to_tibble("gene") %>%
gather(comparison, foldChange, -gene) %>%
mutate(abs.FC = abs(foldChange)) %>%
group_by(comparison) %>%
top_n(n = 200, wt = abs.FC) %>%
pull(gene) %>%
unique()

idx <- which(rownames(counts.norm) %in% select)
rm(select)

#run the KS test
message('Running KStest')
KSres <- KStest(counts.norm[idx, colnames(counts.norm) %in% classSub$sample], classSub$class, cores = 2)
save(KSres, file="/home/mesenchymalSubsetAnalysis/data/KSres.rda", compress = "bzip2")
message('Done with KStest analysis.')
