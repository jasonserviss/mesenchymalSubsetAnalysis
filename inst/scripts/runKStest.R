message('Running KStest analysis.')

library(mesenchymalSubsetAnalysis)

#import data
message('Importing data')
counts <- counts.nodups
counts.norm <- countsNorm(counts)
classSub <- subset(sampleClasses, !class %in% c("UndefinedMesenchymal")) %>%
filter(class %in% c("Acinar", "Beta"))

#run the KS test
message('Running KStest')
KSres <- KStest(counts.norm[, colnames(counts.norm) %in% classSub$sample], classSub$class, cores = 2)
save(KSres, file="/home/mesenchymalSubsetAnalysis/data/KSres.rda", compress = "bzip2")
message('Done with KStest analysis.')
