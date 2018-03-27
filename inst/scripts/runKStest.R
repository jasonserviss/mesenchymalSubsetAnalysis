message('Running KStest analysis.')

library(mesenchymalSubsetAnalysis)

#import data
counts <- counts.nodups
counts.norm <- countsNorm(counts)

#run the KS test
classSub <- subset(sampleClasses, !class %in% c("UndefinedMesenchymal"))
KSres <- KStest(counts.norm[ , colnames(counts.norm) %in% classSub$sample], classSub$class, cores = 2)
save(KSres, file="/home/mesenchymalSubsetAnalysis/data/KSres.rda", compress="bzip2")
message('Done with KStest analysis.')
