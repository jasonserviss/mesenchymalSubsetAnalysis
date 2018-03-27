
message('Running classification analysis.')

packages <- c(
    "mesenchymalSubsetAnalysis", 
    "printr",
    "ggthemes",
    "tidyverse",
    "mclust",
    "viridis"
)
purrr::walk(packages, library, character.only = TRUE)
rm(packages)

#import counts data and major cell types (identified by marker expression)
message('Importing data')
tsne <- my.tsne.2d.full.top2000.log
counts <- counts.nodups
counts.norm <- countsNorm(counts)

groups.mesenchymal <- groups.mesenchymal.n
groups.fetal <- groupNames(groups.fetal)

#subset mesenchymal cells
message('Subsetting mesenchymal')
mes.tsne <- tsne[groups.fetal == "Mesenchymal", ]
mes.counts <- counts[, groups.fetal == "Mesenchymal"]
mes.norm <- counts.norm[, groups.fetal == "Mesenchymal"]

#run Mclust to classify mesenchymal subtypes
message('Running Mclust')
BIC = mclustBIC(mes.tsne, G = 1:20)
mod1 <- Mclust(mes.tsne, G = 1:20, x = BIC)

#concatenate mesenchymal tsne and classifications
message('Concatenating classification data')
mesDF <- tibble(
    sample = rownames(mes.tsne),
    tsne.dim1 = mes.tsne[, 1],
    tsne.dim2 = mes.tsne[, 2],
    uncertainty = mod1$uncertainty, 
    class = case_when(
        mod1$classification == 1  ~  "Mesenchymal_1",
        mod1$classification == 2  ~  "Mesenchymal_2",
        mod1$classification == 3  ~  "Mesenchymal_3",
        mod1$classification == 4  ~  "Mesenchymal_4",
        mod1$classification == 5  ~  "Mesenchymal_5",
        mod1$classification == 6  ~  "Mesenchymal_6",
        mod1$classification == 7  ~  "Mesenchymal_7",
        mod1$classification == 8  ~  "Mesenchymal_8",
        mod1$classification == 9  ~  "Mesenchymal_9",
        mod1$classification == 10 ~  "Mesenchymal_10",
        TRUE ~ as.character(mod1$classification)
    )
)

#remove cells with high classification uncertainty
message('Removing cells with high uncertainty')
clean <- filter(mesDF, uncertainty < 0.2)
clean %>%
  ggplot() +
  geom_point(aes(tsne.dim1, tsne.dim2, colour = class)) +
  theme_few() +
  scale_colour_ptol()

#save classification data
message('Saving classifications')
classes <- tibble(
    sample = colnames(counts),
    class = groups.fetal
)

classes$class[classes$sample %in% clean$sample] <- clean$class
classes$class <- ifelse(
    classes$class == "Mesenchymal", 
    "UndefinedMesenchymal", 
    classes$class
)
sampleClasses <- classes
save(sampleClasses, file="./data/sampleClasses.rda", compress = "bzip2")
message('Done with classification analysis.')

