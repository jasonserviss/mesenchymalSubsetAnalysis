## Mesenchymal subtype analysis
_Classification_  
Cell types identified as mesenchymal via their expression of the THY1 marker were sub-classified using the Mclust<sup>1</sup> software version 5.3 within the R statistical programming language<sup>2</sup>. Fourteen Gaussian finite mixture models with component numbers ranging from 1-20 were applied and evaluated via the Bayesian information criterion. A spherical, varying volume model with 10 components was found to best represent the underlying data. The uncertainty in converting a conditional probablility from expectation–maximization to a classification in model-based clustering was reported and cells with a uncertainty > 0.2 (105 cells = ~16%) were removed from further downstream analysis.

_Gene expression profile identification_  
Identified mesenchymal subtypes were grouped with previously characterized major cell types and analysed to identify gene expression profiles. Approximately 6000 genes were pre-selected by choosing those 200 genes exhibiting the highest absolute fold change for each cell type classification. Next, the Kolmogorov-Smirnov test was used to compare each pairwise cell type class for each of the pre-selected genes. 583 genes were identified as significantly (p < 0.05) differentially expressed in all pairwise tests for a single class and were annotated as being specific for that class.


<sup>1</sup> Scrucca L., Fop M., Murphy T. B. and Raftery A. E. (2016) mclust
  5: clustering, classification and density estimation using
  Gaussian finite mixture models The R Journal 8/1, pp. 205-233  
<sup>2</sup> R Core Team. R: A Language and Environment for Statistical Computing, [https://www.R-project.org/](https://www.R-project.org/) (2017).