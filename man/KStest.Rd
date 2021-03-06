% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ksTest.R
\name{KStest}
\alias{KStest}
\title{KStest}
\usage{
KStest(counts, classes)
}
\arguments{
\item{counts}{Matrix; counts matrix with samples as columns and genes as rows.
Should have both colnames and rownames.}

\item{classes}{Character; vector with length ncol(exp) indicating the class
of each sample.}
}
\value{
A tibble with one row per gene and class combination and the
 corresponding test statistic and p.value.
}
\description{
Runs my.ks.test (giving equivilent results to ks.test) comparing every gene
in exp, for all possible combinations of classes.
}
\examples{
#nothing here yet
}
\author{
Jason T. Serviss
}
