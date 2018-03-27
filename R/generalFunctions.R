#' countsNorm
#'
#'
#' @name countsNorm
#' @rdname countsNorm
#' @author Jason T. Serviss
#' @keywords countsNorm
#' @param counts A counts matrix.
#' @examples
#'
#' \dontrun{countsNorm()}
#'
#' @export
NULL

countsNorm <- function(counts) {
    norm.fact <- colSums(counts)
    t(apply(counts, 1, function(x)
        {x/norm.fact*1000000+1}
    ))
}

#' groupNames
#'
#'
#' @name groupNames
#' @rdname groupNames
#' @author Jason T. Serviss
#' @param groups.fetal Character vector with Matrins groupings of the cells.
#' @keywords groupNames
#' @examples
#'
#' \dontrun{groupNames()}
#'
#' @export
#' @importFrom dplyr case_when
NULL

groupNames <- function(groups.fetal) {
    case_when(
        groups.fetal == "GCG.fetal" ~ "Alpha",
        groups.fetal == "INS.fetal" ~ "Beta",
        groups.fetal == "SST.fetal" ~ "Delta",
        groups.fetal == "ductal.fetal" ~ "Ductal",
        groups.fetal == "mesenchymal.fetal" ~ "Mesenchymal",
        groups.fetal == "blood.fetal" ~ "Blood",
        groups.fetal == "endocrine.progenitor" ~ "Endocrine progenitor",
        groups.fetal == "PRSS1.fetal" ~ "Acinar",
        groups.fetal == "vascular.fetal" ~ "Endothelial",
        names(groups.fetal) %in% .defineUndefined()[[1]] ~ "Undefined1",
        names(groups.fetal) %in% .defineUndefined()[[2]] ~ "Undefined2"
    )
}

.defineUndefined <- function() {
    g <- groups.fetal
    tsne <- my.tsne.2d.full.top2000.log
    
    t <- tsne[rownames(tsne) %in% names(g[g == "undef"]), ]
    undef1 <- rownames(t[t[,2] < 0, ])
    undef2 <- rownames(t[t[,2] > 0, ])
    return(list(undef1, undef2))
}

#' col64
#'
#' Diverging color palette.
#'
#' @name col64
#' @rdname col64
#' @author Jason T. Serviss
#' @keywords col64
#' @examples
#' col64()
#'
#' @export
NULL

col64 <- function() {
    c(
    "#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6",
    "#A30059", "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43",
    "#8FB0FF", "#997D87", "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601",
    "#3B5DFF", "#4A3B53", "#FF2F80", "#61615A", "#BA0900", "#6B7900", "#00C2A0",
    "#FFAA92", "#FF90C9", "#B903AA", "#D16100", "#DDEFFF", "#000035", "#7B4F4B",
    "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F", "#372101", "#FFB500",
    "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09", "#00489C",
    "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
    "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED",
    "#886F4C", "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9",
    "#FF913F", "#938A81", "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700",
    "#04F757", "#C8A1A1", "#1E6E00", "#7900D7", "#A77500", "#6367A9", "#A05837",
    "#6B002C", "#772600", "#D790FF", "#9B9700", "#549E79", "#FFF69F", "#201625",
    "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329", "#5B4534", "#FDE8DC",
    "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C"
    )
}

#' namedListToTibble
#'
#' Converts a named list to a long data frame.
#'
#' @name namedListToTibble
#' @rdname namedListToTibble
#' @author Jason T. Serviss
#' @param l List. The list to be converted.
#' @keywords namedListToTibble
#' @examples
#'
#' l <- list(a=LETTERS[1:10], b=letters[1:5])
#' namedListToTibble(l)
#'
#' @export
#' @importFrom tibble tibble
NULL

namedListToTibble <- function(l) {
    if (length(names(l)) != length(l)) {
        stop("The list you submitted might not be named.")
    }
    if (!is.null(names(l[[1]]))) {
        ni <- gsub(".*\\.(.*)$", "\\1", names(unlist(l)))
        n <- rep(names(l), lengths(l))
        tibble::tibble(
        names = n,
        inner.names = ni,
        variables = unname(unlist(l))
        )
    } else {
        n <- rep(names(l), lengths(l))
        tibble::tibble(
        names = n,
        variables = unname(unlist(l))
        )
    }
}

#' scaleFeatures
#'
#' Scales rows of a matrix between 0 and 1.
#'
#' @name scaleFeatures
#' @rdname scaleFeatures
#' @author Jason T. Serviss
#' @param mat The marrix to be scaled.
#' @keywords scaleFeatures
#' @examples
#'
#' mat <- matrix(1:100, ncol = 10, nrow = 10)
#' scaleFeatures(mat)
#'
#' @export
#' @importFrom tibble tibble
NULL

scaleFeatures <- function(mat) {
    t(apply(mat, 1, function(x)
        (x - min(x)) / (max(x) - min(x))
    ))
}

#' scaleFeaturesV
#'
#' Scales a vector between 0 and 1.
#'
#' @name scaleFeaturesV
#' @rdname scaleFeaturesV
#' @author Jason T. Serviss
#' @param v The marrix to be scaled.
#' @keywords scaleFeaturesV
#' @examples
#'
#' v <- 1:100
#' scaleFeaturesV(v)
#'
#' @export
#' @importFrom tibble tibble
NULL

scaleFeaturesV <- function(v) {
    (v - min(v)) / (max(v) - min(v))
}


#' foldChangePerGroup
#'
#'
#' @name foldChangePerGroup
#' @rdname foldChangePerGroup
#' @author Jason T. Serviss
#' @param mat The matrix holding expression values.
#' @param class A tibble with columns \emph{class} and \emph{sample} indicating
#'    the class ID and sample ID respectivley.
#' @param subset An optional character vector in the case that the fold change
#'    for only a subset of groups should be calculated.
#' @keywords foldChangePerGroup
#'
#'
#' @export
#' @importFrom tibble tibble
NULL

foldChangePerGroup <- function(mat, classes, subset = NULL) {
    uGroups <- unique(classes$class)
    
    if(length(subset) > 0) {
        uGroups <- uGroups[uGroups %in% subset]
    }
    
    res <- sapply(1:length(uGroups), function(x) {
        samplesA <- filter(classes, class == uGroups[x])$sample
        samplesB <- filter(classes, class != uGroups[x])$sample
        a <- rowMeans(mat[, colnames(mat) %in% samplesA])
        b <- rowMeans(mat[, colnames(mat) %in% samplesB])
        a/b
    })
    colnames(res) <- uGroups
    return(res)
}

#' meanPerGroup
#'
#'
#' @name meanPerGroup
#' @rdname meanPerGroup
#' @author Jason T. Serviss
#' @param mat The matrix holding expression values.
#' @param class A tibble with columns \emph{class} and \emph{sample} indicating
#'    the class ID and sample ID respectivley.
#' @keywords meanPerGroup
#'
#'
#' @export
#' @importFrom tibble tibble
NULL

meanPerGroup <- function(mat, classes) {
    uGroups <- unique(classes$class)
    
    res <- sapply(1:length(uGroups), function(x) {
        samplesA <- filter(classes, class == uGroups[x])$sample
        rowMeans(mat[, colnames(mat) %in% samplesA])
    })
    colnames(res) <- uGroups
    return(res)
}

#' col64
#'
#'
#' @name col64
#' @rdname col64
#' @author Jason T. Serviss
#' @keywords col64
#' @examples
#' col64()
#' @export
NULL

col64 <- function() {
    return(
        c(
            "#9F94F0", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941",
            "#006FA6", "#A30059", "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC",
            "#B79762", "#004D43", "#8FB0FF", "#997D87", "#5A0007", "#809693",
            "#78AFA1", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
            "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9",
            "#B903AA", "#D16100", "#DDEFFF", "#000035", "#7B4F4B", "#A1C299",
            "#300018", "#0AA6D8", "#013349", "#00846F", "#372101", "#FFB500",
            "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
            "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68",
            "#7A87A1", "#788D66", "#885578", "#FAD09F", "#FF8A9A", "#D157A0",
            "#BEC459", "#456648", "#0086ED", "#886F4C", "#34362D", "#B4A8BD",
            "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
            "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757",
            "#C8A1A1", "#1E6E00", "#7900D7", "#A77500", "#6367A9", "#A05837",
            "#6B002C", "#772600", "#D790FF", "#9B9700", "#549E79", "#FFF69F",
            "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
            "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804",
            "#324E72", "#6A3A4C"
        )
    )
}

#' doMerge
#'
#'
#' @name doMerge
#' @rdname doMerge
#' @author Jason T. Serviss
#' @keywords doMerge
#' @param counts.norm; Matrix with normalized expression values.
#' @param class A tibble with columns \emph{class} and \emph{sample} indicating
#'    the class ID and sample ID respectivley.
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
NULL

doMerge <- function(class, counts.norm) {
    
    #find all merge combinations to calculate the sum of fold changes for
    combs <- combn(unique(class$class), 2)
    
    c <- t(sapply(1:ncol(combs), function(x) {
        if_else(class$class %in% combs[, x], "m", class$class)
    }))
    
    #calculate the sum of fold changes for the merged possibilities
    fcs <- sapply(1:nrow(c), function(x) {
        fc <- foldChangePerGroup(
            counts.norm,
            class,
            subset = "m"
        )
        sum(abs(fc))
    })
    
    #calculate the original (unmerged) sum of fold changes
    original <- foldChangePerGroup(counts.norm, class)
    o <- colSums(original)
    
    #reformat results for easy plotting
    bind_rows(
        tibble(
            class1 = c(combs[1, ], unique(class$class)),
            class2 = c(combs[2, ], unique(class$class)),
            sumFcs = c(fcs, o)
        ),
        tibble(
            class1 = c(combs[2, ], unique(class$class)),
            class2 = c(combs[1, ], unique(class$class)),
            sumFcs = c(fcs, o)
        )
    )
}

#' doSplit
#'
#'
#' @name doSplit
#' @rdname doSplit
#' @author Jason T. Serviss
#' @keywords doSplit
#' @param counts.norm; Matrix with normalized expression values.
#' @param class A tibble with columns \emph{class} and \emph{sample} indicating
#'    the class ID and sample ID respectivley.
#' @param iter A numeric vector of length 1 indicating the number of random
#'    splits to test.
#' @param subset A character vector of length 1 indicating the specific class to
#'    investigate.
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
NULL

doSplit <- function(class, counts.norm, iter, subset, seed = 1829) {
    
    set.seed(seed)
    splits <- sapply(1:iter, function(x) {
        n <- length(class$class[class$class %in% subset])
        idx <- which(class$class %in% subset)
        mix <- sample(idx, size = n, replace = FALSE)
        new <- sample(c("s1", "s2"), size = length(mix), replace = TRUE)
        class$class[idx] <- new
        class$class
    })
    
    fcs <- sapply(1:iter, function(x) {
        fc <- foldChangePerGroup(
            counts.norm,
            tibble(
                sample = class$sample,
                class = splits[, x]
            ),
            subset = c("s1", "s2")
        )
        mean(colSums(fc))
    })
    
    original <- colSums(foldChangePerGroup(counts.norm, class, subset))
    
    bind_rows(
        tibble(
            fc = original,
            split = list(class$class)
        ),
        tibble(
            fc = fcs,
            split = lapply(1:ncol(splits), function(x) splits[, x])
        )
    )
        
}

#' coloursFromTargets
#'
#' Diverging color palette with 40 colors.
#'
#' @name coloursFromTargets
#' @rdname coloursFromTargets
#' @author Jason T. Serviss
#' @param pal character; A colour palette with length = length(markers).
#' @param counts matrix; A matrix containing counts.
#' @param markers character; The markers to evaluate. Must be present in
#'  rownames(counts).
#' @param ... additional arguments to pass on.
#' @keywords coloursFromTargets
NULL

#' @rdname coloursFromTargets
#' @importFrom dplyr "%>%" group_by ungroup mutate arrange summarize select
#' @importFrom rlang .data
#' @importFrom tibble tibble add_column
#' @importFrom tidyr gather unnest spread
#' @importFrom purrr pmap pmap_chr
#' @importFrom grDevices col2rgb rgb
#' @importFrom readr parse_factor

coloursFromTargets <- function(
  pal,
  counts,
  markers,
  ...
){
  
  if(is.null(markers) | is.null(pal) | length(markers) == 1) {
    return(tibble('Sample' = colnames(counts)))
  }

  markers <- sort(markers)
  pal <- pal[1:length(markers)]
  
  counts[rownames(counts) %in% markers, ] %>%
  matrix_to_tibble(., 'geneName') %>%
  gather('Sample', 'count', -.data$geneName) %>%
  #normalize
  group_by(.data$geneName) %>%
  mutate('normalized' = normalizeVec(.data$count)) %>%
  ungroup() %>%
  #calculate fraction
  group_by(.data$Sample) %>%
  mutate('fraction' = .data$normalized / sum(.data$normalized)) %>%
  mutate('fraction' = if_else(is.nan(.data$fraction), 1 / n(), .data$fraction)) %>%
  #setup initial hex colors
  arrange(.data$geneName) %>%
  mutate('colint' = pal) %>%
  ungroup() %>%
  #convert to rgb and calculate new colors
  mutate('rgb' = pmap(
    list(.data$colint, .data$normalized, .data$fraction),
    function(x, y, z) {
      (255 - ((255 - col2rgb(x)) * y)) * z
    }
  )) %>%
  unnest() %>%
  add_column('col' = rep(c("r", "g", "b"), nrow(.) / 3)) %>%
  group_by(.data$Sample, .data$col) %>%
  summarize('sumRGB' = sum(.data$rgb) / 256) %>%
  ungroup() %>%
  spread('col', 'sumRGB') %>%
  #convert back to hex
  mutate('Colour' = pmap_chr(
    list(.data$r, .data$g, .data$b),
    function(x, y, z) {
    rgb(red = x, green = y, blue = z)
    }
  )) %>%
  select(-(.data$b:.data$r)) %>%
  #fix factor levels so ggplot legend will cooperate
  #https://community.rstudio.com/t/drop-false-with-scale-fill-identity/5163/2
  mutate('Colour' = parse_factor(
    .data$Colour,
    levels = unique(c(.data$Colour, pal[!pal %in% .data$Colour]))
  ))
}

#' matrix_to_tibble
#'
#' Converts a matrix to a tibble without removing rownames.
#'
#' @name matrix_to_tibble
#' @rdname matrix_to_tibble
#' @author Jason T. Serviss
#' @param data matrix; The matrix to be converted.
#' @param rowname character; Length 1 vector indicating the colname that
#'  rownames should have upon tibble conversion.
#' @keywords matrix_to_tibble
#' @examples
#'
#' m <- matrix(rnorm(20), ncol = 2, dimnames = list(letters[1:10], LETTERS[1:2]))
#' output <- matrix_to_tibble(m)
#'
#' @export
#' @importFrom tibble as_tibble rownames_to_column

matrix_to_tibble <- function(data, rowname = "rowname") {
  data %>%
  as.data.frame() %>%
  rownames_to_column(var = rowname) %>%
  as_tibble()
}

#' normalizeVec
#'
#' Normalizes a vector (x) using x - min(x) / max(x) - min(x).
#'
#' @name normalizeVec
#' @rdname normalizeVec
#' @author Jason T. Serviss
#' @param vec numeric; A numeric vector.
#' @keywords normalizeVec
#' @examples
#'
#' normalizeVec(rnorm(100))
#'
#' @export

normalizeVec <- function(vec) {
  (vec - min(vec)) / (max(vec) - min(vec))
}


