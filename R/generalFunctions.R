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
  t(t(counts) / colSums(counts) * 10^6 + 1)
}

#' groupNames
#'
#'
#' @name groupNames
#' @rdname groupNames
#' @author Jason T. Serviss
#' @param groups.fetal Character vector with Martin's groupings of the cells.
#' @keywords groupNames
#' @examples
#'
#' \dontrun{groupNames()}
#'
#' @export
#' @importFrom dplyr case_when
NULL

groupNames <- function(g) {
  case_when(
    g == "GCG.fetal" ~ "Alpha",
    g == "INS.fetal" ~ "Beta",
    g == "SST.fetal" ~ "Delta",
    g == "ductal.fetal" ~ "Ductal",
    g == "mesenchymal.fetal" ~ "Mesenchymal",
    g == "blood.fetal" ~ "Blood",
    g == "endocrine.progenitor" ~ "Endocrine progenitor",
    g == "PRSS1.fetal" ~ "Acinar",
    g == "vascular.fetal" ~ "Endothelial",
    names(g) %in% c(
      "X1000101303.G4", "X1000101306.H6", "X1000101306.E4",
      "X1000101306.A9", "X1000101309.A10", "X1000101306.D8",
      "X1000101309.G2", "X1000101302.A4", "X1000101302.H11",
      "X1000101301.B5"
    ) ~ "Undefined1",
    names(g) %in% c(
      "X1000101303.C11", "X1000101303.D6", "X1000101801.G12",
      "X1000101309.E4", "X1000101309.C4", "X1000101306.G10",
      "X1000101302.G6", "X1000101302.D6", "X1000101302.F11",
      "X1000102401.G5", "X1000101301.D5", "X1000102602.D9",
      "X1000101301.H11", "X1000101803.E7", "X1000102602.B11"
    ) ~ "Undefined2"
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

#' foldChangePerClass
#'
#' Calculates fold change for each gene with each class vs all other classes.
#'
#' @name foldChangePerClass
#' @rdname foldChangePerClass
#' @author Jason T. Serviss
#' @param counts The matrix holding expression values.
#' @param class A tibble with columns \emph{class} and \emph{sample} indicating
#'    the class and sample ID respectivley.
#' @keywords foldChangePerClass
#'
#'
#' @export
#' @importFrom dplyr filter
NULL

foldChangePerClass <- function(counts, classes) {
  uGroups <- unique(classes$class)
  combs <- combn(uGroups, 2)
  
  res <- sapply(1:ncol(combs), function(x) {
    samplesA <- filter(classes, class == combs[1, x])$sample
    samplesB <- filter(classes, class == combs[2, x])$sample
    a <- rowMeans(counts[, colnames(counts) %in% samplesA])
    b <- rowMeans(counts[, colnames(counts) %in% samplesB])
    a/b
  })
  colnames(res) <- paste(combs[1, ], combs[2, ], sep = "-")
  return(res)
}
