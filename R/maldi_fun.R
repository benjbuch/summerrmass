# ==== FILE IMPORT AND EXPORT ====

#' Establish the experiment layout from nested directories for MALDI analyses.
#'
#' This wrapper conveniently establishes the experiment layout from nested file
#' groups as exported by Bruker FlexAnalysis. Here, measurements for each plate
#' position (well) are saved in a folder such as "0_A1", "0_A2", etc. Replicate
#' measurements of the same well are grouped in subfolders such as "0_A1/1",
#' "0_A1/2" etc.
#'
#' @inheritParams import_layout_from_paths
#'
#' @details
#' \code{pivot} must contain a regular expression to identify the well, i.e.,
#' contain [A-Z]+[0-9]+".
#'
#' @seealso \code{\link{import_layout_from_paths}}
#'
#'
import_layout_from_paths.maldi <- function(paths, pivot = "[0-9]_[A-Z]+[0-9]+",
                                           relative_to = getwd()) {

  require(dplyr, quietly = TRUE)

  well_regex <- "[A-Z]+[0-9]+"

  stopifnot(grepl(well_regex, x = pivot, fixed = TRUE))

  datad <- import_layout_from_paths(paths = paths, pivot = pivot,
                                    relative_to = relative_to)

  datam <- dplyr::mutate(datad, as_well(stringr::str_extract(pivot, well_regex),
                                        as.tibble = TRUE)) %>%
    dplyr::arrange(well, sub_1, .by_group = TRUE)

  attr(datam, "dir") <- attr(datad, "dir")

  return(datam)

}
