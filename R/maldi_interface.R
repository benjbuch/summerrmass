# USER INTERFACE ---------------------------------------------------------------

#' Batch MALDI peak analysis of a directory
#'
#' This function is to control the flow of actions when batch processing a
#' directory. It establishes the save and recover policies of previously
#' analysed data so that the same data is not reanalyzed twice if new data
#' is added to a directory.
#'
#' @param path Directory with mass spectra to analyze, store and import results;
#' may contain the plate layout.
#' @param manual ...
#' @param review ...
#'
#' @details
#' The batch process is split into different stages starting from the selected
#' \code{path} as main directory:
#'
#' \enumerate{
#' \item{import and pre-processing of the spectra},
#' \item{automated (or manual) peak picking for each spectrum,
#' depending if \code{manual = FALSE} (or \code{TRUE})},
#' \item{review of automated peak picking if \code{review = TRUE}}.
#' }
#'
#' These tasks are performed by group and the results are stored in the folder
#' enclosing all data of the same group. This is the parent to the folders that
#' match \code{pivot} (the well folders). See \link{import_layout_from_paths.maldi}.
#'
#' If such a folder contains already analyses saved as \code{stored_peaks.generic}
#' or \code{stored_spect.generic}, these analyses can be imported for the next stage;
#' if not, the original files are renamed with a timestamp and new files added.
#'
#' If such a folder contains a plate layout (see \link{import_layout_from_plate}),
#' the data is combined.
#'
#' @return A list with one element for each group.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
maldi_batch <- function(path = NULL,
                        manual = FALSE,
                        review = TRUE,
                        stored_peaks.generic = "maldi_peaks.rds",
                        stored_spect.generic = "maldi_specs.rds",
                        pivot = "[0-9]_[A-Z]+[0-9]+",
                        MoreArgs = list()) {

  ##### Determine directory and handling pre-existing files ####################

  old_wd <- getwd()
  on.exit(setwd(old_wd))

  if (is.null(path))  path <- select_directory()

  # make sure to re-enter the current working directory if anything goes wrong

  path <- normalizePath(path)

  setwd(path)

  # establish groups

  list_rawdata <- function(pattern, path) {

    import_layout_from_paths.maldi(paths = list.files(
      path = path, pattern = pattern, ignore.case = TRUE,
      recursive = TRUE, full.names = TRUE),
      pivot = pivot, relative_to = path) %>%
      dplyr::ungroup() %>%
      dplyr::pull("path_to_group")

  }

  list_expdata <- function(pattern, path) {

    list.files(path = path, pattern = paste0("^", pattern, "$"), ignore.case = TRUE,
               recursive = TRUE, full.names = TRUE) %>%
      dirname()

  }

  log_task("assessing directory", sQuote(path))

  # applying cbind and rbind is strictly necessary to suppress unwanted recycling

  data_in_path <- tibble::as_tibble(cbind(
    rbind(sapply(c(raw_xml_files = "\\.mzxml",
                   raw_fid_files = "fid"), list_rawdata,
                 path = path, USE.NAMES = TRUE, simplify = FALSE)),
    rbind(sapply(c(processed_spectra_rds = stored_spect.generic,
                   processed_peaks_rds = stored_peaks.generic), list_expdata,
                 path = path, USE.NAMES = TRUE, simplify = FALSE))
  )) %>%
    tidyr::pivot_longer(dplyr::everything(), values_to = "path_to_group") %>%
    tidyr::unnest("path_to_group") %>%
    # make sure to get trimmed dirnames
    dplyr::mutate(path_to_group = stringr::str_remove(.data$path_to_group, path)) %>%
    dplyr::group_by(.data$name, .data$path_to_group) %>%
    dplyr::summarize(status = dplyr::n()) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "status") %>%
    dplyr::select(c("path_to_group", "raw_fid_files", "raw_xml_files",
                    "processed_spectra_rds", "processed_peaks_rds"))

  log_process("found", nrow(data_in_path), ifelse(nrow(data_in_path) > 1,
                                                  "groups", "group"))

  log_object(data_in_path)


}


