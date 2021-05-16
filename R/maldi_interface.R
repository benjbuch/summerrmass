# USER INTERFACE ---------------------------------------------------------------

#' Batch MALDI peak analysis of a directory
#'
#' This function is to control the flow of actions when batch processing a
#' directory. It establishes the save and recover policies of previously
#' analysed data so that the same data is not reanalyzed twice if new data
#' is added to a directory.
#'
#' @param path Directory with mass spectra to analyze, store and import results;
#' may contain the plate layout(s).
#' @inheritParams import_layout_from_paths.maldi
#' @param manual ...
#' @param review ...
#' @param layout_file,confirm_layout_file,layout_import_FUN Path to a file that
#' contains metadata associated with each well/group. \code{layout_FUN} is applied
#' to \code{layout_file} before it is joined with the measurement result. Must
#' share column names with \link{import_layout_from_paths.maldi}. See Details.
#' @param FUN The function to be applied for peak detection.
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
#' \subsection{Joining with metadata from external plate layout}
#'
#' If \code{layout_file} is \code{NULL}, the results will not be joined with an
#' external file that holds associated metadata. (Skip this section.)
#'
#' External plate layouts can be provided via \code{layout_file} either (1) as an
#' absolute path to a single file somwhere in the file system or (2) a file in
#' expressed relative to the selected \code{path}, e.g. "global_layout.xls".
#' The query is made via \code{\link[base:files]{file.exists}}.
#' In such cases, the file is used for all groups. In case (3) when such a file
#' does not exist, the file is looked for locally in each group directory, where
#' it must be present each time under the same name, e.g., as "layout.xls".
#' If (4) this is neither the case and \code{confirm_layout_file = TRUE}, you
#' will finally be prompted to select a layout file. You can force this behavior
#' by setting \code{layout_file = ""}.
#'
#' If \code{confirm_layout_file = TRUE}, you will also be asked if the file
#' identified according to one of the rules (1-3) above should be taken or not.
#'
#' To successfully merge your layout with the data, common column names must
#' exist between your layout file and the result of \link{import_layout_from_paths.maldi},
#' which extends the generic \link{import_layout_from_paths} by explicitly
#' extracting a well number as column "well".
#'
#' If you choose a graphical approach to define the plate layout, consider using
#' \link{import_layout_from_excel}, which will convert the plate into a (long)
#' table with a column "well" and "content".
#'
#' If you provide such a (long) table directly, provide \code{\link{readxl::read_excel}}
#' or another appropriate function as argument \code{layout_import_FUN}.
#'
#' @return A list with one element for each group.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
maldi_batch <- function(path = NULL,
                        pivot = "[0-9]_[A-Z]+[0-9]+",
                        layout_file = NULL,
                        confirm_layout_file = TRUE,
                        manual = FALSE,
                        review = TRUE,
                        FUN = maldi_peaks_by_well,
                        MoreArgs = list(),
                        layout_import_FUN = import_layout_from_excel,
                        stored_peaks.generic = "maldi_peaks.rds",
                        stored_spect.generic = "maldi_specs.rds"
                        ) {

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


