# ==== FILE IMPORT AND EXPORT ====

#' Establish the experiment layout from nested directories for MALDI analyses.
#'
#' This wrapper conveniently establishes the experiment layout from nested file
#' groups as exported by Bruker FlexAnalysis. Here, measurements for each plate
#' position (well) are saved in a folder such as "0_A1", "0_A2", etc. Replicate
#' measurements of the same well are grouped in subfolders such as "0_A1/1",
#' "0_A1/2" etc.
#'
#' @param file_list A list of file paths or a character vector. See Details.
#' @param well_folder A \link[base:regex]{regular expression} describing the
#' names of the folders in a tree that contain measurements from a single well.
#' The well specifier must be excluded using a lookahead or lookbehind.
#'
#' @details
#' \code{file_path} can be a vector such as \code{c("file1", "file2", "file3")}
#' or a list of character vectors, e.g. after averaging spectra, such as
#' \code{list(c("file1", "file2"), "file3")}.
#'
#' \code{well_folder} must be a unique match for each path.
#'
#' @section Sample groups by folder nesting:
#'
#' Sample groups are determined from the enclosing folders. For example, in
#'
#' \preformatted{
#' /common1/folder1/folderA/0_A1/...
#'                       ../0_A2/...
#'               ../folderB/0_A1/...
#' /common1/folder2/folderA/0_A1/...
#'               ../folderB/0_A1/...
#'                         /0_A2/...
#' }
#'
#' two nested groupings are identified: (1) "folder1" and "folder2" as \code{grp_0},
#' and (2) "folderA" and "folderB" as \code{grp_1}. The common folder "common1"
#' will not be used as a grouping variable. The first enclosing folder is always
#' used. The full (unique) grouping is given as \code{group}.
#'
#' Grouping allows to anaylze multiple directories simultaneously (with their own or
#' a shared plate layout, see elsewhere).
#'
#' @return
#' A tibble with the experiment layout as determined from the paths in \code{file_list}
#' with columns \code{grp_0}, ..., \code{grp_N} specifiying the sample groups,
#' \code{well}, \code{replicate}, \code{n_replicates}, \code{path}.
#'
#' @examples
#' get_layout_from_tree.maldi(c("folderA/0_A1/1/file.x", "folderA/0_A1/2/file.x",
#'   "folderA/0_A2/1/file.x", "folderB/0_A1/1/file.x")
get_layout_from_tree.maldi <- function(file_list, well_folder = "[0-9]_(?=[A-Z]+[0-9]+)") {

  require(dplyr, quietly = TRUE)

  # several scenarios need to be dealt with
  #
  # 1) a plain character vector
  #    c("file1", "file2", "file3")
  #
  # 2) a list of character vectors, e.g. after averaging spectra
  #    list(c("file1", "file2"), "file3")
  #
  # this transforms both cases into a plain character vector, for which only the
  # first element in a nested list will be kept

  datad <- sapply(file_list, "[[", 1) %>% normalizePath() %>%
    tibble::as_tibble() %>%
    # since levels of folder groups may be nested to different extents, split at
    # the well folder using a look behind to preserve the well number
    tidyr::separate(value, into = c("V1", "V2"), sep = well_folder) %>%
    # remove trailing path separator which is not accepted under Windows
    mutate(V1 = stringr::str_replace(string = V1, pattern = paste0(.Platform$file.sep, "$"),
                                     replacement = "")) %>%
    as.data.frame()

  # this is critical

  stopifnot(ncol(datad) == 2)

  # # determine common basis and replace with "."
  #
  # folderlist <- stringr::str_split(datad$V1, pattern = .Platform$file.sep, simplify = TRUE)
  # foldersame <- apply(folderlist, MARGIN = 2, FUN = dplyr::n_distinct)
  # foldersame <- folderlist[1, which(foldersame == 1)]
  #
  # # replace the most basic tree
  #
  # if (length(foldersame) > 0) datad$V1 <- sapply(datad$V1, sub, pattern = paste0(
  #   foldersame, collapse = .Platform$file.sep), replacement = ".", fixed = TRUE)

  datad$V1 <- sapply(datad$V1, sub, pattern = normalizePath(getwd()),
                     replacement = ".", fixed = TRUE)

  datad <- datad %>%
    # unnest the file groups
    dplyr::mutate(
      nest_level = stringr::str_count(V1, pattern = .Platform$file.sep),
      subs_level = stringr::str_count(V2, pattern = .Platform$file.sep)
    ) %>%
    tidyr::separate(V1, into = paste0("grp_", 0:max(.$nest_level)),
                    sep = .Platform$file.sep, fill = "right", extra = "drop",
                    remove = FALSE) %>%
    dplyr::mutate_all(list(~ na_if(., ""))) %>%
    dplyr::rename(group = V1) %>%
    # remove trailing path separators
    dplyr::mutate(group = stringr::str_replace(group, paste0(.Platform$file.sep,
                                                             "$"), "")) %>%
    # unnest the sample specification
    tidyr::separate(V2, into = paste0("sub_", 0:max(.$subs_level)),
                    sep = .Platform$file.sep, fill = "right", extra = "drop") %>%
    # extract well number
    dplyr::mutate(
      well_let = stringr::str_extract(sub_0, stringr::regex("[A-Z]+", ignore_case = TRUE)),
      well_num = sprintf("%02d", as.numeric(stringr::str_extract(sub_0, "[0-9]+$"))),
      well = paste0(well_let, well_num)) %>%
    # add group-relative file path
    dplyr::bind_cols(path = sapply(file_list, function(x) x[[1]])) %>%
    # preserve the file index
    dplyr::mutate(findex = dplyr::row_number()) %>%
    # preserve the group index
    dplyr::group_by(dplyr::across(tidyselect::starts_with("grp_"))) %>%
    dplyr::mutate(gindex = dplyr::cur_group_id()) %>%
    # sort by well in alphabetical order (in case needed); replicates are at
    # level sub_1; if no subfolder exists, sub_1 will equal "file", which is
    # acceptable
    dplyr::arrange(well, sub_1, .by_group = TRUE) %>%
    # group replicates per well
    dplyr::group_by(well, .add = TRUE) %>%
    # add replicate information
    dplyr::mutate(replicate = sub_1,
                  n_replicates = dplyr::n_distinct(replicate)) %>%
    dplyr::group_by(replicate, .add = TRUE) %>%
    dplyr::select(!tidyselect::starts_with("sub_"))

  # if spectra were averages, we adjust replicate and path information

  if (any(lengths(file_list) > 1)) {

    datad$n_replicates <- lengths(file_list)
    if ("replicate" %in% pattern_subtree) datad$replicate[which(lengths(file_list) > 1)] <- NA
    # not sure if this may cause complications later
    datad$path <- file_list

  }

  # if grp_0 is not the only group, drop it

  if (max(datad$nest_level) > 1) {

    datad <- datad %>% dplyr::ungroup(grp_0) %>% dplyr::select(-grp_0)

  }

  datad <- datad %>%
    dplyr::select(!tidyselect::any_of(c("nest_level", "subs_level"))) %>%
    dplyr::select(dplyr::group_vars(datad),
                  tidyselect::any_of("n_replicates"),
                  findex, gindex, tidyselect::everything())

  # attr(datad, "dir") <- paste0(foldersame, collapse = .Platform$file.sep)

  attr(datad, "dir") <- normalizePath(getwd())

  return(datad)

}
