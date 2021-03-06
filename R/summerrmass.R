utils::globalVariables(".")

#' summerrmass-package
#'
#' @author Benjamin Buchmuller \email{benjamin.buchmuller@@tu-dortmund.de}
#'
#' @title summerrmass
#'
#' @description
#'
#' Import and analyze peaks from MALDI mass spectra.
#'
#' @details
#'
#' This package makes heavily use of \link{MALDIquant}. It
#' combines the functionality to pre-assembled "pipelines" that can be readily
#' used to batch process (nested) direcotries containing data from measurments
#' of plates. Each directory can be associated with a user-specified plate layout.
#' The metadata can be used in further analysis steps.
#'
#' @section Routines for MALDI peak calling from Burker FlexAnalysis:
#'
#' \link{maldi_import_spectra}
#'
#' \link{maldi_average_spectra} is extended by \link{maldi_average_by_well}
#'
#' \link{maldi_find_peaks_by_well} and \link{maldi_draw_peaks_by_well}
#'
#' \link{maldi_tapply} a general wrapper to extend/apply functions over groups
#' such as wells
#'
#' \subsection{Convenience Wrappers}{
#'
#' \link{maldi_get_paths}
#'
#' }
#'
#' @section Interactive Workflows:
#'
#' \link{maldi_batch}
#'
#' @docType package
#' @import summerr
#' @name summerrmass-package
NULL

#' Use a template for a MALDI script
#'
#' @param version A template version identifier.
#'
#' @details
#'
#' \describe{
#' \item{A01}{MALDI batch processing for 384-well assay; used in Palei (2021)}
#' }
#'
#' @export
use_template <- function(version = "A01") {

  summerr::get_template(package = "summerrmass", filename = "template",
                        version = version)

}
