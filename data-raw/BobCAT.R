## code to prepare `BobCAT` dataset goes here

BobCAT <- maldi_import_spectra(path = "data-raw/BobCAT/")

# replace path information to refelect current working directory as "."

purge_personal_information <- function(x, old_path, new_path = ".") {

  y <- x

  MALDIquant::metaData(y)$parentFile <- ""
  MALDIquant::metaData(y)$file <- sub(MALDIquant::metaData(x)$file,
                                      pattern = old_path, replacement = new_path,
                                      fixed = TRUE)

  y

}

BobCAT <- lapply(BobCAT, purge_personal_information, old_path = attr(BobCAT, "dir"))

usethis::use_data(BobCAT, overwrite = TRUE)
