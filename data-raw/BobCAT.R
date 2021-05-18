## code to prepare `BobCAT` dataset goes here

BobCAT <- maldi_import_spectra(path = "data-raw/BobCAT/")

usethis::use_data(BobCAT, overwrite = TRUE)
