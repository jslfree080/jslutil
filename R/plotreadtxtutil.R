#' saveRDSforplotread
#'
#' Explain later
#'
#'
#'
#'
#' @param rdsdir path to the output rds file
#' @return saves rds object for plotreading test
#' @export
saveRDSforplotread <- function(rdsdir, os = "macOS") {
  plotread_txt <- Sys.glob("*.txt")
  ess_plotread_validation_input <- purrr::map(seq_along(plotread_txt), ~ readr::read_fwf(plotread_txt[.], col_positions = readr::fwf_widths(rep(1, 90))) %>% .[,which(.[3,] == "*")])
  names(ess_plotread_validation_input) <- plotread_txt
  osmark <- ifelse(os == "macOS", "/", "\\")
  saveRDS(ess_plotread_validation_input, file = file.path(rdsdir, "ess_plotread_validation_input.rds", fsep = osmark))
}

#' prepareRDSforplotread
#'
#' Explain later
#'
#'
#'
#'
#' @param rdsdir path to the output rds file
#' @return preperation for plotreading test
#' @export
prepareRDSforplotread <- function(rdsdir, os = "macOS") {
  ess_plotread_validation_input <- readRDS(file.path(rdsdir, "ess_plotread_validation_input.rds", fsep = osmark))
  plotread_txt <- names(ess_plotread_validation_input)
  sample_vector <- map(names(ess_plotread_validation_input), ~ strsplit(., "[_]")[[1]][4]) %>% unlist()
  ess_plotread_validation_input <- map(seq_along(sample_vector), ~ ess_plotread_validation_input[[.]] %>% as.data.frame())
  normal_index <- grep("N", sample_vector); normal_index <- c(normal_index, grep("neg", sample_vector))
  tumor_index <- seq_along(sample_vector)[!seq_along(sample_vector) %in% normal_index]
  normal_ref <- map( normal_index, ~ as.vector(as.vector(ess_plotread_validation_input[[.]][-c(1:3),1])==as.character(ess_plotread_validation_input[[.]][2,1])) %>% discard(., is.na) %>% mean() ) %>% unlist()
  plotread_txt[normal_index][order(normal_ref)] %>% head(); normal_ref[order(normal_ref)] %>% head()
  tumor_alt <- map( tumor_index, ~ as.vector(as.vector(ess_plotread_validation_input[[.]][-c(1:3),1])!=as.character(ess_plotread_validation_input[[.]][2,1])) %>% discard(., is.na) %>% mean() ) %>% unlist()
  tumor_alt_position <- map(plotread_txt[tumor_index][order(tumor_alt)], ~ strsplit(., "[_]")[[1]][3]) %>% unlist()
  tumor_alt_position
}