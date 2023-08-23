#' Load a matrix from a file
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param rdsdir path to the output rds file
#' @return saves rds object for plotreading test
#' @export
saveRDSforplotread <- function(rdsdir, os = "macOS"){
  plotread_txt <- Sys.glob("*.txt")
  ess_plotread_validation_input <- map(seq_along(plotread_txt), ~ read_fwf(plotread_txt[.], col_positions = fwf_widths(rep(1, 90))) %>% .[,which(.[3,] == "*")])
  names(ess_plotread_validation_input) <- plotread_txt
  osmark <- ifelse(os == "macOS", "/", "\\")
  saveRDS(ess_plotread_validation_input, file = file.path(rdsdir, "ess_plotread_validation_input.rds", fsep = osmark))
}