p_load <- function(...){
  pkgs <- as.character(match.call(expand.dots = FALSE)[[2]])
  pkgs_avail <- pkgs %in% .packages(all.available = TRUE)
  install.packages(pkgs[!pkgs_avail])
  lapply(pkgs, library, character.only = TRUE)
  return()
}


write_hc_json <- function(hc, path){
  
}
