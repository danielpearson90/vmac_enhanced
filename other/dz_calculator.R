#' Calculator for d<sub>z</sub> (within subjects effect size)
#'
#' Calculator for d<sub>z</sub> (within subjects effect size)
#' @param x,y vectors of the DV for each level of the IV
#' @export
#' @examples dz_calculator()

dz_calculator <- function(x,y) {

  out <- tibble::tibble(contrast = x - y) %>%
    dplyr::summarise(mean_contrast = mean(contrast), sd_contrast = sd(contrast), d_z = mean_contrast/sd_contrast) %>% dplyr::pull(d_z)
  return(abs(out))


}
