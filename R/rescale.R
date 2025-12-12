#' Rescale z-scaled data
#'
#' @param scaled_data
#' @param mean Provide a mean for rescaling. Otherwise attribute mean data is read from the scaled_data object
#' @param scale Provide a standard deviation for rescaling. Otherwise attribute std data is read from the scaled_data object
#'
#' @returns
#' @export
#'
#' @examples
rescale <- function(scaled_data,
                    mean = ifelse(is.null( attr(scaled_data, "scaled:center")), 0, attr(scaled_data, "scaled:center")),
                    scale = ifelse(is.null( attr(scaled_data, "scaled:scale")), 1, attr(scaled_data, "scaled:scale"))){

  rs <- scaled_data * scale + mean
  attributes(rs) <- NULL
  rs
}
