rescale <- function(scaled_data,
                    mean = ifelse(is.null( attr(scaled_data, "scaled:center")), 0, attr(scaled_data, "scaled:center")),
                    scale = ifelse(is.null( attr(scaled_data, "scaled:scale")), 1, attr(scaled_data, "scaled:scale"))){

  rs <- scaled_data * scale + mean
  attributes(rs) <- NULL
  rs
}
