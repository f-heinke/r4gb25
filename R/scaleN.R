scaleN <- function(x, center = T, scale = T){
  cx <- ifelse(center, mean(x), 0)
  sx <- ifelse(scale,  sdN(x), 1)

  z <- (x - cx) / sx
  if(center) attr(z, "scaled:center") <- cx
  if(scale)  attr(z, "scaled:scale")  <- sx
  return(z)
}
