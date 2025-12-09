#' Implementation of the Grassberger-Procaccia algorithm
#'
#' @param D A symmetric distance / dissimilarity matrix of N objects
#' @param r Optional positive vector of distance cutoffs at which the correlation integral is approximated
#'
#' @returns List of results
#' @export
#'
#' @examples
gp <- function(D, r = NULL){

  D <- as.matrix( D )
  N <- nrow(D) # num of data points
  Np <- N*(N-1) # weighting the approx. integral through the number of unique data pairs
  Ds <- D[upper.tri(D)] # extracting only unique, non-diagonal elements of the distance matrix. Remember d_ii = 0 and d_ij = d_ji, thus only d_ij with i > j are of interest.

  if(is.null(r)){
    dmax <- max(D)
    dmin <- min(Ds)*1.001
    # r <- seq(from = dmax, to =  dmin, -(dmax - dmin) / 1000)
    # # decrasing sequence of epsilons, starting from max distance and ending at min. distance in the data
    eps <- seq(from = dmax, to =  dmin, length.out = 1000)

  }


  # for each epsilon, count the number of data points with distance less than epsilon over the entire set of pairwise distances
  cv <- sapply(eps, function(e) sum((Ds < e)))

  # apply weighting factor
  Cr <- 1/Np * cv

  # gather the results
  gpstats <- list(
    Cr = Cr, # approximated correlation integrals for each epsilon
    eps = eps, # the epsilons
    log.eps =  log(eps),
    log.Cr  =  log(Cr),
    eps.min =  last( eps ),
    N =  N
  )
  return(gpstats)
}

#' Graphical analyses of Grassberger-Procaccia approximated cor integral
#'
#' @param gp.result A list of results obtained through the gp function
#' @param ... Options to be passed to plot
#'
#' @returns List of results.
#' @export
#'
#' @examples
gp_graph_analyses <- function(gp.result, ...){
  x11()
  plot(gp.result$log.eps, gp.result$log.Cr, xlab = "log(e)", ylab = "log(C_e)", ...)

  pident <<- sort( identify(gp.result$log.eps, gp.result$log.Cr))

  le_sel <- gp.result$log.eps[ pident[1]:pident[2]]
  lCr_sel <- gp.result$log.Cr[ pident[1]:pident[2]]

  lmfit <- lm(lCr_sel ~ le_sel)

  fit_slope <- lmfit$coefficients[2]

  dc <- fit_slope
  emin <- gp.result$eps.min

  camastra.index <- 2/log(1 / emin) * log(gp.result$N)

  x11()
  plot(gp.result$log.eps, gp.result$log.Cr,
       type = "b",
       col = 8,
       ylim = c(min(gp.result$log.Cr),0),
       xlab = "log(e)", ylab = "log(C_e)")

  abline(h = 0, col = 8)
  abline(lmfit, col = 4, lwd =2)

  legend("bottomright",
         legend = c(
           paste0("dc = ", round(dc,3)),
           paste0("di = ", round(dc,0)),
           paste0("C-idx  = ", round(camastra.index,3))
         ))

  res.analyses <- list(
    dc = dc,
    di = round(dc, 0),
    camastra.index = camastra.index
  )
  res.analyses
}

