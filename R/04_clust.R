
#' Calculate Bimodal Index
#'
#' @title A function to creat new report with required files
#' copied and created to work on
#'
#' @param dataset A matrix or data.frame, usually with columns representing samples
#' and rows representing genes or proteins.
#' @param na.rm Logical. If remove na before calculating bi. If False, bi will not be calculated for
#' any row  with NA,
#' @param verbose A logical value; should the function output a stream of information while it is working?
#' @return
#' Returns a data frame containing six columns, with the rows corresponding to
#'  the rows of the original data set.
#'  The columns contain the four parameters from the normal mixture model (mu1, mu2, sigma, and pi)
#'  along with the standardized distance delta and the bimodal index BI.

#' @export
#' @import mclust
#' @references
#' Wang J, Wen S, Symmans WF, Pusztai L, Coombes KR.
#' The bimodality index: A criterion for discovering and ranking bimodal signatures from cancer gene expression profiling data.
#' Cancer Informatics, 2009 Aug 5; 7:199–216.
#' @note
#' update from bimodalIndex {BimodalIndex}
#'
bindex = function (dataset,na.rm = FALSE, verbose = TRUE)
{
  #require(mclust)
  # better to qc data before calculating bi
  if(is.vector(dataset))dataset=matrix(dataset,nrow=1)
  bim <- matrix(NA, nrow = nrow(dataset), ncol = 6)
  if (verbose)
    cat("1 ")
  for (i in 1:nrow(dataset)) {
    if (verbose && 0 == i%%100) cat(".")
    if (verbose && 0 == i%%1000) cat(paste("\n", 1 + i/1000, " ", sep = ""))

    x <- as.vector(as.matrix(dataset[i, ]))
    #if (any(is.na(x))) next
    if(na.rm) {
      x = dropNA(x) # for drop NA before calculate bi
    } else if(any(is.na(x))) {
      next
      } # if not drop NA, skip this row

    mc <- try(mclust::Mclust(x, G = 2, modelNames = "E"),silent = TRUE)
    if (class(mc) != "Mclust") { # skip rows for failure to fit models
      next
    } else{
      sigma <- sqrt(mc$parameters$variance$sigmasq)
      delta <- abs(diff(mc$parameters$mean))/sigma
      pi <- mc$parameters$pro[1]
      bi <- delta * sqrt(pi * (1 - pi))
    }
    bim[i, ] <- c(mc$parameters$mean, sigma = sigma, delta = delta,
                  pi = pi, bim = bi)
  }
  if (verbose)
    cat("\n")
  dimnames(bim) <- list(rownames(dataset), c("mu1", "mu2", "sigma", "delta", "pi", "BI"))
  bim <- as.data.frame(bim)
  bim
}
