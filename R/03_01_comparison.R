
#' @name t-test
#'
#' @title conduct t-test row-by-row
#'
#' @param m matrix
#' @param v vector for class label
#' @param na.rm logical FALSE
#' @param pf paired vector
#' @return list with statistics, df and pvalues
#'
#' @rdname t-test
#' @export
#' @examples
#' x = matrix(rnorm(120),20,6)
#' y = rep(1:2,each=3)
#' z = rep(1:3,2)
#' t1 = row_t(x,y)
row_t = function (m, v, na.rm = FALSE) {
  if(is.vector(m)) m = matrix(m,nrow=1)
  v = factor(v)
  v <- v == levels(v)[1]
  ma = m[,v,drop=FALSE] #keep matrix structure
  mb = m[,!v,drop=FALSE]
  if (na.rm) {
    an = matrixStats::rowCounts(!is.na(ma))
    bn = matrixStats::rowCounts(!is.na(mb))
  }
  else {
    an = sum(v)
    bn = sum(!v)
  }
  am = rowMeans(ma,na.rm=na.rm)
  av = matrixStats::rowVars(ma,na.rm=na.rm)
  bm = rowMeans(mb,na.rm=na.rm)
  bv = matrixStats::rowVars(mb,na.rm=na.rm)

  tt = (am - bm)/sqrt(((an - 1) * av + (bn - 1) * bv)/(an + bn - 2))/sqrt(1/an + 1/bn)
  df = apply(m, 1, function(x) sum(!is.na(x)) - 2)
  p.values = 2 * pt(-abs(tt), df)
  list(ts = tt, df = df,p.values = p.values)
}


#' @rdname t-test
#' @export
#' @examples
#' t2 = row_t_unequal(x,y)
row_t_unequal = function (m, v, na.rm = FALSE){
  if(is.vector(m)) m = matrix(m,nrow=1)
  v = factor(v)
  v <- v == levels(v)[1]
  ma = m[,v,drop=FALSE]
  mb = m[,!v,drop=FALSE]
  if (na.rm) {
    an = matrixStats::rowCounts(!is.na(ma))
    bn = matrixStats::rowCounts(!is.na(mb))
  }
  else {
    an = sum(v)
    bn = sum(!v)
  }
  am = rowMeans(ma,na.rm=na.rm)
  av = matrixStats::rowVars(ma,na.rm=na.rm)
  bm = rowMeans(mb,na.rm=na.rm)
  bv = matrixStats::rowVars(mb,na.rm=na.rm)
  #u <- bv/av
  #df <- trunc((1/an + u/bn)^2/(1/(an^2 * (an - 1)) + u^2/(bn^2 * (bn - 1))))
  #df <- (1/an + u/bn)^2/(1/(an^2 * (an - 1)) + u^2/(bn^2 * (bn - 1)))
  std_a = av/an
  std_b = bv/bn
  tt = (am - bm)/sqrt(std_a + std_b)
  df = (std_a + std_b)^2/(std_a^2/(an-1)+std_b^2/(bn-1))
  p.values = 2 * pt(-abs(tt), df)
  list(ts = tt, df = round(df,2),p.values = p.values)
}

#' @rdname t-test
#' @export
#' @examples
#' t3 = row_t_paired(x,y,z)
#' t3
row_t_paired = function (m, v, pf, na.rm = FALSE) {
  if (is.vector(m)) m = matrix(m, nrow = 1)
  v = factor(v)
  pf = factor(pf)
  #v <- v == levels(v)[1]
  m <- m[, order(v, pf), drop = FALSE]
  v = sort(v)
  id <- v == levels(v)[1]
  am <- m[, id, drop = FALSE]
  bm <- m[, !id, drop = FALSE]
  pd <- am - bm
  pdm <- rowMeans(pd, na.rm = na.rm)
  pdv <- matrixStats::rowVars(pd, na.rm = na.rm)
  n = matrixStats::rowCounts(!is.na(pd), na.rm = na.rm)
  tt = pdm/(sqrt(pdv/(n)))
  df = n - 1
  p.values <- 2 * (1 - pt(abs(tt), df))
  list(ts = tt, df = round(df, 2), p.values = p.values)
}









