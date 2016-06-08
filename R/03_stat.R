#' @name row-test
#'
#' @title A wrapper to conduct t-test/F-test row-by-row
#'
#' @param data matrix/data.frame
#' @param class vector/factor for class label
#' @param na.rm logical FALSE. should remove NA before test?
#' @param pair paired vector
#' @param var.equal Logical FALSE
#' @param base the base for input data to calculate fold change
#' @return data.frame with statistics, df and pvalues,fc
#'
#' @rdname row-test
#' @export
row_ttest = function(data,class,pair=NULL,na.rm=TRUE,var.equal=FALSE,base){
  class = as.factor(class)
  if(is.null(pair)){
    if(var.equal) res = row_t(data,class,na.rm = na.rm) else
      res = row_t_unequal(data,class,na.rm = na.rm)
  }else{# paired data
    res = row_t_paired(data,class,pf = pair,na.rm=na.rm)
  }
  if(is.vector(data)) m = t(tapply(data, class, mean))else{
  v = split(1:ncol(data),class) # divide index by class
  m = sapply(v,function(x)rowMeans(data[,x,drop=FALSE],na.rm = na.rm)) # get class mean
  }
  del = col_diff_pair(m)

  fc = round(sign(del)*base^abs(del),2)
  colnames(fc) = "fc"
  rlt = data.frame(statistics = res$ts,p.values=res$p.values ,m,del,
                   fc ,Significance=p2f(res$p.values),test="ttest")
  rownames(rlt) = rownames(data)
  return(rlt)
}

#' @export
#' @rdname row-test
#'
row_ftest = function(data,class,na.rm = TRUE,var.equal=FALSE,base){
  class = as.factor(class)
  if(is.vector(data)) xx = t(data) else xx = data
  res = genefilter::rowFtests(x = xx,fac=class,var.equal = var.equal)
  colnames(res) = c("statistics","p.values")
  # calculate mean and fold change
  if(is.vector(data)) m = t(tapply(data, class, mean))else{
    v = split(1:ncol(data),class) # divide index by class
    m = sapply(v,function(x)rowMeans(data[,x,drop=FALSE],na.rm = na.rm)) # get class mean
  }
  del = col_diff_pair(m)
  fc = round(sign(del)*base^abs(del),2)
  colnames(fc) = paste("foldchange",colnames(fc),sep="_")
  rlt = data.frame(res,m,del,fc)
  rlt$Significance = p2f(rlt$p.values)
  rlt$test = "ftest"
  rlt
}

#' @export
row_cortest = function(xmtx,yvec,method="pearson",use = "pairwise"){
  #require(psych)
  xmtx = t(xmtx)
  yvec = as.numeric(matrix(yvec,ncol = 1))
  res = psych::corr.test(x=xmtx,y=yvec,use=use,method=method,adjust="none",alpha=alpha,ci=FALSE)
  rlt = data.frame(statistics = res$r,p.values=res$p,Significance = p2f(res$p))
  rlt$test = "correlation"
  rlt$type = method
  #class(rlt)=append(class(rlt),"testResult")
  rlt
}

#' @export
mtxRow_cortest = function(xmtx, ymtx, method = "pearson", use = "pairwise",
                          adjust = "none", alpha = 0.05, ci = FALSE){
  x = t(xmtx)
  y = t(ymtx)
  res = psych::corr.test(x=x,y=y,use=use,method=method,adjust=adjust,alpha=alpha,ci=ci)
  return(res)
}
