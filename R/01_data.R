
#' @export
# return NA index in a vector
which.na = function(x) (1:length(x))[is.na(x)]

#' @export
# drop NA in vectors
dropNA = function(x)x[!is.na(x)]

#' @export
# drop na row
complete_row = function(df){
  df[complete.cases(df),]
}

#' @export
# grep exact match, return the index
grep_exact = function(string,string_set){
  #which(string_set ==string)
  string = paste("^",string,"$",sep="")
  grep(string,string_set)
}

#' @export
# return top value index
# default: top smallest with NA dropped
top_nid = function(p,n,na.last = NA,increasing=TRUE,...){
  top_id = order(p,na.last = na.last,decreasing = !increasing,...)
  head(top_id,n)
}

#' @export
# return pairwise difference of columns in data.frame/matrix
col_diff_pair = function(df){
  if(is.vector(df)) df = matrix(df,nrow=1)
  df = as.data.frame(df)
  nm = outer(colnames(df), colnames(df), paste, sep="_")
  drop_id =  which(upper.tri(nm, diag=TRUE))
  res = outer(1:ncol(df), 1:ncol(df), function(x,y){df[,x]- df[,y]})
  colnames(res) = nm
  res[-drop_id]
}

#' @export
# cut p values into sig categories
p2f = function(pvals,breaks=c(0,0.001,0.01,0.05,1)){
  p = cut(pvals,breaks=breaks,labels=c("p<0.001","p<0.01","p<0.05","p>0.05"),include.lowest = TRUE)
  factor(p,levels = c("p<0.001","p<0.01","p<0.05","p>0.05"))
}

#' @export
#  name for cell lines
clean_name = function(x,cap=TRUE){
  if(cap) gsub("[[:punct:][:space:]]", "",toupper(x)) else gsub("[[:punct:][:space:]]", "",x)
}


