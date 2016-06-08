
#----------------------------------
# plot scatter plot
#----------------------------------
#' @export
plot_scatter = function(x,y,cex = 1,...){
  # x and y are two numerical vector
  x = as.numeric(x)
  y = as.numeric(y)

  cor_pearson  = cor.test(x, y, method="pearson")
  cor_spearman = cor.test(x, y, method="spearman",exact = FALSE)
  lm_xy = summary(lm(y~x))
  e_pearson = round(cor_pearson$estimate,3)
  e_spearman = round(cor_spearman$estimate,3)
  p_pearson  = format.pval(cor_pearson$p.value,3)
  p_spearman = format.pval(cor_spearman$p.value,3)

  p_lm = format.pval(lm_xy$coefficients[2,4],3)
  r2_lm = round(lm_xy$adj.r.squared,3)

  mymain = paste("Pearson Correlation: ", "r = ",e_pearson,", p =",p_pearson,
                 "\nSpearman Correlation: ", " rho = ",e_spearman,", p =",p_spearman,
                 "\nLinear Model: ", "Adj-Rsquare = ",r2_lm,", p =", p_lm )
  plot(x,y,panel.first = grid(5, 5),pch=19,col="gray21",cex.axis=cex,...)
  title(mymain,cex.main=cex)
  abline(lm(y~x),col="blue",lty=3,lwd=2)
}

#----------------------------------
# plot Concordance Plot
#----------------------------------
#' @export
plot_ccc = function (v1, v2, conf.level = 0.95, cex = 0.8,
                     tag = "", main = NA,#sample_name = NA,
                     xlab = NA, ylab = NA, plot = T, MDplot = F,
                     xlim = NULL,ylim = NULL, text_col = "red",pt_col = "gray50",
                     pch = 21,lty = c(1,2),lcol = c("blue","red"),alpha = 0.05 ,...)
{
  v1 = as.vector(v1)
  v2 = as.vector(v2)

#   if (is.na(xlab)) xlab = deparse(substitute(v1))
#   if (is.na(ylab)) ylab = deparse(substitute(v2))

  if(is.null(xlim)) xlim = range(v1,na.rm = T)else xlim = xlim
  if(is.null(ylim)) ylim = range(v2,na.rm = T)else ylim = ylim

  if (length(v1) != length(v2)) stop("Length of v1 and v2 is unequal!")
  ccc = epiR::epi.ccc(x = v1, y = v2, ci = "z-transform", conf.level = conf.level)
  # ccc summary
  tt = unlist(ccc$rho.c)
  scale_shift = ccc$s.shift
  loc_shift = ccc$l.shift
  ccc_est = tt["est"]
  ccc_lo = tt["lower"]
  ccc_hi = tt["upper"]
  Cb = ccc$C.b

  ccc_rlt = c(ccc_est, scale_shift, loc_shift, ccc_lo, ccc_hi, Cb)
  names(ccc_rlt) <- c("ccc_est", "scale_shift", "loc_shift", "ccc_lo", "ccc_hi", "bcf")

  md_data = data.frame(v1 = v1,v2 = v2, mean = (v1+v2)/2,delta = (v2 - v1))

  # stat summary
  mean_v1 <- mean(v1, na.rm = TRUE)
  mean_v2 <- mean(v2, na.rm = TRUE)
  sd_v1 <- sd(v1, na.rm = TRUE)
  sd_v2 <- sd(v2, na.rm = TRUE)
  stat_summary = c(mean_v1 = mean_v1, mean_v2 = mean_v2, sd_v1 = sd_v1, sd_v2 = sd_v2)

  # correlation test
  cor_pearson  = cor.test(v1, v2, method="pearson")
  cor_spearman = cor.test(v1, v2, method="spearman",exact = FALSE)
  e_pearson = round(cor_pearson$estimate,3)
  e_spearman = round(cor_spearman$estimate,3)
  p_pearson  = format.pval(cor_pearson$p.value,3)
  p_spearman = format.pval(cor_spearman$p.value,3)
  #cor_rlt = data.frame(pearson_r = e_pearson, pearson_pval = p_pearson, spearman_rho = e_spearman,spearman_pval = p_spearman)
  cor_rlt = c(pearson_r = e_pearson, pearson_pval = cor_pearson$p.value,
              spearman_rho = e_spearman,spearman_pval = cor_spearman$p.value)
  names(cor_rlt) = c("pearson_r","pearson_pval","spearman_rho","spearman_pval")

  lab = paste("CCC: ", round(ccc_rlt["ccc_est"], digits = 2),
              " (", conf.level * 100, "%",
              " CI ", round(ccc_rlt["ccc_lo"], digits = 2), " - ", round(ccc_rlt["ccc_hi"], digits = 2), ")",
              "\n",
              "Scale shift: ", round(ccc_rlt["scale_shift"], 2),
              "\n",
              "Location shift: ", round(ccc_rlt["loc_shift"], 2), sep = "")

  z = lm(v2 ~ v1)

  if (is.na(main)) main = paste("Concordance Plot : ",
                                "CCC = ", round(ccc_rlt["ccc_est"], 2), "\n",
                                "Pearson Cor = ", round(e_pearson, 2),", pvalue: ",p_pearson,"\n",
                                "Spearman Cor = ", round(e_spearman, 2),", pvalue",p_spearman)
  if(plot){
    plot(v1, v2, xlim = xlim ,ylim = ylim, xlab = xlab, ylab = ylab, col = pt_col,
         panel.first = grid(5, 5), pch = pch, cex.axis = cex,...)
    abline(a = 0, b = 1, lty = lty[1], lwd = 2, col = lcol[1])
    abline(z, lty = lty[2], lwd = 2,col = lcol[2])
    title(main = stringr::str_c(tag, "\n", main, collapse = ""), cex.main = cex)
    legend(x = "bottomright", legend = c("Perfect concordance", "Linear Trend"),
           lty = lty, lwd = c(2, 2), col = lcol, bty = "n", cex = cex, text.col = text_col)
    legend("topleft", lab, bty = "n", cex = cex, text.col = text_col)
  }

  out_rlt = c(ccc_rlt,stat_summary,cor_rlt)

  if(MDplot) {
    diff_mean = mean(md_data$delta, na.rm = TRUE)
    diff_sd = sd(md_data$delta, na.rm = TRUE)
    extremes = qnorm(c(alpha/2, 1 - alpha/2), diff_mean, diff_sd)
    md_data$isOutlier = md_data[, "delta"] < extremes[1] | md_data[, "delta"] > extremes[2]
    gtitle = stringr::str_c(tag,"\n",sprintf("%d possible outliers at significance level of %.3f",
                                    sum(md_data$isOutlier, na.rm = TRUE), alpha, collapse = ""))
    ylim = c(min(c(extremes, md_data$delta), na.rm = TRUE),
             max(c(extremes, md_data$delta), na.rm = TRUE))
    plot(md_data$mean,md_data$delta,main= gtitle,ylim = ylim, cex.axis = cex,pch = pch,col= pt_col,
         xlab = "Average of two measurement", ylab = "Difference between two measurements")
    abline(h = extremes, lty = 2, col = "red")
    abline(h = diff_mean, lty = 2, col = "black")
    outlier_id = which(md_data$isOutlier)
    out_rlt = list(summary = c(ccc_rlt,stat_summary,cor_rlt),md_data = md_data,outlier_id = outlier_id)
  }
  # save result
  invisible(out_rlt)
}

#----------------------------------
# plot density plot
#----------------------------------
#' @export
plot_density = function(x,class,na.rm=FALSE,title="",showBI=TRUE,showRug=TRUE,xlab=NULL,...){
  # x is vector
  x = matrix(x,nrow=1)
  nna = length(which.na(x))
  if(nna & na.rm) warning("there exists ",nna," NA in x, dropped for density and bi")
  if(showBI){
    bi = bindex(x,na.rm = na.rm,verbose = F)
    bi_main = paste("BI = ", round(bi$BI,digits = 2),sep="" )
  } else bi_main =""
  mymain = paste(title,bi_main)
  hist(x,freq=FALSE,col="gray90",breaks=pretty(x,n=20),main=mymain,xlab=xlab,...)
  lines(density(x,na.rm = na.rm),lwd=2,col="red")
  if(showRug){
  if(!missing(class)){
    class = as.factor(class)
    for (i in 1:nlevels(class)) rug(x[class==levels(class)[i]],col = i)
    }else rug(x,col = "blue")
  }
}

#' @export
plot_multi_density = function(data,showArea = TRUE,alpha=0.5,xlab = "",
                              legend_pos = "top",legend_title = "group"){
  xx = reshape2::melt(data,id.vars = NULL)
  p = ggplot(xx,aes(x= value, col=variable ,fill=variable))
  if(showArea) p = p + geom_density(alpha=alpha,show.legend =FALSE)
  p = p + stat_density(aes(x=value, colour=variable),geom="line",position="identity")+
    #theme_classic() +
    xlab(xlab)+
    guides(color=guide_legend(title=legend_title)) +
    theme(legend.position = legend_pos)
  p
}

#----------------------------------
# plot waterfall plot
#----------------------------------
#' @export
plot_waterfall = function(test_rlt,xlab="",ylab="",title="",cex = 5){
  test_rlt$labs = rownames(test_rlt)
  if(test_rlt$test[1] == "ttest") {
    p = ggplot(test_rlt,aes(x = reorder(labs,fc),y = fc,fill=Significance),
               environment = environment()) + geom_bar(stat ="identity")
    p = p + coord_flip()+  xlab(xlab) + ylab(ylab) + labs(title=title) + theme_bw() +
    theme(axis.text.y = element_text(colour="grey20",size=cex,angle=0,hjust=1,vjust=0))+
      scale_fill_manual(values = c("darkred","blue", "green","lightgray"),drop = FALSE)
  }
  if(test_rlt$test[1] == "correlation"){
    p = ggplot(test_rlt,aes(x = reorder(labs,statistics),y = statistics,fill=Significance),
               environment = environment()) + geom_bar(stat ="identity")
    p = p + coord_flip() + xlab(xlab) + ylab(ylab) + labs(title=title) + theme_bw() +
      theme(axis.text.y = element_text(colour="grey20",size=cex,angle=0,hjust=1,vjust=0))+
   scale_fill_manual(values = c("darkred","blue", "green","lightgray"),drop = FALSE)
  }
 p
}

#----------------------------------
# boxplot and saving result
# test_rlt: output of test
# exp_dat: corresponding data which used to do test
#----------------------------------
#' @export
batch_boxplot = function(test_rlt,exp_dat,class,index=1:nrow(test_rlt),file,
                         layout=c(1,1),order_by= c("none","pvalue","fc","pfc"),
                         color = 1,xlab="",ylab="",...){
  if(is.null(rownames(exp_dat))) rownames(exp_dat) = rownames(test_rlt)

  #test_rlt = complete_row(test_rlt[index,])  # for some data with NA, no need to drop
  #exp_dat = complete_row(exp_dat[index,])

  order_by = match.arg(order_by)

  if(identical(rownames(test_rlt),rownames(exp_dat))){
    if(test_rlt$test[1] == "ttest"){
      p = order(test_rlt$p.values)
      f = order(abs(test_rlt$fc),decreasing = T)
      pf = order(test_rlt$p.values,abs(test_rlt$fc))
      test_rlt = switch (order_by,
                         none = test_rlt,
                         pvalue = test_rlt[p,],
                         fc = test_rlt[f,],
                         pfc = test_rlt[pf,])

      exp_dat = switch (order_by,
                        none = exp_dat,
                        pvalue = exp_dat[p,],
                        fc = exp_dat[f,],
                        pfc = exp_dat[pf,])
      mainTitleUsed = paste(rownames(test_rlt),", t-test p-value = ", format.pval(test_rlt$p.values,digits = 3),"\n",
                            "fc = ", test_rlt$fc)
    }else if(test_rlt$test[1]=="ftest"){
      if(!(order_by %in% c("none","pvalue"))) stop("ftest order by p-values only")
      p = order(test_rlt$p.values)
      test_rlt = switch (order_by,
                         none = test_rlt,
                         pvalue = test_rlt[p,])
      exp_dat = switch (order_by,
                        none = exp_dat,
                        pvalue = exp_dat[p,])
      mainTitleUsed = paste(rownames(test_rlt),", ANOVA p-value = ", format.pval(test_rlt$p.values,digits = 3))
    }

    pdf(file)
    par(mfrow=layout)
    for (i in 1:nrow(test_rlt)){
      dat = as.numeric(exp_dat[i,])
      maxvalue = max(dat,na.rm = T)
      minvalue = min(dat,na.rm = T)
      boxplot(dat ~ class,main=mainTitleUsed[i] , ylim=c(minvalue, maxvalue),
              xlab=xlab , ylab= ylab, outline=FALSE,
              cex.main=0.7, cex.axis=0.7, cex.lab=0.7, boxwex = 0.4,...)
      stripchart(dat ~ class, vertical=TRUE,pch=16,cex=1,method='jitter', add=TRUE,col=color)
    }
    dev.off()
  }else warning("check inputs")
}

#----------------------------------
# scatterplot for correlation analysis and saving result
# test_rlt: output of correlation test
# exp_dat: corresponding data which used to do test
#----------------------------------
#' @export
batch_scatter = function(test_rlt,exp_dat,yvec,index=1:nrow(test_rlt),file,
                         layout=c(1,1),order_by= c("none","pvalue","cor","pcor"),
                         cex = 0.8,ylab="",...){
  if(is.null(rownames(exp_dat))) rownames(exp_dat) = rownames(test_rlt)
  test_rlt = complete_row(test_rlt[index,])
  exp_dat = complete_row(exp_dat[index,])

  yvec = as.vector(yvec)

  order_by = match.arg(order_by)
  if(identical(rownames(test_rlt),rownames(exp_dat))){
    if(test_rlt$test[1] == "correlation"){
      p = order(test_rlt$p.values)
      r = order(abs(test_rlt$statistics),decreasing = T)
      pr = order(test_rlt$p.values,abs(test_rlt$statistics))
      test_rlt = switch (order_by,
                         none = test_rlt,
                         pvalue = test_rlt[p,],
                         cor = test_rlt[r,],
                         pcor = test_rlt[pr,])

      exp_dat = switch (order_by,
                        none = exp_dat,
                        pvalue = exp_dat[p,],
                        cor = exp_dat[r,],
                        pcor = exp_dat[pr,])
      mainTitleUsed = paste(test_rlt$type[1]," correlation: ", round(test_rlt$statistics,3),
                            ", p-value = ", format.pval(test_rlt$p.values,digits = 3))
    }
    pdf(file)
    par(mfrow=layout)
    for (i in 1:nrow(test_rlt)){
      dat = as.numeric(exp_dat[i,])
      maxvalue = max(dat,na.rm = T)
      minvalue = min(dat,na.rm = T)
      plot(dat,yvec,panel.first = grid(5, 5),pch=19,col="gray21",cex.axis=cex,ylab=ylab,
           xlab=rownames(test_rlt)[i],...)
      title(mainTitleUsed[i],cex.main=cex)
      abline(lm(yvec~dat),col="blue",lty=3,lwd=2)
    }
    dev.off()
  } else warning("check inputs")
}

#' @export
bum_table = function(pvals, method='FDR',
                     fdrs=c(0.01, 0.05, 0.10, 0.20), pin = c(0.001,0.01,0.05,0.1),
                     res=100, heights=c(3,1), main="Bum Plot",plot=TRUE) {
  a = ClassComparison::Bum(pvals)
  counts = sapply(fdrs, function(alpha) ClassComparison::countSignificant(a, alpha, by=method))
  cuts = sapply(fdrs, function(alpha) ClassComparison::cutoffSignificant(a, alpha, by=method))
  mat = data.frame(fdrs, counts, format.pval(cuts,4))
  colnames(mat) = c(paste(method, "s",sep=''), "n_sig", "pvalues")

  # bum plot
  if(plot == TRUE){
    fdat = data.frame(pvals=pvals)
    xvals = (0:res)/res
    fit = a@lhat + (1 - a@lhat) * dbeta(xvals, a@ahat, 1)
    betaf = data.frame(xvals=xvals, fit=fit)[-1,]
    den.plot = ggplot2::ggplot(fdat,aes(x=pvals)) +
      geom_histogram(aes(y = ..density..), colour="black", fill = "white", binwidth = 0.01) +
      geom_line(data=betaf, aes(xvals,fit), colour="darkgreen") +
      geom_hline(yintercept = a@pihat, colour="blue") +
      labs(x="P Values", y="Density", title=main) +
      theme_classic()
    # output tablet
    ptab = data.frame(pvalues=pin, n_in_set=sapply(pin,function(x)length(which(pvals<=x))))
    ptab.plot = gridExtra::tableGrob(ptab)
    ftab.plot = gridExtra::tableGrob(mat)
    gridExtra::grid.arrange(den.plot, gridExtra::grid.arrange(ftab.plot,ptab.plot,nrow=1,widths=unit(c(0.4,0.4),"npc")),
                 ncol=1, heights=heights)
  }

  # select gene id
  gid.adjust = sapply(fdrs,function(alpha)ClassComparison::selectSignificant(a,alpha,by=method))
  colnames(gid.adjust) = paste("gidAdj",fdrs,sep="")
  gid.p = sapply(pin, function(x)pvals <= x)
  colnames(gid.p) = paste("gidP",pin,sep="")
  gid = data.frame(gid.adjust,gid.p)
  invisible(gid)
}



