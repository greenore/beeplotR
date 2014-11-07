#' @title Create a beeswarm Plot
#' @export
#' 
#' @description \code{beePlot}
#'   
#' @param id
#' @param category
#' @param num_var
#' @param rank
#' @param data
#' @param color
#' @param range
#' @param conf
#' @param median
#' @param mean
#' @param label
#' @param bee_plot
#' @param cex
#' @param cex.axis
#' @param cex.lab
#' @param cex.med
#' @param cex.minmax
#' @param lwd.med
#' @param method
#' @param corral
#' @param las
#'

beePlot <- function(id, category, num_var, rank, data, color, range=c(0, max),
                    conf=TRUE, median=TRUE, mean=FALSE, label=TRUE, bee_plot=TRUE,
                    cex=6, cex.axis=1.8, cex.lab=1.4, cex.med=2.2,
                    cex.minmax=1.3, lwd.med=3, las=1, method='center', corral="random"){
  require(visualizeR)
  
  # Ceiling
  data[, num_var] <- as.numeric(data[, num_var])
  df <- data[!is.na(data[, num_var]), ]
  df <- df[!is.na(df[, rank]), ]
  
  max <- ceiling(max(df[, num_var])/100)*100
  
  # Number of Top 3
  n_top_3 <- table(data[, rank], data[, category])
  n_top_3 <- colSums(n_top_3[1:3, ])/colSums(n_top_3)
  n_top_3 <- n_top_3[order(n_top_3, decreasing=TRUE)]
  
  # Order according to Top 3
  df[, category] <- factor(df[, category], levels=names(n_top_3))
  
  # Aggregation
  aggPrime <- aggFun(praemie=num_var, category=category, data=df)
  
  # Plotting
  par(mar=c(8, 6, 4, 2) + 0.1)
  
  if (bee_plot == FALSE){
    boxplot(eval(parse(text=textFun(num_var, category))), data=df, border=FALSE,
            cex=cex, las=las, cex.axis=cex.axis, ylim=range)
  }
  
  # Beeplot
  if (bee_plot){
    col <- as.character(df[, color])
    
    set.seed(123)
    bee <- beeswarm(eval(parse(text=textFun(num_var, category))), data=df,
                    vertical=TRUE, spacing=0.7, cex=cex,
                    pwcol=col, pch=18,
                    xlab='', ylab='', main='',
                    ylim=range, las=las, cex.axis=cex.axis,
                    pwbg=as.character(df[, id]),
                    method=method, corral=corral)
    
    # Label: Numbers
    if (label){
      text(bee, labels=bee$bg, cex=cex.lab, col='white', font=2)
    }
    
    # Line: Median
    if (median){
      boxplot(eval(parse(text=textFun('Median', category))), add=TRUE, data=aggPrime,
              xlab="", ylab="", axes=F, border='black', boxwex=.8, lwd=lwd.med, ylim=range)
    }
    
    if (mean){
      boxplot(eval(parse(text=textFun('Mean', category))), add=T, data=aggPrime,
              xlab="", ylab="", axes=F, border=balCol('rot'), boxwex=.8, lwd=lwd.med, ylim=range)
    }
    
    # Label: Median
    if (median){
      lab <- paste('Median', '=', sep='')
      text(rep(0.5 * range[2]/100, length(levels(df[, category]))),
           labels=paste(lab, round(aggPrime$Median, 0), '.-', sep=''),
           cex=cex.med, col='black', font=2)
    }
    
    # Conf Intervall
    col <- ifelse(conf, rgb(0/255, 51/255, 153/255, alpha=0.3), NA)
    box <- boxplot(eval(parse(text=textFun(num_var, category))), data=df, plot=FALSE)
    rect((1:length(levels(df[, category]))) - 0.45, box$conf[1, ],
         (1:length(levels(df[, category]))) + 0.45, box$conf[2, ],
         col=col, border=NA)
    
    # Label: CI_L & CI_U
    if (conf){
      text(rep(-2.5 * range[2]/100, length(levels(df[, category]))),
           labels=paste('95% CI: [', round(box$conf[1, ], 0), '.- ; ',
                        round(box$conf[2, ], 0), '.-]', sep=''),
           cex=cex.minmax, col='black', font=2)
    }
    
    # Label: Min Max
    if (conf == FALSE & median){
      text(rep(-2.5 * range[2]/100, length(levels(df[, category]))),
           labels=paste('(Min = ', round(aggPrime$Min, 0), '.-',
                        '  Max = ', round(aggPrime$Max, 0), '.-)', sep=''),
           cex=cex.minmax, col='black', font=2)
    }
  }
}
