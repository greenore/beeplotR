#' @title Create a beeswarm Plot
#' @export
#' 
#' @description \code{beePlot}
#'   
#' @param id
#' @param category
#' @param praemie
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

beePlot <- function(id, category, praemie, rank, data, color, range = c(0, max),
                    conf = T, median = T, mean = F, label = T, bee_plot = T,
                    cex = 6, cex.axis = 1.8, cex.lab = 1.4, cex.med = 2.2, 
                    cex.minmax = 1.3, lwd.med = 3, las = 1, method = 'center', corral = "random"){
  
  # Ceiling
  data[, praemie] <- as.numeric(data[, praemie])
  df <- data[!is.na(data[, praemie]), ]
  
  max <- ceiling(max(df[, praemie])/100)*100
  
  # Aggregation
  aggPrime <- aggFun(praemie = praemie, category = category, data = df)
  aggRank <- aggFun(praemie = rank, category = category, data = df)
  names(aggRank)[names(aggRank) == 'Mean'] <- 'averageMean'
  
  # Merge
  df <- merge(df, aggRank[, c(category, 'averageMean')], by = category)
  aggPrime <- merge(aggPrime, aggRank[, c(category, 'averageMean')], by = category)
  
  # Order according to mean rank
  aggPrime[, category] <- reorder(aggPrime[, category], aggPrime$averageMean, FUN = 'mean')
  aggPrime <- aggPrime[order(aggPrime$averageMean), ]
  df[, category] <- reorder(df[, category], df$averageMean, FUN = 'mean')
  
  # Plotting
  par(mar = c(5, 6, 4, 2) + 0.1)
  
  if (bee_plot == F){
    boxplot(eval(parse(text = textFun(praemie, category))), data = df, border = F,
            cex = cex, las = las, cex.axis = cex.axis, ylim = range)
  }
  
  # Beeplot
  if (bee_plot == T){
    col <- as.character(df[, color])
    
    set.seed(123)
    bee <- beeswarm(eval(parse(text = textFun(praemie, category))), data = df, 
                    vertical = T, spacing = 0.7, cex = cex,
                    pwcol = col, pch = 18,
                    xlab = '', ylab = '', main = '',
                    ylim = range, las = las, cex.axis = cex.axis,
                    pwbg = as.character(df[, id]), 
                    method = method, corral = corral)
    
    # Label: Numbers
    if (label == T){
      text(bee, labels = bee$bg, cex = cex.lab, col = 'white', font = 2)
    }
    
    # Line: Median
    if (median == T){
      boxplot(eval(parse(text = textFun('Median', category))), add = T, data = aggPrime,
              xlab = "", ylab = "", axes = F, border = 'black', boxwex = .8, lwd = lwd.med, ylim = range)
    }
    
    if (mean == T){
      boxplot(eval(parse(text = textFun('Mean', category))), add = T, data = aggPrime,
              xlab = "", ylab = "", axes = F, border = balCol('rot'), boxwex = .8, lwd = lwd.med, ylim = range)
    }
    
    # Label: Median
    if (median == T){
      lab <- paste('Median', ' = ', sep = '')
      text(rep(0.5 * range[2]/100, length(levels(df[, category]))),
           labels = paste(lab, round(aggPrime$Median, 0), '.-', sep = ''),
           cex = cex.med, col = 'black', font = 2)
    }
    
    # Conf Intervall
    col <- ifelse(conf, rgb(0/255, 51/255, 153/255, alpha = 0.3), NA)
    box <- boxplot(eval(parse(text = textFun(praemie, category))), data = df, plot = F)
    rect((1:length(levels(df[, category]))) - 0.45, box$conf[1, ],
         (1:length(levels(df[, category]))) + 0.45, box$conf[2, ],
         col = col, border = NA)
    
    # Label: CI_L & CI_U
    if (conf == T){
      text(rep(-2.5 * range[2]/100, length(levels(df[, category]))),
           labels = paste('95% CI: [', round(box$conf[1, ], 0), '.- ; ',
                          round(box$conf[2, ], 0), '.-]', sep = ''),
           cex = cex.minmax, col = 'black', font = 2)
    } 
    
    # Label: Min Max
    if (conf == F & median == T){
      text(rep(-2.5 * range[2]/100, length(levels(df[, category]))),
           labels = paste('(Min = ', round(aggPrime$Min, 0), '.-',
                          '  Max = ', round(aggPrime$Max, 0), '.-)', sep = ''),
           cex = cex.minmax, col = 'black', font = 2)
    } 
  }
}