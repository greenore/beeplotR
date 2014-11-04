# beeswarm.R
#
# Aron Charles Eklund
#
# A part of the "beeswarm" R package
# 


beeswarm <- function (x, ...) 
  UseMethod("beeswarm")


## here x should be a list or data.frame or numeric
beeswarm.default <- function(x, 
    method = c("swarm", "center", "hex", "square"), 
    vertical = TRUE, horizontal = !vertical, 
    cex = 1, spacing = 1, breaks = NULL,
    labels, at = NULL, 
    corral = c("none", "gutter", "wrap", "random", "omit"),
    corralWidth,
    pch = par("pch"), col = par("col"), bg = NA, 
    pwpch = NULL, pwcol = NULL, pwbg = NULL,
    do.plot = TRUE, add = FALSE, log = FALSE, 
    xlim = NULL, ylim = NULL, dlim = NULL, glim = NULL,
    xlab = NULL, ylab = NULL, dlab = "", glab = "",
    ...) {
    
  method = match.arg(method)
  corral = match.arg(corral)
  if(length(cex) > 1) {
    stop('the parameter "cex" must have length 1')
  }
  if(is.numeric(x)) {
    x <- list(x)
  }

  n.groups <- length(x)

  #### Resolve group labels
  if(missing(labels) || is.null(labels)) {
    if(is.null(names(x))) {
      if(n.groups == 1) {
        labels <- NA
      } else {
        labels <- 1:n.groups
      }
    } else {
      labels <- names(x)
    }
  } else {
    labels <- rep(labels, length.out = n.groups)
  }

  if (is.null(at)) 
    at <- 1:n.groups
  else if (length(at) != n.groups) 
    stop(gettextf("'at' must have length equal to %d, the number of groups", 
      n.groups), domain = NA)

  if (is.null(dlab)) 
     dlab <- deparse(substitute(x))

  ## this function returns a "group" vector, to complement "unlist"
  unlistGroup <- function(x, nms = names(x)) rep(nms, sapply(x, length))

  x.val <- unlist(x)
  x.gp <- unlistGroup(x, nms = labels)
  if((range(x.val, finite = TRUE)[1] <= 0) && log)
    warning('values <= 0 omitted from logarithmic plot')
  
  n.obs <- length(x.val)
  n.obs.per.group <- sapply(x, length)
  
  #### Resolve xlim, ylim, dlim, xlab, ylab
  if(is.null(dlim)) {
      if(log) {
        dlim <- 10 ^ (extendrange(log10(x.val[x.val > 0])))
      } else {
        dlim <- extendrange(x.val, f = 0.01)
      }
  }
  if(is.null(glim)) {
    glim <- c(min(at) - 0.5, max(at) + 0.5)
  }
  if(horizontal) { 
    if(is.null(ylim)) 
      ylim <- glim
    if(is.null(xlim)) {
      xlim <- dlim
    } else {
      dlim <- xlim
    }
    if (is.null(xlab)) 
      xlab <- dlab
    if (is.null(ylab)) 
      ylab <- glab
  } else {     ## vertical
    if(is.null(xlim)) 
      xlim <- glim
    if(is.null(ylim)) {
      ylim <- dlim
    } else {
      dlim <- ylim
    }
    if (is.null(ylab)) 
      ylab <- dlab
    if (is.null(xlab)) 
      xlab <- glab
  }
  
  #### Resolve plotting characters and colors
  if(is.null(pwpch)) {
    pch.out <- unlistGroup(x, nms = rep(pch, length.out = n.groups))
  } else {
    if(is.list(pwpch)) {
      names(pwpch) <- names(x)
      stopifnot(all(sapply(pwpch, length) == n.obs.per.group))
      pch.out <- unlist(pwpch)
    } else {
      pch.out <- pwpch
    }
  }
  stopifnot(length(pch.out) == n.obs)

  if(is.null(pwcol)) {
    col.out <- unlistGroup(x, nms = rep(col, length.out = n.groups))
  } else {
    if(is.list(pwcol)) {
      names(pwcol) <- names(x)
      stopifnot(all(sapply(pwcol, length) == n.obs.per.group))
      col.out <- unlist(pwcol)
    } else {
      col.out <- pwcol
    }
  }
  stopifnot(length(col.out) == n.obs)

  if(is.null(pwbg)) {
    bg.out <- unlistGroup(x, nms = rep(bg, length.out = n.groups))
  } else {
    if(is.list(pwbg)) {
      names(pwbg) <- names(x)
      stopifnot(all(sapply(pwbg, length) == n.obs.per.group))
      bg.out <- unlist(pwbg)
    } else {
      bg.out <- pwbg
    }
  }
  stopifnot(length(bg.out) == n.obs)
  
  #### Set up the plot
  if(do.plot & !add) {
    plot(xlim, ylim, 
      type = 'n', axes = FALSE, 
      log = ifelse(log, ifelse(horizontal, 'x', 'y'), ''),
      xlab = xlab, ylab = ylab, ...)
  }

  #### Calculate the size of a plotting character along group- or data-axis
  sizeMultiplier <- par('cex') * cex * spacing
  if(horizontal) {
    size.g <- yinch(0.08, warn.log = FALSE) * sizeMultiplier
    size.d <- xinch(0.08, warn.log = FALSE) * sizeMultiplier
  } else {    # vertical
    size.g <- xinch(0.08, warn.log = FALSE) * sizeMultiplier
    size.d <- yinch(0.08, warn.log = FALSE) * sizeMultiplier
  }
  
## Calculate point positions g.pos, d.pos 
  if(method == 'swarm') {
    if(horizontal) {
      g.offset <- lapply(x, function(a) swarmy(x = a, y = rep(0, length(a)), cex = sizeMultiplier)$y)
    } else {
      g.offset <- lapply(x, function(a) swarmx(x = rep(0, length(a)), y = a, cex = sizeMultiplier)$x)
    }
    d.pos <- x
  } else {
      if(method == 'hex') size.d <- size.d * sqrt(3) / 2
    
      if(log) {
        if(is.null(breaks))
          breaks <- 10 ^ seq(log10(dlim[1]), log10(dlim[2]) + size.d, by = size.d)
        if(length(breaks) == 1 && is.na(breaks[1])) {
          d.index <- x
          d.pos <- x
        } else {
          mids <- 10 ^ ((log10(head(breaks, -1)) + log10(tail(breaks, -1))) / 2)
          d.index <- lapply(x, cut, breaks = breaks, labels = FALSE)
          d.pos <- lapply(d.index, function(a) mids[a])  
        }
      } else {
        if(is.null(breaks))
          breaks <- seq(dlim[1], dlim[2] + size.d, by = size.d)
        if(length(breaks) == 1 && is.na(breaks[1])) {
          d.index <- x
          d.pos <- x
        } else {
          mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
          d.index <- lapply(x, cut, breaks = breaks, labels = FALSE)
          d.pos <- lapply(d.index, function(a) mids[a])  
        }
      }  
    
      x.index <- lapply(d.index, function(v) {
        if(length(na.omit(v)) == 0) return(v)
        v.s <- lapply(split(v, v), seq_along)
        if(method == 'center')
          v.s <- lapply(v.s, function(a) a - mean(a))
        else if(method == 'square')
          v.s <- lapply(v.s, function(a) a - floor(mean(a)))
        else if(method == 'hex') {
          odd.row <- (as.numeric(names(v.s)) %% 2) == 1
          v.s[odd.row] <- lapply(v.s[odd.row], function(a) a - floor(mean(a)) - 0.25)
          v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - ceiling(mean(a)) + 0.25)
        }
        unsplit(v.s, v)
      }) 
      
      g.offset <- lapply(1:n.groups, function(i) x.index[[i]] * size.g)
  }

  ## now check for runaway points (if "corral" has been set)
  if(corral != 'none') {
    if(missing(corralWidth)) {
      if(n.groups > 1) {
        corralWidth <- min(at[-1] - at[-n.groups]) - (2 * size.g)
      } else {
        corralWidth <- 2 * (min(diff(c(par('usr')[1], at, par('usr')[2]))) - size.g)
      }
    } else {
      stopifnot(length(corralWidth) == 1)
      stopifnot(corralWidth > 0)
    }
    halfCorralWidth <- corralWidth / 2
    if(corral == 'gutter') {
      g.offset <- lapply(g.offset, function(zz) pmin(halfCorralWidth, pmax(-halfCorralWidth, zz)))
    }
    if(corral == 'wrap') {
      g.offset <- lapply(g.offset, function(zz) ((zz + halfCorralWidth) %% (halfCorralWidth * 2)) - halfCorralWidth)
    }  
    if(corral == 'random') {
      g.offset <- lapply(g.offset, function(zz) ifelse(zz > halfCorralWidth | zz < -halfCorralWidth, runif(length(zz), -halfCorralWidth, halfCorralWidth), zz))
    }
    if(corral == 'omit') {
      g.offset <- lapply(g.offset, function(zz) ifelse(zz > halfCorralWidth, NA, ifelse(zz < -halfCorralWidth, NA, zz)))
    }
  }
  
  g.pos <- lapply(1:n.groups, function(i) at[i] + g.offset[[i]])

  out <- data.frame(x = unlist(g.pos), y = unlist(d.pos), 
                    pch = pch.out, col = col.out, bg = bg.out,
                    x.orig = x.gp, y.orig = x.val,
                    stringsAsFactors = FALSE)

  if(do.plot) {
    if(horizontal) { 
      points(out$y, out$x, pch = out$pch, col = out$col, bg = out$bg, cex = cex)  
      if(!add) {
        axis(1, ...)
        axis(2, at = at, labels = labels, tick = FALSE, ...)
        box(...)
      }
    } else {
      points(out$x, out$y, pch = out$pch, col = out$col, bg = out$bg, cex = cex)  
      if(!add) {
        axis(2, ...)
        axis(1, at = at, labels = labels, tick = FALSE, ...)
        box(...)
      }
    }
  }
  invisible(out)
}

  
beeswarm.formula <- function (formula, data = NULL, subset, na.action = NULL, 
    pwpch = NULL, pwcol = NULL, pwbg = NULL, dlab, glab, ...) 
{
    if (missing(formula) || (length(formula) != 3)) 
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$dlab <- NULL
    m$glab <- NULL
    m$na.action <- na.action
    require(stats, quietly = TRUE)
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    if (missing(dlab)) 
        dlab <- names(mf)[response]
    if (missing(glab)) 
        glab <- as.character(formula)[3]
    f <- mf[-response]
    f <- f[names(f) %in% attr(attr(mf, "terms"), "term.labels")]
    if(!is.null(mf$'(pwpch)')) pwpch <- split(mf$'(pwpch)', f)
    if(!is.null(mf$'(pwcol)')) pwcol <- split(mf$'(pwcol)', f)
    if(!is.null(mf$'(pwbg)')) pwbg <- split(mf$'(pwbg)',f)
    beeswarm(split(mf[[response]], f), 
      pwpch = pwpch, pwcol = pwcol, pwbg = pwbg,
      dlab = dlab, glab = glab, ...)
}


## hidden function  
.calculateSwarm <- function(x, dsize, gsize) {
  if(length(x) == 0) return(numeric(0))
  out <- data.frame(x = x / dsize, y = 0, i = seq(along = x))
  out <- out[order(out$x), ]
  if(nrow(out) > 1) {
    for (i in 2:nrow(out)) {
      xi <- out$x[i]
      yi <- out$y[i]
      pre <- out[1:(i - 1), ] # previous points
      wh <- xi - pre$x < 1  # which ones are potentially overlapping
      wh[is.na(wh)] <- FALSE  # missing values are not potentially overlapping
      if(any(wh)) {
        pre <- pre[wh, ]
        pre <- pre[order(abs(pre$y)), ]
        poty.off <- sqrt(1 - ((xi - pre$x) ^ 2)) # potential y offset
        poty <- c(0, pre$y + poty.off, pre$y - poty.off) # potential y values
        poty.bad <- sapply(poty, function(y) { # check for overlaps
          any(((xi - pre$x) ^ 2 + (y - pre$y) ^ 2) < 0.999)
        })
        poty[poty.bad] <- Inf
        out$y[i] <- poty[which.min(abs(poty))]
      } else {
        out$y[i] <- 0
      }
    }
  }
  out <- out[order(out$i), ]
  out[is.na(out$x), 'y'] <- NA  # missing x values should have missing y values
  out$y * gsize
}


# jitter points horizontally
swarmx <- function(x, y, 
    xsize = xinch(0.08, warn.log = FALSE), 
    ysize = yinch(0.08, warn.log = FALSE),
    log = NULL, cex = par("cex")) { 
  if(is.null(log)) 
    log <- paste(ifelse(par('xlog'), 'x', ''), ifelse(par('ylog'), 'y', ''), sep = '')
  xlog <- 'x' %in% strsplit(log, NULL)[[1L]]
  ylog <- 'y' %in% strsplit(log, NULL)[[1L]]
  xy <- xy.coords(x = x, y = y, recycle = TRUE, log = log)
  stopifnot((length(unique(xy$x)) <= 1))
  if(xlog) xy$x <- log10(xy$x)
  if(ylog) xy$y <- log10(xy$y)
  x.new <- xy$x + .calculateSwarm(xy$y, dsize = ysize * cex, gsize = xsize * cex)
  out <- data.frame(x = x.new, y = y)
  if(xlog) out$x <- 10 ^ out$x
  out
}

# jitter points vertically
swarmy <- function(x, y, 
    xsize = xinch(0.08, warn.log = FALSE), 
    ysize = yinch(0.08, warn.log = FALSE),
    log = NULL, cex = par("cex")) { 
  if(is.null(log)) 
    log <- paste(ifelse(par('xlog'), 'x', ''), ifelse(par('ylog'), 'y', ''), sep = '')
  xlog <- 'x' %in% strsplit(log, NULL)[[1L]]
  ylog <- 'y' %in% strsplit(log, NULL)[[1L]]
  xy <- xy.coords(x = x, y = y, recycle = TRUE, log = log)
  stopifnot((length(unique(xy$y)) <= 1))
  if(xlog) xy$x <- log10(xy$x)
  if(ylog) xy$y <- log10(xy$y)
  y.new <- xy$y + .calculateSwarm(xy$x, dsize = xsize * cex, gsize = ysize * cex)
  out <- data.frame(x = x, y = y.new)
  if(ylog) out$y <- 10 ^ out$y
  out
}

