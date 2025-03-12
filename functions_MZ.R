


subsetLAI <- function(df, minnrHH=4, maxSD=0.05){
  
  # All rings have some observations
  tab <- table(df$Date, df$Ring)
  dats <- as.Date(rownames(tab)[rowSums(tab) == 6])
  
  df$minn <- ave(df$Gapfraction.n, df$Date, FUN=min)
  dats2 <- unique(df$Date[df$minn >= minnrHH])
  
  dats <- as.Date(intersect(dats, dats2), origin="1970-1-1")
  df <- subset(df, Date %in% dats)
  df$minn <- NULL
  
  # find max SD by date
  df$maxSDring <- ave(df$Gapfraction.sd, df$Date, FUN=max, na.rm=TRUE)
  
  # discard days where at least one ring has excessive within-day variance.
  df <- subset(df, maxSDring < maxSD)
  
  # Order
  df <- df[order(df$Date, df$Ring),]
  return(df)
}


get_litring <- function(){
  df <- read.csv("data/FACE_P0017_RA_LEAFLITTER_2012-2023.csv")
  
  df$Date <- as.Date(df$Date, format="%d/%m/%Y")
  df$dLAIlitter <- (df$Leaf/0.1979)*52.6*0.0001
  df[1] <- NULL
  
  return(df)
}

litterbyring <- function(dfr){
  
  n <- function(x,...)length(x[!is.na(x)])
  litagg <- summaryBy(Leaf + dLAIlitter ~ Ring + Date, FUN=c(mean,sd,n),na.rm=TRUE, 
                      data=dfr, id=~treatment)
  
  litagg$n <- litagg$LEAF.n
  litagg$LEAF.n <- litagg$dLAIlitter.n <- NULL
  
  dats <- sort(unique(litagg$Date))
  datdf <- data.frame(Date=dats, ndays=c(NA,diff(dats)))
  litagg <- merge(litagg, datdf)
  
  return(litagg) 
}


#' Function for smoothplot. Probably not use otherwise.
fitgam <- function(X,Y,dfr, k=-1, R=NULL){
  dfr$Y <- dfr[,Y]
  dfr$X <- dfr[,X]
  if(!is.null(R)){
    dfr$R <- dfr[,R]
    model <- 2
  } else model <- 1
  dfr <- droplevels(dfr)
  
  
  if(model ==1){
    g <- gam(Y ~ s(X, k=k), data=dfr)
  }
  if(model ==2){
    g <- gamm(Y ~ s(X, k=k), random = list(R=~1), data=dfr)
  }
  
  return(g)
}


smoothplot <- function(x,y,g=NULL,data,
                       fittype=c("gam","lm"),
                       kgam=4,
                       R=NULL,
                       randommethod=c("lmer","aggregate"),
                       log="",
                       axes=TRUE,
                       fitoneline=FALSE,
                       pointcols=NULL,
                       linecols=NULL, 
                       xlab=NULL, ylab=NULL,
                       polycolor=alpha("lightgrey",0.7),
                       plotit=TRUE, add=FALSE,
                       npred=101,
                       ...){
  
  fittype <- match.arg(fittype)
  randommethod <- match.arg(randommethod)
  if(log != "")require(magicaxis)
  
  if(!is.null(substitute(g))){
    data$G <- as.factor(eval(substitute(g),data))
  } else {
    fitoneline <- TRUE
    data$G <- 1
  }
  data$X <- eval(substitute(x),data)
  data$Y <- eval(substitute(y),data)
  data <- droplevels(data)
  
  data <- data[!is.na(data$X) & !is.na(data$Y) & !is.na(data$G),]
  nlev <- length(unique(data$G))
  if(length(polycolor) == 1)polycolor <- rep(polycolor,nlev)
  
  if(class(data$X) == "Date"){
    xDate <- TRUE
    data$X <- as.numeric(data$X)
  } else {
    xDate <- FALSE
  }
  
  if(is.null(pointcols))pointcols <- palette()
  if(is.null(linecols))linecols <- palette()
  
  if(is.null(xlab))xlab <- substitute(x)
  if(is.null(ylab))ylab <- substitute(y)
  
  # If randommethod = aggregate, average by group and fit simple gam.
  if(!is.null(R) && randommethod == "aggregate"){
    data$R <- data[,R]
    
    data <- summaryBy(. ~ R, FUN=mean, na.rm=TRUE, keep.names=TRUE, data=data,
                      id=~G)
    R <- NULL
  }
  
  if(!fitoneline){
    
    d <- split(data, data$G)
    
    if(fittype == "gam"){
      fits <- lapply(d, function(x)try(fitgam("X","Y",x, k=kgam, R=R)))
      if(!is.null(R))fits <- lapply(fits, "[[", "gam")
    } else {
      fits <- lapply(d, function(x)lm(Y ~ X, data=x))
    }
    hran <- lapply(d, function(x)range(x$X, na.rm=TRUE))
  } else {
    if(fittype == "gam"){
      fits <- list(fitgam("X","Y",data, k=kgam, R=R))
      if(!is.null(R))fits <- lapply(fits, "[[", "gam")
    } else {
      fits <- list(lm(Y ~ X, data=data))
    }
    hran <- list(range(data$X, na.rm=TRUE))
    
  }
  
  if(plotit){
    if(xDate){
      data$X <- as.Date(data$X, origin="1970-1-1")
    }
    
    if(!add){
      with(data, plot(X, Y, axes=FALSE, pch=16, col=pointcols[G],
                      xlab=xlab, ylab=ylab, ...))
    } else {
      with(data, points(X, Y, pch=16, col=pointcols[G],
                        ...))
    }
    
    if(!add && axes){
      if(log=="xy")magaxis(side=1:2, unlog=1:2)
      if(log=="x"){
        magaxis(side=1, unlog=1)
        axis(2)
        box()
      }
      if(log=="y"){
        magaxis(side=2, unlog=2)
        axis(1)
        box()
      }
      if(log==""){
        if(xDate)
          axis.Date(1, data$X)
        else
          axis(1)
        axis(2)
        box()
      }
    }
    
    for(i in 1:length(fits)){
      
      if(fittype == "gam"){
        nd <- data.frame(X=seq(hran[[i]][1], hran[[i]][2], length=npred))
        if(!inherits(fits[[i]], "try-error")){
          p <- predict(fits[[i]],nd,se.fit=TRUE)
          addpoly(nd$X, p$fit-2*p$se.fit, p$fit+2*p$se.fit, col=polycolor[i])
          lines(nd$X, p$fit, col=linecols[i], lwd=2)
        }
      }
      if(fittype == "lm"){
        pval <- summary(fits[[i]])$coefficients[2,4]
        LTY <- if(pval < 0.05)1 else 5
        predline(fits[[i]], col=linecols[i], lwd=2, lty=LTY)
      }
    }
  }
  return(invisible(fits))
}



makesmoothLAI <- function(dat, timestep="3 days", kgam=15, how=c("byring","mean")){
  
  how <- match.arg(how)
  
  if(how == "mean"){
    
    x <- dat
    x <- x[order(x$Date),]
    gamfit <- smoothplot(as.numeric(Date),LAI,data=x,kgam=kgam, plotit=FALSE)
    
    dfr <- data.frame(X=as.numeric(seq.Date(min(x$Date), max(x$Date), by=timestep)))
    dfr$LAIsmooth <- predict(gamfit[[1]],dfr)
    names(dfr)[1] <- "Date"
    dfr$Date <- as.Date(dfr$Date, origin="1970-1-1")
    
    dfr$dLAI <- c(NA, diff(dfr$LAIsmooth))
    dfr$ndays <- c(NA, diff(dfr$Date))
    return(dfr)
    
  }
  
  if(how == "byring"){
    rings <- split(dat, dat$Ring)
    smoothlai <- lapply(rings, function(x){
      
      x <- x[order(x$Date),]
      
      gamfit <- smoothplot(as.numeric(Date),LAI,data=x,kgam=kgam, plotit=FALSE)
      
      dfr <- data.frame(X=as.numeric(seq.Date(min(x$Date), max(x$Date), by=timestep)))
      dfr$LAIsmooth <- predict(gamfit[[1]],dfr)
      names(dfr)[1] <- "Date"
      dfr$Date <- as.Date(dfr$Date, origin="1970-1-1")
      
      dfr$dLAI <- c(NA, diff(dfr$LAIsmooth))
      dfr$ndays <- c(NA, diff(dfr$Date))
      return(dfr)
    })
  }
  
  return(smoothlai)
}

splitbydate <- function(dfr, datevec){
  
  datevec <- as.Date(datevec)
  
  l <- list()
  for(i in 2:length(datevec)){
    l[[i]] <- subset(dfr, Date >= datevec[i-1] & Date < datevec[i])
  }
  l[[1]] <- NULL
  
  return(l)
}


make_dLAI_litter_before <- function(dat, litter_before, kgam=15){
  # dat <- lai_before # for debugging (Assaf)
  # LAI by ring with smoother
  dat <- makesmoothLAI(dat, kgam=kgam, timestep="1 day")
  
  # Make LAI change, combine with litter fall
  litterDates <- sort(unique(litter_before$Date))
  dat <- lapply(dat, splitbydate, litterDates)
  
  # Get change in LAI for each inter-litter interval.
  getdlai <- function(x){
    do.call(rbind,lapply(x, function(z){
      n <- nrow(z)
      dLAI <- z$LAIsm[n] - z$LAIsm[1]
      d <- diff(z$LAIsm)
      dnegLAI <- sum(d[d < 0])
      dposLAI <- sum(d[d > 0])
      return(data.frame(dLAI=dLAI, dnegLAI=dnegLAI, dposLAI=dposLAI, LAI=mean(z$LAIsm)))
    }))
  }
  
  # r will be a dataframe with litterfall and change in LAI
  r <- list()
  for(i in 1:5){
    r[[i]] <- cbind(data.frame(Date=litterDates[2:length(litterDates)]), getdlai(dat[[i]]))
    r[[i]] <- merge(r[[i]], subset(litter, Ring == paste0("R",i)), by=c("Date"))
  }
  r <- do.call(rbind,r)
  
  # Absolute change in LAI
  r$absdLAI <- with(r, -dnegLAI + dposLAI)
  r$LAIchange <- as.factor(r$dLAI > 0)
  levels(r$LAIchange) <- c("decreasing","increasing")
  return(r)
}




make_dLAI_litter_after <- function(dat, litter_after, kgam=15){
  
  # LAI by ring with smoother
  dat <- makesmoothLAI(dat, kgam=kgam, timestep="1 day")
  
  # Make LAI change, combine with litter fall
  litterDates <- sort(unique(litter_after$Date))
  dat <- lapply(dat, splitbydate, litterDates)
  
  # Get change in LAI for each inter-litter interval.
  getdlai <- function(x){
    do.call(rbind,lapply(x, function(z){
      n <- nrow(z)
      dLAI <- z$LAIsm[n] - z$LAIsm[1]
      d <- diff(z$LAIsm)
      dnegLAI <- sum(d[d < 0])
      dposLAI <- sum(d[d > 0])
      return(data.frame(dLAI=dLAI, dnegLAI=dnegLAI, dposLAI=dposLAI, LAI=mean(z$LAIsm)))
    }))
  }
  
  # r will be a dataframe with litterfall and change in LAI
  r <- list()
  for(i in 1:4){
    r[[i]] <- cbind(data.frame(Date=litterDates[2:length(litterDates)]), getdlai(dat[[i]]))
    r[[i]] <- merge(r[[i]], subset(litter, Ring == paste0("R",i)), by=c("Date"))
  }
  r <- do.call(rbind,r)
  
  # Absolute change in LAI
  r$absdLAI <- with(r, -dnegLAI + dposLAI)
  r$LAIchange <- as.factor(r$dLAI > 0)
  levels(r$LAIchange) <- c("decreasing","increasing")
  return(r)
}



to.pdf <- function(expr, filename, ..., verbose=TRUE) {
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}


addpoly <- function(x,y1,y2,col=alpha("lightgrey",0.8),...){
  ii <- order(x)
  y1 <- y1[ii]
  y2 <- y2[ii]
  x <- x[ii]
  polygon(c(x,rev(x)), c(y1, rev(y2)), col=col, border=NA,...)
}


timeseries_axis <- function(labels=TRUE, tclminor=-0.2){
  
  XLIM <- par()$usr[1:2]
  
  xAT <- seq.Date(as.Date("2021-1-1"), by="1 month", length=100)
  xAT <- xAT[xAT > XLIM[1] & xAT < XLIM[2]]
  labs <- substr(format(xAT, "%b"),1,1)
  
  axis.Date(1, at=xAT, labels=FALSE, cex.axis=0.6, tcl=tclminor)
  if(labels)mtext(labs, side=1, line=0, at=xAT, cex=0.7)
  
  maj <- seq.Date(as.Date("2021-1-1"),by="1 year", length=10)
  maj <- maj[maj > XLIM[1] & maj < XLIM[2]]
  
  axis.Date(1, at=maj, labels=FALSE, tcl=-0.2, lwd.ticks=2)
  axis.Date(1, at=maj, labels=FALSE, tcl=0.4, lwd.ticks=2)
  yrlab <- seq.Date(as.Date("2021-1-1"), by='1 year', length=10)
  labs <- as.character(seq(2021,length=length(yrlab)))
  ii <- yrlab > XLIM[1] & yrlab < XLIM[2]
  yrlab <- yrlab[ii]
  labs <- labs[ii]
  
  if(labels)mtext(labs, side=1, line=1, at=yrlab)
  
}


my_co2cols <- function()c("blue","red")




eucfaceBA <- function(){
  read.table(text="
  Ring  nstems	BA
  R1	30	25.2
  R2	41	24.3
  R3	39	25.9
  R4	55	20.9
  R5	54	38
  R6	44	29", header=TRUE)
  
}


# EucFACE treatment key.
eucface <- function(){
  data.frame(Ring=paste0("R",1:6), 
             treatment=c("elevated","ambient","ambient","elevated","elevated","ambient"))
}
