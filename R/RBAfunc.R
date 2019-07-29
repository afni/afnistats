sumGLM <- function(dataTable,ll, tm, nR, DF, nd) {
   th <- qt(c(0.025, 0.05, 0.5, 0.95, 0.975), DF)
   rr <- matrix(, nrow = nR, ncol = 8, dimnames=list(levels(dataTable$ROI), c('mean', 'SD', '2-sided-p', '2.5%', '5%', '50%', '95%', '97.5%')))
   rownames(rr) <- levels(dataTable$ROI)
   if(tm == 'Intercept') tm <- '(Intercept)'
   for(ii in 1:nR) {
     u1 <- ll[[ii]]$coefficients[tm,1] # mean
     u2 <- ll[[ii]]$coefficients[tm,2] # sd
     u3 <- ll[[ii]]$coefficients[tm,4] # 2-sided p
     rr[ii,] <- round(c(u1, u2, u3, u1+u2*th),nd)
   } 
   return(rr)
}


addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.

  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))

  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

plotPDP <- function(fn, ps, nR, nr, nc, w=8) {
   h <- ceiling(8*nr/(nc*2))  # plot window height
   pdf(paste0(fn, ".pdf"), width=w, height=h)
   #dev.new(width=w, height=h)
   par(mfrow=c(lop$PDP[1], nc), mar=c(2.5,0,0.0,0.8), oma=c(0,0,0,0))
   qq <- apply(ps, 2, quantile, c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)) # 95% central interval
   kk <- 0
   for(ii in 1:nR) {
      kk <- kk+1
      #x <- quantile(ps[,ii], probs = c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975))
      dens <- density(ps[,ii])
      #par(mar=c(1.85,0.2,0.0,0.8))
      plot(dens, main='', axes=F, bty="n", xlab='', ylab='')
      axis(side = 1)
      abline(v=0, col='blue')
      #if(is.even(kk)) mtext(dimnames(ps)[[2]][ii], side = 1, line=-7, las=0) else
      mtext(dimnames(ps)[[2]][ii], side = 3, line=-2, las=0)
      x1 <- min(which(dens$x >= qq[6,ii]))  # 97.5% 
      x2 <- max(which(dens$x <  4e10))         # infinity
      x3 <- min(which(dens$x >= -4e10))        # -infinity
      x4 <- max(which(dens$x <  qq[1,ii]))  # 2.5%
      x5 <- min(which(dens$x >= qq[5,ii]))  # 95%
      x6 <- max(which(dens$x <  qq[2,ii]))  # 5%
      x7 <- min(which(dens$x >= qq[4,ii]))  # 90%
      x8 <- max(which(dens$x <  qq[3,ii]))  # 10%
      with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=addTrans('green',175))) # right tail
      with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col=addTrans('green',175))) # left tail
      with(dens, polygon(x=c(x[c(x5,x5:x1,x1)]), y= c(0, y[x5:x1], 0), col=addTrans('orange',150)))
      with(dens, polygon(x=c(x[c(x4,x4:x6,x6)]), y= c(0, y[x4:x6], 0), col=addTrans('orange',150)))
      with(dens, polygon(x=c(x[c(x7,x7:x5,x5)]), y= c(0, y[x7:x5], 0), col=addTrans('gray',125)))
      with(dens, polygon(x=c(x[c(x6,x6:x8,x8)]), y= c(0, y[x6:x8], 0), col=addTrans('gray',125)))
      #with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="green")) # right tail
      #with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col="green")) # left tail
      #with(dens, polygon(x=c(x[c(x5,x5:x1,x1)]), y= c(0, y[x5:x1], 0), col="orange"))
      #with(dens, polygon(x=c(x[c(x4,x4:x6,x6)]), y= c(0, y[x4:x6], 0), col="orange"))
      #with(dens, polygon(x=c(x[c(x7,x7:x5,x5)]), y= c(0, y[x7:x5], 0), col="gray"))
      #with(dens, polygon(x=c(x[c(x6,x6:x8,x8)]), y= c(0, y[x6:x8], 0), col="gray"))
      if(qq[1,ii] > 0 | qq[6,ii] < 0) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = 'solid', border = addTrans('green',200), lwd=3)
      if((qq[1,ii] < 0 & qq[2,ii] > 0) | (qq[5,ii] < 0 &  qq[6,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = 'solid', border = addTrans('orange',150), lwd=3)
      if((qq[2,ii] < 0 & qq[3,ii] > 0) | (qq[4,ii] < 0 &  qq[5,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = 'solid', border = addTrans('gray',100), lwd=3)
      #if(qq[1,ii] > 0 | qq[6,ii] < 0) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'green', lwd=3)
      #if((qq[1,ii] < 0 & qq[2,ii] > 0) | (qq[5,ii] < 0 &  qq[6,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'orange', lwd=3)
      #if((qq[2,ii] < 0 & qq[3,ii] > 0) | (qq[4,ii] < 0 &  qq[5,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'gray', lwd=3)
   }
   dev.off()
}

# write data.frame to a file
outDF <- function(DF, fl) cat(capture.output(DF), file = paste0(fl, '.txt'), sep = '\n', append=TRUE)
fisher <- function(r) ifelse(abs(r) < .995, 0.5*(log(1+r)-log(1-r)), stop('Are you sure that you have correlation values so close to 1 or -1?'))
#  Rhat checking #
dd <- function(ll) any(ll[,'Rhat'] > 1.2)
# compute P+
cnt <- function(x, ns) return(sum(x>0)/ns)

########## region effects #############
# posterior samples at ROIs for a term
psROI <- function(aa, bb, tm) {
  ps <- apply(bb[['ROI']][,,tm], 2, '+', aa[,tm])
  return(ps)
}
# ps <- ww(aa, bb, 'Intercept', nR)

# summary for ROIs: nd - number of digits to output
sumROI <- function(R0, ns, nd) {
  hubs <- data.frame(cbind(apply(R0, 2, mean), apply(R0, 2, sd), apply(R0, 2, cnt, ns), t(apply(R0, 2, quantile, 
      probs=c(0.025, 0.05, 0.5, 0.95, 0.975)))))
  names(hubs) <- c('mean', 'SD', 'P+', '2.5%', '5%', '50%', '95%', '97.5%')
  return(round(hubs,nd))
}
#gg <- sumROI(gg, ns, 3)

#is.even <- function(x) x %% 2 == 0

addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.

  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))

  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

plotPDP <- function(fn, ps, nR, nr, nc, w=8) {
   h <- ceiling(8*nr/(nc*2))  # plot window height
   pdf(paste0(fn, ".pdf"), width=w, height=h)
   #dev.new(width=w, height=h)
   par(mfrow=c(lop$PDP[1], nc), mar=c(2.5,0,0.0,0.8), oma=c(0,0,0,0))
   qq <- apply(ps, 2, quantile, c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)) # 95% central interval
   kk <- 0
   for(ii in 1:nR) {
      kk <- kk+1
      #x <- quantile(ps[,ii], probs = c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975))
      dens <- density(ps[,ii])
      #par(mar=c(1.85,0.2,0.0,0.8))
      plot(dens, main='', axes=F, bty="n", xlab='', ylab='')
      axis(side = 1)
      abline(v=0, col='blue')
      #if(is.even(kk)) mtext(dimnames(ps)[[2]][ii], side = 1, line=-7, las=0) else
      mtext(dimnames(ps)[[2]][ii], side = 3, line=-2, las=0)
      x1 <- min(which(dens$x >= qq[6,ii]))  # 97.5% 
      x2 <- max(which(dens$x <  4e10))         # infinity
      x3 <- min(which(dens$x >= -4e10))        # -infinity
      x4 <- max(which(dens$x <  qq[1,ii]))  # 2.5%
      x5 <- min(which(dens$x >= qq[5,ii]))  # 95%
      x6 <- max(which(dens$x <  qq[2,ii]))  # 5%
      x7 <- min(which(dens$x >= qq[4,ii]))  # 90%
      x8 <- max(which(dens$x <  qq[3,ii]))  # 10%
      with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=addTrans('green',175))) # right tail
      with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col=addTrans('green',175))) # left tail
      with(dens, polygon(x=c(x[c(x5,x5:x1,x1)]), y= c(0, y[x5:x1], 0), col=addTrans('orange',150)))
      with(dens, polygon(x=c(x[c(x4,x4:x6,x6)]), y= c(0, y[x4:x6], 0), col=addTrans('orange',150)))
      with(dens, polygon(x=c(x[c(x7,x7:x5,x5)]), y= c(0, y[x7:x5], 0), col=addTrans('gray',125)))
      with(dens, polygon(x=c(x[c(x6,x6:x8,x8)]), y= c(0, y[x6:x8], 0), col=addTrans('gray',125)))
      #with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="green")) # right tail
      #with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col="green")) # left tail
      #with(dens, polygon(x=c(x[c(x5,x5:x1,x1)]), y= c(0, y[x5:x1], 0), col="orange"))
      #with(dens, polygon(x=c(x[c(x4,x4:x6,x6)]), y= c(0, y[x4:x6], 0), col="orange"))
      #with(dens, polygon(x=c(x[c(x7,x7:x5,x5)]), y= c(0, y[x7:x5], 0), col="gray"))
      #with(dens, polygon(x=c(x[c(x6,x6:x8,x8)]), y= c(0, y[x6:x8], 0), col="gray"))
      if(qq[1,ii] > 0 | qq[6,ii] < 0) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = 'solid', border = addTrans('green',200), lwd=3)
      if((qq[1,ii] < 0 & qq[2,ii] > 0) | (qq[5,ii] < 0 &  qq[6,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = 'solid', border = addTrans('orange',150), lwd=3)
      if((qq[2,ii] < 0 & qq[3,ii] > 0) | (qq[4,ii] < 0 &  qq[5,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = 'solid', border = addTrans('gray',100), lwd=3)
      #if(qq[1,ii] > 0 | qq[6,ii] < 0) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'green', lwd=3)
      #if((qq[1,ii] < 0 & qq[2,ii] > 0) | (qq[5,ii] < 0 &  qq[6,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'orange', lwd=3)
      #if((qq[2,ii] < 0 & qq[3,ii] > 0) | (qq[4,ii] < 0 &  qq[5,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'gray', lwd=3)
   }
   dev.off()
}