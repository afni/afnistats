
fit_rba <- function(dataTable, model, chains, iterations, ROI = "ROI1") {
  if (model == 1) {
    modelForm <- as.formula(paste("Y ~ 1 + (1|Subj) + (1|", ROI, ")"))
  } else {
    modelForm <- as.formula(paste("Y~", model, "+(1|Subj)+(", model, "|", ROI, ")"))
  }

  if (model == 1) {
    fm <- brm(modelForm,
      data = dataTable, chains = chains,
      iter = iterations, control = list(adapt_delta = 0.99, max_treedepth = 15)
    )
  } else {
    fm <- brm(modelForm,
      data = dataTable,
      prior = c(prior(normal(0, 1), class = "Intercept"), prior(normal(0, 0.5), class = "sd")),
      chains = chains, iter = iterations, control = list(adapt_delta = 0.99, max_treedepth = 15)
    )
  }
  fm
}


post_process_rba <- function(fm, outFN, iterations, chains, EOIq, EOIc, qContr, ptm, ROI,PDP,model,dataTable) {
  print(format(Sys.time(), "%D %H:%M:%OS3"))
  nR <- get_nr(dataTable,ROI)
  # Stop the clock
  proc.time() - ptm

  save.image(file = paste0(outFN, ".RData"))

  cat(format(Sys.time(), "%D %H:%M:%OS3"), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
  cat(capture.output(proc.time() - ptm), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)

  ##################### Post Processing ####################

  cat("
++++++++++++++++++++++++++++++++++++++++++++++++++++
")
  cat("***** Summary information of model information *****
")
  (rs <- summary(fm))
  cat("
***** End of model information *****
")
  cat("++++++++++++++++++++++++++++++++++++++++++++++++++++

")

  cat("
***** Summary information of model results *****
", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)

  if (any(sapply(c(list(fixed = rs$fixed, spec_pars = rs$spec_pars, cor_pars = rs$cor_pars), rs$random), dd))) {
    cat("
***** Warning: convergence issue!!! *****
", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
    cat("Consider increasing the number of iterations for Markov chains!
", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
  }

  cat(capture.output(rs), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
  cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)

  ns <- iterations * chains / 2
  aa <- fixef(fm, summary = FALSE) # Population-Level Estimates
  bb <- ranef(fm, summary = FALSE) # Extract Group-Level (or random-effect) Estimates


  # for Intercept and quantitative variables
  if (any(!is.na(EOIq) == TRUE)) {
    for (ii in 1:length(EOIq)) {
      cat(sprintf("===== Summary of region effects for %s =====", EOIq[ii]),
        file = paste0(outFN, ".txt"), sep = "\n", append = TRUE
      )
      ps0 <- psROI_RBA(aa, bb, EOIq[ii], ROI)
      gg <- sumROI(ps0, ns, 4)
      cat(capture.output(gg), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      if (any(!is.na(PDP) == TRUE)) plotPDP(EOIq[ii], ps0, nR, PDP[1], PDP[2], 8)
    }
  }

  # for contrasts among quantitative variables
  if (any(!is.na(qContr) == TRUE)) {
    for (ii in 1:(length(qContrL) / 2)) {
      cat(sprintf("===== Summary of region effects for %s vs %s =====", qContrL[2 * ii - 1], qContrL[2 * ii]),
        file = paste0(outFN, ".txt"), sep = "\n", append = TRUE
      )
      ps0 <- psROI_RBA(aa, bb, qContrL[2 * ii - 1], ROI) - psROI_RBA(aa, bb, qContrL[2 * ii], ROI)
      gg <- sumROI(ps0, ns, 4)
      cat(capture.output(gg), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      if (any(!is.na(PDP) == TRUE)) plotPDP(paste0(qContrL[2 * ii - 1], "-", qContrL[2 * ii]), ps0, nR, PDP[1], PDP[2], 8)
    }
  }

  # for factor
  if (any(!is.na(EOIc) == TRUE)) {
    for (ii in 1:length(EOIc)) {
      lvl <- levels(dataTable[[EOIc[ii]]]) # levels
      nl <- nlevels(dataTable[[EOIc[ii]]]) # number of levels: last level is the reference in deviation coding
      ps <- array(0, dim = c(nl, ns, nR)) # posterior samples
      for (jj in 1:(nl - 1)) ps[jj, , ] <- psROI_RBA(aa, bb, paste0(EOIc[ii], jj), ROI)
      ps[nl, , ] <- psROI_RBA(aa, bb, "Intercept", ROI) # Intercept: averge effect
      psa <- array(0, dim = c(nl, ns, nR)) # posterior samples adjusted
      for (jj in 1:(nl - 1)) {
        psa[jj, , ] <- ps[nl, , ] + ps[jj, , ]
        psa[nl, , ] <- psa[nl, , ] + ps[jj, , ]
      }
      psa[nl, , ] <- ps[nl, , ] - psa[nl, , ] # reference level

      oo <- apply(psa, 1, sumROI, ns, 4)

      cat(sprintf("===== Summary of region effects for %s =====", EOIc[ii]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      for (jj in 1:nl) {
        cat(sprintf("----- %s level: %s", EOIc[ii], lvl[jj]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
        cat(capture.output(oo[[jj]]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
        cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      }

      if (any(!is.na(PDP) == TRUE)) {
        for (jj in 1:nl) {
          plotPDP(EOIc[ii], psa[jj, , ], nR, PDP[1], PDP[2], 8)
        }
      }

      cat(sprintf("===== Summary of region effects for %s comparisons =====", EOIc[ii]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      for (jj in 1:(nl - 1)) {
        for (kk in (jj + 1):nl) {
          cat(sprintf("----- level comparison: %s vs %s", lvl[jj], lvl[kk]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
          oo <- sumROI(psa[jj, , ] - psa[kk, , ], ns, 4)
          cat(capture.output(oo), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
          cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
          if (any(!is.na(PDP) == TRUE)) plotPDP(paste0(lvl[jj], "-", lvl[kk]), psa[jj, , ] - psa[kk, , ], nR, PDP[1], PDP[2], 8)
        }
      }
    }
  }

  ##################### conventional GLM #####################
  mm <- list()
  GLM <- as.formula(paste("Y ~", model))
  for (ii in levels(dataTable[ROI][[1]])) mm[[ii]] <- lm(GLM, data = dataTable[dataTable[ROI][[1]] == ii, ])
  nn <- lapply(mm, summary)
  ll <- lapply(nn, `[`, "coefficients")



  # for Intercept and quantitative variables
  if (any(!is.na(EOIq) == TRUE)) {
    for (ii in 1:length(EOIq)) {
      cat(sprintf("===== Summary of region effects under GLM for %s: no adjustment for multiplicity =====", EOIq[ii]),
        file = paste0(outFN, ".txt"), sep = "\n", append = TRUE
      )
      gg <- sumGLM(dataTable, ll, EOIq[ii], nR, nn[[ii]]$df, 4, ROI)
      cat(capture.output(gg), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
    }
  }

  # for contrasts among quantitative variables
  if (any(!is.na(qContr) == TRUE)) {
    for (ii in 1:(length(qContrL) / 2)) {
      cat(sprintf("===== Summary of region effects for %s vs %s =====", qContrL[2 * ii - 1], qContrL[2 * ii]),
        file = paste0(outFN, ".txt"), sep = "\n", append = TRUE
      )
      cat(capture.output(gg), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
    }
  }

  # for factor
  if (any(!is.na(EOIc) == TRUE)) {
    for (ii in 1:length(EOIc)) {
      lvl <- levels(dataTable[[EOIc[ii]]]) # levels
      nl <- nlevels(dataTable[[EOIc[ii]]]) # number of levels: last level is the reference in deviation coding
      ps <- array(0, dim = c(nl, ns, nR)) # posterior samples
      for (jj in 1:(nl - 1)) ps[jj, , ] <- psROI_RBA(aa, bb, paste0(EOIc[ii], jj), ROI)
      ps[nl, , ] <- psROI_RBA(aa, bb, "Intercept", ROI) # Intercept: averge effect
      psa <- array(0, dim = c(nl, ns, nR)) # posterior samples adjusted
      for (jj in 1:(nl - 1)) {
        psa[jj, , ] <- ps[nl, , ] + ps[jj, , ]
        psa[nl, , ] <- psa[nl, , ] + ps[jj, , ]
      }
      psa[nl, , ] <- ps[nl, , ] - psa[nl, , ] # reference level

      oo <- apply(psa, 1, sumROI, ns, 4)

      cat(sprintf("===== Summary of region effects for %s =====", EOIc[ii]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      for (jj in 1:nl) {
        cat(sprintf("----- %s level: %s", EOIc[ii], lvl[jj]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
        cat(capture.output(oo[[jj]]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
        cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      }

      cat(sprintf("===== Summary of region effects for %s comparisons =====", EOIc[ii]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      for (jj in 1:(nl - 1)) {
        for (kk in (jj + 1):nl) {
          cat(sprintf("----- level comparison: %s vs %s", lvl[jj], lvl[kk]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
          oo <- sumROI(psa[jj, , ] - psa[kk, , ], ns, 4)
          cat(capture.output(oo), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
          cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
        }
      }
    }
  }
  ################

  # save it again
  save.image(file = paste0(outFN, ".RData"))
  cat("\nCongratulations! The above results are saved in file ", outFN, "\n\n", sep = "")
}

sumGLM <- function(dataTable, ll, tm, nR, DF, nd, ROI = "ROI") {
  th <- qt(c(0.025, 0.05, 0.5, 0.95, 0.975), DF)
  rr <- matrix(, nrow = nR, ncol = 8, dimnames = list(levels(dataTable[ROI][[1]]), c("mean", "SD", "2-sided-p", "2.5%", "5%", "50%", "95%", "97.5%")))
  rownames(rr) <- levels(dataTable[ROI][[1]])
  if (tm == "Intercept") tm <- "(Intercept)"
  for (ii in 1:nR) {
    u1 <- ll[[ii]]$coefficients[tm, 1] # mean
    u2 <- ll[[ii]]$coefficients[tm, 2] # sd
    u3 <- ll[[ii]]$coefficients[tm, 4] # 2-sided p
    rr[ii, ] <- round(c(u1, u2, u3, u1 + u2 * th), nd)
  }
  return(rr)
}

#  Rhat checking #
dd <- function(ll) any(ll[, "Rhat"] > 1.2)

########## region effects #############
# posterior samples at ROIs for a term
psROI_RBA <- function(aa, bb, tm, ROI) {
  ps <- apply(bb[[ROI]][, , tm], 2, "+", aa[, tm])
  return(ps)
}
# ps <- ww(aa, bb, 'Intercept', nR)


# is.even <- function(x) x %% 2 == 0

addTrans <- function(color, trans) {
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.

  if (length(color) != length(trans) & !any(c(length(color), length(trans)) == 1)) stop("Vector lengths not correct")
  if (length(color) == 1 & length(trans) > 1) color <- rep(color, length(trans))
  if (length(trans) == 1 & length(color) > 1) trans <- rep(trans, length(color))

  num2hex <- function(x) {
    hex <- unlist(strsplit("0123456789ABCDEF", split = ""))
    return(paste(hex[(x - x %% 16) / 16 + 1], hex[x %% 16 + 1], sep = ""))
  }
  rgb <- rbind(col2rgb(color), trans)
  res <- paste("#", apply(apply(rgb, 2, num2hex), 2, paste, collapse = ""), sep = "")
  return(res)
}

plotPDP <- function(fn, ps, nR, nr, nc, w = 8) {
  h <- ceiling(8 * nr / (nc * 2)) # plot window height
  pdf(paste0(fn, ".pdf"), width = w, height = h)
  # dev.new(width=w, height=h)
  par(mfrow = c(lop$PDP[1], nc), mar = c(2.5, 0, 0.0, 0.8), oma = c(0, 0, 0, 0))
  qq <- apply(ps, 2, quantile, c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)) # 95% central interval
  kk <- 0
  for (ii in 1:nR) {
    kk <- kk + 1
    # x <- quantile(ps[,ii], probs = c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975))
    dens <- density(ps[, ii])
    # par(mar=c(1.85,0.2,0.0,0.8))
    plot(dens, main = "", axes = F, bty = "n", xlab = "", ylab = "")
    axis(side = 1)
    abline(v = 0, col = "blue")
    # if(is.even(kk)) mtext(dimnames(ps)[[2]][ii], side = 1, line=-7, las=0) else
    mtext(dimnames(ps)[[2]][ii], side = 3, line = -2, las = 0)
    x1 <- min(which(dens$x >= qq[6, ii])) # 97.5%
    x2 <- max(which(dens$x < 4e10)) # infinity
    x3 <- min(which(dens$x >= -4e10)) # -infinity
    x4 <- max(which(dens$x < qq[1, ii])) # 2.5%
    x5 <- min(which(dens$x >= qq[5, ii])) # 95%
    x6 <- max(which(dens$x < qq[2, ii])) # 5%
    x7 <- min(which(dens$x >= qq[4, ii])) # 90%
    x8 <- max(which(dens$x < qq[3, ii])) # 10%
    with(dens, polygon(x = c(x[c(x1, x1:x2, x2)]), y = c(0, y[x1:x2], 0), col = addTrans("green", 175))) # right tail
    with(dens, polygon(x = c(x[c(x3, x3:x4, x4)]), y = c(0, y[x3:x4], 0), col = addTrans("green", 175))) # left tail
    with(dens, polygon(x = c(x[c(x5, x5:x1, x1)]), y = c(0, y[x5:x1], 0), col = addTrans("orange", 150)))
    with(dens, polygon(x = c(x[c(x4, x4:x6, x6)]), y = c(0, y[x4:x6], 0), col = addTrans("orange", 150)))
    with(dens, polygon(x = c(x[c(x7, x7:x5, x5)]), y = c(0, y[x7:x5], 0), col = addTrans("gray", 125)))
    with(dens, polygon(x = c(x[c(x6, x6:x8, x8)]), y = c(0, y[x6:x8], 0), col = addTrans("gray", 125)))
    # with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="green")) # right tail
    # with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col="green")) # left tail
    # with(dens, polygon(x=c(x[c(x5,x5:x1,x1)]), y= c(0, y[x5:x1], 0), col="orange"))
    # with(dens, polygon(x=c(x[c(x4,x4:x6,x6)]), y= c(0, y[x4:x6], 0), col="orange"))
    # with(dens, polygon(x=c(x[c(x7,x7:x5,x5)]), y= c(0, y[x7:x5], 0), col="gray"))
    # with(dens, polygon(x=c(x[c(x6,x6:x8,x8)]), y= c(0, y[x6:x8], 0), col="gray"))
    if (qq[1, ii] > 0 | qq[6, ii] < 0) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = "solid", border = addTrans("green", 200), lwd = 3)
    if ((qq[1, ii] < 0 & qq[2, ii] > 0) | (qq[5, ii] < 0 & qq[6, ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = "solid", border = addTrans("orange", 150), lwd = 3)
    if ((qq[2, ii] < 0 & qq[3, ii] > 0) | (qq[4, ii] < 0 & qq[5, ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = "solid", border = addTrans("gray", 100), lwd = 3)
    # if(qq[1,ii] > 0 | qq[6,ii] < 0) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'green', lwd=3)
    # if((qq[1,ii] < 0 & qq[2,ii] > 0) | (qq[5,ii] < 0 &  qq[6,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'orange', lwd=3)
    # if((qq[2,ii] < 0 & qq[3,ii] > 0) | (qq[4,ii] < 0 &  qq[5,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'gray', lwd=3)
  }
  dev.off()
}
