parse_args <- function(args){
  ExecName <- 'MBA'
  lop <- read.MBA.opts.batch(args, verb = 0)
  lop['outFN'] <- lop['prefix']
  lop['prefix'] <- NULL
  lop <- process.MBA.opts(lop, verb = lop$verb)
  lop
}

get_eoiq <- function(qVars, EOI) {
  EOIq <- strsplit(qVars, ",")[[1]]
  if (!("Intercept" %in% EOIq)) EOIq <- c("Intercept", EOIq)
  EOIq <- intersect(strsplit(EOI, ",")[[1]], EOIq)
}

get_eioc <- function(cVars, EOI) {
  if (is.null(cVars)) {
    EOIc <- NA
  } else {
    EOIc <- intersect(strsplit(EOI, ",")[[1]], strsplit(cVars, ",")[[1]])
  }
}


post_process <- function(fm,outFN,iterations,chains,EOIq,EOIc,qContr,ptm,ROI1,ROI2){
  nR <- get_nr(dataTable,c(ROI1,ROI2))
  print(format(Sys.time(), "%D %H:%M:%OS3"))
  # Stop the clock
  proc.time() - ptm

  save.image(file = paste0(outFN, ".RData"))

  cat(format(Sys.time(), "%D %H:%M:%OS3"), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
  cat(utils::capture.output(proc.time() - ptm), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)

  rs <- summary(fm)
  rs_text <- utils::capture.output(rs)
  cat("
++++++++++++++++++++++++++++++++++++++++++++++++++++
")
  cat("***** Summary information of model information *****
")
  cat(rs_text, fill = 2)
  cat("
***** End of model information *****
")
  cat("++++++++++++++++++++++++++++++++++++++++++++++++++++

")

  cat("
***** Summary information of model results *****
", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)


  cat(rs_text, file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
  cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)

  # union(levels(dataTable[ROI1][[1]]), levels(dataTable[ROI2][[1]]))

  # <- list(outFN="Tara", EOI=c("Intercept", "e4", "site"), EOIc=c("e4", "site"), EOIq="Intercept")
  # ["EOIq"]] <- "Intercept"

  ns <- iterations * chains / 2
  # nR <- nlevels(dataTable[ROI1][[1]])
  aa <- brms::fixef(fm, summary = FALSE) # Population-Level Estimates
  bb <- brms::ranef(fm, summary = FALSE) # Extract Group-Level (or random-effect) Estimates

  if (nR != length(dimnames(bb$mmROI1ROI2)[[2]])) {
    cat("
***** Warning: something strange about the ROIs! *****
", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
  }




  ########## region effects #############
  # posterior samples at ROIs for a term

  # gg <- psROI(aa, bb, 'Intercept', nR)

  # summary for ROIs: nd - number of digits to output

  # gg <- sumROI(gg, ns, 3)

  # for Intercept and quantitative variables
  if (any(!is.na(EOIq) == TRUE)) {
    for (ii in 1:length(EOIq)) {
      cat(sprintf("===== Summary of region effects for %s =====", EOIq[ii]),
          file = paste0(outFN, ".txt"), sep = "\n", append = TRUE
      )
      gg <- sumROI(psROI(aa, bb, EOIq[ii], nR), ns, 3)
      cat(utils::capture.output(gg), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
    }
  }

  # for contrasts among quantitative variables
  if (any(!is.na(qContr) == TRUE)) {
    for (ii in 1:(length(qContrL) / 2)) {
      cat(sprintf("===== Summary of region effects for %s vs %s =====", qContrL[2 * ii - 1], qContrL[2 * ii]),
          file = paste0(outFN, ".txt"), sep = "\n", append = TRUE
      )
      gg <- sumROI(psROI(aa, bb, qContrL[2 * ii - 1], nR) - psROI(aa, bb, qContrL[2 * ii], nR), ns, 3)
      cat(utils::capture.output(gg), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
    }
  }

  # for factor
  if (any(!is.na(EOIc) == TRUE)) {
    for (ii in 1:length(EOIc)) {
      lvl <- levels(dataTable[[EOIc[ii]]]) # levels
      nl <- nlevels(dataTable[[EOIc[ii]]]) # number of levels: last level is the reference in deviation coding
      ps <- array(0, dim = c(nl, ns, nR)) # posterior samples
      for (jj in 1:(nl - 1)) ps[jj, , ] <- psROI(aa, bb, paste0(EOIc[ii], jj), nR)
      ps[nl, , ] <- psROI(aa, bb, "Intercept", nR) # Intercept: averge effect
      psa <- array(0, dim = c(nl, ns, nR)) # posterior samples adjusted
      for (jj in 1:(nl - 1)) {
        psa[jj, , ] <- ps[nl, , ] + ps[jj, , ]
        psa[nl, , ] <- psa[nl, , ] + ps[jj, , ]
      }
      psa[nl, , ] <- ps[nl, , ] - psa[nl, , ] # reference level
      dimnames(psa)[[3]] <- dimnames(bb$mmROI1ROI2)[[2]]

      oo <- apply(psa, 1, sumROI, ns, 3)







      cat(sprintf("===== Summary of region effects for %s =====", EOIc[ii]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      for (jj in 1:nl) {
        cat(sprintf("----- %s level: %s", EOIc[ii], lvl[jj]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
        cat(utils::capture.output(oo[[jj]]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
        cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      }

      cat(sprintf("===== Summary of region effects for %s comparisons =====", EOIc[ii]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      for (jj in 1:(nl - 1)) {
        for (kk in (jj + 1):nl) {
          cat(sprintf("----- level comparison: %s vs %s", lvl[jj], lvl[kk]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
          oo <- sumROI(psa[jj, , ] - psa[kk, , ], ns, 3)
          cat(utils::capture.output(oo), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
          cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
        }
      }
    }
  }
  ########## region pair effects #############
  # for intercept or quantitative variable
  if (any(!is.na(EOIq) == TRUE)) {
    for (ii in 1:length(EOIq)) {
      xx <- vv(ww(aa, bb, EOIq[ii], nR, ns), ns, nR)
      cat(sprintf("===== Summary of region pair effects for %s =====", EOIq[ii]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      prnt(90, 1, res(bb, xx, 0.1, 3), outFN, "region pairs")
      prnt(95, 1, res(bb, xx, 0.05, 3), outFN, "region pairs")
      prnt(95, 2, res(bb, xx, 0.025, 3), outFN, "region pairs")
      cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      mPlot(xx, EOIq[ii])
    }
  }

  # for contrasts among quantitative variables
  if (any(!is.na(qContr) == TRUE)) {
    for (ii in 1:(length(qContrL) / 2)) {
      xx <- vv(ww(aa, bb, qContrL[2 * ii - 1], nR, ns) - ww(aa, bb, qContrL[2 * ii], nR, ns), ns, nR)
      cat(sprintf("===== Summary of region pair effects for %s vs %s =====", qContrL[2 * ii - 1], qContrL[2 * ii]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      prnt(90, 1, res(bb, xx, 0.1, 3), outFN, "region pairs")
      prnt(95, 1, res(bb, xx, 0.05, 3), outFN, "region pairs")
      prnt(95, 2, res(bb, xx, 0.025, 3), outFN, "region pairs")
      cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      mPlot(xx, paste0(qContrL[2 * ii - 1], "vs", qContrL[2 * ii]))
    }
  }

  # for factor
  if (any(!is.na(EOIc) == TRUE)) {
    for (ii in 1:length(EOIc)) {
      lvl <- levels(dataTable[[EOIc[ii]]]) # levels
      nl <- nlevels(dataTable[[EOIc[ii]]]) # number of levels: last level is the reference in deviation coding
      ps <- array(0, dim = c(nl, ns, nR, nR)) # posterior samples
      for (jj in 1:(nl - 1)) ps[jj, , , ] <- ww(aa, bb, paste0(EOIc[ii], jj), nR)
      ps[nl, , , ] <- ww(aa, bb, "Intercept", nR)
      psa <- array(0, dim = c(nl, ns, nR, nR)) # posterior samples adjusted
      for (jj in 1:(nl - 1)) {
        psa[jj, , , ] <- ps[nl, , , ] + ps[jj, , , ]
        psa[nl, , , ] <- psa[nl, , , ] + ps[jj, , , ]
      }
      psa[nl, , , ] <- ps[nl, , , ] - psa[nl, , , ] # reference level
      dimnames(psa)[[3]] <- dimnames(bb$mmROI1ROI2)[[2]]
      dimnames(psa)[[4]] <- dimnames(bb$mmROI1ROI2)[[2]]

      # oo <- array(apply(psa, 1, vv, ns, nR), dim=c(nR, nR, 8, nl))
      # dimnames(oo)[[3]] <- c('mean', 'sd', 'P+', '2.5%', '5%', '50%', '95%', '97.5%')

      cat(sprintf("===== Summary of region pair effects for %s =====", EOIc[ii]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      for (jj in 1:nl) {
        cat(sprintf("----- %s level: %s", EOIc[ii], lvl[jj]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
        oo <- vv(psa[jj, , , ], ns, nR)
        prnt(90, 1, res(bb, oo, 0.1, 3), outFN, "region pairs")
        prnt(95, 1, res(bb, oo, 0.05, 3), outFN, "region pairs")
        prnt(95, 2, res(bb, oo, 0.025, 3), outFN, "region pairs")
        cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
        mPlot(oo, paste0(EOIc[ii], "_", lvl[jj]))
      }

      cat(sprintf("===== Summary of region pair effects for %s comparisons =====", EOIc[ii]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
      for (jj in 1:(nl - 1)) {
        for (kk in (jj + 1):nl) {
          cat(sprintf("----- level comparison: %s vs %s", lvl[jj], lvl[kk]), file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
          oo <- vv(psa[jj, , , ] - psa[kk, , , ], ns, nR)
          prnt(90, 1, res(bb, oo, 0.1), outFN, "region pairs")
          prnt(95, 1, res(bb, oo, 0.05), outFN, "region pairs")
          prnt(95, 2, res(bb, oo, 0.025), outFN, "region pairs")
          cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append = TRUE)
          mPlot(oo, paste0(EOIc[ii], "_", lvl[jj], "vs", lvl[kk]))
        }
      }
    }
  }

  # save it again
  save.image(file = paste0(outFN, ".RData"))
  cat("\nCongratulations! The above results are saved in file ", outFN, "\n\n", sep = "")
}




setup_dataTable <- function(data_path,model,MD,r2z,cVars,qVars,stdz,
                            qContr,Y,Subj,ROI1, ROI2=NULL){

  dataTable <- utils::read.table(data_path,header=T)
  # standardize the names for Y, ROI and subject
  names(dataTable)[names(dataTable)==Subj] <- "Subj"
  names(dataTable)[names(dataTable)==Y] <- "Y"
  names(dataTable)[names(dataTable)==ROI1] <- "ROI1"

  # make sure ROI1, ROI2 and Subj are treated as factors
  if(!is.factor(dataTable[ROI1][[1]])) dataTable[ROI1][[1]] <- as.factor(dataTable[ROI1][[1]])
  if(!is.factor(dataTable$Subj)) dataTable$Subj <- as.factor(dataTable$Subj)

  if (!is.null(ROI2)){
    if(!is.factor(dataTable[ROI2][[1]])) dataTable[ROI2][[1]] <- as.factor(dataTable[ROI2][[1]])
    names(dataTable)[names(dataTable)==ROI2] <- "ROI2"
  }
  # verify variable types
  if(model==1) terms <- 1 else terms <- strsplit(model, "\\+")[[1]]
  if(length(terms) > 1) {
    #terms <- terms[terms!="1"]
    for(ii in 1:length(terms)) {
      if(!is.null(cVars[1])) if(terms[ii] %in% strsplit(cVars, ",")[[1]] & !is.factor(dataTable[[terms[ii]]])) # declared factor with quantitative levels
        dataTable[[terms[ii]]] <- as.factor(dataTable[[terms[ii]]])
      if(terms[ii] %in% strsplit(qVars, ",")[[1]] & is.factor(dataTable[[terms[ii]]])) # declared numerical variable contains characters
        stop(sprintf("Column %s in the data table is declared as numerical, but contains characters!", terms[ii]))
    }
  }

  dataTable$w <- 1

  # standardization
  if(!is.null(stdz)) {
    sl <- strsplit(stdz, ",")[[1]]
    for(ii in 1:length(sl)) if(is.numeric(dataTable[[sl[ii]]]))
      dataTable[[sl[ii]]] <- scale(dataTable[[sl[ii]]], center = TRUE, scale = TRUE) else
        stop(sprintf("The column %s is categorical, not numerical! Why are you asking me to standardize it?", sl[ii]))
  }

  # number of ROIs
  nR <- get_nr(dataTable,c(ROI1,ROI2))

  if(!MD) if(nlevels(dataTable$Subj)*nR*(nR-1)/2 > nrow(dataTable))
    stop(sprintf("Error: with %d regions and %d subjects, it is expected to have %d rows per subject, leading to toally %d rows in the input data table. However, there are only %d rows. If you have missing data, use option -MD", nR, nlevels(dataTable$Subj), nR*(nR-1)/2, nlevels(dataTable$Subj)*nR*(nR-1)/2, nrow(dataTable)))



  if(any(!is.null(qContr))) {
    qContrL <- unlist(strsplit(qContr, ","))
    # verify "vs" in alternating location
    ll <- which(qContrL %in% "vs")
    if(!all(ll == seq(2,300,3)[1:length(ll)]))
      stop(sprintf("Quantitative contrast specification -qContr is incorrect!"))
    qContrL <- qContrL[!qContrL %in% "vs"]
    # verify that variable names are correct
    if(!all(qContrL %in% c(QV, "Intercept")))
      stop(sprintf("At least one of the variable labels in quantitative contrast specification -qContr is incorrect!"))
  }

  dataTable
}

#' Get number of rows based on the count of variable levels
#'
#' Given a dataframe with columns that represent categorical
#' variables this function will return the total number of unique
#' elements that are found across all columns.
#'
#' @param df A dataframe in which some categorical variables are stored
#' @param col_names The column labels that refer to categorical variables
#' used to fit the model
#'
#' @return count
#' @export
#'
#' @examples
#' col_names <- c("cat_var_1","cat_var_2")
#'
#' df <- tibble::tribble(
#' ~col_1, ~cat_var_1,       ~cat_var_2,
#' "text", "unique_val_1",   "unique_val_1",
#' "text", "unique_val_2",   "unique_val_1",
#' "text", "unique_val_1",   "unique_val_3",
#' "text", "unique_val_1",   "unique_val_4",
#' )
#'
#' get_nr(df,col_names)
get_nr <- function(df,roi_names){
  purrr::map(roi_names, ~ as.character(df[.x][[1]])) %>%
    purrr::flatten_chr() %>%
    dplyr::n_distinct()
}

run_mba <- function(dataTable,model,chains,iterations){
  set.seed(1234)
  if(model==1) {

    modelForm <- stats::as.formula(paste("Y ~ 1 + (1|Subj) + (1|ROI1:ROI2) +
        (1|mm(ROI1, ROI2, weights = cbind(w, w), scale=FALSE)) +
        (1|mm(ROI1:Subj, ROI2:Subj, weights = cbind(w, w), scale=FALSE))"))
  }else{

    modelForm <- stats::as.formula(paste("Y~", model, "+(1|Subj)+(", model, "|ROI1:ROI2)+(",
                                         model, "|mm(ROI1, ROI2, weights = cbind(w, w), scale=FALSE))"))
  }
  if(model==1){

    fm <- brm(modelForm, data=dataTable, chains = chains,
              iter=iterations, control = list(adapt_delta = 0.99, max_treedepth = 15))
  }else{

    fm <- brm(modelForm, data=dataTable,
              prior=c(prior(normal(0, 1), class = "Intercept"),prior(normal(0, 0.5), class = "sd")),
              chains = chains, iter=iterations, control = list(adapt_delta = 0.99, max_treedepth = 15))
    fm
  }
}

log_setup_info <- function(dataTable,outFN,ROI1,ROI2=NULL){
  nR <- get_nr(dataTable,c(ROI1,ROI2))
  cat("===== Summary of variable information =====", file = paste0(outFN, ".txt"), sep = "\n", append=TRUE)
  cat(sprintf("Total number of ROIs: %i", nR),
      file = paste0(outFN, ".txt"), sep = "\n", append=TRUE)
  cat(sprintf("Response variable Y - mean: %f; SD: %f", mean(dataTable$Y), stats::sd(dataTable$Y)),
      file = paste0(outFN, ".txt"), sep = "\n", append=TRUE)
  outDF(summary(dataTable$Y), outFN)
  cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append=TRUE)
  cat("Data structure:", file = paste0(outFN, ".txt"), sep = "\n", append=TRUE)
  outDF(utils::str(dataTable), outFN)
  cat("Subjects:", file = paste0(outFN, ".txt"), sep = "\n", append=TRUE)
  outDF(summary(dataTable$Subj), outFN)
  cat("ROIs:", file = paste0(outFN, ".txt"), sep = "\n", append=TRUE)
  outDF(summary(dataTable[ROI1][[1]]), outFN)
  if (!is.null(ROI2)) outDF(summary(dataTable[ROI2][[1]]), outFN)
  cat("\n", file = paste0(outFN, ".txt"), sep = "\n", append=TRUE)
}

# write data.frame to a file
outDF <- function(DF, fl) cat(utils::capture.output(DF), file = paste0(fl, '.txt'), sep = '\n', append=TRUE)

# Fisher transformation
fisher <- function(r) ifelse(abs(r) < .995, 0.5*(log(1+r)-log(1-r)), stop('Are you sure that you have correlation values so close to 1 or -1?'))

# compute P+
cnt <- function(x, ns) return(sum(x>0)/ns)

# extract region-pair posterior samples for an effect 'tm'
ww <- function(aa, bb, tm, nR,ns) {
  ps0 <- array(apply(bb[['mmROI1ROI2']][,,tm], 2, "+", bb[['mmROI1ROI2']][,,tm]), c(ns, nR, nR))
  ps <- apply(ps0, c(2,3), '+', aa[,tm])

  dimnames(ps) <- list(1:ns, dimnames(bb$mmROI1ROI2)[[2]], dimnames(bb$mmROI1ROI2)[[2]])
  tmp <- ps

  sel1 <- match(dimnames(bb$`ROI1:ROI2`)[[2]], outer(dimnames(ps)[[2]],dimnames(ps)[[3]], function(x,y) paste(x,y,sep="_")))
  sel2 <- match(dimnames(bb$`ROI1:ROI2`)[[2]], outer(dimnames(ps)[[2]],dimnames(ps)[[3]], function(x,y) paste(y,x,sep="_")))

  ad <- function(tt,bb,s1,s2) {tt[s1] <- tt[s1] + bb; tt[s2] <- tt[s2] + bb; return(tt)}
  for(ii in 1:ns) tmp[ii,,] <- ad(tmp[ii,,], bb$`ROI1:ROI2`[ii,,tm], sel1, sel2)
  ps <- tmp
  return(ps)
}
# ps <- ww(aa, bb, 'Intercept', nR)

# obtain summary informatin of posterior samples for RPs
vv <- function(ps, ns, nR) {
  mm <- apply(ps, c(2,3), mean,ns)
  for(ii in 1:nR) for(jj in 1:nR) ps[,ii,jj] <- sqrt(2)*(ps[,ii,jj] - mm[ii,jj]) + mm[ii,jj]
  RP <- array(NA, dim=c(nR, nR, 8))
  RP[,,1] <- apply(ps, c(2,3), mean)
  RP[,,2] <- apply(ps, c(2,3), stats::sd)
  RP[,,3] <- apply(ps, c(2,3), cnt, ns)
  RP[,,4:8] <- aperm(apply(ps, c(2,3), stats::quantile, probs=c(0.025, 0.05, 0.5, 0.95, 0.975)), dim=c(2,3,1))
  dimnames(RP)[[1]] <- dimnames(ps)[[2]]
  dimnames(RP)[[2]] <- dimnames(ps)[[3]]
  dimnames(RP)[[3]] <- c('mean', 'SD', 'P+', '2.5%', '5%', '50%', '95%', '97.5%')
  return(RP)
}

# full region pair result without thresholding
#xx <- vv(ww(aa, bb, 'Intercept', nR), ns, nR)
#subset(xx[,,c(1,8)], xx[,,'P+'] >= 0.975 | xx[,,'P+'] <= 0.025)

# graded thresholding
res <- function(bb, xx, pp, nd) {
  RP <- which(xx[,,'P+'] >= 1-pp | xx[,,'P+'] <= pp, arr.ind = T)
  RP <- RP[RP[,1] < RP[,2],]
  tmp <- data.frame(ROI1=factor(), ROI2=factor(), mean=factor(), SD=factor(), `P+`=factor(), check.names = FALSE)
  if(length(RP) > 2) {
    tmp <- cbind(dimnames(bb$mmROI1ROI2)[[2]][RP[,1]], dimnames(bb$mmROI1ROI2)[[2]][RP[,2]],
                 round(t(mapply(function(i, j) xx[i, j, 1:3], RP[,1], RP[,2])), nd))
    colnames(tmp)[1:2] <- c('ROI1', 'ROI2')
    tmp <- data.frame(tmp, row.names = NULL, check.names = FALSE) } else
      if(length(RP)==2) {
        tmp <- c(dimnames(bb$mmROI1ROI2)[[2]][RP[1]], dimnames(bb$mmROI1ROI2)[[2]][RP[2]], round(xx[RP[1], RP[2], 1:3],3))
        #tmp <- paste(RP[1], RP[2], round(xx[RP[1], RP[2], 1:3], nd))
        #names(tmp)[1:2] <- c('ROI1', 'ROI2')
        tmp <- data.frame(t(tmp), row.names = NULL, check.names = FALSE)
      }
  return(tmp)
}

# standardize the output
prnt <- function(pct, side, dat, fl, entity) {
  cat(sprintf('***** %i %s based on %i-sided %i uncertainty interval *****',
              nrow(dat), entity, side, pct), file = paste0(fl, '.txt'), sep = '\n', append=TRUE)
  if(nrow(dat) > 0) cat(utils::capture.output(dat), file = paste0(fl, '.txt'), sep = '\n', append=TRUE) else
    cat('NULL', file = paste0(fl, '.txt'), sep = '\n', append=TRUE)
}

# matrix plot for RPs: assuming no diagonals for now


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
  rgb <- rbind(grDevices::col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

mPlot <- function(xx, fn) {
  mm <- xx[,,6]  # median
  pp <- xx[,,3]  # P+
  BC1 <- ((pp >= 0.975 ) | (pp <= 0.025))   # background color
  BC  <- ((pp >= 0.95 ) | (pp <= 0.05))   # background color
  BC2 <- (((pp > 0.9) & (pp < 0.95)) | ((pp < 0.1) & (pp > 0.05)))
  BC[BC  == T] <- addTrans('yellow',150)
  BC[BC1 == T] <- addTrans('green',175)
  BC[BC  == F] <- "white"
  BC[BC2 == T] <- addTrans('gray',125)
  #BC[BC  == T] <- "blue"
  #BC[BC1 == T] <- "green"
  #BC[BC  == F] <- "white"
  #BC[BC2 == T] <- 'yellow'
  rng <- range(mm)
  diag(mm) <- NA  # diagonals are meaningful in the case of correlation matrix
  diag(BC) <- "white" # if the diagonal values shall be white
  ii <- !kronecker(diag(1, nrow(BC)), matrix(1, ncol=1, nrow=1))
  BC <- matrix(BC[ii], ncol = ncol(BC)-1)
  col2 <- grDevices::colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                                        "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                                        "#4393C3", "#2166AC", "#053061"))
  grDevices::pdf(paste0(fn, ".pdf"), width=8, height=8)
  corrplot::corrplot(mm, method="circle", type = "full", is.corr = FALSE, bg=BC, tl.pos='lt', tl.col='black', col=rev(col2(200)), cl.pos='r', na.label = "square", na.label.col='white')
  grDevices::dev.off()
}

sumROI <- function(R0, ns, nd) {
  hubs <- data.frame(cbind(apply(R0, 2, mean), apply(R0, 2, stats::sd), apply(R0, 2, cnt, ns), t(apply(R0, 2, stats::quantile,
                                                                                                       probs=c(0.025, 0.05, 0.5, 0.95, 0.975)))))
  names(hubs) <- c('mean', 'SD', 'P+', '2.5%', '5%', '50%', '95%', '97.5%')
  return(round(hubs,nd))
}

psROI <- function(aa, bb, tm, nR) {
  R0 <- apply(bb$mmROI1ROI2[,,tm], 2, '+', 0.5*aa[,tm])
  for(jj in 1:nR) {
    mm <- stats::quantile(R0[,jj], probs=.5)
    R0[,jj] <- sqrt(2)*(R0[,jj] - mm)+mm
  }
  return(R0)
}


first.in.path <- function(file) {
  ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
  ff<-ff[lapply(ff,file.exists)==TRUE];
  #cat('Using ', ff[1],'\n');
  return(gsub('//','/',ff[1], fixed=TRUE))
}

pprefix.AFNI.name <- function(an) {
  if (is.character(an)) an <- parse.AFNI.name(an);
  return(an$pprefix);
}

prefix.AFNI.name <- function(an) {
  if (is.character(an)) an <- parse.AFNI.name(an);
  return(an$prefix);
}

view.AFNI.name <- function(an) {
  if (is.character(an)) an <- parse.AFNI.name(an);
  return(an$view);
}

pv.AFNI.name <- function(an) {
  if (is.character(an)) an <- parse.AFNI.name(an);
  return(paste(an$pprefix,an$view,sep=''));
}

head.AFNI.name <- function(an) {
  if (is.character(an)) an <- parse.AFNI.name(an);
  if (an$type == 'BRIK' && !is.na(an$view)) {
    return(paste(an$pprefix,an$view,".HEAD",sep=''));
  } else {
    return((an$orig_name));
  }
}

brik.AFNI.name <- function(an) {
  if (is.character(an)) an <- parse.AFNI.name(an);
  if (an$type == 'BRIK' && !is.na(an$view)) {
    return(paste(an$pprefix,an$view,".BRIK",sep=''));
  } else {
    return((an$orig_name));
  }
}

compressed.AFNI.name <- function(an) {
  if (is.character(an)) an <- parse.AFNI.name(an);
  if (length(grep('\\.gz$', an$ext))) {
    return('gz')
  } else if (length(grep('\\.bz2$', an$ext))) {
    return('bz2')
  } else if (length(grep('\\.Z$', an$ext))) {
    return('Z')
  } else {
    return('')
  }

}

modify.AFNI.name <- function (name, what="append", val="_new", cwd=NULL) {
  if (!is.loaded('R_SUMA_ParseModifyName')) {
    err.AFNI("Missing R_io.so");
    return(NULL);
  }
  an <- .Call("R_SUMA_ParseModifyName",
              name = name,
              what = what,
              val = val,
              cwd = cwd)
  return(an)
}


parse.AFNI.name <- function(filename, verb = 0) {
  if (filename == '-self_test') { #Secret testing flag
    note.AFNI('Function running in test mode');
    show.AFNI.name(parse.AFNI.name('DePath/hello.DePrefix', verb))
    show.AFNI.name(parse.AFNI.name('DePath/DePrefix+acpc', verb))
    show.AFNI.name(parse.AFNI.name('DePath/DePrefix+acpc.', verb))
    show.AFNI.name(parse.AFNI.name('DePath/DePrefix+acpc.HEAD', verb))
    show.AFNI.name(parse.AFNI.name('DePath/DePrefix+acpc.BRIK.gz', verb))
    show.AFNI.name(parse.AFNI.name('DePath/DePrefix+acpc.HEAD[23]', verb))
    show.AFNI.name(
      parse.AFNI.name('DePath/DePrefix+acpc.HEAD[DeLabel]{DeRow}', verb))
    show.AFNI.name(
      parse.AFNI.name('DePath/DePrefix+acpc[DeLabel]{DeRow}', verb))
    show.AFNI.name(
      parse.AFNI.name('DePath/DePrefix+acpc.[DeLabel]{DeRow}', verb))
    return(NULL)
  }
  an <- list()
  an$view <- NULL
  an$pprefix <- NULL
  an$brsel <- NULL;
  an$rosel <- NULL;
  an$rasel <- NULL;
  an$insel <- NULL;
  an$type <- NULL;
  an$path <- NULL;
  an$orig_name <- filename;
  an$file <- NULL;

  if (verb) { cat ('Parsing >>',filename,'<<\n', sep=''); }
  if (!is.character(filename)) {
    warning(paste('filename >>',
                  filename, '<< not a character string\n', sep=''),
            immediate. = TRUE);
    traceback();
    return(NULL);
  }
  #Deal with special names:
  if (length(grep("^1D:.*$",filename))) {
    an$type = '1Ds'
    return(an)
  } else if (length(grep("^R:.*$",filename))) {
    an$type = 'Rs'
    return(an)
  }

  #Deal with selectors
  n <- parse.AFNI.name.selectors(filename, verb)
  filename <- n$name
  an$file  <- n$name
  an$brsel <- n$brsel;
  an$rosel <- n$rosel;
  an$rasel <- n$rasel;
  an$insel <- n$insel;

  #Remove last dot if there
  filename <- sub('\\.$','',filename)

  #NIFTI?
  n <- strip.extension(filename, c('.nii', '.nii.gz'), verb)
  if (n$ext != '') {
    an$ext <- n$ext
    an$type <- 'NIFTI'
    an$pprefix <- n$name_noext
  } else {
    #remove other extensions
    n <- strip.extension(filename, c('.HEAD','.BRIK','.BRIK.gz',
                                     '.BRIK.bz2','.BRIK.Z',
                                     '.1D', '.1D.dset',
                                     '.niml.dset',
                                     '.'  ),
                         verb)
    if (n$ext == '.1D' || n$ext == '.1D.dset') {
      an$type <- '1D'
    } else if (n$ext == '.niml.dset') {
      an$type <- 'NIML'
    } else {
      an$type <- 'BRIK'
    }

    if (n$ext == '.') {
      n$ext <- ''
    }
    an$ext <- n$ext
    filename <- n$name_noext

    n <- strip.extension(filename, c('+orig','+tlrc','+acpc'), verb)
    if (n$ext != '') {
      an$view <- n$ext
    } else {
      an$view <- NA
    }
    an$pprefix <- n$name_noext
  }

  #a prefix with no path
  an$prefix <- basename(an$pprefix)

  #and the path
  an$path <- dirname(an$orig_name)

  if (verb > 2) {
    note.AFNI("Browser not active");
    # browser()
  }
  if (  an$type != '1D' && (
    !is.null(an$brsel) || !is.null(an$rosel) ||
    !is.null(an$rasel) || !is.null(an$insel))) {
    #Remove trailing quote if any
    an$prefix <- gsub("'$", '', an$prefix);
    an$prefix <- gsub('"$', '', an$prefix);
    an$pprefix <- gsub("'$",'', an$pprefix);
    an$pprefix <- gsub('"$','', an$pprefix);
  }

  if ( an$type != 'BRIK' ) {
    #Put the extension back on
    an$pprefix <- paste(an$pprefix,an$ext, sep='');
    an$prefix <- paste(an$prefix,an$ext, sep='');
  }
  return(an)
}

exists.AFNI.name <- function(an) {
  if (is.character(an)) an <- parse.AFNI.name(an);

  ans <- 0
  if (file.exists(head.AFNI.name(an))) ans <- ans + 1;

  if (file.exists(brik.AFNI.name(an)) ||
      file.exists(paste(brik.AFNI.name(an),'.gz', sep='')) ||
      file.exists(paste(brik.AFNI.name(an),'.Z', sep=''))) ans <- ans + 2;
  return(ans);
}

AFNI.new.options.list <- function(history = '', parsed_args = NULL) {
  lop <- list (com_history = history);
  #Look for defaults
  lop$overwrite <- FALSE
  for (i in 1:length(parsed_args)) {
    opname <- strsplit(names(parsed_args)[i],'^-')[[1]];
    opname <- opname[length(opname)];
    switch(opname,
           overwrite = lop$overwrite <- TRUE )
  }
  return(lop)
}

parse.AFNI.name.selectors <- function(filename,verb=0) {
  n <- list()
  n$brsel<- NULL;
  n$rosel<- NULL;
  n$rasel<- NULL;
  n$insel<- NULL;

  selecs <- strsplit(filename,"\\[|\\{|<|#")[[1]];
  n$name <- selecs[1]
  for (ss in selecs[2:length(selecs)]) {
    if (length(grep("]",ss))) {
      n$brsel <- strsplit(ss,"\\]")[[1]][1];
    } else if (length(grep("}",ss))) {
      n$rosel <- strsplit(ss,"\\}")[[1]][1];
    } else if (length(grep(">",ss))) {
      n$rasel <- strsplit(ss,">")[[1]][1];
    }
  }
  selecs <- strsplit(filename,"#")[[1]];
  if (length(selecs) > 1) {
    n$insel <- selecs[2]
  }

  return(n)
}

strip.extension <- function (filename, extvec=NULL, verb=0) {
  n <- list()
  if (is.null(extvec)) {
    ff <- strsplit(filename, '\\.')[[1]]
    if (length(ff) > 1) {
      n$ext <- paste('.',ff[length(ff)], sep='')
      n$name_noext <- paste(ff[1:length(ff)-1],collapse='.')
    } else {
      n$ext <- ''
      n$name_noext <- filename
    }
  } else {
    n$ext <- ''
    n$name_noext <- filename
    for (ex in extvec) {
      patt <- paste('\\',ex,'$',collapse='', sep='')
      if (length(grep(patt, filename))) {
        n$ext <- ex
        n$name_noext <- sub(patt,'',filename)
        return(n)
      }
    }
  }
  return(n)
}
