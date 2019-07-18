library("brms")

#' MBA
#'
#'MBA performs matrix-based analysis (MBA) as theoretically elaborated in the
#'manuscript: https://www.biorxiv.org/content/10.1101/459545v1. The input data should
#'be formulated in a pure-text table that codes the regions and variables. The
#'response variable is usually correlation values (with or without
#'Fisher-transformation) or white-matter properties (e.g., fractional
#'anisotropy, mean diffusivity, radial diffusivity, axial diffusivity, etc.),
#'but it can also be any values from a symmetric matrix (e.g., coherence,
#'mutual information, entropy). In other words, the effects are assumed to be
#'non-directional or non-causal. Diagonals can be included in the input if
#'sensible.

#'0) You are performing Bayesian analysis!!! So, you will directly obtain
#'    the probability of an effect being positive or negative with your data,
#'    instead of witch hunt - hunting the straw man of p-value (weirdness of your
#'    data when pretending that absolutely nothing exists).
#'
#' 1) Avoid using pure numbers to code the labels for categorical variables. The
#'    column order does not matter. . You can specify those column names as you
#'    prefer, but it saves a little bit scripting if you adopt the default naming
#'    for subjects (\'Subj\'), regions (\'ROI1\' and \'ROI2\') and response variable (\'Y\').
#'    The column labels ROI1 and ROI2 are meant to indicate the two regions
#'    associated with each response value, and they do not mean any sequence or
#'    directionality.
#'
#' 2) Only provide half of the off-diagonals in the table (no duplicates allowed).
#'    Missing data are fine (e.g., white-matter property deemed nonexistent).
#'
#' 3) Simple analysis can be done in a few minutes, but computational cost can be
#'    very high (e.g., weeks or even months) when the number of regions or subjects
#'    is large or when a few explanatory variables are involved. Be patient: there
#'    is hope in the near future that further parallelization can be implemented.
#'
#' 4) Add more columns if explanatory variables are considered in the model. Currently
#'    only between-subjects variables (e.g., sex, patients vs. controls, age) are
#'    allowed. Each label in a between-subjects factor (categorical variable)
#'    should be coded with at least 1 character (labeling with pure numbers is fine
#'    but not recommended). If preferred, you can quantitatively code the levels of a
#'    factor yourself by creating k-1 columns for a factor with k levels. However, be
#'    careful with your coding strategy because it would impact how to interpret the
#'    results. Here is a good reference about factor coding strategies:
#'    https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/
#'
#' 5) It is strongly suggested that a quantitative explanatory variable be
#'    standardized with option -stdz; that is, remove the mean and scale by
#'    the standard deviation. This will improve the chance of convergence
#'    with each Markov chain. If a between-subjects factor (e.g., sex) is
#'    involved, it may be better to standardize a quantitative variable
#'    within each group in terms of interpretability if the mean value differs
#'    substantially. However, do not standardize a between-subjects factor if
#'    you quantitatively code it. And do not standardize the response variable
#'    if the intercept is of interest!
#'
#' 6) With within-subject variables, try to formulate the data as a contrast
#'    between two factor levels or as a linear combination of multiple levels.
#'
#' 7) The results from MBA are effect estimates for each region pair and at each
#'    region. They can be slightly different across different runs or different
#'    computers and R package versions due to the nature of randomness involved
#'    in Monte Carlo simulations.
#'
#' 8) The range in matrix plot may vary across different effects within an analysis.
#'    It is possible to force the same range for all plots through fine-tuning
#'    within R using the output of .RData. The criteria of color coding for the
#'    strength of evidence in matrix plots in the output is as follows:
#'     Green  - two-tailed 95% compatible/uncertainty interval (or probability of effect
#'              being positive >= 0.975 or <= 0.025)
#'     Yellow - one-tailed 95% compatible/uncertainty interval (or probability of effect
#'     Gray   - one-tailed 90% compatible/uncertainty interval (or probability of effect
#'              being positive >= 0.90 or <= 0.10)
#'     white  - anything else
#'
#'@param dataTable Path. Path to the file on disk that contains the data structure. The data structure should be a table in long format (cf. wide
#' format) in R with a header as the first line.
#' @param prefix PREFIX. Prefix is used to specify output file names. The main output is
#' a text with prefix appended with .txt and stores inference information
#' for effects of interest in a tabulated format depending on selected
#' options. The prefix will also be used for other output files such as
#' visualization plots such as matrix plot, and saved R data in binary
#' mode. The .RData can be used for post hoc processing such as customized
#' processing and plotting. Remove the .RData file to save disk space once
#' you deem such a file is no longer useful.
#'@param chains N. Specify the number of Markov chains. Make sure there are enough
#' processors available on the computer. Most of the time 4 cores are good
#' enough. However, a larger number of chains (e.g., 8, 12) may help achieve
#' higher accuracy for posterior distribution. Choose 1 for a single-processor
#' computer, which is only practical only for simple models.
#'@param iterations N. Specify the number of iterations per Markov chain. Choose 1000 (default)
#' for simple models (e.g., one or no explanatory variables). If convergence
#' problem occurs as indicated by Rhat being great than 1.1, increase the number of
#' iterations (e.g., 2000) for complex models, which will lengthen the runtime.
#' Unfortunately there is no way to predict the optimum iterations ahead of time.
#'@param model FORMULA. This option specifies the effects associated with explanatory
#' variables. By default (without user input) the model is specified as
#' 1 (Intercept). Currently only between-subjects factors (e.g., sex,
#' patients vs. controls) and quantitative variables (e.g., age) are
#' allowed. When no between-subject factors are present, simply put 1
#' (default) for FORMULA. The expression FORMULA with more than one
#' variable has to be surrounded within (single or double) quotes (e.g.,
#' '1+sex', '1+sex+age'. Variable names in the formula should be consistent
#' with the ones used in the header of data table. A+B represents the
#' additive effects of A and B, A:B is the interaction between A
#' and B, and A*B = A+B+A:B. Subject as a variable should not occur in
#' the model specification here.
#'@param MD  This option indicates that there are missing data in the input. With n
#' regions, at least n(n-1)/2 values are assumed from each subject in the
#' input with no missing data (default). When missing data are present,
#' invoke this option so that the program will handle it properly.
#'@param r2z  This option performs Fisher transformation on the response variable
#' (column Y) if it is correlation coefficient. Do not invoke the option
#' if the transformation has already been applied or the variable is
#' not correlation coefficient.
#'@param cVars variable_list. Identify categorical (qualitive) variables (or
#' factors) with this option. The list with more than one variable
#' has to be separated with comma (,) without any other characters such
#' as spaces and should be surrounded within (single or double) quotes.
#' For example, -cVars "sex,site"
#'@param qVars variable_list. Identify quantitative variables (or covariates) with
#' this option. The list with more than one variable has to be
#' separated with comma (,) without any other characters such as
#' spaces and should be surrounded within (single or double) quotes.
#' For example, -qVars "Age,IQ"
#'@param stdz variable_list. Identify quantitative variables (or covariates) to be
#' standardized. To obtain meaningful and interpretable results and to
#' achieve better convergence of Markov chains with reasonable iterations,
#' it is recommended that all quantitative variables be standardized
#' except for the response variable and indicator variables that code for
#' factors. For example, -stdz \"Age,IQ\". If the mean of a quantitative
#' variables varies substantially between groups, it may make sense to
#' standardize the variable within each group before plugging the values
#' into the data table. Currently MBA does not offer the option to perform
#' within-group standardization.
#'@param EOI variable_list. Identify effects of interest in the output by specifying the
#' variable names separated with comma (,). For example, -EOI \"sex,age\".
#' By default the Intercept is considered to be an effect of interest.
#' Currently only variables, not their interactions, can be directly
#' requested for output. However, most interaction effects can be obtained by
#' either properly coding the variables (see example 3) or post processing.
#'@param qContr contrast_list. Identify comparisons of interest between quantitative
#' variables in the output separated with comma (,). It only allows for
#' pair-wise comparisons between two quantitative variables. For example,
#' -qContr \"age vs IQ, age vs weight, IQ vs weight\", where V1, V2, and V3 are three
#' quantitative variables and three comparisons, V1 - V2, V1 - V3 and V2 - V3
#' will be provided in the output. Make sure that such comparisons are
#' meaningful (e.g., with the same scale and unit. This can be used to
#' formulate comparisons among factor levels if the user quantitatively
#' codes the factor levels.
#'@param Y var_name. var_name is used to specify the column name that is designated as
#' as the response/outcome variable. The default (when this option is not
#' invoked) is 'Y'.
#'@param Subj var_name. var_name is used to specify the column name that is designated as
#' as the measuring unit variable (usually subject). The default (when this
#' option is not invoked) is 'Subj'.
#'@param ROI1 var_name. var_name is used to specify the column name that is designated as
#' as the region variable for the first set of each region pair. The default
#' (when this option is not invoked) is 'ROI1'.
#'@param ROI2 var_name. var_name is used to specify the column name that is designated as
#' as the region variable for the second set of each region pair. The default
#' (when this option is not invoked) is 'ROI2'.
#'@param verb VERB. Speicify verbose
#'@param dbgArgs No longer used
#'
#' @note 1) There should have at least four columns in the table. These minimum
#' four columns can be in any order but with fixed and reserved with labels:
#' 'Subj', 'ROI1', 'ROI2', and 'Y'. The two columns 'ROI1' and 'ROI2' are
#' meant to code the two regions that are associated with each value under the
#' column Y, and they do not connotate any indication of directionality other
#' than you may want to keep track of a consistent order, for example, in the
#' correlation matrix. More columns can be added in the table for explanatory
#' variables (e.g., groups, age, site) if applicable. Only subject-level
#' (or between-subjects) explanatory variables are allowed at the moment. The
#' columns of 'Subj', 'ROI1' and 'ROI2' code each subject and the two regions
#' associated with each region pair, and these labels that can be any identifiable
#' characters including numbers. The column 'Y' can be correlation value,
#' Fisher-transformed correlation value, or white-matter property between
#' grey-matter regions (e.g., mean diffusivity, fractional anisotropy, radial
#' diffusivity and axial diffusivity).
#' @note 2) Each row is associated with one and only one 'Y' value, which is the
#' response variable in the table of long format (cf. wide format) as
#' defined in R. In the case of correlation matrix or white-matter property
#' matrix, provide only half of the off-diagonals. With n regions, there
#' should have at least n(n-1)/2 rows per subject, assuming no missing data.
#' @note 3) It is fine to have variables (or columns) in the table that are
#' not used in the current analysis.
#' @note 4) The context of the table can be saved as a separate file, e.g., called
#' table.txt. In the script specify the data with '-dataTable table.txt'.
#' This option is useful when: (a) there are many rows in the table so that
#' the program complains with an 'Arg list too long' error; (b) you want to
#' try different models with the same dataset.
#' @export
#' @example vignettes/MBA-demo.Rmd
MBA <- function(dataTable,prefix="result",chains=4,iterations=1000,model=1,MD=FALSE,
r2z=FALSE, cVars=NULL, qVars='Intercept', stdz=NA, EOI='Intercept', qContr=NA,
Y='Y', Subj='Subj', ROI1='ROI1', ROI2='ROI2', verb=0,dbgArgs=FALSE){

  fname <- dataTable
  dataTable <- utils::read.table(fname, header=T)
  outFN <- pprefix.AFNI.name(prefix)
  # standardize the names for Y, ROI and subject
  names(dataTable)[names(dataTable)==Subj] <- 'Subj'
  names(dataTable)[names(dataTable)==Y] <- 'Y'
  names(dataTable)[names(dataTable)==ROI1] <- 'ROI1'
  names(dataTable)[names(dataTable)==ROI2] <- 'ROI2'

  # make sure ROI1, ROI2 and Subj are treated as factors
  if(!is.factor(dataTable$ROI1)) dataTable$ROI1 <- as.factor(dataTable$ROI1)
  if(!is.factor(dataTable$ROI2)) dataTable$ROI2 <- as.factor(dataTable$ROI2)
  if(!is.factor(dataTable$Subj)) dataTable$Subj <- as.factor(dataTable$Subj)

  # verify variable types
  if(model==1) terms <- 1 else terms <- strsplit(model, '\\+')[[1]]
  if(length(terms) > 1) {
    #terms <- terms[terms!='1']
    for(ii in 1:length(terms)) {
      if(!is.null(cVars[1])) if(terms[ii] %in% strsplit(cVars, '\\,')[[1]] & !is.factor(dataTable[[terms[ii]]])) # declared factor with quantitative levels
        dataTable[[terms[ii]]] <- as.factor(dataTable[[terms[ii]]])
      if(terms[ii] %in% strsplit(qVars, '\\,')[[1]] & is.factor(dataTable[[terms[ii]]])) # declared numerical variable contains characters
        stop(sprintf('Column %s in the data table is declared as numerical, but contains characters!', terms[ii]))
    }
  }

  # number of ROIs
  nR <- length(union(levels(dataTable$ROI1), levels(dataTable$ROI2)))

  cat('===== Summary of variable information =====', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
  cat(sprintf('Total number of ROIs: %i', nR),
      file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
  cat(sprintf('Response variable Y - mean: %f; SD: %f', mean(dataTable$Y), stats::sd(dataTable$Y)),
      file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
  outDF(summary(dataTable$Y), outFN)
  cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
  cat('Data structure:', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
  outDF(utils::str(dataTable), outFN)
  cat('Subjects:', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
  outDF(summary(dataTable$Subj), outFN)
  cat('ROIs:', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
  outDF(summary(dataTable$ROI1), outFN)
  outDF(summary(dataTable$ROI2), outFN)
  cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)

  if(!MD) if(nlevels(dataTable$Subj)*nR*(nR-1)/2 < nrow(dataTable))
    stop(sprintf('Error: with %d regions and %d subjects, it is expected to have %d rows per subject, leading to toally %d rows in the input data table. However, there are only %d rows. If you have missing data, use option -MD', nR, nlevels(dataTable$Subj), nR*(nR-1)/2, nlevels(dataTable$Subj)*nR*(nR-1)/2, nrow(dataTable)))

  EOIq <- strsplit(qVars, '\\,')[[1]]
  if(!('Intercept' %in% EOIq)) EOIq <- c('Intercept', EOIq)
  EOIq <- intersect(strsplit(EOI, '\\,')[[1]], EOIq)
  if(is.null(cVars)) EOIc <- NA else
    EOIc <- intersect(strsplit(EOI, '\\,')[[1]], strsplit(cVars, '\\,')[[1]])

  if(any(!is.na(qContr))) {
    qContrL <- unlist(strsplit(qContr, '\\,'))
    # verify 'vs' in alternating location
    ll <- which(qContrL %in% 'vs')
    if(!all(ll == seq(2,300,3)[1:length(ll)]))
      stop(sprintf('Quantitative contrast specification -qContr is incorrect!'))
    qContrL <- qContrL[!qContrL %in% 'vs']
    # verify that variable names are correct
    if(!all(qContrL %in% c(QV, 'Intercept')))
      stop(sprintf('At least one of the variable labels in quantitative contrast specification -qContr is incorrect!'))
  }

  # deviation coding: -1/0/1 - the intercept is associated with the mean across the levels of the factor
  # each coding variable corresponds to the level relative to the mean: alphabetically last level is
  # is baseline or reference level
  options(contrasts = c("contr.sum", "contr.poly"))
  options(mc.cores = parallel::detectCores())


  # combine the levels between the two region lists: NO! It seems to mess up the modeling wih brm
  #levels(dataTable$ROI1) <- union(levels(dataTable$ROI1), levels(dataTable$ROI2))
  #levels(dataTable$ROI2) <- union(levels(dataTable$ROI1), levels(dataTable$ROI2))

  # standardization
  if(!is.na(stdz)) {
    sl <- strsplit(stdz, '\\,')[[1]]
    for(ii in 1:length(sl)) if(is.numeric(dataTable[[sl[ii]]]))
      dataTable[[sl[ii]]] <- scale(dataTable[[sl[ii]]], center = TRUE, scale = TRUE) else
        stop(sprintf('The column %s is categorical, not numerical! Why are you asking me to standardize it?', sl[ii]))
  }

  set.seed(1234)
  dataTable$w <- 1

  # Start the clock!
  ptm <- proc.time()

  ## for testing only: remove this soon ####
  #dataTable$V1 <- rnorm(nrow(dataTable))
  #dataTable$V2 <- rnorm(nrow(dataTable), mean=0.5, sd=1)
  #model <- '1+V1+V2'

  ##################### MCMC ####################
  if(model==1) modelForm <- stats::as.formula(paste('Y ~ 1 + (1|Subj) + (1|ROI1:ROI2) +
        (1|mm(ROI1, ROI2, weights = cbind(w, w), scale=FALSE)) +
        (1|mm(ROI1:Subj, ROI2:Subj, weights = cbind(w, w), scale=FALSE))')) else
          modelForm <- stats::as.formula(paste('Y~', model, '+(1|Subj)+(', model, '|ROI1:ROI2)+(',
                                        model, '|mm(ROI1, ROI2, weights = cbind(w, w), scale=FALSE))'))

  if(model==1) fm <- brms::brm(modelForm, data=dataTable, chains = chains,
                             iter=iterations, control = list(adapt_delta = 0.99, max_treedepth = 15)) else
                               fm <- brms::brm(modelForm, data=dataTable,
                                         prior=c(prior(normal(0, 1), class = "Intercept"), prior(normal(0, 0.5), class = "sd")),
                                         chains = chains, iter=iterations, control = list(adapt_delta = 0.99, max_treedepth = 15))

  print(format(Sys.time(), "%D %H:%M:%OS3"))

  post_process(fm,outFN,iterations,chains,nR,EOIq,qContr,EOIc,ptm)

}

