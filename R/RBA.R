#'Region-Based Analysis Program through Bayesian Multilevel Modeling
#'
#'RBA performs region-based analysis (RBA) as theoretically elaborated in the
#'manuscript: https://rdcu.be/bhhJp and is conducted with a shell script (as
#'shown in the examples below). The input data should be formulated in a
#'pure-text table that codes the regions and variables. The response variable is
#'some effect at the individual subject level.
#'
#' \enumerate{
#' \item A data table in pure text format is needed as input for an RBA script. The
#' data table should contain at least 3 columns that specify the information
#' about subjects, regions and the response variable values with the following
#' fixed header. The header labels are case-sensitive, and their order does not
#' matter.
#' \preformatted{
#' Subj   ROI        Y      Age
#' S1     Amyg    0.2643    11
#' S2     BNST    0.3762    16
#' ...}
#' \item You are performing Bayesian analysis!!! So, you will directly obtain
#'    the probability of an effect being positive or negative with your data,
#'    instead of witch hunt-hunting the straw man of p-value (weirdness of your
#'    data when pretending that absolutely nothing exists).
#' \item Avoid using pure numbers to code the labels for categorical variables. The
#'    column order does not matter. You can specify those column names as you
#'    prefer, but it saves a little bit scripting if you adopt the default naming
#'    for subjects (\'Subj\'), regions (\'ROI\') and response variable (\'Y\').
#' \item Add more columns if explanatory variables are considered in the model. Currently
#'    only between-subjects variables (e.g., sex, patients vs. controls, age) are
#'    allowed. Capability of modeling within-subject or repeated-measures variables
#'    may be added in the future. Each label in a between-subjects factor (categorical
#'    variable) should be coded with at least 1 character (labeling with pure numbers
#'    is fine but not recommended). If preferred, you can quantitatively code the
#'    levels of a factor yourself by creating k-1 columns for a factor with k levels.
#'    However, be careful with your coding strategy because it would impact how to
#'    interpret the results. Here is a good reference about factor coding strategies:
#'    https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/
#' \item It is strongly suggested that a quantitative explanatory variable be
#'    standardized with option -stdz; that is, remove the mean and scale by
#'    the standard deviation. This will improve the chance of convergence
#'    with each Markov chain. If a between-subjects factor (e.g., sex) is
#'    involved, it may be better to standardize a quantitative variable
#'    within each group in terms of interpretability if the mean value differs
#'    substantially. However, do not standardize a between-subjects factor if
#'    you quantitatively code it. And do not standardize the response variable
#'    if the intercept is of interest!
#' \item For within-subject variables, try to formulate the data as a contrast
#'    between two factor levels or as a linear combination of multiple levels.
#' \item The results from RBA are effect estimates for each region. They can be
#'    slightly different across different runs or different computers and R
#'    package versions due to the nature of randomness involved in Monte Carlo
#'    simulations.
#' \item The evidence for region effects in the output can be assessed through P+,
#'    the probability that the effect is postive conditional on the current
#'    dataset. The following criterion for highlighting the results is only
#'    suggestive:
#'    \preformatted{ P+ >= 0.975 or <= 0.025 - very strong evidence
#'     P+ >= 0.95 or <= 0.05   - strong evidence
#'     P+ >= 0.90 or <= 0.10   - moderate evidence
#'     else                    - little evidence}
#'    The same criterion is applied to the color coding in the posterior
#'    distribution plots generated with the option -PDP:
#'     \preformatted{P+ >= 0.975 or <= 0.025 - green:    very strong evidence
#'     P+ >= 0.95 or <= 0.05   - yellow:   strong evidence
#'     P+ >= 0.90 or <= 0.10   - gray:     moderate evidence
#'     else                    - no color: little evidence}}
#'@param prefix   Prefix is used to specify output file names. The main output
#'  is a text with prefix appended with .txt and stores inference information
#'  for effects of interest in a tabulated format depending on selected options.
#'  The prefix will also be used for other output files such as visualization
#'  plots, and saved R data in binary format. The .RData can be used for post
#'  hoc processing such as customized processing and plotting. Remove the .RData
#'  file to save disk space once you deem such a file is no longer useful.
#'@param chains  Specify the number of Markov chains. Make sure there are enough
#'  processors available on the computer. Most of the time 4 cores are good
#'  enough. However, a larger number of chains (e.g., 8, 12) may help achieve
#'  higher accuracy for posterior distribution. Choose 1 for a single-processor
#'  computer, which is only practical only for simple models.
#'@param iterations  Specify the number of iterations per Markov chain. Choose
#'  1000 (default) for simple models (e.g., one or no explanatory variables). If
#'  convergence problem occurs as indicated by Rhat being great than 1.1,
#'  increase the number of iterations (e.g., 2000) for complex models, which
#'  will lengthen the runtime. Unfortunately there is no way to predict the
#'  optimum iterations ahead of time.
#'@param verb  Specify verbose level.
#'@param model  This option specifies the effects associated with explanatory
#'  variables. By default (without user input) the model is specified as 1
#'  (Intercept). Currently only between-subjects factors (e.g., sex, patients
#'  vs. controls) and quantitative variables (e.g., age) are allowed. When no
#'  between-subject factors are present, simply put 1 (default) for FORMULA. The
#'  expression FORMULA with more than one variable has to be surrounded within
#'  (single or double) quotes (e.g., '1+sex', '1+sex+age'. Variable names in the
#'  formula should be consistent with the ones used in the header of data table.
#'  A+B represents the additive effects of A and B, A:B is the interaction
#'  between A and B, and A*B = A+B+A:B. Subject as a variable should not occur
#'  in the model specification here.
#'@param dbgArgs  This option will enable R to save the parameters in a file
#'  called .RBA.dbg.AFNI.args in the current directory so that debugging can be
#'  performed.
#'@param MD  This option indicates that there are missing data in the input.
#'  With n regions, at least n(n-1)/2 values are assumed from each subject in
#'  the input with no missing data (default). When missing data are present,
#'  invoke this option so that the program will handle it properly.
#'@param r2z  This option performs Fisher transformation on the response
#'  variable (column Y) if it is correlation coefficient.
#'@param cVars  Identify categorical (qualitive) variables (or factors) with
#'  this option. The list with more than one variable has to be separated with
#'  comma (,) without any other characters such as spaces and should be
#'  surrounded within (single or double) quotes. For example, -cVars
#'  \"sex,site\"
#'@param qVars  Identify quantitative variables (or covariates) with this
#'  option. The list with more than one variable has to be separated with comma
#'  (,) without any other characters such as spaces and should be surrounded
#'  within (single or double) quotes. For example, -qVars \"Age,IQ\"
#'@param stdz  Identify quantitative variables (or covariates) to be
#'  standardized. To obtain meaningful and interpretable results and to achieve
#'  better convergence of Markov chains with reasonable iterations, it is
#'  recommended that all quantitative variables be standardized except for the
#'  response variable and indicator variables that code for factors. For
#'  example, -stdz \"Age,IQ\". If the mean of a quantitative variables varies
#'  substantially between groups, it may make sense to standardize the variable
#'  within each group before plugging the values into the data table. Currently
#'  RBA does not offer the option to perform within-group standardization.
#'@param EOI  Identify effects of interest in the output by specifying the
#'  variable names separated with comma (,). For example, -EOI \"sex,age\". By
#'  default the Intercept is considered to be an effect of interest. Currently
#'  only variables, not their interactions, can be directly requested for
#'  output. However, most interaction effects can be obtained by either properly
#'  coding the variables (see example 3) or post processing.
#'@param qContr  Identify comparisons of interest between quantitative variables
#'  in the output separated with comma (,). It only allows for pair-wise
#'  comparisons between two quantitative variables. For example, \"age vs IQ,
#'  age vs weight, IQ vs weight\", where V1, V2, and V3 are three quantitative
#'  variables and three comparisons, V1 - V2, V1 - V3 and V2 - V3 will be
#'  provided in the output. Make sure that such comparisons are meaningful
#'  (e.g., with the same scale and unit. This can be used to formulate
#'  comparisons among factor levels if the user quantitatively codes the factor
#'  levels.
#'@param Y  var_name is used to specify the column name that is designated as as
#'  the response/outcome variable. The default (when this option is not invoked)
#'  is 'Y'.
#'@param Subj  var_name is used to specify the column name that is designated as
#'  as the measuring unit variable (usually subject). The default (when this
#'  option is not invoked) is 'Subj'.
#'@param ROI  var_name is used to specify the column name that is designated as
#'  as the region variable. The default (when this option is not invoked) is
#'  'ROI'.
#'@param PDP  Specify the layout of posterior distribution plot (PDP) with nr
#'  rows and nc columns among the number of plots. For example, with 16 regions,
#'  you can set the number of rows and columns to each. be 4. The region names
#'  will be shown in each plot. So label the regions concisely.
#'@param dataTable  List the data structure in a table of long format (cf. wide
#'  format) in R with a header as the first line.
#'@references If you want to cite the approach for RBA, consider the following:
#'
#'  Chen G, Xiao Y, Taylor PA, Riggins T, Geng F, Redcay E, 2019. Handling
#'  Multiplicity in Neuroimaging through Bayesian Lenses with Multilevel
#'  Modeling. Neuroinformatics. https://rdcu.be/bhhJp
#'
#'  Thanks to Paul-Christian Bürkner and the Stan/R communities for the strong support.
#'
#'@note
#'\enumerate{
#'\item There should have at least three columns in the table. These minimum
#'three columns can be in any order but with fixed and reserved with labels:
#''Subj', 'ROI', and 'Y'. The column 'ROI' is meant to code the regions that are
#'associated with each value under the column Y. More columns can be added in
#'the table for explanatory variables (e.g., groups, age, site) if applicable.
#'Only subject-level (or between-subjects) explanatory variables are allowed at
#'the moment. The labels for the columns of 'Subj' and 'ROI' can be any
#'identifiable characters including numbers. \item Each row is associated with one
#'and only one 'Y' value, which is the response variable in the table of long
#'format (cf. wide format) as defined in R. With n subjects and m regions, there
#'should have totally mn rows, assuming no missing data. \item It is fine to have
#'variables (or columns) in the table that are not used in the current analysis.
#'\item The context of the table can be saved as a separate file, e.g., called
#'table.txt. In the script specify the data with '-dataTable table.txt'. This
#'option is useful when: (a) there are many rows in the table so that the
#'program complains with an 'Arg list too long' error; (b) you want to try
#'different models with the same dataset.}
#'@export
#'@author \href{https://afni.nimh.nih.gov/gangchen_homepage}{Gang Chen} \email{gangchen@@mail.nih.gov}
#'@seealso \code{\link{MBA}}
#'@section CLI info:
#'See \code{\link{create_parser_from_function}} for details on this section.
#' \preformatted{
#' ~dest,         ~metavar,                ~type,         ~nargs,
#' "prefix",       "PREFIX",                "character",   "1",
#' "chains",       "N",                     "integer",     "1",
#' "iterations",   "N",                     "integer",     "1",
#' "verb",         "N",                     "integer",     "1",
#' "model",        "FORMULA",               "character",   "1",
#' "dbgArgs",      " ",                     "character",   "0",
#' "MD",           " ",                     "character",   "0",
#' "r2z",          " ",                     "character",   "0",
#' "cVars",        "CSV_STRING",            "character",   "1",
#' "qVars",        "CSV_STRING",            "character",   "1",
#' "stdz",         "CSV_STRING",            "character",   "1",
#' "EOI",          "CSV_STRING",            "character",   "1",
#' "qContr",       "CSV_STRING",            "character",   "1",
#' "Y",            "STRING",                "character",   "1",
#' "Subj",         "STRING",                "character",   "1",
#' "ROI",          "STRING",                "character",   "1",
#' "PDP",          "NDIM",                  "integer",     "2",
#' "dataTable",    "PATH or DATA_AS_STRING","character",   "1",
#' }
RBA <- function(dataTable,prefix,chains=1, iterations=1000, model=1, cVars=NULL, qVars='Intercept', stdz=NULL, EOI='Intercept', qContr=NULL, Y='Y', Subj='Subj', ROI='ROI', PDP=NULL,
dbgArgs=FALSE , MD=FALSE , r2z=FALSE, verb=0,overwrite=FALSE){
  dpath <-dataTable
  dataTable <- utils::read.table(dpath,header=T)

  outFN  <- pprefix.AFNI.name(prefix)
  an <- parse.AFNI.name(outFN)
   if(!overwrite && (
            file.exists(paste0(dataTable,".txt")) ||
            file.exists(paste0(dataTable,".RData")) ||
            file.exists(paste0(dataTable,".pdf"))) ) {
         errex.AFNI(c("File ", dataTable, " exists! Try a different name.\n"))
         return(NULL)
   }

if(!is.null(cVars[1])) CV <- strsplit(cVars, '\\,')[[1]]
if(!is.na(qVars[1])) QV <- strsplit(qVars, '\\,')[[1]]
# standardize the names for Y, ROI and subject
names(dataTable)[names(dataTable)==Subj] <- 'Subj'
names(dataTable)[names(dataTable)==Y] <- 'Y'
names(dataTable)[names(dataTable)==ROI] <- 'ROI'

# make sure ROI and Subj are treated as factors
if(!is.factor(dataTable$ROI)) dataTable$ROI <- as.factor(dataTable$ROI)
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
nR <- nlevels(dataTable$ROI)

cat('===== Summary of variable information =====', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
cat(sprintf('Total number of ROIs: %i', nR),
   file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
cat(sprintf('Response variable Y - mean: %f; SD: %f', mean(dataTable$Y), sd(dataTable$Y)),
   file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
outDF(summary(dataTable$Y), outFN)
cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
cat('Data structure:', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
outDF(str(dataTable), outFN)
cat('Subjects:', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
outDF(summary(dataTable$Subj), outFN)
cat('ROIs:', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
outDF(summary(dataTable$ROI), outFN)

cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)

if(!MD) if(nlevels(dataTable$Subj)*nR < nrow(dataTable))
   stop(sprintf('Error: with %d regions and %d subjects, it is expected to have %d rows per subject, leading to toally %d rows in the input data table. However, there are only %d rows. If you have missing data, use option -MD', nR, nlevels(dataTable$Subj), nR, nlevels(dataTable$Subj)*nR, nrow(dataTable)))

EOIq <- strsplit(qVars, '\\,')[[1]]
if(!('Intercept' %in% EOIq)) EOIq <- c('Intercept', EOIq)
EOIq <- intersect(strsplit(EOI, '\\,')[[1]], EOIq)
if(is.null(cVars)) EOIc <- NA else
   EOIc <- intersect(strsplit(EOI, '\\,')[[1]], strsplit(cVars, '\\,')[[1]])

if(!is.na(qContr)) {
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

# Fisher transformation
if(r2z) dataTable$Y <- fisher(dataTable$Y)

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

##################### MCMC ####################
if(model==1) modelForm <- as.formula(paste('Y ~ 1 + (1|Subj) + (1|ROI)')) else
   modelForm <- as.formula(paste('Y~', model, '+(1|Subj)+(', model, '|ROI)'))

if(model==1) fm <- brm(modelForm, data=dataTable, chains = chains,
      iter=iterations, control = list(adapt_delta = 0.99, max_treedepth = 15)) else
   fm <- brm(modelForm, data=dataTable,
      prior=c(prior(normal(0, 1), class = "Intercept"), prior(normal(0, 0.5), class = "sd")),
      chains = chains, iter=iterations, control = list(adapt_delta = 0.99, max_treedepth = 15))

print(format(Sys.time(), "%D %H:%M:%OS3"))

# Stop the clock
proc.time() - ptm

save.image(file=paste0(outFN, ".RData"))

cat(format(Sys.time(), "%D %H:%M:%OS3"), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
cat(capture.output(proc.time() - ptm), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)

##################### Post Processing ####################

cat('\n++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
cat('***** Summary information of model information *****\n')
(rs <- summary(fm))
cat('\n***** End of model information *****\n')
cat('++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')

cat('\n***** Summary information of model results *****\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)

if(any(sapply(c(list(fixed=rs$fixed, spec_pars=rs$spec_pars, cor_pars=rs$cor_pars), rs$random), dd))) {
   cat('\n***** Warning: convergence issue!!! *****\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   cat('Consider increasing the number of iterations for Markov chains!\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
}

cat(capture.output(rs), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)

ns <- iterations*chains/2
aa <- fixef(fm, summary = FALSE) # Population-Level Estimates
bb <- ranef(fm, summary = FALSE) # Extract Group-Level (or random-effect) Estimates


# for Intercept and quantitative variables
if(any(!is.na(EOIq) == TRUE)) for(ii in 1:length(EOIq)) {
   cat(sprintf('===== Summary of region effects for %s =====', EOIq[ii]),
      file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   ps0 <- psROI(aa, bb, EOIq[ii])
   gg <- sumROI(ps0, ns, 4)
   cat(capture.output(gg), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   if(any(!is.na(PDP) == TRUE)) plotPDP(EOIq[ii], ps0, nR, PDP[1], PDP[2], 8)
}

# for contrasts among quantitative variables
if(any(!is.na(qContr) == TRUE)) for(ii in 1:(length(qContrL)/2)) {
   cat(sprintf('===== Summary of region effects for %s vs %s =====', qContrL[2*ii-1], qContrL[2*ii]),
      file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   ps0 <- psROI(aa, bb, qContrL[2*ii-1]) - psROI(aa, bb, qContrL[2*ii])
   gg <- sumROI(ps0, ns, 4)
   cat(capture.output(gg), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   if(any(!is.na(PDP) == TRUE)) plotPDP(paste0(qContrL[2*ii-1], '-', qContrL[2*ii]), ps0, nR, PDP[1], PDP[2], 8)
}

# for factor
if(any(!is.na(EOIc) == TRUE)) for(ii in 1:length(EOIc)) {
   lvl <- levels(dataTable[[EOIc[ii]]])  # levels
   nl <- nlevels(dataTable[[EOIc[ii]]])  # number of levels: last level is the reference in deviation coding
   ps <- array(0, dim=c(nl, ns, nR)) # posterior samples
   for(jj in 1:(nl-1)) ps[jj,,] <- psROI(aa, bb, paste0(EOIc[ii],jj))
   ps[nl,,] <- psROI(aa, bb, 'Intercept') # Intercept: averge effect
   psa <- array(0, dim=c(nl, ns, nR)) # posterior samples adjusted
   for(jj in 1:(nl-1)) {
      psa[jj,,] <- ps[nl,,]  + ps[jj,,]
      psa[nl,,] <- psa[nl,,] + ps[jj,,]
   }
   psa[nl,,] <- ps[nl,,] - psa[nl,,]  # reference level

   oo <- apply(psa, 1, sumROI, ns, 4)

   cat(sprintf('===== Summary of region effects for %s =====', EOIc[ii]), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:nl) {
      cat(sprintf('----- %s level: %s', EOIc[ii], lvl[jj]), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
      cat(capture.output(oo[[jj]]), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   }

   if(any(!is.na(PDP) == TRUE)) for(jj in 1:nl)
      plotPDP(EOIc[ii], psa[jj,,], nR, PDP[1], PDP[2], 8)

   cat(sprintf('===== Summary of region effects for %s comparisons =====', EOIc[ii]), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:(nl-1)) for(kk in (jj+1):nl) {
      cat(sprintf('----- level comparison: %s vs %s', lvl[jj], lvl[kk]), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
      oo <- sumROI(psa[jj,,] - psa[kk,,], ns, 4)
      cat(capture.output(oo), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
      if(any(!is.na(PDP) == TRUE))  plotPDP(paste0(lvl[jj], '-', lvl[kk]), psa[jj,,] - psa[kk,,], nR, PDP[1], PDP[2], 8)
   }
}

##################### conventional GLM #####################
mm <- list()
GLM <- as.formula(paste('Y ~', model))
for(ii in levels(dataTable$ROI)) mm[[ii]] = lm(GLM, data=dataTable[dataTable$ROI==ii,])
nn <- lapply(mm, summary)
ll <- lapply(nn, `[`, 'coefficients')



# for Intercept and quantitative variables
if(any(!is.na(EOIq) == TRUE)) for(ii in 1:length(EOIq)) {
   cat(sprintf('===== Summary of region effects under GLM for %s: no adjustment for multiplicity =====', EOIq[ii]),
      file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   gg <- sumGLM(dataTable,ll, EOIq[ii], nR, nn[[ii]]$df, 4)
   cat(capture.output(gg), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
}

# for contrasts among quantitative variables
if(any(!is.na(qContr) == TRUE)) for(ii in 1:(length(qContrL)/2)) {
   cat(sprintf('===== Summary of region effects for %s vs %s =====', qContrL[2*ii-1], qContrL[2*ii]),
      file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   cat(capture.output(gg), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
}

# for factor
if(any(!is.na(EOIc) == TRUE)) for(ii in 1:length(EOIc)) {
   lvl <- levels(dataTable[[EOIc[ii]]])  # levels
   nl <- nlevels(dataTable[[EOIc[ii]]])  # number of levels: last level is the reference in deviation coding
   ps <- array(0, dim=c(nl, ns, nR)) # posterior samples
   for(jj in 1:(nl-1)) ps[jj,,] <- psROI(aa, bb, paste0(EOIc[ii],jj))
   ps[nl,,] <- psROI(aa, bb, 'Intercept') # Intercept: averge effect
   psa <- array(0, dim=c(nl, ns, nR)) # posterior samples adjusted
   for(jj in 1:(nl-1)) {
      psa[jj,,] <- ps[nl,,]  + ps[jj,,]
      psa[nl,,] <- psa[nl,,] + ps[jj,,]
   }
   psa[nl,,] <- ps[nl,,] - psa[nl,,]  # reference level

   oo <- apply(psa, 1, sumROI, ns, 4)

   cat(sprintf('===== Summary of region effects for %s =====', EOIc[ii]), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:nl) {
      cat(sprintf('----- %s level: %s', EOIc[ii], lvl[jj]), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
      cat(capture.output(oo[[jj]]), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   }

   cat(sprintf('===== Summary of region effects for %s comparisons =====', EOIc[ii]), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:(nl-1)) for(kk in (jj+1):nl) {
      cat(sprintf('----- level comparison: %s vs %s', lvl[jj], lvl[kk]), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
      oo <- sumROI(psa[jj,,] - psa[kk,,], ns, 4)
      cat(capture.output(oo), file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(outFN, '.txt'), sep = '\n', append=TRUE)
   }
}
################

# save it again
save.image(file=paste0(outFN, ".RData"))
cat("\nCongratulations! The above results are saved in file ", outFN, "\n\n", sep='')}
