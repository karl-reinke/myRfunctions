MakeIndirectDistPlot <- function(type = list("Two-Stage","Full"),
                                 a, a_error,
                                 b, b_error,
                                 abcov = NULL,
                                 title = NULL) {
  if (length(type) > 1) {stop("Please specify type = 'Two-Stage' or 'Full'")}
  require(MASS); set.seed(04262021)
  rep = 20000; conf = 95; pest = c(a,b)
  if (type == "Two-Stage") { # Set abcov to zero
    acov <- matrix(c(
    (a_error*a_error), 0,
    0, (b_error*b_error)
    ),2,2) }
  if (type == "Full") { # Take value for abcov
    if (is.null(abcov)) {
      stop("Please specify the covariance of a and b with abcov =")
      }
    acov <- matrix(c(
      a_error, abcov,
      abcov, b_error
    ),2,2) }
  mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
  ab <- mcmc[,1]*mcmc[,2]
  low=(1-conf/100)/2; upp=((1-conf/100)/2)+(conf/100);
    LL=quantile(ab,low); UL=quantile(ab,upp)
    LL4=round(LL,4); UL4=round(UL,4)
  if (is.null(title)) {title = "Distribution of Indirect Effect"}
  print(title); print(paste0("Type: ",type))
  print(paste(paste(conf,"% CI: [",LL4,",",UL4,"]")))

  cat("\nComments from Quantpsy.org:\nIf you use SEM, path analysis, multilevel modeling, or some other \nmultivariate method to obtain both a and b from a single model, \nthen var(a), var(b), and cov(a,b) can be found in the \nasymptotic covariance matrix of the parameter estimates.\nIf you use regression to obtain a and b in separate steps,\nthen var(a) and var(b) are simply the squared standard errors,\nand cov(a,b) = 0\n\nComments from KHR:\nWhen type = Two-Stage, input for a_error and b_error are assumed to be the standard errors of a and b and are squared in the asymptotic covariance matrix used for calculations. The covariance of a and b is also set to zero.\n\nWhen type = Full, input for a_error and b_error are assumed to be taken directely from the asymptotic covariance matrix (and thus not squared) and the covariance of a and b must be given to the function.\n\nNote that squaring the standard errors of the parameter estimates in a multivariate model provide slightly inflated estimates of the values obtained directly from the asymptotic covariance matrix (about 1% upward bias), presumably because software outputs rounded versions of the parameters' standard errors.")
  
  suppressPackageStartupMessages(library(ggplot2))
  ggplot(as.data.frame(ab), aes(x = ab)) +
    geom_density(color = "black", fill = "skyblue", alpha = 0.5) +
    geom_vline(aes(xintercept = as.numeric(LL4)),linetype = 2,linewidth = 0.5) +
    geom_vline(aes(xintercept = mean(ab)),linetype = 1,linewidth = 0.5) +
    geom_vline(aes(xintercept = as.numeric(UL4)),linetype = 2,linewidth = 0.5) +
    labs(x = paste("Indirect Effect (a*b = ", round(mean(ab), 3),")", sep = ""),
         y = paste("Density (",rep, "iterations )"),
         title = title,
         subtitle = paste(conf,"% CI: [",LL4,",",UL4,"]"))
}
