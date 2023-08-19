#### Reports Estimate of Variance Explained in Multilevel Model ####
totalRsquare <- function(model, outcome){
  print("'outcome' must be specified as data$outcome ")
  Pred <- data.frame(predict(model, re.form = NA)) # Does not use random effects
                                                   # in estimation of predicted
  outcome <- data.frame(as.numeric(outcome)); outcome <- na.omit(outcome)
  r_square <- cor(Pred, outcome, use = "pairwise.complete.obs")^2
  r_square[1]
}
