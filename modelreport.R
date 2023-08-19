#### Simple Model Reports in lieu of summary() ####
# Outputs a data frame with each model term name and estimate (with standard errors and asterisks for
# significance testing (p < 0.05 = *; p < .01 = **, p < .001 = ***)).
# Specify the model and request both term names and estimates (i.e., "all"), only the term name
# (i.e., "terms") or only the estimates (i.e., "params").
modelreport <- function(model, request ="all"){
  print(noquote("Formatted Results for Model:"))
  print(noquote(model$call)); model_type <- class(model)
  if(model_type == "lm" | model_type == "gls") { # Linear Model
    model_report = data.frame(suppressWarnings(coef(summary(model))[,c(1:2,4)]))
    model_report <- cbind(c(row.names(model_report)), data.frame(model_report, row.names = NULL))
    names(model_report) <- c("Term", "Est.", "SE", "p")
  }
  if(model_type == "lme") { # Mixed Effects Model
    model_report = data.frame(suppressWarnings(coef(summary(model))[,c(1:2,5)]))
    model_report <- cbind(row.names(model_report),model_report)
    names(model_report) <- c("Term", "Est.", "SE", "p")
  }
  if(model_type == "rq") { # Quantile Regression
    model_report = data.frame(coef(summary.rq(model, se = "boot"))[,c(1,2,4)])
    model_report <- cbind(row.names(model_report),model_report)
    names(model_report) <- c("Term", "Est.", "SE", "p")
  }
  num_terms = nrow(model_report)
  three_dec <- function(est) format(round(est, 3), nsmall = 3) # for estimates
  two_dec <- function(se) format(round(se,2), nsmall = 2) # for standard errors
  model_report$Estimate <- ifelse(model_report$p < 0.001, 
                                  paste(three_dec(model_report$Est.),"(",two_dec(model_report$SE),")","***",sep=""),
                                  ifelse(model_report$p < 0.01,
                                         paste(three_dec(model_report$Est.),"(",two_dec(model_report$SE),")","**",sep=""),
                                         ifelse(model_report$p < 0.05,
                                                paste(three_dec(model_report$Est.),"(",two_dec(model_report$SE),")","*",sep=""),
                                                paste(three_dec(model_report$Est.),"(",two_dec(model_report$SE),")"))))
  
  model_report <- model_report[,c("Term","Estimate")]
  model_report$Estimate <- gsub(" ", "", model_report$Estimate); model_report$Estimate <- gsub("\\(", " (", model_report$Estimate)
  if(request == "all") {result <- data.frame(model_report)}
  if(request == "terms") {result <- data.frame(model_report$Term)}
  if(request == "params") {result <- data.frame(model_report$Estimate)}
  print.data.frame(result, row.names = FALSE)
}
