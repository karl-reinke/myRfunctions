#### Model Reports in R ####
library(multilevel)
data(bh1996)

# Linear Models
model.1 <- lm(G.COHES ~ G.HRS, data = bh1996)
model.2 <- lm(G.COHES ~ G.HRS + G.LEAD, data = bh1996)
model.3 <- lm(G.COHES ~ G.HRS + G.LEAD + G.WBEING, data = bh1996)

# Multilevel Models
model.1.lme <- lme(fixed = WBEING ~ COHES, random = ~ 1|GRP, data = bh1996)
model.2.lme <- lme(fixed = WBEING ~ COHES, random = ~ 1 + COHES|GRP, data = bh1996)
model.3.lme <- lme(fixed = WBEING ~ COHES*G.LEAD, random = ~ 1 | GRP, data = bh1996)

#### Simple Model Reports in lieu of summary() ####
# Outputs a data frame with each model term name and estimate (with standard errors and asterisks for
# significance testing (p < 0.05 = *; p < .01 = **, p < .001 = ***)).
# Specify the model and request both term names and estimates (i.e., "all"), only the term name
# (i.e., "terms") or only the estimates (i.e., "params").
model_report <- function(model, request ="all"){
  print(noquote("Formatted Results for Model:"))
  print(noquote(model$call)); model_type <- class(model)
  if(model_type == "lm" | model_type == "gls") {
    model_report = data.frame(suppressWarnings(coef(summary(model))[,c(1:2,4)]))
    model_report <- cbind(c(row.names(model_report)), data.frame(model_report, row.names = NULL))
    names(model_report) <- c("Term", "Est.", "SE", "p")
  }
  if(model_type == "lme") {
    model_report = data.frame(suppressWarnings(coef(summary(model))[,c(1:2,5)]))
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

model_report(model = model.1, request = "all") # prints terms and estimates
model_report(model = model.1, request = "terms") # prints terms only
model_report(model = model.1, request = "params") # prints estimates only
model_results_as_object <- model_report(model = model.3.lme) # if unspecified, request defaults to all


#### Concatenate Model Results into Single Table ####
# model_report() function must be loaded, and dplyr package must be installed. Groups a list of nested
# models into a single table. Assumes all models are specified in a list() object from least to most
# complex. NAs are added where models do not provide estimates.
concat_model_reports <- function(num_mod, mod_list){
  if(num_mod == length(mod_list)){
    print(noquote("Model Dependent Variables:"))
    for(i in 1:length(mod_list)){mod <- mod_list[[i]]; print(noquote(paste("Model ",i,": ",mod$terms[[2]])))}
    if(!"dplyr" %in% (.packages())){library(dplyr)}
    sink("NUL") # To suppress output, otherwise results will print for each model everytime model_report() runs
    if(class(mod_list) == "list"){
      if(num_mod > 1){
        nterms <- function(x) nrow(model_report(x)); totalterms <- sum(sapply(mod_list, nterms))
        output <<- list(); output <<- vector("list", length = num_mod) # creates list in GlobalEnv to store results
        table <- data.frame("Term" <- rep(NA, totalterms)); names(table) <- "Term"
        for (i in 1:num_mod){
          mod_terms <- lapply(mod_list, nterms)[[i]]; mod_out <- data.frame(lapply(mod_list, model_report)[[i]])
          output[[i]] <<- mod_out; model_reports <<- dplyr::bind_rows(output) # sends to list
          i = i + 1; table[,i] <- NA # add 1 to i so results for term column are not overwritten
          colnames(table)[i] <- paste("Model_",(i-1),sep="") # label each column
          table <- table[c(1:n_distinct(model_reports$Term)),]; table$Term <- unique(model_reports$Term)
          if(i == 2) {table[c(1:mod_terms),i] <- model_reports$Estimate[c(1:mod_terms)]}
          if(i > 2) {
            iminus2 = i - 2; last_i_terms <- lapply(mod_list, nterms)[[iminus2]] # gets row number from last model
            this_i_end_terms <- last_i_terms + mod_terms
            table[c(1:mod_terms),i] <- model_reports$Estimate[c((last_i_terms+1):this_i_end_terms)]
          }
        }
        remove(output, envir = .GlobalEnv); remove(model_reports, envir = .GlobalEnv) # removes objects from global environment
        sink() # To reallow output to be printed
      } else print(noquote("ERROR: Function Requires More Than One Model."))
    } else print(noquote("ERROR: Models Must Be In a List. Use the list() Function to Group Models."))
  } else print(noquote("ERROR: num_mod [first_arg] Must Equal Number of Elements in model_list [second_arg]"))
  table
}

table <- concat_model_reports(num_mod = 3, mod_list = list(model.1,model.2, model.3))
