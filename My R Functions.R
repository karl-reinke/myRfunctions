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

# Simple Model Reports in lieu of summary()
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

model_report(model = model.1, request = "all")
model_report(model = model.1, request = "terms")
model_report(model = model.1, request = "params")
model_report(model = model.3.lme)

# Group all Models into a single Table. This is for nested models only #
concat_model_reports <- function(num_mod, mod_list){
  if(num_mod == length(mod_list)){
  print(noquote("Model Dependent Variables:"))
  for(i in 1:length(mod_list)){mod <- mod_list[[i]]; print(noquote(paste("Model ",i,": ",mod$terms[[2]])))}
  if(!"dplyr" %in% (.packages())){library(dplyr)}
  sink("NUL") # To suppress output, otherwise results will print for each model
    if(class(mod_list) == "list"){
    if(num_mod > 1){
      nterms <- function(x) nrow(model_report(x)); totalterms <- sum(sapply(mod_list, nterms))
      output <<- list(); output <<- vector("list", length = num_mod) # creates list in GlobalEnv to store results
      table <- data.frame("Term" <- rep(NA, totalterms)); names(table) <- "Term"
      for (i in 1:num_mod){
      mod_terms <- lapply(mod_list, nterms)[[i]]; mod_out <- data.frame(lapply(mod_list, model_report)[[i]])
      output[[i]] <<- mod_out; model_reports <<- dplyr::bind_rows(output) # sends to big list
      i = i + 1; table[,i] <- NA # add 1 to i so results for term column are not overwritten
      colnames(table)[i] <- paste("Model_",(i-1),sep="") # label each column
      table <- table[c(1:n_distinct(model_reports$Term)),]; table$Term <- unique(model_reports$Term)
      if(i == 2) {table[c(1:mod_terms),i] <- model_reports$Estimate[c(1:mod_terms)]}
      if(i > 2) {
        iminus2 = i - 2; last_i_terms <- lapply(mod_list, nterms)[[iminus2]]
        this_i_end_terms <- last_i_terms + mod_terms
        table[c(1:mod_terms),i] <- model_reports$Estimate[c((last_i_terms+1):this_i_end_terms)]
        }
      }
      remove(output, envir = .GlobalEnv); remove(model_reports, envir = .GlobalEnv)
      sink() # To reallow output to be printed
    } else print(noquote("ERROR: Function Requires More Than One Model."))
  } else print(noquote("ERROR: Models Must Be In a List. Use the list() Function to Group Models."))
  } else print(noquote("ERROR: num_mod [first_arg] Must Equal Number of Elements in model_list [second_arg]"))
  table
}

table <- concat_model_reports(num_mod = 3, mod_list = list(model.1,model.2, model.3))


#### Reports Estimate of Variance Explained in Multilevel Model ####
totalRsquare <- function(model, outcome){
  Pred <- data.frame(predict(model, re.form = NA)) # Does not use random effects
                                                   # in estimation of predicted
  outcome <- data.frame(as.numeric(outcome)); outcome <- na.omit(outcome)
  r_square <- cor(Pred, outcome, use = "pairwise.complete.obs")^2
  r_square[1]
}

totalRsquare(model = model.3.lme, outcome = bh1996$WBEING)

class(model.3.lme)

VarCorr(model.3.lme)

#### Reports pseudo-R^2 ####
pseudoRsquare <- function(model.1, model.2, component = "residual"){
  if (component == "within") {component = "residual"}
  if (component == "between") {component = "(Intercept)"}
  if(class(model.1) == "lme" & class(model.2) == "lme"){
    if(component == "residual"){
      model.1 = VarCorr(model.1); model.2 = VarCorr(model.2)
      error1 = as.numeric(model.1[nrow(model.1),"Variance"])
      error2 = as.numeric(model.2[nrow(model.2),"Variance"])
      print(round((error1 - error2) / error1, 10))
    }
    if(component == "(Intercept)"){
      model.1 = VarCorr(model.1); model.2 = VarCorr(model.2)
      var1 = as.numeric(model.1[1,"Variance"])
      var2 = as.numeric(model.2[1,"Variance"])
      print(round((var1 - var2) / var1, 10))
    }
  }
  # lmerMod is class w lme4() but lmerModLmerTest is class w lmerTest(), extract first 7 characters
  if(substr(class(model_1)[1], 1, 7) == "lmerMod" & substr(class(model_2)[1], 1, 7) == "lmerMod"){
    if(component == "residual"){
      error1 = attr(VarCorr(model.1), "sc")^2
      error2 = attr(VarCorr(model.2), "sc")^2
      print(round((error1 - error2) / error1, 10))
    }
    if(!component == "residual"){
      model.1 = data.frame(VarCorr(model.1))
      var1 = as.numeric(model.1[which(model.1$var1 == component),ncol(model.1)][1])^2
      model.2 = data.frame(VarCorr(model.2))
      var2 = as.numeric(model.2[which(model.2$var1 == component),ncol(model.2)][1])^2
      print(round((var1 - var2) / var1, 10))
    }
  }
}
