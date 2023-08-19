# Simple Model Reports in lieu of summary()
modelreport <- function(model, request ="all"){
  print(noquote("Formatted Results for Model:"))
  print(noquote(model$call)); model_type <- class(model)
  if(model_type == "lm" | model_type == "gls") {
    modelreport = data.frame(suppressWarnings(coef(summary(model))[,c(1:2,4)]))
    modelreport <- cbind(c(row.names(modelreport)), data.frame(modelreport, row.names = NULL))
    names(modelreport) <- c("Term", "Est.", "SE", "p")
    }
  if(model_type == "lme") {
    modelreport = data.frame(suppressWarnings(coef(summary(model))[,c(1:2,5)]))
    modelreport <- cbind(row.names(modelreport),modelreport)
    names(modelreport) <- c("Term", "Est.", "SE", "p")
  }
  num_terms = nrow(modelreport)
  three_dec <- function(est) format(round(est, 3), nsmall = 3) # for estimates
  two_dec <- function(se) format(round(se,2), nsmall = 2) # for standard errors
  modelreport$Estimate <- ifelse(modelreport$p < 0.001, 
                                  paste(three_dec(modelreport$Est.),"(",two_dec(modelreport$SE),")","***",sep=""),
                                  ifelse(modelreport$p < 0.01,
                                         paste(three_dec(modelreport$Est.),"(",two_dec(modelreport$SE),")","**",sep=""),
                                         ifelse(modelreport$p < 0.05,
                                                paste(three_dec(modelreport$Est.),"(",two_dec(modelreport$SE),")","*",sep=""),
                                                paste(three_dec(modelreport$Est.),"(",two_dec(modelreport$SE),")"))))
  
  modelreport <- modelreport[,c("Term","Estimate")]
  modelreport$Estimate <- gsub(" ", "", modelreport$Estimate); modelreport$Estimate <- gsub("\\(", " (", modelreport$Estimate)
  if(request == "all") {result <- data.frame(modelreport)}
  if(request == "terms") {result <- data.frame(modelreport$Term)}
  if(request == "params") {result <- data.frame(modelreport$Estimate)}
  print.data.frame(result, row.names = FALSE)
}

# Group all Models into a single Table. This is for nested models only #
concat_modelreports <- function(num_mod, mod_list){
  if(num_mod == length(mod_list)){
  print(noquote("Model Dependent Variables:"))
  for(i in 1:length(mod_list)){mod <- mod_list[[i]]; print(noquote(paste("Model ",i,": ",mod$terms[[2]])))}
  if(!"dplyr" %in% (.packages())){library(dplyr)}
  sink("NUL") # To suppress output, otherwise results will print for each model
    if(class(mod_list) == "list"){
    if(num_mod > 1){
      nterms <- function(x) nrow(modelreport(x)); totalterms <- sum(sapply(mod_list, nterms))
      output <<- list(); output <<- vector("list", length = num_mod) # creates list in GlobalEnv to store results
      table <- data.frame("Term" <- rep(NA, totalterms)); names(table) <- "Term"
      for (i in 1:num_mod){
      mod_terms <- lapply(mod_list, nterms)[[i]]; mod_out <- data.frame(lapply(mod_list, modelreport)[[i]])
      output[[i]] <<- mod_out; modelreports <<- dplyr::bind_rows(output) # sends to big list
      i = i + 1; table[,i] <- NA # add 1 to i so results for term column are not overwritten
      colnames(table)[i] <- paste("Model_",(i-1),sep="") # label each column
      table <- table[c(1:n_distinct(modelreports$Term)),]; table$Term <- unique(modelreports$Term)
      if(i == 2) {table[c(1:mod_terms),i] <- modelreports$Estimate[c(1:mod_terms)]}
      if(i > 2) {
        iminus2 = i - 2; last_i_terms <- lapply(mod_list, nterms)[[iminus2]]
        this_i_end_terms <- last_i_terms + mod_terms
        table[c(1:mod_terms),i] <- modelreports$Estimate[c((last_i_terms+1):this_i_end_terms)]
        }
      }
      remove(output, envir = .GlobalEnv); remove(modelreports, envir = .GlobalEnv)
      sink() # To reallow output to be printed
    } else print(noquote("ERROR: Function Requires More Than One Model."))
  } else print(noquote("ERROR: Models Must Be In a List. Use the list() Function to Group Models."))
  } else print(noquote("ERROR: num_mod [first_arg] Must Equal Number of Elements in model_list [second_arg]"))
  table
}
