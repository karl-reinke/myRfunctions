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
  if(substr(class(model.1)[1], 1, 7) == "lmerMod" & substr(class(model.2)[1], 1, 7) == "lmerMod"){
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
