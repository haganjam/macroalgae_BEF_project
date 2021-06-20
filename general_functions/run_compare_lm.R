
# run and compare several linear models

# set up a function to run different models that can then be compared
lm.allo <- function(data, resp, e.vars) {
  
  # set an output list for the model coefficients
  est.lm <- vector("list", length(e.vars))
  names(est.lm) <- seq(1:length(e.vars))
  
  # set an output list for the model fit statistics
  fit.lm <- vector("list", length(e.vars))
  names(fit.lm) <- seq_along(1:length(e.vars))
  
  for (i in 1:length(e.vars) ) {
    
    # fit model using chosen predictors
    lm.e.vars <- lm(reformulate(e.vars[[i]], resp), data = data)
    
    # write coefficients to the est.lm list
    est.lm[[i]] <- broom::tidy(lm.e.vars)
    
    # write fit statistics to the fit.lm list
    fit.lm[[i]] <- broom::glance(lm.e.vars)
      
    }
    
  # create a model.list table
  model.list = data.frame(model=double(), terms=character())
  for(j in 1:length(e.vars)) {
    
    model.list = rbind(model.list,
                       data.frame(model = as.character(j),terms = paste(e.vars[[j]], collapse=" + ")))
  }
  
  # join fit statistics to the model list data
  df.fit <- 
    full_join(model.list, 
              bind_rows(fit.lm, .id = "model"),
              by = "model")
  
  # convert lists to data.frames and join
  df.models <- 
    full_join(df.fit,
              bind_rows(est.lm, .id = "model"),
              by = "model")

  return(df.models)
}







