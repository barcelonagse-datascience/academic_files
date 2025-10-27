checkSubmission <- function() {
  file <- file.choose()
  sol.env <- new.env()
  
  capture.output(
    run <- suppressWarnings(tryCatch(
      source(file, local = sol.env),
      error = function(cond) {
        message(sprintf(
          "Your code does not seem to run. I got the following message from R:\n%s",
          conditionMessage(cond)
        ))
      }
    ))
  )
  
  # List all variables defined by the student
  envVars <- ls(envir = sol.env)
  
  # List of variables expected for the PS_1 assignment
  solutionVars <- c(
    "data", "T", "N", "cnames", "countries", "uniqCountries",
    "spainCols", "es.df", "esMdl", "esCoef",
    "mainSeries", "inflationData", "avgInflation",
    "y", "X", "pred.df", "train.df", "test.df",
    "fullMdl", "fmPred",
    "eaVars", "eaMdl", "eaPred",
    "lasso", "lassoPred", "ridge", "ridgePred",
    "elnet", "elnetPred",
    "pred", "mse"
  )
  
  # If the code ran
  if (length(run) > 0) {
    if (all(solutionVars %in% envVars)) {
      cat("Your submission runs and contains all required variables.\n")
      cat("Thanks and have a great day!\n")
    } else {
      whichMiss <- solutionVars[!(solutionVars %in% envVars)]
      cat("I can't seem to find the following variables:\n")
      cat(paste(whichMiss, collapse = ", "), "\n")
      cat("You can submit the code as is, but you will not get points for the questions related to these variables.\n")
    }
  }
}

