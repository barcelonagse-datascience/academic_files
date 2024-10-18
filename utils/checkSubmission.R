checkSubmission <- function(){
  file <- file.choose()
  sol.env <- new.env()
  capture.output( run <- suppressWarnings(tryCatch(
                   source(file, local=sol.env) ,
                   error = function(cond) {
                     message(sprintf('Your code does not seem to run. I got the following message from R: \n %s',
                                     conditionMessage(cond) ))
                   })) )
  envVars <- ls(envir=sol.env)
  solutionVars <- c('avgInflation', 'cnames', 'countries', 'data', 'eaMdl', 'eaPred',
            'eaVars', 'elnet', 'elnetPred','es.df', 'esCoef', 'esMdl', 'fmPred',
            'fullMdl', 'inflationData', 'lasso', 'lassoPred', 'mainSeries',
            'mse', 'N', 'pred', 'pred.df', 'ridge', 'ridgePred',
            'spainCols', 'T' , 'test.df', 'train.df', 'uniqCountries',
            'inflationSep24')

  # Catch if not all variables are defined, but the code runs.
  if (length(run)>0){
    if ( all(solutionVars %in% envVars)){
      cat(sprintf('Your submission runs and contains all required variables. Thanks and have a great day!'))
    } else {
      whichMiss <- solutionVars[ !(solutionVars %in% envVars) ]
      cat(sprintf('I cant seem to find the following variables: \n'))
      cat(whichMiss)
      cat(sprintf('\n You can submit the code as is, but you will not get any points for the questions that request these variables.'))
    }
  
  }
  
}

