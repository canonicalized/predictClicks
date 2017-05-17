#' Take some pageviews, output Markov model prediction
#'
#' @param current_url the url to predict from
#'
#' @return The prediction
#' @import markovchain
#' @export

load("data/model.rda")
petmart <- function(current_url){
   out <- try(predict(model$estimate, newdata = current_url), silent = TRUE)
   if(inherits(out, "try-error")){
     out <- "None"
   }
   out
}
