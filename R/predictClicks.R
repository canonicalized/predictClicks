#' Take some pageviews, output Markov model prediction
#'
#' @param current_url the url to predict from
#'
#' @return The prediction
#' @import markovchain
#' @export

predictClicks <- function(current_url){
  out <- try(predict(model$estimate, newdata = current_url), silent = TRUE)
  if(inherits(out, "try-error")){
    out <- "None"
  }
  out
}
