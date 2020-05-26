impFeatures <- function(rfModel,n) {
  ii <- importance(rfModel)
  return (ii[order(ii,decreasing=T)[1:n],])
}

LogLoss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}
