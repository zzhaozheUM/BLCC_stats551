# Accuracy
multi_accuracy = function(actual, prediction) {
  result = 0
  for (i in 1:nrow(prediction)) {
    set_true = which(actual[i,] == 1)
    set_pred = which(prediction[i,] == 1)
    if (length(set_true) == 0 & length(set_pred) == 0) {
      result = result + 1
    }
    else {
      result = result + 
        length(intersect(set_pred, set_true))/ length(union(set_pred, set_true))
    }
  }
  
  result = result / nrow(prediction)
  return(result)
}

# Precision (label-wise)
multi_precision = function(actual, prediction) {
  # total positives
  denom = apply(prediction, 2, sum)
  # true positives
  num = apply((prediction == actual & actual == 1), 2, as.numeric)
  num = apply(num, 2, sum)
  precision = num / denom
  return(precision)
}

# Recall (label-wise)
multi_recall = function(actual, prediction) {
  
}

# AUC

multi_auc = function(actual, prediction) {
  precision = multi_precision(actual, prediction) 
  recall = multi_precision(actual, prediction)
}

# Macro-averaged F1

multi_F1 = function(actual, prediction) {
}