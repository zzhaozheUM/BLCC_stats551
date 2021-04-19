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

# Precision (macro)
macro_precision = function(actual, prediction) {
  # total positives
  denom = apply(prediction, 2, sum)
  # true positives
  num = apply((prediction == actual & actual == 1), 2, as.numeric)
  num = apply(num, 2, sum)
  precision = num / denom
  return(precision)
}

# Recall (macro)
macro_recall = function(actual, prediction) {
  # true positives
  num = apply((prediction == actual & actual == 1), 2, as.numeric)
  num = apply(num, 2, sum)
  
  # false negatives
  denom = apply((prediction != actual & actual == 1), 2, as.numeric)
  denom = apply(denom, 2, sum)
  denom = denom + num
  recall = num / denom
  
  return(recall)
}


# Macro-averaged F1
macro_F1 = function(actual, prediction) {
  precision = macro_precision(actual, prediction)
  recall = macro_recall(actual, prediction)
  f1 = mean(2 * precision * recall / (precision + recall))
  return(f1)
}

# Precision (micro)
<<<<<<< HEAD
macro_precision = function(actual, prediction) {
  
}

# 
=======
macro_precision = function(actul, prediction) {
  
}
>>>>>>> 8c68dc9e61dc6b33fc0f3b4ca4f5ccc49aa00004
