avgPred = function ( formula, weights = NULL, data ) {
  # Computes the average predicated value for a one - way ANOVA
  #   using weighted averages.
  #
  # Args: 
  #   formula: The formula to be used.
  #   weights: A vector of weights for each factor level. Must sum to one.
  #   data: The data set to be used. Needs to be n x 2 data frame.
  #
  # Return:
  #   An lm object that is the same as the one returned from lm()
  #   except with an added variable "avgVal" which is the weighted
  #   average predicted value among the factors.
  reg.mod = lm ( formula , data = data )
  
  counts = table ( data [ , 2 ] ) 
  if ( is.null ( weights ) ) { 
    dweights = rep ( 1 / sum ( counts ), sum ( counts ) )
  } else {
    dweights = rep ( weights / counts , counts )
  }
  
  avgVal = predict ( reg.mod, data ) %*% dweights
  
  reg.mod [["avgVal"]] = as.numeric ( avgVal )
  return ( reg.mod )
}


# example ---------
reg.mod = avgPred ( count ~ spray, data = InsectSprays )
reg.mod$avgVal

w = c ( .01, .01, .01, .01, .01, .95 )
reg.mod = avgPred ( count ~ spray, weights = w, data = InsectSprays )
reg.mod$avgVal