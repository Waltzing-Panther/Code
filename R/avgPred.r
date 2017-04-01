avgPred = function ( formula, weights = NULL, data ) {
  # requires data to be a n x 2 dataframe.
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

reg.mod = avgPred ( count ~ spray, data = InsectSprays )
reg.mod$avgVal

w = c ( .01, .01, .01, .01, .01, .95 )
reg.mod = avgPred ( count ~ spray, weights = w, data = InsectSprays )
reg.mod$avgVal