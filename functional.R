# Functional programming methods

zipVectors <- function (x, y) {
   minLength <- min(length(x), length(y))
   maxLength <- max(length(x), length(y)) + 1

   subVectors <- c(x[-(minLength+1):-maxLength], y[-(minLength+1):-maxLength])
   zipped     <- matrix(subVectors, nrow = minLength)
   return(zipped)
}

# Fold columns from left.
colFoldl <- function (f, acc, xs) {
   if (!is.matrix(xs)) {
      return(f(acc, xs))
   } else {
      return(colFoldl(f, f(acc, xs[,1]), xs[,-1]))
   }
}

# Fold rows from "left".
rowFoldl <- function (f, acc, xs) {
   colFoldl(f, acc, t(xs))
}
