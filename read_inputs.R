# Read CSV formatted game, output and stock tables. Get the data by
# exporting corresponding excel sheets as CSV files.

read.game_table <- function (filename) read.csv_table(filename, 142)
read.output_table <- function (filename) read.csv_table(filename, 39)
read.stock_table <- function (filename) read.csv_table(filename, 28)

read.csv_table <- function (filename, n) {
   raw_inputs <- read.csv(filename)
   inputs <- raw_inputs[1:n,]
   inputs <- Filter(function (x) !all(is.na(x)), inputs)
   
   # create the input object
   acc <- list()
   xs  <- as.matrix(t(inputs))
   for (i in 1:n) {
      current <- xs[,i] # i'th column 

      # extract attributes
      abbreviation <- current[1]
      name   <- toString(current[2])
      rest   <- tail(current, length(current) - 2)
      values <- as.vector(sapply(rest, as.numeric))

      input_object <- list(name = name, values = values)
      acc[[abbreviation]] <- input_object
   }

   return(acc)
}
