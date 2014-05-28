myplot <- function(x, y) {
  if(missing(y)) {
    print ("missing y")
    y <- x
    x <- 1:length(y)
  }
  plot(x, y)
}