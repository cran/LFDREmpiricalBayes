PRGM.action <- function (x1,x2) {

  notEqualLen <- length(x1) != length(x2)

  if(notEqualLen){
    stop("Vectors must be of equal length!")
  }

  for(i in 1:length(x1)){
    lfdrOutOfBounds <- (x1[i] < 0 || x1[i] > 1)||(x2[i] < 0 || x2[i] > 1)

    if(lfdrOutOfBounds){
      stop("Each index in vector x1 or x2 must contain a value between 0
           and 1.")
    }
  }

  PR=1-(x1+x2)/2
  return(list(PRGM=PR))
}
