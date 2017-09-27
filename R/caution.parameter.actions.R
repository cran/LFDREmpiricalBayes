caution.parameter.actions<- function (x1,x2,l1=4,l2=1) {
    # l1 and l2 are our definition of the loss values.
    # Take l1 = 4 and l2 = 1 as default values.
    # x1 and x2 are vectors of two different reference classes

    threshold <- l2/(l1+l2)  # threshold for deriving the Bayes actions

    notEqualLen <- length(x1) != length(x2)
    lossValZeroNeg <- (l1 <= 0) || (l2 <= 0)

    if(notEqualLen){
      stop("Vectors must be of equal length.")
    }

    if(lossValZeroNeg){
      stop("Loss values must be greater than 0.")
    }

    for(i in 1:length(x1)){
      lfdrOutOfBounds <- (x1[i] < 0 || x1[i] > 1)||(x2[i] < 0 || x2[i] > 1)

      if(lfdrOutOfBounds){
        stop("Each index in vector x1 or x2 must contain a value
             between 0 and 1.")
        }
    }

    x <- cbind(x1,x2)
    infx <- rowMins(x)          # infimum of LFDRs for each variant
    supx <- rowMaxs(x)          # supremum of LFDRs for each variant
    l <- length(infx)

    CG1 <- CG0 <- CG0.5 <- c()

    for (i in 1:l){
      CGM1Rule <- l1*supx[i] <= l2*(1-infx[i])
      CGM0Rule <- l1*infx[i] <= l2*(1-supx[i])
      CGMHalfRule <- l1*(supx[i]+infx[i]) <= l2*(2-supx[i]-infx[i])

      ifelse(CGM1Rule, CGM1 <- 1, CGM1 <- 0)
      ifelse(CGM0Rule, CGM0 <- 1, CGM0 <- 0)
      ifelse(CGMHalfRule, CGM0.5 <- 1, CGM0.5 <- 0)

      CG1 <- c(CG1,CGM1)
      CG0 <- c(CG0,CGM0)
      CG0.5 <- c(CG0.5,CGM0.5)
    }

    cat("\nGiven a threshold of: ", threshold, "\n\n")
    return(list(CGM1 = CG1, CGM0 = CG0, CGM0.5 = CG0.5))
}
