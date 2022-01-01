sample.size <- function(x, fun, nrep = 1000, graph = TRUE, ...) {
   stopifnot(is.atomic(x))

   x <- as.numeric(x)

   stopifnot(is.function(fun))
   stopifnot(nrep > 1)

   n <- length(x)
   size <- 2:(n-1)
   nrep <- nrep

   aux <- matrix(NA, nrow = nrep, ncol = length(size))
   colnames(aux) <- size

   for(j in 1:length(size)) {
      for(i in 1:nrep){
         aux[i, j] <- fun(sample(x, size = size[j], replace = TRUE))
      }
   }

   stats <- colMeans(aux)
    
   if(graph) {
      lim1 <- min(stats, fun(x))
      lim2 <- max(stats, fun(x))
      myplot(stats ~ size, type = "l", ylim = c(lim1, lim2),
         xlab = "Sample size", ylab = "fun", ...)
      abline(h = fun(x), col = "blue")
   }

   out <- NULL
   attributes(out) <- list(sample.size = size, stats = stats)

   class(out) <- "sample.size"
   invisible(out)
}