spred2 <- function(coords, data, grid) {
   y <- data
   n <- nrow(coords)
   N <- nrow(grid)
   pred <- data.frame(grid, pred = NA)
   i = 1

   repeat{
      s <- sweep(coords, 2, as.matrix(grid[i, ]), FUN = "-")
      d <- sqrt(apply(s^2, 1, sum))
      w <- 1 / (d^2)
      max.w <- max(w[is.finite(w)])
      w[w == Inf] <- max.w
      pred$pred[i] <- weighted.mean(y, w)
      i = i + 1

      if (i > N) {
         break()
      }

   }
   
   if (n == N) {
      pame <- 100*mean(abs(data - pred$pred)/data)

      cat("PAME:", round(pame, 2), "%\n")
   }
   
   return(pred)
}