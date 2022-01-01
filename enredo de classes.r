source('http://arsilva.weebly.com/uploads/2/1/0/0/21008856/function_myplot.r')
plotPCA <- function(obj) {
   stopifnot(inherits(obj, 'princomp'))
   layout(matrix(c(1, 3, 3, 1, 3, 3, 2, 4, 4), nr = 3))

   par(las = 1, cex = 0.7, mar = c(2.5, 5, 1, 1))
   barplot(obj$loadings[, 1], ylab = 'Loadings', col = 0)
   abline(h = pretty(par('usr')[3:4]), col = 'gray')

   barplot(obj$loadings[, 1], 
      add = TRUE, xpd = F,
      col = sign(obj$loadings[, 1]) + 3)

   par(las = 1, cex = 0.7, mar = c(4.5, 4.5, 1, 1))
   prop <- obj$sdev^2 / sum(obj$sdev^2)

   myplot(prop, 
      type = 'b',
      col = 2,
      pch = 16,
      ylab = 'Proportion',
      xlab = 'Principal Component')
   
   par(las = 1, cex = 0.8, mar = c(4.5, 4.5, 1, 1))

   myplot(obj$scores, 
      xlab = paste('Comp.1 (', round(prop[1], 2), ')', sep = ''), 
      ylab = paste('Comp.2 (', round(prop[2], 2), ')', sep = ''),
      type = 'n')

   text(obj$scores, rownames(obj$scores), 
      cex = 0.8, col = 4)

   abline(v = mean(obj$scores[, 1]), 
      h =  mean(obj$scores[, 1]), lty = 3)
    
   par(las = 1, cex = 0.7, mar = c(4.5, 4.5, 1, 1))

   barplot(obj$loadings[, 2], 
      horiz = TRUE, xlab = 'Loadings', col = 0)

   abline(v = pretty(par('usr')[1:2]), col = 'gray')

   barplot(obj$loadings[, 2],
      horiz = TRUE, add = TRUE,
      col = sign(obj$loadings[, 2]) + 3)

   invisible(obj)
} 
