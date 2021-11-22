cat("---------------------------------------------------------- \n")
cat("rod: plotar um gráfico de rod para as frequências observadas (y) de x. \n")
cat("---------------------------------------------------------- \n")

rod <- function(x, y, xlab=NULL, ylab=NULL, y2lab=NULL, xlim=NULL, ylim=NULL) {
   stopifnot(is.numeric(x))
   stopifnot(is.numeric(y))

   x <- as.vector(x)
   y <- as.vector(y)
   n <- length(x)

   par(mar=c(5,5,3,5.5), las=1)

   plot(x, y, pch=16, xaxp=c(min(x), max(x), n-1), xaxs="i", xlim=xlim, ylim=ylim, ylab=ylab, xlab=xlab)
   abline(h = 0, col="gray")

   for(j in 1:n) {
	   lines(x = c(x[j], x[j]), y = c(0, y[j]), col="blue")
	}

   ax4 <- seq(min(y), max(y), len = 5)
   axis(side=4, at=ax4, labels = round(100*ax4 / sum(y), 1))

   if(is.null(y2lab)) y2lab <- "frequência relativa (%)"
   
   mtext(y2lab, side=4, las=0, padj=4.5)
}