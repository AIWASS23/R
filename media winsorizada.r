cat("-------------------------------------------------------------- \n")
cat("calcula a média winsorizada para uma proporção de dados. \n")
cat("-------------------------------------------------------------- \n")

winmean <- function(x, p) {
   if(length(p) != 1 || p < 0 || p > 0.5)
	{stop('"p" deve ser um valor entre 0 e 0.5!')}

   qx <- quantile(x, c(p, 1-p))
   x[x < qx[1]] <- qx[1]
   x[x > qx[2]] <- qx[2]

   return(mean(x))
}