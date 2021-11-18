cat("----------------------------------------------------------------- \n")
cat("uma função para elevar uma matriz quadrada a uma potência \n")
cat("----------------------------------------------------------------- \n")

power.matrix <- function(x, power = 1) {
   if(!inherits(x, "matrix") || nrow(x) != ncol(x))
   {stop("'x' deve ser uma matriz quadrada!")}

   eig <- eigen(x)
   val <- diag(eig$values ^ power)
   vec <- eig$vectors
   res <- vec %*% val %*% t(vec)

   return(res)
}