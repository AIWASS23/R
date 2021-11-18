cat("---------------------------------------------------- \n")
cat("moda: calcular o valor modal para dados desagrupados. Pode ser numérico ou como caractere. \ n ")
cat("---------------------------------------------------- \n")
moda <- function(x) {
	if(is.matrix(x))
	{x <- as.vector(x)}

	else{x <- x}

	t <- table(x)
	m <- names(t)[t == max(t)]

	c = NULL

	for(i in 1:length(t)) {
		if(max(t[i])==max(t)) {
		   c[i] <- 1
		} else{c[i] <- 0}
	}

	if(length(t)==sum(c)) {
	   m <- message("Amodal!")
	} else{m <- m}
	
	return(m)
}