cat("calcular a matriz de covariância da matriz de correlação. \ n")
cat("Cor:matriz de correlação. \ n")
cat("vars:um vetor contendo variâncias de variáveis. \ n")

cor.cov <- function(Cor, vars) {
	if(class(Cor)!="matrix") 
	{stop("Cor deve ser uma matriz!")}

	if(ncol(Cor)!=nrow(Cor)) 
	{stop("Cor deve ser uma matriz simétrica!")}

	if(ncol(Cor)!=length(vars)) 
	{stop("vars deve ter uma dimensão apropriada para Cor!")}

	nvar <- ncol(Cor)
	l1 <- rep(1:nvar, times=nvar)
	l2 <- rep(1:nvar, each=nvar)
	aux <- rbind(l1, l2)
	Smat <- matrix(NA, nrow=nvar, ncol=nvar)
	colnames(Smat) <- colnames(Cor)
	rownames(Smat) <- rownames(Cor)

	for(i in 1:ncol(aux)) {
		Smat[aux[1,i], aux[2,i]] <- Cor[aux[1,i], aux[2,i]] * sqrt(vars[aux[1,i]] * vars[aux[2,i]])
	}
	
	return(Smat)
}