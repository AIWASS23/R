dist.Pen <- function(data, vars = NULL) {
	if(class(data)!='data.frame') {
		stop("os dados devem ser um data.frame!")
	}

	data <- as.matrix(data)

	if(is.numeric(vars)) {
		V <- diag(vars)
	}
	else {
		V <- diag(diag(cov(data)))
	}

	no <- nrow(data)
	p <- ncol(data)
	k1 <- rep(1:no, times=no)
	k2 <- rep(1:no, each=no)
	aux <- cbind(rbind(k1[k1==k2], k2[k1==k2]), rbind(k1[k1>k2], k2[k1>k2]))
	Dp <- matrix(NA, nrow=no, ncol=no)

	for(i in 1:ncol(aux)) {
		if(aux[1,i]!=aux[2,i]) {
			Dp[aux[1,i], aux[2,i]] <- 1/p * crossprod((data[aux[1,i],] - data[aux[2,i],]), solve(V, (data[aux[1,i],] - data[aux[2,i],])))
		}
		else {
			Dp[aux[1,i], aux[2,i]] <- 0
		}
	}

	return(Dp = as.dist(Dp))
}