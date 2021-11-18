cat("----------------------------------------------------------------- \n")
cat("Teste de comparação de medias (Teste de Tukey)\n")
cat("----------------------------------------------------------------- \n")

tukeyCor <- function (y, trt, DFerror, MSerror, cor.trt = NULL, alpha = 0.05) {
    if(is.null(cor.trt)) {
        trt <- as.factor(trt)
        trt.frame <- data.frame(tapply(y, trt, list))
        colnames(trt.frame) <- as.character(levels(trt))
        cor.trt <- cor(trt.frame)
    } 
    else {cor.trt = cor.trt}

    if(ncol(cor.trt) != length(unique(trt)))
    {stop("'cor.trt' and 'trt' têm dimensões incompatíveis!")}

    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    means <- tapply.stat(junto[, 1], junto[, 2], stat = "mean")
    sds <- tapply.stat(junto[, 1], junto[, 2], stat = "sd")
    nn <- tapply.stat(junto[, 1], junto[, 2], stat = "length")
    means <- data.frame(means, std.err = sds[, 2]/sqrt(nn[, 2]), replication = nn[, 2])
    names(means)[1:2] <- c(name.t, name.y)
    ntr <- nrow(means)
    Tprob <- qtukey(1 - alpha, ntr, DFerror)
    nr <- unique(nn[, 2])
    nfila <- c("Alpha", "Graus de erro de liberdade", "Quadrado médio do erro", "Valor crítico da faixa estudentizada")
    nvalor <- c(alpha, DFerror, MSerror, Tprob)
    xtabla <- data.frame(...... = nvalor)
    row.names(xtabla) <- nfila

    if (length(nr) == 1) {
        HSD <- Tprob * sqrt(MSerror/nr)
    }
    else {
        nr1 <- 1/mean(1/nn[, 2])
        HSD <- Tprob * sqrt(MSerror/nr1)
    }

    
    cat("\nMeios de tratamento de grupos\n")

    output <- order.group_(means[, 1], means[, 2], means[, 4], MSerror, Tprob, means[, 3], parameter = 0.5, cor.trt = cor.trt)

}

require(ExpDes)

order.group_ <- function (trt, means, N, MSerror, Tprob, std.err, parameter = 1, cor.trt) {
    N <- rep(1/mean(1/N), length(N))
    n <- length(means)
    z <- data.frame(trt, means, N, std.err)
    w <- z[order(z[, 2], decreasing = TRUE), ]
    M <- rep("", n)
    k <- 1
    j <- 1
    k <- 1
    cambio <- n
    cambio1 <- 0
    chequeo = 0
    M[1] <- letters[k]

    while (j < n) {
        chequeo <- chequeo + 1

        if (chequeo > n) 
        {break}

        for (i in j:n) {
            minimo <- Tprob * sqrt( parameter * (MSerror * (1/N[i] + 1/N[j]) - 2 * cor.trt[i, j] *MSerror / sqrt(N[i] * N[j])) )
            s <- abs(w[i, 2] - w[j, 2]) <= minimo

            if (s) {
                if (lastC(M[i]) != letters[k]) 
                {M[i] <- paste(M[i], letters[k], sep = "")}
            }
            else {
                k <- k + 1
                cambio <- i
                cambio1 <- 0
                ja <- j

                for (jj in cambio:n) M[jj] <- paste(M[jj], " ", sep = ""){
                    M[cambio] <- paste(M[cambio], letters[k], sep = "")
                }
                for (v in ja:cambio) {
                  if (abs(w[v, 2] - w[cambio, 2]) > minimo) {
                    j <- j + 1
                    cambio1 <- 1
                  }
                  else 
                  {break}
                }
                break
            }
        }
        if (cambio1 == 0) 
        {j <- j + 1}
    }
    w <- data.frame(w, stat = M)
    trt <- as.character(w$trt)
    means <- as.numeric(w$means)
    N <- as.numeric(w$N)
    std.err <- as.numeric(w$std.err)

    for (i in 1:n) {
        cat(M[i], "\t", trt[i], "\t", means[i], "\n")
    }
    output <- data.frame(trt, means, M, N, std.err)
    
    return(output)
}