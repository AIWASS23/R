scottknott2 <- function (y, trt, DFerror, SSerror, alpha = 0.05) {

    sk <- function(medias, s2, dfr, prob) {
        bo <- 0
        si2 <- s2
        defr <- dfr
        parou <- 1
        np <- length(medias) - 1

        for (i in 1:np) {
            g1 <- medias[1:i]
            g2 <- medias[(i + 1):length(medias)]
            B0 <- sum(g1)^2/length(g1) + sum(g2)^2/length(g2) - (sum(g1) + sum(g2))^2/length(c(g1, g2))

            if (B0 > bo) {
                bo <- B0
                parou <- i
            }
        }
        g1 <- medias[1:parou]
        g2 <- medias[(parou + 1):length(medias)]
        teste <- c(g1, g2)
        sigm2 <- (sum(teste^2) - sum(teste)^2/length(teste) + defr * si2)/(length(teste) + defr)
        lamb <- pi * bo/(2 * sigm2 * (pi - 2))
        v0 <- length(teste)/(pi - 2)
        p <- pchisq(lamb, v0, lower.tail = FALSE)
        
        if (p < prob) {
            for (i in 1:length(g1)) {
                cat(names(g1[i]), "\n", file = "skresult", append = TRUE)
            }
            cat("*", "\n", file = "skresult", append = TRUE)
        }
        if (length(g1) > 1) {
            sk(g1, s2, dfr, prob)
        }
        if (length(g2) > 1) {
            sk(g2, s2, dfr, prob)
        }
    }
    medias <- sort(tapply(y, trt, mean), decreasing = TRUE)
    dfr <- DFerror
    rep <- tapply(y, trt, length)
    s0 <- MSerror <- SSerror/DFerror
    s2 <- s0/rep[1]
    prob <- alpha
    sk(medias, s2, dfr, prob)
    f <- names(medias)
    names(medias) <- 1:length(medias)
    resultado <- data.frame(r = 0, f = f, m = medias)

    if (file.exists("skresult") == FALSE) {
        stop
    }
    else {
        xx <- read.table("skresult")
        file.remove("skresult")
        x <- xx[[1]]
        x <- as.vector(x)
        z <- 1

        for (j in 1:length(x)) {
            if (x[j] == "*") {
                z <- z + 1
            }
            for (i in 1:length(resultado$f)) {
                if (resultado$f[i] == x[j]) {
                  resultado$r[i] <- z
                }
            }
        }
    }
    letras <- letters

    if (length(resultado$r) > 26) {
        l <- floor(length(resultado$r)/26)

        for (i in 1:l) { 
            letras <- c(letras, paste(letters, i, sep = ""))
        }
    }
    res <- 1

    for (i in 1:(length(resultado$r) - 1)) {
        if (resultado$r[i] != resultado$r[i + 1]) {
            resultado$r[i] <- letters[res]
            res <- res + 1

            if (i == (length(resultado$r) - 1)) {
                resultado$r[i + 1] <- letters[res]
            }
        }
        else {
            resultado$r[i] <- letters[res]

            if (i == (length(resultado$r) - 1)) {
                resultado$r[i + 1] <- letters[res]
            }
        }
    }
    
    names(resultado) <- c("groups", "trt", "means")
    resultado
}

tukey2 <- function (y, trt, DFerror, SSerror, alpha = 0.05) {
    MSerror <- SSerror/DFerror
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

    nfila <- c("Alpha", "Graus de liberdade do erro", "Quadrado médio do erro", "Valor crítico do intervalo estudentizado")

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
        output <- order.group2(means[, 1], means[, 2], means[, 4], MSerror, Tprob, means[, 3], parameter = 0.5)
    output
}

lsd2 <- function (y, trt, DFerror, SSerror, alpha = 0.05) {
    MSerror <- SSerror/DFerror
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    means <- tapply.stat(junto[, 1], junto[, 2], stat = "mean")
    sds <- tapply.stat(junto[, 1], junto[, 2], stat = "sd")
    nn <- tapply.stat(junto[, 1], junto[, 2], stat = "length")
    means <- data.frame(means, std.err = sds[, 2]/sqrt(nn[, 2]), replication = nn[, 2])
    names(means)[1:2] <- c(name.t, name.y)
    ntr <- nrow(means)
    Tprob <- qt(1 - (alpha/2), DFerror) * sqrt(2)
    nr <- unique(nn[, 2])

    nfila <- c("Alpha", "Graus de liberdade do erro", "Quadrado médio do erro", "Valor crítico do intervalo estudentizado")

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
        output <- order.group2(means[, 1], means[, 2], means[, 4], MSerror, Tprob, means[, 3], parameter = 0.5)
    output
}

order.group2 <- 

function (trt, means, N, MSerror, Tprob, std.err, parameter = 1) {
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

        if (chequeo > n) { 
            break
        }

        for (i in j:n) {
            minimo <- Tprob * sqrt(parameter * MSerror * (1/N[i] + 1/N[j]))
            s <- abs(w[i, 2] - w[j, 2]) <= minimo

            if (s) {
                if (lastC(M[i]) != letters[k]) {
                  M[i] <- paste(M[i], letters[k], sep = "")
                }
            }
            else {
                k <- k + 1
                cambio <- i
                cambio1 <- 0
                ja <- j

                for (jj in cambio:n) {
                    M[jj] <- paste(M[jj], " ", sep = "")
                    M[cambio] <- paste(M[cambio], letters[k], sep = "")
                }

                for (v in ja:cambio) {
                    if (abs(w[v, 2] - w[cambio, 2]) > minimo) {
                        j <- j + 1
                        cambio1 <- 1
                    }
                    else { 
                        break
                    }
                }

                break
            }
        }

        if (cambio1 == 0) {
            j <- j + 1
        }
    }

    w <- data.frame(w, stat = M)
    trt <- as.character(w$trt)
    means <- as.numeric(w$means)
    N <- as.numeric(w$N)
    std.err <- as.numeric(w$std.err)
    output <- data.frame(trt, means, group = M, n, std.err)
    
    return(output)
}


