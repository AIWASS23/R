cat("-------------------------------------------------------------- \n")
cat("curva.max: calcular o ponto máximo de curvatura. \n")
cat("-------------------------------------------------------------- \n")

curve.max <- function(x.range, fun, graph = TRUE, ...) {
    stopifnot(is.atomic(x.range))

    if(!is.numeric(x.range))
	{stop("'x.range' deve ser um vetor numérico!")}

    if(length(x.range) != 2)
	{stop("'x.range' deve ser um vetor de comprimento dois!")}

    if(diff(x.range) < 0)
	{stop("por favor, reordene 'x.range'.")}


    if(!inherits(fun, "function"))
	{stop("'fun' deve ser uma 'função' de x!")}

    dfun <- deriv(fun2form(fun), "x", func = TRUE)

    if(attr(dfun(x.range[1]), "gradient") == attr(dfun(x.range[2]), "gradient"))
    {stop("'fun' não deve ser uma função linear de x!")}

    b <- lm(range(fun(x.range)) ~ x.range)$coef
    newx <- seq(x.range[1], x.range[2], length.out = 2000)
    newy <- fun(newx)
    
    if(fun(x.range[1]) > fun(x.range[2])) {si <- -1 } 
    else { si <- 1 }

    pred.lm <- mean(fun(x.range)) - si* b[2] * mean(x.range) + si * b[2] * newx
    delta <- NULL

    for(i in 1:length(newy)) {
	    delta[i] <- abs(newy[i] - pred.lm[i])
    }

    ind <- which.max(delta)

    if(graph) {
        curve(fun(x), from = x.range[1], to = x.range[2], main = "Maximo ponto da curva", las = 1, ...)
        lines(x = c(newx[ind], newx[ind]), y = c(-1e+10, newy[ind]), lty = 3)
        lines(x = c(-1e+10, newx[ind]), y = c(newy[ind], newy[ind]), lty = 3)
    }

    cat( "<<Ponto de curvatura máxima >> \n", "critico x: ", newx[ind], "\n", "critico y: ", fun(newx[ind]),"\n")
    out <- list(x = newx[ind], y = newy[ind])

    class(out) <- "curva max"
    invisible(out)
}

fun2form <- function(fun, y = NULL) {
    if(!inherits(fun, "function"))
    {stop("'fun' deve ser um objeto da classe'function'!")}

    if(!is.null(y) & !inherits(y, "character"))
    {stop("'y' deve ser um 'character' que vai definir o lado esquerdo da fórmula!")}

    form <- as.formula(paste(y, "~", deparse(fun)[2]))
    return(form)
}