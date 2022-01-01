cat("rankByBlock: a function to rank experimental data by block\n")
rankByBlock <- function(resp, block, data = NULL)
{
    nameresp <- deparse(substitute(resp))
    nameblock <- deparse(substitute(block))
    resp <- data[, nameresp]
    block <- data[, nameblock]
    lab <- paste(nameresp, ".ranks", sep = "")
    data[, lab] <- NA
    ra <- with(data, tapply(resp, block, rank))
    nrep <- length(ra)
    lev <- names(ra)
    for(i in 1:length(ra)) {
       data[block == lev[i], lab] <- ra[[i]]
    }
    return(data)
}