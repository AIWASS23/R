cat("'myplot': a plot in my style! \n")
myplot <- function(x, y = NULL, ...)
{
    plot(x, y, las = 1, ...)
    coord <- par("usr")
    out <- xy.coords(x, y)
    rect(coord[1], coord[3], coord[2], coord[4], col = gray(0.9))
    abline(v = pretty(par("xaxp")[1:2]), h = pretty(par("yaxp")[1:2]), col = "white")
    points(x, y, ...)
}