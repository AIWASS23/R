require(graphics)

ogiva <- function(histogram, freq = FALSE, ...) {
	if(class(histogram)!="histogram") {
		stop("você deve usar um histograma!")

	}

	nc <- length(histogram$mids)
	c <- NULL
	d <- NULL

	for(i in 1:nc) {
		c[i] <- sum(histogram$counts[1:i])
		d[i] <- sum(histogram$density[1:i])
	}

	histogram$counts <- c
	histogram$density <- d

	plot(histogram, prob = NULL, freq = freq,
	   main = paste("Ogiva de", histogram$xname), ...)

	result <- list(breaks = histogram$breaks, counts = c,
		intensities = histogram$intensities, density = d,
		mids = histogram$mids, xname = histogram$xname,
		equidist = histogram$equidist)
}