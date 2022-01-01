plotCI <- function(obj, effect, conf.level = 0.95, equal.var = TRUE, ordered = TRUE, ...) {
   effect <- paste(deparse(substitute(effect)))

   stopifnot(effect %in% names(obj$model))

   if (!inherits(obj, c("lm", "aov"))) {
      stop("o objeto deve ser de classe lm ou aov")
   }

   yhat <- fitted(obj)
   y <- yhat + residuals(obj)
   lsm <- tapply(yhat, obj$model[, effect], mean)
   nlev <- length(lsm)
   nrep <- tapply(yhat, obj$model[, effect], length)
   rdf <- obj$df.residual
   sigma <- sqrt(deviance(obj) / rdf)
   if (equal.var) {
      se <- rep(sigma, nlev) / sqrt(nrep)
   } 
   else {
      se <- tapply(y, obj$model[, effect], sd) / sqrt(nrep)
   }

   ncomp. <- choose(nlev, 2)
   ncomp <- ifelse(ncomp. < 2, 2, ncomp.)
   tval <- qt(conf.level^ncomp, rdf)
   ul <- lsm + se * tval
   ll <- lsm - se * tval

   out <- data.frame(LSmeans = lsm, se = se, nrep = nrep, 
      UCL = ul, LCL = ll, levels = levels(obj$model[, effect]))

   if (ordered) {
      o <- order(out[, 1])
      out <- out[o, ]
   }

   means <- out[, 1]

   plot(means, xaxt = "n", ...)
   axis(1, at = 1:nlev, labels = out[, 6])
   abline(v = seq(1, nlev), col = "gray")

   for(i in 1:nlev) {
      polygon(x = c(-100, -100, i, i), 
         y = c(out[i, 5], out[i, 4], out[i, 4], out[i, 5]), 
         border = "gray", lty = 3,
         col = adjustcolor("orange", alpha.f = 0.1))

      arrows(i, out[i, 4], i, out[i, 5], 
         length = 0.05, angle = 90, code = 3, ...)

   }
   cat("\n Taxa de erro tipo I para a família:", round(1-conf.level^ncomp, 4), "\n")

   invisible(out)
}
