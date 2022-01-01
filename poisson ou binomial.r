plotCIglm <- function(obj, effect, conf.level = 0.95, ordered = TRUE, color = "orange", ...) {
   effect <- paste(deparse(substitute(effect)))
   stopifnot(effect %in% names(obj$model))

   if (!inherits(obj, "glm")){
      stop("o objeto deve ser de classe glm")
   }

   obj <- update(obj, formula = update(obj$formula, ~ . -1))
   lsm <- tapply(fitted(obj), obj$model[, effect], mean)
   nlev <- length(lsm)
   co <- confint(obj)
   fam <- family(obj)

   if (fam$family == "poisson" && fam$link == "log") {
      cop <- exp(co)
   } 
   else if (fam$family == "binomial" && fam$link == "logit") {
      cop <- exp(co) / (exp(co) + 1)
   } 
   else if (fam$family == "binomial" && fam$link == "probit") {
      cop <- pnorm(co)
   } 
   else {
      stop("apenas poisson(log de link)foi usado, use binomial(logit de link ou probit)")
   }

   ul <- cop[, 2]
   ll <- cop[, 1]
   out <- data.frame(LSmeans = lsm, se = NA, nrep = NA,
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
         col = adjustcolor(color, alpha.f = 0.1))
      arrows(i, out[i, 4], i, out[i, 5], 
         length = 0.05, angle = 90, code = 3, ...)
   }

   print(fam)
   invisible(out)
}
