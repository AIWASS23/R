library(ExpDes)

joint <- function(exp, treat, block, resp, quali = TRUE, mcomp = "lsd", alpha = 0.05) {
   mcomp <- match.arg(mcomp, choices = c("lsd", "tukey", "scottknott"))
   exp <- as.factor(exp)
   exp.lev <- levels(exp)
   trt <- as.factor(treat)
   blc <- as.factor(block)

   dat <- data.frame(exp, trt, blc, resp)
   ld <- split(dat, exp)
   mods <- lapply(ld, aov, formula = resp ~ blc + trt)
   anovas <- lapply(mods, anova)
   MSeS <- sapply(mods, deviance) / sapply(mods, df.residual)

   mod <- aov(resp ~ exp/blc + exp*trt)
   tab <- anova(mod)
   p.int <- tab["exp:trt", "Pr(>F)"]
   DFe <- df.residual(mod)
   SSe <- deviance(mod)
   MSe <- deviance(mod)/DFe
   CV <- 100*sqrt(MSe)/mean(resp)
   med <- model.tables(mod, "means")$tables$"exp:trt"

   testlist <- list()

   if (quali) {
      for(i in 1:length(exp.lev)) {
         if(mcomp == "lsd") {
            testlist[[i]] <- lsd2(y = resp[exp == exp.lev[i]], 
               trt = trt[exp == exp.lev[i]], 
               DFerror = DFe, SSerror = SSe, alpha = alpha)
         } 
         else if (mcomp == "tukey") {
            testlist[[i]] <- tukey2(y = resp[exp == exp.lev[i]], 
               trt = trt[exp == exp.lev[i]], 
               DFerror = DFe, SSerror = SSe, alpha = alpha)
         } 
         else if (mcomp == "scottknott") {
            testlist[[i]] <- scottknott2(y = resp[exp == exp.lev[i]], 
               trt = trt[exp == exp.lev[i]], 
               DFerror = DFe, SSerror = SSe, alpha = alpha) 
         }
      }
   } 
   else {
      for(i in 1:length(exp.lev)) {
         testlist[[i]] <- reglinquad(x = tapply(treat, treat, mean),
            y = med[exp.lev[i], ])
      }
   }

   names(testlist) <- exp.lev

   out <- list(anovas = anovas, MSerrors = MSeS, 
      model = mod, quali = quali, comparisons = testlist, 
      means = med, CV = CV, test = mcomp)

   class(out) <- "joint"

   return(out)
   
}

# print method
print.joint <- function(x, ...)
{
   ratio <- max(x$MSerrors)/min(x$MSerrors)
   cat("\nRatio between the largest and the smallest residual mean square:", round(ratio, 2))
   cat("\n\nJoint Analysis of Variance\n\n")
   print(summary(x$model), ...)
   cat("\nCoef. of variation:", round(x$CV, 2), "%\n")
   if (x$quali) {
      cat("\nComparison of treatment means (", x$test, "test ):\n")
      print(x$comparisons, ...)
   } else {
     cat("\nPolynomial regression analysis:\n")
     print(x$comparisons, ...)
     cat("\nMeans:\n")
     print(x$means, ...)
   }
   invisible(x)
}

# aux function: reglinquad
reglinquad <- function(x, y) {
    m1 <- lm(y ~ x)
    b <- round(coef(m1), 6)
    r21 <- round(summary(m1)$r.squared, 2)
    eq1 <- paste(" y = ", b[1], " + ", b[2], "x", sep = "")

    m2 <- lm(y ~ x + I(x^2))
    a <- round(coef(m2), 6)
    r22 <- round(summary(m2)$r.squared, 2)
    eq2 <- paste(" y = ", a[1], " + ", a[2], "x", " + ", a[3], "x^2", sep = "")

    out <- data.frame(Equation = c(eq1, eq2), Rsq = c(r21, r22))
    rownames(out) <- c("linear", "quadratic")
    out
}
