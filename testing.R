mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)

result <- lm(mpg ~ cyl + carb + vs + am, data = mtcars)
summary(result)

str(result)

conf <- round(confint(result),3)
conf <- data.frame(conf[,1],result$coefficients,conf[,2])
colnames(conf) <- c("2.5%","Estimate","97.5%")
conf

seq(.3,1.1,.05)

newdat <- data.frame(cyl = mean(mtcars$cyl), carb = mean(mtcars$carb), vs = "1", am = "1")
predict(reg, new ,interval = 'prediction')

mod <- ggplot2::fortify(result)
vars <- as.character(attr(result$terms,'variables'))[-1]
reg_var1 <- vars[1]
reg_var2 <- vars[-1]
dat <- mod[,reg_var2, drop = FALSE]

isFct <- sapply(dat, is.factor)
isNum <- sapply(dat, is.numeric)

if(sum(isNum) + sum(isFct) < dim(dat)[2]) cat("\nThe model includes data-types that cannot be used for prediction at this point\n")

newdat <- ""
if(sum(isNum) > 0)  newdat <- data.frame(newdat,t(colMeans(dat[,isNum, drop = FALSE])))
if(sum(isFct) > 0)  newdat <- data.frame(newdat,t(apply(dat[,isFct, drop = FALSE],2,function(x) names(which.max(table(x))))))
# if(sum(isNum) > 0)  newdatNum <- t(colMeans(dat[,isNum, drop = FALSE]))
# if(sum(isFct) > 0)  newdatFct <- t(apply(dat[,isFct, drop = FALSE],2,function(x) names(which.max(table(x)))))

data.frame(noquote("carb = -1, vs = '0'"))

data.frame(parse(text = "carb = -1"))
nval <- eval(parse(text = "data.frame(carb = -1, vs = '0')"))
nval

names(nval)


newdat[names(nval)] <- list(NULL)
nnd <- data.frame(newdat[-1],nval)
print(nnd, row.names = FALSE)
print(predict(reg, nnd,interval = 'prediction'), row.names = FALSE)
pred <- predict(reg, nnd,interval = 'prediction')

colnames(pred) <- c("pred", "2.5%","97.5%")
data.frame(pred, row.names = FALSE, check.names = FALSE)
print(pred, row.names = FALSE, check.names = FALSE)
print(data.frame(pred), row.names = FALSE, check.names = FALSE)

pred
colnames(pred) <- c("dd","2.5%","97.5%")
pred

xx <- data.frame(predict(reg, nnd,interval = 'prediction'))
class(xx)
print(xx, row.names = FALSE)

sum(names(nval) %in% names(newdat))





reg <- lm(mpg ~ cyl + carb + vs, data = mtcars)
summary(reg)

# new <- data.frame(mpg = 15, cyl <- mean(mtcars$c))
new <- data.frame(cyl = mean(mtcars$cyl), carb = mean(mtcars$carb), vs = "1")
predict(reg, new ,interval = 'prediction')



mtcars$vs <- as.factor(mtcars$vs)
colMeans(mtcars)


cat("Nr obs:",dim(reg$model)[1])

reg
out <- print(summary(reg))
str(out)

getAnywhere(print.summary.lm)

?print

out <- print(x$coefficients, digits = 3)
attr(a, "heading")[1] <- "Sum of squares table"

a_sub <- a[,c("Df","Sum Sq")]
Total <- colSums(a_sub)
Total
rbind(a_sub,Total)

atab <- anova(reg)
atab
dim(atab)
a[,c("Df","Sum Sq")]

?format.pval format.pval(summary(lm.D9)$coef[ , 4], eps = .001, digits = 2)

atab <- anova(reg)
nr_rows <- dim(atab)[1]
df_reg <- sum(atab$Df[-nr_rows])
df_err <- sum(atab$Df[nr_rows])
df_tot <- df_reg + df_err

ss_reg <- sum(atab$`Sum Sq`[-nr_rows])
ss_err <- sum(atab$`Sum Sq`[nr_rows])
ss_tot <- ss_reg + ss_err
ssTable <- data.frame(matrix(nrow = 3, ncol = 2))
rownames(ssTable) <- c("Regression","Error","Total")
colnames(ssTable) <- c("df","SS")
ssTable$df <- c(df_reg,df_err,df_tot)
ssTable$SS <- c(ss_reg,ss_err,ss_tot)



# x$coefficients <- round(x$coefficients,3)
# format.pval(x$coefficients, eps = .001, digits = 2)
# x$coefficients[x$coefficients == 0] <- "<.001"
# x$coefficients <- data.frame(x$coefficients, check.names = FALSE)

reg <- lm(mpg ~ carb, data = mtcars)
reg.sum <- summary(reg)
.print.summary.lm(reg.sum)

str(reg.sum)


.print.summary.lm <- function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor,
          signif.stars = getOption("show.signif.stars"), ...) {

  if (length(x$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  }
  else {
    cat("\nCoefficients:\n")
    coefs <- x$coefficients
    if (!is.null(aliased <- x$aliased) && any(aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn,
                                                              colnames(coefs)))
      coefs[!aliased, ] <- x$coefficients
    }
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
                 na.print = "NA", ...)
  }

  cat("\n")
  if (nzchar(mess <- naprint(x$na.action)))
    cat("  (", mess, ")\n", sep = "")
  if (!is.null(x$fstatistic)) {
#     cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
    cat("R-squared: ", formatC(x$r.squared, digits = digits))
    cat(", Adjusted R-squared: ", formatC(x$adj.r.squared,
                                           digits = digits),
        "\nF-statistic:", formatC(x$fstatistic[1L], digits = digits), "on", x$fstatistic[2L], "and",
        x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L],
                                                          x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE),
      digits = digits)
    )
#     cat(paste0("\nNr obs:",dim(x$model)[1]))
    cat(paste0("\nNr obs: ",length(x$residuals)))
    cat("\n")
  }
}
