library(devtools); install_github("hadley/rvest")
library(magrittr)
library(rvest)
demo('rvest')

united <- html_session("http://www.united.com/")
account <- united %>% follow_link("Account")
account <- NULL

search <- html_form(html("http://127.0.0.1:4475/"))

html("http://127.0.0.1:4475/")

rad <- html_session("http://127.0.0.1:4475/")

rad %>%
  set_values("Datasets","Diamonds") %>%
  html_nodes("datasets") ->  xxx

str(xxx)

%>%
  extract2(1)




grid graphics
install_github('pmur002/gridGraphics')

install.packages('gridGraphics')

library(gridGraphics)
library(gridSVG)
plot(mpg ~ disp, mtcars, pch=16)
grid.echo()
grid.garnish("graphics-plot-1-points-1", group=FALSE, title=rownames(mtcars))
# grid.garnish("graphics_plot", group=FALSE, title=rownames(mtcars))
grid.export("murrell-echo.svg")


library(gridGraphics)
library(gridSVG)
plot(mpg ~ disp, mtcars, pch=16)
grid.echo()
grid.garnish("graphics-plot-1-points-1", group=FALSE,
             title=rownames(mtcars))
grid.export("murrell-echo.svg")





library(dplyr)
result <- lm(mpg ~ cyl + vs, data = mtcars)

nval <- mtcars
vars <- as.character(attr(result$terms,'variables'))[-1]
vars
select_(nval, .dots = vars[-1])





# qq - plot
x1 <- sort(rnorm(1000))
x2 <- sort(rnorm(1000))
plot(x1,x2)


library(dplyr)

Nash_N <- 1503
Nash_mean <- 1105
Nash_std_dev <- 221

Precision_N <- 509
Precision_mean <- 3016
Precision_std_dev <- 4100

sim_int <- function(sampN, sampMean, sampSd, sampMeanTol = .5, sampSdTol = .5) {

  mean_tol <- sampMeanTol + 100
  std_tol <-  sampSdTol + 100
  itt <- 1

  while(mean_tol > sampMeanTol | std_tol > sampSdTol) {

    sampInt <- rnorm(sampN, mean = sampMean, sd = sampSd)
    sampInt <- ((sampInt - mean(sampInt)) / sd(sampInt))
    sampInt <- ((sampInt - mean(sampInt)) / sd(sampInt)) * sampSd + sampMean
    sampInt <- round(sampInt,0)

    (mean(sampInt) - sampMean) %>% abs -> mean_tol
    (sd(sampInt) - sampSd) %>% abs -> std_tol
    itt <- itt + 1

    # if(min(sampInt) < 0) {
    #   mean_tol <- sampMeanTol + 100
    #   std_tol <-  sampSdTol + 100
    # }

    c(itt,mean(sampInt),sd(sampInt),mean_tol,std_tol) %>% round(2) %>% print
  }

  return(sampInt)
}

Nash_data <- sim_int(1503, 1105, 221)
Precision_data <- sim_int(509, 3016, 4100)

Total <- c(Nash_data,Precision_data)
mean(Total)
sd(Total)

Precision_data <- s_prec

while(tol_mean > .5 | tol_std > .5) {

  s_prec <- round(rnorm(Nash_N, mean = Nash_mean, sd = Nash_std_dev),0)
  (mean(s_prec) - Precision_mean) %>% abs -> tol_mean
  (sd(s_prec) - Precision_std_dev) %>% abs -> tol_std
  itt <- itt + 1

  print(round(c(itt,mean(s_prec),sd(s_prec),tol_mean,tol_std),2))
}








require(dplyr)

mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)

input <- list()
input$tr_columns <- c("cyl","vs")
input$tr_transfunction <- "log"

getdata <- function(dat = mtcars) {
  dat
}

dat <- dat * NA
dat


dat <- getdata()[,input$tr_columns]

vars <- colnames(dat)
dat %>% mutate_each_(input$tr_transfunction, vars)

ret <- try(dat %>% mutate_each_(input$tr_transfunction, vars), silent = TRUE)

is(ret, 'try-error')

attr(ret,"condition")$message



# select_ needs .dots for a vector or strings!!!
select_(getdata(),input$tr_columns)
select_(getdata(),.dots = input$tr_columns)


# dplyr levels error
# levels(getdata()[,input$tr_columns[1]])
# getdata() %>% select_(input$tr_columns[1]) %>% .[1] %>% class
# getdata() %>% .[,input$tr_columns[1]] %>% levels
# getdata() %>% select_(input$tr_columns[1]) %>% levels -> levs

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
