# **** Make MonthYear Count Variable *****;
# Data Vincent;
# Set Vincent;
# MonthYear = (Year - 1960)*12 + Month;
# run;

### installing and loading the plyr package
options(repos = "http://cran.rstudio.com/")
install.packages('plyr')
library(plyr)

### loading data
setwd('~/rtut/radyant/inst/finance/')
tsdata <- read.csv('data/tsdata.csv')
tsdata$MonthYear <- (tsdata$Year - 1960) * 12 + tsdata$Month

### cleaning data
tsdata <- na.omit(tsdata)
firms <- as.factor(tsdata$PERMNO)
nrObs <- table(firms) > 5
sel <- names(nrObs)[nrObs]
tsdata <- tsdata[tsdata$PERMNO %in% sel,]

### saving data
save(tsdata, file = 'data/tsdata.rda')
# load('data/tsdata.rda')

# **** Calculate Market Return by MonthYear and put in dataset called Market *****;
# Proc Sort Data=Vincent; by MonthYear;
# Proc Univariate Data=Vincent noprint; by MonthYear;
# Var RET;
# weight MktCap;
# output out=Market mean=Market_Ret; run;

Market <- ddply(tsdata, .(MonthYear), function(x) data.frame(Market_Ret=weighted.mean(x$RET, x$MktCap)))
head(Market)

### Only keep variables we need
tsdata <- tsdata[,c("PERMNO","RET","MonthYear")]
 
# **** Calculate 3-month Momentum for each stock *****;
# Proc Sort Data=Vincent; by permno MonthYear;
# Data Vincent; Set Vincent;
# Lag1RET= lag1(RET); Lag1Permno = lag1(permno); If permno ne Lag1Permno then Lag1Ret = .;
# Lag2RET= lag2(RET); Lag2Permno = lag2(permno); If permno ne Lag2Permno then Lag2Ret = .;
# Lag3RET= lag3(RET); Lag3Permno = lag3(permno); If permno ne Lag3Permno then Lag3Ret = .;
# Last3_Ret = (1+Lag3RET)*(1+Lag2RET)*(1+Lag1RET) - 1;
# run;

### generate lags of the RET variable
genlags <- function(x,lags) {
	data.frame(embed(as.matrix(x),lags + 1)[,-1])
}

### calculate momement for variable 'var', 'lags' periods back
momentum <- function(x, var, lags = 3) {
	lagvar <- genlags(x[,var], lags)
	LastX <- Reduce('*',lagvar + 1) - 1
	data.frame(cbind(x[-(1:lags),], 'LastX' = LastX))
}

### calculate momentum - takes a minute or three - 
mom <- ddply(tsdata, .(PERMNO), function(x) momentum(x,'RET',3))
head(mom)

# **** Put Stocks into decile bins based on Last3_Ret *****;
# Proc Sort Data=Vincent; by MonthYear;
# proc rank data=Vincent
# groups=10
# out=Vincent;
# var Last3_Ret;
# by MonthYear;
# ranks Last3_Ret_rank;
# run;

deciles <- function(x, var, nrCuts = 10) {
	x$Rank <- cut(x[,var], breaks=quantile(x[,var], probs=seq(0,1, by=1/nrCuts)), 
								include.lowest=TRUE, labels = FALSE)
	x
}

rank <- ddply(mom, .(MonthYear), function(x) deciles(x, 'LastX', 10))
head(rank)
 
# **** Calculate Trading Strategy Return in Each Decile Bin *****;
# Proc Sort Data=Vincent; by MonthYear Last3_Ret_rank;
# Proc Univariate Data=Vincent noprint; by MonthYear Last3_Ret_rank;
# Var RET;
# output out=Vincent2 mean=Strategy_Ret; run;

Strategy_Ret <- ddply(rank, .(MonthYear,Rank), function(x) data.frame('RET' = mean(x$RET)))
head(Strategy_Ret)

# **** Merge Trading Strategy Returns with Market Return *****;
# PROC SQL;
# CREATE TABLE Vincent3 AS
# SELECT *
# FROM Vincent2 a LEFT JOIN Market b
# ON (a.MonthYear = b.MonthYear);
# QUIT; RUN;

regdat <- join(Strategy_Ret, Market)
head(regdat)

# **** Calculate Beta and Alpha and put in a Table *****;
# Proc Sort Data=Vincent3; by Last3_Ret_rank;
# OPTIONS NoCenter NoDate NoNumber LS=64; ODS LISTING; ODS Trace On;
# Proc SurveyReg Data=Vincent3;
# by Last3_Ret_rank;
# model Strategy_Ret = Market_Ret / Solution;
# Ods output ParameterEstimates=Vincent4;
# QUIT;
# RUN;

mods = dlply(regdat, .(Rank), lm, formula = RET ~ Market_Ret)
llply(mods,summary)
