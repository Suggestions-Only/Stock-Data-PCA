#loading in all 5 data sets
require(readr)
apple <- read_csv("~/Fun Stuff/Stock Data/Apple Stock Data.csv")
google <- read_csv("~/Fun Stuff/Stock Data/Google Stock Data.csv")
ibm <- read_csv("~/Fun Stuff/Stock Data/IBM Stock Data.csv")
intel <- read_csv("~/Fun Stuff/Stock Data/Intel Stock Data.csv")
microsoft <- read_csv("~/Fun Stuff/Stock Data/Microsoft Stock Data.csv")
nintendo <- read_csv("~/Fun Stuff/Stock Data/Nintendo Stock Data.csv")
sony <- read_csv("~/Fun Stuff/Stock Data/Sony Stock Data.csv")
texas.inst <- read_csv("~/Fun Stuff/Stock Data/Texas Instruments Stock Data.csv")
yahoo <- read_csv("~/Fun Stuff/Stock Data/Yahoo Stock Data.csv")

#combining all data sets (Nintendo too small, N = 54)
raw.complete <- do.call("rbind", list(apple, google, ibm, intel, 
                                      microsoft, sony, texas.inst,
                                      yahoo))

#make "Stock Name" and "Date" factor variables
raw.complete$Stock_Name <- as.factor(raw.complete$Stock_Name)
raw.complete$Date <- as.factor(raw.complete$Date)

#checking for outliers (terrible number of outliers)
mahal <- mahalanobis(raw.complete[ , -c(1,8)],
                     colMeans(raw.complete[ , -c(1,8)]),
                     cov(raw.complete[ , -c(1,8)]),
                     tol = 1e-20)
cutoff <- qchisq(1-.001, ncol(raw.complete[ , -c(1,8)]))
summary(mahal > cutoff)

#checking for normality (THIS IS WHERE THE PROBLEM IS!)
hist(raw.complete$Open)
hist(raw.complete$High)
hist(raw.complete$Low)
hist(raw.complete$Close)
hist(raw.complete$Volume)
hist(raw.complete$Adj_Close)

#melting data to prepare for analysis
##loading required packages
require(reshape)
require(lubridate)
require(tidyr)
##placing Opening, Closing, High, and Low Stock Prices in same variable
raw.complete <- as.data.frame(raw.complete)
melted.data <- melt(raw.complete, 
                    id = c("Date", "Volume", "Stock_Name", "Adj_Close"))
##creating composite of Opening, Closing, High, and Low Stock Prices
agg.data <- aggregate(value~Date+Stock_Name, data = melted.data, mean,
                      na.rm=TRUE)
##converting data to wide format
wide.data <- spread(agg.data, Stock_Name, value)

#checking number of complete cases (analysis reveals that Nintendo must
#be excluded, N = 54)
require(psych)
describe(wide.data)

#removing missing value cases
no.missing <- na.omit(wide.data)

#remove Date variable
cut.data <- no.missing[,-1]
final.data <- as.data.frame(scale(cut.data))

#factor analysis (data is non-normal, no measurement error,
#PCA is recommended)
##parallel analysis
require(psych)
require(GPArotation)
KMO(final.data)
cortest(final.data)
parallel <- fa.parallel(final.data, fm = "pa", fa = "both", 
                        ylabel = "Eigenvalues", show.legend = FALSE)
cleaner.graph <- fa.parallel(final.data, fm = "pa", fa = "pc", 
                             ylabel = "Eigenvalues", show.legend = FALSE)

#competing models (one factor appears best)
two.factor <- principal(final.data, nfactors = 2, rotate = "none",
                        scores = FALSE, missing = FALSE)
fa.sort(two.factor)
one.factor <- principal(final.data, nfactors = 1, rotate = "none",
                        scores = FALSE, missing = FALSE)
fa.sort(one.factor)

#raw correlation matrix
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
require(Hmisc)
r.list <- rcorr(as.matrix(final.data))
flattenCorrMatrix(r.list$r, r.list$P)