# Melissa Mosier
# IST719
# Credit Card Fraud

##### Read in Data #####--------------------------------

my.dir <- "C:\\Users\\mmosi\\OneDrive\\Documents\\SyracuseFiles\\IST719_VIZ\\data files\\"
ccData <- read.csv(file=paste0(my.dir, "ccfraud1.csv"), 
                   header = TRUE, 
                   stringsAsFactors = FALSE)
View(ccData)
# unnamed* | transdatetime | ccnum | merchant | category | amt | first | last
# gender | street | city | state | zip | lat | long | citypop | job | dob
# trans_num | unixtime* | merch_lat | merch_long | is_fraud

str(ccData)
summary(ccData)
(ncol(ccData)*4)*(nrow(ccData)/100)   # 9200
# What does the data set represent?
# Transactions made legitimately or fraudulently, and all metadata associated
# with those transactions, like when and where they occurred, for what purpose,
# and the account information, etc. 

# FIRST VISUAL 
table(ccData$is_fraud)               # 9945:55
pie(table(ccData$is_fraud), main = "Cases of Credit Card Fraud")
# Exported graph, edited in Illustrator

ccData2 <- ccData

##### Cleaning #####--------------------------------

# How did I clean in Python?
##Unnamed: 0 - remove. It doesn't do anything.
#unix_time - remove. I don't know what this is. 
ccData2 <- ccData2[,-c(1,20)]

#trans_date_trans_time - convert to datetime
#cc_num	merchant - remove "fraud_"
ccData2$merchant <- gsub("fraud_", "", ccData$merchant)

#dob - convert to age
# install.packages("eeptools")
library(eeptools)
library(lubridate)
today <- Sys.Date()

which(is.na(ccData2$dob))
ccData2$dob <- dmy(ccData2$dob)

ccData2$dob <- as.Date(ccData2$dob)
age <- age_calc(ccData2$dob, today, units="years")
ccData2$age <- floor(age)

#dummy variable for gender
# install.packages("fastDummies")
library(fastDummies)
ccData2$genDummy <- dummy_cols(ccData2$gender)

# These are all fine
# category  /  amt  /  first  /  last
# street  /  city  /  state  /  zip  /  lat  /  long
# city_pop  /  job  /  trans_num  /  merch_lat
# merch_long  /  is_fraud 

View(ccData2)
summary(ccData2)

##### Exploratory Distributions #####--------------------------------
# Ex: Histograms, box plots, density plots; frequencies in bar chart or pie chart

par(mfrow = c(2,1))

fraudSubset <- subset(ccData2, ccData2$is_fraud==1)

barplot(table(ccData2$gender), main="Total Transactions by gender", ylim=c(0,6000))            # gen
barplot(table(fraudSubset$gender), main="Fraudulent Transactions by gender", ylim=c(0,30))   # fraud

hist(log(ccData2$city_pop), main="Total Transactions by city population")            # gen pop
hist(log(fraudSubset$city_pop), main="Fraudulent Transactions by city population")   # frauded pop

#amt, take log
hist(log(ccData2$amt), xlim=c(0,10), ylim=c(0,2500), main="Total Transactions by transaction amount")
hist(log(fraudSubset$amt), xlim=c(0,10), ylim=c(0,25), main="Fraudulent Transactions by transaction amount")
# look into this difference!

# Exported each set of graphs as PDFs



# Multidimensional plot using at least 2-3 columns
# Indicate which plots are single dimension vs multidimension

# Age vs City_pop

plot(ccData2$city_pop, ccData2$age)
library(RColorBrewer)
col.vec <- rep(rgb(30,144,255,maxColorValue=255), nrow(ccData2))
col.vec[ccData2$is_fraud==1] <- rgb(255,64,64, maxColorValue = 255)
plot(ccData2$city_pop, ccData2$age, pch=16, cex=1, col=col.vec)
# Exported graph, edited in Illustrator


### CORRELATION ###----------------------------------------

numVars <- c("amt", "zip", "city_pop", "age", "is_fraud")
ccDataNUM <- ccData2[numVars]

#install.packages("corrplot")
library(corrplot)
cor <- cor(ccDataNUM, method="pearson")
corrplot(cor, method="circle", type = "upper")
# Exported graph, edited in Illustrator


### RANDOM FOREST MODEL ###----------------------------------------

x <- c("gender", "amt", "zip", "city_pop", "mlat", "mlong", "age")
y <- c(0.012,   0.419,  0.089,  0.082,    0.160,   0.159,   0.0779)
rf <- data.frame(x, y)
y2 <- sort(y)
rf2 <- rf[order(rf$y),]

barplot(y2, horiz=TRUE,
        xlim = c(0, 0.5),
        names.arg = c("gender", "age", "city_pop", "zip", "mlong", "mlat", "amt"),
        ylab = "Predictive Variables",
        main="Random Forest Model")
# Exported graph, edited in Illustrator


### XX ###----------------------------------------
# time series
# amt vs is_fraud   <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

library(ggplot2)
timeseries <- c("trans_date_trans_time", "is_fraud")
ccTime <- ccData2[timeseries]
summary(ccTime)

# Gave up on this one too
# It doesn't show a meaningful pattern anyway
# for reference:  https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

### CHOROPLETH ###----------------------------------------

library(maps)
library(mapproj)
# library(raster)
library(plotrix)
table <- aggregate(ccData2$is_fraud, list(ccData2$state), sum)
colnames(table) <- c("state", "fraud")
table

# assigning colors
num.cols <- 5
my.color.vec <- rev(heat.colors( num.cols))
pie(rep(1,num.cols), col=my.color.vec)

table$index <- round(rescale(x = table$fraud, c(1,num.cols)),0)
table$color <- my.color.vec[table$index]
table

# map abbreviations to states
state.abb
state.name
stateabb <- rbind(state.abb, state.name)
stateabb <- data.frame(stateabb)
stateabb <- t(stateabb)
View(stateabb)

choroTable <- merge (table, stateabb, by.x ="state", by.y="state.abb")
vars <- c("state.name", "color")
choroTable <- choroTable[vars]
choroTable

# map colors to numbers to states
m <- map("state")
m$names

state.order <- match.map(database = "state",
                         regions = choroTable$state.name,
                         exact=FALSE, warn=TRUE)
cbind(m$names, choroTable$state.name[state.order])
map("state", col=choroTable$color, fill=TRUE,
    resolution=0, lty=1, projection="polyconic", border="tan")


### XX ###----------------------------------------

# Violin plot?
library(vioplot)
vioplot()
### Realized I didn't even know what I wanted to put in this plot........
### The choroplet is better anway

