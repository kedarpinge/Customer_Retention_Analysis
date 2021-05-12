library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(reshape2)

#Importing the dataset
df<-read.csv('K:/Desktop/Personal Work/Myriad Analytics/Sales_Dataset.csv')
head(df)

#Let's check for any missing values in the data
colSums(is.na(df))

#In this scenario, we have 251 missing values in the Unit Price Column, we can actually
#it entirely given how less the data we potentially lose compared to our orginal data

df <- na.omit(df)

#We'll check for datatypes :

str(df)

#Finding the distribution of customers in each country
Geo_Dist <- table(df$Country)
Geo_Dist

barplot(Geo_Dist)

#Create a new column for Total
df$Total<-df$Unit_Price*df$Quantity

#We observe maximum number of customers in USA, higher than any other country

df$Month <- month(df$Order_Date)
head(df)

sales_by_month <- df %>% group_by(Month) %>%
  summarise(Total_Sales=sum(Total)) %>% ungroup

sales_by_month<-as.data.frame(sales_by_month)
#Plot this

barplot(sales_by_month$Total_Sales,main = "Total Sales in a month",xlab = "Month",ylab = "Total Sales",names.arg = sales_by_month$Month ,col = "darkred",horiz = FALSE)

#We can use Customer Lifetime Value (CLV) approach based on the data provided

head(df)






#We will use Cohort Analysis to determine the behavioral patterns of the customers
#Let's create a dataframe of Customer IDs and Dates for this

ctime<-df[,6:7]
head(ctime)  

#Convert date into YYYY MM format for creating cohorts with monthly time intervals
ctime$Order_Date<-format(as.Date(df$Order_Date, "%Y-%m-%d"), "%Y-%m")
head(ctime)

#Create Cohort for Analysis

cohort <- ctime %>%                         # store in cohort table, get from ctime
  group_by(Customer_ID) %>%                      # group all users together
  mutate(first = min(Order_Date)) %>%                # for every user, find the first period
  group_by(first, Order_Date) %>%                    # group by this first period + the other periods
  summarise(users = n_distinct(Customer_ID)) %>% # for each combination, count the number of users
  spread(Order_Date, users)   # and make columns with period names 

View(cohort)

#This cohort table should be aligned to the left and our column names should be left aligned as well.


shiftrow <- function(v) {
  # put a vector in, strip off leading NA values, and place that amount at the end
  first_na_index <- min( which(!is.na(v)) )
  
  # return that bit to the end,  and pad with NAs.
  c(v[first_na_index:length(v)], rep(NA, first_na_index-1))
}

# create a new dataframe, with shifted rows (and keep the first one)
shifted <- data.frame(
  cohort = cohort$first,
  t(apply( select(as.data.frame(cohort), 2:ncol(cohort)), # 2nd column to the end
           1, # for every row
           shiftrow ))
)

# and make column names readable
# first should be "cohort" and the rest month.<number>, (padded)
colnames(shifted) <- c("cohort", sub("","month", str_pad(1:(ncol(shifted)-1),2,pad = "0")))

View(shifted)

#This now looks like a cohort table layout, however we don't need values, we need percentages.

shifted_pct <- data.frame(
  cohort = shifted$cohort, # first column
  shifted[,1:nrow(shifted)+1] / shifted[["month01"]] # rest: divide by week.01
)

View(shifted_pct)

#Plotting the Data

# ggplot loves long data. Let's melt it. One for the absolute values, one for the percentages
plotdata_abs <- gather(shifted,     "cohort_age", "people"  ,2:ncol(shifted    ))
plotdata_pct <- gather(shifted_pct, "cohort_age", "percent" ,2:ncol(shifted_pct))
# now add some data.. we need pretty labels..
# first bit is the length of the width of the wide column (minus 1, that's the cohort name)
# that contains the absolute numbers
# last bit is the rest, those are percentages.
labelnames <- c( plotdata_abs$people[1:(ncol(shifted)-1)],
                 plotdata_pct$percent[(ncol(shifted)):(nrow(plotdata_pct))])
# we need pretty labels.
pretty_print <- function(n) {
  case_when( n <= 1  ~ sprintf("%1.0f %%", n*100),
             n >  1  ~ as.character(n),
             TRUE    ~ " ") # for NA values, skip the label
}

# create the plot data
plotdata <- data.frame(
  cohort     = plotdata_pct$cohort,
  cohort_age = plotdata_pct$cohort_age,
  percentage = plotdata_pct$percent,
  label      = pretty_print(labelnames)
)
plotdata[which(plotdata$percentage == 1), "percentage"] <- 0
ggplot(plotdata, aes(x = cohort_age, y = reorder(cohort, desc(cohort)))) +
  geom_raster(aes(fill = percentage)) +
  scale_fill_continuous(guide = FALSE) + # no legend
  geom_text(aes(label = label), color = "white") +
  xlab("cohort age") + ylab("cohort") + 
  ggtitle(paste("Retention Table (cohort) for XYZ")) +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1))
#connect libraries


shifted[is.na(shifted)]<-0

cohort.clients.r <- shifted #create new data frame
totcols <- ncol(cohort.clients.r) #count number of columns in data set
for (i in 1:nrow(cohort.clients.r)) { #for loop for shifting each row
  df <- cohort.clients.r[i,] #select row from data frame
  df <- df[ , !df[]==0] #remove columns with zeros
  partcols <- ncol(df) #count number of columns in row (w/o zeros)
  #fill columns after values by zeros
  if (partcols < totcols) df[, c((partcols+1):totcols)] <- 0
  cohort.clients.r[i,] <- df #replace initial row by new one
}

c <- ncol(cohort.clients.r)
reten.r <- cohort.clients.r
for (i in 2:c) {
  reten.r[, (c+i-1)] <- reten.r[, i] / reten.r[, 2]
}
reten.r <- reten.r[,-c(2:c)]
colnames(reten.r) <- colnames(cohort.clients.r)

#charts
reten.r <- reten.r[,-2] #remove M01 data because it is always 100%
#dynamics analysis chart
cohort.chart1 <- melt(reten.r, id.vars = 'cohort')
colnames(cohort.chart1) <- c('cohort', 'month', 'retention')
cohort.chart1 <- filter(cohort.chart1, retention != 0)
p <- ggplot(cohort.chart1, aes(x=month, y=retention, group=cohort, colour=cohort))
p + geom_line(size=2, alpha=1/2) +
  geom_point(size=3, alpha=1) +
  geom_smooth(aes(group=1), method = 'loess', size=2, colour='red', se=FALSE) +
  labs(title="Cohorts Retention ratio dynamics")