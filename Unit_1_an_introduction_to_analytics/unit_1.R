library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)

x <- fread('Unit_1_an_introduction_to_analytics/WHO.csv')

mean(x$Over60)

x[Over60 == min(Over60)]
x[LiteracyRate == max(LiteracyRate)]

tapply(x$ChildMortality, x$Region, mean) %>% which.min()


# An Analytical Detective -------------------------------------------------

## Problem 1.1
mvt <- fread('Unit_1_an_introduction_to_analytics/mvtWeek1.csv')
nrow(mvt)
ncol(mvt)
max(mvt$ID)
min(mvt$Beat)
sum(mvt$Arrest)
nrow(mvt[LocationDescription == 'ALLEY'])

# Problem 2.1
mvt$Date %>% head()

# Problem 2.2
mvt[, Date := as.Date(Date, format = '%m/%d/%y %H:%M')]
summary(mvt)

# Problem 2.3
mvt[, Month := months(Date)]
mvt[, Weekday := weekdays(Date)]
table(mvt$Month) %>% which.min()

# Problem 2.4
table(mvt$Weekday) %>% which.max()

# Problem 2.5
table(mvt$Month[mvt$Arrest == 1]) %>% which.max()

# Problem 3.1
hist(mvt$Date, breaks=100)

# Problem 3.2
mvt %>%
  ggplot() +
  geom_boxplot(aes(Arrest, Date))

# Problem 3.3
mvt[Year==2001, .N, Arrest] %>%
  .[, .(N / sum(N), Arrest)]

# Problem 3.4
mvt[Year==2007, .N, Arrest] %>%
  .[, .(N / sum(N), Arrest)]

mvt[Year==2012, .N, Arrest] %>%
  .[, .(N / sum(N), Arrest)]

# Problem 4.1
sort(table(mvt$LocationDescription))

# Problem 4.2
location <- sort(table(mvt$LocationDescription)) %>% names()
top_5_location <- location[!grepl('OTHER', location)] %>% tail(5)

Top5 <- mvt[LocationDescription %in% top_5_location]

Top5 %>% dim

# Problem 4.3
Top5[, mean(Arrest), LocationDescription]

# Problem 4.4
Top5[LocationDescription == 'GAS STATION',
     .N, Weekday] %>% .[order(N, decreasing = TRUE)]

# Problem 4.5
Top5[LocationDescription == 'DRIVEWAY - RESIDENTIAL',
     .N, Weekday] %>% .[order(N)]


# Stock Dynamics ----------------------------------------------------------

ibm <- fread('Unit_1_an_introduction_to_analytics/IBMStock.csv')
coca <- fread('Unit_1_an_introduction_to_analytics/CocaColaStock.csv')
ge <- fread('Unit_1_an_introduction_to_analytics/GEStock.csv')
boeing <- fread('Unit_1_an_introduction_to_analytics/BoeingStock.csv')
pg <- fread('Unit_1_an_introduction_to_analytics/ProcterGambleStock.csv')

ibm[, Date := mdy(Date)]
coca[, Date := mdy(Date)]
ge[, Date := mdy(Date)]
boeing[, Date := mdy(Date)]
pg[, Date := mdy(Date)]

total <- rbind(ibm[, stock := 'IBM'],
               coca[, stock := 'COCA'],
               ge[, stock := 'GE'],
               boeing[, stock := 'BE'],
               pg[, stock := 'PG'])

# Problem 1.1
total[, .N, stock]

# Problem 1.2
total[, min(Date), stock]

# Problem 1.3
total[, max(Date), stock]

# Problem 1.4
total[, mean(StockPrice), stock]

# Problem 1.5
total[, min(StockPrice), stock]

# Problem 1.6
total[, max(StockPrice), stock]

# Problem 1.7
total[, median(StockPrice), stock]

# Problem 1.8
total[, sd(StockPrice), stock]

# Problem 2.1
total %>%
  ggplot() +
  geom_line(aes(x = Date, y = StockPrice, col = stock))

# Problem 2.2
total %>%
  ggplot() +
  geom_line(aes(x = Date, y = StockPrice, col = stock)) +
  geom_vline(aes(xintercept = ymd("2000-03-01")))

# Problem 3.1
total %>%
  ggplot() +
  geom_line(aes(x = Date, y = StockPrice, col = stock)) +
  xlim(c(ymd('1995-01-01'), ymd('2005-01-01')))

# Problem 3.2
total %>%
  ggplot() +
  geom_line(aes(x = Date, y = StockPrice, col = stock)) +
  xlim(c(ymd('1995-01-01'), ymd('2005-01-01')))

# Problem 3.3
total %>%
  ggplot() +
  geom_line(aes(x = Date, y = StockPrice, col = stock)) +
  xlim(c(ymd("1997-09-01"), ymd("1997-11-01"))) +
  ylim(c(0,150))

# Problem 3.4
total %>%
  ggplot() +
  geom_line(aes(x = Date, y = StockPrice, col = stock)) +
  xlim(c(ymd("2004-01-01"), ymd("2005-12-01"))) +
  ylim(c(0,120))

# Problem 4.1
total[, mean(StockPrice), .(stock, months(Date))] %>%
  .[stock == 'IBM'] %>%
  .[order(V1, decreasing = T)]

# Problem 4.2
total[, mean(StockPrice), .(stock, months(Date))] %>%
  .[stock %in% c('BE', 'COCA')] %>%
  .[order(V1, decreasing = T)]

# Problem 4.3
total[, mean(StockPrice), .(stock, months(Date))] %>%
  .[months %in% c('January', 'December')] %>%
  dcast.data.table(... ~ stock, value.var = 'V1')


# Demographics and Employment in the United States ------------------------

cps <- fread('Unit_1_an_introduction_to_analytics/CPSData.csv')

# Problem 1.1
dim(131302)

# Problem 1.2
cps[,.N,Industry] %>% .[order(N, decreasing = TRUE)]

# Problem 1.3
sort(table(cps$State))

# Problem 1.4
cps[,.N,Citizenship] %>%
  .[, .(N / sum(N), Citizenship)]

# Problem 1.5
cps[Hispanic == 1,.N,Race] %>%
  .[N >=250 ]

# Problem 2.1
cps %>% is.na() %>% colSums()

# Problem 2.2
cps[, .N, .(Age, is.na(cps$Married))] %>%
  .[, .(percent = N / sum(N), is.na), Age] %>%
  dcast.data.table(...  ~ is.na, value.var = 'percent')

# Problem 2.3
cps[, .N, .(State, is.na(cps$MetroAreaCode))] %>%
  .[, .(percent = N / sum(N), is.na), State] %>%
  .[percent == 1]

# Problem 2.4
cps[, .N, .(Region, is.na(cps$MetroAreaCode))] %>%
  .[, .(percent = N / sum(N), is.na), Region] %>%
  .[`is.na` == TRUE] %>%
  .[order(percent, decreasing = TRUE)]

# Problem 2.5
cps[, .(distance_to_30 = abs(mean(is.na(MetroAreaCode)) - 0.3)), .(State)] %>%
  .[order(distance_to_30)]

cps[, .(non_metro_percent = abs(mean(is.na(MetroAreaCode)))), .(State)] %>%
  .[non_metro_percent < 1] %>%
  .[order(non_metro_percent, decreasing = TRUE)]

# Problem 3.1
MetroAreaCode <- fread('Unit_1_an_introduction_to_analytics/MetroAreaCodes.csv')
CountryOfBirthCode <- fread('Unit_1_an_introduction_to_analytics/CountryCodes.csv')

# Problem 3.2
cps <- MetroAreaCode[cps, on = .(Code = MetroAreaCode)]
cps$MetroArea %>% is.na %>% sum()

# Problem 3.3
cps[,.N,MetroArea] %>%
  .[order(N, decreasing = T)]

# Problem 3.4
cps[,mean(Hispanic == 1),MetroArea] %>%
  .[order(V1, decreasing = T)]

# Problem 3.5
cps[,mean(Race == 'Asian'),MetroArea] %>%
  .[V1 >= 0.2]

# Problem 3.6
cps[,mean(Education == "No high school diploma", na.rm = TRUE),MetroArea] %>%
  .[order(V1)]

# Problem 4.1
cps <- CountryOfBirthCode[cps, on = .(Code = CountryOfBirthCode)]
cps$Country %>% is.na %>% sum()

# Problem 4.2
cps[, .N, Country] %>%
  .[order(N, decreasing = T)]

# Problem 4.3
cps[MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA',
    mean(Country != 'United States', na.rm = TRUE)]

# Problem 4.4
cps[Country == 'India', .N, MetroArea] %>%
  .[order(N, decreasing = TRUE)] %>%
  head(10)
cps[Country == 'Brazil', .N, MetroArea] %>%
  .[order(N, decreasing = TRUE)] %>%
  head(10)
cps[Country == 'Somalia', .N, MetroArea] %>%
  .[order(N, decreasing = TRUE)] %>%
  head(10)
