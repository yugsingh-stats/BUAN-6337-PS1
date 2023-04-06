##### Clear memory
rm(list=ls())

##### Set working directory
#setwd("D:/Dropbox/Teaching/UTD/predictive_analysis/2023/problem sets/pset1/solution")
setwd("~/Dropbox/Teaching/UTD/predictive_analysis/2023/problem sets/pset1/solution")

### Read required packages
library("data.table")
library("ggplot2")



##### Q1

### 1.b. Read data
# Read
swineflu = fread(file = "SwinFlu2009.csv",
                 na.strings = c("NA", ""), 
                 sep = "auto",
                 stringsAsFactors = FALSE,
                 data.table = TRUE,
                 header = FALSE, # The data does not contain header
                 colClasses = c(V5 = "character",
                                V14 = "character")
                 )

# Examine the column classes
sapply(swineflu, 
       class)


### 1.c. Assign the proper column names
colnames(swineflu) = c("observation_id",
                       "firstcase_date_id",
                       "firstcase_continent_id",
                       "country",
                       "firstcasereport_date",
                       "cum_case_April",
                       "cum_case_May",
                       "cum_case_June",
                       "cum_case_July",
                       "cum_case_August",
                       "cum_case_Aug09",
                       "firstdeath_date_id",
                       "firstdeath_continent_id",
                       "firstdeath_date",
                       "cum_death_May",
                       "cum_death_Jun",
                       "cum_death_Jul",
                       "cum_death_Aug",
                       "cum_death_Sep",
                       "cum_death_Oct",
                       "cum_death_Nov",
                       "cum_death_Dec")


### 1.d. Change the date columns' data types
swineflu = swineflu[, 
                    firstcasereport_date := as.Date(firstcasereport_date, format = "%m/%d/%Y")]
swineflu = swineflu[, 
                    firstdeath_date := as.Date(firstdeath_date, format = "%m/%d/%Y")]

# Examine the type
class(swineflu$firstcasereport_date)
class(swineflu$firstdeath_date)


### 1.e. Date difference from the first case report date across the world
swineflu = swineflu[, 
                    case_days_from_first_incidence := (firstcasereport_date - as.Date("4/24/2009", format =  "%m/%d/%Y"))]


### 1.f. Subset and save
swineflu_sub = subset(swineflu,
                      select = c("firstcase_date_id",
                                 "country",
                                 "case_days_from_first_incidence"))

fwrite(swineflu_sub,
       "SwineFlu2009_days_from_first_incidence.csv",
       row.names = FALSE)




##### Q2

# 2.a. Read data
pizza = fread(file = "Pizza.csv",
              na.strings = c("NA", ""), 
              sep = "auto",
              stringsAsFactors = FALSE,
              data.table = TRUE,
              header = TRUE # Note header=TRUE here
              )

# 2.b. Print the data in console
pizza

# 2.c. Examine the class of the data columns
sapply(swineflu, 
       class)

# 2.d. Try to examine the data, and compare it with the data read by, e.g., notepad.
library("psych")
describe(pizza)

# 2.f. 
pizza = fread(file = "Pizza.csv",
              na.strings = c("NA", ""), 
              sep = "auto",
              stringsAsFactors = FALSE,
              data.table = TRUE,
              header = TRUE, # Note header=TRUE here
              colClasses = c(SurveyNum = "character") # This is the key part
              )

# 2.g. mean of the ratings
pizza_ratingonly = subset(pizza,
                          select = c("Arugula", "PineNut", "Squash", "Shrimp", "Eggplant"))

pizza = pizza[, 
              AvgRating := rowMeans(pizza_ratingonly, na.rm = TRUE)]



##### Q3

# 3.a. Read data
hotel = fread(file = "hotel.csv",
              na.strings = c("NA", ""), 
              sep = "auto",
              stringsAsFactors = FALSE,
              data.table = TRUE,
              header = FALSE, # Note there's no header
              fill = TRUE
              )

# 3.b. Assign some column names
setnames(hotel, "V1", "room_no")
setnames(hotel, "V2", "no_guests")

# 3.c. check-in and check-out date operations
hotel = hotel[, 
              checkin_date := paste0(V3, "/", V4, "/", V5)]
hotel = hotel[, 
              checkin_date := as.Date(checkin_date, format = "%m/%d/%Y")]
hotel = hotel[, 
              checkout_date := paste0(V6, "/", V7, "/", V8)]
hotel = hotel[, 
              checkout_date := as.Date(checkout_date, format = "%m/%d/%Y")]

# 3.d. days_internetuse
hotel = hotel[V9 == "YES", 
              days_internetuse := V10]
hotel = hotel[V9 == "NO",
              days_internetuse := 0]

class(hotel[, days_internetuse])
hotel = hotel[, days_internetuse := as.numeric(days_internetuse)]

# 3.e. room type
hotel = hotel[V9 == "YES", 
              roomtype := V11]
hotel = hotel[V9 == "NO", 
              roomtype := V10]

# 3.f. room rate
hotel = hotel[V9 == "YES", 
              roomrate := V12]
hotel = hotel[V9 == "NO", 
              roomrate := V11]

class(hotel[, roomrate])
hotel = hotel[, 
              roomrate := as.numeric(roomrate)]


# 3.g. subset the cleaned variables
setnames(hotel, "V9", "internet_use")
hotel = subset(hotel,
               select = c("room_no", "no_guests", 
                          "checkin_date", "checkout_date",
                          "internet_use", "days_internetuse",
                          "roomtype", "roomrate"))

# 3.h. subtotal
hotel = hotel[, 
              nights_stayed := checkout_date - checkin_date]
hotel = hotel[, 
              nights_stayed := as.numeric(nights_stayed)]
hotel = hotel[, 
              subtotal := roomrate * nights_stayed + (no_guests - 1) * 10 * nights_stayed + 5.95 * days_internetuse]
hotel = hotel[internet_use > 0, 
              subtotal := subtotal + 9.95]

# 3.i. grand total
hotel = hotel[, grandtotal := subtotal * 1.0875]

# 3.j. room 247?
hotel_247 = hotel[room_no == 247]