library(plyr)
library(stringr)
library(lubridate)
library(zoo)
library(tidyverse)
library(dplyr)
library(tidyverse)
library(xts)

library(readr)
Crime_Data_from_2020_to_Present <- read_csv("Crime_Data_from_2020_to_Present.csv")

df = Crime_Data_from_2020_to_Present

rm(Crime_Data_from_2020_to_Present)

descent = c('A', 'B', 'C', 'D', 'F', 'G', 'H', 'I',
            'J', 'K', 'L', 'O', 'P', 'S', 'U', 'V', 
            'W', 'X', 'Z')

race = c("asian", "black", "chinese", "cambodian",
         "filipino", "guamannina", "hispanic", "american indian",
         "japanese", "korean", "laotian", "other",
         "pacific", "samoan", "hawaiian", "vietnamese",
         "white", "unknown", "asian indina")


df$race = mapvalues(df$`Vict Descent`, from = descent,
                    to = race)
detach(package:plyr)
#df$time = format(round(as.POSIXct(df$`TIME OCC`, format = "%H%M"), units = "hours"), format = "%H:%M")

#df$month = factor(month.abb[as.integer(format(as.POSIXct(df$`DATE OCC`, format = "%m/%d/%y"), format = "%m"))], levels = month.abb)

#df$year = as.integer(format(as.POSIXct(df$`DATE OCC`, 
                                       #format = "%m/%d/%Y"),
                            #format = "%Y"))

#days = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
#df$weekDay =  factor(weekdays(as.Date(df$`DATE OCC`, format = "%m/%d/%y")), levels = days)

df$stdTime = as.Date(df$`DATE OCC`, format = "%m/%d/%Y")

battery = df%>%
  filter(str_detect(`Crm Cd Desc`, "BATTERY")) %>%
  mutate(crimeType = "battery")

theft = df%>%
  filter(str_detect(`Crm Cd Desc`, "THEFT OF IDENTITY")) %>%
  mutate(crimeType = "identity theft")

shoplifting = df%>%
  filter(str_detect(`Crm Cd Desc`, "SHOPLIFTING")) %>%
  mutate(crimeType = "shoplifting")

pickpoketing = df%>%
  filter(str_detect(`Crm Cd Desc`, "PICKPOCKET") | str_detect(`Crm Cd Desc`, "PURSE")) %>%
  mutate(crimeType = "pickpoketing")


vehicle = df%>%
  filter(str_detect(`Crm Cd Desc`, "VEHICLE") & str_detect(`Crm Cd Desc`, "STOLEN")) %>%
  mutate(crimeType = "vehicle theft")

robbery = df%>%
  filter(str_detect(`Crm Cd Desc`, "ROBBERY")) %>%
  mutate(crimeType = "robbery")

burglary = df%>%
  filter(str_detect(`Crm Cd Desc`, "BURGLARY")) %>%
  mutate(crimeType = "burglary")

assault = df%>%
  filter(str_detect(`Crm Cd Desc`, "ASSAULT WITH DEADLY WEAPON") | str_detect(`Crm Cd Desc`, "INTIMATE")) %>%
  mutate(crimeType = "assault")

arson = df%>%
  filter(str_detect(`Crm Cd Desc`, "ARSON")) %>%
  mutate(crimeType = "arson")

myCrime = rbind(battery, shoplifting, assault, vehicle, pickpoketing, arson, burglary, robbery, theft)

rm(battery, shoplifting, assault, vehicle, pickpoketing, arson, burglary, robbery, theft)
rm(df)

areas = myCrime %>%
  group_by(`AREA NAME`) %>%
  filter(LON != 0 & LAT != 0) %>%
  summarise_at(vars(LON, LAT), list(name = mean))


###########################################

act = unique(myCrime$crimeType)
choiceAct = setNames(1:9, act)
area = unique(myCrime$`AREA NAME`)

###########################################

test = myCrime %>%
  group_by(stdTime, crimeType) %>%
  tally()
test = as.data.frame(test)
test

beta = test %>%
  group_by(crimeType) %>%
  mutate(MA = rollmean(n, 20, fill = NA))
beta = as.data.frame(beta)
beta

mer = xts()

time = function(crm) {
 temp = beta %>%
   filter(crimeType == crm)
 tempPlot = xts(temp$MA, temp$stdTime)
 return(tempPlot)
}


for (i in 1:9) {
  mer = merge(mer, time(act[i]))
  print(mer)
}

names(mer) = act

######################################

foo = myCrime %>%
  group_by(stdTime, `AREA NAME`) %>%
  tally()
foo

foo = foo %>%
  group_by(`AREA NAME`) %>%
  mutate(MA = rollmean(n, 50, fill = NA))

foo = as.data.frame(foo)
foo

fmer = xts()

time = function(are) {
 temp = foo %>%
   filter(`AREA NAME` == are)
 tempPlot = xts(temp$MA, temp$stdTime)
 return(tempPlot)
}

for (i in 1:21) {
  fmer = merge(fmer, time(area[i]))
}

names(fmer) = area
#####################################

cood = areas
cood$LON_name = cood$LON_name - 0.01
cood$LAT_name = cood$LAT_name + 0.01
















