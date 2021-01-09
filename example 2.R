Data1 <- c(1:100)
Data2 <- c(101:200)
plot(Data1, Data2, col="red")
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)
#plot
plot(Data3, Data4, col="blue")
df <- data.frame(Data1, Data2)
plot(df, col="green")
library(tidyverse)
#show the first 10 and then last 10 rows of data in df...
df %>%
head()
df %>%
tail()
data.frame[row,column]
df[1:10, 1]
df[5:15,]
df[c(2,3,6),2]
df[,1]
library(dplyr)
df <- df %>%
dplyr::rename(column1 = Data1, column2=Data2)
dplyr::select(column1)
df %>% 
  dplyr::select(column1)
df$column1
df["column1"]
LondonDataOSK<- read.csv("data/ward-profiles-excel-version.csv", 
                        sep=",")
LondonDataOSK<- read.csv("data/ward-profiles-excel-version.csv", 
                         header = TRUE, sep = ",", encoding = "latin1")
install.packages("here")
library(here)
LondonData <- read_csv("https://files.datapress.com/london/dataset/ward-profiles-and-atlas/2015-09-24T14:21:24/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")
class(LondonData)
LondonBoroughs<-LondonData[626:658,]
Femalelifeexp<- LondonData %>% 
  filter(`Female life expectancy -2009-13`>90)

