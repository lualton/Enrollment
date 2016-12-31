##########
#Finance Simulation# 
##########
#loading the text file
# rm(list=ls())

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)




# Reading Data In ---------------------------------------------

# figure out how to reutrn a dataframe from a function
# Select in function
# rbind.fill should move to a function as well
setwd("/Users/lualt/OneDrive/Work/CRPE/Enrollment/CCD Data")

read.function <- function(filename){
  data <- read.delim(filename, header = TRUE, sep="\t", na.strings = 'N')
}

data <- read.function('F1994_95.txt') #131
df <- data

data <- read.function('F1995_96.txt') #127
df <- rbind.fill(df, data)

data <- read.function('F1996_97.txt') #131
df <- rbind.fill(df, data)

data <- read.function('F1997_98.txt') #129
df <- rbind.fill(df, data)

data <- read.function('F1998_99.txt') 
data <- select(data, 1:128)
df <- rbind.fill(df, data)

data <- read.function('F1999_00.txt')
data <- select(data, 1:128)
df <- rbind.fill(df, data)

data <- read.function('F2000_01.txt')
data <- select(data, 1:128)
df <- rbind.fill(df, data)

data <- read.function('F2001_02.txt')
data <- select(data, 1:128)
df <- rbind.fill(df, data)

data <- read.function('F2002_03.txt')
data <- select(data, 1:129)
df <- rbind.fill(df, data)

data <- read.function('F2003_04.txt')
data <- select(data, 1:134)
df <- rbind.fill(df, data)

df$LEAID <- str_pad(df$LEAID, width = 7, side = "left", pad = "0")

data <- read.function('F2004_05.txt')
data <- select(data, 1:134)
df <- rbind.fill(df, data)

data <- read.function('F2005_06.txt')
data <- select(data, 1:138)
df <- rbind.fill(df, data)

data <- read.function('F2006_07.txt')
data <- select(data, 1:138)
df <- rbind.fill(df, data)

data <- read.function('F2007_08.txt')
data <- select(data, 1:138)
df <- rbind.fill(df, data)

data <- read.function('F2008_09.txt')
data <- select(data, 1:141)
df <- rbind.fill(df, data)

data <- read.function('F2009_10.txt')
data <- select(data, 1:141)
df <- rbind.fill(df, data)

data <- read.function('F2010_11.txt')
data <- select(data, 1:141)
df <- rbind.fill(df, data)

data <- read.function('F2011_12.txt')
data <- select(data, 1:141)
df <- rbind.fill(df, data)

data <- read.function('F2012_13.txt')
data <- select(data, 1:141)
df <- rbind.fill(df, data)

data <- df

head(df %>% select(LEAID, NAME) %>% arrange(LEAID), 30)

data <- select(df, -CCDNF, -CENFILE, -GSLO, -GSHI, -MEMBERSCH, -WEIGHT)

# Going to need to make a function for this as a loop
data$LEAID <- as.character(data$LEAID)
data$NAME <- as.character(data$NAME)
data$STNAME <- as.character(data$STNAME)
data$STABBR <- as.character(data$STABBR)
data$CBSA <- as.character(data$CBSA)
data$LEAID <- as.integer(data$LEAID)
data$CBSA <- as.integer(data$CBSA)

data <- filter(data, LEAID != "NA")
data <- filter(data, V33 >= 1)

# Data wrangling ------------------------------------------------------------
setwd("/Users/lualt/OneDrive/Work/CRPE/Enrollment")
#write.csv(data, "Full Data.csv")

# Making 500 largest LEAID overall

data <- read.csv("Full Data.csv")
backup <- data
#data <- backup

# data <- backup

data$NUMBER[data$YEAR == 13] <- 19
data$NUMBER[data$YEAR == 12] <- 18
data$NUMBER[data$YEAR == 11] <- 17
data$NUMBER[data$YEAR == 10] <- 16
data$NUMBER[data$YEAR == 9] <- 15
data$NUMBER[data$YEAR == 8] <- 14
data$NUMBER[data$YEAR == 7] <- 13
data$NUMBER[data$YEAR == 6] <- 12
data$NUMBER[data$YEAR == 5] <- 11
data$NUMBER[data$YEAR == 4] <- 10
data$NUMBER[data$YEAR == 3] <- 9
data$NUMBER[data$YEAR == 2] <- 8
data$NUMBER[data$YEAR == 1] <- 7
data$NUMBER[data$YEAR == 0] <- 6
data$NUMBER[data$YEAR == 99] <- 5
data$NUMBER[data$YEAR == 98] <- 4
data$NUMBER[data$YEAR == 97] <- 3
data$NUMBER[data$YEAR == 96] <- 2
data$NUMBER[data$YEAR == 95] <- 1

data$YEAR[data$YEAR == 13] <- 2013
data$YEAR[data$YEAR == 12] <- 2012
data$YEAR[data$YEAR == 11] <- 2011
data$YEAR[data$YEAR == 10] <- 2010
data$YEAR[data$YEAR == 9] <- 2009
data$YEAR[data$YEAR == 8] <- 2008
data$YEAR[data$YEAR == 7] <- 2007
data$YEAR[data$YEAR == 6] <- 2006
data$YEAR[data$YEAR == 5] <- 2005
data$YEAR[data$YEAR == 4] <- 2004
data$YEAR[data$YEAR == 3] <- 2003
data$YEAR[data$YEAR == 2] <- 2002
data$YEAR[data$YEAR == 1] <- 2001
data$YEAR[data$YEAR == 0] <- 2000
data$YEAR[data$YEAR == 99] <- 1999
data$YEAR[data$YEAR == 98] <- 1998
data$YEAR[data$YEAR == 97] <- 1997
data$YEAR[data$YEAR == 96] <- 1996
data$YEAR[data$YEAR == 95] <- 1995

leaidLIST <- function(dataframe) {
  z <- 1
  listofdistricts <- dataframe %>% filter(YEAR == 1995) %>%
    arrange(desc(V33)) 
  listofdistricts <- listofdistricts[1:500,]
  
  for(i in 1:18) {
    z <- z + 1
    yearz <- dataframe %>% filter(YEAR == 1995 + z) %>%
      arrange(desc(V33)) 
    yearz <- yearz[1:500,]
    
    listofdistricts <- rbind.fill(listofdistricts, yearz)
  }
  return(unique(listofdistricts$LEAID))
}

list.leaid <- leaidLIST(data)

data <- filter(data, LEAID %in% list.leaid)
write.csv(data, "500 Districts.csv")

# To check for which schools don't have the full 19 schools
checkforerror <- function(dataframe){
  z <- 0
  listofdistricts <- "No Error"
  
  for(i in 1:11041){
    x <- count(dataframe$LEAID[i])
    if(x < 20){
      z <- z + 1
      listofdistricts[z] <- dataframe$Part1[i]
    }
  }
  return(unique(listofdistricts))
}

