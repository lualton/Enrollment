#### Enrollment ####


# rm(list=ls())

library(lme4)
# library(car)
# library(foreign)
# library(MASS)
# library(robustlmm)
library(ggplot2)
library(ggthemes)
library(plotly)
library(plyr)
library(dplyr)
library(tidyr)
library(quantmod)
library(ggrepel)

#Sys.setenv("plotly_username"="altonlu")
#Sys.setenv("plotly_api_key"="lkMBr47wPUZnXNf1jDJh")
#plotly_POST(p, filename = "plot1")

setwd("/Users/lualt/OneDrive/Work/CRPE/Enrollment")
data <- read.csv("500 Districts.csv")
#data <- read.csv("500 LEAID.csv")

errorschools <- c(600047, 600153, 601329, 601330, 4503902,
                  601332, 623160, 691078, 4100023, 4900142,
                  4100046)

data <- filter(data, !(LEAID %in% errorschools))

# Manipulating Data --------------------------------------------------
# Data is for change, df is for percent of total

data$NAME <- as.character(data$NAME)
data$STNAME <- as.character(data$STNAME)
data$STABBR <- as.character(data$STABBR)
#data[data <= 0] <- NA
#data[data == -2] <- NA

data <- select(data, -FIPST, -SCHLEV, -FIPSCO, -CMSA, -AGCHRT, -CONUM, -CSA, -CBSA)

data <- mutate(data, ENR = V33)
data <- data[,c(1:6, 141, 7, 142, 8:140)]

# Inflation Adjustment
cpi <- read.csv("cpi.csv")

d95 <- filter(data, YEAR == 1995)
d95[,10:142] <- d95[,10:142] * cpi$Rate[cpi$Year == 1995]
d96 <- filter(data, YEAR == 1996)
d96[,10:142] <- d96[,10:142] * cpi$Rate[cpi$Year == 1996]
d97 <- filter(data, YEAR == 1997)
d97[,10:142] <- d97[,10:142] * cpi$Rate[cpi$Year == 1997]
d98 <- filter(data, YEAR == 1998)
d98[,10:142] <- d98[,10:142] * cpi$Rate[cpi$Year == 1998]
d99 <- filter(data, YEAR == 1999)
d99[,10:142] <- d99[,10:142] * cpi$Rate[cpi$Year == 1999]
d00 <- filter(data, YEAR == 2000)
d00[,10:142] <- d00[,10:142] * cpi$Rate[cpi$Year == 2000]
d01 <- filter(data, YEAR == 2001)
d01[,10:142] <- d01[,10:142] * cpi$Rate[cpi$Year == 2001]
d02 <- filter(data, YEAR == 2002)
d02[,10:142] <- d02[,10:142] * cpi$Rate[cpi$Year == 2002]
d03 <- filter(data, YEAR == 2003)
d03[,10:142] <- d03[,10:142] * cpi$Rate[cpi$Year == 2003]
d04 <- filter(data, YEAR == 2004)
d04[,10:142] <- d04[,10:142] * cpi$Rate[cpi$Year == 2004]
d05 <- filter(data, YEAR == 2005)
d05[,10:142] <- d05[,10:142] * cpi$Rate[cpi$Year == 2005]
d06 <- filter(data, YEAR == 2006)
d06[,10:142] <- d06[,10:142] * cpi$Rate[cpi$Year == 2006]
d07 <- filter(data, YEAR == 2007)
d07[,10:142] <- d07[,10:142] * cpi$Rate[cpi$Year == 2007]
d08 <- filter(data, YEAR == 2008)
d08[,10:142] <- d08[,10:142] * cpi$Rate[cpi$Year == 2008]
d09 <- filter(data, YEAR == 2009)
d09[,10:142] <- d09[,10:142] * cpi$Rate[cpi$Year == 2009]
d10 <- filter(data, YEAR == 2010)
d10[,10:142] <- d10[,10:142] * cpi$Rate[cpi$Year == 2010]
d11 <- filter(data, YEAR == 2011)
d11[,10:142] <- d11[,10:142] * cpi$Rate[cpi$Year == 2011]
d12 <- filter(data, YEAR == 2012)
d12[,10:142] <- d12[,10:142] * cpi$Rate[cpi$Year == 2012]
d13 <- filter(data, YEAR == 2013)
d13[,10:142] <- d13[,10:142] * cpi$Rate[cpi$Year == 2013]

data <- rbind(d95, d96, d97, d98, d99, d00, d01, d02, d03, d04, d05,
              d06, d07, d08, d09, d10, d11, d12, d13)

# Adjust to per pupil basis
data[,10:142] <- data[,10:142]/data$V33 # Per Pupil basis
data <- arrange(data, LEAID, YEAR)

data.home <- data
nameslist <- colnames(data.home)

rm(d95, d96, d97, d98, d99, d00, d01, d02, d03, d04, d05,
   d06, d07, d08, d09, d10, d11, d12, d13)

# Delt Data Manipulation Start -------------------------------------
# data <- data.home

for(i in 1:134) {
  data[,(i + 142)] <- as.numeric(Delt(data[,(i + 8)]))
}

data <- select(data, 1:8, 143:276)
colnames(data) <- nameslist


year8 <- filter(data, YEAR == 1995)
year8[,9:142] <- NA

data <- filter(data, YEAR != 1995)

data <- data %>% rbind(year8)
data <- data %>%
  group_by(LEAID) %>%
  mutate(m_enr = mean(ENR, na.rm = TRUE),
         m_texp = mean(TOTALEXP, na.rm = TRUE),
         m_trev = mean(TOTALREV, na.rm = TRUE))

# Might not be needed
dataorg <- data %>% 
  ungroup() %>%
  select(m_enr, m_trev, m_texp)

# Percent of Total ----------------------------------------
# Two ratios - interst to total revenue, and debt to total revenue
# _19H - Long debt beginning of year
# _21F - Long debt issued
# _31F - Long debt retired
# _41F - Long debt end of year
# _61F - Short debt beginning
# _66V - Short debt end

# df is for percent
# data is for delta
# data.home is total
rm(df)
df <- data.home

df <- mutate(df, INTCOV = I86/TOTALREV, DEBTREV = X_41F/TOTALREV)

df <- df %>% group_by(LEAID) %>%
  mutate(m_int = mean(INTCOV, na.rm = TRUE),
         m_v33 = mean(V33, na.rm = TRUE),
         m_debt = mean(DEBTREV, na.rm = TRUE))

df <- ungroup(df)
df <- cbind(df, dataorg)

# Percentage of Total Revenue and Total Expenditure
df[,11:58] <- df[,11:58]/df$TOTALREV
df[,140:142] <- df[,140:142]/df$TOTALREV
df[,60:139] <- df[,60:139]/df$TOTALEXP



# This plot shows interest coverage by changes in enrollment
ggplot(filter(df, YEAR == 2010), aes(y=m_enr, x = m_int)) +
  geom_point() +
  theme_tufte() +
  labs(x = "Interest Coverage",
       y = "Enrollment Change",
       title = "Interest Coverage Ratio (Int/Rev)") +
  geom_hline(yintercept = 0, col = "grey") +
  geom_text_repel(
    data = subset(filter(df, YEAR == 2013), 
                  m_int > .09 | m_enr > .09), 
    aes(x = m_int, y = m_enr, label = NAME),
    size = 3)

ggsave("/Users/lualt/OneDrive/Work/CRPE/Enrollment/m_int.png",
       width = 10, height = 5)


# This is debt to revenue ratio
ggplot(filter(df, YEAR == 2010), aes(y=m_enr, x = m_debt)) +
  geom_point() +
  theme_tufte() +
  labs(x = "Debt Ratio, End of Year",
       y = "Enrollment Change",
       title = "Debt to Revenue Ratio (Debt outstanding/Rev)") +
  geom_hline(yintercept = 0, col = "grey")

ggsave("/Users/lualt/OneDrive/Work/CRPE/Enrollment/m_debt.png",
       width = 10, height = 5)



# Percentage of overall  -------------------------

df.revenue <- ddply(df, c("YEAR"), summarise,
                 TFEDREV = mean(TFEDREV, na.rm = TRUE),
                 TSTREV = mean(TSTREV, na.rm = TRUE),
                 TLOCREV = mean(TLOCREV, na.rm = TRUE),
                 ARRA = mean(HR1 + HE1 + HE2, na.rm = TRUE))

df.revenue[,2:5] <- df.revenue[,2:5] * 100
df.revenue # Shows average percent of all expenditures
colMeans(df.revenue, na.rm = TRUE)

d.pupil <- gather(d.pupil, key, value, 2:4)

ggplot(df.revenue, aes(x = YEAR, y = value, group = key)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_tufte(base_size = 15) +
  theme(axis.title = element_blank()) +
  scale_x_continuous(breaks = d.pupil$YEAR, 
                     label = d.pupil$YEAR) +
  annotate("text", x = 1995, y = c(800, 3750, 4600), family = "serif", 
           label = c("Federal", "Local", "State")) +
  annotate("text", x = 1995, y = 6000, family = "serif", 
           label = "Per Pupil\nrevenue in\ninflation-adjusted dollars", hjust = 0)

###### Try to figure out how to do ggrepel for labels at the end

df.mean <- ddply(df, c("YEAR"), summarise,
              TCURINST = mean(TCURINST, na.rm = TRUE),
              TCURSSVC = mean(TCURSSVC, na.rm = TRUE),
              TCUROTH = mean(TCUROTH, na.rm = TRUE),
              TNONELSE = mean(TNONELSE, na.rm = TRUE),
              TCAPOUT = mean(TCAPOUT, na.rm = TRUE),
              L12 = mean(L12, na.rm = TRUE),
              M12 = mean(M12, na.rm = TRUE),
              Q11 = mean(Q11, na.rm = TRUE),
              I86 = mean(I86, na.rm = TRUE),
              V91 = mean(V91, na.rm = TRUE),
              V92 = mean(V92, na.rm = TRUE))

df.mean[,2:11] <- df.mean[,2:11] * 100
df.mean # Shows average percent of all expenditures
colMeans(df.mean)

df.median <- ddply(df, c("YEAR"), summarise,
                 TCURINST = median(TCURINST, na.rm = TRUE),
                 TCURSSVC = median(TCURSSVC, na.rm = TRUE),
                 TCUROTH = median(TCUROTH, na.rm = TRUE),
                 TNONELSE = median(TNONELSE, na.rm = TRUE),
                 TCAPOUT = median(TCAPOUT, na.rm = TRUE),
                 L12 = median(L12, na.rm = TRUE),
                 M12 = median(M12, na.rm = TRUE),
                 Q11 = median(Q11, na.rm = TRUE),
                 I86 = median(I86, na.rm = TRUE),
                 V91 = median(V91, na.rm = TRUE),
                 V92 = median(V92, na.rm = TRUE))

df.median[,2:11] <- df.median[,2:11] * 100
df.median # this shows percent of expenditures in each category (table)
colMeans(df.median) # average of all years 


# Graph and Analysis ---------------------------------------------------

# This graph describes the quandrants

ggplot(data, aes(x = 0, y = 0)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "grey") +
  geom_vline(xintercept = 0, col = "grey") +
  theme_tufte() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = "Change in PP Expenditure or Revenue", 
       y = "Delta Enrollment",
       title = "Quadrant Descriptions (Per Pupil = PP)") +
  lims(x = c(-.3, .3), y = c(-.5, .5)) +
  annotate("text", x = .13, y = .3, 
           label = "Quadrant 1 \n Increase in PP Expenditure/Revenue \n Increase in Enrollment") +
  annotate("text", x = -.13, y = .3, 
           label = "Quadrant 2 \n Decrease in PP Expenditure/Revenue \n Increase in Enrollment") + 
  annotate("text", x = -.13, y = -.3, 
           label = "Quadrant 3 \n Decrease in PP Expenditure/Revenue \n Decrease in Enrollment") +
  annotate("text", x = .13, y = -.3, 
           label = "Quadrant 4 \n Increase in PP Expenditure/Revenue \n Decrease in Enrollment")

ggsave("/Users/lualt/OneDrive/Work/CRPE/Enrollment/quad.png",
       width = 8, height = 4)

# Descriptive Statistics ------------------------------------------

# Takes the average over 5 years of data
# Need a for loop to do this
test <- data
data <- test

data.avg <- data %>% 
  group_by(LEAID) %>% 
  mutate(m_enr = mean(ENR, na.rm = TRUE), 
         m_trev = mean(TOTALREV, na.rm = TRUE),
         m_frev = mean(TFEDREV, na.rm = TRUE), 
         m_srev = mean(TSTREV, na.rm = TRUE),
         m_lrev = mean(TLOCREV, na.rm = TRUE), 
         m_texp = mean(TOTALEXP, na.rm = TRUE),
         m_iexp = mean(TCURINST, na.rm = TRUE), 
         m_cexp = mean(TCURELSC, na.rm = TRUE),
         m_Suexp = mean(TCURSSVC, na.rm = TRUE), 
         m_rexp = mean(TCUROTH, na.rm = TRUE),
         m_nexp = mean(TNONELSE, na.rm = TRUE), 
         m_oexp = mean(TCAPOUT, na.rm = TRUE),
         diff = m_trev - m_texp, na.rm = TRUE) %>%
  select(LEAID, NAME, STABBR, YEAR, V33, 143:155) %>%
  filter(YEAR == 2013) # 2013

data.avg <- data %>% 
  group_by(LEAID) %>% 
  mutate(m_enr = median(ENR, na.rm = TRUE), 
         m_trev = median(TOTALREV, na.rm = TRUE),
         m_frev = median(TFEDREV, na.rm = TRUE), 
         m_srev = median(TSTREV, na.rm = TRUE),
         m_lrev = median(TLOCREV, na.rm = TRUE), 
         m_texp = median(TOTALEXP, na.rm = TRUE),
         m_iexp = median(TCURINST, na.rm = TRUE), 
         m_cexp = median(TCURELSC, na.rm = TRUE),
         m_Suexp = median(TCURSSVC, na.rm = TRUE), 
         m_rexp = median(TCUROTH, na.rm = TRUE),
         m_nexp = median(TNONELSE, na.rm = TRUE), 
         m_oexp = median(TCAPOUT, na.rm = TRUE),
         diff = m_trev - m_texp, na.rm = TRUE) %>%
  select(LEAID, NAME, STABBR, YEAR, V33, 143:155) %>%
  filter(YEAR == 2013) # 2013


ggplot(data.avg, aes(x = m_texp, y = m_enr)) +
  geom_point() + 
  geom_hline(yintercept = 0, col = "grey") +
  geom_vline(xintercept = 0, col = "grey") +
  labs(x = "Change in Per Pupil Expenditure", 
       y = "Delta Enrollment",
       title = "Expenditure") +
  theme_tufte() +
  lims(x = c(-.11,.11),
       y = c(-1,1))

ggsave("/Users/lualt/OneDrive/Work/CRPE/Enrollment/ppexp.png",
       width = 8, height = 4)

ggplot(data.avg, aes(x = m_trev, y = m_enr)) +
  geom_point() + 
  geom_hline(yintercept = 0, col = "grey") +
  geom_vline(xintercept = 0, col = "grey") +
  labs(x = "Change in Per Pupil Revenue", 
       y = "Delta Enrollment",
       title = "Revenue") +
  theme_tufte() +
  lims(x = c(-.11,.11)) 

ggsave("/Users/lualt/OneDrive/Work/CRPE/Enrollment/pprev.png",
       width = 8, height = 4)

# filter and look at expenses in each quadrant
glimpse(data.avg)

# -------------------------------------------------
# declining enrollment and per pupil revenue (bad)
ggplot(filter(data.avg, m_enr < 0 & m_trev < 0), aes(x = m_texp, y = m_enr)) +
  geom_point() + 
  geom_hline(yintercept = 0, col = "grey") +
  geom_vline(xintercept = 0, col = "grey") +
  labs(x = "Change in Per Pupil Expenditure", 
       y = "Delta Enrollment",
       title = "Declining enrollment and per pupil revenue (bad)") +
  theme_tufte() +
  lims(x = c(-.11,.11),
       y = c(-.11,.11))

# increasing enrollment and per pupil revenue (happy)
ggplot(filter(data.avg, m_enr > 0 & m_trev > 0), aes(x = m_texp, y = m_enr)) +
  geom_point() + 
  geom_hline(yintercept = 0, col = "grey") +
  geom_vline(xintercept = 0, col = "grey") +
  labs(x = "Change in Per Pupil Expenditure", 
       y = "Delta Enrollment",
       title = "increasing enrollment and per pupil revenue") +
  theme_tufte() +
  lims(x = c(-.11,.11),
       y = c(-.11,.11))

# decreasing enrollment and increasing per pupil revenue (reasonable)
ggplot(filter(data.avg, m_enr < 0 & m_trev > 0), aes(x = m_texp, y = m_enr)) +
  geom_point() + 
  geom_hline(yintercept = 0, col = "grey") +
  geom_vline(xintercept = 0, col = "grey") +
  labs(x = "Change in Per Pupil Expenditure", 
       y = "Delta Enrollment",
       title = "ecreasing enrollment and increasing per pupil revenue (reasonable)") +
  theme_tufte() +
  lims(x = c(-.11,.11),
       y = c(-.11,.11))

# increasing enrollment and decreasing per pupil revenue (reasonable)
ggplot(filter(data.avg, m_enr > 0 & m_trev < 0), aes(x = m_texp, y = m_enr)) +
  geom_point() + 
  geom_hline(yintercept = 0, col = "grey") +
  geom_vline(xintercept = 0, col = "grey") +
  labs(x = "Change in Per Pupil Expenditure", 
       y = "Delta Enrollment",
       title = "increasing enrollment and decreasing per pupil revenue (reasonable)") +
  theme_tufte() +
  lims(x = c(-.11,.11),
       y = c(-.11,.11))



# data.home work--------------------------------------------------------
d.pupil <- data.home

d.pupil <- ddply(data.home, c("YEAR"), summarise,
                 frev = mean(TFEDREV, na.rm = TRUE),
                 srev = mean(TSTREV, na.rm = TRUE),
                 lrev = mean(TLOCREV, na.rm = TRUE))

d.pupil <- gather(d.pupil, key, value, 2:4)

ggplot(d.pupil, aes(x = YEAR, y = value, group = key)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_tufte(base_size = 15) +
  theme(axis.title = element_blank()) +
  scale_x_continuous(breaks = d.pupil$YEAR, 
                     label = d.pupil$YEAR) +
  annotate("text", x = 1995, y = c(800, 3750, 4600), family = "serif", 
           label = c("Federal", "Local", "State")) +
  annotate("text", x = 1995, y = 6000, family = "serif", 
           label = "Per Pupil\nrevenue in\ninflation-adjusted dollars", hjust = 0)

ggsave("/Users/lualt/OneDrive/Work/CRPE/Enrollment/picture.png",
       width = 8, height = 4)

# Falling Schools -------------------------------
# data - contains the delta change

fallingschools <- filter(data, m_enr <= -.01) # All schools with 1% enrollment decrease average
listSchools <- unique(fallingschools$LEAID) 

# data is delt
# data.home is per pupil totals
# df is percentage

# 2 is percentage. # 1 is absolute
data.1 <- filter(data.home, LEAID %in% listSchools)
data.1 <- filter(data.home, !(LEAID %in% listSchools))
data.2 <- filter(df, !(LEAID %in% listSchools))
data.2 <- filter(df, LEAID %in% listSchools)
data.3 <- filter(data, LEAID %in% listSchools)
data.3 <- filter(data, !(LEAID %in% listSchools))


df.2 <- ddply(data.2, c("YEAR"), summarise,
              Instruction = mean(TCURINST, na.rm = TRUE), 
              Support = mean(TCURSSVC, na.rm = TRUE), 
              Other = mean(TCUROTH, na.rm = TRUE),
              NonSchool = mean(TNONELSE, na.rm = TRUE), 
              CapitalOutlay = mean(TCAPOUT, na.rm = TRUE))
df.2

df.2[,2:6] <- df.2[,2:6] * 100
df.2 <- gather(df.2, key, value, 2:6)

ggplot(df.2, aes(x= YEAR, y = value, group = key, col = key)) +
  scale_color_brewer(palette = "Set1") +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_tufte(base_size = 15) +
  labs(title = "Falling Schools - instruction falls, capital grows") +
  theme(axis.title = element_blank()) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  annotate("text", x = 1995, y = 20, family = "serif", 
           label = "Percentage of\ntotal expenditures\nin other schools", 
           hjust = 0) +
  annotate("text", x = c(1995,2013, 1995, 2013, 2011, 2011), y = c(50, 44, 8, 10, 42, 13), 
           family = "serif", label = c("53", "45", "5.5", "8", "44", "10.5"))




# Debt And Interests ---------------------------------------------------------
df.3 <- ddply(data.home, c("YEAR"), summarise,
              InterestCoverage = mean(INTCOV, na.rm = TRUE), 
              DebtRevenueRatio = mean(DEBTREV, na.rm = TRUE),
              enrollment = mean(V33, na.rm = TRUE))

df.3 <- ddply(data.home, c("YEAR"), summarise,
              InterestCoverage = mean(I86, na.rm = TRUE), 
              DebtRevenueRatio = mean(X_41F, na.rm = TRUE),
              enrollment = mean(V33, na.rm = TRUE))

# Stable
data.2 <- filter(df, !(LEAID %in% listSchools))
df.3 <- ddply(data.2, c("YEAR"), summarise,
              InterestCoverage = mean(INTCOV, na.rm = TRUE), 
              DebtRevenueRatio = mean(DEBTREV, na.rm = TRUE))
# falling
data.2 <- filter(df, LEAID %in% listSchools)
df.4 <- ddply(data.2, c("YEAR"), summarise,
              InterestCoverage = mean(INTCOV, na.rm = TRUE), 
              DebtRevenueRatio = mean(DEBTREV, na.rm = TRUE))
cbind(df.3, df.4)
data.2 <- filter(df, LEAID %in% listSchools)

df.3 <- gather(df.3, key, value, 2:3)

ggplot(df.3, aes(x= YEAR, y = value, group = key, col = key)) +
  scale_color_brewer(palette = "Set1") +
  geom_line(size = 1) +
  geom_point(size = 2)


# Going to make a graph for falling schools that shows the percentage of 
# expenditures types as a total expendiutre




# nrow(data[data$LEAID == 5509600,]) #count rows for certain classification 
# 5509600
mil <- filter(df, LEAID == 5509600)

mil <- select(mil, LEAID, YEAR, V33, TCURINST, 
              TCURSSVC, TCUROTH, TNONELSE, TCAPOUT)
names(mil) <- c("LEAID", "YEAR", "V33", "Instruction", "Support", "Other",
                "NonSchool", "Capital")


mil[,4:8] <- mil[,4:8] * 100
mil <- gather(mil, key, value, 4:8)

ggplot(mil, aes(x= YEAR, y = value, group = key, col = key)) +
  scale_color_brewer(palette = "Set1") +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_tufte(base_size = 15) +
  labs(title = "Milwaukee sees large instructional decreases") +
  theme(axis.title = element_blank()) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  annotate("text", x = 1995, y = 20, family = "serif", 
           label = "Percentage of\ntotal expenditures\nin Milwaukee District", 
           hjust = 0)


