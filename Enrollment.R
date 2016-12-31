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

# Manipulating Data --------------------------------------------------
# Data is for change, df is for percent of total

data <- data %>% 
  filter(V33 >= 1) %>% 
  arrange(LEAID, YEAR)

data$NAME <- as.character(data$NAME)
data$STNAME <- as.character(data$STNAME)
data$STABBR <- as.character(data$STABBR)
data <- filter(data, V33 > 10)
data[data <= 0] <- NA
data[data == -2] <- NA

data <- select(data, -FIPST, -SCHLEV, -FIPSCO, -CMSA, -AGCHRT, -CONUM, -CSA, -CBSA)

data <- mutate(data, ENR = V33)
data <- data[,c(1:6, 141, 7, 142, 8:140)]

data[,10:142] <- data[,10:142]/data$V33 # Per Pupil basis
data <- arrange(data, LEAID, YEAR)

data.home <- data
nameslist <- colnames(data.home)


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

data <- data %>% rbind(year8) %>%
  group_by(LEAID) %>% 
  mutate(m_enr = mean(V33, na.rm = TRUE), 
         m_trev = mean(TOTALREV, na.rm = TRUE),
         m_texp = mean(TOTALEXP, na.rm = TRUE)) %>%
  arrange(LEAID, YEAR)

dataorg <- data %>% 
  ungroup() %>%
  select(m_enr, m_trev, m_texp)


# Note Dec 30th
# I think it works up to here. Will have to check whether the data was cleaned correctly

# Percent of Total ----------------------------------------
# Two ratios - interst to total revenue, and debt to total revenue
# _19H - Long debt beginning of year
# _21F - Long debt issued
# _31F - Long debt retired
# _41F - Long debt end of year
# _61F - Short debt beginning
# _66V - Short debt end

df <- data.home

df <- mutate(df, INTCOV = I86/TOTALREV, DEBTREV = X_41F/TOTALREV)

df <- df %>% group_by(LEAID) %>%
  mutate(m_int = mean(INTCOV, na.rm = TRUE),
         m_v33 = mean(V33, na.rm = TRUE),
         m_debt = mean(DEBTREV, na.rm = TRUE))

df <- ungroup(df)
  
df <-  cbind(df, dataorg) 

df[,14:137] <- df[,14:137]/df$TOTALEXP

head(arrange(df, desc(m_int)))


# This plot shows interest coverage by changes in enrollment
ggplot(filter(df, YEAR == 13), aes(y=m_enr, x = m_int)) +
  geom_point() +
  geom_point(data = subset(filter(df, YEAR == 13 & STABBR == "TX")), 
             aes(x = m_int, y = m_enr), col = "red") +
  theme_tufte() +
  labs(x = "Interest Coverage",
       y = "Enrollment Change") +
  geom_hline(yintercept = 0, col = "grey") +
  annotate("text", x = .14, y = -.14, label = "Red is Texas") +
  geom_text_repel(
    data = subset(filter(df, YEAR == 13), 
                  m_int > .1 | m_enr < -.1), 
    aes(x = m_int, y = m_enr, label = NAME),
    size = 3)

ggsave("/Users/lualt/OneDrive/Work/CRPE/Enrollment/m_int.png",
       width = 10, height = 5)


# This is debt to revenue ratio
ggplot(filter(df, YEAR == 13), aes(y=m_enr, x = m_debt)) +
  geom_point() +
  theme_tufte() +
  labs(x = "Debt Ratio, End of Year",
       y = "Enrollment Change") +
  geom_hline(yintercept = 0, col = "grey")


# Percentage of overall  -------------------------

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

# Mean is larger in all categories after TCUROTH
# Slightly lower in instruction
# Mean may have larger outliers in other categories


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

data.avg <- data %>% group_by(LEAID) %>% 
  mutate(m_enr = mean(V33, na.rm = TRUE), 
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
  select(LEAID, NAME, STABBR, YEAR, V33, 138:150) %>%
  filter(YEAR == 2013)


# Big enrollment change, changes where expenditure change greater than revene
count(filter(data.avg, m_enr > 0 & m_texp < 0)) # enrollment growth, ppexp decline, 179
count(filter(data.avg, m_enr > 0 & m_texp > 0)) # Enrollment growth, ppexp growth, 120
count(filter(data.avg, m_enr < 0 & m_texp > 0)) # enrollment decline, ppexp growth, 94
count(filter(data.avg, m_enr < 0 & m_texp < 0)) # enrollment decline, ppexp decline, 105

count(filter(data.avg, m_enr > 0 & m_trev < 0)) # enrollment growth, pprev decline, 152
count(filter(data.avg, m_enr > 0 & m_trev > 0)) # Enrollment growth, pprev growth, 147
count(filter(data.avg, m_enr < 0 & m_trev > 0)) # enrollment decline, pprev growth, 99
count(filter(data.avg, m_enr < 0 & m_trev < 0)) # enrollment decline, pprev decline, 100

ggplot(data, aes(x = m_texp, y = m_enr)) +
  geom_point() + 
  geom_point(aes(x = m_trev, y = m_enr)) +
  geom_hline(yintercept = 0, col = "grey") +
  geom_vline(xintercept = 0, col = "grey") +
  labs(x = "Change in Per Pupil Expenditure", 
       y = "Delta Enrollment",
       title = "Expenditure") +
  theme_tufte() +
  lims(x = c(-.11,.11)) +
  annotate("text", x = -.09, y = .1, label = "179") +
  annotate("text", x = .09, y = .1, label = "120") +
  annotate("text", x = .09, y = -.1, label = "94") +
  annotate("text", x = -.09, y = -.1, label = "105")

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
  lims(x = c(-.11,.11)) +
  annotate("text", x = -.09, y = .1, label = "152") +
  annotate("text", x = .09, y = .1, label = "147") +
  annotate("text", x = .09, y = -.1, label = "99") +
  annotate("text", x = -.09, y = -.1, label = "100")

ggsave("/Users/lualt/OneDrive/Work/CRPE/Enrollment/pprev.png",
       width = 8, height = 4)

# filter and look at expenses in each quadrant
glimpse(data.avg)

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


# Percentages of the Changes in expenditure -------------------------------
# data - contains the delta change





























# Model ------------------------------------------------------------

lm <- lmer(TEXP ~ ENR + (1|LEAID), data = data)
summary(lm)
arm::display(lm)

fit.res <- resid(lm)
plot(fit.res)
abline(0,0)
plot(density(fit.res)) #A density plot
qqnorm(fit.res) # A quantile normal plot - good for checking normality
qqline(fit.res)



# Next steps
# Do it by 5 year gap. From 2007-08 to 2012-13
# Simplify graph to only single average LEAID

# Start here, create random data to show the distribution of ideal scenario
a = rnorm(50, sd = .05)
b = rnorm(50, sd = 5)

data.test <- data.frame(a, b)

ggplot(data.test, aes(x = a, y = b)) +
  geom_point() +
  geom_point(aes(x = 5, y = 5), col = "red") +
  xlim(-10, 10) +
  geom_hline(yintercept = 0, col = "grey") +
  geom_vline(xintercept = 0, col = "grey") +
  theme_tufte() +
  labs(x = "Change in Per Pupil Revenue/Expenditure", 
       y = "Delta Enrollment",
       title = "Perfect Funding/Expenditure correspondence")

ggsave("/Users/lualt/OneDrive/Work/CRPE/Enrollment/example.png",
       width = 8, height = 4)
  




