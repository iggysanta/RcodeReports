#---------------------------------------------------
# Prep libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
# creates object out of the url to download database
url <- "https://download.bls.gov/pub/time.series/sm/sm.data.1.AllData"
# 4348400001 being the supersector.industry code
# this pulls data
allemp <- read.delim(url, header = TRUE)# this pulls data
trucktran <- allemp

trucktran$value <- as.numeric(trucktran$value) # turns value into number

trucktran <- trucktran %>% # Filters down to years needed
  filter(year >= 1999)

trucktran <- trucktran[trucktran$period != "M13", ]

# removes footnotes, filters down by job, groups by year/month, and creates total of employment.
trucktran1 <- trucktran %>%
  select(-footnote_codes) %>%
  filter(grepl("4348400001", trucktran$series_id)) %>%
  group_by(year, period) %>%
  summarise(Monthly_Total = sum(value))


trucktran1 <- as.data.frame(trucktran1) # sets data to data frame to use mutate
trucktran1 <- trucktran1 %>%
  mutate(
    MoM_pct_chg = 100 * ((Monthly_Total - lag(Monthly_Total)) / lag(Monthly_Total)),
    YoY_pct_chg = 100 * ((Monthly_Total - lag(Monthly_Total, 12)) / lag(Monthly_Total, 12)),
    pos = YoY_pct_chg >= 0
  ) # this creates a month/year over month/year percentage change

trucktran1$YM <- paste(trucktran1$year, trucktran1$period, sep= "-") # creates date variable for labling

ggplot(trucktran1, aes(x = YM, y = YoY_pct_chg, fill = pos)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_discrete(name = "Date", breaks= c("2001-M01","2006-M01","2011-M01","2016-M01","2021-M06")) +
  labs(y = "Percent Change", title = "Monthly Change Year over Year") +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))# plots out the YoY monthly changes

par(mfrow=c(1,1))
plot(trucktran1$Monthly_Total, type = "l")
