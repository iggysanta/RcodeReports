# Change this for state/nation level data
url <- "https://download.bls.gov/pub/time.series/sm/sm.data.37.Oklahoma"
# 4348400001 being the supersector.industry code
# this pulls data
statedata <- read.delim(url, header = TRUE)
sd <- statedata

sd <- sd %>% 
  filter(year >= 2018)

sd <- sd[sd$period != "M13", ]

# do not forget to change the series_id
sd1 <- sd %>%
  select(-footnote_codes) %>%
  filter(grepl("SMS40000000000000001", sd$series_id)) %>%
  group_by(year, period) %>%
  summarise(Monthly_Total = sum(value))

# should be left with 42 obs if 0 didn't change series_id
sd1 <- sd1 %>%
  mutate(
    MoM_pct_chg = 100 * ((Monthly_Total - lag(Monthly_Total)) / lag(Monthly_Total)))

# X= 10 States removing bonus UI Utah, Alabama, Georgia, Florida, Tennessee, Iowa, Ohio, Indiana, South Carolina, Oklahoma
# Y= 10 States not removing bonus UI Cali, Oregon, Louisiana, Virginia, Michigan, Washington, Kentucky, New Mexico, Nevada, Minnesota
#y <- vector()
#x <- vector()

#sd1$MoM_pct_chg[42] The last point: June jobs change in pct
#x <-na.omit(x)
x <- append(x,sd1$MoM_pct_chg[42])

shapiro.test(y); shapiro.test(x) #assumption of normality holds.

t.test(x,y) # we can not say that they are different 
# There is not enough evidence to say that, the cutting of the extra UI benefits have any 
x
#> x sample values
#[1] 0.5786527 0.7321446 0.7308052 0.9434948 0.4439671
#[6] 0.7205973 0.2567817 0.5917383 0.5799012 0.2982249
#[11] 0.4671461
y 
#> y sample values for recreatability.
#[1]  0.44973108  0.40573438  0.36568060  0.93935657
#[5]  0.08224107  0.33759168  0.33759168  0.17170145
#[9]  0.28850978  1.16613660 -0.02122842


