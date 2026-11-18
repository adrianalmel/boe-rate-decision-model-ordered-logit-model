

library(boe)
library(tidyverse)
library(lubridate)


# 0. Bank Rate

bank_rate <- boe_get("IUDBEDR", from = "1997-01-01")

ggplot(bank_rate, aes(x = date, y = value)) +
  geom_line()


##### 1. Ddependent variable#####

# boe_mpc_votes() has one row per meeting per member, so distinct dates give
# every meeting, holds included. boe_mpc_decisions() lists rate changes only.
meetings <- boe_mpc_votes() %>%
  distinct(meeting_date = date) #this gives the meeting dates since the start (1997) 

decisions <- boe_mpc_decisions() %>%
  select(meeting_date = date, direction) #this gives us interest rate hikes and cuts

spine <- meetings %>%
  full_join(decisions, by = "meeting_date") %>%   # catches emergency moves missing from votes data
  mutate(
    decision = case_when(
      direction == "cut"  ~ "cut",
      direction == "hike" ~ "hike",
      TRUE                ~ "hold"
    ),
    decision = factor(decision, levels = c("cut", "hold", "hike"), ordered = TRUE)
  ) %>%
  select(-direction) %>%
  arrange(meeting_date) #now whenever interest rates stay constant this is an interest rate HOLD

# boe_mpc_decisions() misses the first MPC move (6 Jun 1997: 6.25% -> 6.50%)
spine <- spine %>%
  mutate(
    decision = if_else(meeting_date == as.Date("1997-06-06"), "hike", as.character(decision)),
    decision = factor(decision, levels = c("cut", "hold", "hike"), ordered = TRUE)
  )

count(spine, decision)
# Check for near-duplicate dates (same meeting recorded a day apart):
spine %>% filter(as.numeric(meeting_date - lag(meeting_date)) < 7)


##### 2. Downloading an ONS time series as a tidy data frame#####

ons_get <- function(uri, freq = c("month", "quarter")) {
  freq <- match.arg(freq)
  url  <- paste0("https://www.ons.gov.uk/generator?format=csv&uri=", uri)
  raw  <- read_csv(url, col_names = c("period", "value"),
                   col_types = "cc", show_col_types = FALSE)
  message("Series: ", raw$value[1])   # prints the series title so you can verify it
  pattern <- if (freq == "month") "^\\d{4} [A-Z]{3}$" else "^\\d{4} Q[1-4]$"
  raw %>%
    filter(str_detect(period, pattern)) %>%
    mutate(
      date  = if (freq == "month") ym(period) else yq(period),
      value = as.numeric(value)
    ) %>%
    select(date, value)
}

##### 3. Regressors — each gets an approximate `release` date (when the MPC could first have seen it), not just the reference period#####


# CPI 12-month rate: month m published ~20th of month m+1
cpi <- ons_get("/economy/inflationandpriceindices/timeseries/d7g7/mm23") %>%
  transmute(release = date %m+% months(1) + days(19), cpi = value) #code tells us when cpi is released, same with other regressors

# RPIX 12-month rate (target measure before Dec 2003) — verify printed title

rpix <- ons_get("/economy/inflationandpriceindices/timeseries/cdkq/mm23") %>%
  transmute(release = date %m+% months(1) + days(19), rpix = value)

# CPI services 12-month rate — verify printed title
services <- ons_get("/economy/inflationandpriceindices/timeseries/d7nn/mm23") %>%
  transmute(release = date %m+% months(1) + days(19), services_cpi = value)

# ILO unemployment rate (rolling 3-month LFS); Jul-Sep published ~mid-Nov
unemp <- ons_get("/employmentandlabourmarket/peoplenotinwork/unemployment/timeseries/mgsx/lms") %>%
  arrange(date) %>%
  transmute(
    release      = date %m+% months(2) + days(14),
    unemp_chg_3m = value - lag(value, 3)
  )

# GDP quarter-on-quarter growth; first estimate ~6 weeks after quarter end
gdp <- ons_get("/economy/grossdomesticproductgdp/timeseries/ihyq/qna", freq = "quarter") %>%
  transmute(release = date %m+% months(4) + days(14), gdp_growth = value)

# 2-year nominal gilt yield (daily, from BoE fitted curve)
gilt2 <- boe_curve_panel(curve = "nominal", frequency = "daily",
                         from = "1997-01-01", maturities = 2) %>%
  select(release = date, gilt_2y = m2)

# Bank Rate, daily, for the market-expectations spread
bank_rate_d <- bank_rate %>%
  select(release = date, bank_rate = value)

# Brent crude (FRED, no API key needed) -> monthly average -> 3-month % change
brent <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=DCOILBRENTEU",
                  na = ".", show_col_types = FALSE) %>%
  set_names(c("date", "brent")) %>%
  drop_na() %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(brent = mean(brent), .groups = "drop") %>%
  mutate(
    oil_chg_3m = 100 * (brent / lag(brent, 3) - 1),
    release    = month %m+% months(1)
  ) %>%
  select(release, oil_chg_3m)

##### 4. Rolling join: latest value released strictly before each meeting and constructing our dataframe #####

roll <- function(x, y) {
  x %>%
    left_join(y, by = join_by(closest(meeting_date > release))) %>%
    select(-release)
} #normally a join needs dates to match exactly, but it doesn't happen here, 
#so this says for each meeting, find the rows of y released strictly before the meeting, 
#and take the closest one, which is the most recent release.

model_df <- spine %>%
  roll(cpi) %>%
  roll(rpix) %>%
  roll(services) %>%
  roll(unemp) %>%
  roll(gdp) %>%
  roll(brent) %>%
  roll(gilt2) %>%
  roll(bank_rate_d) %>%
  mutate(
    # Remit switched from RPIX 2.5% to CPI 2% on 10 Dec 2003
    inf_gap    = if_else(meeting_date < as.Date("2003-12-10"), rpix - 2.5, cpi - 2),
    market_exp = gilt_2y - bank_rate
  )

summary(model_df)   # check NAs — first few meetings will lack 3-month changes

write_csv(model_df, "mpc_model_data.csv")   # saving to working directory as csv


