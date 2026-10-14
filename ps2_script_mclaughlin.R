#3.1
library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

#4.1
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"    ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )

#4.2 
#filling in oct 2025
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

#adding unemployment rate change column
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

#unemp recent
unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

#summarizations
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_recent_summary_ <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

#4.3
unemp_top_increases <- unemp |>
  group_by(region) |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases <- unemp |>
  group_by(region) |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)

#4.4
unemp_top_increases_no_covid <- unemp |>
  filter(year(date) !=2020) |>
  group_by(region) |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases_no_covid <- unemp |>
  filter(year(date) !=2020) |>
  group_by(region) |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)

#5.1
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(title = "Unemployment Over Time by Region", x = "Date", y = "Unemployment Rate") +
  theme_minimal()

#5.2
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~region) +
  labs(title = "Unemployment Over Time by Region", x = "Date", y = "Unemployment Rate") +
  theme_minimal()

#5.3
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate, .desc = TRUE),
                          y = mean_rate,
                          fill = region)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Mean Unemployment by Region", x = "Region", y = "Mean Unemployment Rate") +
  theme_minimal()

#5.4
ggplot(unemp_recent, aes(x = date, y = unemployment_rate_change, color = region)) +
  geom_line() +
  labs(title = "Change in Unemployment Over Time by Region", x = "Date", y = "Change in Unemployment Rate") +
  theme_minimal()

#i like this graph because it puts into perspective the scale of impact on 
#employment. its kind of funny how the 5 years before and after 2020 look
#basically flat compared to the spike. it also kind of looks like a heartbeat. 



