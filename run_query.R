# https://nowdata.rcc-acis.org/arx/
# https://www.rcc-acis.org/docs_webservices.html
# https://builder.rcc-acis.org/?
library(httr2)
library(tidyverse)
#
# j <- jsonlite::toJSON(list(
#   elems = list(list(
#     interval = c(1, 0, 0),
#     duration = "std",
#     name = "mint",
#     reduce = list(
#       reduce = "last_le_32",
#       add = "value,mcnt"
#     ),
#     maxmissing = "5",
#     season_start = "08-01"
#     # , smry = c("max", "mean", "min")
#   )),
#   sid = "RSTthr 9",
#   sDate = "1993-07-01",
#   eDate = "2023-07-31"
# ), auto_unbox = TRUE)
# print(j)
#
# req <- request("https://data.rcc-acis.org/StnData") %>%
#   req_url_query(params=j) %>%
#   req_perform()
# frost <- req %>%
#   resp_body_json() %>%
#   "[["("data") %>%
#   map(2) %>%
#   map_dfr(set_names, c("date", "low", "nmiss"))


tmp <- map_dfr(.id = "city", c("Lake City" = "214438 2", "Zumbrota" = "219249 2", "Rochester" = "RSTthr 9"), function(sid) {

  j <- jsonlite::toJSON(list(
    elems = map(c("mint", "maxt", "snow", "pcpn"), function(nm) {
      list(
        interval = c(0, 0, 1),
        duration = "dly",
        name = nm
      )
    }),
    sid = sid, # got this from the selection menu on the NOWData site
    sDate = "por",
    eDate = "por"
  ), auto_unbox = TRUE)
  print(j)

  req <- request("https://data.rcc-acis.org/StnData") %>%
    req_url_query(params=j) %>%
    req_perform()
  dat <- req %>%
    resp_body_json()
  together <- dat$data %>%
    map_dfr(set_names, c("date", "min", "max", "snowfall", "precipitation")) %>%
    mutate(
      across(c(max, min), ~ as.numeric(replace(.x, .x == "M", NA))),
      across(c(snowfall, precipitation), function(x) {
        x %>%
          replace(x %in% c("S", "M"), NA_real_) %>%
          replace(x == "T", 0) %>%
          sub(pattern = "A", replacement = "") %>%
          as.numeric()
      }),
      date = as.Date(date)
    )
}) %>%
  pivot_longer(c(min, max, snowfall, precipitation), values_to = "value", names_to = "measure")

together <- tmp %>%
  arrange(measure, desc(date), city == "Rochester", city == "Zumbrota") %>%
  filter(!is.na(value)) %>%
  distinct(measure, date, .keep_all = TRUE) %>%
  complete(measure, date = full_seq(date, 1))



together %>%
  filter(date >= "1993-01-01", measure %in% c("max", "min")) %>%
  ggplot(aes(x = date, y = value, color = measure)) +
  geom_line() +
  scale_color_manual(values = c(max = "red", min = "blue")) +
  ylab("Degrees (F)") +
  xlab("Date")

together %>%
  filter(date >= "1993-01-01", measure == "snowfall") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  ylab("Snowfall (in)") +
  xlab("Date")


together %>%
  filter(date >= "1993-01-01", measure == "precipitation") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  ylab("Precipitation (in)") +
  xlab("Date")

# -------------------------------------------------------------------------


frost2 <- together %>%
  mutate(year = year(date)) %>%
  filter(
    year < 2024,
    date >= "1943-01-01",
    date < paste0(year, "-07-01"),
    measure == "min"
  ) %>%
  group_by(year) %>%
  summarize(
    nmiss = sum(is.na(value)),
    `<= 32` = max(date[value <= 32], na.rm = TRUE),
    `<= 35` = max(date[value <= 35], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_longer(starts_with("<="), names_to = "cutoff", values_to = "date") %>%
  mutate(
    days_in = as.integer(difftime(date, floor_date(date, "year"))),
    md = as.Date("2023-01-01") + days_in
  )
ggplot(frost2, aes(x = year, y = md, color = cutoff)) +
  geom_point() +
  ylab("Last frost date") +
  xlab("Year") +
  geom_smooth(method = "lm", se = FALSE)

LL <- lm(days_in ~ I(year - 2000), data = frost2, subset = cutoff == "<= 32")
conf <- map_dfr(rev(c(0.5, 0.75, 0.9, 0.95)), function(x) {
  predict(LL, data.frame(year = 2024), interval  = "prediction", level = x) %>%
    as.data.frame() %>%
    mutate(alpha = x, year = 2024)
}) %>%
  mutate(
    cutoff = "<= 32",
    upr = as.Date("2023-01-01") + upr,
    lwr = as.Date("2023-01-01") + lwr
  )
ggplot(filter(frost2, cutoff == "<= 32"), aes(x = year, y = md)) +
  geom_point() +
  ylab("Last frost date") +
  xlab("Year") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_rect(
    data = conf,
    aes(ymax = upr, ymin = lwr, fill = factor(alpha), xmin = year - 0.5, xmax = year + 0.5),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = c("#111111", "#555555", "#999999", "#dddddd"))

