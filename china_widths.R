library(readxl)
library(tidyverse)
library(dplR)
library(readr)
library(zoo)

#import data
china_widths <- read_excel("tree ring width.xlsx", sheet = 1, col_types = rep("numeric", 2)) %>% 
  filter(!is.na(width))

#california widths
#https://www.ncdc.noaa.gov/paleo/study/3254 
cali <- read_fwf(
  file = "ca506.crn", 
  fwf_widths(
    widths = c(5, 5, rep(c(4, 3), 10)), 
    col_names = c("series", "decade", paste0(rep(c("w", "n"), 10), rep(0:9, each = 2)))
  ), 
  skip = 3) %>% 
  select(decade, starts_with("w")) %>% 
  gather(key = year, value = width, -decade) %>% 
  mutate(year = as.numeric(gsub("w", "", year))) %>% 
  mutate(year = decade + year) %>% 
  filter(width != 9990, decade %% 10 == 0) %>% 
  arrange(year)

ggplot(cali, aes(x = year, y = width)) + geom_line()

ggplot(china_widths, aes(x = id, y = width)) + geom_line()

#detrend
china_widths <- china_widths %>%  bind_cols(detrend.series(pull(., width)))


cors <- data_frame(
  year = cali$year[1:(nrow(cali) - nrow(china_widths) + 1)],
  cor = rollapply(cali$width, width = nrow(china_widths), FUN = function(x){cor(x, china_widths$Spline, method = "spearman")}, align = "left")
)

ggplot(cors, aes(x = year, y = cor)) + 
  geom_line() +
  geom_vline(xintercept = c(-3400, -3300), colour = "red")


ggplot(cors, aes(x = year, y = cor)) + 
  geom_line() + 
  xlim(-3400, -3300)

