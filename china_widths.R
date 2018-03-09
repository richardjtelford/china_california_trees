#### import packages ####
library("readxl")
library("tidyverse")
library("dplR")
library("readr")
library("zoo")
library("magrittr")

#### import data ####
#Chinese ring widths
china_widths <- read_excel("tree ring width.xlsx", sheet = 1, col_types = rep("numeric", 2)) %>% 
  filter(!is.na(width))

#california widths
#https://www.ncdc.noaa.gov/paleo/study/3254 
cali1 <- read.crn("ca506.crn")

read_crn <- function(file){
  read_fwf(
    file = file, 
    fwf_widths(
      widths = c(5, 5, rep(7, 10)), 
      col_names = c("series", "decade", paste0(rep(c("w n"), 10), 0:9))
    ), 
    skip = 3) %>%   
    mutate(decade = floor(decade/10) * 10) %>% 
    gather(key = year, value = w_n, -decade, -series) %>% 
    mutate(year = as.integer(gsub("w n", "", year))) %>% 
    mutate(year = decade + year) %>% 
    mutate(
      series = as.character(series),
      width = substring(w_n, 1, nchar(w_n) - 3),
      width = as.numeric(trimws(width)),
      n = substring(w_n, nchar(w_n) - 2, nchar(w_n)),
      n = as.integer(trimws(n))
           ) %>% 
    filter(width != 9990) %>% 
    select(-w_n) %>% 
    arrange(year)
}

cali <- read_crn("ca506.crn")#white mountain master
meth_walk <- read_crn("ca535.crn")# METHUSELAH WALK  

bind_rows(cali, meth_walk) %>% 
  ggplot(aes(x = year, y = width, colour = series)) + geom_line()
ggplot(cali, aes(x = year, y = width)) + geom_line()

cali %>% 
  left_join(meth_walk, by = "year", suffix = c(".cali", ".meth")) %>%  
  ggplot(aes(x = width.cali, y = width.meth)) +
  geom_point()

cali %>% 
  left_join(meth_walk, by = "year", suffix = c(".cali", ".meth")) %$%
  cor(width.cali, width.meth, use = "pair")
  
ggplot(cali, aes(x = year, y = n)) + geom_line()

ggplot(china_widths, aes(x = id, y = width)) + geom_line()

#### detrend chinese ring widths ####
china_widths <- china_widths %>%  bind_cols(detrend.series(pull(., width)))

cor(select(china_widths, -id, -width), use = "pair")
n_rings <- nrow(china_widths)

#### Calculate correlations ####

cors <- data_frame(year = cali$year[1:(nrow(cali) - n_rings + 1)]) %>% 
  bind_cols(
    cor = rollapply(
      cali$width,
      width = n_rings,
      FUN = function(y) {
        sapply(select(china_widths, -id, -width),
               function(x) {
                 cor(x, y, method = "pearson", use = "pair")
               })
      },
      align = "left") %>% 
    as_data_frame()
  ) %>% 
  gather(key = method, value = correlation, -year) %>% 
  mutate(t = correlation * sqrt((n_rings - 2) / (1 - correlation ^ 2)))

cors %>% filter(correlation > 0.5)

#### Plot correlations ####

g <- ggplot(cors, aes(x = year, y = correlation, colour = method)) + 
  geom_line() + 
  geom_vline(xintercept = -3388, colour = "orange")

g
g + xlim(-3400, -3300)

g +  aes(y = t)
g +  aes(y = t) + xlim(-3400, -3300)
