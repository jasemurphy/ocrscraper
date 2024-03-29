library(rvest)
library(tidyverse)


today <- format(Sys.Date(),"%y%m%d")

url <- paste0("https://www2.asx.com.au/data/data/markets/futures/reports/EODWebMarketSummary",today, "SFT.htm")

IBF <- read_html(url)

IBFtablelist <- IBF %>% 
  html_elements("table") %>% 
  html_table() 



IBFtib <- IBFtablelist[2] %>% 
 tibble() %>% 
  unnest(cols = c(.)) %>%
 mutate(scrape_date = Sys.Date())

new_data <- IBFtib %>% 
  mutate(check = if_else(str_detect(X1, "0 Day Interbank Cash Rate") == TRUE, "1",NA_character_), 
         end = if_else(str_detect(X1, "90-Day Bank Bills") == TRUE, "1",NA_character_)) %>%
  fill(check, .direction = "down") %>% 
  fill(end, .direction = "up") %>% 
  mutate(end = as.numeric(end)) %>% 
  mutate(check = as.numeric(check)) %>% 
  mutate(check2 = end *check) %>% 
  filter(check2 == 1) %>% 
select(X1, X6) %>% 
  mutate(check  = if_else(str_detect(X1, "202") == TRUE, 1,0)) %>% 
  filter(check == 1) %>% 
  mutate(cash_rate = as.numeric(X6)) %>% 
  mutate(cash_rate = 100- cash_rate) %>% 
  mutate(date = lubridate::my(X1)) %>% 
  select(-check, -X1,-X6) %>% 
 mutate(scrape_date = Sys.Date())%>%
select(date, everything())


# Write a CSV of today's rates data
write_csv(new_data, file.path("daily_data",
                              paste0("scraped_cash_rate_", Sys.Date(), ".csv")))

#write csv of all futures data
write_csv(IBFtib, file.path("daily_futures_data",
                              paste0("All_futures", Sys.Date(), ".csv")))

# Load all existing rates data, combine with latest data
all_data <- file.path("daily_data") |>
  list.files(pattern = ".csv",
             full.names = TRUE) |>
  read_csv(col_types = "DdD")

saveRDS(all_data,
        file = file.path("combined_data",
                         "all_data.Rds"))
