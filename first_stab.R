library(tidyverse)
library(pdftools)
library(tesseract)

#get pdf as text


d <- pdf_ocr_text("https://www.asx.com.au/data/trt/ib_expectation_curve_graph.pdf", dpi = 2200)

datebase <- unlist(d %>% 
         str_split("\n")) %>% 
  as_tibble() %>% 
  mutate(date = if_else(str_detect(value, "at market close on"), value, NA_character_)) %>% 
  mutate(date = str_remove(date, "ASX As at market close on ")) %>% 
  mutate(date = str_remove_all(date, "[°\"\\))]")) %>% 
  fill(date,.direction = "down") %>% 
  mutate(date = as.Date(date, format("%d %B %Y"))) %>% 
  select(date) %>% 
  filter(!is.na(date)) %>% 
  unique() %>% 
  pull()

OCR_latest_predictions <- 
  unlist(d %>% 
  str_split("\n")) %>% 
 as_tibble() %>% 
 
  mutate(id = row_number()) %>% 
  mutate(date = if_else(str_detect(value, "at market close on"), value, NA_character_)) %>% 
mutate(date = str_remove(date, "ASX As at market close on ")) %>%  #this line doesn't look too flexible.
  mutate(date = str_remove_all(date, "[°\"\\))]")) %>% 
  mutate(values = str_detect(value, "ield")) %>% 
  fill(date,.direction = "down") %>% 
    filter(id>5) %>%  
    
  filter(values == "TRUE") %>% 
  mutate(value = str_remove_all(value, "[a-zA-Z|''VvY(°)‘\\]]")) %>% 
    mutate(value = str_replace_all(value, "   ", " ")) %>% 
  mutate(value = str_replace_all(value, "  ", " ")) %>% 
    mutate(value = str_split(value, " ")) %>% 
   
    unnest(value) %>% 
      mutate(value = as.numeric(value))%>% 
    filter(!is.na(value)) %>% 
    select(date, value) %>% 
    mutate(date = as.Date(date, format("%d %B %Y"))) %>% 
    #mutate(predictiondate = seq(as.Date(datebase), by = "month", length = 18)) %>% 
    mutate(scrapedate =  Sys.time())
    
    
    
# Write a CSV of today's data
write_csv(OCR_latest_predictions, file.path("daily_data",
                              paste0("scraped_cash_rate_", Sys.Date(), ".csv")))

# Load all existing data, combine with latest data
all_data <- file.path("daily_data") |>
  list.files(pattern = ".csv",
             full.names = TRUE) |>
  read_csv(col_types = "DdD")

saveRDS(all_data,
        file = file.path("combined_data",
                         "all_data.Rds"))
