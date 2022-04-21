library(tidyverse)
library(pdftools)
library(tesseract)

#get pdf as text

c <- pdf_ocr_text("https://www.asx.com.au/data/trt/ib_expectation_curve_graph.pdf", dpi = 1200)

#this first bit is not the key part, it just reads the date off the pdf (useful later)

datebase <- unlist(c %>% 
         str_split("\n")) %>% 
  as_tibble() %>% 
  mutate(date = if_else(str_detect(value, "at market close on"), value, NA_character_)) %>% 
  mutate(date = str_remove(date, "ASX As at market close on ")) %>% 
  mutate(date = str_remove_all(date, "[\"))]")) %>% 
  fill(date,.direction = "down") %>%
  mutate(date = as.Date(date, format("%d %B %Y"))) %>% 
  select(date) %>% 
  unique() %>% 
  pull()
  
  #create dataframe of ocr predictions

OCRtext <- 
  unlist(c %>%                #these 3 lines make a tibble
  str_split("\n")) %>% 
 as_tibble() %>% 
  mutate(id = row_number()) %>% 
  mutate(date = if_else(str_detect(value, "at market close on"), value, NA_character_)) %>%     #these 3 lines find the date and clean it
mutate(date = str_remove(date, "ASX As at market close on ")) %>%  #this line doesn't look too flexible.
  mutate(date = str_remove_all(date, "[\"))]")) %>% 
  mutate(values = str_detect(value, "Yield")) %>%  #this line finds the cell that has the numbers we want in it
  fill(date,.direction = "down") %>% 
    filter(id>5) %>%  #this line drops a TRUE result I don't want. the TRUE result I want will be near the end of the document not in the first 5 lines
  filter(values == "TRUE") %>% 
  mutate(value = str_remove_all(value, "[impliedyield|''Y()‘]")) %>%   #cleaning the numbers and turning them into a vector
    mutate(value = str_replace_all(value, "   ", " ")) %>% 
  mutate(value = str_replace_all(value, "  ", " ")) %>% 
    mutate(value = str_split(value, " ")) %>% 
    unnest(value) %>% 
      mutate(value = as.numeric(value))%>% 
    filter(!is.na(value)) %>% 
    select(date, value) %>% 
    mutate(date = as.Date(date, format("%d %B %Y"))) %>% 
    mutate(predictiondate = seq(as.Date(datebase), by = "month", length = 18)) %>%  #the months the predictions refer to 
    mutate(scrapedate =  Sys.time()) #add when the scraping happened. might be useful in case things break.
