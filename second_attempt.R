
library(stringr)
library(tidyr)
library(dplyr)
library(pdftools)
library(tesseract)



e <- pdf_ocr_data("https://www.asx.com.au/data/trt/ib_expectation_curve_graph.pdf", dpi = 2200)

datebase2 <- format(Sys.time(), "%Y-%m-%d")%>% 
  as_tibble() %>% 
  mutate(date = as.character(value)) %>% 
  mutate(date = str_trunc(date, width = 8, ellipsis = "")) %>% 
  mutate(date = paste0( date, "01")) %>% 
  mutate(date = as.Date(date)) %>% 
  select(date) %>% 
  pull()

OCR_latest_predictions<- e %>% 
  bind_rows() %>% 
  separate(bbox, ",", into = (c("l", "t", "r", "b"))) %>% 
mutate(l=as.numeric(l)) %>% 
  mutate(t=as.numeric(t)) %>% 
  mutate(r=as.numeric(r)) %>% 
  mutate(b=as.numeric(b)) %>% 

  mutate(start = str_detect(word, "ield")) %>% 
    fill(start, .direction = "down") %>% 
  filter(t>14000) %>% 
mutate(junk =   str_detect(word, "[A-Za-z|]")) %>% 
         filter(junk == FALSE) %>% 
  mutate(value= as.numeric(word)) %>% 
  mutate(predictiondate = seq(as.Date(datebase2), by = "month", length = 18)) %>% 
  mutate(scrapedate =  Sys.time()) %>% 
select(value, predictiondate, scrapedate) %>% 
mutate(date = as.Date(datebase2))
