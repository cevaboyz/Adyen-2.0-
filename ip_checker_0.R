#################################Adyen 2.0#####################################
###############################################################################
################################IP CHECKER#####################################
###############################################################################
######https://www.ipqualityscore.com/free-ip-lookup-proxy-vpn-test/lookup/#####
###############################################################################
###############################################################################
###############################################################################
###############################################################################
######################script to check the quality of the ip/vpn usert##########
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(
  dplyr,
  readr,
  ggplot2,
  hrbrthemes,
  RColorBrewer,
  lubridate,
  countrycode,
  stringi,
  tidyverse,
  rvest,
  purrr,
  tidyr,
  openxlsx,
)


ip_range <- threedsecure_authentication_report_tidy %>% pull(shopper_ip)

ip_range <- unique(ip_range)

ip_range <- trimws(ip_range)

purrr::map_df(ip_range, ~ { 
  
  url_reviews <- paste0("https://ipleak.net/?q=",.x)
  
  page <- read_html(url_reviews) 










url <- "https://www.ipqualityscore.com/free-ip-lookup-proxy-vpn-test/lookup/151.43.191.80"

url_1 <- "https://ipleak.net/?q=109.113.183.105"
page <- read_html(url_1)



nodes <- as.data.frame(nodes)

tbls <- html_nodes(page, "table")

tbls2 <- html_table(tbls)

table2 <- as.data.frame(tbls2)

table2 <- table2[c(1,7),2]

test2 <- t(test2)

city_table <- 

table2 <- table2 %>% rename(raw_acquirer_response = Raw.acquirer.response)

table2 <- table2 %>% select(-Status)



