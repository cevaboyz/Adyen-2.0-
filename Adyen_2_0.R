#################################Adyen 2.0#####################################
###############################################################################
#######################3D Secure Authentication Report#########################
###############################################################################
##############################Api Connection###################################
###############################################################################
###############################################################################
##############https://docs.adyen.com/reporting/3d-secure-reports###############
###############################################################################
######################official documentation about the report##################
###############################################################################
#
#
#
#
#
#$ wget --http-user='report@Company.[YourCompanyAccount]' --http-password='[BasicAuthPassword]' --quiet --no-check-certificate https://ca-test.adyen.com/reports/download/MerchantAccount/[YourMerchantAccount]/[ReportFileName]
#
#https://pal-test.adyen.com/pal/servlet/Payment/v67/authorise3d
#
#
#Libraries and Packages
#auto updater and installer if the package is not already installed
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(dplyr, readr, ggplot2, hrbrthemes, RColorBrewer, lubridate, countrycode, stringi, tidyverse)
#



library(dplyr)
library(readr)
library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)
library(lubridate)
library(countrycode)
library(stringi)
library(tidyverse)

###############################################################################
###############################################################################
#########################Auto Fetch############################################

getwd()

files <- list.files(pattern = ".xlsx$")

df_list <- lapply(files, read.excel)

df_bind <- bind_rows(df_list)



#################################################################################
#######################################Manual Insertion##########################
threedsecure_authentication_report <-
  read_csv("threedsecure_authentication_report_2021-07-01T00 00 00_2021-09-30T23 59 59.csv")

names(threedsecure_authentication_report)

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report %>% select(
    payment_method,
    directory_server,
    shopper_reference,
    currency,
    amount,
    issuer_name,
    issuer_country_code,
    creation_date,
    risk_score,
    device_channel,
    threeDS_version,
    authentication_only,
    threeDS2_redirect,
    card_holder_info,
    acquirer_response,
    raw_acquirer_response,
    cvc_supplied,
    cvc_response_code,
    avs_response,
    interaction_counter,
    rreq_authentication_type,
    results_status,
    liability_shift,
    shopper_name,
    shopper_ip,
    shopper_country,
    shopper_email,
    shopper_interaction
  )

rm(threedsecure_authentication_report)

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>%  mutate(payment_method = ifelse(
    as.character(payment_method) == "mc",
    "Mastercard",
    as.character(payment_method)
  ))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(payment_method = ifelse(
    as.character(payment_method) == "visa",
    "Visa",
    as.character(payment_method)
  ))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>%  mutate(payment_method = ifelse(
    as.character(payment_method) == "amex",
    "Amex",
    as.character(payment_method)
  ))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>%  mutate(payment_method = ifelse(
    as.character(payment_method) == "maestro",
    "Maestro",
    as.character(payment_method)
  ))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(creation_date = as.POSIXct(creation_date))
as.POSIXct(threedsecure_authentication_report_tidy$creation_date)

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(shopper_name = tolower(shopper_name))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(shopper_email = tolower(shopper_email))

threedsecure_authentication_report_tidy$issuer_country_code_1 <-
  threedsecure_authentication_report_tidy %>% mutate(
    issuer_country_code = countrycode(
      threedsecure_authentication_report_tidy$issuer_country_code,
      origin = "iso2c",
      destination = "country.name"
    )
  )

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(issuer_country_code = threedsecure_authentication_report_tidy$issuer_country_code_1$issuer_country_code)

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(issuer_country_code = toupper(issuer_country_code))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(issuer_name = gsub("\\.", "", issuer_name))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(issuer_name = tolower(issuer_name))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(issuer_name = gsub("spa", "", issuer_name))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(issuer_name = ifelse(is.na(issuer_name), "Not Available", issuer_name))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(issuer_name = trimws(issuer_name))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(issuer_name = toupper(issuer_name))


threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(acquirer_response = gsub("approved", "Approved", acquirer_response))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(acquirer_response = gsub("declined", "Declined", acquirer_response))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(acquirer_response = gsub("unknown", "Unknown", acquirer_response))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(acquirer_response = ifelse(is.na(acquirer_response), "Unknown", acquirer_response))


threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(raw_acquirer_response = str_sub(raw_acquirer_response, 5, -1))

threedsecure_authentication_report_tidy$issuer_country_code_2 <-
  threedsecure_authentication_report_tidy %>% mutate(
    shopper_country = countrycode(
      threedsecure_authentication_report_tidy$shopper_country,
      origin = "iso2c",
      destination = "country.name"
    )
  )

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(shopper_country = threedsecure_authentication_report_tidy$issuer_country_code_2$shopper_country)

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(shopper_country = toupper(shopper_country))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(liability_shift = gsub("FALSE", "NO FRIENDLY FRAUD PROTECTION", liability_shift))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(liability_shift = gsub("TRUE", "FRIENDLY FRAUD PROTECTION", liability_shift))

ip_range <-
  threedsecure_authentication_report_tidy %>% select(shopper_ip)

ip_range <- ip_range %>% trimws()

ip_range <- ip_range %>% pull(ip_range)

geolocation_by_ip <-
  rgeolocate::ip_api(ip_range, as_data_frame = TRUE, delay = TRUE)#need to use it with a proxy, otherwise is blocked by the corpo grid

geolocation_by_ip_tidy <-
  geolocation_by_ip %>% select(city_name, latitude, longitude, as_code, zip_code)


match_for_ip <-
  threedsecure_authentication_report_tidy %>% select(
    shopper_ip,
    shopper_email,
    shopper_name,
    shopper_country,
    shopper_country,
    raw_acquirer_response,
    creation_date
  )

botnet_user <-
  match_for_ip %>% group_by(shopper_email) %>% mutate(changed_ip = c("no", "yes")[1 +
                                                                                    (n_distinct(shopper_ip) > 1)]) %>% ungroup

botnet_user_yes <- botnet_user %>% filter(changed_ip == "yes")

botnet_user_count <-
  botnet_user_yes %>% group_by(shopper_email) %>% summarize(count = n())


botnet_user_with_names <-
  merge(botnet_user_count,
        threedsecure_authentication_report_tidy,
        all.x = TRUE)

botnet_user_with_names <-
  botnet_user_with_names %>% select(
    shopper_email,
    shopper_name,
    shopper_ip,
    count,
    raw_acquirer_response,
    creation_date
  )

botnet_trial_final <-
  botnet_user_with_names %>% group_by(shopper_email) %>% filter(creation_date ==
                                                                  max(creation_date))

botnet_final <-
  botnet_user_with_names %>% group_by(shopper_email) %>% filter(creation_date ==
                                                                  max(creation_date))

botnet_final <- botnet_final %>% arrange(desc(count))

botnet_final_minus_ip <-
  botnet_user_with_names %>% group_by(shopper_email) %>% filter(creation_date ==
                                                                  max(creation_date)) %>% select(-shopper_ip)


botnet_final_minus_ip <-
  botnet_final_minus_ip %>% arrange(desc(count))

#################################################################################
#################################################################################
not_botnet_users_trials <-
  botnet_user %>% filter(changed_ip == "no")

not_botnet_user_count <-
  not_botnet_users_trials %>% group_by(shopper_email) %>% summarize(count = n())

not_botnet_user_with_names <-
  merge(not_botnet_user_count,
        threedsecure_authentication_report_tidy,
        all.x = TRUE)

not_botnet_user_with_names <-
  not_botnet_user_with_names %>% select(
    shopper_email,
    shopper_name,
    shopper_ip,
    count,
    raw_acquirer_response,
    creation_date
  )

not_botnet_user_with_names_trial_final <-
  not_botnet_user_with_names %>% group_by(shopper_email) %>% filter(creation_date == max(creation_date))


not_botnet_user_with_names_trial_final <-
  not_botnet_user_with_names_trial_final %>% filter(count >= 2)
#################################################################################
#################################################################################
##################################Fast Lookup####################################








email <-
  manual_extraction %>% select(`Shopper Email`) %>% group_by(`Shopper Email`) %>% summarise(count = n())

try_table <-
  email %>% select(count) %>% group_by(count) %>% summarise(count = n())

distribution_try_mean <- mean(email$count)

try_table2 <- as.data.frame(table(email$count))

total_transaction <- sum(unlist(try_table2$Frequenza))

try_table2 <-
  try_table2 %>% rename(`Numero di tentativi` = Var1, Frequenza = Freq)

try_table2 <-
  try_table2 %>% mutate(`Numero di tentativi` = as.character(as.factor(`Numero di tentativi`)))

max_axis_y <- summarize(try_table2$Frequenza)

#########################3D Offered#################################
threed_offered <-
  manual_extraction %>% select(`3D Offered`) %>% group_by(`3D Offered`) %>% summarise(count = n())















################Graphic 1################
max_axis_y <-  try_table2 %>%  summarise_if(is.numeric, max)

coul <- brewer.pal(9, "YlOrRd")

graph_1 <-
  barplot(
    height = try_table2$Frequenza,
    names.arg = try_table2$`Numero di tentativi`,
    ylim = c(0, 1.1 * max(unlist(
      try_table2$Frequenza
    ))),
    col =  coul,
    ylab = "Frequenza",
    xlab = "Numero di Tentativi",
    main = "Tentativi di pagamento effettuati per ID utente in questo mese"
  )

grid(col = "black")
text(graph_1,
     +3,
     round(try_table2$Frequenza, 1),
     cex = 1,
     pos = 3)

top_10_tryharders <-
  email %>% filter(rank(desc(count)) <= 10) %>% arrange(desc(count))


#density and distribution of risk sccore non aggregati
#1
#
ggplot(risk_curve, aes(x = risk_score)) +
  geom_histogram(
    aes(y = ..density..),
    binwidth = 5,
    colour = "black",
    fill = "white"
  ) +
  geom_density(fill = "blue", alpha = .2)

risk_curve_aggregate$rounded <- round(risk_curve_aggregate$x)


risk_curve_aggregate <-
  risk_curve_aggregate %>% rename(shopper_mail = Group.1, risk_score_mean = rounded)
risk_curve_aggregate <- risk_curve_aggregate %>% select(-x)











#######PIECHART########

# Compute the position of labels
try_table2 <- try_table2 %>%
  arrange(desc(`Numero di tentativi`)) %>%
  mutate(prop = try_table2$Frequenza / sum(try_table2$Frequenza) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop)

ggplot(
  try_table2,
  aes(
    x = "",
    y = try_table2$Frequenza,
    fill = try_table2$`Numero di tentativi`
  )
) +
  geom_bar(stat = "identity",
           width = 1,
           color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "none") +
  
  geom_text(
    aes(y = ypos, label = try_table2$`Numero di tentativi`),
    color = "white",
    size = 6
  ) +
  
  
  ggplot(
    try_table2,
    aes(
      x = "",
      y = try_table2$Frequenza,
      fill = try_table2$`Numero di tentativi`
    )
  ) + geom_bar(stat = "identity", width = 1) + coord_polar("y", start = 0) +
  theme_void() + geom_text(aes(label = paste0(
    Frequenza,
    " (",
    scales::percent(Frequenza / sum(Frequenza)),
    ")"
  )),
  position = position_stack(vjust = 0.5))
