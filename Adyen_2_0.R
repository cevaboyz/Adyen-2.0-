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
require(dplyr)
library(readr)
require(ggplot2)
library(hrbrthemes)
library(RColorBrewer)
library(lubridate)
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

names(threed_secure_authentication_report)

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report %>% select(
    payment_method,
    directory_server,
    shopper_reference,
    currency,
    amount,
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
  threedsecure_authentication_report_tidy %>%
  +mutate(payment_method = ifelse(
    as.character(payment_method) == "maestro",
    "Maestro",
    as.character(payment_method)
  ))

threedsecure_authentication_report_tidy <-
  as.POSIXct(threedsecure_authentication_report_tidy$creation_date)

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(shopper_name = tolower(shopper_name))

threedsecure_authentication_report_tidy <-
  threedsecure_authentication_report_tidy %>% mutate(shopper_email = tolower(shopper_email))





#################################################################################
##################################Fast Lookup####################################



email <- manual_extraction %>% select(`Shopper Email`) %>% group_by(`Shopper Email`) %>% summarise(count = n()) 

try_table <- email %>% select(count) %>% group_by(count) %>% summarise(count = n())  
  
distribution_try_mean <- mean(email$count)

try_table2 <- as.data.frame(table(email$count))

total_transaction <- sum(unlist(try_table2$Frequenza))

try_table2 <- try_table2 %>% rename(`Numero di tentativi` = Var1, Frequenza = Freq)

try_table2 <- try_table2 %>% mutate(`Numero di tentativi` = as.character(as.factor(`Numero di tentativi`)))

max_axis_y <- summarize(try_table2$Frequenza)

#########################3D Offered#################################
threed_offered <- manual_extraction %>% select(`3D Offered`) %>% group_by(`3D Offered`) %>% summarise(count = n()) 















################Graphic 1################
max_axis_y<-  try_table2 %>%  summarise_if(is.numeric, max)

coul <- brewer.pal(9,"YlOrRd")

graph_1 <- barplot(height = try_table2$Frequenza, names.arg = try_table2$`Numero di tentativi`, ylim=c(0,1.1*max(unlist(try_table2$Frequenza))), col =  coul, ylab = "Frequenza", xlab = "Numero di Tentativi", main = "Tentativi di pagamento effettuati per ID utente in questo mese" )

grid(col = "black")
text(graph_1, +3, round(try_table2$Frequenza, 1),cex=1,pos=3) 

top_10_tryharders <- email %>% filter(rank(desc(count)) <=10) %>% arrange(desc(count))














#######PIECHART########

# Compute the position of labels
try_table2 <- try_table2 %>% 
  arrange(desc(`Numero di tentativi`)) %>%
  mutate(prop = try_table2$Frequenza / sum(try_table2$Frequenza) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(try_table2, aes(x="", y=try_table2$Frequenza, fill=try_table2$`Numero di tentativi`)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = try_table2$`Numero di tentativi`), color = "white", size=6) +
 
  
  ggplot(
    try_table2,
    aes(
      x = "",
      y = try_table2$Frequenza,
      fill = try_table2$`Numero di tentativi`
    )
  )+geom_bar(stat = "identity", width = 1)+coord_polar("y", start = 0) +
  theme_void() + geom_text(aes(label = paste0(
    Frequenza,
    " (",
    scales::percent(Frequenza / sum(Frequenza)),
    ")"
  )),
  position = position_stack(vjust = 0.5))







