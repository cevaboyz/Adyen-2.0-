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
require(ggplot2)
library(hrbrthemes)
library(RColorBrewer)
###############################################################################
###############################################################################
#Manual Insertion
threed_secure_authentication_report <- read_csv("threedsecure_authentication_report_2020-09-01T00_00_00_2021-09-01T23_59_59.csv", na = "0")

names(threed_secure_authentication_report)

names(manual_extraction)






#################################################################################
##################################Fast Lookup####################################

names(manual_extraction)

email <- manual_extraction %>% select(`Shopper Email`) %>% group_by(`Shopper Email`) %>% summarise(count = n()) %>%

try_table <- email %>% select(count) %>% group_by(count) %>% summarise(count = n())  
  
distribution_try_mean <- mean(email$count)

try_table2 <- as.data.frame(table(email$count))

try_table2 <- try_table2 %>% rename(`Numero di tentativi` = Var1, Frequenza = Freq)

try_table2 <- try_table2 %>% mutate(`Numero di tentativi` = as.character(as.factor(`Numero di tentativi`)))

max_axis_y <- summarize(try_table2$Frequenza)

max_axis_y<-  try_table2 %>%  summarise_if(is.numeric, max)

coul <- brewer.pal(9,"YlOrRd")

graph_1 <- barplot(height = try_table2$Frequenza, names.arg = try_table2$`Numero di tentativi`, ylim=c(0,1.1*max(unlist(try_table2$Frequenza))), col =  coul, ylab = "Frequenza", xlab = "Numero di Tentativi", main = "Tentativi di pagamento effettuati per ID utente in questo mese")

grap_1 <- text(graph_1, try_table2$Frequenza,cex=1, pos = 3)

grap_1 <- 

ggplot(data = try_table2, aes(x=`Numero di tentativi`, y=`Frequenza`))+


top_10_tryharders <- email %>% filter(rank(desc(count)) <=10) %>% arrange(desc(count))
