# preparation 
library(magrittr)  
library(dplyr)
library(ggplot2)
library(plyr) 
library(reshape2)
library(plotly) #interative 
library(lubridate) #time 
setwd("D:/BCG/Day2")
client_dat <- read.csv("client_data.csv")
price_dat <- read.csv("price_data.csv") 
glimpse(client_dat)
glimpse(price_dat) 
class(client_dat$churn) #integer >> transform into factor
class(client_dat$has_gas) 
client_dat$has_gas <- as.factor(ifelse(client_dat$has_gas == 't',1,0))
client_dat$has_gas
client_dat$churn <- as.factor(client_dat$churn)

#----------------------------------
# Overview of churn rate 
#----------------------------------
plot_1 <- client_dat %>%
  group_by(churn) %>%
  na.omit()
churn_num <- client_dat %>%
  filter(churn == 1) %>%
  summarise(n=n()) 
churn_percent <- (as.integer(churn_num) / length(client_dat$churn))*100
label = paste0(sprintf("%.1f", churn_percent), "%")
label #churn rate = 9.7% 

ggplot(plot_1,aes(x=churn)) + 
  geom_bar() + 
  labs(x="Churn or Not",y="Number of Customers") + 
  ggtitle("Plot1: Overview")


#---------------------------------
#Churn rate by sales channel 
#---------------------------------
plot_2 <- client_dat %>%
  dplyr::select(id,churn,channel_sales) %>%
  filter(churn == 1) 

channel_origin <- unique(plot_2$channel_sales)
channel_origin
plot_2$channel[plot_2$channel_sales == "foosdfpfkusacimwkcsosbicdxkicaua"] <- "Channel 1"
plot_2$channel[plot_2$channel_sales == "lmkebamcaaclubfxadlmueccxoimlema"] <- "Channel 2"
plot_2$channel[plot_2$channel_sales == "ewpakwlliwisiwduibdlfmalxowmwpci"] <- "Channel 3"
plot_2$channel[plot_2$channel_sales == "usilxuppasemubllopkaafesmlibmsdf"] <- "Channel 4"
plot_2$channel[plot_2$channel_sales == "epumfxlbckeskwekxbiuasklxalciiuu"] <- "Channel 5"
plot_2$channel[plot_2$channel_sales == "sddiedcslfslkckwlfkdpoeeailfpeds"] <- "Channel 6"
plot_2$channel[plot_2$channel_sales == "fixdbufsefwooaasfcxdxadsiekoceaa"] <- "Channel 7"
head(plot_2$channel)

ggplot(plot_2,aes(x=factor(channel))) + 
  geom_bar() + 
  stat_count() + 
  xlab("Channels") + 
  ylab("Churn") + 
  ggtitle("Plot2:Churn by sales channel")


#---------------------------------
#Churn rate by consumption 
#---------------------------------
plot_3 <- client_dat %>%
  dplyr::select(id,churn,has_gas,contains("cons")) %>%
  filter(churn == 1)
names(plot_3) 
summary(cons_12m) 
summary(cons_gas_12m)
summary(cons_last_month)
summary(imp_cons)

ggplot(plot_3,aes(cons_gas_12m)) + 
  geom_boxplot() + 
  scale_x_continuous(breaks = c(1000000,2000000,3000000,4000000,5000000,6000000),labels = c("1M","2M","3M","4M","5M","6M")) + 
  labs(x="gas consumption of the past 12 months",y="Frequency") + 
  ggtitle("Plot3-1: Gas Consumption and Churn") 

ggplot(plot_3,aes(x=cons_12m)) + 
  geom_boxplot() + 
  labs(x="Electricity consumption of the past 12 months",y="Frequency") + 
  ggtitle("Plot3-2: Electricity Consumption and Churn")  

ggplot(plot_3,aes(x=cons_last_month)) + 
  geom_boxplot() + 
  labs(x="electricity consumption of the last month",y="Frequency") + 
  ggtitle("Plot3-3: Electricity Consumption (last month) and Churn")  

ggplot(plot_3,aes(x=imp_cons)) + 
  geom_boxplot() + 
  labs(x="current paid consumption",y="Frequency") + 
  ggtitle("Plot3-4: Current Paid Consumption and Churn") 


#------------------------------
# Churn and Contract Type 
#------------------------------
plot_4 <- client_dat %>%
  dplyr::select(id,churn,has_gas)
plot_4$has_gas
ggplot(plot_4,aes(x=has_gas,fill=factor(churn))) + 
  geom_bar() + 
  labs(x="Also a gas client",y="Churn rate") + 
  ggtitle("Plot4:Churn and Contract Type") 


#------------------------------
# Churn and subscribed power
#------------------------------
plot_5 <- client_dat %>%
  dplyr::select(id,churn,pow_max) %>%
  filter(churn == 1)

summary(plot_5$pow_max)
ggplot(plot_5,aes(x=pow_max)) + 
  geom_boxplot() + 
  labs(x="Subscribed Power",y="Churn") + 
  scale_x_continuous(breaks = c(10,20,100,200,300)) + 
  ggtitle("Plot5:Churn and Subscribed Power") 

#------------------------------
# Hypothesis Testing 
#------------------------------
glimpse(price_dat)
summary(price_dat)
#merge data 
merge <- full_join(client_dat,price_dat,by="id")
glimpse(merge)
merge1 <- merge %>%
  dplyr::select(id,date_end,churn,contains("price_"))
glimpse(merge1)

price_select <- merge1 %>%
  mutate(
    price_p1 = price_off_peak_fix + price_off_peak_var,
    price_p2 = price_peak_fix + price_peak_var,
    price_p3 = price_mid_peak_fix + price_mid_peak_var
  ) %>%
  select(price_p1,price_p2,price_p3) 
head(price_select) 

#create 3-month sensitivity index 
price_3m_select <- merge1 %>%
  filter(merge1$price_date > "2015-09-01") %>%
  mutate(
    price_p1_3m = price_off_peak_fix + price_off_peak_var,
    price_p2_3m = price_peak_fix + price_peak_var,
    price_p3_3m = price_mid_peak_fix + price_mid_peak_var
  ) %>%
price_3m_select <- price_3m_select %>% 
  select(contains("3m"))
head(price_3m_select) 

price <- merge(price_select,price_3m_select)
head(price) 

#heat map 
corr <- cor(price[3:8])
heatmap(corr,Rowv=NA,Colv=NA,scale="column",margin=c(3,6))





