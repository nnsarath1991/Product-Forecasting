# --------------------  -------------------- # 
# Author:  Sarathkumar Nachiappan Nallusamy
# Subject: EDA Part 2 - Segmentation
# Date:    May 2017
# --------------------  -------------------- # 

# load packages
library('dplyr')
library('tidyr')
library('scales')
library('grid')
library('ggplot2')
library('ggthemes')
library('lubridate')


# ------------------------------- DATA MANIPULATION -------------------------------- # 
# ----------------------------------------  ---------------------------------------- # 

# load data 
data <- read.csv('sample_data.csv', na.strings='', stringsAsFactors=F)

# --------------------  -------------------- # 
# data manipulation


data  <- data %>%
  mutate(ShipDate = as.Date(ShipDate, format='%m/%d/%Y'),
         ShipWeek = floor_date(ShipDate, "week"),
         ShipMonth = floor_date(ShipDate, "month"),
         ShipBiMonth = floor_date(ShipDate, "bimonth"),
         ShipQuarter = floor_date(ShipDate,"quarter"),
         ShipHalfYear = floor_date(ShipDate, "halfyear"),
         ShipYear = floor_date(ShipDate, "year"),
         PlantNumber = as.factor(PlantNumber),
         ItemNumber = as.factor(ItemNumber),
         DieNumber = as.factor(DieNumber),
         SoldTo = as.factor(SoldTo),
         ShipTo = as.factor(ShipTo),
         ShippedCartons = as.integer(gsub(',','',ShippedCartons))) %>%
  select(ShipDate, ShipWeek, ShipMonth, ShipBiMonth, ShipQuarter, ShipHalfYear, ShipYear,  
         PlantNumber, DieNumber, ItemNumber, 
         SoldTo, ShipTo, ShippedCartons)

##Checking number of combinations


#Creating a table orders 
orders1 <- data %>% select(ItemNumber, ShipTo) %>% distinct() %>% mutate(order_index = 1:n())
sapply(orders1, function(x) length(unique(x))) #checking unique+

orders2 <- data %>% select(ItemNumber, SoldTo) %>% distinct() %>% dplyr::mutate(order_index = 1:n())
sapply(orders2, function(x) length(unique(x))) #checking unique

orders3 <- data %>% select(DieNumber, ShipTo) %>% distinct() %>% dplyr::mutate(order_index = 1:n())
sapply(orders3, function(x) length(unique(x))) #checking unique

orders4 <- data %>% select(DieNumber, SoldTo) %>% distinct() %>% dplyr::mutate(order_index = 1:n())
sapply(orders4, function(x) length(unique(x))) #checking unique


#Creating a table orders 
orders <- data %>% select(ItemNumber,ShipTo) %>% distinct() %>% dplyr::mutate(order_index = 1:n())
head(orders)
tail(orders)
sapply(orders, function(x) length(unique(x))) #checking unique

data <- data %>% left_join(orders) %>% 
  arrange(order_index, desc(ShipDate)) %>% group_by(order_index) %>% 
  dplyr::mutate(period = ShipDate - lead(ShipDate)) %>% 
  dplyr::summarise(num = n(), 
            cartons_sum = sum(as.numeric(ShippedCartons), na.rm=T), 
            cartons_mean = mean(as.numeric(ShippedCartons), na.rm=T),
            cartons_med = median(as.numeric(ShippedCartons), na.rm=T),
            cartons_sd = sd(as.numeric(ShippedCartons),na.rm=T),
            cartons_cov = cartons_sd/cartons_mean,
            periods_med = median(period, na.rm=T))  %>% ungroup() 


sapply(data, function(x) length(unique(x))) #checking unique
head(data)
tail(data)

data$periods_med[is.na(data$periods_med)] <- 1e5  #ask why do we do this
max(data$periods_med)


data <- data %>% 
  dplyr::mutate(OrderFrequency = 'Once',
         OrderFrequency = ifelse(periods_med <= 52*7, '1-Year', OrderFrequency), 
         OrderFrequency = ifelse(periods_med <= 26*7, 'Half-Year', OrderFrequency), 
         OrderFrequency = ifelse(periods_med <= 13*7, '1-Quarter', OrderFrequency), 
         OrderFrequency = ifelse(periods_med <= 5*7, '1-Month', OrderFrequency), 
         OrderFrequency = ifelse(periods_med <= 2*7, '2-Weeks', OrderFrequency), 
         OrderFrequency = ifelse(periods_med <= 1*7, '1-Week', OrderFrequency)) %>% 
  mutate(OrderFrequency = factor(OrderFrequency, levels=c('1-Week', 
                                                          '2-Weeks', 
                                                          '1-Month', 
                                                          '1-Quarter', 
                                                          'Half-Year', 
                                                          '1-Year', 
                                                          'Once') ) )

# --------------------  -------------------- # 
# classify orders

data <- data %>% arrange(desc(cartons_sum)) %>% 
  dplyr::mutate(cum_order = 1:n(), 
         cum_sum = cumsum(as.numeric(cartons_sum)) ) %>% 
  dplyr::mutate(OrderClass = 'C',
         OrderClass = ifelse(cum_sum < 0.90 * max(cum_sum), 'B', OrderClass), 
         OrderClass = ifelse(cum_sum < 0.75 * max(cum_sum), 'A', OrderClass)) 

# --------------------------------- CLASSIFY ITEMS --------------------------------- # 
# ----------------------------------------  ---------------------------------------- # 

# ---------------------------------------- # 
# plot order classes

class_A_x <- data %>% filter(OrderClass == 'A') %>% select(cum_order) %>% max()
class_A_y <- data %>% filter(OrderClass == 'A') %>% select(cum_sum) %>% max()
class_B_x <- data %>% filter(OrderClass == 'B') %>% select(cum_order) %>% max()
class_B_y <- data %>% filter(OrderClass == 'B') %>% select(cum_sum) %>% max()
class_C_x <- data %>% filter(OrderClass == 'C') %>% select(cum_order) %>% max()
class_C_y <- data %>% filter(OrderClass == 'C') %>% select(cum_sum) %>% max()

p01 <- ggplot(data) + 
  geom_line(aes(x=cum_order, y=cum_sum, color=OrderClass), size=1) + 
  geom_vline(aes(xintercept=class_A_x), alpha=0.5) +
  geom_vline(aes(xintercept=class_B_x), alpha=0.5) +
  geom_vline(aes(xintercept=class_C_x), alpha=0.5) +
  geom_hline(aes(yintercept=class_A_y), alpha=0.5) +
  geom_hline(aes(yintercept=class_B_y), alpha=0.5) +
  geom_hline(aes(yintercept=class_C_y), alpha=0.5) +
  geom_text(aes(x=class_A_x, y=0, label=class_A_x), size=3.5, angle=90, vjust=-0.4, hjust=0) +
  geom_text(aes(x=class_B_x, y=0, label=class_B_x), size=3.5, angle=90, vjust=-0.4, hjust=0) +
  geom_text(aes(x=class_C_x, y=0, label=class_C_x), size=3.5, angle=90, vjust=-0.4, hjust=0) +
  geom_text(aes(x=0, y=class_A_y, label=' 75%'), size=3.5, angle=0, vjust=-0.2, hjust=0.4) +
  geom_text(aes(x=0, y=class_B_y, label=' 90%'), size=3.5, angle=0, vjust=-0.2, hjust=0.4) +
  geom_text(aes(x=0, y=class_C_y, label='100%'), size=3.5, angle=0, vjust=-0.2, hjust=0.4) +
  scale_x_continuous(labels=comma) +
  scale_y_continuous(labels=comma) +
  xlab('Number of Orders - Top Ranked') + ylab('Number of Cartons') + 
  ggtitle("ABC Classification on ItemNumber-ShipTo Combinations") + 
          theme_hc() + scale_colour_discrete(name = "Class of Order")
p01
ggsave(filename='ABC_ItShip_Allplants_Upd.png',
       plot=p01, width=5.5, height=5.5, units='in')


# --------------------  -------------------- # 

p01 <- ggplot(data) + 
  geom_bar(aes(x=OrderFrequency, fill=OrderClass)) + 
  scale_y_continuous(labels=comma) + 
  scale_fill_discrete(name = "Class of Order") + 
  xlab('Frequency Class') + ylab('Number of Orders') + 
  ggtitle("Frequency of Orders - Histogram", 
          subtitle='Classified using the median of the number of days \
          between two transaction') + 
  theme_hc()

p01
ggsave(filename='plots/e1_classify_orders_itemnumer_soldto_frequencies.png',
       plot=p01, width=5.5, height=5.5, units='in')

p02 <- ggplot(data) + geom_density(aes(x=cartons_med, fill=OrderClass), alpha=0.5) + 
  scale_fill_discrete(name = "Class of Order") + 
  xlab('Number of Cartons per Shipment') + ylab('Density') + 
  ggtitle("Distribution - Median Size of Shipment", 
          subtitle='Given classes of orders and their frequencies') + 
  facet_grid(OrderFrequency~OrderClass) + 
  scale_x_log10(labels=comma) + 
  theme_hc()

ggsave(filename='plots/e1_classify_orders_itemnumer_soldto_size.png',
       plot=p02, width=7, height=7, units='in')

summ <- data %>% 
  group_by(OrderClass, OrderFrequency) %>% 
  summarise(count = n(),
            cartons_sum = sum(as.numeric(cartons_sum), na.rm=T) )

ggplot(summ)
