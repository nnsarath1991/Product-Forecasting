# --------------------  -------------------- # 
# Author:  Sarathkumar Nachiappan Nallusamy
# Subject: EDA Part 1 - ggplots
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

summary(data)
str(data)

# --------------------  -------------------- # 
# data manipulation

data <- data %>% 
  mutate(ShipDate = as.Date(ShipDate), 
         ShipWeek = floor_date(ShipDate, 'week'), 
         ShipMonth = floor_date(ShipDate, 'month'), 
         PlantNumber = as.factor(PlantNumber), 
         ItemNumber = as.factor(ItemNumber), 
         SoldTo = as.factor(SoldTo), 
         ShipTo = as.factor(ShipTo), 
         ShippedCartons = as.integer(gsub(',', '', ShippedCartons)) ) %>% 
  select(ShipDate, ShipWeek, ShipMonth, 
         PlantNumber, DieNumber, ItemNumber, 
         SoldTo, ShipTo, ShippedCartons)


# ------------------------------- BASIC DATA EXPLORE ------------------------------- # 
# ----------------------------------------  ---------------------------------------- # 

# --------------------  -------------------- # 
# plot sales trend

data.summ1 <- data %>% 
  group_by(ShipWeek) %>% 
  summarise(num=n()/n_distinct(ShipDate), 
            cartons_mean = sum(ShippedCartons, na.rm=T)/n_distinct(ShipDate), 
            cartons_median = median(ShippedCartons, na.rm=T) ) %>% ungroup() %>% 
  filter(ShipWeek != min(ShipWeek) & ShipWeek != max(ShipWeek))

data.summ2 <- data %>% 
  group_by(ShipMonth) %>% 
  summarise(num=n()/n_distinct(ShipDate), 
            cartons_mean = sum(ShippedCartons, na.rm=T)/n_distinct(ShipDate), 
            cartons_median = median(ShippedCartons, na.rm=T) ) %>% ungroup() %>% 
  filter(ShipMonth != min(ShipMonth) & ShipMonth != max(ShipMonth))

p01 <- ggplot() + 
  geom_line(data=data.summ1, aes(x=ShipWeek, y=cartons_mean), color=1, size=0.6, alpha=0.5) + 
  geom_line(data=data.summ2, aes(x=ShipMonth, y=cartons_mean), color=2, size=1, alpha=1) + 
  xlab('') + ylab('') + 
  ggtitle("Number of Cartons Shipped per Day", subtitle='Averaged by week / by month') + 
  theme_hc()

p02 <- ggplot() + 
  geom_line(data=data.summ1, aes(x=ShipWeek, y=num), color=1, size=0.6, alpha=0.5) + 
  geom_line(data=data.summ2, aes(x=ShipMonth, y=num), color=2, size=1, alpha=1) + 
  xlab('') + ylab('') + 
  ggtitle("Number of Transactions per Day", subtitle='Averaged by week / by month') + 
  theme_hc()

p03 <- ggplot() + 
  geom_line(data=data.summ1, aes(x=ShipWeek, y=cartons_median), color=1, size=0.6, alpha=0.5) + 
  geom_line(data=data.summ2, aes(x=ShipMonth, y=cartons_median), color=2, size=1, alpha=1) + 
  xlab('') + ylab('') + 
  ggtitle("Median, Number of Cartons Shipped per transaction", 
          subtitle='Analyzed by week / by month') + 
  theme_hc()

ggsave(filename='plots/e0_average_daily_trend.png',
       plot=rbind(ggplotGrob(p01), ggplotGrob(p02), ggplotGrob(p03), size = "last"), 
       width=7, height=7, units='in')


# --------------------  -------------------- # 
# use only two years' data

data <- data %>% 
  filter(format(ShipDate, '%Y') %in% c('2015', '2016') )


# --------------------  -------------------- # 
# plot histogram of carton size

p01 <- ggplot(data) + 
  geom_histogram(aes(x=ShippedCartons, y=..density..), binwidth=1, alpha=0.7) +
  geom_density(aes(x=ShippedCartons), adjust=1) + 
  scale_x_log10(labels=comma) + 
  scale_y_continuous(labels=comma) + 
  xlab('Number of Cartons per Transaction') + ylab('Density') + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  ggtitle("Distribution, Number of Cartons per Transaction", subtitle='Number of cartons in log scale') + 
  theme_hc()

ggsave(filename='distribution_of_number_of_cartons_per_order.png',
       plot=p01, width=5.5, height=5.5, units='in')

# --------------------  -------------------- # 
# plot histogram of different factories

data.summ1 <- data %>% 
  group_by(PlantNumber) %>% 
  summarise(num = n(), 
            cartons_sum = sum(as.numeric(ShippedCartons), na.rm=T), 
            cartons_median = median(as.numeric(ShippedCartons), na.rm=T)) %>% ungroup() %>% 
  mutate(PlantNumber = as.character(PlantNumber)) %>% 
  arrange(desc(cartons_sum))

data.summ1$PlantNumber <- factor(data.summ1$PlantNumber, levels=data.summ1$PlantNumber)

p01 <- ggplot(data.summ1) + 
  geom_histogram(aes(x=PlantNumber, y=cartons_sum), stat="identity", alpha=0.7) +
  xlab('') + ylab('') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Total number of cartons shipped", subtitle='During 2015 and 2016, per plant') + 
  theme_hc()

p02 <- ggplot(data.summ1) + 
  geom_histogram(aes(x=PlantNumber, y=num), stat="identity", alpha=0.7) +
  scale_y_continuous(labels=comma) + 
  xlab('') + ylab('') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Total number of transactions", subtitle='During 2015 and 2016, per plant') + 
  theme_hc()

p03 <- ggplot(data.summ1) + 
  geom_histogram(aes(x=PlantNumber, y=cartons_median), stat="identity", alpha=0.7) +
  scale_y_continuous(labels=comma) + 
  xlab('Plant ID Number') + ylab('') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Median, number of cartons shipped per transaction", subtitle='During 2015 and 2016, per plant') + 
  theme_hc()

ggsave(filename='plots/e0_per_factory_all.png',
       plot=rbind(ggplotGrob(p01), ggplotGrob(p02), ggplotGrob(p03), size = "last"), 
       width=7, height=7, units='in')

