# Question 
# Can we increase active customers and customer buying activities 
# by improving the order recommendation system based on their previous orders

# Dataset
# The dataset consists of information about 3.4 million orders and distributes across 6 csv files

library(dplyr)
library(ggplot2)
library(gridExtra)

### Import Files into R ###
aisles <- read.csv("aisles.csv")
departments <- read.csv("departments.csv")
order_products__prior <- read.csv("order_products__prior.csv")
order_products__train <- read.csv("order_products__train.csv")
orders <- read.csv("orders.csv")
products <- read.csv("products.csv")

### Data Overview #### 
# 1) aisles
head(aisles)
str(aisles)
summary(aisles)
# 134 types of aisles

# 2) departments
head(departments)
str(departments)
summary(departments)
# 21 types of departments

# 3) products
head(products)
str(products)
summary(products)

# 4) order_products__prior
head(order_products__prior)
str(order_products__prior)
summary(order_products__prior)
# range for add to cart order is from 1 to 145 in prior data
# Note: the 'reordered' contains 1 and 0 which 1 means reordered and 0 means not reordered

# 5) order_products__train
head(order_products__train)
str(order_products__prior)
summary(order_products__train)
# range for add to cart order is from 1 to 80 in train data

# 6) orders
head(orders,20)
str(orders)
summary(orders)
# 'order_dow' means 'order day of week', 0 stands for monday, it is not missing value
# 'order hour of day', 0 stand for the first hour of a day which is 1 am
# 206209 NAs' for days since prior order in table orders which count 5% of total
# evaluation set for train has 131209 sets and test for 75000 sets which we predict reordered items only for the test data set


### Exploratory Data Analysis (EDA) ### 


## Part 1: When do the customers order?

# Start exploratory from single table such as orders

str(orders)

# 1. Number of Orders vs Hour of Day
p1 <- orders %>% ggplot(aes(x = order_hour_of_day)) + 
  geom_bar(stat = "count", fill = "orange") + ylab('number of orders') + 
  scale_y_continuous(labels = scales::comma) +   
  ggtitle('Number of Orders in Hour of Day') +
  theme(plot.title = element_text(hjust = 0.5))
p1
# Most orders are from 8am to 18pm daily

# 2. Number of Orders vs Day of Week
p2 <- orders %>% ggplot(aes(x = order_dow)) + 
  geom_histogram(stat = "count", fill = "orange") + ylab('number of orders') + 
  scale_y_continuous(labels = scales::comma) +
  ggtitle('Number of Orders in Day of Week') +
  theme(plot.title = element_text(hjust = 0.5))
p2
# Most orders are on Monday and Tuesday

# 3. Days since Prior Order
p3 <- orders %>% filter(days_since_prior_order != '') %>% 
  ggplot(aes(x = days_since_prior_order)) +
  geom_histogram(stat = "count", fill = "orange") + ylab('number of orders') +
  scale_y_continuous(labels = scales::comma) +
  ggtitle('Days since Prior Order') +
  theme(plot.title = element_text(hjust = 0.5))
p3
# Reorder frequnency shows increase trend in first 7 days, then decreasing 
# The highest frequency of re-order is once per month, second once per week


## Part 2: How many prior orders? How many items in an order?

# 4. how many prior orders do the custumers have?
p4 <- orders %>% filter(eval_set == 'prior') %>% count(order_number) %>%
  ggplot(aes(order_number,n)) + geom_point(color = "blue", size = 2) +
  ggtitle('Number of Prior Orders of Customers') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('number of custumers') + xlab('number of prior orders')
p4
# the dataset include users that have 4~100 orders
# we can see that there are at least 3 prior orders for each consumer 
# the number of customers is decreasing as the order_number increases


# 5. how many items each customer buy each time?

# Start exploratory from single table to order_product
summary(order_products__prior)

## prior table
p51 <- order_products__prior %>% group_by(order_id) %>% 
  summarise(number_of_items = last(add_to_cart_order)) %>%
  ggplot(aes(x = number_of_items)) + 
  geom_histogram(stat = 'count', fill = 'blue') + xlim(0,80) + 
  ggtitle('Number of Items of Each Order in prior table') +
  theme(plot.title = element_text(hjust = 0.5)) + ylab('number of orders')
## train table
p52 <- order_products__train %>% group_by(order_id) %>% 
  summarise(number_of_items = last(add_to_cart_order)) %>%
  ggplot(aes(x = number_of_items)) +
  geom_histogram(stat = 'count', fill = 'orange') + xlim(0,80) +
  ggtitle('Number of Items of Each Order in train table') +
  theme(plot.title = element_text(hjust = 0.5)) + ylab('number of orders')
grid.arrange(p51,p52,nrow=2)
# The overall performance from prior and train table is smiliar, most people buy around 5 items 

# 6. how many of the items are re-ordered?
# 59 percentages of orders are re-ordered
df6 <- order_products__prior %>% group_by(reordered) %>% 
  summarise(count = n()) %>%
  mutate(proportion = round(count/sum(count), 2))
df6
p6 <- df6 %>% ggplot(aes(x = reordered, y = count, fill = reordered)) + 
  geom_bar(stat = 'identity') + theme(legend.position = "none") + 
  scale_y_continuous(labels = scales::comma) + ylab('number of items') +
  ggtitle('Number of Reordered Items') + # geom_col(aes(fill = reordered)) 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = proportion)) 
p6 
# 59% of the items in prior table are re-ordered


## Part 3: What are the popular products?

# 7. what products are the best seller or most likely re-ordered?
df7 <- order_products__prior %>% group_by(product_id) %>% 
  summarise(proportion_reordered = mean(reordered), num_of_items=n())
df7
# But what we don't know is the name for each product
# so we need to manipulate with products table in this case
df71 <- df7 %>% arrange(desc(num_of_items)) %>% left_join(products, by='product_id') %>% head(10)
p7 <- df71 %>% ggplot(aes(x=reorder(product_name,-num_of_items), y=num_of_items)) + 
  geom_bar(stat="identity",fill="green") + 
  scale_y_continuous(labels = scales::comma) + ylab('number of items') +
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank()) +
  ggtitle('The Top 10 mostly Purchased Products') +
  theme(plot.title = element_text(hjust = 0.5))
p7
# banana is the bestseller

df7 %>% arrange(desc(proportion_reordered)) %>% left_join(products, by= 'product_id') %>% head(10)
# the highest re-order percentage product is raw veggie wrappers


# 8. what product in their first choice add to cart?
df8 <-order_products__train %>% group_by(product_id, add_to_cart_order) %>%
  summarise(count = n()) %>% mutate(pct = count/sum(count))
df8
df81 <- df8 %>% filter(add_to_cart_order == 1, count>20) %>%
   left_join(products, by = 'product_id') %>% 
  select(product_name, pct, count) %>% arrange(desc(pct)) %>% head(10)
p8 <- df81 %>% ggplot(aes(x=reorder(product_name,-pct), y=pct))+
  geom_bar(stat="identity",fill="green")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank()) + 
  coord_cartesian(ylim=c(0.4,0.7)) +
  ggtitle('The Top 10 Products that are first added to cart') +
  theme(plot.title = element_text(hjust = 0.5))
p8
# white multifold towels have the highest percentage add to cart

# 9. organic food vs non-organic food
head(products)
# Split product into organic and non-organic from products table
library(stringr)
df9 <- products %>% mutate(organic = ifelse(str_detect(str_to_lower(products$product_name),'organic'),
                                          'organic','not organic'), organic = as.factor(organic))
head(df9)
prop.table(table(df9$organic))
# 10% products are organic while 90% products are not organic

# 10. how is the re-ordered percentage of organic and non-organic product
df10 = order_products__train %>% left_join(df9, by = 'product_id') %>% group_by(organic) %>%
  summarise(mean_reordered = mean(reordered))
df10 %>% ggplot(aes(x= organic,y=mean_reordered, fill = organic)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = round(mean_reordered, 2), y= mean_reordered + 0.03)) +
  ggtitle('Reordered Percentage vs Organic') +
  theme(plot.title = element_text(hjust = 0.5))
# organic product has 65 percentages reordered
# although organic product only counts 10 percent as total 
# but reorder rate of organic product is much higher than non-organic 


# Part 4: visualizing the product portfolio

# 11. How are aisles organized within departments?
# install.packages('treemap')
library(treemap)
df11 <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n()) %>%
  left_join(departments,by="department_id") %>%
  left_join(aisles,by="aisle_id")

map1 <- order_products__prior %>% 
  group_by(product_id) %>% 
  summarize(count=n()) %>% 
  left_join(products,by="product_id") %>% 
  ungroup() %>% 
  group_by(department_id,aisle_id) %>% 
  summarize(sumcount = sum(count)) %>% 
  left_join(df11, by = c("department_id", "aisle_id")) %>% 
  mutate(onesize = 1)

treemap(map1,index=c("department","aisle"),
        vSize="onesize",vColor="department",
        palette="Set3",title="Aisles organized within Departments",
        sortID="-sumcount", 
        border.col="#FFFFFF",type="categorical", 
        fontsize.legend = 0,bg.labels = "#FFFFFF")

# 12. How many unique products are offered in each department/aisle?
treemap(df11,index=c("department","aisle"),
        vSize="n",title="Aisles (affected by unique products number) organized within Departments",
        palette="Set3",border.col="#FFFFFF")

# 13. How often are products from the department/aisle sold?
treemap(map1,index=c("department","aisle"),
        vSize="sumcount",title="Aisles (affected by products quantity sold) organized within Departments",
        palette="Set3",border.col="#FFFFFF")


# Main take-aways
# 1. When to recommend:
# - Recommend at 7 days and 30 days after previous order.
# - Recommend at 8:45 or 1:00 when users at break.
# - Recommend when there are discounts in markets.

# 2. How many items to recommend:
# - Mostly 4 to 7 items at a time, maximum 10.
# - Divided into sublists for user-friendly viewing.

# 3. What to recommend:
# - Popular products based on chosen aisles and departments of users.
# - Related or potentially-interested aisles based on his ordered items.
