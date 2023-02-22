library(lubridate)
library(tidyverse)
library(tidyquant)
library(ggsci)

data <-read_csv("NHS Staff Annual Earnings Estimates to September 2022 in NHS Trusts and other core organisations in England, Provisional Statistics CSV text file.csv")
# Identify column names 
data %>%
  colnames()
# Identify staff groups
data %>%
  select(`Staff Group`)%>%
  unique()%>%
  print(n=27)
# Identify payment types
data %>% 
  select(`Payment type`) %>% 
  unique()

# look at date format
data %>% 
  select(`Date`) %>% 
  head()

# Convert date to date

data.1 <- data %>% 
  mutate(date = dmy(Date))

#check it worked
data.1 %>% 
  head()

#lets try plotting to see how it goes - start with all staff basic pay per fte
data.1 %>% 
  filter(str_detect(.$`Staff Group`, "All staff")) %>% 
  filter(str_detect(.$`Payment type`, "PER_FTE")) %>% 
  ggplot(aes(x = date, y = Amount)) +
  geom_point() +
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("Date")+
  ylab("£ per FTE")

# Lets try comparing doctors
data.1 %>% 
  filter(str_detect(.$`Staff Group`, regex("doctor|consultant|core tra|staff grade|associate sp|specialty reg",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, "PER_FTE")) %>% 
  ggplot(aes(x = date, y = Amount, colour = `Staff Group`)) +
  geom_line() +
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("Date")+
  ylab("£ per FTE")
  
# some weird spikes there - try changing plotting metric

# Lets try comparing doctors
data.1 %>% 
  filter(str_detect(.$`Staff Group`, regex("doctor|consultant|core tra|staff grade|associate sp|specialty reg",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, "EARNINGS")) %>% 
  ggplot(aes(x = date, y = Amount, colour = `Staff Group`)) +
  geom_line() +
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("Date")+
  ylab("£ per FTE")

# try plotting a moving average
# Lets try comparing doctors
data.1 %>% 
  filter(str_detect(.$`Staff Group`, regex("doctor|consultant|core tra|staff grade|associate sp|specialty reg",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, "PER_FTE")) %>% 
  ggplot(aes(x = date, y = Amount, colour = `Staff Group`)) +
  geom_line()+
  geom_smooth()+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("Date")+
  ylab("£ per FTE")

#import data on average monthly pay for everyone else
ukpay <- read_excel("earn01feb2023.xls", 
                    sheet = "1. AWE Total Pay", col_names = FALSE, 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"), skip = 8)

slimmed <- ukpay %>%
  select(date = ...1, Amount = ...2)%>%
  mutate(`Staff Group` = "UK Average wage",
         `Payment type` = "EARNINGS",
         date = ymd(date)) %>% 
  filter(date > dmy("30/09/2009"))

#import inflation data
cpih <- read_csv("cpih.csv", col_names = FALSE, 
                 skip = 443)%>%
  rename(Date = X1, Amount = X2)


cpih <- cpih %>%
  mutate(date = ym(Date),
         `Staff Group` = "CPI-H",
        `Payment type` = "EARNINGS") %>% 
  filter(date > dmy("30/09/2009"))

#house price data - I fixed the dates in excel first as it was just easier 
houseprices <- read_excel("houseprices.xls", 
                          sheet = "Sheet1") %>% 
  mutate(date = ymd(Date),
         `Staff Group` = "Average English House Price",
         `Payment type` = "EARNINGS") %>% 
  filter(date > dmy("30/09/2009")) %>% 
  select(-Date)

#healthcare spending data
nhscosts <- read_excel("2020referencetablesv2.0.xlsx", 
                       sheet = "1a", skip = 3, n_max = 6)

nhscosts %>%
  head()

nhscosts.1 <-nhscosts %>%
  select(-starts_with("ICHA")) %>% 
  gather(year, Amount, -starts_with("Fina") ) %>% 
  rename(`Staff Group` = `Financing Scheme`) %>% 
  #assume the spending ends at the end of the year
  mutate(date = (ymd(year, truncated = 2L) + years(1)),
         `Payment type` = "EARNINGS") %>% 
  filter(date > dmy("30/09/2009")) 

#pensions - couldn't find on ONS so copied it from https://adviser.royallondon.com/technical-central/rates-and-factors/state-pension/basic-state-pension-rates/
pensions <- read_csv("pensions.csv") %>% 
  mutate(date = dmy(Date),
        `Staff Group` = "State Pension",
        `Payment type` = "EARNINGS") %>% 
  filter(date > dmy("30/09/2009")) %>% 
  select(-Date)

# Join it all together

data1.1 <- data.1 %>%
  bind_rows(slimmed, cpih, houseprices, nhscosts.1, pensions)

data1.1 %>% 
  filter(str_detect(`Staff Group`, regex("Pen")))


  

# rearrange data to rebase at first marker at 100

data.2 <- data1.1 %>% 
  group_by(`Staff Group`,`Payment type`) %>%
  arrange(date) %>%
  mutate(first_time = Amount[1]) %>%
  mutate(change_from_base = 100 *Amount/first_time)

# now plot by change from base
data.2 %>% 
  ungroup() %>% 
  filter(str_detect(.$`Staff Group`, regex("consultant|Year 1|specialty reg|core|visit|UK|cpi|price|gov|pens",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, "EARNINGS"), ignore_case = TRUE) %>% 
  ggplot(aes(x = date, y = change_from_base, colour = `Staff Group`)) +
  #geom_point()+
  geom_smooth(se = FALSE)+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("Date")+
  ylab("Earnings, 100% benchmarked to 30th Sep 2009")+
  scale_x_date(breaks = "2 year") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", face = "bold", size = 12),
        legend.text = element_text(family = "sans", face = "italic", size = 11),
        legend.title = element_text(family = "sans", face = c("bold"), size = 12),
        axis.text = element_text(family = "sans", size = 11)) +
 scale_colour_jco()

data.1 %>% 
  ungroup() %>% 
  filter(str_detect(.$`Staff Group`, regex("UK",ignore_case = TRUE ))) %>% 
  #filter(str_detect(.$`Payment type`, "EARNINGS"), ignore_case = TRUE) %>% 
  ggplot(aes(x = date, y = change_from_base, colour = `Staff Group`)) +
  #geom_point()+
  geom_smooth(se = FALSE)+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("Date")+
  ylab("Earnings, 100 benchmarked to 30th Sep 2009")+
  scale_x_date(breaks = "2 year") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", face = "bold", size = 12),
        legend.text = element_text(family = "sans", face = "italic", size = 11),
        legend.title = element_text(family = "sans", face = c("bold"), size = 12),
        axis.text = element_text(family = "sans", size = 11)) +
  scale_colour_lancet()

data.2%>%
  ungroup() %>% 
  select(date) %>%
  arrange() %>% 
  head()
