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

data %>% 
  select(`Staff Group Sort Order`) %>% 
  unique()
# Identify payment types
data %>% 
  select(`Payment type`) %>% 
  unique()

# look at date format
data %>% 
  select(`Date`) %>% 
  head()

# Convert date to date and identify junior doctors

data.1.0 <- data %>% 
  mutate(date = dmy(Date),
         juniordoc = str_detect(.$`Staff Group`, regex("Foundation|core|specialty reg", ignore_case = TRUE)))

#check it worked
data.1.0 %>% 
  filter(juniordoc == TRUE) %>%
  select(`Staff Group`) %>% 
  unique()

# just juniors
data.jd <- data.1.0 %>% 
  filter(juniordoc == TRUE) %>% 
  group_by(date, `Payment type`) %>% 
  mutate(tot = Amount * `Sample Size`) %>% 
  summarise(tot = sum(tot),
            `Sample Size` = sum(`Sample Size`)) %>% 
  mutate(Amount = tot / `Sample Size`,
         `Staff Group` = "Junior Doctors") 

data.1 <- data.1.0 %>% 
  bind_rows(data.jd) %>% 
  select(-Date, -`Staff Group Sort Order`,-juniordoc)

  
  
#lets try plotting to see how it goes - start with all staff basic pay per fte
data.1 %>% 
  filter(str_detect(.$`Staff Group`, "All staff")) %>% 
  filter(str_detect(.$`Payment type`, "EARNINGS")) %>% 
  ggplot(aes(x = date, y = Amount)) +
  geom_point() +
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("Date")+
  ylab("£ per FTE")

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

data.1 %>% 
  filter(str_detect(.$`Staff Group`, regex("junior|consultant|Found|Core|reg",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, "EARNINGS")) %>% 
  ggplot(aes(x = date, y = Amount, colour = `Staff Group`)) +
  geom_line() +
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("Date")+
  ylab("Earnings") +
  scale_colour_jco()
  
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
library(readxl)
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
         FTE = 1L,
         `Payment type` = "EARNINGSFTE",
         date = ymd(date)) %>% 
  filter(date > dmy("30/09/2009"))

#import inflation data
cpih <- read_csv("cpih.csv", col_names = FALSE, 
                 skip = 443)%>%
  rename(Date = X1, Amount = X2)


cpih <- cpih %>%
  mutate(date = ym(Date),
         `Staff Group` = "CPI-H",
         FTE = 1L,
        `Payment type` = "EARNINGSFTE") %>% 
  filter(date > dmy("30/09/2009"))

cpih.period <- cpih %>% 
  mutate(cpiperiod = interval(start = date, end = lead(date)))

#house price data - I fixed the dates in excel first as it was just easier 
houseprices <- read_excel("houseprices.xls", 
                          sheet = "Sheet1") %>% 
  mutate(date = ymd(Date),
         `Staff Group` = "Average English House Price",
         FTE = 1L,
         `Payment type` = "EARNINGSFTE") %>% 
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
         `Payment type` = "EARNINGSFTE",
         FTE = 1L,) %>% 
  filter(date > dmy("30/09/2009")) 

#pensions - couldn't find on ONS so copied it from https://adviser.royallondon.com/technical-central/rates-and-factors/state-pension/basic-state-pension-rates/
pensions <- read_csv("pensions.csv") %>% 
  mutate(date = dmy(Date),
        `Staff Group` = "State Pension",
        `Payment type` = "EARNINGSFTE",
        FTE = 1L,) %>% 
  filter(date > dmy("30/09/2009")) %>% 
  select(-Date)

#gmc data - I had to manually scrape from GMC website as they are useless
GMC_DATA <- read_excel("GMC DATA.xlsx") %>% 
  mutate(date = ymd(date),
           `Staff Group` = "GMC Survey",
         `Payment type` = "EARNINGSFTE",
         FTE = 1L) %>% 
  rename(Amount = overall)
  
# student loans data
student_loan <- read_csv("student loan.csv") %>% 
  mutate(date = dmy(date),
         `Staff Group` = "Total Annual Student Loan debt for all current students",
         `Payment type` = "EARNINGSFTE",
         FTE = 1L)


# Join it all together
data1.1 <- data.1 %>%
  left_join(workforce_data.1) %>% 
  bind_rows(slimmed, cpih, houseprices, nhscosts.1, pensions, student_loan)

data1.1 %>% 
  filter(str_detect(`Staff Group`, regex("Pen")))



  

# rearrange data to rebase at first marker at 100


cpi_adjust <- function(dates) {
  matched_rows <- cpih.period[match(dates, cpih.period$cpiperiod), "Amount"]
  if (any(!is.na(matched_rows))) {
    return(as.numeric(matched_rows))
  } else {
    return(rep(NA, length(dates)))
  }
}



# Create a function to adjust values for inflation
cpi_adjust <- function(x) {
  row <- cpih[which.min(abs(x - cpih$date)), "Amount"]
  if (!is.na(row)) {
    return(as.numeric(row))
  } else {
    return(NULL)
  }
}

sapply(cpi_adjust(dmy("10/10/2018")))

# Apply the inflation adjustment to each row in the main dataframe
data1.1$cpiattime <- sapply(data1.1$date, cpi_adjust)

cpi_adjust(dmy("10/10/2016"))

data.2 <- data1.1 %>% 
  mutate(cpiadjusted = Amount / cpiattime ) %>% 
  bind_rows(GMC_DATA) %>% 
  group_by(`Staff Group`,`Payment type`) %>%
  arrange(date) %>%
  mutate(first_time = Amount[1]) %>%
  mutate(change_from_base = 100 *Amount/first_time) %>% 
  mutate(first_time_adjusted = cpiadjusted[1]) %>%
  mutate(change_from_base2 = 100 *cpiadjusted/first_time_adjusted)

head(data.2)

# now plot by change from base
data.2 %>% 
  ungroup() %>% 
  filter(str_detect(.$`Staff Group`, regex("consultant|Year 1|specialty reg|junior|core|visit|UK|cpi|price|gov|pens",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, regex("fTE", ignore_case = TRUE))) %>% 
  ggplot(aes(x = date, y = change_from_base2, colour = `Staff Group`)) +
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

data.2 %>% 
  ungroup() %>% 
  filter(str_detect(.$`Staff Group`, regex("GMC",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, regex("fTE", ignore_case = TRUE))) %>% 
  ggplot(aes(x = date, y = change_from_base2, colour = `Staff Group`)) +
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
  labs(caption = "GMC Survery")+
  scale_colour_jco()

GMC_DATA %>% 
  select(date, starts_with("BURN"), Amount) %>% 
  gather("question", "response", -date) %>% 
  filter(str_detect(.$question, regex("Amo", ignore_case = TRUE))) %>% 
  ggplot(aes(x=date, y= response, colour = question))+
  geom_point()+
  geom_smooth(se = FALSE, size = 2)+
  #add some code just to tidy up the graph appearance
  theme_few()+
  xlab("Date")+
  ylab("Overall Trainee Satisfaction on GMC survery (%)")+
  scale_x_date(breaks = "2 year", date_labels = "%b %Y") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", size = 12),
        legend.text = element_text(family = "sans", face = "italic", size = 11),
        legend.title = element_text(family = "sans", size = 12),
        axis.text = element_text(family = "sans", size = 11),
        legend.position = "None") +
  labs(caption = "Source: GMC Survey") +
  scale_colour_jco()

positions <- c("BURNOUT_HIGH", "BURNOUT_MOD", "BURNOUT_LOW")

GMC_DATA %>% 
  select(date, starts_with("BURN"), Amount) %>% 
  gather("question", "response", -date) %>% 
  filter(str_detect(.$question, regex("burnout", ignore_case = TRUE)),
         date > ymd("2019/01/01")) %>% 
  ggplot(aes(x=date, y= response, fill = factor(question, levels= c("BURNOUT_LOW", "BURNOUT_MOD", "BURNOUT_HIGH"))))+
  geom_col()+
  #geom_smooth(se = FALSE)+
  #add some code just to tidy up the graph appearance
  theme_few()+
  xlab("Date")+
  ylab("Percent of Trainees")+
  #scale_x_date(breaks = "2 year") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", size = 12),
        legend.text = element_text(family = "sans", face = "italic", size = 11),
        legend.title = element_text(family = "sans", size = 12),
        axis.text = element_text(family = "sans", size = 11)) +
  labs(caption = "Source: GMC Survey") +
  #scale_y_discrete(limits = positions)+
  scale_fill_jco(name = "Degree of Burnout",
                 labels = c("Low", "Moderate", "High"))

student_loan %>% 
  ggplot(aes(x = date, y = Amount))+
  geom_point()+
  geom_point()+
  #geom_line()+
  #stat_smooth(se = FALSE)+
  geom_smooth(se = FALSE, size = 2, span = .5)+
  #add some code just to tidy up the graph appearance
  theme_few()+
  xlab("Date")+
  ylab("Total Average Student Loan bowrrowing in £ GBP for all students")+
  scale_x_date(breaks = "3 year", date_labels = "%b %Y") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", size = 14),
        legend.text = element_text(family = "sans", face = "italic", size = 14),
        legend.title = element_text(family = "sans", size = 15),
        axis.text = element_text(family = "sans", size = 13)) +
  labs(caption = "Source: House of Commons Library")
  scale_colour_jco()
  

  
  ungroup() %>% 
  filter(str_detect(.$`Staff Group`, regex("UK",ignore_case = TRUE ))) %>% 
  #filter(str_detect(.$`Payment type`, "EARNINGS"), ignore_case = TRUE) %>% 
  ggplot(aes(x = date, y = change_from_base, colour = `Staff Group`)) +
  #geom_point()+
  geom_smooth(se = FALSE)+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("Date")+
  ylab("Earnings, 100 benchmarked to 30th Sep 2009 and adjusted for inflation (CPIH)")+
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


data.2 %>% 
  ungroup() %>% 
  filter(str_detect(.$`Staff Group`, regex("core|Year 2|house pr|UK|cons",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, "EARNINGS"), ignore_case = TRUE) %>% 
  filter(change_from_base2 > 80) %>%
  ggplot(aes(x = date, y = change_from_base2, colour = `Staff Group`)) +
  geom_point()+
  #geom_line()+
  #stat_smooth(se = FALSE)+
  geom_smooth(se = FALSE, size = 2, span = .5)+
  geom_smooth(data = data.2 %>% ungroup () %>%  filter(str_detect(`Staff Group`, regex("Core|Year 2"))) %>% 
                filter(str_detect(.$`Payment type`, "EARNINGS"), change_from_base2 > 80
                      ), size = 2.8, se= FALSE, span = .15)+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("Date")+
  ylab("Earnings,benchmarked to 30th Sep 2009 and adjusted for inflation (CPIH)")+
  scale_x_date(breaks = "3 year", date_labels = "%b %Y") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", face = "bold", size = 14),
        legend.text = element_text(family = "sans", face = "italic", size = 14),
        legend.title = element_text(family = "sans", face = c("bold"), size = 15),
        axis.text = element_text(family = "sans", size = 13)) +
  scale_colour_lancet(name = "Group", 
                      #labels = c("English House Prices", "Core Training Doctors (PGY 3+)", "FY2 Doctors (PGY 2)", "UK Average Wage")
                      )


data.2 %>% 
  ungroup() %>% 
  filter(str_detect(.$`Staff Group`, regex("core",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, "EARNINGS"), ignore_case = TRUE) %>% 
  print(n=50)

data.2 %>% 
  ungroup() %>% 
  filter(str_detect(.$`Staff Group`, regex("cons|junior|house pr|UK|pension",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, "EARNINGS"), ignore_case = TRUE) %>% 
  filter(change_from_base2 > 60) %>%
  ggplot(aes(x = date, y = change_from_base2, colour = `Staff Group`)) +
  #stat_smooth(se = FALSE)+
  geom_smooth(se = FALSE, size = 2, span = .15)+
  #add some code just to tidy up the graph appearance
  theme_few()+
  xlab("Date")+
  ylab("Earnings CPIH adjusted, 100% benchmarked to 30th Sep 2009")+
  scale_x_date(breaks = "3 year", date_labels = "%b %Y") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", size = 14),
        legend.text = element_text(family = "sans", face = "italic", size = 14),
        legend.title = element_text(family = "sans", size = 15),
        axis.text = element_text(family = "sans", size = 13)) +
  labs(caption = "Sources: NHS Digital, ONS, UK Gov")+
  scale_colour_jco(name = "", 
                   labels = c("English House Prices", "Consultant Doctors", "Junior Doctors", "State Pension", "UK Average Wage")
                   )

data.2 %>% 
  ungroup() %>% 
  filter(str_detect(.$`Staff Group`, regex("jun|pensi|house pr|UK|gov",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, "EARNINGS"), ignore_case = TRUE) %>% 
  filter(change_from_base2 > 60) %>%
  ggplot(aes(x = date, y = change_from_base2, colour = `Staff Group`)) +
  geom_point()+
  geom_smooth(se = FALSE, span = 0.25)+
  #stat_smooth(se = FALSE)+
  #geom_smooth(se = FALSE, size = 3, span = .15)+
  #geom_smooth(data = data.2 %>% ungroup () %>%  filter(str_detect(`Staff Group`, regex("Cons|Year 2"))) %>% 
   #             filter(str_detect(.$`Payment type`, "EARNINGS"), change_from_base2 > 60
    #            ), size = 2.8, se= FALSE, span = .15)+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("Date")+
  ylab("Earnings CPIH adjusted, 100% benchmarked to 30th Sep 2009")+
  scale_x_date(breaks = "3 year", date_labels = "%b %Y") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", face = "bold", size = 14),
        legend.text = element_text(family = "sans", face = "italic", size = 14),
        legend.title = element_text(family = "sans", face = c("bold"), size = 15),
        axis.text = element_text(family = "sans", size = 13)) +
  scale_colour_lancet(name = "Group", 
                      #labels = c("English House Prices", "Consultant Doctors (PGY 9+)", "Government Healthcare Spending", "State Pension", "UK Average Wage")
                      )

data.2 %>% 
  ungroup() %>% 
  filter(str_detect(.$`Staff Group`, regex("reg|core|fou|cons|UK",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, "EARNINGS"), ignore_case = TRUE) %>% 
  filter(change_from_base2 > 60) %>%
  ggplot(aes(x = date, y = change_from_base2, colour = `Staff Group`)) +
    geom_smooth(se = FALSE, span = 0.8, size = 2)+
  #stat_smooth(se = FALSE)+
  #geom_smooth(se = FALSE, size = 3, span = .15)+
  #geom_smooth(data = data.2 %>% ungroup () %>%  filter(str_detect(`Staff Group`, regex("Cons|Year 2"))) %>% 
  #             filter(str_detect(.$`Payment type`, "EARNINGS"), change_from_base2 > 60
  #            ), size = 2.8, se= FALSE, span = .15)+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("Date")+
  ylab("Earnings CPIH adjusted, 100% benchmarked to 30th Sep 2009")+
  scale_x_date(breaks = "3 year", date_labels = "%b %Y") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", face = "bold", size = 14),
        legend.text = element_text(family = "sans", face = "italic", size = 14),
        legend.title = element_text(family = "sans", face = c("bold"), size = 15),
        axis.text = element_text(family = "sans", size = 13)) +
  labs(caption = "Source: NHS Digital & ONS")+
  scale_colour_jco()+
  theme_few()

# Lets import some more data about junior doctor numbers
workforce_data <- read_excel("NHS Workforce Statistics, October 2022 Doctors by Grade and Specialty.xlsx", 
                               sheet = "Data") %>% 
  mutate(date = ymd(Date)) %>% 
  group_by(date,Grade) %>% 
  summarise(FTE = sum(FTE)) %>% 
  ungroup() %>% 
  mutate(juniordoc = str_detect(.$`Grade`, regex("Foundation|core|specialty reg", ignore_case = TRUE)))

jd_workforce <- workforce_data %>% 
  filter(juniordoc == TRUE) %>% 
  group_by(date) %>% 
  summarise(FTE = sum(FTE)) %>% 
  mutate(Grade = "Junior Doctors")

workforce_data.1 <- workforce_data %>% 
  bind_rows(jd_workforce) %>% 
  rename(`Staff Group` = Grade)

workforce_data.1 %>%
  select(`Staff Group`) %>% 
  unique()

workforce_data.1%>% 
  filter(str_detect(.$`Staff Group`, regex("cons|core|found|reg",ignore_case = TRUE ))) %>% 
  ggplot(aes(x = date, y = FTE, colour = `Staff Group`))+
  geom_point()+
  geom_smooth(se = FALSE) +
  theme_light()+
  xlab("Date")+
  ylab("n")+
  ylim(0,75000)+
  scale_x_date(breaks = "3 year", date_labels = "%b %Y") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", face = "bold", size = 14),
        legend.text = element_text(family = "sans", face = "italic", size = 14),
        legend.title = element_text(family = "sans", face = c("bold"), size = 15),
        axis.text = element_text(family = "sans", size = 13)) +
  scale_colour_lancet()


data.2.1 <- data1.1 %>% 
  mutate(totcost = Amount * FTE) %>% 
  mutate(cpiadjusted = Amount / cpiattime,
         totcpiadjust = totcost / cpiattime) %>% 
  group_by(`Staff Group`,`Payment type`) %>%
  arrange(date) %>%
  mutate(first_time = Amount[1]) %>%
  mutate(change_from_base = 100 *Amount/first_time) %>% 
  mutate(first_time_adjusted = cpiadjusted[1]) %>%
  mutate(change_from_base2 = 100 *cpiadjusted/first_time_adjusted) %>% 
  mutate(first_time_tot_adjusted = totcpiadjust[1]) %>% 
  mutate(change_from_base3 = 100 *totcpiadjust/first_time_tot_adjusted) %>% 
  mutate(first_time_tot = totcost[1] ) %>% 
  mutate(change_from_base4 = 100*totcost / first_time_tot)

data.2.1 %>% 
  ungroup() %>% 
  filter(str_detect(.$`Staff Group`, regex("junior|cons|gov",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, "EARNINGS"), ignore_case = TRUE) %>% 
  filter(change_from_base3 > 60) %>%
  ggplot(aes(x = date, y = change_from_base4, colour = `Staff Group`)) +
  geom_point()+
  geom_smooth(se = FALSE, size = 2.5) +
  theme_light()+
  xlab("Date")+
  ylab("Earnings CPIH adjusted, 100% benchmarked to 30th Sep 2009")+
  scale_x_date(breaks = "3 year", date_labels = "%b %Y") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", face = "bold", size = 14),
        legend.text = element_text(family = "sans", face = "italic", size = 14),
        legend.title = element_text(family = "sans", face = c("bold"), size = 15),
        axis.text = element_text(family = "sans", size = 13)) +
  scale_colour_lancet(name = "Group", labels = c( "Total Consultant Doctor Cost", "Government Healthcare Spending", "Total Junior Doctor cost"))


data.2.1 %>% 
  colnames()
data.2.1 %>%
  ungroup() %>% 
 filter(str_detect(.$`Staff Group`, regex("junior",ignore_case = TRUE )))

workforce_data.1 %>% 
  left_join(data1.1)

data.2.1 %>% 
  ungroup() %>% 
  filter(str_detect(.$`Staff Group`, regex("junior|gov",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`Payment type`, "EARNINGS"), ignore_case = TRUE) %>% 
  filter(change_from_base2 > 60) %>%
  ggplot(aes(x = date, y = change_from_base3, colour = `Staff Group`)) +
  geom_point()+
  geom_smooth(se = FALSE, span = 0.25)+
  #stat_smooth(se = FALSE)+
  #geom_smooth(se = FALSE, size = 3, span = .15)+
  #geom_smooth(data = data.2 %>% ungroup () %>%  filter(str_detect(`Staff Group`, regex("Cons|Year 2"))) %>% 
  #             filter(str_detect(.$`Payment type`, "EARNINGS"), change_from_base2 > 60
  #            ), size = 2.8, se= FALSE, span = .15)+
  #add some code just to tidy up the graph appearance
  theme_tufte()+
  xlab("Date")+
  ylab("Earnings CPIH adjusted, 100% benchmarked to 30th Sep 2009")+
  scale_x_date(breaks = "3 year", date_labels = "%b %Y") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", face = "bold", size = 14),
        legend.text = element_text(family = "sans", face = "italic", size = 14),
        legend.title = element_text(family = "sans", face = c("bold"), size = 15),
        axis.text = element_text(family = "sans", size = 13)) +
 
  scale_colour_lancet(name = "Group", labels = c("Government Healthcare Spending", "Total Junior Doctor Cost"))

