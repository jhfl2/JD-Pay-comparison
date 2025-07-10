library(lubriDATE)
library(tidyverse)
library(tidyquant)
library(ggsci)
library(ggthemes)

data <- read_csv("Monthly Earnings Estimates to March 2025, NHS Trusts and other core orgs, CSV.csv")
#data <-read_csv("NHS Staff Annual Earnings Estimates to March 2025, in NHS Trusts and other core organisations in England, Provisional Statistics CSV text file.csv")
# Identify column names 
data %>%
  colnames()
# Identify STAFF_GROUPs
data %>%
  select(`STAFF_GROUP`)%>%
  unique()%>%
  print(n=27)

data %>% 
  select(`STAFF_GROUP_ORDER`) %>% 
  unique()
# Identify PAYMENT_TYPEs
data %>% 
  select(`PAYMENT_TYPE`) %>% 
  unique()

# look at DATE format
data %>% 
  select(`DATE`) %>% 
  head()

# Convert DATE to DATE and identify junior doctors

data.1.0 <- data %>% 
  mutate(DATE = ymd(DATE),
         juniordoc = str_detect(.$`STAFF_GROUP`, regex("Foundation|core|specialty reg", ignore_case = TRUE)))

#check it worked
data.1.0 %>% 
  filter(juniordoc == TRUE) %>%
  select(`STAFF_GROUP`) %>% 
  unique()

# just juniors
data.jd <- data.1.0 %>% 
  filter(juniordoc == TRUE) %>% 
  group_by(DATE, `PAYMENT_TYPE`) %>% 
  mutate(tot = AMOUNT * `SAMPLE_SIZE`) %>% 
  summarise(tot = sum(tot),
            `SAMPLE_SIZE` = sum(`SAMPLE_SIZE`)) %>% 
  mutate(AMOUNT = tot / `SAMPLE_SIZE`,
         `STAFF_GROUP` = "Junior Doctors") 

data.1 <- data.1.0 %>% 
  bind_rows(data.jd) %>% 
  select( -`STAFF_GROUP_ORDER`,-juniordoc)

  
  
#lets try plotting to see how it goes - start with all staff basic pay per fte
data.1 %>% 
  filter(str_detect(.$`STAFF_GROUP`, "All staff")) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS")) %>% 
  ggplot(aes(x = DATE, y = AMOUNT)) +
  geom_point() +
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("DATE")+
  ylab("£ per FTE")

# Lets try comparing doctors
data.1 %>% 
  filter(str_detect(.$`STAFF_GROUP`, regex("doctor|consultant|core tra|staff grade|associate sp|specialty reg",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS")) %>% 
  ggplot(aes(x = DATE, y = AMOUNT, colour = `STAFF_GROUP`)) +
  geom_line() +
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("DATE")+
  ylab("£ per FTE")

data.1 %>% 
  filter(str_detect(.$`STAFF_GROUP`, regex("junior|consultant|Found|Core|reg",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS")) %>% 
  ggplot(aes(x = DATE, y = AMOUNT, colour = `STAFF_GROUP`)) +
  geom_line() +
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("DATE")+
  ylab("Earnings") +
  scale_colour_jco()
  
# some weird spikes there - try changing plotting metric

# Lets try comparing doctors
data.1 %>% 
  filter(str_detect(.$`STAFF_GROUP`, regex("doctor|consultant|core tra|staff grade|associate sp|specialty reg",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS")) %>% 
  ggplot(aes(x = DATE, y = AMOUNT, colour = `STAFF_GROUP`)) +
  geom_line() +
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("DATE")+
  ylab("£ per FTE")

# try plotting a moving average
# Lets try comparing doctors
data.1 %>% 
  filter(str_detect(.$`STAFF_GROUP`, regex("doctor|consultant|core tra|staff grade|associate sp|specialty reg",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, "PER_FTE")) %>% 
  ggplot(aes(x = DATE, y = AMOUNT, colour = `STAFF_GROUP`)) +
  geom_line()+
  geom_smooth()+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("DATE")+
  ylab("£ per FTE")

#import data on average monthly pay for everyone else
library(readxl)
ukpay <- read_excel("earn01jun2025.xls", 
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
                                  "numeric"), skip = 9)

ukpay
slimmed <- ukpay %>%
  select(DATE = ...1, AMOUNT = ...2)%>%
  mutate(`STAFF_GROUP` = "UK Average wage",
         FTE = 1L,
         `PAYMENT_TYPE` = "EARNINGSFTE",
         DATE = ymd(DATE)) %>% 
  filter(DATE > dmy("30/09/2009"))

slimmed
#import inflation data
cpih <- read_csv("cpih_updated.csv", col_names = FALSE, 
                 skip = 443)%>%
  rename(DATE = X1, AMOUNT = X2)


cpih <- cpih %>%
  mutate(DATE = ym(DATE),
         `STAFF_GROUP` = "CPI-H",
         FTE = 1L,
        `PAYMENT_TYPE` = "EARNINGSFTE") %>% 
  filter(DATE > dmy("30/09/2009"))

cpih.period <- cpih %>% 
  mutate(cpiperiod = interval(start = DATE, end = lead(DATE)))

#house price data - I fixed the DATEs in excel first as it was just easier 
houseprices <- read_excel("houseprices.xls", 
                          sheet = "Sheet1") %>% 
  mutate(DATE = ymd(DATE),
         `STAFF_GROUP` = "Average English House Price",
         FTE = 1L,
         `PAYMENT_TYPE` = "EARNINGSFTE") %>% 
  filter(DATE > dmy("30/09/2009")) %>% 
  select(-DATE)

#healthcare spending data
nhscosts <- read_excel("2020referencetablesv2.0.xlsx", 
                       sheet = "1a", skip = 3, n_max = 6)

nhscosts %>%
  head()

nhscosts.1 <-nhscosts %>%
  select(-starts_with("ICHA")) %>% 
  gather(year, AMOUNT, -starts_with("Fina") ) %>% 
  rename(`STAFF_GROUP` = `Financing Scheme`) %>% 
  #assume the spending ends at the end of the year
  mutate(DATE = (ymd(year, truncated = 2L) + years(1)),
         `PAYMENT_TYPE` = "EARNINGSFTE",
         FTE = 1L,) %>% 
  filter(DATE > dmy("30/09/2009")) 

#pensions - couldn't find on ONS so copied it from https://adviser.royallondon.com/technical-central/rates-and-factors/state-pension/basic-state-pension-rates/
pensions <- read_csv("pensions.csv") %>% 
  mutate(DATE = dmy(DATE),
        `STAFF_GROUP` = "State Pension",
        `PAYMENT_TYPE` = "EARNINGSFTE",
        FTE = 1L,) %>% 
  filter(DATE > dmy("30/09/2009")) %>% 
  select(-DATE)

#gmc data - I had to manually scrape from GMC website as they are useless
GMC_DATA <- read_excel("GMC DATA.xlsx") %>% 
  mutate(DATE = ymd(DATE),
           `STAFF_GROUP` = "GMC Survey",
         `PAYMENT_TYPE` = "EARNINGSFTE",
         FTE = 1L) %>% 
  rename(AMOUNT = overall)
  
# student loans data
student_loan <- read_csv("student loan.csv") %>% 
  mutate(DATE = dmy(DATE),
         `STAFF_GROUP` = "Total Annual Student Loan debt for all current students",
         `PAYMENT_TYPE` = "EARNINGSFTE",
         FTE = 1L)


# Join it all together
data1.1 <- data.1 %>%
  bind_rows(slimmed, cpih)

data1.1 %>% 
  filter(str_detect(`STAFF_GROUP`, regex("Pen")))



  

# rearrange data to rebase at first marker at 100


cpi_adjust <- function(DATES) {
  matched_rows <- cpih.period[match(DATES, cpih.period$cpiperiod), "AMOUNT"]
  if (any(!is.na(matched_rows))) {
    return(as.numeric(matched_rows))
  } else {
    return(rep(NA, length(DATES)))
  }
}



# Create a function to adjust values for inflation
cpi_adjust <- function(x) {
  row <- cpih[which.min(abs(x - cpih$DATE)), "AMOUNT"]
  if (!is.na(row)) {
    return(as.numeric(row))
  } else {
    return(NULL)
  }
}

sapply(cpi_adjust(dmy("10/10/2018")))

# Apply the inflation adjustment to each row in the main dataframe
data1.1$cpiattime <- sapply(data1.1$DATE, cpi_adjust)

cpi_adjust(dmy("10/10/2016"))

data.2 <- data1.1 %>% 
  mutate(cpiadjusted = AMOUNT / cpiattime ) %>% 
  bind_rows(GMC_DATA) %>% 
  group_by(`STAFF_GROUP`,`PAYMENT_TYPE`) %>%
  arrange(DATE) %>%
  mutate(first_time = AMOUNT[1]) %>%
  mutate(change_from_base = 100 *AMOUNT/first_time) %>% 
  mutate(first_time_adjusted = cpiadjusted[1]) %>%
  mutate(change_from_base2 = 100 *cpiadjusted/first_time_adjusted)

head(data.2)

# now plot by change from base
data.2 %>% 
  ungroup() %>% 
  filter(str_detect(.$`STAFF_GROUP`, regex("consultant|Year 1|specialty reg|junior|core|visit|UK|cpi|price|gov|pens",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, regex("fTE", ignore_case = TRUE))) %>% 
  ggplot(aes(x = DATE, y = change_from_base2, colour = `STAFF_GROUP`)) +
  #geom_point()+
  geom_smooth(se = FALSE, span = 0.4, size = 1.8)+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("DATE")+
  ylab("Earnings, 100% benchmarked to 30th Sep 2009")+
  scale_x_date(breaks = "2 year") +
  #ylim(c(80,120))+
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", face = "bold", size = 12),
        legend.text = element_text(family = "sans", face = "italic", size = 11),
        legend.title = element_text(family = "sans", face = c("bold"), size = 12),
        axis.text = element_text(family = "sans", size = 11)) +
 scale_colour_jco()

data.2 %>% 
  ungroup() %>% 
  filter(str_detect(.$`STAFF_GROUP`, regex("GMC",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, regex("fTE", ignore_case = TRUE))) %>% 
  ggplot(aes(x = DATE, y = change_from_base2, colour = `STAFF_GROUP`)) +
  #geom_point()+
  geom_smooth(se = FALSE)+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("DATE")+
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
  select(DATE, starts_with("BURN"), AMOUNT) %>% 
  gather("question", "response", -DATE) %>% 
  filter(str_detect(.$question, regex("Amo", ignore_case = TRUE))) %>% 
  ggplot(aes(x=DATE, y= response, colour = question))+
  geom_point()+
  geom_smooth(se = FALSE, size = 2)+
  #add some code just to tidy up the graph appearance
  theme_few()+
  xlab("DATE")+
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
  select(DATE, starts_with("BURN"), AMOUNT) %>% 
  gather("question", "response", -DATE) %>% 
  filter(str_detect(.$question, regex("burnout", ignore_case = TRUE)),
         DATE > ymd("2019/01/01")) %>% 
  ggplot(aes(x=DATE, y= response, fill = factor(question, levels= c("BURNOUT_LOW", "BURNOUT_MOD", "BURNOUT_HIGH"))))+
  geom_col()+
  #geom_smooth(se = FALSE)+
  #add some code just to tidy up the graph appearance
  theme_few()+
  xlab("DATE")+
  ylab("Percent of Trainees")+
  #scale_x_date(breaks = "2 year") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", size = 12),
        legend.text = element_text(family = "sans", face = "italic", size = 11),
        legend.title = element_text(family = "sans", size = 12),
        axis.text = element_text(family = "sans", size = 11)) +
  labs(caption = "Source: GMC Survey") +
  #scale_y_discrete(limits = positions)+
  scale_fill_jco(name = "Risk of Burnout",
                 labels = c("Low", "Moderate", "High"))

student_loan %>% 
  ggplot(aes(x = DATE, y = AMOUNT))+
  geom_point()+
  geom_point()+
  #geom_line()+
  #stat_smooth(se = FALSE)+
  geom_smooth(se = FALSE, size = 2, span = .5)+
  #add some code just to tidy up the graph appearance
  theme_few()+
  xlab("DATE")+
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
  filter(str_detect(.$`STAFF_GROUP`, regex("UK",ignore_case = TRUE ))) %>% 
  #filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS"), ignore_case = TRUE) %>% 
  ggplot(aes(x = DATE, y = change_from_base, colour = `STAFF_GROUP`)) +
  #geom_point()+
  geom_smooth(se = FALSE)+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("DATE")+
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
  select(DATE) %>%
  arrange() %>% 
  head()


data.2 %>% 
  ungroup() %>% 
  filter(str_detect(.$`STAFF_GROUP`, regex("core|Year 2|house pr|UK|cons",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS"), ignore_case = TRUE) %>% 
  filter(change_from_base2 > 80) %>%
  ggplot(aes(x = DATE, y = change_from_base2, colour = `STAFF_GROUP`)) +
  geom_point()+
  #geom_line()+
  #stat_smooth(se = FALSE)+
  geom_smooth(se = FALSE, size = 2, span = .5)+
  geom_smooth(data = data.2 %>% ungroup () %>%  filter(str_detect(`STAFF_GROUP`, regex("Core|Year 2"))) %>% 
                filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS"), change_from_base2 > 80
                      ), size = 2.8, se= FALSE, span = .15)+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("DATE")+
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
  filter(str_detect(.$`STAFF_GROUP`, regex("core",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS"), ignore_case = TRUE) %>% 
  print(n=50)

data.2 %>% 
  ungroup() %>% 
  filter(str_detect(.$`STAFF_GROUP`, regex("cons|junior|house pr|UK|pension",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS"), ignore_case = TRUE) %>% 
  filter(change_from_base2 > 60) %>%
  ggplot(aes(x = DATE, y = change_from_base2, colour = `STAFF_GROUP`)) +
  #stat_smooth(se = FALSE)+
  geom_smooth(se = FALSE, size = 2, span = .15)+
  #add some code just to tidy up the graph appearance
  theme_few()+
  xlab("DATE")+
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
  filter(str_detect(.$`STAFF_GROUP`, regex("jun|pensi|house pr|UK|gov",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS"), ignore_case = TRUE) %>% 
  filter(change_from_base2 > 60) %>%
  ggplot(aes(x = DATE, y = change_from_base2, colour = `STAFF_GROUP`)) +
  geom_point()+
  geom_smooth(se = FALSE, span = 0.25)+
  #stat_smooth(se = FALSE)+
  #geom_smooth(se = FALSE, size = 3, span = .15)+
  #geom_smooth(data = data.2 %>% ungroup () %>%  filter(str_detect(`STAFF_GROUP`, regex("Cons|Year 2"))) %>% 
   #             filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS"), change_from_base2 > 60
    #            ), size = 2.8, se= FALSE, span = .15)+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("DATE")+
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
  filter(str_detect(.$`STAFF_GROUP`, regex("reg|core|fou|cons|UK",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS"), ignore_case = TRUE) %>% 
  filter(change_from_base2 > 60) %>%
  ggplot(aes(x = DATE, y = change_from_base2, colour = `STAFF_GROUP`)) +
    geom_smooth(se = FALSE, span = 0.15, size = 2)+
  #stat_smooth(se = FALSE)+
  #geom_smooth(se = FALSE, size = 3, span = .15)+
  #geom_smooth(data = data.2 %>% ungroup () %>%  filter(str_detect(`STAFF_GROUP`, regex("Cons|Year 2"))) %>% 
  #             filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS"), change_from_base2 > 60
  #            ), size = 2.8, se= FALSE, span = .15)+
  #add some code just to tidy up the graph appearance
  theme_light()+
  xlab("DATE")+
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
  mutate(DATE = ymd(DATE)) %>% 
  group_by(DATE,Grade) %>% 
  summarise(FTE = sum(FTE)) %>% 
  ungroup() %>% 
  mutate(juniordoc = str_detect(.$`Grade`, regex("Foundation|core|specialty reg", ignore_case = TRUE)))

jd_workforce <- workforce_data %>% 
  filter(juniordoc == TRUE) %>% 
  group_by(DATE) %>% 
  summarise(FTE = sum(FTE)) %>% 
  mutate(Grade = "Junior Doctors")

workforce_data.1 <- workforce_data %>% 
  bind_rows(jd_workforce) %>% 
  rename(`STAFF_GROUP` = Grade)

workforce_data.1 %>%
  select(`STAFF_GROUP`) %>% 
  unique()

workforce_data.1%>% 
  filter(str_detect(.$`STAFF_GROUP`, regex("cons|core|found|reg",ignore_case = TRUE ))) %>% 
  ggplot(aes(x = DATE, y = FTE, colour = `STAFF_GROUP`))+
  geom_point()+
  geom_smooth(se = FALSE) +
  theme_light()+
  xlab("DATE")+
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
  mutate(totcost = AMOUNT * FTE) %>% 
  mutate(cpiadjusted = AMOUNT / cpiattime,
         totcpiadjust = totcost / cpiattime) %>% 
  group_by(`STAFF_GROUP`,`PAYMENT_TYPE`) %>%
  arrange(DATE) %>%
  mutate(first_time = AMOUNT[1]) %>%
  mutate(change_from_base = 100 *AMOUNT/first_time) %>% 
  mutate(first_time_adjusted = cpiadjusted[1]) %>%
  mutate(change_from_base2 = 100 *cpiadjusted/first_time_adjusted) %>% 
  mutate(first_time_tot_adjusted = totcpiadjust[1]) %>% 
  mutate(change_from_base3 = 100 *totcpiadjust/first_time_tot_adjusted) %>% 
  mutate(first_time_tot = totcost[1] ) %>% 
  mutate(change_from_base4 = 100*totcost / first_time_tot)

data.2.1 %>% 
  ungroup() %>% 
  filter(str_detect(.$`STAFF_GROUP`, regex("junior|cons|gov",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS"), ignore_case = TRUE) %>% 
  filter(change_from_base3 > 60) %>%
  ggplot(aes(x = DATE, y = change_from_base4, colour = `STAFF_GROUP`)) +
  geom_point()+
  geom_smooth(se = FALSE, size = 2.5) +
  theme_light()+
  xlab("DATE")+
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
 filter(str_detect(.$`STAFF_GROUP`, regex("junior",ignore_case = TRUE )))

workforce_data.1 %>% 
  left_join(data1.1)

data.2.1 %>% 
  ungroup() %>% 
  filter(str_detect(.$`STAFF_GROUP`, regex("junior|gov",ignore_case = TRUE ))) %>% 
  filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS"), ignore_case = TRUE) %>% 
  filter(change_from_base2 > 60) %>%
  ggplot(aes(x = DATE, y = change_from_base3, colour = `STAFF_GROUP`)) +
  geom_point()+
  geom_smooth(se = FALSE, span = 0.25)+
  #stat_smooth(se = FALSE)+
  #geom_smooth(se = FALSE, size = 3, span = .15)+
  #geom_smooth(data = data.2 %>% ungroup () %>%  filter(str_detect(`STAFF_GROUP`, regex("Cons|Year 2"))) %>% 
  #             filter(str_detect(.$`PAYMENT_TYPE`, "EARNINGS"), change_from_base2 > 60
  #            ), size = 2.8, se= FALSE, span = .15)+
  #add some code just to tidy up the graph appearance
  theme_tufte()+
  xlab("DATE")+
  ylab("Earnings CPIH adjusted, 100% benchmarked to 30th Sep 2009")+
  scale_x_date(breaks = "3 year", date_labels = "%b %Y") +
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.title = element_text(family = "sans", face = "bold", size = 14),
        legend.text = element_text(family = "sans", face = "italic", size = 14),
        legend.title = element_text(family = "sans", face = c("bold"), size = 15),
        axis.text = element_text(family = "sans", size = 13)) +
 
  scale_colour_lancet(name = "Group", labels = c("Government Healthcare Spending", "Total Junior Doctor Cost"))

