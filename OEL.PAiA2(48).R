#Open ended lab 1 programming for AI
#Kainat Moin(23f-AI-48)
#section:A2

#1
install.packages()
library(readr)
library(tidyverse)
customer_churn <- read_csv("customer_churn.csv")
view(customer_churn)
cus_churn<- customer_churn %>% filter(Churn=="Yes")
view(cus_churn)

#2
customer_churn<- customer_churn %>% mutate(ChargeGap=TotalCharges - (MonthlyCharges * Tenure),na.rm=TRUE)
view(customer_churn)

#3
long_active_cus <- customer_churn %>% filter(Tenure>24 & Churn=="No")
view(long_active_cus)

#4
avg_monthcharges_contarct <- customer_churn %>% group_by(ContractType) %>% summarize(Average_monthlycharge=mean(MonthlyCharges))
view(avg_monthcharges_contarct)
#5
customer_churn <- customer_churn %>% mutate(AgeGroup = case_when( Age < 25 ~ "Youth",Age >= 25 & Age <= 55 ~ "Adult",
                                                                  Age > 55 ~ "Senior",TRUE ~ "Unknown"  ) )view(customer_churn)
#6
Top5_cities <- customer_churn_yes %>% count(City) %>% arrange(desc(n)) %>% head(5)
view(Top5_cities)
#7
high_value_customers <- data %>%
  filter(TotalCharges > 3000, Contract == "Month-to-month", Churn == "Yes") %>%
  select(Name, City)
view(high_value_customers)

#8
contract_stats <- data %>%
  group_by(Contract) %>%
  summarise(
    AvgTenure = mean(tenure, na.rm = TRUE),
    TotalEarnings = sum(TotalCharges, na.rm = TRUE)
  )
view(contract_stats)
