library(pacman)
pacman::p_load(sqldf, ggplot2, lubridate, xlsx, plyr, dplyr, magrittr, reshape2, knitr, tidyverse)
library(xlsx)

#load data 
txn_monitor = read.xlsx("txn_monitor.xlsx", 1, header=TRUE) #base_table
names(txn_monitor)
head(txn_monitor)
summary(txn_monitor)
dim(txn_monitor)

rule_model = read.csv("rules_within_models.csv")
names(rule_model)
head(rule_model)

threshold = read.csv("model_threshold.csv")
names(threshold)
head(threshold)

#data table manipulation
rule_model$Customer_type_1 = substr(rule_model$Customer_type, 1, 3)
rule_model$Customer_type_1 = toupper(rule_model$Customer_type_1)
rule_model$Customer_type_1[rule_model$Customer_type_1 == "COR"] = "CP" 
rule_model$Customer_type_1[rule_model$Customer_type_1 == "IND"] = "INV" 
rule_model$model_name_1 = paste(rule_model$Model_name, rule_model$Customer_type_1, sep = "_" )

head(rule_model)
summary(rule_model)

#SQL base table
df1 = sqldf('select distinct a.customer_id, a.customer_type, a.rule_name, a.hit_date, 
            b.model_name_1, b.score, c.threshold,
            case when b.score >= c.threshold then 1 else 0 end Alert,
            case when a.hit_date is not null then 1 else 0 end Hit_Count
            from txn_monitor as a 
            left join rule_model as b
            on a.rule_name = b.rule_name and a.customer_type = b.customer_type
            left join threshold as c
            on b.model_name_1 = c.model_name
            where b.rule_name is not null and b.customer_type is not null
            order by a.customer_id, a.hit_date')
dim(df1)
head(df1)

df1$Hit_Day = weekdays(df1$Hit_Date)
df1$Hit_DayNum = as.POSIXlt(df1$Hit_Date)$wday
df1$Hit_Month = month(df1$Hit_Date)
df1 = transform(df1, Bin_by_Week = cut(Hit_DayNum, 7)) #initial assessment to identify 7 days sum by bins

head(df1, 7)
View(df1)
str(df1)

#checks on base table
sum(duplicated(df1)) #sum 0
sum(is.na(df1)) #sum 0
df1_count_check = sqldf('select distinct a.customer_id, a.customer_type, a.rule_name, a.hit_date, b.customer_type,
                        b.model_name_1, b.score, c.threshold, count(*)
                        from txn_monitor as a 
                        left join rule_model as b
                        on a.rule_name = b.rule_name and a.customer_type = b.customer_type
                        left join threshold as c
                        on b.model_name_1 = c.model_name
                        group by a.customer_id, a.customer_type, a.rule_name, a.hit_date, b.customer_type,
                        b.model_name_1, b.score, c.threshold
                        having count(*) > 1')
dim(df1_count_check) #0 rows, 9 cols

#Alert_Trend_Overview
Alert_Trend_Overview = dcast(df1, model_name_1 ~ Hit_Month, value.var = "Alert", fun.aggregate = sum)
names(Alert_Trend_Overview)
Alert_Trend_Overview = Alert_Trend_Overview %>%
  rename("Model_Name" = model_name_1, "Jan-17" = "1", "Feb-17" = "2", "Mar-17" = "3", "Apr-17" = "4", "May-17" = "5", "Jun-17" = "6")
kable(Alert_Trend_Overview, format = "markdown")
View(Alert_Trend_Overview)

#Rule Hits Overview
Alert_Hits = dcast(df1, Rule_name ~ Alert, value.var = "Alert", fun.aggregate = sum)
Hits = dcast(df1, Rule_name ~ Hit_Count, value.var = "Hit_Count", fun.aggregate = sum)
Rule_Hits = cbind(Rule_Name = Alert_Hits[,1], Number_of_Hits = Hits[,2], Number_of_Alerts = Alert_Hits[,3])
kable(Rule_Hits, format = "markdown")
View(Rule_Hits)

#Visual view of customer and alert
#Cust_Histo = qplot(df1$Customer_ID, data = df1, geom = "histogram", binwidth = 0.5, xlab = "Customer_ID")
#Cust_Histo
Alert_Histo = qplot(df1$Alert, data = df1, geom = "histogram", binwidth = 0.5, xlab = "Customer_ID")
Alert_Histo #more alerts

#Customer with most alert
Cust_Most_Alert = df1 %>%
  group_by(Customer_ID, Alert) %>%
  summarise(Count_Level = n(),
            Percentage = (n()/nrow(df1)))
head(Cust_Most_Alert)
most_cust = sqldf('select * from Cust_Most_Alert where Alert = 1 order by percentage desc')
kable(most_cust, format = "markdown")
View(most_cust) #customer_id 86

#Alert by Customer_Type
head(df1)
alert_cust_type = sqldf('select customer_type, alert, Count(*) from df1 group by customer_type, alert')
kable(alert_cust_type, format = "markdown")
View(alert_cust_type)

#write to excel file
write.xlsx(df1, "Max.xlsx", sheetName = "Base_Data_Frame")
write.xlsx(Alert_Trend_Overview, "Max.xlsx", sheetName = "Alert_Trend_Overview", append = TRUE)
write.xlsx(Rule_Hits, "Max.xlsx", sheetName = "Rule_Hits_Overview", append = TRUE)
write.xlsx(most_cust, "Max.xlsx", sheetName = "Most_Alert_Cust", append = TRUE)
write.xlsx(alert_cust_type, "Max.xlsx", sheetName = "Alert_Cust_Type", append = TRUE)