############################################ Project Work 1 ############################################
# A).
library(readxl)
?read_excel
acsheet = read_excel("C:\\Users\\user\\Downloads\\Unfinished-Work-Directory\\Intellipaat\\Project 1\\Activity_Sheet_Pb1.xlsx",sheet = "Data")
write.csv(acsheet,"C:\\Users\\user\\Downloads\\stationery_store.csv", row.names = FALSE)
setwd("C:\\Users\\user\\Downloads")
getwd()

library(arules)
stat_store = read.transactions(file="stationery_store.csv", rm.duplicates= FALSE, format="single",sep=",",cols =c(1,2))

class(stat_store)
str(stat_store)
summary(stat_store)

library(arules)
library(arulesViz)

inspect(stat_store)
stat_store
image(stat_store)
itemFrequency(stat_store)
itemFrequencyPlot(stat_store)


rules0 = apriori(stat_store, parameter = list(support=0.5, confidence=0.6, target="rules"))
summary(rules0)
inspect(rules0)

plot(rules0)




# B). Association Rules for transaction data : 
library(readr)
##transac = read.csv("C:\\Users\\user\\Downloads\\Unfinished-Work-Directory\\Intellipaat\\Project 1\\Transactions.csv")

setwd("C:\\Users\\user\\Downloads\\Unfinished-Work-Directory\\Intellipaat\\Project 1")
getwd()
trans = read.transactions(file="Transactions.csv", rm.duplicates= FALSE, format="single",sep=",",cols =c(1,2))

inspect(trans)
trans
image(trans)
itemFrequency(trans)
itemFrequencyPlot(trans)

rules1 = apriori(trans,parameter = list(sup = 0.5, conf =0.6,target="rules"))
summary(rules1)
inspect(rules1)




# C). Association Rules for "Pharmacovigilance audit Data" dataset: Generate the rules and identify the patterns.
Data = read.csv("Pharmacovigilance_audit_Data.csv")
summary(Data)

Data$Age = as.factor(Data$Age)
class(Data$Age)

data.frame(colnames(Data))
match("PatientID",names(Data))
Data = Data[-6]
names(Data)

auditData = as(Data, "transactions")
auditData

rules2 = apriori(auditData, parameter = list(support = 0.06, confidence =0.6))
inspect(rules2)
inspect(head(rules2))

rules.classfilter1 = as(subset(rules2, subset = rhs %in% "LocationID=Location2"),"data.frame")

write.csv(rules.classfilter1,"C:\\Users\\user\\Downloads\\New_Pharmacovigilance_audit_Data.csv", row.names = FALSE)

