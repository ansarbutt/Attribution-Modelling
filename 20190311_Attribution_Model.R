# install.packages("ChannelAttribution")
# install.packages("RODBC")
# install.packages("tidyverse")

library(ChannelAttribution)
library(RODBC)
library(tidyverse)
library(dplyr)

# get data from SQL Server through ODBC
# sourceMediumPath was split into 2 fields due to the 8000 varchar limitation of RODBC
cn <- odbcConnect('VW_Analytics')
mydata <- sqlQuery(cn, "select conversionDate, CAST(sourceMediumPath1 as varchar(8000)) sourceMediumPath1, CAST(sourceMediumPath2 as varchar(8000)) sourceMediumPath2, totalConversions from [VW_Analytics].[dbo].[GA_VW_conv_model_20190312]  where conversionGoalNumber = 12")

# merge the 2 sourceMediumPath fields and check the size of the largest path
merged_mydata <- unite(mydata, "sourceMediumPath", c("sourceMediumPath1", "sourceMediumPath2"), sep = "")
max(nchar(merged_mydata[, 2]))


# remove spacing
merged_mydata[, 2] <- gsub(" / ", "/", mydata[,2])  

# build markov model
mm <- markov_model(merged_mydata, var_path = "sourceMediumPath",
                   var_conv = "totalConversions",
                   order=1)
mm$total_conversions <- round(mm$total_conversions,0)

# run the heuristic model
hm <- heuristic_models(merged_mydata, var_path = "sourceMediumPath",
                       #var_value = "value",
                       var_conv = "totalConversions")
hm$linear_touch <- round(hm$linear_touch,0)

# merge markov and heuristic model results
model_data <- merge.data.frame(hm, mm, all=T, by="channel_name")
colnames(model_data)[colnames(model_data)=="total_conversions"] <- "markov_model"

