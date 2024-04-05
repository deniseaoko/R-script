#Title: Denise Aoko
#R Script: TB script
#Developed: 04 Apr 2024

#install package
install.packages("openxlsx")
install.packages("dplyr")
library(dplyr)
library(tidyverse)
# setting working directory
setwd("C:/Users/denise.aoko/Documents/R manenos/TB query")
# import file 1
data1 <- read.csv("census.csv")
#import file 2
data2 <- read.csv("HTS TB query.csv")
is.data.frame(data1)
is.data.frame(data2)
View(data1)
names(data1)
names(data2)
View(data2)
#rename column in data2
data2 <- data2 %>% rename(MFL = organisationunitcode)
View(data2)
# Delete columns using select() function from dplyr
data1 <- subset(data1, select = -c(5,6,7,8,9))
View(data1)
# Join the data frames based on the ID column
joined_df <- left_join(data2, data1, by = "MFL")
View(joined_df)
joined_df <- subset(joined_df, select = -c(1,3,4,5,6,8,25))
View(joined_df)

# renaming period name to month
joined_df <- joined_df %>% rename(Month = periodname)
names(joined_df)
# rearranging columns
joined_df <- select(joined_df,1,18,2,17,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
View(joined_df)
# Replace NA with 0
joined_df[is.na(joined_df)] <- 0



# Print the updated data frame
print(joined_df)
#Adding columns
joined_df$TB_cases <- ifelse(joined_df$HTS.Opt.TB..Number.Seen == joined_df$TB.Clinic..Number.of.New...relapse.clients,"TRUE",
                      ifelse(joined_df$HTS.Opt.TB..Number.Seen != joined_df$TB.Clinic..Number.of.New...relapse.clients,"FALSE","NONE"))
joined_df$Known_Positive <- ifelse(joined_df$HTS.Opt.TB..Known.HIV.Positive == joined_df$TB.Clinic..Number.of.Known.HIV..VE.status,"TRUE",
                            ifelse(joined_df$HTS.Opt.TB..Known.HIV.Positive != joined_df$TB.Clinic..Number.of.Known.HIV..VE.status,"FALSE","NONE"))
joined_df$Eligible <- ifelse(joined_df$HTS.Opt.TB..Number.Eligible == joined_df$TB.Clinic..Number.Eligible.for.HIV.testing,"TRUE",
                      ifelse(joined_df$HTS.Opt.TB..Number.Eligible != joined_df$TB.Clinic..Number.Eligible.for.HIV.testing,"FALSE","NONE"))
joined_df$Tested <- ifelse(joined_df$HTS.Opt.TB..Number.Tested == joined_df$TB.Clinic..Number.tested.for.HIV,"TRUE",
                    ifelse(joined_df$HTS.Opt.TB..Number.Tested != joined_df$TB.Clinic..Number.tested.for.HIV,"FALSE","NONE"))
joined_df$Positive <- ifelse(joined_df$HTS.Opt.TB..Newly.Identified.Positive == joined_df$TB.Clinic..Number.NEW.Positive,"TRUE",
                     ifelse(joined_df$HTS.Opt.TB..Newly.Identified.Positive != joined_df$TB.Clinic..Number.NEW.Positive,"FALSE","NONE"))
                     
joined_df$New_on_ART <- ifelse(joined_df$HTS.Opt.TB..Linked.to.HAART == joined_df$TB.Clinic..Newly.initiated.on.ART,"TRUE",
                        ifelse(joined_df$HTS.Opt.TB..Linked.to.HAART != joined_df$TB.Clinic..Newly.initiated.on.ART,"FALSE","NONE"))
joined_df$Pos_vs_Started_on_ART <- ifelse(joined_df$TB.Clinic..Number.NEW.Positive == joined_df$TB.Clinic..Newly.initiated.on.ART,"TRUE",
                                    ifelse(joined_df$TB.Clinic..Number.NEW.Positive != joined_df$TB.Clinic..Newly.initiated.on.ART,"FALSE","NONE"))
joined_df$KP_vs_KP_on_ART <- ifelse(joined_df$TB.Clinic..Number.of.Known.HIV..VE.status == joined_df$TB.Clinic..Already.on.ART_KP.ART,"TRUE",
                                    ifelse(joined_df$TB.Clinic..Number.of.Known.HIV..VE.status != joined_df$TB.Clinic..Already.on.ART_KP.ART,"FALSE","NONE"))
View(joined_df)
colnames(joined_df)
is.data.frame(joined_df)
joined_df$errors <- rowSums(joined_df[,c(19:26)]=="FALSE")
view(joined_df)
#filtering false column
TB_query <- joined_df %>% filter(errors != 0)
view(TB_query)

library(writexl)
# Export the filtered data frame
write_xlsx(TB_query, "TB_clinic_query.xlsx")