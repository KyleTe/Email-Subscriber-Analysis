setwd=(path)
sub=export_All.Subscribers_10202020
library(tidyverse)
library(ggplot2)
#install.packages("gender")
library(gender)
#install.packages(c("gender", "genderdata"),
                 #repos = "http://packages.ropensci.org",
                 #type = "source")

bounce=sub%>%
  filter(Status=="Bounced")
  

subname = sub%>%
  select(Full.Name,Status)%>%
  filter(!is.na(Full.Name) & Full.Name != "")
         

subname$name = tolower(gsub("^(.*?)\\s.*", "\\1", subname$Full.Name))

name_gender$name=tolower(name_gender$name)

subname$gender=subname$prob=0

gender(subname$name)



joined <- dplyr::inner_join(subname, name_gender, by = "name") %>% 
  filter(Status %in% c("Unsubscribed","Held","Bounced","Active"))%>%
    na.omit()

joined=joined[,c(2,5,6,7)]

table(joined$Status)

data <- joined%>%
  group_by(gender.y)%>%
  summarise(mean(gender.y),
            mean(Status))

plot1 <- joined %>%
  ggplot(aes(fill=Status, y=Status, x=gender.y)) + 
  geom_bar(position="fill", stat="identity")