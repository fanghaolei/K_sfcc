# It seems that not many scripts have looked an suicide and attempted suicide in this data, so I did an exploratory
# analysis and some visualization for it. There are some pretty interesting pattern in this data
library(lubridate)
library(ggplot2)
library(ggmap)
train <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)

# make some transformation to the data
# pull out all tragic suicidal cases
suicide <- train[which(train$Category == 'SUICIDE'), c(1, 3, 4, 8, 9)]

# sort the suicide types to succeed and attempted
suicide[, 'type'] <- NA
suicide[, "type"][grep("ATTEMPTED", suicide[, 2])] <- "Attempted"
suicide[, "type"][-grep("ATTEMPTED", suicide[, 2])] <- "Succeeded"

# create some time variables 
suicide[, 1] <- parse_date_time(suicide[, 1], "%Y-%m-%d %H:%M:%S", tz = "UTC")
suicide[, "year"] <- as.numeric(format(suicide[, 1], "%Y"))
suicide[, "month"] <- as.numeric(format(suicide[, 1], "%m"))
suicide[, "minute"] <- as.numeric(format(suicide[, 1], "%M"))
suicide[, "hour"] <- as.numeric(format(suicide[, 1], "%H"))

# Q0: Do most people kill themselves by jumping off the bridge?
# A: Nope, or it might be the sampling error, the bridge area data might not be fully collected.
map<-get_map(location="sanfrancisco",zoom=12,source="osm", color = 'bw')
ggmap(map)+
    geom_point(data = suicide, aes(x = X, y = Y, color = as.factor(type)))+
    ggtitle("Suicide Location")+
    scale_color_manual(name = "Type", values = c("#11c2d7", "#9f0303"))


# Q1: Is there a pattern of suicide cases during a day cycle? 
# A: well, it seems that most deadly suicidal attempts occurred/were reported at 
# noon (12:00), but unsuccessful attempts peaked during night time. 
ggplot(suicide)+
    geom_density(aes(x = hour, color = as.factor(type)))+
    scale_x_continuous(breaks = c(1:24))+
    theme_bw()+
    theme(panel.border = element_blank(),
          axis.line = element_line(color = "black"))+
    ggtitle("Suicide by Hours in a Day")+
    scale_colour_manual(name = "Type", values = c("#11c2d7", "#9f0303"))

# Q2: Is there a patter of suicide cases during an hour cycle? 
# A: I think there is a data collection bias here. Most cases were marked at 0 or 30 minutes time stamp.
ggplot(suicide)+
    geom_bar(position = "dodge", aes(x = minute, fill = as.factor(type)))+
    theme_bw()+
    theme(panel.border = element_blank(),
          axis.line = element_line(color = "black"))+
    ggtitle("suicide by minutes in an hour")+
    scale_colour_manual(name = "Type", values = c("#11c2d7", "#9f0303"))

# Q3: What about patterns for months in a year, aka, a seasonal effect?   
# A: attempted sucide occured mostly in May and November, this is kind of interesting. 
# Maybe it is the seaonal transition that made people blue? 
ggplot(suicide)+
    geom_bar(aes(x =  as.factor(month)))+
    facet_grid(.~type)