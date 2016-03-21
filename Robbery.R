# This is an in-depth exploratory analysis toward robbery in the Bay area. 
# The description of robbery can be simplified with regards to weapons used in the criminal acts
# "gun", "bodily force", "knife", and "unspecified". Let's parse out these strings.
rob[, "weapon"] <- NA
rob[grep("GUN", rob[, "Descript"]), "weapon"]       <- "gun"
rob[grep("KNIFE", rob[, "Descript"]), "weapon"]     <- "knife"
rob[grep("BODILY", rob[, "Descript"]), "weapon"]    <- "bodily force"
rob[grep("STRONGARM", rob[, "Descript"]), "weapon"] <- "bodily force"
rob[, "weapon"][which(is.na(rob[, "weapon"]))]      <- "unspecified"

# pare the date variable, obatining more information
rob[, 1] <- parse_date_time(rob[, 1], "%Y-%m-%d %H:%M:%S", tz = "UTC")
rob[, "year"]  <- as.numeric(format(rob[, 1], "%Y"))
rob[, "month"] <- as.numeric(format(rob[, 1], "%m"))
rob[, "hour"]  <- as.numeric(format(rob[, 1], "%H"))

# plot robbery weapon 
ggplot(data = rob, aes(x = weapon, fill = weapon))+
        geom_bar()+
        stat_count(aes(label = ..count..), geom = "text", hjust = -.1)+
        coord_flip()+
        xlab("Case Count")+
        ylab("Weapon Used")+
        ggtitle("Types of Weapon Used in Robbery")
            
# Robbery locations
map<-get_map(location="sanfrancisco",zoom=12,source="osm", color = 'bw')
ggmap(map)+
        geom_point(data = rob, alpha = I(1/20), aes(x = X, y = Y, color = as.factor(weapon)))+
        scale_color_manual(name = "Weapon", values = c("#f08080","#22bb22", "#00ced1", "#9900cc"))+
        facet_wrap(~weapon)+
        ggtitle("Robbery Locations by Weapon Type")

# Robbery Density by Weapon Type
ggmap(map)+
        stat_density2d(data = rob, geom = "polygon", n = 500, 
                   aes(x = X, y = Y, fill = ..level.., alpha = ..level..))+
        scale_fill_gradient(low = "#ff3333", high = "#b30000")+
        facet_wrap(~weapon)+
        ggtitle("Robbery Density by Weapon Type")

# plot by hours in a day
# gun violence peaked at 8pm.
ggplot(rob)+
    geom_density(aes(x = hour, color = weapon))+
        theme_bw()+
        theme(panel.border = element_blank(),
              axis.line = element_line(color = "black"))+
        scale_x_continuous(breaks = c(1:24))+
        ggtitle("Robbery by Hours")


# data from 2015 are incomplete, so they should be removed for month plot to reduce bias
monthplot <- rob[which(rob[, 'year'] != 2015), ]
ggplot(monthplot)+
    geom_bar(stat = 'count', aes(x = as.factor(month), fill = weapon))+
    theme_bw()+
    theme(panel.border = element_blank(),
          axis.line = element_line(color = "black"))+
    facet_grid(~weapon)+
    coord_flip()+
    ggtitle("Robbery by Months")

# Robbery by year
ggplot(rob)+
    geom_bar(stat = 'count', aes(x = as.factor(year), fill = weapon))+
    theme_bw()+
    theme(panel.border = element_blank(),
          axis.line = element_line(color = "black"))+
    coord_flip()+
    facet_grid(~weapon)+
    ggtitle("Robbery by Year")


# Robbery by days in a Week
ggplot(rob)+
    geom_bar(stat = 'count', aes(x = factor(as.factor(DayOfWeek), 
                                            c("Monday", "Tuesday", 
                                              "Wednesday","Thursday"
                                             ,"Friday", "Saturday","Sunday"))
                                 , fill = weapon))+
    theme_bw()+
    theme(panel.border = element_blank(),
          axis.line = element_line(color = "black"))+
    coord_flip()+
    facet_grid(~weapon)+
    ggtitle("Robbery by Days of a Week")
