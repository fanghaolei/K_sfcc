# a function to plot any category of crime
plotcrime <- function(category){
    library(lubridate)
    library(ggplot2)
    library(ggmap)
    time <- parse_date_time(train[, 1], "%Y-%m-%d %H:%M:%S", tz = "UTC")
    toplot <- train[which(train$Category == category), ]
    
    # location plot
    map<-get_map(location="sanfrancisco",zoom=12,source="osm", color = 'bw')
    pl <- ggmap(map)+
                geom_point(data = toplot, alpha = I(1/10), aes(x = X, y = Y, color = Descript))+
                ggtitle(paste(tolower(category), "locations"))
        
return(pl)
}












