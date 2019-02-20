commute_map <- function(city){
  require(ggmap)
  require(data.table)
  require(plyr)
  require(RColorBrewer)
  require(ggsn)
  setwd('~/Documents/Personal/Misc/City_Commutes/')
  register_google(key='AIzaSyBu8Gg2sOosNIgMyB2htj9-JRStddtNfYU')
  
  cities <- c('Boston', 'New York City', 'Tokyo', 'Hong Kong', 'Toronto', 'Bangkok', 'Manila', 'Dubai', 'Chicago', 
              'Paris', 'Vienna', 'Washington, DC', 'Sydney', 'San Francisco', 'Shanghai')
  countries <- c('USA', 'USA', 'Japan', 'Hong Kong', 'Canada', 'Thailand', 'Philippines', 'UAE', 'USA', 
                 'France', 'Austria', 'USA', 'Australia', 'USA', 'China')
  centers <- geocode(paste0(cities, ' City Hall, ',countries), override_limit = T)
  
  
  homes <- vector('list', length(cities))
  center_list  <- vector('list', length(cities))
  distance_list <- vector('list', length(cities))
  distance_df <- vector('list', length(cities))
  seq_lon <- seq(-0.072, 0.072, by=0.005)
  added_df <- data.frame(lon = rep(seq_lon, length(seq_lon)), lat = rep(seq_lon, each=length(seq_lon)))
  for (i in 1:length(cities)){
    center_list[[i]] <- do.call("rbind", replicate(nrow(added_df), centers[i,], simplify = FALSE))
    homes[[i]] <- center_list[[i]] + added_df
  }
  na_df <- data.frame(from=NA, to=NA, m=NA, km=NA, miles=NA, seconds=NA, minutes=NA, hours=NA)
  #j will change to run this for 1 city
  j <- which(cities==city)
  if (is.na(j)) {
    stop(paste0("Cannot run commute map for city named ", city))
  }
  #calls google and gets base map centered at the center for this city, at city level zoom
  basemap <- get_map(location = centers[j,], zoom=12)
  ggmap(basemap) + geom_point(data=homes[[j]], aes(x=lon, y=lat))

  
  t <- strftime(Sys.time(), '%H:%M')
  distance_list[[j]] <- vector('list', nrow(homes[[j]]))
  for (i in 1:nrow(homes[[j]])) {
    current_points <- homes[[j]]
    distance_list[[j]][[i]] <- tryCatch(mapdist(from = as.numeric(current_points[i,]), to = as.numeric(centers[j,]), mode='transit'), error=function(err) na_df)
    if(i %% 10 == 0){
      print(i)
    }
  }
  distance_df[[j]] <- do.call('rbind.fill', distance_list[[j]])
  distance_df[[j]] <- setDT(cbind(distance_df[[j]], homes[[j]][1:nrow(distance_df[[j]]),]))
  distance_df[[j]]$minutes_discrete <- cut(distance_df[[j]]$minutes, breaks=seq(0, 120, by=5))
  distance_df[[j]]$city <- cities[j]
  write.csv(distance_df[[j]], paste0(cities[j], '_', t, '_rawdata.csv'), row.names=F)
  names(distance_df[[j]]) <- mapvalues(names(distance_df[[j]]), from = 'lon', to= 'long')
  
  min_long <- min(abs(distance_df[[j]]$long))* sign(min(distance_df[[j]]$long))
  min_lat <- min(abs(distance_df[[j]]$lat)) * sign(min(distance_df[[j]]$lat))
  
  ggmap(basemap) + geom_point(data=distance_df[[j]][minutes<=40],  aes(x=long, y=lat, fill=minutes), size=2, shape=23) +
    geom_density_2d(data=distance_df[[j]][minutes<=40],  aes(x=long, y=lat), color='black', alpha=0.75) +
    scale_fill_gradient2(low='green', mid='yellow', high='red', midpoint=20, 
                         breaks=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40)) +
    ggtitle(paste0(cities[j], ' Commute Map')) + 
    geom_point(data=centers[j,], aes(x=lon, y=lat), size=3, alpha=0.6) +
    theme(plot.title = element_text(hjust=0.5)) +
    scalebar(distance_df[[j]][minutes<=40], dist=5, dd2km = T, model = 'WGS84', location = 'bottomright',
             height=0.01, 
             anchor=c(x=min_long-0.001 * sign(min(distance_df[[j]]$long)), y=min_lat-0.001 * sign(min(distance_df[[j]]$lat))))
  ggsave(paste0(cities[j],'_', t,'_Commute_Map.jpeg'), width=8.5, height=7.5, dpi=300)
}