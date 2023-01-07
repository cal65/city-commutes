args = commandArgs(trailingOnly=TRUE)
city = args[1]
country = args[2]

commute_map <- function(city, country){
  require(ggmap)
  require(data.table)
  require(plyr)
  require(RColorBrewer)
  require(ggsn)
  setwd('~/Documents/CAL/Real_Life/LogiCal/city-commutes/')
  register_google( key = Sys.getenv(x='GOOGLE_API'))
  
  center <- geocode(paste0(city, ' City Hall, ',country), override_limit = T)
  if (all(is.na(center))) {
    stop(paste0("Cannot run commute map for city named ", city))
  }
  
  seq_lon <- seq(-0.072, 0.072, by=0.005)
  added_df <- data.frame(lon = rep(seq_lon, length(seq_lon)), lat = rep(seq_lon, each=length(seq_lon)))

  centers <- do.call("rbind", replicate(nrow(added_df), center, simplify = FALSE))
  homes <- centers + added_df
  
  na_df <- data.frame(from=NA, to=NA, m=NA, km=NA, miles=NA, seconds=NA, minutes=NA, hours=NA)
  #j will change to run this for 1 city

  #calls google and gets base map centered at the center for this city, at city level zoom
  basemap <- get_map(location = center, zoom=12)
  ggmap(basemap) + geom_point(data=homes, aes(x=lon, y=lat))
  
  t <- strftime(Sys.time(), '%H:%M')
  distance_list <- vector('list', nrow(homes))
  for (i in 1:nrow(homes)) {
    current_points <- homes
    distance_list[[i]] <- tryCatch(mapdist(
      from = as.numeric(current_points[i,]), 
      to = as.numeric(center), mode='transit'), 
                                   error=function(err) na_df)
    if(i %% 10 == 0){
      print(i)
    }
  }
  distance_df <- do.call('rbind.fill', distance_list)
  distance_df <- setDT(cbind(distance_df, homes[1:nrow(distance_df),]))
  distance_df$minutes_discrete <- cut(distance_df$minutes, breaks=seq(0, 120, by=5))
  distance_df$city <- city
  write.csv(distance_df, paste0(city, '_', t, '_rawdata.csv'), row.names=F)
  names(distance_df) <- mapvalues(names(distance_df), from = 'lon', to= 'long')
  
  min_long <- min(abs(distance_df$lon))* sign(min(distance_df$lon))
  min_lat <- min(abs(distance_df$lat)) * sign(min(distance_df$lat))
  
  time_ext <- 40
  
  ggmap(basemap) + geom_point(data=distance_df[minutes<=time_ext],  
                              aes(x=long, y=lat, fill=minutes), size=2, shape=23) +
    geom_density_2d(data=distance_df[minutes<=time_ext],  
                    aes(x=long, y=lat), color='black', alpha=0.75) +
    scale_fill_gradient2(low='green', mid='yellow', high='red', midpoint=time_ext/2, 
                         breaks=seq(0, time_ext, 10), labels=seq(0, time_ext, 10)) +
    ggtitle(paste0(city, ' Commute Map')) + 
    geom_point(data=center, aes(x=lon, y=lat), size=3, alpha=0.6) +
    theme(plot.title = element_text(hjust=0.5)) +
    scalebar(distance_df[minutes<=time_ext], 
             dist=5, dist_unit = 'km', transform = T, model = 'WGS84', 
             location = 'bottomright', st.size = 3,
             height=0.02, 
             anchor=c(x=min_long-0.001 * sign(min(distance_df$long)), 
                      y=min_lat-0.005 * sign(min(distance_df$lat))))
  ggsave(paste0(gsub(' ', '_', city),'_', t,'_Commute_Map.jpeg'), width=8.5, height=7.5, dpi=300)
}

commute_map(city, country)

cities <- c('Boston', 'New York City', 'Tokyo', 'Hong Kong', 'Toronto', 'Bangkok', 'Manila', 'Dubai', 'Chicago', 
            'Paris', 'Vienna', 'Washington, DC', 'Sydney', 'San Francisco', 'Shanghai', 'Atlanta')
countries <- c('USA', 'USA', 'Japan', 'Hong Kong', 'Canada', 'Thailand', 'Philippines', 'UAE', 'USA', 
               'France', 'Austria', 'USA', 'Australia', 'USA', 'China', 'USA')

taskscheduler_create(taskname = "run Europe 1", rscript = commute_map('Istanbul', 'Turkey'), 
                     schedule = "ONCE", starttime = format(Sys.time() + 62, "%H:%M"))
