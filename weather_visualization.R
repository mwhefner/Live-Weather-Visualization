# Full description available at http://matthefner.com/dataweather.html

asc <- function(x) { strtoi(charToRaw(x),16L) }

# Download XMLs directly from the national weather service
download.file("https://w1.weather.gov/xml/current_obs/index.xml", destfile = ".\\stations.xml")
download.file("https://w1.weather.gov/xml/current_obs/all_xml.zip", destfile = "temp.xml.zip")

stations <- xmlToDataFrame(".\\stations.xml")
# Get rid of the header information, which is not needed for these purposes
stations <- stations[-c(1,2,3,4,5,6),]

# Unzips observations
zipF <- "temp.xml.zip"
outDir <- ".\\obervations"
unzip(zipF,exdir=outDir)

# Initializes observation list
data <- list()

# Converts stations XML to data frame
for (i in 1:length(stations$station_id)) {
  tryCatch(
    {
      paste(stations$station_id[i], ".xml", sep="")
      loc <- paste(".\\obervations\\", stations$station_id[i], ".xml", sep="")
      data[[i]] <- xmlToDataFrame(loc)
    },
    error = function(error_message) 
    {
      # Just ignore... NWS' XML formatting isn't always (is never) consistent, and mistakes happen.
    }
  )
}

# Create master Weather data frame
Weather <- data.frame(Station = character(), Lon = double(), Lat = double(), Temperature = double(), Humidity = double())

# Populate Weather with NWS observations from near the US

#Observation indexes:
###6 - location
###7 - call number
###8 - lon
###9 - lat

for (i in 1:length(stations$station_id)) {
  tryCatch(
    { # Longitude and Latitude restraints on observations
      if (as.numeric(data[[i]][[1]][9]) < -65 && 
          as.numeric(data[[i]][[1]][9]) > -130 &&
          as.numeric(data[[i]][[1]][8]) < 50 &&
          as.numeric(data[[i]][[1]][8]) > 25)
      {
        # Some stations record temperature and humidity at a sligtly different index
        # in their XML; the following accounts for that.
        if (as.numeric(asc(substring(data[[i]][[1]][12], 1, 1))) < 58)
        {
          # Some stations store temp (F) at col index 13 and humidity at 15
          Obs <- list(Station = data[[i]][[1]][7], 
                      Lon = as.numeric(data[[i]][[1]][9]), 
                      Lat = as.numeric(data[[i]][[1]][8]), 
                      Temperature = as.numeric(data[[i]][[1]][13]), 
                      Humidity = as.numeric(data[[i]][[1]][15]))
          Weather <- rbind(Weather, Obs, stringsAsFactors = FALSE)
        } else {
          # Most others store temp at index 14 and humidity at 16
          Obs <- list(Station = data[[i]][[1]][7], 
                      Lon = as.numeric(data[[i]][[1]][9]), 
                      Lat = as.numeric(data[[i]][[1]][8]), 
                      Temperature = as.numeric(data[[i]][[1]][14]), 
                      Humidity = as.numeric(data[[i]][[1]][16]))
          Weather <- rbind(Weather, Obs, stringsAsFactors = FALSE)
        }
      } else {
        # Then the data was not near the US and they're not pertinent
      }},
    error = function(error_message) 
    {
      # Again, just ignore... NWS' XML formatting isn't always consistent.
      # Use what we can and move on.
    }
  )
}

# load the Datatable library for a nice table to look at the observations.
library(DT)
datatable(Weather, class = 'cell-border stripe')

#Interpolate Temperature data
di <- interp(x = Weather$Lon, y = Weather$Lat, 
             z = Weather$Temperature,
             xo=seq(min(Weather$Lon), max(Weather$Lon), length=400),
             yo=seq(min(Weather$Lat), max(Weather$Lat), length=400),
             duplicate = "mean")
# store the interpolated data as a data frame
dat_interp <- data.frame(expand.grid(x=di$x, y=di$y), z=c(di$z))
# only use completed cases
dat_interp <- dat_interp[complete.cases(dat_interp),]

#Temperature Plot
ggplot() + 
  coord_fixed(1.3) + 
  geom_raster(data = dat_interp, aes(x = x, y = y, fill = z), alpha= 0.8, interpolate = TRUE) +
  scale_fill_gradientn(name = "", colors = c("pink", "violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"), space = "Lab",
                       na.value = "grey50", guide = "colourbar", aesthetics = "fill", limits = c(-15,115)) +
  theme_classic() +
  labs(x = "Longitude", y = "Latitude", 
       title = "Current U.S. Temperatures (F)") +
  geom_polygon(data = map_data("state"), 
               aes(x=long, y = lat, group = group), 
               fill = NA, color = "black", size = 0.5) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 20), 
        legend.key.width=unit(1,"cm"), plot.subtitle = element_text(hjust = 0.5, size = 10))

#Save image
ggsave('NationalTemperature.png', width = 16, height = 9, dpi = 100)

# Display attributes
ax <- list(
  visible = FALSE,
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

#Temperature Plot
plot_ly(x = ~long, y = ~lat, colors = c("white", "pink", "violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red"), xaxis = ax, yaxis = ax,  showlegend = FALSE) %>%
  add_heatmap(x = dat_interp$x, y= dat_interp$y, z = dat_interp$z, hoverinfo = "none", data = dat_interp) %>%
  add_polygons(data = map_data("county") %>% group_by(group), x = ~long, y = ~lat, hoverinfo = "none", color = I('rgba(100, 100, 100, 0.5)'), fillcolor = 'rgba(7, 164, 181, 0)') %>%
  add_polygons(x = ~long, y = ~lat, hoverinfo = "none", color = I("black"), data = map_data("state") %>% group_by(group), fillcolor = 'rgba(7, 164, 181, 0)') %>%
  add_markers(text = ~paste(Station, "<br />", Temperature), hoverinfo = "text", 
              color = I('rgba(0, 0, 0, 0.2)'), data = Weather, x = ~Lon, y = ~Lat) %>%
  layout(xaxis = ax, yaxis = ax)

#Interpolate Humidity data just the same as before
di <- interp(x = Weather$Lon, y = Weather$Lat, 
             z = Weather$Humidity,
             xo=seq(min(Weather$Lon), max(Weather$Lon), length=400),
             yo=seq(min(Weather$Lat), max(Weather$Lat), length=400),
             duplicate = "mean")
# Store in data frame
dat_interp <- data.frame(expand.grid(x=di$x, y=di$y), z=c(di$z))
# only store complete cases
dat_interp <- dat_interp[complete.cases(dat_interp),]

#Humidity Plot
ggplot() + 
  coord_fixed(1.3) + 
  geom_raster(data = dat_interp, aes(x = x, y = y, fill = z), alpha= 0.8, interpolate = TRUE) +
  scale_fill_gradientn(name = "", colors = c("red","yellow", "lightyellow", "lightgreen", "green","forestgreen"), space = "Lab",
                       na.value = "grey50", guide = "colourbar", aesthetics = "fill", limits = c(0,100)) +
  theme_classic() +
  labs(x = "Longitude", y = "Latitude", legend = "", 
       title = "Current U.S. Relative Humidity") +
  geom_polygon(data = map_data("state"), 
               aes(x=long, y = lat, group = group), 
               fill = NA, color = "black", size = 0.5) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 20), 
        legend.key.width=unit(1,"cm"))

#Save image
ggsave('NationalHumidity.png', width = 16, height = 9, dpi = 100)

#Interpolate Temperature data
di <- interp(x = Weather$Lon, y = Weather$Lat, 
             z = Weather$Humidity,
             xo=seq(min(Weather$Lon), max(Weather$Lon), length=500),
             yo=seq(min(Weather$Lat), max(Weather$Lat), length=500),
             duplicate = "mean")
dat_interp <- data.frame(expand.grid(x=di$x, y=di$y), z=c(di$z))
dat_interp <- dat_interp[complete.cases(dat_interp),]

# display attributes, again
ax <- list(
  visible = FALSE,
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

#Temperature Plot
plot_ly(x = ~long, y = ~lat, colors = c("red","yellow", "lightyellow", "lightgreen", "green","forestgreen"), xaxis = ax, yaxis = ax,  showlegend = FALSE) %>%
  add_heatmap(x = dat_interp$x, y= dat_interp$y, z = dat_interp$z, hoverinfo = "none", data = dat_interp) %>%
  add_polygons(data = map_data("county") %>% group_by(group), x = ~long, y = ~lat, hoverinfo = "none", color = I('rgba(100, 100, 100, 0.5)'), fillcolor = 'rgba(7, 164, 181, 0)') %>%
  add_polygons(x = ~long, y = ~lat, hoverinfo = "none", color = I("black"), data = map_data("state") %>% group_by(group), fillcolor = 'rgba(7, 164, 181, 0)') %>%
  add_markers(text = ~paste(Station, "<br />", Humidity), hoverinfo = "text", 
              color = I('rgba(0, 0, 0, 0.2)'), data = Weather, x = ~Lon, y = ~Lat) %>%
  layout(xaxis = ax, yaxis = ax)



# Get rid of empty cases and impossibilities
Weather <- Weather[complete.cases(Weather),]
Weather <- Weather[Weather$Temperature < 125,]