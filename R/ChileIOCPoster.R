library(motus)
library(maps)
library(data.table)
library(PBSmapping)

## all tags:
#26895, 26897, 26899, 26900, 26901, 26902, 26903, 26904, 26905, 26906, 27401, 27409, 27410, 27411, 27412, 27413, 
#27414, 27415, 27417, 27418, 27419, 27420, 27422, 27424, 27425, 27426, 27427, 27428, 27429, 27430, 27431, 27432, 
#27433, 27434, 27435, 27436, 27437, 27438, 27439, 27440, 27441, 27442, 27443, 27444, 27445, 27446, 27447, 27448, 
#27449, 27450, 27451, 27452, 27453, 27454, 27455, 27456, 27457, 27458, 27459, 27460, 27461, 27462, 27463, 27464, 
#27465, 27466, 27467, 27468, 27469, 27470, 27471, 27472, 27473, 27474, 27475, 27478, 27479, 27482

hourly <- mutate(rekn, tsRound = round_date(ts, "hours"))
hourly <- hourly %>% group_by(motusTagID, tsRound) %>% mutate(nHits = length(sig))
hourly <- select(hourly, finalChile, runLen, nHits, tsRound, motusTagID, recvDeployName, recvDeployLat, recvDeployLon, speciesEN, siteLat, recvProjID, online) %>% distinct()
hourly <- hourly[with(hourly, order(motusTagID, tsRound)),]

## As of July 12, tags detected outside of Chile: 27409, 27413, 27419, 27450, 27451, 27457, 27465, 27470
## remove false hits
hourly <- filter(hourly, !(motusTagID == 27451 & recvDeployName == "Los Vientos Forest" | recvDeployName == "Stutchbury"))
hourly <- filter(hourly, !(motusTagID == 27409 & recvDeployName == "SKID"))
hourly <- filter(hourly, !(motusTagID == 27465 & recvDeployName == "Cambridge_RARE"))

## base maps
na.lakes <- map_data(map = "lakes")
na.lakes <- mutate(na.lakes, long = long - 360)
# Include all of the Americas to begin
worldmap <- map_data(map = "world2")
worldmap <- mutate(worldmap, long = long - 360)
setnames(worldmap, c("X","Y","PID","POS","region","subregion"))

## periods of no data
# Buque Quemado - only 2 days of data
# E. Pepita - dec 8 - april 15, missing mar 11 - apr 2
# Mira Mar, dec 9 - june 2, antennas offline  feb 15 - april 6
# E. El Pantano, jan 17 - jun 2, no more coming
# Punta Catalina, feb 25-may 22, no more coming


#########################################################################################################
## Map of Bahia Lomas Stations
#########################################################################################################
## Google Map
gmap <-  get_map(location = c(lon = -69, lat = -52.5), # lon/lat to centre map over
                 maptype = "satellite", # select maptype
                 source = "google",
                 zoom = 9) # zoom, must be a whole number
p <- ggmap(gmap)
p + geom_point(data=filter(recvs, recvProjID == 174), aes(recvLon, recvLat), pch=21, colour = "black", fill = "red", size = 2) + 
  geom_text(data=filter(recvs, recvProjID == 174), aes(recvLon, recvLat, label = recvDeployName), col = "white", hjust = -0.1, vjust = 0.4, size = 3) + 
  theme_bw() + labs(title = "Bahia Lomas Stations") +
  theme(axis.title = element_blank(), legend.position = "bottom", text = element_text(size = 8))
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/ChileMap_google.pdf")

## outline map
## NOT GOOD, NOT ENOUGH DETAIL
xlim = c(-71, -67)
ylim = c(-53, -52)
worldmap1 = clipPolys(worldmap, xlim=xlim, ylim=ylim, keepExtra = TRUE)
ggplot(na.lakes, aes(long, lat)) + coord_map(xlim = xlim, ylim = ylim) +
  geom_polygon(data = worldmap1,aes(X, Y, group = PID), colour = "grey", 
               fill = "grey98", size = 0.1) + 
  geom_polygon(aes(group = group),colour = "grey", fill = "white") + xlab("") + 
  ylab("") + theme_bw() + 
  geom_point(data=filter(recvs, recvProjID == 174), aes(recvLon, recvLat), pch=21, colour = "black", fill = "red", size = 2) + 
  geom_text(data=filter(recvs, recvProjID == 174), aes(recvLon, recvLat, label = recvDeployName), col = "white", hjust = -0.1, vjust = 0.4, size = 3) +   theme(axis.title = element_blank(), legend.position = "bottom", text = element_text(size = 8),
                                                                                                                                                                panel.grid.major = element_line(size = 0.1), axis.ticks = element_line(size = 0.2))
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/ChileMap_outline.pdf")





#########################################################################################################
############################## Map of migration, potentials 27470, 27409 ################################
#########################################################################################################
ggplot(filter(hourly, runLen > 2, motusTagID %in% c(27409, 27413, 27419, 27450, 27451, 27457, 27465, 27470)), 
       aes(tsRound, siteLat, group = motusTagID, col = as.factor(motusTagID))) + 
  geom_point() + geom_path() + theme(legend.position = "none") + theme_bw()

## Google Map
gmap <-  get_map(location = c(lon = -70, lat = 0), # lon/lat to centre map over
                 maptype = "satellite", # select maptype
                 source = "google",
                 zoom = 2) # zoom, must be a whole number
p <- ggmap(gmap)
p + geom_point(data=recvs, aes(recvLon, recvLat), pch=21, colour = "black", fill = "red", size = 1) + 
  geom_point(data=filter(hourly, runLen > 2, motusTagID %in% c(27470, 27409)), 
             aes(recvDeployLon, recvDeployLat), pch=21, colour = "black", fill = "yellow", size = .75) +
  geom_path(data=filter(hourly, runLen > 2, motusTagID %in% c(27470, 27409)), 
            aes(recvDeployLon, recvDeployLat, group=motusTagID, col = as.factor(motusTagID))) +
  theme_bw() + labs(color = "Tag ID") +
  theme(axis.title = element_blank(), legend.position = "bottom", text = element_text(size = 8))
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/migMap_google.pdf")

## outline map
xlim = c(-125, -25)
ylim = c(-58, 45)
setnames(worldmap, c("X","Y","PID","POS","region","subregion"))
worldmap1 = clipPolys(worldmap, xlim=xlim, ylim=ylim, keepExtra = TRUE)
ggplot(na.lakes, aes(long, lat)) + coord_map(xlim = xlim, ylim = ylim) +
  geom_polygon(data = worldmap1,aes(X, Y, group = PID), colour = "grey", 
               fill = "grey98", size = 0.1) + 
  geom_polygon(aes(group = group),colour = "grey", fill = "white") + xlab("") + 
  ylab("") + theme_bw() + 
  geom_point(data=recvs, aes(recvLon, recvLat), pch=1, colour = "gray15", size = 0.75) +
  geom_point(data=filter(hourly, runLen > 2, motusTagID %in% c(27470, 27409)), 
             aes(recvDeployLon, recvDeployLat), cex = 2, 
             pch = 21, fill = "green4", colour = "green4", size = 0.8) + 
  geom_path(data=filter(hourly, runLen > 2, motusTagID %in% c(27470, 27409)), position = position_jitter(w=0.7, h = 0),
            aes(recvDeployLon, recvDeployLat, group=motusTagID, col = as.factor(motusTagID))) + 
  scale_colour_manual(values = c("red1", "royalblue2")) + labs(color = "Tag ID") +
  theme(axis.title = element_blank(), legend.position = "bottom", text = element_text(size = 8),
        panel.grid.major = element_line(size = 0.1), axis.ticks = element_line(size = 0.2))
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/migMap_outline.pdf")

## plot showing last half hour in Chile plus migrations for the two good tags
tmp <- filter(hourly, runLen > 2, motusTagID %in% c(27470, 27409))
tmp <- tmp[with(tmp, order(motusTagID, tsRound)),]

ggplot(filter(tmp, tsRound > (finalChile - 3600)) , aes(tsRound, siteLat, group = motusTagID, col = as.factor(motusTagID))) + 
  geom_path() + geom_point(aes(size = nHits)) + theme_bw() + 
  labs(x = "Station, ordered by decreasing latitude", colour = "Tag ID", size = "Number of Detections",
       title = "Migratory detections of two tags, including the last hour of detections in Chile") +
  theme(text = element_text(size = 8))
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/migDetections.pdf")
#ggplot(filter(tmp, ts > (finalChile - 18000), recvProjID == 174) , aes(ts, sig, group = motusTagID, col = as.factor(motusTagID))) + geom_point()




#########################################################################################################
## Example of good bird in Chile
#########################################################################################################
daily <- rekn %>% group_by(motusTagID, date) %>% mutate(nHits = length(sig))
daily <- select(daily, finalChile, runLen, nHits, date, motusTagID, recvDeployName, recvDeployLat, recvDeployLon, speciesEN, siteLat, recvProjID, online) %>% distinct()
daily <- daily[with(daily, order(motusTagID, date)),]

## for the two migrating birds:
tmp <- filter(daily, motusTagID %in% c(27470, 27409), recvProjID == 174)
ggplot(tmp, aes(date, siteLat, col = siteLat, group = motusTagID)) + geom_point(aes(size = nHits)) + geom_path(col = "black") + facet_grid(motusTagID~.)

## for examples of good birds predominantly at E. El. Pantano 26900, 26902, 27434
## for examples of good birds predominantly at E. Pepita 26906, 27410, 27420, 27424, 27436, 27441, 27444

## plot showing daily detections of select birds, plus offline receiver periods
tmp <- filter(daily, recvProjID == 174, motusTagID %in% c(26900, 26902, 27434, 26906, 27410, 27420, 27424, 27436, 27441, 27444))
ggplot(tmp, aes(date, siteLat, col = siteLat, group = motusTagID)) + geom_point(aes(size = nHits)) + 
  geom_path(data = tmp, aes(date, siteLat, group = motusTagID), col = "black") + 
  geom_path(data = filter(offline, online == "FALSE"), aes(date, siteLat, group = siteLat)) +
  facet_grid(motusTagID~.)





#########################################################################################################
## comparing sites
## violin plots of detection distributions (seasonal and daily)
#########################################################################################################





#########################################################################################################
## Pepitas site use
## mean time boxplots, maybe with tidal coefficient
## site use and tide somehow?
#########################################################################################################





#########################################################################################################
## departure timing from Bahia Lomas
#########################################################################################################

