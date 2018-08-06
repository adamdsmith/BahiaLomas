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
hourly <- select(hourly, finalChile, runLen, runID, nHits, tsRound, motusTagID, recvDeployName, recvDeployLat, recvDeployLon, speciesEN, siteLat, recvProjID) %>% distinct()
hourly <- filter(hourly, recvDeployLon < 0) ## get rid of European detections
hourly <- filter(hourly, !(recvProjID != 174 & tsRound < finalChile)) ## remove migration detections occuring before last detection in Chile
hourly <- hourly[with(hourly, order(motusTagID, tsRound)),]

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
## No good - not enough detail
xlim = c(-71, -67)
ylim = c(-53, -52)
worldmap1 = clipPolys(worldmap, xlim=xlim, ylim=ylim, keepExtra = TRUE)
ggplot(na.lakes, aes(long, lat)) + coord_map(xlim = xlim, ylim = ylim) +
  geom_polygon(data = worldmap1,aes(X, Y, group = PID), colour = "grey", 
               fill = "grey98", size = 0.1) + 
  geom_polygon(aes(group = group),colour = "grey", fill = "white") + xlab("") + 
  ylab("") + theme_bw() + 
  geom_point(data=filter(recvs, recvProjID == 174), aes(recvLon, recvLat), pch=21, colour = "black", fill = "red", size = 2) + 
  geom_text(data=filter(recvs, recvProjID == 174), aes(recvLon, recvLat, label = recvDeployName), col = "white", hjust = -0.1, vjust = 0.4, size = 3) +   
  theme(axis.title = element_blank(), legend.position = "bottom", text = element_text(size = 8),
        panel.grid.major = element_line(size = 0.1), axis.ticks = element_line(size = 0.2))
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/ChileMap_outline.pdf")





#########################################################################################################
### Map of migration, potentials 27470, 27409, 27418, 27480, 27450, 27451
#########################################################################################################
ggplot(filter(rekn, runLen > 2, recvProjID != 174, recvDeployLon < 0), 
       aes(ts, sig, group = motusTagID, col = recvDeployName)) + 
  geom_point() + theme_bw() + facet_wrap(~motusTagID, scales = "free")

ggplot(filter(hourly, runLen > 2, motusTagID %in% c(27409, 27413, 27418, 27450, 27451, 27457, 27465, 27470))) 
# list of potential migrants
#unique(filter(hourly, recvProjID != 174, runLen >2, tsRound > finalChile, recvDeployLon < 0)$motusTagID)
## As of July 23, tags detected outside of Chile: 27409, 27418, 27450, 27451, 27470, 27480
## remove false hits
# 27409: lots of runLen <2, runID 27696421 and 29063715 is in the Bahamas, a day after and  day before FL detections, could be true?
# also 29034130 and 29034108 in Michigan
# 27418:good at fortescue
# 27450:maybe one FL station
# 27451: brighton detections don't make sense (runID 26775020)
# 27470: good
# 27480: Hudsonian Godwit, good
# 27401 - no Chile detections
# 27465 - bad detections at RARE

ggplot(filter(tmp, runLen > 2, motusTagID %in% c(27409, 27418, 27450, 27451, 27470, 27480)), aes(tsRound, siteLat, group = motusTagID, col = as.factor(motusTagID))) + 
  geom_point() + geom_path() + theme(legend.position = "none") + theme_bw() + facet_wrap(~motusTagID, scales = "free")

tmp = filter(rekn, motusTagID %in% c(27409, 27413, 27418, 27450, 27451, 27465, 27470, 27480), recvDeployLon < 0, (ts > finalChile-3600 | ts > finalChile))
tmp <- tmp[with(tmp, order(motusTagID, tsRound)),]

ggplot(filter(tmp, motusTagID == 27409, runLen > 2), aes(ts, sig, col = as.factor(runLen))) + geom_point() + theme_bw() + facet_grid(siteLat~.)
gmap <-  get_map(location = c(lon = -70, lat = 0), # lon/lat to centre map over
                 maptype = "satellite", # select maptype
                 source = "google",
                 zoom = 2) # zoom, must be a whole number
p <- ggmap(gmap)
p + geom_path(data=filter(tmp, motusTagID == 27480, runLen > 2), 
            aes(recvDeployLon, recvDeployLat, group=motusTagID, col = as.factor(motusTagID))) +
  theme_bw() + labs(color = "Tag ID") +
  theme(axis.title = element_blank(), legend.position = "bottom", text = element_text(size = 8))

hourly <- filter(hourly, motusTagID %in% c(27409, 27413, 27418, 27450, 27451, 27470, 27480)) ## keep migrating birds
tmp <- filter(hourly, !(runID %in% c(27696421, 26775020, 29063715, 29034130, 29034108))) ## remove certain runIDs (explained above)
tmp <- filter(tmp, (tsRound > finalChile-3600 | tsRound > finalChile), !(runLen <= 2 & recvProjID != 174)) ## keep only last hour of Chile data, and remove runLen < 2 with the exception of chile stations
tmp <- tmp[with(tmp, order(motusTagID, tsRound)),]

## latitude plot
ggplot(tmp, aes(tsRound, recvDeployLat, col = as.factor(motusTagID), shape = speciesEN, group = motusTagID)) + 
  geom_point(size = 1) + geom_path(size = 0.5) + theme_bw() +
  labs(y = "Latitude", x = NULL, colour = "Tag ID", shape = "Species") + 
  theme(legend.position = "bottom", text = element_text(size = 8), legend.box = "vertical", legend.title.align = 0.5)
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/LatitudeMigration.pdf")
ggsave("C:/Users/cryslerz/Documents/RProjects/ChilePlots/LatitudeMigration.pdf")

## Google Map
gmap <-  get_map(location = c(lon = -70, lat = 0), # lon/lat to centre map over
                 maptype = "satellite", # select maptype
                 source = "google",
                 zoom = 2) # zoom, must be a whole number
p <- ggmap(gmap)
p + geom_point(data=recvs, aes(recvLon, recvLat), pch=21, colour = "black", fill = "red", size = 1) + 
  geom_point(data=tmp, 
             aes(recvDeployLon, recvDeployLat), pch=21, colour = "black", fill = "yellow", size = .75) +
  geom_path(data=filter(tmp), 
            aes(recvDeployLon, recvDeployLat, group=motusTagID, col = as.factor(motusTagID))) +
  theme_bw() + labs(color = "Tag ID", title = "ID 27480 = Hudsonian Godwit") +
  theme(axis.title = element_blank(), legend.position = "bottom", text = element_text(size = 8))
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/migMap_google.pdf")

## outline map
xlim = c(-125, -25)
ylim = c(-55, 47)
setnames(worldmap, c("X","Y","PID","POS","region","subregion"))
worldmap1 = clipPolys(worldmap, xlim=xlim, ylim=ylim, keepExtra = TRUE)
ggplot(na.lakes, aes(long, lat)) + coord_map(xlim = xlim, ylim = ylim) +
  geom_polygon(data = worldmap1,aes(X, Y, group = PID), colour = "grey", 
               fill = "grey98", size = 0.1) + 
  geom_polygon(aes(group = group),colour = "grey", fill = "white") + xlab("") + 
  ylab("") + theme_bw() + 
  geom_point(data=recvs, aes(recvLon, recvLat), pch=1, colour = "gray15", size = 0.75) +
  geom_point(data=tmp, 
             aes(recvDeployLon, recvDeployLat), cex = 2, 
             pch = 21, size = 0.8) + 
  geom_path(data=tmp, position = position_jitter(w=0.7, h = 0),
            aes(recvDeployLon, recvDeployLat, group=motusTagID, col = as.factor(motusTagID))) + 
  labs(color = "Tag ID", title ="ID 27480 = Hudsonian godwit") +
  theme(axis.title = element_blank(), legend.position = "bottom", text = element_text(size = 8),
        panel.grid.major = element_line(size = 0.1), axis.ticks = element_line(size = 0.2))
ggsave("C:/Users/cryslerz/Documents/RProjects/ChilePlots/migMap_outline.pdf")
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/migMap_outline.pdf")

## plot showing last half hour in Chile plus migrations for the two good tags
tmp <- filter(hourly, !(runLen <= 2 & recvProjID != 174), motusTagID %in% c(27409, 27413, 27418, 27450, 27451, 27470, 27480))
tmp <- tmp[with(tmp, order(motusTagID, tsRound)),]

ggplot(filter(tmp, tsRound > (finalChile - 3600)) , aes(tsRound, siteLat, group = motusTagID, col = as.factor(motusTagID))) + 
  geom_path() + geom_point(aes(size = nHits)) + theme_bw() + 
  labs(y = "Station, ordered by decreasing latitude", x = NULL, colour = "Tag ID", size = "Number of Detections",
       title = "Migratory detections, including the last hour of detections in Chile, tag 27480 = Hudsonian Godwit") +
  theme(text = element_text(size = 8))
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/migDetections.pdf")
#ggplot(filter(tmp, ts > (finalChile - 18000), recvProjID == 174) , aes(ts, sig, group = motusTagID, col = as.factor(motusTagID))) + geom_point()






#########################################################################################################
## Florida Birds
#########################################################################################################
## 27409, 27450, 27470
tmp <- filter(hourly, motusTagID %in% c(27409, 27450, 27470),!(runID %in% c(27696421, 26775020, 29063715, 29034130, 29034108)), runLen >2)
tmp <- tmp[with(tmp, order(motusTagID, tsRound)),]

## just FL detections
ggplot(filter(tmp, recvProjID != 174), aes(tsRound, recvDeployLat, col = as.factor(motusTagID), group = motusTagID)) + 
  geom_point(aes(size = nHits)) + geom_path() + theme_bw() +
  labs(y = "Latitude", x = NULL, colour = "Tag ID", size = "Number of Detections", title = "Florida Detections by Latitude") +
  theme(text = element_text(size = 8),
        legend.position = "bottom", legend.box = "horizontal", legend.title.align = 0.5) 
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/FL_Detections.pdf")

## FL plus Chile detections
ggplot(tmp, aes(tsRound, recvDeployLat, col = as.factor(motusTagID), group = motusTagID)) + 
  geom_point(aes(size = nHits)) + geom_path() + theme_bw() +
  labs(y = "Latitude", x = NULL, colour = "Tag ID", size = "Number of Detections", title = "Florida Detections by Latitude, including Chile detections") +
  theme(text = element_text(size = 8),
        legend.position = "bottom", legend.box = "horizontal", legend.title.align = 0.5) 
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/FL_Detections.pdf")









#########################################################################################################
## Example of good bird in Chile
#########################################################################################################
daily <- rekn %>% group_by(motusTagID, date) %>% mutate(nHits = length(sig))
daily <- select(daily, finalChile, runLen, nHits, date, motusTagID, recvDeployName, recvDeployLat, recvDeployLon, speciesEN, siteLat, recvProjID) %>% distinct()
daily <- daily[with(daily, order(motusTagID, date)),]

## for the two migrating birds:
tmp <- filter(daily, motusTagID %in% c(27470, 27409), recvProjID == 174)
ggplot(tmp, aes(date, siteLat, col = siteLat, group = motusTagID)) + geom_point(aes(size = nHits)) + geom_path(col = "black") + facet_grid(motusTagID~.)

## for examples of good birds predominantly at E. El. Pantano 26900, 26902, 27434
## for examples of good birds predominantly at E. Pepita 26906, 27410, 27420, 27424, 27436, 27441, 27444

## plot showing daily detections of select birds, plus offline receiver periods
tmp <- filter(daily, recvProjID == 174, motusTagID %in% c(27434, 26906, 27410, 27420, 27424, 27436))
offline$date <- as_date(offline$date)
ggplot(tmp, aes(date, recvDeployName, col = recvDeployName, group = motusTagID)) + geom_point(aes(size = nHits)) + 
  geom_path(data = filter(offline, online == "FALSE"), aes(date, recvDeployName, group = recvDeployName), col = "gray20") +
  geom_path(data = filter(end, start == "FALSE"), aes(date, recvDeployName, group = recvDeployName), col = "gray20") +
  geom_path(data = tmp, aes(date, recvDeployName, group = motusTagID), col = "black") + 
  facet_grid(motusTagID~.) + theme_bw() + 
  labs(y = NULL, x = NULL, colour = "Receiver Stations", size = "Number of Detections") +
  theme(text = element_text(size = 8), legend.direction = "horizontal",
        legend.position = "bottom", legend.box = "vertical", legend.title.align = 0.5) +
  scale_size(guide = guide_legend(title.position = "top")) +
  scale_colour_discrete(guide = guide_legend(nrow = 2, title.position = "top"))
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/chileDetections.pdf")





#########################################################################################################
## comparing sites
## violin plots of detection distributions (seasonal and daily)
#########################################################################################################
# violin plot scaled to show relative counts
tmp <- filter(visits, speciesEN == "Red Knot", date < as.Date("2018-06-01")) 
tmp <- merge(tmp, offline, all = TRUE)
tmp <- merge(tmp, end, all = TRUE)
ggplot(visits, aes(recvDeployName, date)) + 
  geom_violin(aes(fill = recvDeployName, weight = visitLength), scale = "count") +
  geom_path(data = filter(tmp, online == "FALSE"), col = "white") +
  geom_path(data = filter(tmp, online == "FALSE"), col = "black", linetype = "longdash") +
  geom_path(data = filter(tmp, start == "FALSE"), col = "white") +
  geom_path(data = filter(tmp, start == "FALSE"), col = "black", linetype = "longdash") +
  coord_flip() + theme_bw() + 
  theme(legend.position = "none", text = element_text(size = 8), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Total length of daily visits by site, dashed line represent periods of missing data", 
       y = NULL, x = NULL)
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/violinDetections.pdf")

# violin plot not scaled and using number of hits
tmp <- filter(visits, speciesEN == "Red Knot", date < as.Date("2018-06-01")) 
tmp <- merge(tmp, offline, all = TRUE)
tmp <- merge(tmp, end, all = TRUE)
ggplot(visits, aes(recvDeployName, date)) + 
  geom_violin(aes(fill = recvDeployName)) +
  geom_path(data = filter(tmp, online == "FALSE"), col = "white") +
  geom_path(data = filter(tmp, online == "FALSE"), col = "black", linetype = "longdash") +
  geom_path(data = filter(tmp, start == "FALSE"), col = "white") +
  geom_path(data = filter(tmp, start == "FALSE"), col = "black", linetype = "longdash") +
  coord_flip() + theme_bw() + 
  theme(legend.position = "none", text = element_text(size = 8), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Total length of daily visits by site, dashed line represent periods of missing data", 
       y = NULL, x = NULL)
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/violinDetections_notscaled.pdf")

## histogram of visit length
tmp <- filter(visits, speciesEN == "Red Knot", date < as.Date("2018-06-01")) 
tmp <- merge(tmp, offline, all = TRUE)
tmp <- merge(tmp, end, all = TRUE)
ggplot(tmp, aes(date, visitLength)) + 
  geom_bar(stat = "identity", aes(fill = recvDeployName)) +
  geom_path(data = filter(tmp, start == "FALSE"), aes(x = date, y = 0), col = "black", linetype = "longdash") +
  geom_path(data = filter(tmp, online == "FALSE"), aes(x = date, y = 0), col = "black", linetype = "longdash") +
  theme_bw() + facet_grid(recvDeployName~., scales = "free") + 
  theme(legend.position = "none", text = element_text(size = 8), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Sum of total time for each tag spent at each station per day", 
       y = "Cumulative time of all tags at a station per day (mins)", x = NULL)
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/histogramDetections.pdf")




#########################################################################################################
## Pepitas site use
## mean time boxplots, maybe with tidal coefficient
## site use and tide somehow?
#########################################################################################################
## total time per day per bird at a site, tidal coefficient
prop <- filter(visits, speciesEN== "Red Knot") %>% group_by(date, motusTagID, recvDeployName, coefficient) %>% 
  summarize(visitLength = sum(visitLength))

ggplot(filter(prop, recvDeployName == "E. Pepita", date < as.Date("2018-03-11")), aes(x = date)) + 
  geom_boxplot(aes(y=visitLength, group = date), lwd=0.2, outlier.size = 0.2) +
  geom_line(data = filter(tide, date < as.Date("2018-03-11"), date > as.Date("2018-01-19"), date < as.Date(max(prop$date))), aes(y=coefficient*2, col = "Tidal Coefficient")) +
  theme_bw() + theme(text = element_text(size = 5), plot.title = element_text(hjust = 0.5),
                     legend.title = element_blank(),
                     legend.position = c(0.87,0.94)) +
  labs(title = "Total daily time spent at E. Pepita per tag, Red Knots",
       x = "Minutes spent at E. Pepita/tag", y = NULL)
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/PepitaUseTidalCoefficient.pdf")

ggplot(filter(prop, recvDeployName == "E. Pepita", date < as.Date("2018-03-11")), aes(linear, visitLength)) + geom_point()

## total time per day per bird at a site, tide level
prop <- filter(visits, speciesEN== "Red Knot") %>% group_by(date, motusTagID, recvDeployName, linear) %>% 
  summarize(visitLength = sum(visitLength))

ggplot(filter(visits, recvDeployName == "E. Pepita", date < as.Date("2018-02-01")), aes(x = tsRound)) + 
  geom_boxplot(aes(y=visitLength, group = tsRound), lwd=0.2, outlier.size = 0.2) +
  geom_line(data = filter(visits, date < as.Date("2018-02-01")), aes(y=linear*5, col = "Tide height (m)")) +
  theme_bw() + theme(text = element_text(size = 5), plot.title = element_text(hjust = 0.5),
                     legend.title = element_blank(),
                     legend.position = c(0.87,0.94)) +
  labs(title = "Total daily time spent at E. Pepita per tag, Red Knots",
       x = "Minutes spent at E. Pepita/tag", y = NULL)
ggsave("/Users/zoecrysler/Desktop/chilePosterPlots/PepitaUseTideHeight.pdf")



ggplot(filter(prop, recvDeployName == "E. Pepita", date < as.Date("2018-02-01")), aes(visitLength, coefficient)) + 
  geom_point() + scale_y_continuous(trans ="log2")


#########################################################################################################
## departure timing from Bahia Lomas
#########################################################################################################
#26895, 26897, 26899, 26900, 26901, 26902, 26903, 26904, 26905, 26906, 27401, 27409, 27410, 27411, 27412, 27413, 
#27414, 27415, 27417, 27418, 27419, 27420, 27422, 27424, 27425, 27426, 27427, 27428, 27429, 27430, 27431, 27432, 
#27433, 27434, 27435, 27436, 27437, 27438, 27439, 27440, 27441, 27442, 27443, 27444, 27445, 27446, 27447, 27448, 
#27449, 27450, 27451, 27452, 27453, 27454, 27455, 27456, 27457, 27458, 27459, 27460, 27461, 27462, 27463, 27464, 
#27465, 27466, 27467, 27468, 27469, 27470, 27471, 27472, 27473, 27474, 27475, 27478, 27479, 27482
## get last hr for each bird
tmp <- filter(rekn, ts > finalChile-3600, recvProjID == 174, motusTagID %in% c(26895, 26897, 26899, 26900, 26901, 26902, 26903))
ggplot(tmp, aes(ts, sig)) + geom_point() + facet_grid(~motusTagID, scales = "free_x")

tmp <- filter(daily, recvProjID == 174)
ggplot(tmp, aes(date, as.factor(motusTagID), col = siteLat)) + geom_point()























log scale 
