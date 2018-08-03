library(motus)
library(tidyr)
library(imputeTS)
## run latLongDist.R and rad.R

projnum = 174
reknalltags <- tagme(projnum, update = TRUE, forceMeta = TRUE, dir = "C:/Users/cryslerz/Documents/motusDownloads")
reknalltags <- tagme(projnum, update = TRUE, forceMeta = TRUE, dir = "/Users/zoecrysler/Documents/BSC 2016/REKN/")
rekn <- tbl(reknalltags, "alltags")
rekn <- rekn %>% collect() %>% as.data.frame()  # for all fields in the df
rekn <- mutate(rekn, ts = as_datetime(ts, tz = "UTC"),
               tagDeployEnd = as_datetime(tagDeployEnd, tz = "UTC"),
               tagDeployStart = as_datetime(tagDeployStart, tz = "UTC"),
               date = as.Date(ts))
rekn$tsRound <- as.POSIXct(round(rekn$ts, "hours"))

#rekn <- filter(rekn, recvProjID == 174)
rekn <- filter(rekn, tagDeployEnd > as.POSIXct("2017-11-01"))
rekn <- filter(rekn, ts > tagDeployStart)
rekn <- rekn[with(rekn, order(ts, motusTagID, runID)),]
rekn$siteLat <- paste(rekn$recvDeployName, rekn$recvDeployLat, sep = "_")
rekn$siteLat <- paste(rekn$siteLat, rekn$recvDeployLon, sep = ", ")
rekn <- within(rekn, siteLat <- reorder(siteLat, recvDeployLat))
## sunrise info
#rekn <- rename(rekn, recvDeployLat = recvLat, recvDeployLon = recvLon) ## need to rename, need ot fix this in function code!
#rekn <- timeToSunriset(rekn, units = "min")
#rekn <- rename(rekn, recvLat = recvDeployLat, recvLon = recvDeployLon, mints = ts) ## need to rename, need ot fix this in function code!
rekn <- select(rekn, - sigsd, -noise, -freq, -freqsd, -slop, -tagType, -codeSet, -tagModel, -tagLifespan, -nomFreq,
               -pulseLen, - markerType, -markerNumber, -tagDeployComments, - tagDeployAlt, - recvDeployAlt, 
               - recvSiteName, -antHeight, -speciesFR, - speciesSci, -speciesGroup)
## lets get the last detection in Chile for each tag
tmp <- filter(rekn, recvProjID == 174) %>% group_by(motusTagID) %>% summarize(finalChile = max(ts))
rekn <- merge(rekn, tmp, all.x = TRUE)

## create dataframes for offline periods/no data
recvDeployName = "E. Pepita"
siteLat = "E. Pepita_-52.4693, -69.3849"
date = seq(as.Date("2018-03-11"), as.Date("2018-04-02"), by = "day")
PepitaMissing <- data.frame(recvDeployName, siteLat, date)
recvDeployName = "E. Pepita"
siteLat = "E. Pepita_-52.4693, -69.3849"
date = seq(as.Date("2018-04-14"), as.Date("2018-07-01"), by = "day")
PepitaEnd <- data.frame(recvDeployName, siteLat, date)

recvDeployName = "MiraMar"
siteLat = "MiraMar_-52.5473, -69.3191"
date = seq(as.POSIXct("2018-02-16"), as.POSIXct("2018-04-05"), by="day")
MiraMarMissing <- data.frame(recvDeployName, siteLat, date)
recvDeployName = "MiraMar"
siteLat = "MiraMar_-52.5473, -69.3191"
date = seq(as.POSIXct("2018-06-02"), as.POSIXct("2018-07-01"), by="day")
MiraMarEnd <- data.frame(recvDeployName, siteLat, date)

recvDeployName = "E. El Pantano"
siteLat = "E. El Pantano_-52.6858, -69.0608"
date = seq(as.POSIXct("2018-06-03"), as.POSIXct("2018-07-01"), by="day")
PantanoEnd <- data.frame(recvDeployName, siteLat, date)

recvDeployName = "Punta Catalina"
siteLat = "Punta Catalina_-52.5497, -68.7729"
date = seq(as.POSIXct("2018-03-22"), as.POSIXct("2018-07-01"), by="day")
CatalinaEnd <- data.frame(recvDeployName, siteLat, date)
recvDeployName = "Punta Catalina"
siteLat = "Punta Catalina_-52.5497, -68.7729"
date = seq(as.POSIXct("2018-01-01"), as.POSIXct("2018-02-25"), by="day")
CatalinaMissing <- data.frame(recvDeployName, siteLat, date)

offline <- rbind(PepitaMissing, MiraMarMissing, CatalinaMissing)
offline$online <- "FALSE"
end <- rbind(PepitaEnd, MiraMarEnd, PantanoEnd, CatalinaEnd)
end$start <- "FALSE"

# get receiver metadata
recvs <- tbl(reknalltags, "recvDeps")
recvs <- recvs %>% collect() %>% as.data.frame()  # for all fields in the df
recvs <- mutate(recvs, tsStart = as_datetime(tsStart, tz = "UTC"),
                tsEnd = as_datetime(tsEnd, tz = "UTC"))
recvs <- rename(recvs, recvLat = latitude, recvLon = longitude, recvDeployName = name,
                recvStart = tsStart, recvEnd = tsEnd, recvProjID = projectID, recvDeployID = deployID)
# for deployments with no end dates, make an end date a year from now
recvs$recvEnd <- as.POSIXct(ifelse(is.na(recvs$recvEnd), 
                                   as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")) + 
                                     lubridate::dyears(1), recvs$recvEnd), tz = "UTC", 
                            origin = "1970-01-01")
recvs <- recvs[!is.na(recvs$recvStart),]
# get running intervals for all receiver deployments
siteOp <- with(recvs, lubridate::interval(recvStart,recvEnd))  
# set the date range you're interested in
dateRange <- lubridate::interval(as.POSIXct("2018-01-01"), 
                                 as.POSIXct("2018-07-31"))
# create new variable 'active' which will be set to
# TRUE if the receiver was active at some point
# during your specified date range, and FALSE if
# not
recvs$active <- lubridate::int_overlaps(siteOp, dateRange)

## get tide data
tide <- read.csv("./data/tides_formatted.csv")
tide$day <- as.numeric(as.character(tide$day))
tide <- tide[!is.na(tide$day),]
## deal with repeated columns of height and time
tmp <- select(tide, -time.1, -height.1, -time.2, -height.2, -time.3, -height.3) ## keep time/height
tmp1 <- select(tide, -time, -height, -time.2, -height.2, -time.3, -height.3) ## keep time/height.1
tmp1 <- rename(tmp1, time = time.1, height = height.1)
tmp2 <- select(tide, -time.1, -height.1, -time, -height, -time.3, -height.3) ## keep time/height.2
tmp2 <- rename(tmp2, time = time.2, height = height.2)
tmp3 <- select(tide, -time.1, -height.1, -time.2, -height.2, -time, -height) ## keep time/height.3
tmp3 <- rename(tmp3, time = time.3, height = height.3)
tide <- rbind(tmp, tmp1, tmp2, tmp3)
tide <- tide[!(tide$height == ""), ]
## get a date column
tide <- tide %>% mutate(date = paste(Year, Month, day, sep = "-"),
                        ts = paste(date, time, sep = " "),
                        ts = as.POSIXct(as.character(ts)),
                        ts = ts - (3600*3),
                        tsRound = as.POSIXct(round(ts, "hours")),
                        height = as.numeric(as.character(height)),
                        coefficient = as.numeric(as.character(coefficient))) 
tmp <- seq(as.POSIXct("2018-01-01 00:00:00"), as.POSIXct("2018-06-30 24:00:00"), by="hour") ## get sequence of hourly dates
tmp <- as.data.frame(tmp)
tmp <- rename(tmp, tsRound = tmp)
tide <- merge(tide, tmp, all = TRUE)
## NOTE! Here the date won't match ts to keep the coefficient with the correct date after time change!
tide <- mutate(tide, day = na.locf(day),
               Month = na.locf(Month),
               Year = na.locf(Year),
               date = paste(Year, Month, day, sep = "-"),
               date = as.Date(date))
tide <- tide[with(tide, order(tsRound)),]
tide <- tide %>% mutate(linear = na.interpolation(height),
                        spline = na.interpolation(height, option = "spline"),
                        stine = na.interpolation(height, option = "stine"),
                        coefficient = na.locf(coefficient))## fill in coefficient for each entry


## break down by periods of detection
##, ts > as.POSIXct("2018-02-26"), ts < as.POSIXct("2018-03-13"), speciesEN=="Red Knot"
visits <- filter(rekn, recvProjID == 174) %>% group_by(speciesEN, motusTagID, recvDeployName, recvDeployID, runID, batchID) %>%
  summarize(mints = min(ts),
            maxts = max(ts),
            numHits = length(motusTagID))
visits <- as.data.frame(visits)
visits <- visits[with(visits, order(motusTagID, mints)),]

visits <- visits %>%
  mutate(diffsec = as.numeric(mints) - lag(as.numeric(maxts)),
         diffmin = as.numeric(diffsec/60)) %>% as.data.frame

## assign a group number for each group of hits at a station per tag by time
visits <- visits[with(visits, order(motusTagID, mints)),] ## first make sure it's ordered correctly
#visits$count1 <- cumsum(c(0,as.numeric(diff(as.factor(visits$recvDeployName)))!=0))
visits <- visits %>% group_by(motusTagID) %>% mutate(count1 = cumsum(c(1,as.numeric(diff(as.factor(recvDeployName)))!=0)))

## assign a group number each time the time difference from one runID to another is > 5
visits <- visits[with(visits, order(motusTagID, mints)),] ## first make sure it's ordered correctly
visits$diffmin[is.na(visits$diffmin)] <- 0 ## set NA in time differences to 0
#visits$count2 <- cumsum(c(0,as.numeric(diff(visits$diffmin))>=5))
visits <- visits %>% group_by(motusTagID, count1) %>% mutate(count2 = c(cumsum(diffmin >5)),
                                                             visitID = paste(count1, count2, sep = ".")) ## now get one value to number each variable

## now lets get the total time for each "visit"
visits <- visits %>% group_by(speciesEN, motusTagID, recvDeployName, recvDeployID, visitID) %>%
  summarize(mints = min(mints),
            maxts = max(maxts))
visits <- visits %>% mutate(visitLength = as.numeric(difftime(maxts, mints), units = "mins"),
                            date = as.Date(mints),
                            tsRound = as.POSIXct(round(mints, "hours")))
visits <- merge(visits, select(recvs, recvDeployName, recvDeployID, recvLat, recvLon, recvStart, recvEnd, recvProjID), by = "recvDeployID", all.x = TRUE)
visits <- rename(visits, recvDeployName = recvDeployName.x)
visits <- select(visits, -recvDeployName.y)
## get distance travelled for the visit
visits <- visits[with(visits, order(as.numeric(motusTagID, mints))),]
visits$distance <- with(visits, latLonDist(lag(recvLat), lag(recvLon), recvLat, recvLon)) ## distance for a visit is the distance it took to get there from the last station
## add sunrise info
visits <- rename(visits, recvDeployLat = recvLat, recvDeployLon = recvLon, ts = mints) ## need to rename, need ot fix this in function code!
visits <- timeToSunriset(visits, units = "min")
visits <- rename(visits, recvLat = recvDeployLat, recvLon = recvDeployLon, mints = ts) ## need to rename, need ot fix this in function code!
visits <- merge(visits, select(tide, tsRound, linear), by = "tsRound", all = TRUE)
visits <- distinct(merge(visits, select(tide, date, coefficient), by = "date", all = TRUE))
day <- filter(visits, mints > sunrise & mints < sunset)
day$period <- "day"
night <- filter(visits, mints < sunrise | mints > sunset)
night$period <- "night"
visits <- rbind(day, night)



















#########################################################################################################
## EVERYTHING BEYOND HERE CAN BE IGNORED 
#########################################################################################################

## length of overall stay
mean <- rekn %>% group_by(motusTagID) %>% summarize(maxDet = max(ts),
                                                    minDet = min(ts),
                                                    length = difftime(maxDet, minDet, units = "days"))
mean(mean$length)

## mean length per station by day
meanSiteDay <- rekn %>% group_by(motusTagID, recvDeployName) %>%
  summarize(maxDet = max(ts),
            minDet = min(ts),
            length = difftime(maxDet, minDet, units = "days"))
meanSite <- meanSiteDay %>% group_by(recvDeployName) %>%
  summarize(mean = mean(length))

## mean length per station by n hits
meanSiteHits <- rekn %>% group_by(motusTagID, recvDeployName) %>%
  summarize(nHits = length(sig))
meanHits <- meanSiteHits %>% group_by(recvDeployName) %>%
  summarize(meanHits = mean(nHits))

## number of tags per station
nTagsStation <- rekn %>% group_by(recvDeployName) %>%
  summarize(ntags = length(unique(motusTagID)))

## number of stations per tag
nStationTag <- rekn %>% group_by(motusTagID) %>%
  summarize(nStation = length(unique(recvDeployName)))

## distrubution of tags at each station per day
rekn$date <- as.Date(rekn$ts)
nTagsDate <- rekn %>% group_by(date, recvDeployName, speciesEN) %>%
  summarize(nTags = length(unique(motusTagID)))

ggplot(nTagsDate, aes(date, nTags, fill = recvDeployName)) + theme_bw()+
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(speciesEN~., scales = "free_y") + labs(y = "n Tags") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
ggsave("./TagsPerDay.pdf")


## distrubution of hits at each station per day
nHitsDate <- rekn %>% group_by(date, recvDeployName, speciesEN) %>%
  summarize(nHits = length(motusTagID))


ggplot(nHitsDate, aes(date, nHits, fill = recvDeployName)) +
  geom_bar(stat = "identity", position = position_dodge()) + theme_bw() +
  facet_grid(speciesEN~., scales = "free_y") + labs(y = "n Hits") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
ggsave("./HitsPerDay.pdf")


## movement between stations
hourly <- mutate(rekn, hour = round_date(ts, "hours"))
hourly <- hourly %>% group_by(motusTagID, recvDeployName, hour) %>%
  mutate(numHits = length(motusTagID)) %>% as.data.frame()
hourly <- select(hourly, numHits, hour, motusTagID, recvDeployName, recvDeployLat, speciesEN) %>% distinct()
hourly <- hourly[with(hourly, order(hour)),]

ggplot(filter(hourly, motusTagID %in% c(27475, 27458, 27459, 26904, 26401, 27417, 27416, 27421, 27423, 27481, 27480)),
       aes(hour, recvDeployName, group= motusTagID, col = as.factor(motusTagID))) +
  geom_point() + geom_path() + facet_grid(speciesEN~.) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") + theme_bw()

ggplot(filter(hourly, motusTagID %in% c(27480)),
       aes(hour, recvDeployName, group= motusTagID, col = as.factor(motusTagID))) +
  geom_point() + geom_path() + facet_grid(speciesEN~.) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") + theme_bw()

## example tag 27470 sized by number of hits
ggplot(filter(hourly, motusTagID%in% c(27470)),
       aes(hour, recvDeployName, group= motusTagID, col = recvDeployName)) +
  geom_point(aes(size = numHits)) + geom_path(col = "black") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") + theme_bw()



## max number of days monitored
days <- visits %>% group_by(speciesEN, motusTagID) %>% summarize(mints  = min(mints),
                                                                 maxts = max(maxts),
                                                                 length = as.numeric(difftime(maxts, mints), units = "days"))
mean(filter(days, speciesEN == "Red Knot")$length)
mean(filter(days, speciesEN == "Hudsonian Godwit")$length)
## num hits
hits <- rekn %>% group_by(speciesEN) %>% summarize(nHits  = length(sig))


## proportion of time spent at each station per day (E. Pepita = roosting)
prop <- filter(visits, speciesEN== "Red Knot") %>% group_by(date, motusTagID, recvDeployName.x, coefficient) %>% 
  summarize(visitLength = sum(visitLength))
mean(filter(prop, recvDeployName.x == "E. Pepita")$visitLength)
mean(filter(prop, recvDeployName.x != "E. Pepita")$visitLength)


## number of visits to each station per day

## timing of visits to each station, ie. time of day vs tides


## distance travelled per day by tag
tagDist <- filter(visits, speciesEN == "Red Knot") %>% group_by(motusTagID, date) %>% summarize(totDist = sum(distance),
                                                                                                nVisits = length(motusTagID))
meanDist <- tagDist %>% group_by(date) %>% summarize(meanDist = mean(totDist),
                                                     nMeanVisits = mean(nVisits))






## roosting proportion
#26895, 26897, 26899, 26900, 26901, 26902, 26903, 26904, 26905, 26906, 27401, 27409, 27410, 27411, 27412, 27413, 
#27414, 27415, 27417, 27418, 27419, 27420, 27422, 27424, 27425, 27426, 27427, 27428, 27429, 27430, 27431, 27432, 
#27433, 27434, 27435, 27436, 27437, 27438, 27439, 27440, 27441, 27442, 27443, 27444, 27445, 27446, 27447, 27448, 
#27449, 27450, 27451, 27452, 27453, 27454, 27455, 27456, 27457, 27458, 27459, 27460, 27461, 27462, 27463, 27464, 
#27465, 27466, 27467, 27468, 27469, 27470, 27471, 27472, 27473, 27474, 27475, 27478, 27479, 27482


roost <- filter(rekn, recvProjID == 174)
roost <- sunRiseSet(roost)
tmp <- filter(roost, motusTagID %in% c(27409, 27410, 27411, 27412, 27413))
tmp <- tmp[with(tmp, order(ts)),]
ggplot(tmp, aes(ts, siteLat, col = as.factor(motusTagID), group = motusTagID)) + geom_path() + geom_point()

## example of shit roost maybe at another fucking site
tmp <- filter(roost, motusTagID == 26902)
ggplot(tmp, aes(ts, sig, col = siteLat)) + geom_point() + 
  geom_vline(xintercept = roost$sunset, col = "blue") +
  geom_vline(xintercept = roost$sunrise, col = "orange")
tmp <- filter(roost, motusTagID == 27411, ts > as.POSIXct("2018-02-01"), ts < as.POSIXct("2018-02-10"))
ggplot(tmp, aes(ts, sig, col = siteLat)) + geom_point(size = 0.2) + 
  geom_vline(xintercept = roost$sunset, col = "blue", size = .2) +
  geom_vline(xintercept = roost$sunrise, col = "orange", size = 0.2) + labs(title = "tagID 27411") +
  theme_bw() + theme(legend.position = "bottom", text = element_text(size = 5))
ggsave("/Users/zoecrysler/Desktop/27411.pdf")

## example of ok roost maybe
tmp <- filter(roost, motusTagID == 26899, ts > as.POSIXct("2018-01-25"), ts < as.POSIXct("2018-02-02"))
ggplot(tmp, aes(ts, sig, col = siteLat)) + geom_point(size = 0.2) + 
  geom_vline(xintercept = roost$sunset, col = "blue", size = .2) +
  geom_vline(xintercept = roost$sunrise, col = "orange", size = 0.2) + labs(title = "tagID 26899") +
  theme(legend.position = "bottom", text = element_text(size = 5)) + theme_bw()

tmp <- filter(roost, motusTagID == 26906, ts > as.POSIXct("2018-01-25"), ts < as.POSIXct("2018-01-30"))
ggplot(tmp, aes(ts, sig, col = siteLat)) + geom_point(size = 0.2) + 
  geom_vline(xintercept = roost$sunset, col = "blue", size = .2) +
  geom_vline(xintercept = roost$sunrise, col = "orange", size = 0.2) + labs(title = "tagID 26906") +
  theme_bw() + theme(legend.position = "bottom", text = element_text(size = 5))
ggsave("/Users/zoecrysler/Desktop/26906.pdf")

tmp <- filter(roost, motusTagID == 27409, ts > as.POSIXct("2018-02-11"), ts < as.POSIXct("2018-02-13"))
ggplot(tmp, aes(ts, sig, col = siteLat)) + geom_point(size = 0.2) + 
  geom_vline(xintercept = roost$sunset, col = "blue", size = .2) +
  geom_vline(xintercept = roost$sunrise, col = "orange", size = 0.2) + labs(title = "tagID 27409") +
  theme_bw() + theme(legend.position = "bottom", text = element_text(size = 5))
ggsave("/Users/zoecrysler/Desktop/27409.pdf")

## distribution of time at Pepita
ggplot(filter(prop, recvDeployName.x == "E. Pepita",date < "2018-02-01"), aes(as.factor(motusTagID), visitLength, group = as.factor(motusTagID))) + 
  geom_boxplot(lwd=0.2, outlier.size = 0.2) + theme_bw() + theme(text = element_text(size = 5)) + labs(title = "time spent at Pepita by ID for January")
ggsave("/Users/zoecrysler/Desktop/TagTimeAtPepita_January.pdf")

## distribution of time at Pepita
ggplot(filter(prop, recvDeployName.x == "E. Pepita"), aes(x = date)) + geom_boxplot(aes(y=visitLength, group = date)) +
  geom_line(aes(y=coefficient*5, col = "coefficient"))

ggplot(filter(prop, recvDeployName.x == "E. Pepita"), aes(date, visitLength, group = date)) + 
  geom_boxplot(lwd=0.2, outlier.size = 0.2) + theme_bw() + theme(text = element_text(size = 5)) + 
  labs(title = "mean time spent at pepita, alltags")
ggsave("/Users/zoecrysler/Desktop/meanTimePepita.pdf")

## number of detections vs tide
rekn <- rekn %>% group_by(motusTagID, recvDeployName, tsRound) %>% mutate(nhits = length(sig)) %>% as.data.frame()
hourly <- rekn %>% select(motusTagID, mfgID, speciesEN, tagDeployStart, recvDeployLat,
                          recvDeployLon, recvDeployName, tsRound, nhits, runLen) %>% distinct()
hourly <- merge(hourly, select(tide, tsRound, linear, coefficient, coefficientCategory), by = "tsRound", all.x = TRUE)
## add sunrise/sunset to get day vs night
hourly <- rename(hourly, ts = tsRound) ## need to rename, need ot fix this in function code!
hourly <- sunRiseSet(hourly)
hourly <- rename(hourly, tsRound = ts) ## need to rename, need ot fix this in function code!
day <- filter(hourly, tsRound > sunrise & tsRound < sunset)
day$period <- "day"
night <- filter(hourly, tsRound < sunrise | tsRound > sunset)
night$period <- "night"
hourly <- rbind(day, night)

## see distribution of day vs night and site
ggplot(hourly, aes(linear, nhits)) + geom_point() + facet_grid(period~recvDeployName)

## just see distribution for day vs night
ggplot(filter(hourly, tsRound < as.POSIXct("2018-02-01")), aes(tsRound, nhits, col = period)) + geom_point() + 
  geom_vline(xintercept = hourly$sunset, col = "blue") +
  geom_vline(xintercept = hourly$sunrise, col = "orange")

## tide and numhits on one plot
tmp <- filter(hourly, tsRound < as.POSIXct("2018-02-01"), recvDeployName == "E. Pepita")
sun <- tmp %>% select(sunrise, sunset) %>% distinct()
sun$group <- seq.int(nrow(sun))
p <- ggplot(tmp, aes(x = tsRound))
p <- p + geom_rect(data = sun, inherit.aes = FALSE, aes(xmin = sunrise, xmax = sunset, ymin=min(hourly$nhits), 
                                                        ymax = max(tmp$nhits), group = group), colour = "transparent", 
                   fill = "orange", alpha = 0.3)
p <- p + geom_rect(data = sun, inherit.aes = FALSE, aes(xmin = sunrise, xmax = lag(sunset), ymin=min(hourly$nhits), 
                                                        ymax = max(tmp$nhits), group = group), colour = "transparent", 
                   fill = "blue", alpha = 0.3)
p <- p + geom_point(aes(y = nhits, col = "period"))
p <- p + geom_line(aes(y = linear*5, col = "tide level"))
p <- p + scale_y_continuous(sec.axis = sec_axis(~./5, name = "tide level (m)"))
p <- p + scale_colour_manual(values = c("black", "blue"))
p
ggsave("./PepitaTides.pdf")

## coefficient and numhits on one plot
tmp <- filter(hourly, tsRound < as.POSIXct("2018-02-01"), recvDeployName == "E. Pepita")
sun <- tmp %>% select(sunrise, sunset) %>% distinct()
sun$group <- seq.int(nrow(sun))
p <- ggplot(tmp, aes(x = tsRound))
p <- p + geom_rect(data = sun, inherit.aes = FALSE, aes(xmin = sunrise, xmax = sunset, ymin=min(hourly$nhits), 
                                                        ymax = max(tmp$nhits), group = group), colour = "transparent", 
                   fill = "orange", alpha = 0.3)
p <- p + geom_rect(data = sun, inherit.aes = FALSE, aes(xmin = sunrise, xmax = lag(sunset), ymin=min(hourly$nhits), 
                                                        ymax = max(tmp$nhits), group = group), colour = "transparent", 
                   fill = "blue", alpha = 0.3)
p <- p + geom_point(aes(y = nhits, col = "period"))
p <- p + geom_line(aes(y = coefficient*5, col = "tide level"))
p <- p + scale_y_continuous(sec.axis = sec_axis(~./5, name = "tide level (m)"))
p <- p + scale_colour_manual(values = c("black", "blue"))
p
ggsave("./PepitaTides.pdf")

## lets look at detection periods vs.day and night (anova)
hvisits <- filter(visits, recvDeployName.x == "E. Pepita", speciesEN == "Red Knot") %>% group_by(motusTagID, date, period, recvDeployName.x) %>% summarize(totDailyLength = sum(visitLength))
ggplot(hvisits, aes(period, totDailyLength)) + geom_boxplot()

## lets look at detection periods vs.tide level (regression)
hvisits <- filter(visits, recvDeployName.x == "E. Pepita", speciesEN == "Red Knot") %>% group_by(motusTagID, tsRound, linear, recvDeployName.x) %>% 
  summarize(totHourlyLength = sum(visitLength))
ggplot(hvisits, aes(totHourlyLength, linear)) + geom_point()
## lets look at detection periods hours before/after sunriset


## modelling
# response: total time at a station (hour or day?): continuous
# explanatory: period(day/night), time before/after sunset, tide level = categorical and continuous
# therefore ANCOVA
hvisits <- filter(visits, recvDeployName.x == "E. Pepita", speciesEN == "Red Knot") %>% 
  group_by(date, motusTagID, tsRound, linear, recvDeployName.x, ts_to_set, ts_since_set, ts_to_rise, ts_since_rise, period) %>% 
  summarize(totHourlyLength = sum(visitLength))
m1 <- aov(totHourlyLength~linear*period, data = hvisits)
m2 <- aov(totHourlyLength~linear+period, data = hvisits)
summary(m1)
anova(m1, m2)
# so there is a significant interaction between linear and period
ggplot(hvisits, aes(date, totHourlyLength)) + facet_wrap(~period) + geom_point()


## coefficient vs tide height

ggplot(filter(tide, tsRound < as.POSIXct("2018-02-01")), aes(x = tsRound))  + geom_line(aes(y=coefficient, col = "coefficient")) + 
  geom_line(aes(y=linear*10, col = "height"))

