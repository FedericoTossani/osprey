# =============================== #
#       1.SetWD & packages       #
# =============================== #

# Here you will load all the packages needed and also the working directory
          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_WD_packages.r")

# =========================================== #
#          2.Data import & selection          #
# =========================================== #

# Here you will load the raw dataset and the filtered one 
          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_data_import_selection.r")

# ===================================== #
#          3.Basemaps & extent          #
# ===================================== #

# Here you will load all the maps needed and also the cropped geographic extent for all individual ospreys
          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_basemap_extent.r")


# ============================================ #
#            Descriptive statistics            #
# ============================================ #



# NSD #

nsds <- do.call(rbind, osprey_lt)


osp_nsd <- osp_lt_df %>%
  group_by(id) %>%
  summarize(nsd = sum(R2n),
            msd = mean(R2n))%>%
  arrange(desc(nsd))
osp_nsd

nsd_plot <- ggplot(osp_nsd, aes(x = reorder(id, -nsd), y = nsd)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Net Squared Displacement (NSD) for Each Individual",
       x = "Individual ID",
       y = "NSD Value") +
  theme_minimal()
nsd_plot

#ggsave("path/to/save/your/plot.jpg", nsd_plot, width = 8, height = 6)

r2n_plot <- ggplot(osp_lt_df, aes(x = date, y = R2n)) +
  geom_line() +
  labs(title = "Squared Distance",
       x = "Date",
       y = "R2n Value") +
  facet_wrap(~id)+
  theme_minimal()
r2n_plot


###########################
# Visualize the movements #
###########################

         # let's check if there is na in position's columns
         table(is.na(osprey$lon))
         table(is.na(osprey$lat))

         countries <- vect('C:/Tesi/data/countries_boundaries_4326.shp')

         #
         osprey_ext <- ext(c(-7.436733, 21.24755, 35.40968, 55.77745))
         osprey_eu <- crop(countries, osprey_ext)


         osprey_track <- 
         ggplot(osprey_eu) +
         geom_spatvector() + 
          geom_path(data = osprey, aes(x = lon, y = lat, color = ID), 
                     linewidth = 0.5, lineend = "round") +
         labs(x = " ", y = " ", title = "Inividual tracks") +
         #facet_wrap(~ ID) +
         theme(legend.position="none") +
         theme_minimal()

         osprey_track

         # This should create a more beautiful map BUT need to be fix!
         # cbbox <- make_bbox(lon = osprey$lon, lat = osprey$lat, f = .1) #from ggmap
         # sq_map <- get_map(location = cbbox, maptype = "terrain", source = "stamen")


##########################
# Distances between fix  #
##########################

# H7


         # Estimating movement rate
                  h7_winter <- osprey%>%
                                    dplyr::filter(ID == "H7" & date >= "2014-12-01" & date < "2015-04-02")%>%
                                    dplyr::select(ID, time, lon, lat)   

                  crdref <- "+proj=longlat +datum=WGS84"
                  h7_winter_shp <- vect(h7_winter, crs=crdref)
                  h7_winter_df <- as.data.frame(h7_winter_shp)


                  getMoveStats <- function(df){
                    # df - is a generic data frame that will contain X,Y and Time columns
                    Z <- df$lon + 1i*df$lat
                    Time <- df$time
                    Step <- c(NA, diff(Z)) # we add the extra NA because there is no step to the first location
                    dT <- c(NA, difftime(Time[-1], Time[-length(Time)], hours) %>% as.numeric)

                    SL <- Mod(Step)/1e3 # convert to km - so the speed is in km/hour
                    MR <- SL/dT # computing the movement rate

                    # this is what the function returns
                    data.frame(df, Z, dT, Step, SL, MR)
                  }


                  h7_winter_move <- getMoveStats(h7_winter) 
                                    
                  ggplot(h7_winter_move, aes(ID, MR)) +
                           geom_boxplot()


                  mr_summarystats <- h7_winter_move %>%
                           plyr::ddply(c("ID"), summarize,
                            min = min(MR, na.rm = TRUE), max = max(MR, na.rm = TRUE),
                            n = length(MR), NA.count = sum(is.na(MR)),
                            Zero.count = sum(MR == 0, na.rm = TRUE))

         # Mean ditance between fixs during winter

         # Mean ditance between fixs during natal dispersal




##########################
# Descriptive statistics #
##########################

# Usa questo pezzo di codice per esportare tabelle in Latex

è

Desc(osprey)

d.ID <- Desc(osprey$ID, maxrows=15)
tab_ID <- d.ID[[1]]$freq

         dates <- osprey%>%
                  group_by(ID)%>%
                  summarize(start = min(time), end = max(time))%>% 
                  dplyr::select(ID, start, end)%>%
                  unique()

tab_ID <- tab_ID%>%
                  left_join(dates, by = c("level" = "ID"))%>%
                  mutate(duration = round(difftime(end, start)))

tab_ID <- tab_ID%>%
         mutate(perc = perc*100,
                cumperc = cumperc*100)

# export this table to tex

         tab_ID %>%
             kable(format = 'latex', booktabs = TRUE, digits = c(0,0,1,0,1,0,0,0)) 

########################
## Mean fixes per day ##
########################

         osprey_mean_fix <-
         osprey%>%
         group_by(ID)%>%
         count(date)%>%
         summarize(mean_day_fix = mean(n), 
                   max_day_fix = max(n), 
                   min_day_fix = min(n))



# osprey fix summary stat per individual
         osprey_fix_stat_id <- 
                  table(osprey$ID) %>%
                  data.frame %>%
                  summary

#                               Var1        Freq      
# Balearics2013_FOSP11-Juv_ringH7 :1   Min.   : 4007  
# Corsica2013_FOSP17_Juv_ringCBK  :1   1st Qu.: 9338  
# Corsica2014_FOSP21-Juv_ringCIV  :1   Median :13026  
# Italy2014_FOSP27-Juv_ringE7_Edy :1   Mean   :14149  
# Italy2015_FOSP30-juv-ringA7-Cook:1   3rd Qu.:16566  
# Italy2015_FOSP33-juv-Antares    :1   Max.   :39531 
# (Other)                         :8 

         dates <- osprey%>%
                  group_by(ID)%>%
                  summarize(start = min(time), end = max(time))%>% 
                  dplyr::select(ID, start, end)%>%
                  unique()


         osprey_fix_stat_id_tab <- osprey%>%
                  group_by(ID)%>%
                  count(ID)%>%
                  mutate(n_fix = n,
                         percent = (n/sum(osprey_fix_stat_id_tab$n))*100)%>%
                  dplyr::select(-n)%>%
                  left_join(dates, by = ('ID'))%>%
                  arrange(start)%>%
                  mutate(duration = round(difftime(end, start)))

# export this table to tex

         osprey_fix_stat_id_tab %>%
             kable(format = 'latex', booktabs = TRUE) 

# osprey fix summary stat per season
         osprey_fix_stat_season <- 
           table(osprey$season) %>%
           data.frame %>%
           summary

         #       Var1        Freq      
         # Fall  :1   Min.   :40266  
         # Spring:1   1st Qu.:45368  
         # Summer:1   Median :48048  
         # Winter:1   Mean   :49522  
         #            3rd Qu.:52202  
         #            Max.   :61725 

# number of fix per season
         osprey%>%
           group_by
           count(season)

         #   season     n
         #     Fall 47069
         #   Spring 49027
         #   Summer 61725
         #   Winter 40266

# for how long this ospreys are monitored?
         difftime(max(osprey$time), min(osprey$time), units = "days")
         #Time difference of 3645.625 days

         (difftime(max(osprey$time), min(osprey$time), units = "days") %>% as.numeric)/365.25
         #  9.98 years

# create a table that shows the number of fix per seasons grouped by individual
         osprey_summary1 <- osprey %>%
           group_by(ID, season) %>%
           count(season)%>%
           arrange(ID,desc(n))

         osprey_plot_season <-
                  osprey_summary1%>%
                  ggplot(osprey, mapping = aes(x = ID, y = n, fill = season))+
                  geom_bar(stat = "identity")+
                  geom_text(aes(label = n), vjust = 1)+
                  labs()+
                  theme_bw()+         
                  facet_wrap(~season)


# summary lon lat delta

lon_lat_summary <- osprey%>%
                           arrange(ID)%>%
                           group_by(ID)%>%
                           summarize(dlon = max(osprey$lon)-lag(osprey$lon),
                                     dlat = max(osprey$lat)-lag(osprey$lat))

lon_lat_summary <- osprey %>%
         group_by(ID)%>%
         mutate(lat_delta = c(0, diff(osprey$lat)),
                    lon_delta = c(0, diff(osprey$lon))) %>%
           summarise(max_lat_delta = max(lat_delta),
                     max_lon_delta = max(lon_delta))


# visualize monitoring duration per individual
         n_summary <- osprey %>%
                         group_by(ID) %>% 
                         summarize(start = min(time), end = max(time))%>% 
                         arrange(start) %>% 
                         mutate(ID = factor(ID, levels = ID))

         osprey_dead <- osprey%>%
                           dplyr::select(c("signal_interruption_cause", "ID"))%>%
                           drop_na(signal_interruption_cause)%>%
                           unique()

         n_summary <- n_summary%>%
                           left_join(osprey_dead, by = c("ID"))%>% 
                           mutate(ID = factor(ID, levels = ID))

         Start <- min(n_summary$start)
         End <- max(n_summary$end)
         as.vector(difftime(End, Start, units='years'))
#[1] 707

         bystart <- with(n_summary, reorder(ID, start))

         ggplot(n_summary, aes(y = ID, xmin = start, xmax = end, colour = )) + 
         geom_linerange(linewidth = 1)+
         geom_jitter(data = n_summary[n_summary$signal_interruption_cause == "Death", ], aes(x = start, y = ID), position = position_nudge(y = 0.3), shape = "†", size=4, color="black")+
         geom_text(aes(x = start, y = ID, label = ID), nudge_y = -0.25) +
         theme_bw()+
         xlab("Year")+
         ylab("Animals ID")+
         theme(axis.text.x = element_text(color="#000000", size=10),
               axis.text.y = element_text(color="#000000", size=10),
               axis.title.x = element_text(color="#000000", size=13, face="bold"),
               axis.title.y = element_text(color="#000000", size=13, face="bold"))

         ggsave( "mon_duration.jpg", plot = last_plot())

