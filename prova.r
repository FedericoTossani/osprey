# ================================= #
#          2.Data import            #
# ================================= #

         list_csv <- list.files(pattern = "20")
         csv_allfile <- lapply(list_csv, read.csv)

         col_selected <- c("timestamp", "location.long", "location.lat", "external.temperature", 
                           "individual.local.identifier")

         csv_file_sel_col <- lapply(csv_allfile, "[", , col_selected)
         osprey_raw <- bind_rows(csv_file_sel_col)

         osprey_death <- read.csv("osprey_death.csv")


# ================================= #
#        3. Filtering data          #
# ================================= #

# dead ospreys

         osprey_dead <- c("Balearics2013_FOSP11-Juv_ringH7", "Corsica2013_FOSP17_Juv_ringCBK",
                          "Italy2017_FOSP43_juv_ringIBI_Agrippa", "Italy2018_FIOS45_juv_ringIAB_Anassagora")

# dataset to use for further analysis

         osprey <- osprey_raw %>%
                     dplyr::select("timestamp", "location.long", "location.lat", "external.temperature", 
                                   "individual.local.identifier")%>%
                     rename("lon"="location.long",
                            "lat"="location.lat",
                            "ext_temp"="external.temperature",
                            "id"="individual.local.identifier")%>%
                     mutate(id = as.factor(id),
                            time = as.POSIXct(timestamp, tz = "UTC"),
                            signal_interruption_cause = ifelse (id %in% osprey_dead, "Death", "GPS lifecycle"),
                            date = as_date(parse_date_time(timestamp, orders = c("%Y-%m-%d %H:%M:%S"))),
                            day = day(time),
                            month = month(time),
                            year = year(time))%>%
                     unite(m_day, c(month, day), sep="/", remove = F)%>%
                     left_join(osprey_death, by = c("id" = "id"))%>%
                     mutate( season = case_when(
                                month %in% 10:12 ~ "Fall",
                                month %in%  1:3  ~ "Winter",
                                month %in%  4:6  ~ "Spring",
                                TRUE ~ "Summer"),
                             ID = case_when(
                             id == "Balearics2013_FOSP11-Juv_ringH7" ~ "H7",
                             id == "Corsica2013_FOSP17_Juv_ringCBK" ~ "CBK",
                             id == "Corsica2014_FOSP21-Juv_ringCIV" ~ "CIV",
                             id == "Italy2014_FOSP27-Juv_ringE7_Edy" ~ "E7",
                             id == "Italy2015_FOSP30-juv-ringA7-Cook" ~ "A7",
                             id == "Italy2015_FOSP33-juv-Antares" ~ "Antares",
                             id == "Italy2016_FOSP28_juv_ringIAD_Ciccia" ~ "IAD",
                             id == "Italy2016_FOSP37-adult-ringCAM Mora" ~ "CAM",
                             id == "Italy2017_FOSP43_juv_ringIBI_Agrippa" ~ "IBI",
                             id == "Italy2018_FIOS45_juv_ringIAB_Anassagora" ~ "IAB",
                             id == "Italy2019_OrnitelaWhite_juv_ringICZ_Odaba" ~ "ICZ",
                             id == "Italy2020_FIOS21_juv_ringIBS_Mauna Loa" ~ "IBS",
                             id == "Italy2020_Ornitela_juv_ringIBH_Infiernillo" ~ "IBH",
                             id == "Italy2020_Ornitela_juv_ringIBK_Imbabura" ~ "IBK",
                             id == "Italy2022_OSPI08_juv_ringIFP_Ildebrando" ~ "IFP"))

         osprey<- osprey%>%
                  select(-c("timestamp", "id"))%>%
                  relocate("ID", "time", "date", "day", "month", "year", "m_day",
                                       "death_date", "season", "ext_temp", "lon", "lat", "signal_interruption_cause", "death_comment")%>%
                  unique()






# COORD vs TIME per animal

  # H7 
        h7 <- osprey%>%
            filter(ID == "H7")

        h7_lat_time <-
        ggplot(h7, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(h7$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

        h7_lon_time <-
        ggplot(h7, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(h7$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "H7 longitude and latitude movement graph")

        h7_lon_lat <- ggarrange(h7_lon_time, h7_lat_time, ncol = 1, nrow = 2)
         
        h7_lon_lat

       # ggsave("h7_lon_lat.jpg", plot=h7_lon_lat)
