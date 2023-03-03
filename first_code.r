
# ------------------------------------------------ #       
#         Ranging behavior of non-breeding
#         Mediterranean ospreys tracked by
#         GPS-satellite telemetry
# ------------------------------------------------ #



# Relatore: Prof. Duccio Rocchini
# Correlatori: Dott. Francesco Pezzo
#              Dott. Flavio Monti
#              Dott. Andrea Sforzi
# Univesity: Alma Mater Studiorum University of Bologna



# =========================== #
         ## SUMMARY ##
# =========================== #

# 1. SetWD and packages
# 2. Data import
# 3. Filtering data
# 4. 
# 5. 
# 6. 

# ================================ #
      ## SetWD and packages ##
# ================================ #
setwd("C:/Tesi/R/osprey/data/")

list.of.packages <- c("tidyverse",
                      "dplyr",
                      "lubridate",
                      "sf", 
                      "mapview", 
                      "move",
                      "adehabitatLT",
                      "stargazer",
                      "gt",
                      "kableExtra")

# with this line of code I check if alll the packages are installed and then I load it

{
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) install.packages(new.packages)
  
  lapply(list.of.packages, require, character.only = TRUE)
}

# ================================= #
          ## Data import ##
# ================================= #


osprey_raw <- read.csv("data/osprey_all.csv")

list_csv <- list.files(pattern = "20")
csv_allfile <- lapply(list_csv, read.csv)

col_selected <- c("timestamp", "location.long", "location.lat", "external.temperature", 
                  "gsm.gsm.signal.strength", "sensor.type", "individual.local.identifier")

csv_file_sel_col <- lapply(csv_allfile, "[", , col_selected)
osprey_raw_2 <- bind_rows(csv_file_sel_col)


osprey_death <- read.csv("data/osprey_death.csv")

sum(is.na(osprey_raw$timestamp))

# ================================= #
         ## Filtering data ##
# ================================= #

# dead ospreys

osprey_dead <- c("Balearics2013_FOSP11_Juv_ringH7", "Corsica2013_FOSP17_Juv_ringCBK",
                 "Italy2017_FOSP43_juv_ringIBI_Agrippa", "Italy2018_FIOS45_juv_ringIAB_Anassagora")

osprey <- osprey_raw %>%
            dplyr::select("timestamp", "location_long", "location_lat", "external_temperature", 
                          "gsm.gsm_signal_strength", "sensor_type", "individual_local_identifier")%>%
            rename("long"="location_long",
                   "lat"="location_lat",
                   "ext_temp"="external_temperature",
                   "gsm_signal_strength"="gsm.gsm_signal_strength",
                   "sensor_type"="sensor_type",
                   "id"="individual_local_identifier")%>%
            mutate(id = as.factor(id),
                   timestamp = as.POSIXct(timestamp, tz = "UTC"),
                   signal_interruption_cause = ifelse (id %in% osprey_dead, "Death", "GPS lifecycle"),
                   day = day(timestamp),
                   month = month(timestamp),
                   year = year(timestamp))%>%
            unite(m_day, c(month, day), sep="/", remove = F)%>%
            left_join(osprey_death, by = c("id" = "id"))%>%
            mutate( season = case_when(
              month %in% 10:12 ~ "Fall",
              month %in%  1:3  ~ "Winter",
              month %in%  4:6  ~ "Spring",
              TRUE ~ "Summer"),
                    ring_id = case_when(
                    id == "Balearics2013_FOSP11_Juv_ringH7" ~ "H7",
                    id == "Corsica2013_FOSP17_Juv_ringCBK" ~ "CBK",
                    id == "Corsica2014_FOSP21_Juv_ringCIV" ~ "CIV",
                    id == "Italy2014_FOSP27_Juv_ringE7_Edy" ~ "E7",
                    id == "Italy2015_FOSP30_juv_ringA7_Cook" ~ "A7",
                    id == "Italy2015_FOSP33_juv_Antares" ~ "Antares",
                    id == "Italy2016_FOSP28_juv_ringIAD_Ciccia" ~ "IAD",
                    id == "Italy2016_FOSP37_adult_ringCAM Mora" ~ "CAM",
                    id == "Italy2017_FOSP43_juv_ringIBI_Agrippa" ~ "IBI",
                    id == "Italy2018_FIOS45_juv_ringIAB_Anassagora" ~ "IAB",
                    id == "Italy2019_OrnitelaWhite_juv_ringICZ_Odaba" ~ "ICZ",
                    id == "Italy2020_FIOS21_juv_ringIBS_Mauna Loa" ~ "IBS",
                    id == "Italy2020_Ornitela_juv_ringIBH_Infiernillo" ~ "IBH",
                    id == "Italy2020_Ornitela_juv_ringIBK_Imbabura" ~ "IBK"))

head(osprey)


# let's check if there is na in position's columns
table(is.na(osprey$long))
table(is.na(osprey$lat))


# let's calculate some basic statistics per individual
table(osprey$id) %>% sd


# osprey fix summary stat per individual
osprey_fix_stat_id <- 
table(osprey$id) %>%
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
difftime(max(osprey$timestamp), min(osprey$timestamp), units = "days")
#Time difference of 3539.25 days

(difftime(max(osprey$timestamp), min(osprey$timestamp), units = "days") %>% as.numeric)/365.25
# 9.69 years

# create a table that shows the number of fix per seasons grouped by individual
osprey_summary1 <- osprey %>%
  group_by(id, season) %>%
  count(season)%>%
  arrange(id,desc(n))

# gt_table code

              data_blank <- osprey_summary1

              data_blank <- sapply(data_blank, as.character)
              data_blank[is.na(data_blank)] <- ""                     # Replace NA with blank
              data_blank  

              # constants ----
              n = 0
              c_col = c("#1e3048", "#274060", "#2f5375", "#4073a0", "#5088b9")
              c_col_light_blue = c("#edf2fb", "#e2eafc", "#d7e3fc", "#ccdbfd", "#c1d3fe")
              c_container_width = px(800)
              c_table_width = px(650)
              c_rn = 30
              c_save = TRUE
              c_format = "pdf"

              # Questo pezzo di codice serve per organizzare i dati in base ai valori di una colonna
              # in questo caso raggruppa i falchi pescatori per nome

              gt_season <- osprey_summary1 %>% 
                arrange(id) %>% 
                gt(
                  groupname_col = "id",
                  rowname_col = "season"
                )

              # Usa il codice seguente per modicificare il nome delle colonne

              gt_season <- gt_season %>%
                tab_header(
                  title = md("Number of fix per season per individual"),
                  subtitle = md("Project: Osprey in Mediterranean (Corsica, Italy, Balearics)"))%>%
                cols_label(
                  id = md("Name"),
                  season = md("Season"),
                  n = md("Number of fix")
                )

              gt_season <- gt_season %>% 
                #fmt_date(
                # columns = c("start", "end"),
                #) %>% 
                fmt_number(
                  columns = c("n"),
                  suffixing = F,
                  decimals = 0,
                  sep_mark = ""
                ) %>% 
                cols_align(
                  align = "center",
                  columns = vars(c("n"))
                ) %>% 
                cols_align(
                  align = "right",
                  columns = vars(c("season"))
                ) %>% 
                # cols_align(
                #   align = "left",
                #   columns = vars(c("))
                # ) %>% 
                cols_width(
                  #vars(c("")) ~ px(150),
                  vars("n") ~ px(200),
                  vars("season") ~ px(300),
                  vars("id") ~ px(370),
                ) %>%
                opt_row_striping()

              # let's add summary statistics
              gt_season <- gt_season%>%
                summary_rows(
                  groups = T,
                  columns = vars("n"),
                  fns = list(AVERAGE = "mean"),
                  formatter = fmt_number,
                  decimals = 0,
                  sep_mark = ""
                )


              gt_season <- gt_season %>%
                tab_options(
                  table.font.name = "Calibri",
                  table.font.color = c_col[1],
                  table.border.top.style = "none",
                  table.border.bottom.style = "solid",
                  table.border.bottom.color = c_col[2],
                  table.border.bottom.width = px(3),
                  column_labels.border.top.color = "white",
                  column_labels.border.top.width = px(3),
                  column_labels.border.bottom.color = c_col[2],
                  column_labels.border.bottom.width = px(3),
                  data_row.padding = px(10)
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      size = px(38),
                      weight = "normal",
                      align = "left",
                      font = "Calibri"
                    )
                  ),
                  locations = list(
                    cells_title(groups = "title")
                  )
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      size = px(18),
                      align = "left"
                    )
                  ),
                  locations = list(
                    cells_title(groups = "subtitle")
                  )
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      size = px(17)
                    ),
                    cell_borders(
                      sides = c("bottom", "top"),
                      color = c_col[1],
                      weight = px(1)
                    )
                  ),
                  locations = list(
                    cells_body(gt::everything())
                  )
                ) %>% 
                tab_style(
                  style = list( 
                    cell_text(
                      size = px(18),
                      color = "#000000",
                      weight = "bold",
                      font = "Calibri"
                    )
                  ),
                  locations = list(
                    cells_column_labels(everything())
                  )
                ) %>% 
                tab_style(
                  style = list( 
                    cell_text(
                      size = px(18),
                      color = "#000000",
                      font = "Calibri"
                    ),
                    cell_borders(
                      sides = c("bottom"),
                      style = "solid",
                      color = c_col[1],
                      weight = px(2)
                    )
                  ),
                  locations = list(
                    cells_row_groups(gt::everything())
                  )
                ) %>% 
                tab_style(
                  style = list( 
                    cell_text(
                      size = px(18),
                      align = "right",
                      weight = "normal",
                      color = "#000000",
                      font = "Calibri"
                    ),
                    cell_borders(
                      sides = c("bottom", "right"),
                      style = "solid",
                      color = "white",
                      weight = px(1)
                    )
                  ),
                  locations = list(
                    cells_stub(gt::everything()),
                    cells_stubhead()
                  )
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      font = "Calibri", size = px(10), 
                      color = "#000000")
                  ),
                  location = list(
                    cells_body(columns = vars(id))
                  )
                ) 

gt_season

gtsave(gt_season, "seasons_fix_gt.html")

# create a table that shows monitoring's duration per individual grouped by signal interrumption cause
osprey_summary2 <- osprey %>%
  group_by(id, signal_interruption_cause) %>% 
  summarize(start = min(timestamp), end = max(timestamp)) %>%
  mutate(duration = round(difftime(end, start)))%>%
  arrange(desc(signal_interruption_cause))

osprey_summary2$duration <- as.numeric(osprey_summary2$duration)

# gt_table code

              data_blank <- osprey_summary2

              data_blank <- sapply(data_blank, as.character)
              data_blank[is.na(data_blank)] <- ""                     # Replace NA with blank
              data_blank  

              # constants ----
              n = 0
              c_col = c("#1e3048", "#274060", "#2f5375", "#4073a0", "#5088b9")
              c_col_light_blue = c("#edf2fb", "#e2eafc", "#d7e3fc", "#ccdbfd", "#c1d3fe")
              c_container_width = px(800)
              c_table_width = px(650)
              c_rn = 30
              c_save = TRUE
              c_format = "pdf"

              # Questo pezzo di codice serve per organizzare i dati in base ai valori di una colonna
              # in questo caso raggruppa i falchi pescatori per la causa di interruzione del segnale GPS

              gt_duration <- osprey_summary2 %>% 
                arrange(id) %>% 
                gt(
                  groupname_col = "signal_interruption_cause",
                  rowname_col = "id"
                )

              # Usa il codice seguente per modicificare il nome delle colonne

              gt_duration <- gt_duration %>%
                tab_header(
                  title = md("Monitoring's duration per individual"),
                  subtitle = md("Osprey in Mediterranean (Corsica, Italy, Balearics)"))%>%
               cols_label(
                 id = md("Name"),
                 signal_interruption_cause = md("Signal interruption cause"),
                 start = md("First location date"),
                 end = md("Last location date"),
                 duration = md("Total days of monitoring")
               )

              gt_duration <- gt_duration %>% 
                #fmt_date(
                # columns = c("start", "end"),
                #) %>% 
                fmt_number(
                  columns = c("duration"),
                  suffixing = F,
                  decimals = 0,
                  sep_mark = ""
                ) %>% 
                cols_align(
                  align = "center",
                  columns = vars(c("start", "end",
                                   "signal_interruption_cause", "duration"))
                ) %>% 
                cols_align(
                 align = "right",
                 columns = vars(c("id"))
                ) %>% 
                # cols_align(
                #   align = "left",
                #   columns = vars(c("))
                # ) %>% 
                cols_width(
                  #vars(c("")) ~ px(150),
                  vars("start", "end", "signal_interruption_cause") ~ px(200),
                  vars("duration") ~ px(200),
                  vars("id") ~ px(370),
                ) %>%
                opt_row_striping()

              # let's add summary statistics
              gt_duration <- gt_duration%>%
                  summary_rows(
                    groups = T,
                    columns = vars("duration"),
                  fns = list(AVERAGE = "mean"),
                  formatter = fmt_number,
                  decimals = 0
                  )


              gt_duration <- gt_duration %>%
                tab_options(
                  table.font.name = "Calibri",
                  table.font.color = c_col[1],
                  table.border.top.style = "none",
                  table.border.bottom.style = "solid",
                  table.border.bottom.color = c_col[2],
                  table.border.bottom.width = px(3),
                  column_labels.border.top.color = "white",
                  column_labels.border.top.width = px(3),
                  column_labels.border.bottom.color = c_col[2],
                  column_labels.border.bottom.width = px(3),
                  data_row.padding = px(10)
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      size = px(38),
                      weight = "normal",
                      align = "left",
                      font = "Calibri"
                    )
                  ),
                  locations = list(
                    cells_title(groups = "title")
                  )
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      size = px(18),
                      align = "left"
                    )
                  ),
                  locations = list(
                    cells_title(groups = "subtitle")
                  )
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      size = px(17)
                    ),
                    cell_borders(
                      sides = c("bottom", "top"),
                      color = c_col[1],
                      weight = px(1)
                    )
                  ),
                  locations = list(
                    cells_body(gt::everything())
                  )
                ) %>% 
                tab_style(
                  style = list( 
                    cell_text(
                      size = px(18),
                      color = "#000000",
                      weight = "bold",
                      font = "Calibri"
                    )
                  ),
                  locations = list(
                    cells_column_labels(everything())
                  )
                ) %>% 
                tab_style(
                  style = list( 
                    cell_text(
                      size = px(18),
                      color = "#000000",
                      font = "Calibri"
                    ),
                    cell_borders(
                      sides = c("bottom"),
                      style = "solid",
                      color = c_col[1],
                      weight = px(2)
                    )
                  ),
                  locations = list(
                    cells_row_groups(gt::everything())
                  )
                ) %>% 
                tab_style(
                  style = list( 
                    cell_text(
                      size = px(18),
                      align = "right",
                      weight = "normal",
                      color = "#000000",
                      font = "Calibri"
                    ),
                    cell_borders(
                      sides = c("bottom", "right"),
                      style = "solid",
                      color = "white",
                      weight = px(1)
                    )
                  ),
                  locations = list(
                    cells_stub(gt::everything()),
                    cells_stubhead()
                  )
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      font = "Calibri", size = px(10), 
                      color = "#000000")
                  ),
                  location = list(
                    cells_body(columns = vars(signal_interruption_cause))
                  )
                ) 

gt_duration

gtsave(gt_duration, "duration_monit_gt.html")


# visualize monitoring duration per individual
n_summary <- osprey %>%
                group_by(ring_id, signal_interruption_cause) %>% 
                summarize(start = min(timestamp), end = max(timestamp))%>% 
                arrange(start) %>% 
                mutate(ring_id = factor(ring_id, levels = ring_id))

bystart <- with(n_summary, reorder(ring_id, start))

  ggplot(n_summary, aes(y = ring_id, xmin = start, xmax = end, color = signal_interruption_cause)) + 
    geom_linerange()+
    theme_bw()+
    xlab("Year")+
    ylab("Animal ID")+
    labs(color = "Signal interruption cause",
         title = "Monitoring's duration",
         subtitle = "Plot of monitoring's duration per individual",
         caption = "Data source: Osprey in Mediterranean (Corsica, Italy, Balearics)")+
    theme(legend.position = "bottom")

# ========================================== #
#       Processing data
# ========================================== #


getMoveStats <- function(df){
  # df - is a generic data frame that will contain X,Y and Time columns
  Z <- df$long + 1i*df$lat
  Time <- df$timestamp
  Step <- c(NA, diff(Z)) # we add the extra NA because there is no step to the first location
  dT <- c(NA, difftime(Time[-1], Time[-length(Time)], hours) %>% as.numeric)
  
  SL <- Mod(Step)/1e3 # convert to km - so the speed is in km/hour
  MR <- SL/dT # computing the movement rate

  # this is what the function returns
  data.frame(df, Z, dT, Step, SL, MR)
}



osprey <- osprey %>% 
  plyr::ddply(c("ring_id", "year", "season"), getMoveStats)


head(osprey[, 15:21])

ggplot(osprey, aes(ring_id, MR)) +
geom_boxplot() +
facet_wrap(.~season)


# ------------- NON FUNZIONA ------------- #
getDuplicatedTimestamps(x=as.factor(osprey$ring_id), 
                        timestamps=as.POSIXct(osprey$timestamp, 
                                              format="%Y-%m-%d %H:%M:%S", tz="UTC"),
                        sensorType=osprey$sensor_type)

osprey_2 <- osprey[!duplicated(osprey),]

osprey_move <- move(x=osprey$long,
               y=osprey$lat,
               time=as.POSIXct(osprey$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
               data=osprey,
               animal = as.factor(osprey$ring_id),
               sensor = as.factor(osprey$sensor_type),
               proj=CRS("+proj=longlat"))

# plot all the individuals
plot(osprey_move, type="l") # all of them

# split the stack to plot each individuals
ids <- split(osprey_move)

par(mfrow=c(2,2))
plot(ids[[1]], type='l', main= names(ids[1]))
plot(ids[[2]], type='l', main= names(ids[2]))
plot(ids[[3]], type='l', main= names(ids[3]))
plot(ids[[4]], type='l', main= names(ids[4]))

# convert our movestack to a sf object
osprey_sf <- 
  osprey_raw %>% 
  na.omit() %>%
  st_as_sf(coords = c("location.long", "location.lat"), crs=4326)

osprey_sf %>% 
  group_by(tag.local.identifier) %>% 
  tally()









