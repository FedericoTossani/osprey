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


         # Estimating movement rate

                  crdref <- "+proj=longlat +datum=WGS84"
                  osprey_shp <- vect(osprey, crs=crdref)
                  osprey_df <- as.data.frame(osprey_shp)


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


                  osprey_move <- getMoveStats(osprey) 
                                    
                  ggplot(osprey_move, aes(ID, MR)) +
                           geom_boxplot()


                  mr_summarystats <- osprey_move %>%
                           plyr::ddply(c("ID", "season"), summarize,
                            min = min(MR, na.rm = TRUE), max = max(MR, na.rm = TRUE),
                            n = length(MR), NA.count = sum(is.na(MR)),
                            Zero.count = sum(MR == 0, na.rm = TRUE))
