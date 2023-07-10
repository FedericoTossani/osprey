
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


# The idea is to use this script to store all the work and thanks to the source() function keep it clean and tidy.
# From here you will be able to select between ospreys and variables and get the data visualization plots.


# =========================== #
         ## SUMMARY ##
# =========================== #

# 1.SetWD & packages
# 2.Data import & selection
# 3.Basemaps & extent
# 4. Processing data
# 5. 
# 6. 

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
