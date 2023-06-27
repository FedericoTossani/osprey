# this is the list of all packages required in this project

         list.of.packages <- c("tidyverse",
                               "lubridate",
                               "sf", 
                               "mapview", 
                               "adehabitatLT",
                               "adehabitatHR",
                               "stargazer",
                               "kableExtra",
                               "ggmap",
                               "terra",
                               "tidyterra",
                               "gridExtra",
                               "ggpubr")

         # with this line of code I check if alll the packages are installed and then I load it

         {
           new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

           if(length(new.packages)) install.packages(new.packages)

           lapply(list.of.packages, require, character.only = TRUE)
         }
