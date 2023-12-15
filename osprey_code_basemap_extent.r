# Let's import countries boundaries shapefile
                  countries <- vect('C:/Tesi/data/countries_boundaries_4326.shp')
                  osprey_ext <- ext(c(-7.436733, 21.24755, 35.40968, 55.77745))
                  osprey_eu <- crop(countries, osprey_ext)
                  osprey_eu_utm <- terra::project(osprey_eu, proj_crs)


# "A7"
                  # First let's crop
                           A7_ext <- ext(c(7.00000, 17.50000, 40.00000, 46.50000 ))
                           A7_eu <- crop(countries, A7_ext)
                           A7_eu_utm <- terra::project(A7_eu, proj_crs)

# "Antares" 
                  # First let's crop
                           Antares_ext <- ext(c(8.50000, 14.50000, 40.50000, 45.50000 ))
                           Antares_eu <- crop(countries, Antares_ext)
                           Antares_eu_utm <- terra::project(Antares_eu, proj_crs)

# "CAM"    
                  # First let's crop
                           CAM_ext <- ext(c(5.00000, 12.00000, 39.50000, 44.50000 ))
                           CAM_eu <- crop(countries, CAM_ext)
                           CAM_eu_utm <- terra::project(CAM_eu, proj_crs)

# "CBK"     
                  # First let's crop
                           CBK_ext <- ext(c(7.0000, 11.0000, 38.5000, 43.50000))
                           CBK_eu <- crop(countries, CBK_ext)
                           CBK_eu_utm <- terra::project(CBK_eu, proj_crs)

# "CIV"  
                  # First let's crop
                           CIV_ext <- ext(c(4.0000, 12.0000, 36.0000, 44.00000))
                           CIV_eu <- crop(countries, CIV_ext)
                           CIV_eu_utm <- terra::project(CIV_eu, proj_crs)

# "E7"     
                  # First let's crop
                           E7_ext <- ext(c(-1.00000, 17.5000, 36.9409, 49.00000))
                           E7_eu <- crop(countries, E7_ext)
                           E7_eu_utm <- terra::project(E7_eu, proj_crs)

# "H7"      
                  # First let's crop
                           H7_ext <- ext(c(-7.0000, 8.50000, 35.5000, 45.00000))
                           H7_eu <- crop(countries, H7_ext)
                           H7_eu_utm <- terra::project(H7_eu, proj_crs)

# "IAB"     
                  # First let's crop
                           IAB_ext <- ext(c(7.00000, 17.50000, 37.00000, 45.00000 ))
                           IAB_eu <- crop(countries, IAB_ext)
                           IAB_eu_utm <- terra::project(IAB_eu, proj_crs)

# "IAD"    
                  # First let's crop
                           IAD_ext <- ext(c(1.50000, 19.50000, 38.00000, 54.00000 ))
                           IAD_eu <- crop(countries, IAD_ext)
                           IAD_eu_utm <- terra::project(IAD_eu, proj_crs)

# "IBH"     
                  # First let's crop
                           IBH_ext <- ext(c(9.00000, 16.50000, 36.50000, 45.50000))
                           IBH_eu <- crop(countries, IBH_ext)
                           IBH_eu_utm <- terra::project(IBH_eu, proj_crs)

# "IBI"    
                  # First let's crop
                           IBI_ext <- ext(c(6.50000, 13.00000, 41.50000, 45.00000))
                           IBI_eu <- crop(countries, IBI_ext)
                           IBI_eu_utm <- terra::project(IBI_eu, proj_crs)

# "IBK"    
                  # First let's crop
                           IBK_ext <- ext(c(9.00000, 12.50000, 41.50000, 44.50000))
                           IBK_eu <- crop(countries, IBK_ext)
                           IBK_eu_utm <- terra::project(IBK_eu, proj_crs)

# "IBS"     
                  # First let's crop
                           IBS_ext <- ext(c(2.00000, 21.00000, 36.50000, 48.00000 ))
                           IBS_eu <- crop(countries, IBS_ext)
                           IBS_eu_utm <- terra::project(IBS_eu, proj_crs)

# "ICZ"     
                  # First let's crop
                           ICZ_ext <- ext(c(10.00000, 16.00000, 37.00000, 44.00000 ))
                           ICZ_eu <- crop(countries, ICZ_ext)
                           ICZ_eu_utm <- terra::project(ICZ_eu, proj_crs)

# "IFP"
                  # First let's crop
                           IFP_ext <- ext(c(4.00000, 17.00000, 41.00000, 46.00000))
                           IFP_eu <- crop(countries, IFP_ext)
                           IFP_eu_utm <- terra::project(IFP_eu, proj_crs)

