


study <- read_tsv("data_files/Study2911040", comment="##",
                  col_types = "Tnncc")
study <- as.data.frame(study)
                       
albatross <- move(x=study$location_long,
              y=study$location_lat,
              time=study$timestamp,
              data=study,
              animal = as.factor(study$individual_id),
              sensor = as.factor(study$tag_id),
              proj=CRS("+proj=longlat"))
## Warning in .local(x, y, time, data, proj, ...): There were NA locations
## detected and omitted. Currently they are not stored in unusedrecords
head(albatross)
##             timestamp location_lat location_long
## 1 2008-05-31 13:30:02    -1.372641     -89.74021
## 2 2008-05-31 15:00:44    -1.372894     -89.74015
## 3 2008-05-31 16:30:39    -1.372881     -89.74014
## 4 2008-05-31 18:00:49    -1.372891     -89.74016
## 5 2008-05-31 19:30:18    -1.372912     -89.74013
## 6 2008-05-31 21:00:20    -1.372904     -89.74017
class(albatross)
## [1] "MoveStack"
## attr(,"package")
## [1] "move"
