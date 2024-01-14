## code to prepare `distance_brazil` dataset goes here
library(countries)
library(sf)
countries_0 <- i_tourism_br%>%
  distinct(iso3)%>%
  na.omit()%>%
  left_join(countries::world, by=c("iso3"="ISO3"))%>%
  filter(!is.na(lat))%>%
  st_as_sf(coords=c("lat", "long"))
brazil <- countries::world%>%filter(ISO3=="BRA")%>%
  st_as_sf(coords=c("lat", "long"))
brazil_distances <- st_distance(brazil, countries_0)
countries_0$dist_brazil <- apply(brazil_distances,2,mean)

distance_brazil <- countries_0%>%
  sf::st_drop_geometry()%>%
  group_by(iso3)%>%
  summarise(dist_brazil=mean(dist_brazil))


usethis::use_data(distance_brazil, overwrite = TRUE)
