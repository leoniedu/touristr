## code to prepare `rex` dataset goes here
library(WDI)
library(dplyr)
intdep0 <- WDI::WDI(country="all", indicator="ST.INT.DPRT")

intdep <-intdep0%>%
  janitor::clean_names()%>%
  transmute(iso3=iso3c, year, departures_total=st_int_dprt)
usethis::use_data(intdep, overwrite = TRUE)

intdep_usa <- intdep%>%
  filter(iso3=="USA")%>%
  na.omit()

arrivals_bra_by_total <- i_tourism_br%>%
  group_by(year=lubridate::year(date), iso3)%>%
  summarise(arrivals_bra=sum(arrivals, na.rm=TRUE))%>%
  left_join(intdep,by=c("iso3", 'year'))#%>%mutate(p=arrivals_bra/departures_total)

res <- arrivals_bra_by_total%>%
  semi_join(top_countries, by="iso3")%>%
  filter(year%in%c(2017,2018,2019))%>%
  group_by(iso3)%>%
  filter(!any(is.na(p)))%>%
  mutate(sem_visto=iso3%in%sem_visto$iso3)%>%
  group_by(sem_visto,year)%>%
  mutate(n=1)%>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm=TRUE)))%>%
  mutate(p=arrivals_bra/departures_total,
         index=p/p[year==2018])

  tidyr::pivot_wider(id_cols=c("iso3"), names_from = year, values_from = c(departures_total, p,arrivals_bra))%>%
  mutate(sem_visto=iso3%in%sem_visto$iso3,
         r2019=`p_2019`/`p_2018`
         #, d2019=`departures_total_2019`/`departures_total_2018`,
         #a2019=`arrivals_bra_2019`/`arrivals_bra_2018`
         #,r2020=`2020`/`2018`
         #r2021=`2021`/`2018`
         #, r2022=`2022`/`2018`
  )

res%>%
  group_by(sem_visto)%>%
  mutate(n=1)%>%
  summarise(across(where(is.numeric), sum))%>%
  mutate(p_departures_2018=arrivals_bra_2018/departures_total_2018,
         p_departures_2019=arrivals_bra_2019/departures_total_2019,
         r_departures_2019=p_departures_2019/p_departures_2018,
         pp_departures_2019=p_departures_2019-p_departures_2018
         )%>%View


res%>%
  group_by(iso3, sem_visto)%>%
  summarise(
    n_countries=n(),
    p_2018=mean(`p_2018`),
    p_2019=mean(`p_2019`),
    #p_2020=mean(`p_2020`),
    r2019=median(r2019, na.rm=TRUE)
    #, r2020=median(r2020, na.rm=TRUE)
  )%>%
  arrange(desc(sem_visto), desc(p_2018))%>%View

