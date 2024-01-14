## code to prepare `rex` dataset goes here
library(WDI)
library(dplyr)
rex0 <- WDI::WDI(country="all", indicator="PX.REX.REER")
rex <- rex0%>%
  janitor::clean_names()%>%
  transmute(iso3=iso3c, year, rex=px_rex_reer)
usethis::use_data(rex, overwrite = TRUE)


