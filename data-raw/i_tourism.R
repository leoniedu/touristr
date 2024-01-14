## code to prepare `i_tourism` dataset goes here
library(dplyr)
library("ckanr")

ckanr_setup("https://dados.turismo.gov.br/")

## find package
p <- package_search(q = 'title:chegada de turistas')
p_list <- package_show(p$results[[1]], as="table")
dname <- file.path("data-raw", janitor::make_clean_names(p_list$title))
dir.create(dname, showWarnings = FALSE)
p_resources <- p_list$resources

message("Increasing timeout limit ...")
options(timeout=600)

library(httr2)
##.x <- p_resources$url[10]
purrr::walk(p_resources$url, ~ {
  fname <- file.path(dname, basename(.x))
  if (!file.exists(fname)) {
    res <- request(.x)|>
      req_retry(is_transient = \(resp) resp_status(resp) %in% c(429, 500, 503), backoff = function(try_n) 10*try_n)|>
      req_perform()
    res_2csv <- res |>
      resp_body_string(encoding="latin1")|>
      readr::read_csv2()|>
      janitor::clean_names()
    readr::write_csv(res_2csv, file=fname)
  }
  })

fnames <- dir("data-raw/chegada_de_turistas_internacionais/", full.names = TRUE)
i_tourism_br_1 <- vroom::vroom(fnames[1:27])|>
  dplyr::rename_with(function(x) gsub("^ordem_", "cod_", x))%>%
  rename(via=via_de_acesso,
         cod_via=cod_via_de_acesso)
i_tourism_br_2 <- vroom::vroom(fnames[-(1:27)])

i_tourism_br_3 <- bind_rows(i_tourism_br_1, i_tourism_br_2)%>%
  transmute(continente, pais, country_code=cod_pais, state=uf, state_code=cod_uf, via, code_mode=cod_via, date=lubridate::make_date(ano, cod_mes), arrivals=chegadas)

library(countries)
country_codes_0 <- i_tourism_br_3%>%
  ungroup%>%
  distinct(country_code, pais)

#automatic matching with package "country"
country_codes_1 <- match_table(x=country_codes_0$pais, to= c("ISO3"))%>%
  janitor::clean_names()
## false matches
fmatches <- tribble(
  ~pais, ~iso3,
  "Países não especificados", NA_character_,
  "Outros países", NA_character_,
  "República da Coreia", "KOR",
  "República Tcheca", "CZE")
pais_iso3 <- country_codes_1%>%
  select(pais=list_countries, iso3)%>%
  rows_update(fmatches)

i_tourism_br <- i_tourism_br_3%>%
  left_join(pais_iso3)

usethis::use_data(i_tourism_br, overwrite = TRUE, compress = TRUE)
