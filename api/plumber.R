#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(dplyr)
library(tidyr)
library(tidycensus)
library(future)


#* @apiTitle TOP Hackathon API
#* @apiDescription Census data API for TOP Hackathon

Sys.setenv(CENSUS_API_KEY = "58191bd42d1e755d54d40334cd0c1fa8184e5aa5")

# Cache shapefiles
options(tigris_use_cache = TRUE)

# Get a list of available census data for 2019
v2019 <- tidycensus::load_variables(2019, "acs5", cache = TRUE)

# We will only look at immigrants origin and language
df_origin <- v2019 %>%
  dplyr::filter(startsWith(name, "B05006")) %>%
  tidyr::separate(label, into = c("type", "stat", "continent", "region", "country", "subcountry"), sep = "!!") %>%
  tidyr::unite(., col = "disp_country", country, subcountry, na.rm = TRUE, sep = "") %>%
  dplyr::filter(disp_country != "" & !stringr::str_detect(name, "PR")) %>%
  dplyr::arrange(disp_country) %>%
  dplyr::select(name, continent, region, disp_country)

df_industry <- v2019 %>%
  dplyr::filter(startsWith(name, "C24050")) %>%
  tidyr::separate(label, into = c("type", "stat", "industry", "subindustry"), sep = ":?!!") %>%
  dplyr::filter(is.na(subindustry) & !is.na(industry)) %>%
  dplyr::arrange(industry)


#* Return a list of countries for use in a dropdown
#* @get /countries
function() {
  df_origin$disp_country
}


#* Return a list of industries for use in a dropdown
#* @get /industries
function() {
  df_industry$industry
}

 
normalize <- function(x) (x - min(x)) / (max(x) - min(x))

#* Returns
#* @param origin Country of origin
#* @param origin_imp Importance of origin
#* @param industry Industry of work
#* @param industry_imp Importance of industry
#* @get /census_data
function(origin="Sweden", industry="Construction", origin_imp=TRUE, industry_imp=TRUE) {
  future::future({
    my_states <- c("MA", "NY", "PA", "NJ")
    my_vars <- c(
      origin = df_origin$name[which(df_origin$disp_country == origin)],
      industry = df_industry$name[which(df_industry$industry == industry)]
    )
    #browser(my_vars)
    multi_state_tract <- tidycensus::get_acs(
      geography = "county",
      variables = my_vars,
      state = my_states,
      year = 2019,
      survey = "acs5",
      geometry = TRUE,
      output = "wide" # get data in wide format for easier mapping
    )

    data <- multi_state_tract %>%
      mutate(
        norm_orig = coalesce(normalize(originM), 0) * as.numeric(origin_imp),
        norm_inds = coalesce(normalize(industryM), 0) * as.numeric(industry_imp),
        tot_score = (norm_orig + norm_inds),
        score = tot_score / max(tot_score) * 100
      )  
    data
  })
}
