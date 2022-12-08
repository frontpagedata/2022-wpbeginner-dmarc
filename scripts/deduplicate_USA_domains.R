library(dplyr)
library(readr)
library(here)
library(janitor)
library(stringr)
library(urltools)
suffix_refresh()

domain_gov <- read_csv(here("proc_data", "processed by vlad", "domain.gov_names.csv"),
                       col_types = cols(use_case = "c",comments="c")) %>% 
  remove_empty("cols",quiet = FALSE)


domain_gov2 <- 
  domain_gov %>% mutate(url=str_remove(url,"^www\\."),
                        tld=suffix_extract(url)$domain,
                        suffix=suffix_extract(url)$suffix,
                        organizational_domain=paste0(tld,".",suffix),
                        subdomain=suffix_extract(url)$subdomain,
                        is_subdomain=if_else(!is.na(subdomain),TRUE,FALSE),
                        .after=url)

# missing values are all NA from merging two data sources
domain_gov2 <- domain_gov2 %>% 
  mutate(country=if_else(is.na(country),"United States of America (USA)",country)) 
# separate world vs USA
domain_gov_noUSA <- domain_gov2 %>% filter(country!="United States of America (USA)")
domain_govUSA <- domain_gov2 %>% filter(country=="United States of America (USA)")

# remove duplicates in everything except id
domain_govUSA <- domain_govUSA %>% distinct(across(-id),.keep_all = TRUE)
# find duplicates
domain_govUSA_dupes <- domain_govUSA %>%   get_dupes(url) %>% remove_empty("cols") %>% 
  select(-country,-starts_with("alpha"))
# count NAs
domain_govUSA_dupes$nacols <- rowSums(is.na(domain_govUSA_dupes))
# identify which of the duplicates has less information
domain_govUSA_drop <- domain_govUSA_dupes %>% group_by(url) %>% slice_max(nacols) %>% ungroup()

# drop duplicated
domain_govUSA_f <- domain_govUSA %>% filter(!id %in% domain_govUSA_drop$id)

# put USA back in
domain_gov_all <- bind_rows(domain_gov_noUSA,domain_govUSA_f)

# export
write_csv(domain_gov_all,here("proc_data","processed by Luis","domain_gov_names_deduplicated.csv"))
