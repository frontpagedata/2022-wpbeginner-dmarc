library(dplyr)
library(readr)
library(here)
library(janitor)
library(stringr)
library(urltools)
library(tidyr)
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
domain_govUSA_drop <- domain_govUSA_dupes %>% group_by(url) %>% 
  slice_max(nacols,with_ties = TRUE) %>% ungroup()

# tag remaining duplicates
domain_govUSA_drop <- domain_govUSA_drop %>% 
  group_by(url) %>% mutate(distincturl=n())

# ready to drop by ID
domain_govUSA_drop_rdy <- domain_govUSA_drop %>% filter(distincturl==1) %>% ungroup()

# for coalescing and deduplicating
domain_govUSA_drop_dup <- domain_govUSA_drop %>% filter(distincturl==2)
# fill within groups, keep either record
domain_govUSA_drop_dup <- domain_govUSA_drop_dup %>% fill(everything(),.direction = "downup") %>% sample_n(1) %>% ungroup()
# combine
domain_govUSA_drop_all <- bind_rows(domain_govUSA_drop_dup,domain_govUSA_drop_rdy)

# drop duplicated
domain_govUSA_f <- domain_govUSA %>% filter(!id %in% domain_govUSA_drop_all$id)

# put USA back in
domain_gov_all <- bind_rows(domain_gov_noUSA,domain_govUSA_f)

# repeat deduplication, keeping records with more information
# count NAs in duplicates
domain_gov_all_dupes <- domain_gov_all %>% get_dupes(url)
domain_gov_all_dupes$nacols <- rowSums(is.na(domain_gov_all_dupes))
# identify which of the duplicates has less information
domain_gov_all_dupes <- domain_gov_all_dupes %>% group_by(url) %>% 
  slice_max(nacols,with_ties = TRUE) %>% ungroup()

# tag remaining duplicates
domain_gov_all_drop <- domain_gov_all_dupes %>% 
  group_by(url) %>% mutate(distincturl=n())

# ready to drop by ID
domain_gov_all_drop_rdy <- domain_gov_all_drop %>% filter(distincturl==1) %>% ungroup()

# for coalescing and deduplicating
domain_gov_all_drop_dup <- domain_gov_all_drop %>% filter(distincturl==2)
# fill within groups, keep either record
domain_gov_all_drop_dup <- domain_gov_all_drop_dup %>% fill(everything(),.direction = "downup") %>% sample_n(1) %>% ungroup()

# combine
domain_gov_drop_all <- bind_rows(domain_gov_all_drop_dup,domain_gov_all_drop_rdy)

# drop duplicated
domain_gov_all <- domain_gov_all %>% filter(!id %in% domain_gov_drop_all$id)

domain_gov_all %>% get_dupes(url)
# export
write_csv(domain_gov_all,here("proc_data","processed by Luis","domain_gov_names_deduplicated.csv"))
