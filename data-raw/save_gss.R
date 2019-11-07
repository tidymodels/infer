library(dplyr)
library(srvyr)
library(ggplot2)

# pull gss data

temp <- tempfile()
download.file("https://gss.norc.org/documents/stata/GSS_stata.zip",temp)
gss_orig <- haven::read_dta(unz(temp, filename = "GSS7218_R1.DTA")) %>%
  haven::as_factor()
unlink(temp)

# select relevant columns

gss_small <- gss_orig %>%
  filter(!stringr::str_detect(sample, "blk oversamp")) %>%
  select(year, age, sex, race, partyid, hompop, hours = hrs1, 
         income, class, finrela, weight = wtssall) %>%
  mutate_if(is.factor, ~forcats::fct_collapse(., NULL = c("IAP", "NA"))) %>%
  mutate(age = age %>%
           forcats::fct_recode("89" = "89 or older",
                               NULL = "DK") %>% # truncated at 89
           as.character() %>%
           as.numeric(),
         hompop = hompop %>%
           forcats::fct_collapse(NULL = c("DK", "NA")) %>%
           as.character() %>%
           as.numeric(),
         hours = hours %>%
           forcats::fct_recode("89" = "89+ hrs",
                               NULL = "DK") %>% # truncated at 89
           as.character() %>%
           as.numeric(),
         weight = weight %>%
           as.character() %>%
           as.numeric(),
         partyid = forcats::fct_collapse(partyid, 
           dem = c("strong democrat", "not str democrat"),
           rep = c("strong republican", "not str republican"),
           ind = c("ind,near dem", "independent", "ind,near rep"),
           other = "other party"
         ),
         income = factor(income, ordered = TRUE)
         )

# sample 3k of the full data set

set.seed(20191105)
gss <- gss_small %>%
  sample_n(3000)

# check that the sample is similar unweighted to weighted

gss_wt <- srvyr::as_survey_design(gss, weights = weight)

unweighted <- gss %>%
  group_by(year, sex, partyid) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(year, sex) %>%
  mutate(prop = n / sum(n))

weighted <- gss_wt %>%
  group_by(year, sex, partyid) %>%
  summarize(prop = srvyr::survey_mean())

# ehhhh close enough until you really drill down, we'll put a disclaimer

# save data into package

usethis::use_data(gss, overwrite = TRUE)

devtools::document()
