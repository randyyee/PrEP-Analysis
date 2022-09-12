library(tidyverse)

df <- read_delim("C:/Users/pcx5/Downloads/Genie_PSNU_IM_MultipleOUs_Frozen_f79aede5-1db6-4019-b45c-27f823385571.zip",
                 "\t",
                 escape_double = FALSE,
                 trim_ws = TRUE,
                 col_types = cols(.default   = col_character(),
                                  targets    = col_double(),
                                  qtr1       = col_double(),
                                  qtr2       = col_double(),
                                  qtr3       = col_double(),
                                  qtr4       = col_double(),
                                  cumulative = col_double())
) %>%
  pivot_longer(targets:cumulative,
               names_to = "attribute",
               values_to = "value") %>%
  unite("period",
        c("fiscal_year", "attribute"),
        sep = "_",
        remove = F) %>%
  pivot_wider(names_from = indicator, values_from = "value")

df2 <- df %>%
  select(-c(operatingunit,
            operatingunituid,
            snu1uid,
            psnuuid,
            snuprioritization,
            prime_partner_name,
            mech_code,
            mech_name,
            prime_partner_duns,
            prime_partner_uei,
            award_number,
            dataelementuid,
            categoryoptioncombouid,
            disaggregate,
            modality,
            statustb,
            statuscx,
            approvalleveldescription,
            age_2018,
            hiv_treatment_status,
            source_name,
            approvallevel)) %>%
  group_by_if(is.character) %>%
  summarise(HTS_TST_POS = sum(HTS_TST_POS, na.rm = T),
            PrEP_CURR      = sum(PrEP_CURR, na.rm = T),
            PrEP_NEW       = sum(PrEP_NEW, na.rm = T),
            PrEP_CT       = sum(PrEP_CT, na.rm = T)) %>%
  ungroup() %>%
  mutate(empty = HTS_TST_POS + PrEP_CURR + PrEP_NEW + PrEP_CT) %>%
  filter(empty != 0) %>%
  select(-empty) %>%
  mutate(HTS_TST_POS   = na_if(HTS_TST_POS, 0),
         PrEP_CURR     = na_if(PrEP_CURR, 0),
         PrEP_NEW      = na_if(PrEP_NEW, 0),
         PrEP_CT       = na_if(PrEP_CT, 0))

write.csv(df2, "PREPtoNEED_Q3Initial.csv", na = "", row.names = F)
