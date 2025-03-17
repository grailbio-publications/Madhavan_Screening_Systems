#seer incidence
#read seer incidence, impute where needed

seer_path<-"data/20230323 SEER17 50-79 crude incidence rates by AJCC 6th stage 2006-2015 for Sarina.xlsx"

seer_raw<-read_excel(seer_path,
           sheet="Rates",
           range="A1:E136")

#fix names, impute unknown/missing where appropriate
seer_imputed <-seer_raw %>%
  rename(Cancer=...1,Stage=...2) %>%
  mutate(Stage=case_when(Stage=="Unknown/missing" ~ "NotStaged",
                         TRUE ~ Stage)) %>%
  mutate(stage_flag=case_when(Cancer=="Lymphoid Leukemia" ~ 1.0*(Stage=="NotStaged"),
                              Cancer=="Myeloid Neoplasm" ~ 1.0*(Stage=="NotStaged"),
                              Cancer=="Plasma Cell Neoplasm" ~ 1.0*(Stage=="NotStaged"),
                              Cancer=="CUP" ~ 1.0*(Stage=="NotStaged"),
                              Cancer=="Brain" ~ 1.0*(Stage=="NotStaged"),
                              Cancer=="[OTHER]" ~ 1.0,
                              TRUE ~ 1.0*(Stage!="NotStaged"))) %>%
  group_by(Cancer) %>%
  mutate(imputed=sum(Rate)*(Rate*stage_flag/sum(Rate*stage_flag))) %>%
  ungroup()

seer_imputed_final<-seer_imputed %>%
  filter(stage_flag>0) %>%
  select(Cancer,Stage,IR=imputed)