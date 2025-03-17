#proportion accounted by 10 cancers - supplemental age/cancer grouping
#"The 10 cancer types used in our SCED-10 and MCED-10 tests, 
#in the screening-eligible age group of 50–79 years, 
#represent 37% of all cases (54% of those at ages 50–79)"

#use the age_data_fixed
#combined with system definition

sced_ten_just_selected<-sced_ten_defn %>% filter(Selected>0) %>% select(Cancer)
#translate?

SEER_to_sens<-tribble(~CancerSEER,~Cancer,~Sex,
                      "Breast","Breast","Female",
                      "Esophagus","Esophagus", "Male and female",
                      "Lymphoma","Lymphoma","Male and female",
                      "Ovary","Ovary", "Female",
                      "Pancreas","Pancreas","Male and female",
                      "Lung and Bronchus","Lung","Male and female",
                      "Corpus and Uterus, NOS","Uterus", "Female",
                      "Urinary Bladder","Bladder","Male and female",
                      "Colon and Rectum", "Colon/Rectum","Male and female",
                      "Liver and Intrahepatic Bile Duct","Liver/Bile-duct","Male and female") 

#within 10 cancers
ten_cancer_cases<-age_data_fixed %>%
  rename(CancerSEER=Cancer) %>%
  semi_join(SEER_to_sens) %>% 
  left_join(SEER_to_sens) %>% 
  semi_join(sced_ten_just_selected) %>%
  mutate(valid_range=1*(lower>=50)*(upper<=79)) %>%
  summarize(total_in_range=sum(value*valid_range),
            total=sum(value))

#within all sites
all_cancer_cases<-age_data_fixed %>%
  filter(Cancer=="All Sites",Sex=="Male and female") %>%
  mutate(valid_range=1*(lower>=50)*(upper<=79)) %>%
  summarize(total_in_range=sum(value*valid_range),
            total=sum(value)) 


misc_ten_fraction_cases<-ten_cancer_cases %>% 
  cross_join(all_cancer_cases,suffix=c("_ten","_all")) %>%
  mutate(prop_ten_in_range=total_in_range_ten/total_in_range_all,
         prop_ten_in_total=total_in_range_ten/total_all)


misc_ten_fraction_cases<-ten_cancer_cases %>% 
  cross_join(all_cancer_cases,suffix=c("_ten","_all")) %>%
  mutate(prop_ten_in_range=total_in_range_ten/total_in_range_all,
         prop_ten_in_total=total_in_range_ten/total_all)

write_tsv(misc_ten_fraction_cases,sprintf("generated_data/%s_fraction_cases_in_age_range.tsv",date_code))
