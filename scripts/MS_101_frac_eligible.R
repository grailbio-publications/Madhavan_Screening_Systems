#supplemental: fraction cancers in eligible population, fraction population eligible
#from SEER cancers
#from census  https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/national/asrh/nc-est2021-agesex-res.csv.

age_path<-"data/20230327_SEER_Age_Incidence.xlsx"

age_data<-read_excel(age_path,
                     sheet="Incidence",
                     range="A3:AB104",
                     col_names=FALSE)
#need to add male-female
age_header<-read_excel(age_path,
                       sheet="Incidence",
                       range="A1:AB2",
                       col_names=FALSE)

age_header_data<-age_header %>%
  rename(DataType=...1) %>%
  mutate(DataType=c("Sex","Age")) %>%
  pivot_longer(-DataType) %>%
  pivot_wider(names_from=DataType)

#fix sex marks
age_data_fixed<-age_data %>%
  rename(Cancer=...1) %>%
  pivot_longer(-Cancer) %>%
  left_join(age_header_data) %>%
  select(-name) %>%
  mutate(Age=gsub(" years","-",Age)) %>%
  mutate(Age=gsub("00","0",Age,fixed=TRUE)) %>%
  mutate(Age=gsub("+","-120",Age,fixed=TRUE)) %>%
  mutate(Age=gsub("80-","80-80",Age,fixed=TRUE)) %>%
  mutate(Age=gsub("75-","75-75",Age,fixed=TRUE)) %>%
  separate(Age,into=c("lower","upper"),extra="drop") %>%
  type_convert()

age_pop<-read_csv("data/nc-est2021-agesex-res.csv")

test_age_spec<-tribble(~Cancer,~AgeRange, ~SexRange,
                       "Lung and Bronchus","50-79", "Male and female",
                       "Colon and Rectum","50-75", "Male and female",
                       "Breast", "50-74", "Female",
                       "Cervix Uteri", "50-65","Female")

test_age_spec_fixed<-test_age_spec %>%
  separate(AgeRange,into=c("lower","upper"),sep="-") %>%
  type_convert()

age_cancer_in_eligible<-age_data_fixed %>%
  semi_join(test_age_spec_fixed %>% select(Cancer,Sex=SexRange)) %>%
  left_join(test_age_spec_fixed %>% rename(lower_p=lower,upper_p=upper)) %>%
  mutate(valid_uspstf=1*(lower>=lower_p)*(upper_p>=upper),
         valid_screen=1*(lower>=50)*(upper<=79)) %>%
  group_by(Cancer) %>%
  summarize(uspstf_cancer=sum(valid_uspstf*value),
            screen_cancer=sum(valid_screen*value)) %>%
  ungroup() %>%
  mutate(PropCancerInEligible=uspstf_cancer/screen_cancer)

age_pop_in_eligible<-age_pop %>%
  mutate(Sex=case_when(SEX==0 ~ "Male and female",
                       SEX==1 ~ "Male",
                       SEX==2 ~ "Female",
                       TRUE ~ "Bad data")) %>% 
  filter(AGE<120) %>% #get rid of data summations
  mutate(value=POPESTIMATE2021) %>%
  left_join(test_age_spec_fixed %>% rename(Sex=SexRange),multiple='all') %>%
  filter(!is.na(lower)) %>%
  mutate(valid_uspstf=1*(AGE>=lower)*(upper>=AGE),
         valid_screen=1*(AGE>=50)*(AGE<=79)) %>% 
  group_by(Cancer) %>%
  summarize(uspstf_pop=sum(valid_uspstf*value),
            screen_pop=sum(valid_screen*value)) %>%
  ungroup() %>%
  mutate(PropEligible=uspstf_pop/screen_pop)

age_based_eligibility<-age_pop_in_eligible %>%
  left_join(age_cancer_in_eligible) 

#lung is special
#For lung we can assume
#68.6% of cases eligible based on smoking criteria, per Pasquinelli et al. 2021 (https://doi.org/10.1016/j.jtocrr.2020.100137); 
# and 14.5% of the overall population aged 50-80 y eligible based on smoking criteria, 
#per Landy et al. 2021 (https://doi.org/10.1093/jnci/djaa211, calculated as 14508450/100000000).

final_eligibility<-age_based_eligibility %>%
  mutate(final_PropEligible=case_when(Cancer=="Lung and Bronchus" ~ PropEligible*0.1450845,
                                      TRUE ~ PropEligible),
         final_PropCancerInEligible=case_when(Cancer=="Lung and Bronchus" ~ PropCancerInEligible*0.686,
                                              TRUE ~ PropCancerInEligible)) %>%
  left_join(test_age_spec_fixed)
  

#for system defn purposes, rounded to 3 digits but have full precision here.
write_tsv(final_eligibility,sprintf("generated_data/%s_final_eligible_pops_raw.tsv",date_code))


  