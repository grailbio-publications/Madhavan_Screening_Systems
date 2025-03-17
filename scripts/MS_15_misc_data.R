#miscellaneous data statements within body of paper

#PPV per individual cancer in SCED
#"Finally, the PPVs of each individual SCED test are quite low, ranging from 0.10% to 1.20%"

sced_ten_ppv<-sced_ten_compute_tests %>%
  select(Cancer,Selected,TP,FP) %>%
  mutate(PPV=TP/(TP+FP)) %>%
  filter(Selected>0) %>%
  mutate(PPV_percent=PPV*100)

write_tsv(sced_ten_ppv,sprintf("reports/%s_text_sced_ten_ppv.tsv",date_code))

#compute system cost with altered up front screening cost
#for a hypothetical future reduction in screening costs
#that does not affect diagnostic costs

cost_multiplier<-0.75

system_discount_cost<-system_cost %>%
  mutate(DiscountScreeningCost=case_when(System=="mced_all" ~ SumScreeningCost*cost_multiplier,
                                         TRUE ~ SumScreeningCost)) 

write_tsv(system_discount_cost,sprintf("generated_data/%s_system_future_cost.tsv",date_code))


