#table one
#system parameter summaries

table_one_raw<-bind_rows(
  uspstf=uspstf_compute_perf_summary %>%
    mutate(Pop=Male_Pop+Female_Pop) %>%
    mutate(AvgSensitivity=Total_TP/Total_Eligible_In_Screening,
           AvgFPR=Total_FP/Total_Screen,
           AggSensitivity=Total_TP/Total_Eligible,
           AggFPR=Individual_FP/Pop) %>%
    select(AvgSensitivity,AvgFPR,AggSensitivity,AggFPR),
  
  
  sced_ten=sced_ten_compute_perf_summary %>%
    mutate(Pop=Male_Pop+Female_Pop,
           Total_Eligible_In_Screening=Total_Eligible) %>%
    mutate(AvgSensitivity=Total_TP/Total_Eligible_In_Screening,
           AvgFPR=Total_FP/Total_Screen,
           AggSensitivity=Total_TP/Total_Eligible,
           AggFPR=Individual_FP/Pop) %>%
    select(AvgSensitivity,AvgFPR,AggSensitivity,AggFPR),
  
  mced_ten=mced_ten_compute_perf_summary %>%
    mutate(Pop=Total_Screen,
           Total_Eligible_In_Screening=Total_Eligible) %>%
    mutate(AvgSensitivity=Total_TP/Total_Eligible_In_Screening,
           AvgFPR=Total_FP/Total_Screen,
           AggSensitivity=Total_TP/Total_Eligible,
           AggFPR=Individual_FP/Pop) %>%
    select(AvgSensitivity,AvgFPR,AggSensitivity,AggFPR),
  
  mced_all=mced_all_compute_perf_summary %>%
    mutate(Pop=Total_Screen,
           Total_Eligible_In_Screening=Total_Eligible) %>%
    mutate(AvgSensitivity=Total_TP/Total_Eligible_In_Screening,
           AvgFPR=Total_FP/Total_Screen,
           AggSensitivity=Total_TP/Total_Eligible,
           AggFPR=Individual_FP/Pop) %>%
    select(AvgSensitivity,AvgFPR,AggSensitivity,AggFPR),
  .id="System"
)

table_one_paper_flip<-table_one_raw %>%
  pivot_longer(-System) %>% pivot_wider(names_from=System)

write_tsv(table_one_raw,sprintf("reports/%s_table_one_raw.tsv",date_code))
write_tsv(table_one_paper_flip,sprintf("reports/%s_table_one_paper_flip.tsv",date_code))
