#table two

table_two_paper<-system_results %>%
  mutate(Aggregate_PPV=Total_TP/(Total_TP+Total_FP),
         Aggregate_NPV=Pool_NPV,
         Aggregate_NNS=Total_Screen/Total_TP,
         Aggregate_FPR=Total_FP/Total_Screen,
         Aggregate_FP_Per_Cancer=Total_FP/Total_TP) %>%
  select(System,Total_Eligible,Total_Screen,Total_TP,
         Aggregate_PPV,Aggregate_NPV,System_NPV,Aggregate_NNS,Total_FP,Individual_FP,Aggregate_FP_Per_Cancer)

#longer
table_two_paper_flip<-table_two_paper %>% pivot_longer(-System) %>% pivot_wider(names_from=System)

write_tsv(table_two_paper,sprintf("reports/%s_table_two_paper.tsv",date_code))
write_tsv(table_two_paper_flip,sprintf("reports/%s_table_two_paper_flip.tsv",date_code))

