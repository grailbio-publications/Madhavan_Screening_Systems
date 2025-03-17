#aggregate results over all systems

system_results<-bind_rows(
  uspstf=uspstf_compute_perf_summary %>% 
    select(Total_Eligible,Total_Screen,Total_TP,Total_FP,Total_TN,Total_FN,Individual_TN,Individual_FP,Pool_NPV,System_NPV),
  sced_ten=sced_ten_compute_perf_summary %>%
    select(Total_Eligible,Total_Screen,Total_TP,Total_FP,Total_TN,Total_FN,Individual_TN,Individual_FP,Pool_NPV,System_NPV),
  mced_ten=mced_ten_compute_perf_summary %>%
    select(Total_Eligible,Total_Screen,Total_TP,Total_FP,Total_TN,Total_FN,Individual_TN,Individual_FP,Pool_NPV,System_NPV),
  mced_all=mced_all_compute_perf_summary %>%
    select(Total_Eligible,Total_Screen,Total_TP,Total_FP,Total_TN,Total_FN,Individual_TN,Individual_FP,Pool_NPV,System_NPV),
  .id="System")

system_cost<-bind_rows(
  uspstf=uspstf_compute_cost_summary %>% 
    select(SumScreeningCost,SumFPDiagnosticCost,SumTotalDiagnosticCost),
  sced_ten=sced_ten_compute_cost_summary %>%
    select(SumScreeningCost,SumFPDiagnosticCost,SumTotalDiagnosticCost),
  mced_ten=mced_ten_compute_cost_summary %>%
    select(SumScreeningCost,SumFPDiagnosticCost,SumTotalDiagnosticCost),
  mced_all=mced_all_compute_cost_summary %>%
    select(SumScreeningCost,SumFPDiagnosticCost,SumTotalDiagnosticCost),
  .id="System")

write_tsv(system_results,sprintf("generated_data/%s_system_results_raw.tsv",date_code))
write_tsv(system_cost,sprintf("generated_data/%s_system_cost_raw.tsv",date_code))