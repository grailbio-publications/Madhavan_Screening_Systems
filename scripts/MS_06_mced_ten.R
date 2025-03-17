#MCED-10
#slightly different because of universal FP
#amortized across individual TP

mced_ten_compute_tests_per_stage<-seer_post_uspstf_incidence %>%
  left_join(mced_ten_defn) %>%
  left_join(mced_ten_sens) %>%
  cross_join(post_uspstf_pop_df) %>%
  mutate(across(where(is.numeric), function(x){replace_na(x,replace=0.0)})) %>% 
  mutate(IR_p=IRpostU*1e-5,
         SexEligible=1) %>% #everyone is screened
  mutate(TotalCancers=Pop*IR_p) %>%
  mutate(EligibleCancers=Pop*IR_p*PropCancersInEligible) %>%
  mutate(EligiblePop=Pop*PropEligibleInPop*SexEligible) %>%
  mutate(NumScreenTests=Selected*EligiblePop*Adherence*TestIntervalPerYear) %>%
  mutate(EligibleCancersInScreening=EligibleCancers*Adherence*TestIntervalPerYear*IntervalCapture) %>%
  mutate(stage_TP=Selected*EligibleCancersInScreening*Sensitivity) %>%
  mutate(stage_FN=Selected*(EligibleCancers-stage_TP))

mced_ten_compute_tests<-mced_ten_compute_tests_per_stage %>%
  group_by(Cancer) %>%
  summarize(Pop=Pop[1],
            Selected=Selected[1],
            MaleEligible=MaleEligible[1],
            FemaleEligible=FemaleEligible[1],
            TotalCancers=sum(TotalCancers),
            EligibleCancers=sum(EligibleCancers),
            NumScreenTests=NumScreenTests[1],
            EligibleCancersInScreening=sum(EligibleCancersInScreening),
            TP=sum(stage_TP),
            FN=sum(stage_FN)) %>%
  ungroup() %>%
  cross_join(mced_ten_spec) %>%
  mutate(global_FP=Selected*(1-Specificity)*(NumScreenTests-sum(EligibleCancersInScreening*Selected))) %>%
  mutate(amortized_FP=global_FP*TotalCancers/sum(TotalCancers*Selected)) %>%
  mutate(amortized_TN=Selected*(Pop-TP-FN-amortized_FP)) %>%
  mutate(amortized_NumScreenTests=NumScreenTests*TotalCancers/sum(TotalCancers*Selected),
         System_FN=TotalCancers-TP, #everything!
         System_TN=Pop-TP-amortized_FP-System_FN,
         System_NPV_line=1-System_FN/(System_FN+System_TN),
         System_NPV=prod(System_NPV_line))

mced_ten_compute_perf_summary<-mced_ten_compute_tests %>%
  filter(Selected>0) %>%
  select(Cancer,Pop,EligibleCancers,amortized_NumScreenTests,TP,amortized_FP,amortized_TN,FN,System_NPV) %>%
  mutate(NPV_line=1-FN/(FN+amortized_TN)) %>%
  summarize(Total_Screen=sum(amortized_NumScreenTests),
            Total_Eligible=sum(EligibleCancers),
            Total_TP=sum(TP),
            Total_FP=sum(amortized_FP),
            Total_TN=sum(amortized_TN),
            Total_FN=sum(FN),
            Pop=Pop[1],
            Pool_NPV=prod(NPV_line),
            System_NPV=System_NPV[1]) %>%
  mutate(Individual_FP=Total_FP,
         Individual_TN=Pop-Total_TP-Total_FP-Total_FN)

#cost
mced_ten_compute_costs<-mced_ten_compute_tests %>%
  select(Cancer,amortized_NumScreenTests,TP,amortized_FP) %>%
  cross_join(mced_ten_cost %>% select(ScreeningCost)) %>%
  left_join(diagnostic_cost_df) %>%
  mutate(across(where(is.numeric), function(x){replace_na(x,replace=0.0)})) %>% 
  mutate(TotalScreeningCost=ScreeningCost*amortized_NumScreenTests) %>%
  mutate(Total_FP_Diagnostic_Cost=amortized_FP*DiagnosticCost) %>%
  mutate(Total_Diagnostic_Cost=(TP+amortized_FP)*DiagnosticCost)

mced_ten_compute_cost_summary<-mced_ten_compute_costs %>% 
  summarize(SumScreeningCost=sum(TotalScreeningCost),
            SumFPDiagnosticCost=sum(Total_FP_Diagnostic_Cost),
            SumTotalDiagnosticCost=sum(Total_Diagnostic_Cost)) 

#write out rich intermediate data for archive/interest
write_tsv(mced_ten_compute_tests,sprintf("generated_data/%s_mced_ten_compute_tests.tsv",date_code))
write_tsv(mced_ten_compute_costs,sprintf("generated_data/%s_mced_ten_compute_costs.tsv",date_code))

