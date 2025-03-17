#now do sced-10

sced_ten_compute_tests_per_stage<-seer_post_uspstf_incidence %>%
  left_join(sced_ten_defn) %>%
  left_join(sced_ten_sens) %>%
  left_join(sced_ten_spec %>% rename(Cancer=TestFor)) %>%
  cross_join(post_uspstf_pop_df) %>%
  mutate(across(where(is.numeric), function(x){replace_na(x,replace=0.0)})) %>% 
  mutate(IR_p=IRpostU*1e-5,
         SexEligible=(MaleEligible+FemaleEligible)/2) %>%
  mutate(TotalCancers=Pop*IR_p) %>%
  mutate(EligibleCancers=Pop*IR_p*PropCancersInEligible) %>%
  mutate(EligiblePop=Pop*PropEligibleInPop*SexEligible) %>%
  mutate(NumScreenTests=Selected*EligiblePop*Adherence*TestIntervalPerYear) %>%
  mutate(EligibleCancersInScreening=EligibleCancers*Adherence*TestIntervalPerYear*IntervalCapture) %>%
  mutate(stage_TP=Selected*EligibleCancersInScreening*Sensitivity) %>%
  mutate(stage_FN=Selected*(EligibleCancers-stage_TP)) 

#collapse down to per-cancer
sced_ten_compute_tests<-sced_ten_compute_tests_per_stage %>%
  group_by(Cancer) %>%
  summarize(Pop=Pop[1],
            Selected=Selected[1],
            MaleEligible=MaleEligible[1],
            FemaleEligible=FemaleEligible[1],
            Specificity=Specificity[1],
            TotalCancers=sum(TotalCancers),
            EligibleCancers=sum(EligibleCancers),
            NumScreenTests=NumScreenTests[1],
            EligibleCancersInScreening=sum(EligibleCancersInScreening),
            TP=sum(stage_TP),
            FN=sum(stage_FN)) %>%
  ungroup() %>%
  mutate(FP=Selected*(NumScreenTests-EligibleCancersInScreening)*(1-Specificity)) %>% #tested,without cancer, receive positive
  mutate(TN=Selected*(Pop-TP-FP-FN),
         System_FN=TotalCancers-TP, #everything!
         System_TN=Pop-TP-FP-System_FN,
         System_NPV_line=1-System_FN/(System_FN+System_TN),
         System_NPV=prod(System_NPV_line)) 

#performance summary
sced_ten_compute_perf_summary<-sced_ten_compute_tests %>% 
  select(Cancer,Selected, MaleEligible, FemaleEligible, EligibleCancers,EligibleCancersInScreening,NumScreenTests,Pop,TP,FP,TN,FN,System_NPV) %>%
  filter(Selected>0) %>%
  mutate(Male_FP=FP*MaleEligible/(MaleEligible+FemaleEligible),
         Female_FP=FP*FemaleEligible/(MaleEligible+FemaleEligible),
         Male_Pop=Pop*0.5,
         Female_Pop=Pop*0.5) %>%
  mutate(Male_no_FP=(1-Male_FP/Male_Pop),
         Female_no_FP=(1-Female_FP/Female_Pop),
         NPV_line=1-FN/(FN+TN)) %>%
  summarize(Male_Pop=Male_Pop[1],
            Female_Pop=Female_Pop[1],
            Male_ratio_no_FP=prod(Male_no_FP),
            Female_ratio_no_FP=prod(Female_no_FP),
            Total_Eligible=sum(EligibleCancers),
            Total_Eligible_In_Screening=sum(EligibleCancersInScreening),
            Total_Screen=sum(NumScreenTests),
            Total_TP=sum(TP),
            Total_FP=sum(FP),
            Total_TN=sum(TN),
            Total_FN=sum(FN),
            Pool_NPV=prod(NPV_line),
            System_NPV=System_NPV[1]) %>%
  mutate(Individual_FP=Male_Pop*(1-Male_ratio_no_FP)+Female_Pop*(1-Female_ratio_no_FP),
         Individual_TN=(Male_Pop+Female_Pop-Total_TP-Individual_FP-Total_FN))

#cost summary
sced_ten_compute_costs<-sced_ten_compute_tests %>%
  select(Cancer,NumScreenTests,TP,FP) %>%
  left_join(sced_ten_cost %>% rename(Cancer=TestFor)) %>%
  left_join(diagnostic_cost_df) %>%
  mutate(across(where(is.numeric), function(x){replace_na(x,replace=0.0)})) %>% 
  mutate(TotalScreeningCost=ScreeningCost*NumScreenTests) %>%
  mutate(Total_FP_Diagnostic_Cost=FP*DiagnosticCost) %>%
  mutate(Total_Diagnostic_Cost=(TP+FP)*DiagnosticCost)

sced_ten_compute_cost_summary<-sced_ten_compute_costs %>% 
  summarize(SumScreeningCost=sum(TotalScreeningCost),
            SumFPDiagnosticCost=sum(Total_FP_Diagnostic_Cost),
            SumTotalDiagnosticCost=sum(Total_Diagnostic_Cost)) 

#write out rich intermediate data for archive/interest
write_tsv(sced_ten_compute_tests,sprintf("generated_data/%s_sced_ten_compute_tests.tsv",date_code))
write_tsv(sced_ten_compute_costs,sprintf("generated_data/%s_sced_ten_compute_costs.tsv",date_code))
