#figure two (figure one is just a picture)


#root box
#population, number of cancers
figure_two_root_box<-uspstf_compute_tests %>%
  summarize(Pop=Pop[1],
            CancerIncidence=sum(TotalCancers))

#row two boxes
#eligible for uspstf screening
#not eligible fo uspstf screening
#not covered by uspstf screening
figure_two_row_two_boxes<-uspstf_compute_tests %>%
  summarize(TotalC=sum(TotalCancers),
            TotalUSPSTF=sum(TotalCancers*Selected),
            TotalEligible=sum(EligibleCancers)) %>%
  mutate(uspstf_cancers_covered_eligible=TotalEligible,
         uspstf_cancers_covered_not_eligible=TotalUSPSTF-TotalEligible,
         not_uspstf_cancers=TotalC-TotalUSPSTF) %>%
  select(uspstf_cancers_covered_eligible,
         uspstf_cancers_covered_not_eligible,
         not_uspstf_cancers)


#uspstf boxes
#eligible and adherent
#eligible and not adherent
#screen false negative and adherent

figure_two_uspstf_three_boxes<-uspstf_compute_tests %>% 
  select(Cancer,Selected,EligibleCancers,TP) %>% 
  left_join(uspstf_defn %>% select(Cancer,Adherence)) %>%
  filter(Selected>0) %>%
  summarize(TotalEligibleAdherent=sum(EligibleCancers*Adherence),
            TotalEligibleNotAdherent=sum(EligibleCancers*(1-Adherence)),
            ScreenMiss=TotalEligibleAdherent-sum(TP)) 



#parallel systems
figure_two_system_box<-system_results %>% 
  select(System,Total_Eligible,Total_TP,Total_FN) %>%
  left_join(system_cost) %>%
  mutate(TotalCost=SumScreeningCost+SumTotalDiagnosticCost) %>%
  mutate(TotalCostPretty=sprintf("$%sM",round(TotalCost*1e-6,0))) %>% #put in millions
  select(System,CancerEligible=Total_Eligible,CancerDetected=Total_TP,System_FN=Total_FN,TotalCostPretty)

supplemental_screen_total=figure_two_row_two_boxes %>%
  select(uspstf_cancers_covered_not_eligible,not_uspstf_cancers) %>%
  cross_join(figure_two_uspstf_three_boxes %>% select(TotalEligibleNotAdherent,ScreenMiss)) %>%
  mutate(total=uspstf_cancers_covered_not_eligible+not_uspstf_cancers+TotalEligibleNotAdherent+ScreenMiss) %>%
  pull(total)


#overkill: diagram to lay out numbers
#using the diagram package

system_str<-figure_two_system_box %>% 
  mutate(my_str=sprintf("%s\nEligible=%s\nDetected=%s\nMissed=%s\nCost=%s",System,
                        round(CancerEligible),
                        round(CancerDetected),
                        round(System_FN),
                        TotalCostPretty)) %>%
  select(my_str)

figure_two_data_df<-tribble(~box,~content,~coordx,~coordy,~parent,
                            1,sprintf("Cancers\nN=%s",round(figure_two_root_box$CancerIncidence[1])),0.5,0.9,NA,
                            2,sprintf("USPSTF\nEligible\nN=%s",round(figure_two_row_two_boxes$uspstf_cancers_covered_eligible[1])),0.25,0.7,1,
                            3,sprintf("USPSTF\nNot Eligible\nN=%s",round(figure_two_row_two_boxes$uspstf_cancers_covered_not_eligible[1])),0.5,0.7,1,
                            4,sprintf("Not USPSTF\nN=%s",round(figure_two_row_two_boxes$not_uspstf_cancers[1])),0.75,0.7,1,
                            5,sprintf("Adherent\nN=%s",round(figure_two_uspstf_three_boxes$TotalEligibleAdherent[1])),0.1,0.55,2,
                            6,sprintf("Not Adherent\nN=%s",round(figure_two_uspstf_three_boxes$TotalEligibleNotAdherent[1])),0.4,0.55,2,
                            7,sprintf("FN\nN=%s",round(figure_two_uspstf_three_boxes$ScreenMiss[1])),0.3,0.4,5,
                            8,sprintf("Supplemental\nN=%s",round(supplemental_screen_total)),0.6,0.35,4,
                            9,system_str$my_str[1],0.2,0.15,2,
                            10,system_str$my_str[2],0.4,0.15,8,
                            11,system_str$my_str[3],0.6,0.15,8,
                            12,system_str$my_str[4],0.8,0.15,8)


#plot the data
plot_figure_two_sketch<-function(figure_two_data_df){
nbox=length(figure_two_data_df$box)                           
plot_A=matrix(0,nbox,nbox)
for (ii in 1:nbox){
  if (!is.na(figure_two_data_df$parent[ii])){
  plot_A[figure_two_data_df$box[ii],figure_two_data_df$parent[ii]]<-""
  }
}
plot_A[8,3]<-""
plot_A[8,7]<-""
plot_A[8,6]<-""

plot_pos<-matrix(0,2,nbox)
plot_pos[1,]<-figure_two_data_df$coordx
plot_pos[2,]<-figure_two_data_df$coordy

plotmat(plot_A,
        pos=t(plot_pos),
        name=figure_two_data_df$content,
        box.size=0.05,
        box.type="rect",
        shadow.size=0.0,
        main="Disposition of Cancers in 100,000 50-79 individuals",
        cex.main=2)
}

pdf(sprintf("figs/%s_figure_two_sketch.pdf",date_code),
    width=15,height=12)

plot_figure_two_sketch(figure_two_data_df)
dev.off()

#setEPS()
#postscript(sprintf("figs/%s_figure_two_sketch.eps",date_code),
#           width=15,height=12)
#plot_figure_two_sketch(figure_two_data_df)
#dev.off()

write_tsv(figure_two_data_df,sprintf("reports/%s_raw_figure_two_data.tsv",date_code))

