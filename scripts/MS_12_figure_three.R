#figure 3
#absolute number of cancers detected
#all target cancers
#SCED-10

fig_three_table<-bind_rows(
  uspstf=uspstf_compute_tests %>%
    select(Cancer,TP) %>%
    summarize(Caught=sum(TP)) %>%
    mutate(Target="USPSTF"),
  
  mced_all=mced_all_compute_tests %>%
    select(Cancer,TP) %>%
    summarize(Caught=sum(TP)) %>%
    mutate(Target="MCED-All"),
  
  sced_ten=sced_ten_compute_tests %>%
    select(Cancer,TP) %>%
    summarize(Caught=sum(TP)) %>% 
    mutate(Target="CED") %>%
    select(Target,Caught) %>%
    bind_rows(sced_ten_compute_tests %>%
                select(Target=Cancer,Caught=TP) %>%
                filter(Caught>0)) ,
  
  mced_ten=mced_ten_compute_tests %>%
    select(Cancer,TP) %>%
    summarize(Caught=sum(TP)) %>% 
    mutate(Target="CED") %>%
    select(Target,Caught) %>%
    bind_rows(mced_ten_compute_tests %>%
                select(Target=Cancer,Caught=TP) %>%
                filter(Caught>0)),
  .id="System"
)

fig_three_levels<-fig_three_table %>%
  group_by(Target) %>%
  summarize(m=max(Caught)) %>%
  ungroup() %>% 
  mutate(system_order=case_when(Target=="USPSTF" ~ 1,
                                Target=="MCED-All" ~ 2,
                                TRUE ~ 3)) %>%
  arrange(desc(system_order),m) 

fig_three_alias<-tribble(~Target,~Alias,
                       "USPSTF","USPSTF A&B\nTarget Cancers",
                       "MCED-All","MCED-All\nTarget Cancers",
                       "CED","SCED-10 and MCED-10\nTarget Cancers",
                       "Lung","Lung",
                       "Breast","Breast",
                       "Lymphoma","Lymphoma",
                       "Colon/Rectum","Colon and Rectum",
                       "Uterus","Corpus and Uterus",
                       "Pancreas","Pancreas",
                       "Liver/Bile-duct","Liver and Intrahepatic\nBile Duct",
                       "Bladder","Bladder",
                       "Ovary","Ovary",
                       "Esophagus","Esophagus")

fig_three_plot<-fig_three_table %>%
  mutate(Target_Leveled=factor(Target,levels=fig_three_levels$Target)) %>%
  ggplot(aes(x=Target_Leveled,y=Caught,fill=System))+
  geom_col(aes(),position="dodge",color="black",width=0.6)+
  geom_text(aes(label=sprintf(" %s",round(Caught))),position = position_dodge(width = 0.6),
            hjust = 0,size=8)+
  coord_flip()+
  scale_x_discrete(breaks=fig_three_alias$Target,labels=fig_three_alias$Alias)+
  scale_fill_manual(name="",
                    #values=hue_pal()(4),
                    values=four_system_color,
                    breaks=c("uspstf","mced_all","sced_ten","mced_ten"),
                    labels=c("USPSTF","MCED-All","SCED-10","MCED-10"))+
  expand_limits(y=c(0,500))+
  theme_bw()+
  theme(
    legend.pos=c(0.8,0.1),
    title=element_text(size=18,face='bold'),
    text=element_text(size=16),
    panel.border=element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title=element_text(face='bold'),
    axis.text=element_text(size=14)
  )+
  labs(x="Cancer Subtype (Ordered by Prevalence)",y="Absolute Number of Cancers Detected")+
  ggtitle("Absolute Number of Cancers Detected by Cancer Subtype (SCED-10 vs MCED-10)")

ggsave(sprintf("figs/%s_figure_three_plot.pdf",date_code),
      fig_three_plot,
       width=18,height=14)

#ggsave(sprintf("figs/%s_figure_three_plot.eps",date_code),
#       fig_three_plot,
#       width=18,height=14)

#raw data for bars
write_tsv(fig_three_table,sprintf("reports/%s_figure_three_raw.tsv",date_code))
