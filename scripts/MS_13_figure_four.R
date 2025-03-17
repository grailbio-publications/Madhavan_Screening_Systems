
#this might be better as a table

figure_four_table<-system_cost %>% 
  left_join(diagnostic_cost_multiplier_df %>% 
              mutate(System=gsub("10","ten",System)) %>%  #translate from external to internal names
              rename(workup_multiplier=RelativeDiagnosticCost)) %>%
  mutate(NonFPDiagnosticCost=SumTotalDiagnosticCost-SumFPDiagnosticCost) %>% 
  mutate(across(contains("Diagnostic"),function(x){x*workup_multiplier})) %>%
  mutate(TotalCost=SumScreeningCost+SumFPDiagnosticCost+NonFPDiagnosticCost) %>%
  select(System,SumScreeningCost,SumFPDiagnosticCost,NonFPDiagnosticCost,TotalCost)

fp_break<-c(5e7,1e8,15e7,2e8,25e7)
fp_label<-sprintf("$%s",prettyNum(fp_break,scientific=FALSE,big.mark=','))

eq_text_size<-6

figure_four_fp<-figure_four_table %>%
  mutate(prettyCost=sprintf("$%s",prettyNum(SumFPDiagnosticCost,big.mark=','))) %>%
  mutate(offset=5e7,offset_plus=3e7) %>%
  ggplot(aes(x=System))+
  geom_col(aes(y=SumFPDiagnosticCost,fill=System),color="black")+
  geom_text(aes(y=offset,label=prettyCost),hjust=1,size=eq_text_size)+
  geom_text(aes(y=offset_plus,label="+"),hjust=1,size=eq_text_size)+
  annotate("text", x=4.2,y=5e7,label="FP Workup Costs",hjust=1,size=eq_text_size)+
  expand_limits(y=c(0,2.7e8))+
  coord_flip()+
  scale_y_reverse(breaks=fp_break,labels=fp_label,expand=c(0,0))+
  scale_fill_manual(name="",
                    #values=hue_pal()(4),
                    values=four_system_color,
                    breaks=c("uspstf","mced_all","sced_ten","mced_ten"),
                    labels=c("USPSTF","MCED-All","SCED-10","MCED-10"))+
  #scale_y_discrete()+
  theme_bw()+
  theme(
    plot.title=element_text(hjust=0.5),
    legend.position="none",
    axis.line.x = element_line(colour = "black"),
    axis.line.y.right=element_line(colour = "black"),
    axis.text.y=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    axis.text = element_text(size=12),
    title=element_text(size=18))+
  labs(y="",x="System")+
  ggtitle("")

tp_break<-c(0,5e5,1e6,2e6)
tp_label<-sprintf("$%s",prettyNum(tp_break,scientific=FALSE,big.mark=',',preserve_width="none"))

figure_four_tp<-figure_four_table %>%
  ggplot(aes(x=System))+
  geom_col(aes(y=NonFPDiagnosticCost,fill=System),color="black")+
  geom_text(aes(y=2.5e5,label=c("USPSTF","SCED-10","MCED-10","MCED-All")),size=eq_text_size)+
  expand_limits(y=c(0,2.1e6))+
  coord_flip()+
  scale_y_continuous(breaks=tp_break,labels=tp_label,expand=c(0,0))+
  scale_fill_manual(name="",
                    #values=hue_pal()(4),
                    values=four_system_color,
                    breaks=c("uspstf","mced_all","sced_ten","mced_ten"),
                    labels=c("USPSTF","MCED-All","SCED-10","MCED-10"))+
  theme_bw()+
  theme(
    plot.title=element_text(hjust=0.5),
    legend.position="none",
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size=12),
    axis.text.y=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    title=element_text(size=18))+
  labs(y="")+
  ggtitle("")

figure_four_total=figure_four_table %>%
  mutate(offset=-1e8,
         prettyCost=sprintf("$%s",prettyNum(TotalCost,big.mark=','))) %>%
  mutate(offset_screen=-4e8,
         prettyScreenCost=sprintf("$%s",prettyNum(SumScreeningCost,big.mark=','))) %>%
  mutate(offset_nonfp=-7e8,
         prettyNonCost=sprintf("$%s",prettyNum(NonFPDiagnosticCost,big.mark=','))) %>%
  ggplot(aes(x=System))+
  geom_text(aes(y=offset,label=prettyCost),hjust=1,size=eq_text_size)+
  geom_text(aes(y=-3.5e8,label="="),size=eq_text_size)+
  geom_text(aes(y=offset_screen,label=prettyScreenCost),hjust=1,size=eq_text_size)+
  geom_text(aes(y=-6.5e8,label="+"),size=eq_text_size)+
  geom_text(aes(y=offset_nonfp,label=prettyNonCost),hjust=1,size=eq_text_size)+
  annotate("text", x=4.2,y=-7e8,label="TP Workup Costs",hjust=1,size=eq_text_size)+
  annotate("text", x=4.2,y=-4e8,label="Screening Costs",hjust=1,size=eq_text_size)+
  annotate("text", x=4.2,y=-1e8,label="Total Costs",hjust=1,size=eq_text_size)+
  geom_col(aes(y=TotalCost,fill=System),color="black")+
  expand_limits(y=c(-9e8,5e8))+
  coord_flip()+
  scale_fill_manual(name="",
                    #values=hue_pal()(4),
                    values=four_system_color,
                    breaks=c("uspstf","mced_all","sced_ten","mced_ten"),
                    labels=c("USPSTF","MCED-All","SCED-10","MCED-10"))+
  theme_void()+
  theme(legend.position="none")



final_figure_four_plot<-figure_four_fp+figure_four_tp+figure_four_total& theme(plot.margin=margin(1,0,0,0))

#ggsave(sprintf("figs/%s_figure_four_plot.png",date_code),
#       final_figure_four_plot,
#       width=32,height=8)

ggsave(sprintf("figs/%s_figure_four_plot.pdf",date_code),
       final_figure_four_plot,
       width=32,height=8)

write_tsv(figure_four_table,sprintf("reports/%s_figure_four_table_raw.tsv",date_code))

