#figure 5
#figure 5
#cumulative FPR across 30 years of screening
#expected FP per 'average individual'

figure_five_table<-system_results %>%
  mutate(Total_Individuals=Total_TP+Individual_FP+Individual_TN+Total_FN) %>%
  select(System,Total_Individuals,Individual_FP) %>%
  mutate(YearRate=Individual_FP/Total_Individuals) %>%
  cross_join(tibble(round=0:29)) %>%
  mutate(age=50+round,
         rrisk=(1-YearRate)) %>%
  group_by(System) %>%
  mutate(crisk=cumprod(rrisk)) %>%
  mutate(erisk=cumsum(YearRate)) %>%
  ungroup() 

figure_five_plot_a<-figure_five_table %>%
  filter(System!="uspstf") %>%
  ggplot(aes(x=age,y=erisk))+
  geom_line(aes(color=System,group=System,linetype=System),linewidth=4)+
  scale_color_manual(name="",
                    #values=hue_pal()(4),
                    values=four_system_color,
                    breaks=c("uspstf","mced_all","sced_ten","mced_ten"),
                    labels=c("USPSTF","MCED-All","SCED-10","MCED-10"))+
  scale_linetype_manual(name="",
                        values=c("solid","solid","solid","dashed"),
                        breaks=c("uspstf","mced_all","sced_ten","mced_ten"),
                        labels=c("USPSTF","MCED-All","SCED-10","MCED-10"))+
  theme_bw()+
  theme(
    legend.pos=c(0.8,0.3),
    title=element_text(size=18,face='bold'),
    text=element_text(size=30),
    panel.border=element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title=element_text(face='bold'),
    axis.text=element_text(size=24)
  )+
  labs(x="Patient Age (Years)",y="Cumulative Rounds of Screening Where Individual Receives\nA False Positive")+
  ggtitle("Cumulative Incidence of False Positives in Rounds of Screening")

figure_five_plot_b<-figure_five_table %>%
  filter(System!="uspstf") %>%
  ggplot(aes(x=age,y=crisk))+
  geom_line(aes(color=System,group=System,linetype=System),linewidth=4)+
  scale_color_manual(name="",
                     #values=hue_pal()(4),
                     values=four_system_color,
                     breaks=c("uspstf","mced_all","sced_ten","mced_ten"),
                     labels=c("USPSTF","MCED-All","SCED-10","MCED-10"))+
  scale_linetype_manual(name="",
                        values=c("solid","solid","solid","dashed"),
                        breaks=c("uspstf","mced_all","sced_ten","mced_ten"),
                        labels=c("USPSTF","MCED-All","SCED-10","MCED-10"))+
  theme_bw()+
  theme(
    legend.pos=c(0.8,0.3),
    title=element_text(size=18,face='bold'),
    text=element_text(size=30),
    panel.border=element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title=element_text(face='bold'),
    axis.text=element_text(size=24)
  )+
  labs(x="Patient Age (Years)",y="Odds of No False Positives")+
  ggtitle("Fraction of Individuals With No False Positives")

final_figure_five_plot<-figure_five_plot_a+figure_five_plot_b+plot_annotation(tag_levels='A')

ggsave(sprintf("figs/%s_figure_five_plot.pdf",date_code),
       final_figure_five_plot,
       width=22,height=11)

#ggsave(sprintf("figs/%s_figure_five_plot.eps",date_code),
#       final_figure_five_plot,
#       width=22,height=11)

write_tsv(figure_five_table,sprintf("reports/%s_figure_five_table_raw.tsv",date_code))
