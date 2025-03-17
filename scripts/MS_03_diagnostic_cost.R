#read diagnostic costs

#common across all modalities
#excel_path<-"data/20230317_system_defn.xlsx"

diagnostic_cost_df<-read_excel(excel_path,
                        sheet="Diagnostic",
                        range="A1:B28")

diagnostic_cost_df<-diagnostic_cost_df %>% 
  type_convert()

#multiplier for potentially more complex diagnostic costs for MCED than SCED

diagnostic_cost_multiplier_df<-read_excel(excel_path,
                                          sheet="RelativeCost",
                                          range="A1:B5")

