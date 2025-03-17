#read system definitions for each system

excel_path<-"data/20230317_system_defn.xlsx"

#uspstf system definition has four components
uspstf_defn<-read_excel(excel_path,
                        sheet="uspstf",
                        range="A1:I5")
uspstf_sens<-read_excel(excel_path,
                        sheet="uspstf",
                        range="K1:L5")
uspstf_spec<-read_excel(excel_path,
                        sheet="uspstf",
                        range="N1:O5")
uspstf_cost<-read_excel(excel_path,
                        sheet="uspstf",
                        range="Q1:R5")

#sced_10 system definition
sced_ten_defn<-read_excel(excel_path,
                        sheet="sced_10",
                        range="A1:I28")
sced_ten_sens<-read_excel(excel_path,
                        sheet="sced_10",
                        range="K1:L28")
sced_ten_spec<-read_excel(excel_path,
                        sheet="sced_10",
                        range="N1:O28")
sced_ten_cost<-read_excel(excel_path,
                        sheet="sced_10",
                        range="Q1:R28")

#note that MCED system definitions differ
#mced_ten system definition
mced_ten_defn<-read_excel(excel_path,
                          sheet="mced_10",
                          range="A1:I28")
mced_ten_sens<-read_excel(excel_path,
                          sheet="mced_10",
                          range="K1:M93")
mced_ten_spec<-read_excel(excel_path,
                          sheet="mced_10",
                          range="O1:P2")
mced_ten_cost<-read_excel(excel_path,
                          sheet="mced_10",
                          range="R1:S2")

#mced_all system definition
mced_all_defn<-read_excel(excel_path,
                          sheet="mced_all",
                          range="A1:I28")
mced_all_sens<-read_excel(excel_path,
                          sheet="mced_all",
                          range="K1:M93")
mced_all_spec<-read_excel(excel_path,
                          sheet="mced_all",
                          range="O1:P2")
mced_all_cost<-read_excel(excel_path,
                          sheet="mced_all",
                          range="R1:S2")