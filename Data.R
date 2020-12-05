library(tidyverse)
library(readxl)
library(magrittr)
library(forcats)
library(stringi)


hgs <-  read_excel(path ="Copy of HGS Renal biopsy Spreadsheet.xls" ,skip = 1)
qe <- read_excel("QE-complete-untidy.xlsx",
                 sheet = "Zeba Data collection", na = "?",
                 skip = 1)

qe %<>% select(Hospital_Number:`Side (L/R)`,Radiologist...21:`Post surgical histology...32`,`Date of death`:...44)


qe$HSite <- "qe"
hgs$HSite <- "hgs"



full <- bind_rows(qe,hgs)

full %<>% mutate(comments=coalesce(...36,...39)) %>% rename(Died=...34)
full %<>% select(-contains('...'),-`Date of death`)
full %<>% select_all(~str_replace_all(.,"([^a-zn^A-Z\\.\\w])","_"))
full <- distinct(full)
full %<>% janitor::remove_empty(c("rows","cols"))
full %<>% select(-c(3,4),-Consultant,-Radiologist)




# Initially planning on Removing duplicates with previous negative biopsies during same period
# however decided to treat them as separate instances, will still use the duplicates variable to populate indications
duplicates <- full$Hospital_Number[duplicated(full$Hospital_Number)]
# full %>% filter(!(Hospital_Number %in%  duplicates)) %>% select(Previous_Biopsy_) %>% table


### Clean Up
full %<>% rename(Gender=SEX__M_F_,Surgical_Histology=Post_surgical_histology)
full %<>% select_all(~str_replace(.,"_$",""))
full %<>% mutate(across(contains('eGFR'),~str_replace_all(.,">","")))
full %<>%  mutate(across(c(AGE___biopsy,Passes,Cores,Post_Bx_eGFR,Pre_bx_eGFR),~as.numeric(.)))
full %<>%  mutate(across(c(Gender,HSite),as_factor))



full %<>%
        mutate(Indication=case_when(
                grepl("[Rr]enal [Mm]ass",Indication)~"Renal Mass",
                grepl("[Rr]enal [Tt]umour",Indication)~"Renal Mass",
                grepl("[Rr]enal [Ll]esion",Indication)~"Renal Mass",
                grepl("[Kk]idney [Mm]ass",Indication)~"Renal Mass",
                grepl("[Kk]idney [Ll]esion",Indication)~"Renal Mass",
                grepl("[Cc]yst",Indication)~"Renal Cyst",
                grepl("[Rr]enal [Ll]esion",Indication)~"Renal Cyst",
                is.character(Indication)~"Indication is Not A Renal Lesion",
                T~NA_character_
        ))
full %<>% select(-Indication_Category)


full %<>%  mutate(Previous_Biopsy=case_when(
                           grepl("Y|Yes",Previous_Biopsy)==T~"Yes",
                           !is.na(Previous_Biopsy)==T~"No"))

# full$Previous_Biopsy[str_detect(full$Previous_Biopsy,"Yes|Y")&!is.na(full$Previous_Biopsy)] <- "Yes"
# full$Previous_Biopsy[str_detect(full$Previous_Biopsy,"N")&!is.na(full$Previous_Biopsy)]  <- "No"
# full$Previous_Biopsy[str_detect(full$Previous_Biopsy,"Previous")]  <- NA
# full$Previous_Biopsy[str_detect(full$Previous_Biopsy,"\\.")] <- NA
# full$Previous_Biopsy[str_detect(full$Previous_Biopsy,"RFA")&!is.na(full$Previous_Biopsy)] <- "Yes"
# full$Previous_Biopsy[is.na(full$Previous_Biopsy)&full$Indication=="Repeat Biopsy"] <- "Yes"



full %<>%  mutate(Imaging_Modality=case_when(grepl("[CTct]",Imaging_Modality)==T~"CT",
                                             grepl("[USus]",Imaging_Modality)==T~"USS",
                                             grepl("\\.",Imaging_Modality)==T~NA_character_,
                                             is.na(Imaging_Modality)==T ~ "MRI"))



full %<>% mutate(rT_Stage=case_when(grepl("\\.",rT_Stage)==T ~NA_character_,
                                    grepl("rT-Stage",rT_Stage)==T~NA_character_,
                                   is.na(rT_Stage)==T~NA_character_,
                                   TRUE~as.character(rT_Stage)))

full %<>% mutate(Solid_Cystic=case_when(
        grepl("[Ss]olid",Solid_Cystic)& grepl("[Cc]yst",Solid_Cystic)==T~"Mixed",
        grepl("[Mm]ixed",Solid_Cystic)==T~"Mixed",
        grepl("[Cc]ys",Solid_Cystic)==T ~"Cystic",
        grepl("[Ss]ol",Solid_Cystic)==T~"Solid",
        grepl("repor",Solid_Cystic)==T~"Report Not Available",
        TRUE ~ NA_character_)
        )

size <- full$Size__mm %>% str_split(.,"[0-9*.0-9*]''|x|X|,|''|\\*",simplify = T) %>% as.data.frame
size$V4 <- NA
size$V3 %>% table
size %<>%  mutate(V4=case_when(grepl("cm",.$V1,ignore.case=T )== T~"cm",
                               grepl("mm",.$V1,ignore.case=  T)==T~"mm",
                               grepl("cm",.$V2,ignore.case=T )== T~"cm",
                               grepl("mm",.$V2,ignore.case=  T)==T~"mm",
                               grepl("cm",.$V3,ignore.case=T )== T~"cm",
                               grepl("mm",.$V3,ignore.case=  T)==T~"mm",
                               T~'mm')
                  )
size %<>% mutate(V1=case_when(.$V4=="cm"~as.numeric(str_remove_all(.$V1,"cm"))*10,
                             .$V4=="mm"~as.numeric(str_remove_all(.$V1,"mm")),
                             is.numeric(.$V1)~as.numeric(.$V1),
                              is.na(.$V4)~NA_real_
                             ),
                V2=case_when(.$V4=="cm"~as.numeric(str_remove_all(.$V2,"cm"))*10,
                             .$V4=="mm"~as.numeric(str_remove_all(.$V2,"mm")),
                             is.numeric(.$V2)~as.numeric(.$V2),
                             is.na(.$V4)~NA_real_
                             ),
                V3=case_when(.$V4=="cm"~as.numeric(str_remove_all(.$V3,"cm"))*10,
                             .$V4=="mm"~as.numeric(str_remove_all(.$V3,"mm")),
                             is.numeric(.$V3)~as.numeric(.$V3),
                             is.na(.$V4)~NA_real_
                ))


size$largestmm <- NA
size %<>% rowwise() %>%  mutate(largestmm=max(c(V1,V2,V3),na.rm = T))
full %<>% mutate(largestmm=size$largestmm) %>% select(Hospital_Number:Solid_Cystic,largestmm,Site__U_M_L_H:Surgical_Histology,HSite)

full %>%select(Site__U_M_L_H) %>% table
full %<>% mutate(Endo_vs_Exo=case_when(
                         grepl("En",Endo_vs_Exo,ignore.case = T)==T~"Endophitic",
                         grepl("ex",Endo_vs_Exo,ignore.case = T)==T~"Exophitic",
                         T~NA_character_),
                 Site__U_M_L_H=case_when(
                        grepl("[Kk]idney",Site__U_M_L_H)~"Replaces Kidney",
                        grepl("[U]",Site__U_M_L_H)~"Upper Pole",
                        grepl("[L]",Site__U_M_L_H)~"Lower Pole",
                        grepl("[M]",Site__U_M_L_H)~"Middle Pole",
                        T~NA_character_)
                           )

full %<>% mutate(Side__L_R=case_when(
                        grepl("L",Side__L_R)==T~"Left",
                        grepl("R",Side__L_R)==T~"Right",
                        grepl("\\.",Side__L_R)==T~NA_character_,
                                    T~NA_character_),
                 Grade=case_when(
                        grepl("spr|reg|JSD|MIDDLE",Grade,ignore.case = T)==T~"Middle Grade",
                        grepl("[cC]ons",Grade)==T~"Consultant",
                        T~"No Report"),
                 Modlaity__CT_USS=case_when(
                        grepl("ct",Modlaity__CT_USS,ignore.case = T)==T~"CT",
                        grepl("US",Modlaity__CT_USS,ignore.case = T)==T~"US",
                        grepl(".",Modlaity__CT_USS,ignore.case = T)==T~NA_character_),
                 complicatons=case_when(
                        grepl("haem|hema",complicatons,ignore.case = T)==T~"Peri Renal Haematoma",
                        complicatons=="Abandoned"|complicatons=="Not done"~"Not Done",
                        grepl("Nil|N",complicatons,ignore.case = T)==T~"No complication",
                        TRUE ~ NA_character_),
                 Biopsy_outcome=case_when(
                         grepl("^[Dd]",Biopsy_outcome)==T~"Diagnostic",
                         grepl("^[Nn]|^[Ss]|^[U]",Biopsy_outcome)==T~"Non-Diagnostic",
                         grepl("missed",Biopsy_outcome,ignore.case = T)==T~"Missed Target Lesion",
                         T~NA_character_),
                 Histology_Simplified=case_when(
                         grepl("RCC|pap|pleo",Histology,ignore.case = T)~"Renal Cell Carcinoma",
                         grepl("TCC|T&",Histology,ignore.case = T)~"Transitional Cell Carcinoma",
                         grepl("cyst",Histology,ignore.case = T)~"Cyst",
                         grepl("nec|infarc|debris",Histology,ignore.case =T)~"Necrotic Tissue",
                         grepl("infar|degen",Histology,ignore.case =T)~"Necrotic Tissue",
                         grepl("stromal|Normal|Native|par|no malig|non diag|cortic|no renal| non-diag|no evidence|low grade|suboptimal",
                               Histology,ignore.case =T)~"Normal Renal Tissue or Not Diagnostic ",
                         grepl("fibro|fatt|adipose",Histology,ignore.case = T)~"Fibroadipose Tissue",
                         grepl("myol",Histology,ignore.case = T)~"AML",
                         grepl("[Oo]nco",Histology,ignore.case = T)~"Oncocytoma",
                         grepl("B-cell|hodg",Histology,ignore.case = T)~"Non-Hodgkin Lymphoma",
                         is.na(Histology)~NA_character_,
                         T~"Other"
                         ),
                 ManagementSimp=case_when(
                         grepl("TKI|suni",Management,ignore.case = T)~"Chemotherapy-TKI,PKI",
                         grepl("partial",Management,ignore.case = T)~"Partial Nephrectomy",
                         grepl("Rradical|Neph",Management,ignore.case = T)~"Radical Nephrectomy",
                         grepl("surveillance|watch|TLC|",Management,ignore.case = T)~"Surveillance",
                         grepl("ablation",Management,ignore.case = T)~"Cryoablation",
                         grepl("refer",Management,ignore.case = T)~"Not Primary Renal - Referred"),
                 Surgical_HistologySim=case_when(
                         grepl("RCC|pap|pleo",Surgical_Histology,ignore.case = T)~"Renal Cell Carcinoma",
                         grepl("TCC|T=&",Surgical_Histology,ignore.case = T)~"Transitional Cell Carcinoma",
                         grepl("sarc",Surgical_Histology,ignore.case = T)~"Sarcoma",
                         grepl("scc",Surgical_Histology,ignore.case = T)~"SCC",
                         grepl("onc",Surgical_Histology,ignore.case = T)~"Oncocytoma",
                         grepl("Lymphoma",Surgical_Histology,ignore.case = T)~"Lymphoma",
                         T~NA_character_)
                 )







full %<>% mutate(across(c(Indication,Previous_Biopsy,
                        Imaging_Modality,Solid_Cystic,Endo_vs_Exo,Side__L_R,
                        Modlaity__CT_USS,complicatons,Biopsy_outcome,ManagementSimp
                        ,Histology_Simplified,Surgical_HistologySim),~as_factor(.))) %>%
        select(AGE___biopsy:Post_Bx_eGFR,Histology_Simplified,Biopsy_outcome,ManagementSimp,Surgical_HistologySim,HSite)

full %<>% mutate(Surgery=case_when(
        grepl("Nephrectomy",ManagementSimp)~"Had Surgery",
        T~"Didnt Have Surgery or Histology Not Repoerted"))

Discrepancy<- full %>% filter(Surgery=="Had Surgery") %>% select(Histology_Simplified,Surgical_HistologySim)

full %<>% mutate(Match = case_when(
       stri_compare(Histology_Simplified,Surgical_HistologySim)==-1~"Biopsy Different to Surgical Histology",
       stri_compare(Histology_Simplified,Surgical_HistologySim)==0~"Biopsy Same as Surgical Histology",
       is.na(stri_compare(Histology_Simplified,Surgical_HistologySim))~"Not Applicable"))
