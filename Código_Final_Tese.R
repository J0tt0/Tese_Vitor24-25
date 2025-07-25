library(readr)
library(dplyr)
library(tools)
library(ggplot2)
library(lme4)
library(lmerTest)
library(lmtest)
library(DHARMa)
library(nnet)
library(car)
library(brms)
library(performance)
library(vcdExtra)
library(rstatix)
library(tidyr)
library(gt)
library(gtsummary)
library(emmeans)
library(afex)
library(xgboost)
library(doParallel)
library(kernlab)
library(MLmetrics)
library(caret)
library(ordinal)
library(iml)


#Importar tabelas (Cohort)
Cohort_ADAS_ADNI1 <- read_csv("Cohort_1_ADAS_ADNI1_28Oct2024.csv")

Cohort_ADAS_ADNIGO23 <- read_csv("Cohort_1_ADAS_ADNIGO23_28Oct2024.csv")

Cohort_ADASSCORES <- read_csv("Cohort_1_ADASSCORES_28Oct2024.csv")

Cohort_ADSXLIST <- read_csv("Cohort_1_ADSXLIST.csv")

Cohort_ADVERSE <- read_csv("Cohort_1_ADVERSE.csv")

Cohort_AMNART <- read_csv("Cohort_1_AMNART_28Oct2024.csv")

Cohort_ANTIAMYTX <- read_csv("Cohort_1_ANTIAMYTX_28Oct2024.csv")

Cohort_APOERES <- read_csv("Cohort_1_APOERES_28Oct2024.csv")

Cohort_BACKMEDS <- read_csv("Cohort_1_BACKMEDS_28Oct2024.csv")

Cohort_BHR <- read_csv("Cohort_1_BHR_28Oct2024.csv")

Cohort_BIOMARK <- read_csv("Cohort_1_BIOMARK_28Oct2024.csv")

Cohort_BLCHANGE <- read_csv("Cohort_1_BLCHANGE_28Oct2024.csv")

Cohort_BLSCHECK <- read_csv("Cohort_1_BLSCHECK_28Oct2024.csv")

Cohort_CBBCOMP <- read_csv("Cohort_1_CBBCOMP_28Oct2024.csv")

Cohort_CCI <- read_csv("Cohort_1_CCI_28Oct2024.csv")

Cohort_CDR <- read_csv("Cohort_1_CDR_28Oct2024.csv")

Cohort_CSSRSAD <- read_csv("Cohort_1_CSSRSAD_28Oct2024.csv")

Cohort_DXSUM <- read_csv("Cohort_1_DXSUM_28Oct2024.csv")

Cohort_ECOG12PT <- read_csv("Cohort_1_ECOG12PT_28Oct2024.csv")

Cohort_ECOG12SP <- read_csv("Cohort_1_ECOG12SP_28Oct2024.csv")

Cohort_ECOGPT <- read_csv("Cohort_1_ECOGPT_28Oct2024.csv")

Cohort_ECOGSP <- read_csv("Cohort_1_ECOGSP_28Oct2024.csv")

Cohort_FAQ <- read_csv("Cohort_1_FAQ_28Oct2024.csv")

Cohort_FCI <- read_csv("Cohort_1_FCI_28Oct2024.csv")

Cohort_GDSCALE <- read_csv("Cohort_1_GDSCALE_28Oct2024.csv")

Cohort_IES <- read_csv("Cohort_1_IES_28Oct2024.csv")

Cohort_INITHEALTH <- read_csv("Cohort_1_INITHEALTH_28Oct2024.csv")

Cohort_MEDHIST <- read_csv("Cohort_1_MEDHIST_28Oct2024.csv")

Cohort_MMSE <- read_csv("Cohort_1_MMSE_28Oct2024.csv")

Cohort_MOCA <- read_csv("Cohort_1_MOCA_28Oct2024.csv")

Cohort_MODHACH <- read_csv("Cohort_1_MODHACH_28Oct2024.csv")

Cohort_NEUROBAT <- read_csv("Cohort_1_NEUROBAT_28Oct2024.csv")

Cohort_NEUROEXM <- read_csv("Cohort_1_NEUROEXM_28Oct2024.csv")

Cohort_NPI <- read_csv("Cohort_1_NPI_28Oct2024.csv")

Cohort_NPIQ <- read_csv("Cohort_1_NPIQ_28Oct2024.csv")

Cohort_PEDQCV <- read_csv("Cohort_1_PEDQCV_28Oct2024.csv")

Cohort_PHYSICAL <- read_csv("Cohort_1_PHYSICAL_28Oct2024.csv")

Cohort_PSS <- read_csv("Cohort_1_PSS_28Oct2024.csv")

Cohort_RECADV <- read_csv("Cohort_1_RECADV_28Oct2024.csv")

Cohort_RECBLLOG <- read_csv("Cohort_1_RECBLLOG_28Oct2024.csv")

Cohort_RECCMEDS <- read_csv("Cohort_1_RECCMEDS_28Oct2024.csv")

Cohort_RECMHIST <- read_csv("Cohort_1_RECMHIST_28Oct2024.csv")

Cohort_RYFF <- read_csv("Cohort_1_RYFF_28Oct2024.csv")

Cohort_STAIAD <- read_csv("Cohort_1_STAIAD_28Oct2024.csv")

Cohort_VITALS <- read_csv("Cohort_1_VITALS_28Oct2024.csv")

Cohort_WATC <- read_csv("Cohort_1_WATC_28Oct2024.csv")

Cohort_UPENNBIOMK_ROCHE_ELECSYS_18Nov2024 <- read_csv("Cohort_1_UPENNBIOMK_ROCHE_ELECSYS_18Nov2024.csv")

Cohort_Group <- read_csv("Cohort_1_First_Visit.csv")

Cohort_PTDEMOG <- read_csv("Cohort_1_PTDEMOG_28Jan2025.csv")



#Verificação de NA
colSums(is.na(Cohort_ADAS_ADNIGO23)) 

colSums(is.na(Cohort_ADAS_ADNI1))

colSums(is.na(Cohort_ADASSCORES)) 

colSums(is.na(Cohort_ADSXLIST)) 

colSums(is.na(Cohort_ADVERSE)) 

colSums(is.na(Cohort_AMNART)) 

colSums(is.na(Cohort_ANTIAMYTX))

colSums(is.na(Cohort_APOERES)) 

colSums(is.na(Cohort_BACKMEDS)) 

colSums(is.na(Cohort_BHR)) 

colSums(is.na(Cohort_BIOMARK)) 

colSums(is.na(Cohort_BLCHANGE))

colSums(is.na(Cohort_BLSCHECK)) 

colSums(is.na(Cohort_CBBCOMP))

colSums(is.na(Cohort_CCI)) 

colSums(is.na(Cohort_CDR)) 

colSums(is.na(Cohort_CSSRSAD)) 

colSums(is.na(Cohort_DXSUM)) 

colSums(is.na(Cohort_ECOG12PT)) 

colSums(is.na(Cohort_ECOG12SP)) .

colSums(is.na(Cohort_ECOGPT))

colSums(is.na(Cohort_ECOGSP))

colSums(is.na(Cohort_FAQ)) 

colSums(is.na(Cohort_FCI)) 

colSums(is.na(Cohort_GDSCALE))

colSums(is.na(Cohort_IES)) 

colSums(is.na(Cohort_INITHEALTH)) 

colSums(is.na(Cohort_MEDHIST))

colSums(is.na(Cohort_MMSE)) 

colSums(is.na(Cohort_MOCA))

colSums(is.na(Cohort_MODHACH)) 

colSums(is.na(Cohort_NEUROBAT)) #Cheia de NA, verificar se há necessidade de utilização (1/4 do dataset em falta)

colSums(is.na(Cohort_NEUROEXM)) #Quase limpa 38 NA em praticamente todas as variaveis, 4 quase na sua totalidade ausentes

colSums(is.na(Cohort_NPI)) #Praticamente em falta

colSums(is.na(Cohort_NPIQ)) #Variaveis com 1/3 dos valores em falta.

colSums(is.na(Cohort_PEDQCV)) #Variavel NDREASON na totalidade ausente, restantes com 27 NA.

colSums(is.na(Cohort_PHYSICAL)) #Quase limpa 35 NA em praticamente todas as variaveis, 4 quase na sua totalidade ausentes

colSums(is.na(Cohort_PSS)) #Variavel NDREASON na totalidade ausente, restantes com 22 NA.

colSums(is.na(Cohort_RECADV)) #Bastante NA em diversas variaveis (Quase inutilizavel)

colSums(is.na(Cohort_RECBLLOG)) #Praticamente Limpa, 2 variveis muito ausentes e 1 variavel com 4126 NA.

colSums(is.na(Cohort_RECCMEDS)) #Não vejo utilidade do uso desta tabela

colSums(is.na(Cohort_RECMHIST)) #Não vejo utilidade do uso desta tabela

colSums(is.na(Cohort_RYFF)) #Praticamente limpa apenas 1 NA em VISCODE2 e VISDATE

colSums(is.na(Cohort_STAIAD)) #Praticamente limpa apenas 1 NA em VISCODE2/VISDATE/STAIAD5

colSums(is.na(Cohort_VITALS)) #Tem algumas variaveis com NA, variavel VISCODE bate com a variavel EXAMDATE da tabela ADAS_ADNI1

colSums(is.na(Cohort_WATC)) 

colSums(is.na(Cohort_UPENNBIOMK_ROCHE_ELECSYS_18Nov2024)) 

colSums(is.na(Cohort_Group)) 


###### Conjunto inical ###########

# Criar uma nova coluna padronizada para VISCODE2 em Cohort_UPENNBIOMK_ROCHE_ELECSYS_18Nov2024
Cohort_UPENNBIOMK_ROCHE_ELECSYS_18Nov2024 <- Cohort_UPENNBIOMK_ROCHE_ELECSYS_18Nov2024 %>%
  mutate(VISCODE2_padronizado = ifelse(VISCODE2 == "bl", "sc", VISCODE2))

# Criar uma nova coluna padronizada para VISCODE2 em Cohort_MMSE
Cohort_MMSE <- Cohort_MMSE %>%
  mutate(VISCODE2_padronizado = VISCODE2)


# Realizar o merge usando a nova coluna padronizada (bl,m24,m48)
merged_MMSE <- merge(
  Cohort_UPENNBIOMK_ROCHE_ELECSYS_18Nov2024, 
  Cohort_MMSE, 
  by = c("RID", "VISCODE2_padronizado") # Adicionando RID à fusão
)

# Remover colunas duplicadas terminadas em .y
merged_MMSE <- merged_MMSE %>% select(-ends_with(".y"))

# Renomear colunas removendo o sufixo .x
colnames(merged_MMSE) <- gsub("\\.x$", "", colnames(merged_MMSE))

# Remover duplicatas corretamente por RID e VISCODE2
merged_distMMSE <- merged_MMSE %>% distinct(RID, VISCODE2, .keep_all = TRUE)

# Filtrar apenas os valores de VISCODE2 que interessam
merged_cutMMSE <- merged_distMMSE %>% filter(VISCODE2 %in% c("bl", "m24", "m48"))

# Garantir que cada RID tem exatamente "bl", "m24" e "m48"
merged_endMMSE <- merged_cutMMSE %>% 
  group_by(RID) %>% 
  filter(all(c("bl", "m24", "m48") %in% VISCODE2)) %>% 
  ungroup()

# Adicionar GENOTYPE da tabela Cohort_APOERES
Cohort_APOERES_subset <- Cohort_APOERES %>% select(RID, GENOTYPE)

merged_apoeMMSE <- merge(merged_endMMSE, Cohort_APOERES_subset, by = "RID")

# Ajustar nomes da tabela Cohort_Group e fazer merge corretamente
Cohort_Group_cut <- Cohort_Group %>% filter(subject_visit == "sc") 

# Ajustar nome da coluna para coincidir com merged_apoeMMSE
Cohort_Group_cut <- Cohort_Group_cut %>% rename(PTID = subject_id)

merged_apoe_finalMMSE <- merge(merged_apoeMMSE, Cohort_Group_cut, by = "PTID")


######################### Limpeza do conjunto ########################### 


colSums(is.na(merged_apoe_finalMMSE))

#MMLTR1,MMLTR2,MMLTR3,MMLTR4,MMLTR5,MMLTR6,MMLTR7,WORLDSCORE,USERDATE2,DD_CRF_VERSION_LABEL,LANGUAGE_CODE,HAS_QC_ERROR;ABETA40

cols_remove <- c("MMLTR1","MMLTR2","MMLTR3","MMLTR4","MMLTR5","MMLTR6","MMLTR7",
                 "WORLDSCORE","USERDATE2","DD_CRF_VERSION_LABEL","LANGUAGE_CODE",
                 "HAS_QC_ERROR","ABETA40","DONE","NDREASON","SOURCE","WORDLIST",
                 "merged_MMSE$RID", "merged_MMSE$VISCODE2","USERDATE2","SITEID",
                 "ID","VISCODE","VISDATE","VISCODE2","RUNDATE",
                 "update_stamp","MMDATE","USERDATE","subject_visit")

Data_MMSE_cut <- Remove_columns(merged_apoe_finalMMSE,cols_remove)

colSums(is.na(Data_MMSE_cut))

PTID_NAcomment <- Data_MMSE_cut$PTID[!is.na(Data_MMSE_cut$COMMENT)]

PTID_CommentUnique <- unique(PTID_NAcomment)

Data_3times <- Data_MMSE_cut %>% filter(!Data_MMSE_cut$PTID %in% PTID_CommentUnique)

data_selected <- Data_3times %>% select(-PTID, -RID, -VISCODE2_padronizado, -EXAMDATE,-BATCH,-COMMENT)

Data_3times$VISCODE2_padronizado = factor(ifelse(Data_3times$VISCODE2_padronizado == "sc", "bl",
                                                 ifelse (Data_3times$VISCODE2_padronizado == "m24", "m24", "m48")), levels= c("bl","m24", "m48"))


Data_3times <- Data_3times %>%
  mutate(GENOTYPE = case_when(
    GENOTYPE == "2/3" ~ 0,
    GENOTYPE == "2/4" ~ 1,
    GENOTYPE == "3/3" ~ 2,
    GENOTYPE == "3/4" ~ 3,
    GENOTYPE == "4/4" ~ 4,
    TRUE ~ NA_real_  # Ensures missing values instead of errors
  ))


Data_3times$GENOTYPE <- factor(Data_3times$GENOTYPE, 
                               levels = c(0,1,2,3,4), 
                               labels = c("2/3", "2/4", "3/3", "3/4", "4/4"))

########################## COLOCAR O GÉNERO ################################


Data_3times <- merge(Data_3times, Cohort_PTDEMOG[, c("PTID", "PTGENDER")], 
                     by = "PTID", all.x = TRUE)

names(Data_3times)[names(Data_3times) == "PTGENDER"] <- "Gender"


Data_3times <- Data_3times %>%
  filter(!is.na(Gender))


Data_3times <- Data_3times %>%
  distinct(PTID, VISCODE2_padronizado, .keep_all = TRUE)



##################### COLOCAR A EDUCATION #################################

Data_3times <- merge(Data_3times, Cohort_PTDEMOG[, c("PTID", "PTEDUCAT")], 
                     by = "PTID", all.x = TRUE)

names(Data_3times)[names(Data_3times) == "PTEDUCAT"] <- "Education"

Data_3times <- Data_3times %>%
  filter(!is.na(Education))


Data_3times <- Data_3times %>%
  distinct(PTID, VISCODE2_padronizado, .keep_all = TRUE)

########################## CDR Score ###########################################

Cohort_CDR <- Cohort_CDR %>% 
  mutate(VISCODE2_padronizado = ifelse(VISCODE2 == "sc", "bl", VISCODE2))

Data_3times <- merge(Data_3times, Cohort_CDR[, c("PTID", "VISCODE2_padronizado","CDMEMORY")], 
                     by = c("PTID","VISCODE2_padronizado"), all.x = TRUE)

names(Data_3times)[names(Data_3times) == "CDMEMORY"] <- "MEMORYScore"


Data_3times <- Data_3times %>%
  filter(!is.na(MEMORYScore))


Data_3times <- Data_3times %>%
  distinct(PTID, VISCODE2_padronizado, .keep_all = TRUE)

Data_3times <- Data_3times %>% filter(CDRScore >= 0)

########################## Memory Score ###########################################

Cohort_CDR <- Cohort_CDR %>% 
  mutate(VISCODE2_padronizado = ifelse(VISCODE2 == "sc", "bl", VISCODE2))

Data_3times <- merge(Data_3times, Cohort_CDR[, c("PTID", "VISCODE2_padronizado","CDGLOBAL")], 
                     by = c("PTID","VISCODE2_padronizado"), all.x = TRUE)

names(Data_3times)[names(Data_3times) == "CDGLOBAL"] <- "CDRScore"


Data_3times <- Data_3times %>%
  filter(!is.na(CDRScore))


Data_3times <- Data_3times %>%
  distinct(PTID, VISCODE2_padronizado, .keep_all = TRUE)



########################## Wechsler (Delayed Recall) ###########################################

Cohort_NEUROBAT <- Cohort_NEUROBAT %>% 
  mutate(VISCODE2_padronizado = ifelse(VISCODE2 == "sc", "bl", VISCODE2))

Data_3times <- merge(Data_3times, Cohort_NEUROBAT[, c("PTID", "VISCODE2_padronizado","LDELTOTAL")], 
                     by = c("PTID","VISCODE2_padronizado"), all.x = TRUE)

names(Data_3times)[names(Data_3times) == "LDELTOTAL"] <- "Wechsler"


Data_3times <- Data_3times %>%
  filter(!is.na(Wechsler))


Data_3times <- Data_3times %>%
  distinct(PTID, VISCODE2_padronizado, .keep_all = TRUE)

############################ AGE ##############################################


# Função para calcular a idade em uma nova data de exame
calculate_age <- function(initial_age, initial_date, new_date) {
  age_diff <- as.numeric(difftime(new_date, initial_date, units = "days")) / 365.25
  new_age <- initial_age + age_diff
  return(round(new_age,2))
}

# Aplicar a função para cada RID e para os momentos m24 e m48
for (rid in unique(Data_3times$RID)) {
  # Filtrar os dados para o RID atual
  dados_rid <- subset(Data_3times, RID == rid)
  
  # Obter a idade inicial e a data inicial do exame
  initial_age <- dados_rid$subject_age[dados_rid$VISCODE2_padronizado == "bl"]
  initial_date <- dados_rid$EXAMDATE[dados_rid$VISCODE2_padronizado == "bl"]
  
  # Calcular a idade para os momentos m24 e m48
  Data_3times$subject_age[Data_3times$RID == rid & Data_3times$VISCODE2_padronizado %in% c("m24", "m48")] <- sapply(
    Data_3times$EXAMDATE[Data_3times$RID == rid & Data_3times$VISCODE2_padronizado %in% c("m24", "m48")],
    function(date) {
      calculate_age(initial_age, initial_date, date)
    }
  )
}


############################# Stage ###########################################
Data_3times$research_group <- NULL

Cohort_Group_cut <- Cohort_Group_cut %>% mutate(VISCODE2_padronizado = ifelse(subject_visit == "sc", "bl", subject_visit))

Data_3times <- merge(Data_3times, Cohort_Group_cut[, c("PTID", "VISCODE2_padronizado","research_group")],
                     by = c("PTID","VISCODE2_padronizado"), all.x = TRUE)

names(Data_3times)[names(Data_3times) == "research_group"] <- "Stage"

Data_3times$Stage <- factor(Data_3times$Stage, levels = c("CN", "SMC", "EMCI", "MCI", "LMCI", "AD"))



classificar_paciente2 <- function(data) {
  for (i in 1:nrow(data)) {
    
    # Se MMSE < 20, classifica como MCI e passa para o próximo paciente
    if (data$MMSCORE[i] < 20) {
      data$Stage[i] <- "AD"
      next
    }
    
    if (data$VISCODE2_padronizado[i] %in% c("m24", "m48")) {
      
      
      if (data$CDRScore[i] == 0.5 && data$MMSCORE[i] <= 24) {
        data$Stage[i] <- "AD"
        next
      }
      
      if (data$CDRScore[i] >= 0.5 && data$MMSCORE[i] >= 20 && data$MMSCORE[i] <= 24) {
        if ((data$Wechsler[i] <= 8 && data$Education[i] >= 16) ||
            (data$Wechsler[i] <= 4 && data$Education[i] >= 8 && data$Education[i] <= 15) ||
            (data$Wechsler[i] <= 2 && data$Education[i] <= 7)) {
          data$Stage[i] <- "AD"
          next  
        }
      }
      
      if (data$CDRScore[i] == 0.5 && data$MEMORYScore[i] >= 0.5 &&
          data$MMSCORE[i] >= 24 && data$MMSCORE[i] <= 30) {
        data$Stage[i] <- "MCI"
        next
      }
      
      # 
      if (data$CDRScore[i] == 0.5 && data$MEMORYScore[i] >= 0.5 &&
          data$MMSCORE[i] >= 24 && data$MMSCORE[i] <= 30) {
        if ((data$Wechsler[i] < 11 && data$Education[i] >= 16) ||
            (data$Wechsler[i] <= 9 && data$Education[i] >= 8 && data$Education[i] <= 15) ||
            (data$Wechsler[i] <= 6 && data$Education[i] <= 7)) {
          data$Stage[i] <- "MCI"
          next  
        }
      }
      
      # Classificação CN (caso o paciente não tenha sido classificado antes)
      if (data$CDRScore[i] == 0 && data$MEMORYScore[i] == 0 &&
          data$MMSCORE[i] >= 24 && data$MMSCORE[i] <= 30) {
        if ((data$Wechsler[i] >= 9 && data$Education[i] >= 16) ||
            (data$Wechsler[i] >= 5 && data$Education[i] >= 8 && data$Education[i] <= 15) ||
            (data$Wechsler[i] >= 3 && data$Education[i] <= 7)) {
          data$Stage[i] <- "CN"
          next  
        } else {
          data$Stage[i] <- "CN"
          next
        }
      }
      
      if (data$CDRScore[i] >= 1) {
        data$Stage[i] <- "AD"
        next
      }
      
      if (data$CDRScore[i] >= 0.5 && data$MEMORYScore[i] == 0) {
        data$Stage[i] <- "MCI"
        next
      }
    }
  }
  
  return(data)
}

ata_3times_clean <- classificar_paciente2(Data_3times)


Data_3times <- Data_3times %>% filter(CDRScore >= 0)
Data_3times <- Data_3times %>% filter(ABETA42 >=0)

# Contar as ocorrências de cada PTID
ptid_counts <- Data_3times %>%
  count(PTID, name = "count")  # Usa count() diretamente para simplificar

# Filtrar os PTIDs com menos de 3 entradas
ptids_with_less_than_3_entries <- ptid_counts %>%
  filter(count < 3)

# Exibir o resultado
print(ptids_with_less_than_3_entries)

# Filtrar Data_3times para manter apenas PTIDs com 3 ou mais entradas
Data_3times <- Data_3times %>%
  semi_join(ptid_counts %>% filter(count >= 3), by = "PTID")



Data_3times$Stage[Data_3times$Stage == "SMC"] <- "CN"

Data_3times$Stage[Data_3times$Stage == "EMCI"] <- "MCI"

Data_3times$Stage[Data_3times$Stage == "LMCI"] <- "MCI"


Data_3times <- Data_3times %>%
  mutate(Stage = factor(Stage, levels = c("CN", "MCI", "AD")))


###################### Remover observ. com MRI -3 ou !MPRAGE ########################


ptid_remov = c("005_S_2390", "006_S_4346", "007_S_2394", "007_S_4272", "007_S_4516", "007_S_4611", "007_S_4637",
               "007_S_5265", "022_S_4266", "023_S_0084", "023_S_0139", "023_S_2068", "024_S_5290", "027_S_2183",
               "027_S_2245", "027_S_2336", "027_S_4919", "027_S_6034", "031_S_0618", "033_S_4176", "033_S_4177",
               "033_S_6266", "035_S_6200", "037_S_6031", "041_S_1010", "041_S_1260", "098_S_2079", "099_S_6175",
               "100_S_6164", "123_S_0106", "127_S_4148", "127_S_4197", "127_S_4198", "127_S_4301", "127_S_4604",
               "127_S_5185", "127_S_5200", "127_S_6147", "127_S_6168", "129_S_4422", "129_S_6228", "130_S_6019",
               "141_S_6015", "301_S_6056")

Data_3times_clean <- Data_3times %>% filter(!PTID %in% ptid_remov)

Data_3times_clean$subject_age <- floor(Data_3times_clean$subject_age)

Data_3times_clean <- Data_3times_clean %>%
  mutate(
    CDRScore = factor(CDRScore, levels = c(0, 0.5, 1, 2, 3), ordered = TRUE),
    MEMORYScore = factor(MEMORYScore, levels = c(0, 0.5, 1, 2, 3), ordered = TRUE)
  )

########################### Verificar Normalidade ##############################

shapiro.test(Data_3times_clean$ABETA42) 

shapiro.test(Data_3times_clean$TAU) 

shapiro.test(Data_3times_clean$PTAU) 

shapiro.test(Data_3times_clean$subject_age)

shapiro.test(Data_3times_clean$MMSCORE) 

shapiro.test(Data_3times_clean$Wechsler) 

################################## TESTAR TAU ~ Momento ##################################################
friedman.test(TAU ~ VISCODE2_padronizado | RID, data = Data_3times_clean) # p-value 2.182e-07

pairwise.wilcox.test(Data_3times_clean$TAU, Data_3times_clean$VISCODE2_padronizado, 
                     paired = TRUE, p.adjust.method = "bonferroni")


ggplot(Data_3times_clean, aes(x = VISCODE2_padronizado, y = TAU, group = VISCODE2_padronizado)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Variação dos níveis de TAU ao longo do tempo")

cor.test(Data_3times_clean$subject_age, Data_3times_clean$TAU, 
         method = "spearman", exact = FALSE)

ggplot(Data_3times_clean, aes(x = subject_age, y = TAU)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title = "Correlação entre Idade e TAU",
       x = "Idade",
       y = "TAU")


############################## TESTAR ABETA42 ~ Momento #################################################
friedman.test(ABETA42 ~ VISCODE2_padronizado | RID, data = Data_3times_clean) # p-value 0.0002496

pairwise.wilcox.test(Data_3times_clean$ABETA42, Data_3times_clean$VISCODE2_padronizado, 
                     paired = TRUE, p.adjust.method = "bonferroni")


ggplot(Data_3times_clean, aes(x = VISCODE2_padronizado, y = ABETA42)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Variação dos níveis de ABETA42 ao longo do tempo")

cor.test(Data_3times_clean$subject_age, Data_3times_clean$ABETA42, 
         method = "spearman", exact = FALSE)

ggplot(Data_3times_clean, aes(x = subject_age, y = ABETA42)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title = "Correlação entre Idade e ABETA42",
       x = "Idade",
       y = "ABETA42")


############################# TESTAR PTAU ~ Momento ############################

friedman.test(PTAU ~ VISCODE2_padronizado | RID, data = Data_3times_clean) # p-value 4.766e-08

pairwise.wilcox.test(Data_3times_clean$PTAU, Data_3times_clean$VISCODE2_padronizado, 
                     paired = TRUE, p.adjust.method = "bonferroni")



ggplot(Data_3times_clean, aes(x = VISCODE2_padronizado, y = PTAU)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Variação dos níveis de PTAU ao longo do tempo")

cor.test(Data_3times_clean$subject_age, Data_3times_clean$PTAU, 
         method = "spearman", exact = FALSE)


ggplot(Data_3times_clean, aes(x = subject_age, y = PTAU)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title = "Correlação entre Idade e PTAU",
       x = "Idade",
       y = "PTAU")

############################## TESTAR MMSCORE ~ Momento #################################################
friedman.test(MMSCORE ~ VISCODE2_padronizado | RID, data = Data_3times_clean) # p-value 0.0005807

pairwise.wilcox.test(Data_3times_clean$MMSCORE, Data_3times_clean$VISCODE2_padronizado, 
                     paired = TRUE, p.adjust.method = "bonferroni")



ggplot(Data_3times_clean, aes(x = VISCODE2_padronizado, y = MMSCORE)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Variação dos níveis de MMSCORE ao longo do tempo")

cor.test(Data_3times_clean$subject_age, Data_3times_clean$MMSCORE, 
         method = "spearman", exact = FALSE)

ggplot(Data_3times_clean, aes(x = subject_age, y = MMSCORE)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title = "Correlação entre Idade e MMSCORE",
       x = "Idade",
       y = "MMSCORE")

############################## TESTAR CDRScore ~ Momento #################################################

friedman.test(CDRScore ~ VISCODE2_padronizado | RID, data = Data_3times_clean) # p-value 0.001795

modelo_clm = clmm(CDRScore ~ VISCODE2_padronizado + (1 | RID), data = Data_3times_clean)


emm_modelo <- emmeans(modelo_clm, specs = ~ VISCODE2_padronizado)

pairs(emm_modelo, adjust = "tukey")

ggplot(Data_3times_clean, aes(x = VISCODE2_padronizado, y = CDRScore)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Variação dos níveis de CDRScore ao longo do tempo")

cor.test(Data_3times_clean$subject_age, as.numeric(Data_3times_clean$CDRScore), 
         method = "spearman", exact = FALSE)



ggplot(Data_3times_clean, aes(x = subject_age, y = CDRScore)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title = "Correlação entre Idade e CDRScore",
       x = "Idade",
       y = "CDRScore")

############################## TESTAR Wechsler ~ Momento #################################################

friedman.test(Wechsler ~ VISCODE2_padronizado | RID, data = Data_3times_clean)  


ggplot(Data_3times_clean, aes(x = VISCODE2_padronizado, y = Wechsler)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Variação dos níveis de Wechsler ao longo do tempo")

cor.test(Data_3times_clean$subject_age, Data_3times_clean$Wechsler, 
         method = "spearman", exact = FALSE)


ggplot(Data_3times_clean, aes(x = subject_age, y = Wechsler)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title = "Correlação entre Idade e Wechsler",
       x = "Idade",
       y = "Wechsler")

############################## TESTAR MEMORYScore ~ Momento #################################################

friedman.test(MEMORYScore ~ VISCODE2_padronizado | RID, data = Data_3times_clean) 

modelo_clm_Mem = clmm(MEMORYScore ~ VISCODE2_padronizado + (1 | RID), data = Data_3times_clean)

summary(modelo_clm_Mem)

emm_modelo_Mem <- emmeans(modelo_clm_Mem, specs = ~ VISCODE2_padronizado)

pairs(emm_modelo_Mem, adjust = "tukey")


ggplot(Data_3times_clean, aes(x = VISCODE2_padronizado, y = MEMORYScore)) +
  geom_boxplot(aes(fill = VISCODE2_padronizado), width = 0.5, outlier.shape = 21, outlier.fill = "red") + 
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  theme_minimal() +
  labs(title = "Variação dos níveis de MEMORYScore ao longo do tempo",
       x = "Tempo (VISCODE2_padronizado)",
       y = "MEMORYScore") +
  theme(legend.position = "none")  # Remove a legenda desnecessária


cor.test(Data_3times_clean$subject_age, as.numeric(Data_3times_clean$MEMORYScore), 
         method = "spearman", exact = FALSE)

ggplot(Data_3times_clean, aes(x = subject_age, y = MEMORYScore)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal() +
  labs(title = "Correlação entre Idade e MEMORYScore",
       x = "Idade",
       y = "MEMORYScore")


###################### Tabela Geral da Progressao #################################

summary_table <- Data_3times_clean %>%
  count(VISCODE2_padronizado, Stage, evolucao) %>%
  pivot_wider(names_from = VISCODE2_padronizado, values_from = n, values_fill = 0) %>%
  arrange(Stage,evolucao)

summary_table %>%
  gt() %>%
  tab_header(
    title = "Distribuição de Pacientes por Stage e Evolução ao Longo do Tempo"
  ) %>%
  cols_label(
    Stage = "Estágio",
    evolucao = "Evolução"
  )

################### Tabela Geral de contagem Stage ############################


summary_geral <- Data_3times_clean %>%
  count(VISCODE2_padronizado, Stage) %>%
  pivot_wider(names_from = VISCODE2_padronizado, values_from = n, values_fill = 0) %>%
  arrange(Stage)

summary_geral %>%
  gt() %>%
  tab_header(
    title = "Distribuição de Pacientes por Stage ao Longo do Tempo"
  ) %>%
  cols_label(
    Stage = "Estágio"
  )

#################################### Tabela Medias Stage bl ######################################

table_mean_bl <- Data_3times_clean %>%
  filter(VISCODE2_padronizado == "bl") %>%
  tbl_summary(
    by = Stage,
    include = c(subject_age,ABETA42, PTAU, TAU, MMSCORE, Wechsler, CDRScore, MEMORYScore),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(MMSCORE ~ "continuous",
                subject_age ~ "continuous",
                CDRScore ~ "categorical",
                MEMORYScore ~ "categorical")
  ) %>%
  modify_caption("**Média de medidas pelos diferentes estágios na baseline**")

table_mean_bl

#################################### Tabela Medias Stage m24 ######################################

table_mean_m24 <- Data_3times_clean %>%
  filter(VISCODE2_padronizado == "m24") %>%
  tbl_summary(
    by = Stage,
    include = c(subject_age,ABETA42, PTAU, TAU, MMSCORE, Wechsler, CDRScore, MEMORYScore),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"), 
    type = list(MMSCORE ~ "continuous",
                subject_age ~ "continuous",
                CDRScore ~ "categorical",
                MEMORYScore ~ "categorical")
  ) %>%
  modify_caption("**Média de medidas pelos diferentes estágios aos 24 meses**")

table_mean_m24

############################### Tabela Medias Stage m48 ########################################

table_mean_m48 <- Data_3times_clean %>%
  filter(VISCODE2_padronizado == "m48") %>%
  tbl_summary(
    by = Stage,
    include = c(subject_age,ABETA42, PTAU, TAU, MMSCORE, Wechsler, CDRScore, MEMORYScore),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(MMSCORE ~ "continuous",
                subject_age ~ "continuous",
                CDRScore ~ "categorical",
                MEMORYScore ~ "categorical")
  ) %>%
  modify_caption("**Média de medidas pelos diferentes estágios aos 48 meses**")

table_mean_m48

################################ GERAL ###################################

table_mean <- Data_3times_clean %>%
  tbl_summary(
    by = Stage,                       
    include = c(ABETA42, PTAU, TAU, MMSCORE, Wechsler, GENOTYPE),   
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    type = list(MMSCORE ~ "continuous")
  ) %>%
  modify_caption("**Média de medidas pelos diferentes estágios**")

table_mean

################################# Tabela Geno Stage bl   ####################################

summary_Geno_bl <- Data_3times_clean %>%
  filter(VISCODE2_padronizado == "bl") %>%
  tbl_summary(
    by = Stage,                
    include = GENOTYPE,         
    statistic = GENOTYPE ~ "{n}" 
  ) %>%
  modify_caption("**Frequência do genótipo pelos diferentes estágios na baseline**")

summary_Geno_bl


################################# Tabela Geno Stage m24   ####################################

summary_Geno_m24 <- Data_3times_clean %>%
  filter(VISCODE2_padronizado == "m24") %>%
  tbl_summary(
    by = Stage,                
    include = GENOTYPE,         
    statistic = GENOTYPE ~ "{n}" 
  ) %>%
  modify_caption("**Frequência do genótipo pelos diferentes estágios aos 24 meses**")

summary_Geno_m24

################################# Tabela Geno Stage m48   ####################################

summary_Geno_m48 <- Data_3times_clean %>%
  filter(VISCODE2_padronizado == "m48") %>%
  tbl_summary(
    by = Stage,                
    include = GENOTYPE,         
    statistic = GENOTYPE ~ "{n}" 
  ) %>%
  modify_caption("**Frequência do genótipo pelos diferentes estágios aos 48 meses**")

summary_Geno_m48


###################### Estatistica Genero ##################

model_abeta42 <- lmer(ABETA42 ~ Gender +  (1 | RID), data = Data_3times_clean)
summary(model_abeta42)

model_tau <- lmer(TAU ~ Gender + VISCODE2_padronizado + (1 | RID), data = Data_3times_clean)
summary(model_tau)

model_ptau <- lmer(PTAU ~ Gender + VISCODE2_padronizado + (1 | RID), data = Data_3times_clean)
summary(model_ptau)

model_mmse <- lmer(MMSCORE ~ Gender + VISCODE2_padronizado + (1 | RID), data = Data_3times_clean)
summary(model_mmse)

model_wechsler <- lmer(Wechsler ~ Gender + VISCODE2_padronizado + (1 | RID), data = Data_3times_clean)
summary(model_wechsler)

model_cdr <- clmm(CDRScore ~ Gender + VISCODE2_padronizado + (1 | RID), data = Data_3times_clean)
summary(model_cdr)

model_memory <- clmm(MEMORYScore ~ Gender + VISCODE2_padronizado + (1 | RID), data = Data_3times_clean)
summary(model_memory)


Data_labeled <- Data_3times_clean %>%
  mutate(Gender_MF = case_when(
    Gender == "1" ~ "Male",
    Gender == "2" ~ "Female",
    TRUE ~ as.character(Gender) # Mantém outros valores, se existirem
  )) %>%
  mutate(Gender_MF = as.factor(Gender_MF)) # Converter para fator (opcional, mas recomendado)



table_gender_bl <- Data_labeled %>%
  filter(VISCODE2_padronizado == "bl") %>%
  tbl_summary(
    by = Gender_MF, # Adicionamos a separação por Gender
    include = c(subject_age,ABETA42, PTAU, TAU, MMSCORE, Wechsler, CDRScore, MEMORYScore, Stage),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(MMSCORE ~ "continuous",
                subject_age ~ "continuous",
                CDRScore ~ "categorical",
                MEMORYScore ~ "categorical")
  ) %>%
  modify_caption("**Média de medidas pelos diferentes estágios e género na baseline**")

table_gender_bl


table_gender_m24 <- Data_labeled %>%
  filter(VISCODE2_padronizado == "m24") %>%
  tbl_summary(
    by = Gender_MF, # Adicionamos a separação por Gender
    include = c(subject_age,ABETA42, PTAU, TAU, MMSCORE, Wechsler, CDRScore, MEMORYScore, Stage),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(MMSCORE ~ "continuous",
                subject_age ~ "continuous",
                CDRScore ~ "categorical",
                MEMORYScore ~ "categorical")
  ) %>%
  modify_caption("**Média de medidas pelos diferentes estágios e género aos 24 meses**")

table_gender_m24



table_gender_m48 <- Data_labeled %>%
  filter(VISCODE2_padronizado == "m48") %>%
  tbl_summary(
    by = Gender_MF, # Usamos a nova variável de género
    include = c(subject_age,ABETA42, PTAU, TAU, MMSCORE, Wechsler, CDRScore, MEMORYScore, Stage),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(MMSCORE ~ "continuous",
                subject_age ~ "continuous",
                CDRScore ~ "categorical",
                MEMORYScore ~ "categorical",
                Stage ~ "categorical")
  ) %>%
  modify_caption("**Média de medidas e distribuição de estágios por género aos 48 meses**")


table_gender_m48

table_gender_fixed <- Data_labeled %>%
  filter(VISCODE2_padronizado == "bl") %>%
  tbl_summary(
    by = Gender_MF, # Usamos a nova variável de género
    include = c(Education, GENOTYPE),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(Education ~ "continuous",
                GENOTYPE ~ "categorical")
  ) %>%
  modify_caption("**Gender fixed effects**")

table_gender_fixed

table_fixed_bl <- Data_labeled %>%
  filter(VISCODE2_padronizado == "bl") %>%
  tbl_summary(
    by = Stage,
    include = c(Education, Gender_MF, GENOTYPE),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(Education ~ "continuous",
                Gender_MF ~ "categorical",
                GENOTYPE ~ "categorical")
  ) %>%
  modify_caption("**Efeitos fixos nos estágios na baseline**")


table_fixed_bl



table_fixed_m24 <- Data_labeled %>%
  filter(VISCODE2_padronizado == "m24") %>%
  tbl_summary(
    by = Stage,
    include = c(Education, Gender_MF, GENOTYPE),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(Education ~ "continuous",
                Gender_MF ~ "categorical",
                GENOTYPE ~ "categorical")
  ) %>%
  modify_caption("**Efeitos fixos nos estágios aos 24 meses**")


table_fixed_m24

table_fixed_m48 <- Data_labeled %>%
  filter(VISCODE2_padronizado == "m48") %>%
  tbl_summary(
    by = Stage,
    include = c(Education, Gender_MF, GENOTYPE),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(Education ~ "continuous",
                Gender_MF ~ "categorical",
                GENOTYPE ~ "categorical")
  ) %>%
  modify_caption("**Efeitos fixos nos estágios aos 48 meses**")


table_fixed_m48


################# Importar os volumes para tabela ###########

tabela_final <- data.frame()

# Listar os diretórios PTID
ptid_dirs <- list.dirs(diretorio_principal, recursive = FALSE)

# Especificar as colunas desejadas

for (ptid in ptid_dirs) {
  momentos_dirs <- list.dirs(ptid, recursive = FALSE, full.names = TRUE)
  
  momentos_dirs <- momentos_dirs[basename(momentos_dirs) %in% c("bl", "m24", "m48")]
  
  for (momento in momentos_dirs) {
    arquivos <- list.files(momento, pattern = "\\.csv$", full.names = TRUE)
    
    if (length(arquivos) > 0) {  # Verificar se há ficheiros para processar
      for (arquivo in arquivos) {
        tryCatch({
          dados <- read_delim(arquivo, delim = ";", col_types = cols(.default = "c"))
          
          colunas_desejadas <- c("Hippocampus total volume cm3", 
                                 "Hippocampus right volume cm3", 
                                 "Hippocampus left volume cm3", 
                                 "Hippocampus volume asymmetry")
          colunas_presentes <- colunas_desejadas[colunas_desejadas %in% colnames(dados)]
          
          if (length(colunas_presentes) == 4) {  
            dados <- dados %>% select(all_of(colunas_presentes))
            
            dados <- dados %>% 
              mutate(PTID = basename(ptid), Momento = basename(momento))
            
            tabela_final <- bind_rows(tabela_final, dados)
            message(sprintf("Ficheiro lido com sucesso: %s", arquivo))
          } else {
            message(sprintf("Colunas não encontradas no ficheiro: %s", arquivo))
          }
          
        }, error = function(e) {
          message(sprintf("Erro ao ler ficheiro: %s\nDetalhes: %s", arquivo, e$message))
        })
      }
    } else {
      message(sprintf("Nenhum ficheiro encontrado no diretório: %s", momento))
    }
  }
}

if (nrow(tabela_final) > 0) {
  print(tabela_final)
  write.csv(tabela_final, file = "tabela_final.csv", row.names = FALSE)
  message("Tabela final salva com sucesso!")
} else {
  message("Nenhum dado foi carregado.")
}




################ Merge MRI - Dados Restantes #########################

names(tabela_final)[names(tabela_final) == "Momento"] <- "VISCODE2_padronizado"

Data_3times_MRI = merge(Data_3times_clean, tabela_final, by.x = c("PTID","VISCODE2_padronizado")) 


################ Análise dos dados #############

Data_3times_MRI$`Hippocampus total volume cm3` <- as.numeric(Data_3times_MRI$`Hippocampus total volume cm3`)

shapiro.test(Data_3times_MRI$`Hippocampus total volume cm3`) 



Data_3times_MRI$`Hippocampus right volume cm3` <- as.numeric(Data_3times_MRI$`Hippocampus right volume cm3`)

shapiro.test(Data_3times_MRI$`Hippocampus right volume cm3`) 



Data_3times_MRI$`Hippocampus left volume cm3` <- as.numeric(Data_3times_MRI$`Hippocampus left volume cm3`)

shapiro.test(Data_3times_MRI$`Hippocampus left volume cm3`) 



Data_3times_MRI$`Hippocampus volume asymmetry` <- as.numeric(Data_3times_MRI$`Hippocampus volume asymmetry`)

shapiro.test(Data_3times_MRI$`Hippocampus volume asymmetry`)



############# HTV ~ Momento ################

leveneTest(`Hippocampus total volume cm3` ~ VISCODE2_padronizado, data = Data_3times_MRI) #Pressuposto de homogeneidade de variância foi atendido.

HTV_afex <- aov_ez("PTID", "Hippocampus total volume cm3", 
                   Data_3times_MRI, within = "VISCODE2_padronizado")
summary(HTV_afex)

Posthoc_HTV = emmeans(HTV_afex, pairwise ~ VISCODE2_padronizado, adjust = "bonferroni")
summary(Posthoc_HTV)

ggplot(Data_3times_MRI, aes(x = VISCODE2_padronizado, y = `Hippocampus total volume cm3`)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribuição dos Volumes do Hipocampo",
    x = "Momento",
    y = "Volume Total do Hipocampo (cm³)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16)
  )


cor.test(Data_3times_MRI$subject_age, Data_3times_MRI$`Hippocampus total volume cm3`, 
         method = "spearman", exact = FALSE)

############# HRV ~ Momento ################

leveneTest(`Hippocampus right volume cm3` ~ VISCODE2_padronizado, data = Data_3times_MRI) #Pressuposto de homogeneidade de variância foi atendido.

HRV_afex <- aov_ez("PTID", "Hippocampus right volume cm3", 
                   Data_3times_MRI, within = "VISCODE2_padronizado")
summary(HRV_afex)

Posthoc_HRV = emmeans(HRV_afex, pairwise ~ VISCODE2_padronizado, adjust = "bonferroni")
summary(Posthoc_HRV)

ggplot(Data_3times_MRI, aes(x = VISCODE2_padronizado, y = `Hippocampus right volume cm3`)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribuição dos volumes do lado direito do Hipocampo",
    x = "Momento",
    y = "Volume do lado direito do Hipocampo (cm³)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16)
  )

cor.test(Data_3times_MRI$subject_age, Data_3times_MRI$`Hippocampus right volume cm3`, 
         method = "spearman", exact = FALSE)


############# HLV ~ Momento ################

leveneTest(`Hippocampus left volume cm3` ~ VISCODE2_padronizado, data = Data_3times_MRI) #Pressuposto de homogeneidade de variância foi atendido.

HLV_afex <- aov_ez("PTID", "Hippocampus left volume cm3", 
                   Data_3times_MRI, within = "VISCODE2_padronizado")
summary(HLV_afex)

Posthoc_HLV = emmeans(HLV_afex, pairwise ~ VISCODE2_padronizado, adjust = "bonferroni")
summary(Posthoc_HLV)


ggplot(Data_3times_MRI, aes(x = VISCODE2_padronizado, y = `Hippocampus left volume cm3`)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribuição dos volumes do lado esquerdo do Hipocampo",
    x = "Momento",
    y = "Volume do lado esquerdo do Hipocampo (cm³)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16)
  )

cor.test(Data_3times_MRI$subject_age, Data_3times_MRI$`Hippocampus left volume cm3`, 
         method = "spearman", exact = FALSE)

############# HVA ~ Momento ################

leveneTest(`Hippocampus volume asymmetry` ~ VISCODE2_padronizado, data = Data_3times_MRI) #Pressuposto de homogeneidade de variância foi atendido.

HVA_afex <- aov_ez("PTID", "Hippocampus volume asymmetry", 
                   Data_3times_MRI, within = "VISCODE2_padronizado")
summary(HVA_afex)


ggplot(Data_3times_MRI, aes(x = VISCODE2_padronizado, y = `Hippocampus volume asymmetry`)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribuição da assimetria do volume do Hipocampo",
    x = "Momento",
    y = "Assimetria do volume do Hipocampo (cm³)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16)
  )

cor.test(Data_3times_MRI$subject_age, Data_3times_MRI$`Hippocampus volume asymmetry`, 
         method = "spearman", exact = FALSE)

################ Tabelas Hipocampo ###############################

table_hipo_bl <- Data_3times_MRI %>%
  filter(VISCODE2_padronizado == "bl") %>%
  tbl_summary(
    by = Stage, # Adicionamos a separação por Gender
    include = c(`Hippocampus total volume cm3`, `Hippocampus left volume cm3`, `Hippocampus right volume cm3`, `Hippocampus volume asymmetry`),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(`Hippocampus total volume cm3` ~ "continuous",
                `Hippocampus left volume cm3` ~ "continuous",
                `Hippocampus right volume cm3` ~ "continuous",
                `Hippocampus volume asymmetry` ~ "continuous" )
  ) %>%
  modify_caption("**Média de medidas do hipocampo pelos diferentes estágios na baseline**")

table_hipo_bl


table_hipo_m24 <- Data_3times_MRI %>%
  filter(VISCODE2_padronizado == "m24") %>%
  tbl_summary(
    by = Stage, # Adicionamos a separação por Gender
    include = c(`Hippocampus total volume cm3`, `Hippocampus left volume cm3`, `Hippocampus right volume cm3`, `Hippocampus volume asymmetry`),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(`Hippocampus total volume cm3` ~ "continuous",
                `Hippocampus left volume cm3` ~ "continuous",
                `Hippocampus right volume cm3` ~ "continuous",
                `Hippocampus volume asymmetry` ~ "continuous" )
  ) %>%
  modify_caption("**Média de medidas do hipocampo pelos diferentes estágios aos 24 meses**")

table_hipo_m24



table_hipo_m48 <- Data_3times_MRI %>%
  filter(VISCODE2_padronizado == "m48") %>%
  tbl_summary(
    by = Stage, # Adicionamos a separação por Gender
    include = c(`Hippocampus total volume cm3`, `Hippocampus left volume cm3`, `Hippocampus right volume cm3`, `Hippocampus volume asymmetry`),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(`Hippocampus total volume cm3` ~ "continuous",
                `Hippocampus left volume cm3` ~ "continuous",
                `Hippocampus right volume cm3` ~ "continuous",
                `Hippocampus volume asymmetry` ~ "continuous" )
  ) %>%
  modify_caption("**Média de medidas do hipocampo pelos diferentes estágios aos 48 meses**")

table_hipo_m48



################   BRM Preparation   ##################


Data_3times_MRI$`Hippocampus total volume cm3_z` <- scale(Data_3times_MRI$`Hippocampus total volume cm3`)
Data_3times_MRI$`Hippocampus right volume cm3_z` <- scale(Data_3times_MRI$`Hippocampus right volume cm3`)
Data_3times_MRI$`Hippocampus left volume cm3_z` <- scale(Data_3times_MRI$`Hippocampus left volume cm3`)

colnames(Data_3times_MRI)[colnames(Data_3times_MRI) == "Hippocampus total volume cm3_z"] <- "Hippocampus_total_volume_z"
colnames(Data_3times_MRI)[colnames(Data_3times_MRI) == "Hippocampus right volume cm3_z"] <- "Hippocampus_right_volume_z"
colnames(Data_3times_MRI)[colnames(Data_3times_MRI) == "Hippocampus left volume cm3_z"] <- "Hippocampus_left_volume_z"

Data_3times_MRI$ABETA42_z <- scale(Data_3times_MRI$ABETA42)
Data_3times_MRI$PTAU_z <- scale(Data_3times_MRI$PTAU)
Data_3times_MRI$MMSCORE_z <- scale(Data_3times_MRI$MMSCORE)

cor(Data_3times_MRI[, c("ABETA42_z", "PTAU_z", "MMSCORE_z", "Hippocampus_total_volume_z", "Hippocampus_right_volume_z", "Hippocampus_left_volume_z")])

Data_3times_MRI$Stage <- relevel(Data_3times_MRI$Stage, ref = "CN")

Modelo_Stage_Hip <- brm(
  Stage ~ ABETA42_z + MMSCORE_z + PTAU_z +  Hippocampus_total_volume_z + (1 | RID),
  family = categorical(), 
  data = Data_3times_MRI,
  iter = 4000,
  warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  chains = 4,
  cores = 6,
  threads = 3
)

summary(Modelo_Stage_Hip)

pp_check(Modelo_Stage_Hip)


################# BRM MRI referencia ########################

Data_3times_MRI$Stage <- relevel(Data_3times_MRI$Stage, ref = "MCI")


Modelo_Stage_MCI <- brm(
  Stage ~ ABETA42_z + MMSCORE_z + PTAU_z + Hippocampus_total_volume_z + (1 | RID),
  family = categorical(), 
  data = Data_3times_MRI,
  iter = 4000,
  warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  chains = 4,
  cores = 6,
  threads = threading(3)
)

summary(Modelo_Stage_MCI)

pp_check(Modelo_Stage_MCI)


########################### Preparar para rodar ml

Data_MRI <- Data_3times_MRI  %>% select(PTID,VISCODE2_padronizado,ABETA42,PTAU,MMSCORE,
                                        GENOTYPE,Gender, subject_age, Education, CDRScore, MEMORYScore,
                                        `Hippocampus total volume cm3`, Stage)

Data_MRI <- Data_MRI %>%
  mutate(Gender = case_when(
    Gender == 1 ~ 0,
    Gender == 2 ~ 1,
    TRUE ~ Gender
  ))


Data_MRI$Gender <- factor(Data_MRI$Gender)


Data_MRI$GENOTYPE <- as.numeric(Data_MRI$GENOTYPE)

Data_MRI <- Data_MRI %>%
  mutate(GENOTYPE = case_when(
    GENOTYPE == 1 ~ 0,
    GENOTYPE == 2 ~ 1,
    GENOTYPE == 3 ~ 2,
    GENOTYPE == 4 ~ 3,
    GENOTYPE == 5 ~ 4,
    TRUE ~ GENOTYPE
  ))

Data_MRI$GENOTYPE <- factor(Data_MRI$GENOTYPE)

################################### SVM ######################################## 

set.seed(123)

unique_patients <- unique(Data_MRI$PTID)

train_patient_ids <- sample(unique_patients, size = floor(0.7 * length(unique_patients)), replace = FALSE)
test_patient_ids <- setdiff(unique_patients, train_patient_ids)

train_data <- Data_MRI[Data_MRI$PTID %in% train_patient_ids, ]
test_data <- Data_MRI[Data_MRI$PTID%in% test_patient_ids, ]

train_data$Stage <- factor(train_data$Stage, levels = c("CN", "MCI", "AD"), ordered = TRUE)
test_data$Stage <- factor(test_data$Stage, levels = c("CN", "MCI", "AD"), ordered = TRUE)


patient_stage <- train_data %>%
  group_by(PTID) %>%
  summarize(max_stage = max(Stage))

folds <- createFolds(patient_stage$max_stage, k = 10, list = TRUE)

group_folds <- lapply(folds, function(fold_ptids) {
  which(train_data$PTID %in% patient_stage$PTID[fold_ptids])
})

ctrl <- trainControl(
  method = "cv",
  index = group_folds,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,
  savePredictions = "final"
)

tune_grid <- tune_grid <- expand.grid(
  sigma = 0.016,
  C = 0.946
)

cl <- makeCluster(12)
registerDoParallel(cl)

svm_cv <- train(
  Stage ~ ABETA42 + MMSCORE + PTAU + `Hippocampus total volume cm3` + 
    CDRScore + MEMORYScore + Gender + GENOTYPE + Education + subject_age,
  data = train_data,
  method = "svmRadial",
  preProcess = c("center", "scale"),
  trControl = ctrl,
  tuneGrid = tune_grid,
  metric = "Kappa"
)

stopCluster(cl)

svm_cv$bestTune #Já retirei o best tune que foi sigma = 0.016 e o C = 0.946

predictions_svm <- predict(svm_cv, newdata = test_data)
confusionMatrix(predictions_svm, test_data$Stage)


importance_svm <- varImp(svm_cv, scale = TRUE)
print(importance_svm)


current_importance_df <- importance_svm$importance

new_variable_names <- c(
  "ABETA42" = "Aβ42",
  "PTAU" = "pTau181",
  "MMSCORE" = "MMSE",
  "subject_age" = "Age",
  "GENOTYPE" = "Genotype" 
)


for (i in seq_along(rownames(current_importance_df))) {
  old_name <- rownames(current_importance_df)[i]
  if (old_name %in% names(new_variable_names)) {
    rownames(current_importance_df)[i] <- new_variable_names[old_name]
  }
}


importance_svm$importance <- current_importance_df

plot(importance_svm,
     main = "Importance of Variables - SVM",
     layout = c(3, 1), # Layout horizontal
     par.settings = list(
       strip.background = list(col = "grey90"),
       par.strip.text = list(font = 2)
     )
)


############################## XGBoost ###############################

dados <- Data_MRI

# 2. Pré-processamento
dados <- dados %>%
  mutate(Stage = factor(Stage, levels = c("CN", "MCI", "AD"))) %>%
  na.omit()


dados_ungrouped <- ungroup(dados)

# One-hot encoding
dummy_model <- dummyVars(~ GENOTYPE + Gender, data = dados_ungrouped, fullRank = TRUE)
dados_encoded <- predict(dummy_model, newdata = dados_ungrouped)
dados_encoded <- as.data.frame(dados_encoded)

# Adicionar PTID e Stage explicitamente
dados_com_ptid <- cbind(dados_ungrouped %>% select(-VISCODE2_padronizado, -GENOTYPE, -Gender, -PTID, -Stage), dados_encoded, PTID = dados_ungrouped$PTID, Stage = dados_ungrouped$Stage)

numeric_cols <- names(dados_com_ptid)[sapply(dados_com_ptid, is.numeric) & !names(dados_com_ptid) %in% c("Stage", "PTID")]
dados_final_scaled <- dados_com_ptid %>%
  mutate(across(all_of(numeric_cols), scale))

# 3. Dividir os dados por paciente
pacientes_unicos <- unique(dados_final_scaled$PTID)

set.seed(123)

train_pacientes <- sample(pacientes_unicos, size = 0.7 * length(pacientes_unicos), replace = FALSE)
test_pacientes <- setdiff(pacientes_unicos, train_pacientes)

train_data <- dados_final_scaled %>% filter(PTID %in% train_pacientes) %>% select(-PTID)
test_data <- dados_final_scaled %>% filter(PTID %in% test_pacientes) %>% select(-PTID)

train_x <- train_data %>% select(-Stage)
train_y <- train_data$Stage
test_x <- test_data %>% select(-Stage)
test_y <- test_data$Stage

# Converter para xgb.DMatrix

train_x$`Hippocampus total volume cm3` <- as.numeric(train_x$`Hippocampus total volume cm3`)
test_x$`Hippocampus total volume cm3` <- as.numeric(test_x$`Hippocampus total volume cm3`)
train_x$CDRScore <- as.numeric(train_x$CDRScore)
test_x$CDRScore <- as.numeric(test_x$CDRScore)
train_x$MEMORYScore <- as.numeric(train_x$MEMORYScore)
test_x$MEMORYScore <- as.numeric(test_x$MEMORYScore)

dtrain_no_weights <- xgb.DMatrix(data = as.matrix(train_x), label = as.numeric(train_y) - 1)
dtest <- xgb.DMatrix(data = as.matrix(test_x), label = as.numeric(test_y) - 1)

# 5. Definir a grade de hiperparâmetros para o tuning usando seq()
param_grid_poucas_iteracoes <- expand.grid(
  nrounds = seq(1, 50, by = 1),      
  max_depth = seq(1,5,by = 1),    
  eta = seq(0.05,0.1, by = 0.01),         
  gamma = seq(0.001,0.005,by = 0.001),         
  colsample_bytree = seq(0.6,1, by= 0.1),
  min_child_weight = seq(0.1, 0.4, by = 0.1),   
  subsample = seq (0.5,0.9, by = 0.1)   
)

# 1 2 0.05 0.001 0.7 0.4 0.6 Melhores hiperparametros

# 6. Definir o método de controle para o tuning (validação cruzada)
train_control <- trainControl(
  method = "cv",
  number = 5,
  search = "grid"
)

gc()

hora_inicio <- Sys.time()
print(paste("Início do processo:", hora_inicio))

# 7. Tuning do modelo SEM weights
xgb_tune_no_weights <- train(
  Stage ~ .,
  data = train_data,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = param_grid_poucas_iteracoes,
  verbosity = 0
)

hora_fim <- Sys.time()
print(paste("Fim do processo:", hora_fim))

print(xgb_tune_no_weights)
best_params_no_weights <- xgb_tune_no_weights$bestTune
print(best_params_no_weights)

# 8. Treinar o modelo final
final_model_no_weights <- xgboost(
  params = list(objective = "multi:softprob",
                num_class = 3,
                eval_metric = "mlogloss",
                max_depth = best_params_no_weights$max_depth,
                eta = best_params_no_weights$eta,
                gamma = best_params_no_weights$gamma,
                colsample_bytree = best_params_no_weights$colsample_bytree,
                subsample = best_params_no_weights$subsample,
                min_child_weight = best_params_no_weights$min_child_weight),
  data = dtrain_no_weights,
  nrounds = best_params_no_weights$nrounds
)

# 9. Avaliar o modelo final
pred_prob_no_weights <- predict(final_model_no_weights, dtest)
pred_class_no_weights <- max.col(matrix(pred_prob_no_weights, ncol = 3)) - 1
pred_labels_no_weights <- factor(pred_class_no_weights, levels = 0:2, labels = levels(test_y))

print("Matriz de Confusão:")
print(confusionMatrix(pred_labels_no_weights, test_y))

importance_xgboost <- xgb.importance(model = final_model_no_weights)

# Visualizar a tabela
print(importance_xgboost)

importance_xgboost <- importance_xgboost %>%
  mutate(Feature = case_when(
    Feature == "MMSCORE" ~ "MMSE",
    Feature == "GENOTYPE.1" ~ "Genotype 2/4",
    TRUE ~ Feature
  ))

xgb.plot.importance(importance_xgboost, top_n = nrow(importance_xgboost), 
                    main = "Importance of Variables - XGBoost",
                    rel_to_first = TRUE, 
                    xlab = "Importance")

ggplot(importance_xgboost, aes(x = fct_reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "gray", width = 0.6) +
  coord_flip() +
  labs(title = "Importance of Variables - XGBoost", x = "Features", y = "Importance") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 12) 
  )
