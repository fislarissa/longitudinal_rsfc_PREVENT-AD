##########################################################################################################################

#'Differential Effects of Aging, Alzheimer's Pathology, and APOE4 on Longitudinal Posterior-Medial 
#Functional Connectivity and Episodic Memory'

#Analysis of PREVENT-AD data - longitudinal resting-state fMRI with cognitive and CSF markers
#Larissa Fischer - Multimodal Neuroimaging Lab, DZNE Magdeburg, 2024

#########################################################################################################################

#Import packages:
library(readxl)
library(writexl)
library(dplyr)

#########################################################################################################################
#set working directory:
setwd("/Users/your/path")

#load data
load("/.../data_subsample_A.RData")
load("/.../data_subsample_B.RData")

#remove later:
load("/Users/fischerl/Nextcloud/#Documents/MuMoNi me/#synage/ABBERANT FUNCTION PROJECTS/1. spatio-temp dynamics/1.1 rsfc longi Prevent-AD project/R_PhD_project_1_1_prevent_AD_rsfc/subcohorts_tables/data_subsample_A.RData") 
load("/Users/fischerl/Nextcloud/#Documents/MuMoNi me/#synage/ABBERANT FUNCTION PROJECTS/1. spatio-temp dynamics/1.1 rsfc longi Prevent-AD project/R_PhD_project_1_1_prevent_AD_rsfc/subcohorts_tables/data_subsample_B.RData")


#########################################################################################################################
#subsets and theme

#APOE cognition
data_subsample_B_cognition_APOE4 <- subset(data_subsample_B_cognition, APOE4_carrier == 1)
data_subsample_B_cognition_APOE4_bootstrap <- subset(data_subsample_B_cognition_APOE4, `RBANS_FU24>BL00` != 'NA')
data_subsample_B_cognition_APOE4_noncarriers <- subset(data_subsample_B_cognition, APOE4_carrier == 0)


#cognition
#subsample A
data_subsample_A_cognition <- data_subsample_A %>%
  filter(!(ID %in% c("sub-MTL0366", "sub-MTL0064"))) #possible MCI later
data_subsample_A_cognition <- data_subsample_A_cognition %>%
  mutate(time_BL_to_FU24 = ifelse(ID == "sub-MTL0384", 612, time_BL_to_FU24)) #3x follow-up after 3 months as cognition baseline
data_subsample_A_cognition <- data_subsample_A_cognition %>%
  mutate(time_BL_to_FU24 = ifelse(ID == "sub-MTL0390", 603, time_BL_to_FU24))
data_subsample_A_cognition <- data_subsample_A_cognition %>%
  mutate(time_BL_to_FU24 = ifelse(ID == "sub-MTL0405", 613, time_BL_to_FU24))
#subsample B
data_subsample_B_cognition <- data_subsample_B %>%
  filter(ID != "sub-MTL0297") #possible MCI later
data_subsample_B_cognition <- data_subsample_B_cognition %>%
  mutate(time_BL_to_FU24 = ifelse(ID == "sub-MTL0390", 602, time_BL_to_FU24)) #2x follow-up after 3 months as cognition baseline
data_subsample_B_cognition <- data_subsample_B_cognition %>%
  mutate(time_BL_to_FU24 = ifelse(ID == "sub-MTL0405", 612, time_BL_to_FU24))


plot_theme <- function() {
  theme(
    panel.border = element_blank(),
    axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )
}

#########################################################################################################################
# Demographics
#########################################################################################################################
describe(data_subsample_A$education_years)
describe(data_subsample_B$education_years)
#etc

#########################################################################################################################
# Normal aging - Subsample A
#########################################################################################################################

#FC decrease over time
################################################################################
#FU12 
#cluster 1
t.test(data_subsample_A$`fc_SFG_L_7_7-CG_R_7_4_FU12>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_SFG_L_7_7-CG_R_7_4_FU12>BL`,f = NA, mu = 0)
t.test(data_subsample_A$`fc_SFG_L_7_7-CG_L_7_4_FU12>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_SFG_L_7_7-CG_L_7_4_FU12>BL`,f = NA, mu = 0)
t.test(data_subsample_A$`fc_SFG_L_7_7-CG_L_7_1_FU12>BL_I`, mu = 0)
cohen.d(data_subsample_A$`fc_SFG_L_7_7-CG_L_7_1_FU12>BL_I`,f = NA, mu = 0)
#cluster 2
t.test(data_subsample_A$`fc_PhG_L_6_3-PCun_L_4_4_FU12>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_PhG_L_6_3-PCun_L_4_4_FU12>BL`,f = NA, mu = 0)
t.test(data_subsample_A$`fc_PhG_L_6_3-PCun_R_4_4_FU12>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_PhG_L_6_3-PCun_R_4_4_FU12>BL`,f = NA, mu = 0)
t.test(data_subsample_A$`fc_PhG_R_6_3-PCun_L_4_4_FU12>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_PhG_R_6_3-PCun_L_4_4_FU12>BL`,f = NA, mu = 0)
#FU24
#cluster 1
t.test(data_subsample_A$`fc_CG_R_7_4-CG_R_7_1_FU24>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_CG_R_7_4-CG_R_7_1_FU24>BL`,f = NA, mu = 0)
t.test(data_subsample_A$`fc_CG_L_7_4-CG_R_7_1_FU24>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_CG_L_7_4-CG_R_7_1_FU24>BL`,f = NA, mu = 0)
t.test(data_subsample_A$`fc_SFG_L_7_7-CG_R_7_1_FU24>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_SFG_L_7_7-CG_R_7_1_FU24>BL`,f = NA, mu = 0)
t.test(data_subsample_A$`fc_CG_R_7_4-PCun_R_4_1_FU24>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_CG_R_7_4-PCun_R_4_1_FU24>BL`,f = NA, mu = 0)
t.test(data_subsample_A$`fc_SFG_L_7_7-CG_L_7_1_FU24>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_SFG_L_7_7-CG_L_7_1_FU24>BL`,f = NA, mu = 0)
t.test(data_subsample_A$`fc_CG_R_7_4-PCun_R_4_4_FU24>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_CG_R_7_4-PCun_R_4_4_FU24>BL`,f = NA, mu = 0)
#cluster 2
t.test(data_subsample_A$`fc_Hipp_L_2_2-PCun_R_4_3_FU24>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_Hipp_L_2_2-PCun_R_4_3_FU24>BL`,f = NA, mu = 0)
t.test(data_subsample_A$`fc_PhG_R_6_3-PCun_R_4_3_FU24>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_PhG_R_6_3-PCun_R_4_3_FU24>BL`,f = NA, mu = 0)
t.test(data_subsample_A$`fc_Hipp_L_2_2-PCun_L_4_4_FU24>BL`, mu = 0)
cohen.d(data_subsample_A$`fc_Hipp_L_2_2-PCun_L_4_4_FU24>BL`,f = NA, mu = 0)

#95% CI for p value cluster
p <- 0.032 #change accordingly
M <- 10000  #permutations
z <- 1.96
SE <- sqrt((p * (1 - p)) / M)
CI_lower <- p - z * SE
CI_upper <- p + z * SE
CI_lower <- round(CI_lower, 3)
CI_upper <- round(CI_upper, 3)
cat("95% CI for p-value of cluster: [", CI_lower,",",CI_upper, "]\n")

#FC - age for PHC - PRC
################################################################################
model3_1 <- lm(`fc_PhG_R_6_3-PhG_L_6_2_age_FU12>BL` ~  age_years+ APOE4_carrier+ sex + education_years+time_BL_to_FU12, data = data_subsample_A)
summary(model3_1)
shapiro.test(rstandard(model3_1)) # test normal distribution of residuals of model
bptest(model3_1) # test heteroscedasticity
vif(model3_1) # test multicollinearity
tab_model(model3_1, show.se = TRUE, show.std = TRUE, show.stat = TRUE, file = "model3_1.doc")

plot3_1 <- ggplot(data_subsample_A,aes(y=`fc_PhG_R_6_3-PhG_L_6_2_age_FU12>BL`,x=age_years))+
  geom_point(size=2)+geom_smooth(method="lm", color = "#0054ff") +plot_theme()+
  xlab("Age in years")+ylab('Change in FC lateral PHC r - posterior PRC l over one year')
plot3_1

#FC - age for PHC - PCC I
################################################################################
model3_2 <- lm(`fc_PhG_R_6_6-CG_R_7_1_age_FU12>BL` ~  age_years+ APOE4_carrier+ sex + education_years+time_BL_to_FU12, data = data_subsample_A)
summary(model3_2) 
shapiro.test(rstandard(model3_2)) 
bptest(model3_2)
vif(model3_2) 
tab_model(model3_2, show.se = TRUE, show.std = TRUE, show.stat = TRUE, file = "model3_2.doc")

#FC - age for PHC - PCC II
################################################################################
model3_3 <- lm(`fc_PhG_R_6_3-CG_L_7_1_age_FU12>BL` ~  age_years+ APOE4_carrier+ sex + education_years+time_BL_to_FU12, data = data_subsample_A)
summary(model3_3)
shapiro.test(rstandard(model3_3)) 
bptest(model3_3) 
vif(model3_3) 
tab_model(model3_3, show.se = TRUE, show.std = TRUE, show.stat = TRUE, digits=3, file = "model3_3.doc")


# graph analysis: global efficiency
#########################################################################################################################
### decrease over time
################################################################################
# FU12
t.test(graph_decrease_3$globaleff_SFG_L_7_7_FU12_BL, mu = 0)
cohen.d(graph_decrease_3$globaleff_SFG_L_7_7_FU12_BL,f = NA, mu = 0)
t.test(graph_decrease_3$globaleff_SFG_R_7_7_FU12_BL, mu = 0)
cohen.d(graph_decrease_3$globaleff_SFG_R_7_7_FU12_BL,f = NA, mu = 0)
t.test(graph_decrease_3$globaleff_PhG_L_6_1_FU12_BL, mu = 0)
cohen.d(graph_decrease_3$globaleff_PhG_L_6_1_FU12_BL,f = NA, mu = 0)
# FU24
t.test(graph_decrease_3$globaleff_CG_R_7_4_FU24_BL, mu = 0)
cohen.d(graph_decrease_3$globaleff_CG_R_7_4_FU24_BL,f = NA, mu = 0)
t.test(graph_decrease_3$globaleff_SFG_L_7_7_FU24_BL, mu = 0)
cohen.d(graph_decrease_3$globaleff_SFG_L_7_7_FU24_BL,f = NA, mu = 0)
t.test(graph_decrease_3$globaleff_CG_L_7_4_FU24_BL, mu = 0)
cohen.d(graph_decrease_3$globaleff_CG_L_7_4_FU24_BL,f = NA, mu = 0)
t.test(graph_decrease_3$globaleff_CG_R_7_1_FU24_BL, mu = 0)
cohen.d(graph_decrease_3$globaleff_CG_R_7_1_FU24_BL,f = NA, mu = 0)
t.test(graph_decrease_3$globaleff_SFG_R_7_7_FU24_BL, mu = 0)
cohen.d(graph_decrease_3$globaleff_SFG_R_7_7_FU24_BL,f = NA, mu = 0)
t.test(graph_decrease_3$globaleff_PCun_R_4_4_FU24_BL, mu = 0)
cohen.d(graph_decrease_3$globaleff_PCun_R_4_4_FU24_BL,f = NA, mu = 0)
t.test(graph_decrease_3$globaleff_PCun_L_4_4_FU24_BL, mu = 0)
cohen.d(graph_decrease_3$globaleff_PCun_L_4_4_FU24_BL,f = NA, mu = 0)
t.test(graph_decrease_3$globaleff_CG_L_7_1_FU24_BL, mu = 0)
cohen.d(graph_decrease_3$globaleff_CG_L_7_1_FU24_BL,f = NA, mu = 0)

### global efficiency - age
################################################################################
model3_1_graph <- lm(`globaleff_age_PhG_R_6_2_FU12_BL` ~  age_years + APOE4_carrier +sex + education_years+ time_BL_to_FU12, data = data_subsample_A)
summary(model3_1_graph)
shapiro.test(rstandard(model3_1_graph)) 
bptest(model3_1_graph) 
vif(model3_1_graph) 
tab_model(model3_1_graph, show.se = TRUE, show.std = TRUE, show.stat = TRUE, file = "model3_1_graph.doc")

model3_2_graph <- lm(`globaleff_age_PhG_L_6_4_FU12_BL` ~  age_years + APOE4_carrier +sex + education_years+ time_BL_to_FU12, data = data_subsample_A)
summary(model3_2_graph) 
shapiro.test(rstandard(model3_2_graph)) 
bptest(model3_2_graph) 
vif(model3_2_graph) 
tab_model(model3_2_graph, show.se = TRUE, show.std = TRUE, show.stat = TRUE, file = "model3_2_graph.doc")

model3_3_graph <- lm(`globaleff_age_PhG_L_6_2_FU12_BL` ~  age_years + APOE4_carrier +sex + education_years+ time_BL_to_FU12, data = data_subsample_A)
summary(model3_3_graph) 
shapiro.test(rstandard(model3_3_graph)) 
bptest(model3_3_graph) 
vif(model3_3_graph) 
tab_model(model3_3_graph, show.se = TRUE, show.std = TRUE, show.stat = TRUE, file = "model3_3_graph.doc")

model3_4_graph <- lm(`globaleff_age_PhG_L_6_4_FU12_BL` ~  age_years + APOE4_carrier +sex + education_years+ time_BL_to_FU12, data = data_subsample_A)
summary(model3_4_graph) 
shapiro.test(rstandard(model3_4_graph)) 
bptest(model3_4_graph) 
vif(model3_4_graph) 
tab_model(model3_4_graph, show.se = TRUE, show.std = TRUE, show.stat = TRUE, file = "model3_4_graph.doc")



# COGNITION - episodic memory
#########################################################################################################################
#RBANS_FU24>BL00 - fc_BL
model3_4 <- lm(`RBANS_FU24>BL00` ~ `fc_CG_R_7_4-CG_R_7_1_BL` + age_years + APOE4_carrier+ sex + education_years +time_BL_to_FU24, data = data_subsample_A_cognition)
summary(model3_4) 
shapiro.test(rstandard(model3_4)) 
bptest(model3_4)
vif(model3_4)   #`fc_CG_R_7_4-CG_R_7_1_BL`*APOE4_carrier > 3
tab_model(model3_4, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model3_4.doc")

plot3_4 <- ggplot(data_subsample_A_cognition,aes(x=`fc_CG_R_7_4-CG_R_7_1_BL`,y=`RBANS_FU24>BL00`, color = APOE4_carrier))+
  geom_point(size=2)+geom_smooth(method="lm")+plot_theme()+
  xlab("FC at baseline RSC r - PCC r")+ylab("Change in RBANS delayed memory index score")+
  scale_color_manual(labels = c("Non-Carrier", "Carrier"), values = c("#0072B2", "#D55E00")) + labs(color = "APOE4") 
plot3_4

# RBANS_FU24>BL00 - fc_FU24>BL.
#basic model
model3_5 <- lm(`RBANS_FU24>BL00` ~ `fc_CG_R_7_4-CG_R_7_1_FU24>BL`+age_years_round + sex + education_years+time_BL_to_FU24, data = data_subsample_A_cognition)
summary(model3_5)
shapiro.test(rstandard(model3_5))
bptest(model3_5)
vif(model3_5) 
tab_model(model3_5, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model3_5.doc")

#extended model
model3_6 <- lm(`RBANS_FU24>BL00` ~ `fc_CG_R_7_4-CG_R_7_1_FU24>BL`+age_years_round  + APOE4_carrier+ sex + education_years+time_BL_to_FU24+ `fc_CG_R_7_4-CG_R_7_1_FU24>BL`*APOE4_carrier , data = data_subsample_A_cognition)
summary(model3_6)
shapiro.test(rstandard(model3_6)) 
bptest(model3_6) 
vif(model3_6)   #`fc_CG_R_7_4-CG_R_7_1_FU24>BL`*age > 3
tab_model(model3_6, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model3_6.doc")


# COGNITION - attention
#########################################################################################################################
#RBANS_FU24>BL00 - fc_BL
model3_7 <- lm(`RBANS_FU24>BL00_attention` ~ `fc_CG_R_7_4-CG_R_7_1_BL` + age_years + APOE4_carrier+ sex + education_years +time_BL_to_FU24 + `fc_CG_R_7_4-CG_R_7_1_BL`*APOE4_carrier , data = data_subsample_A_cognition)
summary(model3_7) 
shapiro.test(rstandard(model3_7)) 
bptest(model3_7)
vif(model3_7)   #`fc_CG_R_7_4-CG_R_7_1_BL`*APOE4_carrier > 3
tab_model(model3_7, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model3_7.doc")


# RBANS_FU24>BL00 - fc_FU24>BL.
#basic model
model3_8 <- lm(`RBANS_FU24>BL00_attention` ~ `fc_CG_R_7_4-CG_R_7_1_FU24>BL` +age_years_round  +  sex + education_years+time_BL_to_FU24, data = data_analysis_3_cognition)
summary(model3_8)
shapiro.test(rstandard(model3_8)) 
bptest(model3_8) 
vif(model3_8) 
tab_model(model3_8, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model3_8.doc")

#extended model
model3_9 <- lm(`RBANS_FU24>BL00_attention` ~ `fc_CG_R_7_4-CG_R_7_1_FU24>BL` +age_years_round  + APOE4_carrier+ sex + education_years+time_BL_to_FU24+ `fc_CG_R_7_4-CG_R_7_1_FU24>BL`*APOE4_carrier, data = data_analysis_3_cognition)
summary(model3_9)
shapiro.test(rstandard(model3_9))
bptest(model3_9) 
vif(model3_9)   #`fc_CG_R_7_4-CG_R_7_1_FU24>BL`*age > 3
tab_model(model3_9, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model3_9.doc")



#########################################################################################################################
# AD pathology - Subsample B
#########################################################################################################################

#FC - ratio baseline for anterior hippocampus - superior precuneus
################################################################################
model2_1 <- lm(`fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL` ~ ratio_BL + age_years + APOE4_carrier+ sex + education_years +time_BL_to_FU24, data = data_subsample_B)
summary(model2_1)
shapiro.test(rstandard(model2_1))
bptest(model2_1) 
vif(model2_1)
tab_model(model2_1, df.method = "satterthwaite",show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_1.doc")

plot2_1 <- ggplot(data_subsample_B,aes(y=`fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL`,x=ratio_BL))+
  geom_point(size=2)+geom_smooth(method="lm", color = "#c21759ff")+plot_theme()+
  xlab("p-tau/Aβ ratio at baseline")+ylab ('Change in FC anterior HC r - superior PCun r over two years')
plot2_1

#FC - ratio baseline for parahippocampal cortex - superior precuneus
################################################################################
model2_2 <- lm(`fc_PhG_R_6_6-PCun_R_4_2_FU24>BL` ~ ratio_BL + age_years + APOE4_carrier+ sex + education_years +time_BL_to_FU24, data = data_subsample_B)
summary(model2_2) 
shapiro.test(rstandard(model2_2))
bptest(model2_2) 
vif(model2_2)
tab_model(model2_2, df.method = "satterthwaite",show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_2.doc")

plot2_2 <- ggplot(data_subsample_B,aes(y=`fc_PhG_R_6_6-PCun_R_4_2_FU24>BL`,x=ratio_BL))+
  geom_point(size=2)+geom_smooth(method="lm", color = "#c21759ff")+plot_theme()+
  xlab("p-tau/Aβ1-42 ratio at baseline")+ylab ('Change in FC strength right PHC - right superior PCun over two years')
plot2_2


# COGNITION - episodic memory
#########################################################################################################################
# RBANS_FU24>BL00 - fc BL 
model2_5 <- lm(`RBANS_FU24>BL00` ~ `fc_Hipp_R_2_1-PCun_R_4_2_BL` +ratio_BL + age_years + APOE4_carrier+ sex + education_years +time_BL_to_FU24 + `fc_Hipp_R_2_1-PCun_R_4_2_BL`*APOE4_carrier, data = data_subsample_B_cognition)
summary(model2_5)
shapiro.test(rstandard(model2_5)) 
bptest(model2_5)
vif(model2_5) #`fc_Hipp_R_2_1-PCun_R_4_2_BL`*ratio_BL vif > 5
tab_model(model2_5, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_5.doc") 

plot2_5 <- ggplot(data_subsample_B_cognition,aes(x=`fc_Hipp_R_2_1-PCun_R_4_2_BL`,y=`RBANS_FU24>BL00`, color = APOE4_carrier))+
  geom_point(size=2)+geom_smooth(method="lm")+plot_theme()+
  xlab("FC at baseline anterior HC r - superior PCun r")+ylab("Change in RBANS delayed memory index score")+
  scale_color_manual(labels = c("Non-Carrier", "Carrier"), values = c("#0072B2", "#D55E00")) + labs(color = "APOE4")
plot2_5

#APOE4 carriers
model2_5_1 <- lm(`RBANS_FU24>BL00` ~ `fc_Hipp_R_2_1-PCun_R_4_2_BL` +ratio_BL + age_years + sex + education_years +time_BL_to_FU24, data = data_subsample_B_cognition_APOE4)
summary(model2_5_1)
shapiro.test(rstandard(model2_5_1))
bptest(model2_5_1)
vif(model2_5_1)
tab_model(model2_5_1,  digits=3,show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_5_1.doc") 
#bootstapping
#no missing data for RBANS allowed
set.seed(12345)
model_b <- lm(data =  data_subsample_B_cognition_APOE4_bootstrap,`RBANS_FU24>BL00` ~ `fc_Hipp_R_2_1-PCun_R_4_2_BL`+ ratio_BL + age_years_round  + sex + education_years+time_BL_to_FU24)
fit_b <- Boot(model_b, R = 10000)
summary(fit_b)
confint(fit_b, level = .95)
hist(fit_b) 
#APOE4 non-carriers
model2_5_2 <- lm(`RBANS_FU24>BL00` ~ `fc_Hipp_R_2_1-PCun_R_4_2_BL` +ratio_BL + age_years + sex + education_years +time_BL_to_FU24, data = data_subsample_B_cognition_APOE4_noncarriers)
summary(model2_5_2) #no effect in non-APOE4
shapiro.test(rstandard(model2_5_2)) # normal distribution of residuals
bptest(model2_5_2)
vif(model2_5_2)
tab_model(model2_5_2, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_5_2.doc") 


# RBANS_FU24>BL00 - fc_FU24>BL.
#basic model
model2_6 <- lm(`RBANS_FU24>BL00` ~ `fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL`+ age_years_round + sex + education_years  +time_BL_to_FU24, data = data_subsample_B_cognition)
summary(model2_6)
shapiro.test(rstandard(model2_6)) 
bptest(model2_6)
vif(model2_6)
tab_model(model2_6, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_6.doc")

#extended model
model2_7 <- lm(`RBANS_FU24>BL00` ~ `fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL`+ ratio_BL+ age_years_round+ APOE4_carrier + sex + education_years  +time_BL_to_FU24+ `fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL`*APOE4_carrier , data = data_subsample_B_cognition)
summary(model2_7)
shapiro.test(rstandard(model2_7)) 
bptest(model2_7)
vif(model2_7) #`fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL`*ratio_BL > 3
tab_model(model2_7, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_7.doc")


# COGNITION - attention
#########################################################################################################################
#RBANS_FU24>BL00 - fc_BL
model2_8 <- lm(`RBANS_FU24>BL00_attention` ~ `fc_Hipp_R_2_1-PCun_R_4_2_BL` +ratio_BL + age_years + APOE4_carrier+ sex + education_years +time_BL_to_FU24 + `fc_Hipp_R_2_1-PCun_R_4_2_BL`*APOE4_carrier, data = data_subsample_B_cognition)
summary(model2_8)
shapiro.test(rstandard(model2_8))
bptest(model2_8)
vif(model2_8)
tab_model(model2_8, df.method = "satterthwaite",show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_8.doc")

plot2_8 <- ggplot(data_subsample_B_cognition,aes(x=`age_years_round`,y=`RBANS_FU24>BL00_attention`, color = APOE4_carrier))+
  geom_point()+geom_smooth(method="lm")
plot2_8

#RBANS_FU24>BL00 - fc_FU24>BL
#basic model
model2_9 <- lm(`RBANS_FU24>BL00_attention` ~ `fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL`+ age_years_round + sex + education_years  +time_BL_to_FU24, data = data_subsample_B_cognition)
summary(model2_9)
shapiro.test(rstandard(model2_9))
bptest(model2_9)
vif(model2_9)
tab_model(model2_9, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_9.doc")

#extended model
model2_10 <- lm(`RBANS_FU24>BL00_attention` ~ `fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL`+ ratio_BL+ age_years_round+ APOE4_carrier + sex + education_years  +time_BL_to_FU24+ `fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL`*APOE4_carrier, data = data_subsample_B_cognition)
summary(model2_10)
shapiro.test(rstandard(model2_10))
bptest(model2_10)
vif(model2_10)  #`fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL`*ratio_BL > 3
tab_model(model2_10, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_10.doc")




#########################################################################################################################
# control analyses hippocampal volume
#########################################################################################################################

#Is struct change correlated with pathology in subsample B?
cor.test(data_subsample_B$ratio_BL, data_subsample_B$HC_volume_BL_to_FU24)

#HC struct change explained by pathology?
model_HC <- lm(HC_volume_BL_to_FU24 ~ ratio_BL + age_years + APOE4_carrier+ sex + education_years +time_BL_to_FU24, data = data_subsample_B)
summary(model_HC) 
tab_model(model_HC, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model_HC.doc")

#more atrophy in APOE4?
t.test(Subjects_2e_wide$HC_BL_to_FU24 ~ Subjects_2e_wide$APOE4_carrier)
t.test(Subjects_3_wide$HC_BL_to_FU24 ~ Subjects_3_wide$APOE4_carrier)

#subsample A - age effect
model3_1_HC <- lm(`fc_PhG_R_6_3-PhG_L_6_2_age_FU12>BL` ~  age_years + APOE4_carrier + sex + education_years+time_BL_to_FU12+HC_volume_BL_to_FU12, data = data_subsample_A)
summary(model3_1_HC) 
tab_model(model3_1_HC, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model3_1_HC.doc")

model3_2_HC <- lm(`fc_PhG_R_6_6-CG_R_7_1_age_FU12>BL` ~  age_years + APOE4_carrier + sex + education_years+time_BL_to_FU12+HC_volume_BL_to_FU12, data = data_subsample_A)
summary(model3_2_HC)
tab_model(model3_2_HC, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model3_2_HC.doc")

model3_3_HC <- lm(`fc_PhG_R_6_3-CG_L_7_1_age_FU12>BL` ~  age_years + APOE4_carrier + sex + education_years+time_BL_to_FU12+HC_volume_BL_to_FU12, data = data_subsample_A)
summary(model3_3_HC)
tab_model(model3_3_HC, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model3_3_HC.doc")


#subsample B - effect ratio 
model2_1_HC <- lm(`fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL` ~ ratio_BL +age_years + APOE4_carrier + sex + education_years+time_BL_to_FU24+HC_volume_BL_to_FU24, data = data_subsample_B)
summary(model2_1_HC)
tab_model(model2_1_HC, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_1_HC.doc")

model2_2_HC <- lm(`fc_PhG_R_6_6-PCun_R_4_2_FU24>BL` ~ ratio_BL +age_years + APOE4_carrier + sex + education_years+time_BL_to_FU24 +HC_volume_BL_to_FU24, data = data_subsample_B)
summary(model2_2_HC)
tab_model(model2_2_HC, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_2_HC.doc")

#COGNITION
################################################################################
#change in cognition explained by HC atrophy? - volume instead of FC in subsample A
model3_1_cog_HC <- lm(`RBANS_FU24>BL00` ~ age_years_round + APOE4_carrier + sex + education_years +time_BL_to_FU24+HC_volume_BL_to_FU24+ HC_volume_BL_to_FU24*APOE4_carrier, data = data_subsample_A_cognition)
summary(model3_1_cog_HC)
tab_model(model3_1_cog_HC, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model3_1_cog_HC.doc")

#subsample A
# RBANS_FU24>BL00 - fc BL 
model3_4_cog_HC <- lm(`RBANS_FU24>BL00` ~ `fc_CG_R_7_4-CG_R_7_1_BL` +age_years_round+ APOE4_carrier + sex + education_years +time_BL_to_FU24+HC_volume_BL_to_FU24, data = data_subsample_A_cognition)
summary(model3_4_cog_HC)
tab_model(model3_4_cog_HC, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model3_4_cog_HC.doc")

# RBANS_FU24>BL00 - fc FU24>BL 
model3_6_cog_HC <- lm(`RBANS_FU24>BL00` ~ `fc_CG_R_7_4-CG_R_7_1_FU24>BL`+age_years_round + APOE4_carrier + sex + education_years +time_BL_to_FU24+ `fc_CG_R_7_4-CG_R_7_1_FU24>BL`*APOE4_carrier+HC_volume_BL_to_FU24, data = data_subsample_A_cognition)
summary(model3_6_cog_HC)
tab_model(model3_6_cog_HC, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model3_5_cog_HC.doc")


#subsample B
# RBANS_FU24>BL00 - fc BL 
model2_5_cog_HC <- lm(`RBANS_FU24>BL00` ~ `fc_Hipp_R_2_1-PCun_R_4_2_BL` +ratio_BL + age_years + APOE4_carrier+ sex + education_years +time_BL_to_FU24 + `fc_Hipp_R_2_1-PCun_R_4_2_BL`*APOE4_carrier+HC_volume_BL_to_FU24, data = data_subsample_B_cognition)
summary(model2_5_cog_HC)
tab_model(model2_5_cog_HC,show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_5_cog_HC.doc")
# RBANS_FU24>BL00 - fc FU24>BL 
model2_7_cog_HC <- lm(`RBANS_FU24>BL00` ~ `fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL` + ratio_BL +age_years_round + APOE4_carrier + sex + education_years +time_BL_to_FU24+ `fc_Hipp_R_2_1-PCun_R_4_2_FU24>BL`*APOE4_carrier+HC_volume_BL_to_FU24, data = data_subsample_B_cognition)
summary(model2_7_cog_HC)
tab_model(model2_7_cog_HC, show.stat = TRUE, show.se = TRUE, show.std = TRUE, file = "model2_6_cog_HC.doc")

