######## PROJETO REDES NEURAIS - ALDO MONTEIRO, ARYELL DIAS, MARCOS VILELA E NATÁLIA MAYER ###############

######## PRÉ-PROCESSAMENTO DOS DATASETS ########
breastcancer <- read.csv("~/Documentos/code/redes-neurais/datasets/Breast Cancer Wisconsin Dataset.csv", header=FALSE)
names(breastcancer)<-c('id','clump_thickness','uniformity_of_cell_size','uniformity_of_cell_shape','marginal_adhesion',
                       'epithelial_cell_size','bare_nuclei','bland_chromation','normal_nucleoli','mitoses','class')
diabetes <- read.csv('~/Documentos/code/redes-neurais/datasets/Pima Indians Diabetes Dataset.csv', header=FALSE)
names(diabetes)<-c('n_pregnant','plasma_concentration','blood_pressure','skin_thickness','insulin','body_mass','diabetes_pedigree',
                   'age','class')

#print(summary(breastcancer))
#print(summary(diabetes))

#Breast cancer -> Tratar missing substituindo ? pelo valor mais comum (Ou dropar as linhas)
breastcancer[breastcancer=="?"] <- NA

#Diabetes -> Tratar missings "0" pela média não-nula da coluna
diabetes$skin_thickness[diabetes$skin_thickness==0] <- NA
diabetes$blood_pressure[diabetes$blood_pressure==0]<-NA
#diabetes$skin_thickness <- with(diabetes, impute(diabetes$skin_thickness, mean))
#print(diabetes$skin_thickness)

######## ACHANDO OS PARÂMETROS ########

####### EXPERIMENTOS PRINCIPAIS (SALVANDO OS RESULTADOS DAS MÉTRICAS (Acurácia e F1 Score) EM CSV) #######

####### TESTES ESTATÍSTICOS #########

#Shapiro-Wilk

#Kolmogorov-Smirnov

#Teste-F

#t-Student

#Wilcoxon

