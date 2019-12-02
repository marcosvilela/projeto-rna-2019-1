######## PROJETO REDES NEURAIS - ALDO MONTEIRO, ARYELL DIAS, MARCOS VILELA E NATÁLIA MAYER ###############

library(zoo)
library(neuralnet)

######## PRÉ-PROCESSAMENTO DOS DATASETS ########
breastcancer <- read.csv("./datasets/Breast Cancer Wisconsin Dataset.csv", header=FALSE)
names(breastcancer)<-c('id','clump_thickness','uniformity_of_cell_size','uniformity_of_cell_shape','marginal_adhesion',
                       'epithelial_cell_size','bare_nuclei','bland_chromation','normal_nucleoli','mitoses','class')
diabetes <- read.csv('./datasets/Pima Indians Diabetes Dataset.csv', header=FALSE)
names(diabetes)<-c('n_pregnant','plasma_concentration','blood_pressure','skin_thickness','insulin','body_mass','diabetes_pedigree',
                   'age','class')

#print(summary(breastcancer))
#print(summary(diabetes))

#Breast cancer -> Tratar missing substituindo ? pelo valor mais comum (Ou dropar as linhas)
breastcancer[breastcancer=="?"] <- NA
finalbreastcancer <- na.omit(breastcancer) #Dropa as linhas com NA (são só 16 em 699 linhas, então é OK dropá-las)
print(sum(is.na(finalbreastcancer))) #Debug para ver a quantidade de missings

#Diabetes -> Tratar missings "0" pela média não-nula da coluna
diabetes$skin_thickness[diabetes$skin_thickness==0] <- NA
diabetes$blood_pressure[diabetes$blood_pressure==0]<-NA
finaldiabetes <- na.aggregate(diabetes) #Substitui os valores com missings pela média das colunas
print(sum(is.na(finaldiabetes))) #Debug para ver a quantidade de missings

#Dividindo os dois datasets, 75% e 25%. O set.seed() é opcional, apenas para podermos checar o resultado depois

#set.seed(100)
train_index_breast<-sample(nrow(finalbreastcancer), 0.75*nrow(finalbreastcancer), replace=FALSE)
breast_trainset<-finalbreastcancer[train_index_breast,]
breast_testset<-finalbreastcancer[-train_index_breast,]

#set.seed(200)
train_index_diabetes<-sample(nrow(finaldiabetes), 0.75*nrow(finaldiabetes), replace=FALSE)
diabetes_trainset<-finaldiabetes[train_index_breast,]
diabetes_testset<-finaldiabetes[-train_index_breast,]

#Normalizando os dados

######## ACHANDO OS PARÂMETROS DO MODELO ########


####### EXPERIMENTOS PRINCIPAIS (SALVANDO OS RESULTADOS DAS MÉTRICAS (Acurácia e F1 Score) EM CSV) #######

####### TESTES ESTATÍSTICOS #########

#Shapiro-Wilk

#Kolmogorov-Smirnov

#Teste-F

#t-Student

#Wilcoxon

