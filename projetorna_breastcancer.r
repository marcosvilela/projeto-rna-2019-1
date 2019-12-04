######## PROJETO REDES NEURAIS - ALDO MONTEIRO, ARYELL DIAS, MARCOS VILELA E NATÁLIA MAYER ###############

library(zoo)
library(neuralnet)

######## PRÉ-PROCESSAMENTO DO DATASET ########
breastcancer <- read.csv("~/Documentos/code/redes-neurais/projeto-rna-2019-1/datasets/Breast Cancer Wisconsin Dataset.csv", header=FALSE)
names(breastcancer)<-c('id','clump_thickness','uniformity_of_cell_size','uniformity_of_cell_shape','marginal_adhesion',
                       'epithelial_cell_size','bare_nuclei','bland_chromation','normal_nucleoli','mitoses','class')

cancer_class<-breastcancer$class #guardanndo as classes possiveis de câncer de mama
breastcancer<-breastcancer[,2:10] #Tirando os Ids e as classes do dataset para poder limpá-lo
print(summary(breastcancer))

#Breast cancer -> Tratar missing substituindo ? pelo valor mais comum (Ou dropar as linhas)
breastcancer[breastcancer=="?"] <- NA
finalbreastcancer <- na.omit(breastcancer) #Dropa as linhas com NA (são só 16 em 699 linhas, então é OK dropá-las)
print(sum(is.na(finalbreastcancer))) #Debug para ver a quantidade de missings

#Transformando os dados em dataframes
breast_dataframe <-as.data.frame(lapply(finalbreastcancer, function(x) as.numeric(as.character(x))))

#Normalizando os dados
#Breast Cancer
breastc_max<-apply(breast_dataframe,2,max)
breastc_min<-apply(breast_dataframe,2,min)

scaled_breastcancer<-as.data.frame(scale(breast_dataframe, center=breastc_min, scale=breastc_max - breastc_min))
print(sum(is.na(scaled_breastcancer))) #Debug para ver a quantidade de missings

#Diabetes
diabetes_max<-apply(diabetes_dataframe, 2, max)
diabetes_min<-apply(diabetes_dataframe, 2, min)
scaled_diabetes<-as.data.frame(scale(diabetes_dataframe, center=diabetes_min, scale=diabetes_max - diabetes_min))

#Dividindo o datasets, 75% e 25%. O set.seed() é opcional, apenas para podermos checar o resultado depois

#set.seed(100)
train_index_breast<-sample(nrow(scaled_breastcancer), 0.75*nrow(scaled_breastcancer), replace=FALSE)
breast_trainset<-scaled_breastcancer[train_index_breast,]
breast_testset<-scaled_breastcancer[-train_index_breast,]

View(breast_trainset)

######## ACHANDO OS PARÂMETROS DO MODELO ########

####### EXPERIMENTOS PRINCIPAIS (SALVANDO OS RESULTADOS DAS MÉTRICAS (Acurácia e F1 Score) EM CSV) #######

####### TESTES ESTATÍSTICOS #########

#Shapiro-Wilk

#Kolmogorov-Smirnov

#Teste-F

#t-Student

#Wilcoxon

