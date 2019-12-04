######## PROJETO REDES NEURAIS - ALDO MONTEIRO, ARYELL DIAS, MARCOS VILELA E NATÁLIA MAYER ###############

library(zoo)
library(neuralnet)

######## PRÉ-PROCESSAMENTO DO DATASET ########

diabetes <- read.csv('~/Documentos/code/redes-neurais/projeto-rna-2019-1/datasets/Pima Indians Diabetes Dataset.csv', header=FALSE)
names(diabetes)<-c('n_pregnant','plasma_concentration','blood_pressure','skin_thickness','insulin','body_mass','diabetes_pedigree',
                   'age','class')
View(diabetes)
diabetes_class<-diabetes$class #guardando as classes possíveis de diabetes
diabetes<-diabetes[,1:8]
print(summary(diabetes))

#Diabetes -> Tratar missings "0" pela média não-nula da coluna
#Os que não fazem sentido terem 0 são a primeira coluna e a última coluna. Todo o resto precisa ser substituido de alguma forma
#Na segunda coluna, taxa de glicose, quando a classe final for Diabético, substituir o valor por 180. Quando não for, pela média da coluna
diabetes$blood_pressure[diabetes$blood_pressure==0]<-NA
diabetes$skin_thickness[diabetes$skin_thickness==0] <- NA
diabetes$insulin[diabetes$insulin==0]<-NA
diabetes$body_mass[diabetes$body_mass==0]<-NA
diabetes$diabetes_pedigree[diabetes$diabetes_pedigree==0]<-NA
diabetes$age[diabetes$age==0]<-NA
finaldiabetes <- na.aggregate(diabetes) #Substitui os valores com missings pela média das colunas
print(sum(is.na(finaldiabetes))) #Debug para ver a quantidade de missings restante

#Transformando os dados em dataframes
diabetes_dataframe <- as.data.frame(lapply(finaldiabetes, function(x) as.numeric(x)))

#Normalizando os dados
diabetes_max<-apply(diabetes_dataframe, 2, max)
diabetes_min<-apply(diabetes_dataframe, 2, min)
scaled_diabetes<-as.data.frame(scale(diabetes_dataframe, center=diabetes_min, scale=diabetes_max - diabetes_min))

#Dividindo o datasets, 75% e 25%. O set.seed() é opcional, apenas para podermos checar o resultado depois
#set.seed(200)
train_index_diabetes<-sample(nrow(scaled_diabetes), 0.75*nrow(scaled_diabetes), replace=FALSE)
diabetes_trainset<-scaled_diabetes[train_index_breast,]
diabetes_testset<-scaled_diabetes[-train_index_breast,]

View(diabetes_trainset)

######## ACHANDO OS PARÂMETROS DO MODELO ########

####### EXPERIMENTOS PRINCIPAIS (SALVANDO OS RESULTADOS DAS MÉTRICAS (Acurácia e F1 Score) EM CSV) #######

####### TESTES ESTATÍSTICOS #########

#Shapiro-Wilk

#Kolmogorov-Smirnov

#Teste-F

#t-Student

#Wilcoxon