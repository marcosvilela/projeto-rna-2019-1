######## PROJETO REDES NEURAIS - ALDO MONTEIRO, ARYELL DIAS, MARCOS VILELA E NATÁLIA MAYER ###############

library(zoo)
library(neuralnet)
library("mlbench")

######## PRÉ-PROCESSAMENTO DO DATASET ########

data(PimaIndiansDiabetes) #Importando o dataset da  biblioteca mlbench
diabetes<-PimaIndiansDiabetes
#View(diabetes)
diabetes_class<-diabetes$class #guardando as classes possíveis de diabetes

#Mudamos as 5 ocorrencias de glucose=0 (que caracteriza um missing) para um valor-limite se a linha for de um diabético
diabetes$glucose[350]<-180
diabetes$glucose[503]<-180
#Se não for, mudamos para a média da coluna
diabetes$glucose[diabetes$glucose==0]<-as.numeric(mean(diabetes$glucose))
#Todas as colunas seguintes podem ser substituídas pela média
diabetes$pressure[diabetes$pressure==0]<-as.numeric(mean(diabetes$pressure))
diabetes$triceps[diabetes$triceps==0] <- as.numeric(mean(diabetes$triceps))
diabetes$insulin[diabetes$insulin==0]<-as.numeric(mean(diabetes$insulin))
diabetes$mass[diabetes$mass==0]<-as.numeric(mean(diabetes$mass))
diabetes$pedigree[diabetes$pedigree==0]<-as.numeric(mean(diabetes$pedigree))
diabetes$age[diabetes$age==0]<-as.numeric(mode(diabetes$age)) #A idade é a que mais se repete

print(sum(is.na(diabetes))) #Debug para ver a quantidade de missings restante
finaldiabetes<-diabetes[,1:8]
View(finaldiabetes)

#Dividindo as classes de diabetes e não-diabetes
Diabetes<-diabetes$diabetes
Diabetes<-as.data.frame(Diabetes)
Diabetes<-with(Diabetes,data.frame(model.matrix(~Diabetes+0)))
#View(Diabetes)

#Transformando os dados em dataframes 
#diabetes_dataframe <- as.data.frame(diabetes)
#View(diabetes_dataframe)

#Normalizando os dados
diabetes_max<-apply(finaldiabetes, 2, max)
diabetes_min<-apply(finaldiabetes, 2, min)
scaled_diabetes<-as.data.frame(scale(finaldiabetes, center=diabetes_min, scale=diabetes_max - diabetes_min))
View(scaled_diabetes)

final_data<-as.data.frame(cbind(scaled_diabetes, Diabetes))
View(final_data)

#Dividindo o datasets, 75% e 25%. O set.seed() é opcional, apenas para podermos checar o resultado depois
#set.seed(200)
train_index_diabetes<-sample(nrow(final_data), 0.75*nrow(final_data), replace=FALSE)
diabetes_trainset<-as.data.frame(final_data[train_index_diabetes,])
diabetes_testset<-as.data.frame(final_data[-train_index_diabetes,])

View(diabetes_trainset)
print(sum(is.na(diabetes_trainset)))

######## ACHANDO OS PARÂMETROS DO MODELO ########
n<-names(final_data[1:8])
f<-as.formula(paste("Diabetesneg + Diabetespos ~", paste(n, collapse='+')))

net <- neuralnet(f, data=diabetes_trainset,hidden = 5,linear.output = FALSE)
plot(net)
predict_net_test <- compute(net,diabetes_testset[,1:8])
predict_result<-round(predict_net_test$net.result, digits = 0)
net.prediction = c("neg", "pos")[apply(predict_result, 1, which.max)]
predict.table = table(diabetes$diabetes[-train_index_diabetes], net.prediction)
print(predict.table)

####### EXPERIMENTOS PRINCIPAIS (SALVANDO OS RESULTADOS DAS MÉTRICAS (Acurácia e F1 Score) EM CSV) #######

####### TESTES ESTATÍSTICOS #########

#Shapiro-Wilk

#Kolmogorov-Smirnov

#Teste-F

#t-Student

#Wilcoxon