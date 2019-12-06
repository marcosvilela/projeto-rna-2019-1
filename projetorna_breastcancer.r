######## PROJETO REDES NEURAIS - ALDO MONTEIRO, ARYELL DIAS, MARCOS VILELA E NATÁLIA MAYER ###############

library("mlbench")
library(neuralnet)

######## PRÉ-PROCESSAMENTO DO DATASET ########
data(BreastCancer)
breastcancer<-BreastCancer
#View(breastcancer)
summary(breastcancer)

#Breast cancer -> Tratar missing substituindo ? pelo valor mais comum (Ou dropar as linhas)
cleanedbreastcancer <- na.omit(breastcancer) #Dropa as linhas com NA (são só 16 em 699 linhas, então é OK dropá-las)
print(sum(is.na(finalbreastcancer))) #Debug para ver a quantidade de missings
str(cleanedbreastcancer)
finalbreastcancer<-cleanedbreastcancer[,2:10]

Cancer<-cleanedbreastcancer$Class
Cancer<-as.data.frame(Cancer)
#0 -> benigno, 1 -> maligno
Cancer<-with(Cancer,data.frame(model.matrix(~Cancer + 0)))

#Transformando os dados em dataframes
breast_dataframe <-as.data.frame(lapply(finalbreastcancer, function(x) as.numeric(as.character(x))))
#View(breast_dataframe)

#Normalizando os dados
breastc_max<-apply(breast_dataframe,2,max)
breastc_min<-apply(breast_dataframe,2,min)

scaled_breastcancer<-as.data.frame(scale(breast_dataframe, center=breastc_min, scale=breastc_max - breastc_min))
print(sum(is.na(scaled_breastcancer))) #Debug para ver a quantidade de missings

final_data<-as.data.frame(cbind(scaled_breastcancer,Cancer))
#View(final_data)

#Dividindo o datasets, 75% e 25%. O set.seed() é opcional, apenas para podermos checar o resultado depois

#set.seed(100)
train_index_breast<-sample(nrow(final_data), 0.75*nrow(final_data), replace=FALSE)
breast_trainset<-as.data.frame(final_data[train_index_breast,])
breast_testset<-as.data.frame(final_data[-train_index_breast,])
#View(breast_testset)
#View(breast_trainset)

######## ACHANDO OS PARÂMETROS DO MODELO ########
n <- names(final_data[1:9])
f <- as.formula(paste("Cancerbenign + Cancermalignant ~ ", paste(n, collapse=' + ')))

#Modelo de MLP com os dados de treinamento. Vamos fazer os testes aqui!
#Testaremos 5 vezes com 5 valores de taxas de aprendizado
learnrates<-runif(5) #5 valores aleatórios uniformemente distribuídos
acc<-c()
print(learnrates)
print('For learning rate =')
print(i)
net <- neuralnet(f, data=breast_trainset,hidden = 5,linear.output = FALSE, learningrate = i)
plot(net)
predict_net_test <- compute(net,breast_testset[,1:9])
predict_result<-round(predict_net_test$net.result, digits = 0)
net.prediction = c("benign", "malignant")[apply(predict_result, 1, which.max)]
predict.table = table(cleanedbreastcancer$Class[-train_index_breast], net.prediction)
print(predict.table)
for(i in learnrates){ #Testando 5 vezes para valores de taxa de aprendizado aleatórios entre 0 e 1
  
}



####### EXPERIMENTOS PRINCIPAIS (SALVANDO OS RESULTADOS DAS MÉTRICAS (Acurácia e F1 Score) EM CSV) #######

####### TESTES ESTATÍSTICOS #########

#Shapiro-Wilk

#Kolmogorov-Smirnov

#Teste-F

#t-Student

#Wilcoxon

