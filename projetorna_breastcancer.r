######## PROJETO REDES NEURAIS - ALDO MONTEIRO, ARYELL DIAS, MARCOS VILELA E NATÁLIA MAYER ###############

library("mlbench")
library(neuralnet)

####### FUNÇÕES ÚTEIS PARA CÁLCULO DAS MÉTRICAS ##########


accuracy<-function(vp,vn,fp,fn){
  return((vp+vn)/(vp+vn+fp+fn))
}

sensitivity<-function(vp, fn){
  return(vp/(vp+fn))
}

specificity<-function(vn,fp){
  return(vn/(vn+fp))
}

efficiency<-function(sens, spec){
  return((sens+spec)/2)
}

precision<-function(vp,fp){
  return((vp)/(vp+fp))
}

f1score<-function(vp, fn, fp){
  sens<-sensitivity(vp,fn)
  ppv<-precision(vp,fp)
  return (2*((sens*ppv)/(sens+ppv)))
}

phi<-function(vp,fp,vn,fn){
  return((vp*vn - fp*fn)/sqrt((vp+fp)*(vp+fn)*(vn+fp)*(vn+fn)))
}

######## PRÉ-PROCESSAMENTO DO DATASET ########
data(BreastCancer)
breastcancer<-BreastCancer
View(breastcancer)
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
View(scaled_breastcancer)
final_data<-as.data.frame(cbind(scaled_breastcancer,Cancer))
#View(final_data)

#Dividindo o datasets, 75% e 25%. O set.seed() é opcional, apenas para podermos checar o resultado depois

set.seed(50)
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
#learnrates<-runif(5) #5 valores aleatórios uniformemente distribuídos
#accs<-c()
#f1s<-c()
#for (i in (1:length(learnrates))){
#  print('For learning rate =')
#  net<-neuralnet(f,data=breast_trainset,hidden=5,linear.output = FALSE, learningrate = learnrates[i])
#  predict_net_test <- compute(net,breast_testset[,1:9])
#  predict_result<-round(predict_net_test$net.result, digits = 0)
#  net.prediction = c("benign", "malignant")[apply(predict_result, 1, which.max)]
#  predict.table = table(cleanedbreastcancer$Class[-train_index_breast], net.prediction)
#  print(predict.table)
#  fp<-predict.table[3]
#  fn<-predict.table[2]
#  vp<-predict.table[1]
#  vn<-predict.table[4]
#  
#  accs[i]<-accuracy(vp,vn,fp,fn)
#  f1s[i]<-f1score(vp,fp, fn)
#}

#Finding the best learning rates
#max_acc<-max(accs)
#max_acc_index<-which(accs==max_acc)
#best_learning_rate<-learnrates[max_acc_index]
#print(best_learning_rate) 
#Com seed=50, temos 0,02 como melhor learning rate

#Testando com número de neuronios na camada escondida
#hiddenlayers<-floor(runif(10, min=1, max=11))
#accs_hl<-c()
#f1s_hl<-c()
#for (i in (1:length(hiddenlayers))){
#  print('For learning rate =')
#  net<-neuralnet(f,data=breast_trainset,hidden=hiddenlayers[i],linear.output = FALSE, learningrate = 0.023)
#  predict_net_test <- compute(net,breast_testset[,1:9])
#  predict_result<-round(predict_net_test$net.result, digits = 0)
#  net.prediction = c("benign", "malignant")[apply(predict_result, 1, which.max)]
#  predict.table = table(cleanedbreastcancer$Class[-train_index_breast], net.prediction)
#  print(predict.table)
#  fp<-predict.table[3]
#  fn<-predict.table[2]
#  vp<-predict.table[1]
#  vn<-predict.table[4]
#  
#  accs_hl[i]<-accuracy(vp,vn,fp,fn)
#  f1s_hl[i]<-f1score(vp,fp, fn)
#}

#max_acc<-max(accs_hl)
#max_acc_index<-which(accs_hl==max_acc)
#best_hidden_layers<-hiddenlayers[max_acc_index]
#print(best_hidden_layers) 
#Com seed=50, 3 neuronios na camada escondida nos dão o melhor resultado



####### EXPERIMENTOS PRINCIPAIS (SALVANDO OS RESULTADOS DAS MÉTRICAS (Acurácia e F1 Score) EM UM ARRAY PARA OS TESTES) #######
accs_final<-c()
f1_final<-c()
for(i in 1:30){
  net_final<-neuralnet(f, data=breast_trainset,hidden=3,linear.output=FALSE, learningrate=0.023)
  if(i==30){
    plot(net_final)
  }
  predict_net_test_final <- compute(net_final,breast_testset[,1:9])
  predict_result_final<-round(predict_net_test_final$net.result, digits = 0)
  net.prediction_final = c("benign", "malignant")[apply(predict_result_final, 1, which.max)]
  predict.table_final = table(cleanedbreastcancer$Class[-train_index_breast], net.prediction_final)
  print(predict.table_final)
  fp_final<-predict.table_final[3]
  fn_final<-predict.table_final[2]
  vp_final<-predict.table_final[1]
  vn_final<- predict.table_final[4]
  accs_final[i]<-accuracy(vp_final, vn_final, fp_final, fn_final)
  f1_final[i]<-f1score(vp_final, fp_final, fn_final)
}


####### TESTES ESTATÍSTICOS #########

#Shapiro-Wilk
shapiro_accs<-shapiro.test(accs_final)
shapiro_f1<-shapiro.test(f1_final)
#Kolmogorov-Smirnov

#Teste-F

#t-Student

#Wilcoxon
