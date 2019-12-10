######## PROJETO REDES NEURAIS - ALDO MONTEIRO, ARYELL DIAS, MARCOS VILELA E NATÁLIA MAYER ###############

library(zoo)
library(neuralnet)
library("mlbench")


#Definindo funções para as métricas que iremos utilizar

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

data(PimaIndiansDiabetes) #Importando o dataset da  biblioteca mlbench
diabetes<-PimaIndiansDiabetes
View(diabetes)
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
#View(finaldiabetes)

#Dividindo as classes de diabetes e não-diabetes
Diabetes<-diabetes$diabetes
Diabetes<-as.data.frame(Diabetes)
Diabetes<-with(Diabetes,data.frame(model.matrix(~Diabetes+0)))
#View(Diabetes)

#Normalizando os dados
diabetes_max<-apply(finaldiabetes, 2, max)
diabetes_min<-apply(finaldiabetes, 2, min)
#Possibilidade; Testar mudando o tipo de normalização
scaled_diabetes<-as.data.frame(scale(finaldiabetes, center=diabetes_min, scale=diabetes_max - diabetes_min))
View(scaled_diabetes)

final_data<-as.data.frame(cbind(scaled_diabetes, Diabetes))
#View(final_data)

#Dividindo o datasets, 75% e 25%. O set.seed() é opcional, apenas para podermos checar o resultado depois
#set.seed(200)
train_index_diabetes<-sample(nrow(final_data), 0.75*nrow(final_data), replace=FALSE)
diabetes_trainset<-as.data.frame(final_data[train_index_diabetes,])
diabetes_testset<-as.data.frame(final_data[-train_index_diabetes,])

#View(diabetes_trainset)
print(sum(is.na(diabetes_trainset)))

######## ACHANDO OS PARÂMETROS DO MODELO ########
set.seed(100)
n<-names(final_data[1:8])
f<-as.formula(paste("Diabetesneg + Diabetespos ~", paste(n, collapse='+')))
#matrix_cells<-c()

#net <- neuralnet(f, data=diabetes_trainset,hidden = 5,linear.output = FALSE)
#plot(net)
#predict_net_test <- compute(net,diabetes_testset[,1:8])
#predict_result<-round(predict_net_test$net.result, digits = 0)
#net.prediction = c("neg", "pos")[apply(predict_result, 1, which.max)]
#predict.table = table(diabetes$diabetes[-train_index_diabetes], net.prediction)
#print(predict.table)
#fp<-predict.table[3]
#fn<-predict.table[2]
#vp<-predict.table[1]
#vn<- predict.table[4]

#learnrates_diab<-runif(5) #5 valores aleatórios uniformemente distribuídos
#accs_diab<-c()
#f1s_diab<-c()
#for (i in (1:length(learnrates_diab))){
#  net <- neuralnet(f, data=diabetes_trainset,hidden = 5,linear.output = FALSE)
#  #plot(net)
#  predict_net_test <- compute(net,diabetes_testset[,1:8])
#  print("linha 112")
# predict_result<-round(predict_net_test$net.result, digits = 0)
#  print("linha 114")
#  net.prediction = c("neg", "pos")[apply(predict_result, 1, which.max)]
#  print("linha 116")
#  predict.table = table(diabetes$diabetes[-train_index_diabetes], net.prediction)
#  print("linha 118")
#  print(predict.table)
#  print("linha 120")
#  fp<-predict.table[3]
#  fn<-predict.table[2]
#  vp<-predict.table[1]
#  vn<- predict.table[4]
#  
#  accs_diab[i]<-accuracy(vp,vn,fp,fn)
#  #f1s_diab[i]<-f1score(vp,fp,fn)
#}
#Achando o numero de neuronios na camada escondida
#max_acc_d<-max(accs_diab)
#max_acc_index_d<-which(accs_diab==max_acc_d)
#best_learning_rate_d<-learnrates_diab[max_acc_index_d]
#print(best_learning_rate_d) #0.5523


#
#hiddenlayers<-floor(runif(10, min=1, max=11))
#accs_hl_d<-c()
#f1s_hl_d<-c()

#for (i in (1:length(hiddenlayers))){
#  net <- neuralnet(f, data=diabetes_trainset,hidden = hiddenlayers[i],linear.output = FALSE, learningrate=0.5523)
#  #plot(net)
#  predict_net_test <- compute(net,diabetes_testset[,1:8])
#  predict_result<-round(predict_net_test$net.result, digits = 0)
#  net.prediction = c("neg", "pos")[apply(predict_result, 1, which.max)]
#  predict.table = table(diabetes$diabetes[-train_index_diabetes], net.prediction)
#  print(predict.table)
#  fp<-predict.table[3]
#  fn<-predict.table[2]
#  vp<-predict.table[1]
#  vn<- predict.table[4]
  
#  accs_hl_d[i]<-accuracy(vp,vn,fp,fn)
  #f1s_diab[i]<-f1score(vp,fp,fn)
#}

#Finding the best learning rates
#max_acc_d<-max(accs_hl_d)
#max_acc_index_d<-which(accs_hl_d==max_acc_d)
#best_hl_d<-hiddenlayers[max_acc_index_d]
#print(best_hl_d) #melhor resultado: 6


####### EXPERIMENTOS PRINCIPAIS (SALVANDO OS RESULTADOS DAS MÉTRICAS (Acurácia e F1 Score) EM CSV) #######
#Execuções múltiplas aqui
accs_final_d<-c()
f1_final_d<-c()
for(i in 1:30){
  net_final_d<-neuralnet(f, data=diabetes_trainset,hidden=4,linear.output=FALSE, learningrate=0.5523, stepmax = 1e7)
  if(i==30){
    plot(net_final_d)
  }
  predict_net_test_final <- compute(net_final_d,diabetes_testset[,1:9])
  predict_result_final<-round(predict_net_test_final$net.result, digits = 0)
  net.prediction_final = c("negative", "positive")[apply(predict_result_final, 1, which.max)]
  predict.table_final = table(diabetes$diabetes[-train_index_diabetes], net.prediction_final)
  print(predict.table_final)
  fp_final<-predict.table_final[3]
  fn_final<-predict.table_final[2]
  vp_final<-predict.table_final[1]
  vn_final<- predict.table_final[4]
  accs_final_d[i]<-accuracy(vp_final, vn_final, fp_final, fn_final)
  f1_final_d[i]<-f1score(vp_final, fp_final, fn_final)
}


####### TESTES ESTATÍSTICOS #########

#Shapiro-Wilk

#Kolmogorov-Smirnov

#Teste-F

#t-Student

#Wilcoxon