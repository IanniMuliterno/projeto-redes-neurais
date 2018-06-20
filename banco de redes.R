library(data.table)
library(dplyr)


dt<-fread("./TRN")
str(dt)
dim(dt)


# definindo n paara tirar amostra de 10%
n <- round(389196/10)

# retirando amostra de 10%
dt2 <- dt[sample(1:389196,n),] 
#dim(dt2)

#extraindo classe1
classe1<- dt2[dt2$IND_BOM_1_1==1,]

#extraindo classe2
classe2<- dt2[dt2$IND_BOM_1_2==1,]
#dim(classe1)
#dim(classe2)


#definindo n para retirar 50% da classe 1
nc1_.5 <- round(length(classe1$INDEX)/2)
#definindo n para retirar 50% da classe 2
nc2_.5 <- round(length(classe2$INDEX)/2)

#definindo n para retirar 25% da classe 1
nc1_val <- round(nc1_.5/2)
#definindo n para retirar 25% da classe 2
nc2_val <- round(nc2_.5/2)


# retirando grupo de teste da classe 2 (sem repetição)
c2_teste <- classe2[sample(1:length(classe2$INDEX),nc2_val, replace = TRUE),]

# garantindo que os grupos de treino e validação da classe 2 não terão nenhuma instância que tem no teste 
compl_c2 <-anti_join(classe2,c2_teste)

# retirando grupo de treino das 2 classes sendo a classe 2 com repetição
c1_trein <- classe1[sample(1:length(classe1$INDEX),nc1_.5),]
c2_trein <- compl_c2[sample(1:length(compl_c2$INDEX),nc2_.5, replace = TRUE),]


# garantindo que a separação dos grupos da classe 1 não terão repetição
compl_c1 <- anti_join(classe1,c1_trein)

# retirando grupo de validação para as classes 1 e 2
c1_val <- compl_c1[1:round(length(compl_c1$INDEX)/2),]
c2_val <- compl_c2[sample(1:length(compl_c2$INDEX),nc2_val, replace = TRUE),]

# retirando grupo de teste da classe 1
c1_teste <- anti_join(compl_c1,c1_val)


#juntando grupo de treino da classe 1 e 2 e embaralhando ele
treino_total <- rbind(c1_trein,c2_trein)
treino_total <- treino_total[sample(1:length(treino_total$INDEX),length(treino_total$INDEX)),]

#juntando grupo de validação da classe 1 e 2 e embaralhando ele
val_total <- rbind(c1_val,c2_val)
val_total <- val_total[sample(1:length(val_total$INDEX),length(val_total$INDEX)),]

#juntando grupo de teste da classe 1 e 2 e embaralhando ele
teste_total <- rbind(c1_teste,c2_teste)
teste_total <- teste_total[sample(1:length(teste_total$INDEX),length(teste_total$INDEX)),]

#exporta os bancos de dados separadamente em formatacao .csv
write.csv(treino_total, "./treino_total.csv")
write.csv(val_total, "./val_total.csv")
write.csv(teste_total, "./teste_total.csv")
