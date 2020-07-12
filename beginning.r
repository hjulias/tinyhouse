#Organizando dados de "houses_to_rent_v2"
andares <- houses_to_rent_v2$floor 
cidadesv2 <- houses_to_rent_v2$city 
area_casa <- houses_to_rent_v2$area 

#Verificando a existência de inconsistências e corrigindo-as 
andares[andares== "-"] <- 0 #Substitui os valores - na coluna "floor" por 0. Demonstra que nenhum andar da casa fora construído até o momento. 

summary(houses_to_rent_v2[,])
        
#Criando gráfico de barras para ilustrar a quantidade de casas por cidade
barplot(table(cidadesv2), xlab = "Cidades", ylab = "N° de Casas", col = c("cyan3" , "cyan1", "cyan2", "cyan4", "blue"), main = "Quantidade de casas por cidade", ylim = c(0,6000))  
text(0.7, 900, table(cidadesv2[cidadesv2 == "Belo Horizonte"])) 
text(1.9, 600, table(cidadesv2[cidadesv2 == "Campinas"])) 
text(3.1, 900, table(cidadesv2[cidadesv2 == "Porto Alegre"])) 
text(4.3, 1300, table(cidadesv2[cidadesv2 == "Rio de Janeiro"])) 
text(5.5, 5000, table(cidadesv2[cidadesv2 == "São Paulo"]))

#Atribuindo à variável SP os dados que se relacionam com a cidade de São Paulo, a visualização e escrita do código é simplificada. 
SP <- cidadesv2[cidadesv2 == "São Paulo"] 
area_casaSP <- area_casa[cidadesv2 == "São Paulo"] #possui a área de todas as casas localizadas em São Paulo. 

#agora, veremos quantas dessas casas tem área = 27m². table(SP[area_casa == 27])
reducedSP <- data.frame(table(area_casaSP)) 
x <- reducedSP[,2] #são os valores da segunda coluna de reducedSP 
y <- as.numeric(levels(reducedSP[,1])) #São os valores da primeira coluna de reducedSP; dessa forma, y passa de factor para numeric. 

#Gráfico para ilustrar a quantidade de casas em sp
par(mfrow = c(1,2), mar=c(3,3,2,2), oma=c(3,3,2,2)) 
plot(x,y, col = ifelse(x==9 & y == 27, "red", "black"), main = "Quantidade de casas por área em São Paulo") 
plot(x,y,xlim = c(0,1000), ylim = c(0,1000), col = ifelse(x==9 & y == 27, "red", "black"), main = "Quantidade de casas por área em São Paulo (ampliado x25)") 
mtext(side=1, text="N° de Casas", outer=T) 
mtext(side=2, text="Área em m²", outer=T)

#Gráfico para ilustrar a quantidade de casas com 27m² em sp
par(mfrow = c(1,2), mar=c(3,3,2,2), oma=c(3,3,2,2)) 
plot(x,y,xlim = c(0,100), ylim = c(0,100), col = ifelse(x==9 & y == 27, "red", "black"), main = "Quantidade de casas por área em São Paulo (ampliado x250)") 
plot(x,y,xlim = c(0,30), ylim = c(0,30), col = ifelse(x==9 & y == 27, "red", "black"), main = "Quantidade de casas por área em São Paulo (ampliado x833)") 
mtext(side=1, text="N° de Casas", outer=T) 
mtext(side=2, text="Área em m²", outer=T) 
legend("bottomright", legend="Casas com 27m²", bty="n", fill = "red") 

#Criando “sphouses” 
sphouses <- data.frame(area_casaSP) 

#Adicionando a coluna “Quartos” 
quartos <- houses_to_rent_v2$rooms[houses_to_rent_v2$city == "São Paulo"] 
sphouses["Quartos"] <-quartos 

#Adicionando a coluna “Banheiros” 
banheiros <- houses_to_rent_v2$bathroom[houses_to_rent_v2$city == "São Paulo"] 
sphouses["Banheiros"] <- banheiros 

#Adicionando a coluna “Vagas” 
vagas <- houses_to_rent_v2$parking.spaces[houses_to_rent_v2$city == "São Paulo"] 
sphouses["Vagas"] <- vagas 

#Adicionando a coluna “Hoa” 
hoa <- houses_to_rent_v2$hoa..R..[houses_to_rent_v2$city == "São Paulo"] 
sphouses["Hoa"] <- hoa 

#Adicionando a coluna “Rent Amount” 
rentamount <- houses_to_rent_v2$rent.amount..R..[houses_to_rent_v2$city == "São Paulo"]
sphouses["Rent Amount"] <- rentamount 

#Adicionando a coluna “Property Tax” 
propertytax <- houses_to_rent_v2$property.tax..R..[houses_to_rent_v2$city == "São Paulo"]
sphouses["Property Tax"] <- propertytax 

#Adicionando a coluna “Fire Insurance” 
fireinsur <- houses_to_rent_v2$fire.insurance..R..[houses_to_rent_v2$city == "São Paulo"]
sphouses["Fire Insurance"] <- fireinsur 

#Adicionando a coluna “Total” 
total <- houses_to_rent_v2$total..R..[houses_to_rent_v2$city == "São Paulo"]
sphouses["Total"] <- total 

#Procurando quartos == 2
sphouses[sphouses$area_casaSP == 27 & sphouses$Quartos == 2,]

#Adequando o numero de quartos

potentialth <- sphouses[sphouses$area_casaSP == 27 & sphouses$Quartos == 1 & sphouses$Banheiros == 1 & sphouses$Vagas== 1,] 
print(potentialth)

#CUSTOS

#Criando a variável com os dados da primeira linha de “sphouses” e colunas que se relacionam com os custos totais. 
casa1176 <- (potentialth[1,c(5,6,7,8,9)]) 

#Atribuindo cada valor da coluna a uma variável 
casa1cond <- casa1176[,1]  
casa1alug <- casa1176[,2]  
casa1predial <- casa1176[,3]  
casa1seg <- casa1176[,4]  
casa1total <- casa1176[,5] 

#Atribuindo o valor de “casa1(...)” pelo total a um vetor. 
vector1 <- c(casa1cond/casa1total, casa1alug/casa1total, casa1predial/casa1total, casa1seg/casa1total) 

#Criando o gráfico. 
dev.off()
pie(vector1, main = "Composição dos Custos Totais: Casa 1176", labels = c("24,7%", "72,1%", "2,2%", "0,9%"), col = c("mediumorchid1", "cadetblue3", "darkgreen", "gray80")) 
legend("bottomright", fill = c("mediumorchid1", "cadetblue3", "darkgreen", "gray80 "),legend = c("Condomínio R$", "Aluguel R$", "Contribuição Predial R$", "Seguro Incêndio R$"),cex = 0.5, pt.cex = 1.5)

#Criando a variável com os dados da segunda linha de “sphouses” e colunas que se relacionam com os custos totais. 
casa2182 <- (potentialth[2,c(5,6,7,8,9)]) 

#Atribuindo cada valor da coluna a uma variável 
casa2cond <- casa2182[,1]  
casa2alug <- casa2182[,2]
casa2predial <- casa2182[,3]
casa2seg <- casa2182[,4]
casa2total <- casa2182[,5] 

#Atribuindo o valor de “casa2(...)” pelo total a um vetor. 
vector2 <- c(casa2cond/casa2total, casa2alug/casa2total, casa2predial/casa2total, casa2seg/casa2total) 

#Criando o gráfico 

pie(vector2, main = "Composição dos Custos Totais: Casa 2182", labels = c("", "98, 7%", "", "1,2%"), col = c("mediumorchid1", "cadetblue3", "darkgreen", "gray80")) 
legend("bottomright", fill = c("mediumorchid1", "cadetblue3", "darkgreen", "gray80 "), legend = c("Condomínio R$", "Aluguel R$", "Contribuição Predial R$", "Seguro Incêndio R$"),cex = 0.5, pt.cex = 1.5)
