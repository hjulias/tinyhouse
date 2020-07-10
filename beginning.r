#O dataset utilizado é "houses_to_rent_v2". O objetivo é analisar a maior quantidade de dados possível até chegar a conclusão de quantas casas são potenciais "Tiny Houses".
#Criar variáveis mais simples
andares <- houses_to_rent_v2$floor
cidadesv2 <- houses_to_rent_v2$city
area_casa <- houses_to_rent_v2$area

#Verificação de inconsistências 
andares[andares== "-"] <- 0 #Substitui os valores - na coluna "floor" por 0. Demonstra que nenhum andar da casa fora construído até o momento.

#Para obter os quartis, médias e medianas de uma vez para todas as variáveis, use o comando:
summary(houses_to_rent_v2[,])

#Gráfico de barras ilustrando a quantidade de casas por cidade.
barplot(table(cidadesv2), xlab = "Cidades", ylab = "N de Casas", col = c("pink", "green", "yellow", "red", "blue"), main = "Quantidade de casas por cidade") #é possível utilizar esse comando para qualquer coluna com baixa variação de dados. Ex: rooms, bathroom, parking spaces, floor. xlab indica o nome do eixo das abscissas (X), e ylab das ordenadas(Y).
text(0.7, 900, table(cidadesv2[cidadesv2 == "Belo Horizonte"]))
text(1.9, 600, table(cidadesv2[cidadesv2 == "Campinas"]))
text(3.1, 900, table(cidadesv2[cidadesv2 == "Porto Alegre"]))
text(4.3, 1300, table(cidadesv2[cidadesv2 == "Rio de Janeiro"]))
text(5.5, 5000, table(cidadesv2[cidadesv2 == "São Paulo"]))

#Antes de obter o gráfico, vamos conferir quantas casas existem em SP e quantas possuem 27m².
SP <- cidadesv2[cidadesv2 == "São Paulo"] 
table(SP) #retorna um total de 5887 casas.
table(SP[area_casa == 27]) #Resulta em 9 imóveis que satisfazem a condição de ter 27m²

#Gráfico de dispersão mostrando qnt de casas por área em SP
area_casaSP <- area_casa[cidadesv2 == "São Paulo"] #definindo quais são as áreas de SP
summary(table(area_casa[cidadesv2 == "São Paulo"])) #conferindo valores

#Gráfico mostrando quantas casas com 27m² existem em São Paulo
df <- data.frame(table(area_casaSP)) #atribui a df um data frame com duas colunas, sendo "área das casas paulistas" a primeira e "frequência" a segunda.
#atenção: SP possui 5887 casas, mas df exibe 451 valores pois atribui, para cada área, uma quantidade de casas.  
x <- df[,2] #são os valores da segunda coluna de df
y <- as.numeric(levels(df[,1])) #São os valores da primeira coluna de df; dessa forma, y passa de factor para numeric.
par(mfrow = c(2,2), mar=c(3,3,2,2), oma=c(3,3,2,2))
plot(x,y, col = ifelse(x==9 & y == 27, "green", "black"), main = "Quantidade de casas por área em São Paulo")
plot(x,y,xlim = c(0,1000), ylim = c(0,1000), col = ifelse(x==9 & y == 27, "green", "black"), main = "Quantidade de casas por área em São Paulo (ampliado x25)")
plot(x,y,xlim = c(0,100), ylim = c(0,100), col = ifelse(x==9 & y == 27, "green", "black"), main = "Quantidade de casas por área em São Paulo (ampliado x250)")
plot(x,y,xlim = c(0,30), ylim = c(0,30), col = ifelse(x==9 & y == 27, "green", "black"), main = "Quantidade de casas por área em São Paulo (ampliado x833)")
mtext(side=1, text="N° de Casas", outer=T)
mtext(side=2, text="Área em m²", outer=T)
legend("bottomright", legend="Casas com 27m²", bty="n", fill = "green")

####RASCUNHO
reducedSP <- data.frame(table(area_casaSP))

x <- reducedSP[,2] #são os valores da segunda coluna de reducedSP
y <- as.numeric(levels(reducedSP[,1])) #São os valores da primeira coluna de reducedSP; dessa forma, y passa de factor para numeric.

par(mfrow = c(2,2), mar=c(3,3,2,2), oma=c(3,3,2,2))
plot(x,y, col = ifelse(x==9 & y == 27, "red", "black"), main = "Quantidade de casas por área em São Paulo")
plot(x,y,xlim = c(0,1000), ylim = c(0,1000), col = ifelse(x==9 & y == 27, "red", "black"), main = "Quantidade de casas por área em São Paulo (ampliado x25)")
plot(x,y,xlim = c(0,100), ylim = c(0,100), col = ifelse(x==9 & y == 27, "red", "black"), main = "Quantidade de casas por área em São Paulo (ampliado x250)")
plot(x,y,xlim = c(0,30), ylim = c(0,30), col = ifelse(x==9 & y == 27, "red", "black"), main = "Quantidade de casas por área em São Paulo (ampliado x833)")
mtext(side=1, text="N° de Casas", outer=T)
mtext(side=2, text="Área em m²", outer=T)
legend("bottomright", legend="Casas com 27m²", bty="n", fill = "red")
###################

#Agora, filtraremos nossa busca para descobrir quantas dessas 9 casas possuem dois quartos.
quartos <- houses_to_rent_v2$rooms[houses_to_rent_v2$city == "São Paulo"] #criando uma variável que agrupa todos os quartos das casas de SP.
reducedSP["Quartos"] <- quartos #adicionando a variável ao df.
reducedSP[reducedSP$Area == 27 & reducedSP$Quartos == 2,] #Não existem casas de 27m² com 2 quartos. 
reducedSP[reducedSP$Area == 27,] #nos mostra que todas as casas de 27m² possuem apenas 1 quarto.

barplot(reducedSP[reducedSP$Area == 27,][,3],reducedSP[reducedSP$Area == 27,][,2], ylim = c(0,2), xlab = "Casas em SP com 27m²", ylab = "Qnt. de quartos", col = "blue", main = "Quantidade de quartos em casas paulistas de 27m²" )

#Quantas dessas 9 casas possuem 1 banheiro?
banheiro <- houses_to_rent_v2$bathroom[houses_to_rent_v2$city == "São Paulo"]
reducedSP["Banheiros"] <- banheiro
reducedSP[reducedSP$Area == 27 & reducedSP$Banheiros == 1,]

barplot(reducedSP[reducedSP$Area == 27,][,4],reducedSP[reducedSP$Area == 27,][,2], ylim = c(0,2), xlab = "Casas em SP com 27m²", ylab = "Qnt. de banheiros", col = "pink", main = "Quantidade de banheiros em casas paulistas de 27m²" )

#Quantas dessas 9 casas possuem 1 vaga de estacionamento?
vaga <- houses_to_rent_v2$`parking spaces`[houses_to_rent_v2$city == "São Paulo"]
reducedSP["Vagas"] <- vaga
reducedSP[reducedSP$Area == 27 & reducedSP$Vagas == 1,]

barplot(reducedSP[reducedSP$Area == 27,][,5],reducedSP[reducedSP$Area == 27,][,2], ylim = c(0,2), xlab = "Casas em SP com 27m²", ylab = "Qnt. de vagas de estacionamento", col = "yellow", main = "Quantidade de vagas de estacionamento em casas paulistas de 27m²" )

#Desprezando as colunas seguintes, passaremos a analisar qual das duas casas possuem um custo mensal total inferior a 7.5k.
totaldindin <- houses_to_rent_v2$`total (R$)`[houses_to_rent_v2$city == "São Paulo"]
reducedSP["Total $"] <- totaldindin
reducedSP[reducedSP$Area == 27 & reducedSP$Vagas==1,]

#obj: Encontrar quantas casas satisfazem as condições: SP,27m², 2 quartos, 1 banheiro, 1 estacionamento, cerca de $180k no total (7.5k por mês, dois anos). Qual a % de casas que satisfazem as condições? Do custo total, quantos % são destinados a imposto, seguro, condomínio e aluguel?  
