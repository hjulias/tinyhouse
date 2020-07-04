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

#Obtendo o gráfico, vamos agora buscar quantas casas existem em São Paulo.
SP <- cidadesv2[cidadesv2 == "São Paulo"] 
table(SP) #retorna um total de 5887 casas.
table(SP[area_casa == 27]) #Resulta em 9 imóveis que satisfazem a condição de ter 27m²

#Gráfico de dispersão mostrando qnt de casas por área em SP
area_casa[cidadesv2 == "São Paulo"] #definindo quais são as áreas de SP
summary(table(area_casa[cidadesv2 == "São Paulo"])) #conferindo valores
area_casaSP <- area_casa[cidadesv2 == "São Paulo"] 

#Montar uma tabela com SP e area_casaSP
reducedSP <- data.frame(Cidade = SP, Area = area_casaSP)


#obj: Encontrar quantas casas satisfazem as condições: SP,27m², 2 quartos, 1 banheiro, 1 estacionamento, cerca de $180k no total (7.5k por mês, dois anos). Qual a % de casas que satisfazem as condições? Do custo total, quantos % são destinados a imposto, seguro, condomínio e aluguel?  
