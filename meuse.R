# ------ ESTATISTICA ESPACIAL DESCRITIVA ------

# Carregando as bibliotecas necessárias:
library(sp) # inclui o conjunto de dados meuse
library(spdep) # biblioteca com funções para análise espacial
library(DescTools) # coletânea de funções estatísticas.


# "meuse" é um conjunto de dados que contém informações sobre
# a qualidade da água e a concentração de metais pesados em
# solos e sedimentos do Rio Meuse, na Holanda.

# Carregando conjunto de dados MEUSE

data(meuse)


# Medidas de Centralidade

# Calculando a média espacial da concentração de zinco
media_espacial <- mean(meuse$zinc, na.rm = TRUE)
print(media_espacial)

# Calculando a mediana espacial da concentração de zinco
mediana_espacial <- median(meuese$zinc)
print(mediana_espacial)

# Calculando a moda espacial da concentração de zinco
moda_espacial <- mode(meuse$zinc)




# Medidas de Dispersão
# Calculando o desvio padrão espacial e variância espacial
desvio_padrao_espacial <- sd(meuse$zinc)
print(desvio_padrao_espacial)
variancia_espacial <- var(meuse$zinc)
print(variancia_espacial)

# Calcular o coeficiente de variação espacial
coeficiente_variacao_espacial <- desvio_padrao_espacial / media_espacial
print(coeficiente_variacao_espacial)





# Índices de Autocorrelação Espacial
# Converter o conjunto de dados meuse para um objeto SpatialPointsDataFrame
coordinates(meuse) <- c("x", "y")

# Calcular os "vizinhos" mais próximos usando a distância euclidiana
knn <- knearneigh(coordinates(meuse), k = 6)

# Converter os "vizinhos" mais próximos em uma matriz de pesos espaciais
listw <- knn2nb(knn)


# Tornar a matriz de pesos espaciais em uma matriz binária de pesos espaciais
listw <- nb2listw(listw, style = "B")


# Calcular o Índice de Morrow
# Teste de hipóteses estatísticos,
# em que H0 indica a ausência de correlação espacial,
# logo, se p-value < 0.05, podemos assumir que
# há correlação espacial.
indice_moran <- moran.test(meuse$zinc, listw)
print(indice_moran)

# Calcular o Índice de Geary
indice_geary <- geary.test(meuse$zinc, listw)
print(indice_geary)

# Calcular o Índice de Lisa
indice_lisa <- localmoran(meuse$zinc, listw)
print(indice_lisa)
