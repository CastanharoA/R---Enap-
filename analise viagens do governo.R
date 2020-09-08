#carreganhead(viagens)do dados
?read.csread(viagens)v

viagens <- read.csv(
  file = "C:/Users/Andreia/Desktop/2019_Viagem.csv",
  sep = ';',
  dec = ','
)

#para verificar se o arquivo esta ok - primeiras linhas 
head(viagens)
View(viagens)
#ver dados da tabela 
dim(viagens)
#summary 
summary(viagens)
summary(viagens$Valor.passagens)
summary(viagens$Valor.diárias)
summary(viagens$Valor.outros.gastos)

#transformado dados 
#data 
? as.Date

viagens$data.inicio <- as.Date(viagens$Período...Data.de.início,"%d/%m/%y")


#para verificar
glimpse(viagens)

#formatar data so mes e ano
viagens.inicio.formatda <- format(viagens$data.inicio,"%Y/%m")
viagens.inicio.formatda

hist(viagens$Valor.passagens)

#val max min media 
summary(viagens$Valor.diárias)

#visualizar boxplot 
boxplot(viagens$Valor.passagens)
colSums(is.na(viagens))
#desvio padrao
sd(viagens$Valor.passagens)

?is.na
?colSums

#criando dataframe com 15 orgaos que gastam mais
p1 <- viagens %>%
  group_by(Nome.do.órgão.superior) %>%
  summarise(n = sum(Valor.passagens)) %>%
  arrange(desc(n)) %>%
  top_n(15)

names(p1) <- c("orgao","valor")
p1

#plotando os dados com ggplot
ggplot(p1,aes(x = reorder(orgao,valor),y = valor))+
  geom_bar(stat = "identity" )+
  coord_flip()+
  labs(x = "valor", y = "orgãos")

#valor gasto por cidade
p2 <- viagens %>%
  group_by(Destinos) %>%
  summarise(n = sum(Valor.passagens)) %>%
  arrange(desc(n)) %>%
  top_n(15)

names(p2) <- c('destino','valor')
p2

#criando grafico 
ggplot(p2,aes(x= reorder(destino,valor), y=valor))+
  geom_bar(stat ="identity")+
  geom_text(aes(label=valor), vjust =0.3, size=3)+
  coord_flip()+
  labs(x="valor",y="Destino")

options(scipen=999)

#instalaar rmarkdown
install.packages("rmarkdown")
install.packages("tinyex")




