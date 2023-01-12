
rm(list=ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(nortest)
library(ggpubr)

# 
# install.packages("nortest")
# install.packages("ggpubr")

df <- read_excel("C:/Users/48500/Desktop/projekt/Michał/dane.xlsx")
df$cena <- as.numeric(df$cena)


df_reshape <- tidyr::spread(df, Produkt, cena)

colnames(df)

########## Miary Tendencji Centralnej ##########

#### Polska 

df %>% 
  group_by(Produkt) %>% 
  summarise(srednia_cena = mean(cena),
            mediana_cena = median(cena))


quantile(df_reshape$`mięso wołowe z kością (rostbef)`)

quantile(df_reshape$`olej napędowy`)

quantile(df_reshape$`pasta do zębów`)

#### Wojewodztwa
df %>% 
  group_by(woj,Produkt) %>% 
  summarise(srednia_cena = mean(cena),
            mediana_cena = median(cena))


############# Miary Rozproszenia ##############

# Rozstęp, wariancja, odchylenie, 
### Polska
 df %>% 
  group_by(Produkt) %>% 
  summarise(Rozstep = max(cena) - min(cena),

            Rozstep_cwiartkowy = quantile(cena, 0.75) - quantile(cena, 0.25),
            odchylenie_cwiartkowe = (quantile(cena, 0.75) - quantile(cena, 0.25))/2,
            pozycyjny_wsp_zmiennosci = 100*((quantile(cena, 0.75) - quantile(cena, 0.25))/2)/median(cena),
            wariancja = var(cena),
            odchylenie_standardowe = sd(cena),
            klasyczny_wsp_zmiennosci = 100*sd(cena) / mean(cena))


### woj
miary_rozproszenia_woj <- df %>% 
  group_by(woj,Produkt) %>% 
  summarise(Rozstep = max(cena) - min(cena),
            Rozstep_cwiartkowy = quantile(cena, 0.75) - quantile(cena, 0.25),
            odchylenie_cwiartkowe = (quantile(cena, 0.75) - quantile(cena, 0.25))/2,
            pozycyjny_wsp_zmiennosci% = 100*((quantile(cena, 0.75) - quantile(cena, 0.25))/2)/median(cena),
            wariancja = var(cena),
            odchylenie_standardowe = sd(cena),
            klasyczny_wsp_zmiennosci% = 100*sd(cena) / mean(cena))


#### Histogram


### Polska
ggplot(df_reshape, aes(x=`mięso wołowe z kością (rostbef)`)) +
  geom_histogram()  +
  geom_histogram(color="white", fill="steelblue") 


ggplot(df_reshape, aes(x=`olej napędowy`)) +
  geom_histogram()  +
  geom_histogram(color="white", fill="steelblue") 


ggplot(df_reshape, aes(x=`pasta do zębów`)) +
  geom_histogram()  +
  geom_histogram(color="white", fill="steelblue") 

### woj

plot_hist_by_woj_first <- function(woj = "MAZOWIECKIE"){
  
  df_filter <- df_reshape[df_reshape$woj == woj,]
  
  ggplot(df_filter, aes(x=`mięso wołowe z kością (rostbef)`)) +
    geom_histogram()  +
    geom_histogram(color="white", fill="steelblue") +
    labs(title = paste("mięso wołowe z kością (rostbef):",woj))
  
}

plot_hist_by_woj_first(woj = "MAZOWIECKIE")
plot_hist_by_woj_first(woj = "POMORSKIE")
plot_hist_by_woj_first(woj = "ŚLĄSKIE")

plot_hist_by_woj_second <- function(woj = "MAZOWIECKIE"){
  
  df_filter <- df_reshape[df_reshape$woj == woj,]
  
  ggplot(df_filter, aes(x=`olej napędowy`)) +
    geom_histogram()  +
    geom_histogram(color="white", fill="steelblue") +
    labs(title = paste("olej napędowy:",woj))
  
}

plot_hist_by_woj_second(woj = "MAZOWIECKIE")
plot_hist_by_woj_second(woj = "POMORSKIE")
plot_hist_by_woj_second(woj = "ŚLĄSKIE")

plot_hist_by_woj_third <- function(woj = "MAZOWIECKIE"){
  
  df_filter <- df_reshape[df_reshape$woj == woj,]
  
  ggplot(df_filter, aes(x=`pasta do zębów`)) +
    geom_histogram()  +
    geom_histogram(color="white", fill="steelblue") +
    labs(title = paste("pasta do zębów:",woj))
  
}

plot_hist_by_woj_third(woj = "MAZOWIECKIE")
plot_hist_by_woj_third(woj = "POMORSKIE")
plot_hist_by_woj_third(woj = "ŚLĄSKIE")


#### box plot 


### Polska
ggplot(df_reshape, aes(y=`mięso wołowe z kością (rostbef)`)) +
  geom_boxplot()
  

ggplot(df_reshape, aes(y=`olej napędowy`)) +
  geom_boxplot()


ggplot(df_reshape, aes(y=`pasta do zębów`)) +
  geom_boxplot()  


# woj 
ggplot(df_reshape, aes(x = woj,y=`mięso wołowe z kością (rostbef)`)) +
  geom_boxplot()


ggplot(df_reshape, aes(x = woj, y=`olej napędowy`)) +
  geom_boxplot()


ggplot(df_reshape, aes(x = woj,y=`pasta do zębów`)) +
  geom_boxplot()  



#### przedział ufności dla średniej

# duze proba; n > 30
length(df_reshape$`mięso wołowe z kością (rostbef)`)
# neiznande odchylenie standardowe


licz_przedzial_ufnosci_dla_sredniej <- function(test = df_reshape$`mięso wołowe z kością (rostbef)`, alfa = 0.05){
  n<-length(test)
  #Średnia : 
  m<-mean(test)
  #Odchylenie standard. : z próby : 
  s<-sd(test)
  #Kwantyl rozkładu Normalego : alfa = 0.05
  e<-qnorm(1-alfa/2)
  #Margines błędu : 
  E<-e*s/sqrt(n)
  #Przedział ufności : 
  print("Przedział ufności:")
  m+c(-E,+E)
  }


licz_przedzial_ufnosci_dla_sredniej(test = df_reshape$`mięso wołowe z kością (rostbef)`,alfa = 0.05)

licz_przedzial_ufnosci_dla_sredniej(test = df_reshape$`olej napędowy`)

licz_przedzial_ufnosci_dla_sredniej(test = df_reshape$`pasta do zębów`)


#### Minimalna liczebnosc proby 

licz_minimalna_liczebnosc_proby <- function(test = df_reshape$`mięso wołowe z kością (rostbef)`,alfa = 0.05, d = 0.2){
  
  
  n<-length(test)
  # Wartość średnia :
  m<-mean(test)
  # Wartość odchylenia standardowego (nieznana - wyznaczona z próby): 
  s<-sd(test)
  # Kwantyl rozkładu t-Studenta :
  e<-qt(1-alfa/2,df=n-1)
  # Wyznaczona liczność :
  licz<- e^2*s^2/d^2

  print("Minimalna liczebnosc proby:")
  print(licz)
  
  print("Nasza próba:")
  print(n)
  
  if (licz > n) {
    print("Należy zwiększyć próbe!")
  }
  else { print("Próba jest wystarczająca.")}

}



licz_minimalna_liczebnosc_proby(test = df_reshape$`mięso wołowe z kością (rostbef)`,alfa = 0.05, d = 0.2)

licz_minimalna_liczebnosc_proby(test = df_reshape$`olej napędowy`,alfa = 0.05, d = 0.2)

licz_minimalna_liczebnosc_proby(test = df_reshape$`pasta do zębów`,alfa = 0.05, d = 0.2)

#### test o wartości średniej dla ceny wybranego produktu)

# H0: srednia cena miesa = 25 zł
# H1: srednia cena miesa != 25 zł


mean(df_reshape$`mięso wołowe z kością (rostbef)`)
sd(df_reshape$`mięso wołowe z kością (rostbef)`)


n <- length(df_reshape$`mięso wołowe z kością (rostbef)`)
m0 <- 27
alfa <- 0.05

U <- (mean(df_reshape$`mięso wołowe z kością (rostbef)`)-m0) /sd(df_reshape$`mięso wołowe z kością (rostbef)`)*sqrt(n)
#Wartość testowa
print(U)
#Wartość krytyczna dla testu dwustronnego
qnorm(1-alfa/2) 


if (U > qnorm(1-alfa/2)) {
  print("Należy odrzucić H0 na korzysc hipotezy alternatywnej")
} else { print("Brak podstaw do odrzucenia hipotezy zerowej ")}


### Porównanie srednich dla dwóch lat. 


df_2019 <- df_reshape[df_reshape$Rok == "2019",]
df_2018 <- df_reshape[df_reshape$Rok == "2018",]

### Test dwoch srednich dla pasty do zebow dla lat 2019 i 2018
# srednia 2019 
mean(df_2019$`pasta do zębów`)
mean(df_2018$`pasta do zębów`)





#H0 srednia są równe 
#H1 srednie nie sa równe 


testuj_dwie_srednie <- function(X,Y,alfa=0.1){
  #Wartość testowa
  test_value <- (mean(X)-mean(Y))/sqrt(var(X)*length(X)+var(Y)*length(Y))*sqrt(length(X)*length(Y)*(length(X)+length(Y)-2)/(length(X)+length(Y)))
  print("Wartość testowa:")
  print(test_value)
  
  #Wartość krytyczna dla testu dwustronnego
  k_dwustronny <- qt(1-alfa/2,length(X)+length(Y)-2)
  
  #Wartość krytyczna dla testu jednostronnego
  k_jednostronny <- qt(1-alfa,length(X)+length(Y)-2)
  print("Wartość krytyncza:")
  print(k_dwustronny)
  ### jezeli jest pomiedzy -k_dwustronny a k_dwustrony to wtedy brak podstaw
  
  if (test_value < k_dwustronny & test_value >  - k_dwustronny) {
    print("Brak podstaw do odrzucenia H0. Średnie są równe")
  } else { print("Należy odrzucić H0 na korzysc hipotezy alternatywnej. Srednie nie sa równe.")}
  
  
}

testuj_dwie_srednie(X = df_2019$`pasta do zębów`, Y = df_2018$`pasta do zębów`, alfa = 0.05 )

testuj_dwie_srednie(X = df_2019$`mięso wołowe z kością (rostbef)`, Y = df_2018$`mięso wołowe z kością (rostbef)`, alfa = 0.05 )


### TEstowanie rozkładu 

# H0 zmienna pochodzi z rozkładu normalnego
# H1 zmienna nie pochodzi z rozkladu normalnego 

test_normalnosc <- function(X = df_reshape$`mięso wołowe z kością (rostbef)`){
  test_shapiro <- shapiro.test(X)
  print("P-Value Shapiro:")
  print(test_shapiro$p.value)
  
  test_lilie <- lillie.test(X)

  print("P-Value Lilieforsa:")
  print(test_lilie$p.value)
  
  if (test_shapiro$p.value > 0.05 ) {
    print("Brak podstaw do odrzucenia H0. Dane pochodzą z rozkładu normalnego. Na podstawie testu Shapiro")
  } else { print("Należy odrzucić H0. Dane nie pochodzą z rozkładu normalnego.Na podstawie testu Shapiro")}
  
  
  
  qqnorm(X)
  qqline(X, col = "steelblue", lwd = 2)
  
}
#### mięso wołowe z kością (rostbef)
test_normalnosc(X = df_reshape$`mięso wołowe z kością (rostbef)`)


####  olej napędowy
test_normalnosc(X = df_reshape$`olej napędowy`)

####  pasta do zębów
test_normalnosc(X = df_reshape$`pasta do zębów`)


library('knitr')
