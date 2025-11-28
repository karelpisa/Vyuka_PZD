setwd("../../../Downloads/") #nastavím si cestu do složky, kde mám uložená data
setwd("OneDrive - CZU v Praze/Výuka/PZD/Cv3/")

dt <- read.table("FrenchBroadRiverPZD.txt", header = TRUE) #načtu data do proměnné dt, header = TRUE říká funkci, že data mají názvy sloupců
head(dt) #prvních pár řádků
str(dt) #struktura dt
summary(dt) #základní statistické údaje

complete.cases(dt) #kde je TRUE -> tam je kompletní řádek
!complete.cases(dt) #otočím TRUE a FALSE
which(!complete.cases(dt)) #na kterých řádcích mám nekopletní data
dt$Date[which(!complete.cases(dt))] #které konkrétní datumy to jsou

dt$Date <- as.Date(dt$Date) #převod třídy sloupce Date na "Date"
dt$mesic <- format(as.Date(dt$Date), "%m") #přidávám sloupec mesic pouze s cislem mesice
mesice <- split(dt$Q, as.factor(dt$mesic)) #vytvorim list, kde mám jen hodnoty průtoku po jednotlivych mesicich
prumerQ1 <- round(c(mean(mesice[[1]]), mean(mesice[[2]]), ...), 2) #rucni verze...pocitam prumer z kazdeho prvku listu a zaokrouhlim na 2 desetinna
prumeryQ2 <- round(sapply(mesice, mean), 2) #na vsechny prvky listu aplikuji funkci mean a zaokrouhlim na 2 desetinna
