?airquality #nápověda k datasetu airquality
dt = airquality #dataset přiřadíme k proměnné dt (jen zjednodušení)

sum(is.na(dt)) #kolik NA hodnot je celkem v datasetu
names(which(colSums(is.na(dt))>0)) # které sloupce obsahují NA hodnoty

round(colMeans(dt, na.rm=TRUE),2) #colmeans spočte průměry jednotlivých sloupců, na.rm ignoruje NA hodnoty
#a round zaokrouhlí na 2 desetinná místa

# min a max ve sloupci Wind
dt[dt$Month == 6, ] #1) vyberu jen červenec -> z dt vybírám všechny řádky kde Month je roven 6, na pozici sloupců je prázdno, proto jen čárka a nic
dt[dt$Month == 6, ]$Temp #2) z toho pomocí dolaru vyberu jen hodnoty větru
(dt[dt$Month == 6, ]$Temp) *1.61 #3) dám do závorky a převedu na km/h

range((dt[dt$Month == 6, ]$Temp) *1.61) #4) to celé dám do funkce range()
#řádek 15 stačí -> jen jsem to vypisoval postupně
#dny v červenci kdy je víc než 29 stupňů
which(((dt[dt$Month==7,]$Temp-32)*5/9)>29)

#stejný princip jako v minulé úloze
# vyberu měsíce které jsou rovny 7
# přes dolar vyberu jen hodnoty teploty
# ty pak převedu na stupne celsia
# cele dam do zavorky a porovnam s 29 a zabalím do funkce which()