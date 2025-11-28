seq(from = 1, to = 25, by = 3) #"od, do, po kolika"
seq(1, 25, 3)   #to samé - R rovnou ví, že první číslo je from, druhé to a třetí by
seq(from = 5, to = -5, by = -1) #pořád stejně, jen kleáme, takže by musí být s mínusem
seq(0, 1, 0.1) #tady jen krok je po jedné desetině
rep(x = 0, times = 10) #"co, kolikrát..."

rep(c(1,2), 5) #tady už to "co" je víc čísel, tak zabalíme do vektoru c(1, 2)

rep("ahoj", 5) #opakujeme řetezec znaků "ahoj" - pořád jen jeden prvek, takže vektor není třeba

rep(c("kočka", "pes"), 5) # tady už máme dva prvky jako řetězce znaků, takže jsou opět ve vektoru

seq(from = as.Date("2020-10-05"), to = as.Date("2020-10-15"), by = "day") #stejná seq jako nahoře, jen musíme definovat že ffrom a to je as.Date(), 
#by je před definováno že bere jako vstup "day", "month" a "year", pokud vložím číslo, je to počet dní.

set.seed(1) #nastavuji počáteční bod generátoru (pseudo) náhodných čísel
x <- sample(1:6, 50, replace = TRUE) #do proměnné x přiřadím výstup generátoru náhodných čísel
#vzorek čísel v rozmezí 1 - 6, chci 50 čísel a čísla se mohou opakovat

sum(x == 6) #počet šestek ve vektoru x
which(x == 6)  #na kterých pozicích je šestka
x[1:10] #prvních deset hodnot vektoru x
x[x>3] #hodnoty vektoru X které jsou větší než 3
x[x == 6] <- NA #všechny šestky v X vyměním za NA
