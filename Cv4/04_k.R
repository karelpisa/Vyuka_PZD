FUNsudalicha <- function(x){ #definuji nazev funkce a jeji argument
  if(x %% 2 == 0) {           #do funkce vložím funkci if kde porovnávám výsledek modulo 2 s nulou
    print("Číslo je sudé")    #pokud je podmínka splněna - vypíšu výsledek -> sudé
  } else {                    #pokud ne, vypíšu výsledek 
    print("Číslo je liché")   #liché
  }
}

FUNsudalicha(13159) 


FUNkruh <- function(r) {    #definuji nazev funkce a jeji argument
  o = 2 * pi * r            #uvnitř funkce spočtu potřebné hodnoty a přiřadím je novým proměnným
  s = pi * r^2
  vysledek <- c(o, s)       #výsledky vložím do vektorz "vysledek"
  names(vysledek) <- c("Obvod", "Obsah") #hodnotám ve vektrou výsledek přiřadím jména
  return(vysledek)        #vracím proměnnou výsledek
    }
FUNkruh(14)


prumer <- function(x, NARM = FALSE){ #definuji nazev funkce a jeji argumenty
  if(NARM == FALSE){                #porovnávám hodnotu argumentu NARM
  prumer <- sum(x)/length(x)        #pokud je splněna, vytvořím promennou prumer pomoci funkci sum a length
  return(prumer)                    #vracím hodnotu prumeru
  }else{                            #pokud podminka neni splnena....
    x <-  x[which(!is.na(x))]       #vytvorim nové "x", kam vyberu vsechny prvky z puvodniho "x", ktere nejsou NA
    prumer <- sum(x) / length(x)    #pak uz vytvorim promenou prumer pooci sum a length z "noveho" "x" 
    return(prumer)                  #vracim hodnotu prumeru
  }
}

prumer(c(1,2,3,4,5, NA)) #vraci NA, protoze defaultne je NARM FALSE, tedy neodstranujeme NA hodnoty
prumer(c(1,2,3,4,5, NA), NARM = TRUE) #funguje


