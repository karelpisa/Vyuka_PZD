x <- airquality$Ozone 
str(x)

nthMax <- function(x, n){                 #f
  xsorted <- sort(x, decreasing = TRUE)   #seřadím si x od největší po nejmenší
  vysledek <- xsorted[n]                  #hledám ntou hodnotu v seřazeném vektoru
  return(vysledek)                        #vracaím výsledek
}

nthMax(x, 1:3)



set.seed(1)
x = sample(1:6, 20, TRUE)

n = 1
countOccur <- function(x, n){             #funkce se dvěma argumenty "x" a "n"
  n <- x %in% n                           #ptám se  obsahuje x hodnoty z n? výstupem je TRUE/FALSE
  return(length(n[n == TRUE]))            #vracím počet TRUE hodnot v n vektoru
}
countOccur(x, 1)



delitele <- function(x){                  #funkce se dvěma argumenty "x" a "n"
  if(x < 1){                              #podmínka zda je x menší než 1
    print("X je menší než 0")             #podmínka splněna, píšu hlášku
  }else if(x %% 1 != 0){                  #další odmínka -> je číslo celé?
    print("Zadané číslo není celé číslo") #podmínka splněna, píšu hlášku
  }else{                                  #pokud neni splněna ani jedna podmínka ->
    del <- seq_len(x)                     #do proměnné del přiřadím sekvenci celých číel o deélce x
    del[x%%del==0]                        #z "del" vyberu ty čísla která splňují podmínku že x modulo x je 0
  }
}

delitele(35.5)
