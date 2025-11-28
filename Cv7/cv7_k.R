## Ukol 1 Jasne zkousime na papir

## Ukol 2  While cyklus — S využitím while cyklu sestavte funkci, která bude vypisovat na obrazovku náhodně vygenerovaná čísla pomocí funkce rnorm. Cyklus while v rámci funkce poběží do té doby, dokud hodnota vygenerovaného čísla bude menší než jedna. Vynechejte z výpisu na obrazovku záporné hodnoty (příkaz next, který se používá podobně jako break)

set.seed(3)

GenerujiCisla <- function() {
drum <- rnorm(1)

while (drum<1) {
  drum <- rnorm(1)
  if (drum<0) {
    next
  }
print (drum)
}
}
  
GenerujiCisla()



## Ukol 3 S využitím while cyklu sestavte funkci, která spočítá počet prvků v řadě 1 · 2 · 3 · ... než výsledek této řady překročí nebo dosáhne hodnoty zadané jako argument vytvořené funkce. Např. pro dotazovanou hodnotu 10 je výsledek 4, protože 1 · 2 · 3 · 4 > 10.

cislo=0

pocetNasobku <- function(cislo) {

rada <- 1
pocet <- 1

if(cislo==0)
  return(1)

while (rada <cislo) {
  
  rada <- rada*pocet
  
  pocet <- pocet+1
  
}
return(pocet-1)
}

pocetNasobku(0)
pocetNasobku(100)
pocetNasobku(57492)














## Ukol 4 Vytvořte funkci, která umožní vyzkoušet si hádání skrytého čísla. Po zavolání si funkce vygeneruje náhodnou celočíselnou hodnotu z rozsahu 1–100 a následně bude dotazovat váš tip hodnoty. Po zadání čísla bude buď zvolená hodnota odpovídat neznámemu číslu, pak se funkce ukončí s příslušnou zprávou o správném tipu, nebo hodnota nebude odpovídat neznámému číslu, pak funkce navede uživatele informací zda jeho odhad byl menší nebo větší než neznámé číslo. (Z důvodu charakteru této úlohy zde není náhled řešení.) Užitečné funkce: as.integer(readline()) pro umožnění zadání tipované hodnoty během provádění funkce. Dále již známé funkce sample, message, cyklus while, podm. příkaz if.



tipovacka <- function () {

cislo <- sample(1:100,1)
odpoved <- 0.1
while (odpoved>0) {
  odpoved <- as.integer(readline("Jake Cislo Tipujete? "))
if(odpoved==cislo) {
  message("Dobra prace!")
  opt <- options(show.error.messages = F)
  on.exit(options(opt))
  stop()
} else if (odpoved>cislo) {
  message("Vase odpoved je vetsi nez vygenerovane cislo.")
  next
} else {
  message("Vase odpoved je mensi nez vygenerovane cislo.")
  next
}
}
}

tipovacka()



tipovacka()

