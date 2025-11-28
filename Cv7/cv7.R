
# 1
set.seed(3)  #nastavím seed abych měl replikovatelné výsledky v generátoru náhodných čísel

GenerujiCisla <- function() {
  #vytvorim funkci
  n <- rnorm(1) #vygeneruji prvni cislo n
  while (n < 1) {
    #vytvarim cyklus ktery bezi dokud je "n" mensi nez 1
    n <- rnorm(1) #generuji dalsi cislo
    if (n < 0) {
      #pokud je cislo mensi nez 0, preskakuji ho a jede dalsi kolo cyklu
      next
    }               #pokud je vetsi nez 0, vytisknu ho (radek 12)
    print (n)
  }
}
  
GenerujiCisla() #spusteni funkce



# 2
pocetNasobku <- function(cislo) {
  #vytvářím funkci
  rada <- 1     #předdefinuji řadu (výsledek součinu)
  pocet <- 1  #předdefinuji pocet (pocet čísel, která násobím (1*2*3...atd))
  if (cislo == 0) { #pokud je cislo 0 ->
    return(0) #rovnou vracim 1 a funkce konci
  } else {      #pokud je cilo jine nez 0
    while (rada < cislo) {  #spostim cyklus dokud rada nepřesáhne cislo -> dokud postupny soucin cisel v rade nepřesáhne zadané cislo
      rada <- rada * pocet   #spoctu radu -> tzn předchozí rada * pocet
      pocet <- pocet + 1    #rozsirim pocet o 1
    }
  }
  return(pocet - 1) #pokud while cyklus skoncí, vracím pocet-1, protoze jsem ho v tom cyklu jeste o 1 rozsiril, ale ve vysledku to uz nechci
}

# spusteni funkce s ruznymi cisly
pocetNasobku(0)
pocetNasobku(100)
pocetNasobku(57492)



# 3
tipovacka <- function () {                 # vytvářím funkci s názvem "tipovacka"
  cislo <-                                   
    sample(1:100, 1)                        # generuji náhodné celé číslo v rozmezí 1–100
  odpoved <- 0.1                            # inicializuji proměnnou 'odpoved', aby vstoupila do cyklu while
  while (odpoved > 0) {                     # dokud je odpověď větší než 0, hra pokračuje
    odpoved <- as.integer(                 
      readline("Jake Cislo Tipujete? ")     # požádám uživatele o vstup a převedu jej na celé číslo
    )
    if (odpoved == cislo) {                 # kontroluji, zda hráč trefil správné číslo
      message("Dobra prace!")               # pokud ano, vypíšu pochvalu
      opt <- options(show.error.messages = F) # vypnu zobrazování chybových zpráv (dočasně)
      on.exit(options(opt))                 # po ukončení funkce obnovím původní nastavení
      stop()                                # ukončím program (výhra)
    } else if (odpoved > cislo) {           # pokud je odpověď větší než tajné číslo
      message("Vase odpoved je vetsi nez vygenerovane cislo.")  # upozornění pro hráče
    } else {                                # pokud je odpověď menší než tajné číslo
      message("Vase odpoved je mensi nez vygenerovane cislo.")  # upozornění pro hráče
    }
  }
}                                           # konec funkce


tipovacka()



