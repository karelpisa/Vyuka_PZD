

# Cviceni 1 patek opakovani fci

vzkaz <- function() { message("I toto je funkce")
  
}

# pocetNasobku <- function(zadana_hodnota) {}


x <- 100


pocetNasobku <- function(x) {
nasobek <- 1
pocitani <- 1

while (nasobek<x) {
  
  nasobek <- nasobek*(nasobek+1)
  pocitani <- pocitani+1
}
print(pocitani)
}


