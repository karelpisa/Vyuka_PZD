
n=20

rnormPositiveLess1 <- function(n) {
  i <- 1
  vys <- c()

while ( vys[i]<1) {
  
  vys[i] <- rnorm(n, mean=0.3)
  if(vys[i]<0) {
    next
  }
  return (vys)
 
}
}

set.seed(3)
rnormPositiveLess1()



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


