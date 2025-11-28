#y = -3
#for(i in -2:2){ y = y * i }
#y
# y=0 protoze i=0
#

# y = -2
#for(i in c(1,3,3,5)){
#  y = y + i
#  if(y > 5) break
#}
#y
# 1.kolo y=-1, i=1
# 2.kolo y=2, i=3
# 3.kolo y=5, i=3
# 4. kolo y=10, i=5

# y = 0
#i = 0
#while(i > 2){
#  y = i * i
#  i = i - 1
#}
#y






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


