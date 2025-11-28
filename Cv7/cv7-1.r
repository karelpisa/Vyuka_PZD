## ct 1545

tipujeme <- function () {message("Tohle je skvela nova funkce!")}






vygenerovane_cislo <- sample(1:100, 1)

odpoved <- 0.3

while (odpoved>0) {

  odpoved <- as.integer(readline("CO? "))
if(odpoved==vygenerovane_cislo) {
    message("OK")
  stop()
  } else if (odpoved>vygenerovane_cislo) {
  message("odpoved je vetsi")
} else {
  message("odpoved je mensi")
}
}




pocetNasobku <- function(cislo) {
  rada <- 1
  pocet <- 1
  if(cislo==0){
    return(1)
  }else{
  while (rada < cislo) {
    rada <- rada*pocet
    pocet <- pocet+1
  }}
  return(pocet-1)
}

pocetNasobku(10)
pocetNasobku(100)
pocetNasobku(57492)



tipovacka <- function () {
  cislo <- sample(1:100,1)
  odpoved <- 0.1
  while (odpoved>0) {
    odpoved <- as.integer(readline("Jake Cislo Tipujete? "))
    if(odpoved==cislo) {
      message("Spravne")
      break
    } else if (odpoved>cislo) {
      message("Vase odpoved je vetsi nez vygenerovane cislo.")
    } else {
      message("Vase odpoved je mensi nez vygenerovane cislo.")
    }
  }
}

tipovacka()




