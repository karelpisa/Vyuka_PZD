# 1

kumsoucin <- function(x){
  vysledek <- c(x[1])
  for (i in 2:(length(x)-1)) {
    vysledek[i] <- vysledek[i-1] * x[i]
  }
  print(vysledek)
}


kumsoucin(12:15)


#2
setwd("../OneDrive - CZU v Praze/Výuka/PZD/Cv6/")

dt <- read.table("FrenchBroadRiverPZD.txt", header = TRUE)
dt$Date <- as.Date(dt$Date)
dtQ <- data.frame(Date = dt$Date,
                  Q = dt$Q,
                  day = format(dt$Date, format = "%m-%d"))

denniprum <- function(x){
  dtsplit <- split(x$Q, x$day)
  prum <- sapply(dtsplit, mean)
  return(round(prum, 2))
}

tail(denniprum(x = dtQ), 20)

# 3
## Příprava
set.seed(1)
x = sample(1:6, 20, TRUE)
x

counntOccurAll <- function(x){
  vysledek <- c()
  for (i in unique(x)) {
    n <- x %in% i
    y <- length(n[n == TRUE])
    vysledek[i] <- y
  }
  df <- data.frame(Hodnota = sort(unique(x)),
                   Pocet = vysledek)
  return(df)
}
counntOccurAll(x)

# 4
i = 2
x = 10
fibo <- function(x) {
  if (x < 3) {
    n <- c()
    n[c(1, 2, 3)] <- c(0, 1, 1)
  } else{
    n <- c()
    n[c(1, 2, 3)] <- c(0, 1, 1)
    for (i in 4:x) {
      n[i] <- n[i - 1] + n[i - 2]
    }
  }
  return(n[x])
}

fibo(19)

