# 1

povodi <- readRDS("~/OneDrive - CZU v Praze/github_projects/Vyuka_PZD/Cv8/data_povodi.rds") #načtení dat z minulého cvičení

# vytvoření průměrných hodnot z jednotlivých dataframů
P   <- c(mean(povodi[[1]]$P, na.rm = TRUE),
         mean(povodi[[2]]$P, na.rm = TRUE),
         mean(povodi[[3]]$P, na.rm = TRUE))

PET <- c(mean(povodi[[1]]$PET, na.rm = TRUE),
         mean(povodi[[2]]$PET, na.rm = TRUE),
         mean(povodi[[3]]$PET, na.rm = TRUE))

Q   <- c(mean(povodi[[1]]$Q, na.rm = TRUE),
         mean(povodi[[2]]$Q, na.rm = TRUE),
         mean(povodi[[3]]$Q, na.rm = TRUE))

mat <- cbind(P, PET, Q)     #spojení do matice
rownames(mat) <- c(names(povodi[1]), names(povodi[2]), names(povodi[3]))#jména řádků matice


barplot(mat, beside = TRUE,       #vytvářím barplot, beside určuje jestli jsou hodnoty vedle sebe nebo nad sebou
        legend.text = rownames(mat), #legenda podle jmen řádků v matice
        main = "Průměrné hodnoty", #název grafu
        ylab = "mm/day")      #popis y osy

# -------------zabalení do funkce-------------
plotMeanBar <- function(povodi){
  P   <- c(mean(povodi[[1]]$P, na.rm = TRUE),
           mean(povodi[[2]]$P, na.rm = TRUE),
           mean(povodi[[3]]$P, na.rm = TRUE))
  
  PET <- c(mean(povodi[[1]]$PET, na.rm = TRUE),
           mean(povodi[[2]]$PET, na.rm = TRUE),
           mean(povodi[[3]]$PET, na.rm = TRUE))
  
  Q   <- c(mean(povodi[[1]]$Q, na.rm = TRUE),
           mean(povodi[[2]]$Q, na.rm = TRUE),
           mean(povodi[[3]]$Q, na.rm = TRUE))
  
  mat <- cbind(P, PET, Q)    
  rownames(mat) <- c(names(povodi[1]), names(povodi[2]), names(povodi[3]))
  
  
  barplot(mat, beside = TRUE,   
          legend.text = rownames(mat),
          main = "Průměrné hodnoty", 
          ylab = "mm/day")      
}

plotMeanBar(povodi)

#2
plotVarTime <- function(data, name, var, dtmRange){
  df <- data[[name]]
  
  df <- df[df$Date >= dtmRange[1] &
        df$Date <= dtmRange[2], ]
  if(var == "Q"){
    namevar <- "Průtok"
  }else if(var == "P"){
    namevar <- "Srážka"
  }else{
    namevar <- "PET"
  }
  
  plot(x = df$Date,
       y = df[, var],
       xlab = "Čas",
       ylab = paste (var),
       main = paste(name, ", ", namevar, ", ", dtmRange[1], "až", dtmRange[2]), 
       type = "l")
  
}

plotVarTime(data = povodi, name = "SlateRiver", var = "Q", dtmRange = c("1989-01-01", "1991-01-01"))







# 3
df <- povodi[[1]]

df$month_day <- format(df$Date, format = "%m-%d")
df_noleap <- df[df$month_day != "02-29", ]

df_noleap$year <- format(df_noleap$Date, format = "%Y")

df_split <- split(df_noleap, df_noleap$year)

dates <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = "day")



i = "1950"


for (i in seq_along(names(df_split))) {
  if (i == 1) {
    plot(x = dates, y = df_split[[1]]$P, type = "l", alpha = 0.5)
  }else{
    lines(x = dates, y = df_split[[i]]$P, ylim = max(df_split$P))
  }
}

plotYear <- function(data, name = "NULL", var = "NULL"){
  df <- data[[name]]
  
  df$month_day <- format(df$Date, format = "%m-%d")
  df_noleap <- df[df$month_day != "02-29", ]
  df_noleap$year <- format(df_noleap$Date, format = "%Y")
  df_split <- split(df_noleap, df_noleap$year)
  
  dates <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = "day")
  plot(x = dates, y = df_split[[1]]$P, type = "l")
  
  for (i in seq_along(names(df_split))) {
    if (i == 1) {
      plot(x = dates, y = df_split[[1]][, var], type = "l",
           xlab = "Den",
           ylab = paste(var),
           main = paste(name),
           ylim = range(df[, var], na.rm = TRUE),
           col = "grey")
    }else{
      lines(x = dates, y = df_split[[i]][, var], col = "grey")
    }
  }
  
  
}

plotYear(data = povodi, name = "MattaponiRiver", var = "Q")

range(df[, "Q"], na.rm = TRUE)

