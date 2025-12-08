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
plotVarTime <- function(data, name, var, dtmRange){ # Definuje funkci plotVarTime s parametry: data (list datových rámců), name (název řeky), var (proměnná Q, P, nebo PET), dtmRange (rozsah dat)
  
  df <- data[[name]] # Vybere datový rámec podle jména řeky z listu data
  
  df <- df[df$Date >= dtmRange[1] &
             df$Date <= dtmRange[2], ] # Filtruje data, aby obsahovala jen záznamy mezi začátkem a koncem dtmRange
  
  if(var == "Q"){         # Určí popisek proměnné podle toho, zda se jedná o Q, P nebo PET
    namevar <- "Průtok"
  }else if(var == "P"){
    namevar <- "Srážka"
  }else{
    namevar <- "PET"
  }

  
  plot(x = df$Date,     # Vytvoří čárový graf s daty: osa x = datum, osa y = vybraná proměnná, hlavní titulek obsahuje jméno řeky a proměnnou
       y = df[, var],
       xlab = "Čas",
       ylab = paste (var),
       main = paste(name, ",", namevar, ",", dtmRange[1], "až", dtmRange[2]), 
       type = "l")

  
}

plotVarTime(data = povodi, name = "SlateRiver", var = "Q", dtmRange = c("1989-01-01", "1991-01-01"))        # Volá funkci plotVarTime pro řeku SlateRiver a proměnnou Q, s vybraným datovým rozsahem




# 3
df <- povodi[[1]] # Vybere první datový rámec z listu povodi

df$month_day <- format(df$Date, format = "%m-%d") # Vytvoří novou proměnnou month_day obsahující měsíc a den z data (bez roku)

df_noleap <- df[df$month_day != "02-29", ] # Odstraní přestupné dny (29. února) pro jednotnou délku roku

df_noleap$year <- format(df_noleap$Date, format = "%Y") # Vytvoří sloupec s rokem z datového sloupce Date

df_split <- split(df_noleap, df_noleap$year) # Rozdělí data podle roků do seznamu (každý rok zvlášť)

dates <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = "day") # Vytvoří sekvenci dat pro jeden rok (2019) pro jednotnou osu x


for (i in seq_along(names(df_split))) {
  if (i == 1) {
    plot(x = dates, y = df_split[[1]]$P, type = "l", alpha = 0.5) # Pro první rok vykreslí graf s čárou (s částečnou průhledností)
  }else{
    lines(x = dates, y = df_split[[i]]$P, ylim = max(df_split$P)) # Pro další roky přidá čáru do stávajícího grafu
  }
}



plotYear <- function(data, name = "NULL", var = "NULL"){ # Definuje funkci plotYear, která vykreslí časové řady pro zadanou proměnnou a řeku
  
  df <- data[[name]] # Vybere datový rámec podle jména řeky
  
  df$month_day <- format(df$Date, format = "%m-%d") #vytvářím sloupec ve formátu "mesic-den"
  df_noleap <- df[df$month_day != "02-29", ] # Odstraní přestupné dny
  df_noleap$year <- format(df_noleap$Date, format = "%Y") # Přidá sloupec s rokem
  df_split <- split(df_noleap, df_noleap$year) # Rozdělí data podle roků
  
  dates <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = "day") #vytvoření sekvence datumu roku 2019 podle zadani
  plot(x = dates, y = df_split[[1]]$P, type = "l") # Vykreslí první rok (slouží jen jako základní graf)
  
  for (i in seq_along(names(df_split))) {
    if (i == 1) { # Pro první rok vykreslí graf s osami, titulem a vhodným rozsahem y
      plot(x = dates, y = df_split[[1]][, var], type = "l",
           xlab = "Den",
           ylab = paste(var),
           main = paste(name),
           ylim = range(df[, var], na.rm = TRUE),
           col = "grey") 
    }else{
      lines(x = dates, y = df_split[[i]][, var], col = "grey")      # Pro další roky přidá čáry do grafu ve šedé barvě   
      
    }
  }
}

plotYear(data = povodi, name = "MattaponiRiver", var = "Q")   # Volá funkci plotYear pro řeku MattaponiRiver a proměnnou Q




