setwd("../Vyuka_PZD/Cv8/")                     # nastavení pracovního adresáře

# načtení seznamu souborů z podsložky
soubory <- list.files("DataCv8/")

# funkce pro načtení dat všech povodí
soubor <- soubory[1]
nactiData <- function(soubory) {
  povodi <- list()                             # vytvoření prázdného listu pro data
  for (soubor in soubory) {                    # cyklus přes všechny soubory
    povodi[[soubor]] <- read.table(paste0("DataCv8/", soubor), header = T)  # načtení tabulky
    povodi[[soubor]][povodi[[soubor]] == (-99)] <- NA   # nahrazení -99 hodnotami NA
    povodi[[soubor]]$Date <- as.Date(povodi[[soubor]]$Date) # převedení sloupce Date na datum
  }
  names(povodi) <- substring(soubory, 1, nchar(soubory) - 4) # pojmenování dat podle názvu souborů bez .txt
  return(povodi)                               # vrácení listu s daty
}

data <- nactiData(soubory)                     # načtení všech dat
str(data)                                      # kontrola struktury dat
        
# 2 — jednoduché scatter ploty
par(mfrow = c(1,3), pty="s")                   # rozdělení grafického okna na 1×3 panely
plot(x = data$ConchasRiver$Q,                  # průtok jako x
     y = data$ConchasRiver$P,                  # srážky jako y
     xlab = "Q[mm/den]",                       # popis osy X
     ylab = "P[mm/den]",                       # popis osy Y
     main = "ConchasRiver")                    # název grafu

# funkce na kreslení scatter plotů všech povodí
plotRivers <- function(data, x = "NULL", y = "NULL"){
  for (i in 1:length(data)){                   # cyklus přes všechna povodí
    plot(data[[i]][[x]], data[[i]][[y]],       # vykreslení x vs y
         xlab = "Q [mm/den]",                  # popis osy X
         ylab = "P [mm/den]",                  # popis osy Y
         main = names(data[i]))                # název podle jména povodí
  }
}  

plotRivers(data = data, "Q", "P")              # spuštění funkce, pokud bych měnil vyobrazované proměnné, musel bych vytvořit dynamický popis os

# 3 — časové řady s průměrnou hodnotou
par(mfrow = c(2,3))                           # nastavení plochy na 2×3 grafy

plotrRivers3 <-
  function (data,
            ab_leg = FALSE,                   # přidat legendu? (TRUE/FALSE)
            dtmRange = NULL,                  # časový rozsah (volitelné)
            addTitle = NULL) {                # dodatečný text do názvu (např. ", rok 1990")
    
    if (is.null(dtmRange) == FALSE) {         # pokud je časový rozsah zadán
      for (i in 1:length(data)) {             # cyklus přes všechna povodí
        
        # výběr jen těch řádků, které spadají do zadaného rozsahu dat
        data[[i]] <- data[[i]][data[[i]]$Date >= dtmRange[1] &
                                 data[[i]]$Date <= dtmRange[2], ]
        
        # vykreslení časové řady průtoku Q
        plot(
          data[[i]]$Date,                     # osa X = datum
          data[[i]]$Q,                        # osa Y = průtok
          xlab = "Čas [den]",                 # popis osy X
          ylab = "Q [mm/den]",                # popis osy Y
          
          # název grafu – buď název povodí, nebo název + doplněk addTitle
          main = if (is.null(addTitle) == FALSE) {
            paste0(names(data[i]), as.character(addTitle))
          } else {
            names(data[i])
          },
          
          type = "l"                          # typ grafu = čára
        )
        
        prum = mean(data[[i]]$Q, na.rm = TRUE)     # výpočet průměrného průtoku
        abline(h = prum, col = "red", lty = 1)     # horizontální čára = průměr
        
        if (ab_leg == TRUE) {                       # pokud má být legenda
          legend(
            x = "topright",                         # umístění legendy
            legend = c("Q", "meanQ"),               # texty legendy
            lty = c(1, 1),                          # typy čar
            col = c("black", "red")                 # barvy čar
          )
        }
      }
      
    } else {                                        # pokud není časový rozsah zadán
      
      for (i in 1:length(data)) {                   # cyklus přes všechna povodí
        
        # vykreslení celé časové řady Q (bez filtrování dat)
        plot(data[[i]]$Date, data[[i]]$Q,
             xlab = "Čas [den]",
             ylab = "Q [mm/den]",
             
             # název grafu – opět s volitelným doplňkem addTitle
             main = if (is.null(addTitle) == FALSE){
               paste0(names(data[i]), as.character(addTitle))
             } else {
               names(data[i])
             },
             
             type = "l")                           # typ grafu = čára
        
        prum = mean(data[[i]]$Q, na.rm = TRUE)     # průměrný průtok
        abline(h = prum, col = "red", lty = 1)     # červená čára = průměr
        
        if (ab_leg == TRUE) {                      # legenda, pokud je povolena
          legend(
            x = "topright",
            legend = c("Q", "meanQ"),
            lty = c(1, 1),
            col = c("black", "red")
          )
        }
      }
    }
  }


plotrRivers3(data, ab_leg = FALSE, dtmRange = c("1990-01-01", "1990-02-01"), addTitle = "Leden, 1990")             # vykreslení všech časových řad s legendou

plotrRivers3(data,
             ab_leg = TRUE,
             dtmRange = c("1990-01-01", "1990-12-31"), # omezení na rok 1990
             addTitle = ", rok 1990" )                 # doplněk do názvu

# 4 — boxploty pro srážky a evapotranspiraci
par(mfrow = c(1,2), mar = c(6, 4, 4, 2) + 0.1)         # nastavení okna pro 2 grafy vedle sebe

RiverBoxplot <- function(data){
  boxplot(data[[1]]$P, data[[2]]$P, data[[3]]$P,        # boxplot srážek
          outline = FALSE,
          ylab = "P[mm/den]",
          names = names(data),
          las = 2)                                     # otočení popisů osy X
  
  boxplot(data[[1]]$PET, data[[2]]$PET, data[[3]]$PET,  # boxplot PET
          outline = FALSE,
          ylab = "PET[mm/den]",
          names = names(data),
          las = 2)
}
RiverBoxplot(data)                                      # vykreslení boxplotů

saveRDS(data, "data_povodi.rds")                        # uložení dat jako RDS soubor


