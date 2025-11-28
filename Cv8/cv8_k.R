setwd("../Vyuka_PZD/Cv8/")
#nacteni souboru
soubory <- list.files("DataCv8/")
# soubor <- soubory[1]
nactiData <- function(soubory) {
  povodi <- list()
  for (soubor in soubory) {
    povodi[[soubor]] <- read.table(paste0("DataCv8/", soubor), header = T)
    povodi[[soubor]][povodi[[soubor]]== (-99)] <- NA 
    povodi[[soubor]]$Date <- as.Date(povodi[[soubor]]$Date)
  }
  return(povodi)
}

data <- nactiData(soubory)
names(data) <- substring(soubory, 1, nchar(soubory) - 4)

str(data)

#2
par(mfrow = c(1,3), pty="s")
plot(x = data$ConchasRiver$Q,
     y = data$ConchasRiver$P,
     xlab = "Q[mm/den]",
     ylab = "P[mm/den]",
     main = "ConchasRiver")

plotRivers <- function(data, x = "NULL", y = "NULL"){
  for (i in 1:length(data)){
    plot(data[[i]][[x]], data[[i]][[y]],
         xlab = "Q [mm/den]",
         ylab = "P [mm/den]",
         main = names(data[i]))
  }
}  

plotRivers(data = data, "Q", "P")

# 3
par(mfrow = c(2,3))

plotrRivers3 <-
  function (data,
            ab_leg = FALSE,
            dtmRange = NULL,
            addTitle = NULL) {
    if (is.null(dtmRange) == FALSE) {
      for (i in 1:length(data)) {
        data[[i]] <- data[[i]][data[[i]]$Date >= dtmRange[1] &
                                 data[[i]]$Date <= dtmRange[2], ]
        if (is.null(addTitle) == FALSE) {
          plot(
            data[[i]]$Date,
            data[[i]]$Q,
            xlab = "Čas [den]",
            ylab = "Q [mm/den]",
            main = paste0(names(data[i]), as.character(addTitle)),
            type = "l"
          )
        } else{
          plot(
            data[[i]]$Date,
            data[[i]]$Q,
            xlab = "Čas [den]",
            ylab = "Q [mm/den]",
            main = names(data[i]),
            type = "l"
          )
        }
        prum = mean(data[[i]]$Q, na.rm = TRUE)
        abline(h = prum,
               col = "red",
               lty = 1)
        
        if (ab_leg == TRUE) {
          legend(
            x = "topright",
            legend = c("Q", "meanQ"),
            lty = c(1, 1),
            col = c("black", "red")
          )
        }
      }
    } else{
      for (i in 1:length(data)) {
        plot(
          data[[i]]$Date,
          data[[i]]$Q,
          xlab = "Čas [den]",
          ylab = "Q [mm/den]",
          main = names(data[i]),
          type = "l"
        )
        prum = mean(data[[i]]$Q, na.rm = TRUE)
        abline(h = prum,
               col = "red",
               lty = 1)
        if (ab_leg == TRUE) {
          legend(
            x = "topright",
            legend = c("Q", "meanQ"),
            lty = c(1, 1),
            col = c("black", "red")
          )
        }
        
      }
    }
    
  }p

plotrRivers3(data, ab_leg = TRUE)
plotrRivers3(data, ab_leg = TRUE, dtmRange = c("1990-01-01", "1990-12-31"), addTitle = ", rok 1990" )


par(mfrow = c(1,2), mar = c(6, 4, 4, 2) + 0.1)
RiverBoxplot <- function(data){
  boxplot(data[[1]]$P, data[[2]]$P, data[[3]]$P, outline = FALSE, ylab = "P[mm/den]", names = names(data), las = 2)
  boxplot(data[[1]]$PET, data[[2]]$PET, data[[3]]$PET, outline = FALSE, ylab = "PET[mm/den]", names = names(data), las = 2)
}
RiverBoxplot(data)

saveRDS(data, "data_povodi.rds")


mean(c(0.001, 4.5))
