# Wgranie bibliotek
library(R6)
library(ggplot2)

# Definicja klasy domain
domain <- R6Class("domain",
                    public = list(
                    data = NULL,          
                    resolution = NULL,    
                    plot_object = NULL,   
                    # Inicjalizacja obiektu klasy domain
                    initialize = function(data, resolution) {
                      self$data <- data
                      self$resolution <- resolution
                      self$plot_object <- ggplot(self$data, aes(x=x, y=y)) + 
                        theme_classic() +   
                        theme(legend.position = "none")
                    },
                    
                    plotDomain = function(resolutionIndex, col, border, ...) {
                      if (is.null(self$plot_object)) {
                        self$plot_object <- ggplot(self$data, aes(x=x, y=y))                        
                      }
                      # obliczenie zakresu danych
                      x_range <- range(self$data$x)
                      y_range <- range(self$data$y)
                      
                      # obliczenie maksymalnego zakresu
                      max_range <- max(x_range[2] - x_range[1], y_range[2] - y_range[1])
                      
                      # obliczenie wielkosci hexow
                      binwidth <- max_range / self$resolution[resolutionIndex]
                      # dodanie hexow do wykresu
                      self$plot_object <- self$plot_object + 
                        geom_hex(
                          aes(),
                          fill = col,
                          binwidth = binwidth, 
                          color = border, 
                          alpha = 0.3,
                          ...
                        )
                    },
                    # Dodanie punktow do wykresu
                    plotPoints = function(pch, cex, col) {
                      if (is.null(self$plot_object)) {
                        self$plot_object <- ggplot(self$data, aes(x=x, y=y))
                      }
                      self$plot_object <- self$plot_object + 
                        geom_point(
                          aes(),
                          color = col, 
                          size = cex, 
                          pch = pch
                        )
                    },
                    # Wyswietlenie wykresu
                    showPlot = function() {
                      self$plot_object <- self$plot_object + 
                        coord_fixed(ratio = 1)
                      print(self$plot_object)
                    }
                  ))

# Wylaczenie wyswietlania wykresow
graphics.off()

# Funkcja przetwarzajaca plik i generujaca wykres
generate_plot <- function(filename) {
  # Indicate that the plot is being generated
  cat(sprintf("Generowanie wykresu dla pliku: %s...\n", filename))
  
  # Wczytaj dane z pliku
  data <- read.csv(file = filename)
  
  # Inicjalizacja obiektu klasy domain
  hexmap <- domain$new(data = data, resolution = c(10, 20, 45))
  
  # Warstwa 1: Hexy w kolorze zielonym
  hexmap$plotDomain(resolutionIndex = 1, col = "#94A684", border = "#94A684")
  
  # Warstwa 2: Hexy w kolorze szarym
  hexmap$plotDomain(resolutionIndex = 2, col = "#393E46", border = "#393E46")
  
  # Warstwa 3: Hexy w kolorze niebieskim
  hexmap$plotDomain(resolutionIndex = 3, col = "#41749f", border = "#41749f")
  
  # Warstwa 4: Punkty w kolorze czerwonym
  hexmap$plotPoints(pch = 20, col = "#952323", cex = 3)
  
  # Wyswietl wykres
  hexmap$showPlot()
  cat("Wykres został wygenerowany.\n\n")
}

# Lista plikow do przetworzenia
files <- c("./test_data_00.csv", "./test_data_01.csv", "./test_data_02.csv")

# Petla przetwarzajaca pliki
for (file in files) {
  generate_plot(file)
}

