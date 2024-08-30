# Zuerst die benötigten Bibliotheken laden
library(shiny)
library(dplyr)
library(ggplot2)

# Benutzeroberfläche der Shiny-App definieren
ui <- shinyUI(fluidPage(withMathJax(),
                        titlePanel(span("Prognose", style="color:white"),
                                   windowTitle="Prognose"),
                        
                        sidebarLayout(position="right",
                                      sidebarPanel(
                                        wellPanel(style = "background-color: #FFFFFF;", h5("Bedienfenster"),
                                                  sliderInput("n", "Beobachtungsumfang \\( (N) \\)", value = 3, min = 3, max = 500),
                                                  sliderInput("x0", "Wert für Prognosepunkt \\( (x_{0}) \\)", value = 30, min = 1, max = 500),
                                                  sliderInput("alpha_level", "Signifikanzniveau \\( (\\%) \\)", value = 0.05, min = 0.01, max = 0.5, step = 0.01),
                                                  br(),
                                                  sliderInput("range", "Wähle das Intervall:",value = c(10,50), min = 1, max = 200),
                                                  br(),
                                                  shinysky::actionButton("sample", "Stichprobenerzeugung", styleclass="success"),
                                                  downloadButton('downloadPlot', 'Grafik herunterladen')),
                                                  br(),
                                                  br(),
                                        
                                        wellPanel(p(strong("Redaktion"), style='margin-bottom:1px;color:black;'),
                                                  HTML("<p style='margin-bottom:1px;color:black;'>Programmierung: Luca Grün</p>"),
                                                  p("Text: Ludwig von Auer", style="color:black"),
                                                  HTML("<a , style='margin-bottom:1px;color:black;' href = 'https://www.uni-trier.de/index.php?id=50126' target='_blank'>Professur für Finanzwissenschaft</a>"),
                                                  p("Fachbereich IV - Volkswirtschaftslehre", style = 'margin-bottom:1px;color:black;'),
                                                  p("Universität Trier", style="color:black"),
                                                  p(strong("Lehrbuch"), style = 'margin-bottom:1px; color:black;'),
                                                  HTML("<p style = 'color:black;'>Auer, L. <a href = 'https://www.uni-trier.de/index.php?id=15929' target='_blank'><img src='buch_klein2.jpg' style='float: right;'></a>von (2023)<br>
                    <a href = 'https://www.uni-trier.de/index.php?id=15929' target='_blank' style='color:black'>Ökonometrie - eine Einführung<br>
                    8. Auflage, Springer-Gabler<a/> </p>"),
                                                  
                                                  p(strong("Arbeitsbuch"), style = 'margin-bottom:1px; color:black;'),
                                                  HTML("<p style = 'color:black;'><a href = 'https://www.uni-trier.de/index.php?id=15929' target='_blank'><img src='übuch_klein2.jpg' style='float: right;'></a>Auer, L. von, Hoffmann, S., Kranz, T. (2024)<br>
                    <a href = 'https://www.uni-trier.de/index.php?id=15929' target='_blank' style='color:black'>Ökonometrie - Das R Arbeitsbuch<br>
                    2. Auflage, Springer-Gabler<a/> </p>"),
                                                  
                                                  br(),
                                                  br(),
                                                  HTML('<div class="btn-group dropup">
                                <a class="btn btn-info dropdown-toggle" data-toggle="dropdown" href="#">
                                Weitere Animationen
                                <span class="caret"></span>
                                </a>
                                <ul class="dropdown-menu">
                                <p style="margin-bottom:1px;"><a href="https://oekonometrie.shinyapps.io/Stoergroessen/" target="_blank">&nbsp; Störgrößen</a></p>
                                <p style="margin-bottom:1px;color:black;"><a href="https://oekonometrie.shinyapps.io/WiederholteStichproben/" target="_blank">&nbsp; KQ-Schätzer</p>
                                <a href="https://oekonometrie.shinyapps.io/Intervallschaetzer/" target="_blank">&nbsp; Intervallschätzer</a>
                                <p style="margin-bottom:1px;"><a href="https://oekonometrie.shinyapps.io/t-Test/" target="_blank">&nbsp; t-Test</a></p>
                                <p style="margin-bottom:1px;"><a href="https://oekonometrie.shinyapps.io/t-Test_versus_F-Test/" target="_blank">&nbsp; t-Test versus F-Test</a></p>
								
                                </ul>
                                </div>')),
                                        list(tags$head(tags$style("body {background-color: #6d6d6d; }")))
                                      ),
                                      
                                      mainPanel(
                                        wellPanel(wellPanel(style = "background-color: #FFFFFF;",plotOutput("regressionPlot",height = "550px"))),
                                        wellPanel(style = "background-color: #DEEBF7;",tabsetPanel(type = "pills",                                                        
                                                                                                   tabPanel(h5("Was wird veranschaulicht?"),p("Nachdem der Zusammenhang zwischen den zwei Variablen Rechnungsbetrag und Trinkgeld verlässlich geschätzt wurde,
kann man für jeden neuen Gast eine Prognose über das zu erwartende Trinkgeld aufstellen. 
Es handelt sich hier um eine bedingte Prognose, denn sie gilt nur unter der Annahme, dass die exogene Variable \\( x_i \\) 
einen bestimmten Wert \\( x_0 \\) annimmt, der vor der eigentlichen Prognose feststhet. In dieser Animation sehen Sie die aus einer Stichprobe 
ermittelte Regressionsgerade sowie den für einen vorgegebenen Wert \\(x_{0} \\) prognostizierten Wert \\( \\hat{y}_0 \\)
und den tatsächlich eintretenden Wert \\( y_{0} \\). Anhand dieser beiden Punkte lässt sich der Prognosefehler 
\\( ( \\hat{y}_0 - y_{0} ) \\) veranschaulichen, welcher genutzt wird um ein Prognoseintervall aufzustellen.
Die Breite des Prognoseintervalls hängt von folgenden Paramtern ab: Beobachtungsumfang, Signifikanzniveau und die Streuung 
der \\( x \\)-Werte. Diese sind für Sie frei veränderbar, sodass Sie deren Einfluss auf die Breite des Intervalls studieren können.", style="color:black") ),
                                                                                                   
                                                                                                   tabPanel(h5("Was zeigt die Anfangseinstellung?"), 
                                                                                                            p(HTML("<p style='color:black;'>Die Animation greift das Trinkgeld-Beispiel des Lehrbuches auf. 
Für jeden Gast \\( i \\) wird das beobachtete Trinkgeld \\( y_{i} \\) durch den Rechnungsbetrag 
\\( x_{i} \\) erklärt: 
$$y_{i}=α+βx_{i}+u_{i}$$ 
In der Anfangseinstellung wurde eine Stichprobe mit 3 Beobachtungen genereiert. Diese sind in der obigen Grafik
durch die schwarzen Punkte gekennzeichnet. Daraus wurde eine Regressionsgerade mit den 
ermittelten Schätzern \\( \\hat{\\alpha} \\) und \\( \\hat{\\beta} \\) sowie eine Prognose
\\( \\hat{y}_0 \\) für den vorgegebenen \\( x_{0} \\)-Wert aufgestellt. Dieser Punkt ist durch den
weiß ausgefüllten Punkt auf der Regressionsgerade markiert.</p>"), 
                                                                                                              
                                                                                                              HTML('<p style="color:black;">Der tatsächlich eintretende \\( y \\)-Wert
für den Wert der Anfangseinstellung \\( x_{0} = 30 \\) ist jedoch \\( y_{0} = 4.4 \\). Er ist
als rot ausgefüllter Punkt in der Grafik zu sehen. Die Differenz zwischen dem Prognosepunkt \\( \\hat{y}_0 \\)
und dem tatsächlich eintretenden Wert \\( y_{0} \\) bezeichnet man als Prognosefehler \\( ( \\hat{y}_0 - y_{0} ) \\). </p>'), 
                                                                                                              
                                                                                                              HTML('<p style="color:black;"> Für den Prognosefehler lässt sich anhand folgender Formel die Standardabweichung bestimmen:
\\[ \\hat{\\text{sd}}(\\hat{y}_0 - y_0) \\ = \\sqrt{\\hat{\\text{var}}(\\hat{y}_0 - y_0)} = \\sqrt{\\hat{\\sigma}^2 \\left(1 + \\frac{1}{N} + \\frac{(x_0 - \\bar{x})^2}{S_{xx}}\\right)} \\]
Anhand dieser kann man ein Prognoseintervall aufstellen. Das Prognoseintervall beträgt: \\[ \\left[ \\hat{y}_0 - t_{\\alpha/2} \\cdot \\hat{\\text{sd}}(\\hat{y}_0 - y_0) ; \\hat{y}_0 + t_{\\alpha/2} \\cdot \\hat{\\text{sd}}(\\hat{y}_0 - y_0) \\right] \\]
</p>'),
                                                                                                              
                                                                                                              HTML('<p style="color:black;">Das sich im Beispiel 
ergebende Prognoseintervall ist farblich rot hervorgehoben. Ein solches Prognoseintervall 
ließe sich für jeden beliebigen \\( x \\)-Wert berechnen. Die graue Fläche zeigt die 
Gesamtheit dieser Intervalle. Die beiden schwarzen Kurven ergeben sich druch die Endpunkte der Intervalle.</p>') )
                                                                                                            
                                                                                                   ),
                                                                                                   
                                                                                                   tabPanel(h5("Benutzungshinweise"), 
                                                                                                            p(HTML("Sie können auch eine eigene Prognose durchführen. Im Bedienfenster
sehen Sie verschiedene Schieber <img src='slider.jpg'>, mit denen Sie die
Parameterwerte der Prognose verändern können. Klicken Sie dafür mit der
linken Maustaste auf den entsprechenden Schieber und bewegen sie ihn nach
rechts oder links. Klicken Sie anschließend wieder auf den Knopf <img src='stich.jpg'>."),
                                                                                                              HTML('<ul><li style="color:black;">Der <strong>Beobachtungsumfang-Schieber</strong> 
gibt an, wie
viele Beobachtungen der Stichprobe zugrunde liegen. Eine Erhöhung des 
Stichprobenumfangs verkleinert die Breite der Prognoseintervalle. (Achsenabschnitte beachten!) </li></ul>'),
                                                                                                              
                                                                                                              HTML('<ul><li style="color:black;"> Mit dem <strong>Prognosepunkt-Schieber</strong> 
können Sie selbst den \\( x_{0} \\)-Wert vorgeben, für den der tatsächlich eintretende Wert \\( y_{0} \\) 
generiert wird und überprüfen ob dieser innerhalb des berechneten Prognoseintervalls liegt. </li></ul>'),
                                                                                                              HTML('<ul><li style="color:black;">Mit dem <strong>Signifikanzniveau-Schieber</strong> 
stellen Sie ein, wie groß die
Wahrscheinlichkeit sein soll, dass der tatsächlich eintretende Wert für den Prognosepunkt außerhalb
des Prognoseintervalls liegt. Eine Erhöhung des 
Signifikanzniveaus verkleinert die Breite der Prognoseintervalle (Achsenabschnitte beachten!) </li></ul>'), 
                                                                                                               
                                                                                                              HTML('<ul><li style="color:black;">Der <strong>Intervall-Schieber</strong> 
gibt an, aus welchem Intervall die generierten Werte der Stichprobe stammen.
Je breiter dieses Intervall ist, desto kleiner wird die Breite der Prognoseintervalle.</li></ul>'), 
                                                                                                              HTML('<ul><li style="color:black;"> Um die aktuelle Grafik in einer jpg-Datei zu
speichern, klicken
Sie das Feld <img src="download.jpg"> an.</li></ul>'), 
                                                                                                              HTML("<p style='color:black;'>Um Animationen
zu anderen ökonometrischen Themen zu sehen, klicken Sie bitte auf <img src =
'info.jpg'>.</p>")), style="color:black"),
                                                                                                   
                                                                                                   tabPanel(h5("Details"),
                                                                                                            p(HTML('<p style="color:black;">Der Stichprobenerzeugung liegen die 
„wahren" Parameterwerte \\( \\alpha=0.25 \\) , \\( \\beta=0,125 \\) 
zugrunde. Die  \\( x \\)-Werte werden aus einer Gleichverteilung innerhalb des vorgegebenen Intervalls erzeugt.
Die Störgrößenvarianz beträgt \\( \\sigma^2 = 2 \\). </p>'),HTML("<p
style='color:black;'>Die entsprechenden R-Skripte für die Reproduktion dieser
Seite sind unter folgendem Link aufrufbar: <a href='https://github.com/Oekonometrie-Lernen/t-Test-versus-F-Test' target='_blank'>R
Codes.</a></p>")
                                                                                                              
                                                                                                            ))
                                        )),
                                      )
                        )
)
)

# Serverlogik der Shiny-App definieren
server <- function(input, output) {
  # Definition eines reactiveVal-Objekts, um den Zustand der Berechnungen zu speichern
  perform_calculation <- reactiveVal(TRUE)
  
  # Automatische Ausführung beim Laden der App
  observe({
    if (perform_calculation()) {
      n <- input$n
      x0 <- input$x0
      alpha <- input$alpha_level
      x_min <- input$range[1]
      x_max <- input$range[2]
      
      # Sicherstellen, dass x_min kleiner ist als x_max
      if (x_min >= x_max) {
        x_min <- x_max - 1
      }
      
      # Ziehen einer Stichprobe gleichverteilter x-Werte innerhalb des gegebenen Intervalls
      set.seed(123)
      x <- runif(n, min = x_min, max = x_max)
      
      # Zufällige Generierung der Störgrößenvarianz
      sigma2 <- 2 # Fester Wert für σ²
      
      epsilon <- rnorm(n, mean = 0, sd = sqrt(sigma2))
      
      # Wahre Werte von Alpha und Beta
      beta_true <- 0.125
      alpha_true <- 0.25
      
      # Berechnung von y basierend auf den wahren Alpha- und Beta-Werten
      y <- alpha_true + beta_true * x + epsilon
      
      # Durchführung der einfachen linearen Regression
      model <- lm(y ~ x)
      
      # Berechnung des wahren y-Werts für den eingegebenen x0-Wert
      epsilon_x0 <- rnorm(1, mean = 0, sd = sqrt(sigma2))
      y_true <- alpha_true + beta_true * x0 + epsilon_x0
      
      # Berechnung der geschätzten Störgrößenvarianz
      residuals <- model$residuals
      df <- model$df.residual
      sigma2_hat <- sum(residuals^2) / df
      
      # Erweiterung des Intervalls für x_seq, um sicherzustellen, dass x0 enthalten ist
      x_seq <- seq(min(c(x, x0)), max(c(x, x0)), length.out = 100)
      y_pred <- predict(model, newdata = data.frame(x = x_seq))
      x_bar <- mean(x)
      Sxx <- sum((x - x_bar)^2)
      var_forecast_error <- sigma2_hat * (1 + 1/n + ((x_seq - x_bar)^2 / Sxx))
      sd_forecast_error <- sqrt(var_forecast_error)
      t_alpha_half <- qt(1 - alpha/2, df = n-2)
      lower_bound <- y_pred - t_alpha_half * sd_forecast_error
      upper_bound <- y_pred + t_alpha_half * sd_forecast_error
      
      # Überprüfung, ob der wahre y-Wert innerhalb des Prognoseintervalls liegt
      y_hat_x0 <- predict(model, newdata = data.frame(x = x0))
      var_forecast_error_x0 <- sigma2_hat * (1 + 1/n + ((x0 - x_bar)^2 / Sxx))
      sd_forecast_error_x0 <- sqrt(var_forecast_error_x0)
      lower_bound_x0 <- y_hat_x0 - t_alpha_half * sd_forecast_error_x0
      upper_bound_x0 <- y_hat_x0 + t_alpha_half * sd_forecast_error_x0
      
      within_interval <- (y_true >= lower_bound_x0) & (y_true <= upper_bound_x0)
      
      # Ergebnisse anzeigen
      output$regressionPlot <- renderPlot({
        data <- data.frame(x = x, y = y)
        prediction_data <- data.frame(x = x_seq, y = y_pred, lower = lower_bound, upper = upper_bound)
        
        ggplot(data, aes(x = x, y = y)) +
          geom_point() +
          geom_line(data = prediction_data, aes(x = x, y = y), color = "black", linetype = "dotted", size = 1) +
          geom_ribbon(data = prediction_data, aes(x = x, ymin = lower, ymax = upper), alpha = 0.2, fill = "darkgrey") +
          geom_line(data = prediction_data, aes(x = x, y = lower), colour = "black") +
          geom_line(data = prediction_data, aes(x = x, y = upper), colour = "black") +
          geom_point(aes(x = x0, y = y_true), color = "red", size = 3) +
          geom_linerange(aes(x = x0, ymin = lower_bound_x0, ymax = upper_bound_x0), colour = "red") +
          geom_point(aes(x = x0, y = y_hat_x0), color = "red", size = 3, shape = 21, fill = "white") +
          labs(title = "Regression und Prognoseintervall",
               x = "x",
               y = "y") +
          theme_minimal() +
          theme(
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18)
          )
      })
      
      # Setze perform_calculation auf FALSE, um weitere automatische Berechnungen zu verhindern
      perform_calculation(FALSE)
    }
  })
  
  # Reagiert auf das Klicken des Buttons und führt die Berechnung durch
  observeEvent(input$sample, {
    perform_calculation(TRUE) # Setze den Zustand zurück, damit die Berechnungen erneut durchgeführt werden
    
    n <- input$n
    x0 <- input$x0
    alpha <- input$alpha_level
    x_min <- input$range[1]
    x_max <- input$range[2]
    
    # Der restliche Code für die Berechnung bleibt gleich...
    
    # ...führe die gleichen Berechnungen wie oben durch und aktualisiere die Plot-Anzeige
  })
}


# Shiny-App starten
shinyApp(ui = ui, server = server)

