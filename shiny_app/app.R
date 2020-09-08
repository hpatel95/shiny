############################################################################
# Package
############################################################################
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(ggplot2)
library(DT)
library(dplyr)
library(fmsb)
library(tidyr)

############################################################################
# Database connection
############################################################################

#DF PRINCIPALE
test_DF <- read.csv2("C:/Users/Haris/Desktop/stages_janvier_fevrier_2020/hal_data.csv") #entrer ici le chemin d'acces de la base de donne hal_data

#Creation df pour Spider chart
spider <- as.data.frame(matrix( sample( 2:20 , 5 , replace=T) , ncol=5))
colnames(spider) <- c("Fievre" , "Neurologie" , "Traumatologie" , "Respiratoire" , "Non_renseigne")

# Ajustement pour compatibilite avec le package fmsb, ajout de 2 lignes, max et min de chaque motif 
spider <- rbind(rep(20,5) , rep(0,5) , spider)

############################################################################
# UI
############################################################################

ui <- navbarPage("TEST OPTIM - Mai 2020 - HP", theme = shinytheme("darkly"),   #differents themes disponibles   sur github de shinythemes
                 tabPanel("Jour",
                          fluidRow(
                              dateInput('choix_jour',
                                        label = 'Choix du jour',
                                        value = Sys.Date(), format = "dd-mm-yyyy", language = "fr", weekstart = 1 #ajouter un max
                              ),
                              box(title = "Motifs d'entree", solidHeader = T,
                                  width = 6, collapsible = F,
                                  plotOutput("spider_plot")),
                              box(title = "Nombre de patients", solidHeader = T,
                                  width = 6, collapsible = F,
                                  plotlyOutput("plotly_plot")),
                              box(title = "Examens prescrits", solidHeader = T,
                                  width = 6, collapsible = F,
                                  plotOutput("exam_plot")),
                              box(title = "Tension", solidHeader = T,
                                  width = 6, collapsible = F,
                                  plotOutput("tension_plot"))
                              
                          )),
                 tabPanel("Semaine",
                          fluidRow(
                              dateRangeInput('choix_semaine',
                                                  label = 'Choix de la semaine',
                                                  start = Sys.Date() -7, end = Sys.Date(), separator = "au", format = "dd-mm-yyyy", language = "fr", weekstart = 1  #ajouter un max
                          ),
                              box(title = "Motifs d'entree", solidHeader = T,
                                  width = 6, collapsible = F,
                                  plotOutput("spider2_plot")),
                              box(title = "Nombre de patients", solidHeader = T,
                                  width = 6, collapsible = F,
                                  plotlyOutput("plotly2_plot")),
                              box(title = "Examens prescrits", solidHeader = T,
                                  width = 6, collapsible = F,
                                  plotOutput("exam2_plot")),
                              box(title = "Tension", solidHeader = T,
                                  width = 6, collapsible = F,
                                  plotOutput("tension2_plot"))
                              
                          )),
                 tabPanel("Annuel",
                          fluidRow(
                              selectInput("choix_annee", "Choix de l'annee", choices = c(
                                  "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")   #faire une liste pour les annees?
                              ),
                              box(title = "Motifs d'entree", solidHeader = T,
                                  width = 6, collapsible = F,
                                  plotOutput("spider3_plot")),
                              box(title = "Nombre de patients", solidHeader = T,
                                  width = 6, collapsible = F,
                                  plotlyOutput("plotly3_plot")),
                              box(title = "Examens prescrits", solidHeader = T,
                                  width = 6, collapsible = F,
                                  plotOutput("exam3_plot")),
                              box(title = "Tension", solidHeader = T,
                                  width = 6, collapsible = F,
                                  plotOutput("tension3_plot"))
                              
                          )),
                 
                 tabPanel("A propos",
                          p("Utilisation", style = "font-size:25px"),
                          p("Naviguez entre les differents onglets de votre choix, Jour, Semaine ou Annee et visualisez les differents graphiques correspondants..."),
                          p("Selectionner la date souhaitee : jours, semaines, annees..."),
                          p(a("Contact - CIC IT Lille", href="http://www.cic-it-lille.com/", target="_blank"),style = "font-size:25px"))
)




############################################################################
# Server
############################################################################

# 
server <- function(input, output, session) {
    
    ##################### TRAVAIL DF (plot nbre patients)
    pbarre <- test_DF %>% 
        group_by (annee) %>% 
        summarise(moyenne_age = mean(age, na.rm = TRUE)) #(na.rm Pas compter les NAs)
    #####################
    
    ##################### TRAVAIL DF (plot exam prescrit et tension)
    test_DF <- test_DF %>% 
        replace_na(list(nb_dese_anesth=0,
                        moy_dese_anesth=0,
                        nb_seve_anesth=0,
                        moy_seve_anesth=0))
    
    ### B. Creer variable desflurane, = 0 ou = 1.
    
    test_DF<-test_DF %>%
        mutate(desflurane = ifelse(nb_dese_anesth >= 10 & moy_dese_anesth >= 3.5, 1, 0))
    
    
    ### C. Creer variable sevoflurane, = 0 ou = 1.
    
    test_DF<-test_DF %>%
        mutate(sevoflurane = ifelse(nb_seve_anesth >= 10 & moy_seve_anesth >= 0.8, 1, 0))
    
    #### D. Creation dataframe moyenne desflurane/sevoflurane/annee
    
    moyenne_desf_sev_par_annee <- test_DF %>% 
        select(annee , desflurane, sevoflurane) %>% 
        group_by(annee) %>%
        summarize(moyenne_desf = mean(desflurane , na.rm = TRUE) ,
                  moyenne_sevo = mean(sevoflurane , na.rm = TRUE))
    ####################################
    
    ################################### #PLOTS
    
    # PLOTLY PLOT
    
    output$plotly_plot <- renderPlotly({    
        plot_ly(pbarre, x = ~annee,
                y = ~moyenne_age,
                name = "Moyenne age/annee",
                type = "bar") 
    })
    
    output$plotly2_plot <- renderPlotly({    
        plot_ly(pbarre, x = ~annee,
                y = ~moyenne_age,
                name = "Moyenne age/annee",
                type = "bar") 
    })
    
    output$plotly3_plot <- renderPlotly({    
        plot_ly(pbarre, x = ~annee,
                y = ~moyenne_age,
                name = "Moyenne age/annee",
                type = "bar") 
    })
    
    # SPIDER PLOT
    output$spider_plot <- renderPlot({ 
        radarchart(spider, axistype=1 ,
                   #poly
                   pcol=rgb(0.8,0.4,0,0.7) , pfcol=rgb(0.9,0.6,0.4,0.5) , plwd=4, #pcol=couleur ligne ; pfcol=couleur polygone ; plwd=epaisseur ligne
                   #grille
                   cglcol="grey" , cglty = 1, caxislabels=seq(0,20,5), axislabcol="grey" , #cglcol=couleur toile ; cglty=type de ligne ; caxislabels=proprtions a afficher
                   #legende
                   vlcex=0.8) #taille des legendes
    })
    
    output$spider2_plot <- renderPlot({ 
        radarchart(spider, axistype=1 ,
                   #poly
                   pcol=rgb(0.8,0.4,0,0.7) , pfcol=rgb(0.9,0.6,0.4,0.5) , plwd=4, #pcol=couleur ligne ; pfcol=couleur polygone ; plwd=epaisseur ligne
                   #grille
                   cglcol="grey" , cglty = 1, caxislabels=seq(0,20,5), axislabcol="grey" , #cglcol=couleur toile ; cglty=type de ligne ; caxislabels=proprtions a afficher
                   #legende
                   vlcex=0.8) #taille des legendes
    })
    
    output$spider3_plot <- renderPlot({ 
        radarchart(spider, axistype=1 ,
                   #poly
                   pcol=rgb(0.8,0.4,0,0.7) , pfcol=rgb(0.9,0.6,0.4,0.5) , plwd=4, #pcol=couleur ligne ; pfcol=couleur polygone ; plwd=epaisseur ligne
                   #grille
                   cglcol="grey" , cglty = 1, caxislabels=seq(0,20,5), axislabcol="grey" , #cglcol=couleur toile ; cglty=type de ligne ; caxislabels=proprtions a afficher
                   #legende
                   vlcex=0.8) #taille des legendes
    })
    # EXAM PLOT
    goverlap <- moyenne_desf_sev_par_annee %>% 
        gather(Gaz, Moyenne, moyenne_desf , moyenne_sevo)
    
    output$exam_plot <- renderPlot({
        ggplot(goverlap, aes(x = annee, y = Moyenne, fill = Gaz)) + 
            geom_bar(stat="identity" , position = position_dodge(width = 0.4), alpha = 0.8) 
    })
    
    output$exam2_plot <- renderPlot({
        ggplot(goverlap, aes(x = annee, y = Moyenne, fill = Gaz)) + 
            geom_bar(stat="identity" , position = position_dodge(width = 0.4), alpha = 0.8) 
    })
    
    output$exam3_plot <- renderPlot({
        ggplot(goverlap, aes(x = annee, y = Moyenne, fill = Gaz)) + 
            geom_bar(stat="identity" , position = position_dodge(width = 0.4), alpha = 0.8) 
    })
    # TENSION PLOT
    gstack <- moyenne_desf_sev_par_annee %>% 
        gather(Gaz, Moyenne, moyenne_desf , moyenne_sevo)
    
    output$tension_plot <- renderPlot({ 
        ggplot(gstack, aes(x = annee, y = Moyenne, fill = Gaz)) + 
            geom_bar(stat="identity") 
    })
    
    output$tension2_plot <- renderPlot({ 
        ggplot(gstack, aes(x = annee, y = Moyenne, fill = Gaz)) + 
            geom_bar(stat="identity") 
    })
    
    output$tension3_plot <- renderPlot({ 
        ggplot(gstack, aes(x = annee, y = Moyenne, fill = Gaz)) + 
            geom_bar(stat="identity") 
    })
}




# Run the application 
shinyApp(ui = ui, server = server)
