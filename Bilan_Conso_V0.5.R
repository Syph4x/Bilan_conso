#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(Dict)
library(ggplot2)
library(shinydashboard)
library(tidyr)
library(plotly)





#DATA:
#1- Conso  Clean
Conso_secteur <-Dict$new(
  region = readRDS("C://Users//sguest//Desktop//mon_super_projet_Rshiny//conso_region_clean2.RDS"),
  departement = readRDS("C://Users//sguest//Desktop//mon_super_projet_Rshiny//conso_departemnt_clean2.RDS"),
  commune = readRDS("C://Users//sguest//Desktop//mon_super_projet_Rshiny//conso_commune_clean2.RDS")
)
#2- prod   Clean:
Prod_maille <-Dict$new(
  region = readRDS("C://Users//sguest//Desktop//mon_super_projet_Rshiny//prod_region_clean2.RDS"),  
  departement = readRDS("C://Users//sguest//Desktop//mon_super_projet_Rshiny//prod_departement_clean2.RDS"),
  commune = readRDS("C://Users//sguest//Desktop//mon_super_projet_Rshiny//prod_commune_clean2.RDS")
)

# Input choices : 
vec_maille <- Conso_secteur$keys

ui <- dashboardPage(
  
  # Application title
  dashboardHeader(
    title="Bilan Conso"
  ),
  # Sidebar:
  dashboardSidebar(
    selectInput(inputId = "maille","maille d'interet", #creer un input pour la maille d'interet 
                label="Choisissez votre maille d'interet",
                choices = vec_maille),
    selectInput(inputId = "annee", "annee(s) d'interet",
                choices = NULL,
                multiple = T),
    selectInput(inputId = "maille_viz","maille à visualiser", #creer un input pour les mailles à visualiser 
                multiple = T,
                choices = NULL)
    
  ),
  dashboardBody(
    
    # gestions des onglets:
    tabsetPanel(type = "tabs",
                tabPanel("Resumé",
                         dataTableOutput({"dataframe_Consomations"}),
                         plotOutput("Barplot"),
                         fluidRow(
                           box(width = 5, background = "light-blue",
                               valueBoxOutput("valueBox_totalconso")),
                           
                           box(width = 4, background = "light-blue",
                               valueBoxOutput("valueBox_totalprod")),
                           fluidRow(box(width = 12,
                                        plotlyOutput("gaphe_interactif_evolution")))
                         )
                ),# tabpanel Resumé
                tabPanel("Details",
                         dataTableOutput({"df_conso_ligne_segment"}),
                         downloadButton("download_data1", "Télécharger les données"),
                         dataTableOutput({"df_prod_ligne_segment"}),
                         downloadButton("download_data2", "Télécharger les données")
                )# tabpanel Details
    )# tabSetPanel
  )# dashboardbody
) # dashboardpage


server <- function(session,input, output) {
  y <- reactive({
    return(input$maille)
  })
  
  # mise a jour des inputes
  observe({
    
    # input annee:
    annee <- unlist(Conso_secteur$get(y()) %>% dplyr::select(annee))
    vect_annee <- unique(annee)
    updateSelectInput(session,"annee","annee(s) d'interet",choices = vect_annee, selected = vect_annee[10])
    
    # input maille a visualiser:
    maille<- unlist(Conso_secteur$get(y()) %>% dplyr::select(2))
    vect_mailles_viz <- unique(maille)
    updateSelectInput(session,"maille_viz","mailles a visualiser",choices = vect_mailles_viz,selected =vect_mailles_viz[1] )
  })
  
  # reactive pour df des consomations:
  Conso<-reactive({
    if (y() == "region"){
      Conso <- Conso_secteur$get(y()) %>% filter (nom_region %in% input$maille_viz & annee %in% input$annee)
    }
    
    else if(y()=="departement"){
      Conso <- Conso_secteur$get(y()) %>% filter (nom_departement %in% input$maille_viz & annee %in% input$annee)
    }
    else {
      Conso <- Conso_secteur$get(y()) %>% filter (nom_commune %in% input$maille_viz & annee %in% input$annee)
    }
    return(Conso)
  }) 
  
  # dataframe des consomations selon les inputes:
  # df_conso:
  output$dataframe_Consomations <- renderDataTable({Conso()})
  
  # reactive pour df des Productions:
  
  prod <- reactive({
    if (y() == "region"){
      prod <- Prod_maille$get(y()) %>% filter (nom_region %in% input$maille_viz & annee %in% input$annee)
    }
    
    else if(y()=="departement"){
      prod <- Prod_maille$get(y()) %>% filter (nom_departement %in% input$maille_viz & annee %in% input$annee)
    }
    else {
      prod <- Prod_maille$get(y()) %>% filter (nom_commune %in% input$maille_viz & annee %in% input$annee)
    }
    return(prod)
  })
  
  # Somme des productions
  sum_prod <- reactive({ apply(prod() %>% select(prod_mwh) , 2, sum, na.rm= T)})
  
  # Graphique en barres:
  output$Barplot <- renderPlot({
    couleurs <- c(rainbow(6))
    ggplot()+
      geom_bar(data = Conso() %>% drop_na(),aes (x="conso",conso_totale__mwh_,fill=code_grand_secteur),stat = "identity")  +
      
      geom_bar(data = prod() %>% drop_na() ,aes(x="prod",as.numeric(prod_mwh),colour = type), stat = "identity") + scale_fill_manual(values=couleurs) +
      ggtitle("Barplot des Consomation par Secteur et Production par type")
  })
  
  # valueBox:
  # Conso tot:
  output$valueBox_totalconso <- renderValueBox({
    df <- Conso() %>% drop_na()
    total.conso <- sum(df$conso_totale__mwh_)
    valueBox(
      "Conso Total",
      formatC(total.conso,format = "E")
      ,width = NULL
    )
  })
  
  # Prod tot:
  output$valueBox_totalprod <- renderValueBox({
    valueBox(
      "Prod Total",
      formatC(sum_prod(),format = "E"),
      width = NULL)
    
  })
  
  # graphique interactif des courbes de consomations et production totale:
  output$gaphe_interactif_evolution <- renderPlotly({
    
    # construction des df:
    df_conso_totale <- Conso() %>% group_by(annee,code_grand_secteur,) %>% summarise(consotot=sum(as.numeric(conso_totale__mwh_),na.rm = T)) %>% ungroup()
    df_prod_totale <-  prod() %>% group_by(annee,type)%>% summarise(prodtot=sum(as.numeric(prod_mwh),na.rm = T)) %>% ungroup()
    
    #  plot:
    f <- ggplot() +
      geom_line(data = df_conso_totale, aes(x = annee, y = consotot, color = code_grand_secteur)) +
      geom_line(data = df_prod_totale, aes(x = annee, y = prodtot, color = type)) +
      labs(title = "Evolution des consommations et des productions par code_secteur et par type selon l'année",
           x = "Année", y = "Quantité") + scale_color_discrete(name = "type de prod et Code secteur Conso") 
    
    # transformation en graphe interactif:
    ggplotly(f)
  })
  
  # details:
  
  
  # reactive consomation avec une ligne par segment,colonne par maille,moyenne_france,ecart:
  conso_ligne_segment <- reactive({Conso() %>% group_by_(sprintf("nom_%s",y()),"code_grand_secteur", "annee")%>%
      summarise(consommation = sum(as.numeric(conso_totale__mwh_)))%>%
      pivot_wider(names_from = sprintf("nom_%s",y()),values_from = "consommation") %>% ungroup()
  })
  # Construction du df pour les  Consommations:
  output$df_conso_ligne_segment <- renderDataTable({conso_ligne_segment()})
  
  
  # bouton pour telecharger le df pour les consomations: 
  output$download_data1 <- downloadHandler(
    filename = function() {
      paste("df_conso_ligne_segment_col_maille.csv", # nom du fichier à telecharger 
            sep=";")
    },
    content = function(file) {
      write.csv(conso_ligne_segment(), file,fileEncoding = "UTF-8")
    }
  )
  
  
  # reactive production avec une ligne par type,colonne par maille,moyenne_france,ecart:
  prod_ligne_segment <- reactive({prod() %>% group_by_(sprintf("nom_%s",y()),"type", "annee")%>%
      summarise(production = sum(as.numeric(prod_mwh)))%>%
      pivot_wider(names_from = sprintf("nom_%s",y()),values_from = "production") %>% ungroup() 
  })
  
  # Construction du df pour les  Productions:
  output$df_prod_ligne_segment <- renderDataTable({prod_ligne_segment()})
  
  
  # Bouton pour telecharger le df pour les productions: 
  output$download_data2 <- downloadHandler(
    filename = function() {
      paste("df_prod_ligne_segment_col_maille.csv", # nom du fichier à telecharger 
            sep=";")
    },
    content = function(file) {
      write.csv(prod_ligne_segment(), file,fileEncoding = "UTF-8")
    }
  )
  
  
  # Remarque: 
  # Je dois mettre le chemin des jeux de donnée sinon l'erreur suivante se produit:
  # Error in gzfile(file, "rb") : cannot open the connection
}

# Run the application 
shinyApp(ui = ui, server = server)
