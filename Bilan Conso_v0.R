library(shiny)
library(dplyr)
library(Dict)
library(ggplot2)
library(shinydashboard)
library(tidyr)
library(plotly)

#DATA:
#1- Conso : Clean
Conso_secteur <-Dict$new(
    region = readRDS("C://Users//sguest//Desktop//projet R avance//jeu de donnee//Tables de donnees Conso//projetR//nvx_jdd//conso_region_clean2.RDS"),
    departement = readRDS("C://Users//sguest//Desktop//projet R avance//jeu de donnee//Tables de donnees Conso//projetR//nvx_jdd//conso_departemnt_clean2.RDS"),
    commune = readRDS("C://Users//sguest//Desktop//projet R avance//jeu de donnee//Tables de donnees Conso//projetR//nvx_jdd//conso_commune_clean2.RDS")
)
#2- prod :  Clean:
Prod_maille <-Dict$new(
    region = readRDS("C://Users//sguest//Desktop//projet R avance//jeu de donnee//Tables de donnees Conso//projetR//nvx_jdd//prod_region_clean2.RDS"),
    departement = readRDS("C://Users//sguest//Desktop//projet R avance//jeu de donnee//Tables de donnees Conso//projetR//nvx_jdd//prod_departement_clean2.RDS"),
    commune = readRDS("C://Users//sguest//Desktop//projet R avance//jeu de donnee//Tables de donnees Conso//projetR//prod_commune_clean.RDS")
)

#Input choices : 
vec_maille <- Conso_secteur$keys


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(
    title="Analyses des consommations/productions electriques"
  ),
  # Sidebar 
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
    
    # Show a plot of the generated distribution
    dashboardBody(
        

                    dataTableOutput({"conso_df"}),
                    plotOutput("p1"),
                    fluidRow(
                    box(width = 5, background = "light-blue",
                            valueBoxOutput("totalconso")),
                    
                    box(width = 4,background = "light-blue",
                            valueBoxOutput("totalprod"))
                        ),
                    plotlyOutput("evol"),
                    dataTableOutput({"conso_dff"}),
                    downloadButton('downloadData', 'Download'),
                    #tab prod
                    dataTableOutput({"prod_dff"}),
                    downloadButton('downloadDataProd', 'Download')
                   )
            
            
    
)

server <- function(session,input, output) {
    #ui conditionnelle
     x <- reactive({
    return(input$type)
  })
    y <- reactive({
        return(input$maille)
    })
    observe({
        #input annee:
        annee <- Conso_secteur$get(y()) %>% dplyr::select(annee)
        updateSelectInput(session,"annee","annee(s) d'interet",choices = unique(annee)[,1] )
        
        maille<- Conso_secteur$get(y()) %>% dplyr::select(2)
        updateSelectInput(session,"maille_viz","mailles a viz",choices = unique((maille)[,1]))
    })
    
    
    

    
    #fct df conso
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
    
    #df_conso:
    output$conso_df <- renderDataTable({Conso()})
    
    s_prod <- reactive({ apply(prod() %>% drop_na()
                               %>% select(prod_mwh),
                               MARGIN = 2, FUN = as.numeric ) 
    })
    sum_prod <- reactive({ apply(s_prod(), 2, sum, na.rm= T)})
    
    #les Barplots
    output$p1 <- renderPlot({
        couleurs <- c(rainbow(6))
        ggplot()+
            geom_bar(data = Conso() %>% drop_na(),aes (x="conso",conso_totale__mwh_,fill=code_grand_secteur),stat = "identity")  +
          
          geom_bar(data = prod() %>% drop_na() ,aes(x="prod",prod_mwh,colour = type), stat = "identity") + scale_fill_manual(values=couleurs) + ggtitle("Barplot des Consomation par Secteur et Production par type")
        
        })
    
 
    
    #value Box:
    
    #conso tot:
    output$totalconso <- renderValueBox({
        df <- Conso() %>% drop_na()
        total.conso <- sum(df$conso_totale__mwh_)
        valueBox(
            "Conso Total",
            formatC(total.conso,format = "E")
            ,width = NULL
        )
    })
    
    #Prod_tot:
    output$totalprod <- renderValueBox({
        valueBox(
            "Prod Total",
            formatC(sum_prod(),format = "E"),
           width = NULL)
        
    })
    
    #graphe des evolutions:
    
 
    output$evol <- renderPlotly({
    df_conso <- Conso() %>% group_by(annee,code_grand_secteur,) %>% summarise(consotot=sum(as.numeric(conso_totale__mwh_),na.rm = T)) %>% ungroup()
    df_prod <-  prod() %>% group_by(annee,type)%>% summarise(prodtot=sum(as.numeric(prod_mwh),na.rm = T)) %>% ungroup()
    
    f<- ggplot(df_conso)+
        aes(x=annee,y=as.numeric(consotot), col = code_grand_secteur)+
        geom_line()+ 
        geom_line(data =df_prod,aes(x=annee, y = as.numeric(prodtot) , col = type))+
        scale_fill_brewer(palette="Spectral") + labs(x = "annee") + labs(y = "mwh") + labs(title = " Evolutions des consomation et production durant la periode") + scale_fill_discrete( labels = c("TERTIAIRE", "INDUSTRIE", "RESIDENTIEL", "GRICULTURE", "INCONNU", "PETIT_PROFESSIONNEL", "photovoltaïque_enedis_","eolien_enedis_","hydraulique_enedis_","bio_energie_enedis_","cogeneration_enedis_","autres_filieres_enedis_"))
    ggplotly(f)
})
    
    ### details:
    #df_conso:
    
    conso_dff <- reactive({Conso() %>% group_by_(sprintf("nom_%s",y()),"code_grand_secteur", "annee")%>%
        summarise(consommation=sum(as.numeric(conso_totale__mwh_)))%>%
        pivot_wider(names_from = sprintf("nom_%s",y()),values_from = "consommation")})
    
    
    
    mean_conso_france<-reactive({
      if (y() == "region"){
        mean_conso_france <- mean(Conso_dff()$consommation)
      }
      
      else if(y()=="departement"){
        mean_conso_france <- mean(Conso_dff()$consommation)
      }
      else {
        mean_conso_france <- mean(Conso()$consommation)
      }
      return(mean_conso_france)
    })
    
    prod_dff <- reactive({prod()})
    
    #mean_conso_france <-  reactive({rep(mean(Conso()$conso_moyenne__mwh_),size(Conso()))}) # moy france
    output$conso_dff <- renderDataTable({conso_dff()})
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("conso_dff", ".csv", sep="")
      },
      content = function(file) {
        write.csv(conso_dff(), file, row.names = FALSE)
      }
    )
    #df_prod:
    
    output$prod_dff <- renderDataTable({prod()%>% select(Type_de_production=type, Annee=annee, prod_mwh)%>%
        mutate(Moyenne_France = rep(0,dim(prod())[1]), Ecart= (prod_mwh))})
    
    output$downloadDataProd <- downloadHandler(
      filename = function() {
        paste("prod_dff", ".csv", sep="")
      },
      content = function(file) {
        write.csv(prod_dff(), file, row.names = FALSE)
      }
    )
    traceback()
  # à faire: 
  #  cree les deux onglets (tab)
  #  collectivité moyenne france ???
}



# Run the application code_grand_secteur
shinyApp(ui = ui, server = server)
