rm(list=ls())  # clear all
graphics.off() # close all;
gc() # Clear memmory (residuals of operations?, cache? Not sure)



library(leaflet)
library(sf)
library(ggplot2)
library(rgdal)
library(broom)
library(tidyverse)
library(rio)
library(dplyr)
library(extrafont)
library(leafpop)
library(leafem)
library(leaflegend)
library(png)
library(shiny)
library(r2symbols)

# pred_all_rook<- read.csv("C:/Users/Macroeco/OneDrive/CICESE_ULP/CSL_predictions/input_files/pred_all_rook_CSL_18102023.csv")
# sum_count_predict<- read.csv("C:/Users/Macroeco/OneDrive/CICESE_ULP/CSL_predictions/input_files/pred_total_pop_CSL_eco_18102023.csv")
# sum_count_predict_19_tdy<- read.csv("C:/Users/Macroeco/OneDrive/CICESE_ULP/CSL_predictions/input_files/pred_total_pop_CSL_eco_19_tdy_18102023.csv")
# all_colonies_df<- read.csv("C:/Users/Macroeco/OneDrive/CICESE_ULP/CSL_predictions/input_files/info_on_resting_colonies.csv")

pred_all_rook<- read.csv("https://raw.githubusercontent.com/Ariadna436JR/CICESE_ULP_MEM/main/pred_all_rook_CSL_18102023.csv")
sum_count_predict<- read.csv("https://raw.githubusercontent.com/Ariadna436JR/CICESE_ULP_MEM/main/pred_total_pop_CSL_eco_18102023.csv")
sum_count_predict_19_tdy<- read.csv("https://raw.githubusercontent.com/Ariadna436JR/CICESE_ULP_MEM/main/pred_total_pop_CSL_eco_19_tdy_18102023.csv")
all_colonies_df<- read.csv("https://raw.githubusercontent.com/Ariadna436JR/CICESE_ULP_MEM/main/info_on_resting_colonies.csv")


datos_abundancia <- data.frame(
  año = pred_all_rook$pred_years,
  colonia = pred_all_rook$rook_labels_facet,
  abundancia = pred_all_rook$med_count,
  abundancia_min = pred_all_rook$lwr_count,
  abundancia_max = pred_all_rook$upr_count,
  position_label_x = pred_all_rook$position_label_x,
  position_label_y = pred_all_rook$position_label_y,
  position_label_sub = pred_all_rook$position_label_sub,
  lon= pred_all_rook$lon,
  lat = pred_all_rook$lat
  
)

uso<- levels(as.factor(all_colonies_df$type_colony))
paleta_uso<- colorFactor (c("#F39C6B","#7209b7"), levels = unique(all_colonies_df$type_colony))

symbols <- makeSymbolsSize(
  values = (pred_all_rook$med_count),
  shape = 'diamond',
  color = "#973C21",
  fillColor = "#F39C6B",
  opacity = .5,
  fillOpacity = 0.5,
  baseSize = 15
)

colonia<- unique(datos_abundancia$colonia)
año<- unique(datos_abundancia$año)



#### Definir UI ####
#runApp(list(
ui <- fluidPage(
  fluidRow(height= 20,
           style = "height:20px; background-color: #0c4870;"),
  fluidRow(height= 100,
           style = "background-color: #bfced6;",
           #Logo en el encabezado
           column(width=3,
                  div(style= "height:90px; position: relative; top:2px;",
                    tags$img(src = "https://conahcyt.mx/wp-content/uploads/2021/10/logo_conacyt_con_sintagma_azul_completo.svg",
                             width = "90%", height = "90%"
                             #style="height:80px; padding:30px 0;"
                  ) )),
           column(width = 6,
                  div(h2(strong("Predicciones de abundancia de  lobos marinos de California"),
                         style= "height:90px; padding-top:2px; text-align: center;"),
                      
                  )),
           column(width=3,
                 
                  # offset = 9,
                  div(style= "height:90px; position: relative; padding-top:5px;",
                      tags$img(src = "https://www.cicese.edu.mx/assets/img/cicese.png",
                    #tags$img(src = "http://conecto.senacyt.gob.pa/conecto/file/n305/thumbnail_capture_21+2+.jpg",
                             width = "100%", height = "90%"
                  ) 
                  ))
           
          
           
           
  ),
  fluidRow(height= 20,
           style = "height:20px; background-color: #0c4870;"),
  
  fluidRow(height= 300,
           absolutePanel( top = 95, left = 10, width = 300, height = 180,
                          img(src = "https://raw.githubusercontent.com/Ariadna436JR/CICESE_ULP_MEM/main/imege_zalophus.png",width = 300, height = 180)
           ),
           # column(width=2,
           #        
           #        # offset = 9,
           #        div(style = "height:100px;",
           #            tags$img(src = "https://raw.githubusercontent.com/Ariadna436JR/CICESE_ULP_MEM/main/Zalophus_californianus.png",
           #                     height = "120%"
           #            ) )),
           
    column(3),       
    column(3, h4("Colonias del Golfo de California"),
           selectInput("yearInput", "Año:",
                       choices =  unique(datos_abundancia$año),
                       selected = unique(datos_abundancia$año)[2]
                       )
          ),
    column(1),
    column(4,
           h4("Abundancia por Colonia"),

           selectInput("colonyInput", "Colonia Reproductiva:",
                       choices = unique(datos_abundancia$colonia),
                       selected = unique(datos_abundancia$colonia)[1]
                       )
           )
    
    
  ),
  
    
  fluidRow(
    column(width = 6,
           leafletOutput("map", height = 600)
                  ),
           
    column(width = 6,
           plotOutput("plot", height = 250),
           h4("Población Total en el Golfo de California"),
           plotOutput("populationPlot", height = 350)
                    
                  )),
  fluidRow(height=500,
           column(width = 12,
                  heigth=300,
                  h1(""))),
  
  # fluidRow(
  #          height= 100,
  #          column(
  #            width = 12,
  #            
  #          h5("Producto desarrollado por Pardo Mario A., Juárez-Ruiz Ariadna,  Beier Emilio, Busquets-Vass Geraldine.
  #             Laboratorio de Macroecología Marina, CICESE Unidad La Paz. Con base en: Adame, K., Elorriaga-Verplancken, F. R., Beier, E., Acevedo-Whitehouse, K. y Pardo, M. A. 2020. The demographic decline of a sea lion population followed multi-decadal sea surface warming. Scientific Reports, 10(1), p. 10499. ")
  #          )),
  
  fluidRow(height= 80,
           column(width = 12,
                  style= "background-color: #bfced6;",
                  uiOutput("tab"),
                  style= "height:80px;"
                  )
           
          ),
  fluidRow(height= 70,
           column(width = 12,
                  style= "background-color: #0c4870; text-align: center; color: white; ",
                  uiOutput("tab2"),
                  style= "height:60px; padding-top:10px;"
           )
           
  )

           
)


# Definir Server
server <- function(input, output) {
  # Mapa
  output$map <- renderLeaflet({
    datos_filtrados <- datos_abundancia %>%
      filter(año == input$yearInput)
    
    
    leaflet() %>%
      addTiles() %>%
      
      addCircleMarkers(data = all_colonies_df,
                       lng = ~lon,
                       lat = ~lat,
                       popup = ~rookery,
                       radius = ~ifelse(uso == "Descanso", 5, 4),
                       color =  ~paleta_uso(uso)
                       
      )%>%
      addLegendFactor(
        pal= paleta_uso,
        values= all_colonies_df$type_colony,
        position= "topright",
        opacity = .5,
        title = 'Tipos de colonia',
        orientation = 'vertical'
        ) %>%
      
      addMarkers(data = datos_filtrados,
                 icon = symbols,
                 popup = ~colonia#~leaflet::iconSize(30 * log(Abundancia + 1))
      )%>%
      addLegendSize(
        position= "bottomleft",
        values = (datos_filtrados$abundancia),
        color = "#973C21",
        #labels = exp(datos_abundancia$abundancia),
        fillColor = "#F39C6B",
        opacity = .5,
        title = 'Abundancia',
        shape = 'diamond',
        orientation = 'vertical',
        breaks = 5) 
  })
  # Crear mapa
  
  
  # Crear gráfico de abundancia por colonia
  output$plot <- renderPlot({
    datos_filtrados <- datos_abundancia %>%
      filter(colonia == input$colonyInput)
    
    selected_year <- as.numeric(input$yearInput)
    
    ggplot(datos_filtrados, aes(x = año, y = abundancia)) +
      geom_ribbon(data = datos_filtrados[datos_filtrados$año <= 2019,],
                  aes(x = año,
                      ymin = abundancia_min,
                      ymax = abundancia_max),
                  fill = "#F39C6B",
                  color = NA,
                  alpha = 0.6) +
      geom_ribbon(data = datos_filtrados[datos_filtrados$año >= 2019,],
                  aes(x = año,
                      ymin = abundancia_min,
                      ymax = abundancia_max),
                  fill = "gray70",
                  color = NA,
                  alpha = 0.4) +
      geom_path(data = datos_filtrados,
                aes(x = año,
                    y = abundancia),
                # group = rook_labels),
                color = "#973C21",
                size = 0.6) +
      
      
      geom_errorbar(data = datos_filtrados[datos_filtrados$año == selected_year,],
                    aes(x=año,
                        ymin= abundancia_min,
                        ymax= abundancia_max),
                    width= 0.2,
                    size=0.3,
                    color= "#000814",
                    alpha= 1
                    
      )+
      geom_point(data = datos_filtrados[datos_filtrados$año == selected_year,],
                 aes(x=año,
                     y= abundancia),
                 shape=21,
                 size= 5,
                 fill= "blue",
                 color= "#000814",
                 alpha=1)+
      
      
      
      geom_text(data = datos_filtrados[datos_filtrados$año == selected_year,],
                aes(label= paste(round(abundancia,digits = 0),"individuos"),
                    x = position_label_x-1,
                    y = position_label_y
                    #position = c("bottom-left")
                ),
                #   y= Inf,
                
                # position = position_stack(vjust = 0.5),
                size= 8,
                color= "black"
      )+
      geom_text(data = datos_filtrados[datos_filtrados$año == selected_year,],
                aes(x = position_label_x ,
                    y = position_label_sub ,
                    # just = c("left", "bottom"),
                    label= paste("(",round(abundancia_min,digits = 0)," - ",round(abundancia_max,digits = 0),")",sep = "")),
                size= 6,
                
                color= "black"
      )+
      
      
      scale_x_continuous(limits = c(min(sum_count_predict$pred_years),
                                    max(sum_count_predict$pred_years)),
                         breaks = seq(1970, 2020, 5),
                         expand = c(0, 0)) +
      # coord_cartesian(xlim= c(1977,2024),
      #                 clip="off")+
      #facet_wrap_paginate(~rook_labels_facet, ncol = 1, nrow = 1, page = i, scales = "free_y")+
      # Settings:
      theme_bw() +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        
        # plot.caption = element_rect(colour = "black",size = 3),
        # plot.caption.position = c(0.1, 0.5),
        legend.position = c(0.625 , 0.085),  # c(left, bottom)
        legend.text = element_text(size = 12),
        legend.key.height = unit(0.5,"cm"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 0)),
        plot.background = element_rect(fill = "white"),
        
        
        strip.background = (element_blank()),
        axis.ticks = (element_line(size = 0.1)),
        panel.grid = (element_line(size = 0.12)),
        
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines"), 
        strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm"))  # top, right, bottom, left
      ) +
      
      
      labs(x = "", y = "Abundancia por colonia"
      )
  })
  
  
  # Crear gráfico de población total
  output$populationPlot <- renderPlot({
    # Aquí podrías cargar los datos para el gráfico de población total
    
    selected_year <- as.numeric(input$yearInput)
    
    ggplot() +
      geom_ribbon(data=sum_count_predict,
                  aes(x = pred_years,
                      ymin= lwr_count,
                      ymax= upr_count),
                  fill = NA,
                  color = "gray40",
                  linetype = "dashed",
                  alpha = 0.4)+
      geom_ribbon(data=sum_count_predict[which(sum_count_predict$pred_years >=2019),],
                  aes(x = pred_years,
                      ymin= lwr_count,
                      ymax= upr_count),
                  fill = "gray70",
                  color = "gray40",
                  linetype = "dashed",
                  alpha = 0.4)+
      geom_path(data=sum_count_predict,
                aes(x = pred_years,
                    y = med_count),
                color = "gray40",
                size = 0.5) +
      
      
      
      
      # geom_errorbar(data = sum_count_predict[sum_count_predict$pred_years == selected_year, ], 
      #               aes(x=pred_years,
      #                   ymin= lwr_count,
      #                   ymax= upr_count),
      #               width= 0.2,
      #               size=0.3,
      #               color= "gray40",
      #               alpha= 1
      #               
      # )+
      # geom_point(data = sum_count_predict[sum_count_predict$pred_years == selected_year, ], 
      #            aes(x = pred_years, y = med_count), fill= "red", color= "gray40", shape= 21, size = 5) +
      ## Abun modelo ecológico ##
      geom_ribbon(data=sum_count_predict,
                  aes(x = pred_years,
                      ymin= lwr_abun_eco*1000,
                      ymax= upr_abun_eco*1000),
                  fill = "#6687A5",
                  color = NA,
                  alpha = 0.8)+
      geom_path(data=sum_count_predict,
                aes(x = pred_years,
                    y = med_abun_eco*1000),
                color = "#003C6F",
                size = 0.5) +
      geom_errorbar(data = sum_count_predict[sum_count_predict$pred_years == selected_year, ],
                    aes(x=pred_years,
                        ymin= lwr_abun_eco*1000,
                        ymax= upr_abun_eco*1000),
                    width= 0.2,
                    size=0.3,
                    color= "#000814",
                    alpha= 1

      )+
      # geom_ribbon(data=sum_count_predict_19_tdy,
      #             aes(x = pred_years,
      #                 ymin= lwr_abun_eco*1000,
      #                 ymax= upr_abun_eco*1000),
      #             fill = "gray40",
      #             color = NA,
      #             alpha = 0.8)+
      geom_path(data=sum_count_predict_19_tdy,
                aes(x = pred_years,
                    y = med_abun_eco*1000),
                color = "gray10",
                size = 0.5) +

      geom_point(data = sum_count_predict[sum_count_predict$pred_years == selected_year, ],
                 aes(x = pred_years, y = med_abun_eco*1000), fill= "red", color= "black", shape= 21, size = 5) +

      geom_text(data = sum_count_predict[sum_count_predict$pred_years == selected_year, ],
                aes(label= paste(round(med_abun_eco*1000,digits = 0),"individuos"),
                    x =2015,
                    y = 50000,
                    #position = c("bottom-left")
                ),
                #   y= Inf,
                
                # position = position_stack(vjust = 0.5),
                size= 8,
                color= "black"
      )+
      geom_text(data = sum_count_predict[sum_count_predict$pred_years == selected_year, ],
                aes(x = 2015 ,
                    y = 45000 ,
                    # just = c("left", "bottom"),
                    label= paste("(",round(lwr_abun_eco*1000,digits = 0)," - ",round(upr_abun_eco*1000,digits = 0),")",sep = "")),
                size= 6,
                
                color= "black"
      )+
      scale_x_continuous(limits = c(min(sum_count_predict$pred_years),
                                    max(sum_count_predict$pred_years)),
                         breaks = seq(1970, 2020, 5),
                         expand = c(0, 0)) +
      theme_bw() +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        
        # plot.caption = element_rect(colour = "black",size = 3),
        # plot.caption.position = c(0.1, 0.5),
        legend.position = c(0.625 , 0.085),  # c(left, bottom)
        legend.text = element_text(size = 12),
        legend.key.height = unit(0.5,"cm"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 0)),
        plot.background = element_rect(fill = "white"),
        
        
        strip.background = (element_blank()),
        axis.ticks = (element_line(size = 0.1)),
        panel.grid = (element_line(size = 0.12)),
        
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines"), 
        strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm"))  # top, right, bottom, left
      ) +
      
      labs(x= "", y= "Predicción del tamaño poblacional",title = "")
  })
  
  url <- a("DOI:10.1038/s41598-020-67534-0",href="https://doi.org/10.1038/s41598-020-67534-0")
  output$tab <- renderUI({
    tagList("Producto desarrollado por Pardo Mario A., Juárez-Ruiz Ariadna,  Beier Emilio, Busquets-Vass Geraldine.
              Laboratorio de Macroecología Marina, CICESE Unidad La Paz. Con base en:  Adame, K., Elorriaga-Verplancken, F. R., Beier, E., Acevedo-Whitehouse, K. y Pardo, M. A. 2020. The demographic decline of a sea lion population followed multi-decadal sea surface warming. Scientific Reports, 10(1), p. 10499.", url)
  })
  
  url2 <- a("www.cicese.edu.mx",href="www.cicese.edu.mx")
  output$tab2 <- renderUI({
    tagList("Copyright © 2023, Todos los Derechos Reservados, CICESE", url2)
  })

}


# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

