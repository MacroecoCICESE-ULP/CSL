library(shiny)
library(leaflet)
library(ggplot2)

pred_all_rook<- read.csv("C:/Users/Macroeco/Documents/My_R/ULP/pred_all_rook_CSL.csv")
sum_count_predict<- read.csv("C:/Users/Macroeco/Documents/My_R/ULP/pred_total_pop_CSL.csv")

datos_abundancia <- data.frame(
  año = pred_all_rook$pred_years,
  colonia = pred_all_rook$rook_labels_facet,
  abundancia = pred_all_rook$med_w_at_sea,
  abundancia_min = pred_all_rook$lwr_w_at_sea,
  abundancia_max = pred_all_rook$upr_w_at_sea,
  position_label_x = pred_all_rook$position_label_x,
  position_label_y = pred_all_rook$position_label_y,
  position_label_sub = pred_all_rook$position_label_sub,
  lon= pred_all_rook$lon,
  lat = pred_all_rook$lat
  
)

uso<- levels(as.factor(all_colonies_df$type_colony))
paleta_uso<- colorFactor (c("#F39C6B","#7209b7"), levels = unique(all_colonies_df$type_colony))

symbols <- makeSymbolsSize(
  values = (pred_all_rook$med_w_at_sea),
  shape = 'diamond',
  color = "#973C21",
  fillColor = "#F39C6B",
  opacity = .5,
  fillOpacity = 0.5,
  baseSize = 15
)

URL_DEL_LOGO<- "https://ulp.cicese.mx/wp-content/uploads/2023/04/cicese-ulp-mini.png"
#### Definir UI ####
#runApp(list(
ui <- fluidPage(
  fluidRow(height= 80,
           column(width = 9,
                  div(h1("Predicciones Lobo Marino de California"),
                      style= "height:50px;"
                  )),
           
           #Logo en el encabezado
           column(width= 3,
                  div(tags$img(src = "https://ulp.cicese.mx/wp-content/uploads/2023/04/cicese-ulp-mini.png",
                               height = 80
                  ) ))
  ),
  fluidRow(height= 100,
    column(6, h4("Colonias del Golfo de California"),
           selectInput("yearInput", "Año:",
                       choices =  unique(datos_abundancia$año),
                       selected = unique(datos_abundancia$año)[2])
          ),
    column(6,
           h4("Abundancia por Colonia"),

           selectInput("colonyInput", "Colonia Reproductiva:",
                       choices = unique(datos_abundancia$colonia),
                       selected = unique(datos_abundancia$colonia)[1])
           )
  ),
  fluidRow(
    column(width = 6,
           leafletOutput("map", height = 550)
                  ),
           
    column(width = 6,
           plotOutput("plot", height = 250),
           h4("Población Total en el Golfo de California"),
           plotOutput("populationPlot", height = 250)
                    
                  ))
          
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
                       radius = ~ifelse(uso == "Descanso", 5, 3),
                       color =  ~paleta_uso(uso)
                       
      )%>%
      addMarkers(data = datos_filtrados,
                 icon = symbols,
                 popup = ~colonia#~leaflet::iconSize(30 * log(Abundancia + 1))
      )%>%
      addLegendSize(
        position= "bottomright",
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
      geom_ribbon(data = datos_filtrados,
                  aes(x = año,
                      ymin = abundancia_min,
                      ymax = abundancia_max),
                  fill = "#F39C6B",
                  color = NA,
                  alpha = 0.6) +
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
                    x = position_label_x,
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
                         breaks = seq(1970, 2020, 10),
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
        legend.text = element_text(size = 10),
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
                  fill = "#6687A5",
                  color = NA,
                  alpha = 0.6)+
      geom_path(data=sum_count_predict, 
                aes(x = pred_years, 
                    y = med_count),
                color = "#003C6F",
                size = 0.5) +
      geom_errorbar(data = sum_count_predict[sum_count_predict$pred_years == selected_year, ], 
                    aes(x=pred_years,
                        ymin= lwr_count,
                        ymax= upr_count),
                    width= 0.2,
                    size=0.3,
                    color= "#000814",
                    alpha= 1
                    
      )+
      
      geom_point(data = sum_count_predict[sum_count_predict$pred_years == selected_year, ], 
                 aes(x = pred_years, y = med_count), fill= "red", color= "black", shape= 21, size = 5) +
      
      geom_text(data = sum_count_predict[sum_count_predict$pred_years == selected_year, ],
                aes(label= paste(round(med_count,digits = 0),"individuos"),
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
                    label= paste("(",round(lwr_count,digits = 0)," - ",round(upr_count,digits = 0),")",sep = "")),
                size= 6,
                
                color= "black"
      )+
      scale_x_continuous(limits = c(min(sum_count_predict$pred_years),
                                    max(sum_count_predict$pred_years)),
                         breaks = seq(1970, 2020, 10),
                         expand = c(0, 0)) +
      theme_bw() +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        
        # plot.caption = element_rect(colour = "black",size = 3),
        # plot.caption.position = c(0.1, 0.5),
        legend.position = c(0.625 , 0.085),  # c(left, bottom)
        legend.text = element_text(size = 10),
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
      
      labs(x= "", y= "Tamaño poblacional",title = "")
  })
}


# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

