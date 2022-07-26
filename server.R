function(input, output, session) {


  observe({
    input$Regiones
    Anos <- inputs[inputs$REGION %in% input$Regiones,]
    updateSelectInput(session, "Anos",
                      label = "Seleccione Año",
                      choices = c(Seleccione= '',unique(Anos$ANO))
    )
  })
  
  
  observe({
    input$Anos
    unidades <- inputs[inputs$REGION %in% input$Regiones & inputs$ANO %in% input$Anos,]
    
    updateSelectInput(session, "Unidades", "Seleccione unidad:",
                      choices = c(Seleccione= '',unique(unidades$UNIDAD))
    )
  })
  

  output$map <- renderLeaflet({
    
    snaspe <- input$Unidades
    ano <- input$Anos
    # snaspe <- "Parque Nacional Volcan Isluga"
    snaspe_cod <- as.character(unique(inputs[(inputs$UNIDAD %in% snaspe),]$COD))
    snaspe_mapa <- limite_snaspe[(limite_snaspe$COD_UNIDAD %in% snaspe_cod),]
    
    if (length(snaspe)==0) {
      
    m<- leaflet(data=snaspe_mapa)  
    }else {
    centr <- st_centroid(snaspe_mapa$geometry)
    m<- leaflet( data=snaspe_mapa) %>% addPolygons(color = "#444444", group = "Limites") %>%
        addTiles()}
    
    
    
    
    
    
  })
  
  #########################################################################################
  ###############################a Pie#####################################################      
  
  output$Pie_kms <- renderbs4ValueBox({
    transportes <-  c("Pedestre")
    filtro_transporte <- mydata[mydata$patrol_transport_type %in% transportes,]
    ano <-  ifelse(length(input$Anos)==0,0,input$Anos)
    unidad <- ifelse(length(input$Unidades)==0,"0",input$Unidades)
    unidad <- unique(inputs[inputs$UNIDAD==unidad,]$COD)
    # ano <- 2021; unidad <- "0"
    filtro_tracks_ano <- tracks_data[tracks_data$ano==ano,]
    filtro_tracks_ano <- filtro_tracks_ano[filtro_tracks_ano$conservation_area_id %in% unidad,]
    filtro_tracks_ano <-  filtro_tracks_ano[filtro_tracks_ano$Patrol_ID %in% filtro_transporte$Patrol_ID,]
    kilometros <- sum(filtro_tracks_ano$distances)
    
    caja <- bs4InfoBox(width = 12,
      value = formatC(kilometros/1000, digits = 0, format = "f"),
      title = tags$style(HTML("
            code {
                overflow:hidden;
            }Kilometros")) ,
      icon = my_icon("hiking"),
      color = "orange",
      fill = TRUE
        
    )
    
    
     
    
  })
  
  output$Pie_patrullajes <- renderbs4InfoBox({
    transportes <-  c("Pedestre")
    filtro_transporte <- mydata[mydata$patrol_transport_type %in% transportes,]
    ano <-  ifelse(length(input$Anos)==0,0,input$Anos)
    unidad <- ifelse(length(input$Unidades)==0,"0",input$Unidades)
    unidad <- unique(inputs[inputs$UNIDAD==unidad,]$COD)
    # ano <- 2021; unidad <- "0"
    filtro_tracks_ano <- tracks_data[tracks_data$ano==ano,]
    filtro_tracks_ano <- filtro_tracks_ano[filtro_tracks_ano$conservation_area_id %in% unidad,]
    filtro_tracks_ano <-  filtro_tracks_ano[filtro_tracks_ano$Patrol_ID %in% filtro_transporte$Patrol_ID,]
    patrullajes <- length(unique(filtro_tracks_ano$Patrol_ID))
    
    caja <- bs4InfoBox(width = 12,
                       value = formatC(patrullajes, digits = 0, format = "f"),
                       title = "Patrullajes",
                       icon = my_icon("hiking"),
                       color = "orange",
                       fill = TRUE
                       
    )
    
    # caja$children[[1]]$attribs$class<-"action-button"
    # caja$children[[1]]$attribs$id<-"boton1"
    return(caja)
    
  
  })
  
  
  output$Pie_kms_guarda <- renderbs4InfoBox({
    transportes <-  c("Pedestre")
    filtro_transporte <- mydata[mydata$patrol_transport_type %in% transportes,]
    ano <-  ifelse(length(input$Anos)==0,0,input$Anos)
    unidad <- ifelse(length(input$Unidades)==0,"0",input$Unidades)
    unidad <- unique(inputs[inputs$UNIDAD==unidad,]$COD)
    # ano <- 2021; unidad <- "0"
    filtro_tracks_ano <- tracks_data[tracks_data$ano==ano,]
    filtro_tracks_ano <- filtro_tracks_ano[filtro_tracks_ano$conservation_area_id %in% unidad,]
    filtro_tracks_ano <-  filtro_tracks_ano[filtro_tracks_ano$Patrol_ID %in% filtro_transporte$Patrol_ID,]
    kilometros <- sum(filtro_tracks_ano$distances)
    patrullajes <- length(unique(filtro_tracks_ano$Patrol_ID))
    
    caja <- bs4InfoBox(width = 12,
                       value = formatC(ifelse(patrullajes==0,0,kilometros/(1000*patrullajes)), digits = 0, format = "f"),
                       title = "Km/Patrullajes",
                       icon = my_icon("hiking"),
                       color = "orange",
                       fill = TRUE
                       
    )
    
    return(caja)
  })
  
  #########################################################################################
  ###############################Motorizados###############################################    
  
  output$Motorizado_kms <- renderValueBox({
    transportes <-  c("Camioneta","Motocicleta","Auto")
    filtro_transporte <- mydata[mydata$patrol_transport_type %in% transportes,]
    ano <-  ifelse(length(input$Anos)==0,0,input$Anos)
    unidad <- ifelse(length(input$Unidades)==0,"0",input$Unidades)
    unidad <- unique(inputs[inputs$UNIDAD==unidad,]$COD)
    # ano <- 2021; unidad <- "0"
    filtro_tracks_ano <- tracks_data[tracks_data$ano==ano,]
    filtro_tracks_ano <- filtro_tracks_ano[filtro_tracks_ano$conservation_area_id %in% unidad,]
    filtro_tracks_ano <-  filtro_tracks_ano[filtro_tracks_ano$Patrol_ID %in% filtro_transporte$Patrol_ID,]
    kilometros <- sum(filtro_tracks_ano$distances)
    
    caja <- bs4InfoBox(width = 12,
                       value = formatC(kilometros/1000, digits = 0, format = "f"),
                       title = "kilometros",
                       icon = icon("truck-monster"),
                       color = "danger",
                       fill = TRUE
                       
    )
    
    # caja$children[[1]]$attribs$class<-"action-button"
    # caja$children[[1]]$attribs$id<-"boton1"
    return(caja)
  })
  
  output$Motorizado_patrullajes <- renderValueBox({
    transportes <-  c("Camioneta","Motocicleta","Auto")
    filtro_transporte <- mydata[mydata$patrol_transport_type %in% transportes,]
    ano <-  ifelse(length(input$Anos)==0,0,input$Anos)
    unidad <- ifelse(length(input$Unidades)==0,"0",input$Unidades)
    unidad <- unique(inputs[inputs$UNIDAD==unidad,]$COD)
    # ano <- 2021; unidad <- "0"
    filtro_tracks_ano <- tracks_data[tracks_data$ano==ano,]
    filtro_tracks_ano <- filtro_tracks_ano[filtro_tracks_ano$conservation_area_id %in% unidad,]
    filtro_tracks_ano <-  filtro_tracks_ano[filtro_tracks_ano$Patrol_ID %in% filtro_transporte$Patrol_ID,]
    patrullajes <- length(unique(filtro_tracks_ano$Patrol_ID))
    
    caja <- bs4InfoBox(width = 12,
                       value = formatC(patrullajes, digits = 0, format = "f"),
                       title = "Patrullajes",
                       icon = icon("truck-monster"),
                       color = "danger",
                       fill = TRUE
                       
    )
    
    # caja$children[[1]]$attribs$class<-"action-button"
    # caja$children[[1]]$attribs$id<-"boton1"
    return(caja)
  })
  
  
  output$Motorizado_kms_guarda <- renderValueBox({
    transportes <-  c("Camioneta","Motocicleta","Auto")
    filtro_transporte <- mydata[mydata$patrol_transport_type %in% transportes,]
    ano <-  ifelse(length(input$Anos)==0,0,input$Anos)
    unidad <- ifelse(length(input$Unidades)==0,"0",input$Unidades)
    unidad <- unique(inputs[inputs$UNIDAD==unidad,]$COD)
    # ano <- 2021; unidad <- "0"
    filtro_tracks_ano <- tracks_data[tracks_data$ano==ano,]
    filtro_tracks_ano <- filtro_tracks_ano[filtro_tracks_ano$conservation_area_id %in% unidad,]
    filtro_tracks_ano <-  filtro_tracks_ano[filtro_tracks_ano$Patrol_ID %in% filtro_transporte$Patrol_ID,]
    kilometros <- sum(filtro_tracks_ano$distances)
    patrullajes <- length(unique(filtro_tracks_ano$Patrol_ID))
    
    caja <- bs4InfoBox(width = 12,
                       value = formatC(ifelse(patrullajes==0,0,kilometros/(1000*patrullajes)), digits = 0, format = "f"),
                       title = "Km/Patrullajes",
                       icon = icon("truck-monster"),
                       color = "danger",
                       fill = TRUE
                       
    )
  })
  #########################################################################################
  ###############################Acuaticos#################################################
  
  
  output$Acuatico_kms <- renderValueBox({
    transportes <-  c("Bote")
    filtro_transporte <- mydata[mydata$patrol_transport_type %in% transportes,]
    ano <-  ifelse(length(input$Anos)==0,0,input$Anos)
    unidad <- ifelse(length(input$Unidades)==0,"0",input$Unidades)
    unidad <- unique(inputs[inputs$UNIDAD==unidad,]$COD)
    # ano <- 2021; unidad <- "0"
    filtro_tracks_ano <- tracks_data[tracks_data$ano==ano,]
    filtro_tracks_ano <- filtro_tracks_ano[filtro_tracks_ano$conservation_area_id %in% unidad,]
    filtro_tracks_ano <-  filtro_tracks_ano[filtro_tracks_ano$Patrol_ID %in% filtro_transporte$Patrol_ID,]
    kilometros <- sum(filtro_tracks_ano$distances)
    
    caja <- bs4InfoBox(width = 12,
                       value = formatC(kilometros/1000, digits = 0, format = "f"),
                       title = "kilometros",
                       icon = icon("ship"),
                       color = "primary",
                       fill = TRUE
                       
    )
    
    # caja$children[[1]]$attribs$class<-"action-button"
    # caja$children[[1]]$attribs$id<-"boton1"
    return(caja)
  })
  
  output$Acuatico_patrullajes <- renderValueBox({
    
    transportes <-  c("Bote")
    filtro_transporte <- mydata[mydata$patrol_transport_type %in% transportes,]
    ano <-  ifelse(length(input$Anos)==0,0,input$Anos)
    unidad <- ifelse(length(input$Unidades)==0,"0",input$Unidades)
    unidad <- unique(inputs[inputs$UNIDAD==unidad,]$COD)
    # ano <- 2021; unidad <- "0"
    filtro_tracks_ano <- tracks_data[tracks_data$ano==ano,]
    filtro_tracks_ano <- filtro_tracks_ano[filtro_tracks_ano$conservation_area_id %in% unidad,]
    filtro_tracks_ano <-  filtro_tracks_ano[filtro_tracks_ano$Patrol_ID %in% filtro_transporte$Patrol_ID,]
    patrullajes <- length(unique(filtro_tracks_ano$Patrol_ID))
    
    caja <- bs4InfoBox(width = 12,
                       value = formatC(patrullajes, digits = 0, format = "f"),
                       title = "Patrullajes",
                       icon = icon("ship"),
                       color = "primary",
                       fill = TRUE
                       
    )
    
    # caja$children[[1]]$attribs$class<-"action-button"
    # caja$children[[1]]$attribs$id<-"boton1"
    return(caja)
  })
  
  
  output$Acuatico_kms_guarda <- renderValueBox({
    transportes <-  c("Bote")
    filtro_transporte <- mydata[mydata$patrol_transport_type %in% transportes,]
    ano <-  ifelse(length(input$Anos)==0,0,input$Anos)
    unidad <- ifelse(length(input$Unidades)==0,"0",input$Unidades)
    unidad <- unique(inputs[inputs$UNIDAD==unidad,]$COD)
    # ano <- 2021; unidad <- "0"
    filtro_tracks_ano <- tracks_data[tracks_data$ano==ano,]
    filtro_tracks_ano <- filtro_tracks_ano[filtro_tracks_ano$conservation_area_id %in% unidad,]
    filtro_tracks_ano <-  filtro_tracks_ano[filtro_tracks_ano$Patrol_ID %in% filtro_transporte$Patrol_ID,]
    kilometros <- sum(filtro_tracks_ano$distances)
    patrullajes <- length(unique(filtro_tracks_ano$Patrol_ID))
    
    caja <- bs4InfoBox(width = 12,
                       value = formatC(ifelse(patrullajes==0,0,kilometros/(1000*patrullajes)), digits = 0, format = "f"),
                       title = "Km/Patrullajes",
                       icon = icon("ship"),
                       color = "primary",
                       fill = TRUE
                       
    )
  })
  
  
  
  output$Grafico_datos <- renderPlotly({
    unidad <- input$Unidades
    # unidad <- c("Parque Nacional Volcan Isluga","Reserva Nacional Pampa del Tamarugal")
    unidad <- unique(inputs[inputs$UNIDAD %in% unidad,]$COD)
    ano <- input$Anos
    # ano <- ""
    
    filtro <-  mydata[mydata$ano==ano,]
    df <- (0)
    
    if (length(unidad)<1) {
      
    } else {
      for (i in unidad) {
        filtro_1 <-  filtro[filtro$conservation_area_id==i,]
        tipo_datos <-  as.data.frame(table(filtro_1$observation_category_0))
        data_tipo <- data.frame(tipo_datos,i)
        df <-  rbind(df,data_tipo)
      }
    }
    if (length(df)==1){
      ggplotly(ggplot() + ggtitle("Configure el menú de selección para ver las gráficas") + theme_minimal())
    } else {
      df <- df[-1,]
      
      df <- df[df$Var1 %in% c("Datos de Amenazas","Datos de Fauna", "Datos de Fauna Muerta", "Datos de Uso Publico"),]    
      
      ggplotly(ggplot(df, aes(fill=i, y=Freq, x=Var1)) + 
                 geom_bar(position="stack", stat="identity") + theme_classic() + 
                 ggtitle("Datos recolectados") + # for the main title
                 xlab("Tipo de dato") +# for the x axis label
                 ylab("Frecuencia") +
                 labs(fill = "Unidad"))  %>%
        layout(xaxis = list(autorange = TRUE),
               yaxis = list(autorange = TRUE))
      
      
    }
  })
  
  output$Grafico_kms <- renderPlotly({
    unidad <- input$Unidades
    unidad <- unique(inputs[inputs$UNIDAD %in% unidad,]$COD)
    # unidad <- "PNVI";ano <- 2020
    ano <- input$Anos
    anos <- unique(inputs$ANO)
    data <- tracks_data
    filtro <- data[data$conservation_area_id %in% unidad,]
    filtro$ano <- as.numeric(as.character(filtro$ano))
    filtro$Patrol_Transport_Type <- as.character(filtro$Patrol_Transport_Type)
    df <- (0)
    
    if (length(unidad)<1) {
      
    }else{
      
      for (i in 1:length(filtro$Patrol_Transport_Type)) {
        if (filtro$Patrol_Transport_Type[i] %in% c("Camioneta","Motocicleta")){
          filtro$Patrol_Transport_Type[i] <- "Motorizado"
        }
      }
      
      for (i in min(filtro$ano):max(filtro$ano)) {
        filtro_1 <-  filtro[filtro$ano==i,]
        tipo_datos <-  aggregate(filtro_1$distances/1000, by=list(Category=filtro_1$Patrol_Transport_Type), 
                                 FUN=sum)
        tipo_datos <- data.frame(tipo_datos,i)
        df <-  rbind(df,tipo_datos)
      }
    }
    
    if (length(df)==1){
      ggplotly(ggplot() + ggtitle("Configure el menú de selección para ver las gráficas") + theme_minimal())
    } else {
      df <- df[-1,]
      
      names(df) <-  c("Categoria","Distancia","Ano")
      
      
      ggplotly(ggplot(df, aes(fill=Categoria, y=Distancia, x=Ano)) + 
                 geom_bar(position="stack", stat="identity") +
                 scale_fill_manual(values=c("red", "orange", "blue")) + theme_classic() + 
                 ggtitle("Distancias recorridas por transporte") + # for the main title
                 xlab("Año") +# for the x axis label
                 ylab("Distancia (Km)") +
                 labs(fill = "Transporte")) %>%
        layout(xaxis = list(autorange = TRUE),
               yaxis = list(autorange = TRUE))
    } 
    
  })
  
  
  
  
  observeEvent(input$Unidades, ignoreNULL = F,{
    
    if (nchar(input$Unidades) < 1) {
      leafletProxy("map") %>% clearImages() %>% clearTiles() %>%
        clearControls() %>% addTiles()
    } else {
      # unidad <- "Parque Nacional Volcan Isluga"; ano <- 2021
      unidad <- input$Unidades
      cod<- unique(inputs[inputs$UNIDAD %in% unidad,]$COD)
      ano <- input$Anos
      
      #buscar un archivo raster para crear un raster vacio
      archivo <- paste0("www/cache/",ano,"/",cod,"/heat_track/heat_track.tif")
      raster <-raster(archivo)
      viridis <- c("#440154FF","#481567FF","#482677FF","#453781FF","#404788FF",
                   "#39568CFF","#33638DFF","#2D708EFF","#287D8EFF","#238A8DFF",
                   "#1F968BFF","#20A387FF","#29AF7FFF","#3CBB75FF","#55C667FF",
                   "#73D055FF","#95D840FF","#B8DE29FF","#DCE319FF","#FDE725FF")
      pal1 <- colorNumeric(inlmisc::GetTolColors(256, start = 0.5, end = 1.0), raster::values(raster), na.color = "transparent")
      leafletProxy("map",data = raster ) %>% clearImages() %>% clearTiles() %>%
        clearControls() %>% addTiles() %>%
        addRasterImage(
          raster,
           group = "Mapa de calor",
           opacity = 1,
           colors = pal1) %>%
        addLayersControl(overlayGroups = c("Mapa de calor", "Limites")) %>%
        leaflet::addLegend(
          position = c("topleft"),
          pal = pal1,
          values = raster::values(raster),
          title = paste0("Densidad de muestreo"))
    }
  }
  )
  
  observeEvent(input$boton1, {
    unidad <- input$Unidades
    unidad <- unique(inputs[inputs$UNIDAD %in% unidad,]$COD)
    ano <- input$Anos
    # unidad <- c("Parque Nacional Volcan Isluga", "Reserva Nacional Pampa del Tamarugal"); ano<-2020
    
    filtro <- tracks_data[tracks_data$ano==ano,]
    filtro_1 <- filtro[filtro$conservation_area_id %in% unidad,]
    filtro_2 <- filtro_1[filtro_1$Patrol_Transport_Type=="Pedestre",]
    
    
    leafletProxy("map",data = filtro_2 ) %>% clearGroup(group="Patrullajes") %>% 
      addPolylines(group = "Patrullajes",color="orange") %>%
      addLayersControl(overlayGroups = c("Mapa de calor", "Limites", "Patrullajes"))
    
  })
  
  observeEvent(input$boton2, {
    unidad <- input$Unidades
    unidad <- unique(inputs[inputs$UNIDAD %in% unidad,]$COD)
    ano <- input$Anos
    # unidad <- c("Parque Nacional Volcan Isluga", "Reserva Nacional Pampa del Tamarugal"); ano<-2020
    
    filtro <- tracks_data[tracks_data$ano==ano,]
    filtro_1 <- filtro[filtro$conservation_area_id %in% unidad,]
    filtro_2 <- filtro_1[filtro_1$Patrol_Transport_Type %in% c("Camioneta","Motocicleta"),]
    
    
    leafletProxy("map",data = filtro_2 ) %>% clearGroup(group="Patrullajes") %>% 
      addPolylines(group = "Patrullajes", color="red") %>%
      addLayersControl(overlayGroups = c("Mapa de calor", "Limites", "Patrullajes"))
    
  })
  
  
  observeEvent(input$boton3, {
    unidad <- input$Unidades
    unidad <- unique(inputs[inputs$UNIDAD %in% unidad,]$COD)
    ano <- input$Anos
    # unidad <- c("Parque Nacional Volcan Isluga", "Reserva Nacional Pampa del Tamarugal"); ano<-2020
    
    filtro <- tracks_data[tracks_data$ano==ano,]
    filtro_1 <- filtro[filtro$conservation_area_id %in% unidad,]
    filtro_2 <- filtro_1[filtro_1$Patrol_Transport_Type=="Bote",]
    
    
    leafletProxy("map",data = filtro_2 ) %>% clearGroup(group="Patrullajes") %>% 
      addPolylines(group = "Patrullajes") %>%
      addLayersControl(overlayGroups = c("Mapa de calor", "Limites", "Patrullajes"))
    
  })
  
  
  output$descarga <- downloadHandler(
    filename = function(){"thename.csv"}, 
    content = function(fname){
      write.csv(descarga(), fname)
    }
  )
}
