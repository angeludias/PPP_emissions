  #---
    # title: "Pesticidas Prediction by Regression"
    # Description: Tenemos dos tipos de variables explicativas: Climaticas y Land Use.
    #     la variable a predecir es el ratio de fertilizante por ha.
    #     en un codigo previo se hace la seleccion de las variables explicativas. No se pueden usar todas tendriamos overfitting
    #     Hay muchisismos pesticidas (realmente familia de compuestos: pesticida, funguizida, insecticida). Se crea un modelo para cada pesticida.
    # author: "Angel Udias"
    # date: ", Jan 12, 2020"
    # output: 
    # ---
    
    ####################
    ## MODELOS DE PREDICCION DEL RATIO DE COMPUESTO: kg APLICADOS/ area agricola
    #####################
    
    ## 1. Path for the code
    path_base <- "C:/ANGEL/2020SEP/Pesticidas/"   
    setwd(path_base)
    path_out <-paste0(path_base,"output")
    path_data <-paste0(path_base,"data/v2")
    BASE_Shapes <- paste0(path_base, "shape")
    SCRIPT_DIR <- paste0(path_base, "Code/v6")
    
    ###########################################################
    ## 1. LOADING SOME FUNCTIONS
    library(rpart)
    # library(pROC)
    library(e1071)
    library(rattle)
    library(randomForest)
    library(caret)  
    library(MASS)  # para la regresion robusta
    # library(RGtk2)  
    library(restriktor) # para la regresion con restricciones
    library(Metrics)   #para los errores relativos
    
    ### libraries for the plot
    library("sp")
    library("maptools")
    library("rgdal")
    
    library("fields")
    library("RColorBrewer")
    library("classInt")
    
    
    ##########################################################
    ## 2. LOADING MY FUNCTIONS
    source(paste(SCRIPT_DIR, "functions3.R",sep="/") )

    ############################  
    ## 3. LOADING THE DATA SET:
    setwd(path_data)
     #con este fichero tengo un problema convierte un nut3 en fecha al pasarlo a csv (si lo abro con excel), asi que cuidado
    Pest_Row_dat <- read.csv("pest_emissions_w_corine_v2.csv",header=TRUE,sep=",")   #las cantidades aplicadas de cada pesticida
    Datos_clima <- read.csv("env_predictors.csv",header=TRUE,sep=",")      # clima
    Datos_crops  <- read.csv("clcEuroStat_NUTS3_v2.csv",header=TRUE,sep=",",fileEncoding="UTF-8-BOM")   #uso del suelo
    Datos_categ <- read.csv("Categories.csv",header=TRUE,sep=",")   # relacion categorias y principios activos
    Legal_autoriz <- read.csv("autorizaciones.csv",header=TRUE,sep=",") 
    latlong <- read.csv("LatLong.csv",header=TRUE,sep=",",fileEncoding="UTF-8-BOM") 
    UK_valid <- read.csv("UK_validation.csv",header=TRUE,sep=",",fileEncoding="UTF-8-BOM") 
    Pest_Row_dat$COUNTRY<- as.factor(Pest_Row_dat$COUNTRY)
    Datos_crops$NUTS_ID


    
    
    ## 4. DATA FRAME MEZCLANDO CLIMA, LONG LAT Y CROPS, tambien el country
    #Mezclo al clima la latitud y longitud
    Datos_clima2 <- merge(x = Datos_clima, y = latlong, by = "NUTS_ID", all.x = TRUE)  #left joint :quiero solo que se queden datos de cuando hay pesticidas
    names(Datos_crops)
    nrow(Datos_crops)
    Dat_clima_crop <- merge(x = Datos_crops, y = Datos_clima2, by = "NUTS_ID", all.x = TRUE)  #left joint :quiero solo que se queden datos de cuando hay pesticidas
    names(Dat_clima_crop)  #este DF servira para predecir en los paises que nos falta
    length (unique(Dat_clima_crop$NUTS_ID) )
    
    
    
    
    ######################################3
    ## AREAS agricolas POR TIPO DE CULTIVO EN CADA PAIS. PARA LOS PRINCIPALES AGRICOLAS
    Sumario_Areas(Dat_clima_crop)
    



  
  
    ################
    ## 2. PREPROCESO LOS DATOS
    
    ## 2.1 Datos de pesticidas   ###############
    names(Pest_Row_dat)
    str(Pest_Row_dat)
    head(Pest_Row_dat)
    unique(Pest_Row_dat$NUTS3)
    # "ID_PA"  : el identificador de cada pesticida (es un numero, que deberia ser un factor)
    # "COUNTRY"      "COUNTRY_name" 
    # "NUTS2" :             regiones, hay 130 regiones        
    # "NUTS3"        "NUTS3_name"  : las 770 provincias: 
    # "KG_FINAL"   
    # "HA_FINAL"     
    # "ARABLE_KG"   
    # "RICE_KG"     
    # "FRUIT_KG"   
    # "GRASS_KG"  
    # "OLIVES_KG"   
    # "VINES_KG" 
    # "Ratio"
    
    
    # Francesco limito las areas a la correspondencia entre a que crops se puede aplicar un pesticida y el area de ese pesticida
          table(Pest_Row_dat$HA_FINAL == 0)/nrow(Pest_Row_dat)*100
          table(Pest_Row_dat$KG_FINAL == 0)/nrow(Pest_Row_dat)*100
          table(Pest_Row_dat$HA_FINAL == 0,Pest_Row_dat$KG_FINAL==0) 
          table(Pest_Row_dat$HA_FINAL == 0,Pest_Row_dat$KG_FINAL==0)/nrow(Pest_Row_dat)*100
          
          # Que los Kg_TOTAL aplicado sea cero lo interpreto como que no se aplica el pesticida. Sin embargo, que las Ha_TOTAL sean cero cuando no se aplica pesticida (KG_TOTAL=0) puede ser normal, pero si los Kg_Total no son nulos implica que se ha aplicado algún pesticida en zonas donde no debería haberse aplicado (porque no debería haber cultivos adecuados para esos fertilizantes) Esto solo ocurre el 0.5% de los datos. 
          #veamos el subseting, cuando se aplican pesticidas y no hay area
          temp_DF <- Pest_Row_dat[ Pest_Row_dat$KG_FINAL > 0 & Pest_Row_dat$HA_FINAL==0,  ]
          nrow(temp_DF)
          unique(temp_DF$COUNTRY)
          unique( temp_DF[temp_DF$COUNTRY=="ES", ]$ID_PA)  # pesticidas en los que ocurre en espana
          #  1119      y 1275 en ES120 ES130  ES212  ES533 ES704
          #  1333  en varias provincias de  ES21 ES70 ES41
          #  1525  en ES120 ES130 ES212 ES531 ES533 ES704
          unique( temp_DF[temp_DF$COUNTRY=="IT", ]$ID_PA) 
          #  866, 904,  907, 937,  948, 979 ,1010
          unique( temp_DF[temp_DF$COUNTRY=="FR", ]$ID_PA) 
          #  856 ,  866, 870 , 904 , 910 ,979 , 1010  ,1181  
          
          # ELIMINO LOS REGISTROS  kg_Total>0 y Ha_Total=0. 
          Pest_Row_dat <- Pest_Row_dat[!(Pest_Row_dat$KG_FINAL > 0 & Pest_Row_dat$HA_FINAL==0),  ]
          nrow(Pest_Row_dat)
          
    
    
    
    ####### LO SIGUIENTE SI LO EJECUTO
    
    # CALCULO LA NUEVA VARIABLE RATIO (KG/HA), pero cuando ambas son cero, en lugar indeterminacion, pongo un cero
    Pest_Row_dat$Ratio <- Pest_Row_dat$KG_FINAL / Pest_Row_dat$HA_FINAL
    Pest_Row_dat$Ratio[is.nan(Pest_Row_dat$Ratio)]<-0
    nrow(Pest_Row_dat)
    names(Pest_Row_dat)  
    str(Pest_Row_dat)
    Pest_Row_dat$ID_PA <- factor(Pest_Row_dat$ID_PA)
    str(Pest_Row_dat)
    # ID_PA       : Factor w/ 310 levels  
   
    # calcula el numero de provincias en cada pais
    df2 <-aggregate(paste(NUTS3, COUNTRY) ~ NUTS3 + COUNTRY, data = Pest_Row_dat, FUN = NROW)
    refProvTable <- table(df2$COUNTRY )
    
    
    
    
    ## 3. LOADING THE SHAPE FOR THE MAPS
    # cargamos todos los shapes de los nuts3
    TotalShapes <- Loading_Shapes(BASE_Shapes,"NUTS3_sub.shp")
    # hago el subseting de los nuts que empleare en el modelado (PARA QUE SE VEA MEJOR EL RESULTADO)
    ShapesToPlot <- Subsetingshapes(TotalShapes,Pest_Row_dat)
    # Para cada compuesto se debe hacer otro subseting, porque puede ourrir que sea en concreto ese del que no hay datos
    

    
    
  #################################################
  ## SELECCION DEL PRINCIPIO ACTIVO A ANALIZAR
  ###############################################
    # Seleccion por categoria  # hago subgrupos/categorias por categoria de compuesto
    theCatg <- unique(Datos_categ$Category)
    TC <- theCatg[4]      #1 PG:PLANT GROWTH    3 HB:herbicida           FU: funguicida         IN:insecticida
    id_selec_Pest <- Datos_categ[Datos_categ$Category %in% TC,]$ActiveSubstances_ID
    nrow(Pest_Row_dat)   
    names(Pest_Row_dat)
    Pest_Row_dat_tipo <- Pest_Row_dat[Pest_Row_dat$ID_PA %in% id_selec_Pest,]  #subseting algun tipo
    nrow(Pest_Row_dat_tipo)  
    
    # Seleccion de los 15 compuestos de esa categoria mas usados (en kg totales)
    length(unique(Pest_Row_dat_tipo$ID_PA))  # cuantos compuestos hay de esa categoria
    TotKg_pest_tipo <- aggregate(Pest_Row_dat_tipo$KG_FINAL, by=list(Category=Pest_Row_dat_tipo$ID_PA), FUN=sum)
    TotKg_pest_tipo[rev(order(TotKg_pest_tipo$x)),]   #todos los compuestos del tipo seleccionado ordenados
    id_selec_Pest2 <- TotKg_pest_tipo[rev(order(TotKg_pest_tipo$x)),]   # me quedo con los 10 mas consumidos
    id_selec_Pest2$rate <- id_selec_Pest2$x/ sum(id_selec_Pest2$x) *100          # ratio del total
    id_selec_Pest3 <- id_selec_Pest2$Category    #[1:15]

    descrip_heatmap_pest(Pest_Row_dat, TC, id_selec_Pest3,refProvTable, path_data) 
      

    as <- 6
      # 3.3 Preprocesado del compuesto para aplicar regresion relativa al clima
      id_selec_Pest <-id_selec_Pest3[as]    # introduzco el indice del compuesto de esa categoria
      kg_comp <- id_selec_Pest2[id_selec_Pest2$Category ==id_selec_Pest,]$x

      names(Pest_Row_dat)
      nrow(Pest_Row_dat) #todos los registros del fichero
      Pest_Row_dat_regre <- Pest_Row_dat[Pest_Row_dat$ID_PA %in% id_selec_Pest,]
      nrow(Pest_Row_dat_regre)
  
      
      names(Datos_categ)
      theId <- Datos_categ[Datos_categ$ActiveSubstances_ID == id_selec_Pest, ]$ActiveSubstances_ID
      thename <- Datos_categ[Datos_categ$ActiveSubstances_ID == id_selec_Pest, ]$ActiveSubstance_name

        

    #################################################
    ## SUMMARY DESCRIPTIVE OF THE PESTICIDES QUANTiTiS 
    ###########################################  
    descrip_summary_pest(Pest_Row_dat_regre, TC, id_selec_Pest,refProvTable) 
      

      
            # nrow(Pest_Row_dat_regre) #solo los registros de ese compuesto
            # Pest_Row_dat_regre <- Pest_Row_dat_regre[Pest_Row_dat_regre$KG_FINAL != 0, ]  #elimino los ceros
            # nrow(Pest_Row_dat_regre)  # No esta claro que deba eliminar los registros de kg nulo
            # # se podria eliminar alguna pais 
            # #   clima_Pest_regre<- clima_Pest_regre[clima_Pest_regre$COUNTRY!= "DK", ]
            # #   clima_Pest_regre$COUNTRY <- droplevels(clima_Pest_regre$COUNTRY)
            # clima_Pest_regre <- clima_Pest_regre[!(clima_Pest_regre$HA_FINAL==0),]  #elimina areas a cero, para no tener infinitos
            # nrow(clima_Pest_regre)
            # unique(clima_Pest_regre$COUNTRY)
            # 
            # VarToPlot <- Pest_Row_dat_regre
            
    #################################################
    ## MAPEADO
    ###########################################
    names(Pest_Row_dat_regre)    
    palet <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
    styl <- "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
    ncl <-7 
    Co_list <-"ALL" #  c("ES","FR")     "ALL"    
    Range <- NULL  # c(0,1.5)  # ajustando la escala
    
    theVar <- "Ratio"
    Map_var(TotalShapes,Pest_Row_dat_regre, TG, ThePA, theVar, palet,styl,ncl,Co_list,Range)
    
    Range <- NULL
    
    theVar <- "KG_FINAL"  # 5 "KG_FINAL"   6"HA_FINAL"   7"ARABLE_KG"  [8] "RICE_KG"
    # 9 "FRUIT_KG"   10"GRASS_KG"  11"OLIVES_KG"  12"VINES_KG"   13"Ratio"         
    Map_var(TotalShapes,Pest_Row_dat_regre, TG, ThePA, theVar, palet,styl,ncl,Co_list,Range)

    theVar <- "ARABLE_KG"
    Map_var(TotalShapes,Pest_Row_dat_regre, TG, ThePA, theVar, palet,styl,ncl,Co_list,Range)
    
    theVar <- "GRASS_KG"
    Map_var(TotalShapes,Pest_Row_dat_regre, TG, ThePA, theVar, palet,styl,ncl,Co_list,Range)
    
    theVar <- "FRUIT_KG"
    Map_var(TotalShapes,Pest_Row_dat_regre, TG, ThePA, theVar, palet,styl,ncl,Co_list,Range)
    
    theVar <- "OLIVES_KG"
    Range <- NULL
    Map_var(TotalShapes,Pest_Row_dat_regre, TG, ThePA, theVar, palet,styl,ncl,Co_list, Range)
    
    
    theVar <- "VINES_KG"
    Range <- NULL
    Map_var(TotalShapes,Pest_Row_dat_regre, TG, ThePA, theVar, palet,styl,ncl,Co_list, Range)
    
    theVar <- "RICE_KG"
    Range <- NULL
    Map_var(TotalShapes,Pest_Row_dat_regre, TG, ThePA, theVar, palet,styl,ncl,Co_list, Range)
    
       
    #################################################
    ## REGRESION BASE USO DEL SUELO
    ###########################################               

  
    file <- paste0(path_data,"/MinMaxCo_ArableHB.csv")
    CoMinMax<- read.csv(file =file )  
    
      # as <- 3
    id_selec_Pest <-id_selec_Pest3[as] 
    theId <- Datos_categ[Datos_categ$ActiveSubstances_ID == id_selec_Pest, ]$ActiveSubstances_ID
    thename <- Datos_categ[Datos_categ$ActiveSubstances_ID == id_selec_Pest, ]$ActiveSubstance_name
    
    Pest_Row_dat_regre <- Pest_Row_dat[Pest_Row_dat$ID_PA %in% id_selec_Pest,]

    ##############################################3
    ## DATOS De superficie Agricola   ###############
    #############################################
    names(Datos_crops)
    # la primera y segunda fila son id de los crops y nombres largos. Los semapro
    CropNames_Id <- Datos_crops[1,]  #es es la correspondencia
    Datos_crops <-  Datos_crops[2:nrow(Datos_crops),]     # y la elimino del DF que usare
    names(Datos_crops)[1] <- c("NUTS_ID")  #me gusta mas ese nombre para la primera variable
    str(Datos_crops) 
    #los carga todos como factores, los debo convertir a numero y calcular las proporciones respecto a arable
    cols = c(2:length(Datos_crops));    
    Datos_crops[,cols] = apply(Datos_crops[,cols], 2, function(x) as.numeric(as.character(x)));
    str(Datos_crops) 
    
    # MERGE AREAS CON PESTICIDAS  (considerando el PA que se selecciono?????)
    nrow(Pest_Row_dat_regre)
    table( unique(Datos_crops$NUTS_ID) %in% unique(Pest_Row_dat_regre$NUTS3) )  #atencion que aqui estan todos los NUTS3 (provincias), y para los pesticidas no tenemos todas
    Datos_SupAgr_Pest <- merge(x = Pest_Row_dat_regre, y = Datos_crops, by.x = "NUTS3", by.y="NUTS_ID", all.x = TRUE)  #left joint :quiero solo que se queden datos de cuando hay pesticidas
    length( unique(Datos_SupAgr_Pest$NUTS3) )
    head(Datos_SupAgr_Pest)
    names(Datos_SupAgr_Pest)
    
     
    length(unique(Datos_clima2$NUTS_ID))
    # MERGE AREAS, PESTICIDAS Y CLIMA (seleccion variables clima)
    names(Datos_clima2)
    #Asi que las variables que quedarian son:
    ClimaSelectV <-c("NUTS_ID","P.M10_min","P.M10_max","P.M10_stde","P.M12_min","P.M12_max","P.M3_min","P.M4_min",  
                     "P.M4_max","P.M4_mean","P.M4_stdev","P.M5_max","P.M5_stdev","P.M6_min",  
                     "P.M6_max","P.M7_stdev","P.M9_max","P.M9_stdev","P.Y_min","TEFF_min",  
                     "TEFF_max","TEFF_mean","TEFF_stdev","T.M12_max","Lon","Lat" ) 
    Datos_clima3<- Datos_clima2[,(names(Datos_clima2) %in% ClimaSelectV)]
    # las mezclados, con todas o con parte de las variables de clima
    Datos_SupAgr_clima_Pest <- merge(x = Datos_SupAgr_Pest, y = Datos_clima3, by.x = "NUTS3", by.y="NUTS_ID", all.x = TRUE)  #left joint :quiero solo que se queden datos de cuando hay pesticidas
    nrow(Datos_SupAgr_clima_Pest)
    unique(Datos_SupAgr_clima_Pest$COUNTRY)

    # C0000: Cereals for the production of grain
    # G0000:  Plants harvested green from arable land
    #       G1000: Temporary grasses and grazings  (NO)
    # I0000: Industrial crops 
    # P0000: Dry pulses and protein crops for the production of grain 
    # Q0000: Fallow land
    # R0000: Root crops
    # V0000_S0000:  Fresh vegetables (including melons) and strawberries
    # E0000: Seeds and seedlings
    # N0000: Flowers and ornamental plants (excluding nurseries)
    # K0000: Kitchen gardens     (NO)
    # L0000: Nurseries
    # 
    # F0000: Fruits, berries and nuts (excluding citrus fruits, grapes and strawberries)
    # T0000: Citrus fruits
    # 
    # O1000: Olives
    # 
    # W1000: Grapes
    # 
    # J0000:  Permanent grassland
    # J1000:  Permanent pastures and meadows
    # J2000:  Permanent rough grazings
    # J3000ES:  Permanent agricultural grassland not in use, eligible for subsidies
    
    # 6. ARABLE  (alternativa 2)
    stCrop <- "ARABLE_KG"
#    lVarLand <- c("I0000","R0000","C0000","G0000")
    lVarLand <- c("C0000","G0000","I0000","Q0000","R0000","P0000","V0000_S0000","E0000","L0000")  #varuables explicativas relativas con el area
    lVarClima <- names(Datos_SupAgr_clima_Pest)[c(94:117)]  #    # c(93:117)  c(94:118)] 
    #  lVarClima <-  <-  lVarClima[4]
    ThePreli <- Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest)  

      maxnumvar <- ifelse( length(ThePreli[[2]])<10 ,length(ThePreli[[2]]),7)
    
    Aver_Count <- ThePreli[[1]]
   #   Aver_Count <- ThePreli[[1]][!( ThePreli[[1]] %in% c("NL","BE"))]     #si queremos modelar sin algun pais

    lVarLand <- lVarLand[lVarLand %in%  ThePreli[[2]][1:maxnumvar]]  # de las 15 primeras variable busco si hay alguna de crops
    AditionVars <- ThePreli[[2]][1:maxnumvar] [ !(ThePreli[[2]][1:maxnumvar]  %in% lVarLand) ]
    AditionVars <- AditionVars[!AditionVars %in% "Lon"]   #Elimino Lon que a veces es problematico 
   # AditionVars <- AditionVars[1]
    # 6.2 Regresion   
  #  lVarLand <- c("R0000")   # "C0000","G0000","I0000","Q0000","R0000","P0000","V0000_S0000"
    Min_Count <- c("IT") # CoMinMax[CoMinMax$X==id_selec_Pest,c("MinCo")]  #  c("FR") # NULL    c("ES")   #  c("IE")  #  NULL  #  c("IE") # NULL  #  c("IT")  #   c("IT")   #  NULL  #  c("ES")  # NULL #    #aunque no se si fiarme mucho
    Max_Count <-  c("IT") # CoMinMax[CoMinMax$X==id_selec_Pest,c("MaxCo")]  #
   #  Max_Count <-  c("es")  #  descarto DE. Aplica pero sin ningun area
   # Aver_Count <- c("BE","DE","ES","IE","IT","NL","DK") #   "ALL" #  "FR","DE","ES","IT","NL","BE","DK","IE" #  c("ES","DE","IT","NL","BE","DK )  #        c("DE", "FR") # "ALL" #   #"FR", "ALL"
    RegType <- c("lm","lm","lm" )  # MinMod, MaxMod, AvMod. lm:linearmodel; rlm: robust linear model
#   AditionVars <- c("P.M9_max")   NULL #  c("P.M10_min","P.M4_min") # NULL #  ,"Lon","P.Y_min"  "TEFF_max","P.M10_stde","P.M6_max" ,"P.M3_min","P.M4_max","T.M12_max" #  c("P.M6_max","P.M5_max","TEFF_max","P.Y_min","T.M12_max") #  NULL # c("P.Y_min") #    c("P.M12_min","Lon","P.M10_min")   #   # NULL # c("TEFF_mean","P.M4_mean","P.M12_min","P.M6_max","P.M12_max" "P.Y_min")  # c("TEFF_maX")   #  c("TEFF_mean","TEFF_min") #  c("TEFF_max","P.Y_min")  # c("TEFF_max","P.M12_max","P.M10_max")  #   NULL  # names(Datos_SupAgr_clima_Pest)[ c(101,114,96,99)]    # NULL   names(Datos_SupAgr_clima_Pest)[ c(110,103,105)] # NULL   # names(Datos_SupAgr_clima_Pest)[c(96,107)]  #  names(Datos_SupAgr_clima_Pest)[c(24)] # NULL # names(Datos_SupAgr_clima_Pest)[c(100,98,106)]    #  c("P.M4_max","P.M9_max","P.Y_min","P.M5_max")   # "Lat","Lon",   NULL
    ouputA <-NULL
    ouputA <- Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                                   Min_Count,Max_Count,Aver_Count,RegType, AditionVars, path_out ,TC,id_selec_Pest,Legal_autoriz)

    ouputA[[1]]  # lm model coeficients
    ouputA[[2]]  # lm constraint model coeficients
    ouputA[[5]]  # metricas:  c(raeMod1Big,raeMod2Big,raeMod1Small,raeMod2Small, mapeMod1Big,mapeMod2Big,mapeMod1Small,mapeMod2Small  )
    res <- NULL
    res <- CrosVal(Datos_SupAgr_clima_Pest,Aver_Count,stCrop,lVarLand,AditionVars,ouputA[[1]], path_out ,TC,id_selec_Pest)
    writesummary(path_out ,TC,id_selec_Pest,stCrop,lVarLand,AditionVars,Min_Count,Max_Count,Aver_Count,RegType, ouputA[[1]],  ouputA[[2]] ,res )
    
  #   write.csv(Datos_SupAgr_clima_Pest,"C:/ANGEL/2020MAYO/Pesticidas/output/Prediccion/HB/1438/TrainGlifosato2.csv", row.names = FALSE)
  #   write.csv(Dat_clima_crop,"C:/ANGEL/2020MAYO/Pesticidas/output/Prediccion/HB/1438/PredictGlifosato.csv", row.names = FALSE)
    
    



             #######
             #######
          # 1. GRASS
          stCrop <- "GRASS_KG"
          lVarLand <- c("J0000")  #varuables explicativas relativas con el area
          lVarClima <- names(Datos_SupAgr_clima_Pest)[c(94:117)]  #    # c(93:117)  c(94:118)] 
          ThePreli <- Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest)  
          
          maxnumvar <- ifelse( length(ThePreli[[2]])<5 ,length(ThePreli[[2]]),2)
          
          Aver_Count <- ThePreli[[1]]
    #      Aver_Count <- c("BE","DE","ES","FR","IE","IT","NL")
           AditionVars <- ThePreli[[2]][1:maxnumvar] [ !(ThePreli[[2]][1:maxnumvar]  %in% lVarLand) ]
          AditionVars <- AditionVars[!AditionVars %in% "Lon"]   #Elimino Lon que a veces es problematico 
          Min_Count <- NULL   # CoMinMax[CoMinMax$X==id_selec_Pest,c("MinCo")]  #  c("FR") # NULL    c("ES")   #  c("IE")  #  NULL  #  c("IE") # NULL  #  c("IT")  #   c("IT")   #  NULL  #  c("ES")  # NULL #    #aunque no se si fiarme mucho
          Max_Count <- "FR"
           RegType <- c("lm","lm","lm" )  # MinMod, MaxMod, AvMod. lm:linearmodel; rlm: robust linear model
          ouputA<-NULL
          ouputA <- Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                                         Min_Count,Max_Count,Aver_Count,RegType, AditionVars, path_out ,TC,id_selec_Pest,Legal_autoriz)


          # 2. OLIVES
          stCrop <- "OLIVES_KG"
          lVarLand <- c("O1000") 
          lVarClima <- names(Datos_SupAgr_clima_Pest)[c(94:117)]  #    # c(93:117)  c(94:118)] 
          ThePreli <- Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest)  
          
          maxnumvar <- ifelse( length(ThePreli[[2]])<10 ,length(ThePreli[[2]]),4)
          Aver_Count <- ThePreli[[1]]  # c("ES,"FR","IT")
          AditionVars <- ThePreli[[2]][1:maxnumvar] [ !(ThePreli[[2]][1:maxnumvar]  %in% lVarLand) ]
          AditionVars <- AditionVars[!AditionVars %in% "Lon"]   #Elimino Lon que a veces es problematico 
          Min_Count <- c("ES")  #     c("IT")  # c("IT")  # NULL    "DK",
          Max_Count <- c("IT")  #  "DK",
          ouputO <-NULL
          ouputO <- Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                                         Min_Count,Max_Count,Aver_Count,RegType, AditionVars, path_out ,TC,id_selec_Pest,Legal_autoriz)
          
          # 3. VINES   
          stCrop <- "VINES_KG"
          lVarLand <- c("W1000")  #varuables explicativas relativas con el area
          lVarClima <- names(Datos_SupAgr_clima_Pest)[c(94:117)]  #    # c(93:117)  c(94:118)] 
          ThePreli <- Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest)  
          
          maxnumvar <- ifelse( length(ThePreli[[2]])<10 ,length(ThePreli[[2]]),2)
          Aver_Count <- ThePreli[[1]]
          AditionVars <- ThePreli[[2]][1:maxnumvar] [ !(ThePreli[[2]][1:maxnumvar]  %in% lVarLand) ]
          AditionVars <- AditionVars[!AditionVars %in% "Lon"]   #Elimino Lon que a veces es problematico 
          Min_Count <- NULL #     c("IT")  # c("IT")  # NULL    "DK",
          Max_Count <- c("FR")  #  "DK",
          ouputW <-NULL
          ouputW <- Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                                         Min_Count,Max_Count,Aver_Count,RegType, AditionVars, path_out ,TC,id_selec_Pest,Legal_autoriz)
          

          # 4. FRUITS
          stCrop <- "FRUIT_KG"
          lVarLand <- c("F0000","T0000")  # cuando hay dos conviene ejecutar tambien separadamente.varuables explicativas relativas con el area
          lVarClima <- names(Datos_SupAgr_clima_Pest)[c(94:117)]  #    # c(93:117)  c(94:118)] 
          ThePreli <- Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest)  
          
          maxnumvar <- ifelse( length(ThePreli[[2]])<10 ,length(ThePreli[[2]]),5)
          Aver_Count <- ThePreli[[1]]
          AditionVars <- ThePreli[[2]][1:maxnumvar] [ !(ThePreli[[2]][1:maxnumvar]  %in% lVarLand) ]
          AditionVars <- AditionVars[!AditionVars %in% "Lon"]   #Elimino Lon que a veces es problematico 
          Min_Count <- NULL #     c("IT")  # c("IT")  # NULL    "DK",
          Max_Count <- c("FR")  #  "DK",
          ouputF <-NULL
          ouputF <- Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                                         Min_Count,Max_Count,Aver_Count,RegType, AditionVars, path_out ,TC,id_selec_Pest,Legal_autoriz)
          
          # 6. RICE
          stCrop <- "RICE_KG"
          lVarLand <- c("C2000")  #varuables explicativas relativas con el area
          lVarClima <- names(Datos_SupAgr_clima_Pest)[c(94:117)]  #    # c(93:117)  c(94:118)] 
          ThePreli <- Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest)  
          
          maxnumvar <- ifelse( length(ThePreli[[2]])<4 ,length(ThePreli[[2]]),4)
          Aver_Count <- ThePreli[[1]]
          AditionVars <- ThePreli[[2]][1:maxnumvar] [ !(ThePreli[[2]][1:maxnumvar]  %in% lVarLand) ]
          AditionVars <- AditionVars[!AditionVars %in% "Lon"]   #Elimino Lon que a veces es problematico 
          Min_Count <- NULL #     c("IT")  # c("IT")  # NULL    "DK",
          Max_Count <- c("FR")  #  "DK",
          ouputR<-NULL
          ouputR <- Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                                         Min_Count,Max_Count,Aver_Count,RegType, AditionVars, path_out ,TC,id_selec_Pest,Legal_autoriz)
          
          
          
            
          

              #######
              #######

  ########################################################
  # ESCRIBE CSV CON LAS PREDICCIONES 
  ########################################################
  DF_predicc <- NULL
        #    id_selec_Pest <- 1925
  path_out <- "C:/ANGEL/2020SEP/Pesticidas/output"   
  path_2 <- "/Prediccion/FU/"      #la parte correspondiente a que PA
  path_2 <- paste0(path_2,id_selec_Pest) 
  # Indicar si alguno de los cultivos no se hace
  ListOfMod <- c(1,0,0,0,0,0)  #   "ARA","FRUIT","OLIV","VINES","GRASS","RICE"
  DF_predicc <- read_prediccion(path_out,path_2,TC,id_selec_Pest,ListOfMod)  #devuelve una lista: min, average, max  
  
  # Lo anterior tambien genera el fichero para Chiara
  
  ## PLOT COMPARANDO VALORES REALES Y MODELADOS (BY COUNTRY)
  Compare_real_vs_prediccion(path_out,path_2,TC,id_selec_Pest,ListOfMod,Datos_SupAgr_clima_Pest) 
  
  
  #Los paises training: subsetring por AS (seguramente no hace falta) y ordenacion para poder unirlo
  DF_TotalToMap <- Datos_SupAgr_clima_Pest[Datos_SupAgr_clima_Pest$ID_PA==id_selec_Pest , c("NUTS3", "ARABLE_KG","FRUIT_KG",
                                                                                            "OLIVES_KG","VINES_KG","GRASS_KG","RICE_KG","KG_FINAL","COUNTRY")]
  names(DF_TotalToMap) <- names(DF_predicc[[1]]) 
  
  #lo uno para tres casos: Min, Average, Max
  DF_TotalToMap_Min <- rbind(DF_predicc[[1]], DF_TotalToMap)
  DF_TotalToMap_Av <- rbind(DF_predicc[[2]], DF_TotalToMap)
  DF_TotalToMap_Max <- rbind(DF_predicc[[3]], DF_TotalToMap)
  
  
  #############################################
  #######  VALIDACION UK  #############
  ##########################################
  TheMap <- DF_TotalToMap_Av 
  R1_Uk <- NULL
  (R1_Uk <- Validation_UK(UK_valid,TheMap))
  
  
  
  DF_UK <- rbind(DF_UK, data.frame(id=theId,Name=as.character(thename),
                                   NRMSE= R1_Uk[1], 
                                   SMAPE= R1_Uk[2],
                                   NMAE= R1_Uk[3],
                                   N_S= R1_Uk[4]      )     ) 
      
      
  






  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
    
    ###########################################################
    ##############################################333
    ## DESCRIPTIVE OF THE CROP AREA
    
    #hago el analisis en base a este hervicida, pero deberia hacerse en base a varios princiios activos
    descrip_summary_land(Datos_SupAgr_clima_Pest,TC,id_selec_Pest,refProvTable)
    Summary_land_PA(Datos_SupAgr_clima_Pest,TC,id_selec_Pest,refProvTable)

    
    
    ########################################################
    # REGRESION POR CULTIVOS 
    ########################################################
    source(paste(SCRIPT_DIR, "functions.R",sep="/") )
    names(Datos_SupAgr_clima_Pest)    #para el conjunto a entrenar
    names(Dat_clima_crop)    #para el conjunto a predecir
    
    #   closeAllConnections()
    
    # 1. GRASS
       # 1.1 Regresion Preliminar  
       stCrop <- "GRASS_KG"
       lVarLand <- c("J0000","G1000")  #varuables explicativas relativas con el area
       lVarClima <-names(Datos_SupAgr_clima_Pest)[c(100,108,116)]   #   NULL  #  c(93:117)
       Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest )   
        #ahora escojo modelo de max y de min y genero un csv con output para todos los paises
       
       # 1.2 Regresion
       Min_Count <- NULL  #  "IE" #    NULL
       Max_Count <- c("DK")
       Aver_Count <- c("IT", "FR", "IE") #  c("FR")        "ALL" 
       RegType <- c("lm","lm","lm" )
       lVarLand <- c("G1000")   # ,
       AditionVars <- c("P.M7_stdev") #  c("P.M4_max","P.M9_max","P.Y_min","P.M5_max")   # "Lat","Lon",   NULL
       ouputG <- Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                            Min_Count,Max_Count,Aver_Count,RegType,AditionVars, path_out ,TC,id_selec_Pest)
       ouputG[[1]]  # lm model coeficients
       ouputG[[2]]  # lm constraint model coeficients
       ouputG[[5]]  # metricas: 
       resG <-CrosVal(Datos_SupAgr_clima_Pest,Aver_Count,stCrop,lVarLand,AditionVars,ouputG[[1]], path_out ,TC,id_selec_Pest)
       writesummary(path_out ,TC,id_selec_Pest ,stCrop,lVarLand,AditionVars,Min_Count,Max_Count,Aver_Count,RegType, ouputG[[1]],  ouputG[[2]]  ,resG )
      
     
    # 2. OLIVES
       # 2.1 Regresion Preliminar  
       stCrop <- "OLIVES_KG"
       lVarLand <- c("O1000")  #variables explicativas relativas con el area
       lVarClima <- names(Datos_SupAgr_clima_Pest)[ c(93:117)]   # c(93:117)  c(100,98,106)
       Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest )   
       
       # 2.2 Regresion
       Min_Count <- c("IT") #     c("IT")  # c("IT")  # NULL    "DK",
       Max_Count <- c("ES")  #  "DK",
       Aver_Count <- c("IT","ES") # "ALL" #  c("ES","IT")  #    # c("ES","FR","IT")   # "ALL"   Quizas se podria poner una opcion sin outliers
       RegType <- c("lm","lm","lm" )  # MinMod, MaxMod, AvMod. lm:linearmodel; rlm: robust linear model
       AditionVars <- c("P.M10_max","Lon" )  # NULL # names(Datos_SupAgr_clima_Pest)[c(117)] # NULL # names(Datos_SupAgr_clima_Pest)[c(100,98,106)]    #  c("P.M4_max","P.M9_max","P.Y_min","P.M5_max")   # "Lat","Lon",   NULL
       ouputO <- Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                            Min_Count,Max_Count,Aver_Count,RegType, AditionVars, path_out ,TC,id_selec_Pest)
       ouputO[[1]]
       ouputO[[2]]
       ouputO[[5]]
       resO <- CrosVal(Datos_SupAgr_clima_Pest,Aver_Count,stCrop,lVarLand,AditionVars,ouputO[[1]], path_out ,TC,id_selec_Pest)
       writesummary(path_out ,TC,id_selec_Pest ,stCrop,lVarLand,AditionVars,Min_Count,Max_Count,Aver_Count,RegType, ouputO[[1]],  ouputO[[2]]  ,resO )
       
    # 3. VINES   
       # 3.1 Regresion Preliminar 
       stCrop <- "VINES_KG"
       lVarLand <- c("W1000")  #varuables explicativas relativas con el area
       lVarClima <- names(Datos_SupAgr_clima_Pest)[c(93:117)]   #  NULL  # 
       Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest)  

       # 3.2 Regresion
       Min_Count <- NULL      #  NULL   c("IT") 
       Max_Count <-  c("FR")  #  
       Aver_Count <- c("ES","IT","FR")  #   "ALL"    #   c("IT","ES")   Quizas se podria poner una opcion sin outliers
       RegType <- c("lm","lm","lm" )  # MinMod, MaxMod, AvMod. lm:linearmodel; rlm: robust linear model
       AditionVars <-  c("TEFF_mean")#  NULL # names(Datos_SupAgr_clima_Pest)[c(24)] # NULL # names(Datos_SupAgr_clima_Pest)[c(100,98,106)]    #  c("P.M4_max","P.M9_max","P.Y_min","P.M5_max")   # "Lat","Lon",   NULL
       ouputV <- Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                            Min_Count,Max_Count,Aver_Count,RegType, AditionVars, path_out ,TC,id_selec_Pest)
       ouputV[[1]]
       ouputV[[2]]
       ouputV[[5]]   
       resV <- CrosVal(Datos_SupAgr_clima_Pest,Aver_Count,stCrop,lVarLand,AditionVars,ouputV[[1]], path_out ,TC,id_selec_Pest)
       writesummary(path_out ,TC,id_selec_Pest ,stCrop,lVarLand,AditionVars,Min_Count,Max_Count,Aver_Count,RegType, ouputV[[1]],  ouputV[[2]]  ,resV )
       
       
   # 4. FRUITS
       # 4.1 Regresion Preliminar
       stCrop <- "FRUIT_KG"
       lVarLand <- c("F0000","T0000")  # cuando hay dos conviene ejecutar tambien separadamente.varuables explicativas relativas con el area
       lVarClima <- names(Datos_SupAgr_clima_Pest)[c(93:117)]   # c(93:117)
       Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest)  
       
       # 4.2 Regresion
       lVarLand <- c("T0000","F0000")
       Min_Count <- NULL #   c("IT","FR") #  NULL #    c("ES","IT")     #   c("IT")      #   c("ES")   NULL 
       Max_Count <- c("NL")  #   c("DE") 
       Aver_Count <- c("BE","FR","IT","IE","NL") # "ALL"  # c("DE","ES","FR","IT")   # "ALL"   Quizas se podria poner una opcion sin outliers
       RegType <- c("lm","lm","lm" )  # MinMod, MaxMod, AvMod. lm:linearmodel; rlm: robust linear model
       AditionVars <-c("P.M5_max","P.M6_max","P.M9_max","P.M4_mean")  #   NULL #   c("P.M4_mean") #  NULL # c("P.M14_min","P.Y_min" ) #  c("P.M12_min","P.M7_stdev")  # NULL #   names(Datos_SupAgr_clima_Pest)[c(98,112)]  #  NULL #  names(Datos_SupAgr_clima_Pest)[c(24)] # NULL # names(Datos_SupAgr_clima_Pest)[c(100,98,106)]    #  c("P.M4_max","P.M9_max","P.Y_min","P.M5_max")   # "Lat","Lon",   NULL
       ouputF <- Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                            Min_Count,Max_Count,Aver_Count,RegType, AditionVars, path_out ,TC,id_selec_Pest)
       ouputF[[1]]
       ouputF[[2]]
       ouputF[[5]]
       resF <- CrosVal(Datos_SupAgr_clima_Pest,Aver_Count,stCrop,lVarLand,AditionVars,ouputF[[1]], path_out ,TC,id_selec_Pest)
       writesummary(path_out ,TC,id_selec_Pest ,stCrop,lVarLand,AditionVars,Min_Count,Max_Count,Aver_Count,RegType, ouputF[[1]],  ouputF[[2]]  ,resF )
       
       
    # 5. RIZE
       # 5.1 Regresion Preliminar
       stCrop <- "RICE_KG"
       lVarLand <- c("C2000")  #varuables explicativas relativas con el area
       lVarClima <- names(Datos_SupAgr_clima_Pest)[c(93:117)]   # c(93:117)
       Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest)  
       
       # 5.2 Regresion
       Min_Count <- c("ES","DE")
       Max_Count <- c("ES","DE")  #  descarto DE. Aplica pero sin ningun area
       Aver_Count <- c("ES","DE","FR")  # "ALL"   Quizas se podria poner una opcion sin outliers
       RegType <- c("lm","lm","lm" )  # MinMod, MaxMod, AvMod. lm:linearmodel; rlm: robust linear model
       AditionVars <- NULL   # names(Datos_SupAgr_clima_Pest)[c(96,107)]  #  names(Datos_SupAgr_clima_Pest)[c(24)] # NULL # names(Datos_SupAgr_clima_Pest)[c(100,98,106)]    #  c("P.M4_max","P.M9_max","P.Y_min","P.M5_max")   # "Lat","Lon",   NULL
       Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                            Min_Count,Max_Count,Aver_Count,RegType, AditionVars, path_out ,TC,id_selec_Pest)
       
       
    # 6. ARABLE  (alternativa 1)
       # 6.1 Regresion Preliminar
       stCrop <- "ARABLE_KG"
       lVarLand <- c("ARA")  #varuables explicativas relativas con el area
       lVarClima <- names(Datos_SupAgr_clima_Pest)[ c(93:117)]   # c(93:117)
       Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest)  

       # 6.2 Regresion 
       Min_Count <- c("IT")  #  c("ES")  # NULL #    #aunque no se si fiarme mucho
       Max_Count <- c("IT")  #  descarto DE. Aplica pero sin ningun area
       Aver_Count <- c( "IT","IE","BE")   #"ALL"  # c("FR","DE","ES","IT","DK","NL")  #  "BE",   #  #  # "ALL"   Quizas se podria poner una opcion sin outliers
       RegType <- c("lm","lm","lm" )  # MinMod, MaxMod, AvMod. lm:linearmodel; rlm: robust linear model
       AditionVars <- NULL #  "ALL" # names(Datos_SupAgr_clima_Pest)[ c(117,113)]    # NULL   names(Datos_SupAgr_clima_Pest)[ c(110,103,105)] # NULL   # names(Datos_SupAgr_clima_Pest)[c(96,107)]  #  names(Datos_SupAgr_clima_Pest)[c(24)] # NULL # names(Datos_SupAgr_clima_Pest)[c(100,98,106)]    #  c("P.M4_max","P.M9_max","P.Y_min","P.M5_max")   # "Lat","Lon",   NULL
       Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                            Min_Count,Max_Count,Aver_Count,RegType, AditionVars, path_out ,TC,id_selec_Pest)
       
 
    # 6. ARABLE  (alternativa 2)
       stCrop <- "ARABLE_KG"
       lVarLand <- c("C0000","G0000","I0000","Q0000","R0000","P0000","V0000_S0000")  #varuables explicativas relativas con el area
       lVarClima <- names(Datos_SupAgr_clima_Pest)[c(94:117)]  #    # c(93:117)  c(94:118)] 
       Preliminar_regres( Datos_SupAgr_clima_Pest, stCrop,lVarLand,lVarClima,path_out ,TC,id_selec_Pest)  
      
       # 6.2 Regresion   
       lVarLand <- c("R0000","I0000","Q0000")   # "C0000","G0000","I0000","Q0000","R0000","P0000","V0000_S0000"
       Min_Count <- NULL  #  c("ES")   #  c("IE")  #  NULL  #  c("IE") # NULL  #  c("IT")  #   c("IT")   #  NULL  #  c("ES")  # NULL #    #aunque no se si fiarme mucho
       Max_Count <-  c("NL")  #  descarto DE. Aplica pero sin ningun area
       Aver_Count <- c( "FR","ES","IT","NL","DK","IE" ) #   "ALL" #  "FR","DE","ES","IT","NL","BE","DK","IE" #  c("ES","DE","IT","NL","BE","DK )  #        c("DE", "FR") # "ALL" #   #"FR", "ALL"
       RegType <- c("lm","lm","lm" )  # MinMod, MaxMod, AvMod. lm:linearmodel; rlm: robust linear model
       AditionVars <- c("P.M10_min","P.M4_min") # NULL #  ,"Lon","P.Y_min"  "TEFF_max","P.M10_stde","P.M6_max" ,"P.M3_min","P.M4_max","T.M12_max" #  c("P.M6_max","P.M5_max","TEFF_max","P.Y_min","T.M12_max") #  NULL # c("P.Y_min") #    c("P.M12_min","Lon","P.M10_min")   #   # NULL # c("TEFF_mean","P.M4_mean","P.M12_min","P.M6_max","P.M12_max" "P.Y_min")  # c("TEFF_maX")   #  c("TEFF_mean","TEFF_min") #  c("TEFF_max","P.Y_min")  # c("TEFF_max","P.M12_max","P.M10_max")  #   NULL  # names(Datos_SupAgr_clima_Pest)[ c(101,114,96,99)]    # NULL   names(Datos_SupAgr_clima_Pest)[ c(110,103,105)] # NULL   # names(Datos_SupAgr_clima_Pest)[c(96,107)]  #  names(Datos_SupAgr_clima_Pest)[c(24)] # NULL # names(Datos_SupAgr_clima_Pest)[c(100,98,106)]    #  c("P.M4_max","P.M9_max","P.Y_min","P.M5_max")   # "Lat","Lon",   NULL
       ouputA<-NULL
       ouputA <- Calcula_predicciones(Datos_SupAgr_clima_Pest, Dat_clima_crop,stCrop,lVarLand,
                            Min_Count,Max_Count,Aver_Count,RegType, AditionVars, path_out ,TC,id_selec_Pest)
       ouputA[[1]]  # lm model coeficients
       ouputA[[2]]  # lm constraint model coeficients
       ouputA[[5]]  # metricas:  c(raeMod1Big,raeMod2Big,raeMod1Small,raeMod2Small, mapeMod1Big,mapeMod2Big,mapeMod1Small,mapeMod2Small  )
       res <- NULL
       res <- CrosVal(Datos_SupAgr_clima_Pest,Aver_Count,stCrop,lVarLand,AditionVars,ouputA[[1]], path_out ,TC,id_selec_Pest)
       writesummary(path_out ,TC,id_selec_Pest,stCrop,lVarLand,AditionVars,Min_Count,Max_Count,Aver_Count,RegType, ouputA[[1]],  ouputA[[2]] ,res )
  

      # REVISAR ESTE ULTIMO, DA NEGATIVOS PARA LOS MINIMOS, SIN CON YO HAYA CAMBIADO NADA A RESPECTO AL MODELO DE TODO EL ARABLE
        
# DE LAS FIGURAS ANTERIORES SE SELECCIONA EL MODELO PARA CADA PA
# CON ELLOS SE CALCULA EL MINIMO Y EL MAXIMO, PARA CADA CATEGORIA DE CULTIVOS
# SE HACEN MAPAS, COMPARANDO CON EL VALOR REAL DEL QUE DISPONEMOS
# SE SUMAN LOS MINIMOS Y MAXIMOS Y SE HACE UN CSV CON DICHOS TOTALES
       # EL MAPA ES PARA TODA EUROPA
    
       
     ########################################################
     # CARGAMOS LAS PREDICCIONES 
     ########################################################
     DF_predicc <- NULL
     path_out <- "C:/ANGEL/2020MAYO/Pesticidas/output"
     path_2 <- "/Prediccion/FU/"      #la parte correspondiente a que PA
     path_2 <- paste0(path_2,id_selec_Pest) 
      # Indicar si alguno de los cultivos no se hace
     ListOfMod <- c(1,0,0,0,0,0)  #   "ARA","FRUIT","OLIV","VINES","GRASS","RICE"
     DF_predicc <- read_prediccion(path_out,path_2,TC,id_selec_Pest,ListOfMod)  #devuelve una lista: min, average, max  
       
     # Junto los resultados de los paises predichos, con los training
     names(Datos_SupAgr_clima_Pest)
     names(DF_predicc[[1]])
     
     Datos_SupAgr_clima_Pest[Datos_SupAgr_clima_Pest$ID_PA==id_selec_Pest,]$COUNTRY
     
     #Los paises training: subsetring por AS (seguramente no hace falta) y ordenacion para poder unirlo
     DF_TotalToMap <- Datos_SupAgr_clima_Pest[Datos_SupAgr_clima_Pest$ID_PA==id_selec_Pest , c("NUTS3", "ARABLE_KG","FRUIT_KG",
                       "OLIVES_KG","VINES_KG","GRASS_KG","RICE_KG","KG_FINAL","COUNTRY")]
     names(DF_TotalToMap) <- names(DF_predicc[[1]]) 
     
     #lo uno para tres casos: Min, Average, Max
     DF_TotalToMap_Min <- rbind(DF_predicc[[1]], DF_TotalToMap)
     DF_TotalToMap_Av <- rbind(DF_predicc[[2]], DF_TotalToMap)
     DF_TotalToMap_Max <- rbind(DF_predicc[[3]], DF_TotalToMap)
     # estos DF ya se podrian mapear

     
     ########################################################
     # SUMARIO DE LA PREDICCION 
     ########################################################
     sumary_predictions(DF_TotalToMap_Av,TC,id_selec_Pest )
    
    
     ########################
     # MAPEANDO LA PREDICCION
     #############################  
     names(DF_TotalToMap_Av)    
     palet <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
     styl <- "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
     ncl <-7
     Co_list <-"ALL" #  c("ES","FR")     "ALL"    
     Range <- c(0,400000)   #  NULL  # ajustando la escala
     theVar <- "Arab_KG"
     Map_var(TotalShapes,DF_predicc, TG, id_selec_Pest, theVar, palet,styl,ncl,Co_list,Range)
    # ahora mapeo un mapa total, con los paises de training y los predichos
     Map_var(TotalShapes,DF_TotalToMap_Av, TG, id_selec_Pest, theVar, palet,styl,ncl,Co_list,Range)
     
     ####
     TheMap <- DF_TotalToMap_Av  # DF_TotalToMap_Av   #de minimos maximos o averages

     theVar <- "Fruit_KG" 
     Map_var(TotalShapes,TheMap, TG, id_selec_Pest, theVar, palet,styl,ncl,Co_list,Range)   
    
     theVar <- "Grass_KG" 
     Map_var(TotalShapes,TheMap, TG, id_selec_Pest, theVar, palet,styl,ncl,Co_list,Range)   
     
        
     theVar <- "Vines_KG" 
     Map_var(TotalShapes,TheMap, TG, id_selec_Pest, theVar, palet,styl,ncl,Co_list,Range)   
     
     theVar <- "Oliv_KG" 
     Map_var(TotalShapes,TheMap, TG, id_selec_Pest, theVar, palet,styl,ncl,Co_list,Range)   
     
     theVar <- "Rice_KG" 
     Map_var(TotalShapes,TheMap, TG, id_selec_Pest, theVar, palet,styl,ncl,Co_list,Range)
    
     theVar <- "Arab_KG"
     Map_var(TotalShapes,TheMap, TG, id_selec_Pest, theVar, palet,styl,ncl,Co_list,Range)  
     
     theVar <- "KG_TOT" 
     Map_var(TotalShapes,TheMap, TG, id_selec_Pest, theVar, palet,styl,ncl,Co_list,Range)   
     
     
     

     
     #############################################
     #######  VALIDACION UK  #############
     ##########################################
     TheMap <- DF_TotalToMap_Av 
     R1_Uk <- NULL
     (R1_Uk <- Validation_UK (UK_valid,TheMap))
     

     


     
     
     
     
    #############################################
    #######   verificar lo de las divisiones por area con esto  #############\
    ##########################################
    # Creo una nueva tabla con ratios en lugar de area.
    # atencion que es hasta la columna ara99, las siguientes son operaciones entre otras columnas
    names(Datos_crops)
    rowSums( Datos_crops[,3:17]/Datos_crops[,2],na.rm=T )
    
    
    
    
    
    
    
    
    # En el codigo PesticidasDescripV3.R, se hacia analisis para eliminar alguna variable
    # aqui compruebo las proporcionalidades con las areas 
    

    
    #Asi que las variables que quedarian son:
    ClimaSelectV <-c("NUTS_ID","J0000","C1110", "O1000","C1300","I0000","G1000","Q0000","C1500",
                     "I1110","I1120","C1120") 
    
    names(Datos_crops)
    Datos_crops2<- Datos_crops[,(names(Datos_crops) %in% ClimaSelectV)]
    
    
    

      
    
    
    
    
    
            
            Pest_Row_dat_regre[Pest_Row_dat_regre$COUNTRY=="ES",]
            unique(Pest_Row_dat_regre$COUNTRY)
            
    nrow(ShapesToPlot[[1]]@data)
    nrow(Pest_Row_dat_regre)  #tiene que mandar la referencia del shape
    temp<- merge(ShapesToPlot[[1]]@data,Pest_Row_dat_regre[,c("NUTS3","KG_FINAL")], by.x = "NUTS_ID", by.y = "NUTS3")
    nrow(temp)
    ShapesToPlot[[1]]@data <- temp
    
    temp[substr(temp$NUTS_ID, start = 1, stop = 2) =="ES",]
    
    #atencion, si no hay las mismas columnas
    
             
    
    ShapesToPlot[[1]]@data[substr(ShapesToPlot[[1]]@data$NUTS_ID, start = 1, stop = 2) =="ES",]
    
    
    
    
    
    
    
    
    
    
    
    
    ## 2.3 DATOS Clima   ################
    names(Datos_clima)
    str(Datos_clima) 
    
    ### DESCRIPTIVE CLIMA
    # Parte de las columnas serian valores parciales de otras(cereales: trigo, centeno, etc)
    # asi que para las correlaciones y PCA hago una preseleccion de variables
    library(corrplot)
    nrow(Datos_clima)
    Datos_clima_TMP <- Datos_clima[,c(2:141)]
    sapply(Datos_clima_TMP, function(y) sum(length(which(is.na(y)))))  # falta clima o CU en 123 provincias
    Datos_clima_TMP <- Datos_clima_TMP[complete.cases(Datos_clima_TMP[,]),]
    nrow(Datos_clima_TMP)
    
    # Detect variables high correlated in order to remove them. I basically just set the upper triangle to be zero and then remove any rows that have values over 0.99.
    # systematically remove collinear/correlated variables. Should be considering VIF. But I try those high correlated
    tmp <- cor(Datos_clima_TMP)
    tmp[upper.tri(tmp)] <- 0   
    diag(tmp) <- 0
    # Above two commands can be replaced with 
    # tmp[!lower.tri(tmp)] <- 0
    # tengo que hacerlo iterativamente (eliminando una a una), para no eliminar ninguna que no debo
    dim(Datos_clima_TMP)
    Datos_clima_TMP<-Datos_clima_TMP
    for (i in 1:110){
      tmp <- cor(Datos_clima_TMP)
      tmp[upper.tri(tmp)] <- 0   
      diag(tmp) <- 0
      max(tmp)
      lavartodrop <- which(tmp == max(tmp), arr.ind = TRUE)  
      drop <- attributes(lavartodrop)$dimnames[[1]]
      print(paste0(max(tmp)," ",namecol))
      Datos_clima_TMP = Datos_clima_TMP[,-lavartodrop[1]]  #quito esa columna (se puede hacer eliminando [1] o [2], ya los indices indican fila y columna relacionada)
      print(dim(Datos_clima_TMP))
    }
