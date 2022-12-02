
# Path <- BASE_Shapes     
# NameShp <- "NUTS3.shp"

###############################################
## 2. Loading the shape to plot the Maps
###############################################
Loading_Shapes <- function(Path,NameShp,verbose=TRUE){
    
    if (verbose) message("=======================================================")
    if (verbose) message("[  Loading shapes to plot the maps ...                 ]")
    if (verbose) message("=======================================================")
    
    #########################
    ### LAS REGIONES (NUS2)  
    
    #  POLIGONOS N2
    loadshapepath<-paste(Path,NameShp,sep="/")
    MyPolReg <- readShapePoly(loadshapepath)

    class(MyPolReg)
    MyPolReg@bbox
    summary(MyPolReg)
    plot(MyPolReg)
    attributes(MyPolReg@data)
    attributes(MyPolReg@data)$names
    str(MyPolReg@data)
    
    # LINEAS DE CONTORNO N2
    loadshapepath<-paste(Path,NameShp,sep="/")
    MyLinReg <- readShapeLines(loadshapepath)   #LINEAS DE CONTORNO DE LOS NUTS
    plot(MyLinReg)
    attributes(MyLinReg@data)$names
    str(MyLinReg@data)
    
    lista <- list(MyPolReg,MyLinReg)
    return(lista)  
}


#  MyPolReg$NUTS_ID
  
#  length( unique(Pest_Row_dat$NUTS3) )
#  table( unique(Datos_clima2$NUTS_ID) %in% unique(Pest_Row_dat$NUTS3) )


###############################################
## 1. Loading Info to create the Maps
###############################################

# Shape <- TotalShapes   
# DataPest <- Pest_Row_dat
Subsetingshapes <- function(Shape,DataPest,verbose=TRUE){  
    # [[1]]: NUTS2 poligons
    # [[2]]: NUTS2 lines
 
   
 # unique(Shape[[1]]$NUTS_ID[ substr(Shape[[1]]$NUTS_ID, 1, 2)=="UK" ])
  
    #########################
    ### LAS REGIONES (NUS2)  
    
    #  POLIGONOS N2  (solo las regiones que intersectan la cuenca
    theNuts3 <- unique(DataPest$NUTS3)
    MyPoligonsRsub1 <- Shape[[1]] [Shape[[1]]$NUTS_ID %in% theNuts3 ,]   #el primer elemento de la lista contiene todas las regiones
    plot(MyPoligonsRsub1)
    summary(MyPoligonsRsub1)
    MyPoligonsRsub1@bbox
    MyPoligonsRsub1@data
    
    # LINEAS DE CONTORNO N2
    MyLinesRsub1 <- Shape[[2]] [Shape[[2]]$NUTS_ID %in% theNuts3 ,] 
    plot(MyLinesRsub1)
    

    lista <- list( MyPoligonsRsub1,MyLinesRsub1)
    return(lista)
}


# Esta funcion hace tres regresiones para cada tipo de cultivo:
# La de MINIMOS: Si en algun pais no se emplea ese PA, considera que en nuevos paises no se usa.
#    Si se usa en todos, considera la regresion del pais que menos aplique (Min_Country)
# La de MAXIMOS
# La AVERAGE: realizada con los datos de algunos paises que no sean extremos.
#    se le pasa la lista de los paises que intervienen en la regresion. Y tambien que variables climaticas   
# 


# DF_temp <- Dat_clima_crop
Sumario_Areas <- function(DF_temp){

  #los paises del trainign
  DF_temp$Co <- as.factor(substring(DF_temp$NUTS_ID,1,2))
  names(DF_temp)
  DF_temp2 <- DF_temp[(DF_temp$Co %in% c("BE","DE","DK","ES","FR","IE","IT","NL")),]
  DF_temp2$Co <- factor(DF_temp2$Co)
  par(mfrow=c(2,2)) 
  for (ic in c(3,12,18,37,40,41,45,11,34,32,33)){
    boxplot(DF_temp2[,ic] ~ DF_temp2$Co, las=3, main=names(DF_temp2)[ic],col="orange")
  }
  #con el siguiente agregate se genera la tabla de totales y en excel la de porcentajes
  names(Dat_clima_crop)
  aggregate(DF_temp2[,c(3,12,18,37,40,41,45,11,34,32,33)] , by=list(Category=DF_temp2$Co ), FUN=sum)
  #numero provincias totales
  aggregate(DF_temp2[,c(3,12,18,37,40,41,45,11,34,32,33)] , by=list(Category=DF_temp2$Co ), FUN=NROW)
  # numero de provincias con areas no nulas
  aggregate(DF_temp2[,c(3,12,18,37,40,41,45,11,34,32,33)] , by=list(Category=DF_temp2$Co ), FUN=function(x) {sum(x > 0, na.rm = TRUE)  })
  

  
   #los paises donde se realizara la prediccion
  DF_temp$Co <- as.factor(substring(DF_temp$NUTS_ID,1,2))
  names(DF_temp)
  DF_temp2 <- DF_temp[!(DF_temp$Co %in% c("BE","DE","DK","ES","FR","IE","IT","NL")),]
  DF_temp2$Co <- factor(DF_temp2$Co)
  par(mfrow=c(2,2)) 
  for (ic in c(3,12,18,37,40,41,45,11,34,32,33)){
    boxplot(DF_temp2[,ic] ~ DF_temp2$Co, las=3, main=names(DF_temp2)[ic],col="orange")
  }
  #con el siguiente agregate se genera la tabla de totales y en excel la de porcentajes
  names(Dat_clima_crop)
  aggregate(DF_temp2[,c(3,12,18,37,40,41,45,11,34,32,33)] , by=list(Category=DF_temp2$Co ), FUN=sum)
  #numero provincias totales
  aggregate(DF_temp2[,c(3,12,18,37,40,41,45,11,34,32,33)] , by=list(Category=DF_temp2$Co ), FUN=NROW)
  # numero de provincias con areas no nulas
  aggregate(DF_temp2[,c(3,12,18,37,40,41,45,11,34,32,33)] , by=list(Category=DF_temp2$Co ), FUN=function(x) {sum(x > 0, na.rm = TRUE)  })
  
  
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
  # J3000ES:
  
  
}
  
  

  


# theDatBig <- theDFTrain2
# theDatSmall <- theDFTrain3
# mod1 <- AvMod_Orig     
# mod2 <- AvMod   #con restricciones
# outcome <- strCrop
CalculaMetricas <- function(theDatBig,theDatSmall,mod1,mod2,outcome){
      
      theDatSmall$PredicMod1 <- predict(mod1, newdata = theDatSmall)
      theDatSmall$PredicMod2 <- predict(mod2, newdata = theDatSmall)
      theDatBig$PredicMod1 <- predict(mod1, newdata = theDatBig)
      theDatBig$PredicMod2 <- predict(mod2, newdata = theDatBig)

   #   theDatSmall[(theDatSmall[ ,"ARABLE_KG"])==0,   ]
      
      # RAE
      raeMod1Small <- round(rae(theDatSmall[,outcome],theDatSmall$PredicMod1), digits=3) 
      raeMod1Big <- round(rae(theDatBig[,outcome],theDatBig$PredicMod1), digits=3) 
      raeMod2Small <- round(rae(theDatSmall[,outcome],theDatSmall$PredicMod2), digits=3) 
      raeMod2Big <- round( rae(theDatBig[,outcome],theDatBig$PredicMod2), digits=3) 
      
      
      # NRMSE :normalized    rmse Root Mean Squared Error /sd(estimate)
      nrmseMod1Small <- round( rmse(theDatSmall[,outcome],theDatSmall$PredicMod1)/sd(theDatSmall[,outcome]), digits=3)  
      nrmseMod1Big <- round( rmse(theDatBig[,outcome],theDatBig$PredicMod1)/sd(theDatBig[,outcome]), digits=3)  
      nrmseMod2Small <- round( rmse(theDatSmall[,outcome],theDatSmall$PredicMod2)/sd(theDatSmall[,outcome]), digits=3)  
      nrmseMod2Big <- round( rmse(theDatBig[,outcome],theDatBig$PredicMod2)/sd(theDatBig[,outcome]), digits=3)  
      
      # Los calculos del mape (claculo smape, para evitar los ceros)
           #artificio matematico para evitar que los ceros del outcome den Mape infinito
 #     theDatSmall[(theDatSmall[ ,outcome])==0, outcome  ]<-0.000001   #les asigno un valor pequeño, pero lleva a valores grandes igual
#      theDatBig[(theDatBig[ ,outcome])==0, outcome  ]<-0.000001
      # SMAPE
      smapeMod1Small <- smape(theDatSmall[,outcome],theDatSmall$PredicMod1)
      smapeMod1Big <- smape(theDatBig[,outcome],theDatBig$PredicMod1)
      smapeMod2Small <- smape(theDatSmall[,outcome],theDatSmall$PredicMod2)
      smapeMod2Big <- smape(theDatBig[,outcome],theDatBig$PredicMod2)

      
      # NMAE' for the normalized MAE    MAE / sd(estimate)       mae Mean Absolute Error / sd(estimate)
      nmaeMod1Small <- round( mae(theDatSmall[,outcome],theDatSmall$PredicMod1)/sd(theDatSmall[,outcome]), digits=3)
      nmaeMod1Big <- round( mae(theDatBig[,outcome],theDatBig$PredicMod1)/sd(theDatBig[,outcome]), digits=3)
      nmaeMod2Small <- round( mae(theDatSmall[,outcome],theDatSmall$PredicMod2)/sd(theDatSmall[,outcome]), digits=3)
      nmaeMod2Big <- round( mae(theDatBig[,outcome],theDatBig$PredicMod2)/sd(theDatBig[,outcome]), digits=3)
      
  
      # MASE
      maseMod1Small <- round(mase(theDatSmall[,outcome],theDatSmall$PredicMod1,1), digits=3) 
      maseMod1Big <- round(mase(theDatBig[,outcome],theDatBig$PredicMod1,1), digits=3) 
      maseMod2Small <- round(mase(theDatSmall[,outcome],theDatSmall$PredicMod2,1), digits=3) 
      maseMod2Big <- round(mase(theDatBig[,outcome],theDatBig$PredicMod2,1), digits=3) 

      
      
      # N-s, very similar (Coefficient of determination) represents the coefficient of how well the values fit compared to the original values. The value from 0 to 1 interpreted as percentages. The higher the value is, the better the model is.
      d.Mod1Small = theDatSmall[,outcome] - theDatSmall$PredicMod1
      N_S_Mod1Small = round( 1-(sum((d.Mod1Small)^2)/sum( (theDatSmall[,outcome]-mean(theDatSmall[,outcome]))^2 )), digits=3)
      d.Mod1Big = theDatBig[,outcome] - theDatBig$PredicMod1
      N_S_Mod1Small = round( 1-(sum((d.Mod1Big)^2)/sum( (theDatBig[,outcome]-mean(theDatBig[,outcome]))^2 )), digits=3)
      d.Mod2Small = theDatSmall[,outcome] - theDatSmall$PredicMod1
      N_S_Mod2Small = round( 1-(sum((d.Mod2Small)^2)/sum( (theDatSmall[,outcome]-mean(theDatSmall[,outcome]))^2 )), digits=3)
      d.Mod2Big = theDatBig[,outcome] - theDatBig$PredicMod1
      N_S_Mod2Big = round( 1-(sum((d.Mod2Big)^2)/sum( (theDatBig[,outcome]-mean(theDatBig[,outcome]))^2 )), digits=3)
      
      
              # mase.Tot <- round(mase(cv.Result[,indexOut],cv.Result$predModTot,1), digits=3)
              # 
              # sort(theDatBig[,outcome])
              # sort(theDatBig[,outcome])
              # 
              # length(theDatBig$PredicMod2)
              # table(theDatBig$COUNTRY)
              # theDatBig[theDatBig$COUNTRY=="IE",][1:10,]
              #         aggdata <-aggregate(theDatBig, by=list(theDatBig$COUNTRY),  FUN=mean, na.rm=FALSE)    aggdata[,c("Group.1","ARABLE_KG","C0000","I0000","R0000")]
              # table(cv.Result$COUNTRY)
              # cv.Result[cv.Result$COUNTRY=="IE",][1:10,]
              #       aggdata2 <-aggregate(cv.Result, by=list(cv.Result$COUNTRY),  FUN=mean, na.rm=FALSE)      aggdata2[,c("Group.1","ARABLE_KG","C0000","I0000","R0000" )]
              # 
              #       
              #     plot( theDatBig$PredicMod2, predict(mod2,newdata=cv.Result) )
              #       
              #     
              # length(cv.Result$predModTot)
              # plot( theDatBig[,outcome],theDatBig$PredicMod2)
              # modTot <- lm(fmla, data = cv.Result)  
              # cv.Result$predModTot <- predict(modTot)  #prediccion sobre el training
              # nrow(cv.Result)
              # cv.Result$predModTot2 <- predict(mod1, newdata = cv.Result)
              # plot(  cv.Result$predModTot, cv.Result$predModTot2 )
              # 
      
      
      text1<-paste("big: ",nrmseMod1Big,nrmseMod2Big,smapeMod1Big,smapeMod2Big,nmaeMod1Big,nmaeMod2Big,sep=" ")
      plot( theDatBig[,outcome],  theDatBig$PredicMod1 ,main= text1 )
      points( theDatBig[,outcome],  theDatBig$PredicMod2,col="blue" )
      text2<-paste("Small: ",nrmseMod1Small,nrmseMod2Small,smapeMod1Small,smapeMod2Small,nmaeMod1Small,nmaeMod2Small,sep=" ")
      plot( theDatSmall[,outcome],  theDatSmall$PredicMod1, main=text2 )
      points( theDatSmall[,outcome],  theDatSmall$PredicMod2,col="blue" )
      
      res <- c(nrmseMod1Small,nrmseMod1Big,nrmseMod2Small,nrmseMod2Big, smapeMod1Big,smapeMod2Big,
               smapeMod1Small,smapeMod2Small,    nmaeMod1Small ,  nmaeMod1Big,  nmaeMod2Small, nmaeMod2Big,
               N_S_Mod1Small,N_S_Mod1Small,N_S_Mod2Small,N_S_Mod2Big )
      
      
      
    return(res)  
}

# theDFTrain <- Datos_SupAgr_clima_Pest     
# theDFTotal <- Dat_clima_crop
# strCrop <- stCrop
# listVarLand <- lVarLand
# Min_Country <- Min_Count
# Max_Country <- Max_Count  # "ES"
# Aver_Country <- Aver_Count
# TypeReg <- RegType      # c("lm","rlm","rlm" ) 
# AditionalVars <- AditionVars   #   AditionVars <- NULL  c("Lat","Lon")
# path <-  path_out   
# id_PAtype <- TC
# id_PA <- id_selec_Pest
# LA <- Legal_autoriz
Calcula_predicciones <- function(theDFTrain ,theDFTotal, strCrop, listVarLand, Min_Country,Max_Country
                        ,Aver_Country,TypeReg,AditionalVars,path,id_PAtype,id_PA,LA) {  

    # 1. DF donde se hara la prediccion, por diferencia entre el DFTotal y DFTRain 
      nrow(theDFTotal)
      nrow(theDFTrain)
      DFPredict <- theDFTotal[!(theDFTotal$NUTS_ID %in% unique(theDFTrain$NUTS3) ),  ]  #  levels(theDFTrain$NUTS3) atencion en el train si en un pais que no usa el compuesto no hay filas pero no es que no este en el train, la prediccion es
      nrow(DFPredict)

    # 2. Indice de los Kg de PA aplicados en un cultivo (que sera el outcome)
      index <- which(names(theDFTrain) == strCrop) #indice de la columna a predecir
      indexArea <- which(names(theDFTrain) %in% listVarLand)
      
    # 3. Puntos que aplican cantidad nula de PA permanecen en training (pero veo cuantos son)
      theDFTrainPA_great_0 <- theDFTrain[theDFTrain[ ,index]==0 , ]
      nrow(theDFTrainPA_great_0)
      table(theDFTrainPA_great_0$COUNTRY) 
      
      #REVISAR
      # Eliminar puntos donde NO hay Area dedicada (pero esto depende de la o las variables seleccionadas y el tipo de regresion)
      if(length(indexArea)== 1 ){  
        theDFTrain_ar_great_0 <- theDFTrain[(theDFTrain[ ,indexArea])>0, ]  # si es un solo crop para las areas no pongo el sum
      }else {
        theDFTrain_ar_great_0 <- theDFTrain[rowSums(theDFTrain[ ,indexArea])>0, ]
      }
      table(theDFTrain_ar_great_0$COUNTRY)
      nrow(theDFTrain_ar_great_0)
      #  table(theDFTrain2$COUNTRY) 
      # solo quiero eliminar donde NO hay area dedicada y SI se aplique el compuesto
      # o eliminar lo contrario   (atencion que podrian ser varios crop, no lo he modelado aun)
   #   theDFTrain2 <- theDFTrain[!((theDFTrain[ ,index]==0) & (rowSums(theDFTrain[ ,indexArea])>0)), ] 
      
      #o solo eliminar los puntos donde no hay area dedicada (si hay area y no se aplica compuesto, permanece)
      if(length(indexArea)== 1 ){  
        theDFTrain2 <- theDFTrain[(theDFTrain[ ,indexArea])>0, ]  # si es un solo crop para las areas no pongo el sum
      }else {
        theDFTrain2 <- theDFTrain[rowSums(theDFTrain[ ,indexArea])>0, ]
      }

     ####### IMPRESION PARA JAVI
     #  file <- paste0("C:/ANGEL/2020MAYO/Pesticidas/output/Prediccion/BAYES","/",id_PA,"_Train.csv")
     #  write.csv(theDFTrain , file = file)
      
      table(theDFTrain2$COUNTRY) #imprimer esas tablas al final
      nrow(theDFTrain2)

    # 4. Modelo Regression para MINIMO  (depende del Min_Country, solo se usa si se pone el nulo)
      DFPredict$PredMin <- 0   # si le he indicado que el minimo es nulo 

      
    # 5. Modelo Regression para MAXIMO  (depende del Max_Country)  
        theDFTrain3 <- theDFTrain2 [theDFTrain2$COUNTRY %in% Max_Country,  ]  #el pais que se empleara para este training
        xnam <- listVarLand
        fmla <- as.formula(paste(strCrop, " ~ ", paste(xnam, collapse= "+") ))
        MaxMod <- lm(fmla, data = theDFTrain3)
          # correccion si hay coeficientes negativos
          res <- lm_coef_restric(MaxMod,listVarLand) 

        # prediciendo en base a eso
        if( length(listVarLand)== 1){   #si hay un solo predictor tengo que hacer una transformacion   
          thesinglePred <- data.frame(DFPredict[,c(listVarLand)])
          names(thesinglePred) <- listVarLand
        }else { thesinglePred <- DFPredict[,c(listVarLand)]  }
        
        DFPredict$PredMax <- predict(MaxMod, newdata = thesinglePred )

      
    # 6. Modelo Regression para AVERAGE (depende del Aver_Country)
         # Aver_Country: puede ser un ALL, que seria considerar todos los countries, o una lista de algunos de ellos
       if(Aver_Country[1] == "ALL") {   
          theDFTrain3 <- theDFTrain2    # empleara todos los puntos para el modelo
       } else {   # si no es all, seran los paises 
          theDFTrain3 <- theDFTrain2 [theDFTrain2$COUNTRY %in% Aver_Country,  ]  #el pais que se empleara para este training
       }  

       # No incluir o incluir ciertas variables climaticas
        AditionalVars 
        xnam <- c(listVarLand,AditionalVars)
        fmla <- as.formula(paste(strCrop, " ~ ", paste(xnam, collapse= "+") ))
                     # Elimino filas donde falte algun predictor la regresion (evita problemas en la regresion)
        theDFTrain3 <- theDFTrain3[,c("COUNTRY",strCrop,xnam)]
        nrow(theDFTrain3)
        names(theDFTrain3)
        theDFTrain3 <- theDFTrain3[complete.cases(theDFTrain3),]
        AvMod <- lm(fmla, data = theDFTrain3)
        AvMod_Orig <- AvMod
        print(summary(AvMod))
        # correccion si hay coeficientes negativos
        res <- lm_coef_restric(AvMod,listVarLand)  
        #  attributes( res[[1]] )       res[[1]]$fitted    res[[1]]$b.unrestr    res[[1]]$b.restr
        AvMod <- res[[1]]
        r2_o_av <- res[[2]]   # r2 modelo original
        r2_r_av <- res[[3]]   # r2 modelo con restricciones
        
        MetricsModPred <- CalculaMetricas(theDFTrain2,theDFTrain3,AvMod_Orig,AvMod,strCrop)
      
  #      if(TypeReg[3]=="rlm") { AvMod <- rlm(fmla, data = theDFTrain3, psi = psi.bisquare) }    # robust linear regresion

        # no necesario el plot
          par(mfrow=c(1,1))
          maintextAv <- paste(  paste(fmla[c(2,3)], collapse = " ~ ") , "   R2=" ,r2_o_av ,r2_r_av,sep="  ")
          plot(theDFTrain3[ ,2], predict(AvMod),
               xlab="actual",ylab="predicted" ,col=theDFTrain3$COUNTRY,pch=20,main= maintextAv ,cex.main=0.75 )
          legend("bottomright", legend=unique(theDFTrain3$COUNTRY),col=unique(theDFTrain3$COUNTRY),pch=20)
          abline(a=0,b=1)  
    
          # plot enfrentando area y kg, con la recta (lo primero sumo areas de todas las var explicativas)   
          if (length(listVarLand)==1){
            theDFTrain3$TotArea <- theDFTrain3[,names(theDFTrain3) %in% listVarLand]
          }else{   #si es mas de una variable, sumo las areas de todos para mejorar la grafica
            theDFTrain3$TotArea <- rowSums(theDFTrain3[,names(theDFTrain3) %in% listVarLand])
          }
          plot(theDFTrain3$TotArea, theDFTrain3[ ,2],
               xlab=paste0(listVarLand),ylab=strCrop ,col=theDFTrain3$COUNTRY,pch=20,main= "AvMod",cex.main=0.7 )
          legend("topright", legend=unique(theDFTrain3$COUNTRY),col=unique(theDFTrain3$COUNTRY),pch=20)
          abline(lm(fmla, data = theDFTrain3))  #cuando hay mas de una var explicativa el plot no muestra bien la cosa
          #   library(MASS)
 #         rr.bisquare <- rlm(fmla, data = theDFTrain3, psi = psi.bisquare) #regresion robusta
 #        if( !is.na(coef(rr.bisquare)[2]) ) {abline(rr.bisquare, col="red" )}  #solo pinta la linea cuando no da error
 #        if( is.na(coef(rr.bisquare)[2]) ) { print("error con regresion robusta")}
 #        legend("top", legend=c("LM","RLM"), col=c("black","red"),pch=20)
              
              
        # Antes de predecir con ese modelo. Tenemos que arreglar los NA en los datos de testing
        if( length(xnam)> 1){  # solo cuando hay predictores climaticos
            DFPredict[!complete.cases(DFPredict[,c(xnam)]) ,c(xnam)]     # en el conjunto de datos a predecir, tambien podria haber NA  
            indexColWith_NA <- colnames(DFPredict[,c(xnam)])[colSums(is.na(DFPredict[,c(xnam)])) > 0]
            for(i in indexColWith_NA){  #para todos esos NA, remplazo NA por valores medios en el resto de celdas
              print(i)
              DFPredict[is.na(DFPredict[,i]), i] <- mean(DFPredict[,i], na.rm = TRUE)
            }     
        }    

          ####### IMPRESION PARA JAVI
          #  file <- paste0("C:/ANGEL/2020MAYO/Pesticidas/output/Prediccion/BAYES","/",id_PA,"_Predict.csv")
          #  write.csv(DFPredict , file = file)
          
          
        ## CALCULA LA PREDICCION
          # CONJUNTO NO TRAINING
        nrow(DFPredict)
        if( length(xnam)== 1){   #si hay un solo predictor tengo que hacer una transformacion, por problemas tipo de datos 
          thesinglePred <- data.frame(DFPredict[,c(xnam)])
          names(thesinglePred) <- xnam
        }else { thesinglePred <- DFPredict[,c(xnam)]  }
        DFPredict$PredAv <- predict(AvMod, newdata = thesinglePred )
        DFPredict$PredConstr <- predict(AvMod_Orig,newdata = thesinglePred  )
        head(DFPredict)

     #  newdata <- data.frame(O1000 = c(12, 19, 24)      )
    #    DFPredict$PredAv <- predict(AvMod, newdata = newdata )
        
         # CONJUNTO de training  
        names(theDFTrain2)
        nrow(theDFTrain2)
        theDFTrainBig <- theDFTrain2[,c("COUNTRY","NUTS3",strCrop,xnam)]
        theDFTrainBig <- theDFTrainBig[complete.cases(theDFTrainBig),]
        if(length(listVarLand)== 1 ){  
          theDFTrainBig$TotArea <- (theDFTrainBig[,names(theDFTrainBig) %in% listVarLand])  # si es un solo crop para las areas no pongo el sum
        }else {
          theDFTrainBig$TotArea <- rowSums(theDFTrainBig[,names(theDFTrainBig) %in% listVarLand])
        }
        theDFTrainBig$PredAv <- predict(AvMod, newdata = theDFTrainBig)  #y la prediccion
    
        theDFTrainBig$PredMin <- 0
 
        theDFTrainBig$PredMax <- predict(MaxMod, newdata = theDFTrainBig )
        theDFTrainBig$PredConstr <- predict(AvMod_Orig,newdata = theDFTrainBig)


        # 7. Plot
        if (length(listVarLand)==1){
          DFPredict$TotArea <- DFPredict[,names(DFPredict) %in% listVarLand]  
          theDFTrain3$TotArea <- theDFTrain3[,names(theDFTrain3) %in% listVarLand]
        }else{   #si es mas de una variable, sumo las areas de todos para mejorar la grafica
          DFPredict$TotArea <- rowSums(DFPredict[,names(DFPredict) %in% listVarLand])  
          theDFTrain3$TotArea <- rowSums(theDFTrain3[,names(theDFTrain3) %in%  listVarLand])
        }

        
        
      #plot de max, min, average,training, predict all count
      plot( DFPredict$TotArea , DFPredict$PredMin ,col="black",pch=20,cex=0.5 )
      plot( DFPredict$TotArea , DFPredict$PredAv ,col="green",pch=20,cex=0.5, main= paste("R2=" ,r2_o_av ,r2_r_av ,sep="  ")  )
      plot( DFPredict$TotArea , DFPredict$PredMax ,col="red",pch=20,cex=0.5 )
      plot( DFPredict$TotArea , DFPredict$PredConstr ,col="blue",pch=20,cex=0.5  )
      plot( theDFTrain3$TotArea , theDFTrain3[,c(2)] ,col="orange",pch=20,cex=0.5 )
      plot( theDFTrainBig$TotArea , theDFTrainBig[,c(3)] ,col="yellow",pch=20,cex=0.5 )
   
    # 8. Correccion de los valores de average, para que no salgan del rango de minimos y maximos. Evitara algun negativo adicional
    #  DFPredict$PredMin[DFPredict$PredMin < 0] <- 0  # si el valor de la prediccion minima es menor que cero lo pone a cero
    #  DFPredict$PredMax[DFPredict$PredMax < DFPredict$PredMin] <- DFPredict$PredMin[DFPredict$PredMax < DFPredict$PredMin]  #esta ultima modifica la prediccion max cuando es menor que la minima
   #   DFPredict$PredAv[DFPredict$PredAv > DFPredict$PredMax] <- DFPredict$PredMax[DFPredict$PredAv > DFPredict$PredMax] 
    #  DFPredict$PredAv[DFPredict$PredAv < DFPredict$PredMin] <- DFPredict$PredMin[DFPredict$PredAv < DFPredict$PredMin] 
      DFPredict$PredAv[DFPredict$PredAv < 0] <- 0 # si el valor de la prediccion average es menor que cero lo pone a cero
   
   #   theDFTrainBig$PredMin[theDFTrainBig$PredMin < 0] <- 0  # si el valor de la prediccion minima es menor que cero lo pone a cero
    #  theDFTrainBig$PredMax[theDFTrainBig$PredMax < theDFTrainBig$PredMin] <- theDFTrainBig$PredMin[theDFTrainBig$PredMax < theDFTrainBig$PredMin]  #esta ultima modifica la prediccion max cuando es menor que la minima
    #  theDFTrainBig$PredAv[theDFTrainBig$PredAv > theDFTrainBig$PredMax] <- theDFTrainBig$PredMax[theDFTrainBig$PredAv > theDFTrainBig$PredMax] 
    #  theDFTrainBig$PredAv[theDFTrainBig$PredAv < theDFTrainBig$PredMin] <- theDFTrainBig$PredMin[theDFTrainBig$PredAv < theDFTrainBig$PredMin] 
      theDFTrainBig$PredAv[theDFTrainBig$PredAv < 0] <- 0
      
      
      ######## CORRECCCION DE LA PREDICCION CUANDO UN PRODUCTO NO ESTA AUTORIZADO EN UN PAIS
      TheConst <- LA[LA$ID_EUPD==id_PA, 7:36]    #las legal constraints para ese AS (un 0 indica que no esta permitido usarlo en ese pais)
      CoNonAutor <-names(TheConst)[TheConst==0]  #paises en los que esta prohibido
      DFPredict[substr(DFPredict$NUTS_ID, 1, 2) %in% CoNonAutor,c("PredMin","PredMax","PredAv","PredConstr") ] <- 0  #si esta la constrain tengo que poner la prediccion a nulo
      theDFTrainBig[substr(theDFTrainBig$NUTS_ID, 1, 2) %in% CoNonAutor,c("PredMin","PredMax","PredAv","PredConstr") ] <- 0

   
      ###### CORRECCION DE LA PREDICCION CUANDO NO HAY AREA DE NINGUNO DE LOS PREDICTORES
      # en dichos casos tengo que poner la prediccion a nulo (no lo era por el termino independiente)
      # busco registros en las que todos los datos son nulos
      
    
      if(length(listVarLand)== 1 ){  
        DFPredict[ DFPredict[,c(listVarLand)] ==0, c("PredMin","PredMax","PredAv","PredConstr")] <- 0
        theDFTrainBig[ theDFTrainBig[,c(listVarLand)] ==0, c("PredMin","PredMax","PredAv","PredConstr")] <- 0
      }else {
        DFPredict[rowSums( DFPredict[,c(listVarLand)] )==0, c("PredMin","PredMax","PredAv","PredConstr")] <- 0
        theDFTrainBig[rowSums( theDFTrainBig[,c(listVarLand)] )==0, c("PredMin","PredMax","PredAv","PredConstr")] <- 0
      }
      
      
      
         #plot de todo el conjunto
        plot( DFPredict$TotArea , DFPredict$PredMin ,col="black",pch=20,cex=0.5,ylim=c(min(0,DFPredict$PredMin),max(1*DFPredict$PredAv)),
              main= paste("R2=" ,r2_o_av ,r2_r_av ,sep="  ")    )
        points( DFPredict$TotArea , DFPredict$PredMax ,col="red",pch=20,cex=0.5 )
        points( DFPredict$TotArea,DFPredict$PredConstr ,col="cornsilk3",pch=20,cex=0.5 )
        points( DFPredict$TotArea , DFPredict$PredAv ,col="green",pch=20,cex=0.5 )
        points( theDFTrainBig$TotArea , theDFTrainBig[,c(2)] ,col="blue",pch=20,cex=0.2 )
        points( theDFTrain3$TotArea , theDFTrain3[,c(2)] ,col="orange",pch=20,cex=0.5 )
        legend("topright", legend=c("PredictMin","PredictMax","PredictAv","Training","PredAveAll") 
               ,col= c("black","red","green","orange","blue" ) ,pch=20, title = "Models")
   
       
     #   3.027e+03 +   2.554e-02 *min(DFPredict$TotArea[DFPredict$TotArea!=0]  ) 
    #    plot(DFPredict$TotArea, (3.027e+03 +   2.554e-02*DFPredict$TotArea)   )
 
        #ANADIR LA VERSION ROBUSTA A LAS REGRESIONES, Y QUE SE PUEDA ESCOGER, Y TAMBIEN CORREGIR, AREA NULA, APLICA NULO
        
        
    # 9. Mezclo con los valores de los datos de paises usados en training y escribo un csv
            #9.1 Si no existe creo una carpeta del PA  (atencion que hay que crear toda la estructura de directorio hacia dentro)
          thePath <- paste (path,"Prediccion", sep= "/")
          dir.create(file.path(thePath, id_PAtype), showWarnings = TRUE)
          thePath <- paste (thePath,id_PAtype, sep= "/")
          dir.create(file.path(thePath, id_PA), showWarnings = TRUE)
          thePath <- paste (thePath,id_PA, sep= "/")
          dir.create(file.path(thePath, strCrop), showWarnings = TRUE)

    # 10. Escribe los CSV
      thePath <- paste0(thePath,"/",strCrop)
      # Escribo un CSV con el conjunto de los paises donde concociamos el valor
      file <- paste0(thePath,"/",strCrop,"_Train.csv")
      write.csv(theDFTrain[,c("NUTS3",strCrop)] , file = file)
    
      # Otro CSV con el conjunto de los paises donde se predice ,con MIN, MAX y AV
      file <- paste0(thePath,"/",strCrop,"_Predicc.csv")
      write.csv(DFPredict[,c("NUTS_ID","PredMin","PredAv", "PredMax","PredConstr")] , file = file)
 
      # Prediccion en el total de paises, ultima columna es el valor real
      file <- paste0(thePath,"/",strCrop,"_Train.csv")
      write.table(theDFTrainBig[,c("COUNTRY","NUTS3","PredMin","PredAv", "PredMax","PredConstr",strCrop)], file = file, qmethod = "double") 
      #            file =file, sep = ",", col.names = NA,
      #            qmethod = "double")

      # IMPRIMO UNO TOTAL PARA CHIARA (hago un rbind de la prediccion de ambos casos)
      nrow(theDFTrainBig)
      names(theDFTrainBig)[2] <- c("NUTS_ID")   #renombro el nombre de una columana para poder mezclar
      DF_temp <- rbind(DFPredict[,c("NUTS_ID","PredMin","PredAv", "PredMax","PredConstr")],
            theDFTrainBig[,c("NUTS_ID","PredMin","PredAv", "PredMax","PredConstr")] )
      file <- paste0(thePath,"/",strCrop,"_PredTot.csv")
      write.table(DF_temp[,c("NUTS_ID","PredMin","PredAv","PredMax","PredConstr")], file = file, qmethod = "double",sep=",",dec = ".") 

    # 11. Imprimer el plot en un fichero externo 
      nametot <- paste0(thePath,"/","ScaterModels.jpeg")
      jpeg(nametot, width = 500, height = 500)
      plot( DFPredict$TotArea , DFPredict$PredMin ,col="black",pch=20,cex=0.5,ylim=c(min(0,DFPredict$PredMin),max(0.9*DFPredict$PredMax)) )
      points( DFPredict$TotArea , DFPredict$PredMax ,col="red",pch=20,cex=0.5 )
      points( DFPredict$TotArea , DFPredict$PredAv ,col="green",pch=20,cex=0.5 )
      points( theDFTrain3$TotArea , theDFTrain3[,c(2)] ,col="orange",pch=20,cex=0.5 )
      legend("topright", legend=c("PredictMin","PredictMax","PredictAv","Training") 
             ,col= c("black","red","green","orange" ) ,pch=20, title = "Models")
      dev.off()

    # 12. Imprime fichero resumen de la prediccion
      file <- paste0(thePath,"/",strCrop,"_Models.txt")
      sink(file)
      print("table ALL")
      print(table(theDFTrain$COUNTRY)) # todos
      print("table PA >0")
      print(table(theDFTrainPA_great_0$COUNTRY)) #los de PA >0
      print("table Area >0")
      print(table(theDFTrain_ar_great_0$COUNTRY))  #los de area >0
      print("table The used data. Removing when No area but PA is aplied")
      print(table(theDFTrain2$COUNTRY))   #los usados. Elimino cuando se aplica PA pero no hay area
      print(paste0("types of regression models:", TypeReg))
      print (paste0("Min Model with country: ", Min_Country))
      print(paste0("variables adicionales: ", AditionVars))
      print (paste0("Av Model  ", Aver_Country))
      print(maintextAv)
      print(summary(AvMod))
      print("Metrics mod predict: raeMod1Big  raeMod2Big  raeMod1Small  raeMod2Small  smapeMod1Big smapeMod2Big smapeMod1Small  smapeMod2Small")
      print(MetricsModPred)
      sink() 
    
      
      ## ANADO INFORMACION RESUMIDA DE CADA EJECUCION , UNA FILA PARA CADA AS
      thePath <- paste (path,"Prediccion", sep= "/")
      thePath <- paste (thePath,id_PAtype, sep= "/")
      file <- paste0(thePath,"/","ModelsSumar.csv")
      
      ListC <- c("BE","DE","DK","ES","FR","IE","IT","NL")  # para la informacion de paises
      ICo <- ListC %in% Aver_Country 
      ListCr <- c("C0000","G0000","I0000","P0000","Q0000","R0000","V0000_S0000","E0000","N0000","L0000", "J0000", "O1000","W1000","F0000","T0000")
      ILa <- rep(NA,length(ListCr) )  # por defecto lo dejo todo a NA 
      indMach <- match( names(AvMod$coefficients),ListCr)  #en que posicion de la lista de crop meto cada coeficiente
      indMach <-indMach[!is.na(indMach)]   #quito los NA
      indexcoef <- 2 # un contador
      for (i in indMach){                               # y luego sobreescrito en las posiciones de los coeficientes
        ILa[i] <-round(AvMod$coefficients[indexcoef], digits = 4)  
        indexcoef= indexcoef+1
      }    #el termino independiente lo anado luego
      
      PosFirstCl  <- length(indMach)+2
      Icl <- NULL
      Inamcl <- NULL
      for (i in PosFirstCl:(5+PosFirstCl)){    #solo escribire 4 valores de coeficientes de clima
        print(i)# y luego sobreescrito en las posiciones de los coeficientes
        Icl[i-PosFirstCl+1] <-round(AvMod$coefficients[i], digits = 4)  
        Inamcl[i-PosFirstCl+1] <- names(AvMod$coefficients)[i]
      }   
      #ya se insertan todos los valores en el csv
      # algo parecido para las variables de CORINE (tambien con los coeficientes)
      #si el pais entra deberia meter el coeficiente de la regresion
      # los del clima son distinto cada vez. Tambien seria util una referencia de la importancia
      DF_SUM <- as.data.frame(t(c(id=as.character(id_PA) ,CORINE=strCrop, MinC =Min_Country, MaxC =Max_Country,
                                  BE=ICo[1], DE=ICo[2],DK=ICo[3],ES=ICo[4],FR=ICo[5],IE=ICo[6],IT=ICo[7],NL=ICo[8],
                                  Intercept=as.numeric(AvMod$coefficients[1]),C0000=ILa[1],G0000=ILa[2],I0000=ILa[3],P0000=ILa[4],Q0000=ILa[5],
                                  R0000=ILa[6],V0000_S0000=ILa[7],E0000=ILa[8],N0000=ILa[9],L0000=ILa[10],J0000=ILa[11],O1000=ILa[12],
                                  W1000=ILa[13],F0000=ILa[14],T0000=ILa[15],
                                  CL1n1=Inamcl[1],CL1i1n=Icl[1], CL1n2=Inamcl[2],CL1i2n=Icl[2],
                                  CL1n3=Inamcl[3],CL1i3n=Icl[3], CL1n4=Inamcl[4],CL1i4n=Icl[4] ) ))
      
      write.table(DF_SUM, file, sep = ",", col.names = !file.exists(file), append = T) #voy anadiendo lineas al fichero
      
      
      
      
      return( list(AvMod_Orig, AvMod, fmla,  theDFTrain3, MetricsModPred) )
}     



# Funcion para aplicar una regresion con restricciones en el caso de que o la variable
# independiente o alguno de los coeficientes (relativos a cultivos) sean negativos.
# Ya que en en nuestro problema, eso no seria logico, areas negativas o pesticidas negativos
# No tendria sentido, a mas area de cualquiera de los cultivos es de esperar mas pesticidas, nunca menos
# En cuanto al termino independiente, no deberia poder ser negativo ya que implicaria que 
# cuando no hay area se aplican cantidades de pesticidas negativos
# En el otro sentido, tampoco deberia haber terminos independientes demasiado grantes
# AvMod_f <- MinMod
# listVarLand_f <-listVarLand
lm_coef_restric <- function (AvMod_f, listVarLand_f ){
  
  r2_orig <- round(summary(AvMod_f)$r.squared, digits = 2)
  r2_reduc <- NULL
  # INTERCEPT OR CROP COEF NEGATIVES (THE WEATER COEFICIENTS NOT)
  # 1.Detect if happents (negative)
  nvarcrop <-length(listVarLand_f) + 1  # coeficientes qe me interesas. sumo 1 por el termino independiente  
  TheCoef <- summary(AvMod_f)$coefficients[1:nvarcrop, 1]   
  numberNeg <- sum(TheCoef<0)  #si hay algun negativo tengo el problema

  if(numberNeg>0){        # Contruyo restriccion para los negativos
    
      myConstraints <- " "   #la inicio a vacio
      if (TheCoef[1] < 100000)  {myConstraints <- c(' .Intercept. = 0 ; ')    # si el termino indep es negativo
      }else { myConstraints <- c('  ') }
      #y un trozo para cada uno de los restantes coeficientes
      for(i in 2:nvarcrop ){
        #  i=2
        # print(names(TheCoef)[i])
      #  if(TheCoef[i] < 0){ myConstraints <- paste0(myConstraints,names(TheCoef)[i], c('  > 0 ; ') ) 
      #  }else{ myConstraints <- paste0(myConstraints," "  )   }
        
        myConstraints <- paste0(myConstraints,names(TheCoef)[i], c('  > 0 ; ') )
        
      }  #fin del loop 
      
      # apply the model with constraints  
      AvMod_f.con <- restriktor(AvMod_f, constraints = myConstraints)
      summary(AvMod_f.con)
      # AvMod_f <- AvMod_f.con
      r2_reduc <- round(summary(AvMod_f.con)$R2.reduced, digits = 2)    
      # haqueo el modelo original para que tenga los parametros del modelo con restricciones y asi poder aplicar el metodo PREDICT
      AvMod_f$coefficients <- AvMod_f.con$b.restr
      
  }  #fin de la primera condicion    
  
  return(list(AvMod_f,r2_orig,r2_reduc) ) 
}  #fin de la condicion de construccion de las restricciones y resolucion modelo alternativo




# Funcion para comparar la prediccion del modelo total lm, del modelo con cross validation y lm, y del modelo cv, con restricciones
# En teoria el lm total deberia ser el mejor ya que predice sobre si mismo. Pero tiene el problema de los negativos
# Esos coeficientes negativos pueden hacer que el modelo lm-cv de resultados peores (en zonas donde los negativos conducen malas predicciones en el conjunto testing)
# El modelo cv-constraints podria ser que mejorase al lm-cv   
# mydata <- Datos_SupAgr_clima_Pest     #los datos de todos los paises, aunque no los haya empleado en el modelo base
# Count <- Aver_Count     #los countries que intervienen en el training
# strC <- stCrop
# varL <- lVarLand
# varAd <- AditionVars
# fmlaGlobal <- ouputA[[1]]       
CrosVal <- function (mydata,Count,strC,varL,varAd, fmlaGlobal,path_out ,TC1,id1){    
  
    #Me quedo solo con las columnas que empleara el modelo
    mydata <- mydata[ ,c("COUNTRY", strC,varL,varAd) ]  
    nrow(mydata)
    indexOut <- 2   # creo que siempre es el segundo
    names(mydata)
    head(mydata)
    str(mydata)


    # fmla <- formula(y~ x1+x2 ) 
    k=10    #number of folds
    #Randomly shuffle the data
    mydata2 <- mydata[sample(nrow(mydata)),]
    #Create 10 equally size folds
    folds <- cut(seq(1,nrow(mydata2)),breaks=k,labels=FALSE)
    
    #Perform k fold cross validation
    cv.Result <- NULL
    cv.ResultNoNeg <- NULL
    for(i in 1:k){
      # i=1
      print(i)
      #Segment your data by fold using the which() function 
      testIndexes <- which(folds==i,arr.ind=TRUE)
      testData <- mydata2[testIndexes, ]
      trainData <- mydata2[-testIndexes, ]
      trainData <- trainData[trainData$COUNTRY %in% Count,   ] # el training se hace solo con los paises que se hayan seleccionados. Esperemos que no se de nunca que se quede sin datos. EN ALGUN DATO PIERDEN MUCHA POTENCIA LOS MODELOS 
      
      #la formula a evaluar
      xnam <- c(varL,varAd)
      fmla <- as.formula(paste(strC, " ~ ", paste(xnam, collapse= "+") ))
      
      # Evalua cada fold modelo lm
      mod_fold <- lm(fmla, trainData )   # modelo lm-cv
      testPredict <- predict(mod_fold,newdata=testData)
      cv.train <- cbind(testData, prediccion=testPredict )
      cv.Result <- rbind(cv.Result,cv.train)
      
      #repito con el cv-Constraint
      res <- lm_coef_restric(mod_fold, varL)  #esta funcion nos devuelve el modelo haquedo para predecir
      res[1]   #con ese se hace la otra prediccion
      testPredict2 <- predict(res[[1]],newdata=testData)
      cv.train2 <- cbind(testData, prediccion= testPredict2)
      cv.ResultNoNeg <- rbind(cv.ResultNoNeg,cv.train2)
    }  #fin del cros validation

    # Predict from el Modelo preliminar mio (generado solo con algun pais)
    modTot <- lm(fmlaGlobal, data = cv.Result)  
    cv.Result$predModTot <- predict(modTot)  #prediccion sobre el training
    
    
    ymin <- min(cv.Result$predModTot,cv.Result$prediccion,cv.ResultNoNeg$prediccion ) 
    ymax <- max(cv.Result$predModTot,cv.Result$prediccion,cv.ResultNoNeg$prediccion ) 
    plot(cv.Result[,indexOut],cv.Result$predModTot, ylim=c(ymin,ymax),xlab="Real",ylab="Predict",cex=1)
    points(cv.Result[,indexOut],cv.Result$prediccion, col="red" ,cex=1)
    points(cv.ResultNoNeg[,indexOut],cv.ResultNoNeg$prediccion, col="blue",cex=1)
    abline(a=0,b=1) 
    legend("bottomright", legend=c("lm","lm_cv","constr-cv") 
           ,col= c("black","red","blue") ,pch=20, title = "Models")

        # #segunda version modelo sin negativos
        # resTot <- lm_coef_restric(modTot, VarExplain )     # la prediccion
        # resTot[1]
        # cv.ResultNoNeg$predModTot <- predict(lm(fmla, data = cv.ResultNoNeg))
        # plot(cv.ResultNoNeg[,indexOut],cv.ResultNoNeg$predModTot  )

    #### CALCULO DE LOS ESTIMADORES DE CALIDAD DE TODAS LAS PREDICCIONES #### 
    ind_pred <- which( names(cv.ResultNoNeg) == "prediccion"    )
    metr.cv <- computa_metric(cv.ResultNoNeg,indexOut,ind_pred )
    
    ind_pred <- which( names(cv.Result) == "prediccion"    )
    metr.const <- computa_metric(cv.Result,indexOut,ind_pred )
    
    ind_predT <- which( names(cv.Result) == "predModTot"    )
    metr.Tot <- computa_metric(cv.Result,indexOut,ind_predT )
    
    
    # MAE (Mean absolute error) represents the difference between the original and predicted values extracted by averaged the absolute difference over the data set.
    #  Cuanto mas cerca de cero mejor      # (Scale dependent value)

    # MSE (Mean Squared Error) represents the difference between the original and predicted values extracted by squared the average difference over the data set.
#        # (Scale dependent value)

    # RMSE (Root Mean Squared Error) is the error rate by the square root of MSE.

    # MAPE - Mean Absolute Percentage Error (Scale independent value)   https://support.numxl.com/hc/en-us/articles/115002386943-Forecast-Performance-Measures-Survey
  #  MAPE.const <- mean( abs( (d.const)/cv.ResultNoNeg[,indexOut]) * 100)
  #  MAPE.tot <- mean( abs( (d.tot)/cv.Result[,indexOut]) * 100)     #https://stats.stackexchange.com/questions/203053/calculating-mape
  #  MAPE.cv <- mean( abs( (d.cv)/cv.Result[,indexOut]) * 100)     # para la escala    https://www.researchgate.net/publication/257812432_Using_the_R-MAPE_index_as_a_resistant_measure_of_forecast_accuracy/figures?lo=1
    # < 10 es muy buena prediccion, cuanto mas pequeño mejor
    
    # COMBINO TODO EN UN CSV
    
    # TOTALES
    res <- data.frame (lv.cv= metr.cv[[1]], const.cv= metr.const[[1]],  
                       LmTot = metr.Tot[[1]] ) 
    rownames(res) <- c("NRMSE","SMAPE","NMAE","R2")
    
    # Por paises
    temp <- cbind(Co=100,t(res))
    rownames(metr.cv[[2]]) <- paste0("cv",1:nrow(metr.cv[[2]]))
    rownames(metr.const[[2]]) <- paste0("const",1:nrow(metr.const[[2]]))
    rownames(metr.Tot[[2]]) <- paste0("Tot",1:nrow(metr.Tot[[2]]))
    
    DF_res_tot <- rbind( temp , c(Co=0000,NRMSE=0000,SMAPE=0000,NMAE=1,R2=0000),  
                          metr.cv[[2]] ,c(Co=0000,NRMSE=0000,SMAPE=0000,NMAE=1,R2=0000), 
                         metr.const[[2]], c(Co=0000,NRMSE=0000,SMAPE=0000,NMAE=1,R2=0000), metr.Tot[[2]] )
    
 
    # lo escribe en un csv
    ThePath<- paste(path_out,"Prediccion",TC1,id1,strC,"cv_metricas.csv",sep="/")
    write.csv(DF_res_tot,ThePath)
    

   
  return(t(res))
}

# DF <- cv.ResultNoNeg
# i1 <- indexOut  :indice valor real
# i2 <- ind_pred  :indice valor predicho
computa_metric <- function(DF,i1,i2){  
  
    # Todos los paises
    # RAE  
    rae1 <- round( rae(DF[,i1],DF[,i2]) , digits=3) 
    # SMAPE 
    smape1 <- round( smape(DF[,i1],DF[,i2]) , digits=3)
    # MASE:  Mean Absolute Scaled Error  mase(actual, predicted, step_size)
    mase1<- round( mase(DF[,i1],DF[,i2],1) , digits=3) 
    # NMAE: for the normalized MAE    MAE / sd(estimate)       mae Mean Absolute Error / sd(estimate)
    nmae1 <- round( mae(DF[,i1],DF[,i2])/sd(DF[,i1]), digits=3) 
    # NRMSE:  normalized    rmse Root Mean Squared Error /sd(estimate)
    nrmse1 <- round( rmse(DF[,i1],DF[,i2])/sd(DF[,i1]), digits=3) 
    
    
    # R2
    d.const = DF[,i1] - DF[,i2]   # diferencia real predicho
     # R-squared (Coefficient of determination) represents the coefficient of how well the values fit compared to the original values. The value from 0 to 1 interpreted as percentages. The higher the value is, the better the model is.
    R2_1 = round( 1-(sum((d.const)^2)/sum((DF[,i1]-mean(DF[,i1]))^2)) , digits=3)

    res <- c(nrmse1,smape1,nmae1 ,R2_1)  #solucion para todos los datos
          
    # Pais a pais 
    DF_res <-NULL
    for (co in unique(DF$COUNTRY) ){
      print(co)
      DF2 <- DF[DF$COUNTRY==co, ]
      
      #rae
      raeC <- round( rae(DF2[,i1],DF2[,i2]) , digits=3) 
      # SMAPE 
      smapeC <- round( smape(DF2[,i1],DF2[,i2]) , digits=3)
      # MASE   Mean Absolute Scaled Error  mase(actual, predicted, step_size)
      maseC <- round( mase (DF2[,i1],DF2[,i2],1) , digits=3) 
      
      # NMAE' for the normalized MAE    MAE / sd(estimate)       mae Mean Absolute Error / sd(estimate)
      nmae <- round( mae(DF2[,i1],DF2[,i2])/sd(DF2[,i1]), digits=3)      
      
      # NRMSE :normalized    rmse Root Mean Squared Error /sd(estimate)
      nrmse <- round( rmse(DF2[,i1],DF2[,i2])/sd(DF2[,i1]), digits=3)   
      
      #  # N-s, very similar (Coefficient of determination) represents the coefficient of how well the values fit compared to the original values. The value from 0 to 1 interpreted as percentages. The higher the value is, the better the model is.
      d.const = DF2[,i1] - DF2[,i2]   # diferencia real predicho
      # R-squared (Coefficient of determination) represents the coefficient of how well the values fit compared to the original values. The value from 0 to 1 interpreted as percentages. The higher the value is, the better the model is.
      R2_1b = round( 1-(sum((d.const)^2)/sum((DF[,i1]-mean(DF2[,i1]))^2)) , digits=3)
      
      DF_res <- rbind ( DF_res ,data.frame (co,nrmse,smapeC,nmae,R2_1b ) )
    }   # end of for   
    
    names(DF_res) <- c("Co","NRMSE","SMAPE","NMAE","N-S")
    

    return(list(res,DF_res))    
}


# theData <- Datos_SupAgr_clima_Pest
# strCrop <- stCrop
# listVarLand <- lVarLand
# listVarClima <- lVarClima
# path <- path_out   
# id_PAtype <- TC
# id_PA <- id_selec_Pest
Preliminar_regres <- function( theData, strCrop,listVarLand, listVarClima,path,id_PAtype,id_PA ) {  
     
      index <- which(names(theData)  == strCrop)
      indexArea <- which(names(theData) %in%  listVarLand)
      
      # 3. Deja fuera (trainign) puntos que no aplican ese PA
      theDFTrain2 <- theData[theData[ ,index]>0 , ]
      #  table(theDFTrain2$COUNTRY) 
      # puntos donde hay Area dedicada 
      theDFTrain2 <- theData[theData[ ,indexArea]>0, ] 
      #  table(theDFTrain2$COUNTRY) 
      # solo quiero eliminar donde no hay area dedicada y se aplique el compuesto
      # o eliminar lo contrario   (atencion que podrian ser varios crop, no lo he modelado aun)
      theDFTrain2 <- theData[!((theData[ ,indexArea]==0) & (theData[ ,index]>0)), ] 
      table(theDFTrain2$COUNTRY) #imprimer esas tablas al final
      
      
      # 1. Ploting regresion by country (only if there is aplied in at leats one province of the country)
      names(theData)
      datos2 <- theData[theData[ ,index] >0 , ]
      datos <- theData     # mejor no anularlos porque sino no pasaria por el cero
      print(table(datos$COUNTRY))
      
      xnam <- listVarLand
      fmla <- as.formula(paste(strCrop, " ~ ", paste(xnam, collapse= "+") ,"| factor(COUNTRY)" )) 
      par(mfrow=c(1,1))
      theplot<-xyplot( fmla,
             data = datos, 
             layout = c(4,2),
             xlab = "Area",
             ylab = strCrop,
             panel=function(x,y){
               panel.points(x, y, col = 1)  #Add points
               panel.abline(lm(y~x))        #Add regression line
             })
      
      #only when the PA is aplied
      theplot2<-xyplot( fmla,
                       data = datos2, 
                       layout = c(4,2),
                       xlab = "Area",
                       ylab = strCrop,
                       panel=function(x,y){
                         panel.points(x, y, col = 1)  #Add points
                         panel.abline(lm(y~x))        #Add regression line
                       })
      
      
      print(theplot)  #en pantalla
      print(theplot2)
      thePath <- paste (path,"Prediccion", sep= "/")
      dir.create(file.path(thePath, id_PAtype), showWarnings = TRUE)
      thePath <- paste (thePath,id_PAtype, sep= "/")
      dir.create(file.path(thePath, id_PA), showWarnings = TRUE)
      thePath <- paste (thePath,id_PA, sep= "/")
      dir.create(file.path(thePath, strCrop), showWarnings = TRUE)
      
      thePath <- paste0(thePath,"/",strCrop)
      
      # 1. Open jpeg file
      name <- paste0( strCrop,"_CO", ".jpg") 
      nametot <- paste(thePath,name,sep="/")
      jpeg(nametot, width = 1000, height = 1000)
      # 2. Create the plot
      print(theplot)
        # 3. Close the file
      dev.off()
      
      #los paises que no aplican no deben entrar en la ecuacion. Tengo dudas respecto a provincias que no aplican
      # el modelo predice en esos puntos y al resto asigna cero. En el resto usaran otro PA
      
      # 2. Modelo Minimo (solo con las variables de corine para ese crop)
      theData <- datos
      xnam <- listVarLand
      fmla <- as.formula(paste(strCrop, " ~ ", paste(xnam, collapse= "+") ))
      M1 <- lm(fmla, data = theData)
     # abline(M1, lwd = 5)  #solo se podria dibumar si tiene una unica variables
      summary(M1)
            par(mfrow=c(1,2))
            maintext <- paste(  paste(fmla[c(2,3)], collapse = " ~ ") , "   R2=" ,round(summary(M1)$r.squared, digits = 2),sep="  ")
            plot(theData[ ,index], predict(M1),
                 xlab="actual",ylab="predicted" ,col=theData$COUNTRY,pch=20,main= maintext,cex.main=0.7 )
            legend("bottomright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
            abline(a=0,b=1)  
        #    boxplot(theData$J0000  )   # G1000
            plot(theData[ ,xnam[1]], theData[ ,index],
                 xlab=(xnam[1]),ylab=strCrop ,col=theData$COUNTRY,pch=20,main= maintext,cex.main=0.7 )
            legend("bottomright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
            abline(lm(fmla, data = theData))
            #   library(MASS)
            rr.bisquare <- rlm(fmla, data = theData, psi = psi.bisquare) #regresion robusta
            if( !is.na(coef(rr.bisquare)[2]) ) {abline(rr.bisquare, col="red" )}  #solo pinta la linea cuando no da error
            if( is.na(coef(rr.bisquare)[2]) ) { print("error con regresion robusta")}
            legend("topright", legend=c("LM","RLM"), col=c("black","red"),pch=20)
            
            nrow(theData)
            #  Diagnóstico del modelo
            plot(M1, which = 1, pch = 20)
         #   plot(M1, which = 2, pch = 20)
        #    plot(M1, which = 3, pch = 20)
         #   plot(M1, which = 4, pch = 20)
         #   library(MASS)
            infl <- influence.measures(M1)
            summary(infl)
        #    library(car)
         #   influencePlot(M1, id.n = 2)
        #    outlierTest(M1, cutoff = 0.05, n.max = 10, order = TRUE)
         #   cook <- cooks.distance(M1)
        #    labels <- rownames(theData)
         #   library(faraway)
         #   halfnorm(cook, 3, labs = labels, ylab = "Distancia de Cook")
         #   rr.bisquare <- rlm(fmla, data = theData, psi = psi.bisquare)
        #    summary(rr.bisquare)
            
            
            # hago aqui un plot con las regresiones eliminando iterativamente un pais
            par(mfrow=c(1,1))
            plot(theData[ ,xnam[1]], theData[ ,index],
                 xlab=xnam[1],ylab=strCrop ,col=theData$COUNTRY,pch=20,main= maintext,cex.main=0.7 )
            legend("bottomright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
            abline(lm(fmla, data = theData))
            
            listCO <- unique(theData$COUNTRY)
            vectR2 <- vector()
            for(ico in 1:length(listCO) ) {  #hago regresion quitando los datos de un pais iterativamente
              # ico <-1
              theData_co <-  theData[theData$COUNTRY!=listCO[ico],  ]
              M2_co <- lm(fmla , data = theData_co)
              vectR2[ico] <- round(summary(M2_co)$r.squared,3)
              abline(M2_co, col=ico+1)
            }
            names(vectR2)<- listCO
            legend("topright", legend=listCO ,col= 1+(1:length(listCO)),pch=20, title = "regresion without")

              
              
    
              
            # si quiero hacer alguno manualmente sin outlier (Regresion sobusta)
            # ico <-"ES"
            # theData_co <-  theData[theData$COUNTRY==ico,  ]  #solo ese pais
            # rr.bisquare <- rlm(fmla, data = theData_co, psi = psi.bisquare)
            # abline(rr.bisquare, col="orange")      #ese robusto estaria bien para el maximo
            
            
      # modelo con factor country  (con este no puedo predecir, ya que son distintos paises)    
      fmla <- as.formula(paste(strCrop, " ~ ", paste(xnam, collapse= "+") ,"+ factor(COUNTRY)" )) 
      M2 <- lm(fmla , data = theData)
      summary(M2)
          par(mfrow=c(1,2))
          maintext <- paste(  paste(fmla[c(2,3)], collapse = " ~ ") , "   R2=",round(summary(M2)$r.squared, digits = 2),sep="  ")
          plot(theData[ ,index], predict(M2),
               xlab="actual",ylab="predicted" ,col=theData$COUNTRY,pch=20,main= maintext,cex.main=0.7  )
          legend("bottomright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
          abline(a=0,b=1)  
           # pinto sobre valores reales la recta de regrasion
          plot(theData[ ,xnam[1]], theData[ ,index],
               xlab=(xnam[1]),ylab=strCrop ,col=theData$COUNTRY,pch=20,main= maintext,cex.main=0.7 )
          legend("bottomright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
          abline(lm(fmla, data = theData))

            
      # anadiendo  CLIMA PROBLEMA: si las pongo todas, mejora el ajuste pero por overfitting
      theData <- datos
      nrow(datos)
      names(theData)  # entre esas variables podria haber NA, que estropearia la regresion
      theData2 <- theData[,c("COUNTRY",strCrop,listVarLand,listVarClima )]
      nrow(theData2)
      names(theData2)
      theData2 <- theData2[complete.cases(theData2),]
      xnam <- c(listVarLand,listVarClima) #los nombres son los del tata orginal para no mover indices
      fmla <- as.formula(paste(strCrop, " ~ ", paste(xnam, collapse= "+") )) 
      M3 <- lm(fmla, data = theData2 )  #theData  datos
      summary(M3)
   #   plot(theData2[ ,index], predict(M3) )
      M3_step <- step(M3,trace = FALSE)  #el step no es  necesario y tarda bastante
      summary(M3_step)
          par(mfrow=c(1,2))
          maintext <- paste(  paste(fmla[c(2,3)], collapse = " ~ ") , "   R2=" ,round(summary(M3)$r.squared, digits = 2),sep="  ")
          plot(theData2[ ,2], predict(M3),
               xlab="actual",ylab="predicted" ,col=theData2$COUNTRY,pch=20,main= round(summary(M3_step)$r.squared, digits = 3) )
          legend("bottomright", legend=unique(theData2$COUNTRY),col=unique(theData2$COUNTRY),pch=20)
          abline(a=0,b=1)  
            # pinto sobre valores reales la recta de regrasion  names(theData2), indice 2 porque he reducido las columnas
          plot(theData2[ ,xnam[1]], theData2[ ,2],
               xlab=xnam[1],ylab=strCrop ,col=theData2$COUNTRY,pch=20,main= maintext,cex.main=0.7 )
          legend("bottomright", legend=unique(theData2$COUNTRY),col=unique(theData2$COUNTRY),pch=20)
          abline(lm(fmla, data = theData2))
          abline(M1)
          rr.bisquare <- rlm(fmla, data = theData2, psi = psi.bisquare) #regresion robusta
          if( !is.na(coef(rr.bisquare)[2]) ) {abline(rr.bisquare, col="red" )}  #solo pinta la linea cuando no da error
          if( is.na(coef(rr.bisquare)[2]) ) { print("error con regresion robusta")}
          legend("topright", legend=c("LM","RLM"), col=c("black","red"),pch=20)
          
          
        vi<-varImp(M3_step, scale = FALSE) 
        vi2<-as.data.frame(vi)
        par(mfrow=c(1,1),mar=c(9,3.7,2.1,0.2))
        df <- vi2[ order(vi2$Overall,decreasing = TRUE), ]
        barplot(df, names.arg=rownames(vi2)[order(vi2$Overall,decreasing = TRUE)],las=3 ,cex.names=0.7,main="Importance")
        varSelectionLR <- rownames(vi2)[order(vi2,decreasing = TRUE)][1:10]  #selecciono las 20 mas importantes
        print(varSelectionLR)  #la formula de la seleccion de variables ordenadas por importancia
       
        # xnam <- c(listVarLand,varSelectionLR)
        # fmla <- as.formula(paste(strCrop, " ~ ", paste(xnam, collapse= "+") )) 
        # M4 <- lm(fmla, data =theData )  #theData  datos
        # summary(M4)
        # print (fmla)  
        
       
    return(list( unique(datos$COUNTRY),  rownames(vi2)[order(vi2$Overall,decreasing = TRUE)] )  )
        
}



 # funcion que carga las predicciones de un PA 
# path1 <- path_out
# path2 <- path_2 
# TG <- TC
# id_selec_Pest <- id_selec_Pest
# LoO <- ListOfMod
read_prediccion <- function(path1,path2,TG,id_selec_Pest,LoO) {
  
    totPath <- paste0(path1,path2)
    
    if(LoO[1]==1){
      fileAR <- paste0(totPath,"/ARABLE_KG/ARABLE_KG_Predicc.csv")
      DFArabPred <- read.csv(file = fileAR , header = TRUE)
    }
    if(LoO[2]==1){
      fileFR <- paste0(totPath,"/FRUIT_KG/FRUIT_KG_Predicc.csv")
      DFFruitPred <- read.csv(file = fileFR , header = TRUE)
    } else {  DFFruitPred <- DFArabPred ;DFFruitPred$PredMin<-0 ;DFFruitPred$PredMax<-0 ;DFFruitPred$PredAv<-0  }   #si no se hace ese modelo, se pone la prediccion a nulo
    if(LoO[3]==1){
      fileOL <- paste0(totPath,"/OLIVES_KG/OLIVES_KG_Predicc.csv")
      DFOlivesPred <- read.csv(file = fileOL , header = TRUE)
    } else {  DFOlivesPred <- DFArabPred ;DFOlivesPred$PredMin<-0 ;DFOlivesPred$PredMax<-0 ;DFOlivesPred$PredAv<-0  } 
    if(LoO[4]==1){
      fileVI <- paste0(totPath,"/VINES_KG/VINES_KG_Predicc.csv")
      DFVinesPred <- read.csv(file = fileVI , header = TRUE)
    } else {  DFVinesPred <- DFArabPred ;DFVinesPred$PredMin<-0 ;DFVinesPred$PredMax<-0 ;DFVinesPred$PredAv<-0  } 
    if(LoO[5]==1){
      fileGR <- paste0(totPath,"/GRASS_KG/GRASS_KG_Predicc.csv")
      DFGrassPred <- read.csv(file = fileGR , header = TRUE)
    } else {  DFGrassPred <- DFArabPred ;DFGrassPred$PredMin<-0 ;DFGrassPred$PredMax<-0 ;DFGrassPred$PredAv<-0  } 
    if(LoO[6]==1){
      fileRI <- paste0(totPath,"/RICE_KG/RICE_KG_Predicc.csv")
      DFRicePred <- read.csv(file = fileRI , header = TRUE)
    } else {  DFRicePred <- DFArabPred ;DFRicePred$PredMin<-0 ;DFRicePred$PredMax<-0 ;DFRicePred$PredAv<-0  } 

        #### VERIFICAR SI HAY NEGATIVOS EN ALGUNO #######
    
    # Todos los AVERAGE en un unico DF total, y la suma 
    DF_total_Av <-  Reduce(function(x,y) merge(x = x, y = y, by = "NUTS_ID"), 
           list(DFArabPred[,c(2,4)],DFFruitPred[,c(2,4)],DFOlivesPred[,c(2,4)],DFVinesPred[,c(2,4)],DFGrassPred[,c(2,4)],DFRicePred[,c(2,4)]  ))
    names(DF_total_Av) <- c("NUTS3", "Arab_KG","Fruit_KG","Oliv_KG","Vines_KG","Grass_KG","Rice_KG" )
    DF_total_Av$KG_TOT <- rowSums(DF_total_Av[,2:7])
    #genero country
    DF_total_Av$COUNTRY <-  substr(DF_total_Av$NUTS3, 1, 2)   
 
    # Lo mismo para los MINIMOS
    DF_total_Min <-  Reduce(function(x,y) merge(x = x, y = y, by = "NUTS_ID"), 
                           list(DFArabPred[,c(2,3)],DFFruitPred[,c(2,3)],DFOlivesPred[,c(2,3)],DFVinesPred[,c(2,3)],DFGrassPred[,c(2,3)],DFRicePred[,c(2,3)]  ))
    names(DF_total_Min) <- c("NUTS3", "Arab_KG","Fruit_KG","Oliv_KG","Vines_KG","Grass_KG","Rice_KG" )
    DF_total_Min$KG_TOT <- rowSums(DF_total_Min[,2:7])
    #genero country
    DF_total_Min$COUNTRY <-  substr(DF_total_Min$NUTS3, 1, 2)
    
    # Lo mismo para los MAXIMOS
    DF_total_Max <- Reduce(function(x,y) merge(x = x, y = y, by = "NUTS_ID"), 
                            list(DFArabPred[,c(2,5)],DFFruitPred[,c(2,5)],DFOlivesPred[,c(2,5)],DFVinesPred[,c(2,5)],DFGrassPred[,c(2,5)],DFRicePred[,c(2,5)]  ))
    names(DF_total_Max) <- c("NUTS3", "Arab_KG","Fruit_KG","Oliv_KG","Vines_KG","Grass_KG","Rice_KG" )
    DF_total_Max$KG_TOT <- rowSums(DF_total_Max[,2:7])
    #genero country
    DF_total_Max$COUNTRY <-  substr(DF_total_Max$NUTS3, 1, 2)
    
    
    ##############################################
    # GENERO TAMBIEN EL FICHERO DE CHIARA
    if(LoO[1]==1){
      fileAR <- paste0(totPath,"/ARABLE_KG/ARABLE_KG_PredTot.csv")
      DFArabPred <- read.csv(file = fileAR , header = TRUE)
    }
    if(LoO[2]==1){
      fileFR <- paste0(totPath,"/FRUIT_KG/FRUIT_KG_PredTot.csv")
      DFFruitPred <- read.csv(file = fileFR , header = TRUE)
    } else {  DFFruitPred <- DFArabPred ;DFFruitPred$PredMin<-0 ;DFFruitPred$PredMax<-0 ;DFFruitPred$PredAv<-0  }   #si no se hace ese modelo, se pone la prediccion a nulo
    if(LoO[3]==1){
      fileOL <- paste0(totPath,"/OLIVES_KG/OLIVES_KG_PredTot.csv")
      DFOlivesPred <- read.csv(file = fileOL , header = TRUE)
    } else {  DFOlivesPred <- DFArabPred ;DFOlivesPred$PredMin<-0 ;DFOlivesPred$PredMax<-0 ;DFOlivesPred$PredAv<-0  } 
    if(LoO[4]==1){
      fileVI <- paste0(totPath,"/VINES_KG/VINES_KG_PredTot.csv")
      DFVinesPred <- read.csv(file = fileVI , header = TRUE)
    } else {  DFVinesPred <- DFArabPred ;DFVinesPred$PredMin<-0 ;DFVinesPred$PredMax<-0 ;DFVinesPred$PredAv<-0  } 
    if(LoO[5]==1){
      fileGR <- paste0(totPath,"/GRASS_KG/GRASS_KG_PredTot.csv")
      DFGrassPred <- read.csv(file = fileGR , header = TRUE)
    } else {  DFGrassPred <- DFArabPred ;DFGrassPred$PredMin<-0 ;DFGrassPred$PredMax<-0 ;DFGrassPred$PredAv<-0  } 
    if(LoO[6]==1){
      fileRI <- paste0(totPath,"/RICE_KG/RICE_KG_PredTot.csv")
      DFRicePred <- read.csv(file = fileRI , header = TRUE)
    } else {  DFRicePred <- DFArabPred ;DFRicePred$PredMin<-0 ;DFRicePred$PredMax<-0 ;DFRicePred$PredAv<-0  } 
    
    #### VERIFICAR SI HAY NEGATIVOS EN ALGUNO #######
    
    # Todos los AVERAGE en un unico DF total, y la suma 
    DF_total_Av <-  Reduce(function(x,y) merge(x = x, y = y, by = "NUTS_ID",all = TRUE), 
                           list(DFArabPred[,c(1,3)],DFFruitPred[,c(1,3)],DFOlivesPred[,c(1,3)],DFVinesPred[,c(1,3)],DFGrassPred[,c(1,3)],DFRicePred[,c(1,3)]  ))
    names(DF_total_Av) <- c("NUTS3", "Arab_KG","Fruit_KG","Oliv_KG","Vines_KG","Grass_KG","Rice_KG" )
   # taking care with the NA before sum
    DF_total_Av$KG_TOT <- rowSums(DF_total_Av[,2:7],na.rm=TRUE)  #
    #genero country
    DF_total_Av$COUNTRY <-  substr(DF_total_Av$NUTS3, 1, 2)   
    # Ad colum with the AS id
    DF_total_Av <- cbind(id_selec_Pest,DF_total_Av)
    names(DF_total_Av)[1] <- "ASID"

    # Escrivo la version con NA
    thePath <- paste0(totPath,"/")
    # Escribo un CSV con el conjunto de los paises donde concociamos el valor
    file <- paste0(thePath,"/",id_selec_Pest,"NA","_PredictTotal.csv")
    write.csv( DF_total_Av , file = file,row.names=FALSE)
    
    # Convierto los NA en ceros y escribo otro fichero
    DF_total_Av[is.na(DF_total_Av)] <- 0
    file <- paste0(thePath,"/",id_selec_Pest,"_PredictTotal.csv")
    write.csv( DF_total_Av , file = file,row.names=FALSE)

    
    
    
    return( mis_predicc=list(DF_total_Min,DF_total_Av,DF_total_Max) )   
}




# path1 <- path_out
# path2 <- path_2 
# TG <- TC
# id_selec_Pest <- id_selec_Pest
# LoO <- ListOfMod
# DF_REAL <- Datos_SupAgr_clima_Pest
Compare_real_vs_prediccion <- function(path1,path2,TG,id_selec_Pest,LoO,DF_REAL) {
  
  totPath <- paste0(path1,path2)
  # lectura de los ficheros con la prediccion
  # GENERO TAMBIEN EL FICHERO DE CHIARA
  if(LoO[1]==1){
    fileAR <- paste0(totPath,"/ARABLE_KG/ARABLE_KG_PredTot.csv")
    DFArabPred <- read.csv(file = fileAR , header = TRUE)
  }
  if(LoO[2]==1){
    fileFR <- paste0(totPath,"/FRUIT_KG/FRUIT_KG_PredTot.csv")
    DFFruitPred <- read.csv(file = fileFR , header = TRUE)
  } else {  DFFruitPred <- DFArabPred ;DFFruitPred$PredMin<-0 ;DFFruitPred$PredMax<-0 ;DFFruitPred$PredAv<-0  }   #si no se hace ese modelo, se pone la prediccion a nulo
  if(LoO[3]==1){
    fileOL <- paste0(totPath,"/OLIVES_KG/OLIVES_KG_PredTot.csv")
    DFOlivesPred <- read.csv(file = fileOL , header = TRUE)
  } else {  DFOlivesPred <- DFArabPred ;DFOlivesPred$PredMin<-0 ;DFOlivesPred$PredMax<-0 ;DFOlivesPred$PredAv<-0  } 
  if(LoO[4]==1){
    fileVI <- paste0(totPath,"/VINES_KG/VINES_KG_PredTot.csv")
    DFVinesPred <- read.csv(file = fileVI , header = TRUE)
  } else {  DFVinesPred <- DFArabPred ;DFVinesPred$PredMin<-0 ;DFVinesPred$PredMax<-0 ;DFVinesPred$PredAv<-0  } 
  if(LoO[5]==1){
    fileGR <- paste0(totPath,"/GRASS_KG/GRASS_KG_PredTot.csv")
    DFGrassPred <- read.csv(file = fileGR , header = TRUE)
  } else {  DFGrassPred <- DFArabPred ;DFGrassPred$PredMin<-0 ;DFGrassPred$PredMax<-0 ;DFGrassPred$PredAv<-0  } 
  if(LoO[6]==1){
    fileRI <- paste0(totPath,"/RICE_KG/RICE_KG_PredTot.csv")
    DFRicePred <- read.csv(file = fileRI , header = TRUE)
  } else {  DFRicePred <- DFArabPred ;DFRicePred$PredMin<-0 ;DFRicePred$PredMax<-0 ;DFRicePred$PredAv<-0  } 
  
  #### VERIFICAR SI HAY NEGATIVOS EN ALGUNO #######
  
  # Todos los AVERAGE en un unico DF total, y la suma 
  DF_total_Av <-  Reduce(function(x,y) merge(x = x, y = y, by = "NUTS_ID",all = TRUE), 
                         list(DFArabPred[,c(1,3)],DFFruitPred[,c(1,3)],DFOlivesPred[,c(1,3)],DFVinesPred[,c(1,3)],DFGrassPred[,c(1,3)],DFRicePred[,c(1,3)]  ))
  names(DF_total_Av) <- c("NUTS3", "Arab_KG","Fruit_KG","Oliv_KG","Vines_KG","Grass_KG","Rice_KG" )
  # taking care with the NA before sum
  DF_total_Av$KG_TOT <- rowSums(DF_total_Av[,2:7],na.rm=TRUE)  #
  #genero country
  DF_total_Av$COUNTRY <-  substr(DF_total_Av$NUTS3, 1, 2)   
  # Ad colum with the AS id
  DF_total_Av <- cbind(id_selec_Pest,DF_total_Av)
  names(DF_total_Av)[1] <- "ASID"
  

  par(mfrow=c(2,2) )
  #################################################################
  ## COMPARANDO REAL Y MODELADO 
  # solo en los paises del real (PARA EL MODELADO)
  DF_total_Av2 <- DF_total_Av[DF_total_Av$COUNTRY %in% c("BE","DE","DK","ES","FR","IE","IT","NL"),  ]
  DF_total_Av2$COUNTRY <- as.factor(DF_total_Av2$COUNTRY)
  
  # los mezclo
  DF_Model_R <- merge(x = DF_REAL[,1:16] , y =DF_total_Av2[,1:9] , by = c("NUTS3")  )   #solo se incluyen los que estan en los dos
  names(DF_Model_R)
  # ARABLE_KG: is the real value
  # Arab_KG: is modeled
  
  # ARAB
  if(LoO[1]==1){
    indic <- c(11,18)
    crop <- "ARAB"
    max <- 50000
    Plot_real_vs_prediccion(DF_Model_R,indic,crop,max)   }
  
  # FRUITS
  if(LoO[2]==1){
    indic <- c(13,19)
    crop <- "FRUIT"
    max <- 5000
    Plot_real_vs_prediccion(DF_Model_R,indic,crop,max) }
  
  # OLIV
  if(LoO[3]==1){
    indic <- c(15,20)
    crop <- "OLIV"
    max <- 5000
    Plot_real_vs_prediccion(DF_Model_R,indic,crop,max) }
  
  # VINES
  if(LoO[4]==1){
  indic <- c(16,21)
  crop <- "VINES"
  max <- 20000
  Plot_real_vs_prediccion(DF_Model_R,indic,crop,max)   }
  
  # GRASS
  if(LoO[5]==1){
  indic <- c(14,22)
  crop <- "GRASS"
  Plot_real_vs_prediccion(DF_Model_R,indic,crop,max)  }

  # TOTAL
  indic <- c(9,24)
  crop <- "TOTAL"
  Plot_real_vs_prediccion(DF_Model_R,indic,crop,max)
  
  
  # PLOT MODELADO PARA TODOS LOS E PAISES
  DF_total_Av2 <- DF_total_Av  #[!(DF_total_Av$COUNTRY %in% c("BE","DE","DK","ES","FR","IE","IT","NL")),  ]
  DF_total_Av2$COUNTRY <- as.factor(DF_total_Av2$COUNTRY)
 
  par(mfrow=c(2,3) )
  
  # ARAB OTHER COUNTRIES
  if(LoO[1]==1){
    indic <- c(3)
    crop <- "ARAB OTHER countries"
    names(DF_total_Av2)
    DF_ACT_PRE <- aggregate(DF_total_Av2[,indic] , by=list(Category=DF_total_Av2$COUNTRY), FUN=sum, na.rm=TRUE )
    barplot(height=t(as.matrix(DF_ACT_PRE[,c(2)])), names=DF_ACT_PRE[,c(1)],main=crop,las=3,
            legend = c("modeled"),beside=TRUE)  }
  
  # FRUIT OTHER COUNTRIES
  if(LoO[2]==1){
    indic <- c(4)
    crop <- "FRUIT OTHER countries"
    names(DF_total_Av2)
    DF_ACT_PRE <- aggregate(DF_total_Av2[,indic] , by=list(Category=DF_total_Av2$COUNTRY), FUN=sum, na.rm=TRUE )
    barplot(height=t(as.matrix(DF_ACT_PRE[,c(2)])), names=DF_ACT_PRE[,c(1)],main=crop,las=3,
            legend = c("modeled"),beside=TRUE)   }
  
  # VINES OTHER COUNTRIES
  if(LoO[4]==1){
    indic <- c(6)
    crop <- "VINES OTHER COUNTRIES"
    DF_ACT_PRE <- aggregate(DF_total_Av2[,indic] , by=list(Category=DF_total_Av2$COUNTRY), FUN=sum, na.rm=TRUE )
    barplot(height=t(as.matrix(DF_ACT_PRE[,c(2)])), names=DF_ACT_PRE[,c(1)],main=crop,las=3,
            legend = c("modeled"),beside=TRUE)   }
    
  # GRASS OTHER COUNTRIES
  if(LoO[5]==1){
    indic <- c(7)
    crop <- "GRASS OTHER countries"
    DF_ACT_PRE <- aggregate(DF_total_Av2[,indic] , by=list(Category=DF_total_Av2$COUNTRY), FUN=sum, na.rm=TRUE )
    barplot(height=t(as.matrix(DF_ACT_PRE[,c(2)])), names=DF_ACT_PRE[,c(1)],main=crop,las=3,
            legend = c("modeled"),beside=TRUE)
  }
  
  # OLIV OTHER COUNTRIES
  if(LoO[3]==1){
    indic <- c(6)
    crop <- "OLIV OTHER COUNTRIES"
    DF_ACT_PRE <- aggregate(DF_total_Av2[,indic] , by=list(Category=DF_total_Av2$COUNTRY), FUN=sum, na.rm=TRUE )
    barplot(height=t(as.matrix(DF_ACT_PRE[,c(2)])), names=DF_ACT_PRE[,c(1)],main=crop,las=3,
            legend = c("modeled"),beside=TRUE)   }
  
    
  # TOTAL OTHER COUNTRIES
    indic <- c(9)
    crop <- "TOTAL OTHER countries"
    DF_ACT_PRE <- aggregate(DF_total_Av2[,indic] , by=list(Category=DF_total_Av2$COUNTRY), FUN=sum, na.rm=TRUE )
    barplot(height=t(as.matrix(DF_ACT_PRE[,c(2)])), names=DF_ACT_PRE[,c(1)],main=crop,las=3,
            legend = c("modeled"),beside=TRUE)  
  
  
}



# cr <-crop
# index <- indic
# DF_Model_Real <- DF_Model_R
Plot_real_vs_prediccion  <- function(DF_Model_Real,index,cr,limmax){
  
  plot(DF_Model_Real[,index[1]], DF_Model_Real[,index[2]],
       xlab="actual",ylab="predicted" ,col=DF_Model_Real$COUNTRY,pch=20,main= paste0(cr,"_",id_selec_Pest),cex.main=0.7  )
  legend("bottomright", legend=unique(DF_Model_Real$COUNTRY),col=unique(DF_Model_Real$COUNTRY),pch=20)
  abline(a=0,b=1)  
  # zoom
  plot(DF_Model_Real[,index[1]], DF_Model_Real[,index[2]],
       xlab="actual",ylab="predicted" ,col=DF_Model_Real$COUNTRY,pch=20,main= paste0(cr,"_",id_selec_Pest),cex.main=0.7,
       xlim=c(0,limmax), ylim=c(0,limmax)  )
  legend("bottomright", legend=unique(DF_Model_Real$COUNTRY),col=unique(DF_Model_Real$COUNTRY),pch=20)
  abline(a=0,b=1) 
  
  # totales por paises
  DF_Model_Real
  head(DF_Model_Real)
  names(DF_Model_Real)
  DF_ACT_PRE <- aggregate(DF_Model_Real[,index] , by=list(Category=DF_Model_Real$COUNTRY), FUN=sum, na.rm=TRUE )
 # DF_ACT_PRE$rat <- DF_ACT_PRE$Arab_KG/DF_ACT_PRE$ARABLE_KG     function(x) 
  plot(DF_ACT_PRE[,2] ,DF_ACT_PRE[,3] ,xlab="Real",ylab="Modeled" ,main=cr   )
  text(DF_ACT_PRE[,3] ~ DF_ACT_PRE[,2]  , labels=Category ,data=DF_ACT_PRE, cex=0.9, font=2)
  abline(coef = c(0,1))
  barplot(height=t(as.matrix(DF_ACT_PRE[,c(2,3)])), names=DF_ACT_PRE[,c(1)],main=cr,
          legend = c("actual","modeled"),beside=TRUE)
}




# DF_total <- DF_predicc
# TG <- TC
# id_selec_Pest <- id_selec_Pest
sumary_predictions <- function(DF_total,TG,id_selec_Pest) {
  
    # 1. Cantidades totales por paises
    Kg_by_CO <- aggregate( DF_total$KG_TOT, by=list(Category=DF_total$COUNTRY), FUN=sum)
    rownames(Kg_by_CO) <- Kg_by_CO$Category 
    Kg_by_CO$Category <- NULL
    barplot(t(as.matrix(Kg_by_CO)), ylab="Total Kg by country")
  
  
    # 2. Proporcion  principio activo y pais, especificamente por cierto tipo de cultivos
    totKG <- round(sum(DF_total$KG_TOT),1)  
    totrateARA <- round(sum(DF_total$Arab_KG)/sum(DF_total$KG_TOT)*100,1)
    totrateRIZE <- round(sum(DF_total$Rice_KG)/sum(DF_total$KG_TOT)*100,1)
    totrateFRUT <- round(sum(DF_total$Fruit_KG)/sum(DF_total$KG_TOT)*100,1)
    totrateGRASS <- round(sum(DF_total$Grass_KG)/sum(DF_total$KG_TOT)*100,1)
    totrateOLIV <- round(sum(DF_total$Oliv_KG)/sum(DF_total$KG_TOT)*100,1)
    totrateVINES <- round(sum(DF_total$Vines_KG)/sum(DF_total$KG_TOT)*100,1)
    
    #cantidades Absolutas
    names(DF_total)
    boxplot(DF_total$KG_TOT ~ DF_total$COUNTRY, cex.main=0.8,las=2,
            xlab="country", ylab="KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Kg Totales: ",totKG , "Kg ") )
    
    par(mfrow=c(2,3) )
    boxplot(DF_total$Arab_KG ~ DF_total$COUNTRY   , cex.main=0.8,las=2,
            xlab="country", ylab="ARABLE_KG",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate ARA: ",totrateARA , "%") )
    
    boxplot(DF_total$Fruit_KG ~ DF_total$COUNTRY, cex.main=0.8,las=2,
            xlab="country", ylab="FRUIT_KG",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";   Tot Rate FRUT: ",totrateFRUT , "%") )
    
    boxplot(DF_total$Oliv_KG ~ DF_total$COUNTRY, cex.main=0.8,las=2,
            xlab="country", ylab="OLIVES_KG",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";   Tot Rate OLIV: ",totrateOLIV , "%") )
    
    boxplot(DF_total$Vines_KG/DF_total$KG_TOT  ~ DF_total$COUNTRY, cex.main=0.8,las=2,
            xlab="country", ylab="VINES_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate VINES: ",totrateVINES , "%") )
    
    boxplot(DF_total$Grass_KG ~ DF_total$COUNTRY, cex.main=0.8,las=2,
            xlab="country", ylab="GRASS_KG",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";   Tot Rate GRASS: ",totrateGRASS , "%") )
    
    boxplot(DF_total$Rice_KG ~ DF_total$COUNTRY, cex.main=0.8,las=2,
            xlab="country", ylab="RICE_KG",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate RICE: ",totrateRIZE , "%") )
    
    
#cantidades relativas
    names(DF_total)
    par(mfrow=c(2,3) )
    boxplot(DF_total$Arab_KG/DF_total$KG_TOT ~ DF_total$COUNTRY, cex.main=0.8,las=2,
            xlab="country", ylab="ARABLE_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate ARA: ",totrateARA , "%") )
    
    boxplot(DF_total$Fruit_KG/DF_total$KG_TOT ~ DF_total$COUNTRY, cex.main=0.8,las=2,
            xlab="country", ylab="FRUIT_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";   Tot Rate FRUT: ",totrateFRUT , "%") )
    
    boxplot(DF_total$Oliv_KG/DF_total$KG_TOT ~ DF_total$COUNTRY, cex.main=0.8,las=2,
            xlab="country", ylab="OLIVES_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";   Tot Rate OLIV: ",totrateOLIV , "%") )
    
    boxplot(DF_total$Vines_KG/DF_total$KG_TOT  ~ DF_total$COUNTRY, cex.main=0.8,las=2,
            xlab="country", ylab="VINES_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate VINES: ",totrateVINES , "%") )
    
    boxplot(DF_total$Grass_KG/DF_total$KG_TOT ~ DF_total$COUNTRY, cex.main=0.8,las=2,
            xlab="country", ylab="GRASS_KG /Ara_ha",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";   Tot Rate GRASS: ",totrateGRASS , "%") )
    
    boxplot(DF_total$Rice_KG/DF_total$KG_TOT ~ DF_total$COUNTRY, cex.main=0.8,las=2,
            xlab="country", ylab="RICE_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate RICE: ",totrateRIZE , "%") )
    
     
    # PENDIENTE: Sumario con cantidades totales del compuesto
    
    # QUERRE MAPEAR TAMBIEN LO PREDICHO Y LO QUE TENIAMOS MEDIDO, 
    # Para ello tengo que hacer un DF con todo
    
    
    
}




# En esta descriptiva analizamos como se distribuye por paises 
# theData <- Pest_Row_dat_regre   # esos datos ya son para un unico principio activo
# ThePA <- id_selec_Pest
# TG <- TC
# provTable <- refProvTable   
descrip_summary_pest <- function( theData, TG, ThePA,provTable ) {  
    
  TheMinText <- paste0(TG," ", ThePA)
  
  # 1. sumario del numero de provincias en cada pais, en las que se emplea dicho principio activo
    # con esto podemos saber si en alguna provincia no hay consumo
    table(theData$COUNTRY)
    RefProv <-rbind(provTable, table(theData$COUNTRY))
    rownames(RefProv) <- c("Tot Prov","PA Prov")
    print(RefProv)

    
  # 2. Plot de KG frente a Ha Totales (a mas pendiente ese compuesto se aplica en mayor ratio)
    plot(theData$HA_FINAL,theData$KG_FINAL,col=theData$COUNTRY,pch=20, main=TheMinText )
    legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  # 3. Plot ratio de KG frente a Ha. Si es constante en un mismo pais indica que ha sido desagragado 
    plot(theData$Ratio, col=theData$COUNTRY,pch=20, main=TheMinText ) 
    legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
    # KG totales por pais
    Kg_by_CO <- aggregate(theData$KG_FINAL, by=list(Category=theData$COUNTRY), FUN=sum)
    rownames(Kg_by_CO) <- Kg_by_CO$Category 
    Kg_by_CO$Category <- NULL
    barplot(t(as.matrix(Kg_by_CO)), ylab="Total Kg by country")
 
    Kg_by_CO$rate <- Kg_by_CO$x/sum(Kg_by_CO$x)*100

    firstCo <- Kg_by_CO[order(-Kg_by_CO$rate),][1,]   #saco el pais con su ratio relativo aplicaion
   ncount <- nrow(Kg_by_CO)
    
  # 4. Proporcion  principio activo y pais, especificamente por cierto tipo de cultivos
  totrateARA <- round(sum(theData$ARABLE_KG)/sum(theData$KG_FINAL)*100,1)
  totrateRIZE <- round(sum(theData$RICE_KG)/sum(theData$KG_FINAL)*100,1)
  totrateFRUT <- round(sum(theData$FRUIT_KG)/sum(theData$KG_FINAL)*100,1)
  totrateGRASS <- round(sum(theData$GRASS_KG)/sum(theData$KG_FINAL)*100,1)
  totrateOLIV <- round(sum(theData$OLIVES_KG)/sum(theData$KG_FINAL)*100,1)
  totrateVINES <- round(sum(theData$VINES_KG)/sum(theData$KG_FINAL)*100,1)
  totrateVINES <- round(sum(theData$  VINES_KG)/sum(theData$KG_FINAL)*100,1)
  
  # Por provincias el total de KG final deberia ser (al menos menor que el de esos grupos sumados)
  Prov_Tot <- aggregate(theData$KG_FINAL, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Ara <- aggregate(theData$ARABLE_KG, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Rize <- aggregate(theData$RICE_KG, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Fru <- aggregate(theData$FRUIT_KG, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Gras <- aggregate(theData$GRASS_KG, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Oliv <- aggregate(theData$OLIVES_KG, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Vine <- aggregate(theData$VINES_KG, by=list(Category=theData$NUTS3), FUN=sum)
  
  Total_Prov <- Reduce(function(x, y) merge(x, y, by="Category", all=TRUE), list(Prov_Tot, Prov_Ara, Prov_Rize, Prov_Fru,Prov_Gras,Prov_Oliv,Prov_Vine))
  names(Total_Prov)<-c("NUTS3","FINAL","ARABLE","RICE","FRUIT","GRASS","OLIVES","VINES"  )
  # convierto a ratios
    Total_Prov$ARABLE <- Total_Prov$ARABLE / Total_Prov$FINAL
    Total_Prov$RICE <- Total_Prov$RICE / Total_Prov$FINAL
    Total_Prov$FRUIT <- Total_Prov$FRUIT / Total_Prov$FINAL
    Total_Prov$GRASS <- Total_Prov$GRASS / Total_Prov$FINAL
    Total_Prov$OLIVES <- Total_Prov$OLIVES / Total_Prov$FINAL
    Total_Prov$VINES <- Total_Prov$VINES / Total_Prov$FINAL

    
  # mezclo el campo pais
  theData2 <- merge(x = theData, y = Total_Prov, by = c("NUTS3","NUTS3")  )
    
  par(mfrow=c(2,3) )
  boxplot(theData2$ARABLE ~ theData2$COUNTRY, cex.main=0.9,
          xlab="country", ylab="ARABLE_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate ARA: ",totrateARA , "%") )
 
  boxplot(theData2$FRUIT ~ theData2$COUNTRY, cex.main=0.9,
          xlab="country", ylab="FRUIT_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";   Tot Rate FRUT: ",totrateFRUT , "%") )
  
  boxplot(theData2$OLIVES ~ theData2$COUNTRY, cex.main=0.9,
          xlab="country", ylab="OLIVES_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";   Tot Rate OLIV: ",totrateOLIV , "%") )
  
  boxplot(theData2$VINES  ~ theData2$COUNTRY, cex.main=0.9,
          xlab="country", ylab="VINES_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate VINES: ",totrateVINES , "%") )
  
  boxplot(theData2$GRASS ~ theData2$COUNTRY, cex.main=0.9,
          xlab="country", ylab="GRASS_KG /Ara_ha",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";   Tot Rate GRASS: ",totrateGRASS , "%") )
  
  boxplot(theData2$RICE ~ theData2$COUNTRY, cex.main=0.9,
          xlab="country", ylab="RICE_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate RICE: ",totrateRIZE , "%") )
  
  
  return(list(totrateARA, firstCo,ncount))
  
}


# En esta descriptiva analizamos como se distribuye por paises el area agricola
# theData <- Datos_SupAgr_Pest   # esos datos ya son para un unico principio activo
# ThePA <- id_selec_Pest
# TG <- TC
# ThePA <- id_selec_Pest
# provTable <- refProvTable   
descrip_summary_land <- function( theData, TG, ThePA,provTable ) {  
  
  ###########################33
  ### SOLO RELATIVO A LAS AREAS DEDICADAS
  ############
  
 
  #######################################3
  ###### RELATIVOS A ARABLE   ######
  par(mfrow=c(2,5) )
  # C0000:  Cereales respecto a ARA
  rate <-  round( 100*sum(theData$C0000) / sum(theData$ARA ) , 1)
  boxplot(100*theData$C0000/theData$ARA ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="C0000 / ARA",main = paste0("C0000. Cereales",  "; rate area:  ", rate, "%") )
  
  # G0000: Plants harvested green from arable land respecto a ARA
  rate <-  round( 100*sum(theData$G0000) / sum(theData$ARA ) , 1)
  boxplot(100*theData$G0000/theData$ARA ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="G0000 / ARA",main = paste0(" G0000. Plants harvested green ",  "; rate area:  ", rate, "%") )
  
  # I0000: Industrial crops
  rate <-  round( 100*sum(theData$I0000) / sum(theData$ARA ) , 1)
  boxplot(100*theData$I0000/theData$ARA ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="I0000: Industrial crops / ARA",main = paste0("I0000. Industrial crops",  "; rate area:  ", rate, "%") )
  
  # Q000: Fallow land
  rate <-  round( 100*sum(theData$Q000) / sum(theData$ARA ) , 1)
  boxplot(100*theData$Q000 / theData$ARA ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="Q000: Fallow land/ ARA",main = paste0("Q000: Fallow landn",  "; rate area:  ", rate, "%") )
  
  # R0000: Root crops
  rate <-  round( 100*sum(theData$R0000) / sum(theData$ARA ) , 1)
  boxplot(100*theData$R0000 / theData$ARA ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="R0000: Root crops / ARA",main = paste0( "R0000: Root crops",  "; rate area:  ", rate, "%") )
  
  # P000: Dry pulses and protein crops for the production of grain
  rate <-  round( 100*sum(theData$P0000) / sum(theData$ARA ) , 1)
  boxplot(100*theData$P0000 / theData$ARA ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="P000: Dry pulses and protein crops  / ARA",main = paste0("P0000. Dry pulses and protein",  "; rate area:  ", rate, "%") )
  
  # V0000_S0000: Fresh vegetables (including melons) and strawberries
  rate <-  round( 100*sum(theData$V0000_S0000) / sum(theData$ARA ) , 1)
  boxplot(100*theData$V0000_S0000 / theData$ARA ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="V0000_S0000: Fresh vegetables (including melons  / ARA",main = paste0("V0000_S0000: Fresh vegetable",  "; rate area:  ", rate, "%") )
  
  # E0000: Seeds and seedlings
  rate <-  round( 100*sum(theData$E0000) / sum(theData$ARA ) , 1)
  boxplot(100*theData$E0000 / theData$ARA ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="E0000: Seeds and seedlings  / ARA",main = paste0("E0000: Seeds and seedlings",  "; rate area:  ", rate, "%") )
  
  # L0000: Nurseries
  rate <-  round( 100*sum(theData$L0000) / sum(theData$ARA ) , 1)
  boxplot(100*theData$L0000 / theData$ARA ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="L0000: Nurseries  / ARA",main = paste0("L0000:Nurseries",  "; rate area:  ", rate, "%") )
  
  # N0000 Flowers and ornamental plants
  rate <-  round( 100*sum(theData$N0000) / sum(theData$ARA ) , 1)
  boxplot(100*theData$N0000 / theData$ARA ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="N0000 Flowers and ornamental plants / ARA",main = paste0( "N0000 ornamental",  "; rate area:  ", rate, "%") )
  
  
  
################################3
  ### lo siguiente es para mejorara
  
  
  #analisis comparativo  por nuts de cuanta arrea se dedica a cada tipo de cultivo, absoluto y relativo
  theData$TotalArea <- rowSums(theData[,c("ARA","C0000","E0000" ,"G0000" ,
                                          "I0000","L0000","N0000", "P0000","Q0000","R0000",
                                          "U1000","ARA99","F0000","T0000","J0000",
                                          "O1000","W1000","V0000_S0000")] ) 
  par(mfrow=c(2,4) )
  # cuanto se parece ese area al de francesco,
  boxplot(theData$HA_FINAL / theData$TotalArea ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="ARABLE_KG / KG_TOT",main=paste0("Type: " , "; Comp:  ",";  Tot Rate ARA: " , "%") )
  #pues hay notables diferencias
  
  # veamos comparaciones, para hacernos idea de como se distibuye por provincias  
  boxplot(theData$ARA / theData$TotalArea ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="ARABLE_KG / KG_TOT",main=paste0("Type: " , "; Comp:  ",";  Tot Rate ARA: " , "%") )
  
  
  # ARA, esta deberia incluir a todas las otras, menos grass, y fruits
  rate <-  round( 100*sum(theData$ARA) / sum(theData$TotalArea ) , 1)
  boxplot(theData$ARA / theData$TotalArea ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="ARABLE_KG / KG_TOT",main = paste0("Crop: " , "C0000",  "; rate area:  ", rate, "%") )
  
  # Cereales respecto a total
  rate <-  round( 100*sum(theData$C0000) / sum(theData$TotalArea ) , 1)
  boxplot(theData$C0000 / theData$TotalArea ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="C0000 / Total",main = paste0("Crop: " , "C0000",  "; rate area:  ", rate, "%") )
  
  
  
  
}


# En esta descriptiva analizamos como se distribuye por paises el area agricola
# theData <- Datos_SupAgr_Pest   # esos datos ya son para un unico principio activo
# ThePA <- id_selec_Pest
# TG <- TC
# ThePA <- id_selec_Pest
# provTable <- refProvTable   
Summary_land_PA <- function( theData, TG, ThePA,provTable ) {  
  
 
  
  ########### 
  #### LAND RELATED THE PA
  
  TheMinText <- paste0(TG," ", ThePA)
  
  # 1. sumario del numero de provincias en cada pais, en las que se emplea dicho principio activo
  # con esto podemos saber si en alguna provincia no hay consumo
  table(theData$COUNTRY)
  RefProv <-rbind(provTable, table(theData$COUNTRY))
  rownames(RefProv) <- c("Tot Prov","PA Prov")
  print(RefProv)
  
  #Plot enfrentando kg y area de cada tipo
  par(mfrow=c(2,4) )
  # RICE  .   C2000: RIZE
  plot(theData$RICE_KG, theData$C2000,col=theData$COUNTRY,pch=20, main=TheMinText )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  # GRASS      J0000: Permanent grassland    # G1000	Temporary grasses and grazings
  plot(theData$GRASS_KG, theData$J0000,col=theData$COUNTRY,pch=20, main=TheMinText )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  plot(theData$GRASS_KG, theData$G1000,col=theData$COUNTRY,pch=20, main=TheMinText )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  # OLIV             O1000	Olives
  plot(theData$OLIVES_KG, theData$O1000,col=theData$COUNTRY,pch=20, main=TheMinText )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  # VINES      W1000	Grapes
  plot(theData$VINES_KG, theData$W1000,col=theData$COUNTRY,pch=20, main=TheMinText )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  # FRUIT_KG    F0000	Fruits, berries and nuts (excluding citrus fruits, grapes and strawberries)
    #  T0000	Citrus fruits
  plot(theData$FRUIT_KG, theData$F0000,col=theData$COUNTRY,pch=20, main=TheMinText )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  plot(theData$FRUIT_KG, theData$T0000,col=theData$COUNTRY,pch=20, main=TheMinText )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  #separo el grupo de arable
  #Plot enfrentando kg y area de cada tipo
  par(mfrow=c(2,3) )
  # "ARABLE_KG     ARA	Arable land
  plot(theData$ARABLE_KG, theData$ARA,col=theData$COUNTRY,pch=20, main=TheMinText )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  # C0000	Cereals for the production of grain (including seed)
  plot(theData$ARABLE_KG, theData$C0000,col=theData$COUNTRY,pch=20, main= paste0(TheMinText," Grain Cereals") )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  # G0000 : Plants harvested  green 
  plot(theData$ARABLE_KG, theData$G0000,col=theData$COUNTRY,pch=20, main=paste0(TheMinText," Plants harvested  green ") )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  # E0000 : 
  plot(theData$ARABLE_KG, theData$E0000,col=theData$COUNTRY,pch=20, main=paste0(TheMinText," Plants harvested  green ") )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  plot(theData$G0000, theData$E0000,col=theData$COUNTRY,pch=20, main=paste0(TheMinText," Plants harvested  green ") )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  plot(theData$I0000, theData$E0000,col=theData$COUNTRY,pch=20, main=paste0(TheMinText," Plants harvested  green ") )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  # UN ANALISIS GENERAL SOBRE LAS AREAS DE CULTIVO SE PODRIA HACER PARA VER QUE IMPORTANCIA TIENE 
  # EL AREA DEDICADA A CADA COSA EN CADA PAIS . eSTE ANALISIS SE PUEDE HACER UNA UNICA VEZ

  # names(theData)
  theData$HA_FINAL
 

  
               # "PECR"     da problemas, solo datos para DE
 
  # dudas:  
     # HA_FINAL: resulta ser mucho mayor que ARA, incluso sumando todos los cultivos principales
     # o falta incluir aqui  y forest y urban??
     # ARA:  Incluye solo los cultivos? (pero no cuadra) o trambien grass, fruits,
     # con todo es inferior 
  
  
  
  
  
class(theData$C0000)
  head(theData$ARA)
head( rowSums( theData[,c("ARA","C0000")] ) )

  par(mfrow=c(2,4) )
  boxplot( theData$ARA ~ theData$COUNTRY, cex.main=0.9,
          xlab="country", ylab="ARABLE_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate ARA: ",totrateARA , "%") )
  
  # QUIZAS LA SUMA DE CADA UNA DE LAS CATEGORIAS POR PAISES
  
  totrateARA <- round(sum(theData$ARABLE_KG)/sum(theData$KG_FINAL)*100,1)
  totrateRIZE <- round(sum(theData$RICE_KG)/sum(theData$KG_FINAL)*100,1)
  totrateFRUT <- round(sum(theData$FRUIT_KG)/sum(theData$KG_FINAL)*100,1)
  totrateGRASS <- round(sum(theData$GRASS_KG)/sum(theData$KG_FINAL)*100,1)
  totrateOLIV <- round(sum(theData$OLIVES_KG)/sum(theData$KG_FINAL)*100,1)
  totrateVINES <- round(sum(theData$VINES_KG)/sum(theData$KG_FINAL)*100,1)
  
  
  # Por provincias el total de KG final deberia ser (al menos menor que el de esos grupos sumados)
  Prov_Tot <- aggregate(theData$KG_FINAL, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Ara <- aggregate(theData$ARABLE_KG, by=list(Category=theData$NUTS3), FUN=sum)
  
  
  
  
} 
  

#  TheShapes <- TotalShapes
#  theData <-  DF_total   Pest_Row_dat_regre
#  TG <- TG
#  ThePA <- id_selec_Pest
#  TheVIndex <- theVar  
#  paleta <- palet
#  style <- styl
#  nclr <- ncl
#  Co <- Co_list
#  VME <- Range
Map_var <- function(TheShapes, theData, TG, ThePA, TheVIndex, paleta, style, nclr, Co,VME ) { 
        
  theData[theData$NUTS3=="AT111",]$Arab_KG
    # 1. subseting del shape para tener solo el shape de donde hay compuesto     
      ShapesToPlot_comp <- Subsetingshapes(TheShapes,theData) 
      # VERIFICAMOS QUE TODO LO QUE ESTE EN EL MODELO ESTE EN EL MAPA Y VICEVERSA
      ShapesToPlot_comp[[1]]@data[!(ShapesToPlot_comp[[1]]@data$NUTS_ID %in% theData$NUTS3),]  
      # podria haber alguna region con modelo que ELIMINE A PRIORI DEL MAPA (CANARIAS)
      theData[!(theData$NUTS3 %in% ShapesToPlot_comp[[1]]@data$NUTS_ID),]
      nrow(ShapesToPlot_comp[[1]]@data)
      nrow(theData)
          
          # theData2 <- theData[(theData$NUTS3 %in% ShapesToPlot_comp[[1]]@data$NUTS_ID),]
          # alguna row que esta en mapa y no en modelo pesticidas(en espana ceuta y melilla)
   #       nrow(  ShapesToPlot_comp[[1]]@data[!(ShapesToPlot_comp[[1]]@data$NUTS_ID %in% theData2$NUTS3),]  )
          # alguna esta en modelo y no en el mapa (Canarias) 
  #        theData2[!(theData2$NUTS3 %in% ShapesToPlot_comp[[1]]@data$NUTS_ID),]
          
          names(theData)
    # subseting de pais
  #    if(Co[1] != "ALL") { theData <- theData[theData$COUNTRY %in% Co ,]  }
          
    # 2. Añadir el valor de la variable al data del shape.      #lo mezclo con el shape                                        "hydroid"       "Row.names"
      IndexMerge <- which( names(theData) %in% c("NUTS3",TheVIndex))  # los indices para el merge
      MyDFRsub1 <- merge(ShapesToPlot_comp[[1]]@data, theData[,IndexMerge], by.x = "NUTS_ID", by.y = "NUTS3") #los esfuerzos by rownames. #junto los datos del espatialdataframe, con mi data frame de concentracion
      nrow(MyDFRsub1)
      match(MyDFRsub1[,1],ShapesToPlot_comp[[1]]@data[,1])
      #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
      MyDFRsub1 <- MyDFRsub1[order(match(MyDFRsub1[,1],ShapesToPlot_comp[[1]]@data[,1])),]  #reordena en el mismo orden que en el objeto original
      MyPoligonsRsub1 <- ShapesToPlot_comp[[1]] #sobre una copia el original
      MyPoligonsRsub1@data <- MyDFRsub1  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 
      nrow( MyPoligonsRsub1@data)
          
      names(MyPoligonsRsub1@data)
      index <- which( names(MyPoligonsRsub1@data) == TheVIndex)    
      
      if(!(is.null(VME)) )  {
        MyPoligonsRsub1@data[MyPoligonsRsub1@data[,index] > VME[2],index] <- VME[2]   #limitar el valor maximo a pintar
        MyPoligonsRsub1@data[MyPoligonsRsub1@data[,index] < VME[1],index] <- VME[1]   # limitar valores minimos
      } 
      
      
          #  MyPoligonsRsub1@data[substr(MyPoligonsRsub1@data$NUTS_ID, start = 1, stop = 2) =="ES",]
          #  MyPoligonsRsub1@data[MyPoligonsRsub1@data$KG_FINAL>200000,] 
          
          #  class(ShapesToPlot[[1]]@data)
          #le anado una columna con un valor para el plot (generado aleatoriamente)
          #  ShapesToPlot[[1]]@data$color <- runif( nrow(ShapesToPlot[[1]]@data), min=0, max=10)
    
          maptex <- paste(TC, "   ",id_selec_Pest, "  ",TheVIndex)
    #      paleta <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
    #      style <- "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
    #      nclr <-7
          allvarsvalues <- unlist(MyPoligonsRsub1@data[,index:index])   #selecciono solo una columna a la vez
          min(allvarsvalues)
          max(allvarsvalues)  # atencion que deja en blanco el valor mas alto de la escala 323757.85
          
          breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
          #para que el ultimo intervalo no de problemas, le aumento algo al mayor valor 
          breaks.qt$brks[length(breaks.qt$brks)]<-breaks.qt$brks[length(breaks.qt$brks)]+breaks.qt$brks[length(breaks.qt$brks)]/100
          
          
          plotclr.palette <- brewer.pal(n =nclr, name = paleta)
   
          print(   spplot(MyPoligonsRsub1[,index],  main = maptex , 
                          sub = paste0(style," breaks"),
                          col = "transparent",
                          colorkey=list(space="bottom"),
                          col.regions=plotclr.palette,
                          at = breaks.qt$brks ) 
          )
          
          
}



# funcion para validar con los datos TOTALES de uk
###############################################################333
###### VALIDATION  CON UK ###############################33
###############################################################
# Prediction <- TheMap   # los datos de todos los cultivos. Tambien los paises donde se realizara la prediccion, com UK
# UK_val <- UK_valid    # datos uk for validation
Validation_UK <- function(UK_val,Prediction ){
  
      str(UK_val)
      unique(UK_val$nutscode)
      
      # En el mapa de crops es el que marca la resolucion de la prediccion (provincias)
    #  unique(D_cropt[ substr(D_cropt$NUTS_I, 1, 2) =="UK"  ,]$NUTS_ID)
      
      # Y nuestros datos de validacion son regionales
      unique(UK_val$nutscode)
      
      # La prediccion esta a resolucion mapa (provincial)
      names(Prediction)
      TheMap_UKpred <-  Prediction[substr(Prediction$NUTS3,1,2)== "UK" , ]
      # Podria dibujarlos a esta resolucion  y comparar con el plot a la otra resolucion
      #Prediccion por REGIONES (los agrego por regiones grandes) 
      names(TheMap_UKpred)
      TheMap_UKpred$Region <-  substr(TheMap_UKpred$NUTS3,1,3)
      
      TheMap_UKpredReg <- aggregate(TheMap_UKpred$KG_TOT, by=list(Category=TheMap_UKpred$Region), FUN=sum)
      
      #CARGAMOS EL VALOR REAL  #El valor real
      head(UK_val)
      unique(UK_val$nutscode)
      
      UK_valid_P <- UK_val[ UK_val$as_id == id_selec_Pest ,]
      
      
      # MERGE, REAL Y PREDICHO
      
      UKRealPredic <- merge(x = UK_valid_P, y = TheMap_UKpredReg, by.x = "nutscode", by.y="Category", all.y = TRUE)  #left joint :quiero solo que se queden datos de cuando hay pesticidas
      
      namesAS <- Datos_categ[Datos_categ$ActiveSubstances_ID==id_selec_Pest, ]$ActiveSubstance_name
      
      par(cex=1)
      plot(UKRealPredic$totkg, UKRealPredic$x ,xlab="Real" ,ylab="Predicted",main=namesAS,pch = 20)
      par(cex=0.7)
      with(UKRealPredic, text(x~totkg, labels = UKRealPredic$nutscode , pos = 4))
      abline(a=0,b=1) 
      
      
    # coef for validation  (REAL - PREDICHO)
      UKRealPredic2 <- UKRealPredic[complete.cases(UKRealPredic), ]
     # UKRealPredic3 <- UKRealPredic2[UKRealPredic2$nutscode %in% c("UKC","UKD","UKE","UKG","UKL", "UKM","UKN" ),  ]
      
      # R-squared (Coefficient of determination) represents the coefficient of how well the values fit compared to the original values. The value from 0 to 1 interpreted as percentages. The higher the value is, the better the model is.
      d.tot = UKRealPredic2$totkg - UKRealPredic2$x
      R2_mio = 1-(sum((d.tot)^2)/sum( (UKRealPredic2$totkg-mean(UKRealPredic2$totkg))^2 ))
      
    #  R2.const <-  rsq(UKRealPredic2$totkg , UKRealPredic2$x)
      


      # actual <- c(1, 2, 3, 5, 6, 7)
      # predicted <- c(1, 2, 3, 5, 6, 8000)
      # 
      # actual <- c(10, 20, 30, 50, 60,70)
      # predicted <- c(10, 20, 30, 50, 60,800 )
      # 
      # actual <- c(10, 20, 30, 50, 60, 70,6)
      # predicted <- c(10, 20, 30, 50, 60, 90,6)
      # 
      # 
      # rrse(actual, predicted)
      # rse(actual, predicted )
      # rae(actual, predicted)
      
  #    Relative_Absolute_Error <- rae(UKRealPredic2$totkg,UKRealPredic2$x)
      # mape computes the average absolute percent difference between two numeric vectors.
  #    ThemMase <-  round(mase(UKRealPredic2$totkg,UKRealPredic2$x,1), digits=3)
      
      # NRMSE :normalized    rmse Root Mean Squared Error /sd(estimate)
      nrmse <- round( rmse(UKRealPredic2$totkg,UKRealPredic2$x)/sd(UKRealPredic2$totkg), digits=3)  
      
      TheSMAPE <- smape(UKRealPredic2$totkg,UKRealPredic2$x)

      # NMAE' for the normalized MAE    MAE / sd(estimate)       mae Mean Absolute Error / sd(estimate)
      nmae <- round( mae(UKRealPredic2$totkg,UKRealPredic2$x)/sd(UKRealPredic2$totkg), digits=3)      
      

      
      return(c(nrmse,TheSMAPE,nmae,R2_mio))
  
}

rsq <- function (x, y) cor(x, y) ^ 2

# path1 <- path_out 
# TC1 <- TC
# id1 <- id_selec_Pest
# Land <- lVarLand
# Type <- "ARABLE_KG"
# ClVars <- AditionVars
# result <- res   
# coef_lm <-  ouput[[1]]
writesummary <- function(path1,TC1,id1,Type,Land,ClVars,Min_C,Max_C,Aver_C,RegType,coef_lm,coef_lm_const,result ){
  
  ThePath<- paste(path1,"Prediccion",TC1,id1,Type,sep="/")
  theFile <- paste(ThePath,"config.txt", sep= "/")
  fileConn <- file(theFile)

    line1 <- c("VarLand:   ",toString(Land))
    line2 <- c("Clima:   ",toString(ClVars))
    line3 <- c("MinCountry:   ",toString(Min_C))
    line4 <- c("MaxCountry:   ",toString(Max_C))
    line5 <- c("AverCountry:   ",toString(Aver_C))
    line6 <- c("type of Regresion:   ",toString(RegType))
    line7 <- c("coef linear model:   ",coef_lm$coefficients)
    line9 <- c("coef linear constr model:   ",coef_lm_const$coefficients)
    line8text <- c("Calidad Ajuste:  NRMSE1  NRMSE2  NRMSE3   NRMSE4  SMAPE1    SMAPE2   SMAPE3   SMAPE4   NMAE1   NMAE2  NMAE3   NMAE4  ")
    line8 <- c("Calidad Ajuste:   ",toString(result))
    writeLines(c( line3, line4, line5, line6, line1,line2,line7,line9,line8text,line8), fileConn)

  
  close(fileConn)
}




#######################################################
Myxyplot <- function(Z, MyV, NameY1, MyXlab = "", MyYlab="") {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))
  
  
  library(mgcv)
  library(lattice)
  
  P <- xyplot(AllY ~ AllX|factor(AllID), col = 1,
              xlab = list(MyXlab, cex = 1.5),
              #ylab = list("Response variable", cex = 1.5),
              #ylab = list("Pearson residuals", cex = 1.5),
              ylab = list(MyYlab, cex = 1.5),
              #layout = c(2,2),   #Modify
              strip = function(bg='white', ...)
                strip.default(bg='white', ...),
              scales = list(alternating = TRUE,
                            x = list(relation = "free"),
                            y = list(relation = "same")),
              panel=function(x, y){
                panel.grid(h=-1, v= 2)
                panel.points(x, y, col = 1)
                panel.loess(x, y, span = 0.8,col = 1, lwd = 2)
              })
  
  print(P)
}
#######################################################




# path <- path_data
# theData <- Pest_Row_dat
# ThePA <- id_selec_Pest3    #todos las AS de la subclase
# TG <- TC
# funcion para hacer heatmaps de los 10 AS mas importantes de cada categoria
descrip_heatmap_pest <- function( theData, TG, ThePA) {  
  
  TheMinText <- paste0(TG," ", ThePA)
  
  #solo algunos AS de la categoria
  library(data.table)
  library(gplots)
  theDataSub <- theData[theData$ID_PA %in%   ThePA[c(1:102)], ]  # arable:  FU c(1:38,40:46,48:49,51:56,58:69)]   IN  c(1:25,27,29,30:33,35,36,37,39,40,44 )
  theDataSub$ID_PA  <- factor(theDataSub$ID_PA )
  unique(theDataSub$ID_PA)
  
  table(theDataSub$COUNTRY,theDataSub$ID_PA)
  names(theDataSub)
  
  
  # KG totales por pais
  Kg_by_CO_AS <- aggregate(theDataSub$ARABLE_KG, by=list(Ctry=theDataSub$COUNTRY,AS=theDataSub$ID_PA), FUN=sum)
  Kg_by_CO_AS[sapply(Kg_by_CO_AS, is.infinite)] <- 0
  boxplot(Kg_by_CO_AS$x)
   #  lim=100
   #   Kg_by_CO_AS$x[which(Kg_by_CO_AS$x > lim)] = lim
  thematrix <- dcast(Kg_by_CO_AS, formula = ...  ~ Ctry ) 
  rownames(thematrix)<-thematrix[,1]
  thematrix$AS <- NULL 
  thematrix <- as.matrix(thematrix)
  heatmap.2(thematrix, trace="none",scale = "none",             
            col = colorRampPalette(c("yellow","orange","cyan","blue","darkblue"))(40)  ,
            cexRow =0.7, lhei = c(3,15),
            main = "Kg_ARAB_HB__scale_none" ,  Rowv=NA , Colv=NA )     # 
  
 
  sum(is.infinite(thematrix))
  
  # ratios totales por pais
  names(theDataSub)
 # theDataSub[theDataSub$COUNTRY=="IT" & theData$ID_PA==1925, ]
  Kg_by_CO_AS <- aggregate(theDataSub$Ratio, by=list(Ctry=theDataSub$COUNTRY,AS=theDataSub$ID_PA), FUN=sum)
  Kg_by_CO_AS[sapply(Kg_by_CO_AS, is.infinite)] <- 0  #si faltan datos y genera infinito, los convierto en NA
  
  thematrix <- dcast(Kg_by_CO_AS, formula = ...  ~ Ctry ) 
  rownames(thematrix)<-thematrix[,1]
  thematrix$AS <- NULL 
  thematrix <- as.matrix(thematrix)
  heatmap.2(thematrix, trace="none",scale = "row",              
            col = colorRampPalette(c("white","darkblue"))(20)  ,
            cexRow =0.7, lhei = c(3,15),
            main = "ratio_scale_tow"   )
  
  ## GENERO UN FICHERO CON CUAL SERA EL PAIS CON EL MAXIMO PARA CADA AS
      DF <- as.data.frame(thematrix)    #respecto a la matrix de ratio 
      rownames(DF) <- DF$AS
      DF$AS <- NULL
      DF$MaxCo <-  colnames(DF)[apply(DF,1,which.max)]  # busca el pais que tiene el maximo en cada fila
      DF$MinCo <-  colnames(DF)[apply(DF,1,which.min)] 
      file <- paste0(path,"/MinMaxCo_ArableHB2.csv")
      write.csv(DF[,c("MinCo","MaxCo")] , file =file )

    
      
      
      # KG totales por pais
      Kg_by_CO_AS <- aggregate(theDataSub$GRASS_KG  , by=list(Ctry=theDataSub$COUNTRY,AS=theDataSub$ID_PA), FUN=sum)
      Kg_by_CO_AS[sapply(Kg_by_CO_AS, is.infinite)] <- 0
      thematrix <- dcast(Kg_by_CO_AS, formula = ...  ~ Ctry ) 
      rownames(thematrix)<-thematrix[,1]
      thematrix$AS <- NULL 
      thematrix <- as.matrix(thematrix)
      heatmap.2(thematrix, trace="none",scale = "column",             
                col = colorRampPalette(c("white","darkblue"))(20)  ,
                cexRow =0.7, lhei = c(3,15),
                main = "KgTotal_scale_column" )
      
      
      
      
  DF_temp <- mydata[,c("BAS_COUNT","Year",colnames(mydata[indvar])) ]
  DF_temp$tot_events[DF_temp$tot_events>10]<-10  #si los valores son mayores de 10 los convierto a 10 para mejorar la escala
  thematrix <- dcast(DF_temp, formula = ...  ~ Year ) 
  rownames(thematrix) <- thematrix[,1] #doy nombre a las filas
  thematrix$BAS_COUNT <- NULL  #elimino la columna de nombres
  thematrix <- as.matrix(thematrix) 
  heatmap.2(thematrix, trace="none",scale = "none", 
            col = colorRampPalette(c("white","darkblue"))(20)  ,
            cexRow =mycex,Colv=FALSE, lhei = c(3,15),
            main = colnames(mydata[indvar]) )
  
  
  
  
  
  
  
  
  # 1. sumario del numero de provincias en cada pais, en las que se emplea dicho principio activo
  # con esto podemos saber si en alguna provincia no hay consumo
  table(theData$COUNTRY)

 rownames(RefProv) <- c("Tot Prov","PA Prov")
  print(RefProv)
  
  
  # 2. Plot de KG frente a Ha Totales (a mas pendiente ese compuesto se aplica en mayor ratio)
  plot(theData$HA_FINAL,theData$KG_FINAL,col=theData$COUNTRY,pch=20, main=TheMinText )
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  # 3. Plot ratio de KG frente a Ha. Si es constante en un mismo pais indica que ha sido desagregado 
  plot(theData$Ratio, col=theData$COUNTRY,pch=20, main=TheMinText ) 
  legend("topright", legend=unique(theData$COUNTRY),col=unique(theData$COUNTRY),pch=20)
  
  # KG totales por pais
  Kg_by_CO <- aggregate(theData$KG_FINAL, by=list(Category=theData$COUNTRY), FUN=sum)
  rownames(Kg_by_CO) <- Kg_by_CO$Category 
  Kg_by_CO$Category <- NULL
  barplot(t(as.matrix(Kg_by_CO)), ylab="Total Kg by country")
  
  Kg_by_CO$rate <- Kg_by_CO$x/sum(Kg_by_CO$x)*100
  
  firstCo <- Kg_by_CO[order(-Kg_by_CO$rate),][1,]   #saco el pais con su ratio relativo aplicaion
  ncount <- nrow(Kg_by_CO)
  
  # 4. Proporcion  principio activo y pais, especificamente por cierto tipo de cultivos
  totrateARA <- round(sum(theData$ARABLE_KG)/sum(theData$KG_FINAL)*100,1)
  totrateRIZE <- round(sum(theData$RICE_KG)/sum(theData$KG_FINAL)*100,1)
  totrateFRUT <- round(sum(theData$FRUIT_KG)/sum(theData$KG_FINAL)*100,1)
  totrateGRASS <- round(sum(theData$GRASS_KG)/sum(theData$KG_FINAL)*100,1)
  totrateOLIV <- round(sum(theData$OLIVES_KG)/sum(theData$KG_FINAL)*100,1)
  totrateVINES <- round(sum(theData$VINES_KG)/sum(theData$KG_FINAL)*100,1)
  totrateVINES <- round(sum(theData$  VINES_KG)/sum(theData$KG_FINAL)*100,1)
  
  # Por provincias el total de KG final deberia ser (al menos menor que el de esos grupos sumados)
  Prov_Tot <- aggregate(theData$KG_FINAL, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Ara <- aggregate(theData$ARABLE_KG, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Rize <- aggregate(theData$RICE_KG, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Fru <- aggregate(theData$FRUIT_KG, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Gras <- aggregate(theData$GRASS_KG, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Oliv <- aggregate(theData$OLIVES_KG, by=list(Category=theData$NUTS3), FUN=sum)
  Prov_Vine <- aggregate(theData$VINES_KG, by=list(Category=theData$NUTS3), FUN=sum)
  
  Total_Prov <- Reduce(function(x, y) merge(x, y, by="Category", all=TRUE), list(Prov_Tot, Prov_Ara, Prov_Rize, Prov_Fru,Prov_Gras,Prov_Oliv,Prov_Vine))
  names(Total_Prov)<-c("NUTS3","FINAL","ARABLE","RICE","FRUIT","GRASS","OLIVES","VINES"  )
  # convierto a ratios
  Total_Prov$ARABLE <- Total_Prov$ARABLE / Total_Prov$FINAL
  Total_Prov$RICE <- Total_Prov$RICE / Total_Prov$FINAL
  Total_Prov$FRUIT <- Total_Prov$FRUIT / Total_Prov$FINAL
  Total_Prov$GRASS <- Total_Prov$GRASS / Total_Prov$FINAL
  Total_Prov$OLIVES <- Total_Prov$OLIVES / Total_Prov$FINAL
  Total_Prov$VINES <- Total_Prov$VINES / Total_Prov$FINAL
  
  
  # mezclo el campo pais
  theData2 <- merge(x = theData, y = Total_Prov, by = c("NUTS3","NUTS3")  )
  
  par(mfrow=c(2,3) )
  boxplot(theData2$ARABLE ~ theData2$COUNTRY, cex.main=0.9,
          xlab="country", ylab="ARABLE_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate ARA: ",totrateARA , "%") )
  
  boxplot(theData2$FRUIT ~ theData2$COUNTRY, cex.main=0.9,
          xlab="country", ylab="FRUIT_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";   Tot Rate FRUT: ",totrateFRUT , "%") )
  
  boxplot(theData2$OLIVES ~ theData2$COUNTRY, cex.main=0.9,
          xlab="country", ylab="OLIVES_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";   Tot Rate OLIV: ",totrateOLIV , "%") )
  
  boxplot(theData2$VINES  ~ theData2$COUNTRY, cex.main=0.9,
          xlab="country", ylab="VINES_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate VINES: ",totrateVINES , "%") )
  
  boxplot(theData2$GRASS ~ theData2$COUNTRY, cex.main=0.9,
          xlab="country", ylab="GRASS_KG /Ara_ha",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";   Tot Rate GRASS: ",totrateGRASS , "%") )
  
  boxplot(theData2$RICE ~ theData2$COUNTRY, cex.main=0.9,
          xlab="country", ylab="RICE_KG / KG_TOT",main=paste0("Type: " ,TG, "; Comp:  ",id_selec_Pest,";  Tot Rate RICE: ",totrateRIZE , "%") )
  
  
  return(list(totrateARA, firstCo,ncount))
  
}

