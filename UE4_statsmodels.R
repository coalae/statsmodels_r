# AUTOR: Cordula Eggerth

# UE 4

# Sicherstellen, dass keine alten Objekte im File sind:
rm(list=ls())

# Libraries installieren
install.packages("xlsx")
install.packages("dplyr")
install.packages("tidyr")
install.packages("corrplot")
install.packages("car")
install.packages("MASS")

# Libraries laden
library(xlsx)
library(dplyr)
library(tidyr)
library(corrplot)
library(car)
library(lattice)
library(MASS)


#########################################################################################################
### DATA.FRAME mit allen relevanten Informationen anlegen ###
#########################################################################################################

# Work Directory setzen 
setwd("C:/Users/cordu/Desktop/UE4")

# Daten aus den CSV-Files einlesen
adressen <- read.csv("Adresse.csv", sep=";", dec=",", header=TRUE) # spalten: PLZ, Stadt 
benutzer <- read.csv("Benutzer.csv", sep=";", dec=",", header=TRUE) # spalten: UID, PLZ
features <- read.csv("Features.csv", sep=";", dec=",", header=TRUE, stringsAsFactors = FALSE) 
                                                     # spalten: UID, Datum, variable, value

# Data.Frame anlegen und Daten zusammenfuehren

 # Join von benutzer und adressen auf "PLZ" 
  joined_benutzer_adressen <- benutzer %>% inner_join(adressen, by=c("PLZ"))  
  
 # Join des obigen objekts mit features auf "UID" 
  joined_all <- joined_benutzer_adressen %>% inner_join(features, by=c("UID"))
  
 # joined_all ist das zusammengelegte data.frame, das alle informationen enthaelt
  is.data.frame(joined_all)
  joined_all


#########################################################################################################
### AUFGABE 1 ###
#########################################################################################################

# 1.a. Werten Sie alle in den Daten vorkommenden Features (Feature 1 bis Feature 6) deskriptiv aus.
#      Bitte beachten Sie, dass sich mit den Städtenamen (Tabelle Adresse) und den Datumsangaben 6 verschiedene 
#      Gruppen bilden. 
#      Berechnen Sie den Mittelwert, den Median, die Standardabweichung, den minimalen und maximalen Wert sowie 
#      die 25% und 75% Quantile. 
#      Erstellen Sie für jedes der Features je eine Grafik mit 6 Histogrammen für die Gruppen (horizontal die 3 
#      Städte, vertikal das Datum).
# 1.b. Gibt es fehlende Werte in dem Datensatz? Wenn ja, wie viele? Können Sie diese Daten imputieren? 
#      Falls Sie die Daten nicht imputieren können, dann entfernen Sie unvollständige Datensätze aus dem 
#      Analysebestand.
# 1.c. Visualisieren Sie die Korrelationsmatrizen.
  
  
  # Daten in Feature 1 bis 6 trennen und anzahl der beobachtungen ueberpruefen
  
    daten_feature_1 <- joined_all %>% filter(variable == "Feature 1") 
    nrow(daten_feature_1) # 299 beobachtungen
    
    daten_feature_2 <- joined_all %>% filter(variable == "Feature 2") 
    nrow(daten_feature_2) # 300 beobachtungen
    
    daten_feature_3 <- joined_all %>% filter(variable == "Feature 3") 
    nrow(daten_feature_3) # 299 beobachtungen    
  
    daten_feature_4 <- joined_all %>% filter(variable == "Feature 4") 
    nrow(daten_feature_4) # 300 beobachtungen
    
    daten_feature_5 <- joined_all %>% filter(variable == "Feature 5") 
    nrow(daten_feature_5) # 300 beobachtungen
    
    daten_feature_6 <- joined_all %>% filter(variable == "Feature 6") 
    nrow(daten_feature_6) # 300 beobachtungen
    
    anzahl_UID <- nrow(benutzer) # 150 benutzer
    
    
    
  # fuer jeden benutzer (UID) wurden scheinbar 2 beobachtungen pro feature gemacht
  # es fehlt aber bei feature 3 eine beobachtung und bei feature 1 eine beobachtung, 
  # damit es tatsaechlich 2 beobachtungen pro feature waeren pro benutzer (UID)
  # die werte fuer den betroffenen benutzer (UID) kann man bis auf den value herausfinden.
  # fuer den value koennte man eine schaetzung machen. es gibt hier aber keine vorgabe, 
  # wie man die schaetzung durchfuehren sollte.
    
  # finde UID, deren beobachtungen unvollstaendig sind
    # bezueglich feature 1
    unvollstaendige_UID <- "a"
    
    for (i in 1:anzahl_UID) {
      if(sum(daten_feature_1$UID==benutzer$UID[i]) < 2){
        unvollstaendige_UID <- benutzer$UID[i]
        break
      }
    }
    
    sum(daten_feature_1$UID==30) # tatsaechlich gibt es nur 1 beobachtung der UID 30 in feature 1
    
    # bezueglich feature 3
    unvollstaendige_UID_f3 <- "a"
    
    for (i in 1:anzahl_UID) {
      if(sum(daten_feature_3$UID==benutzer$UID[i]) < 2){
        unvollstaendige_UID_f3 <- benutzer$UID[i]
        break
      }
    }
    
    sum(daten_feature_3$UID==130) # tatsaechlich gibt es nur 1 beobachtung der UID 130 in feature 3
    

    # versuch der imputation (ANNAHME: mit wert des medians der daten)
      # fuer UID 30 in den feature 1 daten
        daten_feature_1_impute <- daten_feature_1
        info_zu_imputieren_f1 <- daten_feature_1_impute[daten_feature_1_impute$UID == unvollstaendige_UID, ]
        laenge_f1 <- nrow(daten_feature_1_impute)
        value_zu_imputieren <- median(as.numeric(as.character(daten_feature_1_impute$value)))
        info_zu_imputieren_f1$Datum <- "20180416T00:00Z"
        info_zu_imputieren_f1$value <- value_zu_imputieren
        daten_feature_1_impute <- rbind(daten_feature_1_impute, info_zu_imputieren_f1)
        daten_feature_1_impute[daten_feature_1_impute$UID == unvollstaendige_UID, ]      
        rownames(daten_feature_1_impute)[300] <- "300"
        laenge_f1 <- nrow(daten_feature_1_impute)
        
      # fuer UID 130 in den feature 3 daten (ANNAHME: mit wert des medians der daten)
        daten_feature_3_impute <- daten_feature_3
        info_zu_imputieren_f3 <- daten_feature_3_impute[daten_feature_3_impute$UID == unvollstaendige_UID_f3, ]
        laenge_f3 <- nrow(daten_feature_3_impute)
        value_zu_imputieren_f3 <- median(as.numeric(as.character(daten_feature_3_impute$value)))
        info_zu_imputieren_f3$Datum <- "20160813T00:00Z"
        info_zu_imputieren_f3$value <- value_zu_imputieren_f3
        daten_feature_3_impute <- rbind(daten_feature_3_impute, info_zu_imputieren_f3)
        daten_feature_3_impute[daten_feature_3_impute$UID == unvollstaendige_UID_f3, ]      
        rownames(daten_feature_3_impute)[300] <- "300"
        laenge_f3 <- nrow(daten_feature_3_impute)
    
        daten_feature_3_impute[290:300,]
        
        
        
    # zur sicherheit, dass keine datenverfaelschungen stattfinden durch den imputationsversuch, 
    # werden die unvollstaendigen UIDs aber aus den daten herausgenommen
    # betroffene UIDs: 30 und 130
        
        liste_featuresdaten <- list(f1=daten_feature_1, f2=daten_feature_2, f3=daten_feature_3,
                                    f4=daten_feature_4, f5=daten_feature_5, f6=daten_feature_6)
        str(liste_featuresdaten)
        
        # listenelemente von den unvollstaendigen UIDs reinigen fuer alle features
        for(i in 1:length(liste_featuresdaten)){
          
          daten_feature_i <- liste_featuresdaten[[i]]
          daten_feature_i_cleaned <- daten_feature_i[(daten_feature_i$UID != unvollstaendige_UID), ]  
          daten_feature_i_cleaned <- daten_feature_i_cleaned[(daten_feature_i_cleaned$UID != unvollstaendige_UID_f3), ]  
          liste_featuresdaten[[i]] <- daten_feature_i_cleaned
        }
        
        # pruefe, ob alle listenelemente nun gleiche anzahl an beobachtungen haben 
        # (hier: 296 nach entfernung der 2 unvollstaendigen UIDs)
        for(i in 1:length(liste_featuresdaten)){
          print(nrow(liste_featuresdaten[[i]]))
        }
        
  ## ---------------------------------------------------------------------------------  
  ## GRUPPEN INNERHALB DER FEATURES BILDEN:
  ## ---------------------------------------------------------------------------------
        # G1: Graz - 20180416T00:00Z
        # G2: Graz - 20160813T00:00Z
        # G3: Salzburg - 20180416T00:00Z
        # G4: Salzburg - 20160813T00:00Z
        # G5: Wien - 20180416T00:00Z
        # G6: Wien - 20160813T00:00Z
        
        # FUNCTION: gruppen fuer ein als parameter uebergebenes feature-dataframe ermitteln
        getGruppenDaten <- function(feature){
          
          # liste fuer gruppendaten des derzeitigen features anlegen
          featuregesamt <- list()
          
          # gruppen bilden
          featuregesamt$G1 <- liste_featuresdaten[[i]] %>% filter(Stadt=="Graz" & Datum=="20180416T00:00Z")
          featuregesamt$G2 <- liste_featuresdaten[[i]] %>% filter(Stadt=="Graz" & Datum=="20160813T00:00Z")
          featuregesamt$G3 <- liste_featuresdaten[[i]] %>% filter(Stadt=="Salzburg" & Datum=="20180416T00:00Z")
          featuregesamt$G4 <- liste_featuresdaten[[i]] %>% filter(Stadt=="Salzburg" & Datum=="20160813T00:00Z")
          featuregesamt$G5 <- liste_featuresdaten[[i]] %>% filter(Stadt=="Wien" & Datum=="20180416T00:00Z")
          featuregesamt$G6 <- liste_featuresdaten[[i]] %>% filter(Stadt=="Wien" & Datum=="20160813T00:00Z")
      
          # RETURN: featuregesamt (d.h. alle 6 gruppen fuer das derzeitige feature)    
          featuregesamt
          
        }


        # DATEN FUER ALLE FEATURES ERMITTELN (GESAMTLISTE ALLER GRUPPEN FUER ALLE FEATURES)
        gruppendaten_gesamtliste <- list()
        
        for(i in 1:length(liste_featuresdaten)){
          
          gruppendaten_pro_feature <- getGruppenDaten(liste_featuresdaten[[i]])
          gruppendaten_gesamtliste[[i]] <- gruppendaten_pro_feature 
          
        }
        
        names(gruppendaten_gesamtliste) <- c("Feature 1", "Feature 2", "Feature 3", 
                                             "Feature 4", "Feature 5", "Feature 6")
        
    # GESAMTLISTE aller gruppierten daten ueber alle features hinweg
        # gesamtliste enthaelt 6 listen (d.h. 1 fuer jedes feature)
        # jedes feature enthaelt 6 dataframes (d.h. 1 fuer jede gruppe)
        str(gruppendaten_gesamtliste)
   
        
  ## ------------------------------------------------------------------------------
  ## DESKRIPTIVE AUSWERTUNG      
  ## ------------------------------------------------------------------------------
  
  # gesamtliste fuer ergebnisse der deskriptiven auswertung
  deskriptiveStatistik_gesamtliste <- list()      

  # FUNCTION: berechne deskriptive statistik fuer uebergebenes feature, 
  #           das 6 eingeteilten gruppen enthaelt
    berechneDeskriptiveStatistik <- function(feature){
    
      # dataframe fuer auswertungsergebnisse anlegen
      df_deskr <- data.frame(mean=numeric(),
                       median=numeric(),
                       sd=numeric(),
                       min=numeric(),
                       max=numeric(),
                       quantile25=numeric(),
                       quantile75=numeric(),
                       stringsAsFactors=FALSE)    
      
      
      # feature ist eine liste der 6 gruppen 
      # d.h. fuer jede gruppe ein dataframe mit auswertung berechnen
      for(i in 1:length(feature)){
        
        # deskriptive kennzahlen berechnen
        mean <- mean(feature[[i]]$value) # i-te gruppe
        median <- median(feature[[i]]$value) 
        sd <- sd(feature[[i]]$value) 
        min <- min(feature[[i]]$value) 
        max <- max(feature[[i]]$value) 
        quantiles <- quantile(feature[[i]]$value, probs=c(0.25,0.75))  
        
        vektor_gruppe <- c(mean, median, sd, min, max, quantiles[1], quantiles[2])
        
        # auswertung auf df_deskr zuweisen
        df_deskr <- rbind(df_deskr, vektor_gruppe)
        
      }
      
      # rownames (i.e. gruppennamen) setzen fuer df_deskr
      rownames(df_deskr) <- c("G1", "G2", "G3", "G4", "G5", "G6")
      # colnames (i.e. deskriptive kennzahlen) setzen fuer df_deskr
      colnames(df_deskr) <- c("mean", "median", "sd", "min", "max", "quantile25", "quantile75")
      
      # RETURN: df_deskr (i.e. dataframe mit den berechneten deskriptiven
      #         statistiken aller gruppen fuer das feature)
      df_deskr
        
    }
    
    
    # CHECKS ONLY (nur Feature 1): (fazit: berechnung der deskriptiven statistiken funktioniert)
      berechneDeskriptiveStatistik(gruppendaten_gesamtliste$`Feature 1`)
      mean(gruppendaten_gesamtliste$`Feature 1`$G1$value)
      median(gruppendaten_gesamtliste$`Feature 1`$G1$value)
      sd(gruppendaten_gesamtliste$`Feature 1`$G1$value)
      min(gruppendaten_gesamtliste$`Feature 1`$G1$value)
      max(gruppendaten_gesamtliste$`Feature 1`$G1$value)
      quantile(gruppendaten_gesamtliste$`Feature 1`$G1$value, probs=c(0.25,0.75))
      
    
    # berechnungsfunktion fuer gesamte featuresliste durchfuehren
    for(i in 1:length(gruppendaten_gesamtliste)){
      
      auswertungen_proFeature_alleGruppen <- berechneDeskriptiveStatistik(gruppendaten_gesamtliste[[i]])
      deskriptiveStatistik_gesamtliste[[i]] <- auswertungen_proFeature_alleGruppen 
      
    }
    
    # namen der listenelemente mit jeweiligem featurename setzen
    names(deskriptiveStatistik_gesamtliste) <- c("Feature 1", "Feature 2", "Feature 3", 
                                         "Feature 4", "Feature 5", "Feature 6")

    
    # fertige gesamtliste deskriptiver daten mit allen 6 gruppen pro feature
    deskriptiveStatistik_gesamtliste
    
             
  
    ## ---------------------------------------------------------------------------------
    ## VISUALISIERUNG (HISTOGRAMME FUER DIE 6 GRUPPEN)    
    ## ---------------------------------------------------------------------------------
    
    # FUNCTION zum erstellen der 6 subplots eines features (fixe achsen)
    # parameter: feature (i.e. daten pro feature aus gruppendaten_gesamtliste)
    erstelleGruppenplots <- function(feature, featurenumber){
      
      # plot-anzeige in 3x2 subplots teilen
      par(mfrow=c(2,3), oma=c(0,0,3,0))
      
      # histogramme fuer die jeweiligen gruppen des derzeitigen feature plotten
      # anordnung: horizontal staedte und vertikal datumswerte
      hist(feature$G1$value, breaks=seq(-160,250,l=15), col="darkseagreen1",
           main="Graz - 16.04.2018", xlab="value", ylab="frequency", ylim=c(0,30), xlim=c(-160,250))           

      hist(feature$G3$value, breaks=seq(-160,250,l=15), col="rosybrown2",
           main="Salzburg - 16.04.2018", xlab="value", ylab="frequency",ylim=c(0,30), xlim=c(-160,250))
      
      hist(feature$G5$value, breaks=seq(-160,250,l=15), col="lightblue3",
           main="Wien - 16.04.2018", xlab="value", ylab="frequency",ylim=c(0,30), xlim=c(-160,250))
      
      hist(feature$G2$value, breaks=seq(-160,250,l=15), col="navajowhite",
           main="Graz - 13.08.2016", xlab="value", ylab="frequency",ylim=c(0,30), xlim=c(-160,250))
      
      hist(feature$G4$value, breaks=seq(-160,250,l=15), col="lightsalmon",
           main="Salzburg - 13.08.2016", xlab="value", ylab="frequency",ylim=c(0,30), xlim=c(-160,250))
      
      hist(feature$G6$value, breaks=seq(-160,250,l=15), col="khaki",
           main="Wien - 13.08.2016", xlab="value", ylab="frequency",ylim=c(0,30), xlim=c(-160,250))
      
      title(paste("Feature",featurenumber), outer=TRUE, cex.main=1.2)
      
    }
    
    
    # plotfunktion "erstelleGruppenplots" (mit fixen achsenlängen) fuer gesamte featuresliste aufrufen
    for(i in 1:length(gruppendaten_gesamtliste)){
      
      x11()  
      erstelleGruppenplots(gruppendaten_gesamtliste[[i]], i)

    }

    
    
    
    # FUNCTION zum erstellen der 6 subplots eines features (an die daten angepasste x-achse)
    # parameter: feature (i.e. daten pro feature aus gruppendaten_gesamtliste)
    erstelleGruppenplots_adapted <- function(feature, featurenumber){
      
      # plot-anzeige in 3x2 subplots teilen
      par(mfrow=c(2,3), oma=c(0,0,3,0))
      
      # histogramme fuer die jeweiligen gruppen des derzeitigen feature plotten
      # anordnung: horizontal staedte und vertikal datumswerte
      hist(feature$G1$value, breaks=6, col="darkseagreen1",
           main="Graz - 16.04.2018", xlab="value", ylab="frequency", ylim=c(0,30))           
      
      hist(feature$G3$value, breaks=6, col="rosybrown2",
           main="Salzburg - 16.04.2018", xlab="value", ylab="frequency",ylim=c(0,30))
      
      hist(feature$G5$value, breaks=6, col="lightblue3",
           main="Wien - 16.04.2018", xlab="value", ylab="frequency",ylim=c(0,30))
      
      hist(feature$G2$value, breaks=6, col="navajowhite",
           main="Graz - 13.08.2016", xlab="value", ylab="frequency",ylim=c(0,30))
      
      hist(feature$G4$value, breaks=6, col="lightsalmon",
           main="Salzburg - 13.08.2016", xlab="value", ylab="frequency",ylim=c(0,30))
      
      hist(feature$G6$value, breaks=6, col="khaki",
           main="Wien - 13.08.2016", xlab="value", ylab="frequency",ylim=c(0,30))
      
      title(paste("Feature",featurenumber), outer=TRUE)
      
    }
    
    
    # plotfunktion "erstelleGruppenplots_adapted" (mit an die daten angepassten achsenlängen) 
    # fuer gesamte featuresliste aufrufen
    for(i in 1:length(gruppendaten_gesamtliste)){
      
      x11()  
      erstelleGruppenplots_adapted(gruppendaten_gesamtliste[[i]], i)
      
    }
    
    # par(mfrow=c(1,1))
        

    
  ## ---------------------------------------------------------------------------------
  ## VISUALISIERUNG (KORRELATIONSMATRIZEN)    
  ## ---------------------------------------------------------------------------------
       
   
  ### KORRELATIONSMATRIX BASIEREND AUF DEN VALUES DER FEATURES (i.e werte der variablen) 
    
    # basisdaten
      # dataframe fuer values der features 1 bis 6 anlegen (basierend auf daten nach
      # entfernen der unvollstaendigen beobachtungen), + datum und stadt
      # datum: 1: 20160813, 2: 20180416
      # stadt: 1: graz, 2: salzburg, 3: wien
      df_features <- data.frame(f1value=liste_featuresdaten$f1$value,
                             f2value=liste_featuresdaten$f2$value,
                             f3value=liste_featuresdaten$f3$value,
                             f4value=liste_featuresdaten$f4$value,
                             f5value=liste_featuresdaten$f5$value,
                             f6value=liste_featuresdaten$f6$value,
                             datum=liste_featuresdaten$f1$Datum,
                             stadt=liste_featuresdaten$f1$Stadt,
                             stringsAsFactors=FALSE) 
    
    str(df_features)
    df_features$datum <- as.numeric(as.factor(df_features$datum))
    df_features$stadt <- as.numeric(as.factor(df_features$stadt))
    

    # korrelationsmatrix berechnen
    korrelationsmatrix_feature_values <- cor(df_features)
    
    # korrelationsmatrix visualisieren (verschiedene typen von visiualisierung)
    # rot: negative korrelation
    # blau: positive korrelation
    
    # plot-anzeige in 2x2 subplots teilen
    
    x11()
    
    par(mfrow=c(2,2), oma=c(0,0,3,0))
    
    corrplot(korrelationsmatrix_feature_values, method="color")
    corrplot(korrelationsmatrix_feature_values, method="number")
    corrplot(korrelationsmatrix_feature_values, type="upper", order="hclust")
    corrplot(korrelationsmatrix_feature_values, method = "circle")
    
    
    title("Korrelationen von Features 1-6, Daten, Stadt", outer=TRUE)
        
     
    
  ### KORRELATIONSMATRIX BASIEREND AUF DEN GESAMTDATEN (joined_all)
    
    # basisdaten
    basisdaten <- joined_all
    str(basisdaten)
    
    # cols als factor und dann numeric umwandeln, damit man cor() anwenden kann
    basisdaten$Datum <- as.numeric(as.factor(basisdaten$Datum))
    basisdaten$variable <- as.numeric(as.factor(basisdaten$variable))
    basisdaten$Stadt <- as.numeric(as.factor(basisdaten$Stadt))
    
    basisdaten_ohneUID_ohnePLZ <- basisdaten[ , 3:ncol(basisdaten)]
    str(basisdaten_ohneUID_ohnePLZ)
    
    # korrelationsmatrix berechnen
    korrelationsmatrix <- cor(basisdaten_ohneUID_ohnePLZ)
    
    # korrelationsmatrix visualisieren (verschiedene typen von visiualisierung)
      # rot: negative korrelation
      # blau: positive korrelation
    corrplot(korrelationsmatrix, method="color")
    
    corrplot(korrelationsmatrix, method="number")
    
    corrplot(korrelationsmatrix, type="upper", order="hclust")
    
    
  ### KORRELATIONSMATRIX BASIEREND AUF DEN IN DIE JEWEILIGEN FEATURES GETRENNTEN DATEN
    
      # basisdaten
      f1 <- daten_feature_1
      f2 <- daten_feature_2
      f3 <- daten_feature_3
      f4 <- daten_feature_4
      f5 <- daten_feature_5
      f6 <- daten_feature_6
      features_corrBasis <- list(f1=f1, f2=f2, f3=f3, f4=f4, f5=f5, f6=f6)
      str(features_corrBasis)
      
      # FUNCTION: fuer plot der correlation matrizen pro feature
      plotCorrelationMatrix <- function(featuredaten, numberOfFeature){
        
        # cols als factor und dann numeric umwandeln, damit man cor() anwenden kann
        featuredaten$Datum <- as.numeric(as.factor(featuredaten$Datum))
        featuredaten$variable <- as.numeric(as.factor(featuredaten$variable))
        featuredaten$Stadt <- as.numeric(as.factor(featuredaten$Stadt))
        
        featuresdaten_ohneUID_ohnePLZ <- basisdaten[ , 3:ncol(featuredaten)]
        str(featuresdaten_ohneUID_ohnePLZ)
        
        # korrelationsmatrix berechnen
        korrelationsmatrix_feature <- cor(featuresdaten_ohneUID_ohnePLZ)
        
        # korrelationsmatrix visualisieren (verschiedene typen von visiualisierung)
        # rot: negative korrelation
        # blau: positive korrelation
        
        # plot-anzeige in 3x2 subplots teilen
        
        x11()
        
        par(mfrow=c(2,2), oma=c(0,0,3,0))
        
        corrplot(korrelationsmatrix_feature, method="color")
        corrplot(korrelationsmatrix_feature, method="number")
        corrplot(korrelationsmatrix_feature, type="upper", order="hclust")
        corrplot(korrelationsmatrix_feature, method = "circle")
        
        title(paste("Feature",i), outer=TRUE)
        
      }
      
        
      # function plotCorrelationMatrix fuer die features_corrBasis liste aufrufen
      for(i in 1:length(features_corrBasis)){
        
        plotCorrelationMatrix(features_corrBasis[[i]], i)
        
      }
      
      
      
  ## KORRELATIONSMATRIZEN PRO GRUPPE:
      # FUNCTION zum erstellen 6 gruppenplots 
      # parameter: gesamtdatenliste (enthält alle features und jeweils gruppeninfos)
      # parameter: gruppenNr (nummer der betrachteten gruppe)
      erstelleGruppenplots_correlation <- function(gesamtdatenliste, gruppenName, gruppenNr){
        
        par(mfrow=c(1,1), oma=c(0,0,0,0), cex.main=0.7)
        
        # features trennen
        feature1 <- gruppendaten_gesamtliste[[1]]
        feature2 <- gruppendaten_gesamtliste[[2]]
        feature3 <- gruppendaten_gesamtliste[[3]]
        feature4 <- gruppendaten_gesamtliste[[4]]
        feature5 <- gruppendaten_gesamtliste[[5]]
        feature6 <- gruppendaten_gesamtliste[[6]]
        
        
        # correlationsmatrix fuer die gerade betrachtete gruppe berechnen
        f1_gr <- feature1[[gruppenNr]]
        f2_gr <- feature2[[gruppenNr]]
        f3_gr <- feature3[[gruppenNr]]
        f4_gr <- feature4[[gruppenNr]]
        f5_gr <- feature5[[gruppenNr]]
        f6_gr <- feature6[[gruppenNr]]
        
        
        korrelationsmatrix_gruppe <- cor(cbind(f1_gr$value, f2_gr$value, f3_gr$value,
                                         f4_gr$value, f5_gr$value, f6_gr$value)
                                         )
        
        # correlation plot
        x11()
        corrplot(korrelationsmatrix_gruppe, method="number", main=paste("Korrelation", gruppenName))
        # title(paste("Korrelation", gruppenName), outer=TRUE)
                 
      }
      
      
      # plotfunktion "erstelleGruppenplots_correlation" fuer gesamte featuresliste aufrufen
      n_gruppen <- 6
      gruppen <- c("G1", "G2", "G3", "G4", "G5", "G6")
      for(i in 1:n_gruppen){
        
        erstelleGruppenplots_correlation(gruppendaten_gesamtliste, gruppen[i], i)
        
      }
      
      str(gruppendaten_gesamtliste)
      
      # CHECK: correlationsberechnung fuer G1
      cor(cbind(gruppendaten_gesamtliste$`Feature 1`$G1$value, gruppendaten_gesamtliste$`Feature 2`$G1$value,
          gruppendaten_gesamtliste$`Feature 3`$G1$value, gruppendaten_gesamtliste$`Feature 4`$G1$value,
          gruppendaten_gesamtliste$`Feature 4`$G1$value, gruppendaten_gesamtliste$`Feature 5`$G1$value,
          gruppendaten_gesamtliste$`Feature 6`$G1$value))
      
      
      
#########################################################################################################
### AUFGABE 2 ###
#########################################################################################################

# 2.a. Erzeugen Sie eine abgeleitete Variable aus der Summe von Feature 5 und Feature 6
      
      # basisdaten
      # dataframe fuer values der features 1 bis 6 anlegen (basierend auf daten nach
      # entfernen der unvollstaendigen beobachtungen)
      df_features_basis <- data.frame(f1value=liste_featuresdaten$f1$value,
                                f2value=liste_featuresdaten$f2$value,
                                f3value=liste_featuresdaten$f3$value,
                                f4value=liste_featuresdaten$f4$value,
                                f5value=liste_featuresdaten$f5$value,
                                f6value=liste_featuresdaten$f6$value,
                                stringsAsFactors=FALSE) 
      
      # neue variable "f_sum56" berechnen (und f5 sowie f6 weglassen, f1 bis f4 behalten)
      df_features_inklSum56 <- df_features_basis[ ,1:4] 
      df_features_inklSum56$f_sum56 <- (df_features_basis$f5value + df_features_basis$f6value)
      
      # stadt und datum informationen einfuegen
      df_features_inklSum56$datum <- liste_featuresdaten$f1$Datum
      df_features_inklSum56$stadt <- liste_featuresdaten$f1$Stadt
      
      str(df_features_inklSum56)
      
      # col datum als factor und umwandeln 
      df_features_inklSum56$datum <- as.factor(df_features_inklSum56$datum)

      
# 2.b. Gibt es Korrelationen zwischen den verbleibenden Variablen und der neuen abgeleiteten Variable?

      # datum: 1: 20160813, 2: 20180416
      # stadt: 1: graz, 2: salzburg, 3: wien
      df_features_inklSum56$datum <- as.numeric(as.factor(df_features_inklSum56$datum))
      df_features_inklSum56$stadt <- as.numeric(as.factor(df_features_inklSum56$stadt))
      
      
      # korrelationsmatrix berechnen
      korrelationsmatrix_inklSum56 <- cor(df_features_inklSum56)
      
      # korrelationsmatrix visualisieren (verschiedene typen von visiualisierung)
      # rot: negative korrelation
      # blau: positive korrelation
      
      # plot-anzeige in 2x2 subplots teilen
      
      x11()
      
      par(mfrow=c(2,2), oma=c(0,0,3,0))
      
      corrplot(korrelationsmatrix_inklSum56, method="color")
      corrplot(korrelationsmatrix_inklSum56, method="number")
      corrplot(korrelationsmatrix_inklSum56, type="upper", order="hclust")
      corrplot(korrelationsmatrix_inklSum56, method = "circle")
      
      
      title("Korrelationen von Features 1-4, neue Variable, Daten, Stadt", outer=TRUE)
      
      

# 2.c. Modellieren Sie die abgeleitete Variable mit einem linearen Modell.
# 2.d. Welche Variablen sind im Modell sinnvoll, wie gehen Sie mit den kategoriellen Variablen um?
# 2.e. Beschreiben Sie Ihre Modellierungsergebnisse und erzeugen Sie Grafiken um Ihre Ergebnisse zu dokumentieren.
# 2.f. Welche Modellierungsmethode verwenden Sie und warum haben Sie sich für dieses Modell entschieden?
      
      # dummy-codierung bzw. treatment-codierung der kategoriellen variablen (je nach anzahl der factorlevels)
      # datum:
      df_features_inklSum56$datum <- as.factor(df_features_inklSum56$datum)
      contr.treatment(2) # contrastmatrix fuer 2 levels des factor
      contrasts(df_features_inklSum56$datum) = contr.treatment(2) # treatmentcontrast zuweisen
      
      # stadt:
      df_features_inklSum56$stadt <- as.factor(df_features_inklSum56$stadt)
      contr.treatment(3) # contrastmatrix fuer 3 levels des factor
      contrasts(df_features_inklSum56$stadt) = contr.treatment(3) # treatmentcontrast zuweisen
      
      str(df_features_inklSum56)
      
      # MATRIX-SCATTERPLOT ALLER DATEN VOR REGRESSION: 
      # (ueberblick ueber die paarweise relation der daten erhalten)
      plot(df_features_inklSum56, pch=1, col="black", main="Matrix Scatterplot aller Variablen")
      
      
      # VERSUCH 1: zunaechst normales lineares modell (additiv die variablen beruecksichtigt)
      limod_results <- lm(f_sum56 ~ f1value+f2value+f3value+f4value+datum+stadt, data = df_features_inklSum56)
      summary(limod_results)      
      
      # diagnose der regressionsresultate
      x11()
      par(mfrow = c(2,2))
      plot(limod_results, main="Modell 1")
      # allgemeine anmerkung zu den plots:
      # 1. Residuals vs. Fitted:
      #    Scatterplot zwischen Residuen und Predicted Values. Dieser sollte moeglichst "zufaellig aussehen". 
      # 2. Normal Q-Q:
      #    Wenn die Fehler normalverteilt sind, liegen die Punkte auf der eingezeichneten Gerade.
      # 3. Scale-Location:
      #    Sollte "zufaellig" ausschauen und keine Muster aufweisen. Kann in etwa die Annahme der Homoskedastie 
      #    beurteilen - Annahme weitgehend erfuellt wenn horizontale Linie und random verstreute Punkte.
      # 4. Residuals vs. Leverage:
      #    Plot hilft, "einflussreiche" Outlier ("leverage points") zu identifizieren.Punkte außerhalb der Cook's Distanz   
      #    haben Einfluss auf die Regressionsergebnisse.

      # influenceplot dafuer 
      par(mfrow = c(1,1))
      influence.measures(limod_results)
      influencePlot(limod_results2, main="Influence Plot", 
                    sub="Anmerkung: Kreisgröße proportional zu Cook's Distance" )
      # outliertest
      outlierTest(limod_results)
        
      
      
      # VERSUCH 2: lineares modell (mit interaktion, R-squared verbessert sich)
      limod_results2 <- lm(f_sum56 ~ f1value*f2value*f3value*stadt, data = df_features_inklSum56)
      summary(limod_results2)
      
      # diagnose der regressionsresultate
      x11()
      par(mfrow = c(2,2))
      plot(limod_results2, main="Modell 2")
  
        # influenceplot dafuer 
        par(mfrow = c(1,1))
        influence.measures(limod_results2)
        influencePlot(limod_results2, main="Influence Plot", 
                      sub="Anmerkung: Kreisgröße proportional zu Cook's Distance" )
        # outliertest
        outlierTest(limod_results2)
      
        # plot dataframe
        plot(df_features_inklSum56)

        
        
        # VERSUCH 3: robuste regression (mit huber estimator; residual standard error reduziert vs. versuch 2)
        #            anmerkung: 
        #            mittels robuster regression wird darauf geachtet, dass die einzelne beobachtung nicht so 
        #            stark ins gewicht faellt, d.h. es werden "iterated re-weighted least squares" eingesetzt.
        #            die default-belegung ist die gewichtung nach huber. bringt gut resultate, falls es einfluss-
        #            reiche outlier gibt. 
        limod_results3 <- rlm(f_sum56 ~ f1value*f2value*f3value*stadt, data = df_features_inklSum56)
        summary(limod_results3)
        
        # diagnose der regressionsresultate
        x11()
        par(mfrow = c(2,2))
        plot(limod_results3,main="Modell 3")
        # anmerkung zu den plots:
        # 1. Residuals vs. Fitted
        # 2. Normal Q-Q
        # 3. Scale-Location
        # 4. Residuals vs. Leverage    
        
        # influenceplot dafuer 
        par(mfrow = c(1,1))
        influence.measures(limod_results3)
        influencePlot(limod_results3, main="Influence Plot", 
                      sub="Anmerkung: Kreisgröße proportional zu Cook's Distance" )
        # outliertest
        outlierTest(limod_results3)

        

        # VERGLEICHE VERSCHIEDENE MODELLE
        # anmerkung: untersucht, ob zunahme je einer weiteren variable signifikant an information beitragen,
        # nachdem die vorherigen variablen bereits beruecksichtigt wurden im modell
        anova_res <- anova( lm(f_sum56 ~ f1value, data = df_features_inklSum56),
                            lm(f_sum56 ~ f1value+f2value, data = df_features_inklSum56),
                            lm(f_sum56 ~ f1value+f2value+f3value, data = df_features_inklSum56),
                            lm(f_sum56 ~ f1value+f2value+f3value+f4value, data = df_features_inklSum56),
                            lm(f_sum56 ~ f1value+f2value+f3value+f4value+datum, data = df_features_inklSum56),
                            lm(f_sum56 ~ f1value+f2value+f3value+f4value+datum+stadt, data = df_features_inklSum56)
                     ) 
        anova_res
        summary(anova_res)
            # interpretation:
            # 1. die variablen f1value, f2value, f3value bringen jeweils neue informationen und sind im modell sinnvoll,
            # 2. die variablen f4value und datum tragen nicht signifikant zu einer verbesserung der modellerklaerung bei
            #    wenn f1value, f2value und f3value bereits beruecksichtigt wurden
            # 3. die variable stadt bringt zusaetzlich noch information und ist im modell sinnvoll 
        
  
          
        # versuch der automatischen modellauswahl mit step()
           # anmerkung:
           # mittels step() wird ein versuch gemacht, ein moeglichst gutes modell zu waehlen nach 
           # dem aic-kriterium, d.h. einer kennzahl, die die modellqualitaet und anzahl der notwendigen
           # schaetzer beruecksichtigt. es wird hier auch acht gegeben, dass nicht zu viele variablen 
           # in das modell genommen werden. man startet hier mit dem vollen modell, und es werden diejenigen 
           # variablen mit nicht-signifikanten beitraegen aussortiert.
           step_result <- step(lm(f_sum56 ~ f1value+f2value+f3value+f4value+datum+stadt, data = df_features_inklSum56))
           step_result
           summary(step_result) 
           # interpretation: 
           # empfehlung, die auch durch anova betrachtung vorgeschlagen wurde, wird auch durch step ermittelt
        



