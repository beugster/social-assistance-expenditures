library(shiny)
library(ggplot2)
library(dplyr)
library(RODBC)
library(shinyWidgets)
library(readxl)
library(tidyverse)

#Wo liegen die Daten?
pfadzudendaten<-'C:/Users/kld1/Downloads/Benchmark_SH_2.accdb'


#Vorbereitungen######
#Welche Gemeinden und Jahre sind in den Daten enthalten?
#Daten einlesen
con <- odbcConnectAccess2007(pfadzudendaten)
#Tabelle mit Individualdaten herauslesen
dtn <- sqlFetch(con, "Tdf_Nettodaten")
#Gemeindelabels vom BFS holen (sie gehen beim Import verloren)
download.file("https://www.bfs.admin.ch/bfsstatic/dam/assets/14709198/master",
              destfile = "gemeindelabels.xlsx",mode="wb")
gemeinden<-read_excel("gemeindelabels.xlsx",
                      sheet = "GDE",trim_ws = T)[,c(3,4)]
names(gemeinden)[1]<-"N_GemeindeNr"

#Gemeindelabels hinzufügen
dtn<-merge(dtn,gemeinden,by="N_GemeindeNr")
gmdn<-unique(dtn$GDENAME)
#Welche Jahre sind in den Daten enthalten?
jahre<-unique(dtn$N_Jahr)


#################################################################
#Hier beginnt die APP#############################################
####################################################################

#User interface######

ui<-fluidPage(
  # Application title
  titlePanel("Sozialhilfekosten - Gemeinde Wohlen"),
  
  sidebarLayout(
    sidebarPanel(
      
      #Dropdown-Menus für Analysen mit Datensatz A
      selectInput(inputId="grundges",
                  label="Wähle eine Grundgesamtheit aus:",
                  choices=c("Sozialhilfedossier"="SHDossier",
                            "Inkassodossier"="InkassoDossier"),
                  selected = c("SHDossier", "InkassoDossier"),
                  multiple = T),
      selectInput(inputId="jahr",
                  label="Wähle ein Jahr",
                  choices=jahre,
                  selected = jahre,
                  multiple = T),
      selectInput(inputId="gemeinde",
                  label="Wähle eine Gemeinde",
                  choices=c(gmdn,"Alle"),
                  selected=c(gmdn,"Alle"),
                  multiple = T),
      selectInput(inputId = "indicator",
                  label="Welche Information zu den Anzahl Fällen in der Sozialhilfe soll dargestellt werden?",
                  choices = c("Anzahl Dossiers"="Dossier",
                              "Anzahl unterstützte Personen"="Personen",
                              "Sozialhilfequote"="Quote",
                              "Personen-Monate"="Personen-Monate"),
                  multiple=T),
      
      #Dropdownmenus für Analysen mit Datensatz B
      selectInput(inputId = "indicatorB",
                  label="Welche Information zu den Kosten der Sozialhilfefälle soll dargestellt werden?",
                  choices = c("Aufwand",
                              "Ertrag",
                              "Ertragsanteil",
                              "Nettoaufwand",
                              "Nettoaufwand pro Person"="NettoaufwandpP",
                              "Nettoaufwand pro Person und Monat"="NettoaufwandpPm"),
                  multiple=T),
      
      #Dropdownmenu Datensatz C
      selectInput(inputId = "indicatorC",
                  label="Welche Kostenkomponente soll dargestellt werden (im Verhältnis zum Totalaufwand)?",
                  choices =c("TotalAufwand", "TotalGrundversorgung","Grundbedarf","Wohnkosten","Gesundheitskosten","KKPrämien","TotalSILZulagen","AHVMindestbeiträge","ÜbrigeSIL","IZU","EFB","TotalPlatzierung","TotalPlatzMKESB","NkMassnahmeMitKESB","ÜberschussKESB","TotalPlatzOKESB","MassnahmeOhneKESB","SchulkostenOhneKESB","NebenkostenOhneKESB","Ambulante","TotalErtrag","Erwerbseinkommen","TotalErsatzeinkommen","ALV","IV","EinkommenSozVersicherung","Familienzulagen","TotalPersönlEinkommen","Alimente","PersRE","ÜbrigeErträge","ErtragGesundheit","Heimatliche","ElternVerwandte"),multiple = T)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "plot"),
      plotOutput(outputId = "plotB"),
      plotOutput(outputId = "plotC")
    )
  )
)

#Server######



server<-function(input,output){
  #Daten einlesen####
  con <- odbcConnectAccess2007(pfadzudendaten)
  #sqlTables(con)Tdf_Gde_Daten
  total.data <- sqlFetch(con, "Tdf_Nettodaten")
  gemeindedaten <- sqlFetch(con, "Tdf_Gde_Daten")
  # Hinzufügen der Einwohnerzahlen #Tabelle gemeindedaten muss geladen sein aus Datenbank 
  total.data <- gemeindedaten %>%
    filter(Jahr %in% jahre) %>%
    select(ID_Gemeinde, Einwohnerzahl, Jahr) %>%
    merge(total.data, by.x = c('Jahr', 'ID_Gemeinde'), by.y = c('N_Jahr', 'N_GemeindeNr'))
  #Spaltennamen aus Originalfile beibehalten
  names(total.data)[names(total.data)==c("Jahr","ID_Gemeinde")]<-c('N_Jahr', 'N_GemeindeNr')
  
  total.data<-total.data%>%
    mutate(
      filter_SHDossier = if_else(N_Aufwand > 0, "SHDossier", "InkassoDossier"))%>%
    mutate(N_Jahr=factor(N_Jahr),
           N_GemeindeNr=factor(N_GemeindeNr,labels=gmdn))
  
  #Datensatz A#####
  #Aggregation
  aggdat <- total.data %>%
    group_by(N_Jahr, N_GemeindeNr, filter_SHDossier) %>% 
    summarise(Dossier = n_distinct(N_DossierNr),
              Personen = n(),
              Quote = n()/Einwohnerzahl,
              PUMt = sum(N_AzMonate)) %>% 
    rbind(total.data %>% 
            group_by(N_Jahr, filter_SHDossier) %>% 
            summarise(N_GemeindeNr = "Alle", 
                      Dossier = n_distinct(N_DossierNr),
                      Personen = n(),
                      Quote = n()/sum(Einwohnerzahl),
                      PUMt = sum(N_AzMonate)))%>%
    pivot_longer(cols=c("Dossier","Personen","Quote","PUMt"))%>%
    mutate(name=ifelse(name=="PUMt","Personen-Monate",name))
  
  
  #Auswahl                                               
  data<-reactive({
    aggdat%>%
      filter(filter_SHDossier%in%input$grundges,
             N_Jahr%in%input$jahr&
               N_GemeindeNr%in%input$gemeinde&
               name%in%input$indicator)
  })
  
  #Plotten
  output$plot<-renderPlot({# Zuweisung einer reaktiven Funktion an den Output; { um Funktionen über mehrere Zeilen laufen zu lassen
    ggplot(data(),aes(x=N_Jahr,
                      y=value,
                      group=N_GemeindeNr,
                      fill=N_GemeindeNr))+
      geom_bar(stat="identity",position = "dodge")+
      facet_grid(.~name)+
      labs(title="Absolute Anzahl Fälle",
           fill="Gemeinde",
           x="Jahr",
           y="Anzahl Fälle")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  })
  
  #Datensatz B######
  aggdatB<-total.data %>% 
    filter(filter_SHDossier == "SHDossier") %>% 
    group_by(N_Jahr, N_GemeindeNr) %>% 
    summarise(Dossier = n_distinct(N_DossierNr),
              Personen = n(),
              PUMt = sum(N_AzMonate),
              Aufwand = sum(N_Aufwand), 
              Ertrag = sum(N_Ertrag), 
              Ertragsanteil = sum(N_Ertrag) / sum(N_Aufwand), 
              Nettoaufwand = sum(N_Aufwand) - sum(N_Ertrag),
              NettoaufwandpP=(sum(N_Aufwand) - sum(N_Ertrag))/n(),
              NettoaufwandpPm=(sum(N_Aufwand) - sum(N_Ertrag))/sum(N_AzMonate)) %>% 
    rbind(total.data %>% 
            filter(filter_SHDossier == "SHDossier") %>% 
            group_by(N_Jahr) %>% 
            summarise(N_GemeindeNr = "Alle", 
                      Dossier = n_distinct(N_DossierNr),
                      Personen = n(),
                      PUMt = sum(N_AzMonate),
                      Aufwand = sum(N_Aufwand), 
                      Ertrag = sum(N_Ertrag), 
                      Ertragsanteil = sum(N_Ertrag) / sum(N_Aufwand), 
                      Nettoaufwand = sum(N_Aufwand) - sum(N_Ertrag),
                      NettoaufwandpP=(sum(N_Aufwand) - sum(N_Ertrag))/n(),
          NettoaufwandpPm=(sum(N_Aufwand) - sum(N_Ertrag))/sum(N_AzMonate)))%>%
    pivot_longer(cols=c("Dossier","Personen","PUMt","Aufwand","Ertrag","Ertragsanteil","Nettoaufwand","NettoaufwandpP","NettoaufwandpPm"))
  #Plotten
  dataB<-reactive({
    aggdatB%>%
      filter(N_Jahr%in%input$jahr&
               N_GemeindeNr%in%input$gemeinde&
               name%in%input$indicatorB)
  })
  output$plotB<-renderPlot({# Zuweisung einer reaktiven Funktion an den Output; { um Funktionen über mehrere Zeilen laufen zu lassen
    ggplot(dataB(),aes(x=N_Jahr,
                       y=value,
                       group=N_GemeindeNr,
                       fill=N_GemeindeNr))+
      geom_bar(stat="identity",position = "dodge")+
      facet_grid(.~name)+
      labs(title="Kosten in der Sozialhilfe",
           fill="Gemeinde",
           x="Jahr",
           y="Anzahl Fälle")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  })
  
  #Datensatz C######
  aggdatC<-total.data %>% 
    filter(filter_SHDossier == "SHDossier") %>% 
    group_by(N_Jahr, N_GemeindeNr) %>% 
    summarise(Dossier = n_distinct(N_DossierNr),
              Personen = n(),
              PUMt = sum(N_AzMonate),
              Einwohner = sum(Einwohnerzahl),
              TotalAufwand = sum(N_Aufwand),
              TotalOrdUnterstAufw = sum(N_TotalOU),
              TotalGrundversorgung = sum(N_TotalGV),
              Grundbedarf = sum(N_GB),
              Wohnkosten = sum(N_Wohnkosten),
              Gesundheitskosten = sum(N_Gesundheitskosten),
              KKPrämien = sum(N_KKPrämien),
              TotalSILZulagen = sum(N_TotalSILZulagen),
              AHVMindestbeiträge = sum(N_AHVMindestbeiträge),
              ÜbrigeSIL = sum(N_ÜbrigeSIL),
              IZU = sum(N_IZU),
              EFB = sum(N_EFB),
              TotalPlatzierung = sum(N_TotalPlatz),
              TotalPlatzMKESB = sum(N_TotalPlatzMKESB),
              NkMassnahmeMitKESB = sum(N_NkMassnahmeMitKESB),
              ÜberschussKESB = sum(N_ÜberschussKESB),
              TotalPlatzOKESB = sum(N_TotalPlatzOKESB),
              MassnahmeOhneKESB = sum(N_MassnahmeOhneKESB),
              SchulkostenOhneKESB = sum(N_SchulkostenOhneKESB),
              NebenkostenOhneKESB = sum(N_NebenkostenOhneKESB),
              Ambulante = sum(N_Ambulante),
              TotalErtrag = sum(N_Ertrag),
              Erwerbseinkommen = sum(N_Erwerbseinkommen),
              TotalErsatzeinkommen = sum(N_TotalErsatzErt),
              ALV = sum(N_ALV),
              IV = sum(N_IV),
              EinkommenSozVersicherung = sum(N_EinkommenSozVersicherung),
              Familienzulagen = sum(N_Familienzulagen),
              TotalPersönlEinkommen = sum(N_TotalPersErt),
              Alimente = sum(N_Alimente),
              PersRE = sum(N_PersRE),
              ÜbrigeErträge = sum(N_ÜbrigeErträge),
              ErtragGesundheit = sum(N_ErtragGesundheit),
              Heimatliche = sum(N_Heimatliche),
              ElternVerwandte = sum(N_ElternVerwandte)) %>% 
    rbind(total.data %>% 
            filter(filter_SHDossier == "SHDossier") %>% 
            group_by(N_Jahr) %>% 
            summarise(N_GemeindeNr = "Alle", 
                      Dossier = n_distinct(N_DossierNr),
                      Personen = n(),
                      PUMt = sum(N_AzMonate),
                      Einwohner = sum(Einwohnerzahl),
                      TotalAufwand = sum(N_Aufwand),
                      TotalOrdUnterstAufw = sum(N_TotalOU),
                      TotalGrundversorgung = sum(N_TotalGV),
                      Grundbedarf = sum(N_GB),
                      Wohnkosten = sum(N_Wohnkosten),
                      Gesundheitskosten = sum(N_Gesundheitskosten),
                      KKPrämien = sum(N_KKPrämien),
                      TotalSILZulagen = sum(N_TotalSILZulagen),
                      AHVMindestbeiträge = sum(N_AHVMindestbeiträge),
                      ÜbrigeSIL = sum(N_ÜbrigeSIL),
                      IZU = sum(N_IZU),
                      EFB = sum(N_EFB),
                      TotalPlatzierung = sum(N_TotalPlatz),
                      TotalPlatzMKESB = sum(N_TotalPlatzMKESB),
                      NkMassnahmeMitKESB = sum(N_NkMassnahmeMitKESB),
                      ÜberschussKESB = sum(N_ÜberschussKESB),
                      TotalPlatzOKESB = sum(N_TotalPlatzOKESB),
                      MassnahmeOhneKESB = sum(N_MassnahmeOhneKESB),
                      SchulkostenOhneKESB = sum(N_SchulkostenOhneKESB),
                      NebenkostenOhneKESB = sum(N_NebenkostenOhneKESB),
                      Ambulante = sum(N_Ambulante),
                      TotalErtrag = sum(N_Ertrag),
                      Erwerbseinkommen = sum(N_Erwerbseinkommen),
                      TotalErsatzeinkommen = sum(N_TotalErsatzErt),
                      ALV = sum(N_ALV),
                      IV = sum(N_IV),
                      EinkommenSozVersicherung = sum(N_EinkommenSozVersicherung),
                      Familienzulagen = sum(N_Familienzulagen),
                      TotalPersönlEinkommen = sum(N_TotalPersErt),
                      Alimente = sum(N_Alimente),
                      PersRE = sum(N_PersRE),
                      ÜbrigeErträge = sum(N_ÜbrigeErträge),
                      ErtragGesundheit = sum(N_ErtragGesundheit),
                      Heimatliche = sum(N_Heimatliche),
                      ElternVerwandte = sum(N_ElternVerwandte)))%>%
  pivot_longer(cols=c("TotalAufwand", "TotalGrundversorgung","Grundbedarf","Wohnkosten","Gesundheitskosten","KKPrämien","TotalSILZulagen","AHVMindestbeiträge","ÜbrigeSIL","IZU","EFB","TotalPlatzierung","TotalPlatzMKESB","NkMassnahmeMitKESB","ÜberschussKESB","TotalPlatzOKESB","MassnahmeOhneKESB","SchulkostenOhneKESB","NebenkostenOhneKESB","Ambulante","TotalErtrag","Erwerbseinkommen","TotalErsatzeinkommen","ALV","IV","EinkommenSozVersicherung","Familienzulagen","TotalPersönlEinkommen","Alimente","PersRE","ÜbrigeErträge","ErtragGesundheit","Heimatliche","ElternVerwandte")) %>% 
    select(N_Jahr, N_GemeindeNr, Dossier, Personen, PUMt, Einwohner, TotalOrdUnterstAufw, name, value) %>%
    mutate(value = value/TotalOrdUnterstAufw) %>%
    arrange(N_GemeindeNr, name)

  
  #Plotten
  dataC<-reactive({
    aggdatC%>%
      filter(N_Jahr%in%input$jahr&
               N_GemeindeNr%in%input$gemeinde&
               name%in%input$indicatorC)
  })
  output$plotC<-renderPlot({# Zuweisung einer reaktiven Funktion an den Output; { um Funktionen über mehrere Zeilen laufen zu lassen
    ggplot(dataC())+
      aes(x=name, y=value, group = N_GemeindeNr, color=N_GemeindeNr) +
      geom_polygon(fill=NA) + 
      coord_polar() + 
      facet_wrap(~ N_GemeindeNr+N_Jahr) + 
      theme(axis.text.x = element_text(size = 5))+
      theme(legend.position = "none")+
      labs(title="Detailkosten in der Sozialhilfe",
           subtitle = "Im Verhältnis zum Total ordentliche Unterstützungsaufwände",
           x="Gemeinde",y="Anteil")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  })
  

}

shinyApp(ui= ui, server=server)


