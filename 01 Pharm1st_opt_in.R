
library(tidyverse)
library(DBI)
library(sf) 
library(tmap)
library(dplyr)
library(readr)
library(stringr)
library(tmap)
library(readxl)
library(leaflet)
library(data.table)

######### get the latest valid pharmacy contractors (non DAC) list ----

con <- dbConnect(odbc::odbc(), "NCDR")

sql <- " select *   FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Ref_Contractor]"
result <- dbSendQuery(con,sql)
Ref_Contractor_full <- dbFetch(result)
dbClearResult(result)

sql <- " select *   FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Ref_PharmaceuticalList]"
result <- dbSendQuery(con,sql)
Ref_PharmList_full <- dbFetch(result)
dbClearResult(result)

sql<- "SELECT *
  FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Ref_ICB_short]"

result <- dbSendQuery(con,sql)
icb_short<- dbFetch(result)
dbClearResult(result)


icb_short<-icb_short%>%rename(`STP Code`=ICB_Code)%>%mutate(ICB_print = paste0(`STP Code`, ": ", Short_ICB_Name))

Ref_Contractor_full<-Ref_Contractor_full%>%mutate(FCode = `ContractorCode`)

pharm_list<-Ref_PharmList_full%>%
  mutate(SnapshotMonth = as.Date(SnapshotMonth)) %>%
  mutate(SnapshotMonth = if_else(SnapshotMonth == as.Date("2022-10-01"), 
                                 as.Date("2022-09-01"), 
                                 SnapshotMonth)) %>%
  rename(FCode = `Pharmacy ODS Code (F-Code)`, 
         postcode = `Post Code`, 
         ICB_Name = `STP Name`, 
         HWB = `Health and Wellbeing Board`)%>%
  left_join(Ref_Contractor_full, "FCode") %>%
  filter(ContractType != "DAC")%>%
  mutate(Region_Name =ifelse(is.na(Region_Name), "Region Unknown", Region_Name))

pharm_list<-pharm_list%>% 
  left_join(icb_short, "STP Code")%>% 
  mutate(`STP Code`= ifelse(is.na(Short_ICB_Name), paste0(`STP Code`, " (Old STP Code)"), `STP Code`))%>%
  mutate(Region_Name =ifelse(is.na(Region_Name), "Region Unknown", Region_Name))


latest_contractor<-Ref_Contractor_full%>%
  filter(is.na(Inactive),`ContractorType` == "Pharmacy")%>%
  select(FCode
         ,`ContractorName`
         ,`ParentOrgName`
         ,`ParentOrgSize` 
         ,`STP`
         ,`RegionCode`
         ,`Region_Name`
         ,`ClusterSize`
         ,`ContractType`)%>%
 collect()

pharm_list_most_recent <-pharm_list%>%
  filter(SnapshotMonth==max(SnapshotMonth)) 

pharm_list_most_recent$STP<-ifelse(pharm_list_most_recent$STP=="Q62","QRV",pharm_list_most_recent$STP)

######## Get all registration data ----
filenames <- as_tibble(list.files(path="N:/_Everyone/Primary Care Group/Pharmacy SMT data pack/Pharmacy First"))

#### Pharm1st opt in -------

pha1st_file <- filenames%>% filter(tolower(substr(value, 1,14)) == "pharmacy first")
Pha1st<-xlsx::read.xlsx(paste0("N:/_Everyone/Primary Care Group/Pharmacy SMT data pack/Pharmacy First/", pha1st_file), sheetName = "Sheet1", password=NULL)
Pha1st<-Pha1st%>%mutate(`OptInDate`=as.Date(`Opt.In.Date`, "%d/%m/%Y" ))

############ Contraception opt in -------

oc_file <- filenames%>% filter(tolower(substr(value, 1,21)) == "contraception service")
OC_opt<-xlsx::read.xlsx(paste0("N:/_Everyone/Primary Care Group/Pharmacy SMT data pack/Pharmacy First/",oc_file), sheetName = "Sheet1", password=NULL)
OC_opt<-OC_opt%>%mutate(`OptInDate`=as.Date(`Opt.In.Date`, "%Y-%m-%d" ))

OC_reg_file<- filenames%>% filter(tolower(substr(value, 1,16)) == "contraception re")
OC_reg<-xlsx::read.xlsx(paste0("N:/_Everyone/Primary Care Group/Pharmacy SMT data pack/Pharmacy First/",OC_reg_file), sheetName = "Sheet1", password=NULL)

OC_dereg_file<- filenames%>% filter(tolower(substr(value, 1,16)) == "contraception de")
OC_dereg<-xlsx::read.xlsx(paste0("N:/_Everyone/Primary Care Group/Pharmacy SMT data pack/Pharmacy First/",OC_dereg_file), sheetName = "Sheet1", password=NULL)
colnames(OC_dereg) <- c("ODS.Code"
                        ,"Date_dereg"
                        ,"Date_notice"
                        ,"Service_end"
                        ,"ICB"
                        ,"Area")

OC_dereg<-OC_dereg%>%
  filter(`Service_end`<=Sys.Date())%>%
  select(ODS.Code,`Service_end`)

OC_reg<-OC_reg%>%filter(Date>="2023-12-01")%>%
  mutate(`Opted.In`=TRUE,`OptInDate`=as.Date(`Date`, "%Y-%m-%d" ))%>%
  rename(`Opt.In.Date`=`Date`,`ODS.Code`=`F.Code`)%>%
  left_join(OC_dereg, "ODS.Code")%>%
  filter(is.na(`Service_end`))%>%
  select(-`Service_end`)

OC_opt<-rbind(OC_opt, OC_reg)


############ BP check service registration -------
BP_file <- filenames%>% filter(tolower(substr(value, 1,12)) == "hypertension")
BP_reg<-xlsx::read.xlsx(paste0("N:/_Everyone/Primary Care Group/Pharmacy SMT data pack/Pharmacy First/", BP_file), sheetName = "Sheet1", password=NULL)
BP_reg<-BP_reg%>%
  mutate(`OptInDate`=as.Date(`Date`, "%d/%m/%Y" ))%>%
  rename(`FCode`=`F.Code`)%>%
  left_join(select(pharm_list_most_recent, c("FCode","SnapshotMonth")), "FCode")

get_BP_totalReg<-function(){
  v<-n_distinct(BP_reg$`FCode`)
  v
}

get_BP_invalid<-function(){
  data<-BP_reg%>%
    filter(is.na(SnapshotMonth))
  v<-n_distinct(data$`FCode`)
  v
}

get_BP_oldReg<-function(){
  data<-BP_reg%>%
    filter(Date<"2023-12-01", !is.na(SnapshotMonth))
  
  v<-n_distinct(data$`FCode`)
  v
}

get_pharmList_date<-function(){
  
  v<- as.character(max(pharm_list_most_recent$`SnapshotMonth`))
  v
}

get_BP_newReg<-function(){
  data<-BP_reg%>%
    filter(Date>="2023-12-01", !is.na(SnapshotMonth))
  
  v<-n_distinct(data$`FCode`)
  v
}

get_last_BP<-function(){
  date<-as.character(max(BP_reg$`OptInDate`))
  date
}

#############################################################

get_pha1st_table_1<- function(){
  
  total<-pharm_list_most_recent %>%
    summarise(`Total contractors`=n_distinct(FCode))
  
  
  data1 <- OC_opt %>%
    filter(`Opted.In`==TRUE)%>%
    summarise(`No of contractors opted in`=n_distinct(`ODS.Code`))%>%

    collect()
  
  
  data <- Pha1st %>%
    filter(`Opted.In`==TRUE)%>%
    summarise(`No of contractors opted in Pharm First`=n_distinct(`ODS.Code`))%>%
    mutate(`Total contractors`= total$`Total contractors`,
           `No of contractors opted in Contraception`= data1$`No of contractors opted in`,
           `% of contractors opted in Pharm1st`=round(`No of contractors opted in Pharm First`/`Total contractors`*100,1),
           `% of contractors opted in Contraception`=round(`No of contractors opted in Contraception`/`Total contractors`*100,1))%>%
    select(`Total contractors`,
           `No of contractors opted in Pharm First`,
           `% of contractors opted in Pharm1st`, 
           `No of contractors opted in Contraception`, 
           `% of contractors opted in Contraception`)%>%
    collect()
  
  
  
  data
  
}

Ph1st_national<- get_pha1st_table_1()

get_total<-function(){
  v= Ph1st_national$ `No of contractors opted in Pharm First`
  v
}

get_total_prec<-function(){
  v= Ph1st_national$`% of contractors opted in Pharm1st`
  v
}

get_total_OC<-function(){
  v=Ph1st_national$ `No of contractors opted in Contraception`
  v
}

get_total_prec_OC<-function(){
  v= Ph1st_national$`% of contractors opted in Contraception`
  v
}




get_pha1st_table_2a<- function(){
  
  ICB_total<-pharm_list_most_recent %>%
    group_by(STP)%>%
    summarise(`Total contractors`=n_distinct(FCode))%>%
    rename(`STP Code`=STP)%>%
    left_join(icb_short, "STP Code")%>%
    rename(ICB_Code= `STP Code`)
  
  region<- latest_contractor%>%
    select(STP, RegionCode, Region_Name)%>%
    distinct()%>%
    rename(`ICB_Code`=STP) 
  
  data1 <- OC_opt %>%
    filter(`Opted.In`==TRUE)%>%
    rename(FCode=ODS.Code,ICB_Code=`ICB.Code`)%>%
    #left_join(pharm_list_most_recent, "FCode")%>%
    group_by(ICB_Code)%>%
    summarise(`No of contractors opted in Contraception`=n_distinct(FCode))%>%
    collect()
  
  data <- Pha1st %>%
    filter(`Opted.In`==TRUE)%>%
    rename(FCode=ODS.Code,ICB_Code=`ICB.Code`)%>%
    #left_join(pharm_list_most_recent, "FCode")%>%
    group_by(ICB_Code)%>%
    summarise(`No of contractors opted in Pharmacy First`=n_distinct(FCode))%>%
    right_join(ICB_total, "ICB_Code")%>%
    right_join(data1,"ICB_Code")%>%
    mutate(`% of contractors opted in Pharm1st`=round(`No of contractors opted in Pharmacy First`/`Total contractors`*100, 1),
           `% of contractors opted in Contraception`=round(`No of contractors opted in Contraception`/`Total contractors`*100, 1))%>%
    left_join(region, "ICB_Code")%>%
    collect()
  
  
  data
  
}

get_pha1st_table_2b<- function(){
  
  data<-get_pha1st_table_2a()%>%
  select(`ICB`=`ICB_print`,  Region_Name, 
           `Total contractors`, 
           `No of contractors opted in Pharmacy First`,  
           `% of contractors opted in Pharm1st`,
           `No of contractors opted in Contraception`,
           `% of contractors opted in Contraception`)%>%
    arrange(desc(`% of contractors opted in Pharm1st`)) %>% 
    collect()
}

get_pha1st_table_2<- function(){
  
  data<-get_pha1st_table_2a()%>%
    group_by(Region_Name)%>%
    summarise(`No of contractors opted in Pharmacy First`=sum(`No of contractors opted in Pharmacy First`,na.rm=T),
              `No of contractors opted in Contraception`=sum(`No of contractors opted in Contraception`,na.rm=T), 
              `Total contractors`=sum(`Total contractors`, na.rm=T))%>%
    mutate(`% of contractors opted in Pharm1st`=round(`No of contractors opted in Pharmacy First`/`Total contractors`*100, 1),
           `% of contractors opted in Contraception`=round(`No of contractors opted in Contraception`/`Total contractors`*100, 1))%>%
    select( Region_Name, 
           `Total contractors`, 
           `No of contractors opted in Pharmacy First`,  
           `% of contractors opted in Pharm1st`,
           `No of contractors opted in Contraception`,
           `% of contractors opted in Contraception`)%>%
    arrange(desc(`% of contractors opted in Pharm1st`)) %>%
    collect()
  
  data
}

ph1st_regional<-get_pha1st_table_2()

get_pha1st_table_3<-function(reg="London"){
  
  data<-get_pha1st_table_2a()%>%
    filter(Region_Name==reg)%>%
    select(`ICB`=`ICB_print`,
           `Total contractors`, 
           `No of contractors opted in Pharmacy First`,  
           `% of contractors opted in Pharm1st`,
           `No of contractors opted in Contraception`,
           `% of contractors opted in Contraception`)%>%
    arrange(desc(`% of contractors opted in Pharm1st`)) %>% 
    collect()
  
  data
  
}

get_last<-function(){
  date<-as.character(max(Pha1st$`OptInDate`))
  date
}

get_last_OC<-function(){
  date<-as.character(max(OC_opt$`OptInDate`))
  date
}

plot_ph1st_national<-function(){
  
  data1 <- Pha1st %>%
    filter(`Opted.In`==TRUE)%>%
    mutate(week=floor_date(as.Date(`Opt.In.Date`, "%Y-%m-%d"), unit="week"))%>%
    group_by(week)%>%
    summarise( `optin`=n_distinct(`ODS.Code`))%>%
    collect()
  
  data2 <- OC_opt%>%
    filter(`Opted.In`==TRUE)%>%
    mutate(week=floor_date(as.Date(`Opt.In.Date`, "%Y-%m-%d"), unit="week"))%>%
    group_by(week)%>%
    summarise( `optin`=n_distinct(`ODS.Code`))%>%
    collect()
  
  
  p1<- ggplot() +
    geom_line(data1, mapping = aes(x = `week` , y = `optin` ), colour = "black") +
    geom_line(data2, mapping = aes(x = `week` , y = `optin` ), colour = "red") +
    geom_point()+
    ggrepel::geom_label_repel(data = data1,
                              mapping = aes(x = `week` , y = `optin`,
                                            label = `optin`),
                              size = 3.5, color = "black",
                              label.size = NA,
                              box.padding = unit(0.5, "lines")) +
    ggrepel::geom_label_repel(data = data2,
                              mapping = aes(x = `week` , y = `optin`,
                                            label = `optin`),
                              size = 3.5, color = "red",
                              label.size = NA,
                              box.padding = unit(0.5, "lines")) +
    scale_x_date(date_labels = "%Y-%m-%d")+ 
    theme_bw() +
    theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5))+
    scale_size_manual(values = 1) +
    scale_y_continuous(label = scales::comma,
                       breaks = scales::pretty_breaks()) +
    labs(title = "Number of Contractors Opted in", y = "", x="Week Commencing")
  
  
  p1
}

plot_ph1st_regional<-function(){
  data<-ph1st_regional%>%
    select(Region_Name, 
           `% of contractors opted in Pharm1st`,
           `% of contractors opted in Contraception`)
  
  data<-reshape2::melt(data, 
             id.vars = c("Region_Name"), 
             variable.name = "Service")
 
    title <- paste( "% of Contractors opted in to Pharmacy first, Contraception, BP check and CPCS services")
    
    p2<- ggplot(data, aes(x = `Region_Name`, y = `value`, fill = `Service`)) +
      geom_bar(width=0.7, position=position_dodge(width=0.75), stat="identity") +
      # geom_bar(position="dodge") +
      scale_y_continuous(label = scales::comma,
                         breaks = scales::pretty_breaks()
      )+
      labs(title = title,
           fill = "Service")+
      xlab(paste(""))+
      ylab(paste("Precentage of contractors opted in")) +
      theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5))
    
    p2
    
  
}
map_pha1st_icb <- function(interactive_mode = FALSE, serv="Pharmacy First"){
  
  #toggle between interactive and static output
  if(interactive_mode == TRUE){
    
    tmap_options(check.and.fix = TRUE)
    tmap::tmap_mode("view")
    
  }else{
    tmap_options(check.and.fix = TRUE)
    tmap::tmap_mode("plot")
    
  }
  
  map_data<-get_pha1st_table_2a()
  
  if(serv=="Pharmacy First"){
  col_name = "% of contractors opted in Pharm1st"
  last <- get_last() }
  else if(serv=="Contraception"){ 
  col_name = "% of contractors opted in Contraception"
  last <- get_last_OC()}
  
  ICB_Boundaries <- sf::st_read("C:/Users/LXu/Documents/Pharmacy_First_Service_Report/maps/Download_ICB_Boundaries.geojson", quiet = TRUE)
  
  map_data <- dplyr::left_join(map_data, ICB_Boundaries, by = c("ICB_Code" = "icb_code"))
  
  map_data<- map_data%>%filter(!is.na(icb_name)) 
  
  # Transform data into sf
  map_data <- sf::st_as_sf(map_data)
  #map_data <- st_sf (map_data)
  
  map_title <- paste0("% of contractors opted in to ",serv," service at ICB Level as on ", last)
  
  
  map <- tm_shape(map_data) +
    tm_polygons(col = col_name,
                n = 5,
                style = "quantile",
                id = "icb_name",
                title = "",
                palette = "Greens",
                contrast = 1, alpha = 1,
                borders.col = "black") +
    tm_scale_bar() +
    tm_compass(size = 3, position = c("0.85", "0.85")) 
  
  #each output uses a different title argument
  if(interactive_mode == TRUE){
    
    map <- map +
      tm_layout(title = map_title,
                main.title.position = "center",
                main.title.size = 1.05,
                legend.title.size = 0.85,
                legend.format = list(fun = function(x) formatC(x, digits = 0, big.mark = " ", format = "f")))
    
  }else{
    
    map <- map +
      tm_layout(title = str_wrap(map_title, 80),
                main.title.position = "center",
                main.title.size = 1.05,
                legend.title.size = 0.85,
                legend.format = list(fun = function(x) formatC(x, digits = 0, big.mark = " ", format = "f")))
    
  }
  
  map
}


