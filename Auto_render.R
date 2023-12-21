# Render pharmacy first weekly update


#### High level PDF output
rmarkdown::render(input = "rmarkdown/Pharm1st-Weekly-Update_pdf.Rmd",
                  output_format = "beamer_presentation",
                  #params = list(level = "National"),
                  output_file = paste0("../Outputs/Pharmacy_First_Opted_In_Weekly_Update ", Sys.Date(),".pdf"))

#### Produce excel output
source("02 Excel_output.R")

#### HTML version - with more detailed views for regions and ICBs
rmarkdown::render(input = "rmarkdown/Pharmacy1st-Weekly-Update.Rmd",
                  output_format = "xaringan::moon_reader",
                  output_file = paste0("../Outputs/Pharmacy_First_Opted_In_Weekly_Update_", Sys.Date(), ".html"))






################################################################################
#### Create contractor level interactive maps ----

#Load in co-ordinates data and join to Pharmlist

co_ords_data <- read_csv("N:/_Everyone/Primary Care Group/Unplanned Closures PHARM-2022_23-003/Data files for monthly pack/ONSPD_NOV_2022_UK.csv") %>%
  select(pcds,
         oseast1m,
         osnrth1m)


pha1st_postcode_coords<-function(serv="OC"){
  
  pharm_list_most_recent <-pharm_list_most_recent%>%
    rename(pcds = postcode)
  
  if(serv=="Pharm1st"){
    #  name = "Pharmacy First Service"
    
    pharm1st <- Pha1st %>%
      rename(FCode=ODS.Code,`Phone`=`F.code.Phone.Number`)%>%
      right_join(pharm_list_most_recent, "FCode")%>%
      mutate(ICB = paste0(`STP Code`,": ", `Short_ICB_Name`), `LPC`=paste0(`LPC.Code`, ": ",`LPC.Name`))%>%
      select(FCode, `Opted.In`,`Registered.for.CPCS`,`Opt.In.Date`,`OptInDate`,`pcds`,`Name`,`Phone`, `LPC`, `ICB`, `Region_Name`,`Pharmacy Trading Name`)%>%
      mutate(`Name`=ifelse(is.na(Name), `Pharmacy Trading Name`, Name))%>%
      collect()
    
  } else if(serv =="OC")
  {
    # name = "Contraception Service"
    pharm1st <- OC_opt %>%
      rename(FCode=ODS.Code,`Phone`=`F.code.Phone.Number`)%>%
      right_join(pharm_list_most_recent, "FCode")%>%
      mutate(ICB = paste0(`STP Code`,": ", `Short_ICB_Name`), `LPC`=paste0(`LPC.Code`, ": ",`LPC.Name`))%>%
      select(FCode, `Opted.In`,`Opt.In.Date`,`OptInDate`,`pcds`,`Name`,`Phone`, `LPC`, `ICB`, `Region_Name`,`Pharmacy Trading Name`)%>%
      mutate(`Name`=ifelse(is.na(Name), `Pharmacy Trading Name`, Name))%>%
      collect()
    
  }
  
  pharmacy_postcode_coords <-  pharm1st %>%
    left_join(co_ords_data, by = "pcds") %>%
    rename(`postcode`=`pcds`)
  
  pharmacy_postcode_coords$`Opted.In`<-ifelse(is.na(pharmacy_postcode_coords$`Opted.In`), FALSE, pharmacy_postcode_coords$`Opted.In`)
  
  pharmacy_postcode_coords
}

create_map <- function(serv ="OC"){
  if(serv=="Pharm1st"){
    name = "Pharmacy First Service"
    lastdate<- as.character(max(Pha1st$`OptInDate`))
    data <- pha1st_postcode_coords(serv)  %>%
      mutate(colour = case_when(`Opted.In`==TRUE & `Registered.for.CPCS`==TRUE~ "Green",
                                `Opted.In`==TRUE & `Registered.for.CPCS`!=TRUE ~ "Amber",
                                TRUE ~ "Red")) %>%
      ungroup()%>%
      filter(!is.na(postcode) & !is.na(oseast1m)) 
    
    data1<-data%>%filter(`Opted.In`== TRUE)
    data2<-data%>%filter(`Opted.In`== FALSE)
    
    #make into shape file
    OptedIn <- sf::st_as_sf(x = data1, coords = c("oseast1m", "osnrth1m"), crs = 27700)
    NotOptedIn <- sf::st_as_sf(x = data2, coords = c("oseast1m", "osnrth1m"), crs = 27700)
    
    #bring red dots to the front
    OptedIn$colour <- factor(OptedIn$colour, levels = c("Green","Amber"))
    OptedIn <- dplyr::arrange(OptedIn, colour)
    
    palette_name<-c(Green='green',Amber = 'yellow', Red='red')
    label_name1 <- c("Opted in to PharmFirst and also registered for CPCS","Opted in to PharmFirst but not registered for CPCS", "Not opted in to PharmFirst yet")
    label_name2 <- c("Not opted in to PharmFirst yet")}
  else if(serv =="OC")
  {name = "Contraception Service"
  lastdate<- as.character(max(OC_opt$`OptInDate`))
  data <- pha1st_postcode_coords(serv)  %>%
    mutate(colour = case_when(`Opted.In`==TRUE ~ "Green",
                              TRUE ~ "Red")) %>%
    ungroup()%>%
    filter(!is.na(postcode) & !is.na(oseast1m)) 
  data1<-data%>%filter(`Opted.In`== TRUE)
  data2<-data%>%filter(`Opted.In`== FALSE)
  
  #make into shape file
  OptedIn <- sf::st_as_sf(x = data1, coords = c("oseast1m", "osnrth1m"), crs = 27700)
  NotOptedIn <- sf::st_as_sf(x = data2, coords = c("oseast1m", "osnrth1m"), crs = 27700)
  
  palette_name<-c(Green='green', Red='red')
  label_name1 <- c("Opted in to Contraception Service")
  label_name2 <- c("Not opted in to Contraception Service yet")}
  
  title1 =paste0("Pharmacies opted in for ",name, " as on ", lastdate)
  
  
  var_list <- c("FCode", "Name","Phone", "LPC", "ICB", "Region_Name", "Opt.In.Date")
  
  
  #get boundaries geopackage
  ICB_boundaries <- sf::st_read("N:/_Everyone/Primary Care Group/Unplanned Closures PHARM-2022_23-003/Data files for monthly pack/ICB_boundaries.gpkg")
  region_boundaries <- sf::st_read("N:/_Everyone/Primary Care Group/Unplanned Closures PHARM-2022_23-003/Data files for monthly pack/Region_boundaries.gpkg")
  `ICB boundaries` <- st_make_valid(ICB_boundaries)
  
  #plot map
  tmap::tmap_mode("view")
  #tmap::tmap_options(check.and.fix = TRUE)
  
  tm<- tm_shape(OptedIn,
                bbox = st_bbox(c(xmin =-7.57216793459, xmax = 1.68153079591, ymax = 58.6350001085, ymin = 49.959999905), crs = st_crs(4326))) +
    tm_dots(col = "colour",
            palette = palette_name,
            labels = label_name1,
            title = "",
            popup.vars = var_list,
            legend.show = TRUE) +
    tm_shape(NotOptedIn,
             bbox = st_bbox(c(xmin =-7.57216793459, xmax = 1.68153079591, ymax = 58.6350001085, ymin = 49.959999905), crs = st_crs(4326))) +
    tm_dots(col = "colour",
            palette = palette_name,
            labels = label_name2,
            title = "",
            popup.vars = var_list,
            legend.show = TRUE) +
    tm_shape(`ICB boundaries`,
             labels = "ICB boundaries") +
    tm_borders(col = "grey40", lwd = 2, lty = "solid", alpha = 0.5) +
    tm_layout(title = title1) +
    tm_scale_bar(position =c("left", "bottom"))
  
  #tm%>%tmap_leaflet()%>%
  #leaflet::hideGroup("NotOptedIn")
  
  tm
}

pha1st_map <- create_map("Pharm1st")
OC_map<-create_map("OC")


tmap_save(pha1st_map, filename = paste0("maps/Pharm1st_opt_in_map_", format(Sys.time(), '%B%Y'), ".html") )
tmap_save(OC_map, filename = paste0("maps/Contraception_opt_in_map_", format(Sys.time(), '%B%Y'), ".html") )
