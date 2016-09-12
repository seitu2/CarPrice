
get_loc_price_data <- function(loc){
  loc.data <- remDr$getPageSource()[[1]] %>%
    read_html()%>%
    html_nodes('#findcars-bytime > div.findcars-data')  #Get each row of car data
  
  each_loc  <- html_nodes(loc.data, '.findcars-row')     #get each location of car data

  
  #use the first location and get coordinates
  coor <- each_loc[loc] %>%
    html_nodes('.findcars-row-left-location')           #For testing... get data from first location
  coor <- each_loc[loc] %>%
    html_nodes('.findcars-row-left-location')           #For testing... get data from first location
  
  add <- html_text(coor)
  lon <-html_attr(coor,'longitude')
  lat <-html_attr(coor,'latitude')
  coor <- data.frame(add=add,
                     lon=lon,
                     lat =lat)
  #print(coor)
  
  get.car.info<- each_loc[loc] %>%
    html_nodes('.findcars-row-right')  %>%
    html_nodes('.car-make-model')%>%    #For testing... get data from first location
    html_text()
  #print(get.car.info)
  
  get.car.name<- each_loc[loc] %>%
    html_nodes('.reserve-car-text')  %>%
    html_nodes('.car-name')%>%    #For testing... get data from first location
    html_text()
  #print(get.car.name)

  get.price.info <-each_loc[loc] %>%
    #html_nodes('.findcars-row-right')  %>%
    html_nodes('.reserve-price')%>%
    html_nodes('p.rates')%>%    #For testing... get data from first location
    html_text()
  #print(get.price.info)

  #get.price.info <-  substr(get.price.info,1,regexpr('/',get.price.info)-1)
   get.price.info <-  get.price.info[seq(1,length(get.price.info),2)]
  #print(get.price.info)
  car.data <- data.frame(car=get.car.info,name=get.car.name,rate=get.price.info,coor)
  return(car.data)
}












  




