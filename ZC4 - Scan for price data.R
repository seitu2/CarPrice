
#close clickerbox if necessary:
scan_for_car_data <- function(neighborhood.num){
  #Determine if no cars are avalable at a location
  no.cars <-'#findcars-bytime > div.findcars-none > div.findcars-none-msg'
  no.cars.elem <- remDr$findElement(using='css selector',no.cars)  
  no.cars.elem.avail <- no.cars.elem$isElementDisplayed()[[1]]
  
  #look for rows of car data
  find.cars.header <- '#findcars-header-row'
  

  find.cars.header.elem <- remDr$findElement(using='css selector',find.cars.header)
  while(find.cars.header.elem$isElementDisplayed()[[1]]==FALSE & no.cars.elem.avail==FALSE){
    Sys.sleep(1)
  find.cars.header.elem <- remDr$findElement(using='css selector',find.cars.header)
  no.cars.elem.avail <- no.cars.elem$isElementDisplayed()[[1]]
  #print("Waiting for car data to load")
  Sys.sleep(1)
  print(paste("Waiting for Car data at",neighborhoods.list[neighborhood.num]))
  }
}


#scan_for_car_data()

close_clicker<-function(){
  no.cars <-'#findcars-bytime > div.findcars-none > div.findcars-none-msg'
  no.cars.elem <- remDr$findElement(using='css selector',no.cars)  
  no.cars.elem.avail <- no.cars.elem$isElementDisplayed()[[1]]
  
  if(no.cars.elem.avail==TRUE){
    print("No cars avaiable")
  }else {
    restricted.css <- '#findcars-results > div.render-div > div:nth-child(1) > div.findcars-row > div.findcars-row-right-wrapper > div.findcars-row-right > div.reserve-car-div-large.is-restricted > div.res_plus_minus > img'
    unrestricted.css <-'#findcars-results > div.render-div > div:nth-child(1) > div.findcars-row > div:nth-child(2) > div.findcars-row-right > div.reserve-car-div-large > div.res_plus_minus > img'
    restricted.selector <-length(remDr$findElements(using='css selector', restricted.css))
    unrestricted.selector <-length(remDr$findElements(using='css selector', unrestricted.css))
    
    if(restricted.selector==0 & unrestricted.selector==1){
      find.clicker <-remDr$findElement(using='css selector',unrestricted.css)
      clicker.displayed <-find.clicker$isElementDisplayed()[[1]]
      if(clicker.displayed==TRUE){find.clicker$clickElement()}
      
    }else
      if(restricted.selector==1 & unrestricted.selector==1){
        find.clicker <-remDr$findElement(using='css selector',restricted.css)
        clicker.displayed <-find.clicker$isElementDisplayed()[[1]]
        if(clicker.displayed==TRUE){find.clicker$highlightElement();find.clicker$clickElement()}
      }
    
  }
  
  
}


#scan_for_car_data()
#close_clicker()


#list <- ls()
#rm(list=list[!list %in% c('close_clicker','remDr')])


