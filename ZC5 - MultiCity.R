#test <- sample(1:length(neighborhoods.list),1)
#test <-43     
#select_city(test)   #select a city


multi_page_collector <-function(neighborhood.num){
  select_city(neighborhood.num)
  no.cars <-'#findcars-bytime > div.findcars-none > div.findcars-none-msg'
  no.cars.elem <- remDr$findElement(using='css selector',no.cars)  
  no.cars.elem.avail <- no.cars.elem$isElementDisplayed()[[1]]
  
  if(no.cars.elem.avail==TRUE){
    print(paste("No cars available at",neighborhoods.list[neighborhood.num]))
    all.files <-data.frame(car=NA_character_,name=NA_character_,rate=NA_character_,
               add=NA_character_,lon=NA_character_,lat=NA_character_,
               neighborhood=neighborhoods.list[neighborhood.num])
    return(all.files)
  }else{
    i <- 1
    repeat{
      scan_for_car_data(neighborhood.num)
      close_clicker()
      loc.data <- remDr$getPageSource()[[1]] %>%
        read_html()%>%
        html_nodes('#findcars-bytime > div.findcars-data')  #Get each row of car data
      
      each_loc  <- html_nodes(loc.data, '.findcars-row')     #get each location of car data
      locations <- length(each_loc)
      
      more.cars <- remDr$findElement(using='css selector','#findcars-footer-row > a.findcars-next')
      more.cars.displayed <-more.cars$isElementDisplayed()[[1]]
      #print(locations)
      out <-data.frame(do.call('rbind',lapply(seq(1:locations),get_loc_price_data)))
      assign(paste0('output',i),out)
      env <- environment()
      i <- i + 1
      if(more.cars.displayed==TRUE){more.cars$clickElement()}
      if(more.cars.displayed==FALSE){
        break
      }
    }
    #print(i)
    output.list <-ls()
    output.list.keep <-substr(output.list,1,6)=='output'
    output.list <-output.list[output.list.keep]
    output.data <- lapply(output.list,get,envir=env,inherits=FALSE)
    
    all.files <- do.call('rbind',output.data)
    all.files$neighborhood <- neighborhoods.list[neighborhood.num]
    print(paste("Completed",neighborhoods.list[neighborhood.num]))
    Sys.sleep(10)
    return(all.files)
  }
  

#print(output1)
#print(output2)
#print(output3)


}

#car_data <-multi_page_collector(44)


car_data <- do.call('rbind',lapply(1:30,multi_page_collector))

write.table(car_data,file='C:/Users/seitu/Documents/TimesSquare.csv',row.names=FALSE,sep=',')
