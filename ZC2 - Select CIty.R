###This function has zip car load up available cars from a neighboorhood in the neighborhood list.
###
select_city <- function(i){
  find.cars <-remDr$findElement(using='css selector','#find-cars-search > div.findcar-form > div.findcar-address-wrapper')
  find.cars$clickElement()
  find.cars.selector <- remDr$findElement(using='css selector','#address-search')
  find.cars.selector$highlightElement()
  find.cars.selector$sendKeysToElement(list(neighborhoods.list[i]))
  find.cars.selector.click <- remDr$findElement(using = "partial link text",neighborhoods.list[i])
  find.cars.selector.click$clickElement()
  
  
  find.cars.button <- remDr$findElement(using='css selector','#find-cars-search > div.findcar-form > div.findcar-btn-search-section.findcar-btn-results > a')
  find.cars.button$highlightElement()
  find.cars.button$clickElement()
  Sys.sleep(2) 
}


