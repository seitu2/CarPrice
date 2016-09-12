###This series of scripts collects car price data from 
###the ZIP car website for the New York Market Area

###ENTER LOGIN CREDENTIALS HERE:

user.name <-'-USER NAME-' 
user.pw   <-'-USER PASSWORD-'
###Load needed libraries.
options(stringsAsFactors=FALSE)
if(!require(pacman)){install.packages('pacman')}
libraries <- c('RSelenium','dplyr','rvest','qdap')
p_load(libraries,character.only=TRUE)


RSelenium::startServer()

#Set up server and navigate to ZIP Car Login.
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444
                      , browserName = "chrome"
)
remDr$open()


remDr$navigate("http://zipcar.com")
webElem.signin <-remDr$findElement(using = 'id',value="signin-trigger")
webElem.signin$clickElement()
rm(webElem.signin)

#post user name and password to site
web.username <-remDr$findElement(using='css selector',value='#user_name_register') 
web.username$sendKeysToElement(list(user.name))

web.password <-remDr$findElement(using='css selector',value='#password_register')
web.password$sendKeysToElement(list(user.pw))
#password_register
webElem.signin <-remDr$findElement(using = 'css selector',
                                    value="#login_form > fieldset > ul > li:nth-child(4) > div > button")
webElem.signin$clickElement()



####Define pickup and drop off times.
####Current setup is for pickup roughly 3 hours from 
####initial script run time.  Dropoff will be 3 hours 
####after pickup.
pickup.time         <- as.POSIXlt(Sys.time()) 
pickup.time$hour    <- pickup.time$hour+3
pickup.time$min     <- round(pickup.time$min/15)*15
pickup.time$mday     <- pickup.time$mday + 4

dropoff.time      <- pickup.time
dropoff.time$hour <- dropoff.time$hour + 3

  
pickup  <-data.frame(times= pickup.time,name=paste( 'pickup',c('time','date')),stringsAsFactors = FALSE)
dropoff <-data.frame(times=dropoff.time,name=paste('dropoff',c('time','date')),stringsAsFactors = FALSE)
ui.times <- rbind(pickup,dropoff)

####clean up workspace
rm(pickup.time,dropoff.time,libraries,web.password,web.username,user.name,user.pw,pickup,dropoff)

ui.times <- mutate(ui.times,
                   year=substr(times,1,4),
                   month=substr(times,6,7),
                   day=substr(times,9,10),
                   hour=substr(times,12,13),
                   min=substr(times,15,16),
                   time=format(strptime(paste0(hour,":",min), '%H:%M'), '%I:%M %p'),
                   date=paste(month,day,year,sep='/'),
                   name=as.character(name),
                   output=ifelse(name %in% c('pickup time','dropoff time'),time,date))

ui.times ###Check that you have the right times.




############################Enter dates into user interface####'##########
##########################################################ff#############time##
#Bring user interface code in to R and find input cells.
Sys.sleep(10)

reserve.page <-remDr$getPageSource()[[1]]
read.site  <- read_html(reserve.page) %>%
              html_nodes('input')

class <- html_attr(read.site,'class')
id    <- html_attr(read.site,'id')

date.control <-data.frame(class,id,stringsAsFactors = FALSE)


###This data frame as the date time values needed for entry into zip car
###User interface.
####Note: The order variable ensures that pickup time values are inputed 
####before drop off time values.
ui.interface <- data.frame(class=c('findcar-pickup-time ui-autocomplete-input',
                      'findcar-return-time ui-autocomplete-input',
                      'findcar-pickup-date hasDatepicker',
                      'findcar-return-date hasDatepicker'),name=
                       c('pickup time','dropoff time','pickup date','dropoff date'),
                       order=c(2,4,1,3),stringsAsFactors = FALSE)


#now input dates into user interface
ui.interface <- merge(ui.interface,date.control, by='class')
ui.interface <- merge(ui.interface,ui.times,by='name')
ui.interface$id <- paste0('#',ui.interface$id)
ui.interface <-ui.interface[order(ui.interface$order),]



for(i in 1:4){
  
  web.username <-remDr$findElement(using='css',value=ui.interface[i,'id'])
  web.username$highlightElement()
  web.username$sendKeysToElement(list(key = 'delete'))
  Sys.sleep(2)
  web.username$sendKeysToElement(list(ui.interface[i,'output'],key='enter'))
}


####Now I need to collect the list of all places in the NYC market area.
###To begin, click on find cars and select NEW York, NY
find.cars <-remDr$findElement(using='css selector','#reserve-form > div > div.findcar-btn-search-section > a')
find.cars$highlightElement()
find.cars$clickElement()
#click on address location#
find.cars <-remDr$findElement(using='css selector','#find-cars-search > div.findcar-form > div.findcar-address-wrapper')
find.cars$clickElement()

#click on ny
new.york <- remDr$findElement(using='css selector','#overlay-address-top > ul > li:nth-child(2) > a')
new.york <- new.york$clickElement()

webElem <- remDr$findElement(using = "partial link text",'New York, NY')
webElem$clickElement()

###Now get NYC locations
neighborhoods <- remDr$getPageSource()[[1]]
neighborhoods.list <- read_html(neighborhoods) %>%
                      html_nodes('#overlay-address-bottom-neighborhoods-results-neighborhoods') %>%
                      html_nodes('li')%>%
                      html_text()


###Most neighborhoods are defined as bourough - town
###or city - state.
###However, some are just city.  This is problematic since some cities across
###country share same name. This section adds "New York" to cities without an
###accompaning city or state name.
neighborhoods.list <- ifelse(grepl("-",neighborhoods.list),neighborhoods.list,
               paste0(neighborhoods.list," / ", "New York"))


#close neighborhood list box
find.clicker <-remDr$findElement(using='css selector','#btn-close-address > img')
find.clicker$highlightElement()
find.clicker$clickElement()


###clean drive. Keep only needed files.
keep.files <- c('neighborhoods.list','remDr')
rmlist <- ls()
rmlist <- rmlist[!rmlist %in% keep.files]
rm(list = rmlist)

