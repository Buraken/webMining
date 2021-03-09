library(RSelenium)
library(httr)
library('rvest')
library(XML)
library(R.utils)
library(stringr)

links = data.frame(links)
write.csv(links,"C:/Users/Bural/Desktop/name.csv")

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()
checkForServer()


remDr$navigate('https://www.yemeksepeti.com/mugla/restoran-arama#ors:true')

remDr$screenshot(display = TRUE)
last_height = 0
repeat {   
  remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
  Sys.sleep(3) #delay by 3sec to give chance to load. 
  
  # Updated if statement which breaks if we can't scroll further 
  new_height = remDr$executeScript("return document.body.scrollHeight")
  if(unlist(last_height) == unlist(new_height)) {
    break
  } else {
    last_height = new_height
  }
}

hlink = read_html(remDr$getPageSource()[[1]])
hlink %>% html_text() -> service_check

rmd_html = html_nodes(hlink,'.withTooltip')
rmd = html_text((rmd_html))

doc <- htmlTreeParse(hlink, useInternal=T)
links <- xpathSApply(doc, "//a[@href]", xmlGetAttr, "href")
links <- links[13:(length(rmd)+ 13 - 1)]

a <- function(){
  for(i in links){
    print(i)
    url = paste('https://www.yemeksepeti.com',i,sep = "")
    webpage <- read_html(url)
    restaurant_menu_titles_html <- html_nodes(webpage,'#restaurant_titles a')
    restaurant_menu_titles <- html_text(restaurant_menu_titles_html)
    count = 0
    for(val in restaurant_menu_titles){
      print(val)
      rm_html = html_nodes(webpage,paste('#menu_',count,' .getProductDetail',sep=""))
      rm = html_text(rm_html)
      rm = rm[!str_detect(rm,pattern="\r\r")]
      rm = rm[!str_detect(rm,pattern="\r \r")]
      print(rm)
      if(length(rm) == 0 ){
        count = count + 1
        next()
      }
      
      rmd_html = html_nodes(webpage,paste('#menu_',count,' .product-desc',sep=""))
      rmd = html_text((rmd_html))
      price_html <- html_nodes(webpage,paste('#menu_',count,' .price',sep=""))
      price = html_text(price_html)
      name_html = html_nodes(webpage,'.ys-h2')
      name = html_text(name_html)
      if(length(rm) == length(rmd)){
        rm_table <- data.frame(rm,rmd,price,val,name)
      }else{
        while(length(rm) != length(rmd)){
          rmd <- rmd[-length(rmd)]
          price <- price[-length(price)]
        }
        rm_table <- data.frame(rm,rmd,price,val,name)
      }
      menu <- rbind(menu,rm_table)
      count = count + 1
    }
    
  }
  colnames(menu) <- c("Food Name","Detail","Price","Category","Restaurant Name")
  menu$address <- gsub(".*,","",menu$`Restaurant Name`)
  menu$district <- gsub(".* ","",menu$address)
  menu$district <- str_sub(menu$district, 2, -2)
  menu$address <- gsub("\\(.*","",menu$address)
  menu$`Food Name` <- gsub("\\,.*","",menu$`Food Name`)
  menu$Detail <- gsub("[,]",":",menu$Detail)
  menu$Price <- gsub("[,]",".",menu$Price)
  return(menu)
}

menu <- data.frame(matrix(ncol = 10, nrow = 0))
menu = a()

marmaris = menu
marmaris =marmaris[marmaris$address == " Marmaris ",]
colnames(marmaris) <- c("Food Name","Detail","Price","Category","Restaurant Name","Address","District")
marmaris =marmaris[!str_detect(marmaris$`Food Name`,pattern="Menü"),]
marmaris =marmaris[!str_detect(marmaris$`Food Name`,pattern="Poþet"),]
icecekler = marmaris[str_detect(marmaris$Category,pattern="Ýçecekler"),]

kalori_icecek = function(){
  icecekler$calories = NA
  icecekler$calories[is.na(icecekler$calories) & (str_detect(icecekler$`Food Name`,pattern="Su ") | str_detect(icecekler$`Food Name`,pattern="Su"))] <- 0
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Coca-Cola (33 cl.)"] <- 149
  icecekler$calories[is.na(icecekler$calories) & str_detect(icecekler$`Food Name`,pattern="Coca-Cola Þekersiz ")] <- 1
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Sprite (33 cl.)"] <- 130
  icecekler$calories[icecekler$`Food Name`=="Þalgam Suyu (33 cl.)"] <- 16
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Ayran (30 cl.)"] <- 111
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Coca-Cola (1 L.)"] <- 450
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Fanta (33 cl.)"] <- 157
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Lipton Ice Tea (33 cl.)"] <- 99
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Ayran (20 cl.)"] <- 67
  icecekler$calories[is.na(icecekler$calories) & str_detect(icecekler$`Food Name`,pattern="Coca-Cola Light ")] <- 1
  icecekler$calories[is.na(icecekler$calories) & str_detect(icecekler$`Food Name`,pattern="Soda ")] <- 1
  icecekler$calories[is.na(icecekler$calories) & str_detect(icecekler$`Food Name`,pattern="Pepsi Light")] <- 0
  icecekler$calories[is.na(icecekler$calories) & str_detect(icecekler$`Food Name`,pattern="Pepsi Max ")] <- 0
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Fuse Tea (33 cl.)"] <- 96
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Limonlu Soda"] <- 75
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Soda"] <- 0
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Fanta (1 L.)"] <- 475
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Sprite (1 L.)"] <- 395
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Spirte (33 cl.)"] <- 130
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Pepsi (33 cl.)"] <- 142
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Yedigün (33 cl.)"] <- 152
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="7UP (33 cl.)"] <- 145
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Limonata (33 cl.)"] <- 120
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Coca-Cola (30 cl.)"] <- 135
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Fanta (30 cl.)"] <- 142
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Sprite (30 cl.)"] <- 118
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Pepsi (1 L.)"] <- 430
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Yedigün (1 L.)"] <- 460
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="7UP (1 L.)"] <- 440
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Pepsi (2,5 L.)"] <- 1075
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Sütaþ Ayran (29,5 cl.)"] <- 96
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Tropicana (33 cl.)"] <- 185
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Ayran"] <- 75
  icecekler$calories[is.na(icecekler$calories) & icecekler$`Food Name`=="Eker Ayran (20 cl.)"] <- 67
  return(icecek$calories)
}
icecek$calories = kalori_icecek()

marmaris =marmaris[!str_detect(marmaris$Category,pattern="Ýçecekler"),]

marmaris$calories[1] = 558
marmaris$calories[2]=700

newmar = data.frame(unique(marmaris$`Restaurant Name`))
marmaris2 = subset(marmaris,marmaris$`Restaurant Name`!="Pizza Tomato")


final_marmaris = rbind(icecekler,marmaris2)
final_marmaris = subset(final_marmaris,final_marmaris$`Restaurant Name`!="Pizza Tomato")

write.csv(menu, file = "MyData.csv")
write.csv(final_marmaris,"final_marmaris.csv")
