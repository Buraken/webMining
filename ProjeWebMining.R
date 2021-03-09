library('rvest')
a <- function(){
  count = 0
  for(val in restaurant_menu_titles){
    rm_html = html_nodes(webpage,paste('#menu_',count,' .getProductDetail',sep=""))
    rm = html_text(rm_html)
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
  return(menu)
}


url = 'https://www.yemeksepeti.com/63-aras-urfa-bodrum-merkez-mugla'
webpage <- read_html(url)

restaurant_menu_titles_html <- html_nodes(webpage,'#restaurant_titles a')
restaurant_menu_titles <- html_text(restaurant_menu_titles_html)

menu <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("name","detail" ,"category")
menu = a()
colnames(menu) <- c("Food Name","Detail","Price","Category","Restaurant Name")

