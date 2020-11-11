#' Function of collecting articles on many press of Mali
#'
#' @description Collection function of all the articles of contry Mali
#' @param start_d yyyy-mm-dd start date of collection (oldest date, eg: yesterday)
#' @param end_d yyyy-mm-dd end date of collection (newest date, eg: today)
#' @param press specify the name of a press.
#' @return a table containing the articles published in the period
#'
#' @import xml2
#' @import rvest
#' @import tidyverse
#' @import lubridate
#' @import stringi
#'
#' @examples
#' library(afnews)
#' dt_ <- ml_news_collect(start_d="2020-10-20", end_d="2020-10-28", press="all")
#' head(dt_)
#' @export
ml_news_collect <- function(start_d, end_d, press="all"){

  ########
  ########  Lesechos ############
  echos <- function(start_d, end_d){
    #collect rubrique url
    url_rubrique <- read_html("https://www.lesechos.ml/") %>%
                    html_nodes("#header-layout2 .container #dropdown ul li a") %>%
                    html_attr("href") %>% str_trim() %>% .[-1]

    # Collect pagination by rubrique
    url_pag <- map_dfr(.x = url_rubrique,
                       .f = function(x){
                         tibble(
                           url_pag = try(
                                       read_html(x) %>%
                                       html_nodes("#wrapper .pagination li a") %>%
                                       html_attr("href") %>% str_trim()))})
    url_pag_echo <- unique(url_pag$url_pag)

    # Collect article url by pagination
    art_echos <- data.frame()
    for(i in 1:length(url_pag_echo)){
      x <- url_pag_echo[i]
      date_article = try(read_html(x) %>%
                           html_nodes(".bg-body .row .row .post-date-dark ul li:last-child") %>%
                           html_text()%>% str_trim())
      french.months <- c("janvier", "f\\u00e9vrier", "mars", "avril", "mai", "juin",
                         "juillet", "ao\\u00fbt", "septembre", "octobre", "novembre", "d\\u00e9cembre")
      date_article <- stringi::stri_replace_all_fixed(date_article, french.months, month.abb, vectorize_all=FALSE) %>% dmy() %>% date()

      if(is.na(tail(date_article, n=1L))){
        next()
      }else{
        d_i <- cbind_na(
            date_article = date_article,
            url_article = tryCatch(
                              read_html(x) %>%
                              html_nodes(".title-semibold-dark a") %>%
                              html_attr("href") %>% str_trim(),
                              error = function(e){NA},
                              warning=  function(e){NA}),
            title_article = tryCatch(
                              read_html(x) %>%
                              html_nodes(".title-semibold-dark a") %>%
                              html_text() %>% str_trim(),
                              error = function(e){NA},
                              warning=  function(e){NA}))
        art_echos <- rbind.data.frame(art_echos, d_i)

        if(tail(date_article, n=1L) > as.Date.character(start_d)-2){
          width <- options()$width
          cat(paste0(rep('>', width), collapse = ''), "\n")
          Sys.sleep(.05)
        }else{
          cat("Done!\n")
          break()
        }
      }
    }
    # return
    return(art_echos)
  }


  ########
  ########  maliweb ############
  maliweb <- function(start_d, end_d){

    # category url collecte
    url_category <- read_html("https://www.maliweb.net/")%>%
                    html_nodes("#td-header-menu .menu-td-demo-header-menu-container > ul > li > a") %>%
                    html_attr("href") %>% str_trim() %>% .[-c(1:2,9:11)]

    # Collect pagination by rubrique
    number_pag <- map_dfr(.x = url_category,
                          .f = function(x){
                            tibble(url = x,
                                   nbr_pg=tryCatch(
                                             read_html(x) %>%
                                             html_nodes(".page-nav .last") %>% html_text(),
                                             error = function(e){NA},
                                             warning=  function(e){NA})
                                        )})
    number_pag$nbr_pg <- as.numeric(gsub("[^0-9.-]+", "", as.character(number_pag$nbr_pg)))

    # Build all url
    number_pag <- na.omit(number_pag)
    number_pag_order <- number_pag %>%
                        split(1:nrow(.)) %>%
                        map_dfr(.f = function(x){
                          tibble(url = x$url,
                                 nbr_pg=1:x$nbr_pg)})
    number_pag_order <- number_pag_order[order(number_pag_order$nbr_pg),]
    url_pag_maliweb <- paste0(number_pag_order$url,'/page/', number_pag_order$nbr_pg)


    # Collect article url by pagination
    art_maliweb <- data.frame()
    for(i in 1:length(url_pag_maliweb)){
          x <- url_pag_maliweb[i]

          date_article = try(
                        read_html(x) %>%
                        html_nodes(".td-ss-main-content .item-details .td-module-meta-info .td-post-date time") %>%
                        html_text())

          french.months <- c("Jan", "F\\u00e9v", "Mar", "Avr", "Mai", "Juin",
                             "Juil", "Ao\\u00fbt", "Sep", "Oct", "Nov", "D\\u00e9c")
          date_article <- stringi::stri_replace_all_fixed(date_article, french.months, month.abb, vectorize_all=FALSE) %>% dmy() %>% date()


          if(is.na(tail(date_article, n=1L))){
            next()
          }else{
          d_i <- cbind_na(
                  date_article = date_article,
                  url_article = tryCatch(
                                  read_html(x) %>%
                                  html_nodes(".td-ss-main-content .item-details") %>%
                                  html_nodes("h3 > a") %>% html_attr("href") %>% str_trim(),
                                error = function(e){NA},
                                warning=  function(e){NA}),
                  title_article = tryCatch(
                                  read_html(x) %>%
                                  html_nodes(".td-ss-main-content .item-details") %>%
                                  html_nodes("h3 > a") %>% html_attr("title") %>% str_trim(),
                                  error = function(e){NA},
                                  warning=  function(e){NA}))

          art_maliweb <- rbind.data.frame(art_maliweb, d_i)

          if(tail(date_article, n=1L) > as.Date.character(start_d)-2){
            width <- options()$width
            cat(paste0(rep('>', width), collapse = ''), "\n")
            Sys.sleep(.05)
          }else{
            cat("Done!\n")
            break()
          }
      }
    }
    # return
    return(art_maliweb)
}


  # Select article for time entries
  duration <- interval(as.Date.character(start_d), as.Date.character(end_d)) %>% as.numeric('days')

  if(duration > 30){
    base::warning("warning: the request interval is greater than 30 days.!")
    article_ <- NA
  }
  else{
    if(press=="lesechos"){
      article <- echos(start_d, end_d)
      article_ <- article %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
    }else{
      if(press=="maliweb"){
        article <- maliweb(start_d, end_d)
        article_ <- article %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
      }else{
        article_1 <- echos(start_d, end_d)
        article_2 <- maliweb(start_d, end_d)
        article_ <- rbind.data.frame(article_1,article_2) %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
      }
    }
  }

  # verified if article show
  if(nrow(article_)==0){
    base::warning("No article publish in this period !")
  }

  return(article_)
}









#' Main functions of collecting news from Mali
#'
#' @description Function for collecting news (title + content) from the media in Mali.
#' @param start_d yyyy-mm-dd start date of collection (oldest date, eg: yesterday)
#' @param end_d yyyy-mm-dd collection end date (newest date, eg: today)
#' @param press specify the name of a press.
#' @return a table containing the news (title + content + date publisher + press) collected
#'
#' @import xml2
#' @import rvest
#' @import tidyverse
#' @import lubridate
#' @import rapportools
#'
#' @examples
#' library(afnews)
#' dt_news <- ml_news_contents(start_d="2020-10-20", end_d="2020-10-28", press='all')
#' head(dt_news)
#' @export
ml_news_contents <- function(start_d, end_d, press='all') {

  ######
  ###### FASONET CONTENT ###
  echos_news_contents <- function(start_d, end_d){

    echos_content <- ml_news_collect(start_d, end_d, press='lesechos')

    title <-  map_dfr(.x = as.character(echos_content$url_article),
                      .f = function(x){
                          tibble(
                             title = tryCatch(
                                         read_html(x) %>%
                                         html_nodes(".bg-body .news-details-layout1 h2") %>%
                                         html_text()%>% str_trim(),
                                         error = function(e){NA},
                                         warning=  function(e){NA}))})
    echos_content$title_article = title$title
    content_collect <- map_dfr(.x = as.character(echos_content$url_article),
                               .f = function(x){
                                    tibble(Content_article = paste0(tryCatch(
                                                              read_html(x) %>%
                                                              html_nodes(".bg-body .news-details-layout1 p") %>%
                                                              html_text()%>% str_trim(),
                                                              error = function(e){NA},
                                                              warning=  function(e){NA}),
                                                              collapse = " "),
                                            press_article = "lesechos")})
    article_echos <- cbind.data.frame(echos_content,content_collect)
    return(article_echos)
  }


  ######
  ###### Mali web ###
  maliweb_news_contents <- function(start_d, end_d){

      maliweb_content <- ml_news_collect(start_d, end_d, press='maliweb')
      content_collect <- map_dfr(.x = as.character(maliweb_content$url_article),
                                 .f = function(x){
                                     tibble(Content_article = paste0(tryCatch(
                                                              read_html(x) %>%
                                                              html_nodes(".td-ss-main-content .td-post-content p") %>%
                                                              html_text()%>% str_trim(),
                                                              error = function(e){NA},
                                                              warning=  function(e){NA}),
                                                              collapse = " "),
                                            press_article = "maliweb")})
      article_maliweb<- cbind.data.frame(maliweb_content,content_collect)
    return(article_maliweb)
  }



  #######
  ######### Call function
  if(is.empty(start_d) || is.empty(end_d) ){
    base::warning("`start_d` `end_d` and can not be empty !")
    article_dt <- NA
  }else{
  if(press=='lesechos'){
    article_dt <- echos_news_contents(start_d, end_d)
  }else{
    if(press=='maliweb'){
      article_dt <- maliweb_news_contents(start_d, end_d)
    }else{
      article_dt_echos <- echos_news_contents(start_d, end_d)
      article_dt_maliweb <- maliweb_news_contents(start_d, end_d)
      article_dt <- rbind.data.frame(article_dt_echos,article_dt_maliweb)
    }
  }
}
  return(article_dt)
}

