#' Function of collecting articles on many press of Burkina Faso
#'
#' @description Collection function of all the articles of contry Burkina Faso
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
#' dt_ <- bf_news_collect(start_d="2020-10-20", end_d="2020-10-28", press="all")
#' head(dt_)
#' @export
bf_news_collect <- function(start_d, end_d, press="all"){

  ########
  ########  FASO NET ############
  fasonet <- function(start_d, end_d){
    #collect rubrique url
    url_rubrique <- read_html("https://lefaso.net/") %>%
                    html_nodes("a") %>%
                    html_attr("href") %>% str_trim() %>%
                    str_subset("^spip.php\\?rubrique")

  # Collect pagination by rubrique
    url_pag <- map_dfr(.x = unique(paste("http://lefaso.net/",url_rubrique,sep="")),
                       .f = function(x){
                        tibble(
                          url_pag=try(c(sub("http://lefaso.net/","",x),
                                       read_html(x) %>%
                                       html_nodes(".pagination a") %>%
                                       html_attr("href") %>% str_trim())))})
    url_pag$url_pag <- unique(paste("http://lefaso.net/",url_pag$url_pag,sep=""))

    # Collect article url by pagination url_category[-c(1,3)]
    art_fasonet <- data.frame()
    for(i in 1:length(url_pag$url_pag)){
      x <- url_pag$url_pag[i]

      date_article = try(
                      read_html(x) %>%
                      html_nodes("abbr") %>%
                      html_text())

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
                              html_nodes(".container .row div h3 a") %>%
                              html_attr("href") %>% str_trim(),
                              error = function(e){NA},
                              warning=  function(e){NA}),

               title_article = tryCatch(
                             read_html(x) %>%
                             html_nodes(".container .row div h3 a") %>%
                             html_text() %>% str_trim(),
                             error = function(e){NA},
                             warning=  function(e){NA}))

        art_fasonet <- rbind.data.frame(art_fasonet, d_i)

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
    # url article
    art_fasonet$url_article <- paste0("http://lefaso.net/",art_fasonet$url_article)
    # return
    return(art_fasonet)
  }



  ########
  ########  BURKINA 24 ############
  burkina24 <- function(start_d, end_d){

    # category url collecte
    url_category <- read_html("https://www.burkina24.com/")%>%
                    html_nodes("#primary-menu .menu-inside > ul > li > a") %>%
                    html_attr("href") %>% str_trim() %>% .[-c(1,3)]

    # Collect pagination by rubrique
    number_pag <- map_dfr(.x = url_category,
                          .f = function(x){
                            tibble(url = x,
                                   nbr_pg=tryCatch(
                                           read_html(x) %>%
                                           html_node(".dots + .page-numbers") %>%
                                           html_text() %>% as.integer(),
                                           error = function(e){NA},
                                           warning=  function(e){NA})
                                           )})

    # Build all url
    number_pag <- na.omit(number_pag)
    int <- map_dfr(.x=number_pag$nbr_pg, .f=function(x){tibble(int=1:x)})
    url_pag_b24 <- paste0(number_pag$url,'page/', sort(int$int))


    # Collect article url by pagination url_category[-c(1,3)]
    art_b24 <- data.frame()
    for(i in 1:length(url_pag_b24)){
          x <- url_pag_b24[i]
          date_article = try(
                        read_html(x) %>%
                        html_nodes(xpath = "//div[@class='posts-lists']/div/article") %>%
                        html_nodes(".meta > span:last-of-type") %>%
                        html_text())

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
                            html_nodes(xpath = "//div[@class='posts-lists']/div/article") %>%
                            html_nodes("h2 > a") %>% html_attr("href") %>% str_trim(),
                          error = function(e){NA},
                          warning=  function(e){NA}),
            title_article = tryCatch(
                            read_html(x) %>%
                            html_nodes(xpath = "//div[@class='posts-lists']/div/article") %>%
                            html_nodes("h2 > a") %>% html_text() %>% str_trim(),
                            error = function(e){NA},
                            warning=  function(e){NA}))

          art_b24 <- rbind.data.frame(art_b24, d_i)

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
    return(art_b24)
}


  # Select article for time entries
  duration <- interval(as.Date.character(start_d), as.Date.character(end_d)) %>% as.numeric('days')

  if(duration > 30){
    base::warning("warning: the request interval is greater than 30 days.!")
    article_ <- NA
  }
  else{
    if(press=="fasonet"){
      article <- fasonet(start_d, end_d)
      article_ <- article %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
    }else{
      if(press=="burkina24"){
        article <- burkina24(start_d, end_d)
        article_ <- article %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
      }else{
        article_1 <- fasonet(start_d, end_d)
        article_2 <- burkina24(start_d, end_d)
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









#' Main functions of collecting news from Burkina Faso
#'
#' @description Function for collecting news (title + content) from the media in Burkina Faso.
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
#' dt_news <- bf_news_contents(start_d="2020-10-20", end_d="2020-10-28", press='all')
#' head(dt_news)
#' @export
bf_news_contents <- function(start_d, end_d, press='all') {

  ######
  ###### FASONET CONTENT ###
  fasonet_news_contents <- function(start_d, end_d){

    fasonet_content <- bf_news_collect(start_d, end_d, press='fasonet')
    content_collect <- map_dfr(.x = as.character(fasonet_content$url_article),
                               .f = function(x){
                                    tibble(Content_article = tryCatch(
                                                            read_html(x) %>%
                                                            html_nodes(xpath = "//div[@class='article_content']") %>%
                                                            html_text()%>% str_trim(),
                                                            error = function(e){NA},
                                                            warning=  function(e){NA}),
                                                  press_article = "fasonet")})
    article_fasonet <- cbind.data.frame(fasonet_content,content_collect)
    return(article_fasonet)
  }

  ######
  ###### BURKINA 24 CONTENTS ###
  burkina24_news_contents <- function(start_d, end_d){

      burkina24_content <- bf_news_collect(start_d, end_d, press='burkina24')
      content_collect <- map_dfr(.x = as.character(burkina24_content$url_article),
                                 .f = function(x){
                                     tibble(Content_article = tryCatch(
                                                      read_html(x) %>%
                                                      html_nodes("article > .page-content") %>%
                                                      html_text()%>% str_trim(),
                                                      error = function(e){NA},
                                                      warning=  function(e){NA}),
                                            press_article = "burkina24")})
      article_burkina24 <- cbind.data.frame(burkina24_content,content_collect)
    return(article_burkina24)
  }



  #######
  ######### Call function
  if(is.empty(start_d) || is.empty(end_d) ){
    base::warning("`start_d` `end_d` and can not be empty !")
    article_dt <- NA
  }else{
  if(press=='fasonet'){
    article_dt <- fasonet_news_contents(start_d, end_d)
  }else{
    if(press=='burkina24'){
      article_dt <- burkina24_news_contents(start_d, end_d)
    }else{
      article_dt_fasonet <- fasonet_news_contents(start_d, end_d)
      article_dt_burkina24 <- burkina24_news_contents(start_d, end_d)
      article_dt <- rbind.data.frame(article_dt_fasonet,article_dt_burkina24)
    }
  }
}
  return(article_dt)
}

