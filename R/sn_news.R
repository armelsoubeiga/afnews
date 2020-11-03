
#' Function of collecting articles on Senegal press
#'
#' @description Collection function of all the articles on Senegal
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
#' dt_ <- sn_news_collect(start_d="2020-10-28", end_d="2020-11-02", press="all")
#' head(dt_)
#' @export
sn_news_collect <- function(start_d, end_d, press="all"){

  ########
  ########  tart_d, end_d ############
  walf <- function(start_d, end_d){
    #collect rubrique url
    url_rubrique <- read_html("http://www.walf-groupe.com/") %>%
                    html_nodes("header .mh-main-nav-wrap .menu-menu-container ul li:nth-child(2) ul li a") %>%
                    html_attr("href") %>% str_trim() %>% .[-8]
    
    #Pagination collect
    number_pag <- map_dfr(.x = url_rubrique,
                          .f = function(x){
                            tibble(url = x,
                                   nbr_pg=read_html(x) %>%
                                     html_nodes("#main-content .nav-links a") %>%
                                     html_text() %>% tail(2) %>% head(1) %>% as.integer())})

    # Build all url
    number_pag <- na.omit(number_pag)
    number_pag_order <- number_pag %>%
                        split(1:nrow(.)) %>%
                        map_dfr(.f = function(x){
                          tibble(url = x$url,
                                 nbr_pg=1:x$nbr_pg)})
    number_pag_order <- number_pag_order[order(number_pag_order$nbr_pg),]
    url_pag_walf <- paste0(number_pag_order$url,'page/', number_pag_order$nbr_pg, "/")
    
    
    # Collect article url by pagination
    art_walf <- data.frame()
    for(i in 1:length(url_pag_walf)){
      x <- url_pag_walf[i]
      date_article = try(read_html(x) %>%
                           html_nodes(".mh-posts-list-header span:first-of-type a") %>%
                           html_text() %>% str_trim())
      french.months <- c("janvier", "f\\u00e9vrier", "mars", "avril", "mai", "juin",
                         "juillet", "ao\\u00fbt", "septembre", "octobre", "novembre", "d\\u00e9cembre")
      date_article <- stringi::stri_replace_all_fixed(date_article, french.months, month.abb, vectorize_all=FALSE) %>% dmy() %>% date()
      
      if(is.na(tail(date_article, n=1L))){
        next()
      }else{
        d_i <- tibble(
            date_article = date_article,
            url_article = tryCatch(
                              read_html(x) %>%
                              html_nodes("#main-content article .mh-posts-list-header h3 a") %>%
                              html_attr("href") %>% str_trim(),
                              error = function(e){NA},
                              warning=  function(e){NA}),
            title_article = tryCatch(
                              read_html(x) %>%
                              html_nodes("#main-content article .mh-posts-list-header h3 a") %>%
                              html_attr("title") %>% str_trim(),
                              error = function(e){NA},
                              warning=  function(e){NA}))
        art_walf <- rbind.data.frame(art_walf, d_i)
        
        if(tail(date_article, n=1L) > as.Date.character(start_d)-1){
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
    return(art_walf)
  }


  ########
  ########  lesoleil ############ 
  lesoleil <- function(start_d, end_d){

    # category url collecte  
    url_category <- read_html("http://lesoleil.sn/")%>%
                    html_nodes(".tt-header-wrapper .toggle-block-container nav ul li:nth-child(-n+3) ul li a") %>%
                    html_attr("href") %>% str_trim()

    # Collect pagination by rubrique
    number_pag <- map_dfr(.x = url_category,
                          .f = function(x){
                            tibble(url = x,
                                   nbr_pg=tryCatch(
                                                 read_html(x) %>%
                                                 html_nodes("#content-wrapper .container .page-numbers a") %>%
                                                 html_text() %>% tail(2) %>% head(1) %>% as.integer(),
                                             error = function(e){NA},
                                             warning=  function(e){NA}))})
    # Build all url
    number_pag <- na.omit(number_pag)
    number_pag_order <- number_pag %>%
                        split(1:nrow(.)) %>%
                        map_dfr(.f = function(x){
                          tibble(url = x$url,
                                 nbr_pg=sort(1:x$nbr_pg))})
    number_pag_order <- number_pag_order[order(number_pag_order$nbr_pg),]
    url_pag_lesoleil <- paste0(number_pag_order$url,'page/', number_pag_order$nbr_pg, "/")

    # Collect article url by pagination 
    art_lesoleil <- data.frame()
    for(i in 1:length(url_pag_lesoleil)){
          x <- url_pag_lesoleil[i]
          
          date_article = try(
                        read_html(x) %>%
                        html_nodes("#content-wrapper .tt-post-info .tt-post-label span:last-of-type") %>%
                        html_text())

          french.months <- c("janvier", "f\\u00e9vrier", "mars", "avril", "mai", "juin",
                             "juillet", "ao\\u00fbt", "septembre", "octobre", "novembre", "d\\u00e9cembre")
          date_article <- stringi::stri_replace_all_fixed(date_article, french.months, month.abb, vectorize_all=FALSE) %>% dmy() %>% date()


          if(is.na(tail(date_article, n=1L))){
            next()
          }else{
          d_i <- tibble(
            date_article = date_article,
            url_article = tryCatch(
                            read_html(x) %>%
                            html_nodes("#content-wrapper .container .tt-post-info .tt-post-title") %>%
                            html_attr("href") %>% str_trim(),
                          error = function(e){NA},
                          warning=  function(e){NA}),
            title_article = tryCatch(
                            read_html(x) %>%
                            html_nodes("#content-wrapper .container .tt-post-info .tt-post-title") %>%
                            html_text() %>% str_trim(), 
                            error = function(e){NA},
                            warning=  function(e){NA}))

          art_lesoleil <- rbind.data.frame(art_lesoleil, d_i)

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
    return(art_lesoleil)
}

  
  # Select article for time entries
  duration <- interval(as.Date.character(start_d), as.Date.character(end_d)) %>% as.numeric('days')

  if(duration > 30){
    base::warning("warning: the request interval is greater than 30 days.!")
    article_ <- NA
  }
  else{
    if(press=="walf"){
      article <- walf(start_d, end_d)
      article_ <- article %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
    }else{
      if(press=="lesoleil"){
        article <- lesoleil(start_d, end_d)
        article_ <- article %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
      }else{
        article_1 <- walf(start_d, end_d)
        article_2 <- lesoleil(start_d, end_d)
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









#' Main functions of collecting information on Senegal
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
#' dt_news <- sn_news_contents(start_d="2020-10-28", end_d="2020-11-02", press='all')
#' head(dt_news)
#' @export
sn_news_contents <- function(start_d, end_d, press='all') {

  ######
  ###### FASONET CONTENT ###
  walf_news_contents <- function(start_d, end_d){

    walf_content <- sn_news_collect(start_d, end_d, press='walf')
    content_collect <- map_dfr(.x = walf_content$url_article,
                               .f = function(x){
                                    tibble(Content_article = paste0(tryCatch(
                                                              read_html(x) %>%
                                                              html_nodes("#main-content article .entry-content p") %>%
                                                              html_text()%>% str_trim(),
                                                              error = function(e){NA},
                                                              warning=  function(e){NA}),
                                                              collapse = " "),
                                            press_article = "walf")})
    article_walf <- cbind.data.frame(walf_content,content_collect)
    return(article_walf)
  }

  
  ######
  ###### Mali web ###
  lesoleil_news_contents <- function(start_d, end_d){

      lesoleil_content <- sn_news_collect(start_d, end_d, press='lesoleil')
      content_collect <- map_dfr(.x = lesoleil_content$url_article,
                                 .f = function(x){
                                     tibble(Content_article = paste0(tryCatch(
                                                              read_html(x) %>%
                                                              html_nodes("#content-wrapper .container article .simple-text p") %>%
                                                              html_text()%>% str_trim(),
                                                              error = function(e){NA},
                                                              warning=  function(e){NA}),
                                                              collapse = " "),
                                            press_article = "lesoleil")})
      article_lesoleil <- cbind.data.frame(lesoleil_content,content_collect)
    return(article_lesoleil)
  }



  #######
  ######### Call function
  if(is.empty(start_d) || is.empty(end_d) ){
    base::warning("`start_d` `end_d` and can not be empty !")
    article_dt <- NA
  }else{
  if(press=='walf'){
    article_dt <- walf_news_contents(start_d, end_d)
  }else{
    if(press=='lesoleil'){
      article_dt <- lesoleil_news_contents(start_d, end_d)
    }else{
      article_dt_walf <- walf_news_contents(start_d, end_d)
      article_dt_lesoleil <- lesoleil_news_contents(start_d, end_d)
      article_dt <- rbind.data.frame(article_dt_walf,article_dt_lesoleil)
    }
  }
}
  return(article_dt)
}

