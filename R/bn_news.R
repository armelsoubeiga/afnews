
#' Function of collecting articles on Benin press
#'
#' @description Collection function of all the articles on Benin
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
#' dt_ <- bn_news_collect(start_d="2020-11-01", end_d="2020-11-10", press="all")
#' head(dt_)
#' @export
bn_news_collect <- function(start_d, end_d, press="all"){

  ########
  ########  leprogres ############
  leprogres <- function(start_d, end_d){
    #collect rubrique url
    url_rubrique <- read_html("https://leprogresinfo.net/") %>%
                    html_nodes("#td-outer-wrap #td-header-menu ul li a") %>%
                    html_attr("href") %>% str_trim() %>% .[-c(1,8)]

    #Pagination collect
    number_pag <- map_dfr(.x = url_rubrique,
                          .f = function(x){
                            tibble(url = x,
                                   nbr_pg=read_html(x) %>%
                                     html_nodes("#td-outer-wrap .td-ss-main-content .page-nav .last") %>%
                                     html_text() %>% as.integer())})

    # Build all url
    number_pag <- na.omit(number_pag)
    number_pag_order <- number_pag %>%
                        split(1:nrow(.)) %>%
                        map_dfr(.f = function(x){
                          tibble(url = x$url,
                                 nbr_pg=1:x$nbr_pg)})
    number_pag_order <- number_pag_order[order(number_pag_order$nbr_pg),]
    url_pag_leprogres<- paste0(number_pag_order$url,'page/', number_pag_order$nbr_pg, "/")


    # Collect article url by pagination
    art_leprogres <- data.frame()
    for(i in 1:length(url_pag_leprogres)){
      x <- url_pag_leprogres[i]
      date_article = try(read_html(x) %>%
                           html_nodes("#td-outer-wrap .td-ss-main-content .td-module-meta-info .td-post-date time") %>%
                           html_text() %>% str_trim() %>% dmy()%>% date())

      if(is.na(tail(date_article, n=1L))){
        next()
      }else{
        d_i <- tibble(
            date_article = date_article,
            url_article = tryCatch(
                              read_html(x) %>%
                              html_nodes("#td-outer-wrap .td-ss-main-content .item-details h3 a") %>%
                              html_attr("href") %>% str_trim(),
                              error = function(e){NA},
                              warning=  function(e){NA}),
            title_article = tryCatch(
                              read_html(x) %>%
                              html_nodes("#td-outer-wrap .td-ss-main-content .item-details h3 a") %>%
                              html_attr("title") %>% str_trim(),
                              error = function(e){NA},
                              warning=  function(e){NA}))
        art_leprogres <- rbind.data.frame(art_leprogres, d_i)

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
    return(art_leprogres)
  }


  ########
  ########  https://lanationbenin.info/ ############
  lanationbenin <- function(start_d, end_d){

    # category url collecte
    url_category <- read_html("https://lanationbenin.info/")%>%
                    html_nodes(".tz-header .tz-header-menu nav .menu-menu-principal-container ul li a") %>%
                    html_attr("href") %>% str_trim()  %>% .[c(2:6,8:13)]

    # Collect pagination by rubrique
    number_pag <- map_dfr(.x = url_category,
                          .f = function(x){
                            tibble(url = x,
                                   nbr_pg=tryCatch(
                                                 read_html(x) %>%
                                                 html_nodes(".container #content ul li") %>%
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
    url_pag_lanationbenin <- paste0(number_pag_order$url,'page/', number_pag_order$nbr_pg, "/")

    # Collect article url by pagination
    art_lanationbenin <- data.frame()
    for(i in 1:length(url_pag_lanationbenin)){
          x <- url_pag_lanationbenin[i]

          date_article = try(
                        read_html(x) %>%
                        html_nodes(".container #content article header time") %>%
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
                            html_nodes(".container #content article header h2 a") %>%
                            html_attr("href") %>% str_trim(),
                          error = function(e){NA},
                          warning=  function(e){NA}),
            title_article = tryCatch(
                            read_html(x) %>%
                            html_nodes(".container #content article header h2 a") %>%
                            html_text() %>% str_trim(),
                            error = function(e){NA},
                            warning=  function(e){NA}))

          art_lanationbenin <- rbind.data.frame(art_lanationbenin, d_i)

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
    return(art_lanationbenin)
}


  # Select article for time entries
  duration <- interval(as.Date.character(start_d), as.Date.character(end_d)) %>% as.numeric('days')

  if(duration > 30){
    base::warning("warning: the request interval is greater than 30 days.!")
    article_ <- NA
  }
  else{
    if(press=="leprogres"){
      article <- leprogres(start_d, end_d)
      article_ <- article %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
    }else{
      if(press=="lanationbenin"){
        article <- lanationbenin(start_d, end_d)
        article_ <- article %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
      }else{
        article_1 <- leprogres(start_d, end_d)
        article_2 <- lanationbenin(start_d, end_d)
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









#' Main functions of collecting information on Benin
#'
#' @description Function for collecting news (title + content) from the media in Benin.
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
#' dt_news <- bn_news_contents(start_d="2020-11-01", end_d="2020-11-10", press='all')
#' head(dt_news)
#' @export
bn_news_contents <- function(start_d, end_d, press='all') {

  ######
  ###### leprogres ###
  leprogres_news_contents <- function(start_d, end_d){

    leprogres_content <- bn_news_collect(start_d, end_d, press='leprogres')
    content_collect <- map_dfr(.x = leprogres_content$url_article,
                               .f = function(x){
                                    tibble(Content_article = paste0(tryCatch(
                                                              read_html(x) %>%
                                                              html_nodes("#td-outer-wrap .td-ss-main-content article .td-post-content p") %>%
                                                              html_text()%>% str_trim(),
                                                              error = function(e){NA},
                                                              warning=  function(e){NA}),
                                                              collapse = " "),
                                            press_article = "leprogres")})
    article_leprogres <- cbind.data.frame(leprogres_content,content_collect)
    return(article_leprogres)
  }


  ######
  ###### lanationbenin ###
  lanationbenin_news_contents <- function(start_d, end_d){

    lanationbenin_content <- bn_news_collect(start_d, end_d, press='lanationbenin')
      content_collect <- map_dfr(.x = lanationbenin_content$url_article,
                                 .f = function(x){
                                     tibble(Content_article = paste0(tryCatch(
                                                              read_html(x) %>%
                                                              html_nodes(".container #content article section .entry-content p") %>%
                                                              html_text()%>% str_trim(),
                                                              error = function(e){NA},
                                                              warning=  function(e){NA}),
                                                              collapse = " "),
                                            press_article = "lanationbenin")})
      article_lanationbenin <- cbind.data.frame(lanationbenin_content,content_collect)
    return(article_lanationbenin)
  }



  #######
  ######### Call function
  if(is.empty(start_d) || is.empty(end_d) ){
    base::warning("`start_d` `end_d` and can not be empty !")
    article_dt <- NA
  }else{
  if(press=='leprogres'){
    article_dt <- leprogres_news_contents(start_d, end_d)
  }else{
    if(press=='lanationbenin'){
      article_dt <- lanationbenin_news_contents(start_d, end_d)
    }else{
      article_dt_leprogres <- leprogres_news_contents(start_d, end_d)
      article_dt_lanationbenin <- lanationbenin_news_contents(start_d, end_d)
      article_dt <- rbind.data.frame(article_dt_leprogres,article_dt_lanationbenin)
    }
  }
}
  return(article_dt)
}

