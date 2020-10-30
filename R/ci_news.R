#' Main function of collecting articles in Ivory Coast
#'
#' @description Collection function of all the articles of Ivory Coast
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
#' article <- ci_news_collect(start_d="2020-10-20", end_d="2020-10-25", press="all")
#' head(article)
#' @export
ci_news_collect <- function(start_d, end_d, press="all"){

  ########
  ########  linfodrome ############
  linfodrome <- function(start_d, end_d){
    # collect category
    url_rubrique <- read_html("https://www.linfodrome.com/") %>%
                    html_nodes("#t3-mainnav > .container ul > li:not(.item-803):not(.item-805):not(.item-806)") %>%
                    html_node("a") %>% html_attr("href")
    url_rubrique_ <- paste0('https://www.linfodrome.com',url_rubrique[3:14][-7])

  #Pagination collect
  number_pag <- map_dfr(.x = url_rubrique_,
                        .f = function(x){
                          tibble(url = x,
                                 nbr_pg=tryCatch(
                                         read_html(x) %>%
                                         html_node(".k2Pagination > ul li:last-child a") %>%
                                         html_attr("href")  %>% str_split("=") %>% .[[1]] %>% .[3] %>% as.integer(),
                                         error = function(e){NA},
                                         warning=  function(e){NA}))
                          })

  # Build all url
  number_pag <- na.omit(number_pag)
  int <- map_dfr(.x=number_pag$nbr_pg, .f=function(x){tibble(int=seq(0,x,10))})
  url_pag_linfodrome <- paste0(number_pag$url,'?limit=10&start=', sort(int$int))

  # Collect article url by pagination
  art_linfodrome <- data.frame()
  for(i in 1:length(url_pag_linfodrome)){
    x <- url_pag_linfodrome[i]

    date_article = try(
                    read_html(x) %>%
                    html_nodes("#itemListLeading .catItemDateCreated") %>%
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
                                paste0('https://www.linfodrome.com',read_html(x) %>%
                                html_nodes("#itemListLeading .catItemHeader") %>%
                                html_nodes("h3 > a") %>% html_attr("href") %>% str_trim()),
                                error = function(e){NA},
                                warning=  function(e){NA}),
              title_article = tryCatch(
                                read_html(x) %>%
                                html_nodes("#itemListLeading .catItemHeader") %>%
                                html_nodes("h3 > a") %>% html_text() %>% str_trim(),
                              error = function(e){NA},
                              warning=  function(e){NA}))

    art_linfodrome <- rbind.data.frame(art_linfodrome, d_i)

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
  return(art_linfodrome)
}


  ########
  ########  yeclo ############
  yeclo <- function(start_d, end_d){
    #Pagination collect
    number_pag <- try(
                  read_html('https://www.yeclo.com/actualite/') %>%
                  html_node(".page-nav .last") %>%
                  html_attr("href") %>% str_extract_all("[[:digit:]]+") %>% as.integer())

    # Build all url
    url_pag_yeclo <- paste0("https://www.yeclo.com/actualite/",'page/',1:number_pag, '/')

    # Collect article url by pagination
    art_yeclo <- data.frame()
    for(i in 1:length(url_pag_yeclo)){
      x <- url_pag_yeclo[i]

      date_article = try(
                      read_html(x) %>%
                      html_nodes(".td-container .td-block-row .td-post-date time") %>%
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
                                html_nodes(".td-container .td-block-row") %>%
                                html_nodes("h3 > a") %>% html_attr("href") %>% str_trim(),
                            error = function(e){NA},
                            warning=  function(e){NA}),
              title_article = tryCatch(
                                read_html(x) %>%
                                html_nodes(".td-container .td-block-row") %>%
                                html_nodes("h3 > a") %>% html_text() %>% str_trim(),
                              error = function(e){NA},
                              warning=  function(e){NA}))

      art_yeclo <- rbind.data.frame(art_yeclo, d_i)

      if(tail(date_article, n=1L) > as.Date.character(start_d)){
        width <- options()$width
        cat(paste0(rep('>', width), collapse = ''), "\n")
        Sys.sleep(.01)
      }else{
        cat("Done!\n")
        break()
      }
      }
    }
    return(art_yeclo)
}



  ########
  ########  yeclo ############
  api <- function(start_d, end_d){
      # category url collecte
      url_category <- read_html("https://aip.ci/") %>%
                      html_nodes("#cssmenu > ul > li:nth-child(2) > ul > li > a") %>%
                      html_attr("href")

      # Collect pagination by rubrique
      number_pag <- map_dfr(.x = url_category,
                            .f = function(x){
                                tibble(url = x,
                                       nbr_pg= tryCatch(
                                             read_html(x) %>%
                                             html_node(".first_last_page") %>%
                                             html_text() %>% as.integer(),
                                             error = function(e){NA},
                                             warning=  function(e){NA})
                                             )})
      # Build all url
      number_pag <- na.omit(number_pag)
      int <- map_dfr(.x=number_pag$nbr_pg, .f=function(x){tibble(int=1:x)})
      url_pag_api <- paste0(number_pag$url,'page/', sort(int$int))


      # Collect article url by pagination
      art_api <- data.frame()
      for(i in 1:length(url_pag_api)){
        x <- url_pag_api[i]

        date_article = try(
                        read_html(x) %>%
                        html_nodes(".contenu-site .main-content .post-meta span") %>%
                        html_text())
        french.months <- c("janvier", "f\\u00e9vrier", "mars", "avril", "mai", "juin",
                           "juillet", "ao\\u00fbt", "septembre", "octobre", "novembre", "d\\u00e9cembre")
        date_article <- stringi::stri_replace_all_fixed(date_article, french.months, month.abb, vectorize_all=FALSE) %>% dmy_hm() %>% date()

        if(is.na(tail(date_article, n=1L))){
          next()
        }else{
        d_i <- cbind_na(
          date_article = date_article,
          url_article = tryCatch(
                          read_html(x) %>%
                          html_nodes(".contenu-site .main-content") %>%
                          html_nodes("h2 > a") %>% html_attr("href") %>% str_trim(),
                        error = function(e){NA},
                        warning=  function(e){NA}),
          title_article = tryCatch(
                          read_html(x) %>%
                          html_nodes(".contenu-site .main-content") %>%
                          html_nodes("h2 > a") %>% html_text() %>% str_trim(),
                        error = function(e){NA},
                        warning=  function(e){NA}))

        art_api <- rbind.data.frame(art_api, d_i)

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
  return(art_api)
}


  # Select article for time entries
  duration <- interval(as.Date.character(start_d), as.Date.character(end_d)) %>%  as.numeric('days')

  if(duration > 30){
    base::warning("warning: the request interval is greater than 30 days.!")
    article_ <- NA
  }
  else{
    if(press=="linfodrome"){
      article <- linfodrome(start_d, end_d)
      article_ <- article %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
    }else{
      if(press=="yeclo"){
        article <- yeclo(start_d, end_d)
        article_ <- article %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
      }else{
        if(press=="api"){
          article <- api(start_d, end_d)
          article_ <- article %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
        }else{
        article_1 <- linfodrome(start_d, end_d)
        article_2 <- yeclo(start_d, end_d)
        article_3 <- api(start_d, end_d)
        article_ <- rbind.data.frame(article_1,article_2,article_3) %>% filter(between(date_article, as.Date.character(start_d), as.Date.character(end_d)))
        }
      }
    }
  }

  # verified if article show
  if(nrow(article_)==0){
    base::warning("No article publish in this period!")
  }

  return(article_)
}









#' Main functions of collecting news from Côte Ivoir
#'
#' @description Function for collecting news (title + content) from the media in Côte Ivoir.
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
#' dt_news <- ci_news_contents(start_d="2020-10-20", end_d="2020-10-25",press='all')
#' head(dt_news)
#' @export
ci_news_contents <- function(start_d, end_d, press='all') {

  ######
  ###### linfodrome ###
  linfodrome_news_contents <- function(start_d, end_d){

    linfodrome_content <- ci_news_collect(start_d, end_d, press='linfodrome')
    content_collect <- map_dfr(.x = as.character(linfodrome_content$url_article),
                               .f = function(x){
                                 tibble(Content_article = paste0(tryCatch({
                                                          read_html(x) %>%
                                                          html_nodes("#k2Container .content") %>%
                                                          html_text()%>% str_trim()},
                                                          error = function(e){NA},
                                                          warning=  function(e){NA}),
                                                          collapse = " "),
                                        press_article = "linfodrome")})
    article_linfodrome<- cbind.data.frame(linfodrome_content,content_collect)
    return(article_linfodrome)
  }

  ######
  ###### yeclo ###
  yeclo_news_contents <- function(start_d, end_d){

    yeclo_content <- ci_news_collect(start_d, end_d, press='yeclo')

    #limitation
    if(nrow(yeclo_content)>100){
      yeclo_content_ <- yeclo_content[1:100,]
    }else{
      yeclo_content_ <- yeclo_content
    } # pourrais change avec le futur

    content_collect <- map_dfr(.x = as.character(yeclo_content_$url_article),
                               .f = function(x){
                                  tibble(
                                     Content_article = paste0(tryCatch({
                                                    read_html(x) %>%
                                                    html_node("article .td-post-content") %>% html_nodes("p") %>%
                                                    html_text(trim = TRUE)%>% str_trim()},
                                                    error = function(e){NA},
                                                    warning=  function(e){NA}
                                                    ),collapse = " "),
                                     press_article = "yeclo")

                                 })
    article_yeclo<- cbind.data.frame(yeclo_content_,content_collect)
    return(article_yeclo)
  }

  ######
  ###### api ###
  api_news_contents <- function(start_d, end_d){

    api_content <- ci_news_collect(start_d, end_d, press='api')
    content_collect <- map_dfr(.x = as.character(api_content$url_article),
                               .f = function(x){
                                 tibble(Content_article = paste0(tryCatch({
                                                          read_html(x) %>%
                                                          html_nodes(".contenu-site .main-content .container .single-post .post-content-area") %>%
                                                          html_text()%>% str_trim()},
                                                          error = function(e){NA},
                                                          warning=  function(e){NA}
                                                          ),collapse = " "),
                                        press_article = "api")})
    article_api<- cbind.data.frame(api_content,content_collect)
    return(article_api)
  }



  if(is.empty(start_d) || is.empty(end_d) ){
    base::warning("`start_d` `end_d` and can not be empty !")
    article_dt <- NA
  }else{
    if(press=='linfodrome'){
      article_dt <- linfodrome_news_contents(start_d,end_d)
    }else{
      if(press=='api'){
        article_dt <- api_news_contents(start_d,end_d)
      }else{
        if(press=='yeclo'){
          article_dt <- yeclo_news_contents(start_d,end_d)
        }else{
          article_dt_yeclo <- yeclo_news_contents(start_d,end_d)
          article_dt_linfodrome <- linfodrome_news_contents(start_d,end_d)
          article_dt_api <- api_news_contents(start_d,end_d)

          article_dt <- rbind(article_dt_linfodrome,article_dt_api,article_dt_yeclo)
        }
      }
    }
  }
  return(article_dt)
}


