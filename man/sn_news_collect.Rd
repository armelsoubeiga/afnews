% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sn_news.R
\name{sn_news_collect}
\alias{sn_news_collect}
\title{Function of collecting articles on Senegal press}
\usage{
sn_news_collect(start_d, end_d, press = "all")
}
\arguments{
\item{start_d}{yyyy-mm-dd start date of collection (oldest date, eg: yesterday)}

\item{end_d}{yyyy-mm-dd end date of collection (newest date, eg: today)}

\item{press}{specify the name of a press.}
}
\value{
a table containing the articles published in the period
}
\description{
Collection function of all the articles on Senegal
}
\examples{
library(afnews)
dt_ <- sn_news_collect(start_d="2020-10-28", end_d="2020-11-02", press="all")
head(dt_)
}
