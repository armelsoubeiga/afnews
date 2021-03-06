% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bn_news.R
\name{bn_news_contents}
\alias{bn_news_contents}
\title{Main functions of collecting information on Benin}
\usage{
bn_news_contents(start_d, end_d, press = "all")
}
\arguments{
\item{start_d}{yyyy-mm-dd start date of collection (oldest date, eg: yesterday)}

\item{end_d}{yyyy-mm-dd collection end date (newest date, eg: today)}

\item{press}{specify the name of a press.}
}
\value{
a table containing the news (title + content + date publisher + press) collected
}
\description{
Function for collecting news (title + content) from the media in Benin.
}
\examples{
library(afnews)
dt_news <- bn_news_contents(start_d="2020-11-01", end_d="2020-11-10", press='all')
head(dt_news)
}
