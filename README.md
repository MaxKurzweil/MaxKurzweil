Sys.setlocale("LC_TIME", "English")
library(ggplot2)
library(anytime)
library(tidyverse)
library(svglite)

polls <- read.table("CAT.csv", header=T, sep=",", fileEncoding="UTF-8", stringsAsFactor=F)
polls$polldate <- as.Date(anydate(polls$polldate))

spansize <- 0.09            # general smoothing parameter for trend line
startdate <- '2017-09-24'   # date of previous election
enddate <- '2021-09-26'     # (latest) date of next election

# retrieve party names from CSV
party1 <- colnames(polls)[2]
party2 <- colnames(polls)[3]
party3 <- colnames(polls)[4]
party4 <- colnames(polls)[5]
party5 <- colnames(polls)[6]
party6 <- colnames(polls)[7]

# define party colors (taken from https://en.wikipedia.org/wiki/Category:Germany_political_party_colour_templates)
col1 <- '#000000'
col2 <- '#EB001F'
col3 <- '#009EE0'
col4 <- '#FFED00'
col5 <- '#BE3075'
col6 <- '#64A12D'

transp <-'55'       # transparency level of points

graph <- ggplot(polls)+
  geom_vline(xintercept = as.Date(startdate), color='#aaaaaabb')+       # vertical line (last election)
  geom_vline(xintercept = as.Date(enddate), color='#aaaaaabb')+         # vertical line (next election)
  geom_segment(aes(x=as.Date(startdate), xend=as.Date(enddate), y=5, yend=5), color='#666666bb', linetype='dashed')+      # horizontal line (election threshold 5%)
  # add poll points
  geom_point(aes_string(x='polldate',y=party1),size=ifelse(polls$polldate==startdate | polls$polldate==enddate,3,1.5),shape=ifelse(polls$polldate==startdate | polls$polldate==enddate,23,21),color=paste0(col1,transp),fill=paste0(col1,transp))+
  geom_point(aes_string(x='polldate',y=party2),size=ifelse(polls$polldate==startdate | polls$polldate==enddate,3,1.5),shape=ifelse(polls$polldate==startdate | polls$polldate==enddate,23,21),color=paste0(col2,transp),fill=paste0(col2,transp))+
  geom_point(aes_string(x='polldate',y=party3),size=ifelse(polls$polldate==startdate | polls$polldate==enddate,3,1.5),shape=ifelse(polls$polldate==startdate | polls$polldate==enddate,23,21),color=paste0(col3,transp),fill=paste0(col3,transp))+
  geom_point(aes_string(x='polldate',y=party4),size=ifelse(polls$polldate==startdate | polls$polldate==enddate,3,1.5),shape=ifelse(polls$polldate==startdate | polls$polldate==enddate,23,21),color=paste0(col4,transp),fill=paste0(col4,transp))+
  geom_point(aes_string(x='polldate',y=party5),size=ifelse(polls$polldate==startdate | polls$polldate==enddate,3,1.5),shape=ifelse(polls$polldate==startdate | polls$polldate==enddate,23,21),color=paste0(col5,transp),fill=paste0(col5,transp))+
  geom_point(aes_string(x='polldate',y=party6),size=ifelse(polls$polldate==startdate | polls$polldate==enddate,3,1.5),shape=ifelse(polls$polldate==startdate | polls$polldate==enddate,23,21),color=paste0(col6,transp),fill=paste0(col6,transp))+
  # add trend lines
  # the "span" (smoothing parameter) should be manually changed for individual parties that have less polling data
  geom_smooth(aes_string(x='polldate',y=party1,color=shQuote('col1')),span=spansize,se=FALSE)+
  geom_smooth(aes_string(x='polldate',y=party2,color=shQuote('col2')),span=spansize,se=FALSE)+
  geom_smooth(aes_string(x='polldate',y=party3,color=shQuote('col3')),span=spansize,se=FALSE)+
  geom_smooth(aes_string(x='polldate',y=party4,color=shQuote('col4')),span=spansize,se=FALSE)+
  geom_smooth(aes_string(x='polldate',y=party5,color=shQuote('col5')),span=spansize,se=FALSE)+
  geom_smooth(aes_string(x='polldate',y=party6,color=shQuote('col6')),span=spansize,se=FALSE)+
  scale_y_continuous(labels = function(x) paste0(x, "%"),limits=c(1,40))+    # add %, manual limits on y-axis
  scale_x_date(limits = as.Date(c(startdate,enddate)), date_minor_breaks = "1 months", date_breaks = "3 months", date_labels = "%b %Y")+    # grid: 1 month, labels: 3 months
  labs(x = "", y = "")+
  scale_color_manual(name="",
                     breaks = c('col1','col2','col3','col4','col5','col6'),
                     labels = c(party1,party2,party3,party4,party5,party6),
                     values = c('col1'=col1,'col2'=col2,'col3'=col3,'col4'=col4,'col5'=col5,'col6'=col6))+
  # legend appearance
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    legend.position="right",
    legend.key.width=unit(24, "pt"),
    legend.key.height=unit(24, "pt"),
    legend.text = element_text(size=16, margin = margin(b = 5, t = 5, unit = "pt")))

graph + theme()

ggsave(file="polls.svg", plot=graph, width=18, height=8)

# workaround since svglite doesn't properly work in Wikipedia
aaa=readLines("polls.svg",-1)
bbb <- gsub(".svglite ", "", aaa)
writeLines(bbb,"polls.svg")
