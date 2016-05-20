"
Aim: Analysis of Complaints Submitted to IPSO
Author: Gaurav Sood
Date: 5/20/2015
"
# set dir
setwd(githubdir)
setwd("ipso_facto/")


### Scrape IPSO 

# Scrape IPSO
library(rvest)
ipso <- read_html("https://www.ipso.co.uk/IPSO/rulings/IPSOrulings.html")

tabs <- 
  ipso %>% 
  html_nodes("table") %>%
  html_table(header=T)

### Clean the data

tab <- tabs[[1]]

tab$Complaint <- gsub("Â|â€|™|“ ", "", tab$Complaint)

tab$media_org <- sapply(strsplit(tab$Complaint, "\\sv\\s"), "[", 2)
tab$media_org <- sapply(strsplit(tab$media_org, "\\["), "[", 1) 
tab$media_org <- gsub("Â|\r\n", "", tab$media_org) 
tab$media_org <- gsub("\\s+$", "", tab$media_org) # remove trailing space
tab$media_org <- tolower(tab$media_org)

# Standardizing media:
tab$media_org[grepl("daily star|dailystar", tab$media_org)] <- "daily star"
tab$media_org[grepl("the sun |the sun$", tab$media_org)] <- "the sun"
tab$media_org[grepl("the daily telegraph|the sunday telegraph|telegraph.co.uk", tab$media_org)] <- "the telegraph"
tab$media_org[grepl("the times$|the sunday times$", tab$media_org)] <- "the times"
tab$media_org[grepl("daily mail$|the mail on sunday|mail online|mail online|sunday mail|the mail$", tab$media_org)] <- "the mail"
tab$media_org[grepl("daily mirror$|mirror.co.uk|sunday mirror$", tab$media_org)] <- "the mirror"
tab$media_org[grepl("daily express$|express.co.uk|sunday express$", tab$media_org)] <- "the express"

# Appeal held/in part
tab$Upheld[grepl("was not upheld", tab$Conclusions)] <- 0
tab$Upheld[grepl("was upheld", tab$Conclusions)] <- 1
write.csv(tab, file="data/ipso_complaints.csv", row.names=F)

### Calculate Basic Stats
# Batting average
library(plyr)
upheld_tab <- ddply(tab, ~media_org, summarise, total_complaints=length(media_org), total_upheld = sum(Upheld))
upheld_tab$prop_upheld <- upheld_tab$total_upheld/upheld_tab$total_complaints
upheld_tab <- subset(upheld_tab, !is.na(prop_upheld))

# Plot top 20
media_tab <- table(tab$media_org)
media_tab2 <- setNames(data.frame(media_tab[order(-media_tab)][1:20]), c("Media", "Frequency"))
media_tab2$Media <- factor(media_tab2$Media, levels=media_tab2$Media[order(media_tab2$Frequency)], ordered=TRUE) #reordering

### Total Complaints Received

# The plot
library(ggplot2)
library(scales)
library(grid)

ggplot(media_tab2, aes(Frequency, Media)) +
geom_point() + 
ylab("") + 
xlab("Total Number of Complaints") + 
theme_minimal(base_size=9) +
theme(panel.grid.major=element_line(color="#F0F0F0",size=.25)) +
theme(panel.grid.minor=element_blank()) +
theme(axis.ticks=element_blank()) +
theme(legend.position="none") +
theme(plot.title=element_text(color="#525252", size=10, vjust=1.25)) +
theme(axis.text.x=element_text(size=9, color="#636363")) +
theme(axis.text.y=element_text(size=9, color="#636363")) +
theme(axis.title.x=element_text(size=10, color="#323232", vjust=0)) +
theme(axis.title.y=element_text(size=10, color="#323232", vjust=1.25)) +
theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
ggsave("figure/ipso_n_complaints.pdf")
ggsave("figure/total_complaints-1.png")

### Total Complaints Upheld 

n_upheld <- setNames(upheld_tab[order(-upheld_tab$total_upheld)[1:20],c("media_org", "total_upheld")], c("Media", "Frequency"))
n_upheld$Media <- factor(n_upheld$Media, levels=n_upheld$Media[order(n_upheld$Frequency)], ordered=TRUE) #reordering

ggplot(n_upheld, aes(Frequency, Media)) +
geom_point() + 
theme_minimal(base_size=9) +
ylab("") + 
xlab("Total Number of Complaints Partly or Fully Upheld") + 
scale_x_continuous(limits=c(0, 10), breaks = c(0,5,10), labels = c("0", "5", "10")) + 
theme(panel.grid.major=element_line(color="#F0F0F0",size=.25)) +
theme(panel.grid.minor=element_blank()) +
theme(axis.ticks=element_blank()) +
theme(legend.position="none") +
theme(plot.title=element_text(color="#525252", size=10, vjust=1.25)) +
theme(axis.text.x=element_text(size=9, color="#636363")) +
theme(axis.text.y=element_text(size=9, color="#636363")) +
theme(axis.title.x=element_text(size=10, color="#323232", vjust=0)) +
theme(axis.title.y=element_text(size=10, color="#323232", vjust=1.25)) +
theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
ggsave("figure/ipso_n_upheld.pdf")
ggsave("figure/total_upheld-1.png")

### Batting Average of Media Organizations with most complaints against them

# Batting average of the top 20

p_upheld    <-  setNames(upheld_tab[upheld_tab$media_org %in% media_tab2$Media, c("media_org", "prop_upheld")], c("Media", "Proportion"))
p_upheld$Media <- factor(p_upheld$Media, levels=p_upheld$Media[order(p_upheld$Proportion)], ordered=TRUE) #reordering

ggplot(p_upheld, aes(Proportion, Media)) +
geom_point() + 
theme_minimal(base_size=9) +
ylab("") + 
xlab("Proportion of Complaints Upheld for Organizations with Most Complaints") + 
scale_x_continuous(limits=c(0, 1), breaks = c(0,.25, .5, .75, 1.0), labels = c("0",".25", ".5", ".75", "1")) + 
theme(panel.grid.major=element_line(color="#F0F0F0",size=.25)) +
theme(panel.grid.minor=element_blank()) +
theme(axis.ticks=element_blank()) +
theme(legend.position="none") +
theme(plot.title=element_text(color="#525252", size=10, vjust=1.25)) +
theme(axis.text.x=element_text(size=9, color="#636363")) +
theme(axis.text.y=element_text(size=9, color="#636363")) +
theme(axis.title.x=element_text(size=10, color="#323232", vjust=0)) +
theme(axis.title.y=element_text(size=10, color="#323232", vjust=1.25)) +
theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
ggsave("figure/ipso_p_upheld.pdf")
ggsave("figure/batting_av-1.png")
