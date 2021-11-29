library(scales)       # Additional graphics functions
library(RColorBrewer) # Color ramps for graphs and maps
library(gridExtra)    # Functions for arranging multiple plots on a page
library(RSocrata)     # Download or Upload 'Socrata' Data Sets
library(Cairo)        # Create high-quality vector (PDF, PostScript and SVG) and bitmap output
library(tidyverse)    # Collection of R packages designed for data science
library(zoo)          # Moving averages     
library(lubridate)    # Makes it easier to work with dates and times.


ces <- read.socrata(
  "https://data.edd.ca.gov/resource/r4zm-kdcg.json?$where=seasonally_adjusted = 'N' AND year >= '2018'") %>%
  filter(
    area_type == "Metropolitan Area" &
      (series_code == "10000000" | series_code == "20000000" | series_code == "30000000" |
         series_code == "40000000" | series_code == "50000000" | series_code == "55000000" |
         series_code == "60000000" | series_code == "65000000" | series_code == "70000000" |
         series_code == "80000000" | series_code == "90000000" | series_code == "0")) %>%
  select(msa = area_name, month = date, industry_title, estimated_employment = current_employment) %>%
  mutate (
    estimated_employment = as.numeric(estimated_employment),
    month = as.Date(month))

d <- paste(getwd(),"/Output/",format(max(ces$month), "%y-%m")," ",month.abb[month(max(ces$month))],sep="")
dir.create(d, showWarnings = FALSE)

industry_list <- unique(ces$industry_title)

quantiles <- ces %>%
  filter(month >= "2019-01-01") %>%
  group_by(msa, industry_title) %>%
  summarize(q1 = quantile(estimated_employment, probs = 0.25), 
            q2 = quantile(estimated_employment, probs = 0.5),
            q3 = quantile(estimated_employment, probs = 0.75))

for (industry in industry_list) {
  
  df <- ces %>%
    arrange(msa,industry_title,month) %>%
    inner_join(quantiles, by = c("msa" = "msa", "industry_title" = "industry_title")) %>%
    mutate(
      rolling_average = rollapply(estimated_employment,3,mean,align="right",fill=NA)) %>%
    filter(
      industry_title == industry &
        str_detect(msa, "Riverside|San Diego|Los Angeles|Anaheim") &
        month >= "2019-01-01") 
  
  min_employment = round(min(df$estimated_employment),0) - 100
  
  max_employment = round(max(df$estimated_employment),0) + 100
  
  emp_breaks = max_employment / 10
  
  colors <- c("Quartile 1"= "#c00000","Quartile 3"="#e2f0d9","Three-Month Rolling Average"="grey","Above"="#44546a","Below"="#c00000")
  
  rolling <-  ggplot(data = df) +
    geom_tile(
      stat = "identity",
      position = "identity",
      fill = "#e2f0d9", 
      alpha = ifelse(df$estimated_employment >= df$q3, 0.8,0),
      size = 1,
      na.rm = FALSE,
      show.legend = NA,
      height = max_employment * 2,
      aes(x=month, y=min_employment))+
    geom_tile(
      stat = "identity",
      position = "identity",
      fill = "#c00000", 
      alpha = ifelse(df$estimated_employment <= df$q1, 0.2,0),
      size = 1,
      na.rm = FALSE,
      show.legend = NA,
      height = max_employment * 2,
      aes(x=month, y=min_employment))+
    geom_point(aes(
      x = month, 
      y = estimated_employment,
      color = case_when(
        estimated_employment >= rolling_average ~ "Above",
        estimated_employment < rolling_average ~ "Below"))) +
    geom_line(aes(
      x = month,
      y = rolling_average,
      color ="Three-Month Rolling Average")) +
    coord_cartesian(ylim = c(0, max_employment)) +
    scale_y_continuous(
      label = comma,
      breaks = seq(0, max_employment, by = emp_breaks)) +
    scale_x_date(
      date_breaks  ="3 month",
      date_labels="%y %b") +
    labs(
      title = industry,
      subtitle = "Monthly Estimated Employment") +
    scale_color_manual(values = colors, ) +
    theme(text = element_text(colour = "#000000", size=14),
          title = element_text(color = "#00587C"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = NA),
          plot.background = element_rect(fill = "#FFFFFF"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = "top",
          plot.title = element_text(
            hjust = 0.5,
            color = "#00587C", 
            size = 18,
            face="bold"),
          plot.subtitle = element_text(
            hjust = 0.5,
            color = "#00587C", 
            size = 16,
            face="bold"),
          axis.title=element_text(size=16,face="bold")) +
    facet_wrap(vars(msa), ncol=2) +
    theme(
      strip.text.x = element_text(
        size = 14, colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1))
  
  file_name <- paste(d,"/",industry,".png",sep="")
  
  ggsave(rolling, filename = file_name, dpi = 300, type = 'cairo',
         width = 13, height = 8.5, units = 'in')
  
  print(file_name)
}


