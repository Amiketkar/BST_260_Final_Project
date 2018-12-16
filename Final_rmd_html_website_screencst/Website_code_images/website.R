library(shiny)
library(gridExtra)
library(XML)
library(reshape2)
library(plyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(openintro)
library(usmap)
library(gridExtra)
library(grid)
library(dslabs)
library(ggrepel)
library(ggthemes)
library(Hmisc)
library(datasets)
library(shinyWidgets)

ui <- tagList(
  navbarPage(
    theme = shinythemes::shinytheme("sandstone"),
    "BST260 Final Project: Media & Suicide", 
    tabPanel("Background & Motivation",
             tags$h1("How Media Impacts Suicide in US ?"),
             tags$h4("With increase in rates of suicides, media attention for this issue has increased over the years. 
                     A review of recent events in Austria and Switzerland indicates that suicide prevention organizations can successfully convince the media to change the 
                     frequency and content of their suicide coverage in an effort to reduce copycat effects"),
             tags$h4("Therefore, we are interested in analyzing and tracking how changes in media coverage of suicide and suicide-related 
                     topics over time impact actual suicide rates in the United States."),
             column(8, offset=2, tags$video(src="video.mp4", type="video/mp4", autoplay=NA, controls="controls", height=500, width=800)),
             #column(8, offset=2, tags$img(src="cloud.png", height=500, width=800)),
             #column(4, offset=8, tags$h5("Contributors: Amita Ketkar, Jui Kothari, Xinye Qiu, Yiwen Zhang")),
             column(8, offset=8, tags$h5("Contact us at:",tags$a("yzhang@hsph.harvard.edu")) ),
             column(8, offset=8, tags$h5("More information:", tags$a("https://github.com/Amiketkar/BST_260_Final_Project"))),
             tags$br(),
             tags$br()
             
             
             
             ),
    tabPanel("Media Coverage",
             fluidRow(
               column(2, tags$img(src="media_cloud.png", height=150, width=150)),
               tags$h1("Media Coverage on Suicide"),
               column(4,offset=2,tags$h4("—— data from Media Cloud"))
             ),
             tags$br(),
             tags$hr(),
             fluidRow(
               column(6, offset=2,
                      imageOutput("media_map")),
               column(2, 
                      sliderInput("year_media_input", "Choose a Year:(or click to see animation)", 
                                  min=2011, max=2018, value=1, step=1,animate = TRUE)
               )
             ),
             tags$br(),
             tags$hr(),
             
             fluidRow(
               mainPanel(plotOutput("media_count_year", click = "plot1_click")),
               sidebarPanel("Click each spike to see number of articles about suicide published on that date:",
                            verbatimTextOutput("map1_detail"))
             ),
             
             tags$br(),
             tags$hr(),
             fluidRow(
               mainPanel(plotOutput("media_ratio_year", click = "plot2_click")),
               sidebarPanel("Click each spike to see the ratio of articles about suicide published on that date:",
                            verbatimTextOutput("map2_detail"))
             ),
             
             tags$br(),
             tags$hr(),
             
             fluidRow(
               column(8, plotOutput("p_top_media"))
             ),
             tags$hr(),
             tags$br(),
             
             fluidRow(
               column(8, plotOutput("p_top_suicide"))
             ),
             tags$br(),
             tags$br()
    ),
    
    tabPanel("Suicide data from CDC", 
             fluidRow(
               column(2, tags$img(src="cdc.png", height=80, width=160)),
               tags$h1("US Suicide Situation"),
               column(4,offset=2,tags$h4("—— data from CDC Wonder"))
             ),
             tags$br(),
             tags$hr(),
             fluidRow(
               column(6, offset=2, 
                      imageOutput("cdc_map")),
               column(2, 
                      sliderInput("year_input", "Choose a Year:(or click to see animation)", min=2011, max=2016, value=1, step=1, animate = TRUE)
               )
               
             ),
             tags$br(),
             tags$hr(),
             fluidRow(
               column(8, offset=2, plotOutput("national_trend"))
             ),
             tags$br(),
             tags$hr(),
             fluidRow(
               column(8, offset=2, plotOutput("race_gender"))
             ),
             tags$br(),
             tags$br()
             
             
    ),
    
    tabPanel("Media & Suicide", 
             tags$br(),
             tags$br(),
             fluidRow(
               column(8, offset=2, imageOutput("combined_map"))
             ),
             tags$br(),
             fluidRow(
               column(4, offset=4, wellPanel(
                 selectInput("select_state", "Choose a State:",
                             c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC",
                               "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN",
                               "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN",
                               "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", 
                               "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI",
                               "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
                 )
               ))
               
             )
             
             
    )
    )
)

media <- read.csv("suicid-or-suicid-and-sampled-stories-20181213194457.csv")
media2 <- read.csv("suicid-or-suicid-and-stories-over-time-20181213191601.csv")
#Extract the year information from the publication date
media <- media %>% mutate(date = ymd_hms(publish_date))
media <- media %>% separate(date, c("date", "hour_min"), " ")
media <- media %>% mutate(year = year(date), month = month(date), day = day(date))
media <- media %>% filter(language == "en")
#Separate the publication state (state, year) into 2 separate variables: state and country
media <- media %>% separate(media_pub_state, c("state", "country"), ",")
#Create a variable with state abbreviations from the state name
media <- media %>% mutate(state_abb = ifelse(nchar(state)>2, state.abb[match(state, state.name)], state))
#Find titles that include the term 'suicid' and create a new variable "suicide_title"" that returns 1 if "suicid" is present, else 0
media <- media %>% mutate(suicide_title = ifelse(str_detect(title, "suicid"), 1, 0))


cdc <- read.csv("new_cdc.csv")
full <- read.csv("Full.csv")



server <- function(input, output) {
  
  # count plot
  output$media_count_year <- renderPlot({
    media2 %>% 
      ggplot(aes(date, count,group = 1)) + 
      geom_line(colour = "darkorchid4") + 
      scale_x_discrete(breaks = c("2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01"), labels = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")) +
      xlab("Year")+ ylab("Count") + ggtitle("Suicide-Related Articles Published on Each Day") + theme_bw() +
      theme(plot.title=element_text(size=20, face="bold"),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            text=element_text(size=15)
      )
  })
  
  media2_plot1_click <- reactive({
    nearPoints(media2, input$plot1_click, maxpoints = 1)
  })
  output$map1_detail <- renderPrint({
    media2_plot1_click()$count
  })
  
  
  # ratio plot 
  output$media_ratio_year <- renderPlot({
    media2 %>% 
      ggplot(aes(date, ratio,group = 1)) + 
      geom_line(colour = "darkorchid4") + 
      scale_x_discrete(breaks = c("2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01"), labels = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")) +
      xlab("Year")+ ylab("Ratio") + ggtitle("Ratio of Suicide-Related Articles to All Articles Published on Each Day") + theme_bw() +
      theme(plot.title=element_text(size=20, face="bold"),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            text=element_text(size=15)
      )
  })
  
  media2_plot2_click <- reactive({
    nearPoints(media2, input$plot2_click, maxpoints = 1)
  })
  output$map2_detail <- renderPrint({
    media2_plot2_click()$ratio
  })
  
  # Media_USMAP plots
  
  media_year_data <- reactive({
    media %>% group_by(state_abb) %>%
      filter(year == input$year_media_input) %>% 
      summarise(num_title = length(title))  %>%
      mutate(state = state_abb) %>%
      filter(state != "") %>%
      filter(!is.na(state))
  })
  
  
  output$media_map <- renderPlot({
    plot_usmap(data = as.data.frame(media_year_data()), values = "num_title") + 
      scale_fill_gradientn(limits = c(0,700), colours=c("white", "red", "red1", "red2", "red3", "red4"), name = "# of Stories") +
      theme(legend.position="right") + ggtitle("Media Coverate Across US States by year") +
      theme(plot.title=element_text(size=20, face="bold"),
            legend.text=element_text(size=15),
            legend.title=element_text(size=15))
    
  })
  
  # top 10 media  
  top_media <-media %>% 
    group_by(media_name) %>%
    summarise(total = length(title))%>%
    arrange(desc(total))
  
  output$p_top_media <- renderPlot({
    ggplot(data = subset(top_media, total %in% total[1:10]), aes(media_name, total, group = 1)) + 
      geom_bar(stat = "identity") + 
      coord_flip() + 
      xlab("Media Name") + ylab("Number of Articles") + 
      ggtitle("Top 10: Media Sources with Articles on Suicide (2011-2018)") + theme_bw() +
      theme(plot.title=element_text(size=20, face="bold"),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            text=element_text(size=15)
      )
  })
  
  # top 10 suicide media  
  top_suicide <- media %>%
    group_by(media_name) %>% 
    summarise(total = sum(suicide_title)) %>%
    arrange(desc(total))
  
  output$p_top_suicide <- renderPlot({
    ggplot(data = subset(top_suicide, total %in% total[1:10]), aes(media_name, total, group = 1)) + 
      geom_bar(stat = "identity") + 
      coord_flip() + 
      xlab("Media Name") + ylab("Number of Articles") +  
      ggtitle("Top 10: Media Sources with 'Suicide' in the Article Titles (2011-2018)") + theme_bw() +
      theme(plot.title=element_text(size=20, face="bold"),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            text=element_text(size=15)
      )
  })
  
  
  #create subset and have national average for each year
  cdc_national=cdc %>% group_by(year) %>% mutate(national_rate=mean(crude_rate)) %>% arrange(year)
  cdc_national=cdc_national %>% select('year','national_rate')
  cdc_national=distinct(cdc_national)
  #line plots with points and texts
  output$national_trend <- renderPlot({
    cdc_national %>% 
      ggplot(aes(x=year, y=national_rate))+
      geom_line(stat='identity',size=1)+
      geom_point(size=4)+
      geom_text_repel(aes(label=round(national_rate, digits=0)), nudge_x=0.50, nudge_y=1.0, size=3)+
      xlab('Year')+
      ylab('National Suicide Rate (Counts/100,000 population)')+
      ggtitle('US National Suicide Rate Change with Year')+
      theme_bw()+
      scale_x_continuous(breaks=seq(1999,2016,by=1))+
      theme(plot.title = element_text(hjust = 0.5, size=20, face="bold"))
  })
  
  
  
  #restricting to recent years 2011-2016 and present average
  cdc_subpop=cdc %>% filter(year %in% c('2011','2012','2013','2014','2015','2016'))
  cdc_subpop=cdc_subpop %>% 
    group_by(year,gender,RACE) %>% 
    mutate(national_rate=mean(crude_rate)) %>% 
    arrange(year,gender,RACE)
  cdc_subpop=cdc_subpop %>% select('year','gender','RACE','national_rate')
  cdc_subpop=distinct(cdc_subpop)
  #pyramid plots from year 2011 to 2016
  output$race_gender <- renderPlot({
    ggplot(data = cdc_subpop, 
           mapping = aes(x = RACE, fill = gender, 
                         y = ifelse(test = gender == "Male", 
                                    yes = -national_rate, no = national_rate))) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = abs, limits = max(cdc_subpop$national_rate) * c(-1,1)) +
      labs(y = "Population")+
      facet_wrap(~year)+
      coord_flip()+
      theme_bw()+
      ylab('National Suicide Count per #100,000')+
      xlab('Race Ethnicity')+
      theme(plot.title = element_text(size=20, face="bold"),
            axis.title.x = element_text(size=15, face="bold"), 
            axis.title.y = element_text(size=15, face="bold"))+
      ggtitle('Race-Gender Grouped US National Suicide Rate')+
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  
  cdc_year_data <- reactive({
    cdc %>% filter(year == input$year_input) %>% 
      group_by(state) %>% 
      mutate(state_rate=mean(crude_rate)) %>% 
      select('year','state','state_abb','state_rate')
    #cdc_state2011=distinct(cdc_state2011)
    
  })
  
  output$cdc_map <- renderPlot({
    plot_usmap(data = cdc_year_data(), values = "state_rate", lines = "grey") + 
      scale_fill_continuous(limits = c(0, 400), name = "Count per #100,000", low = "white", high = "red")+
      theme(legend.position = "right", legend.title=element_text(size=15), legend.text=element_text(size=15), plot.title = element_text(hjust=0.5, size=20, face='bold'))+
      ggtitle('Averaged Suicide Rate Across US States by year')
    
  })
  
  
  # combined_map
  
  full_state <- reactive({
    full %>% filter(state_abb == input$select_state) 
  })
  
  
  output$combined_map <- renderPlot({
    
    ggplot(data=full_state(), aes(x = year)) + 
      geom_line(aes(y = rate_pmil, group = 1, colour = "Sucide Death Rate"), size = 0.75) + 
      geom_point(aes(y = rate_pmil, group = 1, colour = "Sucide Death Rate"), size = 2) +
      geom_line(aes(y = articles/2, group = 1, colour = "Number of Articles"), size = 0.75) +
      geom_point(aes(y = articles/2, group = 1, colour = "Number of Articles"), shape = 18, size = 3) +
      scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Number of Articles on Suicide")) +
      xlab("Year") + ylab("Suicide Death Rate per million people") + 
      ggtitle("Media Coverage and Suicide Trends by State") + theme(legend.title = element_blank()) + theme_bw() +
      theme(plot.title=element_text(size=20, face="bold", hjust=0.5, vjust=1),
            legend.position="bottom",
            legend.text=element_text(size=15),
            legend.title=element_text(size=15),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15, vjust=1),
            text=element_text(size=15))
    
  })
  
}

shinyApp(ui = ui, server = server)
