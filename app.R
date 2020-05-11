library(shiny)
# install.packages('shinyjs')
# library(shinyjs)
# install.packages("shinythemes")
library(shinythemes)

# To be able to deploy the shiny app
library(sf)
library(lwgeom)
### loading the rsconnect to be able to publish my app
# Step 1: load rsconnect
library(rsconnect)

# Step 2: Get the token and paste it as a command line
# Step 3: deploy App
# deployApp()

# install previous version of lwgeom. THis is what I have used after using "brew install postgis" in theterminal
# install.packages("https://cloud.r-project.org/src/contrib/Archive/lwgeom/lwgeom_0.2-1.tar.gz", repo=NULL, type="source")

# Activating packages ----
# install.packages('tmap')
library(ggplot2)
library(plotly)
library(sf)
library(sp)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(dplyr)
library(RColorBrewer)
# display.brewer.all()
# brewer.pal(n = 6, name = "PuBu")

# And this viz packages
library(devtools)
# install_github("mtennekes/tmaptools")
# install.packages('mapview')
#devtools::install_github("Nowosad/spDataLarge")
library(tmaptools)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(shiny)   # for web applications
#install.packages('rgdal')
library(rgdal)
# install.packages('rnaturalearth')
library(rnaturalearth)
library(tidyverse)

# libraries for Data Viz
library("bbplot")
library(magick)
library(gifski)
library(gganimate)
library(tidyr)
library(gapminder)
library(ggalt)
library(forcats)
library(R.utils)
library(png)
library(grid)
library(ggpubr)
library(scales)


# Data
data(World,world,land, rivers,metro)
dataset <- read.csv("Data.csv")
dataset = dataset[,2:9]
dataset = dataset[,-3]
dataset = dataset[,-2]

# Creating the maps ----

uganda = world[world$name_long == "Uganda",]
# uganda= st_union(uganda) 
zambia = world[world$name_long == "Zambia",]
zimbabwe = world[world$name_long == "Zimbabwe",]
namibia = world[world$name_long == "Namibia",]

# group: name of te group to which this layer belongs in view mode
uganda_borders = tm_shape(uganda) +
    tm_borders(lwd = 3,col = "red", group = "uganda")

namibia_borders = tm_shape(namibia) +
    tm_borders(lwd = 3,col = "red",group = "namibia") 

zambia_borders = tm_shape(zambia) +
    tm_borders(lwd = 3,col = "red", group = "zambia")
zimbabwe_borders = tm_shape(zimbabwe) +
    tm_borders(lwd = 3,col = "red", group = "zimbabwe")

my_countries_borders = uganda_borders +
    namibia_borders +
    zambia_borders +
    zimbabwe_borders

Africa <- World[World$continent == "Africa",]
Europe <- World[World$continent == "Europe",]
Europe <- Europe %>% 
    filter(name != "Russia")
North_America <- World[World$continent == "North America",]
South_America <- World[World$continent == "South America",]
Asia <- World[World$continent == "Asia",]
Oceania <- World[World$continent == "Oceania",]


# Land for Africa ----
pal8 <- c("#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4", "#FFED6F", "#7A0177", "#E6E6E6", "#A6CEE3")
mymap_africa = tm_shape(land, bbox = "Africa") + #,ylim = c(-88,88), relative=FALSE
    tm_raster("cover_cls", palette = pal8, title="Africa Land Cover", legend.hist=TRUE, legend.hist.z=0) +
    tm_shape(Africa, is.master = TRUE) +
    tm_borders() +
    tm_legend(text.size=1,
              title.size=1.2,
              position = c("left","bottom"),
              bg.color = "white",
              bg.alpha=.2,
              frame="gray50",
              height=.6,
              hist.width=.2,
              hist.height=.2,
              hist.bg.color="gray60",
              hist.bg.alpha=.5) +
    tm_scale_bar() +
    tm_view(view.legend.position = c("left","bottom")) +
    my_countries_borders

mymap_africa

# Sending my map to leaflet
mymap_africa_land_lf = tmap_leaflet(mymap_africa) %>%
    addLayersControl(overlayGroups = c("uganda","namibia","zambia","zimbabwe"),
                     position = "topleft",
                    options = layersControlOptions(collapsed = FALSE)
    ) %>% addTiles(attribution = "")

# Map Arrivals ----

# Preparing the data for continents
dataset$countries = ifelse(
    dataset$Subregion == "United States of America", "United States",
    ifelse(
        dataset$Subregion == "Tanzania, United Republic of", "Tanzania",  as.character(dataset$Subregion)
    )
)


# Joining the dataset with the world to create the df world_arrivals
world_arrivals = inner_join(World,dataset,by = c("name" = "countries"))

# Function bbc_style modified resizing the title and the axis text ----

#' Add bbc theme to ggplot chart
#'
#' This function allows you to add the bbc theme to your ggplotgraphics.
#' @keywords bbc_style_mod
#' @export
#' @examples
#' line <- ggplot(line_df, aes(x = year, y = lifeExp)) +
#' geom_line(colour = "#007f7f", size = 1) +
#' geom_hline(yintercept = 0, size = 1, colour="#333333") +
#' bbc_style_mod()

bbc_style_mod <- function() {
    font <- "Helvetica"
    
    ggplot2::theme(
        
        #Text format:
        #This sets the font, size, type and colour of text for the chart's title
        plot.title = ggplot2::element_text(family=font,
                                           size=16,
                                           face="bold",
                                           color="#222222"),
        #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
        plot.subtitle = ggplot2::element_text(family=font,
                                              size=10,
                                              margin=ggplot2::margin(9,0,9,0)),
        plot.caption = ggplot2::element_blank(),
        #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
        
        #Legend format
        #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
        legend.position = "top",
        legend.text.align = 0,
        legend.background = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(family=font,
                                            size=10,
                                            color="#222222"),
        
        #Axis format
        #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(family=font,
                                          size=10,
                                          color="#222222"),
        axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
        axis.ticks = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        
        #Grid lines
        #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
        panel.grid.major.x = ggplot2::element_blank(),
        
        #Blank background
        #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
        panel.background = ggplot2::element_blank(),
        
        #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
        strip.background = ggplot2::element_rect(fill="white"),
        strip.text = ggplot2::element_text(size  = 10,  hjust = 0)
    )
}

# ui ----

ui <- bootstrapPage(
    # useShinyjs(),
    # theme = "darkly.css",
    # titlePanel(tags$h1("A trip to Africa")),
            # Creates a list to navigate
            navbarPage(theme = shinytheme("flatly"), #collapsible = TRUE,

                     # Name
                    "Inbound arrivals to Africa, 2013-2018", 

                    
                    tabPanel("Intro",
                                        tabPanel("General information",
                                                  tags$div(
                                                      tags$h4("Key objectives of this site"),
                                                      
                                                      tags$ul(h5("This site aims to:")),
                                                      # ,tags$br(),tags$br(),
                                                      tags$li(h5("Get to know the distribution of Land Cover of the African continent.")),
                                                      # ,tags$br(),tags$br(),
                                                      tags$li(h5("Analyse the origen countries of the arrivals at the borders of four countries in particular: Uganda, Zambia, Zimbabwe and Namibia for the period covering from 2013 to 2018. The arrivals at the borders of the commented countries are analysed through a series of interactive maps and graphs and both high level and in-depth insights can be derived from views provided. This is accomplished in the tabs \"Arrivals: A general view\" and \"Arrivals by continent\"")),
                                                      tags$li(h5("Look at the evolution of arrivals through the period analysed. To achieve this, interactive views showing the trend by Continent, Region and Country/Subregion are displayed. And the bonus bit is that all of these graphs are done in the BBC way thanks to the BBC Visual and Data team, visit", tags$a(href = "https://medium.com/bbc-visual-and-data-journalism/how-the-bbc-visual-and-data-journalism-team-works-with-graphics-in-r-ed0b35693535", "How the BBC Visual and Data Journalism team works with graphics in R"),"for more information."))
                                                      ,tags$br(),
                                                      tags$h4("Background & The Process"),
                                                      "The last 4th of April I was Furloughed from my job at British Airways. At that point in time I tried to look out for opportunities to improve my skillset and to help others. I think \"Furlough time\" is a good time to both reflect on your future objectives and to help others whilst 
                                                      taking care of your wellbeing as much as possible.",
                                                      tags$br(),
                                                      "It happens to me that one of my flatmates works for an important British touroperator which main market is Africa, and I was curious about the African continent, hence wanted to discover more about it! 
                                                      All in all, my main objectives were to increase my knowledge about Africa and help my friend to understand the market share and the trend of arrivals by country at the borders of a set of African countries.",
                                                      tags$br(),
                                                      "I needed data, so I started looking out on the internet for tourism data for African countries  and I found the", tags$a(href="https://www.unwto.org","World Tourism Organization webapage"), "and in particular the", tags$a(href = "https://www.e-unwto.org/toc/unwtotfb/current","Tourism statistics by Economic Group: UNECA"), "with country specific data on arrivals, basic indicators and Outbound tourism 
                                                       from where I was able to extract the data from Excel files with tables set in a compact form",tags$br(),
                                                      tags$br(),
                                                      "These are the steps that were taken to get to the final outcome:",
                                                      tags$ol(""),
                                                      tags$li("Download the data from the website in several Excel files. Set them in a subfolder in order to use Power Query functionality to create a unique table and unpivot columns"),
                                                      tags$li("Preprocesing and Wrangling the data using the dplyr R package and the knowledge gained from the course Jumpstart with R from", tags$a(href = "https://university.business-science.io", "Business Science University."), "This included dealing with different country names from the dataset 
                                                              and the World dataset provided from the \"tmap\" R package. Did you know that you could find either \"Tanzania\" or \"Republic Democratic of Tanzania\"? Interesting!"),
                                                      tags$li("Discover the wonderful world of thematics maps and leaflets by getting in-depth knowledge of the \"tmap\" and \"leaflet\" R packages."),
                                                      tags$li("Solving the question that one always wonder: \"How am I going to show my work to others?\" For this the solution chosen was R Shiny, which was a tedious work to get it up and running but it has provided me a lot of flexibility and allowed 
                                                              me to get a sense of HTML, CSS and Bootstrap themes. And I ended up enjoying it a lot!"),
                                                      tags$br(),
                                                      tags$h4("Code"),
                                                      "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href = "https://github.com/pmascaro", "Github."),
                                                      tags$br(),tags$br(),tags$h4("Sources") ,
                                                       tags$a(href="https://shiny.rstudio.com/gallery/covid19-tracker.html", "Covid-19 tracker"),"A Shiny app built by Edward Parker to track the Covid-19 cases",
                                                      tags$br(),
                                                      tags$a(href = "https://shiny.rstudio.com/tutorial/", "Learn Shiny"),
                                                      tags$br(),
                                                      tags$h4("Author"),
                                                      "Pep Martí Mascaró Monserrat, visit my", tags$a(href = "https://www.linkedin.com/in/pep-mart%C3%AD-mascaró-monserrat-44325289/","Linkedin"), "and my",tags$a(href = "https://github.com/pmascaro", "Github"),
                                                      tags$br(),
                                                      tags$h4("Contact"),
                                                      div("pmmascaro@gmail.com")
                                                
                                                
                                                  )
                                        )
                    ),
                
                    # "Dashboards",
                    tabPanel("Africa: Land Cover map",
                             div(class = "outer",
                                draggable = TRUE, 

                                # Find the countries of Study
                                leafletOutput( "mymap_africa_leaflet", width= "100%", height = "625" ),
                                
                                
                                tags$head(tags$style(
                                    HTML('
                                         #input_date_control {background-color: rgba(0,0,255,0.2);;}
                                         #sel_date {background-color: rgba(0,0,255,1);}')
                                )),
                                absolutePanel(id = "controls", class = "panel panel-default",
                                               top = 120, left = 150, width = 300, fixed=TRUE,
                                               draggable = TRUE, height = "auto",
                                                tags$h6(tags$style("#controls{background-color:#ffffff;
                                                                   opacity:0.6}"
                                                )),
                                                tags$h3("Africa Land Cover map"),
                                                tags$h5("This map represents the African continent and its countries"),
                                                tags$h5("The countries highlighted in red are the ones chosen for analysing the number of arrivals at its borders period from 2013 to 2018", 
                                                style="color:#006d2c")
                                )
                            )
                    ),
                    tabPanel("Arrivals: A general view",
                        sidebarLayout(
                            div( id = "Sidebar", sidebarPanel(
                                    span(tags$i(h5("The map and the graph show the number of arrivals to the chosen country",style="color:#045a8d")),
                                    span(tags$i(h5("The borders of the country of arrival are highlighted in red")),style="color:#045a8d")),
                                    span(tags$i(h5("Now choose a country of arrival, a continent of departure and a year or set of years to see the number of arrivals by country")),
                                            style="color:#045a8d"),
    
    
                                    radioButtons("country_sel",
                                                "Country of arrival:",
                                                      # choices = c("Uganda", "Zambia","Zimbabwe","Namibia"),
                                                choices = world_arrivals$Country[!duplicated(world_arrivals$Country)]),
                                         # selected = c("name"),
                                         # multiple = FALSE)
                                    selectInput("continent_sel",
                                                 "Continent of depature:",
                                                 # choices = c("Africa","Europa","North America","South America","Asia"),
                                                 choices = world_arrivals$continent[!duplicated(world_arrivals$continent)],
                                                 selected = world_arrivals$continent[!duplicated(world_arrivals$continent)],
                                                 multiple = TRUE),
    
                                    selectInput("year_sel",
                                                 "Year:",
                                                 choices = world_arrivals$Year[!duplicated(world_arrivals$Year)],
                                                 selected = world_arrivals$Year[!duplicated(world_arrivals$Year)],
                                                 multiple = TRUE)

                                    ,width = 3)
                            ),
                            mainPanel(
                                     div( leafletOutput("mymap_world_lf", width= "100%", height = "350") ),
                                     div(tags$br(),
                                         textOutput("title1"),
                                         tags$h6(tags$style("#title1{color: black;
                                                            font-size: 24px;
                                                            font-style: bold;
                                                            font-family: Helvetica;
                                                            margin-left:4.5em;
                                                            }"
                                                         )
                                         ),
                                         plotlyOutput("world_top_arrivals", width = "100%", height = "200",inline = TRUE)
                                    )
                            ,width = 9)
                        )
                    ),
            
                    tabPanel("Arrivals by continent",
                             
                        sidebarLayout(
                            sidebarPanel(
                                span(tags$i(h5("Choose a country of arrival, a continent of departure and a year or set of years to see the number of arrivals by country")),
                                     style="color:#045a8d"),
                                span(tags$i(h5("The borders of the country of arrival are highlighted in red")),
                                     style="color:#045a8d"),

                                radioButtons("country_select",
                                             "Country of arrival:",
                                             choices = world_arrivals$Country[!duplicated(world_arrivals$Country)]),
                                 
                                radioButtons("continent_select",
                                             "Continent of depature:",
                                             choices = world_arrivals$continent[!duplicated(world_arrivals$continent)]),

                                selectInput("year_select",
                                                     "Year:",
                                                     choices = world_arrivals$Year[!duplicated(world_arrivals$Year)],
                                                     selected = world_arrivals$Year[!duplicated(world_arrivals$Year)],
                                                     multiple = TRUE)
                                
                            ,width = 3),
                             mainPanel(
                                 div(leafletOutput("mymap_continent_lf", width= "100%", height = "350")),
                                 div( tags$br(),
                                     textOutput("title2"),
                                     tags$h6(tags$style("#title2{color: black;
                                                         font-size: 28px;
                                                         font-style: bold;
                                                         font-family: Helvetica;
                                                         margin-left:4.5em;
                                                         }"
                                                         )
                                    ),
                                    plotlyOutput("continent_top_arrivals", width = "100%", height = "200", inline = TRUE)
                                )
                                ,width = 9)
                        )
                    ),
                    tabPanel("Trends",
                             tabsetPanel(
                                tabPanel("General view by Country of arrival",
                                    fluidRow(style = "background-color:#E0ECF4;",
                                        column(12,style = "color:#045a8d", align = "center",
                                            tags$br(),
                                            selectInput("indicator1",
                                                        "Select Magnitude if you want to emphasize magnitude over trend, select Trend otherwise",
                                                        choices = c("Magnitude","Trend"),
                                                        selected = "Magnitude",
                                                        multiple = FALSE
                                            )
                                        )
                                    ),
                                    fluidRow(
                                        div(align="center", 
                                            plotlyOutput("graph_by_country1",width = "70%", height = "450", inline = TRUE)
                                        )
                                    )
                                ),
                                tabPanel("Evolution of arrivals by Continent of origin",
                                    fluidRow(style = "background-color:#E0ECF4;",
                                        column(5,style = "color:#045a8d", align = "center",
                                            tags$br(),
                                            radioButtons("country_sel_trend",
                                                        "Select a Country of arrival:",
                                                        choices = world_arrivals$Country[!duplicated(world_arrivals$Country)])
                                        ),
                                        column(5,style = "color:#045a8d", align = "center",
                                            tags$br(),
                                            selectInput("indicator2",
                                                "Select Magnitude if you want to emphasize magnitude over trend, select Trend otherwise",
                                                choices = c("Magnitude","Trend"),
                                                selected = "Magnitude",
                                                multiple = FALSE
                                            )
                                        )
                                    ),
                                    fluidRow(
                                        div(plotlyOutput("graph_by_continent",width = "100%", height = "400", inline = TRUE))
                                    )
                                ),
                                tabPanel("Evolution of arrivals by Country of origin",
                                    sidebarPanel(style = "background-color:#E0ECF4;",
                                             # column(4,style = "color:#045a8d", align = "center",
                                                    tags$br(),
                                                    radioButtons("country_sel_trend2",
                                                                 "Select a Country of arrival:",
                                                                 choices = world_arrivals$Country[!duplicated(world_arrivals$Country)]
                                                    ),
                                             # ),
                                             # column(4,style = "color:#045a8d", align = "center",
                                                    tags$br(),
                                                    radioButtons("continent_orig_sel_trend",
                                                                "Choose Continent of origin:",
                                                                choices = world_arrivals$continent[!duplicated(world_arrivals$continent)]
                                                    ),
                                             # ),
                                             # column(4,style = "color:#045a8d", align = "center",
                                                    tags$br(),
                                                    selectInput("indicator3",
                                                                "Select Magnitude if you want to emphasize magnitude over trend, select Trend otherwise",
                                                                choices = c("Magnitude","Trend"),
                                                                selected = "Magnitude",
                                                                multiple = FALSE
                                                    )
                                             # )
                                             
                                    ),
                                    mainPanel(
                                        div(plotlyOutput("graph_by_origincountry", width = "100%", height = "600")) #, inline = TRUE)
                                    )
                            )
                        )
                    )
            )
)


server <- function(input, output) {

    # Toggling sidebar
    observeEvent(input$toggleSidebar, {
        shinyjs::toggle(id = "Sidebar")
    })
    
    
    # Tab: Africa: Land Cover map ---------------------------- 
    # Africa Land Cover
    output$mymap_africa_leaflet <- renderLeaflet({
        mymap_africa_land_lf
    })
    
    # Tab: Arrivals: A general view---------------------------- 
    # World map
    # Creating the dataframe for the graph
    df2 <-  reactive({
        world_arrivals %>%
            filter(Country %in% input$country_sel & continent %in% input$continent_sel & Year %in% input$year_sel) %>% 
            group_by(name,Country) %>% 
            summarize(Number.of.Arrivals = sum(Number.of.Arrivals))
    })
    
    borders1 <-   reactive({
        world_arrivals %>%
            filter(name %in% input$country_sel) %>% 
            group_by(Year)
    })
    
    # Leaflet map of the world 
    output$mymap_world_lf <-  renderLeaflet({
        
        tm <- tm_shape(World, is.master = TRUE) +
            tm_style("classic") + # Options: "white", "gray", "natural"1, "cobalt", "col_blind", "albatross", "beaver"2, "bw", "classic", "watercolor" 
            tm_borders() +
            tm_shape(borders1()) +
            tm_borders(lwd = 3, col = "#A63603") + 
            tm_shape(df2()) +
            tm_borders(lwd = 0.1) +
            tm_bubbles("Number.of.Arrivals", border.col = "black", alpha = 0.6, border.alpha = .8,
                       scale = 2, style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
                       palette ="green3", contrast=1,
                       title.size="Arrivals from this country",
                       legend.size.show = TRUE,
                       legend.shape.show = TRUE) #+

        # Putting it as a leaflet
        tmap_leaflet(tm) %>% setView(lng =5, lat = 28,zoom = 1.75)
    
    })
    
    # Creating the dataframe for the plot, which includes filetering by top countries with more arrivals
    df_world_top_arrivals <-  reactive({
        
        df_rendered <- world_arrivals %>%
            filter(Country %in% input$country_sel & continent %in% input$continent_sel & Year %in% input$year_sel) %>% 
            group_by(name,Country) %>% 
            summarize(Number.of.Arrivals = sum(Number.of.Arrivals)) %>% 
            arrange(desc(Number.of.Arrivals)) 
        
        df_rendered <- df_rendered[c(1:10),]
    })

    # The title of the plotlu and the plotly itself
    output$title1 <- renderText({
        paste("Top 10 Arrivals at the border of ", input$country_sel, "by origin Country")
    })
    
     output$world_top_arrivals <- renderPlotly({

         p <- df_world_top_arrivals() %>%
             mutate(highlight_uk = ifelse(name == "United Kingdom",T,F)) %>%
             ggplot() +
             geom_col(aes(x = factor(name, levels = name[order(desc(Number.of.Arrivals))]), y = Number.of.Arrivals, fill = highlight_uk), col ="black", alpha = 0.6) + 
             bbc_style_mod() +
             scale_y_continuous(labels = scales::comma) +
             theme(legend.position = 'none') +
             labs( y ="Number of Arrivals at the border")
         
         ggplotly(p,tooltip = "Number.of.Arrivals") 
     })
     
    # Tab: Arrivals by continent ----------------------------  
    # Map of one continent
    # Creating the dataframe for the graph of one continent
    df <-  reactive({
        world_arrivals %>%
            filter(Country %in% input$country_select & continent %in% input$continent_select & Year %in% input$year_select) %>% 
            group_by(name,Country) %>% 
            summarize(Number.of.Arrivals = sum(Number.of.Arrivals))
    })
     
     borders2 <-   reactive({
         world_arrivals %>%
             filter(name %in% input$country_select) %>% 
             group_by(Year)
     })
    
    # Leaflet map of one continent
    output$mymap_continent_lf <-  renderLeaflet({

        tm <- tm_shape(world_arrivals %>% filter(continent == input$continent_select), is.master = TRUE) +
            tm_borders() +
            tm_shape(borders2()) +
            tm_borders(lwd = 3, col = "#A63603") +
            tm_shape(df()) +
            tm_borders(lwd = 0.1) +
            tm_fill(col = "Number.of.Arrivals", n = 10)

        # Putting it as a leaflet
        tmap_leaflet(tm) 
    })
    
    
    output$title2 <- renderText({
        paste("Top 10 Arrivals at the border of", input$country_select, "from", input$continent_select)
    })
        # txt <- #span(tags$h3("Top 10 Arrivals at the border from"), input$contient_select, tags$h3("by Country"))
            # input$continent_select
 
    
    df_continent_top_arrivals <-  reactive({
        
        df_rendered2 <- world_arrivals %>%
            filter(Country %in% input$country_select & continent %in% input$continent_select & Year %in% input$year_select) %>% 
            group_by(name,Country) %>% 
            summarize(Number.of.Arrivals = sum(Number.of.Arrivals)) %>% 
            arrange(desc(Number.of.Arrivals)) 
        
        df_rendered2 <- df_rendered2[c(1:10),]
    })
    
    output$continent_top_arrivals <- renderPlotly({
        
        p <- df_continent_top_arrivals() %>%
            mutate(highlight_uk = ifelse(name == "United Kingdom",T,F)) %>%
            ggplot() +
            geom_col(aes(x = factor(name, levels = name[order(desc(Number.of.Arrivals))]), y = Number.of.Arrivals, fill = highlight_uk), col ="black", alpha = 0.6) +#fill = "#e0a899"... , text = paste('Number of Arrivals', Number.of.Arrivals)
            scale_y_continuous(labels = scales::comma) +
            bbc_style_mod() +
            theme(legend.position = 'none') +
            labs( y ="Number of Arrivals")
        ggplotly(p,tooltip = "Number.of.Arrivals") 
        
    })
    
    # Tab: Trend ----------------------------  
    # Graph general Evolution----
    
    # Creating the df
    df_arrivals_by_year_by_country <- reactive ({

            world_arrivals %>%
            group_by(Country, Year) %>%
            summarize(Number.of.Arrivals = sum(Number.of.Arrivals)) %>%
            ungroup()
    })
    
    # Indicator of magnitude or trend
    ind_magnitude_trend_country <- reactive({
        indicator1 <- ifelse(input$indicator1 == "Magnitude","free_x","free_y")
    })
    
    # Graph by Country
    output$graph_by_country1 <- renderPlotly({
        test_graph <- df_arrivals_by_year_by_country() %>%
            ggplot(aes(x = Year, y = Number.of.Arrivals, fill = Country), alpha = 0.6) +
            geom_area(alpha = 0.6) +
            geom_smooth(method="lm",se=FALSE) +
            facet_wrap(~ Country, ncol = 4, scales = ind_magnitude_trend_country() ) +
            bbc_style_mod() +
            geom_hline(yintercept = 0, size = 1, colour = "#333333") +
            #Formating
            theme(legend.position = "none",
                  axis.text.x = element_text(size = 7, angle = 45, hjust = .5, vjust = .2))+
            scale_y_continuous(breaks = c(0, 5000000, 1000000,1500000,2000000),
                               labels = c(0, "500K","1M","1.5M", "2M")) +

            labs(title = "Evolution by Country",
                 subtitle = "Number of Arrivals growth by country, 2013-2018")
        
        ggplotly(test_graph,tooltip = "Number.of.Arrivals")
    })
    
    
    # Graph by Continent evolution----
    
    # Creating the df
    df_arrivals_by_year_by_continent <- reactive ({
  
        world_arrivals %>%
            filter(Country %in% input$country_sel_trend) %>%
            group_by(Country, continent, Year) %>%
            summarize(Number.of.Arrivals = sum(Number.of.Arrivals)) %>%
            ungroup()
    })
    
    # Indicator of magnitude or trend
    ind_magnitude_trend_continent <- reactive({
        indicator2 <- ifelse(input$indicator2 == "Magnitude","free_x","free_y")
    })

    # Graph by Continent
    output$graph_by_continent <- renderPlotly({
        test_graph <- df_arrivals_by_year_by_continent() %>%
            ggplot(aes(x = Year, y = Number.of.Arrivals, fill = continent)) +
            geom_area(alpha = 0.6) +
            geom_smooth(method="lm",se=FALSE) +
            facet_wrap(~ continent, ncol = 7, scales = ind_magnitude_trend_continent()) +
            bbc_style_mod() +
            geom_hline(yintercept = 0, size = 1, colour = "#333333") +
            #Formating
            theme(legend.position = "none",
                  axis.text.x = element_text(size = 7, angle = 45, hjust = .5, vjust = .2)) + 
            scale_y_continuous(breaks = c(0, 5000000, 1000000,1500000,2000000),
                               labels = c(0, "500K","1M","1.5M", "2M")) +
            labs(title = "Evolution by Continent",
                 subtitle = "Number of Arrivals growth by continent, 2013-2018")
        
        ggplotly(test_graph,tooltip = "Number.of.Arrivals")
    })
    
    
    # Graph by Country of origin evolution ----

    origincountries_count <- reactive ({
        
            df <- world_arrivals %>% 
            filter(Country %in% input$country_sel_trend2 & continent %in% input$continent_orig_sel_trend) %>%
            group_by(name) %>%
            summarize(Number.of.Arrivals = sum(Number.of.Arrivals)) %>%
            arrange(desc(Number.of.Arrivals))  %>%
            ungroup()
        
    })
    
    
    n <-  reactive ({
        ind_number_rows <- nrow(origincountries_count())
    })
    
    df_rendered_arrivals_by_year_by_origincountry <- reactive ({
        df_help <- origincountries_count()
        if(n()<6) {
            df_arrivals_by_year_by_origincountry_rendered <- world_arrivals %>%
                        filter(Country %in% input$country_sel_trend2 & continent %in% input$continent_orig_sel_trend) %>%
                        group_by(name,continent, Country, Year) %>%
                        summarize(Number.of.Arrivals = sum(Number.of.Arrivals)) %>%
                        arrange(desc(Number.of.Arrivals)) %>%
                        ungroup()
        } else {
            df_arrivals_by_year_by_origincountry_rendered <- df_help[c(1:5),]
        }
    })
    
    # Step 3 of creating the df: Once we have filtered by the top 5 countries by number of arrivals, we now get the number of arrivals of these countries by year
    df_rendered_arrivals_by_year_by_origincountry_final <- reactive ({

            df_help <- df_rendered_arrivals_by_year_by_origincountry()
            df <- world_arrivals %>%
                  filter(Country %in% input$country_sel_trend2 & continent %in% input$continent_orig_sel_trend & name %in% df_help$name) %>% #continent %in% input$continent_sel_trend &
                  group_by(name,continent, Country, Year) %>%
                  summarize(Number.of.Arrivals = sum(Number.of.Arrivals))
    })

    # Indicator of magnitude or trend
    ind_magnitude_trend_country_origin <- reactive({
        indicator3 <- ifelse(input$indicator3 == "Magnitude","free_x","free_y")
    })
    
    # Graph by origin Country
    output$graph_by_origincountry <- renderPlotly({
        test_graph <- df_rendered_arrivals_by_year_by_origincountry_final() %>%
            ggplot(aes(x = Year, y = Number.of.Arrivals, fill = name)) +
            geom_col(alpha = 0.6 ) +
            geom_smooth(method="lm",se=FALSE) +
            facet_wrap(~ name, ncol = 7, scales = ind_magnitude_trend_country_origin() ) +
            bbc_style_mod() +
            geom_hline(yintercept = 0, size = 1, colour = "#333333") +
            #Formating
            theme(legend.position = "none",
                  axis.text.x = element_text(size = 7, angle = 45, hjust = .5, vjust = .2))+
            scale_y_continuous(breaks = c(0, 5000000, 1000000,1500000,2000000),
                               labels = c(0, "500K","1M","1.5M", "2M")) +
            labs(title = "Evolution by Origin Country",
                 subtitle = "Number of Arrivals growth by origin country, 2013-2018")
        
        ggplotly(test_graph,tooltip = "Number.of.Arrivals")
    })
}

# Trace functino to modify titles and x_Axis and y_axis for bbc style
# trace(bbc_style,edit = TRUE)

shinyApp(ui = ui, server = server)