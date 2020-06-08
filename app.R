# Shiny App for NBA
# Author: Zhihao Chen

library(shiny)
library(tidyverse)
library(jsonlite)
library(httr)
library(shinyWidgets)
library(leaflet)
library(fmsb)
library(rvest)
library(shinythemes)

readRenviron(".Renviron")

# Player information
r <- GET(
    "https://api.sportsdata.io/v3/nba/scores/json/Players",
    query = list(
        key = Sys.getenv("NBA_KEY0")
    )
)
stop_for_status(r)
json <- content(r, as = "text")
players <- fromJSON(json)

# Team information
r <- GET(
    "https://api.sportsdata.io/v3/nba/scores/json/teams",
    query = list(
        key = Sys.getenv("NBA_KEY0")
    )
)
stop_for_status(r)
json <- content(r, as = "text")
teams <- fromJSON(json)

# Stadiums information
r <- GET(
    "https://api.sportsdata.io/v3/nba/scores/json/Stadiums",
    query = list(
        key = Sys.getenv("NBA_KEY0")
    )
)
stop_for_status(r)
json <- content(r, as = "text")
stadiums <- fromJSON(json)

# Scores in 2020
r <- GET(
    "https://api.sportsdata.io/v3/nba/stats/json/PlayerSeasonStats/2020",
    query = list(
        key = Sys.getenv("NBA_KEY0")
    )
)
stop_for_status(r)
json <- content(r, as = "text")
scores_2020 <- fromJSON(json)

# Webscrapping Kobe Stats
Kobe <- read_html("https://www.basketball-reference.com/players/b/bryanko01.html") %>%
    html_node("div.overthrow.table_container table") %>%
    html_table()

# Webscrapping Kobe's quotes
kobe_quotes <- read_html("https://www.goalcast.com/2018/12/28/kobe-bryant-quotes/") %>%
    html_nodes("div.td-post-content") %>% 
    html_nodes("blockquote.td_pull_quote.td_pull_center") %>% 
    html_nodes("p") %>%
    html_text()

ui <- fluidPage(
    theme = shinytheme("cerulean"),
    setBackgroundColor("white"),
    titlePanel(h1("NBA Quick Search", align = "center", 
                  img(src = "https://cdn.freebiesupply.com/images/large/2x/nba-logo-transparent.png", height = 50, width = 25))),
    tabsetPanel(
        tabPanel("Team Info", # First Tab
            sidebarLayout(
                sidebarPanel(
                    selectInput("team_name", "Team", choices = teams$Key),
                    h4("Players", align = "center"),
                    tableOutput("team_members")
                    
                ),
                mainPanel(
                    h3(textOutput("teamname"), align = "center"),
                    h4("Team Information", align = "center"),
                    column(12, align = "center", tableOutput("teaminfo")),
                    h4("Team Logo", align = "center"),
                    h3(uiOutput("teamlogo"), align = "center"),
                    h4("Stadium Map", align = "center"),
                    leafletOutput("statium_map")
                
                )
            )
        ),
        tabPanel("Player Info", # Second Tab
            sidebarLayout(
                sidebarPanel(
                    selectInput("team_of_player", "Team", choices = teams$Key),
                    uiOutput("teamplayer"),
                    h3(uiOutput("player_photo"), align = "center")
                         
                ),
                mainPanel(
                    h3(textOutput("playername"), align = "center"),
                    h4("Player Information", align = "center"),
                    column(12, align = "center", tableOutput("playerinfo")),
                    h4("Birth Information", align = "center"),
                    column(12, align = "center", tableOutput("birthinfo")),
                    h4("Game Information in 2020", align = "center"),
                    column(12, align = "center", tableOutput("gameinfo")),
                    h4("Player Stats Per Game in 2020", align = "center"),
                    column(12, align = "center", tableOutput("playerdata"))
                )
            )
        ),
        tabPanel("Player Comparison", # Third Tab
            sidebarLayout(
                sidebarPanel(
                    h4("Player 1", align = "center"),
                    selectInput("team1", "Team", choices = teams$Key),
                    uiOutput("teamplayer1"),
                    h4("Player 2", align = "center"),
                    selectInput("team2", "Team", choices = teams$Key, selected = "SAC"),
                    uiOutput("teamplayer2"),

                         
                ),
                mainPanel(
                    h4("Stats Comparison", align = "center"),
                    plotOutput("comparison_hist"),
                    h4("Shoting Percentage Comparison", align = "center"),
                    plotOutput("shot_radar")
                         
                )
            )
        ),
        tabPanel("Mamba Forever", # Fourth Tab
            img(src = "https://woub.org/wp-content/uploads/2020/01/kobe-bryant-obit-plasma-2020_1580082165-e1580173763658.jpg"),
            h4("Historical Statistics", align = "center"),
            DT::dataTableOutput("Kobestats"),
            h4("Quotes From Kobe Bryant", align = "center"),
            tableOutput("quote"),
            h3("See You, Kobe", align = "center")
        )
    )
)


server <- function(input, output) {
    # First Tab Output
    output$team_members <- renderTable({
        players %>% 
            filter(Team == input$team_name) %>% 
            select(FirstName, LastName, Jersey, Position)
    },align = 'c')

    output$teamname <- renderText({
        teams %>% filter(Key == input$team_name) %>% select(City, Name) %>% as.character()
    })
    
    output$teaminfo <- renderTable({
        teams %>% 
            inner_join(stadiums, by = "StadiumID") %>%
            filter(Key == input$team_name) %>% 
            select( Country, Conference, Division, Stadium = Name.y, Address, Capacity)
    })
    
    output$teamlogo <- renderUI({
        logo <- teams %>% filter(Key == input$team_name) %>% select(WikipediaLogoUrl) %>% as.character()
        img(src = logo, width = "20%")
    })
    
    output$statium_map <- renderLeaflet({
        teams %>% 
            inner_join(stadiums, by = "StadiumID") %>% 
            filter(Key == input$team_name) %>% 
            select(GeoLong, GeoLat, Name.y) %>% 
            leaflet() %>%
            addTiles() %>%
            addMarkers(~GeoLong, ~GeoLat, popup=~Name.y)
    })
    
    # Second Tab Output
    output$teamplayer <- renderUI({
        currdata <- players %>% filter(players$Team %in% input$team_of_player)
        selectInput("teamplayer", "Player", 
                    c(currdata %>% pull(LastName)) %>% unique %>% sort())
    })
    
    output$player_photo <- renderUI({
        req(!is.null(input$teamplayer))
        player <- players %>% 
            filter(Team == input$team_of_player) %>% 
            filter(LastName == input$teamplayer) %>% select(PhotoUrl) %>% as.character()
        img(src = player, width = "50%")
    })
    
    output$playername <- renderText({
        req(!is.null(input$teamplayer))
        players %>% 
            filter(Team == input$team_of_player) %>% 
            filter(LastName == input$teamplayer) %>% 
            select(FirstName, LastName) %>% 
            as.character()
    })
    
    output$playerinfo <- renderTable({
        req(!is.null(input$teamplayer))
        players %>% 
            filter(Team == input$team_of_player) %>% 
            filter(LastName == input$teamplayer) %>%
            mutate(BirthDate = substr(BirthDate, start = 1, stop = 10)) %>% 
            select(Jersey, Position, Height, Weight, College, Salary, Experience)
    })
    
    output$birthinfo <- renderTable({
        req(!is.null(input$teamplayer))
        players %>% 
            filter(Team == input$team_of_player) %>% 
            filter(LastName == input$teamplayer) %>%
            mutate(BirthDate = substr(BirthDate, start = 1, stop = 10)) %>% 
            select(BirthDate, BirthCity, BirthState)
    })
    
    output$gameinfo <- renderTable({
        req(!is.null(input$teamplayer))
        players %>% 
            full_join(scores_2020, by = "PlayerID") %>% 
            filter(LastName == input$teamplayer & Team.y == input$team_of_player) %>%
            mutate(Min_Per_Game = Minutes/Games) %>% 
            select(Games, Started, Minutes, Min_Per_Game)
    }, rownames = F, colnames = T)
    
    output$playerdata <- renderTable({
        req(!is.null(input$teamplayer))
        players %>% 
            full_join(scores_2020, by = "PlayerID") %>% 
            filter(LastName == input$teamplayer & Team.y == input$team_of_player) %>%
            mutate(Points = Points/Games, Rebounds = Rebounds/Games,  Assists = Assists/Games, Steals = Steals/Games) %>% 
            select(Points, Rebounds, Assists, Steals)
    }, rownames = F, colnames = T)
    
    # Third Tab Output
    output$teamplayer1 <- renderUI({
        currdata <- players %>% filter(players$Team %in% input$team1)
        selectInput("teamplayer1", "Player", 
                    c(currdata %>% pull(LastName)) %>% unique %>% sort())
    })
    
    output$teamplayer2 <- renderUI({
        currdata <- players %>% filter(players$Team %in% input$team2)
        selectInput("teamplayer2", "Player", 
                    c(currdata %>% pull(LastName)) %>% unique %>% sort())
    })
    
    output$comparison_hist <- renderPlot({
        req(!is.null(input$teamplayer1))
        req(!is.null(input$teamplayer2))
        player1 <- players %>% 
            full_join(scores_2020, by = "PlayerID") %>% 
            filter(LastName == input$teamplayer1 & Team.y == input$team1) %>%
            mutate(Points = Points/Games, Rebounds = Rebounds/Games,  Assists = Assists/Games, Steals = Steals/Games) %>% 
            select(LastName, Points, Rebounds, Assists, Steals)
        player2 <- players %>% 
            full_join(scores_2020, by = "PlayerID") %>% 
            filter(LastName == input$teamplayer2 & Team.y == input$team2) %>%
            mutate(Points = Points/Games, Rebounds = Rebounds/Games,  Assists = Assists/Games, Steals = Steals/Games) %>% 
            select(LastName, Points, Rebounds, Assists, Steals)
        tbl <- rbind(player1, player2) %>% pivot_longer(-LastName, names_to = "Stats", values_to = "Val") %>% as.data.frame()
        ggplot(tbl, aes(x = Stats, y = Val, fill = LastName)) + geom_bar(stat="identity", position=position_dodge())
    })
    
    output$shot_radar <- renderPlot({
        req(!is.null(input$teamplayer1))
        req(!is.null(input$teamplayer2))
        player1 <- players %>% 
            full_join(scores_2020, by = "PlayerID") %>% 
            filter(LastName == input$teamplayer1 & Team.y == input$team1) %>%
            select(LastName, TwoPointers = TwoPointersPercentage, ThreePointers = ThreePointersPercentage, FreeThrows = FreeThrowsPercentage, FieldGoals = FieldGoalsPercentage)
        player2 <- players %>% 
            full_join(scores_2020, by = "PlayerID") %>% 
            filter(LastName == input$teamplayer2 & Team.y == input$team2) %>%
            select(LastName, TwoPointers = TwoPointersPercentage, ThreePointers = ThreePointersPercentage, FreeThrows = FreeThrowsPercentage, FieldGoals = FieldGoalsPercentage)
        tbl <- rbind(player1, player2) %>% pivot_longer(-LastName, names_to = "Stats", values_to = "Val") %>% as.data.frame()
        ggplot(tbl, aes(x = Stats, y = Val, fill = LastName)) + geom_bar(stat="identity", position=position_dodge())
    })
    
    # Fourth Tab Output
    output$Kobestats <- DT::renderDataTable({
        Kobe[,c(1,2,6:9,11,12,14,15,17,30)]
    })
    
    output$quote <- renderTable({
        kobe_quotes
    }, colnames = F, align = 'c')
}

shinyApp(ui = ui, server = server)

