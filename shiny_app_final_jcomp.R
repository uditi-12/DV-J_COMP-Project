rm(list = ls())
#install.packages("shiny")
#install.packages("dplyr")
library(shiny)
library(dplyr)
library(readr)
library(DT)
library(data.table)

# load list of last.fm artists: drop columns containing URL's since they aren't needed
lfm_art <- read_delim("C:\\Users\\admin\\OneDrive\\Documents\\VIT\\Sem 6\\D- DV\\J Comp\\Dataset\\artists.dat", delim = "\t") %>% select(id, name)

# cleanup foreign characters in artist names: most will be converted to '?'
lfm_art$name <- iconv(lfm_art$name, from = "UTF-8", to = "ASCII//TRANSLIT")

# load last.fm tags.dat file to get access to genre names
lfm_tags <- read_delim("C:\\Users\\admin\\OneDrive\\Documents\\VIT\\Sem 6\\D- DV\\J Comp\\Dataset\\tags.dat", delim = "\t")


# load required matrices  

# ------------------------
# load artist-genre matrix
ag_mat <- as.matrix(read.csv("C:\\Users\\admin\\OneDrive\\Documents\\VIT\\Sem 6\\D- DV\\J Comp\\Dataset\\ag_mat.csv", check.names = FALSE,
                             header=TRUE, sep = ",", stringsAsFactors = FALSE) )

# set rownames to values in V1
row.names(ag_mat) <- as.numeric(ag_mat[,1])

# now truncate matrix to eliminate col 1
ag_mat <- ag_mat[,2:ncol(ag_mat)]
# --------------------------


# ----------------------------
# load artist similarity matrix
art_sim <- as.matrix(read.csv("C:\\Users\\admin\\OneDrive\\Documents\\VIT\\Sem 6\\D- DV\\J Comp\\Dataset\\art_sim.csv", check.names = FALSE,
                              header=TRUE, sep = ",", stringsAsFactors = FALSE) )

# set rownames to values in V1
row.names(art_sim) <- as.numeric(art_sim[,1])

# now truncate matrix to eliminate col 1
art_sim <- art_sim[,2:ncol(art_sim)]
# ----------------------------

last_sm <- read.csv("C:\\Users\\admin\\OneDrive\\Documents\\VIT\\Sem 6\\D- DV\\J Comp\\Dataset\\last_sm.csv", 
                    header=TRUE, sep = ",", stringsAsFactors = FALSE)

tenrecs <- read.csv("C:\\Users\\admin\\OneDrive\\Documents\\VIT\\Sem 6\\D- DV\\J Comp\\Dataset\\user_tenrecs.csv", 
                    header=TRUE, sep = ",", stringsAsFactors = FALSE)

###### Get the top 815 Artists IDS ---------------

# extract the 815 top artist IDs from art_sim matrix and convert to numeric
artistIDs <- as.numeric(rownames(art_sim) )

# then get associated genre names from lfm_art data frame
a_names <- lfm_art[lfm_art$id %in% artistIDs,]$name
#------------------------------------------------

# remove artist names that start with '?' characters since they cause problems
# when trying to later retrieve the associated artist ID

a_names <- a_names[a_names != '????']
a_names <- a_names[a_names != '?????']
a_names <- a_names[a_names != '??????']

###### Get the list of the top 200 genre names ----

# extract the genre tagIDs from artist-genre matrix and convert to numeric
tagIDs <- as.numeric(colnames(ag_mat))

# then get associated genre names from lfm_tags data frame
g_names <- lfm_tags[lfm_tags$tagID %in% tagIDs,]$tagValue
# --------------------------------------------------

##### Get a list of distinct userID's
userIDs <- unique(last_sm$userID)

u_hdr <- paste("Select a User ID (", min(userIDs), " - ", max(userIDs), " )" )


# ____ui____

ui <- shinyUI(fluidPage(
  
  ###################  CSS   ###################  
  tags$style(type="text/css", "#table th {
    display: none;}"), 
  tags$style(type="text/css", "#table2 th {
    display: none;}"), 
  tags$style(type="text/css", "#profileTable th {
    display: none;"), 
  tags$style(type="text/css", "#profileTable {
    max-height: 240px; 
    overflow-y: auto;}"),
  HTML('<footer>Uditi Gupta 20BCE1445 Tathagata Biswas 20BCE1844</footer>'),
  tags$style(type="text/css", "footer{
    position:absolute;
    bottom:0;
    width:100%;
    height:40px; /* Height of the footer */
    color: white;
    padding: 10px;
    background-color: black;
    z-index: 1000;}"),
  tags$style(type="text/css", "#table tbody td {
    padding: 10px;}"),
  tags$style(type="text/css", "#table2 tbody td {
    padding: 10px;}"),
  tags$style(type="text/css", "#table {
    border-collapse: collapse;
    border-spacing: 10;
    font: normal 14px Arial Black, sans-serif;}"),
  tags$style(type="text/css", "#table2 {
    border-collapse: collapse;
    border-spacing: 10;
    font: normal 14px Arial Black, sans-serif;}"),
  ###############################################
  
  titlePanel(h1(style = "font-family: Arial Black", "Musical Artist Recommender")),  
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("d_userID", u_hdr,
                  choices = c(Enter_User_ID='', userIDs )),
      
      radioButtons("Rec_Choices", label=strong("Select A Recommendation Method:"),
                   choices = list("By Similar Artists (Top 5)" = "art_sim", 
                                  "By Genre (Top 5)" = "ag_mat", 
                                  "10 Artists Recommended by Similar Users" = "tenrecs"),
                   selected = "tenrecs"),
      strong("Select an Artist You Have Previously Listened To:"),
      DT::dataTableOutput("profileTable")
      
    ), # end sidebarPanel
    
    mainPanel(
      h3(textOutput("text")),
      br(),
      uiOutput("selectedItem"),
      br(),
      tableOutput("table"),
      tags$div(id = 'placeholder')
    ) # end mainPanel
    
  )) # end sidebarLayout
)

############################################################################
############################################################################
# ____server____

server <- shinyServer(function(input, output) {
  
  #############################################
  # Function to create dynamic drop down containing appropriate list to choose from
  
  output$selectedItem <- renderUI({
    if(values$show=='panel1'){
      if (input$Rec_Choices == "ag_mat") {
        selectInput("d_genre", "Select Genre:",
                    choices = c("Select a Genre", sort(g_names) ) )
        
      } else if (input$Rec_Choices == "art_sim") {
        selectInput("d_artsim", "Select Artist:",
                    choices = c("Select an Artist", sort(a_names) ) )
        
      }
    } 
  })
  
  ##############################################
  # Function to generate heading for main panel
  
  output$text<- renderText({
    if(values$show=='panel2'){
      paste("Top 5 Artists Similar to Selected Artist")
    }else{
      if (input$Rec_Choices == "ag_mat") {
        paste("Top 5 Artists in Selected Genre")
        
      } else if (input$Rec_Choices == "art_sim") {
        paste("Top 5 Artists Similar to Selected Artist", input$selectedItem)
        
      } else if (input$Rec_Choices == "tenrecs") {
        paste("10 Artists You May Like")
        
      } # end if
    }
  })
  
  ##############################################    
  # function to generate list of recommended artists depending on
  # the method selected by the user
  
  output$table <- renderTable({
    if (input$Rec_Choices == "ag_mat") {
      # Top 5 Artists in Selected Genre
      
      # set number of artists to recommend
      n_recommended <- 5
      
      # get tagID of genre
      g_tag <- lfm_tags[lfm_tags$tagValue == input$d_genre,]$tagID
      
      # fetch the top N artists:
      # the names of the items are the artist IDs
      g_arecs <- sort(ag_mat[,as.character(g_tag)], decreasing = TRUE)[1:n_recommended]
      
      # extract the artist IDs and convert to numeric
      g_arecs_IDs <- as.numeric(names(g_arecs))
      
      # create list of artist names from artist ID's in list
      g_arec_names <- lfm_art[lfm_art$id %in% g_arecs_IDs,]$name
      
      return(g_arec_names)
      
      ############################################
      
    } else if (input$Rec_Choices == "art_sim") {
      # Top 5 Artists Similar to Selected Artist
      
      n_recommended <- 5
      
      # get name of artist from artist list
      a_val <- lfm_art[lfm_art$name == input$d_artsim,]$id
      
      a_val <- as.numeric(sort(a_val))
      
      # fetch their recommendations: this returns a named vector sorted by similarity
      # the names of the items are the artist IDs
      arecs <- sort(art_sim[as.character(a_val),], decreasing = TRUE)[1:n_recommended]
      
      # extract the artist IDs and convert to numeric
      arecs_IDs <- as.numeric(names(arecs))
      
      # create list of artist names from artist ID's in list
      arec_names <- lfm_art[lfm_art$id %in% arecs_IDs,]$name
      
      return(arec_names)
      
      #############################################
      
    } else if (input$Rec_Choices == "tenrecs") {
      # Get 10 Artists You May Like based on similar users
      
      # fetch their recommendations
      urecs <- sort(as.vector(subset(tenrecs, userID == input$d_userID)[2:11]) )
      
      # create list of artist names from artist ID's in list
      rec_names <- subset(lfm_art, id %in% urecs)$name
      
      return(rec_names)
      
    } # end if
    
  }) # end renderTable
  
  #render the profile table
  output$profileTable <- DT::renderDataTable({
    datatable(
      filteredTable_data(), 
      rownames = FALSE, 
      colnames = NULL, 
      selection = "single", 
      options = list(pageLength = -1,dom = 't'))
  })
  
  #Outputs the user datatable selected query
  output$table2 <- renderTable({
    if(length(input$profileTable_rows_selected>0)){
      # rerun the artists top n results
      n_recommended <- 5
      a_val <- lfm_art[lfm_art$name == filteredTable_selected()[[1]][1],]$id
      a_val <- as.numeric(sort(a_val))
      arecs <- sort(art_sim[as.character(a_val),], decreasing = TRUE)[1:n_recommended]
      arecs_IDs <- as.numeric(names(arecs))
      arec_names <- lfm_art[lfm_art$id %in% arecs_IDs,]$name
      return (arec_names)
    }else{
      return("Nothing Selected")
    }
  })
  
  #Keep track of which query system is being applied
  values <- reactiveValues()
  values$show <- 'panel1'
  
  #Grabs the artist value from the data table
  filteredTable_selected <- reactive({
    id <- input$profileTable_rows_selected
    filteredTable_data()[id,]
  })
  
  filteredTable_data <- reactive({
    # get list of previously listened artists for userID
    user_arts <- last_sm$artistID[last_sm$userID == input$d_userID]
    
    # create list of artist names from artist ID's in list
    ul_names <- lfm_art[lfm_art$id %in% user_arts,]$name
    
    # remove any artists that start with ? character
    ul_names <- ul_names[ul_names != '????']
    ul_names <- ul_names[ul_names != '?????']
    ul_names <- ul_names[ul_names != '??????']
    ret <- data.table(sort(ul_names))
  })
  
  # if the table is clicked, perform this UI upheaval
  observeEvent(input$profileTable_rows_selected, {
    values$show <- 'panel2' # toggle observable between 1 and 2
    removeUI(selector = '#table')
    removeUI(selector = '#table2')
    insertUI(selector = '#placeholder',ui = tableOutput('table2'))
  })
  
  # if the radio buttons are clicked, perform this UI upheaval
  observeEvent(input$Rec_Choices, {
    values$show <- 'panel1' # toggle observable between 1 and 2
    removeUI(selector = '#table2')
    removeUI(selector = '#table')
    insertUI(selector = '#placeholder',ui = tableOutput('table'))
  })
  
}) # end server

shinyApp(ui, server)

library(Rserve)
