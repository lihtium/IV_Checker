#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("style.css"),
   # Application title
   titlePanel("Pokemon Go"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h4("Gotta paste them all"),
        img(src="https://i.imgur.com/iNHQO65.png",height=100),
        selectInput(inputId = "pokename",
                    label = "Select your Pokemon",
                    choices = base_stats$Name,
                    selected = "Larvitar"),
        sliderInput(inputId = "iv_percentage",
                    label = "Minimum IV percentage",
                    min = 0,
                    max = 100,
                    value = 93),
        numericInput(inputId = "min_attack",
                     label = "Minimum attack stats : ",
                     value = 0,
                     min = 0,
                     max = 15)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        wellPanel(
        #tags$img(src="pogo.logo.png",height=100),
        h4("Copy and paste this : "),
        h6(textOutput("selected_var"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  library(tidyverse)
  library(stringr)
  
  CPM<-read.csv("./data/CPM.csv",sep=";",dec=",",header=T)
  base_stats <- read.csv("./data/Pokemon_go_CP_list.csv",sep=",",dec=".",header=T,encoding="UTF-8")
  base_stats <- base_stats %>% arrange(id)
  
  CP_GO<-function(name,level,iv=c(15,15,15)){
    
    pokemon<-base_stats %>% filter(Name == name)
    
    cpm = ((CPM %>% filter(Level%in%level)) %>% select(CPM) %>% as.vector)
    
    Base_attack = with(pokemon, Attack + iv[1])
    Base_defense = with(pokemon,Defense + iv[2])
    Base_stamina = with(pokemon,HP + iv[3])
    
    CP = pmax(10,floor(Base_attack*sqrt(Base_defense)*sqrt(Base_stamina)*(cpm^2)/10) %>% c)
    return(list(CP=CP,Percentage = round(sum(iv)/45*100)))
  }
  
  ## Check percentage
  
  percentage.check<-function(iv=c(15,15,15),goal,atk=0,def=0,sta=0){
    
    (round(sum(iv)/45*100)>=goal) & (iv[1] >= atk & iv[2] >= def & iv[3] >= sta)
  } 

  # all x,y,z
  xy<-paste(matrix(0:15,ncol=16,nrow=16),
            t(matrix((0:15),ncol=16,nrow=16)),sep=",") %>% matrix(ncol=16,nrow=16)
  xy<-0:15 %>% map(~xy) %>% reduce(rbind)
  z<-(0:15) %>% map(~rep(.,16)) %>% unlist %>% matrix(.,ncol=16,nrow=16^2)
  temp<-paste(xy,z,sep=",")
  rm(xy)
  rm(z)
  
  conversion<-function(iv){
    a<-gsub(",.*","",iv) %>% as.numeric
    d<-str_extract(iv,",.*,") %>% gsub(",","",.) %>% as.numeric
    s<-gsub(".*,","",iv) %>% as.numeric
    return(c(a,d,s))
  }
  
  iv.list<-temp %>% map(~conversion(.))
  rm(temp)

  iv_paste<-function(name,goal,atk,def,sta){
  
    # Check if it's legendary
    legendary.test<-(base_stats$Legend[base_stats$Name==name]==0)
    if(legendary.test){levels = c(1:35)}else{levels = c(20,25)}
    
    temp<-iv.list[iv.list %>% map(~percentage.check(.,goal,atk,def,sta)) %>% unlist] %>% map(~CP_GO(name,level = levels,iv=.))
    iv.check<-(temp %>% map(~.$CP) %>% unlist %>% unique %>% sort) %>% as_tibble %>% mutate(Difference=c(1,diff(value)))

    A<-iv.check$value[which(iv.check$Difference>1)]
    A<-c(iv.check$value[1],A)
    B<-c(iv.check$value[which(iv.check$Difference>1)-1],tail(iv.check$value,1))
    idx<-(A==B)
    string<-paste(A,B,sep="-")
    string[idx]<-A[idx]
    
    res<-paste(paste(
      base_stats$id[base_stats$Name==name],"&",string %>% paste("cp",.,collapse=", ",sep="")))
    
    return(res)
  }

  output$selected_var <- renderText({ 
    iv_paste(input$pokename,input$iv_percentage,input$min_attack,0,0)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

