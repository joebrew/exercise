library(shiny)
source('global.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Title"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(12, align = 'center',
                       actionButton('button', 'Next', icon(name = 'arrow'),
                                    style = 'font-size:400%'))
            )
        ),
        # Show a plot of the generated distribution
        mainPanel(
           fluidRow(column(3,
                           h1(textOutput('a'))),
                    column(9,
                           imageOutput('b'))),
           fluidRow(column(12,
                           sliderInput('multiplier', 'Multiplier',
                                       min = 0.25,
                                       max = 4,
                                       value = 1,
                                       step = 0.25))),
            fluidRow(column(4,
                           checkboxGroupInput('filter_area', 'Area',
                                              choices = sort(unique(df$area)),
                                              selected = sort(unique(df$area)))),
                     column(4,
                            checkboxGroupInput('filter_intensity', 'Type',
                                               choices = sort(unique(df$intensity)),
                                               selected = sort(unique(df$intensity)))),
                     column(4,
                            checkboxInput('warmup', 'Warmup only', value = FALSE))),
           fluidRow(column(12,
                           p(textOutput('n'), ' exercises selected.')))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    counter <- reactiveVal(value = 0)
    
    filtered_data <- reactive({
        out <- df %>%
            filter(area %in% input$filter_area,
                   intensity %in% input$filter_intensity)
        if(input$warmup){
            out <- out %>% filter(warmup == 'yes')
        }
        out
    })
    
    output$n <- renderText({
        fd <- filtered_data()
        nrow(fd)
    })
    
    data <- reactiveValues(data = df[sample(1:nrow(df), 1),])
    observeEvent(input$button,{
        cc <- counter()
        counter(cc + 1)
        x <- filtered_data()
        s <- sample(1:nrow(x), 1)
        out <- x[s,]
        data$data <- out
    })
    
    output$a <- renderText({
        dd <- data$data
        dd$name
    })
    
    output$b <- renderImage({
        dd <- data$data
        filename <- normalizePath(file.path('./img',
                                            paste(dd$image, sep='')))
        
        # Return a list containing the filename and alt text
        list(src = filename,
             alt = 'Some image')
        
    }, deleteFile = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
