library(shiny)
source('global.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Title"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            uiOutput('ui_button'),
            uiOutput('ui_next')
        ),
        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(column(12, align = 'center',
                            h1(textOutput('a')))),
            fluidRow(column(12, align = 'center',
                            imageOutput('b'))),
            fluidRow(column(12,
                            sliderInput('multiplier', 'Multiplier',
                                        min = 0.25,
                                        max = 4,
                                        value = 1,
                                        step = 0.25))),
            fluidRow(column(3,
                            checkboxGroupInput('filter_area', 'Area',
                                               choices = sort(unique(df$area)),
                                               selected = sort(unique(df$area)))),
                     column(3,
                            checkboxGroupInput('filter_intensity', 'Type',
                                               choices = sort(unique(df$intensity)),
                                               selected = sort(unique(df$intensity)))),
                     column(3,
                            checkboxInput('warmup', 'Warmup only', value = FALSE)),
                     column(3,
                            checkboxInput('ben', 'Ben?', value = FALSE))),
            fluidRow(column(3,
                            checkboxInput('include_barbells', 'Include barbells?', value = TRUE)),
                     column(3, checkboxInput('include_bench', 'Include bench?', value = TRUE)),
                     column(3,
                            checkboxInput('include_bars', 'Include bars?', value = TRUE)),
                     column(3,
                            checkboxInput('include_dumbells', 'Include dumbells?', value = TRUE))),
            fluidRow(column(12,
                            p(textOutput('nr'))))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    counter <- reactiveVal(value = 0)
    n <- reactiveVal(value = 1)
    
    # Create a filtered data object
    filtered_data <- reactive({
        out <- df %>%
            filter(area %in% input$filter_area,
                   intensity %in% input$filter_intensity)
        if(input$warmup){
            out <- out %>% filter(warmup == 'yes')
        }
        if(!input$include_bars){
            out <- out %>% filter(needs_bars == 'no')
        }
        if(!input$include_dumbells){
            out <- out %>% filter(needs_dumbells == 'no')
        }
        if(!input$include_barbells){
            out <- out %>% filter(needs_barbells == 'no')
        }
        if(!input$include_bench){
            out <- out %>% filter(needs_bench == 'no')
        }
        if(input$ben){
            out <- out %>% filter(ben_can_do == 'yes')
        }
        out
    })
    
    # Get the number of rows in the filtered datas
    output$nr <- renderText({
        fd <- filtered_data()
        cc <- counter()
        nn <- nrow(fd)
        paste0('Exercise number ', cc, ' of ', nn)
    })
    
    # Change selected exercise randomly
    observeEvent(input$next_button,{
        fd <- filtered_data()
        nn <- nrow(fd)
        cc <- counter()
        new_cc <- cc + 1
        if(new_cc > nn){
            counter(1)
        } else {
            counter(new_cc)
        }
    })
    
    # Change selected exercise sequentially
    observeEvent(input$button,{
        fd <- filtered_data()
        nn <- nrow(fd)
        new_cc <- sample(1:nn, 1)
        counter(new_cc)
    })
    
    # Observe changes to the counter, and refresh the data accordingly
    the_row <- reactive({
        cc <- counter()
        x <- filtered_data()
        out <- x[cc,]
        out
    })
    
    output$a <- renderText({
        dd <- the_row()
        mm <- input$multiplier
        val <- round(dd$unit * mm)
        out <- paste0(val, ' ', dd$name)
    })
    
    output$b <- renderImage({
        dd <- the_row()
        filename <- normalizePath(file.path('./img',
                                            paste('0.png', sep='')))
        if(!is.null(dd)){
            if(nrow(dd) > 0){
                filename <- normalizePath(file.path('./img',
                                                    paste(dd$image, sep='')))
            }
        }
        list(src = filename,
             alt = 'Some image')
    }, deleteFile = FALSE)
    
    
    # Observe the filters and refresh the counter
    observeEvent(input$filter_area,{
        counter(0)
    })
    observeEvent(input$filter_intensity,{
        counter(0)
    })
    observeEvent(input$warmup,{
        counter(0)
    })
    observeEvent(input$ben,{
        counter(0)
    })
    observeEvent(input$include_bars,{
        counter(0)
    })
    observeEvent(input$include_dumbells,{
        counter(0)
    })
    observeEvent(input$include_barbells,{
        counter(0)
    })
    observeEvent(input$include_bench,{
        counter(0)
    })
    
    # Make a next button ui
    output$ui_next <- renderUI({
        cc <- counter()
        if(cc == 0){
            next_label <- 'Start sequentially'
        } else {
            next_label <- 'Next'
        }
        fluidRow(column(12, align = 'center',
                        actionButton('next_button', next_label, icon(name = 'arrow'),
                                     style = 'font-size:100%')))
    })
    
    output$ui_button <- renderUI({
        cc <- counter()
        if(cc == 0){
            button_label <- 'Start randomly'
            button_size <- 'font-size:200%'
        } else {
            button_label <- 'Random'
            button_size <- 'font-size:400%'
        }
        fluidRow(
            column(12, align = 'center',
                   actionButton('button', button_label, icon(name = 'arrow'),
                                style = button_size))
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
