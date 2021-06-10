library(shiny)
library(bslib)
library(fst)
library(dplyr)
library(ggplot2)
library(ggridges)
library(forcats)

df <- read_fst("hazard.fst")  

regions <- df %>% select(LAD17NM) %>% distinct() %>% arrange(LAD17NM)  
decades <- c("1980-1989", "1990-1999",
             "2020-2029", "2030-2039",
             "2060-2069", "2070-2079")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    
    # Application title
    titlePanel("Hazard - Maximum Temperatures"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("region", "Region:",
                        regions, 
                        selected = 'Cornwall'),
            selectInput("first_decade", "First decade:",
                        decades,
                        selected = '1980-1989'),
            selectInput("second_decade", "Second decade:",
                        decades,
                        selected = '2020-2029'),
            position = "right", width = 3),
        
        mainPanel(
            plotOutput("ukcpPlot", height = 700, width = 800)
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$ukcpPlot <- renderPlot({
        
        df_fil <- read_fst("hazard.fst") %>%  
            dplyr::select(-msoa) %>% 
            dplyr::filter(decade %in% c(input$first_decade, input$second_decade) & LAD17NM == input$region)
        
        xlab <- "Maximum Temperature (°C)"
        
        ggplot(data = df_fil, aes(y = fct_rev(month)))+
            geom_density_ridges(aes(x = value, fill = decade), alpha = 0.6, colour = "transparent") +
            scale_fill_manual(values = c("#151F6D", "#FF004E"), labels = c(input$first_decade, input$second_decade)) +
            theme_ridges(grid = T, center_axis_labels = TRUE)+
            labs(y = "Month", x = xlab, 
                 fill = "Decade", caption = "UKCP18 data - average maximum monthly temperature extracted for each MSOA")+
            theme(text = element_text(size=26),
                  axis.text.x = element_text(color = "grey20", size = 20, face = "plain"),
                  axis.text.y = element_text(color = "grey20", size = 20, face = "plain"),  
                  axis.title.x = element_text(color = "grey20", size = 26, face = "plain"),
                  axis.title.y = element_text(color = "grey20", size = 26, face = "plain"),
                  plot.caption = element_text(size = 10, face = "italic"))+
            guides(fill = guide_legend(override.aes = list(size = 20)))+
            ggtitle(paste0("Maximum temperature data for ", input$region))
    }) %>% 
        bindCache(input$first_decade, input$second_decade, input$region)
}


# Run the application 
shinyApp(ui = ui, server = server)
