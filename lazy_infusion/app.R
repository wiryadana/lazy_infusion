
library(shiny)


# Define UI for application
ui <- navbarPage(
  "Lazy Infusion",
  tabPanel("Syringe Pump",
           fluidPage(
             titlePanel("Hourly infusion rate"),
             navlistPanel(
               id = "tabset",
               
               "Vasopressor",
               tabPanel("Norepinefrine",
                        "Norepinefrine Infusion",
                        fluidRow(
                          column(6,
                                 sliderInput("nor_dose", "Norepinefrine Dose (mcg/kg/min)", value = 0.1, min = 0.1, max = 2),
                                 sliderInput("nor_BB", "Bodyweight (Kg)", value = 50, min = 40, max = 100),
                                 sliderInput("nor_amount", "Amount Dispensed (each ampul 4mg/4ml)", value = 1, min = 1, max = 4),
                                 selectInput("nor_syringe", "Total Dilution (ml)", choices = c(50,20), selected = 50)
                                 ),
                          column(6,"hello")
                        ),
                        fluidRow(
                          column(12, 
                                 textOutput("nor_rate"),
                                 textOutput("nor_eta")
                                 )
                        ),
                        fluidRow(
                          column(12,
                                 tableOutput("nor_table")
                                 )
                        )
                        ),
               
               tabPanel("Dobutamine", "dobu"),
               tabPanel("Dopamine", "dopa"),
               
               "Vasodilator",
               tabPanel("Nicardipin", "nikardipin"),
               tabPanel("ISDN"),
               tabPanel("Nitroglycerin"),
               
               "Diuretics",
               tabPanel("Furosemid"),
               
               "Insulin",
               tabPanel("Hiperglikemia", "texas"),
               tabPanel("Ketoacidosis-HHS"),
               
               "Analgetics, Sedatives, Muscle Relaxant",
               tabPanel("Fentanyl"),
               tabPanel("Morfin"),
               tabPanel("Petidin"),
               tabPanel("Propofol"),
               tabPanel("Midazolam"),
               tabPanel("Rocuronium"),
               
               "Gastroprotector",
               tabPanel("Pantoprazole"),
               
             )
           )
           ),
  tabPanel("Piggyback")
)

# Define server logic 
server <- function(input, output) {
  
  n_r <- reactive(((input$nor_dose * 60 * input$nor_BB)/((input$nor_amount * 4000)/as.numeric(input$nor_syringe))))
  bb <- reactive(seq(from=40,to=100,by=5))
    
  output$nor_rate <- renderText({
    print(paste0("Infusion Rate: ", n_r(), " ml/hour"))
  })
  output$nor_eta <- renderText({
    nor_eta <- round(as.numeric(input$nor_syringe)/n_r(),2)
    print(paste0("Estimated Refill Time: ", nor_eta, " hour"))
    })
  
  output$nor_table <- renderTable({
    dose <- seq(from=0.1, to=2, by=0.1)
    bb <- bb()
    
    nor_tab = matrix(NA, nrow = length(dose), ncol = length(bb))
    for (i in 1:length(dose)) {
      for (j in 1:length(bb)) {
        nor_tab[i,j] = print(dose[i] * 60 * bb[j] * as.numeric(input$nor_syringe) / (4000 * input$nor_amount))
      }
    }
    colnames(nor_tab) = bb
    rownames(nor_tab) = dose
    print(nor_tab)
    
  }, rownames = TRUE, striped = T)
}

# Run the application 
shinyApp(ui = ui, server = server)
