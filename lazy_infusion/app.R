
library(shiny)


# Define UI for application
ui <- navbarPage(
  "Lazy Infusion",
  tabPanel("Syringe Pump",
           fluidPage(
             titlePanel("Hourly infusion rate"),
             navlistPanel(
               id = "tabset", widths = c(2,10), fluid = F, well = F,
               
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
                          column(6,"Explanation")
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
               
               tabPanel("Dobutamine",
                        "Dobutamine Infusion",
                        fluidRow(
                          column(6,
                                 sliderInput("dobu_dose", "Dobutamine Dose (mcg/kg/min)", value = 3, min = 1, max = 20),
                                 sliderInput("dobu_BB", "Bodyweight (Kg)", value = 50, min = 40, max = 100),
                                 sliderInput("dobu_amount", "Amount Dispensed (each ampul 250mg/5ml)", value = 1, min = 1, max = 4),
                                 selectInput("dobu_syringe", "Total Dilution (ml)", choices = c(50,20), selected = 50)
                          ),
                          column(6,"explanation")
                        ),
                        fluidRow(
                          column(12, 
                                 textOutput("dobu_rate"),
                                 textOutput("dobu_eta")
                          )
                        ),
                        fluidRow(
                          column(12,
                                 tableOutput("dobu_table")
                          )
                        )
                        ),
               tabPanel("Dopamine",
                        "Dopamine Infusion",
                        fluidRow(
                          column(6,
                                 sliderInput("dopa_dose", "Dopamine Dose (mcg/kg/min)", value = 3, min = 1, max = 20),
                                 sliderInput("dopa_BB", "Bodyweight (Kg)", value = 50, min = 40, max = 100),
                                 sliderInput("dopa_amount", "Amount Dispensed (each ampul 200mg/5ml)", value = 1, min = 1, max = 4),
                                 selectInput("dopa_syringe", "Total Dilution (ml)", choices = c(50,20), selected = 50)
                          ),
                          column(6,"explanation")
                        ),
                        fluidRow(
                          column(12, 
                                 textOutput("dopa_rate"),
                                 textOutput("dopa_eta")
                          )
                        ),
                        fluidRow(
                          column(12,
                                 tableOutput("dopa_table")
                          )
                        )
                        ),
               
               "Vasodilator",
               tabPanel("Nicardipin", 
                        "Nicardipine Infusion",
                        fluidRow(
                          column(6,
                                 sliderInput("nica_dose", "Nicardipine Dose (mcg/kg/min)", value = 1, min = 0.5, max = 5),
                                 sliderInput("nica_BB", "Bodyweight (Kg)", value = 50, min = 40, max = 100),
                                 sliderInput("nica_amount", "Amount Dispensed (each ampul 10mg/10ml)", value = 5, min = 1, max = 5),
                                 selectInput("nica_syringe", "Total Dilution (ml)", choices = c(50,20), selected = 50)
                          ),
                          column(6,"explanation")
                        ),
                        fluidRow(
                          column(12, 
                                 textOutput("nica_hour_rate"),
                                 textOutput("nica_rate"),
                                 textOutput("nica_eta")
                          )
                        ),
                        fluidRow(
                          column(12,
                                 tableOutput("nica_table")
                          )
                        )
                        ),
               tabPanel("ISDN",
                        "Isosorbide Dinitrat Infusion",
                        fluidRow(
                          column(6,
                                 sliderInput("isdn_dose", "ISDN Dose (mg/hour)", value = 1, min = 1, max = 10),
                                 sliderInput("isdn_amount", "Amount Dispensed (each ampul 10mg/10ml)", value = 5, min = 1, max = 5),
                                 selectInput("isdn_syringe", "Total Dilution (ml)", choices = c(50,20), selected = 50)
                          ),
                          column(6,"explanation")
                        ),
                        fluidRow(
                          column(12, 
                                 textOutput("isdn_rate"),
                                 textOutput("isdn_eta")
                          )
                        )
                        ),
               tabPanel("Nitroglycerin",
                        "Nitroglycerin Infusion",
                        fluidRow(
                          column(6,
                                 sliderInput("ntg_dose", "Nitroglycerin Dose (mcg/min)", value = 10, min = 5, max = 100),
                                 sliderInput("ntg_amount", "Amount Dispensed (each ampul 10mg/10ml)", value = 5, min = 1, max = 5),
                                 selectInput("ntg_syringe", "Total Dilution (ml)", choices = c(50,20), selected = 50)
                          ),
                          column(6,"explanation")
                        ),
                        fluidRow(
                          column(12, 
                                 textOutput("ntg_rate"),
                                 textOutput("ntg_eta")
                          )
                        )
                        ),
               
               "Diuretics",
               tabPanel("Furosemid"),
               
               "Insulin",
               tabPanel("Insulin", "texas"),
               
               "Antikonvulsant",
               tabPanel("Fenitoin", "fenitoin"),
               tabPanel("Fenobarbital", "fenobarbital"),
               
               "Analgetics",
               tabPanel("Fentanyl"),
               tabPanel("Morfin"),
               
               "Sedatives",
               tabPanel("Propofol"),
               tabPanel("Midazolam"),
               
               "Relaxant",
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
  
  bb <- reactive(seq(from=40,to=100,by=5))

############################ norepinefrine ##########################################    
  n_r <- reactive(((input$nor_dose * 60 * input$nor_BB)/((input$nor_amount * 4000)/as.numeric(input$nor_syringe))))
  
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

############################ Dobutamine ########################################## 
  
  dobu_r <- reactive(((input$dobu_dose * 60 * input$dobu_BB)/((input$dobu_amount * 250000)/as.numeric(input$dobu_syringe))))
  
  output$dobu_rate <- renderText({
    print(paste0("Infusion Rate: ", dobu_r(), " ml/hour"))
  })
  output$dobu_eta <- renderText({
    dobu_eta <- round(as.numeric(input$dobu_syringe)/dobu_r(),2)
    print(paste0("Estimated Refill Time: ", dobu_eta, " hour"))
  })
  
  output$dobu_table <- renderTable({
    dose <- seq(from=1, to=20, by=1)
    bb <- bb()
    
    dobu_tab = matrix(NA, nrow = length(dose), ncol = length(bb))
    for (i in 1:length(dose)) {
      for (j in 1:length(bb)) {
        dobu_tab[i,j] = print(dose[i] * 60 * bb[j] * as.numeric(input$dobu_syringe) / (250000 * input$dobu_amount))
      }
    }
    colnames(dobu_tab) = bb
    rownames(dobu_tab) = dose
    print(dobu_tab)
    
  }, rownames = TRUE, striped = T)
  
################### Dopamine #####################################################
  dopa_r <- reactive(((input$dopa_dose * 60 * input$dopa_BB)/((input$dopa_amount * 200000)/as.numeric(input$dopa_syringe))))
  
  output$dopa_rate <- renderText({
    print(paste0("Infusion Rate: ", dopa_r(), " ml/hour"))
  })
  output$dopa_eta <- renderText({
    dopa_eta <- round(as.numeric(input$dopa_syringe)/dopa_r(),2)
    print(paste0("Estimated Refill Time: ", dopa_eta, " hour"))
  })
  
  output$dopa_table <- renderTable({
    dose <- seq(from=1, to=20, by=1)
    bb <- bb()
    
    dopa_tab = matrix(NA, nrow = length(dose), ncol = length(bb))
    for (i in 1:length(dose)) {
      for (j in 1:length(bb)) {
        dopa_tab[i,j] = print(dose[i] * 60 * bb[j] * as.numeric(input$dopa_syringe) / (200000 * input$dopa_amount))
      }
    }
    colnames(dopa_tab) = bb
    rownames(dopa_tab) = dose
    print(dopa_tab)
    
  }, rownames = TRUE, striped = T)
  
############################################### NIKARDIPIN #########################################
  
  nica_r <- reactive(((input$nica_dose * 60 * input$nica_BB)/((input$nica_amount * 10000)/as.numeric(input$nica_syringe))))
  
  output$nica_hour_rate <- renderText({
    ncr <- (input$nica_dose * 60 * input$nica_BB)/1000
    print(paste0("Hourly Dose Equivalent: ", ncr , " mg/hour"))
  })
  
  output$nica_rate <- renderText({
    print(paste0("Infusion Rate: ", nica_r(), " ml/hour"))
  })
  output$nica_eta <- renderText({
    nica_eta <- round(as.numeric(input$nica_syringe)/nica_r(),2)
    print(paste0("Estimated Refill Time: ", nica_eta, " hour"))
  })
  
  output$nica_table <- renderTable({
    dose <- seq(from=0.5, to=5, by=0.5)
    bb <- bb()
    
    nica_tab = matrix(NA, nrow = length(dose), ncol = length(bb))
    for (i in 1:length(dose)) {
      for (j in 1:length(bb)) {
        nica_tab[i,j] = print(dose[i] * 60 * bb[j] * as.numeric(input$nica_syringe) / (10000 * input$nica_amount))
      }
    }
    colnames(nica_tab) = bb
    rownames(nica_tab) = dose
    print(nica_tab)
    
  }, rownames = TRUE, striped = T)

############################################### ISDN ###############################################
  
  isdn_r <- reactive(((input$isdn_dose)/((input$isdn_amount * 10)/as.numeric(input$isdn_syringe))))
  
  output$isdn_rate <- renderText({
    print(paste0("Infusion Rate: ", isdn_r(), " ml/hour"))
  })
  output$isdn_eta <- renderText({
    isdn_eta <- round(as.numeric(input$isdn_syringe)/isdn_r(),2)
    print(paste0("Estimated Refill Time: ", isdn_eta, " hour"))
  })
  
  
  
############################################### NTG ################################################
  
  ntg_r <- reactive(((input$ntg_dose * 60)/((input$ntg_amount * 10000)/as.numeric(input$ntg_syringe))))
  
  output$ntg_rate <- renderText({
    print(paste0("Infusion Rate: ", ntg_r(), " ml/hour"))
  })
  output$ntg_eta <- renderText({
    ntg_eta <- round(as.numeric(input$ntg_syringe)/ntg_r(),2)
    print(paste0("Estimated Refill Time: ", ntg_eta, " hour"))
  })
  
  
############################################### FUROSEMID ##########################################
  
############################################### INSULIN ############################################
  
############################################### FENTANYL ###########################################
  
############################################### MORFIN #############################################
  
############################################### PROPOFOL ###########################################
  
############################################### MIDAZOLAM ##########################################
  
############################################### ROCURONIUM #########################################
  
############################################### PANTOPRAZOLE #######################################
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)

# sudo cp ~/project/lazy_infusion/lazy_infusion/app.R /srv/shiny-server/lazy_infusion/
