library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Optimisation"),
  fluidRow(
    column(3,
           h4("Select Optimisation"),
           selectInput("func", label = h5("Select benchmark function"), 
                       choices = c("Sphere", "Rastrigin",
                                   "Rosenbrock"), selected = "Sphere"),
           radioButtons("opt_func", label = h5("Select optimisation algorithm"),
                        choices = c("LETLBO (Zou et al 2015)", "adaptive PSO","ISA (Gandomi 2014)"),selected = "LETLBO (Zou et al 2015)"),
           sliderInput("pop",label=h5("Population size"),
                       min=10, max=100, value=20),
           sliderInput("gen",label=h5("Number of maximum generations"),
                       min=100, max=2100, value=100),
           background="grey"),
    column(6,h4("Results"),
           textOutput("text"),
           plotOutput("plot_fun")),
  column(3,h4("plot options"),
           sliderInput("phi",label=h5("Vertical plot rotation"),
                       min=-180, max=180, value=-10),
           sliderInput("theta",label=h5("Horizontal  plot rotation"),
                       min=-180, max=180, value=40),
           sliderInput("gen_num",label=h5("Select different generation to plot"),min=1, max=2100,value=2),
           textOutput("text2"))
  )
))