library(shiny)
  library(plot3D)
  source("unimodal.R")
  source("multimodal.R")
  source("TLBO.R")
  source("adPSO.R")
  source("ISA.R")
  z=cbind(seq(-100,100,2),seq(-100,100,2))
  sph_data=matrix(nrow=length(z[,1]),ncol=length(z[,2]))
  ras_data=matrix(nrow=length(z[,1]),ncol=length(z[,2]))
  ros_data=matrix(nrow=length(z[,1]),ncol=length(z[,2]))
  for(i in 1:length(z[,1])){
    for(j in 1:length(z[,2])){
      sph_data[i,j]=sphere(cbind(z[i,1],z[j,2]))
      ras_data[i,j]=rastrigin(cbind(z[i,1],z[j,2]))
      ros_data[i,j]=rosenbrock(cbind(z[i,1],z[j,2]))
    }
  }
  
# Define server logic required to draw plot
shinyServer(
function(input, output,session) {
   searchspace <- reactive({
                          switch(input$func, 
                         "Sphere" = sph_data,
                         "Rastrigin" = ras_data,
                         "Rosenbrock" = ros_data)})
   generation<-reactive({input$gen})
   class_size<-reactive({input$pop})
   fitness<-reactive({input$func})

   res_points<-reactive({
                        switch(input$opt_func,
                        "LETLBO (Zou et al 2015)"=TLBO(class_size(),1,2,-100,100,fitness(),generation()),
                        "adaptive PSO"=AD_PSO(class_size(),1,2,-100,100,fitness(),generation()),
                        "ISA (Gandomi 2014)"=ISA(class_size(),1,2,-100,100,fitness(),generation()))})
   observe({
     res_points()
     updateSliderInput(session, "gen_num", value = 2,
                       min = 1, max = length(res_points())-1,step=1)
   })
   
   text_PSO<-reactive({
     switch(input$opt_func,
            "LETLBO (Zou et al 2015)"="Learning Experience with Teaching Learning Based Optimisation method",
            "adaptive PSO"="adaptive Particle Swarm Optimisation method",
            "ISA (Gandomi 2014)"= "interior search algorithm method")})
   
   gen_plot<-reactive(input$gen_num)
   output$text<-renderText({
     if(input$gen_num > input$gen){paste("Select a smaller generation to plot and execute or increase maximum number of generations")}else{
     paste("You selected a",text_PSO(), " and the minimum at z:", res_points()[[length(res_points())]][3], " x:",res_points()[[length(res_points())]][1], " and y:",res_points()[[length(res_points())]][2])
   }})
   
   
   output$text2<-renderText({
     if(gen_plot()<(length(res_points()))){paste("Input Okay")}else{
       paste("Input is greater than generation cycles. Note, error criterion may have been met before maximum number of generations was reached. Select smaller number!")
     }})
   
   output$plot_fun <- renderPlot({
     res_points()
     ribbon3D(x=z[,1],y=z[,2],z=searchspace(),xlim=c(-100,100),ylim=c(-100,100),zlim=range(searchspace()),zlab="", xlab="",ylab="",alpha=0.7,theta=40,phi=-10,ticktype="simple")
     points3D(x=res_points()[[1]][,1],y=res_points()[[1]][,2],z=res_points()[[1]][,3],col="red",theta=40,phi=-10,ticktype="simple",xlim=c(-100,100),ylim=c(-100,100),zlim=range(searchspace()),add=TRUE)
   })
   
  output$plot_fun <- renderPlot({
    i<-gen_plot() 
    theta_plot<-reactive(input$theta)
    phi_plot<-reactive(input$phi)
    if(i<(length(res_points())-1)){
          ribbon3D(x=z[,1],y=z[,2],z=searchspace(),xlim=c(-100,100),ylim=c(-100,100),zlim=range(searchspace()),zlab="", xlab="",ylab="",alpha=0.5,theta= theta_plot(),phi=phi_plot(),ticktype="simple")
          points3D(x=res_points()[[i]][,1],y=res_points()[[i]][,2],z=res_points()[[i]][,3],col="red",theta= theta_plot(),phi= phi_plot(),ticktype="simple",xlim=c(-100,100),ylim=c(-100,100),zlim=range(searchspace()),add=TRUE)
    }
    })
       
 })

