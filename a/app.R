library(shiny)

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      # sliderInput("integer", "Integer:",
      #             min = 0, max = 1000,
      #             value = 500),
      # 
      # Input: Decimal interval with step value ----
      sliderInput("mean", "Mean:",
                  min = -1, max = 1,
                  value = -0.44, step = 0.01),
      
      # Input: Specification of range within an interval ----
      sliderInput("sd", "SD:",
                  min = 0, max = 1,
                  value = 0.24, step = 0.01),
      
      sliderInput("priorsd", "PriorSigma",
                 min = 0, max = 1,
                 value = 1, step = 0.01),
      

      
      sliderInput("pi", "PiOpt:",
                  min = 0, max = 1,
                  value = 1, step = 0.01)
      # 
      # # Input: Custom currency format for with basic animation ----
      # sliderInput("format", "Custom Format:",
      #             min = 0, max = 10000,
      #             value = 0, step = 2500,
      #             pre = "$", sep = ",",
      #             animate = TRUE),
      # 
      # # Input: Animation with custom interval (in ms) ----
      # # to control speed, plus looping
      # sliderInput("animation", "Looping Animation:",
      #             min = 1, max = 2000,
      #             value = 1, step = 10,
      #             animate =
      #               animationOptions(interval = 300, loop = TRUE)
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
      
    )
  )
)
# Define server logic for slider examples ----




server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    #   mu    <- input$mean
    #   sd <- input$sd
    #   
    # hist(rnorm(10000,mean = mu,sd = sd ))
    
    
    
    
    
    
    
    
    post_mean=function(priormean,priorsd,datamean,datase){
      priorsd^2/(datase^2+priorsd^2)*datamean+datase^2/(datase^2+priorsd^2)*priormean}
    
    post_var=function(priorsd,datase){
      1/(1/datase^2+1/priorsd^2)}
    
    ###Here, we create a funbciton for the posterior weights as p(K|D)=p(D|K)*p(K)/sum_k[p(D|K)*p(K)] where p(K)= 1-pi0 for chosen level of skepticism
    pipost=function(pi1,dataSE,datamean,priorsd){
      sdlik=sqrt(priorsd^2+dataSE^2)
      pi0=1-pi1
      joint=pi1*dnorm(datamean,mean = 0,sd=sdlik)
      marginal=pi1*dnorm(datamean,mean = 0,sd=sdlik)+pi0*dnorm(datamean,mean = 0,sd=dataSE)
      pip=joint/marginal
      return(pip)}
    
    
    pi=input$pi
    dse=input$sd
    dm=input$mean
    priorsd=input$priorsd
    sim=1000
    ##more skeptical prior
    
    priorid=rbinom(sim,prob=pi,size=1)
    priov=sapply(priorid,function(x){x*rnorm(1,0,priorsd)})
    hist(priov, prob=T, main="Histogram With Fitted Density Curve, bw=.5")
    lines(density(priov, bw=.5),
          col="red",
          lwd=2) 
    
    
    
    p1=input$pi
  
    p=pipost(p1,dataSE=dse,datamean=dm,priorsd=priorsd)###computer posterior weight on nonzero component using chosen prior weight
    s=rbinom(n = sim,size=1,prob=p)##create a list indexing whether comes from null or real depending on posterior weight, where for each simulation, an RV (0,1) is simulated from Binomial(n=1,size=1) according to posterio weight
    pm=sapply(s,function(s){post_mean(priormean=0,priorsd=s*priorsd,datamean=dm,datase=dse)})##depending on whether null or alternative chosen, simulate posterior mean of distribution
    ps=sqrt(sapply(s,function(s){post_var(priorsd=s*1,datase=dse)}))#
    b=sapply(seq(1:length(pm)),function(x){rnorm(1,mean = pm[x],sd=ps[x])})
    
   

    plot(density(priov),
          col="red",
          lwd=2,ylim=c(0,5),xlim=c(-2,2));

    lines(density(b),
          col="blue",
          lwd=2) 
    legend(x=0.5,y=2,legend = c("Prior","Posterior"),col=c("red","blue"),lty=c(1,1),lwd=c(1,1))
    
    
  })
  
}














# Run the application 
shinyApp(ui = ui, server = server)
