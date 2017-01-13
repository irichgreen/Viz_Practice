# A shiny app for estimating the growth of electrical generation over a 100 year time period
# resulting from a one-time investment with reinvestment of proceeds under uncertainty.
#  Programmed by John Yagecic in December 2016
#  JYagecic@gmail.com


library(shiny)
library(ggplot2)

ui<-fluidPage(
    sidebarLayout(
        sidebarPanel(
            
            tags$h3("Input Variables"),
            sliderInput("invest", "One Time Investment $", min=500000, max=10000000, value=1000000),
            sliderInput("arcost", "Initial 4kW PVC Array Cost $", min=10000, max=50000, value=c(25000, 35000)),
            sliderInput("PVCinflate", "PVC Annual Inflation Rate %", min=-10, max=10, value=c(-5, 5)),
            sliderInput("electcost", "Initial Electricity cost per kWh $", min=0.05, max=0.25, value=c(0.08, 0.18)),
            sliderInput("elecinflate", "Electricity Annual Inflation Rate %", min=-10, max=10, value=c(-5, 5)),
            sliderInput("maxfail", "Max Annual PVC Failure Rate %", min=0, max=5, value=1),
            sliderInput("generated", "kWh generated per year", min=2000, max=10000, value=c(5004, 5411)),
            tags$a(href="http://pvwatts.nrel.gov/pvwatts.php", "PVWatts Calculator (NREL)")
            
            
        ),
        mainPanel(
            tags$h2("Impact of a One Time Investment in Solar PVC with reinvestment of income:  A Monte Carlo Analysis"),
            
            #tableOutput("table"),
            "An app for estimating the growth of electrical generation over a 100 year time period",
            "resulting from a one-time investment in solar PVC generation with reinvestment of proceeds under uncertainty.",
            
            plotOutput("box"),
            
            "Programmed by John Yagecic, P.E.  (JYagecic@gmail.com)",
            tags$br(),
            tags$br(),
            tags$br(),
            tags$a(href="https://en.wikipedia.org/wiki/Monte_Carlo_method", "More about Monte Carlo method"),
            tags$br(),
            tags$br(),
            "All distributions are uniform distributions with value ranges set by the slider bars.",
            "Computations are made using 100 Monte Carlo iterations.  Estimates of the range of annual generation",
            "by location in the US are available using the",
            tags$a(href="http://pvwatts.nrel.gov/pvwatts.php", "PVWatts Calculator (NREL) tool."),
            tags$br(),
            "If you use this product or the underlying code in any professional or academic product, please consider ",
            "using a citation such as:",
            tags$br(),
            tags$br(),
            "Yagecic, John, December 2016.  Impact of a One Time Investment in Solar PVC with reinvestment of income:  A Monte Carlo Analysis Shiny app.",
            tags$br(),
            tags$br(),
            tags$a(href="https://github.com/JohnYagecic/MonteCarloManningsShinyApp", "Get the script")
        )
    )
)

server<-function(input, output){
    
    maxfail <- reactive({input$maxfail[1]/100})
    
    array.cost.min<-reactive({input$arcost[1]})
    array.cost.max<-reactive({input$arcost[2]})
    
    electric.cost.min<-reactive({input$electcost[1]})
    electric.cost.max<-reactive({input$electcost[2]})
    
    onetimeinvest<-reactive({input$invest[1]})
    
    electric.inflate.min<-reactive({input$elecinflate[1]/100})
    electric.inflate.max<-reactive({input$elecinflate[2]/100})
    
    array.inflate.min<-reactive({input$PVCinflate[1]/100})
    array.inflate.max<-reactive({input$PVCinflate[2]/100})
    
    generated.min<-reactive({input$generated[1]})
    generated.max<-reactive({input$generated[2]})
    
    
    output$box<-renderPlot({
        
        
        solar<-data.frame(year=1:100)
        
        solar$MCloop<-NA
        solar$array.cost<-NA
        solar$array.count<-NA
        solar$inflation.array<-NA
        solar$inflation.electric<-NA
        solar$electric.cost<-NA
        solar$revenue<-NA
        solar$electric.produced<-NA
        
        #producedatyear100<-NA
        
        for (j in 1:100){ # Monte Carlo loop
            
            
            for (k in 1:nrow(solar)){ # year loop 1 to 100
                
                solar$MCloop[k]<-j
                
                if (k==1){ # year 1
                    array.cost.initial<-reactive({runif(1,array.cost.min(),array.cost.max())})
                    electric.cost.initial<-reactive({runif(1,electric.cost.min(),electric.cost.max())})
                    
                    solar$array.cost[k]<-array.cost.initial()
                    solar$array.count[k]<-onetimeinvest()/solar$array.cost[k]
                    solar$electric.produced[k]<-runif(1,generated.min(), generated.max()) # from PVWatts for Local Area
                    solar$electric.cost[k]<-electric.cost.initial()
                    solar$revenue[k]<-solar$electric.produced[k]*solar$electric.cost[k]
                    
                }
                if (k>1){ # years > 1
                    
                    solar$array.cost[k]<-solar$array.cost[k-1]*(1+runif(1,array.inflate.min(),array.inflate.max()))
                    solar$array.count[k]<-solar$array.count[k-1]+(solar$revenue[k-1]/solar$array.cost[k])
                    solar$electric.produced[k]<-solar$array.count[k]*runif(1,generated.min(), generated.max()) #pvWatts for Philadelphia
                    solar$electric.cost[k]<-solar$electric.cost[k-1]*(1+runif(1,electric.inflate.min(),electric.inflate.max()))
                    solar$revenue[k]<-solar$electric.produced[k]*solar$electric.cost[k]
                }
                
                solar$array.count[k]<-solar$array.count[k]*(1-runif(1,0,maxfail())) #system failure at end of year
                
                # do 100 monte carlo iterations and bind the DFs together
                if (k==100){
                    #producedatyear100<-c(producedatyear100, solar$electric.produced[k])
                    if (j==1){
                        solarFull<-solar[solar$year>1,] # don't use year 1
                    }
                    if (j>1){
                        solarFull<-rbind(solarFull, solar[solar$year>1,])
                    }
                }
            }
            
        }
        
        FinalSolar<-reactive({solarFull})
        
        #for (j in 1:100){
        myymax<-quantile(solarFull$electric.produced, probs=0.95, na.rm=FALSE)
        #solarplot<-solarFull[solarFull$MCloop==j,]
        #if (j==1){
        #  plot(solarplot$year, solarplot$electric.produced, type="l", col="gray", xlab="Year",
        #       ylab="kilowatt hours", ylim=c(0,myymax))
        #}
        #if (j>1){
        #  points(solarplot$year, solarplot$electric.produced, type="l", col="gray")
        #}
        #}
        
        boxplot(solarFull$electric.produced ~ solarFull$year, ylim=c(0,myymax), xlab="year",
                ylab="Total kilowatt hours per year")
    })
    
    
    
}

shinyApp(ui=ui, server=server)
