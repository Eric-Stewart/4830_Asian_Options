library(ggplot2)
library(reshape2)
library(shiny)
library(xtable)
library(gridExtra)

# Define server
shinyServer(function(input, output) {
  
  
  #*****************************************************************
  # Shared Reactive functions
  # http://rstudio.github.com/shiny/tutorial/#inputs-and-outputs
  #******************************************************************      
  # Get stock data

  #*****************************************************************
  # Not Reactive helper functions
  #*****************************************************************

# Make table
makeStatsTable <- function() {    
   #out = getData()

  #y = out
  #y.df<-data.frame(y)
  y.df <- data.frame(  
    input$stock_price,
    input$sigma,
    input$time_yr,
    input$dividends,
    input$rate,
    input$strike_price, 
    input$number_stock_price, 
    input$simulation_number) 
    
  colnames(y.df)[1]<-"Stock Price"
  colnames(y.df)[2]<-"Sigma"
  colnames(y.df)[3]<-"Time in Years"
  colnames(y.df)[4]<-"Dividends"
  colnames(y.df)[5]<-"Interest Rate"
  colnames(y.df)[6]<-"Strike Price"
  colnames(y.df)[7]<-"Days"
  colnames(y.df)[8]<-"Simulated Number"
xtable(y.df)

    
}
# Make stock plot
makeStockPlot <- function() {    
  
  stock_price<-as.numeric(input$stock_price)
  sigma<-as.numeric(input$sigma)
  time_yr<-as.numeric(input$time_yr)
  dividends<-as.numeric(input$dividends)
  rate<-as.numeric(input$rate)
  strike_price<-as.numeric(input$strike_price) 
  number_stock_price<-as.numeric(input$number_stock_price) 
  simulation_number<-as.numeric(input$simulation_number)
  ss_steps<-4

  
  #Standard Monte Carlo  
  stock_matrix <- matrix(data=NA, nrow=simulation_number, ncol=number_stock_price)
  G<-rep(NA,simulation_number)
  A<-rep(NA,simulation_number)
  
  #Antithetic Variate Method
  stock_matrix_av <- matrix(data=NA, nrow=simulation_number, ncol=number_stock_price)
  G_av<-rep(NA,simulation_number)
  A_av<-rep(NA,simulation_number)
  
  #Stratified sampling
  stock_matrix_ss <- matrix(data=NA, nrow=simulation_number, ncol=number_stock_price)
  G_ss<-rep(NA,simulation_number)
  A_ss<-rep(NA,simulation_number)
  
  for (j in 1:simulation_number){
    #  set.seed(j) 
    #Generate uniform  (use the same for AV, need to modify for SS) 
    #minus one, start with first stock price
    uniform_simulated <- as.vector(runif(number_stock_price-1, 0, 1))
    u_s_100 <- as.vector(runif(100-1, 0, 1))
    #uniform_simulated_ss <- as.vector(u_s_100+(0:(100-1)))/(100)
    uniform_simulated_ss <- replicate((number_stock_price-1), as.vector((runif(ss_steps, 0, 1))+(0:(ss_steps-1)))/ss_steps)
    
    #    uniform_simulated_ss <- sample(uniform_simulated_ss, length(uniform_simulated_ss), replace=FALSE)
    #    uniform_simulated_ss <- sample(uniform_simulated_ss)
    norm_simulated <- qnorm(uniform_simulated)
    norm_simulated_av <- -1*qnorm(uniform_simulated)
    norm_simulated_ss <- qnorm(uniform_simulated_ss)
    norm_simulated_ss <- cbind(rep(1,ss_steps),norm_simulated_ss)
    
    #Generate Stock Prices
    stock_simulated<-NULL
    stock_simulated<-c(stock_price, rep(NA, length(norm_simulated)))
    #AV
    stock_simulated_av<-NULL
    stock_simulated_av<-c(stock_price, rep(NA, length(norm_simulated)))
    #SS
    stock_simulated_ss<-NULL
    stock_simulated_ss<-t(replicate((number_stock_price), c(stock_price, rep(NA, length(norm_simulated)))))
#    stock_simulated_ss<-stock_price*
#      exp((rate - dividends - .5*sigma^2)*
#            (time_yr/99)+
#            sigma*sqrt(time_yr/99)*norm_simulated_ss)
#    stock_simulated_ss<-rowMeans(stock_simulated_ss)
#    stock_simulated_ss<-as.vector(c(stock_price, stock_simulated_ss))
    
    
    
    #stock_simulated_ss<-c(stock_price, rep(NA, length(norm_simulated_ss)))
    
    n<-length(stock_simulated) - 1
    for (i in 1:n){
      stock_simulated[i+1]<-
        stock_simulated[i]*
        exp((rate - dividends - .5*sigma^2)*
              (time_yr/number_stock_price)+
              sigma*sqrt(time_yr/number_stock_price)*norm_simulated[i])
      
      #for AV
      stock_simulated_av[i+1]<-
        stock_simulated_av[i]*
        exp((rate - dividends - .5*sigma^2)*
              (time_yr/number_stock_price)+
              sigma*sqrt(time_yr/number_stock_price)*norm_simulated_av[i])
      #for SS
          stock_simulated_ss[,i+1]<-
            mean(stock_simulated_ss[,i])*
            exp((rate - dividends - .5*sigma^2)*
                  (1/number_stock_price)+
                  sigma*sqrt(1/number_stock_price)*norm_simulated_ss[,i+1])
    }
    #Populate stock paths
    stock_matrix[j,]<-stock_simulated
    stock_matrix_av[j,]<-stock_simulated_av
    stock_matrix_ss[j,]<-colMeans(stock_simulated_ss)
    
    #Geometric Average Price
    G[j]<-exp(mean(log(stock_simulated)))
    A[j]<-mean(stock_simulated)
    uniform_simulated<-NULL
    #AV
    G_av[j]<-exp(mean(log(stock_simulated_av)))
    A_av[j]<-mean(stock_simulated_av)
    #SS
    G_ss[j]<-exp(mean(log(colMeans(stock_simulated_ss))))
    A_ss[j]<-mean(colMeans(stock_simulated_ss))
    uniform_simulated_ss<-NULL
    
    
  }
  
  
  melt_stock<-melt(stock_matrix, id=stock_matrix[1,])
  colnames(melt_stock)[1]<-"Simulated"
  colnames(melt_stock)[2]<-"Day"
  colnames(melt_stock)[3]<-"Price"
  #for AV
  melt_stock_av<-melt(stock_matrix_av, id=stock_matrix_av[1,])
  colnames(melt_stock_av)[1]<-"Simulated"
  colnames(melt_stock_av)[2]<-"Day"
  colnames(melt_stock_av)[3]<-"Price"
  #for SS
  melt_stock_ss<-melt(stock_matrix_ss, id=stock_matrix_ss[1,])
  colnames(melt_stock_ss)[1]<-"Simulated"
  colnames(melt_stock_ss)[2]<-"Day"
  colnames(melt_stock_ss)[3]<-"Price"
  
  p <- ggplot(melt_stock,aes(x=Day,y=Price,colour=Simulated,group=Simulated)) +
    geom_line() +
    ggtitle("Standard Monte Carlo")    
  p_av <- ggplot(melt_stock_av,aes(x=Day,y=Price,colour=Simulated,group=Simulated)) + 
    geom_line() +
    ggtitle("Antithetic Variate Method")
  p_ss <- ggplot(melt_stock_ss,aes(x=Day,y=Price,colour=Simulated,group=Simulated)) + 
    geom_line() +
    ggtitle("Stratified Sampling Method")
  
  result <- list(p=p,
                 t=stock_matrix,
                 a=A,
                 g=G,
                 C_gap=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply(G, function(x) max(x-strike_price,0))),
                 C_aap=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply(A, function(x) max(x-strike_price,0))),
                 P_gap=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply(G, function(x) max(strike_price-x,0))),
                 P_aap=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply(A, function(x) max(strike_price-x,0))),
                 C_gas=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply((stock_matrix[,number_stock_price]-G), function(x) 
                     max(x,0))),
                 C_aas=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply((stock_matrix[,number_stock_price]-A), function(x) 
                     max(x,0))),
                 P_gas=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply((G-stock_matrix[,number_stock_price]), function(x) 
                     max(x,0))),
                 P_aas=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply((A-stock_matrix[,number_stock_price]), function(x) 
                     max(x,0))),
                 #AV
                 p_av=p_av,
                 t_av=stock_matrix_av,
                 a_av=A_av,
                 g_av=G_av,
                 C_gap_av=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply(G_av, function(x) max(x-strike_price,0))),
                 C_aap_av=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply(A_av, function(x) max(x-strike_price,0))),
                 P_gap_av=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply(G_av, function(x) max(strike_price-x,0))),
                 P_aap_av=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply(A_av, function(x) max(strike_price-x,0))),
                 C_gas_av=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply((stock_matrix_av[,number_stock_price]-G_av), function(x) 
                     max(x,0))),
                 C_aas_av=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply((stock_matrix_av[,number_stock_price]-A_av), function(x) 
                     max(x,0))),
                 P_gas_av=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply((G_av-stock_matrix_av[,number_stock_price]), function(x) 
                     max(x,0))),
                 P_aas_av=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply((A_av-stock_matrix_av[,number_stock_price]), function(x) 
                     max(x,0))),
                 #SS
                 p_ss=p_ss,
                 t_ss=stock_matrix_ss,
                 a_ss=A_ss,
                 g_ss=G_ss,
                 C_gap_ss=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply(G_ss, function(x) max(x-strike_price,0))),
                 C_aap_ss=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply(A_ss, function(x) max(x-strike_price,0))),
                 P_gap_ss=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply(G_ss, function(x) max(strike_price-x,0))),
                 P_aap_ss=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply(A_ss, function(x) max(strike_price-x,0))),
                 C_gas_ss=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply((stock_matrix_ss[,number_stock_price]-G_ss), function(x) 
                     max(x,0))),
                 C_aas_ss=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply((stock_matrix_ss[,number_stock_price]-A_ss), function(x) 
                     max(x,0))),
                 P_gas_ss=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply((G_ss-stock_matrix_ss[,number_stock_price]), function(x) 
                     max(x,0))),
                 P_aas_ss=exp(-rate*time_yr)*
                   (1/simulation_number)*
                   sum(sapply((A_ss-stock_matrix_ss[,number_stock_price]), function(x) 
                     max(x,0)))
                 
  )
  return(result)
    
  }
  
  
  #*****************************************************************
  # Update plot(s) and table(s)
  #******************************************************************    	
  # Generate a plot
  output$stockPlot <- reactivePlot(function() {
    
    temp_plot<-makeStockPlot()
    print(grid.arrange(temp_plot$p, temp_plot$p_av, temp_plot$p_ss, ncol=1))
  }, height = 400, width = 800)
  
  
  output$statsTable <- renderTable({
#    head(makeStatsTable(),
    temp_table<-makeStockPlot()
#    xtable(as.data.frame(c(temp_table[5:12],temp_table[17:24],temp_table[29:36])))
    temp_e<-c("", "Monte Carlo",  "Monte Carlo",  "Antithetic Variate ",	"Antithetic Variate ",	"Stratified Sampling",	"Stratified Sampling",
              "", "Arithmetic ",	"Geometric ",	"Arithmetic ",	"Geometric ",	"Arithmetic ",	"Geometric ",
              "Price Call", round(temp_table$C_aap, 2),  round(temp_table$P_gap, 2),	round(temp_table$C_aap_av, 2),	round(temp_table$C_gap_av, 2),	round(temp_table$C_aap_ss, 2),	round(temp_table$C_gap_ss, 2),
              "Price Put",  round(temp_table$P_aap, 2),  round(temp_table$C_gap, 2),	round(temp_table$P_aap_av, 2),	round(temp_table$P_gap_av, 2),	round(temp_table$P_aap_ss, 2),	round(temp_table$P_gap_ss, 2),
              "Strike Call",round(temp_table$C_aas, 2),  round(temp_table$C_gas, 2),	round(temp_table$C_aas_av, 2),	round(temp_table$C_gas_av, 2),	round(temp_table$C_aas_ss, 2),	round(temp_table$C_gas_ss, 2),
              "Strike Put", round(temp_table$P_aas, 2),  round(temp_table$P_gas, 2),	round(temp_table$P_aas_av, 2),	round(temp_table$P_gas_av, 2),	round(temp_table$P_aas_ss, 2),	round(temp_table$P_gas_ss, 2))
    
    #print(paste(xtable(t(matrix(temp_e, 7)), caption = "Option: Asian")         ))
    xtable(t(matrix(temp_e, 7)))
  
    
  })
  
  #*****************************************************************
  # Download
  #******************************************************************    
  # Download pdf report
  output$downloadReport <- downloadHandler(
    filename = 'report.pdf',
    content = function(file) {
      pdf(file = file, width=8.5, height=11)
      
      #makeStockPlot()
      temp_plot<-makeStockPlot()
      print(grid.arrange(temp_plot$p, temp_plot$p_av, temp_plot$p_ss, ncol=1))
      
      #plot.table(makeStatsTable())
      #makeStatsTable()
      
      temp_table<-makeStockPlot()
      head(xtable(as.data.frame(c(temp_table[5:12],temp_table[17:24]))),
           n=nrow(makeStatsTable()))
      
      dev.off()
    }
  )	
  
  # Download csv data
  output$downloadData <- downloadHandler(
    filename = 'data.csv',
    content = function(file) {
      write.csv(makeStatsTable(), file=file, row.names = F)
    }
  )	
  
 
  
})
