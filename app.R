library(shiny)
library(quantmod)
library(its)
library(PortfolioAnalytics)
library(forecast)
library(plotly)
library(timeSeries)

acciones<-c('No aplica'="",'Alfa'='ALFAA.MX','Alpek'='ALPEKA.MX','Alsea'='ALSEA.MX',
            'America Movil'='AMXL.MX','Grupo Aeroportuario del Sureste'='ASURB.MX',
            'Grupo Financiero Banregio'='GFREGIOO.MX','Becle Cuervo'='CUERVO.MX',
            'Grupo Bimbo'='BIMBOA.MX','Cemex'='CEMEXCPO.MX','Coca-Cola Femsa'='KOFL.MX',
            'Grupo Elektra'='ELEKTRA.MX','Grupo Financiero Banorte'='GFNORTEO.MX',
            'Genomma Lab Internacional'='LABB.MX','Gentera'='GENTERA.MX','Grupo Mexico'='GMEXICOB.MX',
            'Gruma'='GRUMAB.MX','Grupo Aeroportuario del Pacifico'='GAPB.MX',
            'Grupo Carso'='GCARSOA1.MX','Grupo Financiero Inbursa'='GFINBURO.MX',
            'Grupo Financiero Santander Mexico'='SANMEXB.MX','Grupa Lala'='LALAB.MX',
            'Grupo Televisa'='TLEVISACPO.MX','Infraestructura Energetica Nova'='IENOVA.MX',
            'Kimberly-Clark de Mexico'='KIMBERA.MX','Megacable Holdings'='MEGACPO.MX',
            'Mexichem'='MEXCHEM.MX','Nemak'='NEMAKA.MX','Grupo Aeroportuario del Centro Norte'='OMAB.MX',
            'Industrias Penoles'='PE&OLES.MX','Promotora y Operadora de Infraestructura'='PINFRA.MX',
            'Controladora Vuela Compania de Aviacion'='VOLARA.MX','Wal-Mart de Mexico'='WALMEX.MX'
)

# Define UI for application 
ui <- fluidPage(
  # Application title
  titlePanel("Portafolio de inversion para principiantes"),
  
  tabsetPanel(
    tabPanel("Informacion", textOutput("error"),
             tableOutput("texto")),
    tabPanel("Rendimiento",tableOutput("original"),tableOutput("mejor")),
    tabPanel("Estimacion",textOutput("infofor"),plotlyOutput("forecast"),plotlyOutput("forecastc"),
             textOutput("infoform"),plotlyOutput("forecastm"),plotlyOutput("forecastmc"))
  ),
  
  hr(),
  
  # Show a plot of the generated distribution
  fluidRow (
    column(4,selectInput(
      inputId="activo1", label="Elige tu accion", 
      choices=acciones, multiple = FALSE,
      width = NULL, size = NULL),
      selectInput(inputId="activo2", label="Elige tu accion",
                  choices=acciones, multiple = FALSE,
                  width = NULL, size = NULL)),
    column(4,selectInput(inputId="activo3", label="Elige tu accion",
                         choices=acciones, multiple = FALSE,
                         width = NULL, size = NULL),
           selectInput(inputId="activo4", label="Elige tu accion",
                       choices=acciones, multiple = FALSE,
                       width = NULL, size = NULL)),
    column(4,selectInput(inputId="activo5", label="Elige tu accion",
                         choices=acciones, multiple = FALSE,
                         width = NULL, size = NULL),
           numericInput(inputId="dinero", label="Dinero a invertir",
                        value=0, min=0, max=NA,step=NA,width=NULL),
           actionButton("do", "Click Me")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(
    eventExpr = input$do,handlerExpr = {
      fechasf<-seq(as.Date("2017-11-02"), as.Date("2018-02-01"), "days")
      ## Descarga de activos
      descarga<-function(activos){
        dataEnv <- new.env()
        getSymbols(activos,src="yahoo",from="2015-11-01", to="2017-11-01", env=dataEnv)
        plist <- eapply(dataEnv, Ad)
        pframe <- do.call(merge, plist)
        pframe <- as.timeSeries.xts(pframe)
        pframe <- na.locf.default(pframe,na.rm=TRUE,fromLast=TRUE)
        pframe<-as.data.frame(pframe)
        return(pframe)
      }
      
      ## Rendimiento de activos
      rendimientos<-function(precios){
        rendimientos <- log(precios[2:nrow(precios), ] / precios[1:(nrow(precios)-1), ])
        return(rendimientos)
      }
      
      ## rendimiento, riesgo y titulos de 1 activo
      meansd<-function(rend,capital,ultimo){
        meanma<-data.frame(nrow=1,ncol=3)
        meanma[1,1]<-paste("$",round(mean(rend)*capital,2))
        meanma[1,2]<-paste("$",round(sd(rend,na.rm=TRUE)*capital,2))
        meanma[1,3]<-round(capital/ultimo)
        colnames(meanma)<-c("Rendimiento ($)","Riesgo ($)","Titulos")
        return(meanma)
      }
      
      ## matriz
      flattenSquareMatrix <- function(m) {
        if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
        if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
        ut <- upper.tri(m)
        data.frame(i = rownames(m)[row(m)[ut]],
                   j = rownames(m)[col(m)[ut]],
                   cor=t(m)[ut],
                   p=m[ut])
      }
      
      ## seleccion de activos
      seleccion<-function(activos){
        activos<-sort(activos)
        des<-descarga(activos)
        des<-des[ , order(names(des))]
        rendi<-rendimientos(des)
        rendprom<-colMeans(rendi)
        numeros<-c()
        si<-c()
        cor<-c()
        for(i in 1:length(rendprom)){
          if(rendprom[i]<0){
            numeros<-c(numeros,i)
          }else
            si<-c(si,i)
        }
        if(length(numeros)!=0){
          rendprom<-rendprom[-numeros]
          nombres<-activos[-numeros]
        }else{
          nombres<-activos}
        if(length(nombres)!=1){
          rendimientosn<-rendi[,si]
          colnames(rendimientosn)<-c(nombres)
          corrend<-flattenSquareMatrix(cor(rendimientosn, method="spearman" )) 
          buscar<-which(corrend[3]>.3)#
          if(length(buscar)!=0){
            for (i in 1:length(buscar)){
              ac1<-corrend[buscar[i],1]
              ac2<-corrend[buscar[i],2]
              cor<-c(cor,which(nombres==ac1))
              cor<-c(cor,which(nombres==ac2))
            }
            doble<-cor[!duplicated(cor)]
            rendcov<-rendprom[-doble]
            nombres<-nombres[-doble]
          }
        }else{
          rendcov<-rendprom}
        return(nombres)}
      
      
      ## rendimiento, riesgo, participaciones y titulo de un portafolio
      portafolio<-function(acciones,rendimiento,minimo,maximo,capital,info){
        Port1 <- portfolio.spec(assets=acciones)
        Port1 <- add.constraint(portfolio=Port1,
                                type="full_investment") 
        # Restriccion 2: Limites superior e inferior para el valor de los pesos individuales
        Port1 <- add.constraint(portfolio=Port1,
                                type="box", 
                                min=minimo, max=maximo)
        # Restricci?n 3: Objetivo de rendimiento
        Port1 <- add.objective(portfolio=Port1, type="return", name="mean")
        Port1 <- optimize.portfolio(R=rendimiento, portfolio=Port1, optimize_method="random",
                                    trace=TRUE, search_size=5000)
        Portafolios <- vector("list", length = length(Port1$random_portfolio_objective_results))
        for(i in 1:length(Port1)) {
          Portafolios[[i]]$Pesos  <- Port1$random_portfolio_objective_results[[i]]$weights
          Portafolios[[i]]$Medias <- Port1$random_portfolio_objective_results[[i]]$objective_measures$mean
          Portafolios[[i]]$Vars   <- var.portfolio(R = Port1$R, weights = Portafolios[[i]]$Pesos)
          names(Portafolios[[i]]$Medias) <- NULL
        }
        
        df_Portafolios <- data.frame(matrix(nrow=length(Port1),
                                            ncol=2, data = 0))
        colnames(df_Portafolios) <- c("Rendimiento ($)","Riesgo ($)")
        for(i in 1:length(Port1)) {
          
          df_Portafolios[i,1] <- round(Portafolios[[i]]$Medias*252,4)*capital
          df_Portafolios[i,2]  <- round(sqrt(Portafolios[[i]]$Vars)*sqrt(252),4)*capital
          
          for(k in 1:length(acciones)) {
            df_Portafolios[i,paste("Peso de ", acciones[k],sep="")] <- Portafolios[[i]]$Pesos[k]
            
            df_Portafolios[i,paste("Titulos de ", acciones[k],sep="")] <-
              (capital*Portafolios[[i]]$Pesos[k])%/%info[1,k]
          }
          
          
        }
        
        # Portafolio con m?ximo rendimiento esperado
        Port_1 <- df_Portafolios[which.max(df_Portafolios[,1]),]
        
        # Portafolio con m?nima varianza
        Port_2 <- df_Portafolios[which.min(df_Portafolios[,2]),]
        
        # Tasa libre de riesgo
        rf <- 0.008       
        # Rendimiento de portafolio
        rp <- df_Portafolios[,2]
        # Varianza de portafolio
        sp <- df_Portafolios[,2]
        # Indice de sharpe
        sharpe <- (rp-rf)/sp
        
        # Portafolio con m?ximo Sharpe ratio 
        Port_3 <- df_Portafolios[which.max(sharpe),]
        
        Ports <- cbind(rbind(Port_1, Port_2, Port_3),
                       "Portafolio" = c("Maximo Rendimiento","Minima Varianza","Maximo Sharpe Ratio"))
        for(i in 1:3){
          Ports[i,1]<-paste("$",Ports[i,1])
          Ports[i,2]<-paste("$",Ports[i,2])
        }
        return(Ports)
      }
      
      ## Excel de informacion
      saexcel<-function(activos){
        id <- "0B2v9Xt_MUZ4qWGhGeENFYnRlSzQ" # google file ID
        excel<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
        igual<-c()
        for(i in 1:length(activos)){
          sel<-which(activos[i]==excel[,3])
          igual<-c(igual,sel)
        }
        infoa<-excel[igual,2:5]
        infoa<-data.frame(infoa)
        return(infoa)}
      
      ## forecast de solo un activo
      foreuno<-function(precios){
        arma<-auto.arima(precios)
        exac<-accuracy(arma)
        pred<-forecast(arma,h=92)
        pron<-c(pred$mean)
        mov<-pron[63]-pron[1]
        if (exac[1]>0.5){
          if(mov>0){#alcista
            imp<-"Existe una probabilidad que se siga un movimiento alcista. Se recomienda comprar."
          }else{
            imp<-"Existe una probabilidad que se siga un movimiento bajista. Se recomienda no comprar o mantener."
          }}else{
            if(mov>0){#alcista
              imp<-"Existe una alta probabilidad que se siga un movimiento alcista. Se recomienda comprar."
            }else{
              imp<-"Existe una alta probabilidad que se siga un movimiento bajista. Se recomiendo no comprar o mantener."
            }}
        list(imp,pred,pron)
        #plot(pred)
      }
      
      ## crear rendimientos diarios para un portafolio proporciones de max sharpe
      juntar<-function(portef,acciones,precios){
        proporciones<-c()
        suma<-data.frame()
        i=1
        for (j in 1:length(acciones)){
          proporciones<-c(proporciones,portef[3,i+2])
          i=i+2
        }
        for(j in 1:length(precios[,1])){
          for(k in 1:length(acciones)){
            suma[j,k]<-precios[j,k]*proporciones[k]
          }}
        dport<-rowSums(suma)
      }
      activos<-c(input$activo1,input$activo2,input$activo3,input$activo4,input$activo5)
      activos<-activos[activos!=""]
      lana<-input$dinero
      
      info<-descarga(activos)
      info<-na.omit(info)
      j<-as.POSIXct(row.names(info))
      precio<-tail(info, n=1)
      if(length(activos)==1){
        if(lana<precio){
          num<-round(as.numeric(precio[1]),2)
          output$error<-renderText(
            
            (print(paste("La inversion debe ser mayor a",num,sep =," $")))
          )
        }else{
          withProgress(message = 'Cargando informacion',detail = 'Porfavor espere...', {
            rend<-rendimientos(info)
            rendsd<-meansd(rend,lana,precio)
            ex<-saexcel(activos)
            predi<-foreuno(info)
          })
          
          output$texto<-renderTable(
            {ex} ,striped = TRUE, bordered = TRUE,
            spacing = 'm'
            #informacion de activo, industria y precio van aqui
            #pronostico
          )
          
          output$original<-renderTable(
            {rendsd} ,striped = TRUE, bordered = TRUE,
            spacing = 'm')
          
          output$infofor<-renderText(
            predi[[1]]
          )
          
          output$forecast<-renderPlotly({
            plot_ly() %>%
              add_lines(x= j, y= info[,1],
                        color= I("black"), name="Observado") %>%
              layout(title='Precios historicos (Nov 2015- Nov 2017) Portafolio Original',xaxis=list(title='Fecha'),yaxis=list(title='Precio'))
          })
          output$forecastc<-renderPlotly({
            plot_ly() %>%
              add_ribbons(x=fechasf, ymin=predi[[2]]$lower[,2],
                          ymax=predi[[2]]$upper[,2], color=I("gray95"), name="95% confianza") %>%
              add_ribbons(x=fechasf,ymin=predi[[2]]$lower[,1],
                          ymax=predi[[2]]$upper[,1],color=I("gray88"), name="88% confianza") %>%
              add_lines(x=fechasf, y=predi[[2]]$mean, color=I("blue"), name="Prediccion") %>%
              layout(title='Pronostico (Dic 2017- Marzo 2017)',xaxis=list(title='Fecha'),yaxis=list(title='Precio'))
          })
        }}else{
          info<-descarga(activos)
          info<-na.omit(info)
          j<-as.POSIXct(row.names(info))
          precio<-as.numeric(c(tail(info, n=1)))
          precio<-sum(precio)
          if(lana<precio){
            num<-round(as.numeric(precio[1]),2)
            output$error<-renderText(
              print(paste("La inversion debe ser mayor a",num,sep = " $"))
            )}else{
              withProgress(message = 'Cargando informacion',detail = 'Porfavor espere...', { # loading
                rend<-rendimientos(info)
                longitud<-length(activos)
                minimo<-c(rep(.1,longitud))
                maximo<-c(rep(.7,longitud))
                portuno<-portafolio(activos,rend,minimo,maximo,lana,info)
                jun<-as.data.frame(juntar(portuno,activos,info))
                predi1<-foreuno(jun)
                ex<-saexcel(activos)
                uno<-c()
                uno<-seleccion(activos)
                eliminar<-c()
                rest<-0
                cont<-2
                for(i in 1:length(activos)){
                  otro<-which(activos[i]==acciones)
                  eliminar<-c(eliminar,otro)
                  opciones<-acciones[-eliminar]
                }
                while(length(uno)!=length(activos)){
                  cont<-cont+rest
                  rest<-length(activos)-length(uno)
                  resta<-rest+cont-1
                  nuevoacc<-array(opciones[cont:resta])
                  nuevoacc<-c(uno,nuevoacc)
                  uno<-seleccion(nuevoacc)
                }
                infodos<-descarga(uno)
                infodos<-na.omit(infodos)
                j1<-as.POSIXct(row.names(infodos))
                rendo<-rendimientos(info)
                portdos<-portafolio(uno,rendo,minimo,maximo,lana,infodos)
                junn<-as.data.frame(juntar(portdos,uno,infodos))
                predi2<-foreuno(junn)
              })
              #rendimiento y riesgo de portafolio, info y pronostico
              output$texto<-renderTable(
                {ex} ,striped = TRUE, bordered = TRUE,
                spacing = 'm'
              )
              output$original<-renderTable(
                { portuno} ,striped = TRUE, bordered = TRUE,  
                spacing = 'm'
              )
              output$forecast<-renderPlotly({
                plot_ly() %>%
                  add_lines(x= j, y= jun[,1],
                            color= I("black"), name="Observado") %>%
                  layout(title='Precios historicos (Nov 2015- Nov 2017) Portafolio Mejorado',xaxis=list(title='Fecha'),yaxis=list(title='Precio'))
              })
              output$forecastc<-renderPlotly({
                plot_ly() %>%
                  add_ribbons(x=fechasf, ymin=predi1[[2]]$lower[,2],
                              ymax=predi1[[2]]$upper[,2], color=I("gray95"), name="95% confianza") %>%
                  add_ribbons(x=fechasf,ymin=predi1[[2]]$lower[,1],
                              ymax=predi1[[2]]$upper[,1],color=I("gray88"), name="88% confianza") %>%
                  add_lines(x=fechasf, y=predi1[[2]]$mean, color=I("blue"), name="Prediccion") %>%
                  layout(title='Pronostico (Dic 2017- Marzo 2017)',xaxis=list(title='Fecha'),yaxis=list(title='Precio'))
              })
              output$infofor<-renderText(
                predi1[[1]]
              )
              output$infoform<-renderText(
                predi2[[1]]
              )
              output$mejor<-renderTable(
                {portdos} ,striped = TRUE, bordered = TRUE,  
                spacing = 'm'
              )
              
              output$forecastm<-renderPlotly({
                plot_ly() %>%
                  add_lines(x= j1, y= junn[,1],
                            color= I("black"), name="Observado") %>%
                  layout(title='Precios historicos (Nov 2015- Nov 2017)',xaxis=list(title='Fecha'),yaxis=list(title='Precio'))
              })
              output$forecastmc<-renderPlotly({
                plot_ly() %>%
                  add_ribbons(x=fechasf, ymin=predi2[[2]]$lower[,2],
                              ymax=predi2[[2]]$upper[,2], color=I("gray95"), name="95% confianza") %>%
                  add_ribbons(x=fechasf,ymin=predi2[[2]]$lower[,1],
                              ymax=predi2[[2]]$upper[,1],color=I("gray88"), name="88% confianza") %>%
                  add_lines(x=fechasf, y=predi2[[2]]$mean, color=I("blue"), name="Prediccion") %>%
                  layout(title='Pronostico (Dic 2017- Marzo 2017)',xaxis=list(title='Fecha'),yaxis=list(title='Precio'))
              })
            }}
      
      
    }#termina boton
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

