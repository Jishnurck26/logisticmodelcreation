# library(shiny)
# library(shinythemes)
# library(DT)
# library(devtools)
# library(woe)
# library(Hmisc)
# library(data.table)
# library(car)
# library(sqldf)
# library(ROCR)
# library(ineq)
# library(Hmisc)
# library(pryr)
# library(scales)
# library(shinythemes)
#'Logistic model creation
#'
#'Give you ui to create logistic model.
#' @author JIshnu
#' @param See the example
#' @return  you can download the model and all the performance
#' @example example.R
#' @export

logisticmodelcreation<-function (var)
{
  library(shiny)
  library(shinythemes)
  library(DT)
  library(devtools)
  library(woe)
  library(Hmisc)
  library(data.table)
  library(car)
  library(sqldf)
  library(ROCR)
  library(ineq)
  library(Hmisc)
  library(pryr)
  library(scales)
  library(shinythemes)
  m = var
if (interactive()) {
  ui <-navbarPage(
    theme = shinytheme("cosmo")
    ,
    header =
      absolutePanel(
        wellPanel(style = "background-color: lightblue;",
                  textOutput("text"),
                  tags$head(
                    tags$style("#text{color: white;\n                font-size: 12px;\n         }")
                  ))
        ,
        top = "16%",
        left =  "80%",
        width = "250px",
        height = "10%",
        draggable = TRUE
      )
    ,
    "Logistic Regression",
    tabPanel("Information Value",
             fluidPage(
               titlePanel("Information Value"),
               column(
                 4,
                 selectInput(
                   label = "Select Development Dataframe",
                   choices = m$names.x...x....TRUE..,
                   selected = "m",
                   inputId = "dev"
                 ),
                 uiOutput("ID"),
                 uiOutput("Target"),
                 actionButton("ivv", "Get Information Value ")
               ),
               column(6,
                      DT::dataTableOutput("ivvalue"))
             )),
    tabPanel("P Value",
             fluidPage(
               titlePanel("P Value"),
               column(
                 4,
                 uiOutput("IDp"),
                 uiOutput("Targetp"),
                 actionButton("pv", "Get p Value ")
               ),

               column(7,
                      DT::dataTableOutput("pvalue"))
             )),
    tabPanel(
      "Variable Clustering",
      fluidPage(
        titlePanel("Variable Clustering"),
        column(2,
               uiOutput("IDpx"),
               actionButton("vc", "Get Graph ")),

        column(12,
               plotOutput("clusterplot"))
      )
    ),

    tabPanel("Development Model",
             fluidPage(
               titlePanel("Model creation"),
               column(
                 4,

                 # actionButton("colnm",
                 #              "Get Column names"),
                 absolutePanel(
                   textInput(
                     label = "Formula",
                     value = "TARGET~AGE+OCCUPATION+GENDER",
                     inputId = "equation",
                     width = "300px"
                   ),
                   actionButton("getmodel",
                                "Create Model")
                 ),
                 absolutePanel(
                   wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 300px",
                             textOutput("columnnames")),
                   width = "180px",
                   top = "200px",
                   height = "10%"
                 )

               ),
               column(
                 8,
                 absolutePanel(
                   textOutput("Sum"),
                   tags$head(tags$style("#sum{color: red;\n                font-size: 25px;\n         }")),
                   tableOutput("recd"),
                   tableOutput("coef"),
                   textOutput("Nulld"),
                   textOutput("Residuald"),
                   textOutput("AIC"),
                   textOutput("FSI"),
                   tableOutput("VIF"),
                   left = "350px"
                 )
               )
             )),
    tabPanel("Rank Ordering(Dev)",
             fluidPage(
               titlePanel("Rank Ordering"),
               column(
                 4,
                 actionButton("Rankord",
                              "Get Rank Ordering Table"),
                 DT::dataTableOutput("Rankordering")
               )
             )),
    tabPanel("All Measures(Dev)",
             fluidPage(
               titlePanel("All Measures"),
               column(
                 2,
                 actionButton("measure",
                              "Get All measure"),
                 helpText("Concordance will take time to show the output for large datasets."),
                 actionButton("concob",
                              "Get Concordance")
               ),
               column(
                 5,
                 tableOutput("stat"),
                 #textOutput("KS"),
                 tags$head(
                   tags$style("#KS{color: red;\n                font-size: 20px;\n                font-style: bold;\n  }")
                 ),
                 tableOutput("concordance"),
                 tableOutput("chi1"),
                 tableOutput("chi2"),
                 #textOutput("gini"),
                 tags$head(
                   tags$style("#gini{color: red;\n                font-size: 20px;\n                font-style: bold;\n  }")
                 )

               )
             )),
    tabPanel("Validation Model",
             fluidPage(
               titlePanel("Validating the model with validation dataset"),
               column(
                 4,
                 selectInput(
                   label = "Select Validation Dataframe",
                   choices = m$names.x...x....TRUE..,
                   selected = "m",
                   inputId = "val"
                 ),
                 actionButton("validate", "Validate the model "),
                 helpText("Equation used for the model creation"),
                 textOutput("eqnused")
               ),
               column(
                 6,
                 textOutput("Sumv"),
                 tags$head(tags$style("#sumv{color: red;\n                font-size: 25px;\n         }")),
                 tableOutput("betaratio"),
                 tableOutput("summaryval")


               )
             )),
    tabPanel("Rank Ordering(Val)",
             fluidPage(
               titlePanel("Validation Rank Ordering"),
               column(
                 4,
                 actionButton("Rankordv",
                              "Get Rank Ordering Table"),
                 DT::dataTableOutput("Rankorderingv")
               )
             )),
    tabPanel(
      "Model Measures(Val)",
      fluidPage(
        titlePanel("Comparison Dev Vs Val"),
        column(
          2,
          actionButton("Compr",
                       "Get Comparison"),
          helpText("Concordance will take time to show the out put for large datasets"),
          actionButton("Comprconc",
                       "Get Comparison Concordance")
        ),
        column(4,
               absolutePanel(
                 wellPanel(
                   id = "tPanel2",
                   style = "overflow-y:scroll; max-height: 700px",

                   tableOutput("stat2"),
                   tableOutput("concordanced"),
                   tableOutput("concordancev")
                 ),
                 width = "600px",left = "150px",
                 height = "10%"
               ))

      )
    ),
    tabPanel("Save Objects",
             fluidPage(
               titlePanel("save objects"),
               column(4,
                      helpText("Selects objects you want to save")),
               column(6,
                      # uiOutput("savelist"),
                      textAreaInput(inputId="documentation", label="Documentation", value = "",
                                    width = "500px", height = "300px",
                                    cols = NULL, rows = NULL, placeholder = NULL, resize = NULL),
                      #downloadButton('savedisc', 'Save Discription'),
                      helpText("Download All the objects created with documentation"),
                      downloadButton('downloadData', 'Download All  Objects')
                      #, helpText("Performance Measures includes Rank order tables both validation and development
                      #          , comparision table, Information Value table, P value table "),
                      # downloadButton('downloadperformance', 'Download Performance Measures'),
                      # helpText("Save datafiles used"),
                      # downloadButton('downloaddata', 'Download Datafiles'),
                      # helpText("All the objects created in application (includes development and validation dataset"),
                      # downloadButton('downloadall', 'Download All Objects')

                      #actionButton("save_objs", "Save Objects")
               )
             ))


  )
  server <- function(input, output) {
    {
      output$text<-renderText({
        k<- c("K2 Analytics Finishing School Pvt. Ltd.","     Website: http://www.k2analytics.co.in")
        k
      })


      colnam <-
        reactive({
          #for reactive all output based on selected object
          if (input$dev=="m")
            return(NULL)
          op <- data.frame(get((input$dev)))
          j <- data.frame(names(op))
          colnames(j) <- "all"
          j
        })
      output$ID <-
        renderUI({
          selectInput(
            label = "Select Variable/s to Ignore" ,
            choices = colnam() ,
            selected = NULL,
            inputId = "ID",
            multiple = T
          )
        })
      output$Target<-
        renderUI({
          selectInput(
            label = "Select Target Variable" ,
            choices = colnam() ,
            selected = NULL,
            inputId = "Target"
          )
        })


      ivvtb<-reactive({
        op <- data.frame(get((input$dev)))
        id<-input$ID
        targ<-input$Target


        pp<-iv.mult(op[,!names(op) %in% c(id)],targ,TRUE)
        pp$InformationValue<-round(pp$InformationValue,2)
        pp


      })

      ntextivv <- eventReactive(input$ivv, {
        ivvtb()
      })

      output$ivvalue<-DT::renderDataTable({
        ntextivv()
      },
      options = list(
        lengthChange = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
          "}"),
        autowidth = TRUE,
        columnDefs = list(list(width = '70%', targets = 1))
      ))


      output$IDp <-
        renderUI({
          selectInput(
            label = "Select Variable/s to Ignore" ,
            choices = colnam() ,
            selected = input$ID,
            inputId = "IDp",
            multiple = T
          )
        })
      output$Targetp<-
        renderUI({
          selectInput(
            label = "Select Target Variable" ,
            choices = colnam() ,
            selected = input$Target,
            inputId = "Targetp"
          )
        })





      glmfunc<-reactive({
        OneVariableGLM <-
          function(df, target, id) {
            targ<-which( colnames(df)==target)
            id<-which( colnames(df)==id )
            tmp<- df

            head(tmp)
            pp<-lapply( tmp[,c(-id,-targ)], function(x) summary(glm(tmp[,targ] ~ x)) )
            inter<-lapply(pp, coef)
            require(reshape2)
            inter$id <- rownames(inter)
            inter
            ohh<-melt(inter)
            got<-ohh[which(ohh$Var2=="Pr(>|t|)" &ohh$Var1 !="(Intercept)"),]

            row.names(got)<-NULL
            colnames(got)<-c("Value","Indicator","P_value","Variable")
            got<-got[c(4,1,2,3)]
            got<-got[order(got$P_value),]
            got$Value<-substring(got$Value, 2)
            got$Indicator<-NULL

            got
          }

        op <- data.frame(get((input$dev)))
        id<-input$IDp
        targ<-input$Targetp


        OneVariableGLM(df = op,target = targ,id = id)
      })

      ntextp <- eventReactive(input$pv, {
        glmfunc()
      })

      output$pvalue<-DT::renderDataTable({
        ntextp()
      },
      options = list(
        lengthChange = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
          "}"),
        autowidth = TRUE,
        columnDefs = list(list(width = '30%', targets = 1))
      ))


      out_numeric <-
        reactive({
          #for reactive numeric output based on selected object
          if (input$dev=="m")
            return(NULL)
          op <- data.frame(get((input$dev)))
          dfnum<-op[,lapply (op,class) %in% c("numeric","integer")]

          j <- data.frame(names(dfnum))
          colnames(j) <- "all_numeric"
          j

        })
      output$IDpx <-
        #reactive input numeric
        renderUI({
          selectInput(
            label = "Select Numeric Variable/s to Ignore " ,
            choices = out_numeric() ,
            selected = NULL,
            multiple = T,
            inputId = "IDpx"
          )
        })

      plotdata<-reactive({

        op <- data.frame(get((input$dev)))
        in1 <- input$IDpx
        rc<-which( colnames(op)== in1 )
        kop <- op[,!names(op) %in% c(in1)]


        vcls <- function(df)
        {
          tmp <- df[, lapply (df, class) %in% c("numeric", "integer")]

          tmp <- data.matrix(tmp)
          v <- varclus(tmp)
          v
        }
        ppk<-vcls(kop)
        ppk
      })

      ntextpx <- eventReactive(input$vc, {
        plotdata()
      })

      output$clusterplot<-renderPlot({
        ppq<-ntextpx()
        plot(ppq)
      })



      out_columnnames1 <-
        reactive({
          op <- data.frame(get((input$dev)))
          # j <- data.frame(names(op))
          # colnames(j) <- "Colnames"
          j<-names(op)

          j
        })


      ntext <- eventReactive(input$ivv, {
        if (input$dev=="m")
          return(NULL)
        out_columnnames1()
      })

      output$columnnames<-renderText({
        ntext()
      })

      sumt<-eventReactive(input$getmodel, {
        kp<-paste0("MODEL SUMMARY")
      })
      output$Sum<-renderText({
        sumt()
      })




      mylogit<-eventReactive(input$getmodel, {
        op <- data.frame(get((input$dev)))
        equ<-input$equation
        mylogit1<-glm(formula = equ,family = "binomial",data = op)
        mylogit1
      })
      cofelogit<-eventReactive(input$getmodel, {
        mylogit1<-mylogit()
        summ<-summary(mylogit1)
        coeff<-data.table(summ$coefficients)
        coeff1<-data.frame(summ$coefficients)
        Coefficients<-row.names(coeff1)
        coeff<-cbind(Coefficients,coeff)
        coeff

      })

      output$coef<-renderTable({
        cofelogit()
      })

      # recid<-eventReactive(input$getmodel, {
      #   mylogit1<-mylogit()
      #   summ<-summary(mylogit1)
      #   residuals<-t(data.matrix(summ$deviance.resid))
      #   residuals
      # })
      #
      # output$recd<-renderTable({
      #   recid()
      # })



      Nulldt<-eventReactive(input$getmodel, {
        mylogit1<-mylogit()
        summ<-summary(mylogit1)
        rsevalue<-round(summ$null.deviance,4)
        df<-summ$df.null
        rsevalue1<-paste("Null deviance:",rsevalue,"on",df,"degrees of freedom")
        rsevalue1
      })

      output$Nulld<-renderText({
        Nulldt()
      })


      Residualt<-eventReactive(input$getmodel, {
        mylogit1<-mylogit()
        summ<-summary(mylogit1)
        rsevalue<-round(summ$deviance,4)
        df<-summ$df.residual
        rsevalue1<-paste("Residual deviance:",rsevalue,"on",df,"degrees of freedom")
        rsevalue1
      })

      output$Residuald<-renderText({
        Residualt()
      })

      aict<-eventReactive(input$getmodel, {
        mylogit1<-mylogit()
        summ<-summary(mylogit1)
        rsevalue<-round(summ$aic,4)
        rsevalue1<-paste("AIC: ",rsevalue)
        rsevalue1
      })

      output$AIC<-renderText({
        aict()
      })

      FSIt<-eventReactive(input$getmodel, {
        mylogit1<-mylogit()
        summ<-summary(mylogit1)
        rsevalue<-summ$iter
        rsevalue1<-paste("Number of Fisher Scoring iterations:",rsevalue)
        rsevalue1
      })

      output$FSI<-renderText({
        FSIt()
      })

      VIFT<-reactive({
        mylogit1<-mylogit()
        mm<-round(vif(mylogit1),3)
        Variable<-data.frame(vif(mylogit1))
        Variable<-row.names(Variable)
        Variable<-cbind(Variable,mm)
        #Variable<-data.frame(Variable)
        Variable

      })
      VIFC<-eventReactive(input$getmodel, {
        VIFT()
      })
      output$VIF<-renderTable({
        VIFC()
      },bordered =T,
      caption = "VIF Table",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))



      rnkorder<-reactive({
        op <- data.frame(get((input$dev)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }
        ROTable <- function(df, target, probability)
        {
          tmp <- df[, c(target,probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)


          mydata.DT = data.table(tmp) ## Converting the data frame to data table object
          ## Creating Aggregation and Group By similar to as in SQL
          Target_Rate = sum(mydata.DT$Target)/nrow(mydata.DT)
          rank <- mydata.DT[, list(
            min_prob = round(min(prob),3),
            max_prob = round(max(prob),3),
            cnt = length(Target),
            cnt_resp = sum(Target),
            cnt_non_resp = sum(Target == 0)
          ) ,
          by = deciles][order(-deciles)]
          rank$RRate <- rank$cnt_resp / rank$cnt ## computing response rate
          rank$cum_tot <- cumsum(rank$cnt) ## computing cum total customers
          rank$cum_resp <- cumsum(rank$cnt_resp) ## computing cum responders
          rank$cum_non_resp <-
            cumsum(rank$cnt_non_resp) ## computing cum non-responders
          rank$cum_RRate = rank$cum_resp / rank$cum_tot
          rank$cum_rel_resp <- rank$cum_resp / sum(rank$cnt_resp)

          rank$cum_rel_non_resp <- rank$cum_non_resp / sum(rank$cnt_non_resp)

          rank$ks <- rank$cum_rel_resp - rank$cum_rel_non_resp
          rank$lift <- round(rank$cum_RRate / Target_Rate,1)
          rank$RRate<-percent( rank$RRate)
          rank$cum_RRate<-percent( rank$cum_RRate)
          rank$cum_rel_resp<-percent(rank$cum_rel_resp)
          rank$cum_rel_non_resp<-percent(rank$cum_rel_non_resp)
          rank$ks <- percent( rank$ks)

          ## KS
          rank ## display Rank Ordering Table
        }
        names(op)[names(op)==input$Target]="Target"
        rot<- ROTable(op,"Target","prob")
        rot
      })

      rankorderr<-eventReactive(input$Rankord, {
        kp<-rnkorder()
        kp<-data.frame(kp)
        colnames(kp)<-c("Deciles","Min.prob","Max.prob","Cnt","Cnt.Resp","Cnt.Non.Resp","RRate","Cum.Tot","Cum.Resp",
                        "Cum.Non.Resp","Cum.RRate","Cum.Per.Resp", "Cum.Per.Non.Resp","KS","Lift")
        kp
      })
      output$Rankordering<-DT::renderDataTable(
        rankorderr(),rownames= FALSE
        ,    options = list(
          lengthChange = FALSE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
            "}"),
          autowidth = TRUE,
          columnDefs = list(list(width = '10px', targets = "_all"))
        ))



      statT<-eventReactive(input$measure, {

        op <- data.frame(get((input$dev)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")


        names(op)[names(op)==input$Target]="Target"
        library(ineq)
        gini=ineq(op$prob,type = "Gini")

        library(ROCR)
        pred=prediction(op$prob,op$Target)
        perf=performance(pred,"tpr","fpr")
        ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
        ks
        auc=performance(pred,"auc")
        auc=as.numeric(auc@y.values)
        auc
        op<-data.frame(statistics="GINI",value=gini*100)
        pp<-data.frame(statistics="AUC",value=auc*100)
        kp<-data.frame(statistics="KS",value=ks*100)

        df<-rbind(kp,pp,op)

        df$value<-round(df$value,2)
        df
      })

      output$stat<-renderTable({
        statT()

      },bordered =T,
      caption = "Model Statistics",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))

      concotable<-reactive({
        op <- data.frame(get((input$dev)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")

        concordance <- function(df, target, probability)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"

          concordance1 = function(y, yhat)
          {
            Con_Dis_Data = cbind(y, yhat)
            ones = Con_Dis_Data[Con_Dis_Data[, 1] == 1, ]
            zeros = Con_Dis_Data[Con_Dis_Data[, 1] == 0, ]
            conc = matrix(0, dim(zeros)[1], dim(ones)[1])
            disc = matrix(0, dim(zeros)[1], dim(ones)[1])
            ties = matrix(0, dim(zeros)[1], dim(ones)[1])
            for (j in 1:dim(zeros)[1])
            {
              for (i in 1:dim(ones)[1])
              {
                if (ones[i, 2] > zeros[j, 2])
                {
                  conc[j, i] = 1
                }
                else if (ones[i, 2] < zeros[j, 2])
                {
                  disc[j, i] = 1
                }
                else if (ones[i, 2] == zeros[j, 2])
                {
                  ties[j, i] = 1
                }
              }
            }
            Pairs = dim(zeros)[1] * dim(ones)[1]
            PercentConcordance = (sum(conc) / Pairs) * 100
            PercentDiscordance = (sum(disc) / Pairs) * 100
            PercentTied = (sum(ties) / Pairs) * 100
            return(
              list(
                "Percent Concordance" = PercentConcordance,
                "Percent Discordance" = PercentDiscordance,
                "Percent Tied" = PercentTied,
                "Pairs" = Pairs
              )
            )
          }
          concordance_output <- concordance1(tmp$Target, tmp$prob)
          concordance_output
        }
        names(op)[names(op)==input$Target]="Target"
        concordance(op,"Target","prob")
      })

      concordanceb<-eventReactive(input$concob, {
        concotable()
      })

      output$concordance<-renderTable({
        concordanceb()
      },bordered =T,caption = "Concordance",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))

      chi1cal<-reactive({
        op <- data.frame(get((input$dev)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }

        hosmerlem_gof <- function(df, target, probability,g=10)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)

          hosmerlem <-
            function (y, yhat, g1=g) {
              cutyhat <-
                cut(yhat,
                    breaks = quantile(yhat, probs = seq(0, 1, 1 / g1)),
                    include.lowest = T)
              obs <-xtabs(cbind(1 - y, y) ~ cutyhat)
              expect <-xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
              chisq <-sum((obs - expect) ^ 2 / expect)
              P <-1 - pchisq(chisq, g1 - 2)
              c("X^2" = chisq,Df = g1 - 2,"P(>Chi)" = P)
            }
          hl_gof <- hosmerlem(tmp$Target, tmp$prob)
          # print(hl_gof)
          # print("Table")
          # sqldf ("select deciles, count(1) as cnt,
          #        sum (Target) as Obs_Resp, count (Target == 0) as Obs_Non_Resp,
          #        sum (prob) as Exp_Resp, sum (1 - prob) as Exp_Non_Resp
          #        from tmp
          #        group by deciles
          #        order by deciles desc")
          hl_gof
        }
        names(op)[names(op)==input$Target]="Target"

        kp<-data.table(t(hosmerlem_gof(op,"Target","prob")))
        kp
      })


      chi1b<-eventReactive(input$measure, {
        chi1cal()
      })

      output$chi1<-renderTable({
        chi1b()
      },bordered =T,
      caption = "Chi Sq - Goodness of Fit",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))



      chi2cal<-reactive({
        op <- data.frame(get((input$dev)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }

        hosmerlem_gof <- function(df, target, probability,g=10)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)

          hosmerlem <-
            function (y, yhat, g1=g) {
              cutyhat <-
                cut(yhat,
                    breaks = quantile(yhat, probs = seq(0, 1, 1 / g1)),
                    include.lowest = T)
              obs <-xtabs(cbind(1 - y, y) ~ cutyhat)
              expect <-xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
              chisq <-sum((obs - expect) ^ 2 / expect)
              P <-1 - pchisq(chisq, g1 - 2)
              c("X^2" = chisq,Df = g1 - 2,"P(>Chi)" = P)
            }
          hl_gof <- hosmerlem(tmp$Target, tmp$prob)
          # print(hl_gof)
          # print("Table")
          sqldf ("select deciles, count(1) as cnt,
                 sum (Target) as Obs_Resp, count (case when Target == 0 then 1 end) as Obs_Non_Resp,
                 sum (prob) as Exp_Resp, sum (1 - prob) as Exp_Non_Resp
                 from tmp
                 group by deciles
                 order by deciles desc")

        }
        names(op)[names(op)==input$Target]="Target"

        kp<-data.table(hosmerlem_gof(op,"Target","prob"))
        kp
      })


      chi2b<-eventReactive(input$measure, {
        chi2cal()
      })

      output$chi2<-renderTable({
        chi2b()
      },bordered =T,
      caption = "Chi-Sq Calculation",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))




      ###############Validation

      eqnusedt<-eventReactive(input$getmodel, {
        kp<-input$equation
        kp
      })
      output$eqnused<-renderText({
        eqnusedt()
      })

      sumtv<-eventReactive(input$validate, {
        kp<-paste0("VALIDATION MODEL SUMMARY")
      })
      output$Sumv<-renderText({
        sumtv()
      })



      mylogit2<-eventReactive(input$validate, {
        op <- data.frame(get((input$val)))
        equ<-input$equation
        mylogit1<-glm(formula = equ,family = "binomial",data = op)
        mylogit1
      })
      cofelogit2<-eventReactive(input$validate, {
        mylogit2<-mylogit2()
        summ<-summary(mylogit2)
        coeff<-data.table(summ$coefficients)
        coeff1<-data.frame(summ$coefficients)
        Coefficients<-row.names(coeff1)
        coeff<-cbind(Coefficients,coeff)
        coeff

      })

      betacal<-eventReactive(input$validate, {
        mylogit2<-mylogit2()
        summ<-summary(mylogit2)
        coeff<-data.table(summ$coefficients)
        coeff1<-data.frame(summ$coefficients)
        Coefficients<-row.names(coeff1)
        coeff<-cbind(Coefficients,coeff)
        coeff<-data.frame(coeff)
        coeff

        mylogit1<-mylogit()
        summd<-summary(mylogit1)
        coeffd<-data.table(summd$coefficients)
        coeff1d<-data.frame(summd$coefficients)
        Coefficients<-row.names(coeff1d)
        coeffd<-cbind(Coefficients,coeffd)
        coeffd<-data.frame(coeffd)
        coeffd<-coeffd[,1:2]
        coeffd<-merge(coeffd,coeff,by="Coefficients")
        coeffd<-coeffd[,1:3]
        colnames(coeffd)<-c("Coefficients","Estimate_dev","Estimate_val")
        coeffd$beta_ratio<-coeffd$Estimate_dev/coeffd$Estimate_val
        coeffd


      })




      output$betaratio<-renderTable({
        betacal()
      },bordered =T,
      caption = "Beta Ratio Test",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))

      output$summaryval<-renderTable({
        cofelogit2()
      },bordered =T)







      rnkorderv<-reactive({
        op <- data.frame(get((input$val)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }


        ROTable <- function(df, target, probability)
        {
          tmp <- df[, c(target,probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)


          mydata.DT = data.table(tmp) ## Converting the data frame to data table object
          ## Creating Aggregation and Group By similar to as in SQL
          Target_Rate = sum(mydata.DT$Target)/nrow(mydata.DT)
          rank <- mydata.DT[, list(
            min_prob = round(min(prob),3),
            max_prob = round(max(prob),3),
            cnt = length(Target),
            cnt_resp = sum(Target),
            cnt_non_resp = sum(Target == 0)
          ) ,
          by = deciles][order(-deciles)]
          rank$RRate <- rank$cnt_resp / rank$cnt ## computing response rate
          rank$cum_tot <- cumsum(rank$cnt) ## computing cum total customers
          rank$cum_resp <- cumsum(rank$cnt_resp) ## computing cum responders
          rank$cum_non_resp <-
            cumsum(rank$cnt_non_resp) ## computing cum non-responders
          rank$cum_RRate = rank$cum_resp / rank$cum_tot
          rank$cum_rel_resp <- rank$cum_resp / sum(rank$cnt_resp)

          rank$cum_rel_non_resp <- rank$cum_non_resp / sum(rank$cnt_non_resp)

          rank$ks <- rank$cum_rel_resp - rank$cum_rel_non_resp
          rank$lift <- round(rank$cum_RRate / Target_Rate,1)
          rank$RRate<-percent( rank$RRate)
          rank$cum_RRate<-percent( rank$cum_RRate)
          rank$cum_rel_resp<-percent(rank$cum_rel_resp)
          rank$cum_rel_non_resp<-percent(rank$cum_rel_non_resp)
          rank$ks <- percent( rank$ks)

          ## KS
          rank ## display Rank Ordering Table
        }
        names(op)[names(op)==input$Target]="Target"
        rot<- ROTable(op,"Target","prob")
        rot
      })

      rankorderrv<-eventReactive(input$Rankordv, {
        kp<- rnkorderv()
        kp<-data.frame(kp)
        colnames(kp)<-c("Deciles","Min.prob","Max.prob","cnt","cnt.Resp","cnt.Non.Resp","RRate","cum.Tot","cum.Resp",
                        "cum.Non.Resp","cum.RRate","cum.Per.Resp", "cum.Per.Non.Resp","KS","Lift")
        kp
      })
      output$Rankorderingv<-DT::renderDataTable(
        rankorderrv(),rownames= FALSE
        ,    options = list(
          lengthChange = FALSE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
            "}"),
          autowidth = "100%",
          columnDefs = list(list(width = '70%', targets = 1))
        ))


      concordancebd<-eventReactive(input$Comprconc, {
        concotable()
      })

      output$concordanced<-renderTable({
        concordancebd()
      },bordered =T,caption = "Concordance (Development)",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))




      concotablev<-reactive({
        op <- data.frame(get((input$val)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")

        concordance <- function(df, target, probability)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"

          concordance1 = function(y, yhat)
          {
            Con_Dis_Data = cbind(y, yhat)
            ones = Con_Dis_Data[Con_Dis_Data[, 1] == 1, ]
            zeros = Con_Dis_Data[Con_Dis_Data[, 1] == 0, ]
            conc = matrix(0, dim(zeros)[1], dim(ones)[1])
            disc = matrix(0, dim(zeros)[1], dim(ones)[1])
            ties = matrix(0, dim(zeros)[1], dim(ones)[1])
            for (j in 1:dim(zeros)[1])
            {
              for (i in 1:dim(ones)[1])
              {
                if (ones[i, 2] > zeros[j, 2])
                {
                  conc[j, i] = 1
                }
                else if (ones[i, 2] < zeros[j, 2])
                {
                  disc[j, i] = 1
                }
                else if (ones[i, 2] == zeros[j, 2])
                {
                  ties[j, i] = 1
                }
              }
            }
            Pairs = dim(zeros)[1] * dim(ones)[1]
            PercentConcordance = (sum(conc) / Pairs) * 100
            PercentDiscordance = (sum(disc) / Pairs) * 100
            PercentTied = (sum(ties) / Pairs) * 100
            return(
              list(
                "Percent Concordance" = PercentConcordance,
                "Percent Discordance" = PercentDiscordance,
                "Percent Tied" = PercentTied,
                "Pairs" = Pairs
              )
            )
          }
          concordance_output <- concordance1(tmp$Target, tmp$prob)
          concordance_output
        }
        names(op)[names(op)==input$Target]="Target"
        concordance(op,"Target","prob")
      })

      concordancebv<-eventReactive(input$Comprconc, {
        concotablev()
      })

      output$concordancev<-renderTable({
        concordancebv()
      },bordered =T,caption = "Concordance (Validation)",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))



      statT1<-eventReactive(input$Compr, {

        op <- data.frame(get((input$dev)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")

        rtdev<- rnkorder()

        td<-data.frame(statistics="3rd Decile Capture",value=rtdev[3,12])
        lt<-data.frame(statistics="1st lift",value=rtdev[1,15])
        colnames(td)<-c("statistics","value")
        colnames(lt)<-c("statistics","value")






        names(op)[names(op)==input$Target]="Target"
        library(ineq)
        gini=ineq(op$prob,type = "Gini")

        library(ROCR)
        pred=prediction(op$prob,op$Target)
        perf=performance(pred,"tpr","fpr")
        ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
        ks
        auc=performance(pred,"auc")
        auc=as.numeric(auc@y.values)
        auc
        mp<-data.frame(statistics="GINI",value=gini*100)
        pp<-data.frame(statistics="AUC",value=auc*100)
        kp<-data.frame(statistics="KS",value=ks*100)

        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }

        hosmerlem_gof <- function(df, target, probability,g=10)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)

          hosmerlem <-
            function (y, yhat, g1=g) {
              cutyhat <-
                cut(yhat,
                    breaks = quantile(yhat, probs = seq(0, 1, 1 / g1)),
                    include.lowest = T)
              obs <-xtabs(cbind(1 - y, y) ~ cutyhat)
              expect <-xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
              chisq <-sum((obs - expect) ^ 2 / expect)
              P <-1 - pchisq(chisq, g1 - 2)
              c("X^2" = chisq,Df = g1 - 2,"P(>Chi)" = P)
            }
          hl_gof <- hosmerlem(tmp$Target, tmp$prob)
          # print(hl_gof)
          # print("Table")
          # sqldf ("select deciles, count(1) as cnt,
          #        sum (Target) as Obs_Resp, count (Target == 0) as Obs_Non_Resp,
          #        sum (prob) as Exp_Resp, sum (1 - prob) as Exp_Non_Resp
          #        from tmp
          #        group by deciles
          #        order by deciles desc")
          hl_gof
        }
        names(op)[names(op)==input$Target]="Target"

        oo<-data.table(t(hosmerlem_gof(op,"Target","prob")))

        ch<-data.frame(statistics="X^2", value=oo$`X^2`)
        ch1<-data.frame(statistics="X^2 P(>Chi)", value=oo$`P(>Chi)`)

        df<-rbind(kp,pp,mp,ch,ch1)

        df$value<-round(df$value,3)
        df<-rbind(df,td,lt)

        df
      })



      statT2<-eventReactive(input$Compr, {

        op <- data.frame(get((input$val)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        rtval<-rnkorderv()

        td<-data.frame(statistics="3rd Decile Capture",value=rtval[3,12])
        lt<-data.frame(statistics="1st lift",value=rtval[1,15])
        colnames(td)<-c("statistics","value")
        colnames(lt)<-c("statistics","value")


        names(op)[names(op)==input$Target]="Target"
        library(ineq)
        gini=ineq(op$prob,type = "Gini")

        library(ROCR)
        pred=prediction(op$prob,op$Target)
        perf=performance(pred,"tpr","fpr")
        ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
        ks
        auc=performance(pred,"auc")
        auc=as.numeric(auc@y.values)
        auc
        mp<-data.frame(statistics="GINI",value=gini*100)
        pp<-data.frame(statistics="AUC",value=auc*100)
        kp<-data.frame(statistics="KS",value=ks*100)

        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }

        hosmerlem_gof <- function(df, target, probability,g=10)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)

          hosmerlem <-
            function (y, yhat, g1=g) {
              cutyhat <-
                cut(yhat,
                    breaks = quantile(yhat, probs = seq(0, 1, 1 / g1)),
                    include.lowest = T)
              obs <-xtabs(cbind(1 - y, y) ~ cutyhat)
              expect <-xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
              chisq <-sum((obs - expect) ^ 2 / expect)
              P <-1 - pchisq(chisq, g1 - 2)
              c("X^2" = chisq,Df = g1 - 2,"P(>Chi)" = P)
            }
          hl_gof <- hosmerlem(tmp$Target, tmp$prob)
          # print(hl_gof)
          # print("Table")
          # sqldf ("select deciles, count(1) as cnt,
          #        sum (Target) as Obs_Resp, count (Target == 0) as Obs_Non_Resp,
          #        sum (prob) as Exp_Resp, sum (1 - prob) as Exp_Non_Resp
          #        from tmp
          #        group by deciles
          #        order by deciles desc")
          hl_gof
        }
        names(op)[names(op)==input$Target]="Target"

        oo<-data.table(t(hosmerlem_gof(op,"Target","prob")))

        ch<-data.frame(statistics="X^2", value=oo$`X^2`)
        ch1<-data.frame(statistics="X^2 P(>Chi)", value=oo$`P(>Chi)`)

        df<-rbind(kp,pp,mp,ch,ch1)

        df$value<-round(df$value,3)
        df<-rbind(df,td,lt)

        df
      })



      merg<-eventReactive(input$Compr, {
        tb1<-statT1()
        tb2<-statT2()
        tb3<-merge(tb1,tb2,by="statistics")
        colnames(tb3)<-c("Statistics","Development","Validation")
        tb3<-tb3[c(5,2,1,4,6,7,3),]
        tb3
      })



      output$stat2<-renderTable({
        merg()

      },bordered =T,
      caption = "Model Statistics",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))








      output$stat3<-renderTable({
        statT2()

      },bordered =T,
      caption = "Model Statistics",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))



      output$savelist<-
        renderUI({
          checkboxGroupInput(
            label = "Select Objects to Save" ,
            choices = list("Development Data","Validation Data","Model","Rank Ordering Table(Development)",
                           "Rank Ordering Table(Validation)","Comparision Table","Information Value Table",
                           "Single Variable glm Table"),
            selected = NULL,
            inputId = "savelist"
          )
        })


      information_value<-reactiveValues()
      p_value<-reactiveValues()
      Plot_data_varclus<-reactiveValues()
      glmmodel <- reactiveValues()
      vif_table<-reactiveValues()
      rank_order_development<-reactiveValues()
      model_stat<-reactiveValues()
      chi_sq<-reactiveValues()
      chi_sq_calculation<-reactiveValues()
      concordance_dev<-reactiveValues()
      beta_ratio_table<-reactiveValues()
      rank_order_validation<-reactiveValues()
      comparison_table<-reactiveValues()
      concordance_val<-reactiveValues()



      observe({
        if(is.null(ntextivv()))
          isolate(
            information_value <<- NULL
          )
        if(!is.null(ntextivv()))
          isolate(
            information_value <<- ntextivv()
          )
      })


      observe({
        if(is.null(ntextp()))
          isolate(
            p_value <<- NULL
          )
        if(!is.null(ntextp()))
          isolate(
            p_value <<- ntextp()
          )
      })


      observe({
        if(is.null(ntextpx()))
          isolate(
            Plot_data_varclus <<- NULL
          )

        if(!is.null(ntextpx()))
          isolate(
            Plot_data_varclus <<- ntextpx()
          )
      })


      observe({
        if(is.null(mylogit()))
          isolate(
            glmmodel <<- NULL
          )

        if(!is.null(mylogit()))
          isolate(
            glmmodel <<- mylogit()
          )
      })


      observe({

        if(is.null(VIFC()))
          isolate(
            vif_table <<- NULL
          )

        if(!is.null(VIFC()))
          isolate(
            vif_table <<- VIFC()
          )
      })


      observe({

        if(is.null(rankorderr()))
          isolate(
            rank_order_development <<-NULL
          )

        if(!is.null(rankorderr()))
          isolate(
            rank_order_development <<- rankorderr()
          )
      })


      observe({
        if(is.null(statT()))
          isolate(
            model_stat <<- NULL
          )

        if(!is.null(statT()))
          isolate(
            model_stat <<- statT()
          )
      })




      observe({
        if(is.null(chi1b()))
          isolate(
            chi_sq <<- NULL
          )

        if(!is.null(chi1b()))
          isolate(
            chi_sq <<- statT()
          )
      })



      observe({

        if(is.null(chi2b()))
          isolate(
            chi_sq_calculation <<- NULL
          )


        if(!is.null(chi2b()))
          isolate(
            chi_sq_calculation <<- chi2b()
          )
      })


      observe({
        if(is.null(concordanceb()))
          isolate(
            concordance_dev <<- NULL
          )

        if(!is.null(concordanceb()))
          isolate(
            concordance_dev <<- concordanceb()
          )
      })


      observe({
        if(is.null(betacal()))
          isolate(
            beta_ratio_table <<- NULL
          )

        if(!is.null(betacal()))
          isolate(
            beta_ratio_table <<- betacal()
          )
      })



      observe({
        if(is.null(rankorderrv()))
          isolate(
            rank_order_validation <<- NULL
          )

        if(!is.null(rankorderrv()))
          isolate(
            rank_order_validation <<- rankorderrv()
          )
      })


      observe({
        if(is.null(merg()))
          isolate(
            comparison_table <<- NULL
          )
        if(!is.null(merg()))
          isolate(
            comparison_table <<- merg()
          )

      })


      observe({
        if(is.null(concordancebv()))
          isolate(
            concordance_val <<-NULL
          )

        if(!is.null(concordancebv()))
          isolate(
            concordance_val <<- concordancebv()
          )
      })
      docummnt1<-reactive({
        pp<-input$documentation
        pp
      })

      observe({
        if(is.null(docummnt1()))
          isolate(
            docummnt <<- NULL
          )

        if(!is.null(docummnt1()))
          isolate(
            docummnt <<- docummnt1()
          )
      })


      output$downloadData <- downloadHandler(
        filename <- function(){
          paste("All.RData")
        },

        content = function(file) {
          save( information_value,p_value,Plot_data_varclus,glmmodel , vif_table,
                rank_order_development, model_stat,chi_sq,chi_sq_calculation, concordance_dev,beta_ratio_table,
                rank_order_validation, comparison_table,concordance_val, file = file)
          write.table(docummnt,file="documentation.txt")
        }
      )







      }
  }

  shinyApp(ui, server)
}}
