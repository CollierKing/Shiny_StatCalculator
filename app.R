library(shiny)

shinyApp (
  shinyUI(
    navbarPage("Stat Calculator for Marketing Research",
               tabPanel("Sample Size", uiOutput('page1')),
               tabPanel("A/B Testing", uiOutput('page2')),
               tabPanel("Margin of Error", uiOutput('page3'))
    )
  ),
  
  shinyServer(function(input, output, session) {
    
    
    output$page1 <- renderUI({
      sidebarLayout(
        sidebarPanel(
          tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                     tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')),
          radioButtons("radio", label = h3("Compute Sample Size Needed For:"),
                       choices = list("A - Estimation of Sample Proportion - Discrete Variable" = 1, 
                                      "B - Estimation of Sample Mean - Continuous Variable" = 2)),
          
          numericInput("num1", label = h5("Enter Desired Margin of Error $$ \\large\\ e $$"), value = .05),
          
          numericInput("num2", label = h5("Enter Population Standard Deviation $$ \\large\\sigma $$"), value = 100)
          
          
        ),
        mainPanel(
          
          withMathJax(helpText("Formula A - Use when e is a percentage, ex: 5% $$ \\large\\ n=\\frac{1}{e^2}$$ Formula B - Use when e is a continuous variable, ex: 100 $$ \\large\\ n=\\frac{4*\\sigma^2}{e^2}$$")),
          # }),
        
          output$text2 <- renderText({
            "Sample Size:"
          }),
          output$value4646 <- renderPrint({
            if ((input$radio==1)){
              
              
              1/(input$num1^2)
              # "Sample Size Formula (: $$ \\Huge\\frac{1}{e^2}$$"
              
            } else { 
              4*(input$num2^2)/(input$num1^2)
              
            }
          })
            )
        )
      
    })
    
    #Page 1
    output$page2 <- renderUI({
      
      fixedRow(
        column(9,
               "",
               fixedRow(
                 column(3,
                        "",
                        numericInput("numCP",label = h5("Control Population"),value=1000),
                        numericInput("numCR",label = h5("Control Response"),value=100),
                        
                        # output$text999999999 <- renderText({
                        #         
                        #         "test"
                        #         
                        # }),
                        
                        output$valueYLD979984 <- renderPrint({
                          
                          input$numCR/input$numCP
                          
                        })
                 ),
                 column(3,
                        "",
                        numericInput("testCP",label=h5("Test Population"),value=1000),
                        numericInput("testCR",label=h5("Test Response"),value=100),
                        
                        output$Value28433311143 <- renderPrint({
                          
                          input$testCR/input$testCP
                        })
                        
                 ),
                 column(5, offset = 1,
                        "Group Superiority:",
                        output$Value341762342467 <- renderPrint({
                          if (input$numCR/input$numCP == input$testCR/input$testCP){
                            'Equal'
                          } else if (input$numCR/input$numCP > input$testCR/input$testCP){
                            'Control Group'
                          } else {
                            'Test Group'
                          }
                        })
                 ),
                 column(5,
                        "Lift Percent:", offset = 1,
                        output$Value4416871467 <- renderPrint({
                          ((input$testCR/input$testCP) - (input$numCR/input$numCP))/ (input$numCR/input$numCP)
                        })
                 ),
                 
                 column(5,
                        "Z-Score:", offset = 1,
                 
                 output$value87292 <- renderPrint({
                   
                   cntPop <- input$numCP
                   cntRsp <- input$numCR
                   cntYld <- cntRsp/cntPop
                   tstPop <- input$testCP
                   tstRsp <- input$testCR
                   tstYld <- tstRsp/tstPop
                   
                   stdErrorControl <- sqrt(((cntRsp/cntPop)*(1-(cntRsp/cntPop))/cntPop))
                   stdErrorTest <- sqrt(((tstRsp/tstPop)*(1-(tstRsp/tstPop))/tstPop))
                   zScore <- (cntYld - tstYld)/sqrt((stdErrorControl^2)+(stdErrorTest^2))
                   
                   zScore
                   
                 }),
                 
                 column(9,
                        "P value:",
                        
                        output$value454643114 <- renderPrint({
                          
                          cntPop <- input$numCP
                          cntRsp <- input$numCR
                          cntYld <- cntRsp/cntPop
                          tstPop <- input$testCP
                          tstRsp <- input$testCR
                          tstYld <- tstRsp/tstPop
                          
                          stdErrorControl <- sqrt(((cntRsp/cntPop)*(1-(cntRsp/cntPop))/cntPop))
                          stdErrorTest <- sqrt(((tstRsp/tstPop)*(1-(tstRsp/tstPop))/tstPop))
                          zScore <- (cntYld - tstYld)/sqrt((stdErrorControl^2)+(stdErrorTest^2))
                          pVal <- pnorm(zScore)
                          
                          pVal
                          
                        })
                 ),
                 column(7,
                        "90% Confidence Level:",
                        output$value21131313 <- renderPrint({
                          cntPop <- input$numCP
                          cntRsp <- input$numCR
                          cntYld <- cntRsp/cntPop
                          tstPop <- input$testCP
                          tstRsp <- input$testCR
                          tstYld <- tstRsp/tstPop
                          
                          stdErrorControl <- sqrt(((cntRsp/cntPop)*(1-(cntRsp/cntPop))/cntPop))
                          stdErrorTest <- sqrt(((tstRsp/tstPop)*(1-(tstRsp/tstPop))/tstPop))
                          zScore <- (cntYld - tstYld)/sqrt((stdErrorControl^2)+(stdErrorTest^2))
                          pVal <- pnorm(zScore)
                          if (pVal > .9 | pVal < .1){
                            "TRUE"
                          }
                        })
                 ),
                 column(7,
                        "95% Confidence Level:",
                        output$value36161616 <- renderPrint({
                          cntPop <- input$numCP
                          cntRsp <- input$numCR
                          cntYld <- cntRsp/cntPop
                          tstPop <- input$testCP
                          tstRsp <- input$testCR
                          tstYld <- tstRsp/tstPop
                          
                          stdErrorControl <- sqrt(((cntRsp/cntPop)*(1-(cntRsp/cntPop))/cntPop))
                          stdErrorTest <- sqrt(((tstRsp/tstPop)*(1-(tstRsp/tstPop))/tstPop))
                          zScore <- (cntYld - tstYld)/sqrt((stdErrorControl^2)+(stdErrorTest^2))
                          pVal <- pnorm(zScore)
                          if (pVal > .95 | pVal < .05){
                            "TRUE"
                          }
                        })
                 ),
                 column(7,
                        "99% Confidence Level:",
                        output$value46446710 <- renderPrint({
                          cntPop <- input$numCP
                          cntRsp <- input$numCR
                          cntYld <- cntRsp/cntPop
                          tstPop <- input$testCP
                          tstRsp <- input$testCR
                          tstYld <- tstRsp/tstPop
                          
                          stdErrorControl <- sqrt(((cntRsp/cntPop)*(1-(cntRsp/cntPop))/cntPop))
                          stdErrorTest <- sqrt(((tstRsp/tstPop)*(1-(tstRsp/tstPop))/tstPop))
                          zScore <- (cntYld - tstYld)/sqrt((stdErrorControl^2)+(stdErrorTest^2))
                          pVal <- pnorm(zScore)
                          if (pVal > .99 | pVal < .01){
                            "TRUE"
                          }
                        }))
                 
                 
               )
        ),
        column(12,
               "",
               fixedRow(
                 column(6,
                        withMathJax(helpText("Standard Error Formula Control Group $$\\large\\ SE_c = \\sqrt{\\frac{Resp_c}{Resp_t}*(1-\\frac{Yield_c}{Pop_c})}$$")),
                        
                        
                        withMathJax(helpText("Z-Score Formula $$\\large\\ Z = \\frac{Yield_c - Yield_t}{\\sqrt{(SE_c^2+SE_t^2)}}$$"))
                        
                        
                 ),
                 
                 column(6,
                        withMathJax(helpText("Standard Error Formula Test Group $$\\large\\ SE_t = \\sqrt{\\frac{Resp_t}{Resp_c}*(1-\\frac{Yield_t}{Pop_t})}$$")),
                        
                        
                        withMathJax(helpText("P-Value calculated with R function pnorm on Z-Score value"))
                        
                 )
               )
               
        )
      )
      )
      
    })
    
    output$page3 <- renderUI({
      
      fixedRow(
        column(9,
               "",
               fixedRow(
                 column(5,
                        "Estimation of a Sample Proportion",
                        numericInput("sampSize1",label = h5("Sample Size"),value = 1000),
                        numericInput("sampResp1", label= h5("Sample Response"), value = 100),
                        
                        
                        output$text4599898985 <- renderText({
                          "Sample Proportion:"
                        }),
                        
                        output$valueSP4245 <- renderPrint({
                          input$sampResp1/input$sampSize1
                        }),
                        
                        output$text244645 <- renderText({
                          "Standard Error of Sample Proportion:"
                          
                        }),
                        
                        output$valueSE45245 <- renderPrint({
                          
                          sqrt((input$sampResp1/input$sampSize1)*(1-((input$sampResp1/input$sampSize1)))/input$sampSize1)
                          
                          
                        }),
                        
                        
                        output$text34245 <- renderText({
                          "Standard Error 95% CI: Lower:"
                          
                        }),
                        
                        output$valueCI4254 <- renderPrint({
                          
                          (input$sampResp1/input$sampSize1) - (1.95*(sqrt((input$sampResp1/input$sampSize1)*(1-((input$sampResp1/input$sampSize1)))/input$sampSize1)))
                          
                        }),
                        
                        
                        output$text3785272 <- renderText({
                          "Standard Error 95% CI: Upper:"
                          
                        }),
                        
                        output$valueCI42547 <- renderPrint({
                          
                          (input$sampResp1/input$sampSize1) + (1.95*(sqrt((input$sampResp1/input$sampSize1)*(1-((input$sampResp1/input$sampSize1)))/input$sampSize1)))
                          
                        }),
                        
                        
                        output$text452727 <- renderText({
                          
                          "Margin of Error"
                          
                          
                        }),
                        
                        output$me27272744 <- renderPrint({
                          
                          ((input$sampResp1/input$sampSize1) + (1.95*(sqrt((input$sampResp1/input$sampSize1)*(1-((input$sampResp1/input$sampSize1)))/input$sampSize1)))) - (input$sampResp1/input$sampSize1)
                          
                        }),
                        
                        
                        withMathJax(helpText("Standard Error of Sample Proportion Formula $$\\large\\ SE(\\hat{p}) = \\sqrt{\\frac{\\hat{p}*(1-\\hat{p})}{n}}$$")),
                        
                        withMathJax(helpText("95% Confidence Interval of Sample Proportion Formula $$\\large\\ (\\hat{p} - (1.96 * \\hat{p}),   \\hat{p} + (1.96 * \\hat{p}))  $$"))
                        
                 ),
                 column(5, offset=2,
                        "Estimation of a Sample Mean",
                        numericInput("sampSize2",label = h5("Sample Size"),value = 1000),
                        numericInput("sampMean", label= h5("Sample Mean"), value = 2),
                        numericInput("sampSD", label = h5("Sample Standard Deviation"),value=0.5),
                        
                        output$text4888889 <- renderText({
                          
                          "Standard Error of Sample Mean"
                          
                        }),
                        
                        output$ValueSE298983 <- renderPrint({
                          
                          input$sampSD/(sqrt(input$sampSize2))
                          
                        }),
                        
                        output$text3213538 <- renderText({
                          "Standard Error 95% CI: Lower:"
                          
                        }),
                        
                        output$valueCI118984 <- renderPrint({
                          
                          input$sampMean - (1.96*(input$sampSD/(sqrt(input$sampSize2))))
                          
                        }),
                        
                        
                        output$text331618 <- renderText({
                          "Standard Error 95% CI: Upper:"
                          
                        }),
                        
                        output$valueCI224649 <- renderPrint({
                          
                          input$sampMean + (1.96*(input$sampSD/(sqrt(input$sampSize2))))
                          
                        }),
                        
                        output$text4198719 <- renderText({
                          
                          "Margin of Error"
                          
                          
                        }),
                        
                        output$me1989883 <- renderPrint({
                          
                          ((input$sampMean + (1.96*(input$sampSD/(sqrt(input$sampSize2))))) - input$sampMean)
                        }),
                        
                        withMathJax(helpText("Standard Error of Sample Mean Formula $$\\large\\ SE(\\bar X) = \\frac{\\sigma}{\\sqrt{n}} $$")),
                        
                        withMathJax(helpText("95% Confidence Interval of Sample Proportion Formula $$\\large\\ (\\bar X - (1.96 * \\bar X),   \\bar X + (1.96 * \\bar X))  $$"))
                        
                 )
               )
        )
      )
      
    })
    
    
    
    })
)