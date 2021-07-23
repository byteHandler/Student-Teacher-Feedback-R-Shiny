library(shiny)
library(RSQLite)
library(stringdist)
#options(shiny.host = "192.168.0.100")
#options(shiny.port = 7086)
dbdrv = dbDriver("SQLite")
con <- dbConnect(dbdrv,dbname="county.sqlite")
question_array = dbSendQuery(conn = con,"select * from questions")
ress = fetch(question_array)
teacher_array =dbSendQuery(conn = con,"select * from teachers")
ress2 = fetch(dbSendQuery(conn = con,"select * from teachers"))
lengthh <- nrow(ress)
lengthh2 <- nrow(ress2)

samplee = 12
ui <- fluidPage(
  tabsetPanel(id= "main_panel",
    tabPanel(title = "START", value = "p1",strong("ENTER THE NAME AND ROLLNO"),
             textInput("current_name","NAME:"),
             textInput("current_rollno","ROLL NO:"),
             fluidRow(
               actionButton("ok","OK"),
               actionButton("anonymous","BE ANONYMOUS")
             )),
    tabPanel(title = "FEEDBACK",value = "p2","FEEDBACK by ",h6(verbatimTextOutput("current_name")),hr(),h3("Teacher Name :"),verbatimTextOutput("tname"),hr(),h3("Current Question :"),h2(verbatimTextOutput("curr_ques",placeholder = TRUE)),hr(),
            fluidRow(radioButtons("rad_but",label = "SELECT A RATING OUT OF 5",choices = list("1" = 1,"2" = 2,"3" = 3,"4" = 4,"5" = 5),inline = T,selected = 1)),
            actionButton("prev","PREVIOUS"),
            actionButton("nextt","NEXT")
    ),
    tabPanel(title = "STASTICTICS",value = "p3",hr(),tabsetPanel(id = "sub_panel",
                                                            tabPanel(title = "BY Teacher", value = "byteacher",fluidPage(
                                                              titlePanel("Select a teacher"),
                                                              sidebarLayout(
                                                                sidebarPanel(
                                                                  selectInput("teacherstat","Teachers :",choices =  ress2[,]),
                                                                  hr(),
                                                                  helpText("Data Sourced from the obtained feedbacks till now."),
                                                                  hr(),
                                                                  h3("Questions are :"),
                                                                  h4("Q1. Quality of Teaching"),
                                                                  h4("Q2. Class Control"),
                                                                  h4("Q3. Questioning"),
                                                                  h4("Q4. Encouragement to ask questions"),
                                                                  h4("Q5. Gives names of Reference Books"),
                                                                  h4("Q6. Gives proper assignments"),
                                                                  h4("Q7. Punctuality")),
                                                                
                                                                mainPanel(
                                                                  plotOutput("teacherplot")
                                                                )
                                                              )
                                                            )),
                                                            tabPanel(title = "BY Question",value = "byquestion",
                                                                     fluidPage(
                                                                       titlePanel("Select a question"),
                                                                       sidebarLayout(
                                                                         sidebarPanel(
                                                                           selectInput("questionstat","Questions :",choices =  ress[,]),
                                                                           hr(),
                                                                           helpText("Data Sourced from the obtained feedbacks till now."),
                                                                           hr()),  
                                                                         mainPanel(
                                                                           plotOutput("questionplot")
                                                                         )
                                                                       )
                                                                     )
                                                                     )
                                                            ))
  
  )

)

server <- function(input, output,session) {
  rval <- reactiveValues(counter=0,counter2 = 1,counter3 = 1,counter4=1)
  que_val <- reactiveValues(que1 = 1 , que2 = 1 , que3 = 1 , que4 = 1, que5 = 1, que6 = 1, que7 = 1)
  send_val <- reactiveValues(sendit = 0)
  
observeEvent(
  input$ok,
  {
    updateTabsetPanel(session,"main_panel",selected = "p2")
    output$current_name <- renderText({input$current_name})
    output$curr_ques <- renderText({paste(ress[1:1,])})
    output$tname <- renderText({paste(ress2[1:1,])})
  }
)
observeEvent(
  input$anonymous,
  {
    updateTabsetPanel(session,"main_panel",selected = "p2")
    output$current_name <- renderText({paste("ANONYMOUS")})
  }

)
observeEvent(
  input$nextt,
  { print("Before writing")
    print("--------------------")
    print(que_val$que1)
    print(que_val$que2)
    print(que_val$que3)
    print(que_val$que4)
    print(que_val$que5)
    print(que_val$que6)
    print(que_val$que7)
    print(rval$counter)
    print("--------------------")
    if(rval$counter == 0)
    {
      updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que2))
    }
    else if(rval$counter == 1)
    {
      updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que3))
    }
    else if(rval$counter == 2)
    {
      updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que4))
    }
    else if(rval$counter == 3)
    {
      updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que5))
    }
    else if(rval$counter == 4)
    {
      updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que6))
    }
    else if(rval$counter == 5)
    {
      updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que7))
    }
    else if(rval$counter == 6)
    {
      updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que7))
    }
  
     rval$counter = rval$counter + 1
      
      if(rval$counter ==1)
      {
        que_val$que1=as.integer(input$rad_but)
      }
      if(rval$counter ==2)
      {
        que_val$que2=as.integer(input$rad_but)
      }
      if(rval$counter ==3)
      {
        que_val$que3=as.integer(input$rad_but)
      }
      if(rval$counter ==4)
      {
        que_val$que4=as.integer(input$rad_but)
      }
      if(rval$counter ==5)
      {
        que_val$que5=as.integer(input$rad_but)
      }
      if(rval$counter ==6)
      {
        que_val$que6=as.integer(input$rad_but)
      }
      if(rval$counter ==7)
      {
        que_val$que7=as.integer(input$rad_but)
        send_val$sendit = 1
      }
      output$curr_ques <- renderText({paste(ress[(rval$counter+1):(rval$counter+1),])})
      output$tname <- renderText({paste(ress2[rval$counter2:rval$counter2,])})
      if(send_val$sendit == 1 && rval$counter == 7)
      {
        rval$counter = 0
        dbSendQuery(conn = con , paste("insert into feedback values('",input$current_name,"','",paste(ress2[(rval$counter2):(rval$counter2),]),"',",as.integer(que_val$que1),",",as.integer(que_val$que2),",",as.integer(que_val$que3),",",as.integer(que_val$que4),",",as.integer(que_val$que5),",",as.integer(que_val$que6),",",as.integer(que_val$que7),")"))
        print(paste("insert into feedback values('",input$current_name,"','",paste(ress2[(rval$counter2):(rval$counter2),]),"',",as.integer(que_val$que1),",",as.integer(que_val$que2),",",as.integer(que_val$que3),",",as.integer(que_val$que4),",",as.integer(que_val$que5),",",as.integer(que_val$que6),",",as.integer(que_val$que7),")"))
        que_val$counter2 = rval$counter2 + 1
        que_val$que1=1
        que_val$que2=1
        que_val$que3=1
        que_val$que4=1
        que_val$que5=1
        que_val$que6=1
        que_val$que7=1
      }
      print("After everything")
      print("--------------------")
      print(que_val$que1)
      print(que_val$que2)
      print(que_val$que3)
      print(que_val$que4)
      print(que_val$que5)
      print(que_val$que6)
      print(que_val$que7)
      print(rval$counter)
      print("--------------------")
      
  }
)
observeEvent(
  input$prev,
  {
    print("--------------------")
    print(que_val$que1)
    print(que_val$que2)
    print(que_val$que3)
    print(que_val$que4)
    print(que_val$que5)
    print(que_val$que6)
    print(que_val$que7)
    print("--------------------")
    print("Rval-counter")
    print(as.integer(rval$counter))
    if(rval$counter >= 1)
    {
      rval$counter = rval$counter - 1
      print(as.integer(rval$counter))
      if(rval$counter == 0)
      {
        updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que1))
      }
      else if(rval$counter == 1)
      {
        updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que2))
      }
      else if(rval$counter == 2)
      {
        updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que3))
      }
      else if(rval$counter == 3)
      {
        updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que4))
      }
      else if(rval$counter == 4)
      {
        updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que5))
      }
      else if(rval$counter == 5)
      {
        updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que6))
      }
      else if(rval$counter == 6)
      {
        updateRadioButtons(session,"rad_but",selected = as.integer(que_val$que7))
      }
    }
   # if(rval$counter == 1)
    #{
   #   rval$counter = lengthh
    #  rval$counter2 = rval$counter2 - 1
   # }
    output$curr_ques <- renderText({paste(ress[(rval$counter+1):(rval$counter+1),])})
    
    }
)
observeEvent(
  input$add_teacher,
  {
    dbSendQuery(conn = con,paste("insert into teachers values('",input$teacher_add,"')"))
  }
)
output$questionplot <- renderPlot(
  {
    ternn2 <- dbGetQuery(con,"select teach_name,avg(q1),avg(q2),avg(q3),avg(q4),avg(q5),avg(q6),avg(q7) from feedback group by teach_name ")
    print(gsub(" ","",input$questionstat))
    print(ress[1,])
    if(gsub(" ","",input$questionstat) == gsub(" ","",ress[1,]))
    {
      H2 <- c(ternn2[2,2],ternn2[3,2],ternn2[4,2],ternn2[5,2])
    }
    if(gsub(" ","",input$questionstat) == gsub(" ","",ress[2,]) )
    {
      H2 <- c(ternn2[2,3],ternn2[3,3],ternn2[4,3],ternn2[5,3])
    }
    if(gsub(" ","",input$questionstat) == gsub(" ","",ress[3,]) )
    {
      H2 <- c(ternn2[2,4],ternn2[3,4],ternn2[4,4],ternn2[5,4])
    }
    if(gsub(" ","",input$questionstat) == gsub(" ","",ress[4,]) )
    {
      H2 <- c(ternn2[2,5],ternn2[3,5],ternn2[4,5],ternn2[5,5])
    }
    if(gsub(" ","",input$questionstat) == gsub(" ","",ress[5,]))
    {
      H2 <- c(ternn2[2,6],ternn2[3,6],ternn2[4,6],ternn2[5,6])
    }
    if(gsub(" ","",input$questionstat) == gsub(" ","",ress[6,]))
    {
      H2 <- c(ternn2[2,7],ternn2[3,7],ternn2[4,7],ternn2[5,7])
    }
    if(gsub(" ","",input$questionstat) == gsub(" ","",ress[7,]))
    {
      H2 <- c(ternn2[2,8],ternn2[3,8],ternn2[4,8],ternn2[5,8])
    }
    barplot(H2,main=input$questionstat,names.arg =c(ress2[1,1],ress2[2,1],ress2[3,1],ress2[4,1]),ylab = "Feedback Points",xlab = "Teachers" )
  }
)
output$teacherplot <- renderPlot(
  {
    print("Executed")
    ternn <- dbGetQuery(con,"select teach_name,avg(q1),avg(q2),avg(q3),avg(q4),avg(q5),avg(q6),avg(q7) from feedback group by teach_name ")
    #curr_result = fetch(dbSendQuery(conn = con,paste("select * from teacher_performance where name='",gsub(" ","",input$teacherstat),"'",sep = "")
    if(gsub(" ","",input$teacherstat) == gsub(" ","",ress2[1,]))
    {
      curr_result= ternn[1,]
      print("Akshay")
    }
   if(gsub(" ","",input$teacherstat) == "Ritesh")
   {
      curr_result= ternn[2,]
   print("Ritesh")
   }
  if(gsub(" ","",input$teacherstat) == "Manish")
   {
     curr_result= ternn[3,]
   print("Manish")
   }
   if(gsub(" ","",input$teacherstat) == "Rajesh")
  {
     curr_result= ternn[4,]
      print("Rajesh")
    }
   #while(rval$counter3 <= lengthh2)
   # {
   #  print(lengthh2)
   #  print(rval$counter3)
    # print(gsub(" ","",input$teacherstat))
   #  print(gsub(" ","",ress2[rval$counter3,]))
    #  if(gsub(" ","",input$teacherstat) == gsub(" ","",ress2[rval$counter3,]))
   #   {
  #      print("Matched")
   #     rval$counter4 <- rval$counter3
  #      rval$counter3 <- 1
   #     break
  #    }
  #   else
  #   {
  #     rval$counter3 = rval$counter3 + 1  
  #   }
     
  # }
   # curr_result=ternn[rval$counter4,]
    H <- c(curr_result[,2],curr_result[,3],curr_result[,4],curr_result[,5],curr_result[,6],curr_result[,7],curr_result[,8])
    
    barplot(
     H,main = input$teacherstat,
     names.arg = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7"),
      ylab = "Feedback Points",
      xlab = "Question"
   )
   #print(ress[,1])
    #print(paste("select * from teacher_performance where name='",gsub(" ","",input$teacherstat),"'",sep =""))
    #print(gsub(" ","",input$teacherstat))
  }

)
}
#runApp(host = "192.168.0.100",port = "7069")
shinyApp(ui = ui, server = server)