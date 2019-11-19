library(xlsx)
library(ggplot2)
library(plotrix)
library(ggmap)
library(stringr)
library(plotly)
options(show.error.messages = TRUE)
coll = c(
"dark green",
"orange",
"yellow",
"purple",
"orange",
"light blue",
"red",
"light green",
"grey",
"green",
"pink",
"blue",
"yellow",
"violet",
"orange",
"purple",
"brown",
"red",
"gray",
"dark red",
"orange",
"violet"
)
main <- function() {
choice6 <- "yes"
while (choice6 == "yes") {
  print("SELECION MENU FOR DATA ANALYSIS")
  print("1.PRESS FOR TELECOM DATABASE ANALYSIS")
  print("2.PRESS FOR HOSPITAL DATABASE ANALYSIS")
  print("3.PRESS FOR SENTIMENT ANALYSIS WITH THE HELP OF TWITTER")
  choice <- readline(prompt = "Enter the choice: ")
  choice <- as.integer(choice)
  y <- 0
  allow <- 0
  
  if (choice == 1) {
    g <- read.csv("new.csv")
    head(g, 12)
    #to obtain 1st 12 rows of text file
    h <- unique(g$CaseType)
    # to reach the coulmn named CaseType
    n <- unique(g$Circle)
    n <- as.character(n)
    count1 <- c(0, 0, 0, 0)
    final2 <- matrix(nrow = length(n), ncol = 9)
    #length(colnames(final2))
    rownames(final2) <- c(unique(g$Circle))
    colnames(final2) <-
      c("Code",
        "Circle",
        unique(as.character(g$CaseType)),
        "max.aon",
        "min.aon",
        "avg.aon")
    for (j in 1:length(n)) {
      n[j] = as.character(n[j])
      tr <- strtrim(n[j], 3)
      final <-
        g[which(g[, 1] == n[j]), c("Circle", "CaseType", "AON")]
      for (i in 1:length(h)) {
        final1 <-
          final[which(final[, 2] == as.character(h[i])), c("Circle", "CaseType", "AON")]
        count1[i] = length(final1$CaseType)
      }
      roundd <- round(mean(final$AON), 2)
      submitt <-
        c(tr, n[j], count1, max(final$AON), min(final$AON), roundd)
      final2[j, ] <- submitt
    }
    writeLines("\n")
    writeLines("\n")
    print("Generating Intermediate data....Please wait...")
    writeLines("\n")
    
    writeLines("\n")
    print("================================================================")
    writeLines("\n")
    print(final2)
    writeLines("\n")
    print("================================================================")
    choice7 <- "yes"
    while (choice7 == "yes") {
      print("Colnames in the intermediate file are:-------->>>>>")
      index <- 2
      for (index in 3:length(colnames(final2)))
      {
        print(colnames(final2)[index])
      }
      #print(colnames(final2))
      colu <-
        readline(prompt = "Enter column name for Visualization(Circle vs [your input]): ")
      writeLines("\n")
      p <- 0
      for (i in 1:length(colnames(final2))) {
        if (colnames(final2)[i] == colu) {
          p <- i
          break
        }
      }
      if (p == 0) {
        stop("NO SUCH COLUMN NAME")
      }
      else{
        graphtitle = paste("Circle vs", colu)
        xlabel = "Circle"
      }
      final2 <- final2[order(as.integer(final2[, i])), ]
      # to sort in ascending order
      print(paste0("Soting Intermediate data in terms of: ", colu))
      writeLines("\n")
      print(final2)
      choice4 <-
        readline(prompt = "DO YOU WANT VISUALIZATION(yes/no):")
      if (choice4 == "yes") {
        fillcolor = final[, 2]
        # library("xlsx")
        write.xlsx(final2, "intermediate.xlsx")
        print("DATA PROCESSING.... COMPLETE")
        selection(xlabel, colu, allow, choice, p)
      }
      choice7 <-
        readline(prompt = "DO YOU WANT TO KEEP WORKING ON THIS DATABASE(yes/no):")
    }
  }
  else if (choice == 2) {
    #For Working on the 2nd database
    i <- 1
    j <- 1
    choice2 <- 0
    read <-
      read.csv("outcome-of-care-measures.csv", colClasses = "character")
    states <- unique(read$State)
    choice8 <- "yes"
    while (choice8 == "yes") {
      print("SEELCTION MENU:")
      print("1.FOR STATES SPECIFIC QUERIES")
      print("2.TO GET THE COMPRESSED DATA FOR ALL STATES")
      choice2 <- readline(prompt = "ENTER YOUR CHOICE:")
      
      if (choice2 == 1) {
        choice3 <- 0
        
        print("SELECTION MENU:")
        print("1.TO VIEW BEST/WORST HOSPITALS IN A PARTICULAR STATE")
        print("2.TO COMPARE TWO STATES")
        choice3 <- readline(prompt = "PLEASE ENTER YOUR CHOICE:")
        print("states are:")
        print(states)
        if (choice3 == 1) {
          statechoice <- readline(prompt = "ENTER THE NAME OF THE STATE")
          ok <- 0
          for (i in states) {
            if (i == statechoice) {
              ok <- 1
              break
            }
          }
          if (ok != 1) {
            print("NO SUCH STATE")
            yourcall <-
              readline(prompt = "DO YOU WANT TO RESTART THE PRPGRAM(yes/no):")
            if (yourcall == "no") {
              print("ENDING PROGRAM")
              options(show.error.messages = FALSE)
              stop("ABORTING")
            }
            else if (yourcall == "yes") {
              main()
            }
          }
          print("SELECT THE OUTCOME:")
          print("1.MORTALITY DUE TO HEART ATTACK")
          print("2.MORTALITY DUE TO HEART FAILURE")
          print("3.MORTALITY DUE TO PNEUMONIA")
          print("4.READMISSION DUE TO HEART ATTACK")
          print("5.READMISSION DUE TO HEART FAILURE")
          print("6.READMISSION DUE TO PNEUMONIA")
          choice4 <- readline(prompt = "Please chose your option")
          choice4 <- as.integer(choice4)
          bestworstcase <-
            readline(prompt = "SAY WHETHER TO SEE BEST OR WORST HOSPITALS IN THE STATE(best/worst)")
          
          rankhospital(statechoice, choice4, bestworstcase)
        }
        if (choice3 == 2) {
          statechoose <- c("", "")
          conti <- "yes"
          while (conti == "yes") {
            ok <- 0
            j <- 1
            statechoose[1] <-
              readline(prompt = "ENTER THE NAME OF STATE 1")
            for (i in states) {
              if (i == statechoose[1]) {
                ok <- 1
                break
              }
            }
            if (ok != 1) {
              print("NO SUCH STATE")
              yourcall <-
                readline(prompt = "DO YOU WANT TO RESTART THE PRPGRAM(yes/no):")
              if (yourcall == "no") {
                print("ENDING PROGRAM")
                options(show.error.messages = FALSE)
                stop("ABORTING")
              }
              else if (yourcall == "yes") {
                main()
              }
              
            }
            ok <- 0
            statechoose[2] <-
              readline(prompt = "ENTER THE NAME OF STATE 2")
            for (i in states) {
              if (i == statechoose[2]) {
                ok <- 1
                break
              }
            }
            if (ok != 1) {
              print("NO SUCH STATE")
              yourcall <-
                readline(prompt = "DO YOU WANT TO RESTART THE PRPGRAM(yes/no):")
              if (yourcall == "no") {
                print("ENDING PROGRAM")
                options(show.error.messages = FALSE)
                stop("ABORTING")
              }
              else if (yourcall == "yes") {
                main()
              }
            }
            #print(statechoose)
            allow <- 1
            #print(1)
            finaldata <- matrix(ncol = 7, nrow = 2)
            colnames(finaldata) <-
              c(
                "State",
                "mean-death-rate(heart attack)",
                "mean-death-rate(heart failure)",
                "mean-death-rate(pneumonia)",
                "mean-readmission-rate(heart attack)",
                "mean-readmission-rate(heart failure)",
                "mean-readmission-rate(pneumonia)"
              )
            for (i in statechoose) {
              readd <- read[which(read[, 7] == i), c(7, 11, 17, 23, 29, 35, 41)]
              #print(head(readd))
              readdd <-
                readd[which(readd[, 2] != "Not Available"), c(1, 2, 3, 4, 5, 6, 7)]
              m2 <- round(mean(as.integer(readdd[, 2])), 2)
              readdd <-
                readd[which(readd[, 3] != "Not Available"), c(1, 2, 3, 4, 5, 6, 7)]
              m3 <- round(mean(as.integer(readdd[, 3])), 2)
              readdd <-
                readd[which(readd[, 4] != "Not Available"), c(1, 2, 3, 4, 5, 6, 7)]
              m4 <- round(mean(as.integer(readdd[, 4])), 2)
              readdd <-
                readd[which(readd[, 5] != "Not Available"), c(1, 2, 3, 4, 5, 6, 7)]
              m5 <- round(mean(as.integer(readdd[, 5])), 2)
              readdd <-
                readd[which(readd[, 6] != "Not Available"), c(1, 2, 3, 4, 5, 6, 7)]
              m6 <- round(mean(as.integer(readdd[, 6])), 2)
              readdd <-
                readd[which(readd[, 7] != "Not Available"), c(1, 2, 3, 4, 5, 6, 7)]
              m7 <- round(mean(as.integer(readdd[, 7])), 2)
              store <- c(i, m2, m3, m4, m5, m6, m7)
              finaldata[j, ] <- store
              j <- j + 1
            }
            #print(2)
            state1 <-
              c(
                as.integer(finaldata[1, 2]),
                as.integer(finaldata[1, 3]),
                as.integer(finaldata[1, 4]),
                as.integer(finaldata[1, 5]),
                as.integer(finaldata[1, 6]),
                as.integer(finaldata[1, 7])
              )
            state2 <-
              c(
                as.integer(finaldata[2, 2]),
                as.integer(finaldata[2, 3]),
                as.integer(finaldata[2, 4]),
                as.integer(finaldata[2, 5]),
                as.integer(finaldata[2, 6]),
                as.integer(finaldata[2, 7])
              )
            
            # 
            # legend <-
            #   c(
            #     "mortality due to heart attack",
            #     "mortality due to heart failure",
            #     "mortality due to pneumonia",
            #     "readmission due to heart attack",
            #     "readmission due to heart failure",
            #     "readmission due to pnemonia"
            #   )
            # print(
            #   pyramid.plot(
            #     state1,
            #     state2,
            #     labels = legend,
            #     top.labels = c(finaldata[1, 1], "OUTCOMES", finaldata[2, 1]),
            #     gap = 15,
            #     unit = "CASES",
            #     laxlab = c(state1),
            #     show.values = TRUE,
            #     raxlab = c(state2),
            #     ndig = 0,
            #     main = "30 DAY CYCLE"
            #   )
            # )
            # 
            mychoice<-0
            while(mychoice!=99){
            print("1.Pyramid plot")
            print("2.Horizontal plot")
            print("99.EXIT!")
            mychoice<-readline(prompt= "Enter choice:")
            if(mychoice==1)
            {            pyramid(state1,state2,finaldata)}
            else
            {horizontal_bar(state1,state2,statechoose)}
           } 
            conti <-
              readline(prompt = "DO YOU WANT TO ANALYSIS MORE(yes/no):")
          }
        }
      }
      else if (choice2 == 2) {
        j <- 1
        finaldata <- matrix(ncol = 7, nrow = length(states))
        colnames(finaldata) <-
          c(
            "State",
            "mean-death-rate-heart-attack",
            "mean-death-rate-heart-failure",
            "mean-death-rate-pneumonia",
            "mean-readmission-rate-heart-attack",
            "mean-readmission-rate-heart-failure",
            "mean-readmission-rate-pneumonia"
          )
        print(states)
        for (i in states) {
          readd <- read[which(read[, 7] == i), c(7, 11, 17, 23, 29, 35, 41)]
          #print(head(readd))
          readdd <-
            readd[which(readd[, 2] != "Not Available"), c(1, 2, 3, 4, 5, 6, 7)]
          m2 <- round(mean(as.integer(readdd[, 2])), 2)
          readdd <-
            readd[which(readd[, 3] != "Not Available"), c(1, 2, 3, 4, 5, 6, 7)]
          m3 <- round(mean(as.integer(readdd[, 3])), 2)
          readdd <-
            readd[which(readd[, 4] != "Not Available"), c(1, 2, 3, 4, 5, 6, 7)]
          m4 <- round(mean(as.integer(readdd[, 4])), 2)
          readdd <-
            readd[which(readd[, 5] != "Not Available"), c(1, 2, 3, 4, 5, 6, 7)]
          m5 <- round(mean(as.integer(readdd[, 5])), 2)
          readdd <-
            readd[which(readd[, 6] != "Not Available"), c(1, 2, 3, 4, 5, 6, 7)]
          m6 <- round(mean(as.integer(readdd[, 6])), 2)
          readdd <-
            readd[which(readd[, 7] != "Not Available"), c(1, 2, 3, 4, 5, 6, 7)]
          m7 <- round(mean(as.integer(readdd[, 7])), 2)
          store <- c(i, m2, m3, m4, m5, m6, m7)
          finaldata[j, ] <- store
          j <- j + 1
        }
        print(finaldata)
        
        
        
        print("DATA PROCESSING COMPLETE")
        
        choice4 <-
          readline(prompt = "DO YOU WANT GRAPH VISUALIZATION(yes/no):")
        if (choice4 == "yes") {
          colu <-
            readline(prompt = "PLEASE ENTER THE NAME OF THE COLUMN CARFULLY(State vs [your choice]):")
          
          
          
          for (i in 1:length(colnames(finaldata))) {
            if (colnames(finaldata)[i] == colu) {
              p <- i
              print(p)
              break
            }
          }
          if (p == 0) {
            stop("NO SUCH COLUMN NAME")
          }
          else{
            finaldata <- finaldata[order(as.numeric(finaldata[, i])), ]
            print(finaldata)
            library("xlsx")
            write.xlsx(finaldata, "intermediate.xlsx")
            print("DATA PROCESSING COMPLETE")
            xlabel = "State"
            #print(xlabel)
            fillcolor = states
            #print(xlabel)
            #print(colu)
            #print(allow)
            #print(choice)
            #print(p)
            colu <- str_replace_all(colu, "-", ".")
            selection(xlabel, colu, allow, choice, p)
          }
        }
      }
      choice8 <-
        readline(prompt = "DO YOU TO KEEP WORKING ON THIS DATABASE(yes/no):")
    }
  }
  else if (choice == 3) {
    twitteranalyse()
  }
  choice6 <-
    readline(prompt = "DO YOU WANT TO ANALYZE THE DATABASES MORE(yes/no):")
}
}






pyramid<- function(state1,state2,finaldata)
{
  legend <-
    c(
      "mortality due to heart attack",
      "mortality due to heart failure",
      "mortality due to pneumonia",
      "readmission due to heart attack",
      "readmission due to heart failure",
      "readmission due to pnemonia"
    )
  print(
    pyramid.plot(
      state1,
      state2,
      labels = legend,
      top.labels = c(finaldata[1, 1], "OUTCOMES", finaldata[2, 1]),
      gap = 15,
      unit = "CASES",
      laxlab = c(state1),
      show.values = TRUE,
      raxlab = c(state2),
      ndig = 0,
      main = "30 DAY CYCLE"
    )
  )
}








horizontal_bar <- function(state1,state2,statechoose){
  
  y <-  c(  
            "mean-death-rate(heart attack)",
            "mean-death-rate(heart failure)",
            "mean-death-rate(pneumonia)",
            "mean-readmission-rate(heart attack)",
            "mean-readmission-rate(heart failure)",
            "mean-readmission-rate(pneumonia)"
  )
  
  
  data<- data.frame(y,state1,state2)
  p <- plot_ly(data, x = state1, y = ~y, type = 'bar', orientation = 'h', name = statechoose[1],
               marker = list(color = 'rgba(246, 78, 139, 0.6)',
                             line = list(color = 'rgba(246, 78, 139, 1.0)',
                                         width = 3))) %>%
    add_trace(x = ~state2, name = statechoose[2],
              marker = list(color = 'rgba(58, 71, 80, 0.6)',
                            line = list(color = 'rgba(58, 71, 80, 1.0)',
                                        width = 3))) %>%
    layout(barmode = 'stack',
           xaxis = list(title = ""),
           yaxis = list(title =""))
  print(p)
}








selection <- function(xlabel, colu, allow, choice, p) {
print("================================================================")
if (allow != 1) {
  y <- 0
  while (y != 99) {
    print("USER GRAPH SELECTON MENU:(99 TO EXIT)")
    print("1.SCATTER PLOT")
    print("2.BAR GRAPH")
    print("3.MY-MY-PLOT")
    print("4.FLOWER PLOT")
    if (choice == 1) {
      print("5.CIRCULAR WEDGE PLOT")
      print("6.PLOTTING ON MAP")
    }
    print("7.PIECHART PLOT")
    print("8 INTERACTIVE PIE CHART")
    print("9.INTERACTIVE BAR CHART ")
    y <- readline(prompt = "Please enter the choice:")
    #print(n)
    y <- as.integer(y)
    if (y == 1) {
      scatterplot(xlabel, colu, choice)
    }
    else if (y == 2) {
      barplot(xlabel, colu)
    }
    else if (y == 3) {
      mymyplot(xlabel, colu, choice)
    }
    else if (y == 4) {
      flowerplot(xlabel, colu, choice)
    }
    else if (y == 5) {
      circularwedgeplot(xlabel, colu, choice)
    }
    else if (y == 6) {
      mapplot(xlabel, p)
    }
    else if (y == 7) {
      pie3D(xlabel, graphtitle, p, colu)
    }
    else if (y == 8) {
      piechart_new(xlabel, p)
    }
    else if (y==9)
    {bar_interactive(p,colu)}
    
  }
  
}

}

scatterplot <- function(xlabel, colu, choice) {
if (choice == 2) {
  coll = "grey40"
}
xlab = xlabel
bar <- read.xlsx("intermediate.xlsx", 1)
cl <- bar[, colu]
cl <- factor(cl, levels = unique(cl))
cl
graphtitle = paste(xlabel, "vs", colu)
xlabel <- bar[, xlabel]
scat <- ggplot(bar, aes(x = xlabel , y = cl, color = coll))
print(scat)
print(
  scat + geom_point(show.legend = FALSE, size = 3.5) + labs(y = colu, x = xlab) + ggtitle(graphtitle) + theme(
    plot.title = element_text(colour = "black", face = "bold"),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      face = "bold"
    ),
    axis.text.y = element_text(
      angle = 0,
      vjust = 0.5,
      face = "bold"
    )
  )
)

}

barplot <- function(xlabel, colu) {
bar <- read.xlsx("intermediate.xlsx", 1)
cl <- bar[, colu]
cl <- factor(cl, levels = unique(cl))
xlab = xlabel
graphtitle = paste(xlabel, "vs", colu, sep = " ")
xlabel = bar[, xlabel]
scat <- ggplot(bar, aes(x = xlabel , y = cl, color = coll))
#scat<-ggplot(bar,aes(x = xlabel,color=coll))
print(scat)
print(
  scat  + geom_bar(
    stat = "identity",
    color = "black",
    fill = "steelblue"
  ) + labs(y = colu, x = xlab) + ggtitle(graphtitle) + geom_text(
    aes(label = cl),
    vjust = 1.6,
    color = "white",
    size = 3.5
  ) + theme(
    plot.title = element_text(colour = "black", face = "bold"),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      face = "bold",
      size = 12
    ),
    axis.text.y = element_text(
      angle = 0,
      vjust = 0.5,
      face = "bold",
      size = 12
    )
  )
)
}

mymyplot <- function(xlabel, colu, option) {
bar <- read.xlsx("intermediate.xlsx", 1)
cl <- bar[, colu]
cl <- factor(cl, levels = unique(cl))
xlab = xlabel
graphtitle = paste(xlabel, "vs", colu)
xlabel <- bar[, xlabel]
if (option == 2) {
  coll = "grey"
}
scat <- ggplot(bar, aes(x = xlabel , y = cl))
print(scat)
print(
  scat + geom_bar(
    stat = "identity",
    fill = coll,
    width = 1,
    color = "white"
  ) + labs(y = colu, x = xlab) + coord_polar(
    theta = "x",
    start = 0,
    direction = 1
  ) + ggtitle(graphtitle) + theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(colour = "black", face = "bold")
  ) + geom_text(aes(label = cl), colour = "black")
)
}

flowerplot <- function(xlabel, colu, choice) {
if (choice == 2) {
  coll = "grey40"
}
bar <- read.xlsx("intermediate.xlsx", 1)
cl <- bar[, colu]
cl <- factor(cl, levels = unique(cl))
graphtitle = paste(xlabel, "vs", colu)
xlab = xlabel
xlabel <- bar[, xlabel]
xlabel <- factor(xlabel, levels = unique(xlabel))
choice6 <- "yes"
while (choice6 == "yes") {
  print("SELECTION MENU:")
  print("1.flower circular wedge plot")
  print("2.flower mymyplot plot")
  choice5 <-
    readline(prompt = "PLEASE SELECT YOUR CHOICE(99 to exit):")
  choice5 <- as.integer(choice5)
  if (choice5 == 1) {
    scat <- ggplot(bar, aes(x = xlabel , y = cl))
    print(scat)
    print(
      scat + geom_bar(
        stat = "identity",
        fill = coll,
        width = 1,
        color = "white"
      ) + labs(y = colu, x = xlab) + coord_polar(theta = "y") + ggtitle(graphtitle) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(colour = "green")
        ) + geom_text(aes(label = xlabel))
    )
  }
  else if (choice5 == 2) {
    scat <- ggplot(bar, aes(x = xlabel , y = cl))
    print(scat)
    print(
      scat + geom_bar(
        stat = "identity",
        fill = coll,
        width = 1,
        color = "white"
      ) + labs(y = colu, x = xlab) + coord_polar(theta = "x") + ggtitle(graphtitle) + theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(colour = "green")
      ) + geom_text(aes(label = cl), colour = "black"),
      check_overlap = TRUE
    )
  }
  else{
    print("please enter the correct choice")
  }
  choice6 <- readline(prompt = "DO YOU WANT TO CONTINUE(yes/no):")
}
}

piechart <- function(xlabel, graphtitle, p, colu) {
p = p + 1
bar <- read.xlsx("intermediate.xlsx", 1)
bar
cl <- bar[, p]
cl <- factor(cl, levels = unique(cl))
cl

label <- bar[, 2]
label <- factor(label, levels = unique(label))
label
pie3D(
  x = c(cl),
  radius = 1.7,
  height = 0.3,
  start = 0.7,
  labels = label,
  labelcex = 0.7 ,
  mar = c(2, 2, 2, 2),
  explode = 0.4,
  main = paste0("Pie Chart of ", colu),
  shade = 0.8
)
}


piechart_new <- function(xlabel, p,colu) {
p = p + 1
print(p)
bar <- read.xlsx("intermediate.xlsx", 1)
bar
cl <- bar[, p]
label <- bar[, 2]
label
print(plot_ly(type = "pie",
        labels = label,
        values = cl ))   
}




circularwedgeplot <- function(xlabel, colu, option) {
bar <- read.xlsx("intermediate.xlsx", 1)
cl <- bar[, colu]
cl <- factor(cl, levels = unique(cl))
xlab = xlabel
graphtitle = paste(xlabel, "vs", colu)
xlabel = bar[, xlabel]
if (option == 2) {
  coll = "grey14"
}
scat <- ggplot(bar, aes(x = xlabel , y = cl))
print(scat)
print(
  scat + geom_bar(
    stat = "identity",
    fill = coll,
    width = 1,
    color = "white"
  ) + labs(y = colu, x = xlab) + coord_polar(theta = "y") + ggtitle(graphtitle) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(colour = "green")
    ) + geom_text(aes(label = xlabel))
)


#print(scat + geom_point() + stat_smooth(position="identity",geom="smooth",fill=coll,color="white",formula = y ~ x))
}


bar_interactive <- function(p,colu)
{
p=p+1
bar<-read.xlsx("intermediate.xlsx",1)
bar
label<-bar[,2]
cl<-bar[,p]
cl <- factor(cl, levels = unique(cl))
print(plot_ly(
  x = label,
  y = cl,
  name = "BAR CHART",
  type = "bar"
))
}




rankhospital <- function(state, outcome, num = "best") {
data <-
  read.csv("outcome-of-care-measures.csv", colClasses = "character")
#head(data)
if (outcome == 1) {
  m = 11
}
else if (outcome == 2) {
  m = 17
}
else if (outcome == 3) {
  m = 23
}
else if (outcome == 4) {
  m = 29
}
else if (outcome == 5) {
  m = 35
}
else if (outcome == 6) {
  m = 41
}
else{
  stop("invalid outcome")
}
final_data <- data[which(data[, 7] == state), c(2, 7, m)]
final_data1 <-
  final_data[which(final_data[, 3] != "Not Available"), c(3, 1, 2)]
#head(final_data1)
final_data2 <- final_data1[order(as.integer(final_data1[, 1])), ]
if (num == "best") {
  print(head(final_data2, 4))
} else if (num == "worst") {
  print(tail(final_data2, 4))
} else{
  print("BY DEFAULT SHOWING THE BEST HOSPITALS IN THE STATE")
  print(head(final_data2[as.numeric(num), 2]))
}
}

mapplot <- function(xlabel, p) {
read <- read.xlsx("intermediate.xlsx", 1)
maxx <- read.xlsx("MAXCASETYPE.xlsx", 1)
lon <- maxx$longitude
lat <- maxx$latitude
p = p + 1
print(p)
map <-
  get_googlemap(center = c(83, 23) ,
                zoom = 5 ,
                maptype = "terrain")
maxx$size = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
ggmap(map)
if (p == 4) {
  print(
    ggmap(map) + geom_point(
      data = read,
      aes(
        x = lon ,
        y = lat,
        colour = coll,
        alpha = 0.4,
        size = 3.5
      ),
      size = maxx$size,
      show.legend = FALSE
    ) + geom_text(
      data = read,
      aes(
        label = read$Query ,
        x = lon,
        y = lat
      ),
      colour = "grey4",
      inherit.aes = FALSE
    )
  )
}
else if (p == 5) {
  print(
    ggmap(map) + geom_point(
      data = read,
      aes(
        x = lon ,
        y = lat,
        colour = coll,
        alpha = 0.4,
        size = 3.5
      ),
      size = maxx$size,
      show.legend = FALSE
    ) + geom_text(
      data = read,
      aes(
        label = read$Request ,
        x = lon,
        y = lat
      ),
      colour = "grey4",
      inherit.aes = FALSE
    )
  )
}
else if (p == 6) {
  print(
    ggmap(map) + geom_point(
      data = read,
      aes(
        x = lon ,
        y = lat,
        colour = coll,
        alpha = 0.4,
        size = 3.5
      ),
      size = maxx$size,
      show.legend = FALSE
    ) + geom_label(
      data = read,
      aes(
        label = read$Complaint ,
        x = lon,
        y = lat
      ),
      colour = "grey4",
      inherit.aes = FALSE
    )
  )
}
else if (p == 7) {
  print(
    ggmap(map) + geom_point(
      data = read,
      aes(
        x = lon ,
        y = lat,
        colour = coll,
        alpha = 0.4,
        size = 3.5
      ),
      size = maxx$size,
      show.legend = FALSE
    ) + geom_label(
      data = read,
      aes(
        label = read$Fault ,
        x = lon,
        y = lat
      ),
      colour = "grey4",
      inherit.aes = FALSE
    )
  )
}
else if (p == 8) {
  print(
    ggmap(map) + geom_point(
      data = read,
      aes(
        x = lon ,
        y = lat,
        colour = coll,
        alpha = 0.4,
        size = 3.5
      ),
      size = maxx$size,
      show.legend = FALSE
    ) + geom_label(
      data = read,
      aes(
        label = read$max.aon ,
        x = lon,
        y = lat
      ),
      colour = "grey4",
      inherit.aes = FALSE
    )
  )
}
else if (p == 9) {
  print(
    ggmap(map) + geom_point(
      data = read,
      aes(
        x = lon ,
        y = lat,
        colour = coll,
        alpha = 0.4,
        size = 3.5
      ),
      size = maxx$size,
      show.legend = FALSE
    ) + geom_label(
      data = read,
      aes(
        label = read$min.aon ,
        x = lon,
        y = lat
      ),
      colour = "grey4",
      inherit.aes = FALSE
    )
  )
}
else if (p == 10) {
  print(
    ggmap(map) + geom_point(
      data = read,
      aes(
        x = lon ,
        y = lat,
        colour = coll,
        alpha = 0.4,
        size = 3.5
      ),
      size = maxx$size,
      show.legend = FALSE
    ) + geom_label(
      data = read,
      aes(
        label = read$avg.aon ,
        x = lon,
        y = lat
      ),
      colour = "grey4",
      inherit.aes = FALSE
    )
  )
}

#map<-get_googlemap(center = c(83,23) , zoom = 5 , maptype = "terrain")
#ggmap(map)
#maxx$size=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)
#print(ggmap(map) + geom_point(data=read,aes(x = lon , y = lat,colour=coll,alpha=0.4,size=3.5),size=maxx$size,show.legend=FALSE))
}

twitteranalyse <- function() {
library(tm)
#library(SnowballC)
library(wordcloud)
library(twitteR)
library(base64enc)
library(xlsx)
api_key <- "srW41n7zZ14TZwHkLJz2SfsAT"
api_secret <- "yVh85xCY4nPvhUk8WunysPmCdCkXqrIxUpk6iSIPOGKncsejAf"
access_token <-
  "745510041256484864-QEtVvmVWESBgarMlbI5xu5yZgPqlXjK"
access_token_secret <-
  "8phThhAxX7qbCZK77d8Mm3cAfK9OEznUTk9meNBrUcwP4"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
twitterchoice <-
  readline(prompt = "PLEASE ENTER THE SUBJECT ON WHICH YOU WANT TO SEARCH THE TWITTER")
tweet <- searchTwitter(twitterchoice, n = 200, lang = "en")
tweet <- sapply(tweet, function(x)
  x$getText())
#tweet<-tm_map(tweet,stemDocument)
choice9 <-
  readline(prompt = "DO YOU WANT A WORD CLOUD OR SENTIMENT GRAPH(1/2):")
choice9 <- as.integer(choice9)
if (choice9 == 1) {
  tweet <- tweet[!is.na(tweet)]
  names(tweet) = NULL
  tweet <- gsub("(RT|via)((?:\\b\\w*@\\w*?\\w+)+)", "", tweet)
  tweet <- gsub("[[:punct:]]", "", tweet)
  tweet <- gsub("@\\w+", "", tweet)
  tweet <- gsub("#", "", tweet)
  tweet <- gsub("http\\w+", "", tweet)
  tweet <- gsub("&", "", tweet)
  tweet <- gsub("?", "", tweet)
  tweet <- gsub("[ \t]{2,}", "", tweet)
  tweet <- gsub("^\\s+|\\s+$", "", tweet)
  tweet <- gsub("[[:digit:]]", "", tweet)
  tweet <- Corpus(VectorSource(tweet))
  tweet <- tm_map(tweet, PlainTextDocument)
  tweet <- tm_map(tweet, removePunctuation)
  tweet <- tm_map(tweet, removeWords, stopwords("english"))
  tweet <- tm_map(tweet, removeWords, c(twitterchoice))
  tweet <- tm_map(tweet, stemDocument)
  tw <- TermDocumentMatrix(tweet)
  tw <- as.matrix(tw, decreasing = TRUE)
  tw <- sort(rowSums(tw), decreasing = TRUE)
  tw <- data.frame(words = names(tw), freq = tw)
  Dark2 <- brewer.pal(8, "Dark2")
  wordcloud(
    tw$words,
    tw$freq,
    min.freq = 1,
    max.words = Inf,
    random.order = F,
    random.color = TRUE,
    rot.per = 0.15,
    color = Dark2
  )
  
}
else if (choice9 == 2) {
  countneg <- 0
  countpos <- 0
  countneutral <- 0
  z <- "bad evict evicted"
  z
  x <- " evic"
  s <- " "
  z <- paste(s, z, sep = "")
  z
  if (grepl(x, z)) {
    print("Is there...LOADING...")
  } else{
    print("INPUT not there..SORRY!")
  }
  negwords <- read.xlsx("negwordssource.xlsx", 1)
  poswords <- read.xlsx("positivewordssource.xlsx", 1)
  
  for (text in tweet) {
    text <- paste(s, text, sep = "")
    #print(text)
    posi <- 0
    negi <- 0
    for (neg in negwords$NEGWORDS) {
      char <- as.character(neg)
      char <- paste(s, char, s, sep = "")
      check <- grepl(char, text, ignore.case = TRUE)
      if (isTRUE(check)) {
        #print(char)
        negi <- negi + 1
      }
    }
    
    for (poss in poswords$POSTIVEWORDS) {
      char <- as.character(poss)
      char <- paste(s, char, s, sep = "")
      check <- grepl(char, text, ignore.case = TRUE)
      if (isTRUE(check)) {
        #print(char)
        posi <- posi + 1
      }
    }
    
    if (posi == negi) {
      countneutral <- countneutral + 1
    }
    else if (posi > negi) {
      countpos <- countpos + 1
      #countneg<-countneg+negi
    }
    else if (negi > posi) {
      countneg <- countneg + 1
    }
  }
  print(countneg)
  print(countpos)
  print(countneutral)
  
  plott <- matrix(nrow = 3, ncol = 3)
  colnames(plott) <-
    c("attribute", "number of responses", "percentageshare")
  #tweet[2]=as.character(tweet[2])
  plott[1, 1] = "negative"
  plott[1, 2] = countneg
  
  p = countneg / (countneg + countpos + countneutral) * 100
  plott[1, 3] = round(p, 1)
  
  plott[2, 1] = "postive"
  plott[2, 2] = countpos
  
  p = countpos / (countneg + countpos + countneutral) * 100
  plott[2, 3] = round(p, 1)
  
  plott[3, 1] = "neutral"
  plott[3, 2] = countneutral
  
  p = countneutral / (countneg + countpos + countneutral) * 100
  plott[3, 3] = round(p, 1)
  
  plott <- plott[order(as.integer(plott[, 2])), ]
  plott
  
  write.xlsx(plott, "plotingdatasheet.xlsx")
  library(ggplot2)
  pl <- read.xlsx("plotingdatasheet.xlsx", 1)
  plotting <-
    ggplot(pl, aes(
      x = attribute,
      y = c(5, 10, 50),
      width = 1
    )) + labs(y = "number of responses")
  print(
    plotting + geom_bar(
      stat = "identity",
      fill = c("red", "grey", "green"),
      colour = "white"
    ) + coord_polar(theta = "x") + geom_text(
      aes(label = pl$number.of.responses),
      vjust = 1,
      size = 3.5
    ) + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  )
  
}
}

main()
