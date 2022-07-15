# Choose the dataset 
data1 <- read.csv(file.choose(), header=T)
View(data1)
#Cleaning the DataSet
data1 <- data1[ -c(1,15,11:13,17:19) ]
                            
#Understanding
head(data1)
str(data1)
summary(data1)

                        #Scatter Plot
#Import Packages("ggplot2")
library(ggplot2)
#Import Packages("plotly")
library(plotly)
#Create a new ggplot and Determine axis x and y 
p1 <- ggplot(data1,aes(x=Age,y=Matches))
#add values (points) to graph to built Scatterplot and color
p1 <- p1 + geom_point(aes(color=Matches),size=3)
#add scaling of color from blue (low) to red (high)
p1 <- p1 + scale_color_gradient(low="blue",high = "red")
#add title to Graph
p1 <- p1 + ggtitle("Scatter Plot To Show The Experience Of Player")
#make graph more interactive
p1 <- ggplotly(p1) 
#show Graph
p1

                 ##More Interactive for Scatter Plot
#Make a Color Slider  For Scatter Plot

#Basic Slider for Scatter Plot
library(plotly)
# create steps for slider
steps <- list(
  list(args = list("marker.color", "red"), 
       label = "Red", 
       method = "restyle", 
       value = "1"
  ),
  list(args = list("marker.color", "green"), 
       label = "Green", 
       method = "restyle", 
       value = "2"
  ),
  list(args = list("marker.color", "blue"), 
       label = "Blue", 
       method = "restyle", 
       value = "3"
  )
)

pp1<- data1 
#Determine axis x and y 
pp1 <- pp1 %>% plot_ly(x = ~Age, y = ~Matches,
                       mode = "markers", 
                       marker = list(size = 10,
                                     color = 'green'), 
                       type = "scatter") 
pp1 <- pp1 %>% layout(title = "Basic Slider for Scatter Plot",
                      sliders = list(
                        list(
                          active = 1, 
                          currentvalue = list(prefix = "Color: "), 
                          pad = list(t = 60), 
                          steps = steps))) 

pp1



                              #BarPlot
#Import Packages("ggplot2")
library(ggplot2)
#Create dataframe of Names and Goals
data2 <- data1[ , c("Name","Goals")]
#sort of data by goals
sortdata <- order(-data2$Goals)
#create dataframe (datasorted) + Sorting >> add values of variable sortdata 
datasorted <- data2[sortdata,]
#show 6 of rows
data3 <- head(datasorted)
#Create a new ggplot and Determine axis x and y 
p2 <- ggplot(data3, aes(x=Name, y=Goals,fill=Players))
#create a Bar chart and color this by valuesof  column (Name)
p2 <- p2 + geom_bar(aes(fill=Name),stat="identity")
#add title to Graph
p2 <- p2 + ggtitle("Bar Plot To Show The Top 6 Scorers In The Season")
#make graph more interactive
p2<- ggplotly(p2) 
#show Graph
p2

                    ##More Interactive for Bar chart

#Drop down menus for barchart and Piechart
library(plotly)


pp2 <- plot_ly(data3, x = ~Name, y = ~Goals)
pp2 <- pp2 %>% layout(
  title = "Two Graphs Show The Top 6 Scorers In The Season",
  
  updatemenus = list(
    list(
      buttons = list(
        
        list(method = "restyle",
             args = list("type", "bar"),
             label = "Bar chart"),
        
        list(method = "restyle",
             args = list("type", "pie"),
             label = "Pie chart")))
  ))

pp2




                               #PieChart
#Import Packages("plotly")
library(plotly)
#create a dataframe to edit on it 
data4 <- data1
#crate a column which contain the values of NON_Penalties_Goals by Subtract goals and Penalty_Goals
data4$NON_Penalties_Goals <- (data4$Goals - data4$Penalty_Goals)
#Create a variable which contain the Sum of Penalty_Goals
Penalty_Goals = sum(data4$Penalty_Goals)
#Create a variable which contain the Sum of NON_Penalties_Goals
NON_Penalties_Goals = sum(data4$NON_Penalties_Goals)
#crate a dataframe which contain the values of 2 variables 
Goals <- c(Penalty_Goals,NON_Penalties_Goals)
df <- data.frame(Goals)
row.names(df)[1] <- "Penalty_Goals"
row.names(df)[2] <- "NON_Penalties_Goals"
df <- data.frame("Categorie"=rownames(df), df)
data <- df[,c('Categorie', 'Goals')]
#Determine axis x and y 
p3 <- plot_ly(data, x = ~Categorie, y = ~Goals)
p3 <- p3 %>% layout(
  #add title to Graph
  title = "Two Graphs To Show the Number of Goals with Penalties and Without",
  
  updatemenus = list(
    list(
      #Make drop-down Button to change Between two Graphs
      buttons = list(
        
        list(method = "restyle",
             args = list("type", "bar"),
             label = "Bar chart"),
        
        list(method = "restyle",
             args = list("type", "pie"),
             label = "Pie chart")))
  ))
#show Graph
p3


#Animation Graph to Show Each Player Stat

library(plotly)

p4 <- data1 %>%
  plot_ly(
    x = ~Age, 
    y = ~Goals, 
    size = ~Matches, 
    color = ~Club, 
    frame = ~Position, 
    text = ~Name, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )
p4 <- p4 %>% layout(
  title="Animation Graph to Show Each Player Stat",
  xaxis = list(
    type = "log"
  )
)
p4 <- p4 %>%
  animation_slider(
    currentvalue = list(prefix = "Position: ", font = list(color="blue"))
  )
p4




                                 #BoxPlot
#Import Packages("ggplot2")
library(ggplot2)
#Create a new ggplot and Determine axis x and y 
p5 <- ggplot(data1,aes(x=Club,y=Goals,fill=Clubs))
#adding a box and whiskers plot (in the style of Tukey)
p5 <- p5 + geom_boxplot(aes(fill=Club))
#make graph horizontal because there are many clubs
p5 <- p5 + coord_flip() 
#add title to Graph
p5 <- p5 + ggtitle("Box Plot To Show Most clubs Scored In The Season")
#make graph more interactive
p5 <- ggplotly(p5) 
#show Graph
p5


                      ##More Interactive for Box Plot
#Add Scatter Plot to Box plot
pp5 <- data1%>%
  plot_ly( x = ~Club, y = ~Goals, type="scatter"
           , name="ScatterPlot")%>% layout(title="Box and Scatter Plot To Show Most clubs Scored In The Season")%>%
  add_boxplot(name="BoxPlot") 
pp5



#BeeswarmPlot
#Create a variable which Contain values of condition (True Or False) 
englishPlayer <- data1$Nationality == "ENG"
#adding the values to column EnglishPlayer in DataSet (data1)
data1$EnglishPlayer <- englishPlayer
# Import Packages("ggplot2")
library(ggplot2)
# Import Packages("ggbeeswarm")
library(ggbeeswarm)
#Create a new ggplot and Determine axis x and y 
p6 <- ggplot(data1, aes(x = EnglishPlayer, y = Goals,
                        #add color to True and another color to False
                        color = EnglishPlayer))
#Create Beeswarm Plot by using geom_quasirandom()
p6 <- p6 + geom_quasirandom()
#add title to Graph
p6 <- p6 + ggtitle("Beeswarm Plot To Show Goals of English and Foreign Player In The Season")
#make graph more interactive
p6 <- ggplotly(p6) 
#show Graph
p6


#Animation Graph to Show Each Player how many Mins play
#and is English player or not .

library(plotly)

pp6 <- data1 %>%
  plot_ly(
    x = ~Age, 
    y = ~Mins, 
    size = ~Matches, 
    color = ~EnglishPlayer, 
    frame = ~Position, 
    text = ~Name, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )
pp6 <- pp6 %>% layout(
  title="Animation Graph to Show Each Player how many Mins play and is English player or not",
  xaxis = list(
    type = "log")
) 
pp6 <- pp6 %>%
  animation_slider(
    currentvalue = list(prefix = "Position: ", font = list(color="red"))
  )
pp6




                                  #Density Plot
#Import Packages("plotly")
library(plotly)
#Create a dataframe with values of Matches and starts
Matches <- data1$Matches
Starts <- data1$Starts
Matches <- data.frame(Matches_and_Starts = Matches)
Starts <- data.frame(Matches_and_Starts= Starts)
#Now, combine  two dataframes into one.  First make a new column in each. to color data with them 
Matches$Colors <- 'Matches'
Starts$Colors <- 'Starts'
#and combine into your new data frame (data6)
data6 <- rbind(Matches, Starts)
#Create a new ggplot and Determine axis x and y with fill of colors by column Colors
p7 <- ggplot(data6, aes(Matches_and_Starts, fill = Colors)) +
##Create Density Plot by using geom_density()
  geom_density(alpha=0.4) +
  #add title to Graph
  ggtitle("Density Plot of the Matches and Starts")
#make graph more interactive
p7 <- ggplotly(p7) 
#show Graph
p7

                                  #lmplot
#Import Packages("ggplot2")
library(ggplot2)
#Create a new ggplot and Determine axis x and y 
p8 <- ggplot(data1, aes(x = Goals, y = xG)) + 
  #add values (points) 
  geom_point(color="blue") +
  #Creates a smooth line(color:red) by adding the argument method="lm"
  geom_smooth(formula = y ~ x,method = "lm", col = "red")
#add title to Graph
p8 <- p8 + ggtitle("Plot of Goals Vs xG")
#make graph more interactive
p8 <- ggplotly(p8) 
#show Graph
p8
























