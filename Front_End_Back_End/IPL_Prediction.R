library(shiny)
library(randomForest)
library(caret)
library(dplyr)
library(ggplot2)
library(stringr)

matches <- read.csv("C:/Users/omhas/Documents/IPL Project/Front_End_Back_End/matches.csv")
deliveries <- read.csv("C:/Users/omhas/Documents/IPL Project/Front_End_Back_End/deliveries.csv")

inning_score <- deliveries %>% group_by(match_id, inning) %>% summarise(total_runs = sum(total_runs), .groups = 'drop') %>% filter(inning == 1)
inning_score <- inning_score %>% mutate(target = total_runs + 1)
colnames(matches)[colnames(matches) == "id"] <- "id"
colnames(inning_score)[colnames(inning_score) == "match_id"] <- "id"
matches <- matches %>% left_join(inning_score %>% select(id, target), by = "id")

matches$team1 <- gsub('Delhi Daredevils', 'Delhi Capitals', matches$team1)
matches$team2 <- gsub('Delhi Daredevils', 'Delhi Capitals', matches$team2)
matches$winner <- gsub('Delhi Daredevils', 'Delhi Capitals', matches$winner)

# Replace occurrences of 'Kings XI Punjab' with 'Punjab Kings'
matches$team1 <- gsub('Kings XI Punjab', 'Punjab Kings', matches$team1)
matches$team2 <- gsub('Kings XI Punjab', 'Punjab Kings', matches$team2)
matches$winner <- gsub('Kings XI Punjab', 'Punjab Kings', matches$winner)

# Replace occurrences of 'Deccan Chargers' with 'Sunrisers Hyderabad'
matches$team1 <- gsub('Deccan Chargers', 'Sunrisers Hyderabad', matches$team1)
matches$team2 <- gsub('Deccan Chargers', 'Sunrisers Hyderabad', matches$team2)
matches$winner <- gsub('Deccan Chargers', 'Sunrisers Hyderabad', matches$winner)

# Replace occurrences of 'Rising Pune Supergiant' with 'Pune Warriors'
matches$team1 <- gsub('Rising Pune Supergiant', 'Pune Warriors', matches$team1)
matches$team2 <- gsub('Rising Pune Supergiant', 'Pune Warriors', matches$team2)
matches$winner <- gsub('Rising Pune Supergiant', 'Pune Warriors', matches$winner)

# Replace occurrences of 'Rising Pune Supergiants' with 'Pune Warriors'
matches$team1 <- gsub('Rising Pune Supergiants', 'Pune Warriors', matches$team1)
matches$team2 <- gsub('Rising Pune Supergiants', 'Pune Warriors', matches$team2)
matches$winner <- gsub('Rising Pune Supergiants', 'Pune Warriors', matches$winner)

# Replace occurrences of 'Gujarat Lions' with 'Gujarat Titans'
matches$team1 <- gsub('Gujarat Lions', 'Gujarat Titans', matches$team1)
matches$team2 <- gsub('Gujarat Lions', 'Gujarat Titans', matches$team2)
matches$winner <- gsub('Gujarat Lions', 'Gujarat Titans', matches$winner)

teams2024<- c('Rajasthan Royals', 'Royal Challengers Bangalore', 'Sunrisers Hyderabad', 'Delhi Capitals', 'Chennai Super Kings','Gujarat Titans', 'Lucknow Super Giants', 'Kolkata Knight Riders','Punjab Kings', 'Mumbai Indians')
city2024<-c('Chandigarh','Bangalore','Delhi','Mumbai','Abu Dhabi','Kanpur','Chennai','Cape Town','Bengaluru','Jaipur','Ahmedabad','Hyderabad','Johannesburg','Durban','Pune','Sharjah','Kolkata','Navi Mumbai','Port Elizabeth','Dubai','Centurion','Dharamsala','Nagpur','East London','Visakhapatnam','Rajkot','Cuttack','Ranchi','Kimberley','Raipur','Indore','Bloemfontein')

matches <- matches %>% filter(team1 %in% teams2024) %>% filter(team2 %in% teams2024) %>% filter(winner %in% teams2024)

matches <- matches %>% select(id, city, team1, team2, winner, target) %>% na.omit()

deliveries$batting_team <- str_replace_all(deliveries$batting_team, 'Delhi Daredevils', 'Delhi Capitals')
deliveries$batting_team <- str_replace_all(deliveries$batting_team, 'Kings XI Punjab', 'Punjab Kings')
deliveries$batting_team <- str_replace_all(deliveries$batting_team, 'Deccan Chargers', 'Sunrisers Hyderabad')
deliveries$batting_team <- str_replace_all(deliveries$batting_team, 'Rising Pune Supergiant', 'Pune Warriors')
deliveries$batting_team <- str_replace_all(deliveries$batting_team, 'Rising Pune Supergiants', 'Pune Warriors')
deliveries$batting_team <- str_replace_all(deliveries$batting_team, 'Pune Warriorss', 'Pune Warriors')
deliveries$batting_team <- str_replace_all(deliveries$batting_team, 'Gujarat Lions', 'Gujarat Titans')
deliveries <- deliveries %>% filter(batting_team %in% teams2024)

colnames(matches)[colnames(matches) == "ID"] <- "id"
colnames(deliveries)[colnames(deliveries) == "match_id"] <- "id"
final <- matches %>% inner_join(deliveries, by = "id")
final <- final %>% filter(inning == 2)

final$current_score <- ave(final$total_run, final$id, FUN = cumsum)
final$runs_left <- ifelse(final$target - final$current_score >= 0, final$target - final$current_score, 0)
final$balls_left <- ifelse(120 - final$over * 6 - final$ball >= 0, 120 - final$over * 6 - final$ball, 0)
final <- final %>% group_by(id) %>% mutate(wickets_left = 10 - cumsum(is_wicket)) %>% ungroup()
final$current_run_rate <- (final$current_score * 6) / (120 - final$balls_left)
final$required_run_rate <- ifelse(final$balls_left > 0, final$runs_left * 6 / final$balls_left, 0)

result <- function(row) {if (row["batting_team"] == row["winner"]) {return(1)} else {return(0)}}

final$result <- apply(final, 1, result)

if (!"BowlingTeam" %in% colnames(final)) {final$BowlingTeam <- NA}
if (!"BattingTeam" %in% colnames(final)) {final$BattingTeam <- NA}

# Update BowlingTeam
index1 <- which(final$team2 == final$batting_team)
index2 <- which(final$team1 == final$batting_team)

final$BowlingTeam[index1] <- final$team1[index1]
final$BowlingTeam[index2] <- final$team2[index2]

# Update BattingTeam
index3 <- which(final$team2 == final$bowling_team)
index4 <- which(final$team1 == final$bowling_team)

final$BattingTeam[index3] <- final$team1[index3]
final$BattingTeam[index4] <- final$team2[index4]

winningPred <- final %>% select(BattingTeam, BowlingTeam, city, runs_left, balls_left, wickets_left, current_run_rate, required_run_rate, target, result)

trf <- dummyVars(result ~ BattingTeam + BowlingTeam + city, data = winningPred, fullRank = TRUE)
X <- predict(trf, newdata = winningPred)
y <- winningPred$result

train_indices <- createDataPartition(y, p = 0.7, list = FALSE)

# Split the dataset
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

X_train <- as.data.frame(X_train)  
y_train <- as.factor(y_train)   

preProcessObj <- preProcess(X_train, method = 'medianImpute') 
X_train <- predict(preProcessObj, X_train)
X_train <- apply(X_train, 2, function(col) ifelse(is.na(col), Mode(col), col))

X_test <- predict(preProcessObj, X_test)

rf_model <- randomForest(x = X_train, y = as.factor(y_train), ntree =200, mtry = sqrt(ncol(X_train)))

ui <- fluidPage(
  titlePanel("IPL Match Winning Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("batting_team", "Batting Team:", choices = teams2024),
      selectInput("bowling_team", "Bowling Team:", choices = teams2024),
      selectInput("city", "City:", choices = city2024),
      numericInput("runs_left", "Runs Left:", value = 0, min = 0),
      numericInput("balls_left", "Balls Left:", value = 0, min = 0),
      numericInput("wickets_left", "Wickets Left:", value = 0, min = 0),
      numericInput("current_run_rate", "Current Run Rate:", value = 0, min = 0),
      numericInput("required_run_rate", "Required Run Rate:", value = 0, min = 0),
      numericInput("target", "Target:", value = 0, min = 0),
      actionButton("predict", "Predict")),
    mainPanel(
      h3("Winning Probability"),
      textOutput("result")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predict, {
    new_data <- data.frame(
      BattingTeam = input$batting_team,
      BowlingTeam = input$bowling_team,
      city = input$city,
      runs_left = input$runs_left,
      balls_left = input$balls_left,
      wickets_left = input$wickets_left,
      current_run_rate = input$current_run_rate,
      required_run_rate = input$required_run_rate,
      target = input$target
    )
    
    tryCatch({
      new_data_transformed <- predict(trf, newdata = new_data)
      
      prediction <- predict(rf_model, newdata = as.data.frame(new_data_transformed))
      
      output$result <- renderText({
        if (as.integer(prediction) == 1) {
          paste("Prediction: Batting Team", input$batting_team, "is more likely to win.")
        } else {
          paste("Prediction: Bowling Team", input$bowling_team, "is more likely to win.")
        }
      })
    }, error = function(e) {
      output$result <- renderText(paste("Error:", e$message))
    })
  })
}

shinyApp(ui = ui, server = server)   