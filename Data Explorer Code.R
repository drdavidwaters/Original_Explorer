#library
library(plyr) #used for rbind.fill.matrix for writing files
#library(ggplot2)

#change data set to reflect data set (e.g. csem, bema, fci, etc.)
data_set <- "fci"

#Answer keys
if (data_set == "csem") {key = c("B","A","B","B","C","E","B","B","B","C","E","D","E","D","A","E","E","D","A","D","E","D","A","C","D","A","E","C","C","A","E","D")} # answer Key for CSEM test
if (data_set == "bema") {key = c("A","A","B","E","A","D","E","B","B","F","E","E","D","B","G","B","D","B","B","G","A","E","E","A","D","D","C","B","C","F","D")} # answer Key for BEMA test
if (data_set == "fci") {key = c("C","A","C","E","B","B","B","B","C","A","D","B","D","D","A","A","B","B","E","D","E","B","B","A","C","E","C","E","B","C")} # answer Key for FCI test

#Read csv file to import Student Data File. Change C:/path/file.xlsx to import, and run this script.
Ans0 <- read.csv("C:/Users/david.waters/Desktop/DataExplorerResults/FCI_1-4-2019.csv", header=TRUE) #student Answers, whole file, with blanks, Pre and Post

#Define number of extra columns (n) and numbers of questions (m)
if (data_set == "csem") {n=4;m=32}    #CSEM-Table1
if (data_set == "bema") {n=4;m=31}    #BEMA-Table1
if (data_set == "fci")  {n=20;m=30}   #FCI_1-4-2019

#delete leading columns
letters_answers <- Ans0[, -c(1:n)]

#change letters to numbers
if (data_set == "csem") {numbers <- sapply(letters_answers, chartr, old = "ABCDE", new = "12345")}
if (data_set == "bema") {numbers <- sapply(letters_answers, chartr, old = "ABCDEFGHIJ", new = "1234567890")}
if (data_set == "fci") {numbers <- sapply(letters_answers, chartr, old = "ABCDE", new = "12345")}

#separate pre and post
pre <- numbers[,1:m]
post <- numbers[,(m+1):(2*m)]

#Add column with row numbers
nAns0 <- nrow(pre)
rownames(pre) <- 1:nAns0
rownames(post) <- 1:nAns0

#Remove blank rows
pre_no_blanks <- pre[!apply(pre == "", 1, all),]
post_no_blanks <- post[!apply(post == "", 1, all),]

#Find the number of rows and columns
nrStud <- nrow(Ans0) #find number of rows, and that is the number of students in the data set
nrQ <- ncol(Ans0)        #find number of columns in the data file
nrows_pre <- nrow(pre_no_blanks)
nrows_post <- nrow(post_no_blanks)
nrows <- nrows_pre + nrows_post

#Define scoring function - scores concept surveys
scorer = function(
  key = key, # the keys of the test
  responses = Ans0, # student responses
  students = nrStud, # number of students to score
  columns = nrQ # number of columns in the data file
){
  if (data_set == "csem") {answers = t(as.data.frame(c("ID","Gender","Race","Engagement",rep(as.character(key),2))))} #create our working data frame
  if (data_set == "bema") {answers = t(as.data.frame(c("ID","Gender","Race","Engagement",rep(as.character(key),2))))} #create our working data frame
  if (data_set == "fci") {answers = t(as.data.frame(c("ID","DataSet","Course Level","Engagement","Highest Level of Math","Course Grade","Gender","Section","GPA","Year in School","Race","Major","TOEFL Score","Score Percentage","High School Physics","ACT Score","Score Raw","SAT Score","Ethnicity","Expected Graduation Year",rep(as.character(key),2))))} #create our working data frame
  colnames(answers) = colnames(responses) # create the header of the data frame
  rownames(answers) = c("key")
  answers.expanded = answers[rep(rownames(answers),students), 1:columns]
  pre = responses[,c((n+1):(m+n))]
  post = responses[,c((m+n+1):(2*m+n))]
  pre[c(which(rowSums(pre == "") > m/3)),] <- NA # Part of an esoteric solution to denote blank tests
  post[c(which(rowSums(post == "") > m/3)),] <- NA # while also making sure pre and post matchup
  cleanResponses = cbind(pre, post) #recombines for scoring
  #Different strategy for bema
  if (data_set == "bema") {
    scores = as.data.frame((answers.expanded[,-c(1:n)] == cleanResponses))*1 #grade normally, adjust later
    prescore = scores[,c(1:m)] #splits back into pre and post
    postscore = scores[,c((m+1):(2*m))]
    #score 28 and 29 which together count as 1 point
    prescore[,28] <- (prescore[,28] == 1 & prescore[,29] == 1)*1 
    prescore[,29] <- NULL 
    postscore[,28] <- (postscore[,28] == 1 & postscore[,29] == 1)*1
    postscore[,29] <- NULL
    #score 14 and 16
    prescore[,16] <- ((pre[,14] != "" | !is.na(pre[,14])) & (pre[,14] == pre[,16]) & (pre[,15] == "G"))*1
    prescore[, 14] <- NULL
    postscore[,16] <- ((post[,14] != "" | !is.na(post[,14])) & (post[,14] == post[,16]) & (post[,15] == "G"))*1
    prescore[,14] <- NULL
    #score 3
    prescore[,3] <- (
      (pre[,2] == "A" & pre[,3] == "B") |
        (pre[,2] == "B" & pre[,3] == "D") |
        (pre[,2] == "C" & pre[,3] == "C") |
        (pre[,2] == "D" & pre[,3] == "E") |
        (pre[,2] == "E" & pre[,3] == "F") |
        (pre[,2] == "F" & pre[,3] == "G")
    )*1
    postscore[,3] <- (
      (post[,2] == "A" & post[,3] == "B") |
        (post[,2] == "B" & post[,3] == "D") |
        (post[,2] == "C" & post[,3] == "C") |
        (post[,2] == "D" & post[,3] == "E") |
        (post[,2] == "E" & post[,3] == "F") |
        (post[,2] == "F" & post[,3] == "G")
    )*1
    prescoresClean = rowSums(prescore)
    postscoresClean = rowSums(postscore)
    final.scores = cbind(scores,prescoresClean,postscoresClean)
  }
  else {
    #Esoteric Solution pt. 2: 1 for correct answer, 0 for wrong, and NA for blank
    scores = as.data.frame((answers.expanded[,-c(1:n)] == cleanResponses))*1
    prescore = scores[,c(1:m)] #Scores and splits back into pre and post
    postscore = scores[,c((m+1):(2*m))]
    prescoresClean = rowSums(prescore) #Sums up num of correct responses
    postscoresClean = rowSums(postscore)
    final.scores = cbind(scores,prescoresClean,postscoresClean)
  }
  return(final.scores)
}

#Cohen's d function
cohen = function(
  A = prescores,
  B = postscores
){
  mean.gain = mean(B) - mean(A)
  deviation = sqrt(((nrpre-1)*sd(A)*sd(A)+(nrpost-1)*sd(B)*sd(B))/(nrpre+nrpost-2))
  cohen = mean.gain / deviation
  return(cohen)
}

#Grades for all tests
scored <- scorer(key, Ans0, nrStud, nrQ)

#Grades combined with original answers
scored.data <- cbind(Ans0, scored)

#Remove blank scored tests
pre_clean <- na.omit(scored[,c(1:m)]) #Scored Pretests with no blanks
post_clean <- na.omit(scored[,c((m+1):(2*m))])  #Scored Posttests with no blanks
scored_clean <- na.omit(scored)  #Students who took both the pre and post test

#Number of students excluding blanks after scoring
nrpre <- sum(!is.na(scored.data$prescoresClean))
nrpost <- sum(!is.na(scored.data$postscoresClean))
nrscored <- nrpre+nrpost

#Check for number of incorrect easy questions
if (data_set == "csem") {easy_pre <- cbind(pre_clean[,3],pre_clean[,12],pre_clean[,6],pre_clean[,1],pre_clean[,8],pre_clean[,26],pre_clean[,9],pre_clean[,18],pre_clean[,4],pre_clean[,23])}
if (data_set == "bema") {easy_pre <- cbind(pre_clean[,1],pre_clean[,18],pre_clean[,21],pre_clean[,2],pre_clean[,8],pre_clean[,19],pre_clean[,13],pre_clean[,4],pre_clean[,3],pre_clean[,14])}
#if (data_set == "bema") {easy_pre <- cbind(pre_clean[,1],pre_clean[,2],pre_clean[,8],pre_clean[,13],pre_clean[,19],pre_clean[,21])}
if (data_set == "fci") {easy_pre <- cbind(pre_clean[,6],pre_clean[,12],pre_clean[,1],pre_clean[,24],pre_clean[,7],pre_clean[,16],pre_clean[,10],pre_clean[,27],pre_clean[,29],pre_clean[,19])}

if (data_set == "csem") {easy_post <- cbind(post_clean[,3],post_clean[,12],post_clean[,6],post_clean[,1],post_clean[,8],post_clean[,26],post_clean[,9],post_clean[,18],post_clean[,4],post_clean[,23])}
if (data_set == "bema") {easy_post <- cbind(post_clean[,1],post_clean[,21],post_clean[,13],post_clean[,19],post_clean[,8],post_clean[,4],post_clean[,2],post_clean[,15],post_clean[,24],post_clean[,3])}
#if (data_set == "bema") {easy_post <- cbind(post_clean[,1],post_clean[,2],post_clean[,8],post_clean[,13],post_clean[,19],post_clean[,21])}
if (data_set == "fci") {easy_post <- cbind(post_clean[,6],post_clean[,12],post_clean[,1],post_clean[,24],post_clean[,7],post_clean[,16],post_clean[,10],post_clean[,27],post_clean[,29],post_clean[,19])}

freq_easy_pre <- c(table(easy_pre[,1:1]),table(rowSums(easy_pre[,1:2])),table(rowSums(easy_pre[,1:3])),table(rowSums(easy_pre[,1:4])),table(rowSums(easy_pre[,1:5])),table(rowSums(easy_pre[,1:6])),table(rowSums(easy_pre[,1:7])),table(rowSums(easy_pre[,1:8])),table(rowSums(easy_pre[,1:9])),table(rowSums(easy_pre[,1:10])))
freq_easy_post <- c(table(easy_post[,1:1]),table(rowSums(easy_post[,1:2])),table(rowSums(easy_post[,1:3])),table(rowSums(easy_post[,1:4])),table(rowSums(easy_post[,1:5])),table(rowSums(easy_post[,1:6])),table(rowSums(easy_post[,1:7])),table(rowSums(easy_post[,1:8])),table(rowSums(easy_post[,1:9])),table(rowSums(easy_post[,1:10])))

freq_easy <- freq_easy_pre + freq_easy_post
freq_easy_zeros <- c(freq_easy[0.5*1*(1+1)],freq_easy[0.5*2*(2+1)],freq_easy[0.5*3*(3+1)],freq_easy[0.5*4*(4+1)],freq_easy[0.5*5*(5+1)],freq_easy[0.5*6*(6+1)],freq_easy[0.5*7*(7+1)],freq_easy[0.5*8*(8+1)],freq_easy[0.5*9*(9+1)],freq_easy[0.5*10*(10+1)])

freq_easy_zeros_pre <- c(freq_easy_pre[0.5*1*(1+1)],freq_easy_pre[0.5*2*(2+1)],freq_easy_pre[0.5*3*(3+1)],freq_easy_pre[0.5*4*(4+1)],freq_easy_pre[0.5*5*(5+1)],freq_easy_pre[0.5*6*(6+1)],freq_easy_pre[0.5*7*(7+1)],freq_easy_pre[0.5*8*(8+1)],freq_easy_pre[0.5*9*(9+1)],freq_easy_pre[0.5*10*(10+1)])
freq_easy_zeros_post <- c(freq_easy_post[0.5*1*(1+1)],freq_easy_post[0.5*2*(2+1)],freq_easy_post[0.5*3*(3+1)],freq_easy_post[0.5*4*(4+1)],freq_easy_post[0.5*5*(5+1)],freq_easy_post[0.5*6*(6+1)],freq_easy_post[0.5*7*(7+1)],freq_easy_post[0.5*8*(8+1)],freq_easy_post[0.5*9*(9+1)],freq_easy_post[0.5*10*(10+1)])

nrows_easy <- freq_easy_zeros[4]

easy_rows_pre4 <- which(rowSums(easy_pre[,1:4]) == 0)
easy_rows_post4 <- which(rowSums(easy_post[,1:4]) == 0)

easy_rows_pre <- as.integer(rownames(pre_clean)[easy_rows_pre4])
easy_rows_post <- as.integer(rownames(post_clean)[easy_rows_post4])


#Mean, SD, and Median calculations
Mean_Pre <- 100*mean(scored.data$prescoresClean, na.rm=TRUE)/m # percentile average score of all students
Mean_Post <- 100*mean(scored.data$postscoresClean, na.rm=TRUE)/m # percentile average score of all students
Mean_Combined <- 100*mean(c(scored.data$prescoresClean,scored.data$postscoresClean), na.rm=TRUE)/m # percentile average score of all students
SD_Pre <- 100*sd(scored.data$prescoresClean, na.rm=TRUE)/m  # percentile standard deviation  of students scores
SD_Post <- 100*sd(scored.data$postscoresClean, na.rm=TRUE)/m  # percentile standard deviation  of students scores
SD_Combined <- 100*sd(c(scored.data$prescoresClean,scored.data$postscoresClean), na.rm=TRUE)/m # percentile average score of all students
Median_Pre <- 100*median(scored.data$prescoresClean, na.rm=TRUE)/m # percentile median of students scores
Median_Post <- 100*median(scored.data$postscoresClean, na.rm=TRUE)/m # percentile median of students scores

#Find score per question. Which question is answered correctly by most of the students?
score.col <- colSums(scored[,c(1:(2*m))], na.rm=TRUE)

score.col.combined <- 100*(score.col[1:m]+score.col[(m+1):(2*m)])/(nrpre+nrpost)
score.col.pre <- 100*score.col[1:m]/nrpre
score.col.post <- 100*score.col[(m+1):(2*m)]/nrpost

#gains and t-test
gain <- scored_clean$postscoresClean - scored_clean$prescoresClean
summary_gain <- summary(gain)
TableGains <- table(scored$prescoresClean,scored$postscoresClean) #not exported
Cohen <- cohen(A=scored_clean$prescoresClean,B=scored_clean$postscoresClean)
TTest <- t.test(scored_clean$prescoresClean,scored_clean$postscoresClean) #not exported

#Calculate and print the gain three ways:
#1. first by person (only people with pre and post)  <(post - pre)/(questions-pre)>
gain_1 <- mean((scored_clean$postscoresClean[scored_clean$prescoresClean!=m] - scored_clean$prescoresClean[scored_clean$prescoresClean!=m])/(m-scored_clean$prescoresClean[scored_clean$prescoresClean!=m]))

#2. second, by averaging pre and post then normalizing (only people with pre and post) (<post> - <pre>)/(questions-<pre>)
gain_2 <- (mean(scored_clean$postscoresClean) - mean(scored_clean$prescoresClean))/(m-mean(scored_clean$prescoresClean))

#3. third, by averaging pre and post then normalizing (for all people). (<post> - <pre>)/(questions-<pre>)
gain_3 <- (Mean_Post - Mean_Pre)/(100-Mean_Pre)

#Determine distribution of answers per question
if (data_set == "csem") {freq_choice_pre <- cbind(as.matrix(colSums(pre_no_blanks == "1")),as.matrix(colSums(pre_no_blanks == "2")),as.matrix(colSums(pre_no_blanks == "3")),as.matrix(colSums(pre_no_blanks == "4")),as.matrix(colSums(pre_no_blanks == "5")))}
if (data_set == "csem") {freq_choice_post <- cbind(as.matrix(colSums(post_no_blanks == "1")),as.matrix(colSums(post_no_blanks == "2")),as.matrix(colSums(post_no_blanks == "3")),as.matrix(colSums(post_no_blanks == "4")),as.matrix(colSums(post_no_blanks == "5")))}
if (data_set == "bema") {freq_choice_pre <- cbind(as.matrix(colSums(pre_no_blanks == "1")),as.matrix(colSums(pre_no_blanks == "2")),as.matrix(colSums(pre_no_blanks == "3")),as.matrix(colSums(pre_no_blanks == "4")),as.matrix(colSums(pre_no_blanks == "5")),as.matrix(colSums(pre_no_blanks == "6")),as.matrix(colSums(pre_no_blanks == "7")),as.matrix(colSums(pre_no_blanks == "8")),as.matrix(colSums(pre_no_blanks == "9")),as.matrix(colSums(pre_no_blanks == "0")))}
if (data_set == "bema") {freq_choice_post <- cbind(as.matrix(colSums(post_no_blanks == "1")),as.matrix(colSums(post_no_blanks == "2")),as.matrix(colSums(post_no_blanks == "3")),as.matrix(colSums(post_no_blanks == "4")),as.matrix(colSums(post_no_blanks == "5")),as.matrix(colSums(post_no_blanks == "6")),as.matrix(colSums(post_no_blanks == "7")),as.matrix(colSums(post_no_blanks == "8")),as.matrix(colSums(post_no_blanks == "9")),as.matrix(colSums(pre_no_blanks == "0")))}
if (data_set == "fci") {freq_choice_pre <- cbind(as.matrix(colSums(pre_no_blanks == "1")),as.matrix(colSums(pre_no_blanks == "2")),as.matrix(colSums(pre_no_blanks == "3")),as.matrix(colSums(pre_no_blanks == "4")),as.matrix(colSums(pre_no_blanks == "5")))}
if (data_set == "fci") {freq_choice_post <- cbind(as.matrix(colSums(post_no_blanks == "1")),as.matrix(colSums(post_no_blanks == "2")),as.matrix(colSums(post_no_blanks == "3")),as.matrix(colSums(post_no_blanks == "4")),as.matrix(colSums(post_no_blanks == "5")))}

colnames(freq_choice_pre) <- letters[1:5]
colnames(freq_choice_post) <- letters[1:5]

if (data_set == "bema") {colnames(freq_choice_pre) <- letters[1:10]}
if (data_set == "bema") {colnames(freq_choice_post) <- letters[1:10]}

freq_choice_combined <- freq_choice_pre + freq_choice_post

rownames(freq_choice_combined) <- 1:m

#search for rows with more than 50% one letter
freq_50_pre <- cbind(as.matrix(rowSums(pre_no_blanks == "1")),as.matrix(rowSums(pre_no_blanks == "2")),as.matrix(rowSums(pre_no_blanks == "3")),as.matrix(rowSums(pre_no_blanks == "4")),as.matrix(rowSums(pre_no_blanks == "5")),as.matrix(rowSums(pre_no_blanks == "6")),as.matrix(rowSums(pre_no_blanks == "7")),as.matrix(rowSums(pre_no_blanks == "8")),as.matrix(rowSums(pre_no_blanks == "9")))
check_50_letters_pre <- sum(freq_50_pre > m/2)
Rows_50_pre <- which(freq_50_pre > m/2, arr.ind=TRUE)

freq_50_post <- cbind(as.matrix(rowSums(post_no_blanks == "1")),as.matrix(rowSums(post_no_blanks == "2")),as.matrix(rowSums(post_no_blanks == "3")),as.matrix(rowSums(post_no_blanks == "4")),as.matrix(rowSums(post_no_blanks == "5")),as.matrix(rowSums(post_no_blanks == "6")),as.matrix(rowSums(post_no_blanks == "7")),as.matrix(rowSums(post_no_blanks == "8")),as.matrix(rowSums(post_no_blanks == "9")))
check_50_letters_post <- sum(freq_50_post > m/2)
Rows_50_post <- which(freq_50_post > m/2, arr.ind=TRUE)

check_50_letters <- check_50_letters_pre + check_50_letters_post

#look for 8 letters in a row 
Checkletterspre <- as.numeric(grepl("1{8}|2{8}|3{8}|4{8}|5{8}",apply(pre_no_blanks,1,paste0,collapse="")))
Check_letters_pre <- sum(Checkletterspre)
Rows_letters_pre <- which(Checkletterspre == 1)

Checkletterspost <- as.numeric(grepl("1{8}|2{8}|3{8}|4{8}|5{8}",apply(post_no_blanks,1,paste0,collapse="")))
Check_letters_post <- sum(Checkletterspost)
Rows_letters_post <- which(Checkletterspost == 1)

Check_letters <- Check_letters_pre + Check_letters_post

#look for 2 instances of ABCDE, 1 instance of ABCDEDCBA, and 3 instance of ABCD
Checkpatternspre <- as.numeric(grepl("^12345.*12345|^1234.*1234.*1234|123454321",apply(pre_no_blanks,1,paste0,collapse="")))
Check_patterns_pre <- sum(Checkpatternspre)
Rows_patterns_pre <- which(Checkpatternspre == 1)

Checkpatternspost <- as.numeric(grepl("^12345.*12345|^1234.*1234.*1234|123454321",apply(post_no_blanks,1,paste0,collapse="")))
Check_patterns_post <- sum(Checkpatternspost)
Rows_patterns_post <- which(Checkpatternspost == 1)

Check_patterns <- Check_patterns_pre + Check_patterns_post

#check for more than 50% zeros
check50zerospre <- nchar(apply(pre_no_blanks,1,paste0,collapse=""))
check_50_zeros_pre <- sum(check50zerospre < m*0.5)
Rows_50_zeros_pre <- which(check50zerospre < m*0.5)

check50zerospost <- nchar(apply(post_no_blanks,1,paste0,collapse=""))
check_50_zeros_post <- sum(check50zerospost < m*0.5)
Rows_50_zeros_post <- which(check50zerospost < m*0.5)

check_50_zeros <- check_50_zeros_pre + check_50_zeros_post

#Check for the number of uncommon answers chosen
if (data_set == "csem") {uncommon_pre <- as.matrix(
  as.numeric(grepl("1|5",pre_no_blanks[,1]))+as.numeric(grepl("1|5",pre_no_blanks[,3]))+as.numeric(grepl("1|5",pre_no_blanks[,4]))+
    as.numeric(grepl("4|5",pre_no_blanks[,7]))+as.numeric(grepl("1|5",pre_no_blanks[,8]))+as.numeric(grepl("3|5",pre_no_blanks[,12]))+
    as.numeric(grepl("3|4",pre_no_blanks[,13]))+as.numeric(grepl("1|2",pre_no_blanks[,18]))+as.numeric(grepl("1|5",pre_no_blanks[,24]))
)}

if (data_set == "csem") {uncommon_post <- as.matrix(
  as.numeric(grepl("1|5",post_no_blanks[,1]))+as.numeric(grepl("1|5",post_no_blanks[,3]))+as.numeric(grepl("1|5",post_no_blanks[,4]))+
    as.numeric(grepl("4|5",post_no_blanks[,7]))+as.numeric(grepl("1|5",post_no_blanks[,8]))+as.numeric(grepl("3|5",post_no_blanks[,12]))+
    as.numeric(grepl("3|4",post_no_blanks[,13]))+as.numeric(grepl("1|2",post_no_blanks[,18]))+as.numeric(grepl("1|5",post_no_blanks[,24]))
)}

if (data_set == "bema") {uncommon_pre <- as.matrix(
  as.numeric(grepl("3|4|7",pre_no_blanks[,1]))+as.numeric(grepl("3|4|7",pre_no_blanks[,2]))+as.numeric(grepl("8|9",pre_no_blanks[,10]))+
    as.numeric(grepl("4|10",pre_no_blanks[,21]))+as.numeric(grepl("2|4|10",pre_no_blanks[,4]))+as.numeric(grepl("1|8",pre_no_blanks[,25]))+
    as.numeric(grepl("4|10",pre_no_blanks[,5]))+as.numeric(grepl("4|5|6",pre_no_blanks[,14]))+as.numeric(grepl("5|8|9",pre_no_blanks[,3]))
)}

if (data_set == "bema") {uncommon_post <- as.matrix(
  as.numeric(grepl("3|4|7",post_no_blanks[,1]))+as.numeric(grepl("3|4|7",post_no_blanks[,2]))+as.numeric(grepl("8|9",post_no_blanks[,10]))+
    as.numeric(grepl("4|10",post_no_blanks[,21]))+as.numeric(grepl("2|4|10",post_no_blanks[,4]))+as.numeric(grepl("1|8",post_no_blanks[,25]))+
    as.numeric(grepl("4|10",post_no_blanks[,5]))+as.numeric(grepl("4|5|6",post_no_blanks[,14]))+as.numeric(grepl("5|8|9",post_no_blanks[,3]))
)}

if (data_set == "fci") {uncommon_pre <- as.matrix(
  as.numeric(grepl("2|3|4",pre_no_blanks[,4]))+as.numeric(grepl("3|4|5",pre_no_blanks[,6]))+as.numeric(grepl("1|4|5",pre_no_blanks[,12]))+
    as.numeric(grepl("4|5",pre_no_blanks[,15]))+as.numeric(grepl("4|5",pre_no_blanks[,16]))+as.numeric(grepl("2|4",pre_no_blanks[,24]))+
    as.numeric(grepl("3|5",pre_no_blanks[,22]))+as.numeric(grepl("4|5",pre_no_blanks[,27]))+as.numeric(grepl("3|5",pre_no_blanks[,29]))
)}

if (data_set == "fci") {uncommon_post <- as.matrix(
  as.numeric(grepl("2|3|4",post_no_blanks[,4]))+as.numeric(grepl("3|4|5",post_no_blanks[,6]))+as.numeric(grepl("1|4|5",post_no_blanks[,12]))+
    as.numeric(grepl("4|5",post_no_blanks[,15]))+as.numeric(grepl("4|5",post_no_blanks[,16]))+as.numeric(grepl("2|4",post_no_blanks[,24]))+
    as.numeric(grepl("3|5",post_no_blanks[,22]))+as.numeric(grepl("4|5",post_no_blanks[,27]))+as.numeric(grepl("3|5",post_no_blanks[,29]))
)}

#Count number chosen
Freq_uncommon_pre <- table(uncommon_pre)
Freq_uncommon_post <- table(uncommon_post)

uncommon_rows_pre3 <- which(uncommon_pre > 3)
uncommon_rows_post3 <- which(uncommon_post > 3)

if (data_set == "bema") {uncommon_rows_pre3 <- which(uncommon_pre > 2)}
if (data_set == "bema") {uncommon_rows_post3 <- which(uncommon_post > 2)}

nrows_unc <- length(uncommon_rows_pre3) + length(uncommon_rows_post3)

uncommon_rows_pre <- as.integer(rownames(pre_no_blanks)[uncommon_rows_pre3])
uncommon_rows_post <- as.integer(rownames(post_no_blanks)[uncommon_rows_post3])

#Determine number of tests not taken seriously with patterns
pre_rows_patt_clean <- sort(unique(as.integer(c(rownames(pre_no_blanks)[Rows_patterns_pre],rownames(pre_no_blanks)[Rows_letters_pre],rownames(pre_no_blanks)[Rows_50_zeros_pre],rownames(pre_no_blanks)[Rows_50_pre[,1]]))))
post_rows_patt_clean <- sort(unique(as.integer(c(rownames(post_no_blanks)[Rows_patterns_post],rownames(post_no_blanks)[Rows_letters_post],rownames(post_no_blanks)[Rows_50_zeros_post],rownames(post_no_blanks)[Rows_50_post[,1]]))))
nrows_pre_patt <- length(pre_rows_patt_clean)
nrows_post_patt <- length(post_rows_patt_clean)
nrows_patt <- nrows_pre_patt + nrows_post_patt

#Determine overlap of patterns and uncommon
table_pre_patt_unc <- as.data.frame(table(c(pre_rows_patt_clean,uncommon_rows_pre)))
table_post_patt_unc <- as.data.frame(table(c(post_rows_patt_clean,uncommon_rows_post)))

nrows_patt_unc_overlap <- sum(table_pre_patt_unc[,2] == 2) + sum(table_post_patt_unc[,2] == 2)

#Determine overlap of patterns and easy
table_pre_patt_easy <- as.data.frame(table(c(pre_rows_patt_clean,easy_rows_pre)))
table_post_patt_easy <- as.data.frame(table(c(post_rows_patt_clean,easy_rows_post)))

nrows_patt_easy_overlap <- sum(table_pre_patt_easy[,2] == 2) + sum(table_post_patt_easy[,2] == 2)

#Determine overlap of easy and uncommon
table_pre_easy_unc <- as.data.frame(table(c(easy_rows_pre,uncommon_rows_pre)))
table_post_easy_unc <- as.data.frame(table(c(easy_rows_post,uncommon_rows_post)))

nrows_easy_unc_overlap <- sum(table_pre_easy_unc[,2] == 2) + sum(table_post_easy_unc[,2] == 2)

#Determine overlap of all 3 tests
allrows_pre <- sort(c(pre_rows_patt_clean,easy_rows_pre,uncommon_rows_pre))
allrows_post <- sort(c(post_rows_patt_clean,easy_rows_post,uncommon_rows_post))
  
table_allrows_pre <- as.data.frame(table(allrows_pre))
table_allrows_post <- as.data.frame(table(allrows_post))

nrows_overlap <- sum(table_allrows_pre[,2] == 3) + sum(table_allrows_post[,2] == 3)

#Select only not serious test takers and make a new matrix
pre_overlap_clean <- allrows_pre[duplicated(allrows_pre)]
post_overlap_clean <- allrows_post[duplicated(allrows_post)]

pre_overlap_clean3 <- pre_overlap_clean[duplicated(pre_overlap_clean)]
post_overlap_clean3 <- post_overlap_clean[duplicated(post_overlap_clean)]

#Select not serious test takers using patterns and either uncommon or easy
pre_easy_unc <- sort(c(easy_rows_pre,uncommon_rows_pre))
post_easy_unc <- sort(c(easy_rows_post,uncommon_rows_post))

pre_easy_unc_clean <- pre_easy_unc[duplicated(pre_easy_unc)]
post_easy_unc_clean <- post_easy_unc[duplicated(post_easy_unc)]

pre_overlap_clean2 <- sort(unique(as.integer(c(pre_rows_patt_clean,pre_easy_unc_clean))))
post_overlap_clean2 <- sort(unique(as.integer(c(post_rows_patt_clean,post_easy_unc_clean))))

notserious_pre2 <- pre_clean[row.names(pre_clean) %in% pre_overlap_clean2,] 
notserious_post2 <- post_clean[row.names(post_clean) %in% post_overlap_clean2,] 
#NA rows are tests that had fewer than 1/3 of the answers so scoring removed them but not pattern seeking (no_blanks)
#Remove NA rows
notserious_pre2 <- notserious_pre2[complete.cases(notserious_pre2),]
notserious_post2 <- notserious_post2[complete.cases(notserious_post2),]
notserious_scores_pre2 <- rowSums(notserious_pre2)
notserious_scores_post2 <- rowSums(notserious_post2)

#caught by 2 out of 3 tests
notserious_pre <- pre_clean[row.names(pre_clean) %in% pre_overlap_clean,] 
notserious_post <- post_clean[row.names(post_clean) %in% post_overlap_clean,] 
#NA rows are tests that had fewer than 1/3 of the answers so scoring removed them but not pattern seeking (no_blanks)
#Remove NA rows
notserious_pre <- notserious_pre[complete.cases(notserious_pre),]
notserious_post <- notserious_post[complete.cases(notserious_post),]
notserious_scores_pre <- rowSums(notserious_pre)
notserious_scores_post <- rowSums(notserious_post)

#caught by all 3 tests
notserious_pre3 <- pre_clean[row.names(pre_clean) %in% pre_overlap_clean3,] 
notserious_post3 <- post_clean[row.names(post_clean) %in% post_overlap_clean3,] 
#NA rows are tests that had fewer than 1/3 of the answers so scoring removed them but not pattern seeking (no_blanks)
#Remove NA rows
notserious_pre3 <- notserious_pre3[complete.cases(notserious_pre3),]
notserious_post3 <- notserious_post3[complete.cases(notserious_post3),]
notserious_scores_pre3 <- rowSums(notserious_pre3)
notserious_scores_post3 <- rowSums(notserious_post3)

#create file with all processed data to export
scored.results <- rbind.fill.matrix("# of students",nrscored,"gain summary(min,1stq,median,mean,3rdq,max)",summary_gain,"CohenD",Cohen,"Means",Mean_Pre,Mean_Post,"SDs",SD_Pre,SD_Post,"Medians",Median_Pre,Median_Post,"gain 3 ways","<(post - pre)/(questions-pre)>",gain_1,"(<post> - <pre>)/(questions-<pre>) only people with pre and post",gain_2,"(<post> - <pre>)/(questions-<pre>) all people",gain_3,"Scores per question combined",score.col.combined,"Scores per question pre",score.col.pre,"Scores per question post",score.col.post)
freq_chioce <- list("combined",freq_choice_combined,"pre", freq_choice_pre,"post", freq_choice_post)
RawDataResults <- rbind(nrows_patt,nrows_patt_easy_overlap,nrows_easy,nrows_easy_unc_overlap,nrows_patt_unc_overlap,nrows_unc,nrows_overlap,Check_patterns,check_50_letters,Check_letters,check_50_zeros,nrows)
Uncommon_Answers <- rbind(Freq_uncommon_pre,Freq_uncommon_post)
AllScores <- c(na.omit(scored.data$prescoresClean),na.omit(scored.data$postscoresClean))
notserious_scores <- c("2 out of 3 tests",notserious_scores_pre,notserious_scores_post,"all 3",notserious_scores_pre3,notserious_scores_post3,"patterns and 1 other",notserious_scores_pre2,notserious_scores_post2)
#RawDataRows <- rbind.fill.matrix("Unique pre tests",pre_rows_clean,"Unique post tests",post_rows_clean,"pre","patterns",Rows_patterns_pre,"letters",Rows_letters_pre,"50%",Rows_50_zeros_pre,"post","patterns",Rows_patterns_post,"letters",Rows_letters_post,"50%",Rows_50_zeros_post)

#write file
if (data_set == "csem") {write.csv(scored.data, file="scoreddataCSEM.csv")}
if (data_set == "bema") {write.csv(scored.data, file="scoreddataBEMA.csv")}
if (data_set == "fci") {write.csv(scored.data, file="scoreddataFCI.csv")}

if (data_set == "csem") {write.csv(scored.results, file="scoredresultsCSEM.csv")}
if (data_set == "bema") {write.csv(scored.results, file="scoredresultsBEMA.csv")}
if (data_set == "fci") {write.csv(scored.results, file="scoredresultsFCI.csv")}

if (data_set == "csem") {write.csv(freq_chioce, file="CSEMAnswerDistribution.csv")}
if (data_set == "bema") {write.csv(freq_chioce, file="BEMAAnswerDistribution.csv")}
if (data_set == "fci") {write.csv(freq_chioce, file="FCIAnswerDistribution.csv")}

if (data_set == "csem") {write.csv(RawDataResults, file="RawDataResultsCSEM.csv")}
if (data_set == "bema") {write.csv(RawDataResults, file="RawDataResultsBEMA.csv")}
if (data_set == "fci") {write.csv(RawDataResults, file="RawDataResultsFCI.csv")}

if (data_set == "csem") {write.csv(Uncommon_Answers, file="UncommonAnswersCSEM.csv")}
if (data_set == "bema") {write.csv(Uncommon_Answers, file="UncommonAnswersBEMA.csv")}
if (data_set == "fci") {write.csv(Uncommon_Answers, file="UncommonAnswersFCI.csv")}

if (data_set == "csem") {write.csv(freq_easy[10:14], file="EasyAnswersCSEM.csv")}
if (data_set == "bema") {write.csv(freq_easy[10:14], file="EasyAnswersBEMA.csv")}
if (data_set == "fci") {write.csv(freq_easy[10:14], file="EasyAnswersFCI.csv")}

if (data_set == "csem") {write.csv(notserious_scores, file="NotseriousScoresCSEM.csv")}
if (data_set == "bema") {write.csv(notserious_scores, file="NotseriousScoresBEMA.csv")}
if (data_set == "fci") {write.csv(notserious_scores, file="NotseriousScoresFCI.csv")}

if (data_set == "csem") {write.csv(AllScores, file="AllScoresCSEM.csv")}
if (data_set == "bema") {write.csv(AllScores, file="AllScoresBEMA.csv")}
if (data_set == "fci") {write.csv(AllScores, file="AllScoresFCI.csv")}

if (data_set == "csem") {write.csv(pre_no_blanks, file="fixed_data_pre_CSEM.csv")}
if (data_set == "csem") {write.csv(post_no_blanks, file="fixed_data_post_CSEM.csv")}
if (data_set == "bema") {write.csv(pre_no_blanks, file="fixed_data_pre_bema.csv")}
if (data_set == "bema") {write.csv(post_no_blanks, file="fixed_data_post_bema.csv")}
if (data_set == "fci") {write.csv(pre_no_blanks, file="fixed_data_pre_FCI.csv")}
if (data_set == "fci") {write.csv(post_no_blanks, file="fixed_data_post_FCI.csv")}

#add question numbers for plotting
if (data_set == "csem") {questions = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32")}
if (data_set == "bema") {questions = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")}
if (data_set == "fci") {questions = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")}

text(barplot(score.col.combined, col = "red", space = 0.06, axis.lty = 1, offset = 1, xlim = c(0.5,m), 
             border=FALSE, las=2, xlab = "question index", ylab = "score(%)", 
             main = "Combined Score for each question", 
             names.arg=questions,
             ylim=c(0,100)), score.col.combined, paste("", signif(score.col.combined, digits = 3), "", sep=""),cex=0.8)

if (data_set == "csem") {dev.copy(png,'CSEMScores.png')}
if (data_set == "bema") {dev.copy(png,'BEMAScores.png')}
if (data_set == "fci") {dev.copy(png,'FCIScores.png')}
dev.off()