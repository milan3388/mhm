

#packages
library(tm)
library(gofastr)
library(ggthemes)
library(ggrepel)
library(wordcloud)


#----------------- Load Data-----------------------------------
#data was extracted after running the code 5.1 ----------------

#ZPInfo <- read.csv("ZPInfo.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#ZPInfo$X <- NULL
#ZPInfo_QS <- ZPInfo[BearbeitSt == "QS"]
#ZPInfo_QS <- ZPInfo[betrBereich == "AKTIONEN"]
#prob_text <- ZPInfo_QS[, c("Problemtext","FIN")]
#or---
problem_text <- ZPInfo[BearbeitSt == "QS" | betrBereich == "AKTIONEN"]
#prob_text <- ZPInfo[BearbeitSt == "QS" & betrBereich == "AKTIONEN"]
problem_text <- problem_text[, c("Problemtext","FIN")]
prob_text <- problem_text
View(problem_text)



#--------------data cleaning & preprocessing---------------------


# Get rid of all punctuation from text
prob_text$Problemtext <- gsub("([<>])|[[:punct:]]", "\\1", prob_text$Problemtext)


# Get rid of all numbers from text
prob_text$Problemtext <- gsub("[0-9]+","", prob_text$Problemtext)

# Get rid of '>' symbol from text
prob_text$Problemtext <- gsub(">","", prob_text$Problemtext)

#convert all text to lower case
prob_text$Problemtext <- tolower(prob_text$Problemtext)

#remove all german stopwords from texts
stopwords("german")
prob_text$Problemtext <- removeWords(prob_text$Problemtext, stopwords("german"))

#get rid of all single & unnecessary letters
prob_text$Problemtext <- gsub(pattern = "\\b[a-z]\\b{1}", replace="", prob_text$Problemtext)
prob_text$Problemtext <- gsub(" .. ", "", prob_text$Problemtext)
prob_text$Problemtext <- gsub(" ..$", "", prob_text$Problemtext)
#paste(Filter(function(x) nchar(x) > 2, unlist(strsplit(prob_text$Problemtext, " "))), collapse = " ")


#get rid of all extra spaces from text
prob_text$Problemtext <- stripWhitespace(prob_text$Problemtext)
prob_text$Problemtext <- trimws(gsub("\\s+", " ", prob_text$Problemtext))





#to check what we did
pt_comparing <- mutate(problem_text, cleaned_text=prob_text$Problemtext)
pt_comparing <- pt_comparing[, -2]
View(pt_comparing)



## wordcloud 
wordcloud(problem_text$Problemtext, random.order = TRUE, max.words = 200, scale = c(3,0.7), colors = rainbow(3), rot.per = 0.3)
pt <- prob_text %>% group_by(Problemtext)%>% summarise(total_problem = sum(length(Problemtext)), total_vehicle = sum(length(unique(FIN)))) %>% arrange(desc(total_problem))
View(pt)
wordcloud(pt$Problemtext, pt$total_problem, min.freq = 20, scale = c(3,0.7), random.order =FALSE, colors = rainbow(8), rot.per = 0.3)

 #-----------------------------------------------------------------------
#replaceing some letters of few texts due to add them into same category
#prob_text$Problemtext <- gsub("auslieferung", "auslieferfreigabe", prob_text$Problemtext)


#--------------descriptive analysis------------------------

# store the indices of almost same texts in an object named 'idx1' 
#idx <- agrep(pattern = "auslieferfreigabegeserrt", x = prob_text$Problemtext, ignore.case = TRUE, value = FALSE, max.distance = 3)
#idx
#length(idx)

#looking at those rows in the data that matched
#prob_text$Problemtext[idx]

# replacing those text that matches 
#prob_text$Problemtext[idx] <- "auslieferfreigabe gesperrt"
#prob_text$Problemtext[idx]


# let's do the same process for 'idx2'
#idx2 <- agrep(pattern = "wasserprobe entnehmen", x = prob_text$Problemtext, ignore.case = TRUE, value = FALSE, max.distance = 3)
#idx2
#length(idx2)
#prob_text$Problemtext[idx2]

#prob_text$Problemtext[idx2] <- "wasserprobe entnehmen"
#prob_text$Problemtext[idx2]






#getting the unique problems and their total occurence numbers 
length(unique(prob_text$Problemtext))
View(prob_text)
length(which(prob_text$Problemtext=="mmo erledigt"))
which(prob_text$Problemtext=="mmo erledigt")
View(prob_text[which(prob_text$Problemtext=="mmo erledigt"),])
length(unique(prob_text[which(prob_text$Problemtext=="mmo erledigt"),]$FIN))


pt <- prob_text %>% group_by(Problemtext)%>% summarise(total_problem = sum(length(Problemtext)), total_vehicle = sum(length(unique(FIN)))) %>% arrange(desc(total_problem))
View(pt)

#Average number of vehicles per problem_text
sum(pt$total_vehicle)/length(pt$Problemtext)
mean(pt$total_vehicle)

#Average number of problems per vehicle
problems_per_vehicle <- prob_text %>% group_by(FIN) %>% summarise(occured_problems=sum(length(Problemtext)))
View(problems_per_vehicle)
View(prob_text[FIN==41000113112892,])
mean(problems_per_vehicle$occured_problems)


#removing that entry which is total empty
which(pt$Problemtext=="")
pt <- pt[-(which(pt$Problemtext=="")),]



#Scatter plot of top 10 problems---
top_10_prob <- pt %>% arrange(desc(total_problem)) %>% top_n(10)
p <- top_10_prob %>% ggplot(aes(total_problem, total_vehicle, label=Problemtext))
scatter_plot <- p+geom_text_repel(aes(col=Problemtext),nudge_x = 0.02, show.legend = FALSE)+xlab("Total occurence number of each problem")+ylab("Number of Vehicles")+ggtitle("Visualization of top 10 problems")
scatter_plot+geom_point(size=3)+theme_economist()

#------------------------
#filtering problems which occured more than 100 times
#filtered_prob_text <- pt %>% filter(total_problem>=100) %>% arrange(desc(total_problem))

#sum(filtered_prob_text$total_problem)
#sum(pt$total_problem)
#(sum(filtered_prob_text$total_problem)/length(prob_text$Problemtext))*100



#--------------Data Visualization-------------------
# density plot---
#filtered_prob_text %>% ggplot(aes(x=total_problem))+geom_density(fill="light blue")


#Scatter plot of major problems which occured atleast 100 times---
#p <- filtered_prob_text %>% ggplot(aes(total_problem, total_vehicle, label=Problemtext))
#scatter_plot <- p+geom_text_repel(aes(col=Problemtext),nudge_x = 0.15, show.legend = FALSE)+scale_x_log10()+scale_y_log10()+xlab("Total occurence number of each problem(log scale)")+ylab("Number of Vehicles(log scale)")+ggtitle("Visualization of major problems")
#scatter_plot+geom_point(size=3)+theme_economist()




#making corpus & document term matrix....
#corpus <- Corpus(VectorSource(prob_text$Problemtext))
#dtm <- DocumentTermMatrix(corpus)
#m <- as.matrix(dtm)

#finding the most frequent terms
#frequency <- colSums(dtm2)
#frequency <- sort(frequency, decreasing = TRUE)
#head(frequency, 20)




########################################

# store the indices of all texts contain the word 'fehlteil' 
fehlteil <- agrep(pattern = "fehlteil", x = prob_text$Problemtext, ignore.case = TRUE, value = FALSE, max.distance = 3)
fehlteil
length(fehlteil)

#looking at those rows in the data that matched
ZPInfo$Problemtext[fehlteil]
#checking data for Bereich MB03
ZPInfo$Problemtext[]




# defining for loop to make groups of similar words
x <- pt$Problemtext[-c(1,73)]
lst <- list()
for (i in x) {
  if(nchar(i)>7){
    v <- agrep(i, x, value = TRUE, max.distance = list(sub= 3))
    if(length(v)>1){
      lst[[i]] <- v
      x <- x[!x %in% v]
    }
  }
}

View(lst)
View(lst[[7]])
View(lst[["fehlteil"]])


 
# defining for loop to get indexes of similar words
x <- pt$Problemtext[-c(1,73)]
ndx <- list()
for (i in x) {
  if(nchar(i)>7){
    v <- agrep(i, x, value = FALSE, max.distance = list(sub= 3))
    if(length(v)>2){
      ndx[[i]] <- v
      #x <- x[!x %in% x[v]]
    }
  }
}
View(ndx)
ndx[["nachfahren"]]
pt <- pt[-c(1,73),]
View(pt[ndx[["fehlteil"]],])



View(pt$Problemtext[lst[[1]]])

dt <- as.data.table(lst)
View(dt)
v1 <- unlist(lst[[1]])

# using LDA algorithm--------------------


# making corpus of problem text
corp <- VCorpus(VectorSource(p_text$Problemtext))

# making document term matrix of corpus
dtm <- DocumentTermMatrix(corp)
inspect(dtm)

# finding frequent terms
findFreqTerms(dtm, 500)

library(topicmodels)
topic_matrix <- LDA(dtm, k=10)
topic_matrix
