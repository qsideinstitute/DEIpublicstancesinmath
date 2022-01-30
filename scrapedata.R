# Load libraries
library(tidyverse)
library(rvest)
library(stringi)
library(readtext)
library(pdftools)

###############
# AMR members #
###############

# Read html data
page <- read_html("https://amathr.org/founding-members/")

# Scrape name
origname <- page %>%
  html_nodes(".elementor-element-8c45a14 p") %>%
  as.character() %>%
  str_remove("<p><span style=\\\"color\\: blue;\\\">") %>%
  str_remove("</span>") %>%
  str_remove("<p>") %>%
  str_remove("</p>") %>%
  str_split("<br>") %>%
  sapply(function(x) x[1]) %>%
  str_squish()

# Scrape affiliation and clean text
origaffiliation <-  page %>%
  html_nodes(".elementor-element-8c45a14 p") %>%
  html_text() %>%
  str_remove(origname) %>%
  str_squish()

# Clean some text
name <- origname %>%
  stri_trans_general("Latin-ASCII") %>%
  str_replace_all("\\-"," ") %>%
  str_replace_all("\\.","") %>%
  tolower()

# Store in data frame
amrdata <- data.frame(origname=origname, name=name, origaffiliation=origaffiliation, what="AMR Member")

##########################
# AMS letter signatories #
##########################

# Read in text of letters
amsletters <- readtext("amsletters.txt") %>%
  as.character() %>%
  str_replace_all("(?<=[:alpha:])\\.(?=[:alpha:])","") %>%
  str_squish()

# Create data structures
amsdata <- data.frame(name=NULL,instituion=NULL,letternumber=NULL)
amslettertext <- NULL

# Letter 1
letternumber <- 1
beginning <- "I am writing regarding the article"
end <- "for her letter."
name <- "Blake Winter"
origaffiliation <- "Medaille College"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 2
letternumber <- 2
beginning <- "I am writing in support"
end <- "Well done!"
name <- "George E. Andrews"
origaffiliation <- "American Mathematical Society"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 3
letternumber <- 3
beginning <- "I applaud your running"
end <- "fresh and useful in the field."
name <- "Mark Saul"
origaffiliation <- "American Institute of Mathematics"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 4
letternumber <- 4
beginning <- "I applaud Professor Abby"
end <- c("education or social justice.")
name <- c("Hung-Hsi Wu")
origaffiliation <- "University of California Berkeley"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 5
letternumber <- 5
beginning <- "I am appalled"
end <- "editorial practices at the Notices."
name <- "Alejandro Chavez-Dominguez"
origaffiliation <- "University of Oklahoma"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 6 = Letter A
letternumber <- 6
beginning <- "We are a group of concerned"
end <- "community and identity."
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
name <- readtext("letterasignatories.txt") %>%
  as.character() %>%
  str_split("\\n") %>%
  unlist() %>%
  str_extract("^.*?(?=,)") %>%
  stri_trans_general("Latin-ASCII") %>%
  str_squish()
origaffiliation <- readtext("letterasignatories.txt") %>%
  as.character() %>%
  str_split("\\n") %>%
  unlist() %>%
  str_extract("(?<=,).*$") %>%
  stri_trans_general("Latin-ASCII") %>%
  str_squish()
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 7 = Letter B
letternumber <- 7
beginning <- "We write with grave concerns"
end <- "no place in our community."
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
name <- readtext("letterbsignatories.txt") %>%
  as.character() %>%
  str_split("\\n") %>%
  unlist() %>%
  str_extract("^.*?(?=,)") %>%
  stri_trans_general("Latin-ASCII") %>%
  str_squish()
origaffiliation <- readtext("letterbsignatories.txt") %>%
  as.character() %>%
  str_split("\\n") %>%
  unlist() %>%
  str_extract("(?<=,).*$") %>%
  stri_trans_general("Latin-ASCII") %>%
  str_squish()
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 8
letternumber <- 8
beginning1 <- "There is a false equivalence"
end1 <- "equity in their hiring practices."
beginning2 <- "Thompson has opted to politicize"
end2 <- "that of my employer."
name <- "Xander Faber"
origaffiliation <- "IDA/Center for Computing Sciences"
search1 <- paste0(beginning1,"(.|\\n)+",end1)
letter1 <- str_extract(amsletters,search1)
search2 <- paste0(beginning2,"(.|\\n)+",end2)
letter2 <- str_extract(amsletters,search2)
letter <- paste(letter1,letter2)
letter <- letter %>%
  str_replace("Tenney1","Tenney") %>%
  str_replace("bar\\.2","bar\\.") %>%
  str_replace("practices\\.3","practices\\.") %>%
  str_replace("AMS\\.4","AMS\\.") %>%
  str_replace("differently\\.”5","differently\\.”")
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 9
letternumber <- 9
beginning <- "Abigail Thompson’s article which appears in"
end <- "at our peril."
name <- "Terrence Blackman"
origaffiliation <- "Medgar Evars College, CUNY"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 10
letternumber <- 10
beginning <- "Thank you for publishing the article"
end <- "civilized discussion on this topic."
name <- c("Iosef Polterovich","Leonid Polterovich")
origaffiliation <- c("Universite de Montreal","Tel Aviv University")
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 11
letternumber <- 11
beginning <- "I would like to express my gratitude for your courageous"
end <- "support and admiration."
name <- "Mark Levi"
origaffiliation <- "Pennsylvania State University"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 12
letternumber <- 12
beginning <- "The heated debate"
end <- "stereotyped language!"
name <- "Valentin Ovsienko"
origaffiliation <- "CNRS"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 13
letternumber <- 13
beginning <- "In my opinion, diversity is an important"
end <- "on a regular basis."
name <- "Gil Kalai"
origaffiliation <- "Hebrew University of Jerusalem"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 14 = Letter C
letternumber <- 14
beginning <- "In an essay in the"
end <- "on this very important matter."
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
name <- readtext("lettercsignatories.txt") %>%
  as.character() %>%
  str_replace_all("\\n"," ") %>%
  str_split(",") %>%
  unlist() %>%
  stri_trans_general("Latin-ASCII") %>%
  str_squish()
origaffiliation <- rep(NA,length(name))
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 15
letternumber <- 15
beginning <- "I applaud Abigail Thompson for her thought-provoking and brave essay"
end <- "benefit mentioned above."
name <- "Abhishek Saha"
origaffiliation <- "Queen Mary University of London"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 16
letternumber <- 16
beginning <- "I am writing to express my strong support"
end <- "academics at large."
name <- "Victor Vianu"
origaffiliation <- "University of California San Diego"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 17
letternumber <- 17
beginning <- "I was saddened to see the reaction"
end <- "express diverse opinions."
name <- "Shachar Lovett"
origaffiliation <- "University of California San Diego"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 18
letternumber <- 18
beginning <- "I was delighted to see"
end <- "continue on your mission."
name <- "Yannis Papakonstantinou"
origaffiliation <- "University of California San Diego"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 19
letternumber <- 19
beginning <- "Universities that want to value diversity are requiring diversity statements"
end <- "Would Jean Bourgain\\?"
name <- "Svetlana Jitomirskaya"
origaffiliation <- "University of California Irvine"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 20
letternumber <- 20
beginning <- "I read with interest the letters section"
end <- "when those on the outside object."
name <- "Louigi Addario-Berry"
origaffiliation <- "McGill University"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 21
letternumber <- 21
beginning <- "The recent essay by Prof"
end <- "point to those flaws."
name <- "Juan Gutierrez"
origaffiliation <- "University of Texas San Antonio"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 22
letternumber <- 22
beginning1 <- "I am writing to commend Dr"
end1 <- "earlier article by Robert"
beginning2 <- "Shibley"
end2 <- "over a short period of 5 years."
name <- "Eleftherios Gkioulekas"
origaffiliation <- "University of Texas Rio Grande Valley"
search1 <- paste0(beginning1,"(.|\\n)+",end1)
letter1 <- str_extract(amsletters,search1)
search2 <- paste0(beginning2,"(.|\\n)+",end2)
letter2 <- str_extract(amsletters,search2)
letter <- paste(letter1,letter2)
letter <- letter %>%
  str_replace("editorial1","editorial") %>%
  str_replace("rubric2","rubric") %>%
  str_replace("Gilley\\.3","Gilley\\.") %>%
  str_replace("Shibley,4","Shibley,") %>%
  str_replace("book5","book") %>%
  str_replace("speech6","speech") %>%
  str_replace("affiliations7","affiliations")
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 23
letternumber <- 23
beginning1 <- "The UC system could streamline the vetting process on Diversity and Inclusion"
end1 <- "fewer generals."
beginning2 <- "Kudos to Dr. Thompson for having the courage"
end2 <- "budding Berias."
name <- "Hal Schenck"
origaffiliation <- "Auburn University"
search1 <- paste0(beginning1,"(.|\\n)+",end1)
letter1 <- str_extract(amsletters,search1)
search2 <- paste0(beginning2,"(.|\\n)+",end2)
letter2 <- str_extract(amsletters,search2)
letter <- paste(letter1,letter2)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Letter 24
letternumber <- 24
beginning <- "I joined the Mathematics Department at UC Davis in 1966"
end <- "shares her attitude."
name <- "Washek F. Pfeffer"
origaffiliation <- "University of California Davis"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,origaffiliation=origaffiliation,letternumber=rep(letternumber,length(name))))

# Fix names
amsdata$origname <- amsdata$name
amsdata$name <- amsdata$origname %>%
  stri_trans_general("Latin-ASCII") %>%
  str_replace_all("\\-"," ") %>%
  str_replace_all("\\.","") %>%
  tolower()

# Convert letter number to something more informatitive
names(amsdata)[names(amsdata)=="letternumber"] <- "what"
amsdata$newwhat <- as.character(amsdata$what)
amsdata <- amsdata %>%
  mutate(newwhat = replace(newwhat, newwhat == "6", "A")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "7", "B")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "14", "C")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "1", "D")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "2", "E")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "3", "F")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "4", "G")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "5", "H")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "8", "I")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "9", "J")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "10", "K")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "11", "L")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "12", "M")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "13", "N")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "15", "O")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "16", "P")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "17", "Q")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "18", "R")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "19", "S")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "20", "T")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "21", "U")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "22", "V")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "23", "W")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "24", "X"))
amsdata$what <- amsdata$newwhat
amsdata$what <- paste("AMS Letter",amsdata$what)
amsdata <- amsdata %>% select(-newwhat)

##################
# Thompson Essay #
##################

# Get Thompson essay
amsessaytext <- pdf_text("https://www.ams.org/journals/notices/201911/rnoti-p1778.pdf") %>%
  as.character() %>%
  stri_trans_general("Latin-ASCII") %>%
  str_replace_all("\n"," ") %>%
  paste(collapse = " ") %>%
  str_squish()
beginning1 <- "This essay contains my opinions as an individual"
end1 <- "oaths of the 1950s"
beginning2 <- "were wrong\\. Whatever"
end2 <- "and not today\\."
search1 <- paste0(beginning1,"(.|\\n)+",end1)
letter1 <- str_extract(amsessaytext,search1)
search2 <- paste0(beginning2,"(.|\\n)+",end2)
letter2 <- str_extract(amsessaytext,search2)
amsessaytext <- paste(letter1,letter2)
abbyline <- data.frame(name="abigail thompson",origname="Abigail Thompson",origaffiliation="University of California Davis",what="AMS Essay")
amsdata <- rbind(amsdata,abbyline)
amsdata <- amsdata %>%
  mutate(origaffiliation = replace(origaffiliation, origaffiliation == "", NA)) %>%
  mutate(what = replace(what, what == "", NA))
 
#####################################
# Get California Letter Signatories #
#####################################

# Read raw data
page <- read_html("https://www.independent.org/news/article.asp?id=13658")

# Scrape name
origname <- page %>%
  html_nodes("#signatories_style p") %>%
  html_text() %>%
  str_extract("^[^,]*(?=,)")

# Scrape affiliation
origaffiliation <-  page %>%
  html_nodes("#signatories_style p") %>%
  html_text() %>%
  str_remove(paste0(origname,",")) %>%
  str_squish()

# Clean text
name <- origname %>%
  stri_trans_general("Latin-ASCII") %>%
  str_replace_all("\\-"," ") %>%
  str_replace_all("\\.","") %>%
  tolower()

# Create data frame
ca1data <- data.frame(origname=origname, name=name, origaffiliation=origaffiliation, what="California Letter 1")

# Get text of letter
ca1lettertext <- page %>%
  html_nodes("li , p") %>%
  html_text() %>%
  paste(collapse="\n")
beginning <- "California is on the verge"
end <- "this framework rejects."
search <- paste0(beginning,"(.|\\n)+",end)
ca1lettertext <- str_extract(ca1lettertext,search) %>%
  iconv(to='ASCII//TRANSLIT', sub='') %>%
  str_replace_all("\n"," ") %>%
  str_squish()

########################################
# Second California Letter Signatories #
########################################

# Read raw data
page <- read_html("https://sites.google.com/view/k12mathmatters/home")

# Scrape name
origname <- page %>%
  html_nodes("strong") %>%
  html_text()

# Scrape affiliation
origaffiliation <- page %>%
  html_nodes("p") %>%
  html_text() %>%
  str_subset(".+")
lastdrop <- which(origaffiliation=="(Affiliations are provided only for the purpose of identification)")
origaffiliation <- origaffiliation %>%
  tail(-lastdrop) %>%
  head(-1) %>%
  str_remove(paste0(origname,",")) %>%
  str_squish()

# Clean text
name <- origname %>%
  stri_trans_general("Latin-ASCII") %>%
  str_replace_all("\\-"," ") %>%
  str_replace_all("\\.","") %>%
  tolower() %>%
  str_squish()

# Store in data frame
ca2data <- data.frame(origname=origname, name=name, origaffiliation=origaffiliation, what="California Letter 2")

# Scrape full text of letter
ca2lettertext <- page %>%
  html_nodes("p") %>%
  html_text() %>%
  paste0(collapse=" ") %>%
  str_extract("We write to express.*needed for social mobility.") %>%
  iconv(to='ASCII//TRANSLIT', sub='') %>%
  str_squish()

###########################
# Combine Data And Export #
###########################

# Combine individual stances data into one data frame
# Create copy of origaffiliation that will be cleaned in OpenRefine
# We have already created a processed name column while preserving origname
# Name will be further cleaned in OpenRefine
# Put columns in nice order
# Give columns better names

data <- rbind(amrdata,amsdata,ca1data,ca2data)
data$affiliation <- data$origaffiliation
data <- data[,c("origname","name","origaffiliation","affiliation","what")]
names(data) <- c("rawname","name","rawaffiliation","affiliation","stance")
write.csv(data,file="individualstances.csv",row.names=FALSE)

# Now normalize name and affiliation in OpenRefine

#########################
# Export Stance Content #
#########################

# Make nice table of information for stances
stance <- unique(data$stance)
content <- rep(NA,length(stance))
content[1] <- "https://amathr.org"
content[2:25] <- amslettertext
content[26] <- amsessaytext
content[27] <- ca1lettertext
content[28] <- ca2lettertext
stancecontent <- data.frame(stance=stance,content=content)
stancecontent <- stancecontent[c(1,26,7,8,15,2:6,9:14,16:25,27:28),]
write.csv(stancecontent,"stancesfulltext.csv", row.names = FALSE)