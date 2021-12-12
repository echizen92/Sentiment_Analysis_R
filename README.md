# Sentiment_Analysis_R
Performing sentiment analysis using R

Take note of the libraries used:

```
library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(rtweet)
library(plyr)
library(stringr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(dplyr)  
library(hms)
library(lubridate) 
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)
```

Set up authorization to extract tweets by providing the following:

```
api_key <- "redacted"
api_secret_key <- "redacted"
access_token <- "redacted"
access_token_secret <- "redacted"
```
More information with regards to authorisation for twiter can be found in the following package:
```
#vignette("auth", package = "rtweet")
```

Download sentiment score using "opinion lexicon" from http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar

Place the text.file in working directory in set R

You can add additional words if it missing from the txt file

```

positive = scan('positive-words.txt', what = 'character', comment.char = ';')
negative = scan('negative-words.txt', what = 'character', comment.char = ';')

# add your list of words below as you wish if missing in above read lists
pos.words = c(positive,'upgrade','Congrats','prizes','prize','thanks','thnx',
             'Grt','gr8','plz','trending','recovering','brainstorm','leader')
neg.words = c(negative,'wtf','wait','waiting','epicfail','Fight','fighting',
              'arrest','no','not')
```


