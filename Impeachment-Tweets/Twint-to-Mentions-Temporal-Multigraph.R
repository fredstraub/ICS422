######################################################################
# Converting Twint data into User-Mentions-User igraph 
# 
# Given .csv as output by the Twint data gathering tool, parse the 
# 'mentions' column and generate a user-user directed multigraph 
# where edges go from the tweeter to the mentioned account, and are
# annotated with UTC time of tweet in E(g)$utc_datetime and the text
# content of the tweet in E(g)$tweet. 
# 
# Sat Feb 15 2020 Dan Suthers Initialized by copy from related demos 
# * Initial read of full Impeachment data 
# * Conversion of date and time to datetime 
# Sun Feb 16 2020 DS Replacing nonprinting characters
# Tue Feb 18 2020 DS Ditched as HW assignment but using for class
#  demo with smaller data file. With that, this is a HW 2 solution! 
#  Did some cleanup to show it as such. It also shows methods for HW 3. 
# Wed Feb 19 2020 DS Resolved time zone issues. Convert to UTC and 
#  save as character to be human readable. 
# Thu Feb 20 2020 DS Final version to create .graphml for class use 
# Fri Feb 21 2020 DS Changed time field to 'utc_datetime' to make it
#   clearer. Re-ran on full data set to apply this and verify it works. 
# Apr 29 2022 DS Major revisions for ICS 422/622 Spring 2022
# 
######################################################################
# Setup 

library(tidyverse)
# library(igraph) # Later when needed

base_dir <- "~/Desktop/Network-Science-Demos" # Set to yours.
setwd(base_dir) # for default 

# Everything specific to the analysis is here. Check contents. 

(analysis_dir <- paste0(base_dir, "/Impeachment-Tweets"))
dir(analysis_dir)

# Put Twint-Impeachment-200116-200205.csv, a 217MB file, in this directory.
# Also Twint-Impeachment-200122-24.csv a smaller 32MB example for class.

(data_dir <- paste0(analysis_dir, "/Data"))
dir(data_dir)

# Pick one 
# data_csv <- "Twint-Impeachment-200116-200205.csv" # Full data
data_csv <- "Twint-Impeachment-200122-24.csv" # Smaller example 

# Utility functions 

(utility_dir <- paste0(analysis_dir, "/Utility"))
dir(utility_dir)

# Output desired 
# mentions_filename <- "Impeach_Mention_Graph_Multi-Jan-16-Feb-05"

mentions_filename <- "Impeach_Mention_Graph_Multi-Jan-22-24"

######################################################################
# Selective Reading of the Table 
######################################################################

tweets <- 
  read_delim(paste0(data_dir, "/", data_csv), 
             ",", 
             col_names = TRUE,
             col_types = cols_only(
               id = col_character (),             # safer for id (was double)
               conversation_id = col_character(), # safer for id
               created_at = col_skip(),           # using date and time 
               date = col_date(format = ""),
               time = col_time(format = ""),
               timezone = col_character(),
               user_id = col_character(),   # safer for id
               username = col_character(),
               name = col_character(),
               place = col_skip(),          # only a few sparse values
               tweet = col_character(),
               mentions = col_character(),  # list to parse
               urls = col_character(),      # list to parse
               photos = col_character(),    # list to parse 
               # replies_count = col_double(),  # Not currently using
               # retweets_count = col_double(), # Not currently using 
               # likes_count = col_double(),    # Not currently using
               hashtags = col_character(),  # list to parse
               cashtags = col_character(),  # list to parse
               link = col_character(),
               retweet = col_skip(),        # all are FALSE
               quote_url = col_character(),
               video = col_double(),
               near = col_skip(),           # all NA
               geo = col_skip(),            # all NA
               source = col_skip(),         # all NA
               user_rt_id = col_skip(),     # all NA
               user_rt = col_skip(),        # all NA
               retweet_id = col_skip(),     # all NA
               reply_to = col_character(), 
               retweet_date = col_skip(),   # all NA
               translate = col_skip(),      # all NA
               trans_src = col_skip(),      # all NA 
               trans_dest = col_skip()      # all NA
             )
  )

view(head(tweets, 100))

######################################################################
# Data Conversions 
######################################################################
# Dates 

library(lubridate)

# ----------------------
# Method demo (can skip)

# Data looks like this 

(d <- tweets$date[1:10])
(t <- tweets$time[1:10])
(z <- tweets$timezone[1:10])

# We want to combine them for ymd-hms. Easy: 
(dt <- paste(d, t, z))

# Twint specified that this was in HST, but the lubridate
# parser ignores the suffix and parses to UTC 
?ymd_hms
ymd_hms(dt)

# We can force to HST, but it does not change the time.
ymd_hms(dt, tz = "HST")

# Instead, force to UTC and always work in UTC. 
?with_tz
(udt <- with_tz(ymd_hms(dt, tz = "HST"), tzone="UTC"))

# But we want to save as character to be human readable and to avoid
# problems when numeric is saved and read as double.

class(udt)
udt <- as.character(udt)
class(udt)
udt 

# It does not say that it is UTC, but we will assume this from now on. 

# --------------------------
# Applying that to our data: 

# Twint specified that dates and times are in HST, but the lubridate
# ymd_hms parser ignores the suffix and parses to UTC. So, after
# parsing date+time to HST, do conversion to UTC. Save as character
# for human readable output and avoiding problems of double. Call it 
# utc_datetime to be clear. 

unique(tweets$timezone) # just checking that all are HST 

?magrittr # in case you have not seen this before 

tweets$utc_datetime <- 
  paste(tweets$date, tweets$time, tweets$timezone) %>% 
  ymd_hms(tz = "HST") %>% 
  with_tz(tzone = "UTC") %>% 
  as.character()

head(tweets$utc_datetime)
class(head(tweets$utc_datetime))

# -------------------------------------------------------
# Get rid of characters that can't be written in XML 1.0

# From http://www.firstobject.com/control-characters-in-xml.htm
# Control Characters in XML
# In its infinite wisdom the XML 1.0 standard excluded the control
# characters in the range 0x01 to 0x1f except whitespace 0x09, 0x0a,
# 0x0d, even in escaped form. This was reversed in XML 1.1 but it was
# too late.
# DS: in particular, write_graph graphml format uses XML 1.0 

head(tweets$name, 10) # note lines 5 and 9 
tweets$name <- str_replace_all(tweets$name, 
                               '[^A-Za-z0-9\ -_@#$%&*?!]', 
                               "-")
head(tweets$name, 10)

# --------------
# Check Results 

view(head(tweets, 100))

######################################################################
# Building Mentions Edge Table
# Doing this first so we know all users that are mentioned. 
######################################################################
# Parsing out mentions and make a row for each: more complex. 

# Extract lists of mentioned users from the string representation. 
# Unnest into separate rows, filter NA , and select desired columns.

# ****************************************************************
# ***** This takes a VERY LONG TIME with the full data set:  *****
# ***** 56 minutes. About 1.5 minutes with Jan 22-24 data.   *****
# ***** (Those were times on a 2013 machine: faster now.)    *****
# ****************************************************************

date()
mention_edges <- 
  tweets %>% 
  # Extract lists of mentioned users from the string representation.
  mutate(mentioned_user = str_extract_all(tweets$mentions, 
                                          boundary("word"))) %>%
  # Unnest each mention into its own row and filter NA to 
  # drop tweets that don't mention anyone 
  unnest_longer(mentioned_user) %>%
  drop_na(mentioned_user) %>% 
  # Count number of mentions per tweet
  group_by(id)  %>%
  add_tally(name = "mention_count")  %>%
  ungroup() %>%
  # Adjust edge weight to be the inverse of mention_count
  mutate(weight = 1/mention_count) %>% 
  # Make an edge list appropriate for graph_from_data_frame:
  # Source, target, weight, and other attributes. 
  select(Source = username, Target = mentioned_user, weight, 
         # My production code keeps other attributes omitted for demo
         utc_datetime, tweet)  %>% 
  # Remove self-mention edges: not of interest. This will cause
  # some nodes to become isolates, and be filtered later. 
  filter(Source != Target)
date()

# Wider console or use view 
mention_edges

######################################################################
# Building Nodes Table
######################################################################

# Count number of tweets per actor for potential filtering purposes.
# Then select relevant information for each username. 
# We need to rename to match requirements of graph_from_data_frame
# (first column is name) and Gephi ('label' becomes displayed Label). 

active_users <-
  tweets %>% 
  # Count number of tweets per actor 
  group_by(username) %>%  
  add_tally(name = "tweet_count") %>% 
  ungroup() %>%
  select(name=username, user_id, label = name, tweet_count)
active_users

# -------------------
# Removing Duplicates 
# Make the username + name pair unique (Multiple tweets will duplicate)

length(active_users$name)
active_users <- unique(active_users)
length(active_users$name)

# However, if a user changed their name there could be duplicate
# usernames, which causes problems when used as a vertex list

length(unique(active_users$name))

# If those last two numbers were not identical, choose arbitrarily:

active_users <- active_users[!duplicated(active_users$name),]
length(active_users$name)

# --------------
# Missing nodes 

# Some mentioned users may not have tweeted 

missing_nodes <- setdiff(unique(mention_edges$Target), active_users$name)
length(missing_nodes)

add_users <- tibble(name = missing_nodes, 
                    user_id = 0, 
                    label = missing_nodes,
                    tweet_count = 0)
all_nodes <- unique(rbind(active_users, add_users))
length(all_nodes$name)

######################################################################
# Making the Graph 
######################################################################
# Now it's OK to mask tidyverse 

library(igraph)
source(paste0(utility_dir, "/remove_isolates.R"))

mention_graph <- graph_from_data_frame(mention_edges, vertices=all_nodes)
# Make the name of the graph from the name of the file
mention_graph$name <- str_replace_all(str_sub(as.character(mentions_graphml), 
                                              end=-9L),
                                      "[_-]", " ")
summary(mention_graph)

######################################################################
# Filtering the Network
######################################################################
# Removing isolates: 
# those who tweeted without mentioning or being mentioned. 

head(degree_distribution(mention_graph)) # 15.6% isolates
mention_graph <- remove_isolates(mention_graph)
head(degree_distribution(mention_graph))
vcount(mention_graph)

# --------------
# Filtering Bots
# Nodes with tweet_count > 1000/day are obviously bots and can be
# excluded. But we should check all of the top frequency tweeters.
tail(table(V(mention_graph)$tweet_count), 10)

# Find out more about the top tweeters. 
tt <- tibble(
  vid = V(mention_graph)[which(V(mention_graph)$tweet_count 
                               %in%
                                 as.numeric(names(tail(table(V(mention_graph)$tweet_count), 
                                                       10))))]$name, 
  tc  = V(mention_graph)[which(V(mention_graph)$tweet_count 
                               %in%
                                 as.numeric(names(tail(table(V(mention_graph)$tweet_count), 
                                                       10))))]$tweet_count
)
(tt <- arrange(tt, desc(tc)))

# Some top accounts (from both 3 day and full data) 
# ***** This is for full data set and will differ for the 3-day data ***** 
# @openletterbot - Resistbot Open Letters - a bot but it passes on other's letters!
#   Copies of real letters delivered to the President, Congress,
#   Governors, and State Legislators. Send 'resist' as a Direct Message
#   to @resistbot to write yours.
# @ampoliticalsite - Account suspended. Will look at tweets. 
# @impeach_45now - Latina 4 Justice - a real account 
# @stuinsd - Constitutional conservative; retired Navy captain. - real account 
# @impeachresist1 - THE RESISTANCE   I do not answer PMs NO DMs - real account 
# @thomasjames147 - Thomas James - I HAVE A STASH OF TOILET PAPER - real account (unfortunately)
# @impeachbdevos - IamSauerKraut - real account 
# @freddyatton -  ((Fredàé)) #Resist #VoteBlue #Survive - real account 
# @axelrod_ej - real account 
# @realpius_a_has - I Write About God - real account 
# @impeach_cheeto - She_Persisted - real account 
# @cheeto_impeach - real account
# @glitchrekal - Lover of God, Married, & #1 Mastermind [Milkyway Galaxy Origins] - real account
# @impeach_today - Socially Distant Peach - real account
# @impeach_him_pls - real account
# @kovarikpenny - Penny Kovarik - real account

# Which of these merit deletion? We can discuss, but all but the top
# two are legitimate accounts. Let's look at tweets for those top two.

olb <- mention_edges %>% 
  filter(Source == "openletterbot") %>% 
  select(tweet) %>% 
  unique 

# Make the console wide for this 

olb
# Does not seem appropriate for our purposes. Content not useful. 

amp <- mention_edges %>% 
  filter(Source == "ampoliticalsite") %>% 
  select(tweet) %>% 
  unique 
amp
# This appears to be a RT bot, not contributing its own content. 

# Identifying nodes to delete by name so I can be deliberate 
delete_vids <- 
  union(V(mention_graph)[V(mention_graph)$name == "openletterbot"], 
        V(mention_graph)[V(mention_graph)$name == "ampoliticalsite"])
delete_vids

# When we delete vertices we need to delete newly isolated nodes
summary(mention_graph)
mention_graph <- remove_isolates(delete_vertices(mention_graph, delete_vids))
mention_graph$name <- paste(mention_graph$name, "Filtered Bots")
mention_graph$filter_openletter_bot <- TRUE 
summary(mention_graph)

# -------------------------
# Filtering Mention Spammers 
# Find number of actors a given actor mentions per tweet on average,
# divide mentions by number of tweets. These may be bots if high
# values, but I have not yet checked them. Regardless, I am deleting
# mention spammers as these mentions are not likely to be deliberately
# considered.

avg_mpt <- degree(mention_graph, mode="out") / V(mention_graph)$tweet_count
plot(table(avg_mpt))
plot(head(table(avg_mpt), 200)) # adjust to focus on where it drops off

# Above 9 or 10 we are clear of the main group. 

mention_spammers <- V(mention_graph)[which(avg_mpt > 9)] 
length(mention_spammers)
head(mention_spammers, 30)

# Remove them as follows. 

summary(mention_graph)
mention_graph <- remove_isolates(delete_vertices(mention_graph, 
                                                 mention_spammers))
mention_graph$filter_mention_spammers <- TRUE # record its status
summary(mention_graph)
plot(table(degree(mention_graph, mode="out") / V(mention_graph)$tweet_count))


######################################################################
# Writing Network Files 
######################################################################

# Graphml is slow but may be needed for moving to other tools 
write_graph(mention_graph, 
            paste0(analysis_dir, "/", mentions_filename, ".graphml"),
            format="graphml")

# Save in binary representation: much faster to read in
save(mention_graph, 
     file = paste0(analysis_dir, "/", mentions_filename, ".rdata"))

######################################################################
# What about conversation_id? 
# https://blog.twitter.com/engineering/en_us/topics/infrastructure/2017/building-and-serving-conversations-on-twitter.html 


######################################################################
# Pau 