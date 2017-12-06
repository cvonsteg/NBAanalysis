library('readr')
library('dplyr')
library('tidyr')
library('tibble')
library('ggplot2')


#### Load and Combine Raw Data
October <- read_csv('C:/Users/Tino/Desktop/gitProjects/NBA/NBA_1016.csv')
November <- read_csv('C:/Users/Tino/Desktop/gitProjects/NBA/NBA_1116.csv')
December <- read_csv('C:/Users/Tino/Desktop/gitProjects/NBA/NBA_1216.csv')
January <- read_csv('C:/Users/Tino/Desktop/gitProjects/NBA/NBA_0117.csv')
February <- read_csv('C:/Users/Tino/Desktop/gitProjects/NBA/NBA_0217.csv')
March <- read_csv('C:/Users/Tino/Desktop/gitProjects/NBA/NBA_0317.csv')
April <- read_csv('C:/Users/Tino/Desktop/gitProjects/NBA/NBA_0417.csv')

#Combine
regularSeason <- rbind(October, November, December, January, February, March, April)

#### Clean and Manipulate

# Adjust column names
colnames(regularSeason) <- tolower(c('Date','Start','Visitor','Pts_Visitor','Home','Pts_Home','X7','OT_Flag','Notes'))

# Split and extract date information
# remove commas from dates
regularSeason$date <- as.character(gsub(",", " ",as.matrix(regularSeason$date)))

# bind rogether list output of strsplit (removing double spaces)
split_dates <- do.call(rbind, strsplit(regularSeason$date, "  "))
# create weekday column
regularSeason$weekday = split_dates[,1]

# bind and paste the date portions of the split column
dt1 <- cbind(split_dates[,2],split_dates[,3])
dt2 <- paste(dt1[,1], dt1[,2])
regularSeason$date <- dt2

# convert to date format
regularSeason <- regularSeason %>%
        mutate(date = as.Date(date, format = "%b %d %Y"))

# create factors for dates
regularSeason$weekday <- factor(regularSeason$weekday, levels= c("Mon","Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


#### Create Main Data Frame

regularSeason <- regularSeason %>%
        select('date','weekday','start','visitor','home','pts_visitor','pts_home','ot_flag') %>%
        filter(!is.na(weekday)) %>%
# mutate to create indicator of winner and margin of victory        
        mutate(winner = ifelse(pts_visitor > pts_home, 'V', 'H'),
               margin = abs(pts_visitor - pts_home)) %>%
        arrange(date, start) %>%
# create a game id based on row #        
        rownames_to_column(var = 'gameID')


teams <- regularSeason %>%
        gather(home, visitor, key  = 'h_v', value = 'team' ) %>%
        mutate(pts = ifelse(h_v == 'home', pts_home, pts_visitor),
               logPts = log(pts),
               win = ifelse(winner == 'H' & h_v == 'home', 1, ifelse(winner == 'V' & h_v == 'visitor', 1, 0)),
               ptsAllowed = ifelse(win == 1, pts-margin, pts+margin),
               logPtsA = log(ptsAllowed)) %>%
        select(gameID, date, start, weekday, team, h_v, win, pts, ptsAllowed, margin, ot_flag, logPts, logPtsA) %>%
        arrange(date, start, h_v)

# pts scored vs pts allowed, by team
ggplot(teams) + geom_line(aes(x = date, y = pts), colour = 'midnightblue') + 
        geom_line(aes(x = date, y = ptsAllowed), colour = 'lightcoral') + 
        facet_wrap(~team) +
        theme_classic() +
        labs(title = 'Timeseries of Pts Scored vs Pts Allowed')

#distribution of scores per team
ggplot(teams) + geom_density(aes(pts), colour = 'midnightblue', fill = 'midnightblue', alpha = 0.1) + 
        geom_density(aes(ptsAllowed), colour = 'lightcoral', fill = 'lightcoral', alpha = 0.1) + 
        facet_wrap(~team) +
        theme_classic() +
        labs(title = 'Distribution of Scores by Team')


timeAnalysis <- teams %>%
        group_by(team, weekday) %>%
        summarise(weekdayGames = n(),
                weekdayWins = sum(win)) %>%
        ungroup() %>%
        group_by(team) %>%
        mutate(totalWins = sum(weekdayWins),
               dayWinPerc = (weekdayWins/weekdayGames)*100)

# Monday Blues?
ggplot(teams, aes(x = weekday, y = win, fill = weekday, alpha = 0.1)) + 
        geom_col() + 
        scale_fill_brewer(palette = 'Blues') + 
        facet_wrap(~ team) + 
        labs(title = "Wins by Weekday")+
        theme_classic()

# comparing team win percentages by day to visually identify best/worst matchups #
ggplot(timeAnalysis, aes(x = team, y = dayWinPerc, fill = dayWinPerc)) + 
        geom_bar(stat = 'identity') +
        scale_fill_continuous() +
        coord_flip() + 
        facet_grid(.~weekday) + 
        theme_classic()

ggplot(timeAnalysis, aes(x = weekday, y = dayWinPerc, fill = weekday)) + 
        geom_bar(stat = 'identity') + 
        facet_wrap(~team) + 
        theme_classic()

ggplot(timeAnalysis, aes(x = team, y = dayWinPerc, fill = dayWinPerc)) + 
        geom_bar(stat = 'identity') + 
        coord_flip() + 
        scale_fill_gradient(low = 'green', high = 'blue') +
        facet_grid(.~ weekday)

# From here can we determine dream and nightmare matchup e.g. Phoenix > Nightmare = Clippers on a Monday
# this should be a combination of overall likelyhood of winning AND relative best/worst day

teamStats <- teams %>%
        group_by(team) %>%
        summarise(total_games = n_distinct(gameID),
                  wins = sum(win),
                  winRatio = wins/total_games,
                  total_pts = sum(pts),
                  ppg = mean(pts),
                  logPpg = log(ppg),
                  offVol = sd(pts),
                  ppgA = mean(ptsAllowed),
                  logPpgA = log(ppgA),
                  defVol = sd(ptsAllowed),
                  ptsRatio = ppg/ppgA)
                   
# winratio
# ggplot(teamStats, aes(x = team, y = winRatio), colour = team) + geom_bar(stat = 'identity') + theme_classic()
# ggplot(teamStats, aes(winRatio)) + geom_histogram(bins = 50) + geom_density() + theme_classic()
# Distribution of team scores
ggplot(teamStats, aes(total_pts)) + geom_density(fill = 'blue') + labs(title = 'Distribution of PPG')

# offVol vs defVol
ggplot(teamStats) + geom_point(aes(x = team, y = offVol), colour = 'midnightblue') + 
        geom_point(aes(x = team, y = defVol), colour = 'lightcoral') + 
        labs(title = 'Volatility per Team', x = 'Team', y = 'Volatility') + 
        theme_minimal()

# CAN WE FIND ANY CORRELATION BETWEEN OFFENSIVE VOLATILITY AND PERFORMANCE VS DEFENSIVE VOLATITLIY AND PERFORMANCE?!

# Volatility vs wins4
ggplot(teamStats) + 
        geom_point(aes(x = offVol, y = wins), colour = 'midnightblue') + 
        geom_point(aes(x = defVol, y = wins), colour = 'lightcoral') + 
        labs(x = 'Volatility', y = 'Win Count')


# Regression plot of ppg vs wins
ggplot(teamStats, aes(x = ppg, y = wins, colour = team) ) + geom_point() + geom_smooth(method = lm, colour = 'blue')

# Regression plot of ptsRatio vs winRatio
ggplot(teamStats, aes(ptsRatio, winRatio, colour = team)) + geom_jitter() + geom_smooth(method = lm, colour = "black")
# can find volatile vs consistent performers - volatile winRatoio > 0.5 & ptsRatio < 1
ggplot(teamStats, aes(ptsRatio, winRatio)) + 
        geom_rect(data = NULL, aes(xmin = 1, xmax = 1.12, ymin = 0.2, ymax = 0.5), colour = 'red', fill = 'lightcoral', alpha = 0.1) +
        # geom_rect(data = NULL, aes(xmin = 0.92, xmax = 1, ymin = 0.1, ymax = 0.5), colour = 'lightgreen', fill = 'lightgreen') +
        # geom_rect(data = NULL, aes(xmin = 1, xmax = 1.12, ymin = 0.5, ymax = 0.9), colour = 'lightgreen', fill = 'lightgreen') +
        geom_rect(data = NULL, aes(xmin = 0.925, xmax = 1.0, ymin = 0.5, ymax = 0.9), colour = 'red', fill = 'lightcoral', alpha = 0.1) +
        geom_point() + 
        geom_smooth(method = lm, colour = "black") 

ggplot(regularSeason, aes(weekday, fill = winner)) + geom_bar() + geom_line(aes(x = weekday, y = mean(margin)))


basicWinsSummary <- regularSeason %>% 
        group_by(winner) %>% 
        summarise(wins = n(), 
                  avg_margin = mean(margin), 
                  avg_home = mean(pts_home), 
                  avg_away = mean(pts_visitor))

# Home team advantage appears to be very much real, and also more prominent as evidenced #
# by the bigger win margin for home teams #
#Conduct statistical test! #


daySummary <- regularSeason %>%
        group_by(weekday) %>%
        summarise(games = n())
