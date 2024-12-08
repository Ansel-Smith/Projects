# PURPOSE: determine frequency of cards in winning Blackjack hands
library(tidyverse)
library("ggpubr")
theme_set(theme_minimal())
source("blackjack_play.r")

n = 10000
carddata <- play(n)

#===============================================================================
# Frequency of card values in winning hands
winning_card_values <- cbind(carddata$face, carddata$win)
winning_card_values <- winning_card_values[which(winning_card_values[, 2] == 1), ]
colnames(winning_card_values) <- c("Faces", "Freqs")

wcvp <- ggplot(data = winning_card_values) +
    geom_col(mapping = aes(x = winning_card_values[, 1],
                y = winning_card_values[, 2], 
                fill = winning_card_values[, 1]), 
                show.legend = FALSE) +
    ggtitle("Frequencies of Winning Values") +
    xlab("Face Value") + ylab("Frequency") +
    coord_polar()
#===============================================================================

#===============================================================================
# Frequency of hand sizes in winning hands
hand_sizes <- cbind(carddata$game, carddata$win)
hand_sizes <- hand_sizes[which(hand_sizes[, 2] == 1), ]
game_num <- unique(hand_sizes[, 1])                            # winning games
game_size <- numeric(length(game_num))                       # size of hand in each winning game
for (i in seq_along(game_size)) {
    game_size[i] <- length(which(hand_sizes[, 1] == game_num[i]))
}
game_size <- data.frame(game_size)

gsp <- ggplot(data = game_size) +
    geom_bar(mapping = aes(x = game_size)) +
    ggtitle("Frequencies of Winning Hand Sizes") +
    xlab("Hand Size") + ylab("Frequency") +
    coord_flip()
#===============================================================================

#===============================================================================
# Frequency of winning starting hand sums
starting_hands <- carddata[which(carddata$start == 1), ]
starting_hands <- starting_hands[which(starting_hands$win == 1), ]

starting_hands_values <- cbind(starting_hands$value, starting_hands$game)

shsums <- c()
games <- unique(starting_hands_values[, 2])

for (i in seq_along(games)) {
    curr <- starting_hands_values[which(starting_hands_values[, 2] == games[i]), ]
    shsums <- c(shsums, sum(curr[, 1]))
}

shsums <- as.data.frame(shsums)

shp <- ggplot(data = shsums) +
    geom_bar(mapping = aes(x = shsums)) +
    ggtitle("Winning Starting Hand Sums") +
    xlab("Starting Hand Sum") + ylab("Frequency")
#===============================================================================

#===============================================================================
# Frequency of Losing Hand Sums
lstarting_hands <- carddata[which(carddata$start == 1), ]
losing_starting_hands <- lstarting_hands[which(lstarting_hands$win == 0), ]

losing_starting_hands_values <- cbind(losing_starting_hands$value, losing_starting_hands$game)

lshsums <- c()
lgames <- unique(losing_starting_hands_values[, 2])

for (i in seq_along(lgames)) {
    lcurr <- losing_starting_hands_values[which(losing_starting_hands_values[, 2] == lgames[i]), ]
    lshsums <- c(lshsums, sum(lcurr[, 1]))
}

lshsums <- as.data.frame(lshsums)

lshp <- ggplot(data = lshsums) +
    geom_bar(mapping = aes(x = lshsums)) +
    ggtitle("Losing Starting Hand Sums") +
    xlab("Losing Hand Sum") + ylab("Frequency") +
    scale_y_reverse()
#===============================================================================

ggexport(wcvp, gsp, shp, lshp, filename = "blackjackCardStats.png", ncol = 2, nrow = 2)
# system("xdg-open ./blackjackCardStats.png", wait = FALSE)