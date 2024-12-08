# Blackjack Game Simulation
# assume 1 player draws until they get 21 or bust
deck <- read.csv("../csvs/blackjackdeck.csv")

play <- function(n) {
    hands <- data.frame(c())                                                    # collection of your hands for each game

    for (i in 1:n) {
        tmpdeck <- deck                                                         # deck to be edited with draws
        handsum <- 0                                                            # current value of hand

        # starting hand: draw two cards
        card <- tmpdeck[sample(rownames(tmpdeck), 1), ]                        # draw two random cards from tmpdeck, store as table
        tmpdeck <- tmpdeck[-which(rownames(tmpdeck) == (rownames(card))), ]    # remove drawn cards from deck
        hand <- card
        card <- tmpdeck[sample(rownames(tmpdeck), 1), ]                        # draw two random cards from tmpdeck, store as table
        tmpdeck <- tmpdeck[-which(rownames(tmpdeck) == (rownames(card))), ]    # remove drawn cards from deck
        hand <- rbind(hand, card)

        handsum <- sum(hand$value)                                              # sum of cards

        while (TRUE) {
            # 1.) Check if A should be counted as 11
            if (length(which(hand$face == "A")) >= 1) {
                if (sum(hand$value) - 1 == 10) {
                    handsum <- 21
                    win <- 1
                    break
                }
            }
            # 2.) Check if handsum is 21 or bust
            if (handsum == 21) {
                win <- TRUE
                break
            } else if (handsum > 21) {
                win <- 0
                break
            }   # else draw a card and update handsum
            card <- tmpdeck[sample(rownames(tmpdeck), 1), ]
            hand <- rbind(hand, card)
            handsum <- sum(hand$value)
            tmpdeck <- tmpdeck[-which(rownames(tmpdeck) == (rownames(card))), ]
        }
        # indicate which cards were in the starting hand
        start <- numeric(length(hand$value))
        start[1] <- 1
        start[2] <- 1

        hands <- rbind(hands, cbind(hand, game = c(i), win, start))
    }
    rownames(hands) <- c()
    write.csv(hands, "../csvs/carddata.csv")
    return(hands)
}