library(ggplot2)

bg_color = '#5A8E38'

theme_court = function(base_size = 16) {
  theme_bw(base_size) +
    theme(
      text = element_text(color = "#f0f0f0"),
      plot.background = element_rect(fill = bg_color, color = bg_color),
      panel.background = element_rect(fill = bg_color, color = bg_color),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks.length = unit(0, "lines"),
      legend.background = element_rect(fill = bg_color, color = bg_color),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

goal_keeper <- data.frame(loc_x = c(50), loc_y = c(90), formation = 'goalkeeper')

formation_442 <- data.frame(
  loc_x = c(rep(quantile(seq(
    10:90
  ))[2:5], 2), quantile(seq(10:90))[3] + 1, quantile(seq(10:90))[4] - 1),
  loc_y = c(rep(77, 4), rep(67, 4), rep(57, 2)),
  formation = rep('442', 10)
)

formation_433 <- data.frame(
  loc_x = c(quantile(seq(10:90))[2:5], c(32,52,72) ,c(16,52,86)),
  loc_y = c(rep(77, 4), 65,67,65, rep(57, 3)),
  formation = rep('433', 10)
)

formation_4231 <- data.frame(
  loc_x = c(quantile(seq(10:90))[2:5], quantile(seq(10:90))[3:4] ,c(25,52,75), 52),
  loc_y = c(rep(77, 4), rep(67,2), c(60,62,60), 55),
  formation = rep('4231', 10)
)

formation_4312 <- data.frame(
  loc_x = c(quantile(seq(10:90))[2:5], c(36,52,66) ,52, c(39,63)),
  loc_y = c(rep(77, 4), rep(67,3), c(60), c(55,55)),
  formation = rep('4312', 10)
)


formation_4141 <- data.frame(
  loc_x = c(quantile(seq(10:90))[2:5], 52, c(17,37,65,85) ,52),
  loc_y = c(rep(77, 4), 70, rep(64, 4), 55),
  formation = rep('4141', 10)
)

formation_352 <- data.frame(
  loc_x = c(c(30,50,70), c(15,30,50,70,85) ,c(40,60)),
  loc_y = c(rep(77, 3), 67,63,67,63,67, rep(57, 2)),
  formation = rep('352', 10)
)

all_formations <- dplyr::bind_rows(goal_keeper, formation_442,formation_433,formation_4231,formation_4312,formation_4141,formation_352)

create_formation <- function(formation_a, formation_b){
  
  team_a <- all_formations[all_formations$formation == formation_a,]
  team_a <- dplyr::bind_rows(team_a, all_formations[all_formations$formation == 'goalkeeper',])
  team_a$team <- 'team_a'
  
  team_b <- all_formations[all_formations$formation == formation_b,]
  team_b <- dplyr::bind_rows(team_b, all_formations[all_formations$formation == 'goalkeeper',])
  team_b$loc_y <- 100 - team_b$loc_y
  team_b$team <- 'team_b'
  
  formation_df <- dplyr::bind_rows(team_a, team_b)
  return(formation_df)
}

formation_df <- create_formation('4231','4312')
  
empty_court <- ggplot(data = formation_df,
                      aes(x = loc_x, y = loc_y,
                          color = '#000000')) + geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 100), fill = NA, colour = "#000000", size = 1) +
  geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 50), fill = NA, colour = "#000000", size = 1) +
  geom_rect(aes(xmin = 21, xmax = 79, ymin = 17, ymax = 0), fill = NA, colour = "#000000", size = 1) +
  geom_rect(aes(xmin = 21, xmax = 79, ymin = 83, ymax = 100), fill = NA, colour = "#000000", size = 1) +
  geom_rect(aes(xmin = 36.8, xmax = 63.2, ymin = 0, ymax = 6), fill = NA, colour = "#000000", size = 1) +
  geom_rect(aes(xmin = 36.8, xmax = 63.2, ymin = 100, ymax = 94), fill = NA, colour = "#000000", size = 1) +
  theme(rect = element_blank(), 
        line = element_blank(), 
        text = element_blank()) + theme_court()



formation <- empty_court +
  geom_point(
    alpha = 0.8, size = 4
  ) + geom_text(colour = "#000000", aes(label=ifelse(loc_y < 50, formation, '')),hjust=0.5, vjust=-1.2,alpha = 0.8) +
  geom_point(
    alpha = 0.8, size = 4
  ) + geom_text(colour = "#000000", aes(label=ifelse(loc_y > 50, formation, '')),hjust=0.5, vjust=1.8,alpha = 0.8)


formation
