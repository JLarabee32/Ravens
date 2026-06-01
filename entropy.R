library(tidyverse)
library(arrow)

#-------------------Load Data-----------------------------------
games <- read_parquet("games.parquet")
player_play <- read_parquet("player_play.parquet")
players <- read_parquet("players.parquet")
plays <- read_parquet("plays.parquet")
tracking_week_1 <- read_parquet("tracking_week_1.parquet")
tracking_week_2 <- read_parquet("tracking_week_2.parquet")
tracking_week_3 <- read_parquet("tracking_week_3.parquet")
tracking_week_4 <- read_parquet("tracking_week_4.parquet")
tracking_week_5 <- read_parquet("tracking_week_5.parquet")
tracking_week_6 <- read_parquet("tracking_week_6.parquet")
tracking_week_7 <- read_parquet("tracking_week_7.parquet")
tracking_week_8 <- read_parquet("tracking_week_8.parquet")
tracking_week_9 <- read_parquet("tracking_week_9.parquet")

#-----------------Joins & Filters------------------------------
rusher_list <- c("OLB", "DE")

pass_rushers <- players %>%
  filter(position %in% rusher_list) %>%
  select(-displayName)

tracking_week_1_pr <- tracking_week_1 %>%
  left_join(pass_rushers, by = "nflId") %>%
  filter(!is.na(position))
tracking_week_2_pr <- tracking_week_2 %>%
  left_join(pass_rushers, by = "nflId") %>%
  filter(!is.na(position))
tracking_week_3_pr <- tracking_week_3 %>%
  left_join(pass_rushers, by = "nflId") %>%
  filter(!is.na(position))
tracking_week_4_pr <- tracking_week_4 %>%
  left_join(pass_rushers, by = "nflId") %>%
  filter(!is.na(position))
tracking_week_5_pr <- tracking_week_5 %>%
  left_join(pass_rushers, by = "nflId") %>%
  filter(!is.na(position))
tracking_week_6_pr <- tracking_week_6 %>%
  left_join(pass_rushers, by = "nflId") %>%
  filter(!is.na(position))
tracking_week_7_pr <- tracking_week_7 %>%
  left_join(pass_rushers, by = "nflId") %>%
  filter(!is.na(position))
tracking_week_8_pr <- tracking_week_8 %>%
  left_join(pass_rushers, by = "nflId") %>%
  filter(!is.na(position))
tracking_week_9_pr <- tracking_week_9 %>%
  left_join(pass_rushers, by = "nflId") %>%
  filter(!is.na(position))

tracking_all_weeks_pr <- tracking_week_1_pr %>%
  rbind(tracking_week_2_pr) %>%
  rbind(tracking_week_3_pr) %>%
  rbind(tracking_week_4_pr) %>%
  rbind(tracking_week_5_pr) %>%
  rbind(tracking_week_6_pr) %>%
  rbind(tracking_week_7_pr) %>%
  rbind(tracking_week_8_pr) %>%
  rbind(tracking_week_9_pr)

dropbacks <- plays %>%
  filter(isDropback == TRUE) %>%
  select(gameId, playId, isDropback)

tracking_pr <- tracking_all_weeks_pr %>%
  inner_join(dropbacks, by = c("gameId", "playId"))
#---------------------Entropy-------------------------------
shannon_entropy <- function(x) {
  tbl <- table(x)
  p   <- tbl / sum(tbl)
  p   <- p[p > 0]
  -sum(p * log2(p))
}

####Entropy of routes####
rush_end_events <- c("pass_forward", "pass_shovel", "autoevent_passforward", "autoevent_passinterrupted", "qb_sack", "qb_strip_sack")

tracking_pr_adj <- tracking_pr %>%
  mutate(
    adj_x = if_else(playDirection == "right", x, 120 - x),
    adj_y = if_else(playDirection == "right", 53.3 - y, y)
  )

tracking_pr_adj %>% filter(frameType == "SNAP") %>%
  ggplot(mapping = aes(x = adj_x, y = adj_y)) +
  geom_point()

snap_origin <- tracking_pr_adj %>%
  filter(event == "ball_snap") %>%
  group_by(nflId, gameId, playId) %>%
  slice_min(frameId, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(nflId, gameId, playId,
         snap_frame = frameId,
         snap_adj_x = adj_x,
         snap_adj_y = adj_y)

end_frame <- tracking_pr_adj %>%
  filter(event %in% rush_end_events) %>%
  group_by(gameId, playId) %>%
  summarise(end_frame = min(frameId), .groups = "drop")

tracking_pr_rel <- tracking_pr_adj %>%
  left_join(snap_origin, by = c("nflId", "gameId", "playId")) %>%
  mutate(
    rel_x = adj_x - snap_adj_x,
    rel_y = adj_y - snap_adj_y
  )

tracking_pr_rel %>%
  filter(event == "ball_snap") %>%
  ggplot(aes(x = rel_x, y = rel_y)) +
  geom_point()

# Grid parameters (1-yard cells)
x_min <- -11;  x_max <- 1
y_min <- -13;  y_max <-  13

cell_size <- 0.25  # quarter-yard cells

route_frames <- tracking_pr_rel %>%
  inner_join(end_frame, by = c("gameId", "playId")) %>%
  filter(
    !is.na(snap_frame),
    frameId >= snap_frame,
    frameId <= end_frame
  ) %>%
  mutate(
    cell_x = floor(rel_x / cell_size) * cell_size,
    cell_y = floor(rel_y / cell_size) * cell_size
  ) %>%
  filter(
    cell_x >= x_min, cell_x < x_max,
    cell_y >= y_min, cell_y < y_max
  )

route_fingerprints <- route_frames %>%
  distinct(nflId, gameId, playId, cell_x, cell_y) %>%
  arrange(nflId, gameId, playId, cell_x, cell_y) %>%
  group_by(nflId, gameId, playId) %>%
  summarise(
    fingerprint = paste(cell_x, cell_y, sep = ",", collapse = "|"),
    .groups = "drop"
  )

route_entropy <- route_fingerprints %>%
  group_by(nflId) %>%
  summarise(
    n_snaps_route = n(),
    route_entropy = shannon_entropy(fingerprint),
    .groups = "drop"
  ) %>%
  left_join(players, by = "nflId")

pr_stats <- player_play %>%
  group_by(nflId) %>%
  summarise(quarterbackHits = sum(quarterbackHit),
            AvgtimeToPressureAsPassRusher = mean(timeToPressureAsPassRusher, na.rm = T)
            )

entropy_stats <- route_entropy %>%
  left_join(pr_stats, by = "nflId") %>%
  filter(n_snaps_route >= 80) %>%
  mutate(QBHitRate = quarterbackHits/n_snaps_route)

cor(entropy_stats$route_entropy, entropy_stats$quarterbackHits)
cor(entropy_stats$route_entropy, entropy_stats$QBHitRate)
#---------------------VIZ-------------------------------------
sample_route <- route_frames %>%
  filter(nflId == 52456, gameId == 2022092511, playId == 2346) %>%
  arrange(frameId)

sample_cells <- sample_route %>%
  distinct(cell_x, cell_y)

ggplot() +
  geom_tile(data = sample_cells, aes(x = cell_y + cell_size / 2, y = cell_x + cell_size / 2),
            fill  = "steelblue", alpha = 0.3, color = "steelblue", linewidth = 0.3,
            width = cell_size, height = cell_size) +
  geom_vline(xintercept = seq(y_min, y_max, cell_size), color = "grey80", linewidth = 0.2) +
  geom_hline(yintercept = seq(x_min, x_max, cell_size), color = "grey80", linewidth = 0.2) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.8, linetype = "dashed") +
  geom_path(
    data = sample_route,
    aes(x = rel_y, y = rel_x),
    color = "black", linewidth = 0.7
  ) +
  geom_point(
    data = sample_route %>% slice_min(frameId, n = 1),
    aes(x = rel_y, y = rel_x),
    color = "green", size = 3
  ) +
  geom_point(
    data = sample_route %>% slice_max(frameId, n = 1),
    aes(x = rel_y, y = rel_x),
    color = "red", size = 3
  ) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(y_min, y_max, 1), limits = c(y_min, y_max)) +
  scale_y_continuous(breaks = seq(x_min, x_max, 1), limits = c(x_min, x_max)) +
  labs(
    title    = paste("Pass Rush Route —", sample_route$displayName[1]),
    subtitle = paste("gameId:", sample_route$gameId[1], "| playId:", sample_route$playId[1]),
    x        = "Lateral displacement from snap",
    y        = "Yards past line of scrimmage",
    caption  = "Red dashed line = line of scrimmage | Green = snap | Red dot = play end"
  ) +
  theme_minimal()

player_id <- 47889

player_routes <- route_frames %>%
  filter(nflId == player_id) %>%
  arrange(gameId, playId, frameId)

player_cells <- player_routes %>%
  distinct(cell_x, cell_y)

player_name <- player_routes$displayName[1]

ggplot() +
  geom_vline(xintercept = seq(y_min, y_max, cell_size), color = "grey80", linewidth = 0.2) +
  geom_hline(yintercept = seq(x_min, x_max, cell_size), color = "grey80", linewidth = 0.2) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.8, linetype = "dashed") +
  geom_path(
    data = player_routes,
    aes(x = rel_y, y = rel_x, group = interaction(gameId, playId)),
    color = "black", linewidth = 0.3, alpha = 0.4
  ) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(y_min, y_max, 1), limits = c(y_min, y_max)) +
  scale_y_continuous(breaks = seq(x_min, x_max, 1), limits = c(x_min, x_max)) +
  labs(
    title    = paste("All Pass Rush Routes —", player_name),
    subtitle = "High Entropy",
    x        = "Lateral displacement from snap",
    y        = "Yards past line of scrimmage",
    caption  = "Red dashed line = line of scrimmage"
  ) +
  theme_minimal()

player_id <- 44871

player_routes <- route_frames %>%
  filter(nflId == player_id) %>%
  arrange(gameId, playId, frameId)

player_cells <- player_routes %>%
  distinct(cell_x, cell_y)

player_name <- player_routes$displayName[1]

ggplot() +
  geom_vline(xintercept = seq(y_min, y_max, cell_size), color = "grey80", linewidth = 0.2) +
  geom_hline(yintercept = seq(x_min, x_max, cell_size), color = "grey80", linewidth = 0.2) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.8, linetype = "dashed") +
  geom_path(
    data = player_routes,
    aes(x = rel_y, y = rel_x, group = interaction(gameId, playId)),
    color = "black", linewidth = 0.3, alpha = 0.4
  ) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(y_min, y_max, 1), limits = c(y_min, y_max)) +
  scale_y_continuous(breaks = seq(x_min, x_max, 1), limits = c(x_min, x_max)) +
  labs(
    title    = paste("All Pass Rush Routes —", player_name),
    subtitle = "Low Entropy",
    x        = "Lateral displacement from snap",
    y        = "Yards past line of scrimmage",
    caption  = "Red dashed line = line of scrimmage"
  ) +
  theme_minimal()
