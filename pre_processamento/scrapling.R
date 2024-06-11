library(rvest)
library(stringr)
library(dplyr)
rm(list=ls())

# Nome dos times 
data <- read.xlsx("pagina_analise_times/site_shiny/all_leagues.xlsx", sheet = "All Leagues")
team_premier <- data$team[data$league_name == "Premier League"]
team_laliga <- data$team[data$league_name == "La Liga"]
team_seriea <- data$team[data$league_name == "Camp Italiano(SerieA TIM)"]
team_bundesliga <- data$team[data$league_name == "Bundesliga"]
team_ligue <- data$team[data$league_name == "Ligue 1"]
team_brasileirao <- data$team[data$league_name == "Brasileirao(serieA)"] 

# realizando scrapling das informações faltantes

# Função para scrapear dados de vitórias, derrotas e empates
get_match_results_data <- function(url, team) {
  page <- read_html(url)
  
  teams <- team
  matches_played <- page %>% html_nodes("#yw1 > table > tbody > tr > td:nth-child(4)") %>% html_text(trim = TRUE) %>% as.numeric()
  wins <- page %>% html_nodes("#yw1 > table > tbody > tr > td:nth-child(5)") %>% html_text(trim = TRUE) %>% as.numeric()
  draws <- page %>% html_nodes("#yw1 > table > tbody > tr > td:nth-child(6)") %>% html_text(trim = TRUE) %>% as.numeric()
  losses <- page %>% html_nodes("#yw1 > table > tbody > tr > td:nth-child(7)") %>% html_text(trim = TRUE) %>% as.numeric()
  goals <- page %>% html_nodes("#yw1 > table > tbody > tr > td:nth-child(8)") %>% html_text(trim = TRUE) 
  
  match_results_data <- data.frame(
    team = teams,
    matches_played = matches_played,
    goals = goals,
    wins = wins,
    draws = draws,
    losses = losses
  )
  
  return(match_results_data)
}

# URLs das páginas a serem scrapadas
match_results_premier <- "https://www.transfermarkt.com/premier-league/tabelle/wettbewerb/GB1/saison_id/2023"
match_results_laliga <- "https://www.transfermarkt.com/laliga/tabelle/wettbewerb/ES1/saison_id/2023"
match_results_seriea <- "https://www.transfermarkt.com/serie-a/tabelle/wettbewerb/IT1/saison_id/2023"
match_results_bundesliga <- "https://www.transfermarkt.com/bundesliga/tabelle/wettbewerb/L1/saison_id/2023"
match_results_ligue <- "https://www.transfermarkt.com/ligue-1/tabelle/wettbewerb/FR1/saison_id/2023"
match_results_brasileirao <- "https://www.transfermarkt.com/campeonato-brasileiro-serie-a/tabelle/wettbewerb/BRA1?saison_id=2022"
# Scraping dos dados
match_results_premier <- get_match_results_data(match_results_premier,team_premier)
match_results_laliga <- get_match_results_data(match_results_laliga, team_laliga)
match_results_seriea <- get_match_results_data(match_results_seriea, team_seriea)
match_results_bundesliga <- get_match_results_data(match_results_bundesliga, team_bundesliga)
match_results_ligue <- get_match_results_data(match_results_ligue, team_ligue)
match_results_brasileirao <- get_match_results_data(match_results_brasileirao, team_brasileirao)

all_results = bind_rows(match_results_bundesliga, match_results_laliga, match_results_ligue, match_results_premier, match_results_seriea, match_results_brasileirao)

# coluna goals para goals_scored e goals_received
all_results <- all_results %>% separate(goals, into = c("goals_scored", "goals_received"), sep = ":", convert = TRUE)

#juntando ao df final
all_leagues <- full_join(data, all_results, by = "team")


# Exportando o arquivo
output_path <- "all_leagues.xlsx"
write.xlsx(all_leagues, file = output_path, sheetName = "All Leagues", rowNames = FALSE)
cat("DataFrame exportado com sucesso para", output_path)

