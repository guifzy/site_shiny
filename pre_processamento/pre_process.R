library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

caminho <- "pagina_analise_times/site_shiny/DBS.xlsx"  

# função para o pré-processamento
preprocess_league_data <- function(caminho, main, aux, add = NULL, league) {
 
  main_df <- read_excel(caminho, sheet = main)
  aux_df <- read_excel(caminho, sheet = aux)
  
  # converter a coluna 'pay' e 'season_profit' para numérico
  aux_df <- aux_df %>% mutate(pay = as.numeric(pay))
  main_df <- main_df %>% mutate(season_profit = as.numeric(season_profit))
  
  
  # full join com o dataframe principal
  final_df <- full_join(main_df, aux_df, by = "cod")
  
  # se tiver adicional, realizamos mais uma junção
  if (!is.null(add)) {
    add_df <- read_excel(caminho, sheet = add)
    final_df <- full_join(final_df, add_df, by = "cod")
  }
  
  # substituir valores NA por 0
  final_df[is.na(final_df)] <- 0
  
  # adicionar nome da liga
  final_df <- final_df %>% mutate(league_name = league)
  
  return(final_df)
}

serieA_final <- preprocess_league_data(caminho, "SERIEA", "SERIEA_AUX", "SERIEA_ADD", "Brasileirao(serieA)")
ligue_final <- preprocess_league_data(caminho, "LIGUE1", "LIGUE1_AUX", "LIGUE1_ADD", "Ligue 1")
serieTIM_final <- preprocess_league_data(caminho, "SERIEATIM", "SERIEATIM_AUX", NULL, "Camp Italiano(SerieA TIM)")
laliga_final <- preprocess_league_data(caminho, "LALIGA", "LALIGA_AUX", NULL, "La Liga")
premierleague_final <- preprocess_league_data(caminho, "PREMIERLEAGUE", "PREMIERLEAGUE_AUX", "PREMIERLEAGUE_ADD", "Premier League")
bundesliga_final <- preprocess_league_data(caminho, "BUNDESLIGA", "BUNDESLIGA_AUX", NULL, "Bundesliga")

all_leagues_final <- bind_rows(serieA_final, ligue_final, serieTIM_final, laliga_final, premierleague_final, bundesliga_final)
all_leagues_final[is.na(all_leagues_final)] <- 0

# verificação de tipos de dados
str(all_leagues_final)

# remoção de duplicatas
all_leagues_final <- all_leagues_final %>% distinct()

# Exportando o arquivo
output_path <- "all_leagues.xlsx"
write.xlsx(all_leagues_final, file = output_path, sheetName = "All Leagues", rowNames = FALSE)
cat("DataFrame exportado com sucesso para", output_path)
