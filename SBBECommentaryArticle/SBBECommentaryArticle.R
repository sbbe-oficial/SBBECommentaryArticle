### The BEGINNING ~~~~~
##
# Plots maps for the SBBE commentary article | Written by George Pacheco ~


# Cleans environment ~ 
rm(list=ls())


# Sets working directory ~
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loads packages ~
pacman::p_load(tidyverse, ggnewscale, ggtext, ggstar, ggrepel, shadowtext, readxl, writexl, cowplot, patchwork, ggpubr, lemon, reshape2, writexl, stringr, lubridate,
               geobr, ggspatial, showtext, png, extrafont, sf, ggiraphExtra, fontawesome, shiny, DT,
               rvest, stringr, purrr, tibble, dplyr, extrafont, emojifont, grid, rsvg, ggimage, rnaturalearthdata, rnaturalearth, ggh4x)


# Loads extra fonts ~
loadfonts()
font_add_google("Cormorant Garamond", "Cormorant")
showtext_auto()


# Loads data ~
fulldf <- read.csv("./SBBELists/SBBEmembers--12Aug26.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",")


# Loads data ~
extra <- read_excel("./SBBELists/SBBE24_InscriçõesExtraordinárias_R.xlsx")
normal <- read_excel("./SBBELists/ListaParticipante_18-12-2024_11-47-55.xlsx") %>%
          dplyr::filter(Inscrição == "Aprovado") %>%
          dplyr::filter(Categoria != "Curso Galaxy - Apenas para inscritos no congresso") %>%
          dplyr::filter(!ID %in% extra$ID) %>%
          dplyr::rename(Sub_Area = `Sub Área`)


ListSBBE26 <- read_excel("./SBBELists/ListaParticipante_06-07-2026_10-52-00.xlsx") %>%
              dplyr::filter(Inscrição == "Aprovado") %>%
              dplyr::mutate(Conference = "SBBE26",
                            `Nome Crachá` = NA,
                            `Instituição` = NA,
                            `Gênero` = NA,
                            Sub_Area = NA)  %>%
              dplyr::select(Conference, Nome, ID, Categoria, `Nome Crachá`, UF, Cidade, `Instituição`, `Gênero`)


# Merges data frames ~
Attendees_df <- rbind(extra, normal)
Attendees_df <- Attendees_df %>%
                dplyr::mutate(Conference = "SBBE24") %>%
                dplyr::select(Conference, Nome, ID, Categoria, `Nome Crachá`, UF, Cidade, `Instituição`, `Gênero`)


# Merges conferences data frames ~
Attendees_df <- bind_rows(Attendees_df, ListSBBE26)


# Gets Stage ~
fulldf$Stage <- ifelse(grepl("Profissional", fulldf$Labels), "Profissional",
                ifelse(grepl("Pos-Graduacao", fulldf$Labels), "Pós-graduação",
                ifelse(grepl("Graduacao", fulldf$Labels), "Graduação", NA)))


fulldfUp <- fulldf %>%
            dplyr::select(Name, Stage, Gender, State, Institution) %>%
            mutate(Data = "Members",
                   Conference = NA) %>%
            mutate(`Nome Crachá` = "") %>%
            mutate(ID = "")


Attendees_dfUp <- Attendees_df %>%
                  dplyr::rename(Name = Nome, State = UF, Institution = `Instituição`, Name = Nome, Gender = `Gênero`, Stage = Categoria) %>%
                  dplyr::select(Conference, Name, Stage, Gender, State, Institution, `Nome Crachá`, ID) %>%
                  mutate(Data = "Attendees")



# Combines data frames ~
fulldfUltra <- rbind(fulldfUp, Attendees_dfUp)


# Corrects Nome Crachá ~
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Fernanda de Pinho Werneck"), "Fernanda Werneck", fulldfUltra$`Nome Crachá`)
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Jose Alexandre Felizola Diniz Filho"), "Jose Diniz-Filho", fulldfUltra$`Nome Crachá`)
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Clarisse Palma da Silva"), "Clarisse Palma-Silva", fulldfUltra$`Nome Crachá`)
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Andrea Pedrosa Harand"), "Andrea Pedrosa Harand", fulldfUltra$`Nome Crachá`)
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Prof. Dr. Frederico Henning"), "Frederico Henning", fulldfUltra$`Nome Crachá`)
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Eduardo Tarazona"), "Eduardo Tarazona", fulldfUltra$`Nome Crachá`)
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Fabricio Santos"), "Fabrício R. Santos", fulldfUltra$`Nome Crachá`)


# Corrects Nome Crachá ~
fulldfUltra$Stage <- ifelse(fulldfUltra$Name %in% c("Thomaz Pinotti"), "Mestrando, Doutorando e Pós doutorando", fulldfUltra$Stage)
fulldfUltra$State <- ifelse(fulldfUltra$Name %in% c("Jeferson Duran Fuentes"), "SP", fulldfUltra$State)


# Corrects UF ~
levels(fulldfUltra$State <- gsub("Rio Grande do Sul", "RS", fulldfUltra$State))
levels(fulldfUltra$State <- gsub("Goias", "GO", fulldfUltra$State))
levels(fulldfUltra$State <- gsub("Iowa", "SP", fulldfUltra$State))


# Corrects UF speakers ~
fulldfUltra$State <- ifelse(fulldfUltra$ID %in% c("12821302"), "RS",
                     ifelse(fulldfUltra$ID %in% c("9314289"), "PR",
                     ifelse(fulldfUltra$ID %in% c("9176759"), "SP",
                     ifelse(fulldfUltra$ID %in% c("16590615"), "MG", fulldfUltra$State))))



# Corrects UF speakers ~
fulldfUltra$State <- ifelse(fulldfUltra$`Nome Crachá` %in% c("ALENA MAYO INIGUEZ", "Frederico Henning"), "RJ",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Fernanda Werneck"), "AM",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Eduardo Tarazona", "Fabrício Santos", "Luiz Bem"), "MG",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Ana Tourinho"), "MT",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Jose Diniz-Filho"), "GO",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Andrea Pedrosa Harand"), "PE",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Maria Yamamoto", "Felipe de Oliveira Torquato"), "RN",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Clarisse Palma-Silva", "Nelio Bizzo", "Mario Pinna", "Tábita Hünemeier", "Tiago Quental"), "SP",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("waldemir rosa"), "PR",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Kateryna Makov", "Kelly Zamudio", "Santiago Vieyra", "Thomaz Pinotti"), "Estrangeiro", fulldfUltra$State))))))))))


# Corrects State ~
fulldfUltra$State <- ifelse(fulldfUltra$Name %in% c("Alex Anderson Antony Bandeira de Carvalho",
                                                    "Ana Flávia Konig Braz",
                                                    "Bruna Debas Brito",
                                                    "Deborah Miho Chinen Toyomoto",
                                                    "Eduarda Maria de Melo de Faria",
                                                    "Gabriele Tomaz Vieira",
                                                    "George Pacheco",
                                                    "Gustavo Henrique Lopes",
                                                    "Lucas Rafael Cabral Jara",
                                                    "Luiza Teixeira da Silva",
                                                    "Maria Clara Andrade dos Santos",
                                                    "Melissa Vitória Flórido",
                                                    "Rafael Sartori Lam",
                                                    "Ramon Trindade Urbano",
                                                    "rebeca Taborda Ribas Matos",
                                                    "Sofia Demétrio Nicolau",
                                                    "Waldir Miron",
                                                    "Camila Junqueira Mazzoni",
                                                    "Ana Carolina Carnaval",
                                                    "Karina Lucas da Silva-Brandão",
                                                    "Marcelo Gehara",
                                                    "Paula X. Kover"), "Exterior", fulldfUltra$State)


# Corrects State ~
fulldfUltra <- fulldfUltra %>%
               mutate(State = case_when(Conference == "SBBE24" &
                                       (State %in% c("Aberdeen City", "california", "Florida", "Misiones", "Texas", "Córdoba", "Provincia de Buenos Aires",
                                                     "Ontario", "Capital Federal", "La Libertad", "New York") | is.na(State)) ~ "Exterior", TRUE ~ State))


# Corrects Category ~
levels(fulldfUltra$Stage <- sub("CBBE", "SBBE24", fulldfUltra$Stage))


# Converts Nome Crachá to lowercase ~
fulldfUltra$Badge <- tolower(fulldfUltra$`Nome Crachá`)


# Orders df based on Badge ~
fulldfUltra <- fulldfUltra[order(fulldfUltra$Badge), ]


# Gets counts of occurrences ~
NameCounts <- table(fulldfUltra$Badge)


# Prints repeated entries ~
NameCounts[NameCounts > 1]


# Defines entries to exclude ~
RowsToKeep <- !((grepl("alessandra p. lamarca|carlos guerra schrago|clarisse palma-silva|fabrício r. santos|fabricius domingos|fernando sotero|iris hass|nelio bizzo", fulldfUltra$Badge) & fulldfUltra$Stage == "Profissional") |
               (grepl("meari caldeira", fulldfUltra$Badge) & fulldfUltra$Stage == "Mestrando, Doutorando e Pós doutorando") |
               (grepl("lucca fanti", fulldfUltra$Badge) & fulldfUltra$Stage == "Estudante de Graduação"))


# Excludes repeated entries ~
fulldfUltra <- fulldfUltra[RowsToKeep, ]


# Corrects Categoria for repeated entries  ~
fulldfUltra$Stage <- ifelse(fulldfUltra$Badge %in% c("alessandra p. lamarca|carlos guerra schrago|clarisse palma-silva|fabrício r. santos|fabricius domingos|fernando sotero|iris hass|nelio bizzo"), "Profissional + Membro fundador da SBBE", fulldfUltra$Stage)
fulldfUltra$Stage <- ifelse(fulldfUltra$Badge %in% c("meari caldeira"), "Mestrando, Doutorando e Pós doutorando + Membro fundador da SBBE", fulldfUltra$Stage)
fulldfUltra$Stage <- ifelse(fulldfUltra$Badge %in% c("lucca fanti"), "Estudante de Graduação + Membro fundador da SBBE", fulldfUltra$Stage)


# Corrects Institution & State for better visualisation ~
levels(fulldfUltra$Institution <- gsub("Universidade|Universidad|University", "Uni.", fulldfUltra$Institution))
levels(fulldfUltra$Institution <- gsub("Instituto|Institute", "Inst.", fulldfUltra$Institution))
levels(fulldfUltra$State <- gsub("Estrangeiro", "Exterior", fulldfUltra$State))


# Corrects Gender ~
fulldfUltra$Gender <- ifelse(fulldfUltra$Gender %in% c("M", "Masculino"), "Masculino",
                      ifelse(fulldfUltra$Gender %in% c("F", "Feminino"), "Feminino",
                      ifelse(fulldfUltra$Gender %in% c("O", "Outro"), "Outro", "Error")))


# Defines the custom capitalization function ~
capitalize_words <- function(text) {
                    words <- str_split(text, " ")[[1]]
                    exclude_patterns <- c("of", "de", "da", "do", "ABC", "Não-binário", "and", "(EUA)", "del-Rei", "UNIFATECIE")
                    patterns_map <- setNames(exclude_patterns, tolower(exclude_patterns))
                    words <- sapply(words, function(word) {
                    word_lower <- tolower(word)
                    if (word_lower %in% names(patterns_map)) {
                    patterns_map[[word_lower]]}
                    else {str_to_title(word)}})
                    str_c(words, collapse = " ")}


# Apply the function to the pattern column
fulldfUltra$Name <- sapply(fulldfUltra$Name , capitalize_words)
fulldfUltra$Institution <- sapply(fulldfUltra$Institution , capitalize_words)
fulldfUltra$Gender <- sapply(fulldfUltra$Gender, capitalize_words)


# Sets all Brazilian states ~ 
AllBRLStates <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO",
                  "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "AL",
                  "GO", "MT", "MS", "DF",
                  "ES", "MG", "RJ", "SP",
                  "PR", "RS", "SC", "Exterior")


# Sets all Brazilian regions ~ 
AllBRLRegions <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul", "Exterior")


# Defines a common set of levels and ordering for Variable ~
variable_levels <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO",
                     "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "AL",
                     "GO", "MT", "MS", "DF",
                     "ES", "MG", "RJ", "SP",
                     "PR", "RS", "SC",
                     "Profissional",
                     "Pós-graduação",
                     "Graduação",
                     "Dias até o SBBE24",
                     "Seguidores no Bluesky",
                     "Seguidores no X",
                     "Seguidores no Instagram",
                     "Instituições representadas na SBBE",
                     "Afiliados à SBBE",
                     "Novartis",
                     "Okinawa Inst. of Science and Technology",
                     "École Polytechnique Fédérale de Lausanne", 
                     "Uni. of Ottawa",
                     "Pennsylvania State Uni.",
                     "Southwestern Oklahoma State Uni.",
                     "The City Uni. of New York",
                     "Towson Uni.",
                     "Southwest Uni.",
                     "Uni. of Texas",
                     "Texas A&M Uni.",
                     "Uni. of California — Los Angeles",
                     "Rice Uni.",
                     "Uni. of West Florida",
                     "Uni. of Aberdeen",
                     "Uni. of Oslo",
                     "Uni. of Gothenburg",
                     "Uni. of Copenhagen",
                     "Leibniz Inst. For Zoo and Wildlife Research",
                     "Uni. Nacional de Misiones",
                     "Uni. Nacional de Colombia", 
                     "Uni. Nacional de Trujillo",
                     "Inst. de Biología Subtropical",
                     "Inst. Multidisciplinario de Biología Vegetal",
                     "Uni. Nacional de Córdoba",
                     "Uni. de Buenos Aires",
                     "Uni. Federal de Santa Maria",
                     "Uni. Federal do Rio Grande",
                     "Uni. Federal Rural do Semi-Árido",
                     "Uni. Federal de Pelotas",
                     "Uni. Federal de Ciências da Saúde de Porto Alegre",
                     "Uni. Federal do Rio Grande do Sul",
                     "Uni. Federal da Fronteira Sul",
                     "Pontifícia Uni. Católica do Rio Grande do Sul",
                     "Uni. Federal da Integração Latino-Americana",
                     "Uni. Estadual de Maringá",
                     "Centro Universitário Claretiano",
                     "Uningá",
                     "Museu de História Natural Capão da Imbuia",
                     "Inst. Carlos Chagas — Fiocruz Paraná",
                     "Centro Universitário UNIFATECIE",
                     "Uni. Estadual de Ponta Grossa",
                     "Uni. Estadual do Centro-Oeste",
                     "Secretaria de Educação do Estado do Paraná",
                     "Inst. Federal — Paraná",
                     "Uni. Tecnológica Federal do Paraná",
                     "Pontifícia Uni. Católica do Paraná",
                     "Uni. Federal do Paraná",
                     "Hospital Regional Hans Dieter Schmidt",
                     "Centro Universitário Leonardo da Vinci",
                     "Uni. da Região de Joinville",
                     "Uni. Federal de Santa Catarina",
                     "Uni. do Vale do Paraíba",
                     "Inst. Butantan",
                     "Uni. do Vale do Itajaí",
                     "Uni. Santo Amaro",
                     "Uni. do Oeste Paulista",
                     "Uni. de Mogi Das Cruzes",
                     "Uni. Federal de São Carlos",
                     "Uni. Federal do ABC",
                     "Uni. Federal de São Paulo",
                     "Uni. de São Paulo",
                     "Inst. Biológico — São Paulo", 
                     "Uni. Estadual de Campinas",
                     "Uni. Estadual Paulista",
                     "Unisãojosé",
                     "Uni. Estadual do Norte Fluminense Darcy Ribeiro",
                     "Uni. Federal Fluminense",
                     "Inst. Oswaldo Cruz",
                     "Uni. Federal do Rio de Janeiro",
                     "Uni. Federal do Estado do Rio de Janeiro",
                     "Uni. Estadual do Norte Fluminense", 
                     "Fundação Oswaldo Cruz — Rio de Janeiro",
                     "Fundação Oswaldo Cruz — Amazônia",
                     "Jardim Botânico do Rio de Janeiro",
                     "Museu Nacional",
                     "Uni. do Estado do Rio de Janeiro",
                     "Centro Universitário Serra Dos Órgãos",
                     "Uni. Federal do Espírito Santo",
                     "Inst. Nacional da Mata Atlântica",
                     "Colégio Águia de Prata",
                     "Pontifícia Uni. Católica de Minas Gerais",
                     "Uni. Federal de Uberlândia",
                     "Uni. Federal de Itajubá",
                     "Uni. Federal do Triângulo Mineiro",
                     "Uni. Federal de Juiz de Fora",
                     "Centro Universitário de Patos de Minas",
                     "Uni. do Estado de Minas Gerais",
                     "Uni. Federal de Minas Gerais",
                     "Uni. Federal de Lavras",
                     "Uni. Federal de Viçosa", 
                     "Uni. Federal de São João del-Rei",
                     "Uni. Federal de Jataí",
                     "Empresa Brasileira de Pesquisa Agropecuária",
                     "Uni. de Brasília",
                     "Uni. Federal de Goiás",
                     "Uni. Estadual de Goiás",
                     "Uni. do Estado de Mato Grosso",
                     "Uni. Federal de Mato Grosso",
                     "Uni. Federal de Mato Grosso do Sul",
                     "Uni. Federal da Grande Dourados",
                     "Uni. Estadual de Santa Cruz",
                     "Uni. Federal da Bahia",
                     "Uni. Estadual de Feira de Santana",
                     "Uni. Federal do Recôncavo da Bahia",
                     "Uni. Estadual do Sudoeste da Bahia",
                     "Uni. Federal do Maranhão",
                     "Uni. Federal de Sergipe",
                     "Uni. Federal de Alagoas",
                     "Uni. Federal do Vale do São Francisco",
                     "Uni. de Pernambuco",
                     "Inst. Aggeu Magalhães — Fiocruz Pernambuco",
                     "Inst. Federal — Pernambuco",
                     "Inst. Federal — do Sul de Minas",
                     "Uni. Federal de Pernambuco",
                     "Uni. Federal Rural de Pernambuco",
                     "Uni. Estadual da Paraíba",
                     "Secretaria Municipal de Educação — Paraíba",
                     "Uni. Federal da Paraíba",
                     "Secretaria Municipal de Educação",
                     "Uni. Federal do Rio Grande do Norte",
                     "Museu Paraense Emílio Goeldi",
                     "Uni. Federal do Amapá",
                     "Uni. Federal do Pará",
                     "Inst. Nacional de Pesquisas da Amazônia",
                     "Uni. Federal de Roraima",
                     "Uni. Federal do Amazonas",
                     "Inst. Tecnológico Vale",
                     "Feminino",
                     "Masculino",
                     "Outro",
                     "Exterior",
                     "Sul", 
                     "Sudeste",
                     "Centro-Oeste",
                     "Nordeste",
                     "Norte")


# Checks missing Institution ~
setdiff(fulldfUltra$Institution[fulldfUltra$Data == "Members"], variable_levels)


# Expands fulldf by creating Region ~
fulldfUltra$Region <- ifelse(fulldfUltra$State %in% c("AC", "AM", "AP", "PA", "RR", "RO", "TO"), "Norte",
                      ifelse(fulldfUltra$State %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"), "Nordeste",
                      ifelse(fulldfUltra$State %in% c("DF", "GO", "MT", "MS"), "Centro-Oeste",
                      ifelse(fulldfUltra$State %in% c("MG", "ES", "RJ", "SP"), "Sudeste",
                      ifelse(fulldfUltra$State %in% c("PR", "RS", "SC"), "Sul",
                      ifelse(fulldfUltra$State %in% c("Exterior"), "Exterior", "Error"))))))


# Gets general numbers ~
fulldf_Descriptive <- subset(fulldfUltra, Data == "Members") %>%
                      summarise("Afiliados à SBBE" = n_distinct(Name),
                                "Instituições representadas na SBBE" = n_distinct(Institution)) %>%
                      mutate(Stats = "General") %>%
                      pivot_longer(cols = -Stats, names_to = "Variable", values_to = "Percentage") %>%
                      mutate(n = 0) %>%
                      relocate(n, .before = Percentage) %>%
                      relocate(Stats, .after = Percentage) %>%
                      mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Gets percentage for Institution ~
fulldf_StageMembersPerc <- subset(fulldfUltra, Data == "Members") %>%
                           dplyr::count(Stage) %>%
                           dplyr::mutate(Percentage = n / sum(n)) %>%
                           dplyr::rename(Variable = Stage) %>%
                           dplyr::mutate(Stats = "StageMembers") %>%
                           dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Create a data frame with counts and proportions per institution ~
fulldf_StateMembersPerc <- subset(fulldfUltra, Data == "Members") %>%
                           dplyr::count(State) %>%
                           dplyr::mutate(Percentage = n / sum(n)) %>%
                           dplyr::rename(Variable = State) %>%
                           complete(Variable = AllBRLStates, fill = list(n = 0, Percentage = 0)) %>%
                           dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
                           dplyr::mutate(Stats = "StateMembers")


# Create a data frame with counts and proportions per institution ~
fulldf_RegionMembersPerc <- subset(fulldfUltra, Data == "Members") %>%
                            dplyr::filter(Region != "Error") %>%
                            dplyr::count(Region) %>%
                            dplyr::mutate(Percentage = n / sum(n)) %>%
                            dplyr::rename(Variable = Region) %>%
                            complete(Variable = AllBRLRegions, fill = list(n = 0, Percentage = 0)) %>%
                            dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
                            dplyr::mutate(Stats = "RegionMembers")


# Create a data frame with counts and proportions per institution ~
fulldf_StateAttendeesPerc <- subset(fulldfUltra, Data == "Attendees") %>%
                             dplyr::count(Conference, State) %>%
                             dplyr::group_by(Conference) %>%
                             dplyr::mutate(Percentage = n / sum(n)) %>%
                             dplyr::rename(Variable = State) %>%
                             tidyr::complete(Variable = AllBRLStates, fill = list(n = 0, Percentage = 0)) %>%
                             dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
                             dplyr::mutate(Stats = "StateAttendees") %>%
                             dplyr::ungroup()


# Create a data frame with counts and proportions per institution ~
fulldf_RegionAttendeesPerc <- subset(fulldfUltra, Data == "Attendees") %>%
                                     dplyr::filter(Region != "Error") %>%
                                     dplyr::count(Conference, Region) %>%
                                     dplyr::group_by(Conference) %>%
                                     dplyr::mutate(Percentage = n / sum(n)) %>%
                                     dplyr::rename(Variable = Region) %>%
                                     complete(Variable = AllBRLRegions, fill = list(n = 0, Percentage = 0)) %>%
                                     dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
                                     dplyr::mutate(Stats = "RegionAttendees")


fulldfUltra <- fulldfUltra %>% dplyr::mutate(Stage = gsub(" \\+ Membro fundador da SBBE", "", Stage))


# Gets percentage for Gender ~
fulldf_GenderMembersPerc <- subset(fulldfUltra, Data == "Members") %>%
                                   dplyr::filter(Region != "Error") %>%
                                   dplyr::count(Gender) %>%
                                   dplyr::mutate(Percentage = n / sum(n)) %>%
                                   dplyr::rename(Variable = Gender) %>%
                                   dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
                                   dplyr::mutate(Stats = "GenderMembers")


# Gets percentage for Institutions ~
fulldf_InstitutionMembersPerc <- subset(fulldfUltra, Data == "Members") %>%
                                        dplyr::filter(Institution != "") %>%
                                        dplyr::count(Institution, Region) %>%
                                        dplyr::mutate(Percentage = n / sum(n)) %>%
                                        dplyr::rename(Variable = Institution) %>%
                                        dplyr::mutate(Stats = "InstitutionMembers") %>%
                                        dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Combine the data frames ~  
fulldfPlots <- bind_rows(fulldf_StageMembersPerc,
                         fulldf_StateMembersPerc,
                         fulldf_RegionMembersPerc,
                         fulldf_GenderMembersPerc,
                         fulldf_InstitutionMembersPerc)


# Expands fulldfUp by creating BarFill ~
fulldfPlots$BarFill <- ifelse(str_detect(fulldfPlots$Stats, "Members"), "#006837",
                       ifelse(str_detect(fulldfPlots$Stats, "Attendees"), "#41ab5d", "#fbb4ae"))


# Reorders Population ~
fulldfPlots$Stats <- factor(fulldfPlots$Stats, ordered = T,
                            levels = c("StageMembers",
                                       "StateMembers",
                                       "RegionMembers",
                                       "GenderMembers",
                                       "InstitutionMembers"))


# Load geom data ~
BRL_Regions <- read_region(simplified = TRUE, year = 2019)
BRL_States <- read_state(code_state = "all", simplified = TRUE, year = 2019)


# Corrects entries ~
levels(BRL_Regions$name_region <- gsub("Centro Oeste", "Centro-Oeste", BRL_Regions$name_region))
levels(BRL_States$name_region <- gsub("Centro Oeste", "Centro-Oeste", BRL_States$name_region))


# Expands BRL_Regions by creating Region ~
BRL_Regions$Region <- ifelse(BRL_Regions$name_region %in% c("Norte"), "North",
                      ifelse(BRL_Regions$name_region %in% c("Nordeste"), "Northeast",
                      ifelse(BRL_Regions$name_region %in% c("Centro-Oeste"), "Central-West",
                      ifelse(BRL_Regions$name_region %in% c("Sudeste"), "Southeast",
                      ifelse(BRL_Regions$name_region %in% c("Sul"), "South", "Error")))))


# Expands BRL_Regions by adding the SBBE24 & Abroad rows ~
BRL_Regions <- add_row(BRL_Regions, name_region = "SBBE24", Region = "SBBE24")
BRL_Regions <- add_row(BRL_Regions, name_region = "SBBE26", Region = "SBBE26")
BRL_Regions <- add_row(BRL_Regions, name_region = "Exterior", Region = "Exterior")
BRL_States <- add_row(BRL_States, abbrev_state = "Exterior", name_region = "Exterior")


# Creates a data frame with the centroids of the Brazilian regions ~
BRL_Regions_Centroids_df <- data.frame(Region = c("North", "Northeast", "Central-West", "Southeast", "South", "SBBE24", "SBBE26", "Exterior", "SP"),
                                       Longitude = c(-58, -41.25, -53.15, -44.85, -51.2, -49.271111, -43.964837, -65, -48.62),
                                       Latitude = c(-3.5, -8, -15.5, -20, -27.5, -25.429722, -19.872538, -25, -21.9))

 
# Merges data frame to perform the change ~
BRL_Regions <- left_join(BRL_Regions, BRL_Regions_Centroids_df, by = "Region")


# Reduces data ~ 
BRL_Regions <- BRL_Regions %>%
               dplyr::select(-code_region)
BRL_States <- BRL_States %>%
              dplyr::select(-c(code_state, code_region, name_state))


# Renames columns ~
BRL_States <- BRL_States %>%
              rename(Variable = abbrev_state)


# Joins data frames ~
fulldf_RegionMembersPerc <- fulldf_RegionMembersPerc %>%
                            rename(name_region = Variable)
fulldf_RegionAttendeesPerc <- fulldf_RegionAttendeesPerc %>%
                              rename(name_region = Variable)


# Merges data frames individually ~
RegionMembers_df <- BRL_States %>%
                    inner_join(fulldf_RegionMembersPerc, by = "name_region") %>%
                    mutate(Stats = "Members")  %>%
                    mutate(Division = "Per Region")
StateMembers_df <- BRL_States %>%
                   inner_join(fulldf_StateMembersPerc, by = "Variable") %>%
                   mutate(Stats = "Members") %>%
                   mutate(Division = "Per State")
RegionAttendees_df <- BRL_States %>%
                      inner_join(fulldf_RegionAttendeesPerc, by = "name_region", relationship = "many-to-many") %>%
                      mutate(Stats = "Attendees") %>%
                      mutate(Division = "Per Region")
StateAttendees_df <- BRL_States %>%
                     inner_join(fulldf_StateAttendeesPerc, by = "Variable") %>%
                     mutate(Stats = "Attendees") %>%
                     mutate(Division = "Per State")


# Combines data frames ~
combined_sfs <- bind_rows(StateAttendees_df, StateMembers_df, RegionAttendees_df, RegionMembers_df)


# Expands combined_sfs by adding the SBBE24 row ~
combined_sfs <- add_row(combined_sfs, name_region = "SBBE24", Division = "Per Region", Stats = "Members")
combined_sfs <- add_row(combined_sfs, name_region = "SBBE26", Division = "Per Region", Stats = "Members")
combined_sfs <- add_row(combined_sfs, name_region = "SBBE24", Division = "Per Region", Stats = "Attendees")
combined_sfs <- add_row(combined_sfs, name_region = "SBBE26", Division = "Per Region", Stats = "Attendees")


# Converts to data frames ~
combined_dfs <- as.data.frame(combined_sfs)
BRL_Regions_df <- as.data.frame(BRL_Regions)


# Merges data frame to perform the change ~
merged_dfs <- left_join(combined_dfs, BRL_Regions_df, by = "name_region", suffix = c("", ".BRL"))


# Performs the change ~
resulting_dfs <- merged_dfs %>%
                 mutate(geometry = ifelse(Division == "Per Region", geometry.BRL, geometry))


# Eliminates unnecessary column ~
resulting_dfs <- resulting_dfs %>% 
                 dplyr::select(-c(n, geometry.BRL))


# Converts data frame back to sf ~
fulldf_map <- st_as_sf(resulting_dfs, crs = st_crs(combined_sfs))


# Renames columns ~
BRL_Regions_Centroids_df <- BRL_Regions_Centroids_df %>%
                            rename(Variable = Region)


# Converts to data frames ~
combined_dfs <- as.data.frame(combined_sfs)
BRL_Regions_df <- as.data.frame(BRL_Regions)


# Merges data frame to perform the change ~
fulldf_map <- left_join(fulldf_map, BRL_Regions_Centroids_df, by = "Variable", suffix = c("", ".SP"))


# Reorders Population ~
fulldf_map$Stats <- factor(fulldf_map$Stats, ordered = TRUE,
                           levels = c("Members",
                                      "Attendees"))


# Reorders Division ~
fulldf_map$Division <- factor(fulldf_map$Division, ordered = T,
                              levels = c("Per Region",
                                         "Per State"))


# Creates the Circular data frame ~
Circular <- subset(fulldfPlots, Stats == "InstitutionMembers") %>% arrange(desc(Percentage))


# Reorders Population ~
Circular$Region <- factor(Circular$Region, ordered = TRUE,
                          levels = c("Norte",
                                     "Nordeste",
                                     "Centro-Oeste",
                                     "Sudeste",
                                     "Sul",
                                     "Exterior"))


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
to_add <- data.frame(matrix(NA, empty_bar * nlevels(Circular$Region), ncol(Circular)))
colnames(to_add) <- colnames(Circular)
to_add$Region <- rep(levels(Circular$Region), each = empty_bar)
Circular <- rbind(Circular, to_add)
Circular <- Circular %>% arrange(Region)
Circular$ID <- seq(1, nrow(Circular))


# Get the name and the y position of each label
label_data_Circular <- Circular
number_of_bar <- nrow(label_data_Circular)
angle <- 90 - 360 * (label_data_Circular$ID - .5) / number_of_bar
label_data_Circular$hjust <- ifelse(angle < -90, 1, 0)
label_data_Circular$angle <- ifelse(angle < -90, angle + 180, angle)


# Prepares a data frame for base lines ~
base_data_Circular <- Circular %>% 
  group_by(Region) %>% 
  summarize(start = min(ID), 
            end = max(ID) - empty_bar, 
            N = n(), .groups = "drop") %>%
  mutate(end = ifelse(N == 1, start + 1, end)) %>%
  mutate(title = (start + end) / 2)


# Prepares a data frame for grid ~
grid_data_Circular <- base_data_Circular
grid_data_Circular$end <- grid_data_Circular$end[ c( nrow(grid_data_Circular), 1:nrow(grid_data_Circular) -1)] + 1
grid_data_Circular$start <- grid_data_Circular$start - 1
grid_data_Circular <- grid_data_Circular[-1, ]


# Reorders Population ~
fulldf_map$name_region <- factor(fulldf_map$name_region, ordered = TRUE,
                          levels = c("Norte",
                                     "Nordeste",
                                     "Centro-Oeste",
                                     "Sudeste",
                                     "Sul",
                                     "Exterior"))


# Adds English & bilingual labels  ~
fulldf_map <- fulldf_map %>%
              mutate(name_region_EN = case_when(name_region == "Norte" ~ "North",
                                                name_region == "Nordeste" ~ "Northeast",
                                                name_region == "Centro-Oeste" ~ "Central-West",
                                                name_region == "Sudeste" ~ "Southeast",
                                                name_region == "Sul" ~ "South",
                                                name_region == "Exterior" ~ "Abroad", TRUE ~ name_region),
               name_region_Bilingual = paste0("<span style='font-size:78pt; color:#000000;'>", name_region, 
                                              "<span style='font-size:78pt; color:#ffffff;'><br>", name_region_EN, "</span>"))


# Gets Gender data frame ~
Gender <- fulldfPlots %>% 
          filter(Stats %in% c("GenderMembers", "StageMembers")) %>%
          droplevels() %>%
          arrange(desc(Percentage))


# Adds English & bilingual labels  ~
fulldf_map <- fulldf_map %>%
              mutate(name_region_Bilingual_2 = paste0("<span style='font-size:56pt; color:#000000;'>", name_region,
                                                      "<span style='font-size:56pt; color:#ffffff;'><br>", name_region_EN, "</span>"))


fulldf_map <- fulldf_map %>%
              mutate(Division_Bilingual = case_when(Division == "Per State" ~ "<span style='font-size:86pt;'>Por Estado</span><br><span style='font-size:86pt; color:#000000;'>Per State</span>",
                                                    Division == "Per Region" ~ "<span style='font-size:86pt;'>Por Região</span><br><span style='font-size:86pt; color:#000000;'>Per Region</span>", TRUE ~ Division))


# Loads Brazilian biome data  ~
biomes_sf <- read_biomes(year = 2019, simplified = TRUE)
world <- ne_countries(scale = "large", returnclass = "sf")
sa_countries <- subset(world, continent == "South America" | sovereignt == "France")


# Loads Oceans ~
oceans <- ne_download(scale = "large", type = "ocean", category = "physical", returnclass = "sf")


# Load Brazil boundary (national borders)
brazil <- read_country(year = 2019, simplified = TRUE)


# Loads Amazonian rivers shape file ~
rivers_world <- st_read("./SHPs/AmazonicRivers/RiosAmazonicosGeorge.shp")


# Make sure CRS matches ~
rivers_world <- st_transform(rivers_world, st_crs(brazil))


# Clips rivers to Brazil boundary ~
rivers_brazil <- st_intersection(rivers_world, brazil)


# Expands BRL_Regions by creating Region ~
biomes_sf$name_biome_EN <- ifelse(biomes_sf$name_biome %in% c("Amazônia"), "Amazon",
                           ifelse(biomes_sf$name_biome %in% c("Cerrado"), "Cerrado",
                           ifelse(biomes_sf$name_biome %in% c("Caatinga"), "Caatinga",
                           ifelse(biomes_sf$name_biome %in% c("Mata Atlântica"), "Atlantic Forest",
                           ifelse(biomes_sf$name_biome %in% c("Pantanal"), "Pantanal",
                           ifelse(biomes_sf$name_biome %in% c("Pampa"), "Pampa",
                           ifelse(biomes_sf$name_biome %in% c("Sistema Costeiro"), "Coastal System", "Error")))))))


# Expands BRL_Regions by creating Region ~
biomes_sf$colours <- ifelse(biomes_sf$name_biome %in% c("Amazônia"), "#006d2c",
                     ifelse(biomes_sf$name_biome %in% c("Cerrado"), "#2ca25f",
                     ifelse(biomes_sf$name_biome %in% c("Caatinga"), "#66c2a4",
                     ifelse(biomes_sf$name_biome %in% c("Mata Atlântica"), "#99d8c9",
                     ifelse(biomes_sf$name_biome %in% c("Pantanal"), "#ccece6",
                     ifelse(biomes_sf$name_biome %in% c("Pampa"), "#edf8fb",
                     ifelse(biomes_sf$name_biome %in% c("Sistema Costeiro"), "#000000", "Error")))))))


# Reorders Population ~
biomes_sf$name_biome_EN <- factor(biomes_sf$name_biome_EN, ordered = TRUE,
                                  levels = c("Amazon",
                                             "Cerrado",
                                             "Caatinga",
                                             "Atlantic Forest",
                                             "Pantanal",
                                             "Pampa", 
                                             "Coastal System"))


# Gets key locations in the routes of the Romantic travelers ~ 
routes <- list(humboldt = data.frame(
  place = c("Cucuí"),
  lon   = c(-66.837834),
  lat   = c(1.190032),
  fill_col = "#3E99B3"),
  darwin = data.frame(place = c("Saint Peter and Saint Paul Archipelago", "Fernando de Noronha", "Salvador", "Rio de Janeiro"),
                      lon   = c(-29.34, -32.42, -38.50, -43.17),
                      lat   = c(0.92, -3.85, -12.97, -22.90),
                      fill_col = "#a83c23"),
  muller = data.frame(place = c("Blumenau"),
                      lon   = c(-49.06),
                      lat   = c(-26.92),
                      fill_col = "#961c6b"),
  wallace = data.frame(place = c("Belém", "Santarém", "Manaus", "Tefé (Ega)"), 
                       lon   = c(-48.8024, -54.7089, -60.425, -64.7089), 
                       lat   = c(-1.4558, -2.4416, -3.119, -3.3544),
                       fill_col = "#4d4584"),
  bates = data.frame(place = c("Belém", "Cametá", "Óbidos", "Manaus"), 
                     lon   = c(-48.2024, -49.4950, -55.5177, -59.825),
                     lat   = c(-1.4558,  -2.2425, -1.8964, -3.119),
                     fill_col = "#7f5a31"))


# Binds data frame ~ 
routesUp <- bind_rows(routes)


# Sets geopgraphical lines ~ 
lat_equator <- 0
lat_tropic  <- -23.43683


# Equator near left
equator <- st_sfc(st_linestring(matrix(c(-90.2, lat_equator,
                                         -84.2, lat_equator), ncol = 2, byrow = TRUE)), crs = 4326)


# Tropic near right
tropic <- st_sfc(st_linestring(matrix(c(-28.0, lat_tropic,
                                        -22.0, lat_tropic), ncol = 2, byrow = TRUE)), crs = 4326)


# Tropic near right ~
equator_sf <- st_sf(name = "Equator", geometry = equator)
tropic_sf  <- st_sf(name = "Tropic of Capricorn", geometry = tropic)


##########            ##########
###         Figure 1         ###
##########            ##########


# Creates Article Map ~
Map_Article_White <-
  ggplot() +
  geom_sf(data = sa_countries, fill = "#f0f0f0", color = "#000000", size = .1) +
  geom_sf(data = subset(biomes_sf, name_biome_EN != "Coastal System"), aes(fill = name_biome_EN), color = "#000000", size = .1) +
  scale_fill_manual(values = c("#01665e", "#5ab4ac", "#d8b365",  "#a1d99b", "#fcc5c0", "#91bfdb"), name = "Biomes of Brazil") +
  guides(fill = guide_legend(title = "Biomes of Brazil", title.theme = element_text(family = "Cormorant", colour = "#000000", size = 135, face = "bold"),
                             label.theme = element_text(family = "Cormorant", colour = "#000000", size = 125),
                             override.aes = list(size = 2, linewidth = .3, colour = "#000000"))) +
  geom_sf(data = rivers_brazil, colour = "#e0f3f8", linewidth = .3) +
  new_scale_fill() +
  geom_point(data = routesUp, aes(x = lon, y = lat, fill = fill_col),
             shape = 21, size = 4, colour = "#000000", stroke = .3, show.legend = FALSE) +
  scale_fill_identity() +
  annotation_scale(data = oceans,
                   text_family = "Cormorant", location = "br", line_width = 1,
                   text_cex = 14, style = "ticks",
                   pad_x = unit(.5, "in"), pad_y = unit(.2, "in")) +
  annotation_north_arrow(data = oceans,
                         location = "br", which_north = "true", style = north_arrow_fancy_orienteering,
                         pad_x = unit(.5, "in"), pad_y = unit(.275, "in")) +
  geom_sf(data = equator_sf, colour = "#000000", linetype = "dotdash", linewidth = .3) +
  geom_sf(data = tropic_sf,  colour = "#000000", linetype = "dotdash", linewidth = .3) +
  coord_sf(xlim = c(-92.2, -20), ylim = c(-35.75, 6.1), expand = FALSE) +
  theme(plot.margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm"),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        legend.position = c(.125, .3),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.key.spacing.y  = unit(.085, "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank())


# Saves Article Map ~
ggsave("./SBBEPlots/SBBECommentaryArticle_Fig1-EN.png", Map_Article_White,
       device = "png", bg = "transparent", limitsize = FALSE, scale = 1, width = 8, height = 8, dpi = 1000)


##########            ##########
###         Figure 2         ###
##########            ##########


# Sets custom x-axis labels ~
xlabel_PT <- c("Per Region" = "Por Região",
               "Per State" = "Por Estado")
ylabel_PT <- c("Members" = "% de Afiliados à SBBE",
               "Attendees" = "% de Participantes no SBBE24")
xlabel_EN <- c("Members" = "% of SBBE Members",
               "Attendees" = "% of SBBE24 Attendees")
ylabel_EN <- c("Members" = "% of SBBE Members",
               "Attendees" = "% of SBBE24 Attendees")


fulldf_map <- fulldf_map %>%
              mutate(Fig2 = case_when(Conference %in% c("SBBE24") ~ "SBBE24 Attendees",
                                      Conference %in% c("SBBE26") ~ "SBBE26 Attendees",
                                      is.na(Conference) & Region == "SBBE24" & Stats %in% c("Attendees", "Members") ~ "SBBE24 Attendees",
                                      is.na(Conference) & Region == "SBBE26" & Stats %in% c("Attendees", "Members") ~ "SBBE26 Attendees",
                                      is.na(Conference) & !Region %in% c("SBBE24", "SBBE26") & Stats %in% c("Members") ~ "SBBE Members", TRUE ~ NA))


# Function to build and save Article Map plots ~
make_map_plot <- function(filename, x_labels, y_labels, region_label_column,
                          filter_abroad_only = TRUE,
                          format = c("pdf", "png")) {
  format <- match.arg(format)
  
  region_abbrs <- c("North" = "N",
                    "Northeast" = "NE",
                    "Central-West" = "CW",
                    "Southeast" = "SE",
                    "South" = "S",
                    "Abroad" = "Abroad")
  
  label_data_abroad <- fulldf_map %>%
    dplyr::filter(Division == "Per Region", Stats == "Members") %>%
    { if (filter_abroad_only) dplyr::filter(., Region == "Exterior") else . } %>%
    dplyr::mutate(Region_Abbr = dplyr::recode(.data[[region_label_column]], !!!region_abbrs, .default = .data[[region_label_column]]))

Map <- ggplot() +
       geom_sf(data = subset(fulldf_map, Division == "Per Region"), aes(fill = Percentage * 100), colour = "#f7fbff") +
       coord_sf(xlim = c(-75.75, -33), ylim = c(-35, 6.5), expand = FALSE) +
       scale_y_continuous(breaks = c(0, -10, -20, -30)) + 
       geom_star(data = subset(fulldf_map, Stats == "Attendees" & Region == "SBBE24" & Fig2 == "SBBE24 Attendees"),
                 aes(x = Longitude, y = Latitude), size = 3.15, starshape = 15, starstroke = .3,
                 fill = "#365338", colour = "#ffffff") +
       geom_star(data = subset(fulldf_map, Stats == "Attendees" & Region == "SBBE26" & Fig2 == "SBBE26 Attendees"),
                 aes(x = Longitude, y = Latitude), size = 3.15, starshape = 15, starstroke = .3,
                 fill = "#e67033", colour = "#ffffff") +
       geom_star(data = subset(fulldf_map, Region == "Exterior"),
                 aes(x = Longitude, y = Latitude, fill = Percentage), size = 25, starshape = 8,
                 starstroke = .3, colour = "#f7fbff") +
       geom_label(data = label_data_abroad,
                         aes(x = Longitude, y = Latitude, label = Region_Abbr),
                         size = 4.25, family = "Cormorant", colour = "#000000") +
       geom_label(data = subset(fulldf_map, Stats == "Attendees" & Region == "SBBE24" & Fig2 == "SBBE24 Attendees"),
                         aes(x = Longitude, y = Latitude, label = Region),
                         nudge_x = 4.8, nudge_y = -1.35,
                         size = 4.25, family = "Cormorant", colour = "#365338") +
       geom_label(data = subset(fulldf_map, Stats == "Attendees" & Region == "SBBE26" & Fig2 == "SBBE26 Attendees"),
                         aes(x = Longitude, y = Latitude, label = Region),
                         nudge_x = 4.9, nudge_y = -1.6,
                         size = 4.25, family = "Cormorant", colour = "#e67033") +
       scale_fill_continuous(low = "#d9d9d9", high = "#252525",
                             breaks = c(10, 20, 30, 40, 50, 60),
                             labels = c("10%", "20%", "30%", "40%", "50%", "60%"),
                             limits = c(0, 70)) +
    facet_wrap(Fig2 ~ .) +
    annotation_scale(data = subset(fulldf_map, Stats == "Members" & Fig2 == "SBBE Members"),
                     text_family = "Cormorant", location = "bl", line_width = .6,
                     height = unit(.2, "cm"),
                     width_hint = .25,
                     text_cex = 1, style = "ticks",
                     pad_x = unit(2.025, "in"), pad_y = unit(.055, "in")) +
    annotation_north_arrow(data = subset(fulldf_map, Fig2 == "SBBE Members"),
                           location = "bl", which_north = "true", style = north_arrow_fancy_orienteering,
                           height = unit(1, "cm"), width = unit(1, "cm"),
                           pad_x = unit(3.1, "in"), pad_y = unit(.17, "in")) +
    theme(panel.border = element_part_rect(side = "tlbr", colour = "#000000", linewidth = .11, fill = NA),
          #panel.border = element_blank(),
          legend.position = "right",
          legend.margin = margin(t = 0, b = 0, r = 0, l = 12),
          legend.box.margin = margin(t = 0, b = 0, r = 0, l = 0),
          panel.background = element_rect(fill = "#ffffff"),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "cm"),
          plot.margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm"),
          axis.title = element_blank(),
          axis.line = element_line(colour = "#000000", linewidth = .25),
          #axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          strip.text.x = element_text(family = "Cormorant", colour = "#000000", size = 20, face = "bold"),
          strip.text.y = element_blank(),
          strip.background = element_rect(colour = "#000000", fill = "#d6d6d6", linewidth = 0.1)) +
          guides(fill = guide_colourbar(title = "", label.theme = element_text(family = "Cormorant", size = 12, face = "bold"),
                                        barwidth = 1.1, barheight = 12, order = 1, frame.linetype = 1,
                                        frame.colour = "#000000", ticks.colour = "#f7fbff",
                                        direction = "vertical", reverse = FALSE, even.steps = TRUE,
                                        draw.ulim = TRUE, draw.llim = TRUE))

  
# Saves plots ~
if (format == "pdf") {ggsave(filename,
                             plot = Map,
                             device = "pdf",
                             limitsize = FALSE,
                             scale = 1,
                             width = 11.6,
                             height = 4,
                             dpi = 600)}
else {ggsave(filename,
             plot = Map,
             device = "png",
             limitsize = FALSE,
             scale = 1,
             width = 12.25,
             height = 6,
             dpi = 600)}}


# Runs function to get the Article Map in different flavour ~ 
make_map_plot("./SBBEPlots/SBBECommentaryArticle_Fig2-EN.pdf", x_labels = xlabel_EN, y_labels = ylabel_EN,
              region_label_column = "name_region_EN", filter_abroad_only = FALSE, format = "pdf")
make_map_plot("./SBBEPlots/SBBECommentaryArticle_Fig2-EN.png", x_labels = xlabel_EN, y_labels = ylabel_EN,
              region_label_column = "name_region_EN", filter_abroad_only = FALSE, format = "png")




#
##
### The END ~~~~~