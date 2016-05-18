# Ranking Nacional de Futsal atualizado... (com os resultados da 5a. rodada)
# obs: n/a

# Ranking TOP 10 até a LNF 2015
var_rank_from  = seq(1,10,by=1)
var_score_from = c('1980','1484','1232','945','895','832','266','263','238','170')
var_teams_from = c('Orlandia','Carlos Barbosa','Corinthians','Sorocaba','Joinville','Jaragua do Sul','Concordia','Santos','Marechal Rondon','Assoeva')

# Ranking TOP 10 até a LNF 2016 (5a. rodada)
var_rank_to  = seq(1,10,by=1)
var_score_to = c('1798','1233','1077','830','824','716','296','270','237','226')
var_teams_to = c('Orlandia','Carlos Barbosa','Corinthians','Joinville','Jaragua do Sul','Sorocaba','Concordia','Marechal Rondon','Assoeva','Santos')

# Cria a base consolidada
df = data.frame(
     Score_from = var_score_from
    ,Team_from = var_teams_from
    ,Ranking_from = var_rank_from

    ,Ranking_to = var_rank_to
    ,Team_to = var_teams_to
    ,Score_to = var_score_to
)
df

funcRCopy(df)
