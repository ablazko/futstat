# Se fosse por pontos corridos...
# obs: depende da criacao do 'df_main'

library(ggplot2)
library(grid)
library(gridExtra)

# Campeoes vs temporada
v_tmp = c(df_main[ !is.na(champion), list(champion), by=c('season') ])
v_champions = v_tmp[[1]]
v_seasons = v_tmp[[2]]
rm(v_tmp)


# Partida a partida ----

# Funcao para criar a base de cada temporada/campeao
funcTeamDf = function(season_ref='', team_ref='') {

    # Data frame preparation
    df = df_main[ (team_home==team_ref | team_visitor==team_ref) & season==season_ref, ]
    df[, ':=' (
         date = as.Date(paste(substr(date,1,2), substr(date,4,5), season, sep='/'), "%d/%m/%Y")
        ,points = ifelse(winner==team_ref, 3, ifelse(winner=="Empate", 1, 0))
        ,label = ifelse(winner==team_ref, 'Vitória', ifelse(winner=="Empate", 'Empate', 'Derrota'))
        ,matamata = ifelse(phase %in% c('Primeira fase','Segunda fase'), 'no', 'yes')
        ,champion = team_ref
        ,count_wins=ifelse(winner==team_ref, 1, 0)
    )]
    df[, ':=' (
         pct_wins=(count_wins / sum(count_matchs))
    )]
    df = df[, ][order(date)]

    # Calculate the cumulative points for each season
    p_acm = df[, list(points_acm = cumsum(points)), by=c("season") ]
    m_acm = df[, list(match = cumsum(count_matchs)), by=c("season") ]
    df = cbind(df, points_acm=p_acm$points_acm)
    df = cbind(df, match=m_acm$match)
    rm(p_acm,m_acm)
    df = df[, c('points','count_matchs') := NULL]
    return(df)
}

# Cria a base de cada temporada/campeao
df1 = funcTeamDf(season_ref='2010', team_ref='Jaragua do Sul')
df2 = funcTeamDf(season_ref='2011', team_ref='Santos')
df3 = funcTeamDf(season_ref='2012', team_ref='Orlandia')
df4 = funcTeamDf(season_ref='2013', team_ref='Orlandia')
df5 = funcTeamDf(season_ref='2014', team_ref='Sorocaba')
df6 = funcTeamDf(season_ref='2015', team_ref='Carlos Barbosa')

# Junta todas as bases
df = rbindlist(list(df1,df2,df3,df4,df5,df6))
rm(df1,df2,df3,df4,df5,df6)

# Recupera as respectivas informacoes de WINS, LOSSES e TIES
v_pctwins = c(
    funcSummarySeasons(par_scope_ligas='2010')[rank==1,(pct_wins)],
    funcSummarySeasons(par_scope_ligas='2011')[rank==1,(pct_wins)],
    funcSummarySeasons(par_scope_ligas='2012')[rank==1,(pct_wins)],
    funcSummarySeasons(par_scope_ligas='2013')[rank==1,(pct_wins)],
    funcSummarySeasons(par_scope_ligas='2014')[rank==1,(pct_wins)],
    funcSummarySeasons(par_scope_ligas='2015')[rank==1,(pct_wins)]
)
v_pctlosses = c(
    funcSummarySeasons(par_scope_ligas='2010')[rank==1,(pct_losses)],
    funcSummarySeasons(par_scope_ligas='2011')[rank==1,(pct_losses)],
    funcSummarySeasons(par_scope_ligas='2012')[rank==1,(pct_losses)],
    funcSummarySeasons(par_scope_ligas='2013')[rank==1,(pct_losses)],
    funcSummarySeasons(par_scope_ligas='2014')[rank==1,(pct_losses)],
    funcSummarySeasons(par_scope_ligas='2015')[rank==1,(pct_losses)]
)
v_pctties = c(
    funcSummarySeasons(par_scope_ligas='2010')[rank==1,(pct_ties)],
    funcSummarySeasons(par_scope_ligas='2011')[rank==1,(pct_ties)],
    funcSummarySeasons(par_scope_ligas='2012')[rank==1,(pct_ties)],
    funcSummarySeasons(par_scope_ligas='2013')[rank==1,(pct_ties)],
    funcSummarySeasons(par_scope_ligas='2014')[rank==1,(pct_ties)],
    funcSummarySeasons(par_scope_ligas='2015')[rank==1,(pct_ties)]
)
df[, ':=' (
     pct_wins = ifelse(season==2010,v_pctwins[1],ifelse(season==2011,v_pctwins[2],ifelse(season==2012,v_pctwins[3],ifelse(season==2013,v_pctwins[4],ifelse(season==2014,v_pctwins[5],ifelse(season==2015,v_pctwins[6],100))))))
    ,pct_losses = ifelse(season==2010,v_pctlosses[1],ifelse(season==2011,v_pctlosses[2],ifelse(season==2012,v_pctlosses[3],ifelse(season==2013,v_pctlosses[4],ifelse(season==2014,v_pctlosses[5],ifelse(season==2015,v_pctlosses[6],100))))))
    ,pct_ties = ifelse(season==2010,v_pctties[1],ifelse(season==2011,v_pctties[2],ifelse(season==2012,v_pctties[3],ifelse(season==2013,v_pctties[4],ifelse(season==2014,v_pctties[5],ifelse(season==2015,v_pctties[6],100))))))
    )
]

# Formata os campos para serem apresentados no grafico
df[, ':=' (
     pcts_label = paste0('V=', round(pct_wins*100,0), "%\n", ' D=', round(pct_losses*100,0), "%\n", ' E=', round(pct_ties*100,0), "%")
    ,champion=ifelse(champion=='Carlos Barbosa','C. Barbosa',ifelse(champion=='Jaragua do Sul','Jaragua',champion))
    )
]
df[, ':=' (
     tmp_facet=paste('LNF', season, '\n', toupper(champion), '\n\n', pcts_label)
    )
]

# Create the plot :: Partida a partida (3 cores acessas)
var.fsize = 2.5
q = ggplot(df, aes(x=match, y=season))
q = q + geom_line(size=0.2, colour="gray60")
q = q + facet_wrap(~ tmp_facet, nrow=6, ncol=1, scales='free_y', switch='y')
q = q + geom_point(aes(colour=as.factor(label)), size=var.fsize*2, alpha=0.8)
q = q + xlab("\n# da Partida na Liga") + ylab("")
q = q = q + scale_color_manual(values = c("Derrota" = '#D9473D',"Empate" = '#FFC73A',"Vitória" = '#189754'))
q = q + ggtitle(paste('Equipes Campeãs da Liga - Partida a partida'))
q = q + theme(
     plot.background  = element_rect(fill="transparent", colour=NA)
    ,panel.background = element_rect(fill="gray96")
    ,panel.grid.major = element_line(colour="black", size=0.2, linetype="solid")
    ,panel.grid.minor.y=element_blank()
    ,panel.grid.major.y=element_blank()
    ,legend.title=element_blank()
    ,legend.background = element_rect(fill="transparent", colour=NA)
    ,legend.position="top"
    ,legend.text=element_text(colour="gray40", size=var.fsize*4)
    ,axis.text.y=element_blank()
    ,axis.ticks.y=element_blank()
    ,axis.line.y=element_blank()
    ,axis.title.y=element_blank()
    ,axis.title.x=element_text(size=var.fsize*5)
    ,axis.text=element_text(colour="gray40", size=var.fsize*5)
    ,text=element_text(colour="gray40", size=var.fsize*4)
    ,strip.text.y=element_text(size=3*var.fsize, face="bold", angle=180)
    ,strip.background = element_rect(fill="gray96", colour="gray96")
)
q

# Save the plots
outPath   = '/Users/andre_blazko/Documents/github/futsalmaniablog/posts-img'

ggsave(plot=q
   ,filename=paste0(outPath,"/2016-05-12-plot1.png")
   ,width=10, height=7, units="in", dpi=100
)

# Vitorias vs Derrotas vs Empates ----

df2 = df[, list(
          champion=max(champion), Vitória=max(pct_wins), Derrota=max(pct_losses), Empate=max(pct_ties)
      ), by=c('season')
]
df2[, ':=' (champion = ifelse(champion=='C. Barbosa', 'Carlos Barbosa', champion)) ]
df2[, ':=' (champion = ifelse(champion=='Jaragua', 'Jaragua do Sul', champion)) ]
df2[, ':=' (label = paste0(champion,'\n',season)) ]
df2 = melt(df2, id=c("label"), measure=c("Vitória", "Derrota", "Empate"))
df2[, ':=' (variable = paste0(variable,'s')) ]
df2[, ':=' (variable = factor(variable, levels=c('Vitórias','Derrotas','Empates'))) ]
df2[, ':=' (label = factor(label, levels=c('Jaragua do Sul\n2010','Santos\n2011','Orlandia\n2012','Orlandia\n2013','Sorocaba\n2014','Carlos Barbosa\n2015'))) ]

# Create the plot
var.fsize = 2.5
q = ggplot(df2, aes(x=label, y=value, fill=variable))
q = q + geom_bar(stat='identity', alpha=0.7)
q = q + facet_wrap(~ variable, nrow=3, ncol=1, scales='free_y', switch='y')
q = q + geom_text(aes(label=paste0(round(value*100,0),'%')), color="gray20", size=var.fsize*1.5, vjust=2.0)
q = q + xlab("") + ylab("")
q = q + scale_fill_manual(values = c("Derrotas" = '#D9473D',"Empates" = '#FFC73A',"Vitórias" = '#189754'))
q = q + scale_y_continuous(labels=scales::percent, breaks=seq(0,1,by=0.1))
q = q + ggtitle(paste('Equipes Campeãs da Liga\n'))
q = q + theme(
     plot.background  = element_rect(fill="transparent", colour=NA)
    ,panel.background = element_rect(fill="gray96")
    ,panel.grid.major.y=element_blank()
    ,legend.title=element_blank()
    ,legend.background = element_rect(fill="transparent", colour=NA)
    ,legend.position="none"
    ,legend.text=element_text(colour="gray40", size=var.fsize*4)
    ,axis.text.y=element_blank()
    ,axis.ticks.y=element_blank()
    ,axis.line.y=element_blank()
    ,axis.title.y=element_blank()
    ,axis.title.x=element_text(size=var.fsize*5)
    ,axis.text=element_text(colour="gray40", size=var.fsize*5)
    ,text=element_text(colour="gray40", size=var.fsize*4)
    ,strip.text.y=element_text(size=6*var.fsize)
    ,strip.background = element_rect(fill="gray96", colour="gray96")
)
q

# Save the plots
outPath   = '/Users/andre_blazko/Documents/github/futsalmaniablog/posts-img'

ggsave(plot=q
   ,filename=paste0(outPath,"/2016-05-12-plot2.png")
   ,width=10, height=6, units="in", dpi=100
)

# Se fossem pontos corridos, quem seriam os campeões? ----

# Cria as bases de cada temporada
df1 = funcSummarySeasons(par_scope_ligas='2010')[,.(team_name,pct_wins,pct_losses,pct_ties,count_matchs)]
df2 = funcSummarySeasons(par_scope_ligas='2011')[,.(team_name,pct_wins,pct_losses,pct_ties,count_matchs)]
df3 = funcSummarySeasons(par_scope_ligas='2012')[,.(team_name,pct_wins,pct_losses,pct_ties,count_matchs)]
df4 = funcSummarySeasons(par_scope_ligas='2013')[,.(team_name,pct_wins,pct_losses,pct_ties,count_matchs)]
df5 = funcSummarySeasons(par_scope_ligas='2014')[,.(team_name,pct_wins,pct_losses,pct_ties,count_matchs)]
df6 = funcSummarySeasons(par_scope_ligas='2015')[,.(team_name,pct_wins,pct_losses,pct_ties,count_matchs)]

# Calcula os pontos, supondo o desempenho observado de cada equipe
df1 = df1[, ':=' ( Pontos = round(max(count_matchs)*(pct_wins*3 + pct_ties*1),0) ) ][order(-Pontos)]
df2 = df2[, ':=' ( Pontos = round(max(count_matchs)*(pct_wins*3 + pct_ties*1),0) ) ][order(-Pontos)]
df3 = df3[, ':=' ( Pontos = round(max(count_matchs)*(pct_wins*3 + pct_ties*1),0) ) ][order(-Pontos)]
df4 = df4[, ':=' ( Pontos = round(max(count_matchs)*(pct_wins*3 + pct_ties*1),0) ) ][order(-Pontos)]
df5 = df5[, ':=' ( Pontos = round(max(count_matchs)*(pct_wins*3 + pct_ties*1),0) ) ][order(-Pontos)]
df6 = df6[, ':=' ( Pontos = round(max(count_matchs)*(pct_wins*3 + pct_ties*1),0) ) ][order(-Pontos)]

# Ajustes da base
df1[, ':=' ( `Se fossem Pontos corridos...` = paste0(rownames(df1),'o. ',team_name) , season=2010  ) ]
df2[, ':=' ( `Se fossem Pontos corridos...` = paste0(rownames(df2),'o. ',team_name) , season=2011  ) ]
df3[, ':=' ( `Se fossem Pontos corridos...` = paste0(rownames(df3),'o. ',team_name) , season=2012  ) ]
df4[, ':=' ( `Se fossem Pontos corridos...` = paste0(rownames(df4),'o. ',team_name) , season=2013  ) ]
df5[, ':=' ( `Se fossem Pontos corridos...` = paste0(rownames(df5),'o. ',team_name) , season=2014  ) ]
df6[, ':=' ( `Se fossem Pontos corridos...` = paste0(rownames(df6),'o. ',team_name) , season=2015  ) ]



  # Resultado 1 - table1
  # Cria a base com os 3 primeiros colocados
  df1 = df1[1:3, .(`Se fossem Pontos corridos...`,Pontos,season)]
  df2 = df2[1:3, .(`Se fossem Pontos corridos...`,Pontos,season)]
  df3 = df3[1:3, .(`Se fossem Pontos corridos...`,Pontos,season)]
  df4 = df4[1:3, .(`Se fossem Pontos corridos...`,Pontos,season)]
  df5 = df5[1:3, .(`Se fossem Pontos corridos...`,Pontos,season)]
  df6 = df6[1:3, .(`Se fossem Pontos corridos...`,Pontos,season)]
  df_tmp1 = rbindlist(list(df1,df2,df3,df4,df5,df6))
  rm(df1,df2,df3,df4,df5,df6)

  # Cria a base com os 2 primeiros colocados (oficial)
  df_tmp2 = df_main[ !is.na(champion), list(champion, team_home, team_visitor), by=c('season') ]
  df_tmp2[, ':=' ( `Resultado oficial` = ifelse(champion==team_home
                            ,paste0('1o. ',team_home, '\n2o. ',team_visitor)
                            ,paste0('1o. ',team_visitor, '\n2o. ',team_home))
  )]
  df_tmp2 = df_tmp2[, .(`Resultado oficial`, season)]
  df_tmp2

  # Para criar as tabelas no excel
  funcRCopy(df_tmp1)
  funcRCopy(df_tmp2)



  # Resultado 2 - plot3
  # Cria a base com os 5 primeiros colocados
  df1 = df1[1:3, ]
  df2 = df2[1:3, ]
  df3 = df3[1:3, ]
  df4 = df4[1:3, ]
  df5 = df5[1:3, ]
  df6 = df6[1:3, ]
  df1[, ':=' ( Colocacao = as.integer(rownames(df1)) , season=2010  ) ]
  df2[, ':=' ( Colocacao = as.integer(rownames(df2)) , season=2011  ) ]
  df3[, ':=' ( Colocacao = as.integer(rownames(df3)) , season=2012  ) ]
  df4[, ':=' ( Colocacao = as.integer(rownames(df4)) , season=2013  ) ]
  df5[, ':=' ( Colocacao = as.integer(rownames(df5)) , season=2014  ) ]
  df6[, ':=' ( Colocacao = as.integer(rownames(df6)) , season=2015  ) ]
  df_tmp1 = rbindlist(list(df1,df2,df3,df4,df5,df6))
  rm(df1,df2,df3,df4,df5,df6)

  df_tmp1 = melt(df_tmp1, id=c("team_name","season","Colocacao"), measure=c("pct_wins", "pct_losses", "pct_ties"))
  df_tmp1[, ':=' ( team = ifelse(team_name=="Corinthians", paste0(Colocacao,"o.\n      ",team_name), paste0(Colocacao,"o.\n",team_name))  ) ]
  df_tmp1[, ':=' ( variable = ifelse(variable=='pct_wins','Vitórias',ifelse(variable=='pct_losses','Derrotas','Empates'))  ) ]
  df_tmp1[, ':=' ( variable = factor(variable, levels=c('Vitórias','Derrotas','Empates')) ) ]

  # Create the plot
  plotTop = function (var_season)
  {
    var.fsize = 2.5
    q = ggplot(df_tmp1[season==var_season,], aes(x=reorder(team, -Colocacao), y=value, fill=variable))
    q = q + geom_bar(stat='identity', alpha=0.7)
    q = q + coord_flip()
    q = q + facet_grid(season ~ variable, scales='free_x')
    q = q + geom_text(aes(label=paste0(round(value*100,0),'%')), color="black", size=var.fsize*1.3, hjust=-0.2)
    q = q + xlab("") + ylab("")
    q = q + scale_fill_manual(values = c("Derrotas" = '#D9473D',"Empates" = '#FFC73A',"Vitórias" = '#189754'))
    q = q + scale_y_continuous(labels=scales::percent, limits=c(0,0.8), breaks=seq(0,1,by=0.1))
    q = q + theme(
         plot.background  = element_rect(fill="transparent", colour=NA)
        ,panel.background = element_rect(fill="gray96")
        ,panel.grid.major.y=element_blank()
        ,legend.title=element_blank()
        ,legend.background = element_rect(fill="transparent", colour=NA)
        ,legend.position="none"
        ,legend.text=element_text(colour="gray40", size=var.fsize*4)
        ,axis.ticks.y=element_blank()
        ,axis.line.y=element_blank()
        ,axis.title.y=element_blank()
        ,axis.text.x=element_blank()
        ,axis.ticks.x=element_blank()
        ,axis.title.x=element_blank()
        ,axis.text=element_text(colour="gray40", size=var.fsize*4)
        ,text=element_text(colour="gray40", size=var.fsize*4)
        ,strip.text.y=element_text(size=5*var.fsize)
        ,strip.background = element_rect(fill="white", colour="white")
    )
    if (var_season==2010) {
      q = q + theme(strip.text.x=element_text(size=5*var.fsize))
    } else
    {
      q = q + theme(strip.text.x=element_blank())
    }
    return(q)
  }

  q = grid.arrange(ncol=1, top='Os 3 primeiros colocados se a Liga Futsal fosse por pontos corridos\n'
    ,heights=unit(c(4,3.3,3.3,3.3,3.3,3.3), c("cm"))
    ,plotTop(var_season=2010)
    ,plotTop(var_season=2011)
    ,plotTop(var_season=2012)
    ,plotTop(var_season=2013)
    ,plotTop(var_season=2014)
    ,plotTop(var_season=2015)
  )

  # Save the plots
  outPath   = '/Users/andre_blazko/Documents/github/futsalmaniablog/posts-img'

  ggsave(plot=q
     ,filename=paste0(outPath,"/2016-05-12-plot3.png")
     ,width=10, height=9, units="in", dpi=100
  )






