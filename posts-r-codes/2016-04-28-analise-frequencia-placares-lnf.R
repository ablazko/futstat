# Placares mais frequentes da liga
# obs: depende da criacao do 'df_main'

library(ggplot2)

# Frequencia dos placares em tabela
tmp = df_main[, ':=' (placar = paste(max(goals_home, goals_visitor), min(goals_home, goals_visitor), sep=' x ')), by=1:nrow(df_main)]
tmp[, .N ,by=c('placar')][order(-N)]
rm(tmp)

# Criacao da base temporaria
tmp = df_main[, ':=' (placar = paste(goals_home, goals_visitor, sep=':')), by=1:nrow(df_main)]
tmp = tmp[, list(n = .N) , by=c('placar')]
tmp = tmp[, ':=' ( percentage = (n / sum(n))*100 ) ]

tmp$goals_home = as.numeric(do.call(rbind, strsplit(gsub('\\s', '', tmp$placar),':'))[,1])
tmp$goals_visitor = as.numeric(do.call(rbind, strsplit(gsub('\\s', '', tmp$placar),':'))[,2])

# Grafico geral de todas as ligas
q = ggplot(tmp, aes(x=goals_home, y=goals_visitor, fill = percentage)) +
geom_tile() +
scale_fill_gradient(low = "lightblue", high = "#FF0000")+
geom_text(aes(label = paste0(round(percentage,2), "%")), color = "white", size=3.5) +
scale_x_continuous(limits=c(-0.5,11.5), breaks=0:11, labels=0:11) +
scale_y_continuous(limits=c(-0.5,9), breaks=0:9, labels=0:9) +
xlab("\nGols do TIME DA CASA") + ylab("Gols do TIME VISITANTE\n") +
theme(panel.grid.minor.x=element_blank(),
      panel.grid.minor.y=element_blank(),
      legend.position="none",
      axis.text=element_text(colour="gray40", size=13),
      text=element_text(colour="gray40", size=12)) +
ggtitle('Frequência dos placares das partidas\n(LNFs desde 2010)')
q

# Save the plots
outPath   = '/Users/andre_blazko/Documents/github/futsalmaniablog/posts-img'

ggsave(plot=q
 ,filename=paste0(outPath,"/2016-04-28-frequencia-placares-lnf-img1.png")
 ,width=10, height=6, units="in", dpi=100
)

rm(q)

