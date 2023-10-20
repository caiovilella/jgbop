##################################################
##          Baseline's outcomes.                ##
##################################################

jgbop.base.longo %>% filter( name == "Y" ) %>%
  ggplot( aes( x = period, y = value ) ) +
  # ylim(50,60)+
  geom_line( ) +
  ggtitle( "Renda" )


jgbop.base.longo %>% filter( name == "pi" ) %>%
  ggplot( aes( x = period, y = value ) ) +
  # ylim(0.04,0.06)+
  geom_line( ) +
  ggtitle( "Inflação" )

jgbop.base.longo %>% filter( name == "er" ) %>%
  ggplot( aes( x = period, y = value ) ) +
  geom_line( ) +
  # ylim(0.9,1)+
  ggtitle( "Câmbio Real" )


jgbop.base.longo %>% filter (name %in% c("Y","Yk")) %>%
  ggplot(aes( x = period, y = value, group = name, color =name))+
  geom_line()+
  ylim(50, 80)+
  ggtitle( "PIB e PIB potencial")


jgbop.base %>% mutate (wf = omega_f0 - varphi2 * er, 
                       ww = omega_w0 + mu2 * er) %>% 
  pivot_longer( cols=-period) %>%
  filter(name %in% c("omega", "wf", "ww")) %>%
  ggplot(aes( x = period, y = value, group = name, color =name))+
  geom_line()+
  ylim(0.15,0.45)+
  ggtitle( "Metas saláriais e Salário real efetivo" )

jgbop.base %>% 
  pivot_longer( cols=-period) %>%
  filter(name == "K_flow") %>%
  ggplot(aes( x = period, y = value, group = name, color =name))+
  geom_line()+
  ggtitle( "Fluxo de Capital" )

jgbop.base.longo %>% filter (name %in% c("Rcb","net_k_flow")) %>%
  ggplot(aes( x = period, y = value, group = name, color =name))+
  geom_line()+
  ggtitle( "Reservas e Saída de capital")

##################################################
##  Shock to full employment JG or Gov expenses ##
##################################################
jg <- wg_shock  %>% pivot_longer(cols = -period )  %>%mutate(Cenário = "Garantia de Empregos")
jg2<- wg_shock2b %>% pivot_longer(cols = -period ) %>% mutate(Cenário = "Garantia de Empregos Salário alto")
g <- g_scenario %>% pivot_longer(cols=-period) %>%  mutate(Cenário = "Estímulo de demanda")
base <- wg_no_shock  %>%pivot_longer(cols=-period) %>% mutate(Cenário = "Cenário Base")
dados <- rbind(jg,g,base)
dados_g <- rbind(g, base)
dados_jg<- rbind(jg,base)
dados2 <- rbind(jg,g,base,jg2)



jg_tx <- wg_shock / wg_no_shock - 1
jg_tx <- jg_tx[,-1] %>% mutate (period = 1:100) %>% pivot_longer(cols=-period) %>% mutate(Cenário = "Garantia de Empregos")
g_tx <- g_scenario / wg_no_shock -1
g_tx<- g_tx[,-1] %>% mutate(period = 1:100) %>% pivot_longer(cols=-period) %>% mutate(Cenário = "Estímulo de Demanda")
jg2_tx <- wg_shock2b / wg_no_shock - 1
jg2_tx <- jg2_tx[,-1] %>% mutate (period = 1:100) %>% pivot_longer(cols=-period) %>% mutate(Cenário = "Garantia de Empregos Salário Alto")

g_alpha_tx <- g_scenario_alpha / wg_no_shock_alpha -1
g_alpha_tx<- g_alpha_tx[,-1] %>% mutate(period = 1:100) %>% pivot_longer(cols=-period) %>% mutate(Cenário = "Estímulo de Demanda com menor produtividade")
jg_alpha_tx <- wg_shock_alpha / wg_no_shock_alpha - 1
jg_alpha_tx <- jg_alpha_tx[,-1] %>% mutate (period = 1:100) %>% pivot_longer(cols=-period) %>% mutate(Cenário = "Garantia de Empregos com menor produtividade")
dados3_tx <- rbind(jg_tx, g_alpha_tx)  


g_alpha <-g_scenario_alpha %>% pivot_longer(cols=-period) %>%  mutate(Cenário = "Estímulo de demanda com menor produtividade")
jg_alpha <- wg_shock_alpha  %>% pivot_longer(cols = -period )  %>%mutate(Cenário = "Garantia de Empregos com menor produtividade")
base_alpha <- wg_no_shock_alpha  %>%pivot_longer(cols=-period) %>% mutate(Cenário = "Cenário Base com menor produtividade")
dados3 <- rbind(jg_alpha,base_alpha,g_alpha)

dados_tx <- rbind(jg_tx, g_tx)
dados2_tx <- rbind(jg_tx, g_tx, jg2_tx)  

#Income
dados_tx%>% filter (name=="Y") %>%
  ggplot( aes ( x= period, y = value, group = Cenário,
                linetype = Cenário, color = Cenário) )+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("green", "blue"))+
  scale_linetype_manual(values = c("dashed", "longdash"))+
  labs(y="", x="Períodos")+
  # ggtitle("Crescimento da Renda")+
  theme(legend.position = "bottom")+
  geom_line()

#Gastos do Governo
dados_tx %>% filter(name == "Gastos_gov") %>%
  ggplot( aes ( x= period, y = value, group = Cenário,
                linetype = Cenário, color = Cenário) )+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("green", "blue"))+
  scale_linetype_manual(values = c("dashed", "longdash"))+
  labs(y="", x="Períodos")+
  # ggtitle("Crescimento dos gastos")+
  theme(legend.position = "bottom")+
  geom_line()

#Gráfico combinado Y + Gastos 
###############################
# TESE
##############################
dados_tx %>% filter(name %in% c("M", "C", "Y", "Gastos_gov")) %>%
  mutate(name = case_when(
    name == "M" ~ "Importações",
    name == "C" ~ "Consumo",
    name == "Gastos_gov" ~ "Gastos Públicos",
    name == "Y" ~ "Produto",
    TRUE ~ name
  )) %>%
  mutate (name = factor(name, levels = c("Produto", "Gastos Públicos", "Consumo", "Importações"))) %>%
  ggplot(aes ( x= period, y = value, group = Cenário,
               linetype = Cenário, color = Cenário) )+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("green", "blue"))+
  scale_linetype_manual(values = c( "dashed", "longdash"))+
  facet_wrap(~name, nrow=2, ncol=2, scale = "free")+
  labs(y="", x="Períodos")+
  # ggtitle("Crescimento dos gastos, Gastos do Governo, Consumo e Conta Comercial")+
  theme(legend.position = "bottom")+
  geom_line()


dados_tx %>% filter(name %in% c("M", "C", "Y", "Gastos_gov")) %>%
  mutate(name = case_when(
    name == "M" ~ "Importações",
    name == "C" ~ "Consumo",
    name == "Gastos_gov" ~ "Gastos Públicos",
    name == "Y" ~ "Produto",
    TRUE ~ name
  )) %>%
  mutate (name = factor(name, levels = c("Produto", "Gastos Públicos", "Consumo", "Importações"))) %>%
  ggplot(aes ( x= period, y = value, group = Cenário,
               linetype = Cenário, color = Cenário) )+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("green", "blue", "skyblue"))+
  scale_linetype_manual(values = c( "dashed", "longdash", "dotted"))+
  facet_wrap(~name, nrow=2, ncol=2, scale = "free")+
  labs(y="", x="Períodos")+
  # ggtitle("Crescimento dos gastos, Gastos do Governo, Consumo e Conta Comercial")+
  theme(legend.position = "bottom")+
  geom_line()

#Current account
dados %>% filter (name=="CAY") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  # ggtitle("Current Account (% GDP)")+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  theme(legend.position = "bottom")+
  labs(y="", x = "Períodos")+
  scale_y_continuous(labels = scales::percent)+
  geom_line()

#Foreign reserves
dados %>% filter (name=="RcbY") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Foreign reserves (% GDP)")+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  theme(legend.position = "bottom")+
  geom_line()

dados %>% filter (name=="d_rcbY") %>%
  ggplot( aes ( x= period, y = value, group = modelo, 
                linetype = modelo, color = modelo) )+
  # ggtitle("Variação das reservas internacionais (% PIB)")+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  theme(legend.position = "bottom")+
  labs(y="")+
  geom_line()

dados %>% filter (name%in% c("d_rcbM", "d_rcbY")) %>%
  mutate(name = case_when(
    name == "d_rcbY" ~ "Variação das Reservas (% PIB)",
    name == "d_rcbM" ~ "Variação das Reservas (% Importações)",
    TRUE ~ name
  )) %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  # ggtitle("Variação das reservas internacionais (% Importações)")+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~name, ncol=2, scales="free") +
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  theme(legend.position = "bottom")+
  labs(y="", x= "Períodos")+
  geom_line()

#Bonds held by foreigners
jg %>% filter (name %in% c("f1","f2", "f3")) %>%
  ggplot( aes ( x= period, y = value, group = name, 
                linetype = name, color = name) )+
  ggtitle("CA = f1, Bs = f2, Fs = f3")+
  # scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "bottom")+
  geom_line()

#Real exchange rate
dados %>% filter (name=="er") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  # ggtitle("Taxa de câmbio real")+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  theme(legend.position = "bottom")+
  labs(y="", x="Períodos")+
  geom_line()

dados_tx %>% filter (name=="er") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  # ggtitle("Taxa de câmbio real")+
  scale_color_manual(values = c("green", "blue"))+
  scale_linetype_manual(values = c("dashed", "longdash"))+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  labs(y="", x= "Períodos")+
  geom_line()


# Inflation
dados %>% filter (name=="pi") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  # ggtitle("Taxa de inflação")+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  theme(legend.position = "bottom")+
  labs(y="", x= "Períodos")+
  scale_y_continuous(labels = scales::percent)+
  # ylim(0.04,0.06)+
  geom_line()

# Phillips Curve
dados %>% filter (name=="mu1") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  # ggtitle("Poder de Barganha dos Trabalhadores")+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  theme(legend.position = "bottom")+
  labs(y="", x="Períodos")+
  # ylim(0.04,0.06)+
  geom_line()


#Nominal Exchange rate
dados %>% filter(name == "e") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  ggtitle("Cambio nominal")+
  geom_line()

#Capital Flow
dados %>% filter(name == "net_k_flow") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  ggtitle("Fluxo Líquido de Capital Internacional")+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  geom_line()

#Exportações e Importaçoes
dados_tx %>% filter(name == "X") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  scale_y_continuous(labels= scales::percent)+
  scale_color_manual(values = c( "green", "blue"))+
  scale_linetype_manual(values = c("dashed", "longdash"))+
  theme(legend.position = "bottom")+
  ggtitle("Exportações (%)")+
  geom_line()

dados_tx %>% filter(name == "M") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_y_continuous(labels= scales::percent)+
  scale_color_manual(values = c( "green", "blue"))+
  scale_linetype_manual(values = c("dashed", "longdash"))+
  ggtitle("Importações (%)")+
  geom_line()

#Balanço Comercial
dados %>% filter(name == "comercial") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  ggtitle("Balanço comercial")+
  geom_line()

#Balanço de Rendas
dados %>% filter(name == "rendas") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  ggtitle("Balanço de rendas")+
  geom_line()

# Conta Corrente (Volume)
dados %>% filter(name == "CAr") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  ggtitle("Conta Corrente (Volume)")+
  geom_line()

#Distributive conflict

#Markup ratio
dados %>% filter (name=="tau") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Taxa Markup")+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  theme(legend.position = "bottom")+
  # ylim(0.6,0.7)+
  geom_line()

#Componentes do mark-up efetivo
dados %>% filter (name %in% c("tau", "varphi1", "omega", "er")) %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Mark up componentes")+
  facet_wrap(~name, nrow=2, ncol=2, scale= "free")+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  theme(legend.position = "bottom")+
  geom_line()

#############################
# Tese             #
############################
# Mark-up e Poder de barganha
dados %>%
  filter(name %in% c("tau", "mu1", "omega", "er")) %>%
  mutate(name = case_when(
    name == "tau" ~ "Mark-up efetivo",
    name == "mu1" ~ "Poder de Barganha dos Trabalhadores ",
    name == "omega" ~ "Salário real",
    name == "er" ~ "Câmbio real",
    TRUE ~ name
  )) %>%
  mutate(name = factor(name, levels = c("Mark-up efetivo", "Poder de Barganha dos Trabalhadores ",
                                        "Câmbio real", "Salário real"))) %>%
  ggplot(aes(x = period, y = value, group = Cenário, 
             linetype = Cenário, color = Cenário)) +
  facet_wrap(~name, nrow = 2, ncol = 2, scale = "free") +
  scale_color_manual(values = c("orange", "green", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed", "longdash")) +
  theme(legend.position = "bottom") +
  labs(y="", x="Períodos")+
  geom_line()


#Wageshare
dados %>% filter (name=="wage_share") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Wage Share")+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  scale_y_continuous(labels = scales::percent)+
  geom_line()

graf1 <- g %>% filter (name %in% c("wage_share", "profit_share")) %>%
  rename("variáveis" = "name")%>%
  ggplot( aes ( x= period, y = value, col = variáveis) )+
  ylim(0.32,0.46)+
  labs(x="Estímulo de Demanda", y="")+
  theme(legend.direction = "horizontal")+
  geom_line()
graf2 <- jg %>% filter (name %in% c("wage_share", "profit_share")) %>%
  ggplot( aes ( x= period, y = value, col = name) )+
  theme(legend.position = "none")+
  ylim(0.32,0.46)+
  labs(x="Garantia de Empregos", y="")+
  geom_line(linetype = 1)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend<- get_legend(graf1)
graf1 <- graf1 + theme(legend.position = "none")
grid.arrange(graf1, graf2,legend, ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))

#Renda média das famílias
dados_tx %>% filter(name=="wbar") %>%
  ggplot(aes(x=period, y=value, group=Cenário,
             linetype= Cenário, color=Cenário))+
  ggtitle("Renda média das famílias")+
  scale_color_manual(values = c( "green", "blue"))+
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "bottom")+
  labs(x="Períodos", y= "" )+
  geom_line()



# Dívida/PIB
dados %>% filter(name=="BY") %>%
  ggplot(aes(x=period, y=value, group=Cenário,
             linetype= Cenário, color=Cenário))+
  #ggtitle("Dívida Sobre PIB")+
  scale_color_manual(values = c( "orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "bottom")+
  labs(x="Períodos", y= "" )+
  geom_line()

dados %>% filter(name %in% c("BY", "CAY")) %>%
  mutate(name = case_when(
    name == "BY" ~ "Dívida Pública Doméstica (%PIB)",
    name == "CAY" ~ "Conta Corrente (%PIB)",
    TRUE ~ name
  )) %>%
  ggplot(aes ( x= period, y = value, group = Cenário,
               linetype = Cenário, color = Cenário) )+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  facet_wrap(~name, ncol=2, scale = "free")+
  labs(y="", x="Períodos")+
  # ggtitle("Dívida pública e Conta Corrente (%PIB)")+
  theme(legend.position = "bottom")+
  geom_line()

#Capacity utilization
dados %>% filter (name=="u") %>%
  ggplot( aes ( x= period, y = value, group = Cenário,
                linetype = Cenário, color = Cenário) )+
  ggtitle("Grau de utilização do estoque de capital")+
  scale_color_manual(values = c("orange", "green", "blue"))+
  scale_linetype_manual(values = c("solid", "dashed", "longdash"))+
  theme(legend.position = "bottom")+
  geom_line()









###############################################
#      Changing Z_B from 0.2 to 0.1           #
###############################################
jg_mu_b <- wg_mu_b_shock %>% pivot_longer(cols=-period) %>%
  mutate(Cenário = "Garantia de Empregos")
dados_mu_b <- rbind(jg_mu_b,g,base)

jg_mu_b_tx <- wg_mu_b_shock / wg_no_shock - 1
jg_mu_b_tx <- jg_mu_b_tx[,-1] %>% mutate (period = 1:100) %>%
  pivot_longer(cols=-period) %>% mutate(Cenário = "Garantia de Empregos")
dados_mu_b_tx <- rbind(jg_mu_b_tx, g_tx)


#Income
dados_mu_b_tx %>% filter (name=="Y") %>%
  ggplot( aes ( x= period, y = value, group = Cenário,
                linetype = Cenário, color = Cenário) )+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "dashed", "longdash"))+
  scale_colour_manual(values = c( "green", "blue"))+
  ggtitle("Crescimento da Renda")+
  theme(legend.position = "bottom")+
  geom_line()


#Current account
dados_mu_b %>% filter (name=="CAY") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Current Account (% GDP)")+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()

#Foreign reserves
dados_mu_b %>% filter (name=="RcbY") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Reservas Internacionais (% PIB)")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  theme(legend.position = "bottom")+
  geom_line()

dados_mu_b %>% filter (name=="d_rcbY") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Reservas Internacionais (% PIB)")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  theme(legend.position = "bottom")+
  geom_line()

#Real exchange rate
dados_mu_b %>% filter (name=="er") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Taxa de câmbio real")+
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()

# Inflation
dados_mu_b %>% filter (name=="pi") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Taxa de inflação")+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()

# Phillips Curve
dados_mu_b %>% filter (name=="mu1") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Poder de Barganha dos Trabalhadores")+
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()


#Nominal Exchange rate
dados_mu_b %>% filter(name == "e") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  ggtitle("Cambio nominal")+
  geom_line()

#Capital Flow
dados_mu_b %>% filter(name == "net_k_flow") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  ggtitle("Fluxo de Capital Internacional")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()

#Exportações e Importaçoes
dados_mu_b_tx %>% filter(name == "X") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  scale_y_continuous(labels= scales::percent)+
  scale_linetype_manual(values = c( "dashed", "longdash"))+
  scale_colour_manual(values = c( "green", "blue"))+
  theme(legend.position = "bottom")+
  ggtitle("Exportações (%)")+
  geom_line()

dados_mu_b_tx %>% filter(name == "M") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_y_continuous(labels= scales::percent)+
  scale_linetype_manual(values = c( "dashed", "longdash"))+
  scale_colour_manual(values = c( "green", "blue"))+
  ggtitle("Importações (%)")+
  geom_line()

#Balanço Comercial
dados_mu_b %>% filter(name == "comercial") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange", "green", "blue"))+
  ggtitle("Balanço comercial")+
  geom_line()

#Balanço de Rendas
dados_mu_b %>% filter(name == "rendas") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange", "green", "blue"))+
  ggtitle("Balanço de rendas")+
  geom_line()

# Conta Corrente (Volume)
dados_mu_b %>% filter(name == "CAr") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange", "green", "blue"))+
  ggtitle("Conta Corrente (Volume)")+
  geom_line()

#Distributive conflict

#Markup ratio
dados_mu_b %>% filter (name=="tau") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Taxa Markup")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange", "green", "blue"))+
  theme(legend.position = "bottom")+
  # ylim(0.6,0.7)+
  geom_line()

#Wageshare
dados_mu_b %>% filter (name=="wage_share") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype= Cenário, color = Cenário) )+
  ggtitle("Wage Share")+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange", "green", "blue"))+
  geom_line()

graf1 <- g %>% filter (name %in% c("wage_share", "profit_share")) %>%
  rename("variáveis" = "name") %>%
  ggplot( aes ( x= period, y = value, col = variáveis) )+
  ylim(0.36,0.46)+
  labs(x="Estímulo de Demanda", y="")+
  theme(legend.direction = "horizontal")+
  geom_line()
graf2 <- jg_mu_b %>% filter (name %in% c("wage_share", "profit_share")) %>%
  ggplot( aes ( x= period, y = value, col = name) )+
  theme(legend.position = "none")+
  ylim(0.36,0.46)+
  labs(x="Garantia de Empregos", y="")+
  geom_line(linetype = 1)
legend<- get_legend(graf1)
graf1 <- graf1 + theme(legend.position = "none")
grid.arrange(graf1, graf2,legend, ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))



#Capacity utilization
dados_mu_b %>% filter (name=="u") %>%
  ggplot( aes ( x= period, y = value, group = Cenário,
                linetype = Cenário, color = Cenário) )+
  ggtitle("Grau de utilização do estoque de capital")+
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange", "green", "blue"))+
  geom_line()


###########################################################
##      Changing the mu_b from 0.02 to 0.04             ##
###########################################################

jg_mu_b2 <- wg_mu_b_shock2 %>% pivot_longer(cols=-period) %>% 
  mutate(Cenário = "Garantia de Empregos")
dados_mu_b2 <- rbind(jg_mu_b2,g,base)

jg_mu_b_tx2 <- wg_mu_b_shock2 / wg_no_shock - 1
jg_mu_b_tx2 <- jg_mu_b_tx2[,-1] %>% mutate (period = 1:100) %>%
  pivot_longer(cols=-period) %>% mutate(Cenário = "Garantia de Empregos")
dados_mu_b_tx2 <- rbind(jg_mu_b_tx2, g_tx)


#Income
dados_mu_b_tx2 %>% filter (name=="Y") %>%
  ggplot( aes ( x= period, y = value, group = Cenário,
                linetype = Cenário, color = Cenário) )+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "dashed", "longdash"))+
  scale_colour_manual(values = c( "green", "blue"))+
  ggtitle("Crescimento da Renda")+
  theme(legend.position = "bottom")+
  geom_line()


#Current account
dados_mu_b2 %>% filter (name=="CAY") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Current Account (% GDP)")+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()

#Foreign reserves
dados_mu_b2 %>% filter (name=="RcbY") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Reservas Internacionais (% PIB)")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  theme(legend.position = "bottom")+
  geom_line()

dados_mu_b2 %>% filter (name=="d_rcbY") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Reservas Internacionais (% PIB)")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  theme(legend.position = "bottom")+
  geom_line()

#Real exchange rate
dados_mu_b2 %>% filter (name=="er") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Taxa de câmbio real")+
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()

# Inflation
dados_mu_b2 %>% filter (name=="pi") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Taxa de inflação")+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()

# Phillips Curve
dados_mu_b2 %>% filter (name=="mu1") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Poder de Barganha dos Trabalhadores")+
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()


#Nominal Exchange rate
dados_mu_b2 %>% filter(name == "e") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  ggtitle("Cambio nominal")+
  geom_line()

#Capital Flow
dados_mu_b2 %>% filter(name == "net_k_flow") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  ggtitle("Fluxo de Capital Internacional")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()

#Exportações e Importaçoes
dados_mu_b_tx2 %>% filter(name == "X") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  scale_y_continuous(labels= scales::percent)+
  scale_linetype_manual(values = c( "dashed", "longdash"))+
  scale_colour_manual(values = c( "green", "blue"))+
  theme(legend.position = "bottom")+
  ggtitle("Exportações (%)")+
  geom_line()

dados_mu_b_tx2 %>% filter(name == "M") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_y_continuous(labels= scales::percent)+
  scale_linetype_manual(values = c( "dashed", "longdash"))+
  scale_colour_manual(values = c( "green", "blue"))+
  ggtitle("Importações (%)")+
  geom_line()

#Balanço Comercial
dados_mu_b2 %>% filter(name == "comercial") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange", "green", "blue"))+
  ggtitle("Balanço comercial")+
  geom_line()

#Balanço de Rendas
dados_mu_b2 %>% filter(name == "rendas") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange", "green", "blue"))+
  ggtitle("Balanço de rendas")+
  geom_line()

# Conta Corrente (Volume)
dados_mu_b2 %>% filter(name == "CAr") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange", "green", "blue"))+
  ggtitle("Conta Corrente (Volume)")+
  geom_line()

#Distributive conflict

#Markup ratio
dados_mu_b2 %>% filter (name=="tau") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Taxa Markup")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange", "green", "blue"))+
  theme(legend.position = "bottom")+
  # ylim(0.6,0.7)+
  geom_line()

#Wageshare
dados_mu_b2 %>% filter (name=="wage_share") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype= Cenário, color = Cenário) )+
  ggtitle("Wage Share")+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange", "green", "blue"))+
  geom_line()

graf1 <- g %>% filter (name %in% c("wage_share", "profit_share")) %>%
  rename("variáveis" = "name") %>%
  ggplot( aes ( x= period, y = value, col = variáveis) )+
  ylim(0.32,0.49)+
  labs(x="Estímulo de Demanda", y="")+
  theme(legend.direction = "horizontal")+
  geom_line()
graf2 <- jg_mu_b2 %>% filter (name %in% c("wage_share", "profit_share")) %>%
  ggplot( aes ( x= period, y = value, col = name) )+
  theme(legend.position = "none")+
  ylim(0.32,0.49)+
  labs(x="Garantia de Empregos", y="")+
  geom_line(linetype = 1)
legend<- get_legend(graf1)
graf1 <- graf1 + theme(legend.position = "none")
grid.arrange(graf1, graf2,legend, ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))



#Capacity utilization
dados_mu_b2 %>% filter (name=="u") %>%
  ggplot( aes ( x= period, y = value, group = Cenário,
                linetype = Cenário, color = Cenário) )+
  ggtitle("Grau de utilização do estoque de capital")+
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  scale_colour_manual(values = c( "orange", "green", "blue"))+
  geom_line()



###########################################################
##      Changing the mu_b from 0.02 to 0.08             ##
###########################################################

jg1 <- wg_shock %>% pivot_longer(cols=-period) %>% mutate(Cenário = "mu_b = 0.2")
jg2 <- wg_mu_b_shock %>% pivot_longer(cols=-period) %>% mutate(Cenário = "mu_b= 0.1")
jg3<- wg_mu_b_shock2 %>% pivot_longer(cols=-period) %>% mutate(Cenário = "mu_b= 0.4")
jg4 <- wg_mu_b_shock3 %>% pivot_longer(cols=-period) %>% mutate(Cenário = "mu_b= 0.8")
dados_mu_b3 <- rbind(jg2, jg1, jg3, jg4)

jg_mub_tx3 <- wg_mu_b_shock3 / wg_no_shock - 1
jg_mub_tx3 <- jg_mub_tx3[,-1] %>% mutate (period = 1:100) %>% pivot_longer(cols=-period) %>% mutate(Cenário = "mu_b = 0.8")
jg_mub_tx2 <- wg_mu_b_shock2 / wg_no_shock -1
jg_mub_tx2 <- jg_mub_tx2[,-1] %>% mutate (period = 1:100) %>% pivot_longer(cols=-period) %>% mutate(Cenário = "mu_b = 0.4")
jg_mub_tx <- wg_shock / wg_no_shock -1
jg_mub_tx <- jg_mub_tx[,-1] %>% mutate (period = 1:100) %>% pivot_longer(cols=-period) %>% mutate(Cenário = "mu_b = 0.2")
dados_mub_tx3 <- rbind(jg_mub_tx3, jg_mub_tx2, jg_mub_tx)

rbind(jg4, jg1) %>% filter(name == "comercial") %>%
  ggplot(aes (x = period, y = value,  color = Cenário)) +
  ggtitle("Conta Comercial")+
  geom_line()

#Income
dados_mub_tx3 %>% filter (name=="Y") %>%
  ggplot( aes ( x= period, y = value, group = Cenário,
                linetype = Cenário, color = Cenário) )+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "dashed", "longdash", "dotted"))+
  scale_colour_manual(values = c( "green", "blue", "orange"))+
  ggtitle("Crescimento da Renda")+
  theme(legend.position = "bottom")+
  geom_line()


#Current account
dados_mu_b3 %>% filter (name=="CAY") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Current Account (% GDP)")+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  # scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  # scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()

#Foreign reserves
dados_mu_b3 %>% filter (name=="RcbY") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Reservas Internacionais (% PIB)")+
  scale_y_continuous(labels = scales::percent)+
  # scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  # scale_colour_manual(values = c( "orange","green", "blue"))+
  theme(legend.position = "bottom")+
  geom_line()

dados_mu_b3 %>% filter (name=="d_rcbY") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Reservas Internacionais (% PIB)")+
  scale_y_continuous(labels = scales::percent)+
  # scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  # scale_colour_manual(values = c( "orange","green", "blue"))+
  theme(legend.position = "bottom")+
  geom_line()

#Real exchange rate
dados_mu_b3 %>% filter (name=="er") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Taxa de câmbio real")+
  theme(legend.position = "bottom")+
  # scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  # scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()

# Inflation
dados_mu_b3 %>% filter (name=="pi") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Taxa de inflação")+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  # scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  # scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()

# Phillips Curve
dados_mu_b3 %>% filter (name=="mu1") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = Cenário, color = Cenário) )+
  ggtitle("Poder de Barganha dos Trabalhadores")+
  theme(legend.position = "bottom")+
  # scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  # scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()


#Nominal Exchange rate
dados_mu_b3 %>% filter(name == "e") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  # scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  # scale_colour_manual(values = c( "orange","green", "blue"))+
  ggtitle("Cambio nominal")+
  geom_line()

#Capital Flow
dados_mu_b3 %>% filter(name == "net_k_flow") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  ggtitle("Fluxo de Capital Internacional")+
  # scale_linetype_manual(values = c( "solid", "dashed", "longdash"))+
  # scale_colour_manual(values = c( "orange","green", "blue"))+
  geom_line()

#Exportações e Importaçoes
# dados_mu_b_tx2 %>% filter(name == "X") %>%
#   ggplot(aes(x= period, y = value, linetype = modelo, col = modelo)) +
#   scale_y_continuous(labels= scales::percent)+
#   scale_linetype_manual(values = c( "dashed", "longdash"))+
#   scale_colour_manual(values = c( "green", "blue"))+
#   theme(legend.position = "bottom")+
#   ggtitle("Exportações (%)")+
#   geom_line()

# dados_mu_b_tx2 %>% filter(name == "M") %>%
#   ggplot(aes(x= period, y = value, linetype = modelo, col = modelo)) +
#   theme(legend.position = "bottom")+
#   scale_y_continuous(labels= scales::percent)+
#   scale_linetype_manual(values = c( "dashed", "longdash"))+
#   scale_colour_manual(values = c( "green", "blue"))+
#   ggtitle("Importações (%)")+
#   geom_line()

#Balanço Comercial
dados_mu_b3 %>% filter(name == "comercial") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  ggtitle("Balanço comercial")+
  geom_line()

#Balanço de Rendas
dados_mu_b3 %>% filter(name == "rendas") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  ggtitle("Balanço de rendas")+
  geom_line()

# Conta Corrente (Volume)
dados_mu_b3 %>% filter(name == "CAr") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  ggtitle("Conta Corrente (Volume)")+
  ylim(-2,0)+
  geom_line()

#Distributive conflict

#Markup ratio
dados_mu_b3 %>% filter (name=="tau") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype = modelo, color = modelo) )+
  ggtitle("Taxa Markup")+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "bottom")+
  # ylim(0.6,0.7)+
  geom_line()

#Wageshare
dados_mu_b3 %>% filter (name=="wage_share") %>%
  ggplot( aes ( x= period, y = value, group = Cenário, 
                linetype= Cenário, color = Cenário) )+
  ggtitle("Wage Share")+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  geom_line()

graf1 <- g %>% filter (name %in% c("wage_share", "profit_share")) %>%
  rename("variáveis" = "name") %>%
  ggplot( aes ( x= period, y = value, col = name) )+
  ylim(0.32,0.49)+
  labs(x="Estímulo de Demanda", y="")+
  theme(legend.direction = "horizontal")+
  geom_line()
graf2 <- jg_mu_b2 %>% filter (name %in% c("wage_share", "profit_share")) %>%
  ggplot( aes ( x= period, y = value, col = name) )+
  theme(legend.position = "none")+
  ylim(0.32,0.49)+
  labs(x="Garantia de Empregos", y="")+
  geom_line(linetype = 1)
legend<- get_legend(graf1)
graf1 <- graf1 + theme(legend.position = "none")
grid.arrange(graf1, graf2,legend, ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))



#Capacity utilization
dados_mu_b3 %>% filter (name=="u") %>%
  ggplot( aes ( x= period, y = value, group = Cenário,
                linetype = Cenário, color = Cenário) )+
  ggtitle("Grau de utilização do estoque de capital")+
  theme(legend.position = "bottom")+
  geom_line()

#############################################################
#.          Multiplos choques em mu_b                       #
#############################################################

#Renda em diversos cenários
a1 <- conflict_claims %>% bind_rows() %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "Y")
a2 <- g1_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "Y")
a3 <- unemp_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "Y")
ggplot()+
  geom_line(aes ( x=period, y = value, color = as.factor(simulation)), a1)+
  geom_line(aes ( x=period, y = value, linetype = "Estímulo de demanda" ), a2)+
  geom_line(aes ( x=period, y = value, linetype= "Desemprego"), a3)+
  ggtitle("Renda")+
  theme(legend.position = "bottom")+
  scale_colour_hue(expression(~ mu[b]), 
                   labels = c(as.character(seq(0.2, 0.9, 0.05)), 
                              "Estímulo de demanda", "Desemprego"))


#Conta corrente sobre PIB em diversos cenários
a1 <- conflict_claims %>% bind_rows() %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "CAY")
a2 <- g1_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "CAY")
a3 <- unemp_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "CAY")
ggplot()+
geom_line(aes ( x=period, y = value, color = as.factor(simulation)), a1)+
  geom_line(aes ( x=period, y = value, linetype = "Estímulo de demanda" ), a2)+
  geom_line(aes ( x=period, y = value, linetype= "Desemprego"), a3)+
  # ggtitle("Conta Corrente (%PIB)")+
  labs(y="", x= "períodos")+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "bottom")+
  scale_colour_hue(expression(~ mu[b]), 
                   labels = c(as.character(seq(0.2, 0.9, 0.05)), 
                              "Estímulo de demanda", "Desemprego"))

#Poder de Barganha vs Inflação
conflict_claims %>% bind_rows() %>% pivot_longer(cols = -c(period, mu1)) %>%
  filter(period == 100 & name == "pi") %>%
  ggplot(aes(x = mu1, y = value))+
  labs(y="Inflação", x = expression(~mu[1]))+
  scale_y_continuous(labels = scales::percent)+
  geom_point(colour= "blue")+
  geom_point(aes(y=tail(g_scenario$pi, n=1),x= tail(g_scenario$mu1, n=1)),colour="green")+
  theme(legend.position = "bottom")

  
#Reservas internacionais sobre PIB em diversos cenários
a1 <- conflict_claims %>% bind_rows() %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "RcbY")
a2 <- g1_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "RcbY")
a3 <- unemp_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "RcbY")
ggplot()+
  geom_line(aes ( x=period, y = value, color = as.factor(simulation)), a1)+
  geom_line(aes ( x=period, y = value, linetype = "Estímulo de demanda" ), a2)+
  geom_line(aes ( x=period, y = value, linetype= "Desemprego"), a3)+
  ggtitle("Reservas internacionais (%PIB)")+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "bottom")+
  scale_colour_hue(expression(~ mu[b]), 
                   labels = c(as.character(seq(0.2, 0.9, 0.05)), 
                              "Estímulo de demanda", "Desemprego"))

a1 <- conflict_claims %>% bind_rows() %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "d_rcbY")
a2 <- g1_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "d_rcbY")
a3 <- unemp_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "d_rcbY")
ggplot()+
  geom_line(aes ( x=period, y = value,color = as.factor(simulation)), a1)+
  geom_line(aes ( x=period, y = value, linetype = "Estímulo de demanda" ), a2)+
  geom_line(aes ( x=period, y = value, linetype= "Desemprego"), a3)+
  # ggtitle("Variações das reservas internacionais (%PIB)")+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  scale_colour_hue(expression(~ mu[b]), 
                   labels = c(as.character(seq(0.2, 0.9, 0.05)), 
                              "Estímulo de demanda", "Desemprego"))

#Raxa real de câmbio em diversos cenários
a1 <- conflict_claims %>% bind_rows() %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "er")
a2 <- g1_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "er")
a3 <- unemp_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "er")
ggplot()+
  geom_line(aes ( x=period, y = value, color = as.factor(simulation)), a1)+
  geom_line(aes ( x=period, y = value, linetype = "Estímulo de demanda" ), a2)+
  geom_line(aes ( x=period, y = value, linetype= "Desemprego"), a3)+
  ggtitle("Câmbio real")+
  theme(legend.position = "bottom")+
  scale_colour_hue(expression(~ mu[b]), 
                   labels = c(as.character(seq(0.2, 0.9, 0.05)), 
                              "Estímulo de demanda", "Desemprego"))

#Inflação em diversos cenários
a1 <- conflict_claims %>% bind_rows() %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "pi")
a2 <- g1_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "pi")
a3 <- unemp_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "pi")
ggplot()+
  geom_line(aes ( x=period, y = value, color = as.factor(simulation)), a1)+
  geom_line(aes ( x=period, y = value, linetype = "Estímulo de demanda" ), a2)+
  geom_line(aes ( x=period, y = value, linetype= "Desemprego"), a3)+
  # ggtitle("Taxa de inflação")+
  labs(y="", x= "períodos")+
  theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)+
  scale_colour_hue(expression(~ mu[b]), 
                   labels = c(as.character(seq(0.2, 0.9, 0.05)), 
                              "Estímulo de demanda", "Desemprego"))

#Poder de barganha em diversos cenários
a1 <- conflict_claims %>% bind_rows() %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "mu1")
a2 <- g1_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "mu1")
a3 <- unemp_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "mu1")
ggplot()+
  geom_line(aes ( x=period, y = value, color = as.factor(simulation)), a1)+
  geom_line(aes ( x=period, y = value, linetype = "Estímulo de demanda" ), a2)+
  geom_line(aes ( x=period, y = value, linetype= "Desemprego"), a3)+
  # ggtitle("Poder de barganha dos trabalhadores")+
  labs(y="", x= "períodos")+
  theme(legend.position = "bottom")+
  scale_colour_hue(expression(~ mu[b]), 
                   labels = c(as.character(seq(0.2, 0.9, 0.05)), 
                              "Estímulo de demanda", "Desemprego"))

#Fluxo internacional de capitais em diversos cenários
a1 <- conflict_claims %>% bind_rows() %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "net_k_flow")
a2 <- g1_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "net_k_flow")
a3 <- unemp_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "net_k_flow")
ggplot()+
  geom_line(aes ( x=period, y = value, color = as.factor(simulation)), a1)+
  geom_line(aes ( x=period, y = value, linetype = "Estímulo de demanda" ), a2)+
  geom_line(aes ( x=period, y = value, linetype= "Desemprego"), a3)+
  ggtitle("Fluxo internacional de capitais")+
  theme(legend.position = "bottom")+
  scale_colour_hue(expression(~ mu[b]), 
                   labels = c(as.character(seq(0.2, 0.9, 0.05)), 
                              "Estímulo de demanda", "Desemprego"))

#Volumede conta corrente em diversos cenários
a1 <- conflict_claims %>% bind_rows() %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "CAr")
a2 <- g1_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "CAr")
a3 <- unemp_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "CAr")
ggplot()+
  geom_line(aes ( x=period, y = value, color = as.factor(simulation)), a1)+
  geom_line(aes ( x=period, y = value, linetype = "Estímulo de demanda" ), a2)+
  geom_line(aes ( x=period, y = value, linetype= "Desemprego"), a3)+
  ggtitle("Conta Corrente (Volume)")+
  theme(legend.position = "bottom")+
  scale_colour_hue(expression(~ mu[b]), 
                   labels = c(as.character(seq(0.2, 0.9, 0.05)), 
                              "Estímulo de demanda", "Desemprego"))

#Wage_share em diversos cenários
a1 <- conflict_claims %>% bind_rows() %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "wage_share")
a2 <- g1_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "wage_share")
a3 <- unemp_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "wage_share")
ggplot()+
  geom_line(aes ( x=period, y = value, color = as.factor(simulation)), a1)+
  geom_line(aes ( x=period, y = value, linetype = "Estímulo de demanda" ), a2)+
  geom_line(aes ( x=period, y = value, linetype= "Desemprego"), a3)+
  ggtitle("Participação salarial na renda")+
  theme(legend.position = "bottom")+
  scale_colour_hue(expression(~ mu[b]), 
                   labels = c(as.character(seq(0.2, 0.9, 0.05)), 
                              "Estímulo de demanda", "Desemprego"))

#Grau de utilização em diversos cenários
a1 <- conflict_claims %>% bind_rows() %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "u")
a2 <- g1_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "u")
a3 <- unemp_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "u")
ggplot()+
  geom_line(aes ( x=period, y = value, color = as.factor(simulation)), a1)+
  geom_line(aes ( x=period, y = value, linetype = "Estímulo de demanda" ), a2)+
  geom_line(aes ( x=period, y = value, linetype= "Desemprego"), a3)+
  ggtitle("Grau de utilização")+
  theme(legend.position = "bottom")+
  scale_colour_hue(expression(~ mu[b]), 
                   labels = c(as.character(seq(0.2, 0.9, 0.05)), 
                              "Estímulo de demanda", "Desemprego"))


a1 <- conflict_claims %>% bind_rows() %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "M")
a2 <- g1_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "M")
a3 <- unemp_scenario %>% pivot_longer(cols=-c(period, simulation)) %>%
  filter(name == "M")
ggplot()+
  geom_line(aes ( x=period, y = value, color = as.factor(simulation)), a1)+
  geom_line(aes ( x=period, y = value, linetype = "Estímulo de demanda" ), a2)+
  geom_line(aes ( x=period, y = value, linetype= "Desemprego"), a3)+
  ggtitle("Exportações")+
  theme(legend.position = "bottom")+
  scale_colour_hue(expression(~ mu[b]), 
                   labels = c(as.character(seq(0.2, 0.9, 0.05)), 
                              "Estímulo de demanda", "Desemprego"))


#############################################################
#.          Controle de capital.                            #
#############################################################
jg_phi_d <- wg_phi_d_shock %>% pivot_longer(cols = -period) %>% mutate (Cenário = "KC - Garantia de Empregos") 
g_phi_d <- g_phi_d_scenario %>% pivot_longer(cols = -period) %>% mutate (Cenário = "KC - Estímulo de demanda") 
dados_phi_d <- rbind(jg, g, jg_phi_d, g_phi_d, base)


# Conta corrente (%PIB)
dados_phi_d %>% filter(name == "CAY") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  # ggtitle("Conta Corrente (%PIB) no cenário com Controle de Fluxo de Capital")+
  labs(y="", x="períodos")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "dashed", "dotted", "dotted"))+
  scale_colour_manual(values = c( "orange", "green", "blue", "green", "blue"))+
  geom_line()

# Cambio Real
dados_phi_d %>% filter(name == "er") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  # ggtitle("Câmbio Real no cenário com Controle de Fluxo de Capital")+
  labs(y="", x="períodos")+
  scale_linetype_manual(values = c( "solid", "dashed", "dashed", "dotted", "dotted"))+
  scale_colour_manual(values = c( "orange", "green", "blue", "green", "blue"))+
  geom_line()

#Conta Comercial
dados_phi_d %>% filter(name == "comercial") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c( "solid", "dashed", "dashed", "dotted", "dotted"))+
  scale_colour_manual(values = c( "orange", "green", "blue", "green", "blue"))+
  ggtitle("Balanço comercial")+
  geom_line()



# Reservas internacionais
dados_phi_d %>% filter (name%in% c("d_rcbM", "d_rcbY")) %>%
  mutate(name = case_when(
    name == "d_rcbY" ~ "Variação das Reservas (% PIB)",
    name == "d_rcbM" ~ "Variação das Reservas (% Importações)",
    TRUE ~ name
  )) %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  # ggtitle("Variação das Reservas / PIB no cenário com Controle de Fluxo de Capital")+
  labs(y="", x= "períodos")+
  facet_wrap(~name, ncol=2, scale = "free")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "dashed", "dotted", "dotted"))+
  scale_colour_manual(values = c( "orange", "green", "blue", "green", "blue"))+
  geom_line()

# Inflação
dados_phi_d %>% filter(name == "pi") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  # ggtitle("Inflação no cenário com Controle de Fluxo de Capital")+
  labs(y="", x= "períodos")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values = c( "solid", "dashed", "dashed", "dotted", "dotted"))+
  scale_colour_manual(values = c( "orange", "green", "blue", "green", "blue"))+
  geom_line()

# Renda
dados_phi_d %>% filter(name == "Y") %>%
  ggplot(aes(x= period, y = value, linetype = Cenário, col = Cenário)) +
  theme(legend.position = "bottom")+
  # ggtitle("Renda no cenário com Controle de Fluxo de Capital")+
  labs(y="", x= "períodos")+
  scale_linetype_manual(values = c( "solid", "solid", "dashed", "dotted", "dotted"))+
  scale_colour_manual(values = c( "orange", "green", "blue", "red", "blue"))+
  geom_line()



