# Base PNAD TIC 2018 - Leitura dos dados    -------------------------------------#
# Luana 
# Revisão de Marcos Oliveira em 12/08/2020  -------------------------------------#
# V4: inclusão da análise por rede pública e privada em 21/08/2020               #
# Adaptação para calcular total de estudantes com acesso a internet por tipo     #
#--------------------------------------------------------------------------------#

# Quantos domicílios brasileiros tem acesso a internet banda larga, se possível verificar a qualidade dessa banda.
# 
# A tese estrutural da ONU é que todos os domicílios deveriam ter acesso a internet banda larga, 
# para que efetivamente os alunos pudessem ter o mínimo de infraestrutura de aprendizado durante a pandemia.

#### Required packages ####
x=c('survey','PNADcIBGE','dplyr','stringr','purrr','expss')
lapply(x,require,character.only=T)

# Options
options(scipen = 999, OutDec=",")

#### Directories ####

# PNADC.TIC dataset directory
pnadc = '\\\\md1719\\Repositorio\\IBGE\\PNADC'
dir.pnad  <- paste0(pnadc,'\\2018_4\\Dados')
dir.input <- paste0(pnadc,'\\2018_4\\Dicionário')

# Site to download, checar se houve atualizacao:
# https://www.ibge.gov.br/estatisticas/sociais/trabalho/17270-pnad-continua.html?=&t=downloads
# ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Trimestre/Trimestre_4/Dados/
file.copy(paste0(dir.pnad,"\\PNADC_2018_trimestre4.txt"), tempdir())
file.copy(paste0(dir.input,"\\input_PNADC_trimestre4_20200429.txt"), tempdir())
file.pnad<-paste0(tempdir(),"\\PNADC_2018_trimestre4.txt")
file.input<-paste0(tempdir(),"\\input_PNADC_trimestre4_20200429.txt")

# Read database and keep variables of interest
pnad <- read_pnadc(file.pnad,file.input,
                    c("UF","UPA","Estrato", "posest","V1008","V1014", "V1023","V1027", "V1028", "V1029", 
                      'V2001','V2005',"V2007", "V2009","V2010",'V3001','V3002','V3002A','V3003','V3003A',
                      "VD3004", "VD4001", "VDI5011","S07006", "S07007","S07008","S01022", "S01025",
                      'S01028','S01028A',"S01029",'S01030','S010301','S010302','S010303','S010304','S010305',
                      "S01030A1","S01030A2","S01030A3","S01030B","S01030C","S07001")) %>% 
  mutate(REGIAO = as.numeric(str_sub(as.character(UF),1,1)), IDdom = paste0(UPA,V1008,V1014),
         estuda = case_when(V3002 == '1' ~ 1, TRUE ~ 0),
         estuda.EB         = case_when(V3003A %in% c('02','03','04','05','06','07') ~ 1, TRUE ~ 0),
         estuda.ES         = case_when(V3003A %in% c('08') ~ 1,                          TRUE ~ 0),
         estuda.menos18    = case_when(V2009 < 18 & estuda == 1 ~ 1,                     TRUE ~ 0),
         estuda.EB.menos18 = case_when(V2009 < 18 & estuda.EB == 1 ~ 1,                  TRUE ~ 0),
         estuda.pub           = case_when(V3002A == 2  ~ 1, TRUE ~ 0),
         estuda.pri           = case_when(V3002A == 1  ~ 1, TRUE ~ 0),
         estuda.EB.pub        = case_when(V3002A == 2 & estuda.EB == 1  ~ 1, TRUE ~ 0),
         estuda.EB.pri        = case_when(V3002A == 1  & estuda.EB == 1 ~ 1, TRUE ~ 0)
  ) %>%
  select(REGIAO, UF,UPA,Estrato,IDdom,V1008,V1014,posest,V1023,V1027,V1028,V1029,
           V2001,V2005,sexo=V2007, idade = V2009, racacor = V2010, V3001,V3002,V3002A,V3003,V3003A,
           esc=VD3004, pea=VD4001, rendaF = VDI5011, cel=S07006, fixo = S01022,
           pop=V1028, net_cel=S07007, motivo_cel=S07008, net=S01029, tv=S01025, net_3m=S07001,
           S01028,S01028A,S01029,S01030,S010301,S010302,S010303,S010304,S010305,
           internet.movel=S01030A1,internet.discada=S01030A2, internet.banda.larga=S01030A3,
         estuda, estuda.EB, estuda.ES, estuda.menos18, estuda.EB.menos18,
         estuda.pub,estuda.pri,estuda.EB.pub,estuda.EB.pri)


#### Grouping data by house ####
pnadc.dom = pnad %>% 
  group_by(Estrato, UPA, IDdom, V1027, pop, posest, V1029, V2001, net, 
           internet.banda.larga, internet.discada, internet.movel) %>%
  summarise(n.estuda = sum(as.numeric(estuda)), 
            n.estuda.EB = sum(as.numeric(estuda.EB)), 
            n.estuda.ES = sum(as.numeric(estuda.ES)), 
            n.estuda.EB.menos18 = sum(as.numeric(estuda.EB.menos18)),
            n.estuda.pub = sum(estuda.pub),
            n.estuda.pri = sum(estuda.pri),
            n.estuda.EB.pub = sum(estuda.EB.pub),
            n.estuda.EB.pri = sum(estuda.EB.pri)
            ) %>%
  mutate(dom.estuda    = case_when(n.estuda    >= 1 ~ 1, TRUE ~ 0),
         dom.estuda.EB = case_when(n.estuda.EB >= 1 ~ 1, TRUE ~ 0),
         dom.estuda.ES = case_when(n.estuda.ES >= 1 ~ 1, TRUE ~ 0),
         dom.estuda.EB.menos18 = case_when(n.estuda.EB.menos18 >= 1 ~ 1, TRUE ~ 0),
         dom.estuda.pub    = case_when(n.estuda.pub >=1 ~ 1, TRUE ~ 0),
         dom.estuda.pri    = case_when(n.estuda.pri >=1 ~ 1, TRUE ~ 0),
         dom.estuda.EB.pub = case_when(n.estuda.EB.pub >=1 ~ 1, TRUE ~ 0),
         dom.estuda.EB.pri = case_when(n.estuda.EB.pri >=1 ~ 1, TRUE ~ 0)
  )

# # compute mean of house inabitants by posest
# df1 = pnadc.dom %>% group_by(posest) %>%  summarise(tam.dom = mean(V2001))
# pnadc.dom = pnadc.dom %>% right_join(df1,by='posest') %>% mutate(tot.dom = V1029/tam.dom)
# 
# 
# #------------------------------------------------------------------------------------------------#
# #### Sample design adjust for poststratificaton - POPULATION ####
# # Preparation
# prestratified_design = svydesign(id= ~ UPA, strata  = ~ Estrato, data = pnad, weights = ~ V1027, nest = T  )
# # Pos strata sizes
# popc.types = data.frame(posest = as.character(unique(pnad$posest)), Freq = as.numeric(unique(pnad$V1029)))
# popc.types = (popc.types[order(popc.types$posest),])
# # Pos stratification per si
# pnadsvy =   postStratify(    design = prestratified_design,    strata = ~ posest,    population = popc.types  )
# 
# #-------------------------------------------------------------------------------------------------#
# # Sample design adjusting for poststratificaton - HOUSE ####
# # Design withOUT population projection:
# predesign.dom = svydesign( id = ~ UPA, strata = ~ Estrato, data = pnadc.dom, weights = ~ V1027, nest = T  )
# # Houses projection:
# popc.types = data.frame(posest = as.character(unique(pnadc.dom$posest)), Freq = as.numeric(unique(pnadc.dom$tot.dom)))
# popc.types = (popc.types[order(popc.types$posest),])
# # Design with population projection:
# pnadsvy.dom = postStratify(design = predesign.dom,strata = ~ posest,population = popc.types)
#-------------------------------------------------------------------------------------------------#
# Alternative Design without house projection, using weight from IBGE:
design.dom = svydesign(id= ~ UPA,strata= ~ Estrato,data= pnadc.dom,weights = ~ pop, nest= T)

# An approximation to design with pos-stratification is to use directly the weight with pos-stratification.
# The S.E. will be slightly incorrect, just 1 point at 4th decimal number.
# The advantage is that all pontual estimation will be equal to IBGE reports.
# So, it is better to use the approximation that match with IBGE.
# ===>>> Conclusion: use design.dom <<<===


#--------------------------------------------------------#
#### Internet access to houses                        ####
# Use design.dom in order to pnadsvy.dom
#--------------------------------------------------------#

tab = function(x) {
  .formula = reformulate(x) 
  a=svytable(.formula, design.dom, addNA = T)
  print(addmargins(a))
  addmargins(proportions(a))
  }

# Houses with internet
tab('net')
tab('internet.discada')
tab('internet.movel')
tab('internet.banda.larga')
tab('internet.banda.larga + internet.movel')


# Houses with students:
tab('dom.estuda')

# Subpopulation students, houses with internet
sub.estuda = subset(design.dom, dom.estuda == 1)
t1=svytable(~ internet.banda.larga + internet.movel, sub.estuda, addNA = T)
addmargins(t1)
addmargins(prop.table(t1))

#### Subpopulation: houses with at least one student less than 18 years old
tab('dom.estuda.EB.menos18')
sub.EB18 = subset(design.dom, dom.estuda.EB.menos18 == 1 )
t2=svytable(~ internet.banda.larga + internet.movel, sub.EB18, addNA = T)
addmargins(t2)
addmargins(prop.table(t2))


# Subpopulation: houses with 'at least one student in' by rede de ensino:
tab('dom.estuda.pub')
tab('dom.estuda.pri')
tab('dom.estuda.EB.pub')
tab('dom.estuda.EB.pri')
# Function to do cross tables:
tab2 = function(x) {
  sub = subset(design.dom, eval(parse(text=x)) == 1)
  a = svytable(~ internet.banda.larga + internet.movel, sub, addNA = T)
  print(addmargins(a))
  addmargins(proportions(a))
}
tab2('dom.estuda.pub')
tab2('dom.estuda.pri')
tab2('dom.estuda.EB.pub')
tab2('dom.estuda.EB.pri')
# Function to do cross tables for Educacao Basica and menor18:
tab3 = function(x) {
  sub = subset(design.dom, eval(parse(text=x)) == 1 &  dom.estuda.EB.menos18 == 1 )
  a = svytable(~ internet.banda.larga + internet.movel, sub, addNA = T)
  print(addmargins(a))
  addmargins(proportions(a))
}
tab3('dom.estuda.EB.pub')
tab3('dom.estuda.EB.pri')


