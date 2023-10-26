
# Exibe e salva ecos de radar fornecidos pela Rede de Meteorologia do Comando da Aeronáutica (REDEMET-DECEA)
#
# feito com carinho por Danilo Siden
# Maio de 2022

# nova versão de 2023: substitui o pacote aposentado rgdal
# pelo seu sucessor, sf

# informe abaixo uma pasta de trabalho permanente e descomente a linha:
#setwd("C:/.../materia-prima")

rm(list = ls())

################################################################################

pacotes_necessarios <- c("rjson", "httr", "ggplot2", "grid", "sf", "png", "magick")
for(i in pacotes_necessarios){
    if (!(i %in% installed.packages())){
      print(sprintf("Pacote '%s' nao encontrado. Instalando pacote...",i))
      install.packages(i, dependencies = NA)
    }
   if(i=="sf"){
      library(sf, include.only = 'read_sf', verbose = F, warn.conflicts = T, quietly = T)
      }else{
        library(i, character.only = TRUE)
      }
  }
if(!(sum(pacotes_necessarios %in% (.packages()))==length(pacotes_necessarios))){
    aviso <- sprintf("O pacote '%s' nao foi carregado.", pacotes_necessarios[i])
    print(aviso)
    print("Responda c e verifique manualmente.")
    quit(save="ask")
}

# certificar-se da pasta de trabalho
while(strsplit(getwd(), split = "/")[[1]][length(strsplit(getwd(), split = "/")[[1]])]!="materia-prima"){
  cat("\n\tPasta de trabalho incorreta\n\tEscreva o caminho completo ate .../materia-prima:\n")
  setwd(readline())
}

# Importar os shapefiles e imagens necessárias
pontos <- read.csv("pontos.csv")
EstadosBR <- read_sf(dsn = sprintf("%s/EstadosBR_IBGE_LLWGS84.shp", getwd()))
legenda <- sprintf("%s/legenda_dBz.png", getwd())
# Ler os pngs e transformar em grobs
legenda <- readPNG(source = legenda)
legenda <- rasterGrob(legenda)

# largura e altura na imagem em polegadas
largura <- 8
altura <- 8

chave <- readLines("chave-redemet.txt")
if(length(chave)==0||nchar(chave)==0){stop("\n\n\tVerifique sua chave em chave-redemet.txt\n\n")}

cat("Produtos CAPPI: 03km, 05km, 07km, 10km, maxcappi")
lista.produtos <- c('03km', '05km', '07km', '10km', 'maxcappi')
produto <- readline("-> ")
while(!(produto %in% lista.produtos)){
  produto <- readline("-> ")
}

cat("Tempo no formato aaaaMMddHH
ou pressione ENTER para a ultima imagem")
tempo <- readline("-> ")
cat("Codigo do radar (ou ENTER para ver a lista)")
lista.radares <- data.frame("codigo" = c('al', 'be', 'bv', 'cn', 'cz', 'ga', 'jr', 'mq', 'mo', 'mn', 'mi', 'nt', 'pl',
                                         'pc', 'pv', 'sv', 'sn', 'st', 'sg', 'sf', 'ua', 'sl', 'sr', 'tt', 'tf', 'tm'),
                            "local" = c('Almenara/MG', 'Belem/PA', 'Boa Vista/RR', 'Cangucu/RS', 'Cruzeiro do Sul/AC', 'Gama/DF', 'Jaraguari/MS', 'Macapa/AP',
                                        'Maceio/AL', 'Manaus/AM', 'Morro da Igreja/SC', 'Natal/RN', 'Petrolina/PE', 'Pico do Couto/RJ', 'Porto Velho/RO',
                                        'Salvador/BA', 'Santarem/PA', 'Santa Teresa/ES', 'Santiago/RS', 'Sao Francisco/MG', 'Sao Gabriel da Cachoeira/AM',
                                        'Sao Luis/MA', 'Sao Roque/SP', 'Tabatinga/AM', 'Tefe/AM', 'Tres Marias/MG'))
repeat{
  radar <- readline("-> ")
  if(radar %in% lista.radares$codigo)
  {
    cat(sprintf("Radar de %s", lista.radares[lista.radares$codigo==radar, "local"]))
    break
    }else{
      print(lista.radares)
    }
}
cat("\nQuantas imagens animar ate a data? (1 a 15) ")
repeat{
  animar <- readline("-> ")
  if(as.numeric(animar) %in% c(1:15)){break}
}


options(timeout = 4000000)
httr::timeout(4000000)
if(tempo==""){
  solicit <- paste( "https://api-redemet.decea.mil.br/produtos/radar/",
                    produto,
                    "?api_key=",
                    chave,
                    "&area=",
                    radar,
                    "&anima=",
                    animar,
                    sep = "")
}else{
  solicit <- paste( "https://api-redemet.decea.mil.br/produtos/radar/",
                    produto,
                    "?api_key=",
                    chave,
                    "&data=",
                    tempo,
                    "&area=",
                    radar,
                    "&anima=",
                    animar,
                    sep = "")
}
cat("Acessando...\n")
# transformar conteúdo da url (json) em objeto R
resposta <- fromJSON(file = solicit)
if(resposta$status==TRUE){
  cat(sprintf("Produto: %s\n", resposta$data$tipo))
  cat(sprintf("Horarios: %i\n", length(resposta$data$anima)))
  
  # dividir entre imagem estatica e animacao
  if(length(resposta$data$anima)>1){

    # para cada tempo disponivel vamos buscar o radar desejado
    # caso mais de um horario esteja disponivel no mesmo radar
    # vamos prosseguir, ou vamos informar que só há um horário
    
    #salvar os horarios aqui (strings data e hora):
    todos.horarios <- c()
    # links das imagens (strings tbm):
    todos.paths <- c()
    # local de salvar imagens:
    todas.imagens <- c() # strings
    # que vao virar PNGs
    # que vao virar grobs, que da pra plotar
    
    for(h in 1:length(resposta$data$anima)){    # navegar pela hora

      for(l in 1:length(resposta[['data']][['radar']][[h]])){      # navegar pela localidade
        
        # se acharmos localidade == codigo do radar
        if(resposta[['data']][['radar']][[h]][[l]]$localidade==radar){
          
          # extrair propriedades do produto
          long.min <- as.numeric(resposta$data$radar[[h]][[l]]$lon_min)
          long.max <- as.numeric(resposta$data$radar[[h]][[l]]$lon_max)
          lat.min <- as.numeric(resposta$data$radar[[h]][[l]]$lat_min)
          lat.max <- as.numeric(resposta$data$radar[[h]][[l]]$lat_max)
          raio <- as.numeric(resposta$data$radar[[h]][[l]]$raio)
          nome <- resposta$data$radar[[h]][[l]]$nome
          lat.cent <- as.numeric(resposta$data$radar[[h]][[l]]$lat_center)
          lon.cent <- as.numeric(resposta$data$radar[[h]][[l]]$lon_center)
          produto <- resposta$data$tipo
          horario <- resposta$data$radar[[h]][[l]]$data
          
          # salvar horários, nome das imagens e paths:
          todos.horarios <- append(todos.horarios, gsub(":","_",horario))
          todas.imagens <- append(todas.imagens, sprintf("%s/temporarios/radar_%s.png", getwd(), gsub(":","_",horario)))
          todos.paths <- append(todos.paths, resposta$data$radar[[h]][[l]]$path)
          
          # criar um dataframe só para ser visto no terminal
          infos <- data.frame("Nome" = nome,
                              "Produto" = produto,
                              #"Data e hora" = horario,
                              "Raio" = raio,
                              "Latitude central" = lat.cent,
                              "Longitude central" = lon.cent,
                              "Longitude mínima" = long.min,
                              "Longitude máxima" = long.max,
                              "Latitude mínima" = lat.min,
                              "Latitude máxima" = lat.max)
          infos <- t(infos) # transpor (colunas para linhas)
          
        }# achei local
      }# procurou local
    }# procurou horas
    
    # remover repetidos
    todos.paths <- unique(todos.paths)
    todos.horarios <- unique(todos.horarios)
    todas.imagens <- unique(todas.imagens)
    cat("\tRemovendo imagens repetidas...\n")
    
    if( ( length(todos.paths) == length(todos.horarios) ) && ( length(todos.horarios) == length(todas.imagens) ) ){
      cat(sprintf("%i imagens encontradas...", length(todos.paths)))
      for(linque in todos.paths){
        # acessar a imagem e salvar na pasta 'temporarios' com o horario no nome
        download.file(url = linque,
                      destfile = sprintf("%s/temporarios/radar_%s.png", getwd(), todos.horarios[match(linque, todos.paths)]),
                      method = 'auto',
                      quiet = TRUE,
                      mode = 'wb')
                      # indice do linque dentro dos paths vai ter que corresponder a um elemento nos horarios
      }
    }else{
      stop("Discrepancia entre horarios e enderecos.")
    }
    
    # # carregamento e conversão dos pngs
    # todas.imagens <- readPNG(todas.imagens)
    # todas.imagens <- rasterGrob(todas.imagens)
    
    # salvar horários e imagens num dataframe:
    eco.frame <- data.frame("png" = todas.imagens,
                            "tempo" = todos.horarios)
    # pq choras???????????????
    # dataframe não aguenta todas.images engrobado
    
    # fim do caso animação
  }else{
    # caso estática
    for(r in 1:length(resposta$data$radar[[1]])){
      # procurar o radar desejado
      
      if(resposta$data$radar[[1]][[r]]$localidade==radar){
        # se a localidade bater
        
        if(!(is.null(resposta$data$radar[[1]][[r]]$path))){
          # e o caminho não for nulo, mostrar (e salvar) as propriedades
          # extrair propriedades do produto
          long.min <- as.numeric(resposta$data$radar[[1]][[r]]$lon_min)
          long.max <- as.numeric(resposta$data$radar[[1]][[r]]$lon_max)
          lat.min <- as.numeric(resposta$data$radar[[1]][[r]]$lat_min)
          lat.max <- as.numeric(resposta$data$radar[[1]][[r]]$lat_max)
          raio <- as.numeric(resposta$data$radar[[1]][[r]]$raio)
          nome <- resposta$data$radar[[1]][[r]]$nome
          lat.cent <- as.numeric(resposta$data$radar[[1]][[r]]$lat_center)
          lon.cent <- as.numeric(resposta$data$radar[[1]][[r]]$lon_center)
          produto <- resposta$data$tipo
          horario <- resposta$data$radar[[1]][[r]]$data
          
          # criar um dataframe só para ser visto no terminal
          # ABLUBLEBLUBLELBU
          infos <- data.frame("Nome" = nome,
                              "Produto" = produto,
                              "Data e hora" = horario,
                              "Raio" = raio,
                              "Latitude central" = lat.cent,
                              "Longitude central" = lon.cent,
                              "Longitude mínima" = long.min,
                              "Longitude máxima" = long.max,
                              "Latitude mínima" = lat.min,
                              "Latitude máxima" = lat.max)
          infos <- t(infos) # transpor (colunas para linhas)
          
          # acessar a imagem e salvar na pasta 'temporarios'
          download.file(url = resposta$data$radar[[1]][[r]]$path,
                        destfile = sprintf("%s/temporarios/radar.png", getwd()),
                        method = 'auto',
                        quiet = TRUE,
                        mode = 'wb')
          
          # carregar imagem e transformar em grob
          imagemradar <- sprintf("%s/temporarios/radar.png", getwd())
          imagemradar <- readPNG(source = imagemradar)
          imagemradar <- rasterGrob(imagemradar)
          
        }else{  # e se for nulo?
          stop("Imagem indisponível")
          break
        }
      } # radar certo
    } # loop for
  }# anima sim ou não
}else{
  print(sprintf("Erro %i", resposta$status))
  stop("Resposta inválida")
} # status da resposta sim ou não

###################### PLOTAR ########################

if(animar==1){ # plot de imagem estática
  
  graphics.off()
  X11(height = altura,
      width = largura)
  
  plote <- ggplot()+
    # círculos dos raios:
    # 1 grau ~= 111 Km
    annotate("path",
             x=lon.cent+raio/111*cos(seq(0,2*pi,length.out=100)),
             y=lat.cent+raio/111*sin(seq(0,2*pi,length.out=100)),
             colour = "gray",
             linewidth = 0.5
    )+
    # cruz do radar
    geom_point(data=as.data.frame(1),
               x=lon.cent,
               y=lat.cent,
               size = 3,
               shape = 3,
               color = 'gray'
    )+
    geom_sf(data = EstadosBR,
            fill = NA,
              #aes(x=long, y=lat, group = group),
              #alpha = 0.5,
              #color = "gray15",
              size = 0.5
    )+
    geom_point(data = pontos,
               aes(x=lon, y=lat),
               size = 1,
               shape = 1
    )+
    geom_text(data = pontos,
              aes(x=lon, y = lat, label = nome),
              size = 2,
              vjust = -0.5
    )+
    # imagem dos ecos:
    #carregamento dos endereços do dataframe e conversão dos pngs
    annotation_custom(imagemradar,
                      xmin = long.min,
                      xmax = long.max,
                      ymin = lat.min,
                      ymax = lat.max
    )+
    # imagem da legenda de refletividade
    # legenda: https://redemet.decea.mil.br/old/?i=produtos&p=radares-meteorologicos
    # depende do raio
    {
      if(raio==400){
        annotation_custom(legenda,
                          xmin = long.max-2.31,
                          xmax = long.max+0.37,
                          ymin = lat.min-0.55,
                          ymax = lat.min+0.67)
      }else{
        annotation_custom(legenda,
                          xmin = long.max-1.4,
                          xmax = long.max+0.24,
                          ymin = lat.min-0.35,
                          ymax = lat.min+0.45)
      }
    }+
    coord_sf(xlim = c(long.min, long.max),
             ylim = c(lat.min, lat.max),
             clip = "on",
             crs = sf::st_crs(4326),
             expand = TRUE
    )+
    scale_x_continuous(breaks = seq(floor(long.min), ceiling(long.max), 1))+
    scale_y_continuous(breaks = seq(floor(lat.min), ceiling(lat.max), 1))+
    theme_bw()+
    # formatação dos titulos
    theme(legend.position = "right",
          legend.key.height = unit(1.3, "cm"),
          legend.key.width = unit(0.2, "cm"),
          legend.title = element_text(size = 9),
          legend.background = element_blank(),
          axis.text = element_text(size = 12, colour = 1),
          axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
          plot.margin = unit(c(0.3, 0.7, 0.3, 0.2), "cm"),
          plot.title = element_text(face = "bold",
                                    hjust = 0.5)
    )+
    labs(x = "Longitude (°)",
         y = "Latitude (°)",
         title = sprintf("%s    %s UTC", nome, gsub("_",":",horario)),
         subtitle = sprintf("Produto CAPPI: %s    Raio: %s Km    ", produto, raio)
    )
  
  print(plote)
  
  # Abrir o arquivo para salvar a imagem
  ggsave(filename = sprintf("%s/resultados/radar_%s_%s_%s.png", getwd(), radar, produto, gsub(":","_",horario)),
         height = altura,
         width = largura,
         units = "in",
         dpi = 300)
  
}else{# plot de animacao
  
  os.plotes <- c() # onde vamos guardar os endereços dos plotes
  
  for(A in 1:nrow(eco.frame)){
    
    graphics.off()
    X11(height = altura,
        width = largura)
    
    plote <- ggplot()+
      # círculos dos raios:
      # 1 grau ~= 111 Km
      annotate("path",
               x=lon.cent+raio/111*cos(seq(0,2*pi,length.out=100)),
               y=lat.cent+raio/111*sin(seq(0,2*pi,length.out=100)),
               colour = "gray",
               linewidth = 0.5
      )+
      # cruz do radar
      geom_point(data=as.data.frame(1),
                 x=lon.cent,
                 y=lat.cent,
                 size = 3,
                 shape = 3,
                 color = 'gray'
      )+
      geom_sf(data = EstadosBR,
              fill = NA,
                #aes(x=long, y=lat, group = group),
                #alpha = 0.5,
                #color = "gray15",
                size = 0.5
      )+
      geom_point(data = pontos,
                 aes(x=lon, y=lat),
                 size = 1,
                 shape = 1
      )+
      geom_text(data = pontos,
                    aes(x=lon, y = lat, label = nome),
                    size = 2,
                    vjust = -0.5
      )+
      # imagem dos ecos:
      annotation_custom(rasterGrob(readPNG(eco.frame$png[A])),
                        xmin = long.min,
                        xmax = long.max,
                        ymin = lat.min,
                        ymax = lat.max
      )+
      # imagem da legenda de refletividade
      # depende do raio
      {
        if(raio==400){
          annotation_custom(legenda,
                            xmin = long.max-1.4,
                            xmax = long.max+0.24,
                            ymin = lat.min-0.35,
                            ymax = lat.min+0.45)
        }else{
          annotation_custom(legenda,
                            xmin = long.max-1.4,
                            xmax = long.max+0.24,
                            ymin = lat.min-0.35,
                            ymax = lat.min+0.45)
        }
      }+
      coord_sf(xlim = c(long.min, long.max),
               ylim = c(lat.min, lat.max),
               clip = "on",
               crs = sf::st_crs(4326),
               expand = TRUE
      )+
      scale_x_continuous(breaks = seq(floor(long.min), ceiling(long.max), 1))+
      scale_y_continuous(breaks = seq(floor(lat.min), ceiling(lat.max), 1))+
      theme_bw()+
      # formatação dos titulos
      theme(legend.position = "right",
            legend.key.height = unit(1.3, "cm"),
            legend.key.width = unit(0.2, "cm"),
            legend.title = element_text(size = 9),
            legend.background = element_blank(),
            axis.text = element_text(size = 12, colour = 1),
            axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
            plot.margin = unit(c(0.3, 0.7, 0.3, 0.2), "cm"),
            plot.title = element_text(face = "bold",
                                      hjust = 0.5)
      )+
      labs(x = "Longitude (°)",
           y = "Latitude (°)",
           title = sprintf("%s    %s UTC", nome, gsub("_",":",eco.frame$tempo[A])),
           subtitle = sprintf("Produto CAPPI: %s    Raio: %s Km    ", produto, raio)
      )
    
    print(plote)
    
    # Abrir o arquivo para salvar a imagem
    ggsave(filename = sprintf("%s/resultados/radar_anima_%i.png", getwd(), A),
           height = altura,
           width = largura,
           units = "in",
           dpi = 300)

    os.plotes <- append(os.plotes, sprintf("%s/resultados/radar_anima_%i.png", getwd(), A) )
    
    }# cada trecho da animação
  
  # finalizadas as imagens vamos
  # carregar pngs e formar gif
  os.plotes <- lapply(os.plotes, image_read)
  # juntar e animar
  cat("\nAnimando...")
  animado <- image_animate(image_join(os.plotes),
                           fps = 1,
                           delay = NULL,
                           optimize = T)
  
  cat("\nSalvando...")
  image_write(image = animado,
              path = sprintf("%s/resultados/radar_%s_%s_%s.gif", getwd(), radar, produto, gsub(":","_",horario)))
  
} # terminou, se animação

graphics.off()

cat("Fim\n")
