# Versão do script redemet-radar-win.R com GUI em html construído com Shiny
# Exibe e salva ecos de radar fornecidos pela Rede de Meteorologia do Comando da Aeronáutica (REDEMET-DECEA)
#
# feito com carinho por Danilo Siden
# Versão 1: Maio de 2022
# GUI: Dezembro de 2023


# NÃO ALTERAR A PARTIR DESTA LINHA
################################################################################

# nova versão de 2023: substitui o pacote aposentado rgdal
# pelo seu sucessor, sf
# reduziu o tamanho das imagens e introduziu correcao para longitude no raio

rm(list = ls())

pacotes_necessarios <- c("rjson", "httr", "ggplot2", "grid", "sf", "png", "magick", "shiny")
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

# converter grau de longitude em km em funcao da latitude em graus
LON_KM <- function(latitude){
  # presumindo raio da terra 40.075 Km
  40075*cos(latitude*pi/180)/360
}

# função de busca na API
Solicitar_API <- function(produto, chave, data, hora, radar){
  
  withProgress(message = "Iniciar busca", value = 0.1,
               expr = {
                 resposta <- fromJSON(file = paste("https://api-redemet.decea.mil.br/produtos/radar/",
                                                   produto,
                                                   "?api_key=",
                                                   chave,
                                                   "&data=",
                                                   paste(
                                                     strsplit(
                                                       as.character(data),
                                                       "-")[[1]],
                                                     collapse = ""),
                                                   hora,
                                                   "&area=",
                                                   radar,
                                                   # "&anima=",
                                                   # animar,
                                                   sep = ""))
                 setProgress(message = "Buscando...", value = 0.2,
                             detail = "Aguardando resposta")
                 if(resposta$status==FALSE){
                   setProgress(message = "Busca cancelada", value = 0.2,
                               detail = "Resposta inválida")
                   retorno <- FALSE
                   Sys.sleep(2)
                 }else{# resposta válida
                   # caso estática
                   for(r in 1:length(resposta$data$radar[[1]])){
                     # procurar o radar desejado
                     
                     if(resposta$data$radar[[1]][[r]]$localidade==radar){
                       # se a localidade bater
                       
                       if(!(is.null(resposta$data$radar[[1]][[r]]$path))){
                         
                         setProgress(message = "Imagem encontrada", value = 0.6)
                         
                         # e o caminho não for nulo
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
                         
                         # acessar a imagem e salvar na pasta 'temporarios'
                         download.file(url = resposta$data$radar[[1]][[r]]$path,
                                       destfile = sprintf("%s/temporarios/radar.png", getwd()),
                                       method = 'auto',
                                       quiet = TRUE,
                                       mode = 'wb')
                         
                         # carregar imagem
                         imagemradar <- sprintf("%s/temporarios/radar.png", getwd())
                         imagemradar <- readPNG(source = imagemradar)
                         
                         retorno <- list(
                           long.min=long.min, long.max=long.max,
                           lat.min=lat.min, lat.max=lat.max,
                           raio=raio, nome=nome,
                           lat.cent=lat.cent, lon.cent=lon.cent,
                           produto=produto, horario=horario,
                           imagemradar=imagemradar)
                         
                         break
                       }else{
                         # se o caminho for nulo
                         setProgress(message = "Imagem indisponível", value = 0.1,
                                     detail = "Tente outra data/hora")
                         retorno <- FALSE
                         Sys.sleep(3)
                       }
                     }#radar certo
                   }# loop for
                 }# resposta válida
                 return(retorno)
               })
}

# função de montar imagem
Montar_Imagem <- function(lon.cent, lat.cent, raio, nomeradar, produto, EstadosBR,
                          pontos=FALSE, imagemradar, legenda, long.min, long.max,
                          lat.min, lat.max, horario, altura=8, largura=8){
  # imagemradar e legenda = saída de readPNG()
  # EstadosBR = objeto sf (shapefile de linha em WGS84)
  # pontos = dataframe com colunas "lat", "lon" e "nome"
  
  withProgress(message = "Montando imagem", value = 0.8,
               expr = {
                 
                 plote <- ggplot()+
                   # círculos dos raios:
                   annotate("path",
                            x=lon.cent+raio/LON_KM(lat.cent)*cos(seq(0,2*pi,length.out=100)),
                            y=lat.cent+raio/111*sin(seq(0,2*pi,length.out=100)),
                            colour = "gray",
                            linewidth = 0.5
                   )+
                   # cruz do radar
                   geom_point(data=as.data.frame("cruz"=c(1),
                                                 "x"=c(lon.cent),
                                                 "y"=c(lat.cent)),
                              aes(x=lon.cent,y=lat.cent),
                              size = 3,
                              shape = 3,
                              color = 'gray'
                   )+
                   geom_sf(data = EstadosBR,
                           fill = NA,
                           size = 0.5
                   )+
                   # imagem dos ecos:
                   annotation_raster(imagemradar,
                                     xmin = long.min,
                                     xmax = long.max,
                                     ymin = lat.min,
                                     ymax = lat.max
                   )+
                   {
                     if(class(pontos)==class(data.frame())){
                       geom_point(data = pontos,
                                  aes(x=lon, y=lat),
                                  size = 1,
                                  shape = 1)}
                   }+
                   {
                     if(class(pontos)==class(data.frame())){
                       geom_text(data = pontos,
                                 aes(x=lon, y = lat, label = nome),
                                 size = 2,
                                 vjust = -0.5)}
                   }+
                   # imagem da legenda de refletividade
                   {
                     if(raio==400){
                       annotation_custom(rasterGrob(legenda),
                                         xmin = long.max-2.31,
                                         xmax = long.max+0.37,
                                         ymin = lat.min-0.55,
                                         ymax = lat.min+0.67)
                     }else{
                       annotation_custom(rasterGrob(legenda),
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
                        title = sprintf("%s    %s UTC", nomeradar, gsub("_",":",horario)),
                        subtitle = sprintf("Produto CAPPI: %s    Raio: %s Km    ", produto, raio)
                   )
                 
                 setProgress(message = "Montando imagem", value = 0.9)
                 
                 print(plote)
                 
                 setProgress(message = "Montando imagem", value = 1.0)
                 
               })
  
  
}

# Importar os shapefiles e imagens necessárias

EstadosBR <- read_sf(dsn = sprintf("%s/EstadosBR_IBGE_LLWGS84.shp", getwd()))
legenda <- sprintf("%s/legenda_dBz.png", getwd())
# carregar legenda
legenda <- readPNG(source = legenda)


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  
  titlePanel(title = "Imagens de Radar Meteorológico", windowTitle = "Imagens Radar"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "chave", label = "Chave redemet",
                value = readLines("chave-redemet.txt"),
                placeholder =  "Cole sua chave aqui",
                width = "400px"),
      
      selectInput(inputId = "produto", label = "Produto", width = "180px",
                  choices = list("MaxCappi"="maxcappi",
                                 "CAPPI 3 Km"="03km",
                                 "CAPPI 5 Km"="05km",
                                 "CAPPI 7 Km"="07km",
                                 "CAPPI 10 Km"="10km"),
                  selected = "MaxCappi", multiple = F),
      
      selectInput(inputId = "radar", label = "Radar",
                  width = "330px",
                  choices = list(`Norte`=list("Belém/PA"="be", "Boa Vista/RR"="bv", "Cruzeiro do Sul/AC"="cz", "Macapá/AP"="mq",
                                              "Manaus/AM"="mn", "Porto Velho/RO"="pv", "Santarém/PA"="sn", "São Gabriel da Cachoeira/AM"="ua",
                                              "Tabatinga/AM"="tt", "Tefé/AM"="tf"),
                                 `Nordeste`=list("Maceió/AL"="mo", "Natal/RN"="nt", "Petrolina/PE"="pl", "Salvador/BA"="sv", "São Luís/MA"="sl"),
                                 `Centro-Oeste`=list("Gama/DF"="ga", "Jaraguari/MS"="jr"),
                                 `Sudeste`=list("Almenara/MG"="al", "Pico do Couto/RJ"="pc", "Santa Teresa/ES"="st",
                                                "São Francisco/MG"="sf", "São Roque/SP"="sr", "Três Marias/MG"="tm"),
                                 `Sul`=list("Canguçu/RS"="cn", "Morro da Igreja/SC"="mi", "Santiago/RS"="sg")),
                  multiple = F),
      
      dateInput(inputId = "data", label = "Data", min = "2017-01-01",
                format = "yyyy-mm-dd", autoclose = T, value = "2023-11-01",
                language = "pt-BR", width = "180px"),
      
      selectInput(inputId = "hora", label = "Hora", width = "120px",
                  choices = list("00:00"="00", "01:00"="01", "02:00"="02",
                                 "03:00"="03", "04:00"="04", "05:00"="05",
                                 "06:00"="06", "07:00"="07", "08:00"="08",
                                 "09:00"="09", "10:00"="10", "11:00"="11",
                                 "12:00"="12", "13:00"="13", "14:00"="14",
                                 "15:00"="15", "16:00"="16", "17:00"="17",
                                 "18:00"="18", "19:00"="19", "20:00"="20",
                                 "21:00"="21", "22:00"="22", "23:00"="23")),
      
      checkboxInput(inputId = "quer_pontos", label = "Destacar pontos na imagem", value = F),
      
      actionButton(inputId = "botaoSolicit", label = "Buscar", icon = icon("search")),
      downloadButton(outputId = "baixar", label = "Baixar imagem")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Imagem",
          fluidRow(
            #waiter::use_waiter(),
            #textOutput(outputId = "aviso"),
            plotOutput(outputId = "imagem")
          )
        ),
        tabPanel(
          title = "Pontos a exibir",
          fileInput(inputId = "arquivo_pontos",
                    label = "Envie um arquivo contendo os pontos",
                    multiple = F, accept = ".csv", buttonLabel = "Selecionar",
                    placeholder = "formato CSV"),
          tableOutput("pontos")
        )
      )
    )
  )

)

server <- function(input, output, session) {
  
  # ação do botão de busca
  observeEvent(input$botaoSolicit,
               {
                 Sys.sleep(0.2)
                 #waiter::Waiter$new(id = "imagem")$show()
                 busca_dado <- Solicitar_API(produto = input$produto, chave = input$chave,
                                             data = input$data, hora = input$hora, radar = input$radar)
                 req(busca_dado)
                 
                 pontos <- if(input$quer_pontos){tabela_pontos()}else{FALSE}
                 
                 output$imagem <- renderPlot({
                     Montar_Imagem(lon.cent = busca_dado$lon.cent, lat.cent = busca_dado$lat.cent,
                                   raio = busca_dado$raio, nomeradar = busca_dado$nome,
                                   produto = busca_dado$produto,
                                   EstadosBR = EstadosBR,
                                   pontos = pontos,
                                   imagemradar = busca_dado$imagemradar, legenda = legenda,
                                   long.min = busca_dado$long.min, long.max = busca_dado$long.max,
                                   lat.min = busca_dado$lat.min, lat.max = busca_dado$lat.max,
                                   horario = busca_dado$horario,
                                   altura = 8, largura = 8)
                   }, res = 96, width = 700, height = 700)
                 
                 })
  
  # ler arquivo de pontos
  tabela_pontos <- reactive({
    req(input$arquivo_pontos)
    read.csv(input$arquivo_pontos$datapath)
  })
  
  # mostrar tabela de pontos
  output$pontos <- renderTable(expr = tabela_pontos())
  
  # botão de baixar imagem
  output$baixar <- downloadHandler(
    filename = function(){
      sprintf("radar_%s_%s_%s_%s.png", input$radar, input$produto, input$data, input$hora)
      },
    content = function(file){
      ggsave(filename = file,
             plot = last_plot(),
             height = 8,
             width = 8,
             units = "in",
             dpi = 300)
    },
    contentType = "image/png"
  )

}

shinyApp(ui, server)
