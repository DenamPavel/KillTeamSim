library(tidyverse)
library(scales)
library(glue)
library(shiny)
library(shinyWidgets)
library(DT)
library(shinythemes)



ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Kill Team Comparative Simulator"),
    sidebarLayout(
        sidebarPanel( width = 4,fluidRow(
            column(4,            h3('Blue'),
                   tabsetPanel(
                       id = 'Weapon 1',
                       tabPanel(
                           'Weapon Values',
                           value = 1,
                           selectInput("attacks1", label = strong("Attacks"),
                                       choices = c(1:30),
                                       selected = 4),
                           selectInput("bs1", label = strong("BS"), 
                                       choices = c(2:6), 
                                       selected = 3),
                           selectInput("normaldamage1", label = strong("Normal Damage"), 
                                       choices = c(0:10), 
                                       selected = 3),
                           selectInput("critdamage1", label = strong("Critical Damage"), 
                                       choices = c(0:10), 
                                       selected = 4),
                           selectInput("ap1", label = strong("AP"), 
                                       choices = c(0:4), 
                                       selected = 0),
                           selectInput("piercing1", label = strong("Piercing"), 
                                       choices = c(0:4), 
                                       selected = 0),
                           selectInput("lethal1", label = strong("Lethal"), 
                                       choices = c(0:5), 
                                       selected = 0),
                           selectInput("mw1", label = strong("Mortal Wounds"), 
                                       choices = c(0:10), 
                                       selected = 0),
                           pickerInput(
                               'reroll1',
                               label = 'Reroll',
                               choices = c("None", "Ceaseless", "Balanced", "Relentless","Relentless CritFish","Cult Ambush")
                           )
                       ),
                       tabPanel(
                           'Special Rules',
                           value = 2,
                           awesomeCheckbox('rending1',
                                           label = 'Rending',
                                           value = FALSE),
                           awesomeCheckbox('starfire1',
                                           label = 'Starfire (Crit causes Miss -> Hit)',
                                           value = FALSE),
                           awesomeCheckbox('semilethal1',
                                           label = 'Semilethal (One 5+ Hit to Crit)',
                                           value = FALSE),
                           awesomeCheckbox('elite1',
                                           label = 'Elite',
                                           value = FALSE),
                           awesomeCheckbox('closeassault1',
                                           label = 'Close Assault (2 hits cause Miss -> Hit)',
                                           value = FALSE),
                           selectInput("autoretain1", label = strong("AutoRetain"), 
                                       choices = c(0:5), 
                                       selected = 0),
                           awesomeCheckbox('forcecrit1',
                                             label = 'Forced Crit',
                                             value = FALSE),
                           awesomeCheckbox('nocover1',
                                           label = 'No Cover',
                                           value = FALSE),
                           awesomeCheckbox('RR61',
                                           label = 'Force Reroll 6s',
                                           value = FALSE)
                       )
                   )),
            column(4,            h3('Red'),
                   tabsetPanel(
                     id = 'Weapon 1',
                     tabPanel(
                       'Weapon Values',
                       value = 1,
                       selectInput("attacks2", label = strong("Attacks"),
                                   choices = c(1:30),
                                   selected = 4),
                       selectInput("bs2", label = strong("BS"), 
                                   choices = c(2:6), 
                                   selected = 3),
                       selectInput("normaldamage2", label = strong("Normal Damage"), 
                                   choices = c(0:10), 
                                   selected = 2),
                       selectInput("critdamage2", label = strong("Critical Damage"), 
                                   choices = c(0:10), 
                                   selected = 3),
                       selectInput("ap2", label = strong("AP"), 
                                   choices = c(0:4), 
                                   selected = 0),
                       selectInput("piercing2", label = strong("Piercing"), 
                                   choices = c(0:4), 
                                   selected = 0),
                       selectInput("lethal2", label = strong("Lethal"), 
                                   choices = c(0:5), 
                                   selected = 0),
                       selectInput("mw2", label = strong("Mortal Wounds"), 
                                   choices = c(0:10), 
                                   selected = 0),
                       pickerInput(
                         'reroll2',
                         label = 'Reroll',
                         choices = c("None", "Ceaseless", "Balanced", "Relentless","Relentless CritFish","Cult Ambush")
                       )
                     ),
                     tabPanel(
                       'Special Rules',
                       value = 2,
                       awesomeCheckbox('rending2',
                                       label = 'Rending',
                                       value = FALSE),
                       awesomeCheckbox('starfire2',
                                       label = 'Starfire (Crit causes Miss -> Hit)',
                                       value = FALSE),
                       awesomeCheckbox('semilethal2',
                                       label = 'Semilethal (One 5+ Hit to Crit)',
                                       value = FALSE),
                       awesomeCheckbox('elite2',
                                       label = 'Elite',
                                       value = FALSE),
                       awesomeCheckbox('closeassault2',
                                       label = 'Close Assault (2 hits cause Miss -> Hit)',
                                       value = FALSE),
                       selectInput("autoretain2", label = strong("AutoRetain"), 
                                   choices = c(0:5), 
                                   selected = 0),
                       awesomeCheckbox('forcecrit2',
                                       label = 'Forced Crit',
                                       value = FALSE),
                       awesomeCheckbox('nocover2',
                                       label = 'No Cover',
                                       value = FALSE),
                       awesomeCheckbox('RR62',
                                       label = 'Force Reroll 6s',
                                       value = FALSE)
                     )
                   )),
            column(
                4,
                h3('Defender'),
                tabsetPanel(
                    id = 'Defender',
                    tabPanel(
                        'Defender Values',
                        value = 1,
                        selectInput("save", label = strong("Save"), 
                                    choices = c(1:6), 
                                    selected = 3),
                        selectInput("defense", label = strong("Defense"), 
                                    choices = c(0:10), 
                                    selected = 3),
                        selectInput("cover", label = strong("Cover"), 
                                    choices = c(0:4), 
                                    selected = 0)
                    ),
                    tabPanel(
                        'Special Rules',
                        value = 2,
                        selectInput("fnp", label = strong("FNP"), 
                                    choices = c(0:6), 
                                    selected = 0),
                        selectInput("dfrerolls", label = strong("Defensive Rerolls"), 
                                    choices = c(0:6), 
                                    selected = 0),
                        awesomeCheckbox('dgbanner',
                                        label = 'FNP 1/2 Rerolls',
                                        value = FALSE)
                        ,
                        awesomeCheckbox('thuman',
                                        label = 'Transhuman (Save -> Crit Save)',
                                        value = FALSE)
                        ,awesomeCheckbox('starsaves',
                                        label = 'Starfire Saves (Crit -> Convert Failed Save)',
                                        value = FALSE)
                        ,awesomeCheckbox('l5saves',
                                        label = '5+ Crit Saves',
                                        value = FALSE)
                        ,awesomeCheckbox('blessPox',
                                         label = 'Blessings of Pox',
                                         value = FALSE)
                    )
                )
                ,
            )
        )),
        mainPanel(
            checkboxInput(
                inputId = "themeToggle",
                label = icon("sun"),
                value = TRUE
            ),
            fluidRow(plotOutput("ComparisonChart"),
                     dataTableOutput("W3Table"))
        )
    ),
    tags$script(
        "
        // define css theme filepaths
        const themes = {
            dark: 'shinythemes/css/darkly.min.css',
            light: 'shinythemes/css/flatly.min.css'
        }

        // function that creates a new link element
        function newLink(theme) {
            let el = document.createElement('link');
            el.setAttribute('rel', 'stylesheet');
            el.setAttribute('text', 'text/css');
            el.setAttribute('href', theme);
            return el;
        }

        // function that remove <link> of current theme by href
        function removeLink(theme) {
            let el = document.querySelector(`link[href='${theme}']`)
            return el.parentNode.removeChild(el);
        }

        // define vars
        const darkTheme = newLink(themes.dark);
        const lightTheme = newLink(themes.light);
        const head = document.getElementsByTagName('head')[0];
        const toggle = document.getElementById('themeToggle');

        // define extra css and add as default
        const extraDarkThemeCSS = '.dataTables_length label, .dataTables_filter label, .dataTables_info {       color: white!important;} .paginate_button { background: white!important;} thead { color: white;}'
        const extraDarkThemeElement = document.createElement('style');
        extraDarkThemeElement.appendChild(document.createTextNode(extraDarkThemeCSS));
        head.appendChild(extraDarkThemeElement);


        // define event - checked === 'light'
        toggle.addEventListener('input', function(event) {
            // if checked, switch to light theme
            if (toggle.checked) {
                removeLink(themes.dark);
                head.removeChild(extraDarkThemeElement);
                head.appendChild(lightTheme);
            }  else {
                // else add darktheme
                removeLink(themes.light);
                head.appendChild(extraDarkThemeElement)
                head.appendChild(darkTheme);
            }
        })
        "
    )
)
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

server <- function(input, output) {
    RangedSim <-
        function(Attacks,
                 BS,
                 NormalDamage,
                 CritDamage,
                 Save,
                 Defense,
                 k,
                 Name,
                 Piercing,
                 AP,
                 Cover,
                 Rerolls,
                 Rending,
                 Starfire,
                 MW,
                 Lethal,
                 SemiLethal,
                 Elite,
                 FNP,
                 DfRerolls,
                 DGBanner,
                 AutoRetain,
                 ForcedCrit,
                 Transhuman,
                 StarfireSaves,
                 NoCover,
                 L5Saves,
                 PoxBless,
                 CloseAssault,
                 RRSixes) {
            Output <- do.call(rbind, lapply(1:k, function(p) {
                #Uses the same basic seed for both weapons
                Attacks <- max(0,as.numeric(Attacks)-AutoRetain)
                set.seed(SeedH[p])
                Rolls <- sample(1:6, Attacks, replace = T)
                set.seed(SeedL[p])
                if(RRSixes == TRUE){
                Rolls2 <- sample(1:6,length(subset(Rolls,Rolls == 6)), replace = TRUE)
                Rolls <- subset(Rolls, Rolls != 6)
                }
                set.seed(SeedR[p])
                #Lethal
                CritNumber <- ifelse(Lethal > 0, Lethal, 6)

                
                #Rerolls
                Rolls <-
                    if (Rerolls == 'Ceaseless') {
                        as.numeric(c(Rolls[Rolls >= BS], sample(
                            1:6, length(Rolls[Rolls == 1]), replace = T
                        ),Rolls[Rolls>1 & Rolls <BS]))
                    } else if (Rerolls == 'Relentless') {
                        as.numeric(c(Rolls[Rolls >= BS], sample(
                            1:6, length(Rolls[Rolls < BS]), replace = T
                        )))
                    } else if (Rerolls == 'Relentless CritFish') {
                        as.numeric(c(Rolls[Rolls >= CritNumber], sample(
                            1:6, length(Rolls[Rolls < CritNumber]), replace = T
                        )))
                    } else if (Rerolls == 'Cult Ambush') {
                        # Attacks <- 4
                        # BS <- 2
                        # Rolls <- sample(1:6, Attacks, replace = T)
                        as.numeric(c(Rolls[Rolls >= BS], sample(
                            1:6, length(subset(Rolls, Rolls == getmode(Rolls[Rolls<BS]))), replace = T
                        ),Rolls[Rolls < BS & Rolls != getmode(Rolls[Rolls<BS])]))
                    } else{
                        Rolls
                    }
                Rolls <- sort(Rolls)
                Rolls[1] <-
                    if (Rerolls == 'Balanced') {
                        ifelse(sort(Rolls)[1] < BS,
                               sample(1:6, 1, replace = T),
                               sort(Rolls)[1])
                    } else{
                        Rolls[1]
                    }
                if(RRSixes == TRUE){
                Rolls <- c(Rolls, Rolls2)}
                Hits <- suppressWarnings(if(is.na(Rolls)){AutoRetain} else{
                
                    as.numeric(length(Rolls[Rolls >= BS &
                                                Rolls < CritNumber])) + AutoRetain})
                Crits <- suppressWarnings(if(is.na(Rolls)){0} else{
                    as.numeric(length(Rolls[Rolls >= CritNumber &
                                                Rolls >= BS]))})
                
                #Elite
                if(Elite == TRUE & MW + CritDamage >= 2*NormalDamage & Hits > 0){
                  Hits <- Hits - 1
                  Crits <- Crits + 1
                } else if(Elite == TRUE & MW + CritDamage >= 2*NormalDamage & Hits == 0 & Crits < Attacks){
                  Crits <- Crits + 1
                }
                if(Elite == TRUE & Hits + Crits < Attacks){
                Hits <-  Hits + 1
                }else if(Elite == TRUE & Hits + Crits == Attacks & Crits < Attacks){
                Hits <- Hits -1
                Crits <- Crits + 1
                }
                
                
                
                #Semi-Lethal (Convert one 5 into a crit or a 4/5 if you're Lethal 5+)
                SemiL <-
                    ifelse(SemiLethal == TRUE &
                               (
                                   Lethal < 5 &
                                       (length(Rolls[Rolls >= BS &
                                                         Rolls == 5]) >= 1) |
                                       Lethal < 4 &
                                       Lethal > 0 &
                                       length(Rolls[Rolls >= BS &
                                                        Rolls == 4]) >= 1
                               ),
                           1,
                           0)
                Hits <- Hits - SemiL
                Crits <- Crits + SemiL
                if(Hits > 0 & ForcedCrit == TRUE){
                  Crits <- Crits + 1
                  Hits <- Hits - 1
                }
                # Crits <- ifelse(ForcedCrit == TRUE & Hits > 0, Crits + 1, Crits)
                # Hits <- ifelse(ForcedCrit == TRUE & Hits > 0,Hits-1,Hits)
                
                
                #Starfire
                Exploit <-
                    ifelse(Starfire == TRUE &
                               (Crits + Hits) < Attacks &
                               Crits >= 1,
                           1,
                           0)
                Hits <- ifelse(Exploit == 1, Hits + 1, Hits)
                
                #Rending
                Rends <-
                    ifelse(Rending == TRUE &
                               Crits >= 1 & Hits >= 1, 1, 0)
                Crits <- ifelse(Rends == 1, Crits + 1, Crits)
                Hits <- ifelse(Rends == 1, Hits - 1, Hits)
                
                Hits <- ifelse(Hits+Crits >=2 & Hits+Crits < Attacks & CloseAssault == TRUE, Hits + 1, Hits)
                
                #Mortal Wounds
                Mortals <- MW * Crits
                
                #Piercing and AP
                DefenseUse <-
                    if (Piercing > 0 &
                        Piercing >= AP &
                        Crits > 0) {
                        Defense - Piercing
                    } else if (AP > 0) {
                        Defense - AP
                    } else {
                        Defense
                    }
                DefenseUse <- if (Cover > 0 & NoCover == FALSE) {
                    DefenseUse - Cover
                } else{
                    DefenseUse
                }
                DefenseUse <- max(0,DefenseUse)
                set.seed(SeedS[p])
                Saves <- sample(1:6, max(0,DefenseUse), replace = T)
                
                Saves <- if (DfRerolls > 0) {
                    as.numeric(c(Saves[Saves >= Save], sample(
                        1:6, min(length(Saves[Saves < Save]), DfRerolls), replace = T
                    )))
                }  else {
                    Saves
                }
                
                
                NSaves <-
                    as.numeric(length(Saves[Saves >= Save &
                                                Saves < ifelse(L5Saves == FALSE,6,5)]))
                NSaves <- if (Cover > 0 & NoCover == FALSE) {
                    NSaves + Cover
                } else{
                    NSaves
                }
                if(PoxBless == TRUE & sum(Saves[Saves<Save]) >=7){
                  NSaves <- NSaves + 1}
                #Cap saves at Defense dice minus AP
                NSaves <- min(NSaves, Defense - AP)
                CSaves <- if(L5Saves == TRUE){
                  as.numeric(length(Saves[Saves >= 5]))
                  }else{
                as.numeric(length(Saves[Saves == 6]))}
                
                if(Transhuman == TRUE & NSaves > 0){
                    CSaves <- CSaves + 1
                    NSaves <- NSaves - 1
                }
                
                if(StarfireSaves == TRUE & CSaves > 1 & NSaves + CSaves < Defense - AP){
                    NSaves <- NSaves + 1
                }
                
                
                #Convert Excess crit saves to normal saves
                C2H <- 0
                C2H <-
                    ifelse(CritDamage < NormalDamage &
                               Hits - NSaves > 0,
                           max(CSaves, CSaves - (Hits - NSaves)),
                           0)
                
                
                C2H <-
                    ifelse(CSaves > Crits &
                               CritDamage >= NormalDamage,
                           CSaves - Crits,
                           C2H)
                
                CSaves <- CSaves - C2H
                NSaves <- NSaves + C2H
                
                #Convert excess normal saves to crit saves if crit damage is more than or equal to normal damage
                
                H2C <-
                    ifelse(Crits - CSaves > 0 &
                               NSaves > Hits &
                               CritDamage >= NormalDamage &
                               NSaves >= 2,
                           2,
                           0)
                CSaves <- CSaves + (H2C / 2)
                NSaves <- NSaves - H2C
                
                #Do it again in the case of 4 defense dice
                
                H2C <-
                    ifelse(Crits > 0 &
                               NSaves > Hits &
                               CritDamage >= NormalDamage &
                               NSaves >= 2,
                           2,
                           0)
                CSaves <- CSaves + (H2C / 2)
                NSaves <- NSaves - H2C
                
                
                
                # H2C2 <-
                #     ifelse(NSaves - Hits >= 2 &
                #                CritDamage <= 2 * NormalDamage,
                #            floor((NSaves - Hits) / 2),
                #            0)
                
                #Convert all normal saves to crit saves if crit damage is more than double normal damage
                
                H2C2 <-
                    ifelse(CritDamage > 2 * NormalDamage, floor((NSaves - (
                        Crits - CSaves
                    )) / 2), 0)
                CSaves <- CSaves + H2C2
                NSaves <- NSaves - (H2C2 * 2)
                
                #Find obtained Hits and Crits
                
                HitO <- max(0, Hits - NSaves)
                CritO <- max(0, Crits - CSaves)
                # CSaves <- CSaves + (H2C/2) + H2C2
                
                
                # NSaves <- NSaves - (H2C2 * 2)
                
                #Calculate damage
                
                NDamage <-
                    ifelse(HitO * NormalDamage > 0, max(0, HitO * NormalDamage), 0)
                CDamage <-
                    ifelse(CritO * CritDamage > 0, max(0, CritO * CritDamage), 0)
                Damage <-
                    ifelse(NDamage + CDamage < 0, 0, NDamage + CDamage) + Mortals
                
                #FNP rolls
                FNPRolls <- sample(1:6, Damage, replace = T)
                FNPRolls <-
                    if(DGBanner == TRUE){c(FNPRolls[FNPRolls >= 3], sample(1:6, length(FNPRolls[FNPRolls <=
                                                                                                        2 & FNPRolls < FNP]), replace = TRUE))}else {FNPRolls}
                Damage <-
                    max(0,ifelse(FNP > 0, Damage - length(FNPRolls[FNPRolls >= FNP]), Damage))
                DamageOutput <-
                    as.data.frame(Damage)
                return(DamageOutput)
            }))
            
            
            OutputProb <-
                do.call(rbind, lapply(unique(c(
                    0, unique(Output$Damage)
                )), function(m) {
                    TotalDamage <- Output %>% filter(Damage == m) %>% nrow()
                    DamageNumber <-
                        Output %>% filter(Damage >= m) %>% nrow() / k
                    DamageNumber <-
                        as.data.frame(DamageNumber) %>% mutate(Number = m,
                                                               Events = TotalDamage,
                                                               Number2 = m)
                    return(DamageNumber)
                }))
            Number <-
                c(1:max(OutputProb$Number))
            NumberVector <- as.data.frame(Number)
            OutputProb <-
                left_join(NumberVector, OutputProb) %>% fill(DamageNumber, .direction = 'up') %>% fill(Events, .direction = 'up')
            return(OutputProb)
        }
    
    Gun2Chart <- function(A1, B1, Title) {
        W3 <-
            full_join(A1, B1, by = "Number") %>% filter(Number > 0) %>% arrange(Number) %>%
            fill(DamageNumber.x, .direction = 'up') %>%
            fill(DamageNumber.y, .direction = 'up')
        colnames(W3)[c(2, 5)] <- c('W1', "W2")
        
        return(W3)
        #return(ComparisonChart)
    }
    k <- 1000
    SeedH <- sample(1:k, k, replace = TRUE)
    SeedL <- sample(1:k, k, replace = TRUE)
    SeedR <- sample(1:k, k, replace = TRUE)
    SeedS <- sample(1:k, k, replace = TRUE)
    
    
    
    A <- 'Left'
    B <- 'Right'
    C <- reactive({
        input$save
    })
    
    W1 <- reactive({
        W1 <- RangedSim(
            Attacks = as.numeric(input$attacks1),
            BS = as.numeric(input$bs1),
            NormalDamage = as.numeric(input$normaldamage1),
            CritDamage = as.numeric(input$critdamage1),
            Save = as.numeric(input$save),
            Defense = as.numeric(input$defense),
            k = k,
            Name = A,
            Piercing = as.numeric(input$piercing1),
            AP = as.numeric(input$ap1),
            Cover = as.numeric(input$cover),
            Rerolls = input$reroll1,
            Rending = input$rending1,
            Starfire = input$starfire1,
            MW = as.numeric(input$mw1),
            Lethal = as.numeric(input$lethal1),
            SemiLethal = input$semilethal1,
            Elite = input$elite1,
            FNP = as.numeric(input$fnp),
            DfRerolls = as.numeric(input$dfrerolls),
            DGBanner = input$dgbanner,
            AutoRetain = as.numeric(input$autoretain1),
            ForcedCrit = input$forcecrit1,
            Transhuman = input$thuman,
            StarfireSaves = input$starsaves,
            NoCover = input$nocover1,
            L5Saves = input$l5saves,
            PoxBless = input$blessPox,
            CloseAssault = input$closeassault1,
            RRSixes = input$RR61
        ) %>% filter(Number2 > 0)
        return(W1)
    })
    
    
    W2 <- reactive({
        W2 <- RangedSim(
            Attacks = as.numeric(input$attacks2),
            BS = as.numeric(input$bs2),
            NormalDamage = as.numeric(input$normaldamage2),
            CritDamage = as.numeric(input$critdamage2),
            Save = as.numeric(input$save),
            Defense = as.numeric(input$defense),
            k = k,
            Name = B,
            Piercing = as.numeric(input$piercing2),
            AP = as.numeric(input$ap2),
            Cover = as.numeric(input$cover),
            Rerolls = input$reroll2,
            Rending = input$rending2,
            Starfire = input$starfire2,
            MW = as.numeric(input$mw2),
            Lethal = as.numeric(input$lethal2),
            SemiLethal = input$semilethal2,
            Elite = input$elite2,
            FNP = as.numeric(input$fnp),
            DfRerolls = as.numeric(input$dfrerolls),
            DGBanner = input$dgbanner,
            AutoRetain = as.numeric(input$autoretain2),
            ForcedCrit = input$forcecrit2,
            Transhuman = input$thuman,
            StarfireSaves = input$starsaves,
            NoCover = input$nocover2,
            L5Saves = input$l5saves,
            PoxBless = input$blessPox,
            CloseAssault = input$closeassault2,
            RRSixes = input$RR62
        ) %>% filter(Number2 > 0)
        return(W2)
    })
    
    
    GunChart <- reactive({
        Gun2Chart(W1(), W2(), C)
    })
    W3 <- reactive({
        Gun2Chart(W1(), W2(), C)
    })
    output$W3Table <-
        DT::renderDataTable(DT::datatable(
            select(
                W3(),
                Damage = Number,
                Left = W1,
                'Left Events' = Events.x,
                Right = W2,
                'Right Events' = Events.y
            ),
            options = list(paging = TRUE, pageLength = 60,dom = 't',
                           initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'color': '#000'});",
                               "}")
                           )
        )%>% 
          formatPercentage(c("Left", "Right"), 1))
    colors <- c("blue", "red")
    names(colors) <- c(A, B)
    
    output$ComparisonChart <- renderPlot({
        ggplot(W3()) +
            geom_area(
                aes(x = Number, y = W1),
                stat = "identity",
                color = 'blue',
                fill = 'light blue'
            ) +
            geom_line(aes(
                x = min(Number),
                y = min(W1),
                color = A
            )) +
            geom_area(
                aes(x = Number, y = W2),
                stat = "identity",
                color = 'red',
                fill = 'red',
                alpha = .5
            ) +
            geom_text(
                aes(
                    label = paste(Number, '\n', scales::percent(W1, .1)),
                    x = Number,
                    y = W1
                ),
                colour = "blue",
                fontface = "bold",
                size = 3
            ) +
            geom_text(
                aes(
                    label = paste(Number, '\n', scales::percent(W2, .1)),
                    x = Number,
                    y = W2
                ),
                colour = "black",
                fontface = "bold",
                size = 3
            ) +
            scale_x_continuous(breaks = round(seq(
                min(W3()$Number), max(W3()$Number), by = 1
            ), 1)) +
            scale_y_continuous(labels = scales::percent,
                               limits = c(0, max(c(
                                   W3()$W1, W3()$W2
                               )))) +
            theme_bw() +
            xlab('Wounds') +
            ylab('Chance of Doing of at Least X Wounds') +
            labs(color = "Legend", title = C()) +
            scale_color_manual(values = colors)
    })

}
shinyApp(ui, server)
