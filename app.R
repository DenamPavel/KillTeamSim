library(tidyverse)
library(scales)
library(glue)
library(shiny)
library(shinyWidgets)
library(DT)



ui <- fluidPage(
    titlePanel("Kill Team Comparative Simulator"),
    sidebarLayout(sidebarPanel(fluidRow(
        column(
            4,
            h3('Left'),
            numericInputIcon(
                'attacks1',
                label = 'Attacks',
                value = 5,
                min = 1,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'bs1',
                label = 'BS',
                value = 3,
                min = 1,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'normaldamage1',
                label = 'Normal Damage',
                value = 3,
                min = 1,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'critdamage1',
                label = 'Critical Damage',
                value = 4,
                min = 1,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'ap1',
                label = 'AP',
                value = 0,
                min = 0,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'piercing1',
                label = 'Piercing',
                value = 0,
                min = 0,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'lethal1',
                label = 'Lethal',
                value = 0,
                min = 0,
                max = 5,
                step = 1
            ),
            numericInputIcon(
                'mw1',
                label = 'Mortal Wounds',
                value = 0,
                min = 0,
                max = 10,
                step = 1
            ),
            awesomeCheckbox('rending1',
                            label = 'Rending',
                            value = FALSE),
            awesomeCheckbox('starfire1',
                            label = 'Starfire (Crit causes Miss -> Hit)',
                            value = FALSE),
            awesomeCheckbox('semilethal1',
                            label = 'Semilethal (One 5+ Hit to Crit)',
                            value = FALSE),
            pickerInput(
                'reroll1',
                label = 'Reroll',
                choices = c("", "C", "B", "R")
            )
        ),
        column(
            4,
            h3('Right'),
            numericInputIcon(
                'attacks2',
                label = 'Attacks',
                value = 4,
                min = 1,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'bs2',
                label = 'BS',
                value = 3,
                min = 1,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'normaldamage2',
                label = 'Normal Damage',
                value = 3,
                min = 1,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'critdamage2',
                label = 'Critical Damage',
                value = 4,
                min = 1,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'ap2',
                label = 'AP',
                value = 0,
                min = 0,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'piercing2',
                label = 'Piercing',
                value = 0,
                min = 0,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'lethal2',
                label = 'Lethal',
                value = 0,
                min = 0,
                max = 5,
                step = 1
            ),
            numericInputIcon(
                'mw2',
                label = 'Mortal Wounds',
                value = 0,
                min = 0,
                max = 10,
                step = 1
            ),
            awesomeCheckbox('rending2',
                            label = 'Rending',
                            value = FALSE),
            awesomeCheckbox('starfire2',
                            label = 'Starfire (Crit causes Miss -> Hit)',
                            value = FALSE),
            awesomeCheckbox('semilethal2',
                            label = 'Semilethal (One 5+ Hit to Crit)',
                            value = FALSE),
            pickerInput(
                'reroll2',
                label = 'Reroll',
                choices = c("", "C", "B", "R")
            )
        ),
        column(
            4,
            h3('Defender'),
            numericInputIcon(
                'save',
                label = 'Save',
                value = 3,
                min = 1,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'defense',
                label = 'Defense',
                value = 3,
                min = 1,
                max = 10,
                step = 1
            ),
            numericInputIcon(
                'cover',
                label = 'Cover',
                value = 0,
                min = 0,
                max = 4,
                step = 1
            ),
            numericInputIcon(
                'fnp',
                label = 'FNP',
                value = 0,
                min = 0,
                max = 6,
                step = 1
            )
            ,
        )
    )),
    mainPanel(fluidRow(
        plotOutput("ComparisonChart"),
        dataTableOutput("W3Table")
    )))
)


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
                 FNP) {
            Output <- do.call(rbind, lapply(1:k, function(p) {
                set.seed(SeedH[p])
                Rolls <- sample(1:6, Attacks, replace = T)
                set.seed(SeedS[p])
                #Lethal
                CritNumber <- ifelse(Lethal > 0, Lethal, 6)
                
                
                
                
                #Rerolls
                Rolls <-
                    if (Rerolls == 'C') {
                        as.numeric(c(Rolls[Rolls >= BS], sample(
                            1:6, length(Rolls[Rolls == 1]), replace = T
                        )))
                    } else if (Rerolls == 'R') {
                        as.numeric(c(Rolls[Rolls >= BS], sample(
                            1:6, length(Rolls[Rolls < BS]), replace = T
                        )))
                    } else{
                        Rolls
                    }
                Rolls <- sort(Rolls)
                Rolls[1] <-
                    if (Rerolls == 'B') {
                        ifelse(sort(Rolls)[1] < BS,
                               sample(1:6, 1, replace = T),
                               sort(Rolls)[1])
                    } else{
                        Rolls[1]
                    }
                
                Hits <-
                    as.numeric(length(Rolls[Rolls >= BS & Rolls < CritNumber]))
                Crits <-
                    as.numeric(length(Rolls[Rolls >= CritNumber]))
                
                
                #Semi-Lethal
                SemiL <-
                    ifelse(SemiLethal == TRUE &
                               (
                                   Lethal < 5 &
                                       (length(Rolls[Rolls >= BS &
                                                         Rolls == 5]) >= 1) |
                                       Lethal < 4 & length(Rolls[Rolls >= BS & Rolls == 4]) >= 1
                               ),
                           1,
                           0)
                Hits <- Hits - SemiL
                Crits <- Crits + SemiL
                
                #Rending
                Rends <-
                    ifelse(Rending == TRUE & Crits >= 1 & Hits >= 1, 1, 0)
                Crits <- ifelse(Rends == 1, Crits + 1, Crits)
                Hits <- ifelse(Rends == 1, Hits - 1, Hits)
                
                #Starfire
                Exploit <-
                    ifelse(Starfire == TRUE &
                               (Crits + Hits) < Attacks & Crits >= 1,
                           1,
                           0)
                Hits <- ifelse(Exploit == 1, Hits + 1, Hits)
                
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
                DefenseUse <- if (Cover > 0) {
                    DefenseUse - Cover
                } else{
                    DefenseUse
                }
                Saves <- sample(1:6, DefenseUse, replace = T)
                
                
                NSaves <-
                    as.numeric(length(Saves[Saves >= Save & Saves < 6]))
                NSaves <- if (Cover > 0) {
                    NSaves + Cover
                } else{
                    NSaves
                }
                CSaves <- as.numeric(length(Saves[Saves == 6]))
                #FIX FOR MELTAS
                
                

                
                C2H <-
                    ifelse(CritDamage < NormalDamage &
                               Hits - NSaves > 0,
                            max(0,CSaves - (Hits - NSaves)),
                           0)
                
                
                C2H <-
                    ifelse(CSaves > Crits &
                               CritDamage >= NormalDamage,
                           CSaves - Crits,
                           C2H)
                
                CSaves <- CSaves - C2H
                
                H2C <-
                    ifelse(Crits > 0 &
                               NSaves - Hits >= 1 &
                               CritDamage >= NormalDamage &
                               NSaves >= 2,
                           2,
                           0)
                CritO <- Crits - (H2C / 2)
                

                
                NSaves <- NSaves - H2C
                HitO <- max(0, Hits - NSaves - C2H)
                H2C2 <-
                    ifelse(NSaves - Hits >= 2 &
                               CritDamage <= 2 * NormalDamage,
                           floor((NSaves - Hits) / 2),
                           0)
                H2C2 <-
                    ifelse(CritDamage > 2 * NormalDamage, floor((NSaves - Hits) / 2), H2C2)
                
                CSaves <- CSaves + (H2C/2) + H2C2
                
                CritO <- max(0, CritO - CSaves)
                NSaves <- NSaves - (H2C2 * 2)

                NDamage <-
                    ifelse(HitO * NormalDamage > 0, max(0,HitO * NormalDamage), 0)
                CDamage <-
                    ifelse(CritO * CritDamage > 0, max(0,CritO * CritDamage), 0)
                Damage <-
                    ifelse(NDamage + CDamage < 0, 0, NDamage + CDamage) + Mortals
                
                #FNP
                FNPRolls <- sample(1:6, Damage, replace = T)
                Damage <-
                    ifelse(FNP > 0, Damage - length(FNPRolls[FNPRolls >= FNP]), Damage)
                DamageOutput <-
                    as.data.frame(Damage)
                return(DamageOutput)
            }))
            
            
            #m <- 3
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
                c(min(OutputProb$Number):max(OutputProb$Number))
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
    SeedS <- sample(1:k, k, replace = TRUE)
    

    
    A <- 'Left'
    B <- 'Right'
    C <- reactive({input$save})
    
    W1 <- reactive({
        W1 <- RangedSim(
            Attacks = input$attacks1,
            BS = input$bs1,
            NormalDamage = input$normaldamage1,
            CritDamage = input$critdamage1,
            Save = input$save,
            Defense = input$defense,
            k = k,
            Name = A,
            Piercing = input$piercing1,
            AP = input$ap1,
            Cover = input$cover,
            Rerolls = input$reroll1,
            Rending = input$rending1,
            Starfire = input$starfire1,
            MW = input$mw1,
            Lethal = input$lethal1,
            SemiLethal = input$semilethal1,
            FNP = input$fnp
        ) %>% filter(Number2 > 0)
        return(W1)
    })
    
    
    W2 <- reactive({
        W2 <- RangedSim(
            Attacks = input$attacks2,
            BS = input$bs2,
            NormalDamage = input$normaldamage2,
            CritDamage = input$critdamage2,
            Save = input$save,
            Defense = input$defense,
            k = k,
            Name = B,
            Piercing = input$piercing2,
            AP = input$ap2,
            Cover = input$cover,
            Rerolls = input$reroll2,
            Rending = input$rending2,
            Starfire = input$starfire2,
            MW = input$mw2,
            Lethal = input$lethal2,
            SemiLethal = input$semilethal2,
            FNP = input$fnp
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
        DT::renderDataTable(DT::datatable(W3()))
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
