library(tidyverse)
library(scales)
library(glue)
#library(textclean)

Attacks <- 5
BS <- 3
NormalDamage <- 4
CritDamage <- 5
Save <- 3
Defense <- 3
k <- 1000
p <- 1
Piercing <- 0
AP <- 0
Cover <- 0
Rerolls <- ''
Rending <- 0
Starfire <- 0
MW <- 0
Lethal <- 0
FNP <- 0
NSaves <- 3
Hits <- 0


Rolls <- c(6, 6, 1, 1, 4)
Saves <- c(6, 5, 5)
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
          as.numeric(c(Rolls[Rolls>=BS],sample(1:6, length(Rolls[Rolls==1]), replace = T)))
        } else if (Rerolls == 'R') {
          as.numeric(c(Rolls[Rolls>=BS],sample(1:6, length(Rolls[Rolls<BS]), replace = T)))
        } else{
          Rolls
        }
      Rolls <- sort(Rolls)
      Rolls[1] <-
        if (Rerolls == 'B') {
          ifelse(sort(Rolls)[1] < BS, sample(1:6, 1, replace = T), sort(Rolls)[1])
        } else{
          Rolls[1]
        }
      
      Hits <- as.numeric(length(Rolls[Rolls >= BS & Rolls < CritNumber]))
      Crits <- as.numeric(length(Rolls[Rolls >= CritNumber]))
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
      
      #Rending
      Rends <- ifelse(Rending == 1 & Crits >=1 & Hits >= 1,1,0)
      Crits <- ifelse(Rends == 1,Crits+1,Crits)
      Hits <- ifelse(Rends == 1, Hits-1, Hits)
      
      #Starfire
      Exploit <- ifelse(Starfire == 1 & (Crits + Hits) < Attacks & Crits >=1, 1, 0)
      Hits <- ifelse(Exploit == 1, Hits+1, Hits)
      
      #Mortal Wounds
      Mortals <- MW * Crits
      
      
      
      NSaves <- as.numeric(length(Saves[Saves >= Save & Saves < 6]))
      NSaves <- if (Cover > 0) {
        NSaves + Cover
      }else{
        NSaves
      }
      CSaves <- as.numeric(length(Saves[Saves == 6]))
      #FIX FOR MELTAS

      
      CritO <- ifelse(Crits - CSaves >= 0 & CritDamage >= NormalDamage, Crits - CSaves, 0)
      C2H <- ifelse(CSaves > Crits & CritDamage >= NormalDamage, CSaves - Crits, 0)
      C2H <- ifelse(CritDamage < NormalDamage, max(CSaves,Hits-NSaves),C2H)
      
      H2C <-
        ifelse(CritO > 0 &
                 NSaves - Hits >= 1 &
                 CritDamage >= NormalDamage & 
                 NSaves >= 2,
               2,
               0)
      CritO <- CritO - H2C/2
        # ifelse(CritO > 0 &
        #          Hits - NSaves <= -1 &
        #          CritDamage > NormalDamage & 
        #          NSaves >= 2,
        #        CritO - 1,
        #        CritO)
      
      NSaves <- NSaves - H2C
      HitO <- max(0,Hits - NSaves-C2H)
      H2C2 <- ifelse(NSaves - Hits >= 2 & CritDamage <= 2*NormalDamage, floor((NSaves-Hits)/2),0)
      H2C2 <- ifelse(CritDamage > 2*NormalDamage, floor((NSaves-Hits)/2),H2C2)
      CritO <- max(0,CritO-H2C2)
      NSaves <- NSaves - H2C2*2
      #REDO THIS PART
      # H2C2 <-
      #   ifelse(NSaves - Hits > 0 & CritDamage >= NormalDamage, floor((NSaves - Hits) / 2), 0)
      # CritO <- CritO - (H2C + H2C2)
      # NSaves <- NSaves - ifelse(NSaves - Hits > 0 & CritDamage >= NormalDamage, floor((NSaves - Hits)), 0)
      # HitO <-
      #   ifelse((Hits) - (NSaves + C2H) >= 0, (Hits) - (NSaves + C2H), 0)
      NDamage <- ifelse(HitO * NormalDamage > 0, HitO * NormalDamage, 0)
      CDamage <- ifelse(CritO * CritDamage > 0, CritO * CritDamage, 0)
      Damage <- ifelse(NDamage + CDamage < 0, 0, NDamage + CDamage) + Mortals
      
      #FNP
      FNPRolls<- sample(1:6,Damage, replace = T)
      Damage <- ifelse(FNP > 0, Damage - length(FNPRolls[FNPRolls>=FNP]),Damage)
      DamageOutput <-
        as.data.frame(Damage)
      # if (Damage > 20) {
      #   print(Rolls)
      # }
      # if (Damage > 20) {
      #   print(Saves)
      # }
      return(DamageOutput)
    }))
    
    
    #m <- 3
    OutputProb <-
      do.call(rbind, lapply(unique(c(
        0, unique(Output$Damage)
      )), function(m) {
        TotalDamage <- Output %>% filter(Damage == m) %>% nrow()
        DamageNumber <- Output %>% filter(Damage >= m) %>% nrow() / k
        DamageNumber <-
          as.data.frame(DamageNumber) %>% mutate(Number = m,
                                                 Events = TotalDamage,
                                                 Number2 = m)
        return(DamageNumber)
      }))
    Number <- c(min(OutputProb$Number):max(OutputProb$Number))
    NumberVector <- as.data.frame(Number)
    OutputProb <-
      left_join(NumberVector, OutputProb) %>% fill(DamageNumber, .direction = 'up') %>% fill(Events, .direction = 'up')
    return(OutputProb)
  }

Gun2Chart <- function(W3, Title){
  
  colors <- c("blue", "red")
  names(colors) <- c(A,B)
  
  
  W3 <- full_join(W1, W2, by = "Number") %>% filter(Number > 0) %>% arrange(Number) %>%
    fill(DamageNumber.x, .direction = 'up') %>%
    fill(DamageNumber.y, .direction = 'up')
  colnames(W3)[c(2,5)] <- c('W1',"W2")
ComparisonChart <- ggplot(W3) +
  geom_area(
    aes(x = Number, y = W1, fill = Name),
    stat = "identity",
    color = 'blue',
    fill = 'light blue'
  ) +
  geom_line(
    aes(x = min(Number), y = min(W1), color = A)
  ) +
  geom_area(
    aes(x = Number, y = W2, fill = Name),
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
  scale_x_continuous(breaks = round(seq(min(W3$Number), max(W3$Number), by = 1), 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, max(c(W3$W1, W3$W2)))) +
  theme_bw() +
  xlab('Wounds') +
  ylab('Chance of Doing of at Least X Wounds') +
  labs(color = "Legend", title = C) +
  scale_color_manual(values = colors)
return(ComparisonChart)
}
k <- 1000
SeedH <- sample(1:k, k, replace = TRUE)
SeedS <- sample(1:k, k, replace = TRUE)

# Attacks,
# BS,
# NormalDamage,
# CritDamage,
# Save,
# Defense,
# k,
# Name


A <- 'Bolter AP1'
B <- 'Ceaseless Bolter'
C <- '5+'

W1 <- RangedSim(Attacks = 4,
                BS = 3,
                NormalDamage = 3,
                CritDamage = 4,
                Save = 5,
                Defense = 3,
                k = k,
                Name = A,
                Piercing = 0,
                AP = 1,
                Cover = 0,
                Rerolls = '',
                Rending = 0,
                Starfire = 0,
                MW = 0,
                Lethal = 0,
                FNP = 0
) %>% filter(Number2 > 0)

W2 <- RangedSim(Attacks = 4,
                BS = 3,
                NormalDamage = 3,
                CritDamage = 4,
                Save = 5,
                Defense = 3,
                k = k,
                Name = B,
                Piercing = 0,
                AP = 0,
                Cover = 0,
                Rerolls = 'C',
                Rending = 0,
                Starfire = 0,
                MW = 0,
                Lethal = 0,
                FNP = 0
) %>% filter(Number2 > 0)


Gun2Chart(W3, C)
