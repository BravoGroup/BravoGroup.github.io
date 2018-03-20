library(tidyverse, quietly = T)
library(plotly, quietly = T)
library(RColorBrewer, quietly = T)
library(htmlwidgets, quietly = T)
library(htmltools, quietly=T)

data <- read_csv("~/Williams Opposition Research/assets/R/data.csv")

x1 <- data %>%
  select(jobs) %>%
  group_by(jobs) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n), 3)*100) %>%
  select(freq) %>%
  rename(jobs_f = freq)

x2 <- data %>%
  select(paenergy) %>%
  group_by(paenergy) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n), 3)*100) %>%
  select(freq) %>%
  rename(paenergy_f = freq)

x3 <- data %>%
  select(indepsec) %>%
  group_by(indepsec) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n), 3)*100) %>%
  select(freq) %>%
  rename(indepsec_f = freq)

x4 <- data %>%
  select(ussteel) %>%
  group_by(ussteel) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n), 3)*100) %>%
  select(freq) %>%
  rename(ussteel_f = freq)

x5 <- data %>%
  select(cultural) %>%
  group_by(cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n), 3)*100) %>%
  select(freq) %>%
  rename(cultural_f = freq)

x6 <- data %>%
  select(envt) %>%
  group_by(envt) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n), 3)*100) %>%
  select(freq)%>%
  rename(envt_f = freq)

x7 <- data %>%
  select(farmland) %>%
  group_by(farmland) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n), 3)*100) %>%
  select(freq) %>%
  rename(farmland_f = freq)

x8 <- data %>%
  select(safety) %>%
  group_by(safety) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n), 3)*100) %>%
  select(freq) %>%
  rename(safety_f = freq)

x9 <- data %>%
  select(renewables) %>%
  group_by(renewables) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n), 3)*100) %>%
  select(freq) %>%
  rename(renewables_f = freq)

x <- c("The project creates jobs for Pennsylvanians",
       "The project uses Pennsylvania natural resources and helps “put PA energy to work",
       "The project enhances American energy independence and security",
       "The project uses steel manufactured in the United States in its construction",
       "The project route avoids Native American and other cultural and archaeological grounds",
       "The project minimizes impact to wildlife and the environment", 
       "The project minimizes impacts to farmland and agriculture",
       "The project employs the highest safety standards for workers and residents",
       "The project helps to complement and support the growth of renewables")

y <- c("Not at all Important", "Slightly Important", "Somewhat Important", "Very Important", "Extremely Important")

d1 <- data.frame(x8, x6, x7, x5, x9, x4, x1, x3, x2)
d1 <- as.data.frame(t(d1))

y <- c("Not at all Important", "Slightly Important", "Somewhat Important", "Very Important", "Extremely Important")
colnames(d1) <- y

z <- c("Employs Highest Safety Standards",
       "Minimizes Envrionmental Impact",
       "Minimizes Agricultural Impact",
       "Avoids Cultural Grounds",
       "Helps Complement & Support Renewables",
       "Uses US Steel",
        "Creates Jobs", 
       "Promotes Energy Independence & Security", 
       "Promotes  PA Energy")

rownames(d1) <- z


p <- plot_ly(data, x = ~d1$`Not at all Important`, y = ~row.names(d1), type = 'bar', orientation = 'h',
             marker = list(color = '#f1eef6',
                           line = list(color = 'rgb(248, 248, 249)', width = 1, name = "Not at all Important"))) %>%
  add_trace(x = ~d1$`Slightly Important`, marker = list(color = '#bdc9e1'), name="Slightly Important") %>%
  add_trace(x = ~d1$`Somewhat Important`, marker = list(color = '#74a9cf'), name="Somewhat Important") %>%
  add_trace(x = ~d1$`Very Important`, marker = list(color = '#2b8cbe'), name="Very Important") %>%
  add_trace(x = ~d1$`Extremely Important`, marker = list(color = '#045a8d'), name="Extremely Important") %>%
  layout(xaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE,
                      ticksuffix = "%",
                      domain = c(0.15, 1)),
         yaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE,
                      categoryorder = "array",
                      categoryarray = rev(z)),
         barmode = 'stack',
         margin = list(l = 120, r = 10, t = 50, b = 80),
         showlegend = FALSE)%>%
  # labeling the y-axis
  add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = ~row.names(d1),
                  xanchor = 'right',
                  text = ~row.names(d1),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE, align = 'right') %>%
  add_annotations(xref = 'x', yref = 'paper',
                  x = c(5, 20, 40, 70, 90),
                  y = 1.05,
                  text = y,
                  font = list(family = 'Arial', size = 12),
                  showarrow = FALSE) %>% 
  config(displayModeBar = FALSE, showLink = FALSE, displaylogo = FALSE)
p
saveWidget(p, file = "graph2.html", selfcontained = F)

###Chart 2
d2 <- data %>%
  select(asr_feel, supp_cultural, supp_archeo, supp_NA, asr_feel2)


d2$asr_feel <- factor(d2$asr_feel,labels=c("Strongly<br>Disagree","Somewhat<br>Disagree","Slightly<br>Disagree", "No<br>Opinion", "Slightly<br>Agree", "Somewhat<br>Agree", "Strongly<br>Agree"))
d2$asr_feel2 <- factor(d2$asr_feel2,labels=c("Strongly<br>Disagree","Somewhat<br>Disagree","Slightly<br>Disagree", "No<br>Opinion", "Slightly<br>Agree", "Somewhat<br>Agree", "Strongly<br>Agree"))
d2$supp_cultural <- gsub("Increases Support", "Increases<br>Support", d2$supp_cultural)
d2$supp_cultural <- gsub("Decreases Support", "Decreases<br>Support", d2$supp_cultural)
d2$supp_cultural <- gsub("No Change in Support", "No Change<br>in Support", d2$supp_cultural)

d2$supp_archeo <- gsub("Increases Support", "Increases<br>Support", d2$supp_archeo)
d2$supp_archeo <- gsub("Decreases Support", "Decreases<br>Support", d2$supp_archeo)
d2$supp_archeo <- gsub("No Change in Support", "No Change<br>in Support", d2$supp_archeo)

d2$supp_NA <- gsub("Increases Support", "Increases<br>Support", d2$supp_NA)
d2$supp_NA <- gsub("Decreases Support", "Decreases<br>Support", d2$supp_NA)
d2$supp_NA <- gsub("No Change in Support", "No Change<br>in Support", d2$supp_NA)

f1 <- d2 %>%
  group_by(asr_feel) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n), 3)*100)

f2 <- d2 %>%
  group_by(asr_feel2) %>%
  summarise(n2 = n()) %>%
  mutate(freq2 = round(n2/sum(n2), 3)*100)

d3 <- cbind.data.frame(f1,f2) %>%
  select(asr_feel, freq, freq2)

q <- plot_ly(d3, x = ~d3$asr_feel, y = ~freq, type = 'bar', marker = list(color = "#6BAED6"), name = 'Before') %>%
  add_trace(y = ~freq2, marker = list(color = '#2171B5'), name = 'After') %>%
  layout(xaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE),
         yaxis = list(title = "", ticksuffix = "%"),
         annotations = list(x = 0.5 , y = 1.05, text = "Support for ASR", showarrow = F, xref='paper', yref='paper'))
q

f3 <- d2 %>%
  group_by(supp_cultural) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n), 3)*100)

t <- plot_ly(f3, x=~supp_cultural, y=~freq, type ='bar', marker = list(color = "#2171B5"), name = 'Cultural')%>%
  layout(xaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      domain = c(0.15, 1)),
         yaxis = list(title = "", ticksuffix = "%"), showlegend = FALSE)

f4 <- d2 %>%
  group_by(supp_archeo) %>%
  summarise(n1 = n()) %>%
  mutate(freq1 = round(n1/sum(n1), 3)*100)

t1 <- plot_ly(f4, x=~supp_archeo, y=~freq1, type ='bar', marker = list(color = '#2171B5'), name = 'Archeological')%>%
  layout(xaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      domain = c(0.15, 1)),
         yaxis = list(title = "", ticksuffix = "%"), showlegend = FALSE)

f5 <- d2 %>%
  group_by(supp_NA) %>%
  summarise(n2 = n()) %>%
  mutate(freq2 = round(n2/sum(n2), 3)*100)

t2 <- plot_ly(f5, x=~supp_NA, y=~freq2, type ='bar', marker = list(color = '#2171B5'), name = 'Burial')%>%
  layout(xaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      domain = c(0.15, 1)),
         yaxis = list(title = "", ticksuffix = "%"), showlegend = FALSE)

v <- subplot(t, t1, t2) %>%
  config(displayModeBar = FALSE, showLink = FALSE, displaylogo = FALSE)

v <- v %>% layout(annotations = list(
  list(x = 0.08 , y = 1.05, text = "Worked with FERC and PHMC", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.5 , y = 1.05, text = "Coordinated with all Stakeholders", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.95 , y = 1.05, text = "Avoided Burial Sites", showarrow = F, xref='paper', yref='paper'))) 

m <- subplot(q, v, nrows = 2, margin = 0.08)

saveWidget(m, file = "graph3.html", selfcontained = F)

####Demographics

#Sex
s <- data %>%
  group_by(sex) %>%
  summarise(n = n()) %>%
  mutate(pct = round(n/sum(n),3)*100) %>%
  plot_ly(labels = ~sex, values = ~n, marker = list(colors = c("#2171B5","#6BAED6"), line = list(color = '#FFFFFF', width = 1)), textposition = 'inside',insidetextfont = list(color = '#FFFFFF')) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "Respondent Sex", showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         font = list(family = 'Arial', size = 12)) %>%
  config(displayModeBar = FALSE, showLink = FALSE, displaylogo = FALSE)

s

a <- data %>%
  group_by(age) %>%
  summarise(n = n()) %>%
  mutate(pct = round(n/sum(n),3)*100) %>%
  plot_ly(x = ~age, y = ~pct, type = 'bar', marker = list(colors = c('rgba(4, 90, 141, .9)'))) %>%
  layout(title = "Respondent Age", showlegend = F,
         xaxis = list(title=''),
         yaxis = list(title='', ticksuffix='%'),
         font = list(family = 'Arial', size = 12)) %>%
  config(displayModeBar = FALSE, showLink = FALSE, displaylogo = FALSE)
  
a

i <- data %>%
  group_by(income) %>%
  summarise(n = n()) %>%
  mutate(pct = round(n/sum(n),3)*100)
i$income <- gsub("\\$", "", i$income)
i$income <- as.factor(i$income)
i <- as.data.frame(i)
i <-  plot_ly(i, x = ~income, y = ~pct, type = 'bar') %>%
  layout(title = "Respondent Income",
         xaxis = list(title='', tickprefix="$", categoryorder="category descending"),
         yaxis = list(title='', ticksuffix='%'),
         font = list(family = 'Arial', size = 12)) %>%
  config(displayModeBar = FALSE, showLink = FALSE, displaylogo = FALSE)
i

r <- data %>%
  group_by(race) %>%
  summarise(n = n()) %>%
  mutate(pct = round(n/sum(n),3)*100)
r[grep("^Other:",r$race),]$race <- "Other"
r <- plot_ly(r, labels = ~race, values = ~n, textposition = 'inside',insidetextfont = list(color = '#FFFFFF'), marker = list(colors = rev(brewer.pal(7, "Blues"))[c(5,4,2,3,6,7)], line = list(color = '#FFFFFF', width = 1))) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "Respondent Race", showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         font = list(family = 'Arial', size = 12)) %>%
  config(displayModeBar = FALSE, showLink = FALSE, displaylogo = FALSE)
r

pv <- data %>%
  group_by(politicalviews) %>%
  summarise(n = n()) %>%
  mutate(pct = round(n/sum(n),3)*100)
pv$politicalviews <- stringr::str_replace_all(pv$politicalviews," ", "<br>")
pv$politicalviews <- as.factor(pv$politicalviews)
pv <- plot_ly(pv, x = ~politicalviews, y = ~pct, type = 'bar', marker = list(colors = c('rgba(4, 90, 141, .9)'))) %>%
  layout(title = "Respondent Political Views", showlegend = F,
         xaxis = list(title='',
                      categoryorder='array',
                      categoryarray=c('Very<br>liberal', 'Somewhat<br>liberal', 'Moderate',
                                      'Somewhat<br>conservative', 'Very<br>conservative')),
         yaxis = list(title='', ticksuffix='%'),
         font = list(family = 'Arial', size = 12)) %>%
  config(displayModeBar = FALSE, showLink = FALSE, displaylogo = FALSE)

pv

u <- browsable(
  tagList(list(
    tags$div(
      style = 'width:50%;display:block;float:left;',
      a
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      s
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      r
    ),
    tags$div(
      style = 'width:50%;display:block;float:left;',
      pv
    )
  ))
)

u

save_html(u, file = "subplots.html")

subplot(s, a, r, v, nrows = 2) %>%
  layout(title = "Respondent Demographics",
         xaxis = list(domain=list(x=c(0,0.5),y=c(0,0.5))),
         xaxis2 = list(domain=list(x=c(0.5,1),y=c(0,0.5))),
         xaxis3 = list(domain=list(x=c(0.5,1),y=c(0.5,1))),
         showlegend=FALSE,showlegend2=FALSE)

RC
