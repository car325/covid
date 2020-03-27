#####covid code
#clearing environment
    rm(list=ls())
    gc()

#function for downloading and/or applying packages easily
    ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
            install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
    }
    
#names of packages
    packages <- c("readr",
                  "tidyverse",
                  "magrittr",
                  "ggthemes",
                  "ggpubr")
    
#load the required packages 
    ipak(packages)

#download covid csv file from covidtracking.com
    states_daily <- read_csv("http://covidtracking.com/api/states/daily.csv")

#download population csv file from census.gov
    state_pop <- read_csv("http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv?#")

#get 2019 state population estimates
    state_pop %<>%
        mutate(pop2019 = POPESTIMATE2019/1000000,
               state = state.abb[match(NAME,state.name)]) %>%
        filter(!is.na(state)) %>%
        select(state, pop2019)

#add population data
    states_daily <- left_join(states_daily, state_pop, by = "state")

#create variables for prop tests positive & test per capita
    states_daily %<>%
        mutate(prop_pos = positive/total,
               test_capita = total/pop2019,
               date = as.Date(as.POSIXct(dateChecked))) %>%
        arrange(state, desc(date))

#latest tests per million people
    per_capita <- states_daily %>% 
        filter(date == max(date),
               !is.na(test_capita)) %>%
        select(state, test_capita) %>%
        arrange(desc(test_capita))
    
#choose top 5 testing states
    x <- per_capita[1:5, 1]
    gg_states_daily <- states_daily %>%
        filter(state %in% x$state,
               prop_pos < 1) %>%
        arrange(state, desc(date)) %>%
        mutate(state = state.name[match(state,state.abb)])
    
#graph prop positive by top 5 test per million   
    graph <- ggplot(gg_states_daily, aes(x = date, y = prop_pos, color = state)) + 
        geom_line(size = 1.5, alpha = 0.6) +
        theme_bw() +
        scale_colour_discrete(name = "State") +
        theme(text = element_text(size = 12, family = "Garamond")) +
        theme(axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_line(colour = "#d3d3d3"),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        labs(x = "", y = "") +
        ggtitle("Proportion of Positive COVID-19 Tests") +
        scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                                   to = max(x), 
                                                   by = "4 days"),
                     minor_breaks = function(x) seq.Date(from = min(x), 
                                                         to = max(x), 
                                                         by = "2 days"),
                     date_labels = "%m/%d")

#annotate y axis   
    graph <- annotate_figure(graph, left = text_grob("Proportion of Positive Tests",
                     family = "Garamond",
                     size = 12,
                     rot = 90,
                     vjust = 1))
 
#graph testing per million       
    graph1 <- ggplot(per_capita, aes(x = reorder(state, -test_capita), y = test_capita, fill = reorder(state, -test_capita))) + 
        geom_col(size = 1.5, alpha = 0.6) +
        theme_bw() +
        theme(legend.position = "none") +
        theme(text = element_text(size = 12, family = "Garamond")) +
        theme(axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_line(colour = "#d3d3d3"),
              panel.border = element_blank(),
              panel.background = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        labs(x = "", y = "") +
        ggtitle("COVID-19 Tests per Million People by State")

#annotate y axis
    graph1 <- annotate_figure(graph1, left = text_grob("Tests per Million People",
                                                     family = "Garamond",
                                                     size = 12,
                                                     rot = 90,
                                                     vjust = 1))

#create notes section    
    text <- paste("             By Cody Arlie Reed (car325@cornell.edu)", Sys.Date(), sep = " ")
    text1 <- paste("             Data from COVID Tracking Project covidtracking.com", sep = " ")
    text.p <- ggparagraph(text = text,
                          face = "italic",
                          size = 12,
                          color = "black",
                          family = "Garamond")
    text.p1 <- ggparagraph(text = text1,
                           face = "italic",
                           size = 12,
                           color = "black",
                           family = "Garamond")
 
#organize graphs       
    ggarrange(graph1, graph, text.p, text.p1,
              ncol = 1,
              nrow = 4,
              heights = c(5.5, 5.5, .3, .3))
    