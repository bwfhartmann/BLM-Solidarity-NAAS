library(tidyverse)
library(ggpubr)
setwd("/Users/hartmabs/Documents/Masters/New")
data <- readxl::read_excel("huge_matrix2.xlsx")
data$variable = as.factor(data$variable)
data$model = ifelse(data$model=="0", "Unfavorable",ifelse(data$model=="1","No Opinion","Favorable"))
data$group = ifelse(data$group=="AAPI", "Asian American", "Latino/Hispanics")
plot1 = data %>%
  filter(level != "Diff" & level != "Df" ) %>%
  select(group, level, variable, model, lincom, se, pvalue,sig) %>%
  filter(model == "Favorable" & !is.na(group)) 

ggplot(plot1, aes(lincom, variable)) +
  geom_line(aes(group = variable,size = as.factor(sig))) +
  geom_point(aes(shape = level, alpha = level, color = level) , size = 6, alpha = .5) +
  facet_grid(.~group) + 
  geom_vline(xintercept = .5, linetype="dotted", 
             color = "black", size=.5) +
  scale_size_discrete(name = '', labels = c('p>0.1','p<0.1','p<0.05'),range=c(.2, .8,2.5))  +
  theme_pubr() +
  scale_color_discrete(name = 'Level',labels=c("Minimum=0", "Maximum=1")) +
  scale_x_continuous(name="Pr(BLM Favorablity)", limits=c(0.4, 0.75)) + 
  scale_y_discrete(name = "",labels=c('race_important' = "PID", 'group_con' = "GC", 'ethnic_important' = "EID", 'cat_effectall_ethnic'="ELF", 'cat_effectall_a'="PLF")) +
  theme(axis.text.x = element_text(size=9)) + 
  scale_shape_manual(name = "Level",values=c(20,18),labels=c("Minimum=0", "Maximum=1")) +
  ggtitle("Figure 1: Comparing Marginal Effects on BLM Favorability by Panethnic Group")+
    theme(plot.title = element_text(hjust=0, size=11)) +
  #labs(caption = "Source: NAAS 2016 \n PID = Panethnic identity salience, EID = Ethnic identity salience, GC = Group conciousness, ELF = Ethnic linked fate, and PLF = panethnic linked fate \n Adjusting for gender, age, education, income, party identification, region, forgein born, contact with African Americans, and experience with discrimination") +
  theme(plot.caption = element_text(hjust = 0), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") 
ggsave("ppr_figure1.png", width =7, height = 4)

annotate_figure(p1, bottom = text_grob(expression(paste("Source: NAAS 2016 \n PID = Panethnic identity salience, EID = Ethnic identity salience, GC = Group conciousness, ELF = Ethnic linked fate, and PLF = panethnic linked fate \n Adjusting for gender, age, education, income, party identification, region, forgein born, contact with African Americans, and experience with discrimination",
                                                        hjust = 4, x = -4, size = 4))))
plot2 = data %>%
  filter(level != "Diff" & level != "Df" ) %>%
  select(group, level, variable, model, lincom, se, pvalue,sig) %>%
  filter(model != "Favorable" & !is.na(group)) 

ggplot(plot2, aes(lincom, variable, group = model)) +
  scale_shape_manual(name = 'Level', labels=c("Minimum=0", "Maximum=1"),values=c(20,18)) +
  scale_color_discrete(name = 'Level',labels=c("Minimum=0", "Maximum=1")) +
  geom_line(aes(group = c(variable),size = as.factor(sig))) +
  geom_point(aes(shape = level, color = level), size = 6, alpha = .5) +
  facet_grid(group~model) + 
  scale_size_discrete(range=c(.2, .8,2.5)) +
  theme_pubr() +
  theme(panel.border =element_rect(colour = "gray", fill=NA, size=.7)) +
  scale_size_discrete(name = '', labels = c('p>0.1','p<0.1','p<0.05'),range=c(.2, .8,2.5))  +
  scale_x_continuous(name="Pr(BLM Level)", limits=c(0.0, 0.5, by = .25)) + 
  scale_y_discrete(labels=c('race_important' = "PID", 'group_con' = "GC", 'ethnic_important' = "EID", 'cat_effectall_ethnic'="ELF", 'cat_effectall_a'="PLF")) +
  theme(axis.text.x = element_text(size=9)) +
 # labs(caption = "Source: NAAS 2016 \n PID = Panethnic identity salience, EID = Ethnic identity salience, GC = Group conciousness, ELF = Ethnic linked fate, and PLF = panethnic linked fate \n Adjusting for gender, age, education, income, party identification, region, forgein born, contact with African Americans, and experience with discrimination") +
  theme(plot.caption = element_text(hjust = 0), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") +
  ylab("") +
  ggtitle('Figure 3: Comparing Marginal Effects by Panethnic Group on Other BLM Opinions')
ggsave("other_figure2.png", width = 7, height = 7)

plot3 = data %>%
  filter( level == "Diff" ) %>%
  select(group, level, variable, model, lincom, se, pvalue,sig3,sig) %>%
  filter(model == "Favorable") 
plot3$sig_t = as.factor(plot3$sig)
plot3$sig_t2 = ifelse(plot3$sig_t == "0", "p>0.1", "p<0.1")

ggplot(plot3, aes(lincom, variable)) +
  geom_text(aes(label=ifelse(round(lincom,2)==.15,expression(Delta),'')),hjust=5.1,vjust=-0.4, size = 3) 

ggplot(plot3, aes(lincom, variable)) +
  geom_line(aes(group = variable, alpha =as.factor(sig3))) +
  geom_point(aes(shape = group, alpha = as.factor(sig_t), color = group), size = 6) +
  geom_vline(xintercept = 0, linetype="dotted",  color = "black", size=.5) +
  scale_alpha_manual(name = '', labels = c('p>0.1','p<0.1','p<0.05'),values=c(.15, .5,.8))  +
  scale_size_manual(name = '', labels = c('p>0.1','p<0.1'),values=c(2, 4.5))  +
  theme_pubr() +
  scale_shape_discrete(name = 'Panethnic Group', labels =c('AA',"HL")) +
  scale_color_discrete(name = 'Panethnic Group', labels =c('AA',"HL")) + 
  scale_x_continuous(name="DCR of BLM Favorability", limits=c(-0.15, 0.3)) + 
  scale_y_discrete(name = '',labels=c('race_important' = "PID", 'group_con' = "GC", 'ethnic_important' = "EID", 'cat_effectall_ethnic'="ELF", 'cat_effectall_a'="PLF")) +
  theme(axis.text.x = element_text(size=9))  +
  labs(subtitle = "Figure 2: Difference in Discrete Change of BLM Favorability by Panethnic Groups") +
  theme(plot.subtitle = element_text(hjust=0, size=12)) +
  theme(legend.position="right", legend.box = "vertical") +
  theme(plot.caption = element_text(hjust = 0), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") 
  #labs(caption = "Source: NAAS 2016 \n PID = Panethnic identity salience, EID = Ethnic identity salience, GC = Group conciousness, ELF = Ethnic linked fate, and PLF = panethnic linked fate \n Adjusting for gender, age, education, income, party identification, region, forgein born, contact with African Americans, and experience with discrimination") 

ggsave("adc_figure2.png", width = 7, height = 4)
annotate_figure(p3, bottom = text_grob(expression(paste("Source: NAAS 2016 \n PID = Panethnic identity salience, EID = Ethnic identity salience, GC = Group conciousness, ELF = Ethnic linked fate, and PLF = panethnic linked fate \n Adjusting for gender, age, education, income, party identification, region, forgein born, contact with African Americans, and experience with discrimination",
                                                        hjust = 4, x = -4, size = .4))))

plot3 = data %>%
  filter( level == "Diff" ) %>%
  select(group, level, variable, model, lincom, se, pvalue,sig3,sig) %>%
  filter(model != "Favorable") 
plot3$sig_t = as.factor(plot3$sig)
plot3$sig_t2 = ifelse(plot3$sig_t == "0", "p>0.1", "p<0.1")

ggplot(plot3, aes(lincom, variable)) +
  geom_line(aes(group = variable, alpha =as.factor(sig3))) +
  geom_point(aes(shape = group, alpha = as.factor(sig_t), color = group), size = 6) +
  geom_vline(xintercept = 0, linetype="dotted",  color = "black", size=.5) +
  scale_alpha_manual(name = '', labels = c('p>0.1','p<0.1','p<0.05'),values=c(.15, .5,.8))  +
  scale_size_manual(name = '', labels = c('p>0.1','p<0.1'),values=c(2, 4.5))  +
  theme_pubr() +
  facet_grid(.~model) + 
  scale_shape_discrete(name = 'Panethnic Group', labels =c('AA',"HL")) +
  scale_color_discrete(name = 'Panethnic Group', labels =c('AA',"HL")) + 
  scale_x_continuous(name="DCR of BLM Favorability", limits=c(-0.24, 0.2)) + 
  scale_y_discrete(name = '',labels=c('race_important' = "PID", 'group_con' = "GC", 'ethnic_important' = "EID", 'cat_effectall_ethnic'="ELF", 'cat_effectall_a'="PLF")) +
  theme(axis.text.x = element_text(size=9))  +
  labs(subtitle = "Figure 4: Difference in Discrete Change of BLM Favorability by Panethnic Groups") +
  theme(plot.subtitle = element_text(hjust=0, size=12)) +
  theme(legend.position="right", legend.box = "vertical") +
  theme(plot.caption = element_text(hjust = 0), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") 
#labs(caption = "Source: NAAS 2016 \n PID = Panethnic identity salience, EID = Ethnic identity salience, GC = Group conciousness, ELF = Ethnic linked fate, and PLF = panethnic linked fate \n Adjusting for gender, age, education, income, party identification, region, forgein born, contact with African Americans, and experience with discrimination") 
ggsave("adc_figure3.png", width = 7, height = 4)


setwd("/Users/hartmabs/Documents/Masters/New")
data <- readxl::read_excel("huge_matrix.xlsx")
data$variable = as.factor(data$variable)
data$model = ifelse(data$model=="0", "Unfavorable",ifelse(data$model=="1","No Opinion","Favorable"))
data$group = ifelse(data$group=="AAPI", "Asian American", "Latino/Hispanics")

plot1 = data %>%
  filter(level != "Diff" & level != "Df" ) %>%
  select(group, level, variable, model, lincom, se, pvalue,sig1) %>%
  filter(model != "Favorable" & !is.na(group)) 

ggplot(plot1, aes(lincom, variable)) +
  geom_line(aes(group = variable,size = as.factor(sig1))) +
  geom_point(aes(shape = level, alpha = level) , size = 4.5, alpha = .5, fill = "black") +
  facet_grid(.~group+model) + 
  geom_vline(xintercept = .5, linetype="dotted", 
             color = "black", size=.5) +
  scale_size_discrete(name = '', labels = c('p>0.1','p<0.1','p<0.05'),range=c(.2, .8,2.5))  +
  theme_pubr() +
  scale_color_discrete(name = 'Level',labels=c("Minimum=0", "Maximum=1")) +
#  scale_x_continuous(name="Pr(BLM Favorablity)", limits=c(0.4, 0.75)) + 
  scale_y_discrete(name = "",labels=c('race_important' = "PID", 'group_con' = "GC", 'ethnic_important' = "EID", 'cat_effectall_ethnic'="ELF", 'cat_effectall_a'="PLF")) +
  theme(axis.text.x = element_text(size=9)) + 
  scale_shape_manual(name = "Level",values=c(20,18),labels=c("Minimum=0", "Maximum=1")) +
  labs(title = str_wrap("Figure 1: Comparing Marginal Effects of Linked Fate, Group Conciousness, and Identity Salience by Panethnic Group", 150))+
  theme(plot.title = element_text(hjust=0, size=11)) 

plot1 = data %>%
  filter(level != "Diff" & level != "Df" ) %>%
  select(group, level, variable, model, lincom, se, pvalue,sig1) %>%
  filter(model == "Favorable" & !is.na(group)) 

ggplot(plot1, aes(lincom, variable)) +
  geom_line(aes(group = variable,size = as.factor(sig1))) +
  geom_point(aes(shape = level, alpha = level) , size = 4.5, alpha = .5, fill = "black") +
  facet_grid(.~group) + 
  geom_vline(xintercept = .5, linetype="dotted", 
             color = "black", size=.5) +
  scale_size_discrete(name = '', labels = c('p>0.1','p<0.1','p<0.05'),range=c(.2, .8,2.5))  +
  theme_pubr() +
  scale_color_discrete(name = 'Level',labels=c("Minimum=0", "Maximum=1")) +
  scale_x_continuous(name="Pr(BLM Favorablity)", limits=c(0.4, 0.75)) + 
  scale_y_discrete(name = "",labels=c('race_important' = "PID", 'group_con' = "GC", 'ethnic_important' = "EID", 'cat_effectall_ethnic'="ELF", 'cat_effectall_a'="PLF")) +
  theme(axis.text.x = element_text(size=9)) + 
  scale_shape_manual(name = "Level",values=c(20,18),labels=c("Minimum=0", "Maximum=1")) +
  labs(title = str_wrap("Figure 1: Comparing Marginal Effects of Linked Fate, Group Conciousness, and Identity Salience by Panethnic Group", 150))+
  theme(plot.title = element_text(hjust=0, size=11)) 



