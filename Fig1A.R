## Making of FIG 1A for Mitchell, Ahmad Khan et al. RR manuscript
# presenting the 4 lines

library(ggplot2)

dat <- data.frame(LINE = c('A', 'B', 'C', 'D', 'A', 'B', 'C', 'D'), 
                  LOC = c(-40,-40,-80,-80, 40, 80, 40, 80))

dat$SIDE <- cut(dat$LOC, 2, c('left','right'))

dat$LINE <- factor(dat$LINE, level = c('D','C','B','A'))
dat$SIDE <- factor(dat$SIDE)
dat$LOC <- factor(dat$LOC)

ggplot(dat, aes(LOC, LINE)) +
  geom_point(size = 0) +
  geom_line(aes(group = LINE), size = 1) +
  scale_x_discrete(name = 'Location of left and right endpoints (mm)',
                   limits = c('-80','-40','0','40','80')) +
  scale_y_discrete(name = '') +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

sPath <- '/Users/alex/Dropbox/Publications/PIPTOT_RR/figures/'
setwd(sPath)
ggsave('Fig1A.png', plot = last_plot(), scale = 1, 
       width = 4, height = 3, dpi = 300)
