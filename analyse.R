require(ggplot2)
require(mgcv)
require(scales)
require(Cairo)


#set default options in case we use ggplot later
ggtheme <- #theme_bw() +
  theme(
    axis.text.x = element_text(colour='gray50'),
    axis.text.y = element_text(colour='gray50'),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(colour='gray50', fill=NA),
    strip.background = element_blank()
  )

# Load data
venues <- read.csv("2013-07-13 VIC EGMS.csv")

# Subset out valid only
venues.2 <- subset(venues, egms > 0 & !is.na(LossJulDec12) & LossJulDec12 > 0)

# Calculate expenditure per machine (annualise half-year figures)
venues.2$Exp.per.machine <- with(venues.2, LossJulDec12 / egms) * 2
summary(venues.2)

# Do we want to fit a spline or just a straight line?
ggplot(aes(x=egms,y=Exp.per.machine), data=venues.2) +
  geom_point(color='#FF8000',alpha=.75) +
  geom_smooth(se=F, method='gam', formula=y~s(x), color='grey50') +
  geom_smooth(se=F, method='lm', formula=y~x, color='#2957FF') +
  scale_y_continuous(name="Annual player loss per machine", labels=dollar) +
  scale_x_continuous(name="Number of pokies in venue") +
#  facet_wrap(region~type, scales='free_x') +
  ggtheme

# A straight line will do
p <- ggplot(aes(x=egms,y=Exp.per.machine), data=venues.2) +
  geom_point(color='#FF8000',alpha=.75) +
  geom_smooth(se=F, method='lm', formula=y~x, color='#2957FF') +
  scale_y_continuous(name="Annual player loss per machine", labels=dollar) +
  scale_x_continuous(name="Number of pokies in venue") +
  ggtheme

# Save figure
ggsave("VIC_expenditure_per_machine_by_venue_size_small.png", p, type="cairo-png", 
       width=20, height=12.368, units="cm", dpi=90)

ggsave("VIC_expenditure_per_machine_by_venue_size_large.png", p, type="cairo-png", 
       width=20, height=12.368, units="cm", dpi=300)


