if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(ggplot2)){install.packages("ggplot2")}

OrchardSprays

model = lm(decrease ~ rowpos + treatment + rowpos:treatment,
           data = OrchardSprays)
library(multcompView)

library(lsmeans)

leastsquare = lsmeans(model,
                      pairwise ~ rowpos + treatment,
                      adjust = "tukey")


OrchardSprays = OrchardSprays(leastsquare,
          alpha   = 0.05,
          Letters = letters,         ###  Use lowercase letters for .group
          adjust  = "tukey")         ###  Tukey-adjusted comparisons


### Order the levels for printing

OrchardSprays$rowpos = factor(OrchardSprays$rowpos)

OrchardSprays$treatment = factor(OrchardSprays$treatment)




### Plot

library(ggplot2)

pd = position_dodge(0.6)    ### How much to jitter the points on the plot

ggplot(OrchardSprays,
       aes(x     = rowpos,
           y     = leastsquare$lsmean,
           color = treatment,
           label = .group)) +
  
  geom_point(shape  = 15,
             size   = 4,
             position = pd) +
  
  geom_errorbar(aes(ymin  =  lower.CL,
                    ymax  =  upper.CL),
                width =  0.2,
                size  =  0.7,
                position = pd) +
  
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  
  ylab("Least square mean\nsodium intake (mg / day)") +
  
  geom_text(nudge_x = c(-0.22, 0.08, -0.22, 0.22,
                        0.08, 0.08, -0.08, -0.22,
                        0.22, 0.22, -0.08, -0.08),
            nudge_y = rep(270, 12),
            color   = "black") +
    scale_color_manual(values = c("steelblue", "steelblue2",
                                "springgreen4", "springgreen3"))


