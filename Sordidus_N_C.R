#C.sordidus

CS <- read.csv(file = "Sordidus.csv", stringsAsFactors = TRUE)
Location <- as.factor(CS$Location)
CS$Size <- as.factor (CS$Size)
CS_means <- CS %>%
  group_by(Location) %>% 
  summarise(Nm = mean(N), 
            Nsd = sd(N), 
            Cm = mean(C), 
            Csd = sd(C))


CSplot <- ggplot(data = CS_means, ylim = c(6, 14), xlim = c(-19, -17))+ 
  geom_errorbar(data = CS_means, 
                aes(x = Cm, 
                    ymin = Nm - Nsd, ymax = Nm + Nsd, colour = Location), width = 0.2, cex = 1)+ 
  geom_errorbarh(data = CS_means, 
                 aes(y = Nm, 
                     xmin = Cm - Csd, xmax = Cm + Csd, colour = Location), cex = 1)+ 
  scale_x_continuous(limits = c(-16, -8),breaks=seq(-16, -8, 2))+ 
  scale_y_continuous(limits = c(5,12),breaks=seq(5, 12, 2))+
  scale_color_manual(values =c("Chagos " = "#FF6F00B2", "SR" = "#C71000B2"), 
                     labels = c("Chagos", "Scott Reefs"))+ 
  xlab(expression(atop(bold(~delta^13~"C " ("\u2030 " [vs]~"VPDB")))))+ 
  ylab(expression(atop(bold(~delta^15~"N " ("\u2030 " [vs]~"air")))))



fig4 <- CSplot + theme(text = element_text(size = 20))

fig4
