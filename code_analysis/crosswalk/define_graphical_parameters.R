#### DEFINE GRAPHICAL PARAMETERS FOR FIGURES ####

#set sizes and widths
axeswidth <- 0.5
genplotwidth <- 1
size_indivline <- 0.2
size_indivdot <- 1.5
size_sumdot <- 4
size_line_lineplot <- 0.5
stroke_indivdot <- 0.5
stroke_sumdot <- 0.5
sizefont <- 12
sizeannot <- 4
smoothvalue <- 1/2
resxplot <- 1

#define seed for dot jitter
jitter_seed <- 12345

#create custom theme
theme_set (theme_classic(14) + theme(axis.line.x = element_line(colour = 'black', linewidth=axeswidth, linetype='solid'),
                                     axis.line.y = element_line(colour = 'black', linewidth=axeswidth, linetype='solid'),
                                     axis.ticks = element_line(color="black", linewidth=axeswidth),
                                     axis.text.x = element_text(color="black"),
                                     axis.text.y = element_text(color="black"),
                                     text = element_text(family = "Helvetica")))

#create color palettes (old)
hue_border_ttc <- "#006d2c"
pal_ttc_fill <- scale_fill_manual(name = 'TTC',values =  c("#edf8e9","#bae4b3","#74c476","#31a354","#006d2c"))

hue_border_var <- "#54278f"
pal_var_col <- scale_color_manual(name = 'Variability',values =  c("#9e9ac8","#6a51a3","#3f007d"))
pal_var_fill <- scale_fill_manual(name = 'Variability',values =  c("#9e9ac8","#6a51a3","#3f007d"))
pal_varsd_fill <- scale_fill_manual(name = 'Variability',values =  c("#dadaeb","#bcbddc","#9e9ac8","#807dba","#54278f"))

hue_border_session <- "#d94801"
pal_session_fill <- scale_fill_manual(name = 'Session',values =  c("#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801","#a63603"))

#create sequential color palettes using predefined function
hue_border_conf <- "#984ea3" #"#8c6bb1" #"#3f007d"
hue_border_risk <- "#f03869" #"#4daf4a"  #"#41ab5d" #"#17693f"
hue_border_cross <- "#ff7f00" #"" #"#d94801"

pal_col_conds <- scale_color_manual(name = "Condition", values = c(hue_border_conf,hue_border_risk,hue_border_cross))
pal_fill_conds <- scale_fill_manual(name = "Condition", values = c(hue_border_conf,hue_border_risk,hue_border_cross))

#set scale
scalex_trials <-  scale_x_continuous(breaks = c(5, 10, 15, 20, 25), labels = c(5, 10, 15, 20, 25))
scale_x_fullprob <- scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), limits = c(-0.01,1.01))
scale_y_fullprob <- scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), limits = c(-0.01,1.01))
scale_y_thres <- scale_y_continuous(breaks = c(3,4,5,6,7,8), labels = c(3,4,5,6,7,8), limits = c(2.8,8.5))

#define lab names
string_propconf <- "Prop. 'more confident'"
string_proprisk <- "Prop. 'less risky'"
string_propcross <- "Prop. attempted crossings"
string_propsucccross <- "Prop. successful crossings"
string_sesssion <- "Session number"

string_propconf_logodds <-paste (string_propconf,"(log odds)", sep = " ")
string_proprisk_logodds <-paste (string_proprisk,"(log odds)", sep = " ")
string_propcross_logodds <-paste (string_propcross,"(log odds)", sep = " ")
string_propsucccross_logodds <-paste (string_propsucccross,"(log odds)", sep = " ")

xlab_ttc <- xlab("TTC")
xlab_carsd <- xlab("Car velocity variability")
xlab_carsdnum <- xlab("Car velocity SD")
xlab_ttc_prevtri <- xlab("TTC, previous trial")
xlab_carsd_prevtri <- xlab("Car velocity variability, previous trial")
xlab_condfirst_conf <- xlab("Confidence condition first?")
xlab_condfirst_risk <- xlab("Risk condition first?")
xlab_condfirst_cross <- xlab("Cross condition first?")

xlab_propconf <- xlab(string_propconf)
xlab_proprisk <- xlab(string_proprisk)
xlab_propcross <- xlab(string_propcross)
xlab_propsucccross <- xlab(string_propsucccross)
xlab_session <- xlab(string_sesssion)

ylab_propconf <- ylab(string_propconf)
ylab_proprisk <- ylab(string_proprisk)
ylab_propcross <- ylab(string_propcross)
ylab_propsucccross <- ylab(string_propsucccross)
ylab_session <- ylab(string_sesssion)

xlab_propconf_logodds <- xlab(string_propconf_logodds)
xlab_proprisk_logodds <- xlab(string_proprisk_logodds)
xlab_propcross_logodds <- xlab(string_propcross_logodds)
xlab_propsucccross_logodds <- xlab(string_propsucccross_logodds)

ylab_propconf_logodds <- ylab(string_propconf_logodds)
ylab_proprisk_logodds <- ylab(string_proprisk_logodds)
ylab_propcross_logodds <- ylab(string_propcross_logodds)
ylab_propsucccross_logodds <- ylab(string_propsucccross_logodds)

ylab_corpair <- ylab(expression(paste("Pearson's ", italic("r"), " correlation between \n pairs of response proportions")))
ylab_corpair <- ylab("r, correlation between \n pairs of response proportions")

ylab_thresconf <- ylab("Threshold: TTC 50% 'more confident' (s)")
ylab_thresrisk <- ylab("Threshold: TTC 50% 'less risky' (s)")
ylab_threscross <- ylab("Threshold: TTC 50% attempted crossings (s)")

ylab_rt <- ylab("Sidewalk leaving time (s)")

#define scales
scalex_3sds <- scale_x_discrete(labels=c("no","medium","high"))
scalex_3sds_num <- scale_x_continuous(breaks = c(1,2,3), labels = c("no","medium","high"))
scalex_yesno <- scale_x_discrete(breaks = c(0,1), , labels = c("No","Yes"))
scale_y_vels <- scale_y_continuous(breaks = c(1,1.5,2), labels = c(1,1.5,2), limits = c(0.9,2))

scale_fill_confjudg <- scale_fill_manual(labels = c("'less confident'", "'more confident'"), values =  c("0" = "#bcbddc", "1" = "#807dba"))
scale_fill_crossatt <- scale_fill_manual(labels = c("crossing not attempted", "crossing attempted"), values =  c("0" = "#FFD4AA", "1" = "#FF942A"))
scale_fill_riskjudg <- scale_fill_manual(labels = c("riskier", "less risky"), values =  c("0" = "#FABCCD", "1" = "#F25982"))


#new version Joan
hue_border_varno <- "#ef426f"
hue_border_varmedium <- "#00b2a9"
hue_border_varhigh <- "#ff8200"

pal_col_var <- scale_color_manual(name = "Car velocity variability", values=c(hue_border_varno,hue_border_varmedium,hue_border_varhigh))
pal_fill_var <- scale_fill_manual(name = "Car velocity variability", values=c(hue_border_varno,hue_border_varmedium,hue_border_varhigh))