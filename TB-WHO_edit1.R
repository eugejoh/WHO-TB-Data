# VISUALIZING TB DATA
# Datasets based on WHO for all countries

# http://www.who.int/tb/country/data/download/en/
cat("\014") # clears Console (in RStudio)
sessionInfo() # gives session info, ver of R, packages
rm(list=ls()) #removes work space environment

library(ggplot2)

setwd("/Users/eugenejoh/Documents/BU Graduate School/BU SPH/2016 Winter/BS 720/TB") #set working directory
###########################################
# Read-in .csv files from local directory #
###########################################
TB.burden <- read.csv("TB_burden_countries_2016-02-18.csv") #TB burden dataset
TB.not <- read.csv("TB_notifications_2016-02-18.csv") #TB notification dataset
TB.dic <- read.csv("TB_data_dictionary_2016-02-18.csv") #TB documentation dictionary
# TB.dic <- TB.dic[,c(1,4)] # only includes variable name and definition

# SEARCH the TB documentation file
TB.dic[,1] #variable names in directory
TB.dic[,1][4] #first variable name in directory, country

TB.dic[,2][5] #how to only show level, not all 327 levels in output

################### *for later convenience use
# SEARCH FUNCTION #
###################
TB.search <- function (x){ # VERSION CURRENT
	look <- names(x) #assign the names/column names of the subsetted data
	message("Matching variable names found in the documentation files")
	TB.d <- as.character(TB.dic[,4]) # conversion to character to have "clean" output
	out <- look[look %in% TB.dic[,1]] #assignment of vector: which names of the selected subset are in the documentation file
	print(out) #check which variable names from dictionary match the input
	message("Above includes all names for variable names found in the documentation files") #message prompt for output
	z <- readline("Search Variable Definition (CASE-SENSITIVE): ") #interactive read-in of variable name	
	if (z %in% out){ # NEED TO MAKE THE CONDITION TO SEPARATE integer(0) from real integer values
	print(TB.d[grep(paste0("^(",z,"){1}?"),TB.dic[,1])]) #print the definition found in  
	}else{
		repeat{ #repeat the line below for readline() if exact match of doesn't exist
		y<-readline("Please re-input (press 'ESC' to exit search): ") #second prompt
		if (y %in% out) break # the condition that ends the repeat, that the input matches a name in the documentation
		}
	print(TB.d[grep(paste0("^",y,"{1}?"),TB.dic[,1])]) #print the definition found in documentation file TB.dic
	}
}

##################################
# DATA CLEANING AND ORGANIZATION #
##################################

# organizing the data, use of for loop vs. apply because apply() family assumes all columns and rows are of same data type (ie. character or numeric)
for (k in 1:length(names(TB.burden))){ #convert all the rows with type 'integer' to 'numeric
	if (is.integer(TB.burden[,k])){
	TB.burden[,k] <- as.numeric(TB.burden[,k])	
	}
}

##########################
# CHANGING COUNTRY NAMES #
##########################
library(plyr) #convient since you don't have to specify all levels 
# http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
str(TB.burden$country) #previous to changing names
TB.burden$country <- (mapvalues((TB.burden$country), from = c(
	"Bolivia (Plurinational State of)",
	"Bonaire, Saint Eustatius and Saba",
	"China, Hong Kong SAR",
	"China, Macao SAR",
	"Democratic People's Republic of Korea",
	"Democratic Republic of the Congo",
	"Iran (Islamic Republic of)",
	"Lao People's Democratic Republic",
	"Micronesia (Federated States of)",
	"Republic of Korea",
	"Saint Vincent and the Grenadines",
	"Sint Maarten (Dutch part)",
	"The Former Yugoslav Republic of Macedonia",
	"United Kingdom of Great Britain and Northern Ireland",
	"United Republic of Tanzania",
	"Venezuela (Bolivarian Republic of)",
	"West Bank and Gaza Strip")
    
    , to = c(
    "Bolivia",
    "Caribbean Netherlands",
    "Hong Kong",
    "Macao",
    "North Korea",
    "DRC",
    "Iran",
    "Laos",
    "Micronesia",
    "South Korea",
    "St. Vincent & Grenadines",
    "Sint Maarten",
    "Former Yugoslav (Macedonia)",
	"UK & Northern Ireland",
    "Tanzania",
    "Venezuela",
    "West Bank and Gaza")
    ))
unique(TB.burden$country)

###############################
# WORKING WITH TB.burden DATA #
###############################
# DESCRIPTIVES
# examples
SK<-subset(TB.burden,TB.burden$country %in% "South Korea") #subset of data in South Korea
dim(SK)
SK$e_prev_100k[SK$year == 2008]
SK$e_prev_num[SK$year == 2008]*100000/SK$e_pop_num[SK$year == 2008]

PER<-subset(TB.burden,TB.burden$country %in% "Peru") #subset of data in Peru
dim(PER)
PER$e_prev_100k[PER$year == 2014]
PER$e_inc_num[PER$year == 2014]
SK$e_prev_num[SK$year == 2008]*100000/SK$e_pop_num[SK$year == 2008]

for (i in levels(TB.burden$g_whoregion)){ #selection of the WHO regions (AFR, AMR, EMR, EUR, SEA, WPR)
	nam <- paste0(i,"_whoregion") #creation of the name based on acronyms from levels
	assign(nam, subset(TB.burden,TB.burden$g_whoregion %in% paste(i))) #assigning each level a subset based on the region
}
ls() #check the workspace with new subsets with "region"_whoregion ie AFR_whoregion for Africa

#####################
# SETUP FOR GGPLOT2 #
#####################
# custom themes
##############################
# set ggplot2 theme (dark_t) #
##############################
dark_t <- theme_minimal()+
  theme(
  plot.background = element_rect(fill="#191919"), #plot background
  panel.border = element_blank(), #removes border
  panel.background = element_rect(fill = "#000000",colour="#000000",size=2), #panel background and border
  panel.grid = element_line(colour = "#333131"), #panel grid colours
  panel.grid.major = element_line(colour = "#333131"), #panel major grid color
  panel.grid.minor = element_blank(), #removes minor grid
  
  plot.title=element_text(size=16, face="bold",hjust=0,vjust=1,color="#E0E0E0"), #set the plot title
  plot.subtitle=element_text(size=12,face=c("bold","italic"),hjust=0.01,color="#E0E0E0"), #set subtitle
  
  axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,vjust=1,margin=margin(r=10),colour="#E0E0E0"), # axis ticks
  axis.title.x = element_text(size=11,angle = 0,colour="#E0E0E0"), #axis labels from labs() below
  axis.text.y = element_text(size=9,margin = margin(r=5),colour="#E0E0E0"), #y-axis labels
  
  legend.title=element_text(size=11,face="bold",vjust=0.5,colour="#E0E0E0"), #specify legend title color
  legend.background = element_rect(fill="#262626",colour="#383838", size=.5), #legend background and cborder
  legend.text=element_text(colour="#E0E0E0")) #legend text colour
  # plot.margin=unit(c(t=1,r=1.2,b=1.2,l=1),"cm")) #custom margins

###############################
# set ggplot2 theme (light_t) #
###############################
light_t <- theme(legend.background = element_rect(fill="grey95",colour="grey70", size=.5),
	plot.title=element_text(size=16, face="bold",hjust=0,vjust=1), #set the plot title
	plot.subtitle=element_text(size=12,face=c("bold","italic"),hjust=0.01), #set subtitle

	panel.grid.minor = element_blank()) #removes minor grid)
	
# specify scale for year variable
year.scale <- c(1990,1995,2000,2005,2010,2014) # used for breaks below

# use if want to keep scientific notation
fancy_scientific <- function(l) { #http://stackoverflow.com/questions/11610377/how-do-i-change-the-formatting-of-numbers-on-an-axis-with-ggplot
     # turn in to character string in scientific notation
     l <- format(l, scientific = TRUE)
     # quote the part before the exponent to keep all the digits
     l <- gsub("^(.*)e", "'\\1'e", l)
     # turn the 'e+' into plotmath format
     l <- gsub("e", "%*%10^", l)
     # return this as an expression
     parse(text=l)
}
# point-plot of the population in each country

###########################
# WHO REGIONAL COMPARISON #
###########################
# https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
library(reshape2)
library(scales)
# REGION, Year, prev_100k
TB.b1 <- TB.burden[,c(1,5,6,8)];TB.b1[1:30,]
ggplot(TB.b1,aes(y=g_whoregion,x=year)) +
	geom_tile(aes(fill=e_prev_100k))+
	scale_fill_gradient(trans="log10",low="#F9FA00",high="#8C00E6") #log transformation
 	
#rescaled
TB.b1r <- ddply(TB.b1, .(year), transform,rescale = rescale(e_prev_100k));TB.b1[1:30,]
ggplot(TB.b1r,aes(y=g_whoregion,x=year)) +
	geom_tile(aes(fill=rescale))+
	scale_fill_gradient(low="#F9FA00",high="#8C00E6")
	#scale_fill_gradient(low="#F9FA00",high="#8C00E6")

######################
# WHO REGION: AFRICA #
######################
#PREVALENCE
library(forcats) #https://blog.rstudio.org/2016/11/14/ggplot2-2-2-0/
pAFR <-ggplot(AFR_whoregion,aes(y=fct_rev(country),x=e_prev_100k)) + # prevalence (100k)
	labs(title="WHO | Tuberculosis Data",
	subtitle="Africa Region", x="Prevalence (per 100,000)",y="",colour="#E0E0E0") +
	geom_point(shape=20,aes(colour=year,fill=year),alpha=0.6,size=3) +
	scale_y_discrete(expand=c(0.002, 1))+
	scale_x_continuous(limits=c(0,1500),labels=scales::comma)+
scale_fill_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to fill in aes
scale_colour_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to colour in aes
dark_t;
pAFR #colours are for the dark_t theme

abpAFR <-ggplot(AFR_whoregion,aes(y=fct_rev(country),x=e_prev_num)) + # prevalence (raw)
	ggtitle("TB Prevalence (raw count)") +
	labs(title="WHO | Tuberculosis Data",
	subtitle="Africa Region", x="Prevalence (raw count)",y="") +
	geom_point(shape=20,aes(colour=year,fill=year),alpha=0.6,size=3) +
	scale_y_discrete(expand=c(0.002, 1))+
	scale_x_continuous(limits=c(0,600000),labels=scales::comma)+
	scale_fill_gradient(guide_legend(title="Year"),breaks=year.scale,low="#14FF89",high="#8C00E6") + #assigns colours to fill in aes
scale_colour_gradient(guide_legend(title="Year"),breaks=year.scale,low="#14FF89",high="#8C00E6") + #assigns colours to colour in aes
light_t;abpAFR #colours are for the light_t theme

grid.arrange(pAFR,abpAFR,ncol=2)

#MORTALITY (exclude HIV)
mAFR <- ggplot(AFR_whoregion,aes(y=fct_rev(country),x= e_mort_exc_tbhiv_100k)) + # Mortality exclude HIV (100k)
	labs(title="WHO | Tuberculosis Data",
	subtitle="Africa Region",x="Mortality excluding HIV (per 100,000)",y="",colour="#E0E0E0")+
	geom_point(shape=20,aes(colour=year,fill=year),alpha=0.6,size=3) +
	scale_y_discrete(expand=c(0.002, 1)) +
	scale_x_continuous(limits=c(0,250),labels=scales::comma) +
	scale_fill_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to fill in aes
	scale_colour_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to colour in aes
	dark_t

abmAFR <- ggplot(AFR_whoregion,aes(y=fct_rev(country),x= e_mort_exc_tbhiv_num)) + # Mortality exclude HIV (raw)
	labs(title="WHO | Tuberculosis Data",
	subtitle="Africa Region",x="Mortality excluding HIV (raw count)",y="",colour="#E0E0E0")+
	geom_point(shape=20,aes(colour=year,fill=year),alpha=0.6,size=3) +
	scale_y_discrete(expand=c(0.002, 1)) +
	scale_x_continuous(limits=c(0,175000),labels=scales::comma) +
	scale_fill_gradient(guide_legend(title="Year"),breaks=year.scale,low="#14FF89",high="#8C00E6") + #assigns colours to fill in aes
	scale_colour_gradient(guide_legend(title="Year"),breaks=year.scale,low="#14FF89",high="#8C00E6") + #assigns colours to colour in aes
	dark_t

library(gridExtra)
grid.arrange(mAFR,abmAFR,ncol=2)

# MORTALITY (include HIV)
# Mortality include HIV (100k)
pmAFR.HIV <- ggplot(AFR_whoregion,aes(y=fct_rev(country),x=e_mort_tbhiv_100k)) +
	labs(title="WHO | Tuberculosis Data",
	subtitle="Africa Region",x="Mortality including HIV (per 100,000)",y="",colour="#E0E0E0")+
	geom_point(shape=20,aes(colour=year,fill=year),alpha=0.6,size=3) +
	scale_y_discrete(expand=c(0.002, 1)) +
	scale_x_continuous(limits=c(0,250),labels=scales::comma) +
	scale_fill_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to fill in aes
	scale_colour_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to colour in aes
	dark_t

# Mortality include HIV (raw)
mabmAFR.HIV <- ggplot(AFR_whoregion,aes(y=fct_rev(country),x= e_mort_tbhiv_num)) +
	labs(title="WHO | Tuberculosis Data",
	subtitle="Africa Region",x="Mortality including HIV (per 100,000)",y="",colour="#E0E0E0")+
	geom_point(shape=20,aes(colour=year,fill=year),alpha=0.6,size=3) +
	scale_y_discrete(expand=c(0.002, 1)) +
	scale_x_continuous(limits=c(0,250),labels=scales::comma) +
	scale_fill_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to fill in aes
	scale_colour_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to colour in aes
	dark_t

grid.arrange(pmAFR.HIV, mabmAFR.HIV,ncol=2) #side by side plot

# Comparison of the HIV included excluded estimated mortality
grid.arrange(pmAFR.HIV, mAFR,ncol=2) #included, excluded

###################
##### HEATMAP #####
###################
ggplot(AFR_whoregion,aes(y=country,x=year)) +
	geom_tile(aes(fill=e_mort),colour="#F0F0F0") +
	labs(title="WHO | Tuberculosis Data",
	subtitle="Africa Region",
	x = "Year", y = "") +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0),breaks=year.scale) +
	scale_fill_gradient(guide_legend(title="Prevalence (per 100k)"),
	low="#FFF56B",high="#2000B3",
	breaks=c(0,250,500,750,1000,1250,1500),
	limits=c(0,1500),labels=scales::comma)+ 
	theme_grey() + theme(axis.ticks = element_blank(),
	axis.text.x=element_text(size=9,family="Verdana"), #http://www.cookbook-r.com/Graphs/Fonts/#table-of-fonts
	axis.text.y=element_text(size=7.5,family="Verdana"),
	legend.title=element_text(size=9,family="Verdana",vjust=0.5), #specify legend title color
	legend.text=element_text(size=7,family="Verdana",face="italic"),
	legend.background = element_rect(size=.5), #legend background and cborder
	panel.background = element_rect(fill ="grey85",colour="grey85",size=1),
	panel.grid = element_blank()
	)
grid.arrange(
(ggplot(AFR_whoregion,aes(y=country,x=year)) +
	geom_tile(aes(fill=e_mort_tbhiv_100k),colour="#F0F0F0") +
	labs(title="WHO | Tuberculosis Data",
	subtitle="Africa Region",
	x = "Year", y = "") +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0),breaks=year.scale) +
	scale_fill_gradient(guide_legend(title="Prevalence (per 100k)"),
	low="#FFF56B",high="#2000B3",
	labels=scales::comma,na.value="black")+ 
	theme_grey() + theme(axis.ticks = element_blank(),
	axis.text.x=element_text(size=9,family="Verdana"), #http://www.cookbook-r.com/Graphs/Fonts/#table-of-fonts
	axis.text.y=element_text(size=7.5,family="Verdana"),
	legend.title=element_text(size=9,family="Verdana",vjust=0.5), #specify legend title color
	legend.text=element_text(size=7,family="Verdana",face="italic"),
	legend.background = element_rect(size=.5), #legend background and cborder
	panel.background = element_rect(fill ="grey85",colour="grey85",size=1),
	panel.grid = element_blank()
	)),

# MORTALITY TB+HIV 100k
(ggplot(AFR_whoregion,aes(y=country,x=year)) +
	geom_tile(aes(fill=e_mort_tbhiv_100k),colour="#F0F0F0") +
	labs(title="WHO | Tuberculosis Data",
	subtitle="Africa Region",
	x = "Year", y = "") +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0),breaks=year.scale) +
	scale_fill_gradient(guide_legend(title="Prevalence (per 100k)"),
	low="#FFF56B",high="#2000B3",
	labels=scales::comma,na.value="black")+ 
	theme_grey() + theme(axis.ticks = element_blank(),
	axis.text.x=element_text(size=9,family="Verdana"), #http://www.cookbook-r.com/Graphs/Fonts/#table-of-fonts
	axis.text.y=element_text(size=7.5,family="Verdana"),
	legend.title=element_text(size=9,family="Verdana",vjust=0.5), #specify legend title color
	legend.text=element_text(size=7,family="Verdana",face="italic"),
	legend.background = element_rect(size=.5), #legend background and cborder
	panel.background = element_rect(fill ="grey85",colour="grey85",size=1),
	panel.grid = element_blank()
	))
)


AFR_whoregion[AFR_whoregion$country=="South Sudan",]

# WHO REGION: AMERICAS
unique(AMR_whoregion$country)
# point-plot of the population in each country
pAMR <- ggplot(AMR_whoregion,aes(y=(country),x=e_prev_100k)) +
	geom_point(shape=21,aes(colour=year,fill=year),alpha=0.8,size=2) +
	scale_fill_gradient(low="#14FF89",high="#8C00E6") + #assigns colours to fill in aes
	scale_colour_gradient(low="#14FF89",high="#8C00E6") #assigns colours to colour in aes
# line plot for population increase over years *BUT legend is too big, too many countries
ggplot(AMR_whoregion,aes(x=year,y=e_pop_num))+
geom_line(aes(colour=country))
#

# WHO REGION: MIDDLE EAST
unique(EMR_whoregion$country)
# point-plot of the population in each country
pEMR <- ggplot(EMR_whoregion,aes(y=rev(country),x=e_prev_100k)) +
	geom_point(shape=21,aes(colour=year,fill=year),alpha=0.8,size=2) +
	scale_fill_gradient(low="#14FF89",high="#8C00E6") + #assigns colours to fill in aes
	scale_colour_gradient(low="#14FF89",high="#8C00E6") #assigns colours to colour in aes
# line plot for population increase over years *BUT legend is too big, too many countries
ggplot(EMR_whoregion,aes(x=year,y=e_pop_num))+
geom_line(aes(colour=country))
#

# WHO REGION: EUROPE
unique(EUR_whoregion$country)
# point-plot of the population in each country
pEUR<-ggplot(EUR_whoregion,aes(y=rev(country),x=e_prev_100k)) +
	geom_point(shape=21,aes(colour=year,fill=year),alpha=0.8,size=2) +
	scale_fill_gradient(low="#14FF89",high="#8C00E6") + #assigns colours to fill in aes
	scale_colour_gradient(low="#14FF89",high="#8C00E6") #assigns colours to colour in aes

# WHO REGION: SOUTH EAST ASIA
unique(SEA_whoregion$country)

# point-plot of the population in each country
pSEA<-ggplot(SEA_whoregion,aes(y=rev(country),x=e_prev_100k)) +
	geom_point(shape=21,aes(colour=year,fill=year),alpha=0.8,size=2) +
	scale_fill_gradient(low="#14FF89",high="#8C00E6") + #assigns colours to fill in aes
	scale_colour_gradient(low="#14FF89",high="#8C00E6") #assigns colours to colour in aes

# WHO REGION: ASIA AND PACIFIC REGION
unique(WPR_whoregion$country)

# point-plot of the population in each country
pWPR<-ggplot(WPR_whoregion,aes(y=rev(country),x=e_prev_100k)) +
	geom_point(shape=21,aes(colour=year,fill=year),alpha=0.8,size=2) +
	scale_fill_gradient(low="#14FF89",high="#8C00E6") + #assigns colours to fill in aes
	scale_colour_gradient(low="#14FF89",high="#8C00E6") #assigns colours to colour in aes

library(gridExtra)
grid.arrange(pWPR,pEMR,nrow=2)

maxWidth = grid::unit.pmax(pWPR$widths[2:5], pEMR$widths[2:5])
 pWPR$widths[2:5] <- as.list(maxWidth)
 pEMR$widths[2:5] <- as.list(maxWidth)
 grid.arrange(pAMR, pEUR, ncol=1)

gridExtra::grid.newpage()
grid::grid.draw(rbind(pWPR, pEMR))

grid::grid.draw(rbind(pWPR,pEMR))

# RIBBON PLOT of ESTIMATES
# example using Pakistan
PK <-subset(TB.burden,TB.burden$country %in% "Pakistan" | TB.burden$country %in% "Afghanistan")
head(PK)
h <- ggplot(PK,aes(year))
h +
  geom_ribbon(aes(ymin = e_prev_100k_lo, ymax = e_prev_100k_hi),stat="identity", fill = "grey70") +
  geom_line(aes(y = e_prev_100k),stat="identity")

ggplot(PK,aes(year, e_prev_100k)) +
	geom_line(aes(subset(PK,country=="Pakistan"))) +
	geom_line(aes(subset(PK,country=="Afghanistan")))


quebec<-ggplot(data=subset(CO2,Type %in% "Quebec"), aes(x=conc, y=uptake, group=Plant, colour=Plant)) + geom_line() + geom_point() + ggtitle("Concentration vs. Uptake in Quebec Plants") + ylab("Uptake (umol/m^2 sec)") + xlab("Concentration (mL/L)") + ylim(c(5,40))miss<-ggplot(data=subset(CO2,Type %in% "Mississippi"), aes(x=conc, y=uptake, group=Plant, colour=Plant)) + geom_line() + geom_point() + ggtitle("Concentration vs. Uptake in Mississippi Plants") + ylab("Uptake (umol/m^2 sec)") + xlab("Concentration (mL/L)") + ylim(c(5,40))chill<-ggplot(data=subset(CO2,Treatment %in% "chilled"), aes(x=conc, y=uptake, group=Plant, colour=Plant)) + geom_line() + geom_point() + ggtitle("Concentration vs. Uptake by Chilled Treatment") + ylab("Uptake (umol/m^2 sec)") + xlab("Concentration (mL/L)") + ylim(c(5,40))nochill<-ggplot(data=subset(CO2,Treatment %in% "nonchilled"), aes(x=conc, y=uptake, group=Plant, colour=Plant)) + geom_line() + geom_point() + ggtitle("Concentration vs. Uptake non-Chilled Treatment") + ylab("Uptake (umol/m^2 sec)") + xlab("Concentration (mL/L)") + ylim(c(5,40))png("q2.png",height=9000,width=16000,res=1200)grid.arrange(quebec, miss, chill, nochill, ncol=2,nrow=2)dev.off() 