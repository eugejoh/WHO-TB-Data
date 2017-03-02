#TB WHO DATA

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
TB.dic <- read.csv("TB_data_dictionary_2016-02-18.csv") #TB documentation dictionary

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

#####################
# SETUP FOR GGPLOT2 #
#####################
# custom themes
##############################
# set ggplot2 theme (dark_t) #
##############################
ctext <- theme(axis.ticks = element_blank(),
	plot.title=element_text(family="Verdana"),
	axis.text.x=element_text(size=9,family="Verdana"), #http://www.cookbook-r.com/Graphs/Fonts/#table-of-fonts
	axis.title.x=element_text(size=10),
	axis.text.y=element_text(size=7.5,family="Verdana"),
	axis.title.y=element_text(size=10,family="Verdana"),
	legend.title=element_text(size=9,family="Verdana",vjust=0.5), #specify legend title color
	legend.text=element_text(size=7,family="Verdana",face="italic"))

dark_t <- theme_minimal()+
  theme(
  plot.background = element_rect(fill="#191919"), #plot background
  panel.border = element_blank(), #removes border
  panel.background = element_rect(fill = "#000000",colour="#000000",size=2), #panel background and border
  panel.grid = element_line(colour = "#333131"), #panel grid colours
  panel.grid.major = element_line(colour = "#333131"), #panel major grid color
  panel.grid.minor = element_blank(), #removes minor grid
  
  plot.title=element_text(size=16, face="bold",family="Verdana",hjust=0,vjust=1,color="#E0E0E0"), #set the plot title
  plot.subtitle=element_text(size=12,face=c("bold","italic"),family="Verdana",hjust=0.01,color="#E0E0E0"), #set subtitle
  
  axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,vjust=1,margin=margin(r=10),colour="#E0E0E0"), # axis ticks
  axis.title.x = element_text(size=11,angle = 0,colour="#E0E0E0"), #axis labels from labs() below
  axis.text.y = element_text(size=9,margin = margin(r=5),colour="#E0E0E0"), #y-axis labels
  
  legend.title=element_text(size=11,face="bold",vjust=0.5,colour="#E0E0E0"), #specify legend title color
  legend.background = element_rect(fill="#262626",colour="#383838", size=.5), #legend background and cborder
  legend.text=element_text(colour="#E0E0E0")) + #legend text colour
  # plot.margin=unit(c(t=1,r=1.2,b=1.2,l=1),"cm")) #custom margins
ctext

###############################
# set ggplot2 theme (light_t) #
###############################
light_t <- theme(legend.background = element_rect(fill="grey95",colour="grey70", size=.5),
  plot.title=element_text(size=16, face="bold",family="Verdana",hjust=0,vjust=1), #set the plot title
  plot.subtitle=element_text(size=12,face=c("bold","italic"),family="Verdana",hjust=0.01), #set subtitle

  panel.grid.minor = element_blank()) + #removes minor grid)
ctext
	
# specify scale for year variable
year.scale <- c(1990,1995,2000,2005,2010,2014) # used for breaks below

###########################
# CREATE REGIONAL SUBSETS #
###########################
for (i in levels(TB.burden$g_whoregion)){ #selection of the WHO regions (AFR, AMR, EMR, EUR, SEA, WPR)
	nam <- paste0(i,"_whoregion") #creation of the name based on acronyms from levels
	assign(nam, subset(TB.burden,TB.burden$g_whoregion %in% paste(i))) #assigning each level a subset based on the region
}
ls() #check the workspace with new subsets with "region"_whoregion ie AFR_whoregion for Africa

######################
# WHO REGION: AFRICA #
######################
#PREVALENCE per 100k
library(forcats) #https://blog.rstudio.org/2016/11/14/ggplot2-2-2-0/
pAFR <-ggplot(AFR_whoregion,aes(y=fct_rev(country),x=e_prev_100k)) + # prevalence (100k)
	labs(title="WHO | Tuberculosis Data",
	subtitle="Africa Region", x="Estimated Prevalence (per 100,000)",y="",colour="#E0E0E0") +
	geom_point(shape=20,aes(colour=year,fill=year),alpha=0.6,size=3) +
	scale_y_discrete(expand=c(0.002, 1))+
	scale_x_continuous(limits=c(0,1500),labels=scales::comma)+
scale_fill_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to fill in aes
scale_colour_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to colour in aes
dark_t

#MORTALITY (including HIV) per 100k e_inc_tbhiv_100k
mAFR <- ggplot(AFR_whoregion,aes(y=fct_rev(country),x= e_mort_exc_tbhiv_100k)) + # Mortality exclude HIV (100k)
	labs(title="WHO | Tuberculosis Data",
	subtitle="Africa Region",x="Estimated Mortality excluding HIV (per 100,000)",y="",colour="#E0E0E0")+
	geom_point(shape=20,aes(colour=year,fill=year),alpha=0.6,size=3) +
	scale_y_discrete(expand=c(0.002, 1)) +
	#scale_x_continuous(limits=c(0,250),labels=scales::comma) +
	scale_fill_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to fill in aes
	scale_colour_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to colour in aes
dark_t

#INCIDENCE per 100k
iAFR <- ggplot(AFR_whoregion,aes(y=fct_rev(country),x= e_inc_100k)) + # Mortality exclude HIV (100k)
	labs(title="WHO | Tuberculosis Data",
	subtitle="Africa Region",x="Estimated Incidence (per 100,000)",y="",colour="#E0E0E0")+
	geom_point(shape=20,aes(colour=year,fill=year),alpha=0.6,size=3) +
	scale_y_discrete(expand=c(0.002, 1)) +
	scale_x_continuous(limits=c(0,1500),labels=scales::comma) +
	scale_fill_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to fill in aes
	scale_colour_gradient(guide_legend(title="Year"),breaks=year.scale,low="#F9FA00",high="#8C00E6") + #assigns colours to colour in aes
dark_t

grid.arrange(pAFR,iAFR,mAFR,ncol=3)

# SAVING GRAPHICS

setwd("/Users/eugenejoh/GitHub/TB-Data") #set directory for plots through ggsave()

#pAFR
ggsave(plot=pAFR,filename="AFR_prev_TB.png", width=5.5, height=10, dpi=400)

#mAFR
ggsave(plot=mAFR,filename="AFR_mort_TB_exc_HIV.png", width=5.5, height=10, dpi=400)

#iAFR
ggsave(plot=iAFR,filename="AFR_inc_TB.png", width=5.5, height=10, dpi=400)

setwd("/Users/eugenejoh/Documents/BU Graduate School/BU SPH/2016 Winter/BS 720/TB") #set working directory for data