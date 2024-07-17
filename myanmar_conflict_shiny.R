library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(sf)
library(readxl)
library(ggthemes)
library(RColorBrewer)
library(ggiraph)
library(plotly)


MMR_prov <- st_read("MMR_adm1/MMR_adm1.shp") %>%
  st_transform(4326) %>%
  st_as_sf()

MMR_ethn <- st_read("GeoEPR/GeoEPR-2021.shp") %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  filter(statename=="Myanmar (Burma)") %>%
  filter(to!=1948) %>%
  mutate(timeframe=ifelse(to<2018,'1989-2017','2017-2021'))

MMR_conf <- read_xlsx("Conflicts.xlsx") %>%
  filter(best > 0) %>% #chose only conflicts in which someone died
  mutate(year= as.integer(year))

MMR_groups <- read_xlsx("Groups.xlsx")

# Define UI  ----
ui <- fluidPage(theme=shinytheme("cosmo"),
                titlePanel(markdown("# **Myanmar Conflict History** *explorer*")),
                navbarPage("Menu",
                           tabPanel("Welcome",
                                    fluidPage(
                                      verticalLayout(
                                        markdown("### **Welcome to the Myanmar Conflict History *explorer* !**
                                      
                                      
                                      This App visualises the occurrence of violent clashes with at least one casualty in Myanmar.
                                      
                                      ### How to use the App
                                      
                                      This application is designed to visualise the occurrence of violent clashes in Myanmar.
                                      Although it is assumed that that users have a general overview over the conflict dynamics, the **Background** section will provide a brief overview over the issue and suggests some possible patterns to pay attention to when using the interactive map.
                                      
                                      The **Conflict Map** allows to display conflicts with at least one casualty on a map of Myanmar's primary administrative units and dominant ethnic groups for a chosen year.
                                      It will ask for the selection of a period of time (1989-2017 or 2017-2021) for which the settlement pattern of ethnic groups is to be displayed to account for demographic changes brought about by the state-sanctioned escalation of violence in 2016/17.
                                      It is possible to display post-2017 incidents on the 1989-2017 map as the indication of the region in which the Rohingya/Muslim Arakanese minority was previously dominant may still be of explanatory value. An interpretation of the graphic should however take due care of this issue.
                                      
                                      The **Death toll** page includes a static bar chart displaying the total number of deaths per dyad.
                                      Furthermore, as not all conflicts were active at the same time, the option to display the number of deaths over a flexible period of time for a chosen dyad will be given.
                                      The **Death toll** page is intended to give users with a general overview over the conflict dynamics and the involved groups but interested in a specific dyad the opportunity to look into its activity reflected in the casualties per year.
                                      It should be noted that users should allow for a reasonable long time framein order to receive usable graphs.
                                      
                                      
                                      ### Disclaimer
                                      
                                      Explaining the persistence and recurrence of ethnic and political conflicts is beyond the scope and indeed not the purpose of this App.
                                      Its ambitions are much more modest in that merely a graphical overview over the spatial distribution of conflicts is to be given.
                                      Furthermore, although struggles between various groups characterised Myanmar's history since independence, the availability of reliable data is limited to years from 1989 onwards.
                                      
                                      
                                      ### Conflict Parties in Represented
                                      
                                      The App includes data for 25 different groups, listed in Table 1 below. 
                                      For space reasons, the visualisations on the **Conflict Map** and **Death Toll** page use the codes/acronyms when refering to specific groups.
                                      It is advised to consult Table 1 when interpreting the graphs.
                                      As extensive discussions of all the actors is beyond the scope of this App, please refer to specific literature for more detailed information.
                                  
                                      
                                               "
                                        ),
                                        mainPanel(DT::dataTableOutput(outputId = "table")),
                                        tags$figcaption(markdown("**Tab. 1:** Actors included in the data set."), )
                                      ),
                                    )),
                           tabPanel("Background",
                                    fluidPage(
                                      verticalLayout(
                                        markdown("
                                      ## A History of Conflicts
                                      
                                      In 2012/13 and again in 2016/17, Myanmar experienced waves of anti-Muslim violence, amounting to an alleged genocide against the Rohingya (Muslim Arakanese) minority.
                                      Whereas only the recent violent escalations attracted international public attention, the Rohingya have faced persecution and exclusion for decades, being collectively denied recognition of their ethnic identity and access to citizenship, thus the proverbial right to have rights.
                                      As Nick Cheesman (2017) shows, this is consequential to the ideology of *national races* (*taingyntha*) becoming a crucial determinant of social but also official perceptions of belonging to the nation, hence the state, since the 1960s.
                                      According to the idea of *taingyntha*, eight national races, subdivided into 135 ethnic groups, are indigenous to the area that is today is understood as Myanmar, where they live together in peace and harmony, forming the nation of the state as *Union*.
                                      However, rather than harmony, Myanmar experienced violent conflicts between various ethnic and political groups and the central government for decades.
                                      This app intends to sheds light on the diverse history of armed clashes, including but not limited to, the violence against the Rohingya.
                                      
                                      ### Ethnic groups and state building
                                      
                                      Arguably, *taingyntha* is an ideological construct, an idea promoted by a closed authoritarian regime in need for legitimation to create a politcal community against which a foreign Other can be constructed.
                                      As such, it reflects the production of history, rather than describing historically acurate the collective identieties of people living in Myanmar.
                                      Nevertheless, Myanmar is, by all measures, a multi-ethnic state.
                                      Figure 1 shows the distribution of major ethnic groups at the time of independence.
                                    
                                      
                                               "),
                                        # object 3,
                                        
                                        mainPanel(plotOutput(outputId = "map_e")),
                                        tags$figcaption(markdown("**Fig. 1:** Ethnic groups in Myanmar, 1949"), ),
                                        
                                        # object 4,
                                        markdown("
                                      
                                      
                                      
                                      In the course of the anti-colonial struggle, Ethnic Armed Organisations (EAO) claiming affiliation with several of these groups supported the indenpendence movement.
                                      At the 1948 Panglong Conference, leaders of the Shan, Kachin, and Chin EAOs met with the head of the interim government, Aung San, to negotiate the terms of independence.
                                      The agreement reached icluded significant self-determination rights for the respective groups in designated territories in the to-be-built state.
                                      This was reflected in the first [Constitution of the Union of Burma](https://www.ilo.org/dyn/natlex/docs/ELECTRONIC/79573/85699/F1436085708/MMR79573.pdf) in which respective *States* as largely autonomous administrative units were created.
                                      
                                      In the same vein, the [1974 Constitution of the Union of Burma](http://www.myanmar-law-library.org/IMG/pdf/constitution_de_1974.pdf) and the [2008 Constitution of the Union of the Republic of Myanmar](https://www.constituteproject.org/constitution/Myanmar_2008.pdf?lang=en) structure the state's territory in administrative units according to the *imagined homeland* of the ethnic groups recognised in accordance with *taingyntha*.
                                      Thus, 7 regions (*Yin*) representing the Bamar majority and 7 states (*Pyine*) representing *national races* are demarkated.
                                      Firgure 2 illustrates the constitutional set-up of the state.
                                      
                                      
                                      
                                               "),
                                        # object 5,
                                        
                                        mainPanel(girafeOutput(outputId = "map_a")),
                                        tags$figcaption(markdown("**Fig. 2:** Administrative areas of Myanmar"), ),
                                        
                                        # object 6,
                                        markdown(" ### From ethnicity to conflict
                                      
                                      The structural causes for the ongoing conflicts are complex and to be fully explained elsewhere.
                                      As ethnic identity was however always politcal salient (cf. Taylor 2005; Walton 2013, also for opposing arguments on the multi-ethnic formation of the nation), it seems reasonable to briefly reflect on the relationship between ethnicity and conflict. The following will thus suggest several developments to pay attention to when exploring the App.
                                      
                                      **1.** On a very fundamental level, Lieberman and Singh (2012) argue that classification of people into ethnic groups itself increases the potential for conflict.
                                      In Myanmar, the issue of classification is directly linked to the matter of citizenship (Holliday 2014). Arguably, especially in times in which the governing legal framework changes and in which political spaces for competition open, the question of belonging to the political community bears potential for conflict.
                                      It is thus recommended to pay attention to conflicts in 2008, when the new constitution was adopted, in 2010 and 2012 when elections took place, in 2014/15, i.e., in the run-up to the first competitive elections in which also the legal status of certain minorities was politicised, and in 2021 when a coup d'état altered the political landscape when using the **Conflict Map** to explore the history of violent clashes.
                                      
                                      **2.** With Toft (2010) on the other hand it can be suggested that groups' claims for distinct exclusive homelands create the potential for conflicts.
                                      As *Yin* and *Pyine* are supposed to reflect these homelands, it is proposed to also not the proximity (or distance) of clashes to administrative units' boundaries and those of the territories occupied by ethnic groups.
                                      
                                      **3.** Finally, Cederman, Wimmer and Min (2010) argue that groups' exclusion from access to state power increases their likelihood to rebel.
                                      As Figure 2 shows, half the administrative units, and in fact the centre of the state, are ascribed to the Bamar. Indeed, the state itself is largely dominated by Buddhist Bamar majority, making Burmese the official language and Bamar Buddhist identity the norm in institutions of mass socialisation.
                                      In this vein, it is suggested to note to what extend clashes take place in the periphery/at the borders between States and Regions when investigating their occurence with the **Conflict Map**.
                                      
                                      **4.** Beyond that, anti-Muslim discourses have received international attention in the course of the so-called *Rohingya crisis*.
                                      Anti-Muslim violence escalated, as mentioned, in 2012/13 and 2016/17, initially in Rakhine in the West of the country, then spreading over the territory.
                                      Note the disappearance of the Rohingya/Arakanese Muslim group from the map in the time after 2017, reflecting their forced migration.
                                      
                                      **5.** Economic factors and opportunity structure can also increase the likelihood of insurgencies. Myanmar's northern and north-eastern borderlands are mountainous and characterised by low state penetration, hence providing rebel groups to adopt guerilla stategies and persist over a long period of time.
                                      At the same time, natural resources are also present in this region and the production of drugs provides opportunities for - illicite - economic gains.
                                      It may therefore also be of interest, explore to what extend conflicts cluster in these areas.
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                               ")
                                      ))),
                           
                           #Map 
                           tabPanel("Conflict Map", 
                                    sidebarLayout(
                                      sidebarPanel(
                                        h3("Explore Conflict Dynamics."),
                                        selectInput("variable1", "Select a year:", choices = c(1989:2021)),
                                        selectInput("variable2", "Select a time period:", choices = c('1989-2017', '2017-2021')),
                                        submitButton("Show me the map!")
                                      ),
                                      mainPanel(
                                        girafeOutput(outputId = "map1")
                                      )
                                    )),
                           
                           #Death tolls
                           tabPanel("Death tolls",
                                    fluidPage(
                                      markdown("
                      ## How many people died?
                      
                      Conflicts vary in intensity.
                      Note, for example, the high number of people killed in clashes between the central government and civilians, and the central governement and the Karen armed organisation KNU, in contrast to low-intensity conflicts such as the one between the central government and God's Army.
                      Find the number of casualties for a the dyads of your interest in the bar chart below.
                                               
                                               "),
                                      plotlyOutput(outputId = "bars1"),
                                      markdown("The intensity of a given conflicts arguably also varies over time.
                                               Find out how the conflict developed as indicated by the number of casualties per year."),
                                      sidebarLayout(
                                        sidebarPanel(
                                          #h3("Explore Conflict Dynamics."),
                                          selectInput("variable3", "Select a dyad", choices = c("ARSA - Civilians", "Buddhists (Myanmar) - Muslims (Myanmar)", "DKBA - KNU", "Government of Myanmar (Burma) - ABSDF", "Government of Myanmar (Burma) - ARSA", "Government of Myanmar (Burma) - BMA", "Government of Myanmar (Burma) - Civilians", "Government of Myanmar (Burma) - DKBA 5", "Government of Myanmar (Burma) - God's Army", "Government of Myanmar (Burma) - KIO", "Government of Myanmar (Burma) - KNPP", "Government of Myanmar (Burma) - KNU", "Government of Myanmar (Burma) - MNDAA", "Government of Myanmar (Burma) - MTA", "Government of Myanmar (Burma) - NMSP", "Government of Myanmar (Burma) - NSCN-K", "Government of Myanmar (Burma) - NUG", "Government of Myanmar (Burma) - PSLF", "Government of Myanmar (Burma) - RCSS", "Government of Myanmar (Burma) - RSO", "Government of Myanmar (Burma) - SSPP", "Government of Myanmar (Burma) - ULA", "Government of Myanmar (Burma) - UWSA", "KNU - Civilians", "MDA - MDA - LM", "MTA - UWSA", "NUG - Civilians", "RCSS - UWSA")),
                                          sliderInput("variable4", "from:", min(MMR_conf$year), max(MMR_conf$year), min(MMR_conf$year), step = 1, sep = ""),
                                          sliderInput("variable5", "to:", min(MMR_conf$year), max(MMR_conf$year), max(MMR_conf$year), step = 1, sep = ""),
                                          submitButton("Show me the numbers!")
                                        ),
                                        mainPanel(
                                          plotlyOutput(outputId = "line1")
                                        )
                                      ))),
                           #About
                           tabPanel("About",
                                    fluidPage(
                                      markdown("## About **Myanmar Conflict History** *explorer*
                                      
                                      This [Shiny App](https://shiny.rstudio.com/) was built by Jan Schlebusch based on course work conducted as part of the the course [Data Visualization](https://www.ibei.org/en/data-visualization_138263) taught at the Institut Barcelona d'Estudis Internacionals (IBEI).
                                      
                                      ### Data
                                      
                                      For this App, data from three different sources was used: 
                                      
                                      [**DIVA-GIS**](https://www.diva-gis.org/gdata)-data to plot the boundaries of the administrative areas,
                                      
                                      [**GeoEPR**](https://icr.ethz.ch/data/epr/geoepr/) (Vogt et al. 2015) shapefiles describing the ethnic groups, and
                                      
                                      [**ACD2EPR**](https://icr.ethz.ch/data/epr/acd2epr/) (Wucherpfennig et al. 2012; Vogt et al. 2015) for geo-references of ACD 20.1 conflicts.
                                      
                                      
                                      ### References
                                               
                                      Cederman, Lars-Erik, Andreas Wimmer, and Brian Min. 2010. ‘Why Do Ethnic Groups Rebel? New Data and Analysis’. World Politics 62 (1): 87–119.

                                      Cheesman, Nick. 2017. ‘How in Myanmar “National Races” Came to Surpass Citizenship and Exclude Rohingya’. Journal of Contemporary Asia 47 (3): 461–83.

                                      Holliday, Ian. 2014. ‘Addressing Myanmar’s Citizenship Crisis’. Journal of Contemporary Asia 44 (3): 404–21.

                                      Lieberman, Evan S, and Prerna Singh. 2012. ‘The Institutional Origins of Ethnic Violence’. Comparative Politics 45 (1): 1–24.
                                      
                                      Taylor, Robert H. 2005. ‘Do States Make Nations?: The Politics of Identity in Myanmar Revisited’. South East Asia Research 13 (3): 261–86.
                                      
                                      Toft, Monica Duffy. 2010. ‘The Geography of Ethnic Violence’. In The Geography of Ethnic Violence. Princeton University Press.
                                      
                                      Vogt, Manuel, Nils-Christian Bormann, Seraina Rüegger, Lars-Erik Cederman, Philipp Hunziker, and Luc Girardin. 2015. ‘Integrating Data on Ethnicity, Geography, and Conflict: The Ethnic Power Relations Data Set Family’. Journal of Conflict Resolution 59 (7): 1327–42.
                                      
                                      Walton, Matthew J. 2013. ‘The “Wages of Burman-Ness:” Ethnicity and Burman Privilege in Contemporary Myanmar’. Journal of Contemporary Asia 43 (1): 1–27.
                                      
                                      Wucherpfennig, Julian, Nils B. Weidmann, Luc Girardin, Lars-Erik Cederman, and Andreas Wimmer. 2011. ‘Politically Relevant Ethnic Groups across Space and Time: Introducing the GeoEPR Dataset.’ Conflict Management and Peace Science 28 (5): 423–37."
                                               
                                      )
                                    )),
                           fluidRow(
                             br(),
                             column(8,
                                    HTML(  
                                      '<a rel="license" href="http://creativecommons.org/licenses/by-nd/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nd/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nd/4.0/">Creative Commons Attribution-NoDerivatives 4.0 International License</a>'
                                    ),
                                    br(),
                                    p(sprintf("Last updated on: %s", Sys.Date())),
                             )
                           )
                )
)






# Define server logic ----
server <- function(input, output) {
  
  
  
  output$map1<- renderGirafe({
    my_map <-  ggplot() +
      geom_sf(data = MMR_ethn %>%
                filter(timeframe==input$variable2), mapping = aes(fill = as.factor(group)), alpha=0.75)+
      scale_fill_brewer(palette = "Set3")+
      labs(fill="Group:")+
      geom_sf(data = MMR_prov, fill = NA, colour = "black", size = 0.7, show.legend = FALSE) +
      geom_point_interactive(data = MMR_conf %>%
                               filter(year==input$variable1),
                             mapping = aes(x = longitude, y = latitude, tooltip = dyad_name, data_id = dyad_name), colour = "red", size = 1.5, na.rm = T) +
      coord_sf()+
      theme_void()+
      theme(legend.position = "bottom")
    girafe(code= print(my_map))
    
  })
  
  
  
  output$bars1<- renderPlotly({
    my_bars<- ggplot() +
      geom_col(data=MMR_conf %>% group_by(dyad_name) %>%
                 summarise(deaths = sum (best)), aes(x=dyad_name, y=deaths))+
      labs(title = "Total number of deaths per dyad",
           y = "Deaths",
           x = NULL)+
      theme_clean()+
      coord_flip()
    
    ggplotly(my_bars)
  })
  
  
  output$line1 <- renderPlotly({
    my_line<- ggplot(data = MMR_conf %>%
                       filter(dyad_name == input$variable3) %>%
                       group_by(year)%>%
                       summarise(deaths = sum (best)),
                     aes(x=year, y=deaths))+
      geom_smooth(se=F)+
      geom_point()+
      xlim(input$variable4,input$variable5)+
      labs(title = "Number of deaths over time",
           y = "Deaths",
           x = NULL)+
      theme_clean()
    
    ggplotly(my_line)
  })
  
  output$map_e <- renderPlot({
    ggplot() + 
      geom_sf(data = MMR_ethn %>%
                filter(from==1949), mapping = aes(fill = group))+
      labs(fill="Group:")+
      scale_fill_brewer(palette = "Set3")+
      theme_void()+
      theme(legend.position = "bottom")
    #labs(title = "Fig. 1: Ethnic groups in Myanmar, 1949")
    
    
    
  })
  
  output$map_a <- renderGirafe({
    my_map_a<-ggplot() + 
      geom_sf_interactive(data = MMR_prov, aes(fill=TYPE_1, tooltip=NAME_1, data_id=NAME_1),
                          alpha=0.75,
                          colour="black",
                          size=0.25) + 
      labs(fill = "Constitutional Status:")+
      theme_void()+
      theme(legend.position = "bottom", 
            plot.background = element_rect(fill = "white", color = NA))
    #labs(title= "Fig. 2: Administrative units in Myanmar")
    
    girafe(code= print(my_map_a))
    
    
  })
  
  output$table <- DT::renderDataTable(MMR_groups)
  
  
  
  
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)

