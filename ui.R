library(shiny)
#library(tidyverse)
library(dplyr)
library(ggplot2)
library(magrittr)
library(plotly)
library(markdown)

shinyUI(
  navbarPage(
    "Visualisering och faktoranalys av SVTs valkompass inför riksdagsvalet 2010",
    tabPanel(
      "Personnivå",
      sidebarPanel(
        selectInput(
          "fargsattning",
          label = "Välj färgsättning",
          choices = list(
            "Parti",
            "Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?",
            "1. Trängselavgifter ska införas i fler storstäder" ,
            "2. Sverige ska satsa på höghastighetståg",
            "3. Sverige ska ha licensjakt på varg",
            "4. Kärnkraften ska byggas ut",
            "5. Bensinskatten ska höjas",
            "6. Det ska vara fri entré till statliga museer",
            "7. Upphovsrättsskyddat material från internet ska fritt få laddas ner för eget bruk" ,
            "8. TV-licensen ska avskaffas och ersättas med en obligatorisk skatt" ,
            "9. Ingen kommun ska kunna säga nej till att ta emot flyktingar",
            "10. Färre asylsökande ska få stanna i Sverige" ,
            "11. Papperslösa flyktingar ska få fri vård",
            "12. Det ska vara förbjudet för lärare att bära heltäckande slöja i svenska skolor" ,
            "13. En större del av föräldraledigheten ska reserveras för papporna" ,
            "14. \"Positiv särbehandling\" vid antagning till högskolan ska avskaffas",
            "15. Vårdnadsbidraget ska avskaffas",
            "16. Fastighetsskatten ska höjas för villor med taxeringsvärde över 4,5 miljoner" ,
            "17. \"Förmögenhetsskatten\" ska återinföras" ,
            "18. Skatten för höginkomstagare ska höjas" ,
            "19. Pension och lön ska beskattas lika",
            "20. \"RUT-avdraget\" ska behållas" ,
            "21. Skatten på arbete ska sänkas",
            "22. Sexköpslagen ska avskaffas",
            "23. Straffet för rattfylleri ska skärpas",
            "24. Våldsbrott ska bestraffas hårdare" ,
            "25. \"FRA-lagen\" ska rivas upp" ,
            "26. Den som är 58 år ska kunna få förtida pension" ,
            "27. Sjukförsäkringen ska vara tidsbegränsad" ,
            "28. Tandvård ska ingå i sjukförsäkringen",
            "29. Det ska vara mer konkurrens och valfrihet i offentlig verksamhet",
            "30. A-kassan ska vara obligatorisk",
            "31. Kommunernas hemtjänst ska kosta lika mycket oavsett var man bor" ,
            "32. Svensk sjukvård ska erbjuda omskärelse enbart av medicinska skäl",
            "33. Privata vårdföretag som drivs med skattemedel ska kunna ge vinst till ägarna",
            "34. Alkoholmonopolet ska avskaffas",
            "35. Det ska bli svårare att använda bemanningsföretag för att kringgå LAS" ,
            "36. Statliga bolag ska inte säljas",
            "37. Svenska soldater ska snarast tas hem från Afghanistan" ,
            "38. Sveriges vapenexport ska upphöra",
            "39. Biståndet ska inte gå till odemokratiska länder" ,
            "40. Den militära värnplikten ska återinföras",
            "41. Sverige ska lämna EU",
            "42. Stökiga elever ska kunna flyttas från sin skola mot elevens och föräldrarnas vilja",
            "43. Den kommunala skolan ska förstatligas" ,
            "44. Betyg ska införas från årskurs 6",
            "45. Antalet friskolor ska begränsas" ,
            "46. Alla gymnasieprogram ska leda till högskolebehörighet" ,
            "47. Det ska vara straffbart att vara med i rasistiska organisationer",
            "48. Monarkin ska avskaffas",
            "49. Personvalets betydelse i Sverige ska stärkas",
            "50. Det ska hållas fler folkomröstningar i Sverige"
          ),
          selected = "Parti"
        ),
        checkboxGroupInput(
          "vilka",
          label = h3("Vilka personer vill du visa?"),
          choices = list(
            "Valda till riksdagen" = "1",
          #  "Avgått som riksdagsledamöter" = "Avgått",
           # "Statsråd" = "Regeringen",
            "Övriga" = "0"
          ),
          selected = 1
        ),
        checkboxGroupInput(
          "vilka2",
          label = h3("Vilka partier vill du visa?"),
          choices = list(
            "Centerpartiet" = "Centerpartiet",
            "Feministiskt initiativ" = "Feministiskt initiativ",
            "Folkpartiet" = "Folkpartiet",
            "Kristdemokraterna" = "Kristdemokraterna",
            "Miljöpartiet" = "Miljöpartiet",
            "Moderaterna" = "Moderaterna",
            "Piratpartiet" = "Piratpartiet",
            "Socialdemokraterna" = "Socialdemokraterna",
            "Sverigedemokraterna" = "Sverigedemokraterna",
            "Vänsterpartiet" = "Vänsterpartiet"
          ),
          selected = c(
            "Centerpartiet",
            "Feministiskt initiativ",
            "Folkpartiet",
            "Kristdemokraterna",
            "Miljöpartiet",
            "Moderaterna",
            "Piratpartiet",
            "Socialdemokraterna",
            "Sverigedemokraterna",
            "Vänsterpartiet"
          )
        )
        
      ),
      mainPanel(plotlyOutput(
        "plot", width = 800, height = 600
      ))
    ),
    tabPanel(
      "Partinivå (spridning)",
      sidebarPanel(
        radioButtons(
          "vad",
          label = h3("Vad vill du se för punkter?"),
          choices = list(
            "Partimedelvärden" = "0",
            "Samtliga partirepresentanter" = "1"
          )
        ),
        checkboxGroupInput(
          "vilka_parti",
          label = h3("Vilka personer vill du ha med?"),
          choices = list(
            "Valda till riksdagen" = "1",
          #  "Avgått som riksdagsledamöter" = "Avgått",
           # "Statsråd" = "Regeringen",
            "Övriga" = "0"
          ),
          selected = 1
        ),
        checkboxGroupInput(
          "vilka2_parti",
          label = h3("Vilka partier vill du visa?"),
          choices = list(
            "Centerpartiet" = "Centerpartiet",
            "Feministiskt initiativ" = "Feministiskt initiativ",
            "Folkpartiet" = "Folkpartiet",
            "Kristdemokraterna" = "Kristdemokraterna",
            "Miljöpartiet" = "Miljöpartiet",
            "Moderaterna" = "Moderaterna",
            "Piratpartiet" = "Piratpartiet",
            "Socialdemokraterna" = "Socialdemokraterna",
            "Sverigedemokraterna" = "Sverigedemokraterna",
            "Vänsterpartiet" = "Vänsterpartiet"
          ),
          selected = c(
            "Centerpartiet",
            "Feministiskt initiativ",
            "Folkpartiet",
            "Kristdemokraterna",
            "Miljöpartiet",
            "Moderaterna",
            "Piratpartiet",
            "Socialdemokraterna",
            "Sverigedemokraterna",
            "Vänsterpartiet"
          )
        )
        
      ),
      mainPanel(plotlyOutput(
        "plot_parti", width = 800, height = 600
      ))
    ),
    tabPanel(
      "Om",
      helpText(
        "Denna visualisering är byggd av Staffan Betnér (@staffanbetner). Rådatat kommer från SVT Pejl.
        Samtliga individers placeringar är skattade med en IRT-modell, en slags faktoranalys (se Wikipedia) för kategorivariabler. 
        Med hjälp av kandidatöverlappet med 2014 är placeringarna roterade, skalade och förskjutna, så att 2010 års placeringar ska ligga så nära de för 2014 som möjligt."
      ),
      helpText(
        "Skalorna är inte konceptuella/ideologiska, utan bygger rent empiriskt på hur olika åsikter hänger ihop hos samtliga riksdagskandidater.
Dessa skalor kräver tolkning, vilka jag tolkat som en traditionell höger-vänster-skala samt en skala som kan beskrivas som kulturell höger-vänster (många tolkningar av denna konfliktdimenson finns). 
Jag har valt att använda de av Hooghe, Marks och Wilson (2002) stiftade termerna: Grön-Alternativ-Liberal kontra Tradition-Auktoritet-Nationalist (GAL-TAN), men andra tänkbara namn är New Politics eller autonomi-delegeringsdimensionen (Henrik Oscarsson).
        Korrelationen (Spearmans mått) mellan höger-vänsterplaceringen och självskattningen på den skalan är 0.84,
        mellan GAL-TAN och höger-vänster-självskattningen -0.02,
        och mellan placeringarna på de olika axlarna -0.02."
      ),
      helpText((tags$div(
        checked = NA,
        tags$p(tags$a(href = "https://github.com/StaffanBetner/valkompass2010", "Här"),
          " finns koden för appen. Det är inte jättebra dokumenterat, men där finns allt som behövs."
        )
      )))
      )
)
)
