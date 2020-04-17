
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(here)
library(ggthemes)
library(glue)
library(readxl)
library(janitor)
library(vroom)




round2 = function(x, digits) {
    posneg = sign(x)
    z = abs(x)*10^digits
    z = z + 0.5
    z = trunc(z)
    z = z/10^digits
    z*posneg
}




gg <- "https://docs.google.com/spreadsheets/d/117gAvhBJuLdcsMRDITikmoVH5MoLKxrJnbD2XvlheYE/edit#gid=2073491991"
gg <- "https://docs.google.com/spreadsheets/d/1N4DVmiAhbHvGsk9BtVD3ns0BLdWZHheUyeZjkfh1hBg/edit#gid=1229824276"

district <- read_sheet(gg, sheet = 2) %>% 
    mutate(cds = as.character(CDS))
region <- read_sheet(gg, sheet = 3)


ggplot(district, aes(fct_reorder(`District Name:`,`Percentage without access to a device (est.)` ), `Percentage without access to a device (est.)`)) +
    geom_col(fill = "blue") +
    coord_flip() +
    theme_hc() +
    labs(x = "")

ggsave(here("figs","Percent without device - district.png"))


grphit <- function(df, ind, col){
    
    ggplot(df, aes(fct_reorder(`District Name:`,{{ind}} ), {{ind}})) +
        geom_col(fill = col) +
        coord_flip() +
        theme_hc() +
        labs(x = "")    
}


grphit(district, `Percentage without access to a device (est.)`, "green4")
ggsave(here("figs","Percent without device - district.png"))


grphit(district, `Percentage without connectivity (est.)`, "darkseagreen2")
ggsave(here("figs","Percent without connectivity - district.png"))



grphit(district, `Students without access to a device (est.)`, "dodgerblue4")
ggsave(here("figs","Students without device - district.png"))


grphit(district, `Students without connectivity (est.)`, "lightblue2")
ggsave(here("figs","Students without connectivity - district.png"))



grphit(region, `Percentage without access to a device (est.)`, "green4")
ggsave(here("figs","Percent without device - region.png"))


grphit(region, `Percentage without connectivity (est.)`, "darkseagreen2")
ggsave(here("figs","Percent without connectivity - region.png"))



grphit(region, `Students without access to a device (est.)`, "dodgerblue4")
ggsave(here("figs","Students without device - region.png"))


grphit(region, `Students without connectivity (est.)`, "lightblue2")
ggsave(here("figs","Students without connectivity - region.png"))



drive_upload(here("figs","Students without connectivity - region.png"))


drive_put(here("figs","Students without connectivity - region.png"))





### Enrollment data folded in  ----

# https://www.cde.ca.gov/ds/sd/sd/filescupc.asp


cupc <- read_excel(here("data","cupc1819-k12.xlsx"), sheet = "LEA-Level CALPADS UPC Data", range = "A3:AA2302") %>% 
    clean_names(case = "upper_camel")  %>%  
    filter(CountyCode == 27) %>%
    mutate(cds = glue("{CountyCode}{DistrictCode}{SchoolCode}")) %>%
    mutate(cds = as.character(cds))


district_demo <- district  %>%
    left_join(cupc) 

district_demo2 <- district_demo %>% 
    mutate(ELcount = (`Students without connectivity (est.)`*EnglishLearnerEl/TotalEnrollment) %>% round2(),
           FRPMcount = (`Students without connectivity (est.)`*FreeReducedMealProgram/TotalEnrollment) %>% round2(),
           FHomelesscount = (`Students without connectivity (est.)`*Homeless/TotalEnrollment) %>% round2()
    )


