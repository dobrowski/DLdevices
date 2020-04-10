
library(tidyverse)
library(googlesheets4)
library(here)
library(ggthemes)
library(glue)


gg <- "https://docs.google.com/spreadsheets/d/117gAvhBJuLdcsMRDITikmoVH5MoLKxrJnbD2XvlheYE/edit#gid=2073491991"

district <- read_sheet(gg, sheet = 2)
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




