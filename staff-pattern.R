
staff <- c(
"Alicia Diaz"
,"Alicia Gregory"
,"Caryn Lewis"
,"Cathy Cranson"
,"David Dobrowski"
,"Denise Green"
,"Dora Salazar"
,"Dora Ann Salazar"
,"Esther Rubio"
,"jrmendoza"
,"Laurie Ramirez"
,"Michelle Ramirez"
,"Megan Matteoni"
,"Philip Davis"
,"Roberto Nunez"
,"Will Franzell"
)


staff.pattern <- paste(staff, collapse = "|")





ILNpeople %>%
    arrange(desc(TotalMinutes)) %>%
    top_n(20) %>%
    mutate(is.staff =  str_detect(NameOriginalName,staff.pattern)) %>%
    ggplot(aes(x = reorder(NameOriginalName,TotalMinutes), y = TotalMinutes, fill = is.staff)) +
    geom_col() +
    coord_flip() +
    mcoe_theme +
    theme(legend.position = "none") +
    labs(title = "Longest Participation at ILN meetings",
         subtitle = "Ed Services staff are in orange",
         y = "Total number of minutes")


ggsave("Longest-staff.png", width = 6, height = 6)
