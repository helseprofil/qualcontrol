library(profvis)


profvis(select_valcol_pri(names(newcube), "teller")
        )
