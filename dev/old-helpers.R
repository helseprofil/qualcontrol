#
# library(dplyr)
# library(tools)
# library(rlang)
#
# # ---
#
# # Compare KUBE
# #- for comparing two kubes from different years.
# # input = string containing full path and filename of file1 and file2
# #file1 = most recent file
# #file2 = older file to compare with
#
#
# # install pacman - a tool for easy installing and loading the other required packages
# # if (!require("pacman")) install.packages("pacman")
# #
# # # use pacman to install - if needed, and load packages
# # pacman::p_load(dplyr, tools, rlang)
#
#
#
# # compareNA_unequal returns 0 wherever:
# # elements v1 and v2 are identical (within an absolute difference < 0.05),
# # are both missing,
# # OR returns 1 whenever else.
# compareNA_unequal <- function(v1,v2) {
#   notsame <- case_when(abs(v1-v2) < 0.5 ~ 0,
#                        is.na(v1) & is.na(v2) ~ 0,
#                        TRUE ~ 1)
#   return(notsame)
# }
#
# # compareNA_unequal_strict returns 0 wherever:
# # have less difference than 1
# # (the difference makes up less than 5% of v1 - NOT IN USE)
# # are both missing,
# # OR returns 1 whenever else.
# compareNA_unequal_strict <- function(v1,v2) {
#   notsame <- case_when((abs(v1-v2) < 1) ~ 0,
#                        is.na(v1) & is.na(v2) ~ 0,
#                        #abs(v1-v2) / v1 < 0.05 ~ 0,
#                        TRUE ~ 1)
#   return(notsame)
# }
#
#
# # Find exlude year
#
# find_excludeyear <- function() {
#
#   if (n_distinct(kube1$AAR) > n_distinct(kube2$AAR)) {
#     res <- kube1 %>%
#       filter(!AAR %in% kube2$AAR) %>%
#       select(AAR) %>%
#       unique()
#     return(as.vector(res$AAR)
#     )
#
#   } else {
#     print("no exclude year")
#   }
#
# }
#
# exclude_year <- find_excludeyear()
# print(paste0("New year:", exclude_year))
#
# # Print newcols - columns in kube1 (new) that are not found in kube2 (old)
# find_newcols <- function(kube1, kube2) {
#
#   newcols <- names(kube1 %>%
#                      select(!one_of(as.character(names(kube2)))))
#
#   print_newcols <- ifelse(length(newcols) == 0,
#                           print("no columns in kube1, not found in kube2"),
#                           print(paste0("Newcol detected: ", newcols)))
#   #print(newcols)
#   #print(print_newcols)
#   return(print_newcols)
# }
#
# print_newcols <- find_newcols(kube1 = kube1, kube2 = kube2)
#
# # Rename new cols "_uprikk" by removing "_uprikk" to enable comparisons
# kube1 <- kube1 %>%
#   rename_at(.vars = vars(ends_with("_uprikk")),
#             .funs = ~(sub("_uprikk$", "", .)))
# kube2 <- kube2 %>%
#   rename_at(.vars = vars(ends_with("_uprikk")),
#             .funs = ~(sub("_uprikk$", "", .)))
#
# ## input
# # takes input:
# # exclude_year -> wich year is not found in the oldest file,
# # and cannot be compared,
# # usually in the form of e.g. "2020_2020"
# # kube1, tible containing file1
# # kube2, tible containing file2.
# # (file1/file2 -> character string containing path and filename of file1/2, defined in main script)
# # Identify new cols in kube1, and filtering to include total group (0) only
#
# # "måltall"
# check_cols <- c("TELLER", "NEVNER", "RATE", "SMR", "MEIS", "sumNEVNER", "sumTELLER","RATE.n", "MALTALL", "GINI", "DEKNINGSGRAD", "RATEN","antall", "Crude", "Adjusted", "nevner_aarlig", "teller", "nevner", "rate", "meis", "sumteller", "sumnevner", "crude","adjusted","smr", "SPVFLAGG")
#
# compare_join_NAs <- function(exclude_year, kube1, kube2) {
#
#   compare_cols <- intersect(check_cols, colnames(kube1))
#
#
#   # if (any(colnames(kube1) == "TELLER")) {
#   #   compare_cols <- names(kube1 %>%
#   #                           select(TELLER:last_col()))
#   # }
#   #
#   # if (any(colnames(kube1) == "antall")) {
#   #   compare_cols <- names(kube1 %>%
#   #                           select(antall:last_col()))
#   # }
#   #
#   # # HACK for DODE
#   # if (rev(names(kube1))[1] == "ANTALL") {
#   #   compare_cols <- names(kube1 %>%
#   #                           select(ANTALL))
#   # }
#
#   newcols <- names(kube1 %>%
#                      select(!one_of(as.character(names(kube2)))))
#   expcols <- names(kube2 %>%
#                      select(!one_of(as.character(names(kube1)))))
#
#   standdim <- c("KJONN", "UTDANN", "INNVKAT", "LANDB")
#
#   newcols_standdim <- newcols[newcols %in% standdim]
#   expcols_standdim <- expcols[expcols %in% standdim]
#
#   merge_by_cols <- names(kube1 %>%
#                            select(-all_of(compare_cols), -all_of(newcols)))
#   mutate_cols <- c("TELLER", "sumTELLER", "MEIS", "RATE", "SMR", "antall", "Adjusted", "Crude", "SPVFLAGG", "ANTALL")
#   rest_cols <- compare_cols[!compare_cols %in% mutate_cols]
#
#   join <- left_join(kube1 %>%
#                       filter(!AAR %in% c(exclude_year)) %>%
#                       {if(length(newcols_standdim) > 0) filter_at(., vars(newcols_standdim), all_vars(. == 0)) else .},
#                     kube2 %>%
#                       {if(length(expcols_standdim) > 0) filter_at(., vars(expcols_standdim), all_vars(. == 0)) else .},
#                     by = names(kube1 %>%
#                                  select(all_of(merge_by_cols))))
#
#   if(is.data.frame(join) && nrow(join)==0) {
#     print("JOINING KUBES HAS NOT WORKED PROPERLY")
#   } else {
#     print("Joining kubes completed")
#   }
#
#   # detect and mutate MEIS (and similar) cols
#   # in prioritized order MEIS > Adjusted
#   if(any(grepl("MEIS", colnames(join)))){
#     join <- join %>%
#       mutate(MEIS_diff = round(MEIS.x - MEIS.y, 2),
#              MEIS_FLAG = compareNA_unequal_strict(MEIS.x, MEIS.y))    # OBS! cutoff increased to >= 1!
#   } else if(any(grepl("Adjusted", colnames(join)))){
#     join <- join %>%
#       mutate(Adjusted_diff = round(Adjusted.x - Adjusted.y, 2),
#              Adjusted_FLAG = compareNA_unequal(Adjusted.x, Adjusted.y))
#   } else
#     print("No MEIS or Adjusted cols found")
#
#   # detect and mutate RATE (and similar) cols
#   # in prioritized order MEIS > RATE > Adjusted > Crude
#   if(any(grepl("RATE", colnames(join)))){
#     join <- join %>%
#       mutate(RATE_diff = round(RATE.x - RATE.y, 2),
#              RATE_FLAG = compareNA_unequal(RATE.x, RATE.y))
#   } else if(any(grepl("Crude", colnames(join)))){
#     join <- join %>%
#       mutate(Crude_diff = round(Crude.x - Crude.y, 2),
#              Crude_FLAG = compareNA_unequal(Crude.x, Crude.y))
#   } else
#     print("No RATE or Crude cols found")
#
#   # detect and mutate TELLER (and similar) cOls
#   # in prioritized order TELLER > antall
#   if(any(grepl("TELLER", colnames(join)))){
#     join <- join %>%
#       mutate(TELLER_diff = round(TELLER.x - TELLER.y, 2),
#              TELLER_FLAG = compareNA_unequal_strict(TELLER.x, TELLER.y))
#   }	else if(any(grepl("sumTELLER", colnames(join)))){
#     join <- join %>%
#       mutate(sumTELLER_diff = sumTELLER.x - sumTELLER.y,
#              sumTELLER_FLAG = compareNA_unequal_strict(sumTELLER.x, sumTELLER.y))
#   }	else if(any(grepl("antall", colnames(join)))){
#     join <- join %>%
#       mutate(antall_diff = antall.x - antall.y,
#              antall_FLAG = compareNA_unequal_strict(antall.x, antall.y))
#   }	else if(any(grepl("ANTALL", colnames(join)))){
#     join <- join %>%
#       mutate(antall_diff = ANTALL.x - ANTALL.y,
#              antall_FLAG = compareNA_unequal_strict(ANTALL.x, ANTALL.y))
#   } else
#     print("No TELLER, antall or ANTALL cols found")
#
#   # detect and mutate NEVNER (and similar) cOls
#   # in prioritized order TELLER > antall
#   if(any(colnames(join) %in% c("NEVNER")) & any(colnames(kube1) %in% c("NEVNER")) & any(colnames(kube2) %in% c("NEVNER"))){
#     #  if(any(colnames(join) %in% c("NEVNER"))){
#     join <- join %>%
#       mutate(NEVNER_diff = round(NEVNER.x - NEVNER.y, 2),
#              NEVNER_FLAG = compareNA_unequal_strict(NEVNER.x, NEVNER.y))
#   }	else if(any(colnames(join) %in% c("sumNEVNER")) & any(colnames(kube1) %in% c("sumNEVNER")) & any(colnames(kube2) %in% c("sumNEVNER"))){
#     #else if(any(grepl("sumNEVNER", colnames(join)))){
#     join <- join %>%
#       mutate(sumNEVNER_diff = sumNEVNER.x - sumNEVNER.y,
#              sumNEVNER_FLAG = compareNA_unequal_strict(sumNEVNER.x, sumNEVNER.y))
#   } else
#     print("NEVNER or sumNEVNER cols not found in both kubes")
#
#   # detect and mutate SMR col, if present
#   if(any(grepl("SMR", colnames(join)))){
#     join <- join %>%
#       mutate(SMR_diff = SMR.x - SMR.y,
#              SMR_FLAG = compareNA_unequal_strict(SMR.x, SMR.y))
#   }
#
#   # detect and mutate SVPFLAG col
#   if(any(grepl("SPVFLAGG", colnames(join)))){
#     join <- join %>%
#       mutate(SPVFLAGG_diff = SPVFLAGG.x - SPVFLAGG.y,
#              SPVFLAGG_FLAG = compareNA_unequal(SPVFLAGG.x, SPVFLAGG.y))
#   } else
#     print("No SPVFLAGG col found")
#
#
#   compare <- join %>%
#     select(any_of(merge_by_cols), starts_with(mutate_cols), starts_with(rest_cols))
#
#   return(compare)
# }
#
#
#
# sumdiff <- function(compare) {
#
#   res <- compare %>%
#     summarise_at(vars(matches("_diff|_FLAG")),  sum, na.rm = TRUE)
#
#   return(res)
# }
#
# sumdiff_aar <- function(compare) {
#
#   res <- compare %>%
#     group_by(AAR) %>%
#     summarise_at(vars(matches("_diff|_FLAG")),  sum, na.rm = TRUE)
#
#   return(res)
#
# }
#
#
# # main compare cube function
# # creating compare table with *_diff and *_FLAG cols
# compare_kube <- function() {
#
#   compare <- compare_join_NAs(exclude_year = exclude_year, kube1 = kube1, kube2 = kube2)
#
#   return(compare)
# }
#
# # printing filtered output results from compare
# # writing filtered output results from compare to ..
# # "compare_kube_res/x_vs_y.csv"
# view_write_compare_flagged <- function(compare) {
#
#   ID1 <- file_path_sans_ext(basename(file1))
#   ID2 <- file_path_sans_ext(basename(file2))
#
#   res_dir <- Sys.Date()
#   dir.create(paste0("compare_kube_res/", res_dir), recursive = TRUE, showWarnings = FALSE)
#   res_name <- paste0("compare_kube_res/", res_dir, "/", ID1, "vs", ID2, "_flagged.csv")
#
#   flag_cols <- names(compare %>%
#                        select(ends_with("_FLAG")))
#
#   flag_cols_strict <- names(compare %>%
#                               select(any_of(c("TELLER_FLAG", "antall_FLAG", "SPVFLAGG_FLAG", "MEIS_FLAG"))))
#
#   write <- compare %>%
#     filter_at(., .vars = flag_cols, any_vars(. == 1)) %>%
#     filter_at(., .vars = flag_cols_strict, any_vars(. == 1)) %>%  # extra filtering step, ensuring TELLER-, antall- or SPVFLAG FLAG
#     write_delim(res_name, delim = ";")
#
#   view <- sumdiff(compare)
#   view_aar <- sumdiff_aar(compare)
#
#   return(list(write, view, print(view_aar, n = Inf)))
#
# }
#
# view_write_compare_all <- function(compare) {
#
#   ID1 <- file_path_sans_ext(basename(file1))
#   ID2 <- file_path_sans_ext(basename(file2))
#
#   res_dir <- Sys.Date()
#   dir.create(paste0("compare_kube_res/", res_dir), recursive = TRUE,  showWarnings = FALSE)
#   res_name <- paste0("compare_kube_res/", res_dir, "/", ID1, "vs", ID2, ".csv")
#
#   flag_cols <- names(compare %>%
#                        select(ends_with("_FLAG")))
#
#   flag_cols_strict <- names(compare %>%
#                               select(any_of(c("TELLER_FLAG", "antall_FLAG", "SPVFLAGG_FLAG"))))
#
#   write <- compare %>%
#     write_delim(res_name, delim = ";")
#
#   view <- sumdiff(compare)
#   view_aar <- sumdiff_aar(compare)
#
#   return(list(write, view, print(view_aar, n = Inf)))
#
# }
#
# # write compare (whole file) for debugging purposes
# #write_delim(compare, "compare_kube_res/compare_barnevern.csv", delim = ";")
