# ESPN has changed the logos for MLB teams, offering a smaller variety
# of logo options, mostly utilizing tertiary logos that the teams
# themselves don't use.
#
# So this script was made to recycle older logo versions for
# continued use


# Only saving the internal ESPN logos that we kept their info, so not the svgs
# from MLB.com


old_logo_list <- mlbplotR:::logo_list

old_scoreboard_logo_list <- mlbplotR:::scoreboard_logo_list


old_lists <- list(old_logo_list, old_scoreboard_logo_list)


saveRDS(old_lists, "./data-raw/old_data/old_logo_lists.rds")
