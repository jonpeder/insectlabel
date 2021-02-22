#test_that("test insectlabel output", {
#  example_input <- data.frame(
#                col_ID = c("JPL_001", "JPL_002", "JPL_003"),
#                country = c("Norway,", "Norway,", "Norway,"),
#                county = c("Nordland,", "Troms,", "Troms,"),
#                municipality = c("Grane:", "Tromsø:", "Målselv:"),
#                locality1 =c("Auster Vefsna", "Tønsvik", "Dividalen"),
#                locality2 = c("Stilleelva", "Tverrelva", "Kvennelva"),
#                lat = c(65.532188, 69.736644, 68.792052),
#                lon = c(13.728252, 19.178893, 19.695974),
#                habitat = c("River-shore", "River-shore", "River-shore"),
#                method = c("Sweep-net:", "Sweep-net:", "Sweep-net:"),
#                date = c("2018-07-29", "2018-08-01", "2018-08-10"),
#                leg = c("Leg. J. Kjærandsen", "Leg. J.P.Lindemann", "Leg. J.P.Lindemann")
#                )
# insectlabel(example_input)
