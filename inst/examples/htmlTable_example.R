output <-
  matrix(paste("Content", LETTERS[1:16]),
         ncol=4, byrow = TRUE)

htmlTable(output,
          header =  paste(c("1st", "2nd",
                            "3rd", "4th"), "header"),
          rnames = paste(c("1st", "2nd",
                           "3rd", "4th"), "row"),
          rgroup = c("Group A",
                     "Group B"),
          n.rgroup = c(2,2),
          cgroup = c("Cgroup 1", "Cgroup 2&dagger;"),
          n.cgroup = c(2,2),
          caption="Basic table with both column spanners (groups) and row groups",
          tfoot="&dagger; A table footer commment")

# See vignette("tables", package = "htmlTable")
# for more examples