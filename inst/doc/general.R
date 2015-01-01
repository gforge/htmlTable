## ----, results='asis'----------------------------------------------------
library(htmlTable)
# A simple output
output <- matrix(1:4,
                 ncol=2,
                 dimnames = list(list("Row 1", "Row 2"),
                                 list("Column 1", "Column 2")))
htmlTable(output)

## ----, results='asis', echo=FALSE----------------------------------------
cat("```{r, results='asis'}
   htmlTable(output)
```")

## ------------------------------------------------------------------------
mx <-
  matrix(ncol=6, nrow=8)
rownames(mx) <- paste(c("1st", "2nd",
                        "3rd",
                        paste0(4:8, "th")),
                      "row")
colnames(mx) <- paste(c("1st", "2nd",
                        "3rd", 
                        paste0(4:6, "th")),
                      "hdr")

for (nr in 1:nrow(mx)){
  for (nc in 1:ncol(mx)){
    mx[nr, nc] <-
      paste0(nr, ":", nc)
  }
}

## ----, results='asis'----------------------------------------------------
htmlTable(mx, 
          rgroup = paste("Group", LETTERS[1:3]),
          n.rgroup = c(2,4,nrow(mx) - 6))


## ----, results='asis'----------------------------------------------------
htmlTable(mx, 
          rgroup = c(paste("Group", LETTERS[1:2]), ""),
          n.rgroup = c(2,4,nrow(mx) - 6))

## ----, results='asis'----------------------------------------------------
htmlTable(mx, 
          css.rgroup = "",
          rgroup = c(paste("Group", LETTERS[1:2]), ""),
          n.rgroup = c(2,4,nrow(mx) - 6))

## ----, results='asis'----------------------------------------------------
htmlTable(mx,
          cgroup = c("Cgroup 1", "Cgroup 2"),
          n.cgroup = c(2,4))

## ----, results='asis'----------------------------------------------------
htmlTable(mx,
          cgroup = rbind(c("", "Column spanners", NA),
                         c("", "Cgroup 1", "Cgroup 2")),
          n.cgroup = rbind(c(1,2,NA),
                           c(2,2,2)))

## ----, results='asis'----------------------------------------------------
htmlTable(mx,
          cgroup = rbind(c("", "Column spanners", NA),
                         c("", "Cgroup 1", "Cgroup 2")),
          n.cgroup = rbind(c(1,5,NA),
                           c(2,1,3)))

## ----, results='asis'----------------------------------------------------
htmlTable(mx, 
          tspanner = paste("Spanner", LETTERS[1:3]),
          n.tspanner = c(2,4,nrow(mx) - 6))

## ----, results='asis'----------------------------------------------------
htmlTable(mx[1:2,1:2], 
          caption="A table caption above")

## ----, results='asis'----------------------------------------------------
htmlTable(mx[1:2,1:2], 
          pos.caption = "bottom",
          caption="A table caption below")

## ------------------------------------------------------------------------
options(table_counter = TRUE)

## ----, results='asis'----------------------------------------------------
htmlTable(mx[1:2,1:2], 
          caption="A table caption with a numbering")

## ------------------------------------------------------------------------
tblNoLast()
tblNoNext()

## ----, results='asis'----------------------------------------------------
htmlTable(mx[1:2,1:2], 
          tfoot="A table footer")

## ----, results='asis'----------------------------------------------------
htmlTable(mx, 
          col.rgroup = c("none", "#F7F7F7"))

## ----, results='asis'----------------------------------------------------
htmlTable(mx, 
          col.rgroup = c("none", "#F7F7F7"),
          rgroup = c(paste("Group", LETTERS[1:2]), ""),
          n.rgroup = c(2,2,nrow(mx) - 4))

## ----, results='asis'----------------------------------------------------
htmlTable(mx, 
          col.columns = c("none", "#F7F7F7"))

## ----, results='asis'----------------------------------------------------
htmlTable(mx, 
          col.rgroup = c("none", "#F9FAF0"),
          col.columns = c("none", "#F1F0FA"))

## ----, results='asis'----------------------------------------------------
htmlTable(mx, 
          align="r",
          rgroup = paste("Group", LETTERS[1:3]),
          n.rgroup = c(2,4,nrow(mx) - 6),
          cgroup = rbind(c("", "Column spanners", NA),
                         c("", "Cgroup 1", "Cgroup 2&dagger;")),
          n.cgroup = rbind(c(1,2,NA),
                           c(2,2,2)),
          caption="A table with column spanners, row groups, and zebra striping",
          tfoot="&dagger; A table footer commment",
          cspan.rgroup = 2,
          col.columns = c(rep("none", 2),
                          rep("#F5FBFF", 4)),
          col.rgroup = c("none", "#F7F7F7"),
          css.cell = "padding-left: .5em; padding-right: .2em;")


