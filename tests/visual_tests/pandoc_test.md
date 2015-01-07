# Pandoc test


```r
library(htmlTable)
mx <- matrix(1, ncol=1)
colnames(mx) <- c("A")
rownames(mx) <- letters[1]
htmlTable(mx)
```

<table class='gmisc_table' style='border-collapse: collapse;' >
<thead>
<tr>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey;'> </th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>A</th>
</tr>
</thead>
<tbody>
<tr>
<td style='border-bottom: 2px solid grey; text-align: left;'>a</td>
<td style='border-bottom: 2px solid grey; text-align: center;'>1</td>
</tr>
</tbody>
</table>

