# Pandoc test v1.13 bug

# Correct table
<table class='gmisc_table' style='border-collapse: collapse;' >
<thead>
<tr>
<th> </th>
<th>A</th>
</tr>
</thead>
<tbody>
<tr>
<td style='border-bottom: 2px solid grey; text-align: left;'>a</td>
<td style='border-bottom: 2px solid grey; text-align: center;'>1</td>
</tr>
</tbody>
</table>

# Faulty table

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


