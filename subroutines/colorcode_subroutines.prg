
' ##################################################################################################################
' ##################################################################################################################
' ################################################## SUBROUTINES ##################################################
' ##################################################################################################################
' ##################################################################################################################



' ##################################################################################################################

subroutine colorcode(string %sub_tbname, string %sub_rows, string %sub_cols,string %sub_colors, scalar !sub_shades_n, string %sub_by_type, string %sub_absolute_value, string %sub_summary_statistics, string %sub_quantiles, string %sub_keep_table)

' Program variables
' 	-	!sub_order - controls whether low values o best or worst; 1=low values best, 2=high values best

' @ Locations

' 1. Implemeting settings

' Category number
!sub_group_n = @wcount(%sub_colors)
!sub_category_n = !sub_group_n*!sub_shades_n 

' Colors
%sub_color_list = ""

for !gr = 1 to !sub_group_n
	
	%color_code = "" 	

	%base_color = @word(%sub_colors,!gr)

	if @upper(%base_color)="GREEN" then

		%base_ccode = "@rgb(0,128,0)"
		
		!scale_code_min = 78
		!scale_code_max = 255
		!scale_code_step = @floor((!scale_code_max-!scale_code_min)/(!sub_shades_n+1))

		for !sc = 1 to !sub_shades_n

			if !gr = 1 then
				%scale_code =  @str(!scale_code_min+!scale_code_step*(!sc-1))
			else
				%scale_code =  @str(!scale_code_max-!scale_code_step*(!sc-1))
			endif

			%sub_color_list = %sub_color_list + @replace(%base_ccode,"128",%scale_code) + " "
	
		next
	endif

	if @upper(%base_color)="YELLOW" then

		%base_ccode = "@rgb(xxx,255,0)"
		
		!scale_code_min = 50
		!scale_code_max = 255
		!scale_code_step = @floor((!scale_code_max-!scale_code_min)/(!sub_shades_n+1))		

		for !sc = 1 to !sub_shades_n
			if @upper( @word(%sub_colors,1))="GREEN" then
				%scale_code =  @str(!scale_code_max-!scale_code_step*(!sc-1))
			else
				%scale_code =  @str(!scale_code_min+!scale_code_step*(!sc-1))
			endif

			%sub_color_list = %sub_color_list + @replace(@replace(%base_ccode,"255",%scale_code),"xxx","255") + " "	
		next		
	endif

	if @upper(%base_color)="RED" then

		%base_ccode = "@rgb(255,0,0)"
		
		!scale_code_min = 100
		!scale_code_max = 255
		!scale_code_step = @floor((!scale_code_max-!scale_code_min)/(!sub_shades_n+1))		

		for !sc = 1 to !sub_shades_n

			if !gr = 1 then
				%scale_code =  @str(!scale_code_min+!scale_code_step*(!sc-1))
			else
				%scale_code =  @str(!scale_code_max-!scale_code_step*(!sc-1))
			endif

			%sub_color_list = %sub_color_list + @replace(%base_ccode,"255",%scale_code) + " "
	
		next
	endif
next

' Rows and columns
if @instr(@upper(%sub_rows),"-")>0 then
	%sub_row_list = ""
	
	!row_first = @val(@left(%sub_rows,@instr(%sub_rows,"-")-1))
	!row_last = @val(@mid(%sub_rows,@instr(%sub_rows,"-")+1))
 
	for !sub_tr = !row_first to !row_last
		%sub_row_list = %sub_row_list + @str(!sub_tr) + " "
	next
else
	%sub_row_list = %sub_rows
endif

if @instr(@upper(%sub_cols),"-")>0 then
	%sub_col_list = ""
	
	!row_first = @val(@left(%sub_cols,@instr(%sub_cols,"-")-1))
	!row_last = @val(@mid(%sub_cols,@instr(%sub_cols,"-")+1))
 
	for !sub_tr = !row_first to !row_last
		%sub_col_list = %sub_col_list + @str(!sub_tr) + " "
	next
else
	%sub_col_list = %sub_cols
endif

' By-type
!bytype = 0

if @upper(%sub_by_type)="ROWS" then
	!bytype = 1
endif

if @upper(%sub_by_type)="COLS" then
	!bytype = 2
endif

' 2. Color coding
!sumstat_head_created = 0

if !bytype=0 then
	call colorcode_execution(%sub_tbname, %sub_row_list,%sub_col_list, %sub_absolute_value,%sub_quantiles, %sub_summary_statistics, %sub_keep_table)
endif

if !bytype=1 then
	for %sub_row {%sub_row_list}
		call colorcode_execution(%sub_tbname, %sub_row,%sub_col_list, %sub_absolute_value,%sub_quantiles, %sub_summary_statistics, %sub_keep_table)
	next
endif

if !bytype=2 then
	for %sub_col {%sub_col_list}
		call colorcode_execution(%sub_tbname, %sub_row_list,%sub_col, %sub_absolute_value,%sub_quantiles, %sub_summary_statistics, %sub_keep_table)
	next
endif

'delete(noerr)  tb_category_info v_all_values

endsub

' ##################################################################################################################




' ##################################################################################################################

subroutine colorcode_execution(string %sub_tbname, string %sub_rlist,string %sub_clist, string %sub_absolute_value, string %sub_quantiles, string %sub_summary_statistics, string %sub_keep_table)

' 1. Creating vector of all values
!values_n = @wcount(%sub_rlist)*@wcount(%sub_clist)
vector(!values_n) v_all_values = na

!v = 0
for %sub_tr {%sub_rlist}
	for %sub_tc {%sub_clist}

		if @upper(%sub_absolute_value)="T" then
			!value = @abs(@val({%sub_tbname}({%sub_tr},{%sub_tc})))
		else
			!value = @val({%sub_tbname}({%sub_tr},{%sub_tc}))
		endif

		if @isna(!value)=0 then	
			!v = !v + 1 
			v_all_values(!v) = !value
		endif
	next
next

' 2. Creating table with borders and colors

'Creating table
delete(noerr) tb_category_info
table tb_category_info

tb_category_info(1,1) = "Category #"
tb_category_info(1,2) = "Color"
tb_category_info(1,3) = "Lower border"
tb_category_info(1,4) = "Upper border"

' Calculating borders
if @upper(%sub_quantiles)="T" then

	tb_category_info(2,1) = "1"
	tb_category_info(2,2) = @word(%sub_color_list,1)
	tb_category_info(2,3) = @min(v_all_values)-0.0001
	tb_category_info(2,4) = @quantile(v_all_values,1/!sub_category_n)
	
	for !cg = 2 to !sub_category_n-1
		tb_category_info(1+!cg,1) = @str(!cg,"f.0")
		tb_category_info(1+!cg,2) = @word(%sub_color_list,!cg)
		tb_category_info(1+!cg,3) = @quantile(v_all_values,(!cg-1)/!sub_category_n)
		tb_category_info(1+!cg,4) = @quantile(v_all_values,(!cg)/!sub_category_n)
	next
	
	tb_category_info(1+!sub_category_n,1) = @str(!sub_category_n,"f.0")
	tb_category_info(1+!sub_category_n,2) = @word(%sub_color_list,!sub_category_n)
	tb_category_info(1+!sub_category_n,3) = tb_category_info(1+!sub_category_n-1,4)
	tb_category_info(1+!sub_category_n,4) = @max(v_all_values)+0.0001

else

	!category_min = @min(v_all_values)
	!category_width = (@max(v_all_values)-@min(v_all_values))/!sub_category_n

	tb_category_info(2,1) = "1"
	tb_category_info(2,2) = @word(%sub_color_list,1)
	tb_category_info(2,3) = @min(v_all_values)-0.0001
	tb_category_info(2,4) = @min(v_all_values)+!category_width
	
	for !cg = 2 to !sub_category_n-1
		tb_category_info(1+!cg,1) = @str(!cg,"f.0")
		tb_category_info(1+!cg,2) = @word(%sub_color_list,!cg)
		tb_category_info(1+!cg,3) = !category_min+!category_width*(!cg-1)
		tb_category_info(1+!cg,4) = !category_min+!category_width*(!cg)
	next
	
	tb_category_info(1+!sub_category_n,1) = @str(!sub_category_n,"f.0")
	tb_category_info(1+!sub_category_n,2) = @word(%sub_color_list,!sub_category_n)
	tb_category_info(1+!sub_category_n,3) = tb_category_info(1+!sub_category_n-1,4)
	tb_category_info(1+!sub_category_n,4) = @max(v_all_values)+0.0001


endif

for !cg = 1 to !sub_category_n-1
	if @instr(tb_category_info(1+!cg,4),"NA")>0 then
		tb_category_info(1+!cg,4) = tb_category_info(1+!cg,3)
		tb_category_info(1+!cg+1,3) = tb_category_info(1+!cg,4)
	endif
next

' 3. Color-coding
for %sub_tr {%sub_rlist}
	for %sub_tc {%sub_clist}

		if @upper(%sub_absolute_value)="T" then
			!value = @abs(@val({%sub_tbname}({%sub_tr},{%sub_tc})))
		else
			!value = @val({%sub_tbname}({%sub_tr},{%sub_tc}))
		endif

		for !cg = 1 to !sub_category_n
			%lb = tb_category_info(1+!cg,3)
			%ub = tb_category_info(1+!cg,4)
			%color = tb_category_info(1+!cg,2)

			if !value>{%lb} and !value<={%ub} then
				{%sub_tbname}.setfillcolor({%sub_tr},{%sub_tc}) {%color}
				exitloop
			endif
		next
	next
next

' 4. Adding summary_statistics

if @wcount(%sub_summary_statistics)>0 then


	if !bytype = 2 and !sumstat_head_created = 0 then

		!sumstat_head_created = 1

		!sub_last_row = {%sub_tbname}.@rows
		!sub_last_col = {%sub_tbname}.@cols
		
		!sub_next_row = !sub_last_row+1
		{%sub_tbname}.setlines(!sub_next_row,1,!sub_next_row,!sub_last_col) +d

		!sub_next_row = !sub_next_row+1
		{%sub_tbname}.setmerge(!sub_next_row,1,!sub_next_row,!sub_last_col) 
		{%sub_tbname}.setfont(!sub_next_row,1,!sub_next_row,!sub_last_col) +b
		{%sub_tbname}.setlines(!sub_next_row,1,!sub_next_row,!sub_last_col) +b
		{%sub_tbname}(!sub_next_row,1) = "Summary statistics"	

		!sub_summary_end = !sub_last_row+2+@wcount(%sub_summary_statistics)+1
		
		if @instr(@upper(%sub_summary_statistics),"QUARTILES") then
			!sub_summary_end = 	!sub_summary_end +1
		endif

		{%sub_tbname}.setlines(!sub_summary_end,1,!sub_summary_end,!sub_last_col) +d
	
	endif		

	!sub_next_row = !sub_last_row+2
	
	for %sub_sumstat {%sub_summary_statistics}

		if @upper(%sub_sumstat)="MEAN" then 
	
			!sub_next_row = !sub_next_row+1
	
			!sub_mean = @mean(v_all_values)
			{%sub_tbname}(!sub_next_row,1) = "Mean"
			{%sub_tbname}.setfont(!sub_next_row,1) +b
			{%sub_tbname}(!sub_next_row,{%sub_clist}) = @str(!sub_mean,"f.2")
		endif
	
		if @upper(%sub_sumstat)="STDEV" then 
	
			!sub_next_row = !sub_next_row+1
	
			!sub_stdev = @stdev(v_all_values)
			{%sub_tbname}(!sub_next_row,1) = "Std. Deviation"
			{%sub_tbname}.setfont(!sub_next_row,1) +b
			{%sub_tbname}(!sub_next_row,{%sub_clist}) = @str(!sub_stdev,"f.2")
		endif

		if @upper(%sub_sumstat)="MIN" then 
	
			!sub_next_row = !sub_next_row+1
	
			!sub_min = @min(v_all_values)
			{%sub_tbname}(!sub_next_row,1) = "Minimum"
			{%sub_tbname}.setfont(!sub_next_row,1) +b
			{%sub_tbname}(!sub_next_row,{%sub_clist}) = @str(!sub_min,"f.2")
		endif

		if @upper(%sub_sumstat)="MAX" then 
	
			!sub_next_row = !sub_next_row+1
	
			!sub_max = @max(v_all_values)
			{%sub_tbname}(!sub_next_row,1) = "Maximum"
			{%sub_tbname}.setfont(!sub_next_row,1) +b
			{%sub_tbname}(!sub_next_row,{%sub_clist}) = @str(!sub_max,"f.2")
		endif


		if @upper(%sub_sumstat)="QUARTILES" then 
	
			!sub_next_row = !sub_next_row+1
	
			!sub_quartile = @quantile(v_all_values,0.25)
			{%sub_tbname}(!sub_next_row,1) = "1st quartile"
			{%sub_tbname}.setfont(!sub_next_row,1) +b
			{%sub_tbname}(!sub_next_row,{%sub_clist}) = @str(!sub_quartile,"f.2")


			!sub_next_row = !sub_next_row+1
	
			!sub_quartile = @quantile(v_all_values,0.75)
			{%sub_tbname}(!sub_next_row,1) = "3rd quartile"
			{%sub_tbname}.setfont(!sub_next_row,1) +b
			{%sub_tbname}(!sub_next_row,{%sub_clist}) = @str(!sub_quartile,"f.2")
		endif

	next


'{%sub_tbname}.display
'stop
endif

delete(noerr) v_all_values

if @upper(%sub_keep_table)="F" then
	delete(noerr) tb_category_info
endif

endsub

' ##################################################################################################################

