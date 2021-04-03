' ##################################################################################################################
' ##################################################################################################################
' ###################################################### SETTINGS ###################################################
' ##################################################################################################################
' ##################################################################################################################

include .\subroutines\colorcode_subroutines.prg 

' 1. Detemine if  this was run from the GUI
!dogui=1
if @len(@option(1))>0 then
	!dogui=@hasoption("prompt") 'if this is 0, we are NOT running through the GUI
endif

' 2. Specifyin settings

if !dogui=1 then

	%cc_tb_name = _this.@name


	'2.1 Execute user dialog
	%cc_rows = "1-"+ @str({%cc_tb_name}.@rows)
	%cc_cols = "1-"+ @str({%cc_tb_name}.@cols)

	%cc_colors = "green yellow red"
	%cc_shades_n = @str(@ceiling({%cc_tb_name}.@rows/12),"f.2")
	
	!bytype = 1

	!abs = 0

	!metric = 1

	!keep_table = 0

	!result = @uidialog("caption",	"Color code table settings", "edit",%cc_rows,"Enter list of rows to color code", "edit",%cc_cols,"Enter list of columns to color code","edit",%cc_colors,"Enter color pallete","edit",%cc_shades_n,"Enter number of color shades","radio",!bytype,"How do you with to separate values?","""All values in selection together"" ""By row"" ""By column""","check",!abs,"Use absolute value of values","radio",!metric,"On which metric do you want to base the color coding?","Distance Quantiles(ranks)","check",!keep_table,"Do you want to keep table with category information?") 

	if !result = -1 then 'will stop the program unless OK is selected in GUI
		stop
	endif


	!cc_shades_n = {%cc_shades_n}

	%cc_bytype = @word(""""" rows cols",!bytype)

	if !abs = 1 then
		%cc_absolute_value = "T"
	else
		%cc_absolute_value = "F"
	endif


	if !metric = 1 then
		%cc_quantiles = "F"
	else
		%cc_quantiles= "T"
	endif


	if !keep_table = 1 then
		%cc_keep_table = "T"
	else
		%cc_keep_table = "F"
	endif
	
else

	'2.2 Load settings from options
	%cc_tb_name = _this.@name
	%cc_rows =  @equaloption("ROWS")	
	%cc_cols =  @equaloption("COLS")	

	if @isempty(%cc_rows) then
		%cc_rows = "1-" + @str({%cc_tb_name}.@rows)
	endif

	if @isempty(%cc_cols) then
		%cc_cols = "1-" + @str({%cc_tb_name}.@cols)
	endif

	%cc_colors = @equaloption("COLORS")	
	
	if @isempty(%cc_colors) then
		%cc_colors = "green yellow red"
	endif

	%cc_shades_n = @equaloption("SHADES")

	if @isempty(%cc_shades_n) then
		!cc_shades_n = @floor({%cc_tb_name}.@rows/(3*2))
	else
		!cc_shades_n = {%cc_shades_n}
	endif

	%cc_bytype = @equaloption("BYTYPE")	
	%cc_absolute_value = @equaloption("ABS")	

	if @upper(%cc_absolute_value)<>"T" then
		%cc_absolute_value = "F"
	endif

	%cc_quantiles = @equaloption("QUANTILES")	

	%cc_summary_statistics = @equaloption("STATISTICS")	

	if @isempty(%cc_quantiles) then
		%cc_quantiles = "f"
	endif

	%cc_keep_table = @equaloption("KEEP_TABLE")	
	
	if @isempty(%cc_keep_table) then
		%cc_keep_table = "f"
	endif


endif

' ##################################################################################################################
' ##################################################################################################################
' #################################################### EXECUTION ###################################################
' ##################################################################################################################
' ##################################################################################################################

call colorcode(%cc_tb_name,%cc_rows,%cc_cols,%cc_colors,!cc_shades_n,%cc_bytype,%cc_absolute_value,%cc_summary_statistics, %cc_quantiles,%cc_keep_table)


