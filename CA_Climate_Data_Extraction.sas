/* 
* Created by Winnie Kwok

	This Program is used to download Canada Climate Daily Data from :
	https://climate.weather.gc.ca/historical_data/search_historic_data_e.html
	
	More details about the definition of each variable name, please refer to:
	https://climate.weather.gc.ca/glossary_e.html#d
	
Last Update Date: Mar 11, 2023
------------------------------------------------------------------------------------------------------
Update: 

	
*/











*******************************************************************************************************;
*
*
*
*
*
*						Preparation
*
*
*
*
*
*******************************************************************************************************;


/*====================================================================================================*/
/*
/*						Macro
/*
/*====================================================================================================*/


/*____________________________________________________________________________________________________*/
/*
/*						Define Macro Variable
/*
/*____________________________________________________________________________________________________*/

/*----------------------------------------------------------------------------------------------------*/
/*						Date
/*----------------------------------------------------------------------------------------------------*/
/* %let t = %sysfunc(today()); */
/* %let yr = %sysfunc(year(&t.)); */

data _null_; 
t = today();
t_1 = intnx('day',t,-1);
yr = year(t);
time = compress(put(time(),time.),,'kd');

call symputx('t',put(t,yymmddn8.));
call symputx('t_1',put(t_1,yymmddn8.));
call symputx('yr',yr);
call symputx('time',time);
run;
%put NOTE: Today : &t. ,  Yesterday : &t_1. , This Year : &yr. ,  Current Time : &time.;

%let firstyr = 2005;

/*----------------------------------------------------------------------------------------------------*/
/*						Data Extraction Information
/*----------------------------------------------------------------------------------------------------*/

%let datafirstyr_flag = dly_first_year;
%let datalastyr_flag = dly_last_year;

%let climate = https://climate.weather.gc.ca/;

%let climate_dly_path_search = &climate.climate_data/daily_data_e.html;
%let climate_his_path = &climate.historical_data/;
%let climate_his_path_search = search_historic_data_e.html;
%let climate_his_path_search_station = search_historic_data_stations_e.html;

%let climate_his = &climate_his_path.&climate_his_path_search;
%let climate_his_search = &climate_his_path.&climate_his_path_search_station.;

/*----------------------------------------------------------------------------------------------------*/
/*						Working Path
/*----------------------------------------------------------------------------------------------------*/
%let path = ~/Portfolio;


/*____________________________________________________________________________________________________*/
/*
/*						Macro Program
/*
/*____________________________________________________________________________________________________*/
%include "&path/Macro Program.sas";





/*====================================================================================================*/
/*
/*						Library
/*
/*====================================================================================================*/
libname out "&path"; 
%let outpath = %sysfunc(pathname(out));











*******************************************************************************************************;
*
*
*
*
*
*						Web-Scraping
*
*
*
*
*
*******************************************************************************************************;





/*====================================================================================================*/
/*
/*						Climate Station Inventory
/*
/*====================================================================================================*/


/*____________________________________________________________________________________________________*/
/*
/*						1. Preparation
/*	Extract Climate Station Inventory list
/*
/*____________________________________________________________________________________________________*/

/*----------------------------------------------------------------------------------------------------*/
/*						Find "Climate Station Inventory" path & file name
/*----------------------------------------------------------------------------------------------------*/
%url_content(&climate.);

*	Get "Get More Data" path
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
data climate_moredata; set content;

if find(line,'get more data','i');

moredata_path = compress(scan(line,2,'"'));

call symputx('moredata_path',moredata_path);

run;

%url_content(&moredata_path.);

*	Get "Station Inventory file" path
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
%split(	  intable 			= content
		, outtable 			= text
		, split_var			= line
		, split_dlm			= <
		, var_after_split	= text
		);
		
data climate_stationfile; set text;

if find(text,'station inventory','i');

filename = scan(text,2,'"');

filepath = cats("&moredata_path.",filename);

call symputx('moredata_filename',filename);
call symputx('moredata_filepath',filepath);

run;


/*----------------------------------------------------------------------------------------------------*/
/*						Download "Station Inventory" CSV file 
/*----------------------------------------------------------------------------------------------------*/
filename out "&outpath./&moredata_filename.";
proc http url = "&moredata_filepath." out = out method = get;run;


/*____________________________________________________________________________________________________*/
/*
/*						2. Extraction
/*	Import "Station Inventory" CSV file 
/*
/*____________________________________________________________________________________________________*/
/* proc import datafile = "&path./Station Inventory EN.csv" out = Climate_Station dbms = csv replace; datarow = 4; getnames = no; guessingrows = max; run; */
proc import datafile = "&path./&moredata_filename." out = Climate_Station dbms = csv replace; datarow = 4; getnames = no; guessingrows = max; run;


/*____________________________________________________________________________________________________*/
/*
/*						3. Tidy Up
/*	Convert to Valid Variable
/*
/*____________________________________________________________________________________________________*/

/*----------------------------------------------------------------------------------------------------*/
/*						Get Variable List & Replace Space by _
/*----------------------------------------------------------------------------------------------------*/
proc contents data = climate_station out = _clim_stat_var;run;
data _clim_stat_var_rename ; set climate_station (obs = 1);  run;
proc transpose data = _clim_stat_var_rename out = _clim_stat_var_rename;var _all_;run; 
data _clim_stat_var_rename; set _clim_stat_var_rename; length new_var $32.;
new_var = compress(tranwrd(strip(col1),' ','_'),'_','akkdi');
run;

data _null_; set _clim_stat_var_rename;
call symputx('org_var'||strip(_n_),_name_);
call symputx('new_var'||strip(_n_),new_var);
call symputx('total_rename',_n_);
run;

/*----------------------------------------------------------------------------------------------------*/
/*						Rename Variable's Name
/*----------------------------------------------------------------------------------------------------*/
%printmsg(msg = [ Climate Station : Rename Variable Name ] ,printalllog=);
%macro rename();
data Climate_Station; set Climate_Station (firstobs = 2) ;

	%do i = 1 %to &total_rename.;
	
		rename &&org_var&i. = &&new_var&i.;
		
	%end;

run;
%mend;
%rename;
%printmsg(msg= , printalllog=Y);
%deldataset(lib = work,prefix = _);

/*----------------------------------------------------------------------------------------------------*/
/*						Output Climate Station Inventory
/*----------------------------------------------------------------------------------------------------*/
%SQUEEZE( DSNIN = Climate_Station, DSNOUT =out.Climate_Station, drop_in = ,RENAME_OUT_AS_IN =) ;






/*====================================================================================================*/
/*
/*						Climate Figure
/*
/*====================================================================================================*/


/*____________________________________________________________________________________________________*/
/*
/*						1. Preparation
/*						Get available Provinces for Extraction
/*
/*____________________________________________________________________________________________________*/
%url_content(&climate_his.);

%split(	  intable 			= content
		, outtable 			= text
		, split_var			= line
		, split_dlm			= >
		, var_after_split	= text
		);
			
/*----------------------------------------------------------------------------------------------------*/
/*						Prepare Province List 
/*----------------------------------------------------------------------------------------------------*/
data ck_prov 		(keep = prov_short province )
; 

set text ; n=1;

	if find(text, "option value=", "i");
	
*	Province Extraction
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
	prov_patternid = prxparse('/="\D\D">\D/');
	prov_position = prxmatch(prov_patternid,line);	

	if 		prov_position >0 
		then do;
		
*	Long & Short name of Province Extraction
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
			prov_short = substr(line,prov_position+2,2);
			province = upcase(scan(scan(line,2,'>'),1,'<'));
			
			output ck_prov;
		
		end;

run;

/*----------------------------------------------------------------------------------------------------*/
/*						Map Province short name to Climate Station table
/*----------------------------------------------------------------------------------------------------*/
%SQUEEZE( DSNIN = ck_prov, DSNOUT = ck_prov1, drop_in = Y,RENAME_OUT_AS_IN =Y ) ;

proc sql;
create table climate_station as 
select distinct a.*, b.prov_short
from climate_station a left join ck_prov b on  (		scan(a.province,1,'') = scan(b.province,1,'') 
													or 	substr(a.province,1,length(a.province)) = substr(b.province,1,length(a.province))
													or 	substr(a.province,1,length(b.province)) = substr(b.province,1,length(b.province))
														
												)
;quit;

/*----------------------------------------------------------------------------------------------------*/
/*						Output Climate Station Inventory
/*----------------------------------------------------------------------------------------------------*/
%SQUEEZE( DSNIN = Climate_Station, DSNOUT =out.Climate_Station, drop_in = ,RENAME_OUT_AS_IN =) ;

/*----------------------------------------------------------------------------------------------------*/
/*						Select Specific Year Period ONLY
/*----------------------------------------------------------------------------------------------------*/
data climate_station; set climate_station;
if &datafirstyr_flag. >= &firstyr. and &datalastyr_flag. <=&yr.;

do Year = &datafirstyr_flag. to &datalastyr_flag.;	

	output; 
	
	do Month = 1 to 11;
		
		output;
	
	end;

end; 

run;

*	Remove Invalid Month and Year
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
data climate_station; set climate_station; if month not in (.,13) and Year <= &datalastyr_flag.;run;





/*____________________________________________________________________________________________________*/
/*
/*						2. Extraction
/*						Extract Daily Climate by Province
/*
/*____________________________________________________________________________________________________*/

/*----------------------------------------------------------------------------------------------------*/
/*						Checking :
/*	1. Number of records for extraction
/*	2. backup dataset (with org_ prefix) whether exist
/*		--> if yes, keep the largest backup dataset of each day ( days beforrogram run day )
/*	3. Climate_data dataset whether exist
/*		--> if yes, check the number of records & compare with backup 
/*					>> Replaced by backup if the number of records is less than backup
/*					>> Saved a backup if the number of records is more than backup
/*		--> if no, create an empty Climate_Data
/*----------------------------------------------------------------------------------------------------*/
%macro ck_download_dly_climate_by_prov(outlib, outtable);

*	Check: the number of records in Climate_data
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
%ckdataset(lib = &outlib., tablename = &outtable., tablenamelist = , obsnum = Y);
%printmsg(msg = , printalllog = , closealllog = Y);

%let output = &outlib..&outtable.;

*	Check: whether the backup exists & the number of records of each backup
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
%ckdataset(lib = &outlib. , tablename = org_&outtable. , tablenamelist = Y , obsnum = Y, noprint = );
%let org = &ckdatasetexist.;
%let orgobs = &ckdatasetobsnum.;

%printmsg(msg =  &outlib..org_&outtable. contains &orgobs. records   , printalllog = , closealllog = );
%printmsg(msg = , printalllog = , closealllog = Y);
*	Check: keep the largest backup dataset of each day ( days before program run day )
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
%if %length(&org.) > 0  %then %do;

	data ckorgdataset; set ckdataset;run;

/* Extract the date & time of each backup dataset */
	data ckorgdataset; set ckorgdataset;
	date = scan(tablename,-2,'_');
	time = scan(tablename,-1,'_');
	
	if compress(date,,'kd') ^='';
	
/* Focus on day before Program run day dataset ONLY */
	if date < &t. ;

	a = 1;
	run;
	
	proc sort data = ckorgdataset nodupkey tagsort; by a date obs time tablename;run;
	
/* Keep the largest backup dataset */
	data ckorgdataset1; set ckorgdataset;
	by a date;
	if last.date = 0;
	run;
	
/* Check any backup dataset need to delete */
	%ckdataset(lib = work , tablename = ckorgdataset1 , tablenamelist =  , obsnum = Y, noprint = Y);
	
/* Delete all non-largest backup datasets */
	%if %length(&xobs.) =0 %then %do;
	
		data _null_; set ckorgdataset1; 
		call symputx('delds_lib'||strip(_n_), libname); 
		call symputx('delds'||strip(_N_),tablename);
		call symputx('total_del',_n_);
		run;
	
		%do i = 1 %to &total_del.;
		
			%let lib = &&delds_lib&i.;
			%let ds = &&delds&i.;
			%deldataset(lib = &lib. ,prefix = &ds. , noprint = Y);
		
		%end;
		%deldataset(lib = work ,prefix = ckorgdataset1, noprint = Y );
	
	%end;
		
%end;

*	Check: whether the Climate_data exists & the number of records
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
%ckdataset(lib = &outlib. , tablename = &outtable. , tablenamelist = , obsnum = Y, noprint = Y);
%let new = &ckdatasetexist.;
%let newobs = &ckdatasetobsnum.;
%printmsg(msg =  &outlib..&outtable. contains &newobs. records   , printalllog = , closealllog = );

*	Replaced Climate_data by backup if the number of records is less than backup
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
%if %length(&org.) > 0  %then %do;

	%if &orgobs. > &newobs. or %length(new) = 0 %then %do;
		%deldataset(lib = &outlib. ,prefix = &outtable. , noprint = Y);
		data &output.; set &outlib..org_&outtable.;run;
%printmsg(msg =  &outlib..org_&outtable. renamed as &output.  , printalllog = , closealllog = );

	
	%end;
	%else %do;
	
*	Saved a backup ( one with date & time suffix and one without ) if the number of records is more than backup
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;	
		data &outlib..org_&outtable.; set &output.;run;
		data &outlib..org_&outtable._&t._&time.; set &output.;run;
%printmsg(msg =  &output. saved as &outlib..org_&outtable. and &outlib..org_&outtable._&t._&time.  , printalllog = , closealllog = );
	

	%end;

*	Select StationID, Year and Month that not exist in Climate_Data
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
%printmsg(msg = , printalllog = , closealllog = Y);

	proc sql;
	create table climate_station1 as 
	select distinct * from climate_station
	where cats(station_id,year,month) not in (select distinct cats(stationid,year,month) from &output.)
	;quit;

%end;

%else %do;

	%if %length(&new.) >0 %then %do;
	
*	Select StationID, Year and Month that not exist in Climate_Data
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
%printmsg(msg = , printalllog = , closealllog = Y);

		proc sql;
		create table climate_station1 as 
		select distinct * from climate_station
		where cats(station_id,year,month) not in (select distinct cats(stationid,year,month) from &output.)
		;quit;

*	Saved a backup ( one with date & time suffix and one without ) if the number of records is more than backup
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;	
		data &outlib..org_&outtable.; set &output.;run;
		data &outlib..org_&outtable._&t._&time.; set &output.;run;
%printmsg(msg =  &output. saved as &outlib..org_&outtable. and &outlib..org_&outtable._&t._&time.  , printalllog = , closealllog = );

	%end;
	%else %do;

*	Create an empty Climate_data if it is not exist / contains 0 record
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
		data climate_station1; set climate_station;run;
		data &output.;run;
%printmsg(msg =  &output. Created!  , printalllog = , closealllog = );

	%end;
	
%end;
%printmsg(msg =    , printalllog = Y, closealllog = );

%mend;
options nosymbolgen noserror msglevel = i replace;
%ck_download_dly_climate_by_prov(outlib = out, outtable = Climate_data);
options symbolgen serror;

/*----------------------------------------------------------------------------------------------------*/
/*						Set Station id as Macro Variable
/*----------------------------------------------------------------------------------------------------*/
data _null_; set climate_station1;
call symputx('stationid'||strip(_n_),station_id);
call symputx('climateid'||strip(_n_),climate_id);
call symputx('station_name'||strip(_n_),compress(Name,,'p'));
call symputx('station_year'||strip(_n_),year);
call symputx('station_month'||strip(_n_),month);
call symputx('station_startyr'||strip(_n_),first_year);
call symputx('station_endyr'||strip(_n_),last_year);
call symputx('prov_short'||strip(_n_),prov_short);
call symputx('province'||strip(_n_),province);
call symputx('total_stationrecord',_n_);
run;

%printmsg(msg =  Total &total_stationrecord. record(s) will be extracted   , printalllog = , closealllog = );

/*----------------------------------------------------------------------------------------------------*/
/*						Download Data
/*----------------------------------------------------------------------------------------------------*/
%macro download_dly_climate_by_prov(outlib, outtable);
%let output = &outlib..&outtable.;

%printmsg(msg = [ Download Province Climate Data : Start! ] , printalllog = , closealllog = );

%do i = 1 %to &total_stationrecord.;

%loopstatus(total = &total_stationrecord. , inputnumber = &i. , msg = Download Province Climate Data : );
	
*	Define Macro Variables
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
	%let climate_stationid 		= &&stationid&i.;
	%let climate_id				= &&climateid&i.;
	%let provshort 				= &&prov_short&i.;
	%let province 				= &&province&i.;
	%let y 						= &&station_year&i.;
	%let m						= &&station_month&i.;
	%let startyr 				= &&station_startyr&i.;
	%let endyr 					= &&station_endyr&i.;
	%let climate_stationname 	= &&station_name&i.;

*	Prepare Data Path
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
	data climate_path; length path $32767.;
				
	start_path = "&climate_dly_path_search.?hlyRange=%7C";
	dlyrange = "dlyRange=&startyr.-01-01%7C&endyr.-12-31";
	mlyrange = "mlyRange=&startyr.-01-01%7C&endyr.-12-31";
	stationid = "StationID=&climate_stationid.";
	prov = "Prov=&provshort.";
	url = "urlExtension=_e.html";
	searchtype = "searchType=stnProv";
	start_yr = "StartYear=&startyr.";
	end_yr = "EndYear=&endyr.";
	rowpage = "selRowPerPage=100";
	line = "Line=0";
	month = "Month=&m.";
	day = "Day=31";
	firstprov = "lstProvince=";
	timeframe = "timeframe=2";
	year = "Year=&y.";
	
	path = compress(tranwrd(catx('$',start_path,dlyrange,mlyrange,stationid,prov,url,searchtype,start_yr,end_yr,rowpage,line,month,day,firstprov,timegrame,year),'$',"%str(&)"));
	run;
	
	data _null_; set climate_path;
	call symputx('climate_path',compress(path));
	run;
/* %put &climate_path.; */
*	Get the content of Data Path 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
	%url_content(&climate_path., noprint = Y);
			
%printmsg(msg = , printalllog = , closealllog = Y );

	%split(	  intable 			= content
			, outtable 			= text
			, split_var			= line
			, split_dlm			= <
			, var_after_split	= text
			, noprint			= Y
			);
				
%printmsg(msg = , printalllog = , closealllog = Y );

*	Check if the month & year are valid and excits
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

	data ck; set text; keep ck_year ck_month; length ck_year $4. ck_month $2.;
	if find(text,'option value','i');
	value = compress(scan(text,2,'"'),,'kd');
	
	if length(value) = 4 and substr(value,1,2) = '20' then  ck_year = value;
	if length(value) = 2 and value in (1,2,3,4,5,6,7,8,9,10,11,12) then ck_month = value;
	run;
	proc sort data = ck nodupkey tagsort; by _all_;run;
	
	proc sql noprint;
	select distinct count(*) into: cnt 
	from ck
	where ck_year = "&y" or ck_month = "&m."
	;
	quit;
	
%macro downloadfigure();
*	Extract Figure Title, Figures Day and Figure information
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
	data Climate_datadownload; set text; 
	
	if find(line,"th scope=",'i') and find(line,"abbr title",'i') then do;
				
/* Get the Title of each figure */			
		if find(text,"glossary_e.html",'i') then do;
				
			_title = scan(text,2,'>');
				
			if _title ='' then _title = scan(scan(scan(line,3,">"),1,'<'),2,'"');
				
		end;
				
/* Get the Day of each figure */				
		else if find(text,"&y.",'i') then _day = scan(text,2,'>');
		
	end;
	
/* Get the Figure of each day */
	if find(text,"td",'i') then _figure = scan(text,2,'>');
	if _figure = '' and find(text,'#legendM','i') then _figure = '&nbsp;';
	
	if _title ^='' or _day ^='' or _figure ^='';

	run;%errorcheck();
			
*	Mark Data Year, Month, Province, Station ID and Station Name
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
	data climate_datadownload; set climate_datadownload;
	/* Mark the Year, Month, Province, Station ID and Station Name of extracted information */			
	length Year $4. Month $2. Province $30. ClimateID $20. StationID $20. StationName $200. ;
			
	Year = "&y.";
	Month = "&m.";
	Province = "&province.";
	ClimateID = "&climate_id.";
	StationID = "&climate_stationid.";
	StationName = "&climate_stationname.";
	run;%errorcheck();
			
*	Tidy Up: Day of each Figure
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
	data climate_datadownload; set climate_datadownload;
		
	retain _temp_day ;
	if not missing(_day) then _temp_day = _day; else _day = _temp_day;
	drop _temp_day;
				
	obs = _n_;
			
	run;%errorcheck();

*	Tidy Up: Title of each Figure
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
	proc sort data = climate_datadownload tagsort; by _day obs;run;
			
	data climate_datadownload; set climate_datadownload;by _day;retain n;if first._day then n = 0;n+1;run;
	
	proc sql;
	create table _climate_datatitle as 
	select distinct _title, n from climate_datadownload
	where _title ^=''
	;quit;%errorcheck();
	
	proc sql;
	create table climate_datadownload1 as 
	select distinct  a.Province, a.StationID, a.StationName
					, a.Year, a.Month, a._day as Day, a.n
					, tranwrd(strip(b._title),' ','_') as Title
					, a._figure
	from climate_datadownload a left join _climate_datatitle b on a.n=b.n
	where 	a.n>1 
		and a._figure ^='' 
		and a.n in (select n from _climate_datatitle)
	;		
	quit;%errorcheck();
	
	data climate_datadownload1; set climate_datadownload1; 
	length Figure $10.;
	if _figure = '&nbsp;' then Figure = ''; else Figure = _figure;
	drop _figure n;
	run;%errorcheck();
		
*	Tidy Up: Transpose Data, Transpose the Title to variable
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
	proc transpose data = climate_datadownload1 out = climate_datadownload2 (drop = _name_);
	by province stationid stationname year month day;
	id Title;
	var figure;
	run;%errorcheck();
		
*	Append: add extracted data to Climate_data and remove duplication
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
	data &output.; set &output. climate_datadownload2; if province ^='';run;%errorcheck();
	proc sort data = &output. nodupkey tagsort; by _all_;run;%errorcheck();
	
%mend;

*	Extracted the Download figure process only if the month & year contains value in Climate website 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
	%if &cnt. >1 %then %do; %downloadfigure; %end; 
	%else %do;
	
		data climate_xdata;
		length Year $4. Month $2. Province $30. ClimateID $20. StationID $20. StationName $200. NoData $1.
		;
			
		Year = "&y.";
		Month = "&m.";
		Province = "&province.";
		ClimateID = "&climateid.";
		StationID = "&climate_stationid.";
		StationName = "&climate_stationname.";
		NoData = "Y";
		run;
		data &output.; set &output. climate_xdata; if province ^='';run;%errorcheck();
		proc sort data = &output. nodupkey tagsort; by _all_;run;%errorcheck();
		
	%end;
		
	
%end;
/*----------------------------------------------------------------------------------------------------*/
/*						Squeeze Dataset
/*----------------------------------------------------------------------------------------------------*/
%SQUEEZE( DSNIN = &output., DSNOUT = &output.1, drop_in = Y,RENAME_OUT_AS_IN =Y ) ;

%printmsg(msg = [ Download Daily Climate Data Finished! ], printalllog = Y , closealllog = );

/*----------------------------------------------------------------------------------------------------*/
/*						Delete Working Dataset
/*----------------------------------------------------------------------------------------------------*/
%deldataset(lib = work,prefix = climate);
%deldataset(lib = work,prefix = _climate);
%deldataset(lib = work,prefix = ck);
%mend;

/*----------------------------------------------------------------------------------------------------*/
/*						Run Macro Program
/*----------------------------------------------------------------------------------------------------*/
options nosymbolgen noserror msglevel = i replace;
%download_dly_climate_by_prov(outlib = out, outtable = Climate_data);
options symbolgen serror;

