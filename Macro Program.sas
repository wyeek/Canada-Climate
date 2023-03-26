*******************************************************************************************************;
*
*
*
*
*
*										ABC
*
*
*
*
*
*******************************************************************************************************;





/*====================================================================================================*/
/*
/*										abc
/*
/*====================================================================================================*/


/*____________________________________________________________________________________________________*/
/*
/*										1. Preparation
/*
/*____________________________________________________________________________________________________*/

/*----------------------------------------------------------------------------------------------------*/
/*										A. 
/*----------------------------------------------------------------------------------------------------*/

*	Check: 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;


/*____________________________________________________________________________________________________*/
/*
/*										2. Extraction
/*
/*____________________________________________________________________________________________________*/


/*____________________________________________________________________________________________________*/
/*
/*										3. Tidy Up
/*
/*____________________________________________________________________________________________________*/






*******************************************************************************************************;
*
*
*
*
*
*										Macro Program
*
*
*
*
*
*******************************************************************************************************;

/*====================================================================================================*/
/*
/*										Error Check
/*	example:
/*	%errorcheck();
/*
/*====================================================================================================*/
%macro errorcheck();

  %if &syserr > 0 %then %do;
  
    %put ERROR: Program stopped due to &syserr &syserrortext;
    %abort cancel;
    
  %end;
  
%mend;


/*====================================================================================================*/
/*
/*										Print Specific Message / Dont Print any Log
/*	example:
/*	%printmsg(msg = [ &i. / &total_stationid. ] , printalllog= , closealllog = );
/*	%printmsg(msg, printalllog=Y);
/*
/*====================================================================================================*/
%macro printmsg(msg=			/* Print Message */
				, printalllog=	/* Print all Log information */
				, closealllog =	/* Dont print any message */
				);

%let openlog = OPTIONS NOTES  SOURCE /*SYNTAXCHECK*/ VARLENCHK = ERROR;
%let closelog = OPTIONS NONOTES NOSOURCE /*NOSYNTAXCHECK*/ VARLENCHK = NOWARN;

%if %length(&closealllog.) > 0 %then %do;

	&closelog.;

%end;

%else %do;

		%if %length(&msg.) >0  %then %do;
		
			&openlog. ;
			%put NOTE: &msg. , [ %sysfunc(datetime(),datetime20.) ];
			&closelog. ;
			
		%end;
		
		%if %length(&printalllog.) >0 %then %do;
		
			&openlog.;
		
		%end;

%end;

%mend;


/*====================================================================================================*/
/*
/*										Display looping status
/*	example :
/*	%loopstatus(total = 52106,inputnumber = i , msg = Download Data Process);
/*
/*====================================================================================================*/
%macro loopstatus(	total			/* Number of Total Looping */
					,inputnumber	/* Looping Number */
					, msg			/* Print looping message */
					);

data _null_;
div = 10;
i = 1;
n = &inputnumber.;

do until(n<100); n = n / (div*10); i + 1; end;

finaldiv = div ** i;

call symputx('divby', finaldiv);
run;


%if %sysfunc(mod(&inputnumber., &divby.)) = 0 or &inputnumber. = &total. or %length(&inputnumber.) <2 %then %do;

	%printmsg(msg = &msg. - &inputnumber. / &total. , printalllog = , closealllog = );
	
%end;
%else %do;

	%printmsg(msg = , printalllog = , closealllog = Y);

%end;

%mend;


/*====================================================================================================*/
/*
/*										Delete Specific prefix Dataset
/*	example :
/*	%deldataset(lib = work,prefix = _);
/*
/*====================================================================================================*/
%macro deldataset(	lib				/* library of delete dataset */
					, prefix		/* prefix of delete dataset */
					, Noprint = 	/* Dont print any message */
					);

%printmsg(msg = , printalllog = , closealllog = Y);

proc sql noprint;
create table deldslist as 
select distinct memname , libname, compress(cats(libname,".",memname)) as fulltablename
from dictionary.columns
where libname = %upcase("&lib.") and substr(memname,1,%length(&prefix.)) = %upcase("&prefix.")
;
select count(*) into: total_delds from deldslist
;
quit;

%if &total_delds. >0 %then %do;

	data _null_; set deldslist;
	call symputx('delds'||strip(_n_),fulltablename);
	call symputx('total_delds',_n_);
	run;
	
	%do a = 1 %to &total_delds.;
	
		proc sql; drop table &&delds&a.;quit;
		
	%end;
	
	proc sql; drop table deldslist;quit;

	%if %length(&noprint.) =0 %then %do;

%printmsg(msg = [ Total [ %sysfunc(strip(&total_delds.)) ] [ &prefix. ] Prefix Dataset(s) Deleted in &lib. Library. ], printalllog = Y, closealllog = );

	%end;
	%else %do;

%printmsg(msg = , printalllog = Y, closealllog = );

	%end;

%end;
%else %do;

	%if %length(&noprint.) =0 %then %do;

%printmsg(msg = [ No [ &prefix. ] Prefix Dataset(s) in &lib. Library. ], printalllog = Y, closealllog = );

	%end;
	%else %do;

%printmsg(msg = , printalllog = Y, closealllog = );

	%end;

%end;

%mend;


/*====================================================================================================*/
/*
/*										Random Sample Check
/*	example:
/*	%samplecheck(table = ck_year1,samplenumber = 100 , keepvar = , selectioncriteria = , Noprint = );
/*
/*====================================================================================================*/
%macro samplecheck(	table					/* Table for Sample Check */
					, samplenumber			/* Number of sample check */
					, keepvar				/* Keep specific variable only for Sample Check, if null, keep all variable */
					, selectioncriteria		/* Select specific items for sample check, if null, random selection based on the whole table */
					, Noprint				/* Dont print any message */
					);

%printmsg(msg = , printalllog = , closealllog = Y);

data _sample; set &table.;
%if %length(&keepvar.) >0 %then %do; keep &keepvar.; %end;

randomnumber = rand('uniform');run;
proc sort data = _sample tagsort; by randomnumber;run;

data Sample; set _sample (obs = &samplenumber.); drop randomnumber;run;

%deldataset(lib = work,prefix = _sample);

%if %length(&noprint.) =0 %then %do;

%printmsg(msg = [ Random Selected [ &samplenumber. ] from [ &table. ] and Output as [ Sample ] ], printalllog = Y, closealllog = );

%end;
%else %do;

%printmsg(msg = , printalllog = Y, closealllog = );

%end;
%mend;


/*====================================================================================================*/
/*
/*										Merge Multi Records into 1 Record
/*	example:
/*	%mergerow(intable = a, outtable = b , mergevar = name, splitby = , newvar = b, mergeby = memname);
/*
/*====================================================================================================*/
%macro mergerow(intable			/* Input Table Name */
				, outtable		/* Output Table Name */
				, mergevar		/* Variable of Merging Multi Records */
				, splitby 		/* Multi Records separated by any special char, if null, will separate by space */
				, newvar		/* Variable name of the single record */
				, mergeby 		/* Merge the multi records by which variables */
				, Noprint		/* Dont print any message */
				);
%printmsg(msg = , printalllog = , closealllog = Y);

proc sort data = &intable. out = mr_&intable. tagsort; by &mergeby.;run;

%let mergeby_last = %scan(&mergeby.,-1,' ');

data &outtable.;
length &newvar. $32767.;

do until (last.&mergeby_last.);

	set mr_&intable.;
	by &mergeby. ;
	&newvar. = catx("&splitby.",&newvar., &mergevar.);

end;
run;

%deldataset(lib = work ,prefix = mr_&intable., Noprint = Y);

%if %length(&noprint.) =0 %then %do;

%printmsg(msg = [ Merge Multi Row in [ &mergevar.] Finished and Saved as [ &newvar. ] in [ &outtable. ] ], printalllog = Y, closealllog = );

%end;
%else %do;

%printmsg(msg = , printalllog = Y, closealllog = );

%end;

%mend;


/*====================================================================================================*/
/*
/*										Check Dataset whether exist
/*	example:
/*	%ckdataset(lib = out, tablename = climate_data, tablenamelist = );
/*
/*====================================================================================================*/
%macro ckdataset(lib 				/* Check Dataset's library */
				, tablename			/* Check Dataset Name */
				, tablenamelist		/* List Dataset name with special prefix */
				, obsnum			/* Check Number of obs of Check Dataset */
				, Noprint = 		/* Dont Print any message */
				);

%printmsg(msg = , printalllog = , closealllog = Y);

/*----------------------------------------------------------------------------------------------------*/
/*
/*										Preparation
/*
/*----------------------------------------------------------------------------------------------------*/
%global ckdatasetexist;
%let ckdatasetexist = ;

proc sql noprint;
create table ckdataset as 
select distinct libname, memname as TableName, compress(cats(libname,".",memname)) as FullTableName
from dictionary.columns
where libname = %upcase("&lib.") and substr(memname,1,%length(&tablename)) = %upcase("&tablename.")
;
select count(*) into: total_ckds from ckdataset
;
quit;

/*----------------------------------------------------------------------------------------------------*/
/*
/*										If found the dataset
/*
/*----------------------------------------------------------------------------------------------------*/
%if &total_ckds > 0  %then %do;	

	%global ckdatasetexist;
	%let ckdatasetexist = Y;

*....................................................................................................;
*										Count number of obs
*....................................................................................................;
	
	data _null_; set ckdataset; call symputx('ds'||strip(_n_),FullTableName); call symputx('total_ds',_n_);run;

	%do i = 1 %to &total_ds.;

		proc sql noprint; select count(*) into: dsobs from &&ds&i.;quit;
		data ckdataset; set ckdataset; /*length obs best32.;*/
		if fulltablename = "&&ds&i." then obs = &dsobs.;
		run;
	
	%end;
	
	%if %length(&noprint.) =0 %then %do;

%printmsg(msg = [ Dataset [ &tablename. ] exist in [ &lib. ] Library and used Y to represent in Global Macro Variable [ %str(&)ckdatasetexist ] ], printalllog =  , closealllog = );
	
	%end;
	
*....................................................................................................;
*										Del Table Name List if not required
*....................................................................................................;

	%if %length(&tablenamelist.) = 0 %then %do;
	
	%deldataset(lib = work,prefix = ckdataset , noprint = Y);
	
	%end;
	%else %do;
	
	%if %length(&noprint.) =0 %then %do;
	
%printmsg(msg = [ Dataset [ &tablename. ] listed in [ ckdataset ] Table ], printalllog =  , closealllog = );
	
	%end;
	
	%end;
	
*....................................................................................................;
*										Display number of obs if required
*....................................................................................................;
	
	%if %length(&obsnum.) >0 %then %do;
	
%printmsg(msg = , printalllog = , closealllog = Y);

		%global ckdatasetobsnum xobs;
		%let ckdatasetobsnum = 0;
		%let xobs = ;
		proc sql noprint; select count(*) into: ckdatasetobsnum from &lib..&tablename. ;quit;
		
		%if &ckdatasetobsnum. = 0 %then %do; %let xobs = Y; %end;
		
		%if %length(&noprint.) =0 %then %do;
			
				%if %length(&xobs.) >0 %then %do;
				
%printmsg(msg = [ Dataset [ &tablename. ] contains [ %sysfunc(strip(&ckdatasetobsnum.)) ] records and used Y in Global Macro Variable [ %str(&)xobs. ] ], printalllog =  , closealllog = );
				
				%end;
				%else %do;
				
%printmsg(msg = [ Dataset [ &tablename. ] contains [ %sysfunc(strip(&ckdatasetobsnum.)) ] records and stored in Global Macro Variable [ %str(&)ckdatasetobsnum. ] ], printalllog =  , closealllog = );

				%end;
				
		%end;
		
	%end;
	
%end;
%else %do;

	%global ckdatasetexist;
	%let ckdatasetexist = ;
	%deldataset(lib = work,prefix = ckdataset, noprint = Y);

	%if %length(&noprint.) =0 %then %do;
	
%printmsg(msg = [ Dataset [ &tablename. ] NOT exist in [ &lib. ] Library and used Null to represent in Global Macro Variable [ %str(&)ckdatasetexist. ]  ], printalllog =  , closealllog = );

	%end;
%end;



%printmsg(msg = , printalllog = Y, closealllog = );


%mend;


/*====================================================================================================*/
/*
/*										Web content extraction
/*	example :
/*	%url_content(www.google.com);
/*
/*====================================================================================================*/
%macro url_content(	url				/* URL for extraction */
					, Noprint = 	/* Dont print any message */
					);

%printmsg(msg = , printalllog = , closealllog = Y);

	/* extract information from target URL */
	%let outds = out;
	
/* 	data &outds.; run; */
	
	filename out temp;

	proc http url="&url." method="get" out=out;	run;

	/* use table format dataset to show the content of target URL */
	data content;
	
		infile out length=len lrecl=32767;
		input line $varying32767. len;
		line=strip(line);
		if len>0;
		
	run;
	
	%SQUEEZE( DSNIN = content, DSNOUT = content1, drop_in = Y,RENAME_OUT_AS_IN =Y , noprint = Y) ; 
	
%if %length(&noprint.) =0 %then %do;

%printmsg(msg = [ Extraction Finished! ], printalllog = Y , closealllog = );

%end;
%else %do;

%printmsg(msg = , printalllog = Y, closealllog = );

%end;

%mend;


/*====================================================================================================*/
/*
/*										Split String by Delimiter
/*	example :
/*	%split(	  intable 			= content
			, outtable 			= text
			, split_var			= line
			, split_dlm			= >
			, var_after_split	= text
			, noprint			=
			);
			
/*====================================================================================================*/
%macro split(intable			/* Input table */
			, outtable			/* Output table */
			, split_var			/* Splitting variable */
			, split_dlm			/* Split by specific delimiter */
			, var_after_split	/* Variable name after splitting */
			, Noprint =			/* Dont print any message */
			);

%printmsg(msg = , printalllog = , closealllog = Y);

	data &outtable.; set &intable.;

		do _n_=1 to countw(&split_var., "&split_dlm.");
		
			&var_after_split.=scan(&split_var., _n_, "&split_dlm.");
			output;
			
		end;
		
	run;
	%SQUEEZE( DSNIN = &outtable., DSNOUT = &outtable.1, drop_in = Y,RENAME_OUT_AS_IN =Y , noprint = Y) ; 
	
%if %length(&noprint.) =0 %then %do;

%printmsg(msg = [ Splitted &split_var. by [ &split_dlm. ] and Output as [ &outtable. ] ], printalllog = Y , closealllog = );

%end;
%else %do;

%printmsg(msg = , printalllog = Y, closealllog = );

%end;
%mend;


/*====================================================================================================*/
/*
/*										Squeeze dataset
/*	example : 
/*	%SQUEEZE( DSNIN = ck_currentyr, DSNOUT = ck_currentyr1, drop_in = Y, RENAME_OUT_AS_IN =Y ) ;
/*
/*====================================================================================================*/

%macro SQUEEZE( DSNIN       /* name of input SAS dataset                                                      */
              , DSNOUT      /* name of output SAS dataset                                                     */
              , NOCOMPRESS= /* [optional] variables to be omitted from the minimum-length computation process */
              , DROP_IN = 	/* Y = drop input SAS dataset 													  */
              , RENAME_OUT_AS_IN = /* Y = rename output SAS dataset's name as the name of input SAS dataset	  */	
              , Noprint = 			
              ) ;

   /* PURPOSE: create LENGTH statement for vars that minimizes the variable length to:
    *          numeric vars: the fewest # of bytes needed to exactly represent the values contained in the variable
    *
    *          character vars: the fewest # of bytes needed to contain the longest character string
    *
    *          macro variable SQZLENTH is created which is then invoked in a
    *          subsequent data step
    *
    * NOTE:    if no char vars in dataset, produce no char var processing code
    *
    * NOTE:    length of format for char vars is changed to match computed length of char var
    *          e.g., if length( CHAR_VAR ) = 10 after %SQUEEZE-ing, then FORMAT CHAR_VAR $10. ; is generated
    *
    * NOTE:    variables in &DSNOUT are maintained in same order as in &DSNIN
    *
    * NOTE:    variables named in &NOCOMPRESS are not included in the minimum-length computation process
    *          and keep their original lengths as specified in &DSNIN
    *
    * EXAMPLE OF USE:
    *          %SQUEEZE( DSNIN, DSNOUT )
    *          %SQUEEZE( DSNIN, DSNOUT, NOCOMPRESS=A B C D--H X1-X100 )
    *          %SQUEEZE( DSNIN, DSNOUT, NOCOMPRESS=_numeric_          )
    *          %SQUEEZE( DSNIN, DSNOUT, NOCOMPRESS=_character_        )
    */

%printmsg(msg = , printalllog = , closealllog = Y);

   %global SQUEEZE ;
   %local I ;

   %if "&DSNIN" = "&DSNOUT"
   %then %do ;
   
%printmsg(msg = , printalllog = Y , closealllog = );
  
      %put /------------------------------------------------\ ;
      %put | ERROR from SQUEEZE:                            | ;
      %put | Input Dataset has same name as Output Dataset. | ;
      %put | Execution terminating forthwith.               | ;
      %put \------------------------------------------------/ ;

      %goto L9999 ;
   %end ;

   /*############################################################################*/
   /* begin executable code
   /*############################################################################*/

   /*============================================================================*/
   /* create dataset of variable names whose lengths are to be minimized
   /* exclude from the computation all names in &NOCOMPRESS
   /*============================================================================*/

   proc contents data=&DSNIN( drop=&NOCOMPRESS ) memtype=data noprint out=_cntnts_( keep= name type ) ; run ;

   %let N_CHAR = 0 ;
   %let N_NUM  = 0 ;

   data _null_;
      set _cntnts_ end=lastobs nobs=nobs ;

      if nobs = 0 then stop ;

      n_char + ( type = 2 ) ;
      n_num  + ( type = 1 ) ;

      /* create macro vars containing final # of char, numeric variables */

      if lastobs
      then do ;
         call symput( 'N_CHAR', left( put( n_char, 5. ))) ;
         call symput( 'N_NUM' , left( put( n_num , 5. ))) ;
      end ;
   run ;

   /*============================================================================*/
   /* if there are NO numeric or character vars in dataset, stop further processing
   /*============================================================================*/

   %if %eval( &N_NUM + &N_CHAR ) = 0
   %then %do ;
   
%printmsg(msg = , printalllog = Y , closealllog = );
  
      %put /----------------------------------\ ;
      %put | ERROR from SQUEEZE:              | ;
      %put | No variables in dataset.         | ;
      %put | Execution terminating forthwith. | ;
      %put \----------------------------------/ ;

      %goto L9999 ;
   %end ;

   /*============================================================================*/
   /* put global macro names into global symbol table for later retrieval
   /*============================================================================*/

   %do I = 1 %to &N_NUM ;
      %global NUM&I NUMLEN&I ;
   %end ;

   %do I = 1 %to &N_CHAR ;
      %global CHAR&I CHARLEN&I ;
   %end ;

   /*============================================================================*/
   /* create macro vars containing variable names
   /* efficiency note: could compute n_char, n_num here, but must declare macro names to be global b4 stuffing them
   /*
   /* note: if no char vars in data, do not create macro vars
   /*============================================================================*/

   proc sql noprint ;
      %if &N_CHAR > 0 %then %str( select name into :CHAR1 - :CHAR&N_CHAR from _cntnts_ where type = 2 ; ) ;

      %if &N_NUM  > 0 %then %str( select name into :NUM1  - :NUM&N_NUM   from _cntnts_ where type = 1 ; ) ;
   quit ;

   /*============================================================================*/
   /* compute min # bytes (3 = min length, for portability over platforms) for numeric vars
   /* compute min # bytes to keep rightmost character for char vars
   /*============================================================================*/

   data _null_ ;
      set &DSNIN end=lastobs ;

      %if &N_NUM  > 0 %then %str ( array _num_len_  ( &N_NUM  ) 3 _temporary_ ; ) ;

      %if &N_CHAR > 0 %then %str( array _char_len_ ( &N_CHAR ) _temporary_ ; ) ;

      if _n_ = 1
      then do ;
         %if &N_CHAR > 0 %then %str( do i = 1 to &N_CHAR ; _char_len_( i ) = 0 ; end ; ) ;

         %if &N_NUM  > 0 %then %str( do i = 1 to &N_NUM  ; _num_len_ ( i ) = 3 ; end ; ) ;
      end ;

      %if &N_CHAR > 0
      %then %do ;
         %do I = 1 %to &N_CHAR ;
            _char_len_( &I ) = max( _char_len_( &I ), length( &&CHAR&I )) ;
         %end ;
      %end ;

      %if &N_NUM > 0
      %then %do I = 1 %to &N_NUM ;
         if &&NUM&I ne .
         then do ;
            if &&NUM&I ne trunc( &&NUM&I, 7 ) then _num_len_( &I ) = max( _num_len_( &I ), 8 ) ; else
            if &&NUM&I ne trunc( &&NUM&I, 6 ) then _num_len_( &I ) = max( _num_len_( &I ), 7 ) ; else
            if &&NUM&I ne trunc( &&NUM&I, 5 ) then _num_len_( &I ) = max( _num_len_( &I ), 6 ) ; else
            if &&NUM&I ne trunc( &&NUM&I, 4 ) then _num_len_( &I ) = max( _num_len_( &I ), 5 ) ; else
            if &&NUM&I ne trunc( &&NUM&I, 3 ) then _num_len_( &I ) = max( _num_len_( &I ), 4 ) ;
         end ;
      %end ;

      if lastobs
      then do ;
         %if &N_CHAR > 0
         %then %do ;
            %do I = 1 %to &N_CHAR ;
               call symput( "CHARLEN&I", put( _char_len_( &I ), 5. )) ;
            %end ;
         %end ;

         %if &N_NUM > 0
         %then %do I = 1 %to &N_NUM ;
            call symput( "NUMLEN&I", put( _num_len_( &I ), 1. )) ;
         %end ;
      end ;
   run ;

   proc datasets nolist ; delete _cntnts_ ; run ;

   /*============================================================================*/
   /* initialize SQZ_NUM, SQZ_CHAR global macro vars
   /*============================================================================*/

   %let SQZ_NUM      = LENGTH ;
   %let SQZ_CHAR     = LENGTH ;
   %let SQZ_CHAR_FMT = FORMAT ;

   %if &N_CHAR > 0
   %then %do I = 1 %to &N_CHAR ;
         %let SQZ_CHAR     = &SQZ_CHAR %qtrim( &&CHAR&I ) $%left( &&CHARLEN&I ) ;
         %let SQZ_CHAR_FMT = &SQZ_CHAR_FMT %qtrim( &&CHAR&I ) $%left( &&CHARLEN&I ). ;
   %end ;

   %if &N_NUM > 0
   %then %do I = 1 %to &N_NUM ;
      %let SQZ_NUM = &SQZ_NUM %qtrim( &&NUM&I ) &&NUMLEN&I ;
   %end ;

   /*============================================================================*/
   /* build macro var containing order of all variables
   /*============================================================================*/

   data _null_ ;
      length retain $32767 ;
      retain retain 'retain ' ;

      dsid = open( "&DSNIN", 'I' ) ; /* open dataset for read access only */

      do _i_ = 1 to attrn( dsid, 'nvars' ) ;
         retain = trim( retain ) || ' ' || varname( dsid, _i_ ) ;
      end ;

      call symput( 'RETAIN', retain ) ;
   run ;

   /*============================================================================*/
   /* apply SQZ_* to incoming data, create output dataset
   /*============================================================================*/

   data &DSNOUT ;
      &RETAIN ;
      
      %if &N_CHAR > 0 %then %str( &SQZ_CHAR ;     ) ; /* optimize char var lengths      */

      %if &N_NUM  > 0 %then %str( &SQZ_NUM ;      ) ; /* optimize numeric var lengths   */

      %if &N_CHAR > 0 %then %str( &SQZ_CHAR_FMT ; ) ; /* adjust char var format lengths */

      set &DSNIN ;
   run ;

%L9999:

   /*============================================================================*/
   /* Drop input SAS dataset
   /*============================================================================*/
  
%if %length(&drop_in.) >0 %then %do;

	proc sql; drop table &DSNIN.;quit;
	
	
%end;


   /*============================================================================*/
   /* Renmae output SAS dataset as input
   /*============================================================================*/
  
%if %length(&RENAME_OUT_AS_IN.) >0 %then %do;

	data &DSNIN.; set &DSNOUT.;run;
	proc sql; drop table &DSNOUT.; quit;
	
	%if %length(&noprint.) =0 %then %do;
	
	%printmsg(msg = [ Input Table [ &DSNIN. ] Squeezed and Output as [ &DSNIN. ] ], printalllog = Y , closealllog = );
	
	%end;


%end;
%else %do;

	%if %length(&noprint.) =0 %then %do;
	
	%printmsg(msg = [ Input Table [ &DSNIN. ] Squeezed and Output as [ &DSNOUT. ] ], printalllog = Y , closealllog = );
	
	%end;

%end;

%printmsg(msg = , printalllog = Y, closealllog = );

%mend SQUEEZE ;




