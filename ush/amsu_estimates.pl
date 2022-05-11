#! /usr/bin/env perl 
#-w
use warnings;

#use lib "/nw$ENV{envir}/lib/incmod/perl";

use 5.008;
use File::Copy;
use Thread::Semaphore;
use IO::File;
#use threads;
use Date::Manip;
use strict;

#=====================================
#       ***  Environment 
#=====================================

my $HomeDir = $ENV{'HOMEnhc'};
push(@INC, "$HomeDir/ush");
push(@INC, "$HomeDir/exec");

my $job = $ENV{'job'}; 

my $packDir = $ENV{'COMOUT'};
$ENV{"packDir"} = $packDir;

my $envir = $ENV{'envir'};
my $COMIN = $ENV{'COMIN'};
my $COMOUT = $ENV{'COMOUT'};
my $COMNHC = $ENV{'COMNHC'};
my $COMJTWC = $ENV{'COMJTWC'};
my $DATA = $ENV{'DATA'};

# Get the dumplist and dumpjb location
# Note: on wcoss2, both are set in the
# same module 'bufr_dump'. 
my $LIST = $ENV{'BUFR_DUMPLIST'};
my $DUMPJB = $ENV{'DUMPJB'};

my $ExecDir = "$HomeDir/exec";
$ENV{"ExecDir"} = $ExecDir;

my $UshDir = "$HomeDir/ush";
my $FixDir = "$HomeDir/fix";

my $workDir = "$DATA";
$ENV{"workDir"} = $workDir;

my $subWorkDir = "$DATA";
$ENV{"subWorkDir"}= $subWorkDir;

my $dataDir = "$HomeDir/fix";

my $nhcBtkDir = "$COMNHC/btk";
$ENV{"nhcBtkDir"} = $nhcBtkDir;
my $nhcComDir = "$COMNHC/com";
$ENV{"nhcComDir"} = $nhcComDir;
my $nhcAidDir = "$COMNHC/aid";
$ENV{"nhcAidDir"} = $nhcAidDir;

my $jtwcBtkDir = "$COMJTWC/btk";
$ENV{"jtwcBtkDir"} = $jtwcBtkDir;
my $jtwcComDir = "$COMJTWC/com";
$ENV{"jtwcComDir"} = $jtwcComDir;
my $jtwcAidDir = "$COMJTWC/aid";
$ENV{"jtwcAidDir"} = $jtwcAidDir;

my $storeData = 1;
my $storeFixText = 1;
my $storeFixATCF = 1;
my $storeLog = 1;
my $storeFixTextDir = "$COMOUT";
my $storeFixATCFDir = "$COMOUT";
my $storeLogDir = "$COMOUT";
my $storeDataDir = "$COMOUT";

my $dbnroot = $ENV{'DBNROOT'};
my $senddbn = $ENV{'SENDDBN'};
#=====================================
#       ***  Required Perl Code 
#=====================================

require "$UshDir/nhc_generalutils.pl";
require "$UshDir/nhc_atcfstorms.pl";
require "$UshDir/amsu_stormdata.pl";
require "$UshDir/amsu_intensity.pl";


#=====================================
#       ***  AMSU Algorithm Files 
#=====================================

#--- Executables

my $bufrToAsc = "amsu_readbufr";
my $tempRetrieval = "amsu_tempret";

#--- needed for storage
my $FILENAME2 = "COORDINATES";
#---end


#=====================================
#       ***  IBM Query Variables
#=====================================

my $radius = "6.0";

#--- IBM result file names
my $type = "amuata";
my $ibmBufrData = $type . ".ibm";
my $ibmLogFile = $type . ".out";
##my $type2 = "airsev";
##my $ibm2BufrData = $type2 . ".ibm";
##my $ibm2LogFile = $type2 . ".out";

#--- IBM database definition file
my $ibmDBdef = $LIST;

# JY debug - will remove
printf ("dumplist: ibmDBdef=$ibmDBdef.\n");

#--- IBM database tank
# JY my $ibmTank = "/dcom/us007003";
my $ibmTank = $ENV{'TANK'};;

#--- Number of seconds to wait for IBM results
my $totalWait = 900;    #total of 15 (900) minutes wait for the uncorrected bufr data


#=====================================
#       ***  Output Files
#=====================================

#--- ASCII text AMSU data files
#my $ascii_files = ["noaa15.out", "noaa16.out", "noaa18.out"];
##my $ascii_files = ["noaa15.out", "noaa16.out", "noaa18.out","noaa19.out","metop2.out","airsev.out"];
my $ascii_files = ["noaa15.out", "noaa16.out", "noaa18.out","noaa19.out","metop2.out"];

#--- Temperature Retrieval files
#my $retrieval_files = ["noaa15.ret", "noaa16.ret", "noaa18.ret"];
##my $retrieval_files = ["noaa15.ret", "noaa16.ret", "noaa18.ret","noaa19.ret","metop2.ret","aqua.ret"];
my $retrieval_files = ["noaa15.ret", "noaa16.ret", "noaa18.ret","noaa19.ret","metop2.ret"];
my $alerttype_ret = "TCFIX";

#--- Location files
#my $loc_files = ["NOAA15.LOC", "NOAA16.LOC", "NOAA18.LOC"];
my $loc_files = ["NOAA15.LOC", "NOAA16.LOC", "NOAA18.LOC", "NOAA19.LOC", "NOAA02.LOC", "NOAA01.LOC"];
my $alerttype_loc = "TCFIX";

#--- Intensity Estimates - Text format files
#my $fix_files = ["NOAA15.FIX", "NOAA16.FIX", "NOAA18.FIX"];
my $fix_files = ["NOAA15.FIX", "NOAA16.FIX", "NOAA18.FIX", "NOAA19.FIX", "NOAA02.FIX", "NOAA01.FIX"];
my $alerttype_fix = "TCFIX";

#--- Intensity Estimates - ATCF F DECK Format files 
#my $atcf_files = ["NOAA15.AFX", "NOAA16.AFX", "NOAA18.AFX"];
my $atcf_files = ["NOAA15.AFX", "NOAA16.AFX", "NOAA18.AFX", "NOAA19.AFX", "NOAA02.AFX", "NOAA01.AFX"];
my $alerttype_atcf = "TCFIX";

#--- Horizontal and Verical profiles 
#my $xya_files = ["NOAA15.XYA", "NOAA16.XYA", "NOAA18.XYA"]; 
#my $rza_files = ["NOAA15.RZA", "NOAA16.RZA", "NOAA18.RZA"];
my $xya_files = ["NOAA15.XYA", "NOAA16.XYA", "NOAA18.XYA", "NOAA19.XYA", "NOAA02.XYA", "NOAA01.XYA"];
my $rza_files = ["NOAA15.RZA", "NOAA16.RZA", "NOAA18.RZA", "NOAA19.RZA", "NOAA02.RZA", "NOAA01.RZA"];
my $alerttype_xya = "TCFIX";
my $alerttype_rza = "TCFIX";

#--- Statistics files
#my $sta_files = ["NOAA15.STA", "NOAA16.STA", "NOAA18.STA"];
my $sta_files = ["NOAA15.STA", "NOAA16.STA", "NOAA18.STA", "NOAA19.STA", "NOAA02.STA", "NOAA01.STA"];
my $alerttype_sta = "TCFIX";

#---  Temperature Retrieval coefficient files
#my $coefficient_files = [
#                        "n15amsusimreg.txt", "n15regr_land.txt",
#                        "n15limb_land.txt",  "n15regr_sea.txt",
#                        "n15limb_sea.txt",   "n16limb_land.txt",
#                        "n16regr_sea.txt",   "n16limb_sea.txt",
#                        "n16amsusimreg.txt", "n16regr_land.txt",
#                        "n18limb_sea.txt",   "n18amsusimreg.txt",
#                        "n18regr_land.txt",  "n18limb_land.txt",
#                        "n18regr_sea.txt"
#                     ];

##my $coefficient_files = [ "aquaamsusimreg.txt", "n15amsusimreg.txt", "n17regr_land.txt",
##			  "aqualimb_land.txt",  "n15limb_land.txt",  "n17regr_sea.txt",
##			  "aqualimb_sea.txt",   "n15limb_sea.txt",   "n18amsusimreg.txt",
##			  "aquaregr_land.txt",  "n15regr_land.txt",   "n18limb_land.txt",
##		          "aquaregr_sea.txt",   "n15regr_sea.txt",    "n18limb_sea.txt",
my $coefficient_files = [ "n15amsusimreg.txt", "n17regr_land.txt",
			  "n15limb_land.txt",  "n17regr_sea.txt",
			  "n15limb_sea.txt",   "n18amsusimreg.txt",
			  "n15regr_land.txt",   "n18limb_land.txt",
		          "n15regr_sea.txt",    "n18limb_sea.txt",
		          "limball_n19land.07.woch8", "n16amsusimreg.txt",  "n18regr_land.txt",
			  "limball_n19sea.07.woch8",  "n16limb_land.txt",   "n18regr_sea.txt",
			  "lsmask30",              "n16limb_sea.txt",    "n19amsusimreg.txt",
			  "metop2amsusimreg.txt",  "n16regr_land.txt",   "n19limb_land.txt",
		          "metop2limb_land.txt",   "n16regr_sea.txt",    "n19limb_sea.txt",
			  "metop2limb_sea.txt",    "n17amsusimreg.txt",  "n19regr_land.txt",
			  "metop2regr_land.txt",   "n17limb_land.txt",   "n19regr_sea.txt",
			  "metop2regr_sea.txt",    "n17limb_sea.txt"
			];

#--- Log file

my $logFile = "amsu_estimates.log";

#-=-=-=-=-=---=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Program: amsu_estimates.pl
#
# Version: 2.0
#
# Language: Perl 5.8.0
#
# Purpose: This script manages the process of doing a AMSU intensity estimate
#	   on AMSU BUFR format data from the NCEP IBM. 
#
# Called by: Command line or Cron
#
# Usage:	amsu_estimates.pl [-date YYYYMMDDHH]
#
# Examples:	amsu_estimates.pl 
#		amsu_estimates.pl -date 2004060700 
#
#	*** Note: Date should be in the GMT time zone if entered.
#		  Otherwise, if a time in not entered then the current
#		  system time and converted to GMT.
#
#-------------------------------------------------------
# Information:
#    Joint Hurricane Testbed/USWRP project
#    National Hurricane Center / Tropical Prediction Center
#    Department of Commerce - NOAA / NWS
#
# Category: JHT - CIRA AMSU
#
# Written by: Alison Krautkramer 08/2004
# Modified by: A. Kruatkramer 10/2005 
#	 	Updated to run in all basins: IO, SH, AL, EP, CP, WP
#		Stores new AMSU intensity estimates end products	
#		Modifies the acquisition process for the AMSU BUFR Data from
#			sending a job directly to the loadleveller to 
#			creating a shell script run directly from the command line.
# Modified by: A. Krautkramer 10/17/2005
#		Convert system date/time to gmt if date is not provided by 
#		the user. 
# Modified by: A. Krautkramer 01/24/2006
#               Updated atcfStorms.pl to modify the definition of a current active TC.
# Modified by: ARK 04/12/2006
#               Updated atcfStorms.pl and stormData.pl so more then one ATCF deck
#               could be searched for active storms at the same time.  Now instead of one
#               general ATCF pointer there are two, one for JTWC's decks and one for NHC's
#               decks.
# Modified by: M. Sardi 07/04/2014
#               Added leading ./ to several system calls that run local executables;
#               changed 'require' statements to full-path; replaced 'rename' command 
#               with move/copy calls.
# Modified by: M. Sardi 03/20/2019
#               Removed 'use lib "/usrx/local/pm5/share/perl5"' (now set via module pm5,
#               envar $PERL5LIB prepended to $PATH).
# Modified by: M. Sardi 10/15/2021
#               Changed envar LIST to BUFR_LIST to comport with wcoss2 module bufr_dump.
#=-==-=-=-==-=-=-=-=-=-=-=-=-==-=-=-==-=-=-=-=-=-=-=-==-=-==-=-==-=-=-=-==-=-=-=-==-=-

#---------------------------------------
# *** Set up working environment  ***
#---------------------------------------

#--- Create the Work Directory, check lock file, open log

my $LOG = new IO::File;
setupEnv ($workDir, $logFile, \$LOG);
generalUtils::header("***BEGIN amsu_estimates.pl at", $LOG); 

#--------------------------
# *** Get User Input ***
#--------------------------

my $date = getInput();

#-----------------------------------------------------------
# *** Get Storm information ***
#
#	 Figure out what storms are active and get 
#	 information about those storms.  The inforamtion
#	 will be stored in the %queryData hash.
#-----------------------------------------------------------

my %queryData;
unless (stormData::new($date, \%queryData, $LOG) == 1) {

	print ($LOG "Error (amsu_estimates): Getting query data failed.\n");
	exit 1;

}


#-----------------------------
# *** Process Storms ***
#-----------------------------

my $results = 0; #flag to signify whether any AMSU estimates were successfully completed

foreach my $queryFile (keys (%queryData)) {

	if (processStorm($queryFile, \%queryData, $LOG) == 1) {
		$results = 1;
	}
	
}

#-----------------------------
# *** Store Log ***
#-----------------------------

close $LOG;

#--- If AMSU results were produced then copy off and optionally store the log file

if ($storeLog == 1) {

	copy("$workDir/$logFile", "$storeLogDir/$date" . "_$logFile");	


}

if ($storeLog == 0 && $results == 1) {
	
	move("$workDir/$logFile", "$workDir/$date" . "_$logFile");	

}

#--------------------------------------------------------------------------------
#
#	*** 	Subroutines	***
#
#--------------------------------------------------------------------------------

#--------------------------------------------------------
# Subroutine: processStorm 
#
# Purpose:  Manages the process of creating AMSU storm
#	    intensity estimates.
#
# Called: Main
#-------------------------------------------------------
sub processStorm {

   	#-----------------------------
        # *** Define Local Variables
        #-----------------------------

	my ($queryFile, $rhQueryData, $LOG) = @_;
	my $subWorkDir = "";


        #------------------------------------------------
        #  Get the AMSU bufr files from the NCEP IBM
        #------------------------------------------------

        generalUtils::header("Get AMSU BUFR Data from NCEP IBM", $LOG);

        if (getBufrData($queryFile , $rhQueryData, \$subWorkDir, $LOG) != 1) {
	
                print ("Error (amsu_estimates): Problem occured while getting AMSU BUFR data from the NCEP IBM.\n");
                return -1;
	
        }
	
        #------------------------------------------------
        #  Convert the AMSU bufr files from bufr to ascii format
        #------------------------------------------------

        generalUtils::header("Convert AMSU data from BUFR to ASCII format", $LOG);

        if (convertBufToAsc ($subWorkDir, $LOG) != 1) {

                print ("Error (amsu_estimates): Problem occured while converting AMSU BUFR data to PACK data.\n");
                return -1; 

        }


        #------------------------------------------------
        #  Perform the temperature retrieval
        #------------------------------------------------

        generalUtils::header("Perform Temperature Retrieval", $LOG);

        if (temperatureRetrieval($subWorkDir, $LOG) != 1) {

                print ("Error (amsu_estimates): Problems occured while performing the temperature retrieval.\n");
                return -1; 

        }

	
        #------------------------------------------------
        #  Estimate intensity
        #------------------------------------------------

	generalUtils::header("Run the AMSU Intensity Estimation Algorithm", $LOG);

        if (intensityEstimate::new($subWorkDir, $LOG) != 1) {

                print ("Error (amsu_estimates): Problems occured while running the intensity estimatation algorithm.\n");
                return -1;

        }


        #------------------------------------------------
        # Store results
        #------------------------------------------------
	
	my $identifier = $$rhQueryData{$queryFile}{stormName} . "_" . $$rhQueryData{$queryFile}{dtg};

        if ($storeFixText == 1 || $storeFixATCF || $storeData == 1) {
		
		print ("Storing files from $subWorkDir into $identifier \n");
                storage($subWorkDir, $rhQueryData, $queryFile, $identifier, $LOG);

        }


        return 1;

}


#--------------------------------------------------------
# Subroutine: getBufrData
#
# Purpose:  This subroutine manages creating the IBM job, requesting data
#	    from the IBM and making sure the data is recieved from the IBM before
#	    the script returns.  
#			
#	    Different storms are keep separate in the working subdirectory by 
#	    date/time group and geographic area.
# 
#
# Called by: main
#
# Calls: qyeryDB()
#	 fileTransfer()
#	 init_subDirectory()
#
# Inputs: $queryFile (string) Name of a file containing geographic information 
#			      used to query the IBM database
#	  $rhQueryData (ref to createQueryData object) - Object storm data
#	  $rWorkSubDir (ref to a string) -  Unique directory which will be created
#					    to store output
#	  $LOG  - A reference to the log's file handle
#		 
# Outputs:  1 - success
#
# Errors:  -1 - error creating work directory
#	   -2 - error creating IBM job
#	   -3 or -4 - problem getting ibm data
#
#--------------------------------------------------------
sub getBufrData {

	#-----------------------------
	# *** Define Local Variables
	#-----------------------------

	my ($queryFile, $rhQueryData, $rSubDir, $LOG) = @_;


	#-----------------------------------------------
        # *** Create a unique sub directory for results 
        #-----------------------------------------------
	
	unless (init_subDirectory($queryFile, $rhQueryData, $rSubDir, $LOG) == 1) {

		printf ($LOG "Error (amsu_estimates::getBufrData): Problems Creating $$rSubDir\n");
                return -2;

	}
	
	#----------------------------------
	# *** Create the new IBM job 
	#----------------------------------
	
	#if (queryDB($$rSubDir, \%{$$rhQueryData{$queryFile}}, $LOG) != 1 ) {
	#
        #       printf ($LOG "Error (amsu_estimates::getBufrData): Problems Creating $$rSubDir\n");
        #       return -2;
	#
        #}
        
       
	#----------------------------------
	# query NOAA 15-19 and METOP       
	# -------------------------------- 
        my $queryFileName = "queryNOAA.ksh";

        if (queryDB($$rSubDir, \%{$$rhQueryData{$queryFile}}, "NOAA", $queryFileName, $LOG) != 1 ) {

               printf ($LOG "Error (amsu_estimates::getBufrData): Problems Creating $$rSubDir\n");
               return -2;

        }
 
	#-------------------------------
	# *** Submit NCEP IBM Job 
	#-------------------------------

	#chmod (0766, "$$rSubDir/query.ksh");
	#system("cd $$rSubDir; $$rSubDir/query.ksh");
	chmod (0766, "$$rSubDir/$queryFileName");
	system("cd $$rSubDir; $$rSubDir/$queryFileName");
	
	#-------------------------------
	# *** Verify File Transfer
	# ------------------------------
	
	#--- Check for existance of IBM log file
	
	if (generalUtils::checkFileTransfer($$rSubDir, $ibmLogFile, $totalWait) != 1) {
	
		printf ($LOG "Error (amsu_estimates::getBufrData): File $$rSubDir/$ibmLogFile not found.\n");
		return -3;
	
	}
	
	#--- Check for existance of IBM BUFR data
	
	#if (generalUtils::checkFileTransfer($$rSubDir, $ibmBufrData, $totalWait) != 1) {
	#	printf ($LOG "Error (amsu_estimates::getBufrData): File $$rSubDir/$ibmBufrData not found.\n");
	#	return -4;
	#}
	
	#printf ($LOG "Finished IBM job for AMSUA $$rhQueryData{$queryFileName}{stormName} at $$rhQueryData{$queryFileName}{location} " .  localtime() . " \n\n");

	#----------------------	
	# .... query AQUA
	# --------------------
	
##	$queryFileName = "queryAQUA.ksh";

##        if (queryDB($$rSubDir, \%{$$rhQueryData{$queryFile}}, "AQUA", $queryFileName, $LOG) != 1 ) {

##               printf ($LOG "Error (amsu_estimates::getBufrData): Problems Creating $$rSubDir\n");
##               return -2;

##        }
	
	#-------------------------------
	#   *** Submit NCEP IBM Job
	#-------------------------------

##  	  chmod (0766, "$$rSubDir/$queryFileName");
##        system("cd $$rSubDir; $$rSubDir/$queryFileName");
	
	 #-------------------------------
	 #  *** Verify File Transfer
	 # ------------------------------

	 #--- Check for existance of IBM log file

##	 if (generalUtils::checkFileTransfer($$rSubDir, $ibm2LogFile, $totalWait) != 1) {

##                printf ($LOG "Error (amsu_estimates::getBufrData): File $$rSubDir/$ibm2LogFile not found.\n");
##                return -3;

##         }

	 #--- Check for existance of IBM BUFR data
	      
	 #if (generalUtils::checkFileTransfer($$rSubDir, $ibm2BufrData, $totalWait) != 1) {
                #printf ($LOG "Error (amsu_estimates::getBufrData): File $$rSubDir/$ibm2BufrData not found.\n");
                #return -4;
         #}

        printf ($LOG "Finished IBM job for $$rhQueryData{$queryFile}{stormName} at $$rhQueryData{$queryFile}{location} " .  localtime() . " \n\n");

        return 1;

}


#--------------------------------------------------------
# Subroutine: init_subDirectory
#
# Purpose:  Sets up a unique sub-directory in the working directory.
#	    This allows the possibility of adding threads 
#	    and running more then 1 storm at a time.
#
#----------------------------------------------------------
sub init_subDirectory {
  
	#-----------------------------
        # *** Define Local Variables
        #-----------------------------

        my ($queryFile, $rhQueryData, $rSubDir, $LOG) = @_;


  	#--------------------------------------------------
        # *** Create a subdirectory for process results
        #--------------------------------------------------

        $$rSubDir = "$workDir/$type." . $$rhQueryData{$queryFile}{dtg} .
                        ".$radius/G" .  $$rhQueryData{$queryFile}{location};

        #---Check for the previous existance of subdirectory and delete if found

        if (-d $$rSubDir) {
                system ("rm -r $$rSubDir");
        }

        #--- Make sub directory for processing

        system("mkdir -p $$rSubDir");

        unless (-d $$rSubDir) {

                print ($LOG "Error (amsu_estimates::getBufrData): Directory $$rSubDir could not be created.\n");
                return -1;

        }


        #--------------------------------------------------
        # ***  Copy COORDINATES file and Move dumpj_in_##
        #--------------------------------------------------

        copy("$workDir/$queryFile", "$$rSubDir/$queryFile");
        copy("$workDir/$FILENAME2", "$$rSubDir/$FILENAME2");

        #--- Remove any old ibm files

        unlink("$$rSubDir/$ibmBufrData");
        unlink("$$rSubDir/$ibmLogFile");
        #unlink("$$rSubDir/$ibmjob");

	return 1;

}


#--------------------------------------------------------
# Subroutine: queryDB 
#
# Purpose:  
#	    Creates a shell script containing a series of commands that are 
#	    used to obtain AMSU data from the NCEP IBM database. 
#
# Called by: getBufrData()
#
# Inputs: $workSubDir - (string) - directory to store the ibm job and results
#	  $queryFile - (string) - file name of the query information file
#	  $rhQueryData - (ref to hash) - data structure containing data used to 
#					 query IBM 
#	  $LOG - (ref to file) - file for log output
#
# Outputs: 1 - success
#	       output file containing ibm job statements
#
# Errors:   -1 - Can't open file
#--------------------------------------------------------
sub queryDB {

	#-----------------------------
        # *** Define Local Variables
        #-----------------------------

	my ($workSubDir, $rhQueryData, $bufrType, $queryFileName, $LOG) = @_;

	my $storm = $$rhQueryData{stormName};
        my $dtg = $$rhQueryData{dtg};
        my $geo = $$rhQueryData{location};

	#--------------------------------
	# *** Open file to write IBM job
	#-------------------------------

        my $OUTPUT = new IO::File;

        #if (generalUtils::openFile ($workSubDir, "query.ksh", "write", \$OUTPUT) != 1) {
       	if (generalUtils::openFile ($workSubDir, $queryFileName, "write", \$OUTPUT) != 1) { 

                print ($LOG "Error (amsu_estimates::queryDB): Creating IBM Job File.\n");
                return -1;

        }

	print ($OUTPUT "#!/bin/ksh \n");	
        printf ($OUTPUT "DATE=$dtg  # The center date for the dump\n");
        printf ($OUTPUT "RADIUS=$radius       # The dump radius i.e., the number of hours either side of\n");
        #printf ($OUTPUT "TYPE=$type      #    NOAA-15 and NOAA-16 AMSU-A\n");
        if ($bufrType eq "NOAA") {
           printf ($OUTPUT "TYPE=$type      #    NOAA-15 and NOAA-16 AMSU-A\n");
        }
        else {
##           printf ($OUTPUT "TYPE=$type2     #    AQUA\n");
        }
	# JY printf ($OUTPUT "DUMP=/nwprod/ush/dumpjb\n");
	printf ($OUTPUT "DUMP=$DUMPJB\n");
	printf ($OUTPUT "export LALO=$geo\n");
        printf ($OUTPUT "export DATA=$workSubDir\n");
        printf ($OUTPUT "export TANK=$ibmTank\n");
        printf ($OUTPUT "export LIST=$ibmDBdef\n");
	printf ($OUTPUT '$DUMP $DATE $RADIUS $TYPE' . "\n\n");
	
	$OUTPUT->close;
	return 1;
}


#--------------------------------------------------------
# Subroutine: convertBufrToAsc
#
# Purpose:  Converts the AMSU data in bufr format into ascii format.
#
#	The conversion code readbufr is copied into the directory
#	that contains query results from the NCEP IBM 
#	(1bamua.out, 1bamua.ibm).  The readbufr code will create 
#	three ascii files from the ibm bufr data; noaa15.dat, 
#	noaa16.dat, noaa18.dat.
#
# Called by: main
#
# Calls: readbufr - external program written by CIRA to convert the
#		   BUFR data format into ASCII text files
#
# Inputs: $subWorkDir - storm specific working directory
#	  $LOG - file handle to the log file
#		 
# Outputs:  1 - success
#
# Errors:   -1 - Conversion code was not found or correctly copied into
#		 the appropriate subdirectory
#
#--------------------------------------------------------
sub convertBufToAsc {

	#-----------------------------
	# *** Define Local Variables
	#-----------------------------
	
	my ($subWorkDir, $LOG) = @_;
 
	
	#-----------------------------
	# *** Copy and Run CIRA Conversion Code
	#-----------------------------
	
	copy ("$ExecDir/$bufrToAsc", "$subWorkDir/$bufrToAsc") || return -1;
	chmod (0744, "$subWorkDir/$bufrToAsc") || return -1; # or fatal ("Error ($!): Unable to make executable.", $LOG);
	#system("cd $subWorkDir; ./$bufrToAsc") && return -1; # and fatal ("Error ($!): Unable to execute $exe. \n", $LOG);

	#--- Run amuata.ibm
	if (-s "$subWorkDir/$ibmBufrData") {	
		system("cd $subWorkDir; ./$bufrToAsc $ibmBufrData") && return -1; # and fatal ("Error ($!): Unable to execute $exe. \n", $LOG);
	}
	#--- Run airsev.ibm
##	if (-s "$subWorkDir/$ibm2BufrData") {
##		system("cd $subWorkDir; ./$bufrToAsc $ibm2BufrData") && return -1; # and fatal ("Error ($!): Unable to execute $exe. \n", $LOG);
##	}
	
	#-----------------------------
	# *** Check Results of Conversion Code
	#-----------------------------
	
	print ($LOG "Finished BUFR to ascii conversion.\n");


        #-----------------------------
        # *** Check Results of Retrieval Code
        #-----------------------------

        return checkResults($subWorkDir, $ascii_files);
 
} 


#----------------------------------------------------------------------------
# Subroutine: temperatureRetrieval
#
# Purpose: Performs a temperature retrieval using AMSU data in ascii format
#	   and the coefficient files.
# 
#	   To run the temperature retrieval code the 
#	   coefficient files and the temperature retireval code needs to
#	   be copied into the directory with the noaa##.out (ascii) files.
#
# Called by: main 
#
# Calls: temp_ret - external program written by CIRA to perform the temperature
#			retrieval
#
# Inputs: $subWorkDir - storm specific working directory
#	  $LOG - file handle to the log
#		 
# Errors:   -1 - Temperature retrieval code was not found or correctly copied into
#		the appropriate subdirectory
#	    -2 - Coefficient files missing
#----------------------------------------------------------------------------	
sub temperatureRetrieval {
	
	#-----------------------------
	# *** Define Local Variables
	#-----------------------------
	
	my ($subWorkDir, $LOG) = @_;
	
	
	#---------------------------------------------
	#  Copy coefficent files to working directory 
	#---------------------------------------------
	
	foreach my $file (@$coefficient_files) {
	
		copy ("$FixDir/amsu_$file", "$subWorkDir/amsu_$file") || return -2;
	
	}
	
	
	#---------------------------------------------
	#  Copy CIRA retrieval programs to working directory
	#---------------------------------------------
	
	copy ("$ExecDir/$tempRetrieval", "$subWorkDir/$tempRetrieval") || return -1;
	chmod (0744, "$subWorkDir/$tempRetrieval") or return -1; # or fatal ("Error ($!): Unable to make executable.", $LOG);
	system("cd $subWorkDir; ./$tempRetrieval") and return -1; # and fatal ("Error ($!): Unable to execute $exe. \n", $LOG);
	
	
	#---------------------------------------------
	#  Remove coefficent files from working directory 
	#---------------------------------------------
	
	foreach my $file (@$coefficient_files) {
	
		unlink ("$subWorkDir/$file");
	
	}

	
	#-----------------------------
	# *** Append and remove Logs
	#-----------------------------
	
	foreach my $log ("noaa15.log", "noaa16.log", "noaa18.log") {
	
		print ($LOG substr($log, 0, 6) . " Temperature Retrieval .....\n\n");
		generalUtils::appendFile($subWorkDir, $log , $LOG);
		unlink("$subWorkDir/$log");
		print ($LOG "\n");
	
	}
	
	print ($LOG "Finished Temperature retrieval.\n");

		
	#-----------------------------
	# *** Check Results of Retrieval Code
	#-----------------------------

	return checkResults($subWorkDir, $retrieval_files);
	
}


#--------------------------------------------------------
# Subroutine: checkResults 
#
# Purpose: Checks the given directory and makes sure that at least
#	   one of the result files exists and has non-zero size.
#
# Called by: temperature retrieval 
#--------------------------------------------------------
sub checkResults {

	my ($dir, $raFiles) = @_;

	#-----------------------------
        # *** Check Results of Retrieval Code
        #-----------------------------

        my $found = 0;

        foreach my $file (@$raFiles) {

                unless (generalUtils::isFile($dir, $file) == 1) {

			printf ($LOG "Error (amsu_estimates::checkResults): Result file $dir/$file was not found or has zero size. \n");
                        next;

                }

                $found = 1;

        }


	#-----------------------------
        # *** Return success or failure 
        #-----------------------------

        if ($found == 0) {

                printf ($LOG "Error (amsu_estimates::temperatureRetrieval): No retrievals found. \n");
                return -1;

        }

	return 1;
}


#--------------------------------------------------------
# Subroutine: storage 
#
# Purpose: Creates the name of a directory to store the
#	   results.  Then calls the storeFiles subroutine
#	   which moves the array of files to the 
#	   storage directory. 
#
# Calls: storeFiles()
#------------------------------------------------------------
sub storage {

        #-----------------------------
        # *** Define Local Variables
        #-----------------------------

        my ($subWorkDir, $rhQueryData, $queryFile, $id, $LOG) = @_;
        
	#-----------------------------
        # *** Store Fix Text files
        #-----------------------------

        if ($storeFixText == 1) {
                
		my $stormDir = "$storeFixTextDir/$$rhQueryData{$queryFile}{stormName}";
		storeFiles ($stormDir, $subWorkDir, $fix_files, $id, $LOG);

	}

	#-----------------------------
        # *** Store Fix ATCF files
        #-----------------------------

        if ($storeFixATCF == 1) {
                
		my $stormDir = "$storeFixATCFDir/$$rhQueryData{$queryFile}{stormName}";
		storeFiles ($stormDir, $subWorkDir, $atcf_files, $id, $LOG);
         
        }

        #-----------------------------
        # *** Store Data Files
        #-----------------------------

        if ($storeData == 1) {

                my $stormDir = "$storeDataDir/$$rhQueryData{$queryFile}{stormName}";
		my $data_files = [$ibmBufrData, $FILENAME2, $queryFile]; 
##	        my $data_files = [$ibmBufrData, $ibm2BufrData, $FILENAME2, $queryFile];	
               	storeFiles ($stormDir, $subWorkDir, $data_files, $id, $LOG); 
		storeFiles ($stormDir, $subWorkDir, $xya_files, $id, $LOG);
		storeFiles ($stormDir, $subWorkDir, $rza_files, $id, $LOG);
		storeFiles ($stormDir, $subWorkDir, $sta_files, $id, $LOG);
		storeFiles ($stormDir, $subWorkDir, $loc_files, $id, $LOG);
		storeFiles ($stormDir, $subWorkDir, $retrieval_files, $id, $LOG);
        
	}

        #--------------------------
        # Send out the data to TOC
        #--------------------------
        if ($senddbn =~ /YES/) {
                my $stormDir = "$storeDataDir/$$rhQueryData{$queryFile}{stormName}";
                alertFiles ($stormDir, $retrieval_files, $id, $alerttype_ret, $LOG);
                alertFiles ($stormDir, $atcf_files, $id, $alerttype_atcf, $LOG);
                alertFiles ($stormDir, $fix_files, $id, $alerttype_fix, $LOG);
                alertFiles ($stormDir, $xya_files, $id, $alerttype_xya, $LOG);
                alertFiles ($stormDir, $rza_files, $id, $alerttype_rza, $LOG);
                alertFiles ($stormDir, $sta_files, $id, $alerttype_sta, $LOG);
                alertFiles ($stormDir, $loc_files, $id, $alerttype_loc, $LOG);
        }
}

#----------------------------------------------------------------------
# Subroutine: storeFiles
#
# Purpose: Moves a list of files from the current diretory
#	   $subWorkDir to a storage directory.  The 
#	   files are stored in a sub-directory by storm name.
#
# Called by: storage() 
#------------------------------------------------------------------------
sub storeFiles {

	#-----------------------------
        # *** Define Local Variables
        #-----------------------------

	my ($stormDir, $subWorkDir, $raFiles, $id, $LOG) = @_;


	#-----------------------------
	# *** Check if a storm directory exists
	#-----------------------------

        unless (-d $stormDir) {
             
		system("mkdir -p $stormDir \n");

        }


	#-----------------------------
        # ***  Store files
	#-----------------------------

        foreach my $file (@$raFiles) {

             copy("$subWorkDir/$file", "$stormDir/" . $id . "_$file" );
             print ($LOG "Store $stormDir/" . $id . "_$file");

        }

}

#----------------------------------------------------------------------
# Subroutine: alertFiles
#
# Purpose: Use DBN alert to send out products to TOC
#
# Called by: storage() 
#------------------------------------------------------------------------
sub alertFiles {

        # *** Define Local Variables

	my ($stormDir, $raFiles, $id, $alerttype,$LOG) = @_;

        # ***  Alert files

        foreach my $file (@$raFiles) {

          my $filname = "$stormDir/" . $id . "_$file";
          `$dbnroot/bin/dbn_alert MODEL $alerttype $job $filname`;  
          print ($LOG "Successfully sent out $stormDir/" . $id . "_$file");

        }
}

#--------------------------------------------------------
# Subroutine: getInput 
#
# Purpose: Checks if the user included a desired date.  If
#	   they did then use that date however if either
#	   the date is in an invalid format or 
#	   a date was not given then set the date to the current
#	   date/time. 
#
# Modified: ARK 10/11/2005 - Convert Today from system time zone to gmt
#------------------------------------------------------------
sub getInput {

     #-----------------------------
     # *** Check input for valid date
     #------------------------------

     if (defined($ARGV[0])) {

        if ($ARGV[0] eq "date" || $ARGV[0] eq "-date") {

             if (defined($ARGV[1]) && generalUtils::validDateFormat($ARGV[1]) == 1) {

                 return $ARGV[1];

             }

     	}
       
        if (generalUtils::validDateFormat($ARGV[0]) == 1) {

           return $ARGV[0];

        }

   } 

   #---------------------------------------------------
   # *** Set date to the current system time 
   #---------------------------------------------------
 
   my $date= Date::Manip::ParseDate("today");
   $date = Date_ConvTZ($date, "", "GMT"); 
   $date = substr($date, 0, 10);
   print ("Setting Date to current date/time to $date.\n");

   return $date;

}


#--------------------------------------------------------
# Subroutine: usage
#
# Purpose:  Prints out a command syntax statement
#------------------------------------------------------------
sub usage {

        printf ("Usage: amsu_estimates.pl [-date YYYYMMDDHH] \n");
	printf ("       date -> GMT time zone \n");

}


#--------------------------------------------------------
# Subroutine: setupEnv
#
# Purpose: Sets up the environment needed to run this program including:
#
#	- Checking for the existance of the working directory.
#	  If the working directory does not exist then create
#	  create one.
#	- Open a log file
#
# Called by: main
#--------------------------------------------------------
sub setupEnv {


	#-----------------------------------
	# *** Define Variables ***
	#-----------------------------------
	
	my ($workDir, $logfile, $rLOG) = @_;
	
	
	#-----------------------------------
	# *** Create WORK Directory ***
	#-----------------------------------

	if (!(-d $workDir)) {
	
		system ("mkdir $workDir") && die ("Error: Can't create working directory $workDir.\n");
	}


	#----------------------------
	# *** Open Log file ***
	#---------------------------

	unless (generalUtils::openFile($workDir, $logfile, "write", $rLOG) == 1) {
	
		printf ("Error (amsu_estimates::setupEnv): $workDir/$logfile could not be opened for writing.\n");
	
	}

}
