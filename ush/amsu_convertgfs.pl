#! /usr/bin/env perl

use 5.008;
use File::Copy;
use IO::File;
use strict;

push(@INC, "$ENV{'HOMEnhc'}/ush");
push(@INC, "$ENV{'HOMEnhc'}/exec");

my $HomeDir = $ENV{'HOMEnhc'};

my $packDir = $ENV{'COMOUT'};
my $logDir = $packDir;

my $gfsGb2 = $ENV{'COMINgfs'};
#my $gfsDir = "$ENV{'COM'}/amsu";
my $gfsDir = "$ENV{'COM'}";
my $CC = $ENV{'CYC'};

my $envir = $ENV{'envir'};
my $COMIN = $ENV{'COMIN'};

my $ExecDir = "$HomeDir/exec";
my $UshDir = "$HomeDir/ush";
my $FixDir = "$HomeDir/fix";

my $workDir = "$ENV{'DATA'}";

require "$UshDir/nhc_generalutils.pl";

#-=-=-=-=-=---=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Program: convertGFS.pl
#
# Purpose:  This script finds GFS grib files that have not been converted into 
#	    PACK format and converts them.
#
# Called by: Cronjob every 3 hours 45 minutes after the hour
#
#	 ###Convert GFS grib data into PACK files for CIRA AMSU
#	45 0,3,6,9,12,15,18,21 * * * convertGFS.pl
#
#---------------------------------------
# Program Calls: 		(SEE README.gfsToPack for more detail)
#
#
#***** compgfs   
#	
#	Purpose: Determines if there is a new GFS files to convert by comparing
#	         the latest GFS files on the server with the latest stored 
#		 pack files. 
#
#******* pullvar
#
#	Purpose: Creates a shell script, pullvar_nhc.ksh, which pulls appropriate
#  		  fields from GFS grib file
#
#****** wgrib
#	
#	Purpose: wgrib is a program that creates binary files of data from pullvar_nhc.exe
#
#**** bintrans	
#
#	Purpose: Converts fields from *.bin format to *.gtmp 
#
#***** bin2pack	
#
#	Purpose: Converts *.gtmp files to *PACK.DAT file
#
#-----------------------------------------------------
# Information:
#    Joint Hurricane Testbed/USWRP project
#    National Hurricane Center / Tropical Prediction Center
#    Department of Commerce - NOAA / NWS
#
# Category: JHT - AMSU
#
# Written by:  Jack Dostalek CSU/CIRA/RAMM 03/31/03
# Modified by: A. Krautkramer 03/25/2003 Converted from shell program to perl
# Modified by: A. Krautkramer 04/28/2003 Added documentation
# Modified by: A. Krautkramer 06/02/2003 Cleaned up format, added documentation,
# Modified by: A. Krautkramer 07/27/2004 Rewrote for operations
# Modified by: ARK 02/14/2005 Rewritten for IBM
# Modified by: ARK 03/28/2005 removed gfs_config.pl file
# Modified by: M. Sardi 07/07/2014 Changed 'require' statements to full-path; replaced
#                                  'rename' command with move/copy calls.
# Modified by: M. Sardi 02/10/2016 Added logic to convert grib2 gfs files to grib1 using 
#                                  cnvgrib; long term, need to replace wgrib with wgrib2.
# Modified by: M. Sardi 03/20/2019 Replaced 'utilexec' paths with module defined $CNVGRIB,
#                                  $WGRIB envars for Phase 3 migration. 
#
#=-=-=-=-==-=-=-==-=-=-=-=-=-=-=-=-==-=-=-==-=-=-=-=-=-=-=-==-=-==-=-==-=-=-=-==-=-=-=-==-=-



#=====================================
#       ***  Intermetidate Files
#=====================================

my $currentGFS = "srvgfs.list";
my $currentPack = "sysgfs.list";
my $compareData = "compgfs_ibm.dat";

#=====================================
#       ***  Executables
#=====================================

my $compareExe = "amsu_compgfs";
my $extractExe = "amsu_pullvar";
my $extractShellExe = "pullvar_nhc.ksh";
my $wgribExe = "wgrib";
my $cnvgribExe = "cnvgrib";
my $binToGtmpExe = "amsu_bintrans";
my $gtmpToPackExe = "amsu_bin2pack";

#=====================================
#       *** Log Files
#=====================================

my $compLogFile = "compgfs_ibm.log";
my $extractLogFile = "pullvar_nhc.log";
my $binToGtmpLog = "bintrans_nhc.log";
my $gtmpToPackLog = "pack_nhc.log";

#=====================================
#       *** Pack File variables
#=====================================

my $packFileData = "packfile_nhc";
my $packSize = 23410;

#===================================
# *** Set Variables / Definitions***
#===================================

my $logFile = "getgfs_ibm.log";
my $lockFile = "lock";

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                       ***  MAIN ***
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

#===============================================
# *** Set up working environment  ***
#===============================================

#--- Create the Work Directory, check lock file, open log

my $LOG = new IO::File;
setupEnv ($workDir, $logFile, $lockFile, \$LOG);

#============================================================
# *** Find unprocessed GFS grib files  ***
#
#  Use the compiled fortran executable, $compareExe,
#  to compare the the list of current GFS grib files 
#  to a list of current GFS pack files.  Identify the 
#  latest model file and associated information to convert. 
#=============================================================

#--- Print log message

generalUtils::header("***BEGIN convertGFS.pl at", $LOG);

#--- Copy executable to working directory

copy("$ENV{'CNVGRIB'}", $workDir) or 
	fatal ("Error ($!): Unable to successfully copy $cnvgribExe.\n", $LOG);
	
#--- Make the code executable and execute

chmod (0744, "$workDir/$cnvgribExe") or fatal ("Error ($!): Unable to make executable.", $LOG);

#--- Get a list of GFS grib files

unless( create_GFSlist($LOG) == 1) {fatal ("Error ($!) creating a list of current gfs.F000 files", $LOG)};

#--- Get a list of pack files

unless ( create_PACKlist($LOG) == 1) {fatal ("Error ($!) creating a list of current G*_PACK.DAT files", $LOG)};

#--- Print log message

generalUtils::header("***BEGIN $compareExe at", $LOG);

#--- Execute Code to find unprocessed GFS grib files 

executeCode($compareExe, $compLogFile, $LOG);


#===============================================
#*** Get GFS grib file information
#
#  Read the file created by $compareExe.
#  If the variable "command" = 'exit' then 
#  there are no new GFS files to process so 
#  exit this script.  If command is set to 
#  anything else then continue processing 
#  the new GFS file found.
#================================================

#--- Read the output of compgfs_nhc.exe

my ($command, $ymd, $gfsFile, $run);

if (readCompareFile ($workDir, $compareData, \$command, \$ymd, \$gfsFile, \$run, $LOG) != 1) {

	fatal("Error (convertGFS::main): $workDir/$compareData could not be opened for reading.\n", $LOG);
	
}

#--  If the command equals exit then no more processing will be done

if ($command eq 'exit') {

	fatal ("No new GFS files on server.\n", $LOG);
	
}


#=========================================
# *** Pull Data from GFS Grib file ***
#
#  Execute the f90 code to pull/extract
#  prespecified variables from the GFS grib files.
#  The extractExe script creates a ksh
#  which when executed will actually perform the 
#  extraction.
#=========================================

#---  Exporting GFS file path/name to fortran files

$ENV{"GFSFILE"} = "$gfsDir/$gfsFile";

#--- Print log message

generalUtils::header("Begin $extractExe at", $LOG); 

#--- Execute Code to find unprocessed GFS grib files 

executeCode($extractExe, $extractLogFile, $LOG);	

#--- Copy executable to working directory

copy("$ENV{'WGRIB'}", $workDir) or 
	fatal ("Error ($!): Unable to successfully copy $wgribExe.\n", $LOG);
	
#--- Make the code executable and execute

chmod (0744, "$workDir/$wgribExe") or fatal ("Error ($!): Unable to make executable.", $LOG);
executeCode($extractShellExe, $extractShellExe, $LOG);


#=========================================
# *** Convert *.bin files to *.gtmp ***
#=========================================

#---  Exporting the name and path to the avn file
#---  so it can be passed to the f90 file $binToGtmpExe

$ENV{"YMD"} = "$ymd";
$ENV{"RUN"} = "$run";
$ENV{"FHOUR"} = "000";

#--- Print log Message

generalUtils::header("Begin $binToGtmpExe at", $LOG); 

#--- Execute Code to convert binary file to GTMP files

executeCode($binToGtmpExe, $binToGtmpLog, $LOG);


#==================================================
# *** Create PACK files from  *.gtmp files ***
#==================================================

#--- Print log Message

generalUtils::header("Begin $gtmpToPackExe at", $LOG); 

#--- Execute Code to convert binary file to GTMP files

executeCode($gtmpToPackExe, $gtmpToPackLog, $LOG);


#===================================================
# *** Check Pack file size ***
#
#	Find the name of the created Pack file and  
#	check the line count. If the line count
#	does not match the expected line count 
#	then delete the file.  Otherwise, copy the file 
#	to the result directory
#===================================================

my $packFile;

if (checkPackfile($LOG, \$packFile) == 1) {

	#--- Copy the packFile to the appropriate storage directory
	
	copy("$workDir/$packFile", "$packDir/$packFile") or 
		fatal ("Error ($!): Unable to successfully copy $packFile to $packDir.\n", $LOG);
	
	#--- Close the log file and copy the file to the appropriate storage directory
	
	close($LOG);
	my $newlogFile = $logFile . "_" . $ymd . "_" . $run;
	copy("$workDir/$logFile", "$logDir/$newlogFile");

}

generalUtils::header("***End $compareExe at", $LOG);


#=================================
# *** Cleanup ***
#=================================

close($LOG);
# < Commented out by MAS, NHC > clean($packFile);
	
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                       ***  SUBROUTINES ***
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

#--------------------------------------------------------
# Subroutine: create_GFSlist
#
# Purpose: create_GFSlist creates a list of GFS files on the IBM.
#	   
#	   The GFS files on the IBM are stored in a directory structure with
#	   the following format ....
#	
#		gfs.YYYYMMDD/gfs.tHHZ.pgrbfFF
#
#	   Note: In contrast, at NHC all the gfs grib files were stored
#	   in one generic directory and used the following naming convension.
#
#		NHC: yyyymmddhh_gfs.fHHH
#  	
#		
#--------------------------------------------------------

sub create_GFSlist {

	#---------------------------------------------
	#	 Define Variables
	#---------------------------------------------	

	my ($LOG) = @_;
	my @files = ();

	#----------------------------------------------------------	
	#	Identify the GFS files for the current day	
	#---------------------------------------------------------

	my $dateString = Date::Manip::ParseDate("today");
	my $today = Date::Manip::UnixDate($dateString, '%Y%m%d');

	#$today = "20210824";

	my $dir = "gfs." . $today;
	my @currentFiles = ();

        my $gb1file = "";

	if (-d "$gfsGb2/$dir") {

               # generalUtils::findFiles("$gfsGb2/$dir", '\bgfs.t[0-9]{2}z.pgrb2.1p00.f000$', \@currentFiles);
               generalUtils::findFiles("$gfsGb2/$dir/00/atmos", '\bgfs.t[0-9]{2}z.pgrb2.1p00.f000$', \@currentFiles);
               generalUtils::findFiles("$gfsGb2/$dir/06/atmos", '\bgfs.t[0-9]{2}z.pgrb2.1p00.f000$', \@currentFiles);
               generalUtils::findFiles("$gfsGb2/$dir/12/atmos", '\bgfs.t[0-9]{2}z.pgrb2.1p00.f000$', \@currentFiles);
               generalUtils::findFiles("$gfsGb2/$dir/18/atmos", '\bgfs.t[0-9]{2}z.pgrb2.1p00.f000$', \@currentFiles);
	       if (!(-d "$gfsDir/$dir")) {
	          system ("mkdir $gfsDir/$dir") && die ("Error: Can't create working directory $gfsDir/$dir.\n");
               }
               

        }

    if (@currentFiles == 0) {
        print("WARNING:  No GFS files were found in $gfsGb2/$dir\n");
    }
    else
    {
       print("The current day GFS files list are \n@currentFiles\n"); 
    } 

	foreach my $gb2file (@currentFiles) {

                $gb1file = substr($gb2file, 0, 13) . "f00"; 
	        if (!(-e "$gfsDir/$dir/$gb1file")) {
                   system ("$workDir/$cnvgribExe -g21 $gfsGb2/$dir/[0-9][0-9]/atmos/$gb2file $gfsDir/$dir/$gb1file") && die ("ERROR: Converting $gb2file to grib1.\n");
                }
		push (@files, "$dir/$gb1file");

	}

	#----------------------------------------------------------
        #       Identify the GFS files for the previous day
        #---------------------------------------------------------

	$dateString = Date::Manip::DateCalc($dateString, "- 1 day");
	my $yesterday = Date::Manip::UnixDate($dateString, '%Y%m%d');

	#$yesterday = "20210823";

	$dir = "gfs." . $yesterday;
	my @previousFiles = ();

	if (-d "$gfsGb2/$dir") {

               generalUtils::findFiles("$gfsGb2/$dir/00/atmos", '\bgfs.t[0-9]{2}z.pgrb2.1p00.f000$', \@previousFiles);
               generalUtils::findFiles("$gfsGb2/$dir/06/atmos", '\bgfs.t[0-9]{2}z.pgrb2.1p00.f000$', \@previousFiles);
               generalUtils::findFiles("$gfsGb2/$dir/12/atmos", '\bgfs.t[0-9]{2}z.pgrb2.1p00.f000$', \@previousFiles);
               generalUtils::findFiles("$gfsGb2/$dir/18/atmos", '\bgfs.t[0-9]{2}z.pgrb2.1p00.f000$', \@previousFiles);
	       if (!(-d "$gfsDir/$dir")) {
	          system ("mkdir $gfsDir/$dir") && die ("ERROR: Can't create working directory $gfsDir/$dir.\n");
               }

        }

    if (@previousFiles == 0) {
        print("WARNING:  No GFS files were found in $gfsGb2/$dir\n");
    }
    else
    {
       print("The previous GFS files list are \n@previousFiles\n");
    }

	foreach my $gb2file (@previousFiles) {

                $gb1file = substr($gb2file, 0, 13) . "f00"; 
	        if (!(-e "$gfsDir/$dir/$gb1file")) {
                   system ("$workDir/$cnvgribExe -g21 $gfsGb2/$dir/[0-9][0-9]/atmos/$gb2file $gfsDir/$dir/$gb1file") && die ("ERROR: Converting $gb2file to grib1.\n");
                }
                push (@files, "$dir/$gb1file");

        }

	#---------------------------------------------
	# 	  Check Array
	#---------------------------------------------

	if (@files > 0) {
	
		return createFile($workDir, $currentGFS, \@files);

	}

	print ($LOG "Error (convertGFS::create_GFSlist): No GFS grib1 files found in $gfsDir/$dir.\n");
	return -1;

}


#--------------------------------------------------------
# Subroutine: createFile
#
# Purpose:  Writes the list of data contained in $raFiles to
#	    an output file. 
# Input:
#	    $dir (string) - directory to place new file in
#	    $file (string) - name of new file to create
#	    $raFiles (ref array) - ref to an array of strings to 
#			write to the new file
#--------------------------------------------------------

sub createFile {

	#---------------------------------------------
        #        Define Variables
        #---------------------------------------------

	my ($dir, $file, $raFiles) = @_;

	#---------------------------------------------
        #         Write elements of $raFiles to new file
        #---------------------------------------------

	my $OUTPUT = new IO::File;

        if (generalUtils::openFile($dir, $file, "write", \$OUTPUT) != 1) {

                print ($LOG "Error (convertGFS::createFile): $dir/$file could not be opened for reading.\n");
                return -1;

        }

        foreach my $element (@$raFiles) {

                print ($OUTPUT "$element\n");

        }

        close ($OUTPUT);
	return 1;

}


#--------------------------------------------------------
# Subroutine: create_PACKlist
#
# Purpose: create_PACKlist creates a list of GFS pack format files on the IBM.
#--------------------------------------------------------

sub create_PACKlist {

        #--------------------------------- 
        #	 Define Variables
        #--------------------------------- 

        my ($LOG) = @_;

        #----------------------------------------------------------
        #       Identify the GFS PACK files 
        #---------------------------------------------------------

	my @files = ();

        if (-d "$packDir") {

	       generalUtils::findFiles($packDir, "G[0-9]{5}_[X|Y][0-9]{4}_PACK.DAT", \@files);

        }
  	
	#---------------------------------------------
        #         Check Array
        #---------------------------------------------

	#--- If no pack files were found then push a random
	#--- pack file name on the array of files

        if (@files == 0) {

		push (@files, "G00004_Y0826_PACK.DAT");

	}	

        return createFile($workDir, $currentPack, \@files);

}


#--------------------------------------------------------
# Subroutine: checkPackfile
#
# Purpose: Reads the name of the pack file just created.  This pack
#	   file is then opened and the number of lines within the pack
#	   file are counted.  If too few lines are found then an
#	   error is returned to the called.  Otherwise, success is returned.
#
# Inputs: $LOG - (ref to a file) - reference to the scripts log file
#	  $rPackFile - (ref to a string) - returns the name of the pack
#			file created by this script to the caller.
#	
# Outputs: 1 - success
#
# Errors:  -1 error opening file
#		
#--------------------------------------------------------
sub checkPackfile {

	#---------------------------------------------
	#	 Define Variables
	#---------------------------------------------
	
	my ($LOG, $rPackFile) = @_;
	
	
	#---------------------------------------------
	# *** Get PACK file name
	#
	# 	Get the name of the pack file by reading 
	# 	through $packFileData 
	# 	until a line containing the name of the pack file is found.
	# 	Extract the pack file name and store in $rPackFile.
	#------------------------------------------------------
	
	my $INPUT = new IO::File;
	
	generalUtils::openFile($workDir, $packFileData, "read", \$INPUT);

	while (defined(my $line = <$INPUT>)) {
	
		#--- If the line read from the file contains letters
		#--- then remove the new line character and store the file name. 
		
		if ($line =~ /[a-zA-Z]/) {
		
			chomp $line;
			$$rPackFile = $line;
			
		}
	
	}
	
	close $INPUT;
	

	#------------------------------------------------
	#*** Check the PACK file
	#
	# 	Open the pack file and count the number of 
	# 	lines contained in the file.
	# 	If the correct number of lines is found then 
	# 	assume the file is correct and return 1 to the caller.
	# 	Otherwise, assume the file is incorrect and return -1
	# 	to the caller.
	#------------------------------------------------

	#--- Open the packfile
	
	if (generalUtils::openFile ($workDir, $$rPackFile, "read", \$INPUT) != 1) {
		
		printf ($LOG "Error (convertGFS::checkPackfile): Can't open $$rPackFile");
		return -1;
	
	}
	
	#--- Read the number of lines found in the file

	my $numLine = 0;
	
	$numLine++ while <$INPUT>;
	
	close $INPUT;
	
	if ($numLine == $packSize) {
		
		printf ($LOG "\nFile $$rPackFile is complete with $numLine lines.\n");
		return 1;
	
	}

	printf ($LOG "Error (convertGFS::checkPackfile): File $$rPackFile has an incorrect ($numLine) number of lines. \n");
    	printf ($LOG "File $$rPackFile will be deleted. \n");
	
	return -1;
	

}

#--------------------------------------------------------
# Subroutine: executeCode
#
# Purpose: Executes the passed in executable and appends
# 	   the resulting log file to the master log file
#	   kept with this script.
#
# Inputs: $exe - (string) - an executable
#	  $file - (string) - name of the log file produced by $exe
#	  $LOG - (ref to file) - log file
#
# Outputs: 1 - success
#
# Errors:  -1 error opening file
#		
#--------------------------------------------------------
sub executeCode {

	#--------------------------
	#	 Define Variables
	#--------------------------
	
	my ($exe, $file, $LOG) = @_;
	
	#--------------------------	
	#	Execute the $exe code
	#--------------------------
	
	#--- Copy executable to working directory
	
	unless (-e "$workDir/$exe") {
		
		copy("$ExecDir/$exe", $workDir) or 
			fatal ("Error ($!): Unable to successfully copy $exe.\n", $LOG);
	}
	
	#--- Make the code executable and execute

	chmod (0744, "$workDir/$exe") or fatal ("Error ($!): Unable to make executable.", $LOG);

	system ("cd $workDir; ./$exe") and 
		fatal ("Error ($!): Unable to execute $exe. \n", $LOG);
	
	#--------------------------	
	#	Append $exe log to the master log
	#--------------------------
	
	printf ($LOG "-------- Begin $exe Log file contents -------------\n");
	generalUtils::appendFile($workDir, $file, $LOG);
	printf ($LOG "-------- End $exe Log file contents ---------------\n\n\n");

}


#--------------------------------------------------------
# Subroutine: fatal
#
# Purpose: In case of a fatal error...
#		print the error message in the log
#		close the log
#		remove the lock file
#		end the program
#--------------------------------------------------------
sub fatal {
	
	my ($message, $LOG) = @_;
	
	print ($LOG $message);
	close $LOG;
	unlink("$workDir/$lockFile");
	exit 1;

}


#--------------------------------------------------------
# Subroutine: readCompareFile
#
# Purpose: Reads the file produced by the fortran executable
#	   which figures out if there are gfs grid files
#	   which need to be converted into pack files.
#	   The file contains information about the file
#	   to convert including the year/month/day and the name of the
#	   gfs grib file.
#
# Inputs: $dir - (string) - absolute path to file 
#	  $file - (string) - name of the file containing the gfs grib file attributes
#         $rCommand - (ref to string) -  either exit or noexit depending on 
#					whether a new GFS grib file was found 
#	  $rYMD - (ref to string) -  year/month/day of the GFS grib file
#	  $rGFSfile - (ref to string) - name of the GFS grib file
#	  $rRun - (ref to string)  - the run number
#
# Outputs: 1 - success
#
# Errors:  -1 error opening file
#		
#--------------------------------------------------------
sub readCompareFile {
	
	#----------------------------
	# 	Define Variables 
	#----------------------------	
	
	my ($dir, $file, $rCommand, $rYMD, $rGFSfile, $rRun, $LOG) = @_;
	my $INPUT = new IO::File;

	#----------------------------	
	# ***  Open the comparison file 
	#----------------------------
	
	if (generalUtils::openFile($dir, $file, "read", \$INPUT) != 1) {
	
		print ($LOG "Error (convertGFS::readCompareFile): $dir/$file could not be opened for reading.\n");
		return -1;
		
	}
	
	#----------------------------
	# ***  Read through file and set variables
	#----------------------------
	
	my $count = 0;
	
	while (defined (my $line = <$INPUT>)) {
	
		#--- Remove newline character from $line
		
		chomp ($line);
		
		#--- The first line contains the  command 'exit' or 'noexit'
	
		if (($line =~ /exit/) && ($count == 0)) {
		
			$$rCommand = $line;
			$count++;
				
		}
	
		#--- The second line contains the name of the avn grib file
	
		elsif ($count == 1 && $line =~ /gfs/ && $line =~ /z.pgrbf/) {
		
			$$rYMD = substr($line, 6, 6);
			$$rGFSfile = $line;
			$count++;
			
		}
		
		#--- The last line contains the run number
	
		elsif ($count == 2) {
		
			$$rRun = $line;
			$count++;
			
		}
	
	}

	close $INPUT;
	return 1;
	
}


#--------------------------------------------------------
# Subroutine: clean
#
# Purpose: Removes temporary files, log files and executables
#	   used during the processing of the GFS grib data.  
#
# Inputs: $packFile - (string) - name of the pack file that was 
#				 created by this program 
#
# Outputs:  None
#
# Errors:  None
#--------------------------------------------------------
sub clean {

	#===================================
	#*** Define Variables
	#===================================
	
	my ($packFile) = @_;
	
	
	#===================================
	#*** Remove Files
	#===================================
	
	#--- Remove extra files

	unlink ("$workDir/$lockFile");
	unlink ("$workDir/$compareData");  
	unlink ("$workDir/$currentGFS");
	unlink ("$workDir/$currentPack");
	unlink ("$workDir/$packFile");
	
	
	#--- Remove Log files
	
	unlink ("$workDir/$compLogFile");
	unlink ("$workDir/$extractLogFile");
	unlink ("$workDir/$binToGtmpLog");
	unlink ("$workDir/$gtmpToPackLog");
	
	
	#--- Remove Exe Files
	
	unlink ("$workDir/$compareExe");
	unlink ("$workDir/$extractExe");
	unlink ("$workDir/$extractShellExe");
	unlink ("$workDir/$wgribExe");
	unlink ("$workDir/$cnvgribExe");
	unlink ("$workDir/$binToGtmpExe");
	unlink ("$workDir/$gtmpToPackExe");
	
	
	#--- Remove intermediate files
	
	system ("rm -rf $workDir/*.bin");
	system ("rm -rf $workDir/*.gtmp");
}


#--------------------------------------------------------
# Subroutine: setupEnv
#
# Purpose: Sets up the environment needed to run this program including:
#		- Checking for the existance of the working directory.
#		If the working directory does not exist then create
#		create one.
#		- Checking for the existance of a lock file.  If a lock file already
#		exists then return and error message and end.  If a lock file
#		does not exist then create one.
#		- Open a log file
#
# Called by: main
#
# Calls: None
#
# Errors: None
#--------------------------------------------------------
sub setupEnv {

	#===================================
	#*** Define Variables
	#===================================
	

	my ($workDir, $logfile, $lockfile, $rLOG) = @_;
	
	
	#===================================
	# *** Create WORK Directory ***
	#===================================


	#--- Create the Work Directory

	if (!(-d $workDir)) {
	
		system ("mkdir $workDir") && die ("Error: Can't create working directory $workDir.\n");
	}

	#--- Check for existance of a lock file

	my $returnCode = generalUtils::isFile($workDir, $lockfile);

	if ($returnCode != -1) {
	
		if (generalUtils::oldFile($workDir, $lockfile, 5) == 0) {
		
			printf ("Error ($returnCode): Lock directory exists. Program already running. \n");
			exit 1;
			
		}
		
		unlink ("$workDir/$lockfile");
	
	}

	#--- Create a lock file

	system ("touch $workDir/$lockfile");


	#=========================
	# *** Open Log file ***
	#=========================


	$returnCode = generalUtils::openFile($workDir, $logfile, "write", $rLOG);
	
	if ($returnCode != 1) {
	
		printf ("Error ($returnCode): $workDir/$logfile could not be opened for writing.\n");
	
	}

}


