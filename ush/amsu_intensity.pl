package intensityEstimate;

#! /usr/bin/end perl
#-w
use warnings;

use 5.008;
use File::Copy;
use IO::File;
use strict;
use Date::Manip;

#-- Directories

my $ExecDir = $ENV{ExecDir};
my $packDir = $ENV{packDir};
my $ushDir = $ENV{USHnhc};

#--- General functions

require "$ushDir/nhc_generalutils.pl";

#--- File Names

my $tempRetrieval = "temp_ret.dat";
my $packList = "ncep.list";
my $fname = "ncep.fname";
my $gfs = "AVN.DAT";
my $date = "tdate.dat";

#--- Intensity Estimates

#my $fix_files = ["NOAA15.FIX", "NOAA16.FIX", "NOAA18.FIX"];
my $fix_files = ["NOAA15.FIX", "NOAA16.FIX", "NOAA18.FIX", "NOAA19.FIX", "NOAA01.FIX", "NOAA02.FIX"];

#--- Temperature Retrieval files

#my $retrieval_files = ["noaa15.ret", "noaa16.ret", "noaa18.ret"];
my $retrieval_files = ["noaa15.ret", "noaa16.ret", "noaa18.ret", "noaa19.ret", "nasaaq.ret", "metop2.ret"];

#--- Executables 

my $intensityAlg = "amsu_oparet";
my $intensityAlgLog = "oparet.log";
my $findPack = "amsu_pickgfs";


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Program: intensity_estimate.pl
#
# Purpose:  This script finds the latest GFS pack file 
#	    that pertains to a given AMSU pass.  The GFS pack 
#	    file is used in conjunction with the temperature retrieval
#  	    completed for each satellite (NOAA15, NOAA16, NOAA18)
#	    and the executable oparet.exe to produce a estimation
#	    of the storms intensity.
#
# 	    The output of the script are generic text files
# 	    NOAA15.FIX, NOAA16.FIX, and NOAA18.FIX
# 	    which contain the intensity and size estimate
#	    of the storm
#
# Inputs:   Temperature Retrieval Files
#
# Outputs:  Intensity Estimates 
#			
# Called by: amsu_estimates.pl
#
#---------------------------------------
# Information:
#    Joint Hurricane Testbed/USWRP project
#    National Hurricane Center / Tropical Prediction Center
#    Department of Commerce - NOAA / NWS
#
# Category: JHT - AMSU
#
# Written by: A. Krautkramer 05/21/2003
# Modified by: A. Krautkramer 06/03/2003 
#              - added documentation
#              M. Sardi 07/04/2014
#              - added leading ./ to several system calls that run local executables;
#                changed 'require' statements to full-path.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-




#--------------------------------------------------------
# Subroutine: new
#
# Purpose:  Manages the process of using the appropriate 
#	    temperature retrieval files and GFS PACK format
#	    file to perform and intensity estimate.
#
# Calls: findPackFile
#	 performEstimate 
#
# Inputs: $workDir - (string) - directory for temp work 
#	  $LOG - (ref to file) -  file ref for log messages
#
# Outputs: 1 - success
#--------------------------------------------------------

sub new {

	#------------------------
	# 	 Initialize Inputs
	#------------------------

	my ($workDir, $LOG) = @_;

	#-------------------------------------
        #       Copy executables to working dir
        #--------------------------------------

        #--- Executable to find latest GFS PACK file
        copy ("$ExecDir/$findPack", "$workDir/$findPack") || return -1;
        chmod (0700, "$workDir/$findPack") || return -1;

        #-- Intensity Algorithm Executable
        copy ("$ExecDir/$intensityAlg", "$workDir/$intensityAlg") || return -1;
        chmod (0700, "$workDir/$intensityAlg") || return -1;

	#-------------------------------------
	# 	Find get a list of GFS pack files on the server 	
	#-------------------------------------

	if (getPackFiles($workDir, $packList, $LOG) != 1) {
	
		printf ($LOG "Error (intensityEstimate::new): Could not sucessfully create $workDir/$packList.\n");
		return -1;
		
	}

		
	#----------------------------------------------------
	# 	Run the Intensity Algorithm Code
	#----------------------------------------------------
	
	performEstimate ($workDir, $LOG);
	
	
	#----------------------------------------------------
	# 	Check Results of Intensity Algorithm Code
	#----------------------------------------------------
	
	my $found = 0;
	
	foreach my $file (@$fix_files) {
		
		unless (generalUtils::isFile($workDir, $file) == 1) {
		
			printf ($LOG "Error (intensityEstimates::new): Fix file $workDir/$file was not found or had 0 size. \n");
			next;
		
		}
		
		$found = 1;
	
	}
	
	if ($found == 0) {
		
		printf ($LOG "Error (intensityEstimates::new): No Fix files found. \n");
		return -1;
	
	}

	#--- Remove intermediate files

        clean($workDir, $LOG);

	#--- Status message
	
	print ($LOG "Intenisty Algorithm Complete.\n");
	return 1;

}


#--------------------------------------------------------
# Subroutine: performEstimate
#
# Purpose:  On each fo the temperature retrieval files ...
#
#	    	- Run the code to find the nearest applicable GFS 
#	    	  pack file and copy the file to the workDir.  
#	    	- Run the code to perform the intensity
#	    	  estimate on each of the temperature retrieval file.
#
# Calls:  oparet 	(intensity estimate algorithm)
#	  getGFSfile
#
# Inputs: $workDir - (string) - directory for temp work
#	  $LOG - (ref to file) -  file ref for log messages
#--------------------------------------------------------

sub performEstimate {

	#---------------------------
	#	Define local variables
	#---------------------------
	
	my ($workDir, $LOG) = @_;

	#-----------------------------------------
	# 	Process each retrieval file
	#-----------------------------------------
	
	foreach my $file (@$retrieval_files) {
	
		#--- Remove old files
		
		clean($workDir, $LOG);

		#--- Copy the retrieval file into a file named temp_ret.dat
	
		copy ("$workDir/$file", "$workDir/$tempRetrieval") || next;

		#---- Get the correct GFS file

		getGFSfile($workDir, $LOG);
		
		#--- Print status messages
		
		printf ($LOG "Begin Processing $file .... \n\n");
		
		#---  Run intensity/size algorithm  *****
	
		system ("cd $workDir; ./$intensityAlg");
		
		#--- Append Log
	
		generalUtils::appendFile($workDir, $intensityAlgLog , $LOG);
	
	}
	
}

#--------------------------------------------------------
# Subroutine: getGFSfile 
#
# Purpose:  Finds the appropriate GFS pack format file on
#	    the server and copies it to the working directory. 
#
# Calls: pickavn	(choose closest GFS PACK file)
#	readFname 
#
# Inputs: $workDir - (string) - directory for temp work
#         $LOG - (ref to file) -  file ref for log messages
#--------------------------------------------------------

sub getGFSfile {

	#-----------------------------------
        #       Define local variables
        #-----------------------------------

        my ($workDir, $LOG) = @_;

	#-----------------------------------------
        #       Create file with current date/time
        #-----------------------------------------

        system ("date -u > $workDir/$date");


        #-------------------------------------------
        #       Execute code to identify GFS pack file
        #-------------------------------------------

        system("cd $workDir; ./$findPack");


        #----------------------------------------------------
        #       Copy GFS file to working directory
        #---------------------------------------------------

        my ($gfsFile);

        if (readFname ($workDir, $fname, \$gfsFile, $LOG) != 1) {

             printf ($LOG "Error (intensityEstimate::getPackFiles): Problem reading $workDir/$fname.\n");
             return -1;
        }

        copy ("$packDir/$gfsFile", "$workDir/$gfs") || die printf ($LOG "Error: Could not copy $packDir/$gfsFile: $! \n");
        return 1;

}


#--------------------------------------------------------
# Subroutine: clean
#
# Purpose:  Delete intermediate files created while the intensity
#	    estimate alogithm code is running.
#
# Calls: None
#
# Inputs: $workDir - (string) - work directory
#	  $LOG - (ref to file) -  file ref for log messages
#--------------------------------------------------------

sub clean {

	#---------------------------
	#	 Define local variables
	#---------------------------
	
	my ($workDir, $LOG) = @_;

	#---------------------------------------------------	
	#	 Unlink old temporay files used for processing
	#---------------------------------------------------
	
	unlink ("$workDir/$tempRetrieval");
	unlink ("$workDir/$fname"); 
	unlink ("$workDir/$gfs");
	unlink ("$workDir/$intensityAlgLog");
}


#--------------------------------------------------------
# Subroutine: readFname
#
# Purpose:  Reads a file produced by the pickavn fortran code.
#	    The file contains the name of the GFS PACK file that
#	    is closest in time to storm contained in the temperature
#	    retrieval file.  It also contains the amount of time 
#	    between the PACK file and the temperature retrieval.
#
# Calls: None
#
# Inputs: $workDir - (string) - work directory
#	  $fname - (string) - file name of output file from pickavn.exe script
#	  $rGFSfile - (reference to file name) - GFS pack file name
#	  $rNDP - (ref to date/time) - NDT of pack file
#	  $LOG - (ref to file) -  file ref for log messages
#--------------------------------------------------------
sub readFname {

	#---------------------------
	# 	 Define local variables
	#---------------------------
	
	my ($workDir, $fname, $rGFSfile, $LOG) = @_;
	
	
	#----------------------------------------------------
	# 	Read input file
	#----------------------------------------------------
	
	my $INPUT = new IO::File;

    	if (generalUtils::openFile($workDir, $fname, "read", \$INPUT) != 1) {
	
		print ("Error (intensityEstimate::readFname): Problem opening $workDir/$fname.\n");
		return -1;
	
	}

	my @lines = <$INPUT>;
	close $INPUT;


	#----------------------------------------------------
        # 	Read input file
        #----------------------------------------------------

	foreach my $line (@lines) {
	
		chomp $line; 
		$line =~ s/^\s+//;
            	$line =~ s/\s+$//;

        	if ($line =~ /_PACK.DAT/) {
		
			$$rGFSfile = $line;
			return 1;
		}
    	}

	print ("Error (intensityEstimate::readFname): format problem with $fname.\n");
	return -1;

}


#--------------------------------------------------------
# Subroutine: getPackFiles
#
# Purpose: Creates a file named ncep.list that contains a list
#	   of all of the GFS PACK files found in the $packDir.   
#
# Calls: createFiles
#	 readFname
#
# Inputs: $workDir - (string) - path to working directory
#	  $LOG - (ref to a file) - file ref for log messages
#--------------------------------------------------------
sub getPackFiles {
	
	#-----------------------------------
	#	Define local variables
	#-----------------------------------
	
	my ($workDir, $LOG) = @_;


	#------------------------------------------------
        #       Identify the GFS PACK files
        #------------------------------------------------

        my @files = ();

        if (-d $packDir) {

               generalUtils::findFiles($packDir, "G[0-9]{5}_[X|Y][0-9]{4}_PACK.DAT", \@files);

        }

	createFile($workDir, $packList, \@files, $LOG);
	return 1;

}

#--------------------------------------------------------
# Subroutine: createFile
#
# Purpose:  Writes the list of data contained in $raFiles to
#           an output file.
# Input:
#           $dir (string) - directory to place new file in
#           $file (string) - name of new file to create
#           $raFiles (ref array) - ref to an array of strings to
#                       write to the new file
#--------------------------------------------------------

sub createFile {

        #---------------------------------------------
        #        Define Variables
        #---------------------------------------------

        my ($dir, $file, $raFiles, $LOG) = @_;


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


1;
