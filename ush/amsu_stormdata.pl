package stormData;

#! /usr/bin/env perl
#-w
use warnings;

use 5.008;
use File::Copy;
use IO::File;
use strict;
use Date::Manip;

#-- Directories 

my $workDir = $ENV{workDir};
my $ExecDir = $ENV{ExecDir};
my $jtwcBtkDir = $ENV{jtwcBtkDir}; 
my $jtwcAidDir = $ENV{jtwcAidDir};
my $jtwcComDir = $ENV{jtwcComDir};
my $nhcBtkDir = $ENV{nhcBtkDir};
my $nhcAidDir = $ENV{nhcAidDir};
my $nhcComDir = $ENV{nhcComDir};
my $HomeDir = $ENV{HOMEnhc};
my $UshDir = "$HomeDir/ush";

#--- General functions

require "$UshDir/nhc_generalutils.pl";
require "$UshDir/nhc_atcfstorms.pl";

#--- File names 

my $FILENAME1 = "POSITIONS";
my $FILENAME2 = "COORDINATES";

#--- Executables 

my $stormPosition = "amsu_rcarq1";
#my $queryData = "read_COORDINATES";
my $queryData = "amsu_readcoordinate";



#-=-=-=-=-=---=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Program: stormData.pl
#
# Version: 2.0
#
# Purpose: Reads the ATCF decks and extracts position and
#	   intensity information for any storms active at either
#	   the user supplied date or the current system date.
#	   The storm data is then copied into a file named COORDINATES.   
#	   The COORDINATES file is used as input into a fortran program 
#	   that finds the DTG and geographic coordinates 
#	   used to query the AMSU BUFR database on the NCEP IBM.  The COORDINATES
#	   file is open, read and the information is stored in a object which is
#	   passed back to the calling routine (see description of data structure in
#	   the returns section below)
#
# Called by: amsu_estimates.pl
#
# Usage:
#
#	--- For a user supplied date
#		
#		stormData::new("2004072400", $LOG);
#	
#	--- Current system type
#
#		my $date = undef;
#		stormData::new($date, $LOG);
#
# Returns:
#
#	--- data structure containing a list of data needed to 
#	    query the IBM for information including the dtg and a
#	    location of a box around the storm
#
#		$rhResults = {
#				dumpjb.in_## -> {"stormName"} -> BB##YYYY
#						{"dtg"} -> 2004050600
#						{"location"} -> coordinates
#				dumpjb.in_## ->
#
#			}
#
#------------------------------------------------------------
#
# Last Modified:  3/31/2005 A Krautkramer
#		  Modified for running on the IBM
#		  4/12/2006 ARK
#                 8/17/2006 ARK
#                     Bug Fix: Updated the addition of atcfObj's to
#                     @atcfObjs.  The old code would add undef objects to
#                     @atcfObjs.  The new code checks if the atcfObj is defined
#                     first and then if it is defined the object gets added.
#                 7/04/2014 M Sardi
#                     Added leading ./ to several system calls that run local executables;
#                     Changed 'require' statements to full-path.
#                10/15/2021 M Sardi
#                     Removed 'unless defined @$raStorms' on line 312 (was throwing errors).  
#
#=-=-=-=-==-=-=-==-=-=-=-=-=-=-=-=-==-=-=-==-=-=-=-=-=-=-=-==-=-==-=-==-=-=-=-==-=-=-=-==-=-


#-------------------------------------------------------------------
#
# Subroutine: new
#
# Purpose: Reads the ATCF decks and extracts position and
#          intensity information for any storms active at either
#          the user supplied date or the current system date.
#          The storm data is then copied into a file named COORDINATES.
#          The COORDINATES file is used as input into a fortran program
#          that finds the DTG and geographic coordinates
#          used to query the AMSU BUFR database on the NCEP IBM.  The COORDINATES
#          file is open, read and the information is stored in a object which is
#          passed back to the calling routine (see description of data structure in
#          the returns section below)
#--------------------------------------------------------------------
sub new {

	#--------------------------
        # 	Initialize Variables
        #--------------------------

	my ($date, $rhResults, $LOG) = @_;

	#--------------------------	
	#	 Check the format of date
	#--------------------------
	
	unless (generalUtils::validDateFormat($date) == 1) {
	
		print ($LOG "Error (stormData::new): Date format error. Correct format YYYYMMDDHH.\n"); 
		return -1;
	}


	#--------------------------
	#	 Find active storms
	#--------------------------
	
	my $jtwcAtcfObj;
	my $nhcAtcfObj;
	my @storms;

	my $returnCode1 = atcfStorms($date, $LOG, \$nhcAtcfObj, \@storms, "nhc");
	my $returnCode2 = atcfStorms($date, $LOG, \$jtwcAtcfObj, \@storms, "jtwc"); 
	
	unless ($returnCode1 == 1 || $returnCode2 == 1) {
		
		unless ($returnCode1 == -2 || $returnCode2 == -2 ) {
	
	      		print ($LOG "Error (stormData::new): Error creating ATCF storms object.\n");
	      		return -3;

		}
		return -1;
		
	}


 	#--------------------------
        #       Process CARQ records
        #--------------------------
	
	#Start Update 8/28/2006
	#
	#my @atcfObjs = ($jtwcAtcfObj, $nhcAtcfObj);
	my @atcfObjs = ();
	if (defined $jtwcAtcfObj) {
		push( @atcfObjs, $jtwcAtcfObj );
	}
	if (defined $nhcAtcfObj) {
		push( @atcfObjs, $nhcAtcfObj );
	}
	#
	#End Update 8/28/2006
	
	my %carq;

 	#--- Get CARQ Records

	foreach my $atcfObj (@atcfObjs) {

		unless (getCARQ($atcfObj, \@storms, \%carq, $LOG) == 1) {	

			print ($LOG "Error (getCARQ::new): Could not get carq records for one ojbect.\n");
	#		return -1;
		}
	}

	#---  Process CARQ records 
		
	unless (process(\%carq, $LOG) == 1) {
		
		print ($LOG "Error (stormData::new): Could not process the CARQ records.\n");
		return -1;
		
	}


	#------------------------------
	# Get storm data results
	#------------------------------

	unless (getStormData($rhResults, $LOG) == 1) {

               	print ($LOG "Error (stormData::new): Extracting storm data from process result files.\n");
              	return -1;

        }

	return 1;
		
}


#----------------------------------------------------------------
# Subroutine: atcfStorms
#
# Purpose:  Create an ATCF object and finds the number of active storms
#	    during the given date.  If there are no active storms passed back
#	    or the object can't be created then pass back an error condition.
#
# Inputs:   $date - (string) - date/time of interest in YYYYMMDDHH format 
#	    $LOG - (ref to file) - log file
#	    $rATCF - (ref) - ref to pass back the new ATCF object
#	    $raStorms - (ref array) - ref to an array to pass back the
#					names of the active storms.
#	    $deck - (string) - indicates which atcf deck to examine ('jtwc' or 'nhc')
#-----------------------------------------------------------------
sub atcfStorms {

	#--------------------------
	# Initialize Variables
	#--------------------------
	
	my ($date, $LOG, $rATCF, $raStorms, $deck) = @_;

	#-------------------------
	#  Set Varaible to Correct ATCF Deck
	#------------------------

	my ($atcfBtkDir, $atcfAidDir, $atcfComDir);


	if ($deck eq "jtwc") {

		$atcfBtkDir = $jtwcBtkDir;
		$atcfComDir = $jtwcComDir;
		$atcfAidDir = $jtwcAidDir;

	}
	else {

		$atcfBtkDir = $nhcBtkDir;
                $atcfComDir = $nhcComDir;
                $atcfAidDir = $nhcAidDir;

	}
	
	
	#----------------------------------
	#  Find the current synoptic dtgs
	#----------------------------------

        my $synoptic;

        unless (generalUtils::synopticTime($date, 0, \$synoptic) == 1 ) {

                print ($LOG "Error (stormData::atcfStorms): Error finding synpotic times.\n");
                return -1;

        }
        
	#--- Log message

        generalUtils::header("Extracting coordinates for current storms in A decks", $LOG);
        print ($LOG "Current synoptic date/time: $synoptic  \n");

		
	#----------------------------
	# Create an atcfStorms object
	#----------------------------
	
	$$rATCF = atcfStorms::new($atcfBtkDir, $atcfComDir, $atcfAidDir, "date", $synoptic);

	#--- Check that the $$rATCF object was correctly created
	
	unless (defined $$rATCF) {
	
		print ("Error (stormData::atcfStorms): Problem creating ATCF object for $synoptic.\n");
		return -1;
	
	}
	
	
	#----------------------------
	# Check for active storms
	#----------------------------
	
	#--- Add a list of the active storms at $date

	foreach my $storm (@{$$rATCF->getActiveStorms()}) {
		print "Push $storm onto array\n";	
		push (@$raStorms, $storm);

	}	
	
	#--- Check that there were at least 1 active storm at $date

	unless ( @$raStorms > 0 ) {
	
		print ($LOG "No active storms at $synoptic.\n");
		return -2;
	
	}
	
	return 1;

}


#----------------------------------------------------------------
# Subroutine: getCARQ 
#
# Purpose:  Gets all of the CARQ records for a given storm and dtg.
#
# Inputs:   $ATCF - (ref atcfStorms object)
#	    $raStorms - (ref array) - Stores the string names of active storms
#	    $rhCarq - (ref hash) - Stores CARQ records extracted from the ATCF
#	    $LOG - (ref to file) - File to store log messages  
#-----------------------------------------------------------------
sub getCARQ {

	#--------------------------
	# Initialize Variables
	#--------------------------
	
	my ($ATCF, $raStorms, $rhCarq, $LOG) = @_;
	
	
	#----------------------------
	# Process Storm computes
	#
	# For each current storm get the CARQ records
	# for the date the ATCF object was initialized for.
	#----------------------------
	
	#--- Get storm CARQ records
	
	$ATCF->computes();
	
	#--- For each storm create a file named POSITION which containing 
	#--- CARQ records.
	
	foreach my $storm (@$raStorms) {
		
		#---------------
		# Get CARQ records
		#---------------
		
		my $raCARQ = [];

		#--- Get a reference to an array of CARQ records
		
		#Start Update 8/28/2006
		unless ($ATCF->getCARQ($storm, $raCARQ, 0) == 1) {
		#End Update 8/28/2006	
			print ($LOG "Error(stormData::getCarq): Problem initializing CARQ for $storm.\n"); 
			next;

		}
			
		#--- Check that the CARQ array is defined and contains records
		
		unless (@$raCARQ > 0) {
		
			#Start Update 8/28/2006
			unless ($ATCF->getCARQ($storm, $raCARQ, 6) == 1) {
			#End update 8/28/2006	
				print ($LOG "Error(stormData::getCarq): Problem initializing CARQ for $storm.\n");
                        	next;

			}
			
			unless ($raCARQ > 0) {
				
				print ($LOG "Error (stormData::getCarq): Problem initializing the CARQ array for $storm.\n");
				next;
			
			}
		
		}
		
		#Start Update 8/28/2006
		#--- Check that the CARQ array is defined and contains records
		if (@$raCARQ == 0 && ($storm =~ /^sh\d{6}/i)) {
			
			unless ($ATCF->getCARQ($storm, $raCARQ, 12) == 1) {
				
				print ($LOG "Error(stormData::getCarq): Problem initializing CARQ for $storm.\n");
                        	next;

			}
			
			unless ($raCARQ > 0) {
				
				print ($LOG "Error (stormData::getCarq): Problem initializing the CARQ array for $storm.\n");
				next;
			
			}
		
		}
		#End Update 8/28/2006 
		
		#--- Add CARQ records to hash
		
		foreach my $rec (@$raCARQ) {
			
			$$rhCarq{substr($rec, 0, 33)} = $rec
		
		}
	
	
	}

	#--- Check hash for entries

	if (keys(%$rhCarq) > 0) {

		return 1;

	}

	print ($LOG "Error (stormData::getCarq): No CARQ record found for any active storm.\n"); 
	return -1; 

}

#----------------------------------------------------------------
# Subroutine: writeCARQ
#
# Purpose:  Takes CARQ records which are first sorted by forecast time and 
#	    are then printed to a file named POSITIONS.
#
# Inputs:   $rhCarq - (ref to hash) - Hash containing CARQ records
#		for all currently active storms
#           $LOG - (ref to file) - File to store log messages
#-----------------------------------------------------------------
sub writeCARQ {

	#--------------------------
        # Initialize Variables
        #--------------------------
	
	my ($rhCarq, $LOG) = @_;	


	#---------------------
	# Sort CARQ records
	#---------------------
		
	#--- Sort records in hash by key (BB, ##, YYYYMMDDHH, ##, CARQ, HHH)

	my @keys = sort keys %$rhCarq;
		
	if (@keys <= 0) {
		
		print ($LOG "Error (stormData::createCoordinates): No sorted CARQ records found.\n");
		return -1;
			
	}
		
		
	#--------------------------------------
	# Write sorted CARQ records to a file
	#--------------------------------------
		
	#--- Create a file for the sorted CARQ records 
	
	my $OUTPUT = new IO::File;
	
	if (generalUtils::openFile ($workDir, $FILENAME1, 'write', \$OUTPUT) == 1) {
		
		#--- Print records to file
		
		foreach my $key (@keys) {
				
			print ($OUTPUT $$rhCarq{$key});
			
		}
			
		close $OUTPUT;
		return 1;			
	}
			
	print ($LOG "Error (stormData::createCoordinates): Problem creating $FILENAME1.\n");	
	return -1;
	
}


#----------------------------------------------------------------
# Subroutine: process
#
# Purpose:  Creates a file containing the geographic extent 
#	    and dtg used to query the IBM database.
#
# Inputs:    $LOG - (ref to file) - File to store log messages  
#-----------------------------------------------------------------
sub process {

	#--------------------------
	# Initialize Variables
	#--------------------------
	
	my ($rhCarq, $LOG) = @_;


	#--------------------------
	# Write CARQ record to a file
	#--------------------------

	unless (writeCARQ ($rhCarq, $LOG) == 1) {

		print "Error (stormData::process) Problem writing sorted CARQ records to $FILENAME1.\n";
		return -1;
	
	}

	
	#--------------------------
	# Create COORDINATES files
	#--------------------------
	
	#--- Run the fortran program which extracts the 0 and -12 CARQ records and
	#--- writes them to a file named COORDINATES
	
	copy ("$ExecDir/$stormPosition",  $workDir) || print ($LOG "Error (stormData::process): Problem copying $stormPosition. - $!.\n");
	chmod (0744, "$workDir/$stormPosition") || print ($LOG "Error (stormData::process): Problem making $stormPosition executables - $!.\n");
	system ("cd $workDir; cat $FILENAME1 | ./$stormPosition > $FILENAME2") && return -2;
	
	#---  Add the COORDINATES file to the logfile
	
	printf ($LOG "\nContents of $FILENAME2 file ....\n");
	generalUtils::appendFile($workDir, $FILENAME2, $LOG);

	
	#--------------------------
	# Create storm data files
	#--------------------------
	
	#--- Run the fortran program which uses the COORDINATES file
	#--- and determines the geographic extent and dtg to query the IBM database
	
	copy ("$ExecDir/$queryData", $workDir) || print ($LOG "Error (stormData::process): Problem copying $queryData. - $!.\n");
	chmod (0744, "$workDir/$queryData") || print ($LOG "Error (stormData::process): Problem making $queryData executables - $!.\n");
	system ("cd $workDir; ./$queryData") && return -2;

	return 1;

}	


#----------------------------------------------------------------
# Program: getStormData 
#
# Purpose:  Reads file containing the geographic extent
#           and dtg used to query the IBM database.
#
# Inputs:  $rhResults - (ref to hash) - to store results extraced from the file  
#	    $LOG - (ref to file) - File to store log messages
#-----------------------------------------------------------------
sub getStormData {

	#--------------------------
        # Initialize Variables
        #--------------------------

	my ($rhResults, $LOG) = @_;	


	#-----------------------------------------
	# Extract data from each dumpjb.in_* file 
	#-----------------------------------------
	
	my $WORK = new IO::File;
	
	#--- Open a handle to the working directory

	opendir($WORK, $workDir) || print ($LOG "Error (stormData::process) Could not open $workDir : $!");
	
	while (defined  (my $file = readdir($WORK))) {
	
 		if ($file =~ /dumpjb.in_/) {
		
			#--- Read and extract data from the $file and store it in the 
			#--- data object $rhResults
		
			readQueryFile($file, $rhResults, $LOG);	
 		
		}
		
	} 
	
	closedir $WORK;

	#--- Check result hash for records

	if (keys(%$rhResults) > 0) {

                return 1;

        }

	print "Error (stormData::getStormData): Extracting data from dumpjb.in* files.\n";	
	return -1;

}


#--------------------------------------------------------
# Subroutine: readQueryFile
#
# Purpose: Reads an input dumpjb_in_## file and parses out the DTG,
#	   geographic coordinates and the name of the storm.
#
# Inputs:
#	  $file - (string) -  name of the dumpjb_in_## file 
#			      containing dtg and coordinates
#	  $rhResults - (ref to hash) - to store results extraced from the file
#	  $LOG - (ref) - file reference for log messages
#
# Outputs: 1 - success
#
#	    data structure containing a list of data needed to 
#	    query the IBM for information including the dtg and a
#	    location of a box around the storm
#
#	    $rhResults = {
#				dumpjb.in_## -> {"stormName"} -> BB##YYYY
#						{"dtg"} -> 2004050600
#						{"location"} -> coordinates
#				dumpjb.in_## ->
#
#			 }
#
# Errors:  -1 - error opening file
#	   -2 - file format error   
#--------------------------------------------------------

sub readQueryFile {

	#------------------------
	#  Initialize Variables
	#------------------------
	
	my ($file, $rhResults, $LOG) = @_;
	
	
	#-----------------
	#  Open file 
	#-----------------
	
	my $INPUT = new IO::File;
	
	if (generalUtils::openFile($workDir, $file, "read", \$INPUT) != 1) {
	
		print ($LOG "Error (stormData::readQueryFile): Error opening $file.\n");
		return -1;
		
	}

	my @lines = <$INPUT>;

	close $INPUT;

	
	#--------------
	#  Read File 
	#--------------
	
	#--- Keeps count of identified lines for error checking purposes
	 
	my $count = 0;
	
	foreach my $line ( @lines ) {
	
		#---- Remove extra spaces
		
		$line =~ s/\s+//;
	
		#------------------------------------------------------
		#  If the line contains a letter 
		#  and the dtg and geo lines have already been found 
		#  then this should be the storm name.
		#------------------------------------------------------
		
		if (($line =~ /[a-zA-Z]/) && ($count == 2)) {
		
			$line =~tr/A-Z/a-z/;
			$$rhResults{$file}{"stormName"} = $line;
			
		}
		
		elsif ($line =~ /[0-9]/) {
		
			#----------------------------------------------------------
			#  The first line containing a digit should be the DTG.
			#----------------------------------------------------------
			
			if ($count == 0) {	
			
				$$rhResults{$file}{"dtg"} = $line;
				$count++;
				
			} 
			
			#----------------------------------------------------------------
			#  The second line containing a digit should be geographic coordinates
			#------------------------------------------------------------------
			
			else {	
				
				$$rhResults{$file}{"location"} = $line;
				$count++;
				
			}
			
		}
		
	}
	
	unless ($count == 2) {	
		print ($LOG "Error(stormData::readQueryFile): Format error in $file.\n");   
		return -2;
	}
	
	return 1;	
}

1;
