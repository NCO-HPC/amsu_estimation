package atcfStorms;

#! /usr/bin/env perl

use strict;
use Date::Manip;
use IO::File;

my $HomeDir = $ENV{HOMEnhc};
my $UshDir = "$HomeDir/ush";

require "$UshDir/nhc_generalutils.pl";


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# PACKAGE NAME: atcfStorms 
#
# VERSION: 5.5
#
# PURPOSE: Provides a variety of subroutines to
#
#			1. Identify the tropical cyclones active at a specified date/time
#
#				Active TC is defined as having a dated entry in its associated
#				Best Track ATCF file (bdeck) that encompasses either the 
#				the passed in synoptic date/time or the previous 6 hour 
#				synoptic date/time.
#
#				  1st record Btk date/time <= synoptic time <= last Btk date/time
#				OR
#				  1st record Btk date/time <= synoptic time minus 6 hours <= last Btk date/time
#				
#			2. Create a computes file for a tropical cyclone
#
#				A computes file (*.com) is a file which contains the Adeck CARQ 
#				records for the current synoptic time and the Adeck OFCL records for the
#				pervious synoptic time.
#
#				At times a tropical cyclone will be identified as being active
#			        however a CARQ will not be available in the Adeck file.  Because
#				of the forecasting process, CARQ's are not added to the Adeck until 
#				up to 1 hour after synoptic time.	
#
#			3. Find the start/end dates of a tropical cylone
#
#				start = Returns the date/time of the 1st record in the best track 
#				end = Returns the date/time of the last record in the best track
#
# LANGUAGE: Perl 5.8.x
#
# USAGE:  atcfStorms::new(ATCF Best Track Dir, ATCF Computes Dir, ATCF Aid Dir, 'date'|'storm', 'YYYYMMDDHH'|'bb##YYYY')
#
#
#  	INITIALIZTION
#	
#	--> To create a atcfStorms Object for all of the storms active on a specific date
#	
#		* atcfStorms::new(ATCF Best Track Dir, ATCF Computes Dir, ATCF Aid Dir,'date', YYYYMMDDHH); 
#			
#		* EXAMPLE: my $atcf = atcfStorms::new("/data/atcf", "/data/atcf", "/data/atcf", "date", "2003090812");
#
#	--> To create an atcfStorms Object for a particular storm
#
#		* atcfStorms::new(ATCF Best Track Dir, ATCF Computes Dir, ATCF Aid Dir,'storm', bb##YYYY); 
#			
#		* EXAMPLE: my $atcf = atcfStorms::new("/data/atcf", "/data/atcf", "/data/atcf", "storm", "al072004");
#
#
#	QUERY
#
#	--> To get a list of active storms
#
#		* EXAMPLE: my $raStorms = $atcf->getActiveStorms();
#
#	--> To get the start date of a particular storm
#
#		* EXAMPLE: my $date = $atcf->getStartDate("al072004");
#
#	--> To get the end data of a particular storm
#	
#		* EXAMPLE: my $date = $atcf->getEndDate("al072004");
#
#	--> To get a list of dates in which a computes file can be created
#
#		* EXAMPLE: my $date = $atcf->getComputeDates("al072004");
#
#
#	COMPUTES
#
#	--> Initializes the data structure with the the OFCL/CARQ records for a storm/date 
#
#		* EXAMPLE: $atcf->computes(); # computes for all active storms
#							
#				-or-
#							
#		* EXAMPLE: $atcf->stormComputes("al132003"); # computes for 1 storm
#
#	--> To create a computes file.  The date can be obtained by using the getComputeDates() subroutine. 
#
#		* EXAMPLE: $atcf->createComputesFile("al132003", "2004060900", "/home/akrautkr","comab.dat");
#
#
#							
# INPUT FILES: ATCF best track files
#	       ATCF aids files
#	       ATCF computes files
#
# OUTPUT FILES: Computes file with a user defined name
#
#---------------------------------------------------
# DATA STRUCTURE:
#			 {
#	
#				"type" =>"date" | "storm",
#				"dates"	=> [],
#				"storms" => {}
#				"atcfBtkDir" 	=> $atcfBtkDir,
#				"atcfAidDir" 	=> $atcfAidDir,
#				"atcfComDir" 	=> $atcfComDir
#				
#				
#			 };
#
#	atcfBtkDir - ATCF best track directory
#	atcfAidDir - ATCF aid directory
#	atcfComDir - ATCF com directory
#	type - type of input used to initialize the data structure 
#	 	can be either 'date' or 'storm'
#	dates - date/time array in ascending order
#	storms - hash containing
#
#		'stormName' => startDate string in YYYG
#			    => endDate 	-  Date::Manip object representing the end of the storm
#			    => dtg	=> OFCL []	- array of ATCF OFCL records for (synoptic - 6 hours)
#					=> CARQ []	- array of ATCF CARQ records for (synoptic)
#
#---------------------------------------------------
# BRIEF DESCRIPTION OF PROGRAM SUBROUTINES:
#
#	.... INITIALIZTION FUNCTIONS ....
#
#
#
##SUBROUTINE NAME: new
#
#	DESCRIPTION: Initialize the ATCF data object.
#
##SUBROUTINE NAME: activeStorms
#
#	DESCRIPTION: Finds all of the storms active during the
#		    calendar date/time used to initialize the
#		    ATCF object.  The list of active storms are
#		    stored in the ATCF object's storms hash.
#
##SUBROUTINE NAME: activeDates
#
#	DESCRIPTION: Read the ATCF best track file and extracts
#		     a unique list of the DTG's in the file.
#		     All of the DTG's are added to the objects dates array.
#
#
#
#	.... COMPUTES FUNCTIONS .....
#
#
#
##SUBROUTINE NAME: computes
#
#	DESCRIPTION: Populate the $atcfObj with the computes 
#		     for all of the storms
#		     and times stored in the object.
#
##SUBROUTINE NAME: stormComputes
#
#	DESCRIPTION: For a specific storm in the storms hash 
#		     populate the $atcfObj with the computes 
#		     for that storm and that storm's dtg. 
#
##SUBROUTINE NAME: readATCF
#
#	DESCRIPTION: Selects the correct ATCF file to look
#		     for compute records.
#
##SUBROUTINE NAME: extractATCFcomputes
#
#	DESCRIPTION: Examines an ATCF file and matches the
#		     CARQ/OFCL records found to the appropriate dtg's
#		     stored in the passed in storm's hash.  The
#		     CARQ/OFCL records are added to the storms hash
#		     under {storms}{stormName}{dtg}{carq} => []
#		     and {storms}{stormName}{dtg}{ofcl} => [].
#
##SUBROUTINE NAME: atcfLookup
#
#	DESCRIPTION:  Creates a hash of an ATCF file to speed up the operation
#		     of matching storms/dtgs with CARQ/OFCL records.
#
##SUBROUTINE NAME: createStormComputes 
#
#	DESCRIPTION: Creates a computes file for a particular active storm.
#
#
#
# 	... ACCESS FUNCTIONS ....
#
#
#
##SUBROUTINE NAME: getComputeDates
#
#	DESCRIPTION: Returns a reference to an array that contains
#		     a list of dates for a particular storm
#		     that have both CARQ and OFCL records.
#		     Therefore, it passes back a list of the dates
#		     in which a computes file can be created.
#
##SUBROUTINE NAME: getActiveStorms
#
#	DESCRIPTION: Returns a reference to an array that contains
#		     a list of the active storms.  Active storms are
#		     defined as any tropical cyclones in which best track
#		     records are being written during the date specified
#		     by the user.  The date is specified when the
#		     ATCF object is initialized.
#
##SUBROUTINE NAME: getEndDate
#
#	DESCRIPTION: Returns the end date of a given storm.
#
##SUBROUTINE NAME: getStartDate
#
#	DESCRIPTION: Returns the start date of a given storm.
#
#
#
# 	... GENERAL FUCTIONS ....
#
#
#
##SUBROUTINE NAME: nicePrint
#
#	DESCRIPTION: Prints the contents of the atcfObj in
#		     a nice way
#
##SUBROUTINE NAME: extractATCFdtg
#
#	DESCRIPTION: Takes a ATCF format string and parses
#		     out the date/time group field.  The DTG
#		     is passed back to the caller in the $rDate 
#		     variable.
#
##SUBROUTINE NAME: getATCFlines
#
#	DESCRIPTION: Opens a file and reads the contents into
#		     an array which is then passed back to the caller.
#
##SUBROUTINE NAME: stormDuration
#
#	DESCRIPTION: Uses the Best track file to find the date of the first
#		     best track record and the date of the last best
#		     track record.  The start and end dates are returned
#		     to the caller.
#
##SUBROUTINE NAME: getCARQ
#
#	DESCRIPTION: Adds CARQ records for synoptic dtg for
#		     all active storms to an array 
#		     passed into the function. 			
#
##SUBROUTINE NAME: getCARQM6
#
#	DESCRIPTION: Adds CARQM6 records for the previous synoptic dtg 
#		     for all active storms to an array
#		     passed into the funtion.
#
#---------------------------------------------------
# Information:
#    Joint Hurricane Testbed/USWRP project
#    National Hurricane Center / Tropical Prediction Center
#    Department of Commerce - NOAA / NWS
#
# Category: JHT - ATCF
#
# Created: Alison Krautkramer 11/2003
#
# Modified: 3/2004 
#		Prepared code for transfer to operations
#	    July-1-2004
#		Added a startDate subroutine which will pass back the start date of a 
#		specified storm
#	    Dec-9-2004
#		Totally read wrote and reorganize code
#	    Feb-2-2005
#		Added getCARQ subroutine
#	    Oct-4-2005
#		Added IO and SH basins to activeStorms routine
#	    Jan-24-2006
#		Added new logic to the activeStorms routine in order
#		to identify storms that are active however do not yet have a 
#		CARQ in its associated ATCF A Deck file.  This can occur when 
#		this program is run in real time.  As a result of the forecasting
#		processing, CARQ records are not written to an ATCF A deck file
#		for up to 1 hour after synoptic.
#
#		Added docuemntation to the program description and the activeStorms,
#		nicePrint and createComputesFile subroutines.
#
#	   April-12-2006
#		Updated the getCARQ and getCARQM6 functions so that a reference to an
#		array for CARQ and CARQM6 was passed in rather then having a reference
#		to an array being returned to the caller.   Now a return code is passed
#		back to the caller with a code which indicates the success or failure of 
#		of the subroutine.
#
#	    Aug-21-2006
#		Added a new CARQM12 array for the Southern Hemisphere.  The Southern Hemisphere
#		often but not always only run their guidance every 12 hours when a storm is 
#		not threatening land.  A B-deck record is added every 6 hours. 
#
#           July-07-2014 by M. Sardi
#               Changed 'require' statements to full-path; made several non-fatal errors 'warnings'. 
#	
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


#------------------------ Initalization Subroutines-----------------------------------------------------------------------





#***********************************************************
#
#	SUBROUTINE NAME: new
#
#	DESCRIPTION: Initialize the ATCF data object.
#
#	USAGE (CALLING SEQUENCE):external initlization
#
#	VARIABLES AND THEIR USE
#		PASSED:	ATCF directories
#			type key work indicating either 'date' or 'storm'
#
#				if  type = 'date' 
#					A date/time group in YYYYMMDDHH format
#
#				if type = 'storm'
#					A storm name in bb##YYYY format
#
#		RETURNED: An ATCF data Object
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub new {

	#--- Initialize variables

	my ($atcfBtkDir, $atcfComDir, $atcfAidDir, $type, $entry) = @_;
	
	#--- Check directories for existance 
	
	unless (-d $atcfBtkDir && -d $atcfComDir && -d $atcfAidDir) {
	
		print ("Error (atcfStorms::new): One ATCF directory specified does not exist $atcfBtkDir $atcfComDir $atcfAidDir.\n");
		return undef;
	
	}

	#--- Initialize data structure
	
	my $rATCF = {
			"type" 		=> $type,
			"dates"		=> [],
			"storms"	=> {},
			"atcfBtkDir" 	=> $atcfBtkDir,
			"atcfAidDir" 	=> $atcfAidDir,
			"atcfComDir" 	=> $atcfComDir,
		};
		
	bless $rATCF, "atcfStorms";
	
	if ($type eq 'date') {
	
		#---Check entered date for validity
	
		my $returnCode = generalUtils::validDateFormat($entry);
	
		if ($returnCode != 1) {
	
			printf ("Error $returnCode (atcfStorms::new): Date must be a string in YYYYMMDDHH format.\n");
			return undef;
	
		}
		
		#--- Add date to array
		
		push (@{$$rATCF{"dates"}},  $entry);
		
		#--- Fill object with list of active storms and 
		#--- start/end dates from the Best Track file
		
		unless ($rATCF->activeStorms($entry) == 1) {
		
			return undef;
		
		}	
		
	
	}
	
	elsif ($type eq 'storm') {
	
		#--- Check entered storm name for validity
	
		my $returnCode = generalUtils::validStormName($entry);
		
		if ($returnCode != 1) {
		
			printf ("Error $returnCode (atcfStorms::new): Storm must be a string in bb##YYYY format.\n");
			return undef;
		
		}
		
		#--- Fill object with list of date found in the best track file
		
		$entry =~ tr/A-Z/a-z/;	#make storm name lower case
		unless ($rATCF->activeDates($entry) == 1) {
		
			return undef;
		
		}	
		
	}
	else {
	
		printf ("Error (atcfStorms::new): Incorrect type entry.  Valid types: date|storm\n");
		return undef;
	
	}
	
	return $rATCF;
	
}


#***********************************************************
#
#	SUBROUTINE NAME: activeStorms
#
#	DESCRIPTION: Finds all of the storms active during the
#		    calendar date/time used to initialize the
#		    ATCF object.  The list of active storms are
#		    stored in the ATCF object's storms hash.
#
#       ALGORITHM: The date of the first and last record in the ATCF Best Track
#		   is extracted and compared to both the current and previous 
#		   synoptic times.  If either the current or previous synoptic 
#		   time period falls between the start and end date of the 
#		   storm then the storm is added to the storms array.
#
#	USAGE (CALLING SEQUENCE): new()
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF object
#			Date string in YYYYMMDDHH format
#
#		RETURNED: none
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub activeStorms {

	#--- Initialize variable
	
	my ($atcfObj, $date) = @_;
	
	#--- Get a list of all best track files
	
	my @btkFiles;
	
	#--- ATCF database structure
	
	#^b(ep|al|cp|wp)[^8][0-9]{5}\.dat ... start with b followed by 
	#either ep or al or cp or wp which is not an 80's 
	#storm and with 5 digits followed by the string .dat.
	
	my $returnCode = generalUtils::findFiles("$$atcfObj{atcfBtkDir}", "^b(ep|al|cp|wp|io|sh)[^8][0-9]{5}\.dat", \@btkFiles);

	# the following is for testing only...
	# my $returnCode = generalUtils::findFiles("$$atcfObj{atcfBtkDir}", "^b(ep|al|cp|wp|io|sh)[0-9]{6}\.dat", \@btkFiles);
	
	if ($returnCode != 1) {
		
		printf ("Error $returnCode (atcfStorms::activeStorms) : No best track files found in " . $$atcfObj{atcfBtkDir} .  ".\n");
				#--- Add storm information to computes array
	
	}

	#--- Find synoptic times

        my ($synopticDate, $synopticDateM6);

        if ((generalUtils::synopticTime($date, 0, \$synopticDate) != 1) ||
            (generalUtils::synopticTime($date, -1, \$synopticDateM6) != 1)) {

            print ("Error (atcfStorms::getComputes) finding the current synoptic time for $date");
            return -1;

        }


	#--- For each best track file find the storms begining and ending dates 
	#--- using the first line of the file as the start date and the last line of the file
	#--- as the end date.
	#--- Compare the storm boundary dates to the date stored in the ATCF object.

	#--Flag indicating whether any current storms were found	
	my $found = 0;
	
	foreach my $file (@btkFiles) {
		
		my ($startDate, $endDate);
		
		if (stormDuration($file, "$$atcfObj{atcfBtkDir}", \$startDate, \$endDate) == 1) {	
			if ((Date::Manip::Date_Cmp(Date::Manip::ParseDate($startDate),$synopticDate) <= 0 &&
		   	     Date::Manip::Date_Cmp(Date::Manip::ParseDate($endDate), $synopticDate) >= 0) ||
			    (Date::Manip::Date_Cmp(Date::Manip::ParseDate($startDate),$synopticDateM6) <= 0 &&
                             Date::Manip::Date_Cmp(Date::Manip::ParseDate($endDate), $synopticDateM6) >= 0)) {
		
				#--- Add storm name, start date and end date to storms hash
				
				my $stormName = substr($file, 1, 8);
				
				$$atcfObj{storms}->{$stormName}->{"startDate"} = $startDate;
				$$atcfObj{storms}->{$stormName}->{"endDate"} = $endDate;
				
				#--- Add storm information to computes array
				
				$$atcfObj{storms}->{$stormName}->{$date}->{"carq"} = [];
				$$atcfObj{storms}->{$stormName}->{$date}->{"carqM6"} = [];
				#Start Update 8/28/2006
				$$atcfObj{storms}->{$stormName}->{$date}->{"carqM12"} = [];
				#End Update 8/28/2006
				$$atcfObj{storms}->{$stormName}->{$date}->{"ofcl"} = [];
				
				$found = 1;	
		
			}
		}

	}

	if ($found == 0) {
		return -1;
	}
	
	return 1;
}


#***********************************************************
#
#	SUBROUTINE NAME: activeDates
#
#	DESCRIPTION: Read the ATCF best track file and extracts
#		     a unique list of the DTG's in the file.
#		     All of the DTG's are added to the objects dates array.
#
#	USAGE (CALLING SEQUENCE): new()
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF object
#			Storm name string in bb##YYYY format
#
#		RETURNED: none
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub activeDates {

	#--- Initialize variable
	
	my ($atcfObj, $stormName) = @_;
	
	#--- Best track file name
	
	my $btkFile = "b" . $stormName . ".dat";
	
	#--- Read the Best Track file into an array named lines
	
	my @lines = ();
	
	unless (getATCFlines($btkFile, $$atcfObj{atcfBtkDir}, \@lines) == 1) {
	
		printf ("Error  (atcfStorms::activeDates): Error reading $$atcfObj{atcfBtkDir}/$btkFile \n");
		return -1;
	
	}
	
	#--- Extract unique dates and place in date array
	
	my %seen = ();
	my $date;
	
	foreach my $line (@lines) {
	
		#--- Extract date from ATCF line
	
		unless (extractATCFdtg($line, \$date) == 1) {
		
			next;	#error extracting date
			#return -1;	
		
		}
		
		#--- Add unique dates to the dates array
		
		unless ($seen{$date}) {
		
			#push(@{$$atcfObj{"dates"}}, $date) unless $seen{$date}++;
			push(@{$$atcfObj{"dates"}}, $date);
	
			#--- Add storm information to computes array
				
			$seen{$date} = 1;
			$$atcfObj{storms}->{$stormName}->{$date}->{"carq"} = [];
			$$atcfObj{storms}->{$stormName}->{$date}->{"carqM6"} = [];
			#Start Update 8/28/2006
			$$atcfObj{storms}->{$stormName}->{$date}->{"carqM12"} = [];
			#End Update 8/28/2006
			$$atcfObj{storms}->{$stormName}->{$date}->{"ofcl"} = [];	
		
		}
		
	}
	
	
	#--- Add storm name, start date and end date to storms hash
					
	$$atcfObj{storms}->{$stormName}->{"startDate"} = ${$$atcfObj{"dates"}}[0];
	$$atcfObj{storms}->{$stormName}->{"endDate"} = ${$$atcfObj{"dates"}}[-1];	
	
	return 1;
		
}





#------------------------ Computes Subroutines-----------------------------------------------------------------------







#***********************************************************
#
#	SUBROUTINE NAME: computes
#
#	DESCRIPTION: Populate the $atcfObj with the computes 
#		     for all of the storms
#		     and times stored in the object.
#
#	USAGE (CALLING SEQUENCE): external
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF Object
#
#		RETURNED: 0 no current storms found in the storms array
#			  1 success
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub computes {

	#--- Initialize variable
	
	my ($atcfObj) = @_;

	foreach my $stormName (keys %{$$atcfObj{storms}}) {
	
		$atcfObj->stormComputes($stormName);
	
	}
	
	return 1;

}


#***********************************************************
#
#	SUBROUTINE NAME: stormComputes
#
#	DESCRIPTION: For a specific storm in the storms hash 
#		     populate the $atcfObj with the computes 
#		     for that storm and that storm's dtg. 
#
#	USAGE (CALLING SEQUENCE): computes() or external
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF object
#				Storm name in bb##YYYY format
#
#		RETURNED: -1 no CARQ or OFCL records found
#			 1 success
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub stormComputes {

	#--- Initialize variable
	
	my ($atcfObj, $stormName) = @_;
	
	
	#--- Check the computes directory for a computes file.  If
	#--- one is not found, check the appropriate A deck file for 
	#--- CARQ/OFCL records.
	
	if ($$atcfObj{type} eq "date") {
	
		if ($atcfObj->readATCF($stormName, "com") == 1) {
		
			return 1;
		
		}
	
	}
	
	if ($atcfObj->readATCF($stormName, "aid") != 1) {
		
		printf ("Warning (atcfStorms::stormComputes): No CARQ or OFCL records found for ${stormName}.\n");
		return -1;
		
	}

	return 1;	#computes found

}


#***********************************************************
#
#	SUBROUTINE NAME: readATCF
#
#	DESCRIPTION: Selects the correct ATCF file to look
#		     for compute records.
#
#			Optimization for the case where the 
#			current time is selected and the computes
#			files can be read without having to read
#			the entire A deck file.
#
#	USAGE (CALLING SEQUENCE): stormComputes()
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF storm object
#			name of a storm
#			type of ATCF file to examine (com -> computes dat -> adeck)
#
#		RETURNED: -1 command syntax error
#			 -2 .. -5 open file error
#			-3 OFCL/CARQ records not found for date/time
#			 1 success
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub readATCF{

	#--- Initialize variable
	
	my ($atcfObj, $stormName, $type) = @_;
	
	#--- Check $type parameter
	
	my ($pattern, $dir);
	
	if ($type eq "aid") {
	
		$pattern = "a$stormName.dat";
		$dir = $$atcfObj{atcfAidDir};
		
	
	}
	elsif ($type eq "com") {
	
		$pattern = "$stormName.com";
		$dir = $$atcfObj{atcfComDir};
		
	}
	else {
	
		printf ("Error (atcfStorms::readATCFfiles): syntax atcfObj->readATCFfiles(storm, <aid|com>)\n");
		return -1;
	
	}	
	
	#--- If success then add arrays to data structure and return 1
	
	if (extractATCFcomputes ($atcfObj, $pattern, $dir, $stormName) >= 1) {
	
		return 1;
	
	}
	
	return -3;	#Failure


}



#***********************************************************
#
#	SUBROUTINE NAME: extractATCFcomputes
#
#	DESCRIPTION: Examines an ATCF file and matches the
#		     CARQ/OFCL records found to the appropriate dtg's
#		     stored in the passed in storm's hash. The
#		     CARQ/OFCL records are added to the storms hash
#		     under {storms}{stormName}{dtg}{carq} => []
#		     and {storms}{stormName}{dtg}{ofcl} => [].
#		     
#
#	USAGE (CALLING SEQUENCE): readATCFfiles()
#
#	VARIABLES AND THEIR USE
#		PASSED: Reference to an array of ATCF records
#			DTG in YYYYMMDDHH format
#			Reference to an empty array for OFCL records
#			Reference to an empty array for CARQ records
#
#		RETURNED: Array filled with OFCL records that match the desired DTG
#			Array filled with CARQ records that match the desired DTG
#			-1 Error finding the synoptic CARQ and OFCL times
#			-2 No OFCL or CARQ records found
#			 1 Success
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************

sub extractATCFcomputes {

	my ($atcfObj, $file, $dir, $stormName) = @_;
	my $newRecord = 0;
	

	#--- Create a lookup table from the atcf file
	
	my %linesObj = ();
	unless (atcfLookup(\%linesObj, $file, $dir) == 1) {
	
		print ("Warning (atcfSTorms::readATCF): Unable to create ATCF lookup table from ${file}.\n");
		return -1;
	
	}
	
	#--- Find matching CARQ/OFCL records
	
	foreach my $date (@{$$atcfObj{dates}}) {
	
		#--- Find synoptic times
		
		my ($synopticDate, $synopticDateM6, $synopticDateM12);
	
		if ((generalUtils::synopticTime($date, 0, \$synopticDate) != 1) ||
		    (generalUtils::synopticTime($date, -1, \$synopticDateM6) != 1) ||
		    (generalUtils::synopticTime($date, -2, \$synopticDateM12) != 1))  {

			print ("Error (atcfStorms::getComputes) finding the current synoptic time for $date");
			return -1;
		
		}
		
		if (defined $linesObj{$synopticDate}{carq}) {
		
			$$atcfObj{storms}{$stormName}{$date}{carq} = $linesObj{$synopticDate}{carq};
			$newRecord++;
		
		}
		
		if (defined $linesObj{$synopticDateM6}{carq}) {
		
			$$atcfObj{storms}{$stormName}{$date}{carqM6} = $linesObj{$synopticDateM6}{carq};
			$newRecord++;
		
		}
		
		if (defined $linesObj{$synopticDateM12}{carq}) {

			$$atcfObj{storms}{$stormName}{$date}{carqM12} = $linesObj{$synopticDateM12}{carq};
                        $newRecord++;

		}
		
		if (defined $linesObj{$synopticDateM6}{ofcl}) {
		
			$$atcfObj{storms}{$stormName}{$date}{ofcl} = $linesObj{$synopticDateM6}{ofcl};
			$newRecord++;
			
		}
	
	}
	
	return $newRecord;
	
	
	
}


#***********************************************************
#
#	SUBROUTINE NAME: atcfLookup
#
#	DESCRIPTION:  Creates a hash of an atcf file to speed up the operation
#		     of matching storms/dtgs with CARQ/OFCL records.
#			
#	USAGE (CALLING SEQUENCE): 
#
#	VARIABLES AND THEIR USE
#		PASSED: Reference to an array of ATCF records
#			ATCF file name
#			File directory
#
#		RETURNED: Lookup filled with CARQ/OFCL lines read
#			  from the atcf.
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#***********************************************************
sub atcfLookup {

	my ($rObj, $file, $dir) = @_;
	
	
	#--- Read the lines within the ATCF file into the array lines
	
	my @lines = ();
	
	unless (getATCFlines ($file, $dir, \@lines) == 1) {

		printf ("Warning (atcfStorms::readATCFfiles): Unable to open ${dir}/${file}...likely not found.\n");
		return -1;
	
	}
	
	#--- Extract the CARQ/OFCL lines and add them to new data structure

	foreach my $line (@lines) {
	
		my $atcfDate;
		extractATCFdtg($line, \$atcfDate);
		
		#--- ATCF line contains the string CARQ
		
		if ($line =~ /,\sCARQ,\s+/) {
		
			push (@{$$rObj{$atcfDate}{carq}}, $line);
		
		}
		
		#--- ATCF line contains the string OFCL
		
		elsif ($line =~ /,\sOFCL,\s+/) {
		
			push (@{$$rObj{$atcfDate}{ofcl}}, $line);
		
		}
	
	}
	
	return 1;

}


#***********************************************************
#
#	SUBROUTINE NAME: createStormComputes 
#
#	DESCRIPTION: Creates a computes file for a particular active storm.
#		     using the CARQ and OFCL records. 
#
#		     **This subroutine
#		     does not use CARQM6 records in the case that CARQ
#		     records don't exist.**
#				
#
#	USAGE (CALLING SEQUENCE):createComputes() or external
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF Object
#			storm name as a string
#			date
#			directory for the computes file
#			file name for the computes file
#
#		RETURNED: -1 Storm name not found in ATCF object
#			  -2 Problem creating the computes file
#			  -3 No CARQ or OFCL records found for this storm
#			   1 success
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub createComputesFile {

	#--- Initialize variable
	
	my ($atcfObj, $storm, $date, $dir, $filename) = @_;
	
	#---- Check the storm is contained in the computes hash
	
	unless (exists($$atcfObj{storms}->{$storm}) &&
		exists($$atcfObj{storms}->{$storm}->{$date})) {
	
		printf ("Warning (atcfStorms::createStormComputes): Storm $storm not found in computes hash.\n");
		return -1;
	
	}
	
	#--- Open a file for writing computes to
	
	if (@{$$atcfObj{storms}->{$storm}->{$date}->{ofcl}} > 0 && 
	    @{$$atcfObj{storms}->{$storm}->{$date}->{carq}} > 0)   {
		
		printf ("create computes file");
		my $OUTPUT = new IO::File;
		my $returnCode = generalUtils::openFile($dir, $filename, "write", \$OUTPUT);
	
		if ($returnCode != 1) {
	
			printf ("Error $returnCode (atcfStorms::createStormComputes): $dir/$filename could not be opened for writing.\n");
			return $returnCode;
	
		} 
	
		print ($OUTPUT @{$$atcfObj{storms}->{$storm}->{$date}->{ofcl}});
		print ($OUTPUT @{$$atcfObj{storms}->{$storm}->{$date}->{carq}});
		
		close $OUTPUT;
		return 1;
	
	}
	
	return -3;

}



#------------------------ General Subroutines-----------------------------------------------------------------------







#***********************************************************
#
#	SUBROUTINE NAME: stormDuration
#
#	DESCRIPTION: Uses the Best track file to find the date of the first
#		     best track record and the date of the last best
#		     track record.  The start and end dates are returned
#		     to the caller.
#
#	USAGE (CALLING SEQUENCE):activeStorms()
#
#	VARIABLES AND THEIR USE
#		PASSED: Best Track File name
#			Best Track File directory
#			reference to a variable for the start date
#			reference to a variable for the end date
#				
#		RETURNED: start date in Date::Manip format
#			  end date in Date::Manip format
#
#.................MAINTENANCE SECTION..................
#                                                                                  
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub stormDuration {

	#--- Initialize variable
	
	my ($btkFile, $dir, $rStart, $rEnd) = @_;
	
	
	#--- Read the entire ATCF file into an array lines
	
	my @lines = ();
	
	unless (getATCFlines($btkFile, $dir, \@lines) == 1) {
	
		printf ("Error  (atcfStorms::stormDuration): Error reading $dir/$btkFile \n");
		return -1;
	
	}
	
	if (@lines == 1) {		#--- If the ATCF file only contains one line
	
		unless (extractATCFdtg($lines[0], $rStart) == 1) {
		
			return -1;	#error extracting start date
		
		}
		
		$rEnd = $$rStart;	#If there is only one line then the start of the storm
					#equals the end of the storm
	
	}
	elsif ((extractATCFdtg($lines[0], $rStart)) && (extractATCFdtg($lines[@lines - 1], $rEnd))) {
	
		return 1;
	
	}
	
	#--- Error
	
	return -1;

}

#***********************************************************
#
#	SUBROUTINE NAME: getATCFlines
#
#	DESCRIPTION: Opens a file and reads the contents into
#		     an array which is then passed back to the caller.
#
#	USAGE (CALLING SEQUENCE): stormDuration
#
#	VARIABLES AND THEIR USE
#		PASSED: File name
#			File directory
#			Reference to an empty array
#				
#		RETURNED: The empty array is filled with the contents
#			  of the file.
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub getATCFlines {

	#--- Initialize variable
	
	my ($file, $dir, $raLines) = @_;
	
	my $INPUT = new IO::File;

	#--- Open the file
	
	my $returnCode = generalUtils::openFile($dir, $file, "read", \$INPUT);
	
	unless ($returnCode == 1) {
	
		printf ("Warning $returnCode  (atcfStorms::stormDuration): $dir/$file could not be opened for reading.\n");
		return $returnCode;
	
	} 
	
	#--- Read in entire file into an array
	
	@$raLines = <$INPUT>;
	close $INPUT;
	
	#--- Return error if nothing is read into the array
	
	if (@$raLines == 0) {
		
		return -1;	#error empty file
	
	}
	
	return 1;

}


#***********************************************************
#
#	SUBROUTINE NAME: extractATCFdtg
#
#	DESCRIPTION: Takes a ATCF format string and parses
#		     out the date/time group field.  The DTG
#		     is passed back to the caller in the $rDate 
#		     variable.
#
#	USAGE (CALLING SEQUENCE): stormDuration()
#
#	VARIABLES AND THEIR USE
#		PASSED: String in ATCF format
#			Reference to a variable to store the date
#
#		RETURNED Date string in YYYYMMDDHH format
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub  extractATCFdtg {

	#--- Initialize Variables
	
	my ($line, $rDate) = @_;
	
	#--- Parse the line and extract the date/time group
	
	my @parsedLine = split(/\,/, $line);
 	$$rDate = $parsedLine[2];
	
	#--- Remove white spaces
	
	$$rDate =~ s/\s+//g;
	
	#--- Check date for validity
	
	unless (generalUtils::validDateFormat($$rDate) == 1) {
	
		printf ("Error (atcfStorms::extractATCFdtg): Format Error. Date read is $$rDate.\n");
		return -1;
	
	}
	
	return 1;

}


#***********************************************************
#
#	SUBROUTINE NAME: nicePrint
#
#	DESCRIPTION: Prints the contents of the atcfObj in
#		     a nice way
#
#	USAGE (CALLING SEQUENCE): 
#
#	VARIABLES AND THEIR USE
#		PASSED: Reference to the atcfObj
#
#		RETURNED 
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub nicePrint {

	my ($atcfObj) = @_;
	
	
	#--- Print out the type 
	
	print ("The type of object is $$atcfObj{type}.\n");
	
	
	#--- Print out all of the dates stored in the atcfObj
	
	print ("The dates of the storm are .... ");
	foreach my $date (@{$$atcfObj{dates}}) {
	
		print ("$date ");
	
	}
	print ("\n");
	
	
	#--- Print the storms in the atcfObj

	print ("The contents of the computes hash is .... \n");
	print ("Storms ....  ");
	foreach my $storm (keys (%{$$atcfObj{storms}})) {
	
		print ("$storm  ");
	
	}
	print ("\n");
	
	
	#--- Print the dates and corresponding OFCL and CARQ records foreach storm
	
	print ("..... CARQM12/CARQM6/CARQ/ORCL ... \n");
	foreach my $storm (keys (%{$$atcfObj{storms}})) {
	
		foreach my $date (keys %{$$atcfObj{storms}{$storm}}) {
	
			if ($date eq 'endDate' || $date eq 'startDate') {
				next;
			}
		
			if (defined ($$atcfObj{storms}{$storm}{$date}{"carq"})&& @{$$atcfObj{storms}{$storm}{$date}{"carq"}} > 0) {
				print ("The CARQ records ....  \n");
				foreach my $carq (@{$$atcfObj{storms}{$storm}{$date}{"carq"}}) {
				
					print (" $carq \n") ;
				
				}
			}

			if (defined ($$atcfObj{storms}{$storm}{$date}{"ofcl"}) && @{$$atcfObj{storms}{$storm}{$date}{"ofcl"}} > 0) {
				print ("The OFCL records ....  \n");
				foreach my $ofcl (@{$$atcfObj{storms}{$storm}{$date}{"ofcl"}}) {
				
					print (" $ofcl \n") ;
				
				}
			}
			
			if (defined ($$atcfObj{storms}{$storm}{$date}{"carqM6"}) && @{$$atcfObj{storms}{$storm}{$date}{"carqM6"}} > 0) {
				print ("The CARQ - 6hr records ....  \n");
				foreach my $carqM6 (@{$$atcfObj{storms}{$storm}{$date}{"carqM6"}}) {
				
					print (" $carqM6 \n") ;
				
				}
			}
			
			if (defined ($$atcfObj{storms}{$storm}{$date}{"carqM12"}) && @{$$atcfObj{storms}{$storm}{$date}{"carqM12"}} > 0) {
                                
				print ("The CARQ - 12hr records ....  \n");
                                foreach my $carqM12 (@{$$atcfObj{storms}{$storm}{$date}{"carqM12"}}) {

                                        print (" $carqM12 \n") ;

                                }
                        }
	
		}
	}	
	

}





#------------------------ Access Subroutines-----------------------------------------------------------------------


#***********************************************************
#
#	SUBROUTINE NAME: getLocation
#
#	DESCRIPTION: 
#
#	USAGE (CALLING SEQUENCE): external Accessor Method 
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF Object
#			Storm name in bb##YYYY format
#
#		RETURNED: a reference to an array of lat/lon
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub getLocation {

	#--- Initialize Variables
	
	my ($atcfObj, $storm, $date) = @_;
	
	#--- Convert $storm to lower case
	
	$storm =~ tr/A-Z/a-z/;
	
	unless (defined $$atcfObj{"storms"}{$storm}) {

                print ("Warning (atcfStorms::getLocation): Storm $storm not contained within atcfObject.\n");
                return undef;
       
        }
	
	unless (defined $date) {
		
		if (@{$$atcfObj{"dates"}} > 1) {
		
			print ("Error(atcfStorms::getLocation): Specifiy a calendar date.");
			return undef;
		
		}
		
		$date = $$atcfObj{"dates"}[0];
		
	}
	
	
 	foreach my $ele (@{$$atcfObj{"storms"}{$storm}{$date}{"carq"}}) {
	
		$ele =~ s/\s+//g;
	
          	my @parsedLine = ();
		@parsedLine = split(/,/, $ele);
		
		if ($parsedLine[5] eq  "0") {
		
			#--- Remove last character
			chop( $parsedLine[6] );
			chop( $parsedLine[7] );
			
			#--- Divide by 10 and 
			my $lat = sprintf ( "%.1f", ($parsedLine[6] / 10) );
			my $lon = sprintf ("%.1f", ("-" . ($parsedLine[7] / 10)) );
			
			my @loc = ($lat, $lon);
                    	return \@loc;
		}
		    
        }
		
	return undef;

}


#***********************************************************
#
#	SUBROUTINE NAME: getStartDate
#
#	DESCRIPTION: Returns the start date of a given storm.
#
#	USAGE (CALLING SEQUENCE): external Accessor Method 
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF Object
#			Storm name in bb##YYYY format
#
#		RETURNED: A date as a string in YYYYMMDDHH format
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub getStartDate {

	#--- Initialize Variables
	
	my ($atcfObj, $storm) = @_;
	
	#--- Convert $storm to lower case
	
	$storm =~ tr/A-Z/a-z/;
	
	if (defined $$atcfObj{storms}->{$storm}) {
		
		return Date::Manip::UnixDate($$atcfObj{storms}->{$storm}->{startDate}, '%Y%m%d%H');
	
	}
	
	return undef;

}


#***********************************************************
#
#	SUBROUTINE NAME: getEndDate
#
#	DESCRIPTION: Returns the end date of a given storm.
#
#	USAGE (CALLING SEQUENCE): external Accessor Method 
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF Object
#			Storm name in bb##YYYY format
#
#		RETURNED: A date as a string in YYYYMMDDHH format
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub getEndDate {

	#--- Initialize Variables
	
	my ($atcfObj, $storm) = @_;
	
	#--- Convert $storm to lower case
	
	$storm =~ tr/A-Z/a-z/;
	
	if (defined $$atcfObj{storms}->{$storm}) {
		
		return Date::Manip::UnixDate($$atcfObj{storms}->{$storm}->{endDate}, '%Y%m%d%H');
	
	}
	
	return undef;

}

#***********************************************************
#
#	SUBROUTINE NAME: getActiveStorms
#
#	DESCRIPTION: Returns a reference to an array that contains
#		     a list of the active storms.  Active storms are
#		     defined as any tropical cyclones in which best track
#		     records are being written during the date specified
#		     by the user.  The date is specified when the
#		     ATCF object is initialized.
#
#	USAGE (CALLING SEQUENCE): external Accessor Method
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF Object
#
#		RETURNED: Reference to an array of strings that 
#				  uniquely identify storm names.
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub getActiveStorms {

	#--- Initialize Variables
	
	my ($atcfObj) = @_;

	#--- Return a reference to an array of active storms
	
	my @storms = keys %{$$atcfObj{storms}};
	
	return \@storms;

}


#***********************************************************
#
#	SUBROUTINE NAME: getComputeDates
#
#	DESCRIPTION: Returns a reference to an array that contains
#		     a list of dates for a particular storm
#		     that have both CARQ and OFCL records.
#		     Therefore, it passes back a list of the dates
#		     in which a computes file can be created.
#
#	USAGE (CALLING SEQUENCE): external Accessor Method
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF Object
#			storm name in bb##YYYY format			
#
#		RETURNED: Reference to an array of dates
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub getComputeDates {

	#--- Initialize Variables
	
	my ($atcfObj, $storm) = @_;

	#--- Return a reference to an array of active storms
	
	my @dates = ();
	
	foreach my $date (@{$$atcfObj{dates}}) {
	
		if (@{$$atcfObj{storms}{$storm}{$date}{carq}} > 0 &&
		    @{$$atcfObj{storms}{$storm}{$date}{ofcl}} > 0 ) {
		 
		 	push (@dates, $date);   
		    
		}    
	
	} 
	
	return \@dates;

}


#***********************************************************
#
#	SUBROUTINE NAME: getCARQ
#
#	DESCRIPTION: Adds CARQ records assocatied with a particular storm and
#		     date to a passed in array. 
#
#		     The user has an option not to not specify the date parameter.
#		     In that case if the date array only contains one entry
#		     the the CARQ records for that date will be returned.  
#		     If there is more then one date in the date array then an
#		     error is returned to the user.
#
#	USAGE (CALLING SEQUENCE): external Accessor Method
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF Object
#			Storm name
#			Pointer to an array
#			Delta - either the current, 6 hour previous or 12 hour previous CARQ
#			Optional - Date in YYYYMMDDHH format
#			
#
#		RETURNED: Return code
#				1 - Success
#				-1 - Must specify a date 
#				-2 - Passed in storm was not found in object 
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
sub getCARQ {

	#--- Initialize Variables
	
	my ($atcfObj, $storm, $raCARQ, $delta, $date) = @_;

	#--- Check storm is within $atcfObj hash

	unless (defined $$atcfObj{"storms"}{$storm}) {

		print ("Warning (atcfStorms::getCARQ): Storm $storm not contained within atcfObject.\n");
		return -2;
	}
	
	unless (defined $date) {
		
		if (@{$$atcfObj{"dates"}} > 1) {
		
			print ("Error(atcfStorms::getCARQ): Specifiy a calendar date.\n");
			return -1;
		
		}
		
		$date = $$atcfObj{dates}[0];
	}
	
	if ($delta == 0) {
		foreach my $ele (@{$$atcfObj{"storms"}{$storm}{$date}{"carq"}}) {
			push(@$raCARQ, $ele);
		}
		return 1;
	}
	if ($delta == 6) {
		foreach my $ele (@{$$atcfObj{"storms"}{$storm}{$date}{"carqM6"}}) {
                     	push(@$raCARQ, $ele);
                }
		return 1;
	}
	if ($delta == 12) {
		foreach my $ele (@{$$atcfObj{"storms"}{$storm}{$date}{"carqM12"}}) {
                        push(@$raCARQ, $ele);
                }
		return 1;
	}
	
	print "Incorrect time entered\n";
	return -1;

}

#***********************************************************
#
#	SUBROUTINE NAME: getCARQ
#
#	DESCRIPTION: Adds CARQ records assocatied with a particular storm and
#		     date to a passed in array. 
#
#		     The user has an option not to not specify the date parameter.
#		     In that case if the date array only contains one entry
#		     the the CARQ records for that date will be returned.  
#		     If there is more then one date in the date array then an
#		     error is returned to the user.
#
#	USAGE (CALLING SEQUENCE): external Accessor Method
#
#	VARIABLES AND THEIR USE
#		PASSED: ATCF Object
#			Storm name
#			Optional - Date in YYYYMMDDHH format
#			Pointer to an array
#
#		RETURNED: Return code
#				1 - Success
#				-1 - Must specify a date 
#				-2 - Passed in storm was not found in object 
#
#.................MAINTENANCE SECTION..................
#                                                                                   
#  LANGUAGE:  Perl 5.8.x                                               
#                                                             
#  RECORD OF CHANGES:                                         
#                                                            
#************************************************************
#sub getCARQ {
#
#	#--- Initialize Variables
#	
#	my ($atcfObj, $storm, $raCARQ, $date) = @_;
#
#	#--- Check storm is within $atcfObj hash
#
#	unless (defined $$atcfObj{"storms"}{$storm}) {
#
#		print ("Error(atcfStorms::getCARQ): Storm $storm not contained within atcfObject.\n");
#		return -2;
#	}
#	
#	unless (defined $date) {
#		
#		if (@{$$atcfObj{"dates"}} > 1) {
#		
#			print ("Error(atcfStorms::getCARQ): Specifiy a calendar date.\n");
#			return -1;
#		
#		}
#		
#		foreach my $ele (@{$$atcfObj{"storms"}{$storm}{$$atcfObj{dates}[0]}{"carq"}}) {
#			push(@$raCARQ, $ele);
#		}
#		return 1;
#		#return $$atcfObj{"storms"}{$storm}{$$atcfObj{dates}[0]}{"carq"};
#	
#	}
#	
#	foreach my $ele (@{$$atcfObj{"storms"}{$storm}{$date}{"carq"}}) {
#                push(@$raCARQ, $ele);
#        }
#        return 1;	
#	#return $$atcfObj{"storms"}{$storm}{$date}{"carq"};
#
#}


#***********************************************************
#
#       SUBROUTINE NAME: getCARQM6
#
#       DESCRIPTION: Adds CARQ records assocatied with a particular storm and
#                    (date - 6hour) to a passed in array.
#
#                    The user has an option not to not specify the date parameter.
#                    In that case if the date array only contains one entry
#                    the the CARQM6 records for that date will be returned.
#                    If there is more then one date in the date array then an
#                    error is returned to the user.
#
#       USAGE (CALLING SEQUENCE): external Accessor Method
#
#       VARIABLES AND THEIR USE
#               PASSED: ATCF Object
#                       Storm name
#                       Optional - Date in YYYYMMDDHH format
#                       Pointer to an array
#
#               RETURNED: Return code
#                               1 - Success
#                               -1 - Must specify a date
#                               -2 - Passed in storm was not found in object
#
#.................MAINTENANCE SECTION..................
#
#  LANGUAGE:  Perl 5.8.x
#
#  RECORD OF CHANGES:
#
#************************************************************
#sub getCARQM6 {
#
#	#--- Initialize Variables
#	
#	my ($atcfObj, $storm, $raCARQ, $date) = @_;
#
#	#--- Check storm is within $atcfObj hash
#
#        unless (defined $$atcfObj{"storms"}{$storm}) {
#
#                print ("Error(atcfStorms::getCARQM6): Storm $storm not contained within atcfObject.\n");
#                return -2;
#        }
#	
#	unless (defined $date) {
#		
#		if (@{$$atcfObj{"dates"}} > 1) {
#		
#			print ("Error(atcfStorms::getCARQM6): Specifiy a calendar date.");
#			return -1;
#		
#		}
#		
#		foreach my $ele (@{$$atcfObj{"storms"}{$storm}{$$atcfObj{dates}[0]}{"carqM6"}}) {
#                        push(@$raCARQ, $ele);
#                }
#		return 1;
#	
#	}
#	
# 	foreach my $ele (@{$$atcfObj{"storms"}{$storm}{$date}{"carqM6"}}) {
#               push(@$raCARQ, $ele);
#        }
#	return 1;	
#
#}




1;
