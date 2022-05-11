package generalUtils;

#! /usr/bin/env perl
#-w
use warnings;

use Date::Manip;
use File::stat;

#=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Program Name: generalUtils.pl
# Package Name: generalUtils
#
# Version: 1.2.4
#
# Language: Perl 5.8.x 
#
# Purpose:  The generalUtils package contains several helpful utilities
# 		 	functions that 
#
#		* FILE FUNCTIONS:
#
#				openFile(): open files
#			    findFiles(): find files that match a pattern
#				printFiles(): print files  
#				appendFiles(): append two files together 
#				isFile(): check if passed in string is a file
#			    fileErrorMsg(): returns the error message associated with code
#	
# 		  		 File utilities return codes:
#
#					1 success
#
#			 		0 no files found matching pattern
#					-1 file/directory does not exist
#					-2 error opening file/directory
#					-3 file is not writable
#					-4 file not readable
#					-5 file has zero size         
#
#
#		* DATE TIME FUNCTIONS
#
#				continuousDTG(): Takes a sorted array of date/time groups and an interval in integer hours 
#		   						 fills in any missing date/times.  
#				validDateFormat(): Checks that a string represents a valid date/time group in YYYYMMDDHH format.
#				nearestHour():	Rounds a string representing time in HHMM format to the nearest hour.
# 				previousTime(): Adds a users specified number of hours to a Date
#		   					    in YYYYMMDDHH format.  Returns the new Date as a string
#		   						to the calling function.
#				nextTime():	Adds a users specified number of hours to a Date
#		   					in YYYYMMDDHH format.  Returns the new Date as a string
#		   					to the calling function.
#				convert24to00(): Converts a date at 24Z to 00Z the next day.
#				synopticTime(): Converts a date/time string in YYYYMMDDHH format into a 
#								synoptic date/time based on a given interval.
#
# 				Date/time utilities return codes:
#
#					1 success
#
#					 0 No error but no action taken
#					-1 Undefined parameter	
# 					-2 Parameter format error
#					-3 Incorrect year
#					-4 Incorrect month
#					-5 Incorrect day
#					-6 Incorrect hour
#					-7 Incorrect minutes
#
# Called by: 	rcliper_data_v3.pl
#	     		rcliper_plot_v3.pl
#	     		rcliper_v6.pl
#				atcfStorms_v4.pl
#				aids_data.pl
#				btk_data.pl
#				ri.pl
#				ri_data_v2.pl
#----------------------------------------------------------------------
# Information:
#    Joint Hurricane Testbed/USWRP project
#    National Hurricane Center / Tropical Prediction Center
#    Department of Commerce - NOAA / NWS
#
# Category: JHT - Utilities
#
# Written by: Alison Krautkramer 07/2003
# Updated by: ARK 12/2003
#
# Updated by ARK 12/2004
# 
#   Added subroutine: validStormName
#
#	Subroutine: validStormName
#
# 	Purpose: Checks that the string passed into this subroutine
#		   represents a valid storm name in bb##YYYY format.
#
# Updated by: ARK 7/30/04
#	Include checkFileTransfer code
#	Found problem in the oldFile code
#
# Updated by ARK 12/2004
#
#	Added Subroutine: separateFile
#
#		 Purpose: Test whether the path/file_name points to a file.
#			This subroutine contains a zero size check.
#
# Updated by ARK 03/22/2005
#
#	Altered oldFile() routine so to use timelocal instead of the timegm
#	function to calculate the persent time in seconds.   
#
# Updated by ARK 09/15/2005
#
#	Separated   validDateFormat  into two functions: validDTGForamt and validYMDFormat
#
#	Now both YYYYMMDD and YYYYMMDDHH date format can be checked with validDateFormat
#
# Updated by ARK 10/17/2005
#	
#	Added conversion from local computer time zone in synopticTime to
#	GMT time.
#
#	Added IO and SH basins in the validStormName check
#
# Updated by ARK 4/4/2006
#	
#	Added checkNum function which checks that a string only contains digits.
#
#	Removed the check for file existance and size from checkFileTransfer
#
# Updated by Matt Sardi 3/20/2019 
#
#	Removed 'use lib "/usrx/local/pm5/share/perl5"' (now set via module pm5, 
#	envar $PERL5LIB prepended to $PATH).
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=



#--------------------------------------------------------
# Subroutine: fileErrorMsg
#
# Purpose: Returns a descriptive error message for a given
#		   file error code.
#
# Inputs: $_[0] - error code 
#
#	 		 1 - success 	
#           -1 file/directory does not exist
#			-2 error opening file/directory
#			-3 file is not writable
#			-4 file not readable
#			-5 file has zero size
#
# Outputs: String describing error code         
#--------------------------------------------------------
sub fileErrorMsg {

	#---   Define local variables
	
	my ($error_code) = @_;

	#---   Match error code to known codes

	if ($error_code == 1) {
	
		return "Success - subroutine completed successfully";
	
	}
	elsif ($error_code == 0) {
	
		return "No files matching the pattern found";
		
	}
	elsif ($error_code == -1) {
	
		return "Error - file/directory does not exist";
	
	}
	elsif ($error_code == -2) {
	
		return "Error - error opening file/directory";
	
	}
	elsif ($error_code == -3) {
	
		return "Error - file is not writable";
	
	}
	elsif ($error_code == -4) {
		
		return "Error - file not readable";
	
	}
	elsif ($error_code == -5) {
	
		return "Error - file has zero size";
	
	}
	
	#---   No matching error code found
	
	return "Unknown file utilities error code.";
	

}

#--------------------------------------------------------
# Subroutine: appendFile
#
# Purpose: Adds the files pointed to by the path/file_name and 
#		   adds its contents to the file associated with the 
#		   file handle.
#
# Inputs: $_[1] - absolute path to file to add to original file
#		  $_[2] - add file name
#		  $_[3] - orig file handle
#
# Outputs: 1 - success
#		   Handle to the file 
#
# Errors:  -1 file does not exist
#		   -2 error opening file
#		   -4 file is not readable
#		   -5 file has zero size
#--------------------------------------------------------
sub appendFile {

	#--- Define local variables
	
	my ($path_add, $filename_add, $fileHandle) = @_;
	my $returnValue;
	my $line;
	
	#--- Open the file for reading
	
	$returnValue = isFile ($path_add, $filename_add);
	if ($returnValue == 1) {
			
		#--- Check that the file is readable
		
		if (-r "$path_add/$filename_add") {
			
			open (ADD, "$path_add/$filename_add") || return -2;
			
		}
		else {
		
			#---return error: not readable
			
			return -4;
		}
		
	}
	elsif ($returnValue == -5) {
	
		#--- return error: file has zero size
		
		return $returnValue;
	}	
	else {
		return $returnValue;
	}
	
	#--- Write the contents of the file to be added to the original file
	
	while (defined($line = <ADD>)) {
	
		printf ($fileHandle $line);
		
	}	
	
	#--- Close the file
	
	close (ADD);
	return 1;
}


#--------------------------------------------------------
# Subroutine: findFiles
#
# Purpose: Creates a list of files that match a pattern in a given
#	       directory.
#
# Inputs: $_[0] - absolute path 
#		  $_[1] - filename pattern
#		  $_[2] - list of files found
#
# Outputs: 1 - success, one or more files match the given pattern
#		   0 - no files found that match the given pattern
#
# Errors: -1 directory does not exist
#	  -2 error opening directory 
#
#--------------------------------------------------------
sub findFiles {

	#---Define local variables
	
	my ($path, $pattern, $list_ref) = @_;
	
	my $DIR = new IO::File;
	my $file = "";
	
	#--- Open the directory passed in by the user
	
	unless (opendir ($DIR, $path)) {
	 
	 	print ("Error (generalUtils::findFiles): Could not open $path: $! \n"); 
		return -1;
	
	}
	
	#--------------------------------------------------------
	#For each entry found in the directory, compare it
	#to the pattern passed in by the user.  
	#If the file:
	#	
	#	* matches the pattern 
	#		AND
	#   	* the file is a valid file
	#		AND
	#	* the file has non-zero size 
	#
	#then add it to an array of files to be passed back
	#to the caller.
	#-----------------------------------------------------------
	
	while (defined ($file = readdir($DIR))) {
		
		#--- Check if the file name matches the pattern
		
		next unless ($file =~ /$pattern/i);
		
		#--- Check if the file name found to match the pattern is
		#--- truely a file and has non-zero size.
		
		$returnIsFile = isFile($path, $file);
		if ($returnIsFile == 1) {
		
			$$list_ref[@$list_ref] = $file;
			
		}
	}
	
	#--- close the directory passed in by the user
	
	closedir ($DIR);
	
	if (@$list_ref > 0) {
	
		return 1;
		
	}
	
	return 0;
}

#--------------------------------------------------------
# Subroutine: isFile
#
# Purpose: Test whether the path/file_name points to a file.
#			This subroutine contains a zero size check.
#
# Inputs: $_[0] - absolute path to a file
#		  $_[1] - filename
#
# Outputs: 1 - success
#
# Errors:  -1 file does not exist
#		   -5 file has with zero size 
#--------------------------------------------------------
sub isFile {
	
	#--- Define local variables
	
	my($path, $file) = @_;
	
	#--- Check that $path is not a directory
	
	if (! (-d "$path/$file")) {
		
		#--- Check that $path is a file
		
		if (-e "$path/$file") {
			
			#--- Check that the file size neq 0
			
			if (-z "$path/$file") {
			
				return -5;	#--- return error: file has zero size
			
			}
			else {
			
				return 1;
			
			}
		}
		else {
			
			return -1;	#--- Return error: file does not exist
		
		}
	}
	else {
		
		return -1;	#--- Return error: file does not exist
		
	}
}


#--------------------------------------------------------
# Subroutine: separateFile
#
# Purpose: Test whether the path/file_name points to a file.
#			This subroutine contains a zero size check.
#
# Inputs: $_[0] - absolute path to a file
#	 $_[1] - ref filename
#	$_[2] - ref directory
#
# Outputs: 1 - success
#
# Errors:  -1 file does not exist
#		   -5 file has with zero size 
#--------------------------------------------------------
sub separateFile {
	
	#--- Define local variables
	
	my($path, $rFile, $rDir) = @_;
	
	my @parsedLine = split(/\/+/, $path);
	
	if (@parsedLine <= 1) {
	
		print ("Error (generalUtils::separateFile): Invalid absolute file name format. \n");
		return -1;
	}
			
	#--- parse the file name
			
	$$rFile = pop(@parsedLine);
			
	#--- parse the directory name
	
	$$rDir = "";		
	foreach my $element (1 .. $#parsedLine) {
				
		$$rDir = $$rDir . "/" . $parsedLine[$element];
				
	}
	
	return 1;
}


#--------------------------------------------------------
# Subroutine: openFile
#
# Purpose: Opens a file either for reading or writing and passes
#			back a file handle to the caller.
#
# Inputs: $_[0] - absolute path to a file
#		  $_[1] - file name
#		  $_[2] - read/write
#		  $_[3] - reference to a file handle 
#
# Outputs: 1 - success
#		   Handle to the file 
#
# Errors:  -1 file does not exist
#		   -2 error opening file
#		   -3 file is not writable
#		   -4 file is not readable
#		   -5 file has with zero size
#--------------------------------------------------------
sub openFile {

	#--- Define local variables
	
	my ($path, $filename, $operation, $fileHandle_ref) = @_;
	my $returnValue;

	#--- check that the file is writable
	
	if ($operation eq 'write') {
	
		if (-w $path) {
			
			$$fileHandle_ref->open(">$path/$filename") || return -2;
			return 1;
			
		}
		else {
				
				return -3;	#return error: not writable
				
			}
	}
			
	#--- Check that the file is readable and exists
	
	elsif ($operation eq 'read') {
			
		$returnValue = isFile ($path, $filename);
			
		if (($returnValue == -5) || ($returnValue == 1)) {
			
			if (-r "$path/$filename") {
			
				#--- Check that the file size neq 0
					
				if ($returnValue == -5) {
						
					return $returnValue;	#return error: file has zero size
					
				}
				else {
				  		
				  	$$fileHandle_ref->open("$path/$filename") || return -2;
					return 1;
						
				}
			}
			else {
					
				return -4;	#return error: not readable
					
			}
		}
		else {
			
			return $returnValue;
				
		}
	}
}

#--------------------------------------------------------
# Subroutine: printFile
#
# Purpose: Sends the file uniquely specified by $path/$file
#		   to a printer.
#
# Inputs: $_[0] - absolute path to a directory
#		  $_[1] - name of file to print
#		  $_[2] - printer to use
#
# Outputs: 1 - success
#
# Errors:  -1 - file does not exist
#		   -5 - file is empty
#--------------------------------------------------------
sub printFile {

	#--- Define local variables
	
	my ($path, $file, $printer) = @_;
	
	#--- Check that the file exists
	
	$returnIsFile = isFile ($path, $file);
	
	if ($returnIsFile != 1) {
	
		printf ("Error $returnIsFile (generalUtils::printFile): File $path/$file does not exist.\n");
		return $returnIsFile;
	
	}
	
	system("lp -d$printer $path/$file\n");
	
	return 1; 
		
}


#--------------------------------------------------------
# Subroutine: oldFile
#
# Purpose: Figures out if a file is older then a passed in value of 
#		   minutes
#
# Inputs: $_[0] - absolute path to a directory
#	  $_[1] - file name
#	  $_[2] - age limit in minutes
#
# Outputs: 1 - older then passed in time limit
#	   0 - younger then passed in time limit
#
# Errors:  -1 - file does not exist
#--------------------------------------------------------
sub oldFile {

	#--- Define local variables
	
	my ($path, $file, $ageLimit) = @_;
	
	#--- Check that the file exists
	
	$returnIsFile = isFile ($path, $file);

	unless ($returnIsFile == 1 || $returnIsFile == -5) {
	
		printf ("Error $returnIsFile (generalUtils::oldFile): File $path/$file does not exist.\n");
		return $returnIsFile;
	
	}
	
	#--- Calcuate the age of the file by querying the file for its last
	#--- modification time.  Note: Modification times are given in Epoch
	#--- seconds.
	
	use Time::Local;
	
	my $fileStats = stat("$path/$file");
 	my $fileAge = $fileStats->mtime;	
	
	#my $fileAge = (stat("$path/$file"))[9];
	#my $diff = (timegm(localtime()) - $fileAge);
	my $diff = (timelocal(localtime()) - $fileAge);

	if ($diff > ($ageLimit*60)) {
	
		return 1;	#old file
		
	}
	
	return 0;	#newer file
	
		
}




#--------------------------------------------------------
# Subroutine: checkFileTransfer
#
# Purpose: In the case that a file is being ftp'd/rcp'd to the local computer
#	   this subroutine will make sure that:
#	   
#	   1. The path/file exists on the local computer
#	   2. The path/file if fully transfered to the local computer.
#	      It is assumed that if a path/file does not change size over
#	      the course of $shortWait (10seconds) that the file is no
#	      longer in the process of being copied to the local machine.
#
# Calls: isFile()
#
# Inputs: $dir - (string) - absolute path to file
#	  $file - (string) - name of file
#	  $totalWait - (int) - total seconds wait for the file to be rcp'd/ftp'd
#
# Outputs: 1 - success
#
# Errors:  -1 - Time out, file not found 
#	   -4 - File not fully transfered
#	   -5 - File has zero size
#--------------------------------------------------------
sub checkFileTransfer {
	
	#---  Define local variables
	
	my ($dir, $file, $totalWait) = @_;
	
	my $seconds = 0;	#total number of seconds elapsed searching for the file
	my $wait = 15;		#15 seconds between checking the existance the file
	my $shortWait = 1;	#1 seconds between checking inodes of a file
	
	
	#----------------------------------------------------------------
	#  While the file does not exist and looking for the file has not
	#  timed out, keep looking and waiting for the file to show up.
	#------------------------------------------------------------------
	
	
	while (!((-e "$dir/$file") || ($seconds > $totalWait))) {
	
		sleep ($wait);
		$seconds = $seconds + $wait;
		
	}
	
	#--- Check the file exist and has a non-zero size.
	#
	#if (isFile ($dir,$file) != 1) {
	#
	#	#either the file was not found or had zero size
	#	return -1;
	#	
	#}
	
	
	#----------------------------------------------------------------
	# Check that the file has been fully transfered and it not still
	# in the transfer process	
	#----------------------------------------------------------------
	
	
	my $start_inode = stat("$dir/$file");
	sleep ($shortWait);
	my $end_inode = stat("$dir/$file");
	
	$seconds = $shortWait;
	
	while (!(($end_inode->size == $start_inode->size) || ($seconds > $totalWait))) {
		
		$start_inode = stat("$dir/$file");
		sleep ($shortWait);
		$end_inode = stat("$dir/$file");
		
		$seconds = $seconds + $shortWait;
	
	}
	
	#--- Time out --- File never fully transferred
	
	if ($end_inode->size != $start_inode->size) {
	
		return -4;
	
	}
	
	#--- Success
	
	return 1;

}



#----Meteorological Time Routines -----------------


#--------------------------------------------------------
# Subroutine: dtgErrorMsg
#
# Purpose: Returns a descriptive error message for a given
#		   file error code.
#
# Inputs: $_[0] - error code 
#
#	 			 0 No error but no action taken
#				-1 Undefined parameter	
# 				-2 Parameter format error
#				-3 Incorrect year
#				-4 Incorrect month
#				-5 Incorrect day
#				-6 Incorrect hour
#				-7 Incorrect minutes
#
# Outputs: String describing error code         
#--------------------------------------------------------
sub dtgErrorMsg {

	#---   Define local variables
	
	my ($error_code) = @_;

	#---   Match error code to known codes

	if ($error_code == 1) {
	
		return "Success - subroutine completed successfully";
	
	}
	elsif ($error_code == 0) {
	
		return "No error occured but not action taken";
		
	}
	elsif ($error_code == -1) {
	
		return "Error - Undefined parameter";
	
	}
	elsif ($error_code == -2) {
	
		return "Error - Parameter format error";
	
	}
	elsif ($error_code == -3) {
	
		return "Error - Invalid year format";
	
	}
	elsif ($error_code == -4) {
		
		return "Error - Invalid year month";
	
	}
	elsif ($error_code == -5) {
	
		return "Error - Invalid year day";
	
	}
	elsif ($error_code == -6) {
		
		return "Error - Invalid year hour";
	
	}
	elsif ($error_code == -7) {
	
		return "Error - Invalid year minutes";
	
	}
	
	#---   No matching error code found
	
	return "Unknown file utilities error code.";
	

}


#--------------------------------------------------------
# Subroutine: continuousDTG
#
# Purpose: Takes a sorted array of date/time groups and 
#		   fills in any missing times.  
#
# Inputs: $_[0] - reference to a sorted array of date/times
#		  $_[1] - reference to an empty array where a continuous 
#				  array of date/times will be stored
#		  $_[2] - the time interval (in hours) between the date/time groups
#				  in the continuous array
#
#				  Examples:
#
#					$interval = 1 	one hour between subsequent date/times
#					$interval = 6 	6 hours between date/times
#
# Outputs: reference to a sorted array of date time groups
#		   without any time gaps
# 
#--------------------------------------------------------
sub continuousDTG {


    #--- Define local variables

	my ($raSortedDTG, $raCompleteDTG, $interval) = @_;
	my $nextDTG;
	
	#--- Error checking
	
	if (!defined ($raSortedDTG) || !defined ($$raSortedDTG[0])) {
		
		printf ("Error (generalUtils::continuousDTG): No elements in the sorted dtg array.\n");
		return -1;
	
	}
	
 	#--- Get the next date/time group $interval hours in the future
	
	$$raCompleteDTG[0] = $$raSortedDTG[0];
	
	if (validDateFormat($$raCompleteDTG[0]) != 1) {
	
		printf ("Error (generalUtils::continuousDTG): Invalid DTG elements within the sorted dtg array.\n");
		return -2;
	
	}
	
	$nextDTG = nextTime($$raSortedDTG[0], $interval);
	
	for (my $i  = 0; $i < (@$raSortedDTG - 1); $i++) {
		
		my $found = 0;	#the date/time group was found in the sorted array
		my $break = 0;	#an upper limit was hit when looking for date/time groups
		
		while (($found == 0) && ($break < 40)) {
		
			#--------------------------------------------------------------
			# The next date/time was not found in the sorted array
			# so add the value to the complete sorted Array
			#--------------------------------------------------------------
			
			if ($$raSortedDTG[$i+1] != $nextDTG) {
			
				$$raCompleteDTG[@{$raCompleteDTG}] = $nextDTG;
			
			}
			
			#--------------------------------------------------------------
			# The next date/time was found in the sorted array
			# so enter the value in the complete sorted array
			# and move to the next date/time stored in the sorted array
			#--------------------------------------------------------------
			
			else {
			
				$$raCompleteDTG[@{$raCompleteDTG}] = $$raSortedDTG[$i + 1];
				$found = 1;
			
			}
			
			#--- Get the next date/time group $interval hours in the future
			
			if (validDateFormat($nextDTG) != 1) {
	
				printf ("Error (generalUtils::continuousDTG): Invalid DTG elements within the sorted dtg array.\n");
				return -2;
	
			}
			
			$nextDTG = nextTime($nextDTG, $interval);
			$break++;
			
		}
		
	}

}


#--------------------------------------------------------
# Subroutine: synopticTime
#
# Purpose: Converts a date/time string in YYYYMMDDHH format into a 
#			synoptic date/time.
#
#		  	The synoptic date/time returned to the caller is controlled
#			by the interval argument. With this argument the user can
#			specify either the current synoptic time, any previous synoptic
#			time or a future synoptic time.
#
# Packages: Requires use Date::Manip
#
# Inputs: $_[0] - string indicating the date either in 
#					* YYYYMMDDHH format
#				  		OR 
#					* with the key word 'today'
#
#		  $_[1] - the number of synoptic periods either in the future or
#				  in the past
#				   -n - n previous synotic date/time
#				   -1 - 1 previous synoptic date/time
#					0 - current synoptic date/time
#					1 - 1 future synoptic date/time
#					n - n future synoptic date/time
#
#		  $_[2] - reference to a variable for the new synoptic date
#
# Example call: generalUtils::synopticTime("2003093100", 3, \$synoptic_date);
#
#				return the synoptic date/time three periods in the future.
#				For this example the date/time returned is 2003093118.
#
# Outputs: 1 - success
#
# Errors:   -2 Incorrect number of characters
#			-3 Incorrect year
#			-4 Incorrect month
#			-5 Incorrect day
#			-6 Incorrect hour
#
# Modified : 10/17/2005 Convert time zone from system time zone to GMT
#--------------------------------------------------------
sub synopticTime {


	#--- Define Variables

	my ($dateString, $interval, $rSynopticDate) = @_;
	
	#--- Check inputs
	
	if (!defined($dateString) || !defined($interval)) {
	
		return -1;	#undefined inputs
	
	}
	
	$dateString =~ tr/A-Z/a-z/;

	#--- assume the date is in the current systems time zone
        #--- so convert the date string into GMT
	
	if ($dateString eq "today") {
	
		$newDateString= Date::Manip::ParseDate($dateString);
		$dateString = Date_ConvTZ($newDateString, "", "GMT");	
	}
	
	elsif (validDateFormat($dateString) != 1) {
	
		return validDateFormat($dateString);	#date format error
	
	}
	
	$dateString = Date::Manip::ParseDate($dateString);

	#--- Check that the time contains digits between 0 and 9
	
	if ($interval !~ /[0-9]+/) {
		
		return -1;	
	
	}
	
	#--- Parse the date object returned into the correct format
	
	my $hour = Date::Manip::UnixDate($dateString, '%H');
		
	#--- Find the last previous synoptic time (0,6,12,18)

	my $remainder = $hour % 6;
	$hour = (6 * $interval) - $remainder;
	
	my $synopticDate = Date::Manip::DateCalc($dateString, " $hour hours");

	#--- Put the dates in the correct YYYYMMDDHH format

	$$rSynopticDate = Date::Manip::UnixDate($synopticDate, '%Y%m%d%H');


	return 1;

}


#--------------------------------------------------------
# Subroutine: validDateFormat
#
# Purpose: Checks that the string passed into this subroutine
#		   represents a valid storm name in bb##YYYY format.
#
# Inputs: $_[0] - string indicating the storm in bb##YYYY format
#				  
#
# Outputs: 1 - valid
#
# Modified: 10/17/2005 - Include the IO and SH basins 
#--------------------------------------------------------
sub validStormName {


	#--- Define Variables

	my ($stormString) = @_;
	
	#--- Check that the stormString is defined 
	
	unless (defined $stormString)  {
		
		return -1;	#undefined element 
	
	}
	
	#--- Check overall format
	
	unless ($stormString =~ /(io|sh|ep|al|cp|wp)[0-9]{6}$/i) {
	
		return -2;	#incorrect format 
	
	}
	
	#--- Check that the year is between 1950 and 2050
	
	if ((substr($stormString,4,4) < 1950) || (substr($stormString, 4, 4) > 2050))  {
	
			return -3; #incorrect year
			
	}
	
	return 1;
	
}


#--------------------------------------------------------
# Subroutine: validDateFormat
#
# Purpose: Checks that the string passed into this subroutine
#	   represents a valid date.  Both YYYYMMDDHH format and
#	   YYYYMMDD format can be checked.
#
# Inputs: $_[0] - string indicating the date in YYYYMMDDHH format
#		  or YYYYMMDD format
#				  
#
# Outputs: 1 - valid
#
# Errors:   -2 Incorrect number of characters
#			-3 Incorrect year
#			-4 Incorrect month
#			-5 Incorrect day
#			-6 Incorrect hour
#--------------------------------------------------------
sub validDateFormat {


	#--- Define Variables

	my ($dateString) = @_;
	
	#--- Find the number of numbers in the $dateString
	
	$_ = $dateString;
	my $count = tr/0-9//;
	
	#--- Check for the correct number of characters
	
	if ($count == 10) {
	
		return validDTGFormat($dateString);
		
	}
	elsif ($count == 8) {
	
		return validYMDFormat($dateString);
	
	}
	
	return -2; #Incorrect number of characters
	
}


#--------------------------------------------------------
# Subroutine: validDTGFormat
#
# Purpose: Checks that the string passed into this subroutine
#	   represents a valid date/time group in YYYYMMDDHH format.
#
# Inputs: $_[0] - string indicating the date in YYYYMMDDHH format
#				  
#
# Outputs: 1 - valid
#
# Errors:   -2 Incorrect number of characters
#			-3 Incorrect year
#			-4 Incorrect month
#			-5 Incorrect day
#			-6 Incorrect hour
#--------------------------------------------------------
sub validDTGFormat {

	#--- Define Variables

	my ($dateString) = @_;
	
	#--- Check the YMD portion of the date
	
	unless (validYMDFormat(substr($dateString, 0, 8)) == 1) {
	
		return -1; #Wrong YMD format
	
	}
	
	#--- Check that the time is between 0Z and 24Z
	
	if ((substr($dateString, 8, 2) < 0) || (substr($dateString, 8, 2) > 24)) {
	
		return -6; #Incorrect hour
		
	}
	
	
	return 1;

}	


#--------------------------------------------------------
# Subroutine: validYMDFormat
#
# Purpose: Checks that the string passed into this subroutine
#	   represents a valid date/time group in YYYYMMDD format.
#
# Inputs: $_[0] - string indicating the date in YYYYMMDD format
#				  
#
# Outputs: 1 - valid
#
# Errors:   -2 Incorrect number of characters
#			-3 Incorrect year
#			-4 Incorrect month
#			-5 Incorrect day
#			-6 Incorrect hour
#--------------------------------------------------------	
sub validYMDFormat {

	#--- Define Variables

	my ($dateString) = @_;
	
	#--- Check that the year is between 1950 and 2050
	
	if ((substr($dateString,0,4) < 1950) || (substr($dateString, 0, 4) > 2050))  {
	
		return -3; #incorrect year
			
	}
	
	#--- Check that the month is between January(1) and December(12)
	
	if ((substr($dateString, 4, 2) > 12) || (substr($dateString, 4, 2) < 1)) {
	
		return -4; #Incorrect month
		
	}
	
	#--- Check that the day is between the 1st and the 31st
	
	if ((substr($dateString, 6, 2) < 1) || (substr($dateString, 6, 2) > 31)) {
	
		return -5; #Incorrect day
		
	}
	
	
	return 1;

}


#--------------------------------------------------------
# Subroutine: nearestHour
#
# Purpose: Rounds a string representing time in HHMM format
#		   to the nearest hour.
#
#				* if MM <= 30 then return HH
#				* if MM > 30 then return HH + 1
#
# Inputs: $_[0] - string indicating the date in HHMM format
#				  
# Outputs: returns the hour
#
# Errors:  -1	undefined element or format error
#		   -2	incorrect time format
#		   -6 	incorrect hour format
#		   -7	incorrect minutes format
#--------------------------------------------------------

sub nearestHour {

	
	#--- Define Variables

	my ($time) = @_;
	
	
	#------------------------
	# Error/format checking
	#------------------------
	
	
	#--- Check that the time is defined 
	
	if (!defined($time))  {
		
		return -1;	#undefined element or format error
	
	}
	
	#--- Find the number of numbers in the $time
	
	$_ = $time;
	my $count = tr/0-9//;
	
	if ($count != 4) {
	
		return -2;	#incorrect format 
	
	}
	
	
	#-----------------------
	# Rounding
	#-----------------------
	
	
	#--- Find the number of minutes
	
	my $minutes = $time % 100;
	
	if ($minutes > 60) {
	
		return -7;	#incorrect minutes format
	
	}
			
	if ($minutes <= 30) {
			
		return substr($time, 0, 2);		#return the hour portion of the string	
					
	}
	else {
			
		my $hour = substr($time, 0, 2) + 1;
		
		if ($hour > 24) {
		
			return -6; 	#incorrect hour format
		
		}
				
		if ($hour < 10) {
		
			return "0" . $hour;
		
		}
		
		return $hour;
				
	}

}


#--------------------------------------------------------
# Subroutine: convert24to00
#
# Purpose: Converts a date at 24Z to 00Z the next day.
#
# Inputs: $_[0] - reference to a string indicating the date in YYYYMMDDHH format				  
## Packages: Requires use Date::Manip
# Outputs: 1 - success
#
# Errors: -2 undefined element
#		   0 date/time is at 24Z 
#--------------------------------------------------------

sub convert24to00 {

	
	#--- Define Variables

	my ($rTime) = @_;
	
	#--- Check $time for the correct format
	
	if (!defined($rTime) || (validDateFormat($$rTime) != 1)) {
		
		return -2;	#format error
	
	}
	
	my $day = substr($$rTime, 0, 8);
	my $hour = substr($$rTime, 8, 2);
	
	#--- Convert the date/time if the hour is equal to 24
	
	if ($hour == 24) {
	
		my $newDate = Date::Manip::ParseDate($day);
		$newDate = Date::Manip::DateCalc($newDate, "+ 1 day");
		$$rTime = Date::Manip::UnixDate($newDate, "%Y%m%d") . "00";
		
		return 1;
	
	}
	
	return 0;	#date time is not at 24 hour
	
	

}

#--------------------------------------------------------
# Subroutine: nextTime
#
# Purpose: Adds a users specified number of hours to a Date
#		   in YYYYMMDDHH format.  Returns the new Date as a string
#		   to the calling function.
## Packages: Requires use Date::Manip
# Outputs: A Date time in the YYYYMMDDHH format that is $interval hours
#		   after the time passed into this function
#
# Errors: -1 undefined input elements
#		  -2 format error
#--------------------------------------------------------
sub nextTime {

	#--- Define Variables
	
	my ($date, $interval) = @_;
	
	#--- Check date variable and interval
	
	if (!defined($date) || !defined($interval)) {
	
		return -1; # undefined inputs
	
	}
	
	if (($interval !~ /[0-9]+/) || (validDateFormat($date) != 1)) {
	
		return -2; #format error 
	
	}
	
	#--- Calculate date/time interval hours in the future
	
	my $currentDate = Date::Manip::ParseDate($date);
	my $newDate = Date::Manip::DateCalc($currentDate, "+ $interval hours");

	return Date::Manip::UnixDate($newDate, "%Y%m%d%H");
	
}


#--------------------------------------------------------
# Subroutine: previousTime
#
# Purpose: Adds a users specified number of hours to a Date
#		   in YYYYMMDDHH format.  Returns the new Date as a string
#		   to the calling function.
## Packages: Requires use Date::Manip
# Called by: createPlot
#
# Outputs: A Date time in the YYYYMMDDHH format that is $interval hours
#		   after the time passed into this function
#
# Errors: -1 undefined input elements
#		  -2 format error 
#--------------------------------------------------------
sub previousTime {

	#--- Define Variables
	
	my ($date, $interval) = @_;
	
	#--- Check date variable and interval
	
	if (!defined($date) || !defined($interval)) {
	
		return -1; # undefined inputs
	
	}
	
	if (($interval !~ /[0-9]+/) || (validDateFormat($date) != 1)) {
	
		return -2; #format error 
	
	}
	
	#--- Calculate date/time interval hours in the future
	
	my $currentDate = Date::Manip::ParseDate($date);
	my $newDate = Date::Manip::DateCalc($currentDate, "- $interval hours");

	return Date::Manip::UnixDate($newDate, "%Y%m%d%H");
	
}






#----Array Manipulation Routine -----------------


#--------------------------------------------------------
# Subroutine: findMax
#
# Purpose: Finds the maximum numerical value within an array.
#		   The maximum values is returned to the user.
#
#			note: The array can contain undef values 
#
# Inputs: $_[0] - reference to an array
#
# Outputs: 
#
# Errors:  -1 - error could not find maximum
#--------------------------------------------------------

sub findMax {

	#--- Define local variables
	
	my ($rArray) = @_;
	my $i;					#-- Counter variable
	my $max;				#-- Current maximum
	
	#--- If a defined array with some elements was passed in then
	#--- loop through the array and compare each value to the current
	#--- maximum.  If the value is greater then the current maximum
	#--- then that value becomes the current maximum.
	
	if (defined ($rArray) && (@$rArray > 0)) {
		
		#--- Initially set the maximum value too the first defined 
		#--- value in the array
		
		for ($i = 0; $i < @$rArray; $i++ ) {
		
			if (defined($$rArray[$i])) {
			
				$max = $$rArray[$i];
			
			}
		
		}
		
		#--- Compare all other defined values in the array to $max
		
	
		for ($i = 0; $i < @$rArray; $i++) {
		
			if (defined($$rArray[$i]) && ($$rArray[$i] > $max)) {
			
				$max = $$rArray[$i];
			
			}
		
		}
		
		#--- Returns the maximum value
		
		if (defined ($max)) {
		
			return $max;
		
		}
	}
	
	#--- Returned if the passed in array is 
	#				- not defined
	#				- does not contain any elements
	#				- does not have any defined elements
	
	return -1;
	
}

#--------------------------------------------------------
# Subroutine: findMin
#
# Purpose: Finds the minimum numerical value within an array.
#		   The minimum values is returned to the user.
#
# Inputs: $_[0] - reference to an array
#
#			note: The array can contain undef values 
#
# Outputs: minimum
#
# Errors:  -1 - error could not find minimum
#--------------------------------------------------------
sub findMin {

	#--- Define local variables
	
	my ($rArray) = @_;
	my $i;				#-- Counter variable
	my $min;			#-- Current maximum
	
	#--- If a defined array with some elements was passed in then
	#--- loop through the array and compare each value to the current
	#--- minimum.  If the value is greater then the current minimum
	#--- then that value becomes the current minimum.
	
	if (defined ($rArray) && (@$rArray > 0)) {
	
		#--- Initially set the minimum value too the first defined 
		#--- value in the array
		
		for ($i = 0; $i < @$rArray; $i++ ) {
		
			if (defined($$rArray[$i])) {
			
				$min = $$rArray[$i];
			
			}
		
		}
		
		#--- Compare all other defined values in the array to $min
		
		for ($i = 0; $i < @$rArray; $i++) {
		
			if ((defined($$rArray[$i])) && ($$rArray[$i] < $min)) {
			
				$min = $$rArray[$i];
			
			}
		
		}
		
		#--- Returns the minimum value
		
		if (defined ($min)) {
		
			return $min;
		
		}
		
		
	}
	
	#--- Returned if the passed in array is 
	#				- not defined
	#				- does not contain any elements
	#				- does not have any defined elements
	
	return -1;
	
}

#--------------------------------------------------------
# Subroutine: checkInt
#
# Purpose: Check that a value only contains digits and up to 
#	   one negative sign.
#
# Inputs: $_[0] - string
#	  $_[1] - LOG
#
# Outputs: none
#
#--------------------------------------------------------

sub checkInt {

	my ($value, $LOG) = @_;
	
	my $origValue = $value;
	
	#--- Allow for one preceding negative sign
		
	if (substr($value,0,1) eq '-') {
		
		$value = substr($value,1,100);
		
	}
	
	#--- Check if $value  contains at least one digit and only digits
	
	if (($value !~ /[0-9]/) or ($value =~ /\D+/)) {
	
		print ($LOG "Error: All digits expected in $origValue.\n");
		return -1; #  All digits expected in $value
	}

	
	return 1;

}

#--------------------------------------------------------
# Subroutine: checkFloat
#
# Purpose: Check that a value only contains digits, up to one negative
#	   sign and up to one decimal point
#
# Inputs: $_[0] - string
#	  $_[1] - LOG
#
# Outputs: none
#
#--------------------------------------------------------

sub checkFloat {

	my ($value, $LOG) = @_;
	
	my $origValue = $value;
	
	#--- Allow for one preceding negative sign
		
	if (substr($value,0,1) eq '-') {
		
		$value = substr($value,1,100);
		
	}
	
	#--- Allow for one decimal point
		
	if ($value =~ /(.*)\.(.*)/) {
		
		$value = $1 . $2;
		
	}
	
	#--- Check if $value  contains at least one digit and only digits
	
	if (($value !~ /[0-9]/) or ($value =~ /\D+/)) {
	
		print ($LOG "Error: All digits expected in $origValue.\n");
		return -1; #  All digits expected in $value
	}

	
	return 1;

}



#-----------------Log File routine  ------------------------------------

#--------------------------------------------------------
# Subroutine: header
#
# Purpose: Prints a standard header to the LOG file
#
# Inputs: $_[0] - message
#		  $_[1] - LOG
#
# Outputs: none
#
#--------------------------------------------------------
sub header{

	#---define local variables
	
	my($message,  $LOG) = @_;
	
	printf ($LOG "#=========================================================\n");
	printf ($LOG "#   $message " .  localtime() . "\n");
	printf ($LOG "#=========================================================\n\n");

}




1;  # To signify successful initialization
