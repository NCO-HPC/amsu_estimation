#==============================================================================
#
# Makefile for:  The CIRA JHT project to estimate 
# 		 tropical cyclone intensity and wind structure using data from 
# 		 the Advanced Microwave Sounding Unit on the GOES satellite
#		 platform.
#
# This version is for the IBM
#
# Notes: make  - compilies the two executables (pickavn.exe and oparet.exe)
#   	 make clean  - removes the object files and executables 
#   	 make pickavn.exe  - compilies the executable pickavn.exe
#   	 make oparet.exe   - compilies the executable oparet.exe
#   	 make install	- places the *.exe files in the ../bin directory
#
# Executables:  pickavn.exe 
#		oparet.exe
#
# Written by J. Knaff and M. DeMaria, May 2003
# Modified by: A. Krautkramer 5/19/03
#		Added comments and standardized makefile
# Modified by: ARK for    IBM 02/10/2005
# Modified by: ARK for  WCOSS 11/23/2012
# Modified by: MAS for WCOSS2 07/22/2021
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define default macros --
#-----------------------------------------------------------------------------

#--------------------
# Define default shell
#--------------------

SHELL = /bin/sh

RM	= rm -rf

CP	= cp -f

LOC	= ../../exec

#-----------------------------------------------------------------------------
#                       -- Define file lists --
#-----------------------------------------------------------------------------

#---------
# Libraries
#---------

FLIBS	=

#--------
# Includes
#--------

FINCS =

#------------
# Source/Object/Executable files
#------------

SRC	= pickgfs.f utils.f
OBS	= $(SRC:.f=.o)

CMD = amsu_pickgfs
#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------

#------------------
# Compiler
#------------------

#... HP F90 compiler
#FC	= f90	

#... Linux PG F90 compiler
#FC	= pgf90

#... IBM AIX F90 compiler
#FC      = xlf_r
#FC	= ncepxlf

#... WCOSS2 compiler
FC 	= ftn

#-----------------------------
# Compiler Flags 
#-----------------------------

#DEBUG_FLAGS=-g
#COMMON_FLAGS=$(DEBUG_FLAGS) +O2 -DA2.0 	# HPUX
#COMMON_FLAGS=$(DEBUG_FLAGS)  -DA2.0 -g77libs	# Linux
# WCOSS2:
COMMON_FLAGS=-O3 -fp-model precise -align all

#------------ 
# Common Flags 
#------------

FC_FLAGS=$(COMMON_FLAGS) -c
FL_FLAGS=$(COMMON_FLAGS) -o

#-------------
# Make program
#------------

all: $(CMD) 

#-------------
# make executable
#---------------

$(CMD): $(OBS)
	$(FC) $(FL_FLAGS) $@ $(OBS) $(FLIBS)
%.o:%.f
	$(FC) $(FC_FLAGS) $<	

#--------
# Clean up
#--------
	
clean:	
	$(RM) *.o 

# ---------------
#  install executable
# ---------------
#
install: $(CMD)
	$(CP) $(CMD) $(LOC)

