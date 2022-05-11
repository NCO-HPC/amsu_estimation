#==============================================================================
#
# Makefile for: Compiles the code that converts
#		AMSU bufr data into ascii data.
#
#		Compiles object code and executables that are 
#		involved with getting a list of current active storms
#		from the ATCF.  
#
# This version is for the IBM 
#
# Notes:   make                 compilies the one executables (temp_ret.exe)
#   	   make clean           removes the object files and executables
#   	   make install         places the *.exe files in the ../bin directory
#
# Dependacies:   
#
# ** requires BURF library "libbufr.a" 
#
#
# Executable name: rcarq1.exe 
#		   read_COORDINATES.exe 
#		   readbufr.exe
#
# Modification History: Written by: A. Krautkramer
#			Modified: ARK to run on     IBM 02/10/2005
#                       Modified: MAS to run on Phase 3 03/22/2019
#                       Modified: MAS to run on  WCOSS2 07/22/2021
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define default macros --
#-----------------------------------------------------------------------------

# --------------------
# Define default shell
# --------------------

SHELL = /bin/sh

CP	= cp -f

RM	= rm -f

LOC     = ../../exec/

#-----------------------------------------------------------------------------
#                       -- Define file lists --
#-----------------------------------------------------------------------------

# ---------
# Libraries 
# ---------

#LIBS = -L/nwprod/lib -lbufr_4_64
LIBS = $(BUFR_LIB4)

# --------
# Includes
# --------

INCS =

# ------------
# Source/Object/Executable files
# ------------

SRC_FILES = rcarq1.f 
OBJ_FILES = $(SRC_FILES:.f=.o)

CMD = amsu_rcarq1

#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------

# ------------------
# Compiler / Linker
# ------------------

#... HP F90 Compiler
#FC=f90 
#FL=f90

#... Linux PG F90 Compiler
#FC=pgf90
#FL=pgf90

#... IBM
#FC = ncepxlf
#FL = ncepxlf

#... WCOSS2
FC = ftn
FL = ftn 

# -----------------------------
# Compiler Flags / Linker Flags
# -----------------------------

#... Compiler Options ... 
#DEBUG_FLAGS=-g -align all
#COMMON_FLAGS=$(DEBUG_FLAGS) -DA2.0W  		#.... HP 
#COMMON_FLAGS=$(DEBUG_FLAGS) -DA2.0 -g77libs	#.... Linux
#...WCOSS2:
COMMON_FLAGS=-O3 -fp-model precise -align all

# ------------ 
# Common Flags 
# ------------

FC_FLAGS=$(COMMON_FLAGS) -c 
FL_FLAGS=$(COMMON_FLAGS) -o

# -------------
# Make program
# -------------

all: $(CMD) 

$(CMD):	rcarq1.o
	$(FL) $(FL_FLAGS) $@ rcarq1.o

%.o:%.f
	$(FC) $(FC_FLAGS) $<

# --------
# Clean up
# --------

clean:
	$(RM) $(OBJ_FILES)

# ---------------
#  install executable
# ---------------
#
install: $(CMD)
	$(CP) $(CMD) $(LOC)

