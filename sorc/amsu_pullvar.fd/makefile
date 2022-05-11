SHELL=/bin/sh
#
# This makefile was produced by /usr/bin/fmgen at 11:21:07 AM on 10/28/94
# If it is invoked by the command line
#	make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named a.out.
#
# If it is invoked by the command line
#	make -f makefile a.out.prof
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable which profiles
# named a.out.prof.
#
# To remove all the objects but leave the executables use the command line
#	make -f makefile clean
#
# To remove everything but the source files use the command line
#	make -f makefile clobber
#
# To remove the source files created by /usr/bin/fmgen and this makefile
# use the command line
#	make -f makefile void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.
#

SRCS=   pullvar_nhc.f  
OBJS=	pullvar_nhc.o
 

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#
#FC =		ncepxlf
FC = ftn
LDFLAGS =       

LIBS =  

CMD = amsu_pullvar

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
#FFLAGS =       -bshared 
#FFLAGS = -g -align all
FFLAGS = -O3 -fp-model precise -align all
CFLAGS=	

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#

$(CMD):	$(SRCS) $(FINCS)
	$(FC) $(FFLAGS) -o $(@) $(SRCS) $(FINCS) $(LIBS)
	rm -f $(OBJS)

# The following rule reads the required NAWIPS definitions and then recursively
# runs this same makefile with a new target in the spawned shell.
#
clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD)

void:	clobber
	-rm -f $(SRCS) makefile

.SUFFIXES: .f .o
.f.o:
	$(FC) $(FFLAGS) $< $(FINCS) $(LIBS)

install: $(CMD)
	cp -f $(CMD) ../../exec 
