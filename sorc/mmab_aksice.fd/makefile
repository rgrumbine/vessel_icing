SHELL=/bin/ksh
#
# This is the annprdct executable
#    compied for silver and winterhawk nodes
#
SRCS=	aksice.f
OBJS=	aksice.o
CMD =	omb_aksice
#
#Linux
#INC = -I /nwprod/lib/sorc/g2  -I /nwprod/lib/incmod/g2_4/
FC =		ifort
FFLAGS =	-O3 $(INC) 
LDFLAGS =	-O3
LIBS = $(W3NCO_LIB4) $(W3EMC_LIB4) $(BACIO_LIB4)
#LIBS = $(JASPER_LIB) $(W3NCO_LIB4) $(W3EMC_LIB4) $(BACIO_LIB4)
#LIBS = -L/nwprod/lib -lg2_4 -ljasper -lpng -lz -lw3nco_4 -lw3emc_4 -lbacio_4

################################################################
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(CMD) $(INC) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)
