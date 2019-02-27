SHELL=/bin/ksh
#
# This is the annprdct executable
#
SRCS=	gblsice.f rdgrbsst.f acre.f find.f findnh.f grbice.f intlzi.f writei.f
OBJS=	gblsice.o rdgrbsst.o acre.o find.o findnh.o grbice.o intlzi.o writei.o
CMD =	omb_gblsice


#LINUX
FC =		ifort
#FFLAGS =	-O3 $(INC) -std90 
FFLAGS =	-O3 $(INC)
LDFLAGS =	-O3 
LIBS = $(W3NCO_LIB4) $(W3EMC_LIB4) $(BACIO_LIB4)

############################################################
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(CMD) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)
