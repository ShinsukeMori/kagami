#=====================================================================================
#                       MMakefile
#                             bShShinsuke MORI
#                             Last change 1712 September 2009
#=====================================================================================

# Makefile for g++-include
# 
# 以下のことができます。
# % gmake all
# % gmake (target) ..
# % gmake print-all
# % gmake print-(target) ..
# key word "TARGET" で search することで target を知ることができます。


#-------------------------------------------------------------------------------------
#                        set variables
#-------------------------------------------------------------------------------------

NULL     =
PRINT    = a2lpr
README   = 
MAKEFILE = Makefile


#-------------------------------------------------------------------------------------
#                        Compiler Options
#-------------------------------------------------------------------------------------

CC        = g++
#WARNING   = -W
OPTIMIZE  = -O3
#DEBUG     = -g
MATH      = -lm
#PROF      = -p
#GPROF     = -gp
#INCDIR    = -I$(HOME)/SLM/include
INCDIR    = -I.
LIBDIR    = -L.
#MYLIB     = -lmylib
#WCHAR     = -lwchar
#LIBGDBM   = -lgdbm
#LIBDB     = -ldb

OPTION    = $(WARNING) $(OPTIMIZE) $(DEBUG) $(PROF) $(GPROF) $(INCDIR) $(LIBDIR)


#-------------------------------------------------------------------------------------
#                        Definition of all the files for targets
#-------------------------------------------------------------------------------------

#---------- (00) for general

GENERAL_HEADER = constant.h
GENERAL_OTHERS = 

#---------- (01) for server

TARGET01_SOURCE = server.cc
TARGET01_HEADER = TaskVTable.h Markov.h UkWord.h StopChar.h InMorp.h ExMorp.h KKConv.h Connection.h
TARGET01_OTHERS = 
TARGET01_OPTION = 

#---------- (02) for 

TARGET02_SOURCE = 
TARGET02_HEADER = 
TARGET02_OTHERS = 
TARGET02_OPTION = 

#---------- (03) for 

TARGET03_SOURCE = 
TARGET03_HEADER = 
TARGET03_OTHERS = 
TARGET03_OPTION = 

#---------- (04) for 

TARGET04_SOURCE = 
TARGET04_HEADER = 
TARGET04_OTHERS = 
TARGET04_OPTION = 

#---------- (05) for 

TARGET05_SOURCE = 
TARGET05_HEADER = 
TARGET05_OTHERS = 
TARGET05_OPTION = 

#---------- (06) for 

TARGET06_SOURCE = 
TARGET06_HEADER = 
TARGET06_OTHERS = 
TARGET06_OPTION = 

#---------- (07) for 

TARGET07_SOURCE = 
TARGET07_HEADER = 
TARGET07_OTHERS = 
TARGET07_OPTION = 

#---------- (08) for 

TARGET08_SOURCE = 
TARGET08_HEADER = 
TARGET08_OTHERS = 
TARGET08_OPTION = 

#---------- (09) for 

TARGET09_SOURCE = 
TARGET09_HEADER = 
TARGET09_OTHERS = 
TARGET09_OPTION = 

#---------- (10) for 

TARGET10_SOURCE = 
TARGET10_HEADER = 
TARGET10_OTHERS = 
TARGET10_OPTION = 

#---------- (11) for 

TARGET11_SOURCE = 
TARGET11_HEADER = 
TARGET11_OTHERS = 
TARGET11_OPTION = 

#---------- (12) for 

TARGET12_SOURCE = 
TARGET12_HEADER = 
TARGET12_OTHERS = 
TARGET12_OPTION = 

#---------- (13) for 

TARGET13_SOURCE = 
TARGET13_HEADER = 
TARGET13_OTHERS = 
TARGET13_OPTION = 

#---------- (14) for 

TARGET14_SOURCE = 
TARGET14_HEADER = 
TARGET14_OTHERS = 
TARGET14_OPTION = 

#---------- (last) for all to print

ALL_PROGRAMS   = $(README) $(MAKEFILE) $(GENERAL_HEADER) $(GENERAL_OTHERS)\
                 $(TARGET01_HEADER) $(TARGET01_SOURCE) $(TARGET01_OTHERS)\
                 $(TARGET02_HEADER) $(TARGET02_SOURCE) $(TARGET02_OTHERS)\
                 $(TARGET03_HEADER) $(TARGET03_SOURCE) $(TARGET03_OTHERS)\
                 $(TARGET04_HEADER) $(TARGET04_SOURCE) $(TARGET04_OTHERS)\
                 $(TARGET05_HEADER) $(TARGET05_SOURCE) $(TARGET05_OTHERS)\
                 $(TARGET06_HEADER) $(TARGET06_SOURCE) $(TARGET06_OTHERS)\
                 $(TARGET07_HEADER) $(TARGET07_SOURCE) $(TARGET07_OTHERS)\
                 $(TARGET08_HEADER) $(TARGET08_SOURCE) $(TARGET08_OTHERS)\
                 $(TARGET09_HEADER) $(TARGET09_SOURCE) $(TARGET09_OTHERS)\
                 $(TARGET10_HEADER) $(TARGET10_SOURCE) $(TARGET10_OTHERS)\
                 $(TARGET11_HEADER) $(TARGET11_SOURCE) $(TARGET11_OTHERS)\
                 $(TARGET12_HEADER) $(TARGET12_SOURCE) $(TARGET12_OTHERS)\
                 $(TARGET13_HEADER) $(TARGET13_SOURCE) $(TARGET13_OTHERS)\
                 $(TARGET14_HEADER) $(TARGET14_SOURCE) $(TARGET14_OTHERS)


#-------------------------------------------------------------------------------------
#                        Expansion to define TARGET NAME and OBJECT NAME
#-------------------------------------------------------------------------------------

#---------- (00) for general

GENERAL_OBJECT = $(GENERAL_OTHERS:.cc=.o)

#---------- (01) for target01

TARGET01_TARGET = $(TARGET01_SOURCE:.cc=$(NULL))
TARGET01_OBJECT = $(TARGET01_OTHERS:.cc=.o)

#---------- (02) for target02 

TARGET02_TARGET = $(TARGET02_SOURCE:.cc=$(NULL))
TARGET02_OBJECT = $(TARGET02_OTHERS:.cc=.o)

#---------- (03) for target03

TARGET03_TARGET = $(TARGET03_SOURCE:.cc=$(NULL))
TARGET03_OBJECT = $(TARGET03_OTHERS:.cc=.o)

#---------- (04) for target04 

TARGET04_TARGET = $(TARGET04_SOURCE:.cc=$(NULL))
TARGET04_OBJECT = $(TARGET04_OTHERS:.cc=.o)

#---------- (05) for target05

TARGET05_TARGET = $(TARGET05_SOURCE:.cc=$(NULL))
TARGET05_OBJECT = $(TARGET05_OTHERS:.cc=.o)

#---------- (06) for target06 

TARGET06_TARGET = $(TARGET06_SOURCE:.cc=$(NULL))
TARGET06_OBJECT = $(TARGET06_OTHERS:.cc=.o)

#---------- (07) for target07

TARGET07_TARGET = $(TARGET07_SOURCE:.cc=$(NULL))
TARGET07_OBJECT = $(TARGET07_OTHERS:.cc=.o)

#---------- (08) for target08 

TARGET08_TARGET = $(TARGET08_SOURCE:.cc=$(NULL))
TARGET08_OBJECT = $(TARGET08_OTHERS:.cc=.o)

#---------- (09) for target09 

TARGET09_TARGET = $(TARGET09_SOURCE:.cc=$(NULL))
TARGET09_OBJECT = $(TARGET09_OTHERS:.cc=.o)

#---------- (10) for target10 

TARGET10_TARGET = $(TARGET10_SOURCE:.cc=$(NULL))
TARGET10_OBJECT = $(TARGET10_OTHERS:.cc=.o)

#---------- (11) for target11 

TARGET11_TARGET = $(TARGET11_SOURCE:.cc=$(NULL))
TARGET11_OBJECT = $(TARGET11_OTHERS:.cc=.o)

#---------- (12) for target12 

TARGET12_TARGET = $(TARGET12_SOURCE:.cc=$(NULL))
TARGET12_OBJECT = $(TARGET12_OTHERS:.cc=.o)

#---------- (13) for target13 

TARGET13_TARGET = $(TARGET13_SOURCE:.cc=$(NULL))
TARGET13_OBJECT = $(TARGET13_OTHERS:.cc=.o)

#---------- (14) for target14 

TARGET14_TARGET = $(TARGET14_SOURCE:.cc=$(NULL))
TARGET14_OBJECT = $(TARGET14_OTHERS:.cc=.o)

#---------- (last) for all to compile

ALL_TARGETS    = $(TARGET01_TARGET) $(TARGET02_TARGET) $(TARGET03_TARGET)\
                 $(TARGET04_TARGET) $(TARGET05_TARGET) $(TARGET06_TARGET)\
                 $(TARGET07_TARGET) $(TARGET08_TARGET) $(TARGET09_TARGET)\
                 $(TARGET10_TARGET) $(TARGET11_TARGET) $(TARGET12_TARGET)\
                 $(TARGET13_TARGET) $(TARGET14_TARGET)


#-------------------------------------------------------------------------------------
#                        Compilation of the targets
#-------------------------------------------------------------------------------------

#---------- (00) compile general others

$(GENERAL_OBJECT) : $(GENERAL_HEADER)
$(GENERAL_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (01) compile target01

$(TARGET01_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET01_SOURCE) $(TARGET01_HEADER) $(TARGET01_OBJECT)
	$(CC) -o $@ $(TARGET01_SOURCE) $(GENERAL_OBJECT) $(TARGET01_OBJECT)\
              $(OPTION) $(TARGET01_OPTION)

$(TARGET01_OBJECT) : $(GENERAL_HEADER) $(TARGET01_HEADER)
$(TARGET01_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (02) compile target02

$(TARGET02_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET02_SOURCE) $(TARGET02_HEADER) $(TARGET02_OBJECT)
	$(CC) -o $@ $(TARGET02_SOURCE) $(GENERAL_OBJECT) $(TARGET02_OBJECT)\
              $(OPTION) $(TARGET02_OPTION)

$(TARGET02_OBJECT) : $(GENERAL_HEADER) $(TARGET02_HEADER)
$(TARGET02_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (03) compile target03

$(TARGET03_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET03_SOURCE) $(TARGET03_HEADER) $(TARGET03_OBJECT)
	$(CC) -o $@ $(TARGET03_SOURCE) $(GENERAL_OBJECT) $(TARGET03_OBJECT)\
              $(OPTION) $(TARGET03_OPTION)

$(TARGET03_OBJECT) : $(GENERAL_HEADER) $(TARGET03_HEADER)
$(TARGET03_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (04) compile target04

$(TARGET04_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET04_SOURCE) $(TARGET04_HEADER) $(TARGET04_OBJECT)
	$(CC) -o $@ $(TARGET04_SOURCE) $(GENERAL_OBJECT) $(TARGET04_OBJECT)\
             $(OPTION) $(TARGET04_OPTION)

$(TARGET04_OBJECT) : $(GENERAL_HEADER) $(TARGET04_HEADER)
$(TARGET04_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (05) compile target05

$(TARGET05_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET05_SOURCE) $(TARGET05_HEADER) $(TARGET05_OBJECT)
	$(CC) -o $@ $(TARGET05_SOURCE) $(GENERAL_OBJECT) $(TARGET05_OBJECT)\
              $(OPTION) $(TARGET05_OPTION)

$(TARGET05_OBJECT) : $(GENERAL_HEADER) $(TARGET05_HEADER)
$(TARGET05_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (06) compile target06

$(TARGET06_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET06_SOURCE) $(TARGET06_HEADER) $(TARGET06_OBJECT)
	$(CC) -o $@ $(TARGET06_SOURCE) $(GENERAL_OBJECT) $(TARGET06_OBJECT)\
              $(OPTION) $(TARGET06_OPTION)

$(TARGET06_OBJECT) : $(GENERAL_HEADER) $(TARGET06_HEADER)
$(TARGET06_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (07) compile target07

$(TARGET07_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET07_SOURCE) $(TARGET07_HEADER) $(TARGET07_OBJECT)
	$(CC) -o $@ $(TARGET07_SOURCE) $(GENERAL_OBJECT) $(TARGET07_OBJECT)\
              $(OPTION) $(TARGET07_OPTION)

$(TARGET07_OBJECT) : $(GENERAL_HEADER) $(TARGET07_HEADER)
$(TARGET07_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (08) compile target08

$(TARGET08_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET08_SOURCE) $(TARGET08_HEADER) $(TARGET08_OBJECT)
	$(CC) -o $@ $(TARGET08_SOURCE) $(GENERAL_OBJECT) $(TARGET08_OBJECT)\
              $(OPTION) $(TARGET08_OPTION)

$(TARGET08_OBJECT) : $(GENERAL_HEADER) $(TARGET08_HEADER)
$(TARGET08_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (09) compile target09

$(TARGET09_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET09_SOURCE) $(TARGET09_HEADER) $(TARGET09_OBJECT)
	$(CC) -o $@ $(TARGET09_SOURCE) $(GENERAL_OBJECT) $(TARGET09_OBJECT)\
              $(OPTION) $(TARGET09_OPTION)

$(TARGET09_OBJECT) : $(GENERAL_HEADER) $(TARGET09_HEADER)
$(TARGET09_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (10) compile target10

$(TARGET10_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET10_SOURCE) $(TARGET10_HEADER) $(TARGET10_OBJECT)
	$(CC) -o $@ $(TARGET10_SOURCE) $(GENERAL_OBJECT) $(TARGET10_OBJECT)\
              $(OPTION) $(TARGET10_OPTION)

$(TARGET10_OBJECT) : $(GENERAL_HEADER) $(TARGET10_HEADER)
$(TARGET10_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (11) compile target11

$(TARGET11_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET11_SOURCE) $(TARGET11_HEADER) $(TARGET11_OBJECT)
	$(CC) -o $@ $(TARGET11_SOURCE) $(GENERAL_OBJECT) $(TARGET11_OBJECT)\
              $(OPTION) $(TARGET11_OPTION)

$(TARGET11_OBJECT) : $(GENERAL_HEADER) $(TARGET11_HEADER)
$(TARGET11_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (12) compile target12

$(TARGET12_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET12_SOURCE) $(TARGET12_HEADER) $(TARGET12_OBJECT)
	$(CC) -o $@ $(TARGET12_SOURCE) $(GENERAL_OBJECT) $(TARGET12_OBJECT)\
              $(OPTION) $(TARGET12_OPTION)

$(TARGET12_OBJECT) : $(GENERAL_HEADER) $(TARGET12_HEADER)
$(TARGET12_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (13) compile target13

$(TARGET13_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET13_SOURCE) $(TARGET13_HEADER) $(TARGET13_OBJECT)
	$(CC) -o $@ $(TARGET13_SOURCE) $(GENERAL_OBJECT) $(TARGET13_OBJECT)\
              $(OPTION) $(TARGET13_OPTION)

$(TARGET13_OBJECT) : $(GENERAL_HEADER) $(TARGET13_HEADER)
$(TARGET13_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (14) compile target14

$(TARGET14_TARGET) : $(GENERAL_HEADER) $(GENERAL_OBJECT) $(MAKEFILE)\
                     $(TARGET14_SOURCE) $(TARGET14_HEADER) $(TARGET14_OBJECT)
	$(CC) -o $@ $(TARGET14_SOURCE) $(GENERAL_OBJECT) $(TARGET14_OBJECT)\
              $(OPTION) $(TARGET14_OPTION)

$(TARGET14_OBJECT) : $(GENERAL_HEADER) $(TARGET14_HEADER)
$(TARGET14_OBJECT) : %.o : %.cc
	$(CC) -c $< $(OPTION)

#---------- (last) compile all

all : $(ALL_TARGETS)


#-------------------------------------------------------------------------------------
#                        Printing the programs
#-------------------------------------------------------------------------------------

#---------- (00) print general

print-general :
	$(PRINT) $(MAKEFILE) $(GENERAL_HEADER) $(GENERAL_OTHERS)

#---------- (01) print target01

print-$(TARGET01_TARGET) :
	$(PRINT) $(TARGET01_HEADER) $(TARGET01_SOURCE) $(TARGET01_OTHERS)

#---------- (02) print target02

#print-$(TARGET02_TARGET) :
#	$(PRINT) $(TARGET02_HEADER) $(TARGET02_SOURCE) $(TARGET02_OTHERS)

#---------- (03) print target03

#print-$(TARGET03_TARGET) :
#	$(PRINT3) $(TARGET03_HEADER) $(TARGET03_SOURCE) $(TARGET03_OTHERS)

#---------- (04) print target04

#print-$(TARGET04_TARGET) :
#	$(PRINT) $(TARGET04_HEADER) $(TARGET04_SOURCE) $(TARGET04_OTHERS)

#---------- (05) print target05

#print-$(TARGET05_TARGET) :
#	$(PRINT) $(TARGET05_HEADER) $(TARGET05_SOURCE) $(TARGET05_OTHERS)

#---------- (06) print target06

#print-$(TARGET06_TARGET) :
#	$(PRINT) $(TARGET06_HEADER) $(TARGET06_SOURCE) $(TARGET06_OTHERS)

#---------- (07) print target07

#print-$(TARGET07_TARGET) :
#	$(PRINT) $(TARGET07_HEADER) $(TARGET07_SOURCE) $(TARGET07_OTHERS)

#---------- (08) print target08

#print-$(TARGET08_TARGET) :
#	$(PRINT) $(TARGET08_HEADER) $(TARGET08_SOURCE) $(TARGET08_OTHERS)

#---------- (09) print target09

#print-$(TARGET09_TARGET) :
#	$(PRINT) $(TARGET09_HEADER) $(TARGET09_SOURCE) $(TARGET09_OTHERS)

#---------- (10) print target10

#print-$(TARGET10_TARGET) :
#	$(PRINT) $(TARGET10_HEADER) $(TARGET10_SOURCE) $(TARGET10_OTHERS)

#---------- (11) print target11

#print-$(TARGET11_TARGET) :
#	$(PRINT) $(TARGET11_HEADER) $(TARGET11_SOURCE) $(TARGET11_OTHERS)

#---------- (12) print target12

#print-$(TARGET12_TARGET) :
#	$(PRINT) $(TARGET12_HEADER) $(TARGET12_SOURCE) $(TARGET12_OTHERS)

#---------- (13) print target13

#print-$(TARGET13_TARGET) :
#	$(PRINT) $(TARGET13_HEADER) $(TARGET13_SOURCE) $(TARGET13_OTHERS)

#---------- (14) print target14

#print-$(TARGET14_TARGET) :
#	$(PRINT) $(TARGET14_HEADER) $(TARGET14_SOURCE) $(TARGET14_OTHERS)

#---------- (last) print all

print-all :
	$(PRINT) $(ALL_PROGRAMS)


#-------------------------------------------------------------------------------------
#                        Cleaning the directory
#-------------------------------------------------------------------------------------

clean :
	-rm *.o *~

clean-completely :
	-rm *.o *~ $(ALL_TARGETS)


#=====================================================================================
#                        END
#=====================================================================================
