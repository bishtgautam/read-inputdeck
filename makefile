SRC_DIR = ./

# make option to search for source files in $SRC_DIR
VPATH = $(SRC_DIR)

# Import variables/options/rules from PETSc.
include ${PETSC_DIR}/lib/petsc/conf/variables
include ${PETSC_DIR}/lib/petsc/conf/rules
# reseting PETSC_MAKE_STOP_ON_ERROR causes build to stop on error.
PETSC_MAKE_STOP_ON_ERROR=

MYFLAGS = -I. 

ifdef codecov
  MYFLAGS += -fprofile-arcs -ftest-coverage
  LIBS += -lgcov
endif

###############################################################################
# Assign additional compiler/preprocessor flags
###############################################################################

# These flags are supplemental to the PETSc flags
CFLAGS   =
FFLAGS   =
CPPFLAGS = ${MYFLAGS}
FPPFLAGS = ${MYFLAGS}

CLEANFILES = readinputdeck

input_aux.o : option.o pflotran_constants.o string.o units.o
logging.o : pflotran_constants.o
option.o : logging.o option_flow.o option_transport.o pflotran_constants.o
option_flow.o : pflotran_constants.o
option_transport.o : pflotran_constants.o
pflotran_constants.o :
string.o : pflotran_constants.o
units.o : option.o pflotran_constants.o
readinputdeck.o : input_aux.o


# Concatentate dependency groups 
pflotran_obj = \
	input_aux.o \
	logging.o \
	option.o \
	option_flow.o \
	option_transport.o \
	pflotran_constants.o \
	string.o \
	units.o \
	read_inputdeck.o

readinputdeck : $(pflotran_obj)
	${FLINKER} -o readinputdeck $(pflotran_obj) ${PETSC_LIB} ${LIBS} 

clean-pflotran :
	-rm -f $(CLEANFILES) *.o *.mod *.a *.gcov *.gcda *.gcno;

FORCE :
