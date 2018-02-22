# This is a makefile for GNU make.

# 1. Source files and libraries

VPATH = .
Fortran_sources := $(sort testgpc.f gpc_f.f)
C_sources = gpc_2_33.c
lib_list = shapelib_03 fortrangis fortranc shp jumble

# 2. Objects and executable file

objects := $(Fortran_sources:.f=.o) $(C_sources:.c=.o)
execut = testgpc

# 3. Compiler-dependent part

mode = debug
include ${general_compiler_options_dir}/${FC}_${mode}.mk
FFLAGS += -fno-underscoring

CFLAGS = -std=c99 -pedantic -g3 -W -Wall -Wdiv-by-zero -Wmultichar -Wundef -Wshadow -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wcast-align -Wwrite-strings -Waggregate-return -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wmissing-format-attribute -Wpacked -Wredundant-decls -Wnested-externs -Wunreachable-code -Winline -Wlong-long -Wdisabled-optimization -fexceptions

# 4. Rules

SHELL = bash
LINK.o = $(FC) $(LDFLAGS) $(TARGET_ARCH)
.DELETE_ON_ERROR:
.PHONY: all clean clobber depend
all: ${execut} log
${execut}: ${objects}

depend ${VPATH}/depend.mk:
	${CC} -MM ${C_sources} >${VPATH}/depend.mk
	makedepf90 $(addprefix -D, ${cpp_macros}) -free -Wmissing -Wconfused -I${VPATH} -nosrc $(addprefix -u , intrinsic shapelib ${lib_list}) ${Fortran_sources} >>${VPATH}/depend.mk


clean:
	rm -f ${execut} ${objects} log

clobber: clean
	rm -f *.mod ${VPATH}/depend.mk

log:
	hostname >$@
	${FC} ${version_flag} >>$@ 2>&1
	echo -e "\nFC = ${FC}\n\nCPPFLAGS = ${CPPFLAGS}\n\nFFLAGS = ${FFLAGS}\n\nLDLIBS = ${LDLIBS}\n\nLDFLAGS = ${LDFLAGS}" >>$@

ifneq ($(MAKECMDGOALS), clobber)
include ${VPATH}/depend.mk
endif
