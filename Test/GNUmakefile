# This is a makefile for GNU make.

# 1. Executable file and libraries

execut= test_GPC
lib_list = shapelib_03 fortrangis fortranc shp contour_531 jumble nr_util

# 2. Compiler-dependent part

mode = debug
include ${general_compiler_options_dir}/${FC}_${mode}.mk

# 3. Rules

SHELL = bash
LINK.o = $(FC) $(LDFLAGS) $(TARGET_ARCH)
.DELETE_ON_ERROR:
.PHONY: all clean
all: ${execut} log

clean:
	-rm ${execut} log

log:
	hostname >$@
	${FC} ${version_flag} >>$@ 2>&1
	echo -e "\nFC = ${FC}\n\nCPPFLAGS = ${CPPFLAGS}\n\nFFLAGS = ${FFLAGS}\n\nLDLIBS = ${LDLIBS}\n\nLDFLAGS = ${LDFLAGS}" >>$@
