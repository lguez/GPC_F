# This is a makefile for GNU make.

# 1. Executable file and libraries

execut= test_GPC
lib_list = shapelib_03 fortrangis fortranc shp contour_531 jumble nr_util GPC_F GPC

# 2. Compiler-dependent part

mode = debug
include ${general_compiler_options_dir}/${FC}_${mode}.mk

# 3. Rules

.PHONY: all clean
all: ${execut} log
include ${general_compiler_options_dir}/settings.mk

clean:
	-rm ${execut} log
