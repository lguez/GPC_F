# This is a makefile for GNU make.

# 1. Source files

VPATH = .
sources = gpc_polygon_clip_f.f shpobj2pol.f gpc_f.f

# 2. Objects and libraries

objects := $(sources:.f=.o)
lib_dyn = libGPC_F.so
lib_stat = libGPC_F.a

# 3. Compiler-dependent part

mode = debug
include ${general_compiler_options_dir}/${FC}_${mode}.mk

# 4. Rules

SHELL = bash
.DELETE_ON_ERROR:
.PHONY: all clean clobber depend
all: ${lib_stat} log

${lib_dyn}: ${objects}
	$(FC) $(LDFLAGS) ${ldflags_lib_dyn} $^ -o $@

${lib_stat}: ${lib_stat}(${objects})

depend ${VPATH}/depend.mk:
	makedepf90 $(addprefix -D, ${cpp_macros}) -free -Wmissing -Wconfused -I${VPATH} -nosrc $(addprefix -u , intrinsic shapelib contour_531) ${sources} >${VPATH}/depend.mk


clean:
	rm -f ${lib_dyn} ${lib_stat} ${objects} log

clobber: clean
	rm -f *.mod ${VPATH}/depend.mk

log:
	hostname >$@
	${FC} ${version_flag} >>$@ 2>&1
	echo -e "\nFC = ${FC}\n\nCPPFLAGS = ${CPPFLAGS}\n\nFFLAGS = ${FFLAGS}\n\nLDFLAGS = ${LDFLAGS}\n\nldflags_lib_dyn = ${ldflags_lib_dyn}" >>$@

ifneq ($(MAKECMDGOALS), clobber)
include ${VPATH}/depend.mk
endif
