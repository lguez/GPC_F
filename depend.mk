C_fopen.o: C_fopen.c
gpc_2_33.o: gpc_2_33.c gpc.h
gpc_free_vertex.o: gpc_free_vertex.c gpc.h
gpc_cvlist.o: gpc_cvlist.c gpc.h
gpc_gvlist.o: gpc_gvlist.c gpc.h
gpc_gvarea.o: gpc_gvarea.c gpc.h
cgr_centroid.o: cgr_centroid.c
cfl_clos.o : msvcrt.o 
testgpc.o : module_gpc.o cfl_clos.o 
