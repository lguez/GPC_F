gpc_2_33.o: gpc_2_33.c gpc.h
gpc_polygon_clip_f.o : gpc_f.o 
shpobj2pol.o : gpc_polygon_clip_f.o 
test_GPC.o : shpobj2pol.o gpc_polygon_clip_f.o gpc_f.o 
