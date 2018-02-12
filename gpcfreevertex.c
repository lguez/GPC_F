#include <stdlib.h>
#include <float.h>
#include <math.h>
#include "gpc.h"

#define FREE(p)            {if (p) {free(p); (p)= NULL;}}

void gpc_free_vertex(gpc_vertex_list *contour)
/************************************************************************
 * gpc_free_vertex                                               		*
 *                                                                      *
 * This function takes a GPC vertex list structure and frees the memory *
 *                                                                      *
 * gpc_free_vertex ( contour )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *contour  	gpc_vertex_list Vertex list structure		*
 *                                                                      *
 **                                                                     *
 ***********************************************************************/

{

  FREE(contour->vertex);
  contour->num_vertices= 0;
}