#include "gpc.h"

void gpc_gvlist ( gpc_vertex_list *contour, 
		int *npoly, float *px, float *py, int *iret )
/************************************************************************
 * gpc_gvlist                                               		*
 *                                                                      *
 * This function takes a GPC vertex list structure and returns a set of	*
 * polygon points.							*
 *                                                                      *
 * gpc_gvlist ( contour, npoly, px, py, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *contour  	gpc_vertex_list Vertex list structure		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *npoly  	int             Number of points in polygon	*
 *      *px     	float[]         X array				*
 *      *py     	float[]         Y array				*
 *      *iret   	int     		Return code                     *
 *                                                                      *
 **                                                                     *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/

    *iret = 0;

    *npoly = contour->num_vertices;

    for ( ii = 0; ii < *npoly; ii++ )  {
	px[ii] = contour->vertex[ii].x;
	py[ii] = contour->vertex[ii].y;
    }

    return;

}
