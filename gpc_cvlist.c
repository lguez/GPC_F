#include "gpc.h"

#define	G_MALLOC(p,type,np,str)  { if ((np) > 0) { \
		(p)=(type*)malloc((np)*sizeof(type)); \
		if (p==((type*)NULL)) { \
		    fprintf(stderr, "WINGRIDDS malloc failure: %s\n", (str) ); \
		  } \
		} \
		else \
		    (p)=(type*)NULL; \
		}

void gpc_cvlist ( int npoly, float *px, float *py,
			gpc_vertex_list *contour, int *iret )
/************************************************************************
 * gpc_cvlist                                                           *
 *                                                                      *
 * This function generates a GPC vertex list structure given a set of	*
 * polygon points.							*
 *                                                                      *
 * gpc_cvlist ( npoly, px, py, contour, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *npoly  	int             Number of points in polygon	*
 *      *px     	float[]         X array				*
 *      *py     	float[]         Y array				*
 *                                                                      *
 * Output parameters:                                                   *
 *      *contour  	gpc_vertex_list Vertex list structure		*
 *      *iret   	int     	Return code                     *
 *                                                                      *
 **                                                                     *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/

    *iret = 0;

    contour->num_vertices = npoly;

    G_MALLOC ( contour->vertex, gpc_vertex, npoly, "gpc_vertex creation" );

    for ( ii = 0; ii < npoly; ii++ )  {
	contour->vertex[ii].x = px[ii];
	contour->vertex[ii].y = py[ii];
    }

    return;

}
