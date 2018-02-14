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

#define	G_FREE(p,type)      {if (p!=((type*)NULL))  \
		{free(p); (p)= (type*)NULL;}}

//float gpc_gvarea ( gpc_vertex_list *contour )
void gpc_gvarea ( gpc_vertex_list *contour, float *area )
/************************************************************************
 * gpc_gvarea                                               		*
 *                                                                      *
 * This function computes the area bounded by a GPC vertex list.	*
 * Note the area may be positive (points counter clockwise) or negative	*
 * (clockwise).								*
 *                                                                      *
 * gpc_gvarea ( contour )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      *contour  	gpc_vertex_list Vertex list structure		*
 *                                                                      *
 * Output parameters:                                                   *
 *      none								*
 *                                                                      *
 * Returned parameters:                                                 *
 *      gpc_gvarea	float		Area				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     04/04   Created                                 *
 ***********************************************************************/
{
int	ii, nv, nbytes, ier;
//float	*x, *y, xc, yc, area;
float	*x, *y, xc, yc;

/*---------------------------------------------------------------------*/

    nv = contour->num_vertices;

    nbytes = nv * sizeof(float);
    G_MALLOC ( x, float, nbytes, "x" );
    G_MALLOC ( y, float, nbytes, "y" );

    for ( ii = 0; ii < nv; ii++ )  {
	x[ii] = (float)contour->vertex[ii].x;
	y[ii] = (float)contour->vertex[ii].y;
    }

    cgr_centroid ( x, y, &nv, &xc, &yc, &area, &ier );

    G_FREE ( y, float );
    G_FREE ( x, float );

//    return ( area );
	return;

}
