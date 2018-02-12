/* fopen example */
#include <stdio.h>

void C_fopen (FILE **pFile, char *newname, char *mode, int *ier)
{

/*---------------------------------------------------------------------*/

  *ier = 0;

  printf ("C_fopen: fopen mode = %s \n",mode);

  *pFile = fopen (newname,mode);

  printf ("C_fopen: pFile pointer = %p \n",*pFile);

  if (pFile==NULL)
  {
    printf ("C_fopen: fopen error = %p \n",*pFile);
	*ier = 9999;
  }

  else if (*pFile!=NULL)
  {
    printf ("C_fopen: pFile good = %p \n",*pFile);
  }

}