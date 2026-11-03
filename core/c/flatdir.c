/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2000-2026 European Union (Joint Research Centre)

This file is part of miallib.

miallib is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

miallib is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with miallib.  If not, see <https://www.gnu.org/licenses/>.
***********************************************************************/

/** @file
 *  Flow directions on plateaus from \cite soille-gratin94
 *  @author Pierre Soille
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>


#include "miallib.h"
#include "fah.h"
#include "fifo.h"
#include "pqueue.h"


/** \addtogroup group_dem
 *  @{
 */


#include "us_def.h"

#define FLAT_TYPE USHORT
#define PR_MAX 255
#define FLAT_MAX   65535
#define FLAT_MAXM1 65534
#define FLAT_VAL   65533
#define FLAT_DB    65532
#define REF_PIX_MSB  0x8000  /* 32768 */

ERROR_TYPE us_FlatIGeodAFAB(IMAGE *flat, IMAGE *im, int graph)
{
  long int i, j, k, t, nx, ny, nz, shft[27];
  FIFO4 *q, *qflat, *qdst, *qdb, *qdball;
  FIFO **fifo, **fifot, *pf;

  FLAT_TYPE *p, *ptr, *pflat, *pflat0, dcrt, *pm, *pr, *p_k, pr_max;
  PIX_TYPE *pim, *pim0, flatval;


  if (szgeocompat(im, flat) != NO_ERROR){
    (void) sprintf(buf, "FlatIGeodAFAB(): input images must be of same type\n"); errputstr(buf);
    return ERROR;
  }

  pim=(PIX_TYPE *)GetImPtr(im);
  pim0=(PIX_TYPE *)GetImPtr(im);
  pflat=(FLAT_TYPE *)GetImPtr(flat);
  pflat0=(FLAT_TYPE *)GetImPtr(flat); /* used for computing offsets */

  nx =GetImNx(im);
  ny =GetImNy(im);
  nz =GetImNz(im);

  /* set shift array */
  set_seq_shift(nx, ny, nz, graph, shft);

  q = create_fifo4(nx+ny+nz);  /* use for geodesic distance computations */
  if (q == NULL)
    return ERROR;

  qflat = create_fifo4(nx+ny+nz); /* will hold all pixels of flat region */
  if (qflat == NULL)
    return ERROR;

  qdst = create_fifo4(50); /* used for computing distance transform */
  if (qdst == NULL)
    return ERROR;

  qdb = create_fifo4(10); /* will hold descending borders of flat region (one at a time) */
  if (qdb == NULL)
    return ERROR;

  qdball = create_fifo4(nx+ny); /* will hold descending borders of all flat regions  */
  if (qdball == NULL){
    free_fifo4(q);
    free_fifo4(qflat);
    free_fifo4(qdst);
    free_fifo4(qdb);
    return ERROR;
  }

  for (j=nx*ny*nz; j > 0; j--, pflat++, pim++){
    if (*pflat == FLAT_VAL){ /* unprocessed flat region */
      *pflat=FLAT_MAX;
      flatval=*pim;
      fifo4_add(q, (long int)(pflat));
      fifo4_add(qflat, (long int)(pflat));

      while (fifo4_empty(q) == 0){ /* fill in flat region and initialize queues */
	ptr = (FLAT_TYPE *)fifo4_remove(q);
        for (k = 0; k < graph; k++){
	  p = ptr + shft[k];
	  if (*p == FLAT_VAL){
            *p=FLAT_MAX;
	    fifo4_add(q, (long int)p);
	    fifo4_add(qflat, (long int)p);
	  }
	  else if (*(pim0+(p-pflat0))>flatval){ /* external ascending border */
	    *p=FLAT_MAXM1;
	    fifo4_add(qdst, (long int)p);
	  }
	  else if ((*(pim0+(p-pflat0))==flatval)  && (*p==0)){ /* internal descending border */
	    *p=FLAT_DB;
	    fifo4_add(qdb, (long int)p);
	    fifo4_add(qdball, (long int)p);
	  }
	}
      }

      /* compute geodesic distances */
      dcrt = 0;
      while (fifo4_empty(qdst) == 0){
	fifo4_add(qdst, 1L);
	while ((pm = (FLAT_TYPE *)fifo4_remove(qdst)) != (FLAT_TYPE *)1L){
	  *pm = dcrt;
	  for (k=0; k < graph; ++k){
	    if (*(pm + shft[k]) == FLAT_MAX){
	      *(pm + shft[k]) = FLAT_MAXM1;
	      fifo4_add(qdst, (long int)(pm + shft[k]));
	    }
	  }
	}
	dcrt++;
#ifdef DEBUG
	if (dcrt >= FLAT_DB){
	  sprintf(buf, "emergency in us_FlatDir(): dcrt=%ld (>= FLAT_DB)\n", (long int)dcrt);
	  stdputstr(buf);
	}
#endif
      }

      dcrt++;
      if (dcrt==1) /* we want to start at 2 */
	dcrt=2;

      /* invert geodesic distances in flat region */
      if (dcrt==2){ /* flat top */
	while (fifo4_empty(qflat) == 0){
	  pm = (FLAT_TYPE *)fifo4_remove(qflat);
	  *pm = 2;
	}
      }
      else{ /* intermediate plateau */
	while (fifo4_empty(qflat) == 0){
	  pm = (FLAT_TYPE *)fifo4_remove(qflat);
	  *pm = dcrt-*pm;
	}
      }

      /* compute geodesic time function from descending border */

      /* Create an array of FIFO	*/
      if ((fifo = (FIFO **)calloc(PR_MAX + 1, sizeof(FIFO *))) == NULL){
        (void) sprintf(buf, "us_FlatIGeodAFAB(): not enough memory for the FAH\n");
	errputstr(buf);
	free_fifo4(q);
	free_fifo4(qflat);
	free_fifo4(qdst);
	free_fifo4(qdb);
	free_fifo4(qdball);
        return ERROR;
      }

      pr_max = PR_MAX;

      /* initialize the FAH */
      while (fifo4_empty(qdb) == 0){
	pr = (FLAT_TYPE *)fifo4_remove(qdb);
	*pr=0;
	for (k = 0; k < graph; ++k){
	  p_k=pr + shft[k];
	  if (*p_k && (*p_k<=dcrt)){

	    if (*p_k > pr_max){
	      if ((fifot = (FIFO **)calloc(*p_k + 1, sizeof(FIFO *))) == NULL){
		(void) sprintf(buf, "us_FlatIGeodAFAB(): not enough memory for the FAH\n");
		errputstr(buf);
		free((char *)fifo);
		free_fifo4(q);
		free_fifo4(qflat);
		free_fifo4(qdst);
		free_fifo4(qdb);
		free_fifo4(qdball);
		return -9;
	      }
	      for (i = 0; i <= pr_max; ++i)
		fifot[i] = fifo[i];
	      free((char *)fifo);
	      fifo=fifot;
	      fifot=NULL;
	      fifo[*p_k] = alloc_fifo(10L);
	      pr_max = *p_k;
	    }
	    else if (fifo[*p_k] == NULL){
	      fifo[*p_k] = alloc_fifo(10L);
	    }

	    fifo_add(fifo[*p_k], (long int)p_k);
	    *p_k |= REF_PIX_MSB;
	  }
	}
      }

      /* Ordered propagation of geodesic time function */
      for (t = 0; t <= pr_max; t++){
	pf = fifo[t];
	if (pf != NULL){
	  while (fifo_empty(pf) == FALSE){
	    pm = (FLAT_TYPE *)fifo_remove(pf);
	    for (k = 0; k < graph; ++k){
	      p_k = pm + shft[k];
	      if (*p_k & REF_PIX_MSB || (*p_k==0))
		continue;
	      *p_k += t;

	      if (*p_k > pr_max){
		if ((fifot = (FIFO **)calloc(*p_k + 1, sizeof(FIFO *))) == NULL){
		  (void) sprintf(buf, "us_FlatIGeodAFAB(): not enough memory for the FAH\n");
		  errputstr(buf);
		  free((char *)fifo);
		  free_fifo4(q);
		  free_fifo4(qflat);
		  free_fifo4(qdst);
		  free_fifo4(qdb);
		  free_fifo4(qdball);
		  return ERROR;
		}
		for (i = 0; i <= pr_max; ++i)
		  fifot[i] = fifo[i];
		free((char *)fifo);
		fifo=fifot;
		fifot=NULL;
		fifo[*p_k] = alloc_fifo(10L);
		pr_max = *p_k;
	      }
	      else if (fifo[*p_k] == NULL){
		fifo[*p_k] = alloc_fifo(10L);
	      }

	      fifo_add(fifo[*p_k], (long int)p_k);
	      *p_k |= REF_PIX_MSB;
	    }
	  }
	  clear_fifo(pf);
	}
      }
      free((char *)fifo);
    }
  }

  for (j=nx*ny*nz; j > 0; j--, pflat0++)
    if (*pflat0)
      *pflat0 ^= REF_PIX_MSB;

  while (fifo4_empty(qdball) == 0){ /* set to 1 all descending boders */
    pm = (FLAT_TYPE *)fifo4_remove(qdball);
    *pm = 1;
  }

  free_fifo4(q);
  free_fifo4(qflat);
  free_fifo4(qdst);
  free_fifo4(qdb);
  free_fifo4(qdball);
  return NO_ERROR;
}
#undef FLAT_TYPE
#undef PR_MAX
#undef FLAT_VAL
#undef FLAT_MAX
#undef FLAT_MAXM1
#undef REF_PIX_MSB
#undef FLAT_DB
#include "us_undef.h"


ERROR_TYPE FlatIGeodAFAB(IMAGE *flat, IMAGE *im, int graph)
{
  /* Compute geodesic distance away from ascending borders on all flat
     regions, then the geodesic time funtion from descending borders.
     The flat image must be of type USHORT with pixels without
     drainage set to 65533 (FLAT_VAL) First: 06-11-2001
  */

  if (GetImDataType(flat) != t_USHORT){
    (void) sprintf(buf, "FlatIGeodAFAB(): the image of flat areas must be of type USHORT (with flat regions set to 65533\n");
    errputstr(buf);
    return ERROR;
  }
  switch (GetImDataType(im)){

  case t_USHORT:
    us_FlatIGeodAFAB(flat, im, graph);
    break;

  default:
    (void)sprintf(buf, "ERROR in FlatIGeodAFAB(): invalid im ImDataType: must be USHORT\n");
    errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}



#include "us_def.h"

#define FLAT_TYPE USHORT
#define PR_MAX 255
#define FLAT_MAX   65535
#define FLAT_MAXM1 65534
#define FLAT_VAL   65533
#define FLAT_DB    65532
#define REF_PIX_MSB  0x8000  /* 32768 */

IMAGE *us_FlatDir(IMAGE *flat, IMAGE *im, int graph)
{
  long int i, j, k, t, nx, ny, nz, shft[27];
  FIFO4 *q, *qflat, *qdst, *qdb, *qdball, *qab;
  FIFO **fifo, **fifot, *pf;
  IMAGE *imdir;

  FLAT_TYPE *p, *ptr, *pflat, *pflat0, dcrt, *pm, *pr, *p_k, pr_max=0, vngb, lowest;
  PIX_TYPE *pim, *pim0, flatval;
  UCHAR *pdir0, dir;

  unsigned long int stmp, delta, slope; /* slope holds twice the square of actual slope */
  long int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;

  if (szgeocompat(im, flat) != NO_ERROR){
    (void) sprintf(buf, "us_FlatDir(): input images must be of same type\n");
    errputstr(buf);
    return NULL;
  }

  nx =GetImNx(im);
  ny =GetImNy(im);
  nz =GetImNz(im);

  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  imdir = (IMAGE *)create_image(t_UCHAR, nx, ny, nz);
  if (imdir == NULL){
    (void)sprintf(buf,"us_FlatDir(): not enough memory!\n");
    errputstr(buf);
    return NULL;
  }

  pim=(PIX_TYPE *)GetImPtr(im);
  pim0=(PIX_TYPE *)GetImPtr(im);
  pflat=(FLAT_TYPE *)GetImPtr(flat);
  pflat0=(FLAT_TYPE *)GetImPtr(flat); /* used for computing offsets */
  pdir0 = (UCHAR *)GetImPtr(imdir);

  /* set to FLAT_VAL all non-zero pixels */
  for (i=nx*ny;i>0;i--,pflat++)
    if (*pflat!=0)
      *pflat=FLAT_VAL;
  pflat=(FLAT_TYPE *)GetImPtr(flat);

  /* set shift array */
  set_seq_shift(nx, ny, nz, graph, shft);

  q = create_fifo4(1024);     /* use for geodesic distance computations */
  if (q == NULL)
    return NULL;

  qflat = create_fifo4(1024); /* will hold all pixels of flat region */
  if (qflat == NULL)
    return NULL;

  qdst = create_fifo4(1024);        /* used for computing distance transform */
  if (qdst == NULL)
    return NULL;

  qdb = create_fifo4(1024);         /* will hold descending borders of flat region (one at a time) */
  if (qdb == NULL)
    return NULL;

  qdball = create_fifo4(1024);   /* will hold descending borders of all flat regions  */
  if (qdball == NULL)
    return NULL;

  qab = create_fifo4(1024);         /* will hold ascending borders of flat region (one at a time) */
  if (qab == NULL)
    return NULL;

  for (j=nx*ny*nz; j > 0; j--, pflat++, pim++){
    if (*pflat == FLAT_VAL){ /* unprocessed flat region */
      *pflat=FLAT_MAX;
      flatval=*pim;
      fifo4_add(q, (long int)(pflat));
      fifo4_add(qflat, (long int)(pflat));

      while (fifo4_empty(q) == 0){ /* fill in flat region and initialize queues */
	ptr = (FLAT_TYPE *)fifo4_remove(q);
        for (k = 0; k < graph; k++){
	  p = ptr + shft[k];
	  if (*p == FLAT_VAL){
            *p=FLAT_MAX;
	    fifo4_add(q, (long int)p);
	    fifo4_add(qflat, (long int)p);
	  }
	  else if (*(pim0+(p-pflat0))>flatval && (*p==0)){ /* external ascending border */
	    *p=FLAT_MAXM1;
	    fifo4_add(qdst, (long int)p);
            fifo4_add(qab, (long int)p);
	  }
	  else if ((*(pim0+(p-pflat0))==flatval)  && (*p==0)){ /* internal descending border */
	    *p=FLAT_DB;
	    fifo4_add(qdb, (long int)p);
	    fifo4_add(qdball, (long int)p);
	  }
	}
      }

      /* compute geodesic distances */
      dcrt = 0;
      while (fifo4_empty(qdst) == 0){
	fifo4_add(qdst, 1L);
	while ((pm = (FLAT_TYPE *)fifo4_remove(qdst)) != (FLAT_TYPE *)1L){
	  *pm = dcrt;
	  for (k=0; k < graph; ++k){
	    if (*(pm + shft[k]) == FLAT_MAX){
	      *(pm + shft[k]) = FLAT_MAXM1;
	      fifo4_add(qdst, (long int)(pm + shft[k]));
	    }
	  }
	}
	dcrt++;
#ifdef DEBUG
	if (dcrt >= FLAT_DB){
	  sprintf(buf, "emergency in us_FlatDir(): dcrt=%ld (>= FLAT_DB)\n", (long int)dcrt);
	  stdputstr(buf);
	}
#endif
      }

      if (dcrt==0)
	dcrt=1;

      /* invert geodesic distances in flat region */
      fifo4_lookreset(qflat);
      if (dcrt==1){ /* flat top */
	while ((pm=(FLAT_TYPE *)fifo4_look(qflat)) != NULL){
	  *pm = 1;
	}
      }
      else{ /* intermediate plateau */
	while ((pm=(FLAT_TYPE *)fifo4_look(qflat)) != NULL){
	  *pm = dcrt-*pm;
	}
      }

      /* compute geodesic time function from descending border */

      /* Create an array of FIFO	*/
      if ((fifo = (FIFO **)calloc(PR_MAX + 1, sizeof(FIFO *))) == NULL){
        (void) sprintf(buf, "us_FlatDir(): not enough memory for the FAH\n");
	errputstr(buf);
	free_fifo4(q);
	free_fifo4(qflat);
	free_fifo4(qdst);
	free_fifo4(qdb);
	free_fifo4(qdball);
	free_fifo4(qab);
        return NULL;
      }

      pr_max = PR_MAX;

      /* initialize the FAH */
      while (fifo4_empty(qdb) == 0){
	pr = (FLAT_TYPE *)fifo4_remove(qdb);
	*pr=0;
	for (k = 0; k < graph; ++k){
	  p_k=pr + shft[k];
	  if (*p_k && (*p_k<=dcrt)){

	    if (*p_k > pr_max){
	      if ((fifot = (FIFO **)calloc(*p_k + 1, sizeof(FIFO *))) == NULL){
		(void) sprintf(buf, "us_FlatDir(): not enough memory for the FAH\n");
		errputstr(buf);
		free((char *)fifo);
		free_fifo4(q);
		free_fifo4(qflat);
		free_fifo4(qdst);
		free_fifo4(qdb);
		free_fifo4(qdball);
		free_fifo4(qab);
		return (NULL);
	      }
	      for (i = 0; i <= pr_max; ++i)
		fifot[i] = fifo[i];
	      free((char *)fifo);
	      fifo=fifot;
	      fifot=NULL;
	      fifo[*p_k] = alloc_fifo(10L);
	      pr_max = *p_k;
	    }
	    else if (fifo[*p_k] == NULL){
	      fifo[*p_k] = alloc_fifo(10L);
	    }

	    fifo_add(fifo[*p_k], (long int)p_k);
	    *p_k |= REF_PIX_MSB;
	  }
	}
      }

      /* Ordered propagation of geodesic time function */
      for (t = 0; t <= pr_max; t++){
	pf = fifo[t];
	if (pf != NULL){
	  while (fifo_empty(pf) == FALSE){
	    pm = (FLAT_TYPE *)fifo_remove(pf);
	    for (k = 0; k < graph; ++k){
	      p_k = pm + shft[k];
	      if (*p_k & REF_PIX_MSB || (*p_k==0))
		continue;
	      *p_k += t;

	      /* emergency stop in case of overflow */
	      if (*p_k >= REF_PIX_MSB){
		sprintf(buf, "us_FlatDir(): emergency stop: geodesic time exceeds REF_PIX_MSB: *pk=%ld, pr_max=%ld, dcrt=%ld\n", (long int)*p_k, (long int)pr_max, (long int)dcrt);
		errputstr(buf);
		free((char *)fifo);
		free_fifo4(q);
		free_fifo4(qflat);
		free_fifo4(qdst);
		free_fifo4(qdb);
		free_fifo4(qdball);
		free_fifo4(qab);
		return(NULL);
	      }

	      if (*p_k > pr_max){
		if ((fifot = (FIFO **)calloc(*p_k + 1, sizeof(FIFO *))) == NULL){
		  sprintf(buf, "us_FlatDir(): emergency stop: not enough memory for FAH: *pk=%ld, pr_max=%ld, dcrt=%ld\n", (long int)*p_k, (long int)pr_max, (long int)dcrt);
		  errputstr(buf);
		  free((char *)fifo);
		  free_fifo4(q);
		  free_fifo4(qflat);
		  free_fifo4(qdst);
		  free_fifo4(qdb);
		  free_fifo4(qdball);
		  free_fifo4(qab);
		  return (NULL);
		}
		for (i = 0; i <= pr_max; ++i)
		  fifot[i] = fifo[i];
		free((char *)fifo);
		fifo=fifot;
		fifot=NULL;
		fifo[*p_k] = alloc_fifo(10L);
		pr_max = *p_k;
	      }
	      else if (fifo[*p_k] == NULL){
		fifo[*p_k] = alloc_fifo(10L);
	      }

	      fifo_add(fifo[*p_k], (long int)p_k);
	      *p_k |= REF_PIX_MSB;
	    }
	  }
	  clear_fifo(pf);
	}
      }
      free((char *)fifo);

      /* compute drainage directions on created relief */
      fifo4_lookreset(qdball);
      while ( (pm=(FLAT_TYPE *)fifo4_look(qdball)) != NULL) /* reset to 1 all descending borders */
	*pm = 0;

      fifo4_lookreset(qab);
      while ( (pm=(FLAT_TYPE *)fifo4_look(qab)) != NULL)    /* reset to FLAT_MAX all ascending borders */
	*pm = FLAT_MAX;

      fifo4_lookreset(qflat);
      while ( (pm=(FLAT_TYPE *)fifo4_look(qflat)) != NULL)      /* set all flat pixels to their distance value */
	*pm ^= REF_PIX_MSB ;

      fifo4_lookreset(qflat);

      while ( (pm=(FLAT_TYPE *)fifo4_look(qflat)) != NULL){    /* compute drainage direction */
	/* initializations */
	lowest=*pm;
	dir=0;   /* no lower neighbour */
	slope=0; /* no slope to lower neighbour */

        /* process 4-neighbours first */
	if (*(pm+shft1)<lowest){
	  lowest=*(pm+shft1);
	  dir=1;
	}
	if (*(pm+shft2)<lowest){
	  lowest=*(pm+shft2);
	  dir=2;
	}
	if (*(pm+shft3)<lowest){
	  lowest=*(pm+shft3);
	  dir=3;
	}
	if (*(pm+shft4)<lowest){
	  lowest=*(pm+shft4);
	  dir=4;
	}
	if (dir)
	  slope=(unsigned long int)(*pm-lowest)*(unsigned long int)(*pm-lowest)*2;

       /* remaining neighbours */
	vngb=*(pm+shft5);
	if (vngb<lowest){
	  delta=*pm-vngb;
	  if ((stmp=delta*delta)>slope){
	    slope = stmp;
	    dir = 5;
	    lowest=vngb;
	  }
	}
	vngb=*(pm+shft6);
	if (vngb<lowest){
	  delta=*pm-vngb;
	  if ((stmp=delta*delta)>slope){
	    slope = stmp;
	    dir = 6;
	    lowest=vngb;
	  }
	}
	vngb=*(pm+shft7);
	if (vngb<lowest){
	  delta=*pm-vngb;
	  if ((stmp=delta*delta)>slope){
	    slope = stmp;
	    dir = 7;
	    lowest=vngb;
	  }
	}
	vngb=*(pm+shft8);
	if (vngb<lowest){
	  delta=*pm-vngb;
	  if ((stmp=delta*delta)>slope){
	    slope = stmp;
	    dir = 8;
	    lowest=vngb;
	  }
	}
	*(pdir0+(pm-pflat0))=dir;
      }

      while ( (pm=(FLAT_TYPE *)fifo4_remove(qdball)) != NULL) /* reset to 0 all descending borders */
	*pm = 0;
      fifo4_lookreset(qdball);

      while ( (pm=(FLAT_TYPE *)fifo4_remove(qab)) != NULL) /* reset to 0 all ascending borders */
	*pm = 0;
      fifo4_lookreset(qab);

      while ( (pm=(FLAT_TYPE *)fifo4_remove(qflat)) != NULL) /* reset to REF_PIX_MSB all flat pixels */
	*pm = REF_PIX_MSB;
      fifo4_lookreset(qflat);
    }
  }

  /* report on maximum pr_max value */
  sprintf(buf, "us_FlatDir(): maximum pr_max value reached = %ld\n", (long int)pr_max);
  stdputstr(buf);

  /* reset flat image */
  for (j=nx*ny*nz; j > 0; j--, pflat0++)
    if (*pflat0)
      *pflat0 = FLAT_VAL;

  free_fifo4(q);
  free_fifo4(qflat);
  free_fifo4(qdst);
  free_fifo4(qdb);
  free_fifo4(qdball);
  free_fifo4(qab);

  return imdir;

}
#undef FLAT_TYPE
#undef PR_MAX
#undef FLAT_VAL
#undef FLAT_MAX
#undef FLAT_MAXM1
#undef REF_PIX_MSB
#undef FLAT_DB
#include "us_undef.h"



#include "us_def.h"
#define FLAT_TYPE UINT32
#define PR_MAX 255
#define FLAT_MAX   4294967295UL
#define FLAT_MAXM1 4294967294UL  /* UINT32_MAX-1   */
#define FLAT_VAL   4294967293UL  /* UINT32_MAX-2   */
#define FLAT_DB    4294967292UL  /* UINT32_MAX-3   */
#define REF_PIX_MSB   0x80000000
IMAGE *u32_FlatDir(IMAGE *flat, IMAGE *im, int graph)
{
  long int i, j, k, t, nx, ny, nz, shft[27];
  FIFO4 *q, *qflat, *qdst, *qdb, *qdball, *qab;
  FIFO **fifo, **fifot, *pf;
  IMAGE *imdir;

  FLAT_TYPE *p, *ptr, *pflat, *pflat0, dcrt, *pm, *pr, *p_k, pr_max=0, vngb, lowest;
  PIX_TYPE *pim, *pim0, flatval;
  UCHAR *pdir0, dir;

  int delta;
  double slope, stmp, sqrt2=sqrt(2.0);
  long int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;

  if (szgeocompat(im, flat) != NO_ERROR){
    (void) sprintf(buf, "u32_FlatDir(): input images must be of same type\n");
    errputstr(buf);
    return NULL;
  }

  nx =GetImNx(im);
  ny =GetImNy(im);
  nz =GetImNz(im);

  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  imdir = (IMAGE *)create_image(t_UCHAR, nx, ny, nz);
  if (imdir == NULL){
    (void)sprintf(buf,"u32_FlatDir(): not enough memory!\n");
    errputstr(buf);
    return NULL;
  }

  pim=(PIX_TYPE *)GetImPtr(im);
  pim0=(PIX_TYPE *)GetImPtr(im);
  pflat=(FLAT_TYPE *)GetImPtr(flat);
  pflat0=(FLAT_TYPE *)GetImPtr(flat); /* used for computing offsets */
  pdir0 = (UCHAR *)GetImPtr(imdir);

  /* set to FLAT_VAL all non-zero pixels */
  for (i=nx*ny;i>0;i--,pflat++)
    if (*pflat!=0)
      *pflat=FLAT_VAL;
  pflat=(FLAT_TYPE *)GetImPtr(flat);

  /* set shift array */
  set_seq_shift(nx, ny, nz, graph, shft);

  q = create_fifo4(1024);     /* use for geodesic distance computations */
  if (q == NULL)
    return NULL;

  qflat = create_fifo4(1024); /* will hold all pixels of flat region */
  if (qflat == NULL)
    return NULL;

  qdst = create_fifo4(1024);        /* used for computing distance transform */
  if (qdst == NULL)
    return NULL;

  qdb = create_fifo4(1024);         /* will hold descending borders of flat region (one at a time) */
  if (qdb == NULL)
    return NULL;

  qdball = create_fifo4(1024);   /* will hold descending borders of all flat regions  */
  if (qdball == NULL)
    return NULL;

  qab = create_fifo4(1024);         /* will hold ascending borders of flat region (one at a time) */
  if (qab == NULL)
    return NULL;

  for (j=nx*ny*nz; j > 0; j--, pflat++, pim++){
    if (*pflat == FLAT_VAL){ /* unprocessed flat region */
      *pflat=FLAT_MAX;
      flatval=*pim;
      fifo4_add(q, (long int)(pflat));
      fifo4_add(qflat, (long int)(pflat));

      while (fifo4_empty(q) == 0){ /* fill in flat region and initialize queues */
	ptr = (FLAT_TYPE *)fifo4_remove(q);
        for (k = 0; k < graph; k++){
	  p = ptr + shft[k];
	  if (*p == FLAT_VAL){
            *p=FLAT_MAX;
	    fifo4_add(q, (long int)p);
	    fifo4_add(qflat, (long int)p);
	  }
	  else if (*(pim0+(p-pflat0))>flatval && (*p==0)){ /* external ascending border */
	    *p=FLAT_MAXM1;
	    fifo4_add(qdst, (long int)p);
            fifo4_add(qab, (long int)p);
	  }
	  else if ((*(pim0+(p-pflat0))==flatval)  && (*p==0)){ /* internal descending border */
	    *p=FLAT_DB;
	    fifo4_add(qdb, (long int)p);
	    fifo4_add(qdball, (long int)p);
	  }
	}
      }

      /* compute geodesic distances */
      dcrt = 0;
      while (fifo4_empty(qdst) == 0){
	fifo4_add(qdst, 1L);
	while ((pm = (FLAT_TYPE *)fifo4_remove(qdst)) != (FLAT_TYPE *)1L){
	  *pm = dcrt;
	  for (k=0; k < graph; ++k){
	    if (*(pm + shft[k]) == FLAT_MAX){
	      *(pm + shft[k]) = FLAT_MAXM1;
	      fifo4_add(qdst, (long int)(pm + shft[k]));
	    }
	  }
	}
	dcrt++;
#ifdef DEBUG
	if (dcrt >= FLAT_DB){
	  sprintf(buf, "emergency in u32_FlatDir(): dcrt=%ld (>= FLAT_DB)\n", (long int)dcrt);
	  stdputstr(buf);
	}
#endif
      }

      if (dcrt==0)
	dcrt=1;

      /* invert geodesic distances in flat region */
      fifo4_lookreset(qflat);
      if (dcrt==1){ /* flat top */
	while ((pm=(FLAT_TYPE *)fifo4_look(qflat)) != NULL){
	  *pm = 1;
	}
      }
      else{ /* intermediate plateau */
	while ((pm=(FLAT_TYPE *)fifo4_look(qflat)) != NULL){
	  *pm = (dcrt-*pm);
	}
      }

      /* compute geodesic time function from descending border */

      /* Create an array of FIFO	*/
      if ((fifo = (FIFO **)calloc(PR_MAX + 1, sizeof(FIFO *))) == NULL){
        (void) sprintf(buf, "u32_FlatDir(): not enough memory for the FAH\n");
	errputstr(buf);
	free_fifo4(q);
	free_fifo4(qflat);
	free_fifo4(qdst);
	free_fifo4(qdb);
	free_fifo4(qdball);
	free_fifo4(qab);
        return NULL;
      }

      pr_max = PR_MAX;

      /* initialize the FAH */
      while (fifo4_empty(qdb) == 0){
	pr = (FLAT_TYPE *)fifo4_remove(qdb);
	*pr=0;
	for (k = 0; k < graph; ++k){
	  p_k=pr + shft[k];
	  if (*p_k && (*p_k<=dcrt)){

	    if (*p_k > pr_max){
	      if ((fifot = (FIFO **)calloc(*p_k + 1, sizeof(FIFO *))) == NULL){
		(void) sprintf(buf, "u32_FlatDir(): not enough memory for the FAH: *p_k=%u, pr_max=%u\n", *p_k, pr_max);
		errputstr(buf);
		free((char *)fifo);
		free_fifo4(q);
		free_fifo4(qflat);
		free_fifo4(qdst);
		free_fifo4(qdb);
		free_fifo4(qdball);
		free_fifo4(qab);
		return (NULL);
	      }
	      for (i = 0; i <= pr_max; ++i)
		fifot[i] = fifo[i];
	      free((char *)fifo);
	      fifo=fifot;
	      fifot=NULL;
	      fifo[*p_k] = alloc_fifo(10L);
	      pr_max = *p_k;
	    }
	    else if (fifo[*p_k] == NULL){
	      fifo[*p_k] = alloc_fifo(10L);
	    }

	    fifo_add(fifo[*p_k], (long int)p_k);
	    *p_k |= REF_PIX_MSB;
	  }
	}
      }

      /* Ordered propagation of geodesic time function */
      for (t = 0; t <= pr_max; t++){
	pf = fifo[t];
	if (pf != NULL){
	  while (fifo_empty(pf) == FALSE){
	    pm = (FLAT_TYPE *)fifo_remove(pf);
	    for (k = 0; k < graph; ++k){
	      p_k = pm + shft[k];
	      if (*p_k & REF_PIX_MSB || (*p_k==0))
		continue;
	      *p_k += t;

	      /* emergency stop in case of overflow */
	      if (*p_k >= REF_PIX_MSB){
		sprintf(buf, "u32_FlatDir(): emergency stop: geodesic time exceeds REF_PIX_MSB: *pk=%ld, pr_max=%ld, dcrt=%ld\n", (long int)*p_k, (long int)pr_max, (long int)dcrt);
		errputstr(buf);
		free((char *)fifo);
		free_fifo4(q);
		free_fifo4(qflat);
		free_fifo4(qdst);
		free_fifo4(qdb);
		free_fifo4(qdball);
		free_fifo4(qab);
		return(NULL);
	      }

	      if (*p_k > pr_max){
		if ((fifot = (FIFO **)calloc(*p_k + 1, sizeof(FIFO *))) == NULL){
		  sprintf(buf, "u32_FlatDir(): emergency stop: not enough memory for FAH: *pk=%ld, pr_max=%ld, dcrt=%ld\n", (long int)*p_k, (long int)pr_max, (long int)dcrt);
		  errputstr(buf);
		  free((char *)fifo);
		  free_fifo4(q);
		  free_fifo4(qflat);
		  free_fifo4(qdst);
		  free_fifo4(qdb);
		  free_fifo4(qdball);
		  free_fifo4(qab);
		  return (NULL);
		}
		for (i = 0; i <= pr_max; ++i)
		  fifot[i] = fifo[i];
		free((char *)fifo);
		fifo=fifot;
		fifot=NULL;
		fifo[*p_k] = alloc_fifo(10L);
		pr_max = *p_k;
	      }
	      else if (fifo[*p_k] == NULL){
		fifo[*p_k] = alloc_fifo(10L);
	      }

	      fifo_add(fifo[*p_k], (long int)p_k);
	      *p_k |= REF_PIX_MSB;
	    }
	  }
	  clear_fifo(pf);
	}
      }
      free((char *)fifo);

      /* compute drainage directions on created relief */
      fifo4_lookreset(qdball);
      while ( (pm=(FLAT_TYPE *)fifo4_look(qdball)) != NULL) /* reset to 1 all descending borders */
	*pm = 0;

      fifo4_lookreset(qab);
      while ( (pm=(FLAT_TYPE *)fifo4_look(qab)) != NULL)    /* reset to FLAT_MAX all ascending borders */
	*pm = FLAT_MAX;

      fifo4_lookreset(qflat);
      while ( (pm=(FLAT_TYPE *)fifo4_look(qflat)) != NULL)      /* set all flat pixels to their distance value */
	*pm ^= REF_PIX_MSB ;

      fifo4_lookreset(qflat);

      while ( (pm=(FLAT_TYPE *)fifo4_look(qflat)) != NULL){    /* compute drainage D8 direction */
	/* initializations */
	lowest=*pm;
	dir=0;   /* no lower neighbour */
	slope=0; /* no slope to lower neighbour */

	/* process 4-neighbours first */
	if (*(pm+shft1)<lowest){
	  lowest=*(pm+shft1);
	  dir=1;
	}
	if (*(pm+shft2)<lowest){
	  lowest=*(pm+shft2);
	  dir=2;
	}
	if (*(pm+shft3)<lowest){
	  lowest=*(pm+shft3);
	  dir=3;
	}
	if (*(pm+shft4)<lowest){
	  lowest=*(pm+shft4);
	  dir=4;
	}
	if (dir)
	  slope=(double)(*pm-lowest); /* b/a a=1 */

	/* remaining neighbours */
	vngb=*(pm+shft5);
	if (vngb<lowest){
	  delta=*pm-vngb;
	  if ((stmp=((double)delta)/sqrt2)>slope){
	    slope = stmp;
	    dir = 5;
	    lowest=vngb;
	  }
	}
	vngb=*(pm+shft6);
	if (vngb<lowest){
	  delta=*pm-vngb;
	  if ((stmp=((double)delta)/sqrt2)>slope){
	    slope = stmp;
	    dir = 6;
	    lowest=vngb;
	  }
	}
	vngb=*(pm+shft7);
	if (vngb<lowest){
	  delta=*pm-vngb;
	  if ((stmp=((double)delta)/sqrt2)>slope){
	    slope = stmp;
	    dir = 7;
	    lowest=vngb;
	  }
	}
	vngb=*(pm+shft8);
	if (vngb<lowest){
	  delta=*pm-vngb;
	  if ((stmp=((double)delta)/sqrt2)>slope){
	    slope = stmp;
	    dir = 8;
	    lowest=vngb;
	  }
	}
	*(pdir0+(pm-pflat0))=dir;
      }

      fifo4_lookreset(qdball);
      while ( (pm=(FLAT_TYPE *)fifo4_remove(qdball)) != NULL) /* reset to 0 all descending borders */
	*pm = 0;

      fifo4_lookreset(qab);
      while ( (pm=(FLAT_TYPE *)fifo4_remove(qab)) != NULL) /* reset to 0 all ascending borders */
	*pm = 0;

      fifo4_lookreset(qflat);
      while ( (pm=(FLAT_TYPE *)fifo4_remove(qflat)) != NULL) /* reset to REF_PIX_MSB all flat pixels */
	*pm = REF_PIX_MSB;
    }
  }

  /* report on maximum pr_max value */
  sprintf(buf, "u32_FlatDir(): maximum pr_max value reached = %ld\n", (long int)pr_max);
  stdputstr(buf);

  /* reset flat image */
  for (j=nx*ny*nz; j > 0; j--, pflat0++)
    if (*pflat0)
      *pflat0 = FLAT_VAL;

  free_fifo4(q);
  free_fifo4(qflat);
  free_fifo4(qdst);
  free_fifo4(qdb);
  free_fifo4(qdball);
  free_fifo4(qab);

  return imdir;
}
#undef FLAT_TYPE
#undef PR_MAX
#undef FLAT_VAL
#undef FLAT_MAX
#undef FLAT_MAXM1
#undef REF_PIX_MSB
#undef FLAT_DB
#include "us_undef.h"


IMAGE *FlatDir(IMAGE *flat, IMAGE *im, int graph)
{
  /* Compute geodesic distance away from ascending borders on all flat
     regions, then the geodesic time funtion from descending borders.
     The flat image must be of type USHORT or UINT32 with pixels
     without drainage set to 1 (all other pixels set to 0). Then
     compute (and output) the D8 drainage direction for each pixel of
     the flat regions.  First: 06-11-2001
  */

  if ( GetImDataType(im) != t_USHORT ){
    (void) sprintf(buf, "FlatDir(IMAGE *flat, IMAGE *im, int graph): the image im must be of type USHORT \n");
    errputstr(buf);
    return NULL;
  }

  switch (GetImDataType(flat)){

  case t_USHORT:
    return(us_FlatDir(flat, im, graph));
    break;

  case t_UINT32:
    return(u32_FlatDir(flat, im, graph));
    break;

  default:
    (void)sprintf(buf, "Error in FlatDir(IMAGE *flat, IMAGE *im, int graph): \
                invalid ImDataType for flat image: must be of type USHORT \
                or UINT32 (with flat pixels defined as all non-zero pixels) (\n");
    errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@}*/
