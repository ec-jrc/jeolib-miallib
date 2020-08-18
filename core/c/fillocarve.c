/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2002-2020 European Union (Joint Research Centre)

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
 *  Optimal removal of spurious pits in digital elevation models \cite soille2004wrr and \cite soille2004prl
 *  @author Pierre Soille
 */

#include <stdio.h>
#include <stdlib.h>
#include "miallib.h"
#include "fah.h"
#include "fifo.h"
#include "pqueue.h"


#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif


/** \addtogroup group_dem
 *  @{
 */

#include "us_def.h"
#define LABEL_TYPE unsigned short
#define LABEL_MAX 0x7FFF /* 16383 */  /* was  0x7FFF */
#define LABEL_MSB 0x8000
#define FLOODED 0x80
#define REACHED 0x40
#define FLRE    0x0F  /* FLOODED|REACHED */
IMAGE *us_fillocarve_energy(unsigned short *iml, unsigned short *imr, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE 20-11-2002 (1st)
  ** iml: pointer to an image of labels with LABEL_MAX valued border
     (relevant minima)
  ** imr: pointer to a reference image
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */

  /*   char fname[50]; */
  int pcount=0;
  IMAGE im[2];
  int box[6];

  int casea=0, caseae=0, caseb=0, casebe=0, casec=0, casece=0, cased=0, casede=0; /* 2003-11-10 */
  int *eeb; /* 2003-11-12 for energy of embedded pit */

  int *ec, *ef, *af, dyn, iori=0;
  int h, priocrt=0, ah, level, emin;
  struct pqueue *heap;
  PQDATUM apqd[1];
  struct node *pqd;
  UCHAR *apdir, *apdirk;
  int flag=1, flage=0;

  long int index;

  long int k, ofs, ofsk, ofsi=0;
  LABEL_TYPE *pl;
  PIX_TYPE *pr;
  FIFO **fah, *pq;
  long int i, shftk, npix;

  IMAGE *imdir;
  UCHAR *pdir;
  FIFO4 *aq;
  LABEL_TYPE *p1, *p2;
  PIX_TYPE minval;
  long int ak, bofs, crtlevel;


  heap = (struct pqueue *)pqinit(NULL, 100);  /* heap (priority queue) */
  if (heap == NULL)
    return NULL;
  

  npix = (long int)nx*ny*nz;
  maxfl++;


  im[0].p_im = (char *)iml;
  im[0].DataType = 5;
  im[0].nx = nx;
  im[0].ny = ny;
  im[0].nz = nz;
  im[0].NByte = (mia_size_t)nx*ny*nz*2;
  im[0].vol = 0;
  im[0].lut = NULL;

  im[1].p_im = (char *)imr;
  im[1].DataType = 5;
  im[1].nx = nx;
  im[1].ny = ny;
  im[1].nz = nz;
  im[1].NByte = (mia_size_t)nx*ny*nz*2;
  im[1].vol = 0;
  im[1].lut = NULL;

  BOX_2D;


  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("fillocarve(): not enough memory for the FAH\n");
   return NULL;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    fah[i] = alloc_fifo(100);  /* WAS ((nx*ny*nz)/maxfl)/100+1 */
    if (fah[i]==NULL){
      (void) printf("fillocarve(): not enough memory for the FIFO\n");
      free(fah);
      return NULL;
    }
  }

  /* create an image for storing the flood directions */
  imdir = (IMAGE *)create_image(t_UCHAR, nx, ny, nz);
  if (imdir == NULL){
    (void)sprintf(buf,"fillocarve(): not enough memory!\n"); errputstr(buf);
    for (i=0; i<maxfl; i++)
      clear_fifo(fah[i]);
    free(fah);
    return(imdir);
  }
  pdir=(UCHAR *)GetImPtr(imdir);
 

  /* initialize the queues */
  for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
    if ((*pl>1)&&(*pl<LABEL_MAX)){
      for (k=0;k<nshft;k++){
	shftk=shft[k];
	if (*(pl+shftk)==0){
	  if (*(pr+shftk)<maxfl){
	    *(pl+shftk)=*pl|LABEL_MSB; 
	    fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	    pdir[i+shftk]=k;
	  }
	  else
	    *(pl+shftk)=LABEL_MAX;
	}
      }
    }
  }
   
  for (i=0,pl=iml; i<npix; i++,pl++)
    if (*pl>LABEL_MAX)
      *pl ^= LABEL_MSB;

  /* here we go */
  for (i=0; i<maxfl; i++){  // flood from relevant minima
    // printf("crt level =%ld\n", i);
    pq = fah[i];
    while (fifo_empty(pq) == FALSE){
      ofs=fifo_remove(pq);
      if (*(imr+ofs)!=i){
 	// printf("reinsert in queue: i=%d *(imr+ofs)=%d\n", i, *(imr+ofs)); 
        fifo_add(fah[*(imr+ofs)],(long int)ofs);
	continue;
      }
      for (k=0; k<nshft; ++k){
	ofsk=ofs+shft[k];
	if (*(iml+ofsk)<2){
	  if (ofsk==11566){
	    printf("non-positive dynamic, ofsk=%ld!!!\n", ofsk);
	    dumpxyz(&im[0],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
	    dumpxyz(&im[1],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
	    } 
	  if (*(imr+ofsk)<maxfl){
	    crtlevel=*(imr+ofsk);
	    // printf("test whether we go down, i=%ld, crtlevel=%ld\n", i, crtlevel);
	    if (i>crtlevel){ /* perhaps should simply follow steepest slope path */
	      // printf("We go down\n");
	      if (flag){
		iori=i;
		ofsi=ofs;
		/* 	printf("iori=%d\n", iori); */
		flag=0;
	      }
	      i=crtlevel;
	      pq=fah[crtlevel];
	    }
	    /* else if (i<crtlevel){ */
/* 	      printf("We go up\n"); */
/* 	      flag=1; */
/* 	    } */
	    fifo_add(fah[crtlevel],(long int)ofsk);
	    pdir[ofsk]=k;

	    if ( (*(iml+ofsk)==1) &&  ( (iori- *(imr+ofsk)) < 0) ){ // 20090817: a former irrelevant minimum is not a minimum anymore!
	      *(iml+ofsk)=0;
	      aq = create_fifo4(10);
	      fifo4_add(aq, (long int)(ofsk));
	      while ((index = fifo4_remove(aq))){
		for (ak=0; ak < nshft; ak++){
		  if ( *(iml+index+shft[ak])== *(imr+ofsk) ){
		    *(iml+index+shft[ak]) = 0;
	            fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		}
	      }
	      free_fifo4(aq);
	    }
	    if (*(iml+ofsk)==1){   /* ! should only occur when going down ! irrelevant minima reached */
	      flag=1;
	      flage=0;
	      // printf("irrelevant n=minimum reached\n");
	      minval = *(imr+ofsk); /* elevation of minimum, i=elevation of highest point on carving path */
	      dyn = iori-minval; /* relative dynamic of reached minimum */
	      // printf("pit encountered, minval=%d, iori=%d, dyn=%d, crtlevel=%ld, x=%ld, y=%ld, i=%ld, ix=%ld, iy=%ld\n", minval, iori, dyn, crtlevel, ofsk-(long int)(ofsk/nx)*nx, (long int)(ofsk/nx), i, ofs-(long int)(ofs/nx)*nx, (long int)(ofs/nx)); 
	      if ( (ofsk==11566)  || (dyn<1) ){
		printf("non-positive dynamyic, ofsk=%ld!!!\n", ofsk);
		printf("iori=%d\t minval=%d\n", iori, minval);
		dumpxyz(&im[0],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,15,15);
		dumpxyz(&im[1],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,15,15);
		dumpxyz(imdir,ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,15,15);
	      }
	      pcount+=1;

	      
              /* sprintf(fname, "/home/pierre/tmp/rieslbl%03d.tif", pcount);
	      write_tiff(&im[0], fname);
              sprintf(fname, "/home/pierre/tmp/riesref%03d.tif", pcount);
              write_tiff(&im[1], fname); */

	      
	      if ( (ec = (int *)calloc(dyn+1, sizeof(int))) == NULL ){ /* energy of carving  */
		(void)sprintf(buf,"not enough memory for ec array (dyn=%d)\n", dyn); errputstr(buf);
		free_pq(heap);
		return NULL;
	      }
	      if ( (ef = (int *)calloc(dyn+1, sizeof(int))) == NULL){  /* energy of flooding */
		(void)sprintf(buf,"not enough memory for ef array (dyn=%d)\n", dyn); errputstr(buf);
		free(ec);
		free_pq(heap);
		return NULL;
	      }
	      if ( (af = (int *)calloc(dyn+1, sizeof(int))) == NULL){  /* flooded areas      */
		(void)sprintf(buf,"not enough memory for af array (dyn=%d)\n", dyn); errputstr(buf);
		free(ec);
		free(ef);
		free_pq(heap);
		return NULL;
	      }
	      if ( (eeb = (int *)calloc(dyn+1, sizeof(int))) == NULL ){ /* energy of carving  */
		(void)sprintf(buf,"not enough memory for ec array (dyn=%d)\n", dyn); errputstr(buf);
		free_pq(heap);
		free(ec);
		free(ef);
		free(af);
		return NULL;
	      }

	      ef[0]=0;

	      /* initialise heap with neighbours of minima
		 while calculating area of minimum */

	      while ( pqremove(heap, apqd) != NULL ){	 /* make sure heap is empty */
		free((char*) *apqd);
	      }
	

	      aq = create_fifo4(10);
	      af[0]=1;
	      *(pdir+ofsk)|=FLOODED;
	      fifo4_add(aq, (long int)(ofsk));
	      while ((index = fifo4_remove(aq))){
		p1=iml+index;
		apdir=pdir+index;
		for (ak=0; ak < nshft; ak++){
		  p2 = p1 + shft[ak];
		  apdirk=apdir+shft[ak];
		  if ( (*p2 == 1) && (*apdirk==0) ){ /* in minimum and not yet flooded */
		    af[0]+=1;
		    /* *p2 = FLOODED; */ *apdirk = FLOODED;
	            fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		  else if ( (*p2!=1) && (*apdirk<8)  &&  (*(imr+ (int) (apdirk-pdir))) < iori ){ /* if (*p2 == 0){ */ /* border of minimum and not yet REACHED */
		    /* printf("insert border of pit in heap\n"); */
		    pqd = (PQDATUM )malloc(sizeof(struct node));
/* 		    printf("*(imr+ (int) (apdirk-pdir))=%d\t", *(imr+ (int) (apdirk-pdir))); */
/* 		    printf("offset=%d\t", index+shft[ak]); */
/* 		    printf("prio=%d\n", (*(imr+ (int) (apdirk-pdir))-minval)); */
		    pqd->prio = PIX_RANGE-(*(imr+ (int) (apdirk-pdir))-minval);
		    /* pqd->prio = PIX_RANGE-( *(imr+index+shft[ak]) - minval ); */
		    /* pqd->val   = val; not used */
		    pqd->offset= (long int)(index+shft[ak]);
		    pqinsert(heap, pqd);
		    *apdirk |= REACHED;	    
		  }
		}
	      }
	      free_fifo4(aq);
	      ef[1]=af[0];

	      bofs=ofs; /* calculate ec[0] */
	      while (*(imr+bofs)>minval){ /* backtrack */
		if (*(imr+bofs)>minval)
		  ec[0]+= *(imr+bofs)-(minval);   /* PSH missing parenthesis */
		bofs-=shft[pdir[bofs]&FLRE];
	      }
	      emin=ec[0];
	      
	      /* look for procedure with lowest energy */
	      for (h=1,ah=1; h<dyn+1; h++,ah++){  /* (PSHwas dyn+1) modify so as to stop as soon as the energy of the sum increases */

		/* compute energy of carving   */
		bofs=ofs;
		while (*(imr+bofs)>minval){ /* backtrack */
		  if (*(imr+bofs)>minval+h)
		    ec[h]+= *(imr+bofs)-(minval+h);   /* PSH missing parenthesis */
		  bofs-=shft[pdir[bofs]&FLRE];
		}

		/* check whether total energy increases, and break if so */
		ef[h]=ef[h-1]+af[h-1]+eeb[h-1];

	
		if (ec[h]+ef[h] > emin){ /* minimum energy reached */
		  if ( ec[0]==emin )
		    if (flage == 0)
		      casea++;  /* pure plain carving */
		    else{
		      caseae++; /* embedded  carving (should not occur) */
		      // printf("h=%d ec[0]=%d\t ec[1]+ef[1]=%d\n", h, ec[0], ec[1]+ef[1]);
		    }
		  else
		    if (flage == 0)
		      caseb++;  /* pure hybrid approach */
		    else
		      casebe++; /* embedded hybrid */
		  break;
		}
		emin=ec[h]+ef[h];
		
		/* compute energy of fillhole  */
		if ( pqpeek(heap, apqd) == NULL )
		  priocrt=h;
		while ( pqpeek(heap, apqd) != NULL ){ /* it may be empty !!! */
		  priocrt=PIX_RANGE-(*apqd)->prio;
/* 		  printf("h=%d\t priocrt=%d\n", h, priocrt); */
		  if (priocrt > h) /* no pixel to flood at the current level */
		    break; /* while loop */
		  af[h]+= 1; /* only for area part of mewly flooded pixel (downward for embedded) */
		  if (priocrt < h){ /* embedded pits */
		    eeb[h]+=minval+h-*(imr+ (int)(*apqd)->offset);
		    flage=1;
		  }
/* 		  printf("minval=%d\t h=%d\t af[h]=%d\t eeb[%d]=%d\n", minval, h, minval+h+1-*(imr+ (int)(*apqd)->offset ), h, eeb[h]); */
		  pqremove(heap, apqd);
		  apdir=pdir+(*apqd)->offset;
                  free((char*) *apqd);
		  *apdir|=FLOODED;
		  /* printf("af[%d]=%d\n",h,af[h]); */
		  for (ak=0; ak < nshft; ak++){
		    apdirk=apdir+shft[ak];
		    if ( (*apdirk<8) && (*(iml+(apdirk-pdir))!=LABEL_MAX) &&  (*(imr+ (int) (apdirk-pdir))) < iori){ /* not yet flooded */
		      pqd = (PQDATUM )malloc(sizeof(struct node));
		      pqd->prio = PIX_RANGE-(*(imr+ (int) (apdirk-pdir))-minval);

/* 		      printf("*(imr+ (int) (apdirk-pdir))=%d\t", *(imr+ (int) (apdirk-pdir))); */
/* 		      printf("offset=%d\t", apdirk-pdir); */
/* 		      printf("prio=%d\n", (*(imr+ (int) (apdirk-pdir))-minval)); */

		      /* pqd->val   = val; not used */
		      pqd->offset= (long int)(apdirk-pdir);
		      pqinsert(heap, pqd);
		      *apdirk|=REACHED;
		    }
		  }
		}
		af[h]+=af[h-1];

/* 		printf("iori=%d\t minval=%d\t dyn=%d\t priocrt=%d\t h=%d\n", iori, minval, dyn, priocrt, h); */

		/* if (ofsk==35400){
		  printf("non-positive dynamyic, ofsk=%ld!!!\n", ofsk);
		  dumpxyz(&im[0],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
		  dumpxyz(&im[1],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
		  } */
	      } /* for (h=0; h<dyn+1; h++) */

	      ah=h;

	      /* detect whether plain fillhole or degenerated case */
	      if ( ah==dyn+1){
		if (emin==ec[0]){
		  if (flage)
		     casede++; /* degenerated case */
		  else
		    cased++;
		  /* printf("degenerated case, ofsk=%d\t ah=%d\n", ofsk, ah); */
		}
		else
		  if (flage)
		    casece++;
		  else
		    casec++; /* plain pit filling */
	      }

	      
	      ah-=1;  /* optimum level */

	      free(ec); free(af); free(ef); free(eeb);
	      while ( pqremove(heap, apqd) != NULL ){	 /* make sure heap is emptied */
		*(pdir+(*apqd)->offset)&=FLRE; /* reset */
		free((char*) *apqd);
	      }

/* 	      dumpxyz(&im[1],10,0,0,20,20); */

	      /* carve from minval+ah */
	      bofs=ofs;
	      while (*(imr+bofs)>minval){ /* backtrack */
		if (*(imr+bofs)>minval+ah){/* carve */
		  *(imr+bofs)=minval+ah;
		}
		bofs-=shft[pdir[bofs]&FLRE];
	      }

/* 	      printf("before fill by ah\n"); */
/* 	      dumpxyz(&im[0],10,0,0,20,20); */
/* 	      dumpxyz(&im[1],10,0,0,20,20); */

	      /* fill by ah and reset all flooded minima (holds for embedded ones) */
	      level=ah+minval;
	      aq = create_fifo4(10);
	      fifo4_add(aq, (long int)(ofsk));
	      *(imr+ofsk)=level;
	      *(iml+ofsk)=0;
	      while ((index = fifo4_remove(aq))){
		for (ak=0; ak < nshft; ak++){
		  p2 = iml + index + shft[ak];
		  if (*p2 == 1){ /* reset minimum */
		    *p2=0;
		    *(imr+index+shft[ak])=level;
		    fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		  else if (*(imr+index+shft[ak])<level && (index+shft[ak]) != ofsi){
		    *(imr+index+shft[ak])=level;
		    fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		}
	      }
	      free_fifo4(aq);

/* 	      printf("after fill by ah\n"); */
/* 	      dumpxyz(&im[0],10,0,0,20,20); */
/* 	      dumpxyz(&im[1],10,0,0,20,20); */

	      /* reset ALL flooded and reached pixels (FLRE) */
	      aq = create_fifo4(10);
	      fifo4_add(aq, (long int)(ofsk));
	      *(pdir+ofsk)&=FLRE;
	      while ((index = fifo4_remove(aq))){
		for (ak=0; ak < nshft; ak++){
		  if (*(pdir+index+shft[ak]) > 8){ /* also resets FLOODED/REACHED PIXELS */
		    *(pdir+index+shft[ak])&=FLRE;
		    fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		}
	      }
	      free_fifo4(aq);

/* 	      dumpxyz(&im[0],10,0,0,20,20); */
/* 	      dumpxyz(&im[1],10,0,0,20,20); */

	    } /* if (*(iml+ofsk)==1)irrelevant minima reached */
	    
	    *(iml+ofsk)=*(iml+ofs);
	    
	  } 
	  else /* if (*imr+ofsk) <maxfl) */
	    *(iml+ofsk)=LABEL_MAX;
	} /* if (*(iml+ofsk) < 2) */
      } /* for k=0 */
    }
  }
    /* clear_fifo(pq); */

  for (i=0; i<maxfl; i++)
    clear_fifo(fah[i]);
    
  free(fah);
  free_pq(heap);

  printf("casea=%d\n caseae=%d\n caseb(pure hybrid)=%d\n casebe(embedded hybrid)=%d\n casec=%d\n casece=%d\n cased=%d\n casede=%d\n", casea, caseae, caseb, casebe, casec, casece, cased, casede);
  
  us_framebox(&(im[0]),box,0);
  return imdir;
}
#include "us_undef.h"
#undef LABEL_TYPE
#undef LABEL_MAX
#undef LABEL_MSB
#undef FLOODED
#undef REACHED
#undef FLRE



#include "u32_def.h"
#define LABEL_TYPE unsigned short
#define LABEL_MAX 0x7FFF /* 16383 */  /* was  0x7FFF */
#define LABEL_MSB 0x8000
#define FLOODED 0x80
#define REACHED 0x40
#define FLRE    0x0F  /* FLOODED|REACHED */
IMAGE *u32_fillocarve_energy(unsigned short *iml, UINT32 *imr, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE 20-11-2002 (1st)
  ** iml: pointer to an image of labels whith LABEL_MAX valued border
     (relevant minima)
  ** imr: pointer to a reference image
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */

  /*   char fname[50]; */
  int pcount=0;
  IMAGE im[2];
  int box[6];

  int casea=0, caseae=0, caseb=0, casebe=0, casec=0, casece=0, cased=0, casede=0; /* 2003-11-10 */
  int *eeb; /* 2003-11-12 for energy of embedded pit */

  int *ec, *ef, *af, dyn, iori=0;
  int h, priocrt=0, ah, level, emin;
  struct pqueue *heap;
  PQDATUM apqd[1];
  struct node *pqd;
  UCHAR *apdir, *apdirk;
  int flag=1, flage=0;

  long int index;

  long int k, ofs, ofsk, ofsi=0;
  LABEL_TYPE *pl;
  PIX_TYPE *pr;
  FIFO **fah, *pq;
  long int i, shftk, npix;

  IMAGE *imdir;
  UCHAR *pdir;
  FIFO4 *aq;
  LABEL_TYPE *p1, *p2;
  PIX_TYPE minval;
  long int ak, bofs, crtlevel;


  heap = (struct pqueue *)pqinit(NULL, 100);  /* heap (priority queue) */
  if (heap == NULL)
    return NULL;
  

  npix = (long int)nx*ny*nz;
  maxfl++;


  im[0].p_im = (char *)iml;
  im[0].DataType = t_USHORT;
  im[0].nx = nx;
  im[0].ny = ny;
  im[0].nz = nz;
  im[0].NByte = (mia_size_t)nx*ny*nz*2;
  im[0].vol = 0;
  im[0].lut = NULL;

  im[1].p_im = (char *)imr;
  im[1].DataType = t_UINT32;
  im[1].nx = nx;
  im[1].ny = ny;
  im[1].nz = nz;
  im[1].NByte = (mia_size_t)nx*ny*nz*4;
  im[1].vol = 0;
  im[1].lut = NULL;

  BOX_2D;


  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("fillocarve(): not enough memory for the FAH\n");
   return NULL;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    fah[i] = alloc_fifo(100);  /* WAS ((nx*ny*nz)/maxfl)/100+1 */
    if (fah[i]==NULL){
      (void) printf("fillocarve(): not enough memory for the FIFO\n");
      free(fah);
      return NULL;
    }
  }

  /* create an image for storing the flood directions */
  imdir = (IMAGE *)create_image(t_UCHAR, nx, ny, nz);
  if (imdir == NULL){
    (void)sprintf(buf,"fillocarve(): not enough memory!\n"); errputstr(buf);
    for (i=0; i<maxfl; i++)
      clear_fifo(fah[i]);
    free(fah);
    return(imdir);
  }
  pdir=(UCHAR *)GetImPtr(imdir);
 

  /* initialize the queues */
  for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
    if ((*pl>1)&&(*pl<LABEL_MAX)){
      for (k=0;k<nshft;k++){
	shftk=shft[k];
	if (*(pl+shftk)==0){
	  if (*(pr+shftk)<maxfl){
	    *(pl+shftk)=*pl|LABEL_MSB; 
	    fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	    pdir[i+shftk]=k;
	  }
	  else
	    *(pl+shftk)=LABEL_MAX;
	}
      }
    }
  }
   
  for (i=0,pl=iml; i<npix; i++,pl++)
    if (*pl>LABEL_MAX)
      *pl ^= LABEL_MSB;
  

  /* here we go */
  for (i=0; i<maxfl; i++){  // flood from relevant minima
    // printf("crt level =%ld\n", i);
    pq = fah[i];
    while (fifo_empty(pq) == FALSE){
      ofs=fifo_remove(pq);
      if (*(imr+ofs)!=i){
 	// printf("reinsert in queue: i=%d *(imr+ofs)=%d\n", i, *(imr+ofs)); 
        fifo_add(fah[*(imr+ofs)],(long int)ofs);
	continue;
      }
      for (k=0; k<nshft; ++k){
	ofsk=ofs+shft[k];
	if (*(iml+ofsk)<2){
	  if (ofsk==11566){
	    printf("non-positive dynamic, ofsk=%ld!!!\n", ofsk);
	    dumpxyz(&im[0],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
	    dumpxyz(&im[1],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
	    } 
	  if (*(imr+ofsk)<maxfl){
	    crtlevel=*(imr+ofsk);
	    // printf("test whether we go down, i=%d, crtlevel=%d\n", i, crtlevel);
	    if (i>crtlevel){ /* perhaps should simply follow steepest slope path */
	      /*     printf("We go down\n"); */
	      if (flag){
		iori=i;
		ofsi=ofs;
		/* 	printf("iori=%d\n", iori); */
		flag=0;
	      }
	      i=crtlevel;
	      pq=fah[crtlevel];
	    }
	    /* else if (i<crtlevel){ */
/* 	      printf("We go up\n"); */
/* 	      flag=1; */
/* 	    } */
	    fifo_add(fah[crtlevel],(long int)ofsk);
	    pdir[ofsk]=k;

	    if ( (*(iml+ofsk)==1) &&  ( (iori- *(imr+ofsk)) < 0) ){ // 20090817: a former irrelevant minimum is not a minimum anymore!
	      *(iml+ofsk)=0;
	      aq = create_fifo4(10);
	      fifo4_add(aq, (long int)(ofsk));
	      while ((index = fifo4_remove(aq))){
		for (ak=0; ak < nshft; ak++){
		  if ( *(iml+index+shft[ak])== *(imr+ofsk) ){
		    *(iml+index+shft[ak]) = 0;
	            fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		}
	      }
	      free_fifo4(aq);
	    }
	    if (*(iml+ofsk)==1){   /* ! should only occur when going down ! irrelevant minima reached */
	      flag=1;
	      flage=0;
	      /* printf("irrelevant n=minimum reached\n"); */
	      minval = *(imr+ofsk); /* elevation of minimum, i=elevation of highest point on carving path */
	      dyn = iori-minval; /* relative dynamic of reached minimum */
	      // printf("pit encountered, minval=%d, iori=%d, dyn=%d, crtlevel=%d, x=%d, y=%d, i=%d, ix=%d, iy=%d\n", minval, iori, dyn, crtlevel, ofsk-(int)(ofsk/nx)*nx, (int)(ofsk/nx), i, ofs-(int)(ofs/nx)*nx, (int)(ofs/nx)); 
	      if ( (ofsk==11566)  || (dyn<1) ){
		printf("non-positive dynamyic, ofsk=%ld!!!\n", ofsk);
		printf("iori=%d\t minval=%d\n", iori, minval);
		dumpxyz(&im[0],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,15,15);
		dumpxyz(&im[1],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,15,15);
		dumpxyz(imdir,ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,15,15);
	      }
	      pcount+=1;

	      
              /* sprintf(fname, "/home/pierre/tmp/rieslbl%03d.tif", pcount);
	      write_tiff(&im[0], fname);
              sprintf(fname, "/home/pierre/tmp/riesref%03d.tif", pcount);
              write_tiff(&im[1], fname); */

	      
	      if ( (ec = (int *)calloc(dyn+1, sizeof(int))) == NULL ){ /* energy of carving  */
		(void)sprintf(buf,"not enough memory for ec array (dyn=%d)\n", dyn); errputstr(buf);
		free_pq(heap);
		return NULL;
	      }
	      if ( (ef = (int *)calloc(dyn+1, sizeof(int))) == NULL){  /* energy of flooding */
		(void)sprintf(buf,"not enough memory for ef array (dyn=%d)\n", dyn); errputstr(buf);
		free(ec);
		free_pq(heap);
		return NULL;
	      }
	      if ( (af = (int *)calloc(dyn+1, sizeof(int))) == NULL){  /* flooded areas      */
		(void)sprintf(buf,"not enough memory for af array (dyn=%d)\n", dyn); errputstr(buf);
		free(ec);
		free(ef);
		free_pq(heap);
		return NULL;
	      }
	      if ( (eeb = (int *)calloc(dyn+1, sizeof(int))) == NULL ){ /* energy of carving  */
		(void)sprintf(buf,"not enough memory for ec array (dyn=%d)\n", dyn); errputstr(buf);
		free_pq(heap);
		free(ec);
		free(ef);
		free(af);
		return NULL;
	      }

	      ef[0]=0;

	      /* initialise heap with neighbours of minima
		 while calculating area of minimum */

	      while ( pqremove(heap, apqd) != NULL ){	 /* make sure heap is empty */
		free((char*) *apqd);
	      }
	

	      aq = create_fifo4(10);
	      af[0]=1;
	      *(pdir+ofsk)|=FLOODED;
	      fifo4_add(aq, (long int)(ofsk));
	      while ((index = fifo4_remove(aq))){
		p1=iml+index;
		apdir=pdir+index;
		for (ak=0; ak < nshft; ak++){
		  p2 = p1 + shft[ak];
		  apdirk=apdir+shft[ak];
		  if ( (*p2 == 1) && (*apdirk==0) ){ /* in minimum and not yet flooded */
		    af[0]+=1;
		    /* *p2 = FLOODED; */ *apdirk = FLOODED;
	            fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		  else if ( (*p2!=1) && (*apdirk<8)  &&  (*(imr+ (int) (apdirk-pdir))) < iori ){ /* if (*p2 == 0){ */ /* border of minimum and not yet REACHED */
		    /* printf("insert border of pit in heap\n"); */
		    pqd = (PQDATUM )malloc(sizeof(struct node));
/* 		    printf("*(imr+ (int) (apdirk-pdir))=%d\t", *(imr+ (int) (apdirk-pdir))); */
/* 		    printf("offset=%d\t", index+shft[ak]); */
/* 		    printf("prio=%d\n", (*(imr+ (int) (apdirk-pdir))-minval)); */
		    pqd->prio = PIX_RANGE-(*(imr+ (int) (apdirk-pdir))-minval);
		    /* pqd->prio = PIX_RANGE-( *(imr+index+shft[ak]) - minval ); */
		    /* pqd->val   = val; not used */
		    pqd->offset= (long int)(index+shft[ak]);
		    pqinsert(heap, pqd);
		    *apdirk |= REACHED;	    
		  }
		}
	      }
	      free_fifo4(aq);
	      ef[1]=af[0];

	      bofs=ofs; /* calculate ec[0] */
	      while (*(imr+bofs)>minval){ /* backtrack */
		if (*(imr+bofs)>minval)
		  ec[0]+= *(imr+bofs)-(minval);   /* PSH missing parenthesis */
		bofs-=shft[pdir[bofs]&FLRE];
	      }
	      emin=ec[0];
	      
	      /* look for procedure with lowest energy */
	      for (h=1,ah=1; h<dyn+1; h++,ah++){  /* (PSHwas dyn+1) modify so as to stop as soon as the energy of the sum increases */

		/* compute energy of carving   */
		bofs=ofs;
		while (*(imr+bofs)>minval){ /* backtrack */
		  if (*(imr+bofs)>minval+h)
		    ec[h]+= *(imr+bofs)-(minval+h);   /* PSH missing parenthesis */
		  bofs-=shft[pdir[bofs]&FLRE];
		}

		/* check whether total energy increases, and break if so */
		ef[h]=ef[h-1]+af[h-1]+eeb[h-1];

	
		if (ec[h]+ef[h] > emin){ /* minimum energy reached */
		  if ( ec[0]==emin )
		    if (flage == 0)
		      casea++;  /* pure plain carving */
		    else{
		      caseae++; /* embedded  carving (should not occur) */
		      printf("ec[0]=%d\t ec[1]=%d\n", ec[0], ec[1]+ef[1]);
		    }
		  else
		    if (flage == 0)
		      caseb++;  /* pure hybrid approach */
		    else
		      casebe++; /* embedded hybrid */
		  break;
		}
		emin=ec[h]+ef[h];
		
		/* compute energy of fillhole  */
		if ( pqpeek(heap, apqd) == NULL )
		  priocrt=h;
		while ( pqpeek(heap, apqd) != NULL ){ /* it may be empty !!! */
		  priocrt=PIX_RANGE-(*apqd)->prio;
/* 		  printf("h=%d\t priocrt=%d\n", h, priocrt); */
		  if (priocrt > h) /* no pixel to flood at the current level */
		    break; /* while loop */
		  af[h]+= 1; /* only for area part of mewly flooded pixel (downward for embedded) */
		  if (priocrt < h){ /* embedded pits */
		    eeb[h]+=minval+h-*(imr+ (int)(*apqd)->offset);
		    flage=1;
		  }
/* 		  printf("minval=%d\t h=%d\t af[h]=%d\t eeb[%d]=%d\n", minval, h, minval+h+1-*(imr+ (int)(*apqd)->offset ), h, eeb[h]); */
		  pqremove(heap, apqd);
		  apdir=pdir+(*apqd)->offset;
                  free((char*) *apqd);
		  *apdir|=FLOODED;
		  /* printf("af[%d]=%d\n",h,af[h]); */
		  for (ak=0; ak < nshft; ak++){
		    apdirk=apdir+shft[ak];
		    if ( (*apdirk<8) && (*(iml+(apdirk-pdir))!=LABEL_MAX) &&  (*(imr+ (int) (apdirk-pdir))) < iori){ /* not yet flooded */
		      pqd = (PQDATUM )malloc(sizeof(struct node));
		      pqd->prio = PIX_RANGE-(*(imr+ (int) (apdirk-pdir))-minval);

/* 		      printf("*(imr+ (int) (apdirk-pdir))=%d\t", *(imr+ (int) (apdirk-pdir))); */
/* 		      printf("offset=%d\t", apdirk-pdir); */
/* 		      printf("prio=%d\n", (*(imr+ (int) (apdirk-pdir))-minval)); */

		      /* pqd->val   = val; not used */
		      pqd->offset= (long int)(apdirk-pdir);
		      pqinsert(heap, pqd);
		      *apdirk|=REACHED;
		    }
		  }
		}
		af[h]+=af[h-1];

/* 		printf("iori=%d\t minval=%d\t dyn=%d\t priocrt=%d\t h=%d\n", iori, minval, dyn, priocrt, h); */

		/* if (ofsk==35400){
		  printf("non-positive dynamyic, ofsk=%ld!!!\n", ofsk);
		  dumpxyz(&im[0],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
		  dumpxyz(&im[1],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
		  } */
	      } /* for (h=0; h<dyn+1; h++) */

	      ah=h;

	      /* detect whether plain fillhole or degenerated case */
	      if ( ah==dyn+1){
		if (emin==ec[0]){
		  if (flage)
		     casede++; /* degenerated case */
		  else
		    cased++;
		  /* printf("degenerated case, ofsk=%d\t ah=%d\n", ofsk, ah); */
		}
		else
		  if (flage)
		    casece++;
		  else
		    casec++; /* plain pit filling */
	      }

	      
	      ah-=1;  /* optimum level */

	      free(ec); free(af); free(ef); free(eeb);
	      while ( pqremove(heap, apqd) != NULL ){	 /* make sure heap is emptied */
		*(pdir+(*apqd)->offset)&=FLRE; /* reset */
		free((char*) *apqd);
	      }

/* 	      dumpxyz(&im[1],10,0,0,20,20); */

	      /* carve from minval+ah */
	      bofs=ofs;
	      while (*(imr+bofs)>minval){ /* backtrack */
		if (*(imr+bofs)>minval+ah){/* carve */
		  *(imr+bofs)=minval+ah;
		}
		bofs-=shft[pdir[bofs]&FLRE];
	      }

/* 	      printf("before fill by ah\n"); */
/* 	      dumpxyz(&im[0],10,0,0,20,20); */
/* 	      dumpxyz(&im[1],10,0,0,20,20); */

	      /* fill by ah and reset all flooded minima (holds for embedded ones) */
	      level=ah+minval;
	      aq = create_fifo4(10);
	      fifo4_add(aq, (long int)(ofsk));
	      *(imr+ofsk)=level;
	      *(iml+ofsk)=0;
	      while ((index = fifo4_remove(aq))){
		for (ak=0; ak < nshft; ak++){
		  p2 = iml + index + shft[ak];
		  if (*p2 == 1){ /* reset minimum */
		    *p2=0;
		    *(imr+index+shft[ak])=level;
		    fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		  else if (*(imr+index+shft[ak])<level && (index+shft[ak]) != ofsi){
		    *(imr+index+shft[ak])=level;
		    fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		}
	      }
	      free_fifo4(aq);

/* 	      printf("after fill by ah\n"); */
/* 	      dumpxyz(&im[0],10,0,0,20,20); */
/* 	      dumpxyz(&im[1],10,0,0,20,20); */

	      /* reset ALL flooded and reached pixels (FLRE) */
	      aq = create_fifo4(10);
	      fifo4_add(aq, (long int)(ofsk));
	      *(pdir+ofsk)&=FLRE;
	      while ((index = fifo4_remove(aq))){
		for (ak=0; ak < nshft; ak++){
		  if (*(pdir+index+shft[ak]) > 8){ /* also resets FLOODED/REACHED PIXELS */
		    *(pdir+index+shft[ak])&=FLRE;
		    fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		}
	      }
	      free_fifo4(aq);

/* 	      dumpxyz(&im[0],10,0,0,20,20); */
/* 	      dumpxyz(&im[1],10,0,0,20,20); */

	    } /* if (*(iml+ofsk)==1)irrelevant minima reached */
	    
	    *(iml+ofsk)=*(iml+ofs);
	    
	  } 
	  else /* if (*imr+ofsk) <maxfl) */
	    *(iml+ofsk)=LABEL_MAX;
	} /* if (*(iml+ofsk) < 2) */
      } /* for k=0 */
    }
  }
    /* clear_fifo(pq); */

  for (i=0; i<maxfl; i++)
    clear_fifo(fah[i]);
    
  free(fah);
  free_pq(heap);

  printf("casea=%d\n caseae=%d\n caseb(pure hybrid)=%d\n casebe(embedded hybrid)=%d\n casec=%d\n casece=%d\n cased=%d\n casede=%d\n", casea, caseae, caseb, casebe, casec, casece, cased, casede);
  
  us_framebox(&(im[0]),box,0);
  return imdir;
}
#include "u32_undef.h"
#undef LABEL_TYPE
#undef LABEL_MAX
#undef LABEL_MSB
#undef FLOODED
#undef REACHED
#undef FLRE


#include "us_def.h"
#define LABEL_TYPE unsigned short
#define LABEL_MAX 0x7FFF /* 16383 */  /* was  0x7FFF */
#define LABEL_MSB 0x8000
#define FLOODED 0x80
#define REACHED 0x40
#define FLRE    0x0F  /* FLOODED|REACHED */
IMAGE *us_fillocarve_area(unsigned short *iml, unsigned short *imr, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE
  ** iml: pointer to an image of labels whith LABEL_MAX valued border
         (relevant minima)
  ** imr: pointer to a reference image
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */

  /* char fname[50];*/
  int pcount=0;
  IMAGE im[2];
  int box[6];

  int casea=0, caseb=0, casec=0, cased=0; /* 2003-11-10 */

  int hmin; 

  int *ec, *ef, *af, dyn, iori=0;
  int h, priocrt=0, ah, level, emin;
  struct pqueue *heap;
  PQDATUM apqd[1];
  struct node *pqd;
  UCHAR *apdir, *apdirk;
  int flag=1;

  long int index;

  long int k, ofs, ofsk, ofsi=0;
  LABEL_TYPE *pl;
  PIX_TYPE *pr;
  FIFO **fah, *pq;
  long int i, shftk, npix;

  IMAGE *imdir;
  UCHAR *pdir;
  FIFO4 *aq;
  LABEL_TYPE *p1, *p2;
  PIX_TYPE minval;
  long int ak, bofs, crtlevel;


  heap = (struct pqueue *)pqinit(NULL, 100);  /* heap (priority queue) */
  if (heap == NULL)
    return NULL;

  npix = (long int)nx*ny*nz;
  maxfl++;

  im[0].p_im = (char *)iml;
  im[0].DataType = 5;
  im[0].nx = nx;
  im[0].ny = ny;
  im[0].nz = nz;
  im[0].NByte = (mia_size_t)nx*ny*nz*2;
  im[0].vol = 0;
  im[0].lut = NULL;

  im[1].p_im = (char *)imr;
  im[1].DataType = 5;
  im[1].nx = nx;
  im[1].ny = ny;
  im[1].nz = nz;
  im[1].NByte = (mia_size_t)nx*ny*nz*2;
  im[1].vol = 0;
  im[1].lut = NULL;

  BOX_2D;


  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("fillocarve(): not enough memory for the FAH\n");
   return NULL;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    printf("crt level =%ld\n", i);
    fah[i] = alloc_fifo(100);  /* WAS ((nx*ny*nz)/maxfl)/100+1 */
    if (fah[i]==NULL){
      (void) printf("fillocarve(): not enough memory for the FIFO\n");
      free(fah);
      return NULL;
    }
  }

  /* create an image for storing the flood directions */
  imdir = (IMAGE *)create_image(t_UCHAR, nx, ny, nz);
  if (imdir == NULL){
    (void)sprintf(buf,"fillocarve(): not enough memory!\n"); errputstr(buf);
    for (i=0; i<maxfl; i++)
      clear_fifo(fah[i]);
    free(fah);
    return(imdir);
  }
  pdir=(UCHAR *)GetImPtr(imdir);
 

  /* initialize the queues */
  for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
    if ((*pl>1)&&(*pl<LABEL_MAX)){
      for (k=0;k<nshft;k++){
	shftk=shft[k];
	if (*(pl+shftk)==0){
	  if (*(pr+shftk)<maxfl){
	    *(pl+shftk)=*pl|LABEL_MSB; 
	    fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	    pdir[i+shftk]=k;
	  }
	  else
	    *(pl+shftk)=LABEL_MAX;
	}
      }
    }
  }
   
  for (i=0,pl=iml; i<npix; i++,pl++)
    if (*pl>LABEL_MAX)
      *pl ^= LABEL_MSB;
  
  /* here we go */
  for (i=0; i<maxfl; i++){
    /* printf("crt level =%ld\n", i); */
    pq = fah[i];
    while (fifo_empty(pq) == FALSE){
      ofs=fifo_remove(pq);
      if (*(imr+ofs)!=i){
/* 	printf("reinsert in queue: i=%d *(imr+ofs)=%d\n", i, *(imr+ofs)); */
        fifo_add(fah[*(imr+ofs)],(long int)ofs);
	continue;
      }
      for (k=0; k<nshft; ++k){
	ofsk=ofs+shft[k];
	if (*(iml+ofsk)<2){
	  /* if (ofsk==16054){
	    printf("non-positive dynamyic, ofsk=%ld!!!\n", ofsk);
	    dumpxyz(&im[0],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
	    dumpxyz(&im[1],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
	    } */
	  if (*(imr+ofsk)<maxfl){
	    crtlevel=*(imr+ofsk);
	    /*    printf("test whether we go down, i=%d, crtlevel=%d\n", i, crtlevel); */
	    if (i>crtlevel){ /* perhaps should simply follow steepest slope path */
	      /*     printf("We go down\n"); */
	      if (flag){
		iori=i;
		ofsi=ofs;
		/* 	printf("iori=%d\n", iori); */
		flag=0;
	      }
	      i=crtlevel;
	      pq=fah[crtlevel];
	    }
	    /* else if (i<crtlevel){ */
/* 	      printf("We go up\n"); */
/* 	      flag=1; */
/* 	    } */
	    fifo_add(fah[crtlevel],(long int)ofsk);
	    pdir[ofsk]=k;

	    if ( (*(iml+ofsk)==1) &&  ( (iori- *(imr+ofsk)) < 0) ){ // 20090817: a former irrelevant minimum is not a minimum anymore!
	      *(iml+ofsk)=0;
	      aq = create_fifo4(10);
	      fifo4_add(aq, (long int)(ofsk));
	      while ((index = fifo4_remove(aq))){
		for (ak=0; ak < nshft; ak++){
		  if ( *(iml+index+shft[ak])== *(imr+ofsk) ){
		    *(iml+index+shft[ak]) = 0;
	            fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		}
	      }
	      free_fifo4(aq);
	    }

	    if (*(iml+ofsk)==1){   /* ! should only occur when going down ! irrelevant minima reached */
	      flag=1;
	      /* printf("irrelevant n=minimum reached\n"); */
	      minval = *(imr+ofsk); /* elevation of minimum, i=elevation of highest point on carving path */
	      dyn = iori-minval; /* relative dynamic of reached minimum */
	     /*  printf("pit encountered, minval=%d, iori=%d, dyn=%d, crtlevel=%d, x=%d, y=%d\n", minval, iori, dyn, crtlevel, ofsk-(int)(ofsk/nx)*nx, (int)(ofsk/nx)); */
	      if ( (ofsk==110473568)  || (dyn<1) ){
		printf("non-positive dynamyic, ofsk=%ld!!!\n", ofsk);
		printf("iori=%d\t minval=%d\n", iori, minval);
		dumpxyz(&im[0],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
		dumpxyz(&im[1],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
	      }
	      pcount+=1;

	      
              /* sprintf(fname, "/home/pierre/tmp/rieslbl%03d.tif", pcount);
	      write_tiff(&im[0], fname);
              sprintf(fname, "/home/pierre/tmp/riesref%03d.tif", pcount);
              write_tiff(&im[1], fname); */

	      
	      if ( (ec = (int *)calloc(dyn+1, sizeof(int))) == NULL ){ /* energy of carving  */
		(void)sprintf(buf,"not enough memory for ec array (dyn=%d)\n", dyn); errputstr(buf);
		free_pq(heap);
		return NULL;
	      }
	      if ( (ef = (int *)calloc(dyn+1, sizeof(int))) == NULL){  /* energy of flooding */
		(void)sprintf(buf,"not enough memory for ef array (dyn=%d)\n", dyn); errputstr(buf);
		free(ec);
		free_pq(heap);
		return NULL;
	      }
	      if ( (af = (int *)calloc(dyn+1, sizeof(int))) == NULL){  /* flooded areas      */
		(void)sprintf(buf,"not enough memory for af array (dyn=%d)\n", dyn); errputstr(buf);
		free(ec);
		free(ef);
		free_pq(heap);
		return NULL;
	      }
	      ef[0]=0;

	      /* initialise heap with neighbours of minima
		 while calculating area of minimum */

	      while ( pqremove(heap, apqd) != NULL ){	 /* make sure heap is empty */
		free((char*) *apqd);
	      }
	

	      aq = create_fifo4(10);
	      af[0]=1;
	      *(pdir+ofsk)|=FLOODED;
	      fifo4_add(aq, (long int)(ofsk));
	      while ((index = fifo4_remove(aq))){
		p1=iml+index;
		apdir=pdir+index;
		for (ak=0; ak < nshft; ak++){
		  p2 = p1 + shft[ak];
		  apdirk=apdir+shft[ak];
		  if ( (*p2 == 1) && (*apdirk==0) ){ /* in minimum and not yet flooded */
		    af[0]+=1;
		    /* *p2 = FLOODED; */ *apdirk = FLOODED;
	            fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		  else if ( (*p2!=1) && (*apdirk<8)  &&  (*(imr+ (int) (apdirk-pdir))) < iori ){ /* if (*p2 == 0){ */ /* border of minimum and not yet REACHED */
		    /* printf("insert border of pit in heap\n"); */
		    pqd = (PQDATUM )malloc(sizeof(struct node));
/* 		    printf("*(imr+ (int) (apdirk-pdir))=%d\t", *(imr+ (int) (apdirk-pdir))); */
/* 		    printf("offset=%d\t", index+shft[ak]); */
/* 		    printf("prio=%d\n", (*(imr+ (int) (apdirk-pdir))-minval)); */
		    pqd->prio = PIX_RANGE-(*(imr+ (int) (apdirk-pdir))-minval);
		    /* pqd->prio = PIX_RANGE-( *(imr+index+shft[ak]) - minval ); */
		    /* pqd->val   = val; not used */
		    pqd->offset= (long int)(index+shft[ak]);
		    pqinsert(heap, pqd);
		    *apdirk |= REACHED;	    
		  }
		}
	      }
	      free_fifo4(aq);
	      ef[1]=af[0];

	      bofs=ofs; /* calculate ec[0] */
	      while (*(imr+bofs)>minval){ /* backtrack */
		if (*(imr+bofs)>minval)
		  ec[0]+= 1;    /* *(imr+bofs)-(minval); */  /* PSH missing parenthesis */
		bofs-=shft[pdir[bofs]&FLRE];
	      }
	      emin=ec[0];
	      hmin=0;
	      
	      /* look for procedure with lowest energy */
	      for (h=1,ah=1; h<dyn+1; h++,ah++){  /* (PSHwas dyn+1) modify so as to stop as soon as the energy of the sum increases */

		/* compute energy of carving   */
		bofs=ofs;
		while (*(imr+bofs)>minval){ /* backtrack */
		  if (*(imr+bofs)>minval+h)
		    ec[h]+= 1; /* *(imr+bofs)-(minval+h);  */ /* PSH missing parenthesis */
		  bofs-=shft[pdir[bofs]&FLRE];
		}

		/* check whether total energy decreases*/
		ef[h]=af[h-1];    /* 	ef[h]=ef[h-1]+af[h-1]+eeb[h-1]; */
		if (ec[h]+ef[h] <= emin){
		  emin=ec[h]+ef[h];
		  hmin=h;
		}
		
		/* compute energy of fillhole  */
		if ( pqpeek(heap, apqd) == NULL )
		  priocrt=h;
		while ( pqpeek(heap, apqd) != NULL ){ /* it may be empty !!! */
		  priocrt=PIX_RANGE-(*apqd)->prio;
/* 		  printf("h=%d\t priocrt=%d\n", h, priocrt); */
		  if (priocrt > h) /* no pixel to flood at the current level */
		    break; /* while loop */
		  af[h]+= 1; /* only for area part of mewly flooded pixel (downward for embedded) */
/* 		  printf("minval=%d\t h=%d\t af[h]=%d\t eeb[%d]=%d\n", minval, h, minval+h+1-*(imr+ (int)(*apqd)->offset ), h, eeb[h]); */
		  pqremove(heap, apqd);
		  apdir=pdir+(*apqd)->offset;
                  free((char*) *apqd);
		  *apdir|=FLOODED;
		  /* printf("af[%d]=%d\n",h,af[h]); */
		  for (ak=0; ak < nshft; ak++){
		    apdirk=apdir+shft[ak];
		    if ( (*apdirk<8) && (*(iml+(apdirk-pdir))!=LABEL_MAX) &&  (*(imr+ (int) (apdirk-pdir))) < iori){ /* not yet flooded */
		      pqd = (PQDATUM )malloc(sizeof(struct node));
		      pqd->prio = PIX_RANGE-(*(imr+ (int) (apdirk-pdir))-minval);

/* 		      printf("*(imr+ (int) (apdirk-pdir))=%d\t", *(imr+ (int) (apdirk-pdir))); */
/* 		      printf("offset=%d\t", apdirk-pdir); */
/* 		      printf("prio=%d\n", (*(imr+ (int) (apdirk-pdir))-minval)); */

		      /* pqd->val   = val; not used */
		      pqd->offset= (long int)(apdirk-pdir);
		      pqinsert(heap, pqd);
		      *apdirk|=REACHED;
		    }
		  }
		}
		af[h]+=af[h-1];

/* 		printf("iori=%d\t minval=%d\t dyn=%d\t priocrt=%d\t h=%d\n", iori, minval, dyn, priocrt, h); */

		/* if (ofsk==35400){
		  printf("non-positive dynamyic, ofsk=%ld!!!\n", ofsk);
		  dumpxyz(&im[0],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
		  dumpxyz(&im[1],ofsk-(int)(ofsk/nx)*nx,(int)(ofsk/nx),0,10,10);
		  } */
	      } /* for (h=0; h<dyn+1; h++) */

	      ah=hmin; /* optimum level */
	      /* detect which  case */
	      if (hmin==0)
		casea++;
	      else if (hmin<dyn)
		caseb++;
	      else if ( (hmin==dyn) && (ec[0]==emin))
		cased++;
	      else
		casec++;

	      free(ec); free(af); free(ef);
	      while ( pqremove(heap, apqd) != NULL ){	 /* make sure heap is emptied */
		*(pdir+(*apqd)->offset)&=FLRE; /* reset */
		free((char*) *apqd);
	      }

/* 	      dumpxyz(&im[1],10,0,0,20,20); */

	      /* carve from minval+ah */
	      bofs=ofs;
	      while (*(imr+bofs)>minval){ /* backtrack */
		if (*(imr+bofs)>minval+ah){/* carve */
		  *(imr+bofs)=minval+ah;
		}
		bofs-=shft[pdir[bofs]&FLRE];
	      }

/* 	      printf("before fill by ah\n"); */
/* 	      dumpxyz(&im[0],10,0,0,20,20); */
/* 	      dumpxyz(&im[1],10,0,0,20,20); */

	      /* fill by ah and reset all flooded minima (holds for embedded ones) */
	      level=ah+minval;
	      aq = create_fifo4(10);
	      fifo4_add(aq, (long int)(ofsk));
	      *(imr+ofsk)=level;
	      *(iml+ofsk)=0;
	      while ((index = fifo4_remove(aq))){
		for (ak=0; ak < nshft; ak++){
		  p2 = iml + index + shft[ak];
		  if (*p2 == 1){ /* reset minimum */
		    *p2=0;
		    *(imr+index+shft[ak])=level;
		    fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		  else if (*(imr+index+shft[ak])<level && (index+shft[ak]) != ofsi){
		    *(imr+index+shft[ak])=level;
		    fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		}
	      }
	      free_fifo4(aq);

/* 	      printf("after fill by ah\n"); */
/* 	      dumpxyz(&im[0],10,0,0,20,20); */
/* 	      dumpxyz(&im[1],10,0,0,20,20); */

	      /* reset ALL flooded and reached pixels (FLRE) */
	      aq = create_fifo4(10);
	      fifo4_add(aq, (long int)(ofsk));
	      *(pdir+ofsk)&=FLRE;
	      while ((index = fifo4_remove(aq))){
		for (ak=0; ak < nshft; ak++){
		  if (*(pdir+index+shft[ak]) > 8){ /* also resets FLOODED/REACHED PIXELS */
		    *(pdir+index+shft[ak])&=FLRE;
		    fifo4_add(aq, (long int)(index+shft[ak]));
		  }
		}
	      }
	      free_fifo4(aq);

/* 	      dumpxyz(&im[0],10,0,0,20,20); */
/* 	      dumpxyz(&im[1],10,0,0,20,20); */

	    } /* if (*(iml+ofsk)==1)irrelevant minima reached */
	    
	    *(iml+ofsk)=*(iml+ofs);
	    
	  } 
	  else /* if (*imr+ofsk) <maxfl) */
	    *(iml+ofsk)=LABEL_MAX;
	} /* if (*(iml+ofsk) < 2) */
      } /* for k=0 */
    }
  }
    /* clear_fifo(pq); */

  for (i=0; i<maxfl; i++)
    clear_fifo(fah[i]);
    
  free(fah);
  free_pq(heap);

  printf("casea=%d\n caseb1(pure hybrid)=%d\n casec=%d\n cased=%d\n", casea, caseb, casec, cased);
  
  us_framebox(&(im[0]),box,0);
  return imdir;
}
#include "us_undef.h"
#undef LABEL_TYPE
#undef LABEL_MAX
#undef LABEL_MSB
#undef FLOODED
#undef REACHED
#undef FLRE



IMAGE *fillocarve(IMAGE *iml, IMAGE *imr, int graph, int maxfl, int flag)
{
  /*
  ** author: Pierre SOILLE
  ** iml: image of labels with a border = LABEL_MAX
  ** imr: reference image with specific values for border and
          relevant/irrelevant minima
  ** graph: connectivity
  ** maxfl: maximum immersion level
  ** flag: 0->energy, area otherwise
  **
  ** WARNING: irrelevant minima must have all an intensity greater than
              that of the lowest minimum!
  */

  long int shft[27];
  int box[6]={1,1,1,1,0,0};

  set_seq_shift(GetImNx(iml), GetImNy(iml), GetImNz(iml), graph, shft);

  /* here we go */
  switch (GetImDataType(imr)){
  case t_USHORT:
    switch (GetImDataType(iml)){
       case t_USHORT:
	 us_framebox(imr,box,0);
	 us_framebox(iml,box,USHORT_MAX);
	 if (flag==0)
	   return(us_fillocarve_energy((unsigned short *)(GetImPtr(iml)),(unsigned short*)(GetImPtr(imr)), GetImNx(iml), GetImNy(iml), GetImNz(iml), shft, graph, maxfl));
	 else
	   return(us_fillocarve_area((unsigned short *)(GetImPtr(iml)),(unsigned short*)(GetImPtr(imr)), GetImNx(iml), GetImNy(iml), GetImNz(iml), shft, graph, maxfl));
       break;
       default:
    (void)sprintf(buf, "ERROR in fillocarve(): \
                invalid ImDataType for LABEL image\n"); errputstr(buf);
    return(NULL);
    }
  case t_UINT32:
    switch (GetImDataType(iml)){
       case t_USHORT:
	 if (flag==1){
	   (void)sprintf(buf, "ERROR in fillocarve(): \
                invalid ImDataType for grey level image\n"); errputstr(buf);
	   return(NULL);
	 }
	 u32_framebox(imr,box,0);
	 us_framebox(iml,box,USHORT_MAX);
	 return(u32_fillocarve_energy((unsigned short *)(GetImPtr(iml)),(UINT32 *)(GetImPtr(imr)), GetImNx(iml), GetImNy(iml), GetImNz(iml), shft, graph, maxfl));
       break;
       default:
    (void)sprintf(buf, "ERROR in fillocarve(): \
                invalid ImDataType for LABEL image\n"); errputstr(buf);
    return(NULL);
    }
  default:
    (void)sprintf(buf, "ERROR in fillocarve(): \
                invalid ImDataType for grey level image\n"); errputstr(buf);
    return(NULL);
  
  }
}


/*@}*/
