/**
 * @file   oiht.c
 * @author Pierre SOILLE <soillpi@D01RI1600821>
 * @date   Wed Aug 10 14:03:41 2016
 * 
 * @brief  
 * 
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "miallib.h"


/** \addtogroup group_hmtsk
 *  @{
 */


#define OIHTLABEL_TYPE int

extern void free_image(IMAGE *);
extern IMAGE *copy_image(IMAGE *);
extern ERROR_TYPE rdil(IMAGE *, IMAGE *, int, int);
extern ERROR_TYPE generic_blank(IMAGE *, UCHAR );
extern ERROR_TYPE i32_label(IMAGE *, IMAGE *, int, int, int );
extern IMAGE *to_int32(IMAGE *);
extern ERROR_TYPE szcompat(IMAGE *, IMAGE *);
extern ERROR_TYPE generic_addframebox(IMAGE *, int *, UCHAR);
extern ERROR_TYPE us_addframebox(IMAGE *, int *, USHORT);
extern ERROR_TYPE subframebox(IMAGE *, int *);
extern G_TYPE *min_max(IMAGE *);


#include "uc_def.h"
int ronsetest(IMAGE *pm1, IMAGE *pmsimple, int pix1, IMAGE *pmlabel, int* neighbours)
     /* remove all simple pixels as long as potentially new one appears */
     /* the anchor image indicates the pixels which cannot be removed */
{
  int label=0;
  int i,j;
  IMAGE *seed=copy_image(pm1);
  
  PIX_TYPE *ppm1=(PIX_TYPE *)GetImPtr(pm1);
  PIX_TYPE *pseed=(PIX_TYPE *)GetImPtr(seed);
  PIX_TYPE *ppmsimple=(PIX_TYPE *)GetImPtr(pmsimple);
  OIHTLABEL_TYPE *ppmlabel=(OIHTLABEL_TYPE *)GetImPtr(pmlabel);

  generic_blank(seed, 0);
  
  for(i=0;i<4;i++)
    if(ppm1[pix1+neighbours[i]] == 0 )
      label = ppmlabel[pix1+neighbours[i]];
  pseed[pix1]=1;
  rdil(seed,pm1, 8, 1); /* CC(pm2,pix1,8);*/
  pseed=(PIX_TYPE *)GetImPtr(seed); /* pseed redefined in rdil !!! */
  ppm1=(PIX_TYPE *)GetImPtr(pm1); /* ppm1 redefined in rdil !!! */
  for( i=0; i<GetImNPix(pm1); i++ ){
      if( pseed[i] != 0 ){
      if (ppmsimple[i] == 0){ /* a pixel of CC_8^1(P) is not simple */
	free_image(seed);
	return 0;
      }
      else{ /* test 8-deletability */
	for( j=0;j<4;j++){
	  if( ( ppm1[i+neighbours[j]] == 0 )
	      && ( ppmlabel[i + neighbours[j]] != label) ){
	    free_image(seed);
	    return 0;
	  }
	}
      }
    }
  }
  free_image(seed);
  return 1;  /* all pixels of the CC are simple pixels */
}
#include "uc_undef.h"


#include "uc_def.h"
int allsimplenb(IMAGE *pm1, IMAGE *pmsimple, int pix1, int *neighbours, int pixnb)
{
  int i,j;
  int current;
  int common;
  
  PIX_TYPE *ppm1=(PIX_TYPE *)GetImPtr(pm1);
  /* PIX_TYPE *ppmsimple=(PIX_TYPE *)GetImPtr(pmsimple); */

  for(i=0; i<8; i++){
    current = pix1 + neighbours[i];
    if ((current == pixnb) || (ppm1[current] == 0))
      continue;
    for(j=0,common=0; j<8; j++){
      if (current == pixnb + neighbours[j]){
	common=1;
	break;
	/* if ( (ppm1[current] != 0 ) &&
	     (ppmsimple[current] == 0 ) )
	     printf(" ce n'est pas possible POTFERDAM !@!\n"); */
      }
    }
    if (!common)
      return 1; /* no common neighbour */
  }
  return 0;
}
#include "uc_undef.h"


#include "uc_def.h"
int strictindependent(IMAGE *pm1, IMAGE *pmsimple,int pix1,int pixnb,int placenb, int* neighbours)
{
  int i,j;
  int current;

  PIX_TYPE *ppm1=(PIX_TYPE *)GetImPtr(pm1);
  PIX_TYPE *ppmsimple=(PIX_TYPE *)GetImPtr(pmsimple);

  for( i=0; i<8; i++ ){
    current = pix1 + neighbours[i];
    for(j=0;j<8;j++){
      if ( current == pixnb + neighbours[j])
	if ((ppm1[current] != 0) && (ppmsimple[current] == 0))
	  return 1;
    }
  }
  return 0;  
}
#include "uc_undef.h"

#include "uc_def.h"
int oldindependent(IMAGE *pm1,int pix1,int pixnb,int placenb, int* neighbours,IMAGE *pm55,int* neighbours55)
{
  int i,j;
  int current;
  int res = 1;
  int centre55 = 12;
  int find1=0;
  int find2=0;

  IMAGE *pmc55;

  PIX_TYPE *ppm1=(PIX_TYPE *)GetImPtr(pm1);
  PIX_TYPE *ppm55=(PIX_TYPE *)GetImPtr(pm55);
  PIX_TYPE *ppmc55;

  
  for(i=0;i<24;i++)
    ppm55[i]=0;
  
  /* check whether there is another foreground pixel within
     the intersection of their 8-eighbourhood */
  for( i=0; i<8; i++ ){
    current = pix1 + neighbours[i];
    for(j=0;j<8;j++){
      if ( current == pixnb + neighbours[j]){
	if (ppm1[current] != 0 )
	  find1=1;
	else /* prepare for next step */
	  ppm55[centre55 + neighbours55[i]]=1;	
      }
    }
  }
  if (find1 !=1) /* not indepedent */
    return 0;

  /* when $P$ and $Q$ are 4-connected, there is a $4^0$-path
     between them within the intersection of their 8-neighbourhood. */
  if( placenb < 4){
    find2 = 0;
    ppm55[centre55]=1;
    pmc55=(IMAGE *)create_image(3, 5, 5, 1);
    ppmc55=(PIX_TYPE *)GetImPtr(pmc55);
    ppmc55[centre55]=1;
    rdil(pmc55, pm55, 4, 1); /* CC(pm55,centre55,4); */
    ppmc55=(PIX_TYPE *)GetImPtr(pmc55);
    ppm55=(PIX_TYPE *)GetImPtr(pm55);
    ppmc55[centre55]=0;
    for( i=0;i<4;i++){
      current = centre55+neighbours55[placenb]+neighbours55[i];
      res = ppmc55[current];
      if(res !=0)
	find2=1;
    }
    free_image(pmc55);
  }
  else
    return find1;
  
  return(find2);
  
}
#include "uc_undef.h"

#include "uc_def.h"
int independent(IMAGE *pm1, int pix1, int pixnb, int placenb, int *neighbours, IMAGE *pm55, int* neighbours55)
{
  int i,j;
  int current;
  PIX_TYPE *ppm1=(PIX_TYPE *)GetImPtr(pm1);
  
  /* find whether there is another foreground pixel within
     the intersection of their 8-neighbourhood */
  for(i=0; i<8; i++){
    current = pix1 + neighbours[i];
    for(j=0; j<8; j++){
      if ( current == pixnb + neighbours[j]){
	if (ppm1[current])
	  goto found; /* break both loops simultaneously */
      }
    }
  }
  return 0;
  
  found:
  
  /* when $P$ and $Q$ are 4-connected, find whether there is a $4^0$-path
     between them within the intersection of their 8-neighbourhood. */
  if(placenb < 4){
    if ((placenb==0) || (placenb==1)){ /* horizontal neighbours */
      if (!ppm1[pix1+neighbours[2]])
	if (!ppm1[pixnb+neighbours[2]])
	  return 1;
      if (!ppm1[pix1+neighbours[3]])
	if (!ppm1[pixnb+neighbours[3]])
	  return 1;
      return 0;
    }
    else{                             /* vertical neighbours */
      if (!ppm1[pix1+neighbours[0]])
	if (!ppm1[pixnb+neighbours[0]])
	  return 1;
      if (!ppm1[pix1+neighbours[1]])
	if (!ppm1[pixnb+neighbours[1]])
	  return 1;
      return 0;
    }
    /* we should never reach this comment */
  }
  
  return 1;
}
#include "uc_undef.h"


#include "g_def.h"
ERROR_TYPE generic_oiskeleton(IMAGE *pm1NG, IMAGE *imanchor)
{
  int *tobetested=NULL;
  int *tab[2];
  int ind[2];
  int use;
  int neighbours[9];
  int neighbours55[9];
  int i,j;
  int pix1;
  int pixnb;
  int avirer;
  int nbvirer;
  int loop;
  int nbvirerT;
  int ng;
  int minNG;

  int nx, code;
  int box[6];

  /* table of homotopic configurations generated automatically once for all */
  int homotab[256] = { 0,1,1,0,1,1,1,1,1,1,1,1,0,1,1,0,1,1,0,0,1,1,1,1,0,1,0,1,0,1,1,0,1,\
                       1,0,0,0,1,0,1,1,1,1,1,0,1,1,0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,1,0,1,0,\
                       1,0,1,1,1,1,0,0,1,1,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,1,0,1,1,0,0,0,0,\
		       0,0,1,0,1,0,0,1,1,0,1,1,0,0,0,0,0,0,1,0,1,0,0,0,1,0,1,1,0,1,0,1,0,\
		       0,0,1,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,1,1,0,1,0,1,0,1,1,0,0,0,0,0,0,\
		       0,0,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,0,0,0,1,0,0,0,\
		       1,1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,\
		       1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1,0};

  IMAGE *pm1        = NULL;
  IMAGE *pmsimple   = NULL;
  IMAGE *pm2        = NULL;
  IMAGE *pmlabel    = NULL;
  IMAGE *pmlabelcrt = NULL;
  IMAGE *pm55       = NULL;
  IMAGE *pm1not     = NULL;
  IMAGE *pmcrt      = NULL;

  IMAGE* imse =   create_image(t_UCHAR, 3, 3, 1);

  UCHAR *ppm1;
  UCHAR *ppm2;
  UCHAR *ppmsimple;
  UCHAR *pimse;
  PIX_TYPE *ppm1NG, maxNG;
  PIX_TYPE *panchor;
  PIX_TYPE *ppmcrt;
  OIHTLABEL_TYPE *ppmlabel;

  G_TYPE *pg;


  int nbiter=0;

  /* get min & max values */
  pg = min_max(pm1NG);
  if (pg == NULL){
    free_image(imse);
    return(ERROR);
  }
  maxNG = pg[1].generic_val;
  free((char *)pg);

  /* avoid border problems */
  BOX_2D;
  generic_addframebox(pm1NG, box, PIX_MAX);
  generic_addframebox(pm1NG, box, 0);
  generic_addframebox(imanchor, box, 0);
  generic_addframebox(imanchor, box, 0);


  /* create neighbourhood image for labelling */
  pimse = (UCHAR *)GetImPtr(imse);
  for (i=0; i<9; i++)
    pimse[i]=1;
  pimse[0]=0;
  pimse[2]=0;
  pimse[4]=0;
  pimse[6]=0;
  pimse[8]=0;
  
  
  /* allocate some buffers */
  pm55 =     (IMAGE *)create_image(t_UCHAR, 5, 5, 1);
  pmsimple = (IMAGE *)create_image(t_UCHAR,GetImNx(pm1NG), GetImNy(pm1NG), GetImNz(pm1NG));

  /* set shift arrays */   			   
  neighbours[4] = -GetImNx(pm1NG) -1; neighbours[2] = -GetImNx(pm1NG); neighbours[6] = -GetImNx(pm1NG) +1;
  neighbours[0] = -1;                                                  neighbours[1] = +1;
  neighbours[5] = +GetImNx(pm1NG) -1; neighbours[3] = +GetImNx(pm1NG); neighbours[7] = +GetImNx(pm1NG) +1; 
  
  neighbours55[4] = -6; neighbours55[2] = -5; neighbours55[6] = -4;
  neighbours55[0] = -1;                       neighbours55[1] = +1;
  neighbours55[5] = +4; neighbours55[3] = +5; neighbours55[7] = 6;


  tab[0]=NULL; tab[1]=NULL;
  tab[0]= (int*)malloc(GetImNPix(pm1NG)*sizeof(int));
  tab[1]=(int*)malloc(GetImNPix(pm1NG)*sizeof(int));
  tobetested=(int*)calloc(GetImNPix(pm1NG), sizeof(int)); 

  /* les niveaux de gris sont completement indep donc on fait PIX_MAX passages
     long en tps de calcul mais facile a coder a partir de N&B */

  
  pm1 = (IMAGE *)create_image(t_UCHAR,GetImNx(pm1NG), GetImNy(pm1NG), GetImNz(pm1NG));
  nbvirerT=1;
  loop=0;
  pm2 = copy_image(pm1);

  ppm1    = (UCHAR *)GetImPtr(pm1);
  ppm2    = (UCHAR *)GetImPtr(pm2);
  ppmsimple = (UCHAR *)GetImPtr(pmsimple);
  panchor = (PIX_TYPE *)GetImPtr(imanchor);
  ppm1NG  = (PIX_TYPE *)GetImPtr(pm1NG);
  nx = GetImNx(pm1);

  while( nbvirerT != 0 ){ /* while pixels are removed */
    loop++;
    nbvirerT=0;
    #ifdef DEBUB
      printf("pm1 before initial threshold \n");
      dumpxyz(pm1, 5, 5, 0, 20, 20);
    #endif
      
    for( ng=1;ng<=PIX_MAX;ng++ ){ /* process all levels */
      for( i=0,minNG=PIX_MAX; i<GetImNPix(pm1);i++){
	if( (ppm1NG[i] < minNG) &&  (ppm1NG[i] >= ng)){
	  minNG=ppm1NG[i];
	  if ( minNG==ng)
	    i=GetImNPix(pm1NG);
	}
      }
      ng=minNG;
      
      for( i=0; i<GetImNPix(pm1);i++){
	if (maxNG>1) /* at this stage, anchor points are used for binary only */ 
	  panchor[i]=0;
	if(ppm1NG[i] <ng )
	  ppm1[i]=0;
	else{
	  if (ppm1NG[i] > ng )
	    panchor[i]=1;
	  ppm1[i]=1;
	}
      }
      #ifdef DEBUB
        printf("pm1 after initial threshold \n");
        dumpxyz(pm1, 5, 5, 0, 20, 20);
      #endif
      if (pmlabel!=NULL)
	free_image(pmlabel);
      pm1not=copy_image(pm1);
      negation(pm1not);
      generic_addframebox(pm1not, box, 1);
      pmlabel = to_int32(pm1not);
      i32_label(pmlabel, imse, 1, 1, 0);
      subframebox(pmlabel, box);
      free_image(pm1not);
      ppmlabel  = (OIHTLABEL_TYPE *)GetImPtr(pmlabel);


      /* initialising the set of removable pixels  ie (simple and not anchor point). */
      ind[0]=0;
      ind[1]=0;
      use=0;

      for ( i=0 ; i<GetImNPix(pm1) ; i++ ){

	ppmsimple[i]=0;
	code = 0;
	if (ppm1[i]==1 && panchor[i]==0){ /* check if simple */
	  code = ppm1[i-1];
	  code |= ppm1[i+1]<<1;
	  code |= ppm1[i-nx]<<2;
	  code |= ppm1[i+nx]<<3;
	  code |= ppm1[i-nx-1]<<4;
	  code |= ppm1[i+nx-1]<<5;
	  code |= ppm1[i-nx+1]<<6;
	  code |= ppm1[i+nx+1]<<7;
          #ifdef DEBUB
	    printf("code=%d\t homotab=%d\t %d\n", code, homotab[code], i);
	  #endif
	  if (homotab[code]){
	    ppmsimple[i]=1;
	    tab[use][ind[use]]= i;
	    ind[use]++;
	  }
	}
      }
      #ifdef DEBUB
        printf("print simple pixels\n");
        dumpxyz(pmsimple, 5, 5, 0, 20, 20);
      #endif
      nbvirer=1;

      while (nbvirer != 0){ /* as long as this set is not empty loop */
	nbiter++;
	nbvirer = 0;
	for( i=0; i<GetImNPix(pm1);i++)
	  ppm2[i] = ppm1[i];
	
        pmcrt=copy_image(pm1NG);
        ppmcrt=(PIX_TYPE *)GetImPtr(pmcrt);
        pmlabelcrt=copy_image(pmlabel);
	for( i=0 ; i<ind[use] ; i++){ /* for each removable pixels */
	  pix1 = tab[use][i];
	  avirer = 1;
	  for( j=0 ; (avirer==1)&&(j<8) ; j++){  /* pr ts ses voisins */
	    pixnb = pix1+neighbours[j];
	    if ( ppmsimple[pixnb] == 1 ){ /* a simple neighbour */
	      if(independent(pm2,pix1,pixnb,j,neighbours,pm55,neighbours55) ==0){
		avirer=0;
	      }
	      else if (strictindependent(pm2,pmsimple,pix1,pixnb,j,neighbours) == 0){
		if (allsimplenb(pm2,pmsimple,pix1,neighbours,pixnb) != 0)
		  avirer=0;
		else{
		  if (ronsetest(pm2,pmsimple,pix1,pmlabelcrt,neighbours) == 1)
		    avirer=0;
		  ppm2=(UCHAR *)GetImPtr(pm2);
		}
	      }
	    }
	  }
	  /*  si avirer = 1 l'enlever de pm1 */
	  if (avirer==1){
	    nbvirer++;
	    nbvirerT++;
	    ppm1[pix1]=0;
	    for(j=0,minNG=PIX_MIN; j<8; j++) /* look for maximum value of lower 8-neighbours */
	      if ((ppmcrt[pix1+neighbours[j]] < ng) &&
		  (ppmcrt[pix1+neighbours[j]] > minNG) )
		minNG = ppmcrt[pix1+neighbours[j]];
	    ppm1NG[pix1]=minNG;
	    for(j=0;j<4;j++)
	      if (ppm1[pix1+neighbours[j]]==0)
		ppmlabel[pix1]=ppmlabel[pix1+neighbours[j]];
	  }
	} /* for each removable pixels */
        free_image(pmcrt);
        free_image(pmlabelcrt);
	ind[use]=0;
	for ( i=0 ; i<GetImNPix(pm1) ; i++ ){
	  ppmsimple[i]=0;
	  code = 0;
	  if (ppm1[i]==1 && panchor[i]==0){ /* check if simple */
	    code = ppm1[i-1];
	    code |= ppm1[i+1]<<1;
	    code |= ppm1[i-nx]<<2;
	    code |= ppm1[i+nx]<<3;
	    code |= ppm1[i-nx-1]<<4;
	    code |= ppm1[i+nx-1]<<5;
	    code |= ppm1[i-nx+1]<<6;
	    code |= ppm1[i+nx+1]<<7;
            #ifdef DEBUB
	      printf("DOWN UNDER code=%d\t homotab=%d\t %d\n", code, homotab[code], i);
	    #endif
	    if (homotab[code]){
	      ppmsimple[i]=1;
	      tab[use][ind[use]]= i;
	      ind[use]++;
	    }
	  }
	}
        #ifdef DEBUB
	  printf("AFTER DOWNUNDER\n");
	  dumpxyz(ppmsimple, 5, 5, 0, 10, 10);
	#endif
      } /*  while (nbvirer != 0) */
    } /* for NG   */
  } /* while a virerT */


  /* liberate memory and return the result */
  free (tab[0]);
  free (tab[1]);
  free (tobetested);
  free_image(pm55);
  free_image(pm1);
  free_image(imse);
  if (pmlabel!=NULL)
    free_image(pmlabel);


  subframebox(pm1NG, box);
  subframebox(pm1NG, box);
  subframebox(imanchor, box);
  subframebox(imanchor, box);
  return(NO_ERROR);
}
#include "g_undef.h"




#include "us_def.h"
ERROR_TYPE us_oiskeleton(IMAGE *pm1NG, IMAGE *imanchor)
{
  int * tobetested=NULL;
  int * tab[2];
  int ind[2];
  int use;
  int neighbours[9];
  int neighbours55[9];
  int i,j;
  int pix1;
  int pixnb;
  int avirer;
  int nbvirer;
  int loop;
  int nbvirerT;
  int ng;
  int minNG;

  int nx, code;
  int box[6];

  /* table of homotopic configurations generated automatically once for all */
  int homotab[256] = { 0,1,1,0,1,1,1,1,1,1,1,1,0,1,1,0,1,1,0,0,1,1,1,1,0,1,0,1,0,1,1,0,1,\
                       1,0,0,0,1,0,1,1,1,1,1,0,1,1,0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,1,0,1,0,\
                       1,0,1,1,1,1,0,0,1,1,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,1,0,1,1,0,0,0,0,\
		       0,0,1,0,1,0,0,1,1,0,1,1,0,0,0,0,0,0,1,0,1,0,0,0,1,0,1,1,0,1,0,1,0,\
		       0,0,1,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,1,1,0,1,0,1,0,1,1,0,0,0,0,0,0,\
		       0,0,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,0,0,0,1,0,0,0,\
		       1,1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,\
		       1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1,0};

  IMAGE *pm1        = NULL;
  IMAGE *pmsimple   = NULL;
  IMAGE *pm2        = NULL;
  IMAGE *pmlabel    = NULL;
  IMAGE *pmlabelcrt = NULL;
  IMAGE *pm55       = NULL;
  IMAGE *pm1not     = NULL;
  IMAGE *pmcrt      = NULL;

  IMAGE* imse =   create_image(t_UCHAR, 3, 3, 1);

  UCHAR *ppm1;
  UCHAR *ppm2;
  PIX_TYPE *ppm1NG;
  PIX_TYPE *panchor;
  UCHAR *ppmsimple;
  PIX_TYPE *ppmcrt;
  UCHAR *pimse;
  OIHTLABEL_TYPE *ppmlabel;


  int nbiter=0;

  /* avoid border problems */
  BOX_2D;
  us_addframebox(pm1NG, box, PIX_MAX);
  us_addframebox(pm1NG, box, 0);
  us_addframebox(imanchor, box, 0);
  us_addframebox(imanchor, box, 0);



  /* create neighbourhood image for labelling */
  pimse = (UCHAR *)GetImPtr(imse);
  for (i=0; i<9; i++)
    pimse[i]=1;
  pimse[0]=0;
  pimse[2]=0;
  pimse[4]=0;
  pimse[6]=0;
  pimse[8]=0;
  
  
  /* allocate some buffers */
  pm55 =     (IMAGE *)create_image(t_UCHAR, 5, 5, 1);
  pmsimple = (IMAGE *)create_image(t_UCHAR,GetImNx(pm1NG), GetImNy(pm1NG), GetImNz(pm1NG));

  /* set shift arrays */   			   
  neighbours[4] = -GetImNx(pm1NG) -1; neighbours[2] = -GetImNx(pm1NG); neighbours[6] = -GetImNx(pm1NG) +1;
  neighbours[0] = -1;                                                  neighbours[1] = +1;
  neighbours[5] = +GetImNx(pm1NG) -1; neighbours[3] = +GetImNx(pm1NG); neighbours[7] = +GetImNx(pm1NG) +1; 
  
  neighbours55[4] = -6; neighbours55[2] = -5; neighbours55[6] = -4;
  neighbours55[0] = -1;                       neighbours55[1] = +1;
  neighbours55[5] = +4; neighbours55[3] = +5; neighbours55[7] = 6;


  tab[0]=NULL; tab[1]=NULL;
  tab[0]= (int*)malloc(GetImNPix(pm1NG)*sizeof(int));
  tab[1]=(int*)malloc(GetImNPix(pm1NG)*sizeof(int));
  tobetested=(int*)calloc(GetImNPix(pm1NG), sizeof(int)); 

  /* les niveaux de gris sont completement indep donc on fait PIX_MAX passages
     long en tps de calcul mais facile a coder a partir de N&B */

  
  pm1 = (IMAGE *)create_image(t_UCHAR,GetImNx(pm1NG), GetImNy(pm1NG), GetImNz(pm1NG));
  nbvirerT=1;
  loop=0;
  pm2 = copy_image(pm1);

  ppm1    = (UCHAR *)GetImPtr(pm1);
  ppm2    = (UCHAR *)GetImPtr(pm2);
  panchor = (PIX_TYPE *)GetImPtr(imanchor);
  ppm1NG  = (PIX_TYPE *)GetImPtr(pm1NG);
  ppmsimple = (UCHAR *)GetImPtr(pmsimple);

  nx = GetImNx(pm1);

  while( nbvirerT != 0 ){ /* while pixels are removed */
    loop++;
    nbvirerT=0;
    #ifdef DEBUB
      printf("pm1 before initial threshold \n");
      us_dumpxyz(pm1, 5, 5, 0, 20, 20);
    #endif
      
    for( ng=1;ng<=PIX_MAX;ng++ ){ /* process all levels */
      for( i=0,minNG=PIX_MAX; i<GetImNPix(pm1);i++){
	if( (ppm1NG[i] < minNG) &&  (ppm1NG[i] >= ng)){
	  minNG=ppm1NG[i];
	  if ( minNG==ng)
	    i=GetImNPix(pm1NG);
	}
      }
      ng=minNG;
      #ifdef DEBUB
        printf("ng=%d\n",ng);
      #endif
      
      for( i=0; i<GetImNPix(pm1);i++){
	panchor[i]=0;
	if(ppm1NG[i] <ng )
	  ppm1[i]=0;
	else{
	  if (ppm1NG[i] > ng )
	    panchor[i]=1;
	  ppm1[i]=1;
	}
      }
      #ifdef DEBUB
        printf("pm1 after initial threshold \n");
        us_dumpxyz(pm1, 5, 5, 0, 20, 20);
      #endif
      if (pmlabel!=NULL)
	free_image(pmlabel);
      pm1not=copy_image(pm1);
      negation(pm1not);
      generic_addframebox(pm1not, box, 1);
      pmlabel = to_int32(pm1not);
      i32_label(pmlabel, imse, 1, 1, 0);
      subframebox(pmlabel, box);
      free_image(pm1not);
      ppmlabel  = (OIHTLABEL_TYPE *)GetImPtr(pmlabel);


      /* initialising the set of removable pixels  ie (simple and not anchor point). */
      ind[0]=0;
      ind[1]=0;
      use=0;

      for ( i=0 ; i<GetImNPix(pm1) ; i++ ){

	ppmsimple[i]=0;
	code = 0;
	if (ppm1[i]==1 && panchor[i]==0){ /* check if simple */
	  code = ppm1[i-1];
	  code |= ppm1[i+1]<<1;
	  code |= ppm1[i-nx]<<2;
	  code |= ppm1[i+nx]<<3;
	  code |= ppm1[i-nx-1]<<4;
	  code |= ppm1[i+nx-1]<<5;
	  code |= ppm1[i-nx+1]<<6;
	  code |= ppm1[i+nx+1]<<7;
          #ifdef DEBUB
	    printf("code=%d\t homotab=%d\t %d\n", code, homotab[code], i);
	  #endif
	  if (homotab[code]){
	    ppmsimple[i]=1;
	    tab[use][ind[use]]= i;
	    ind[use]++;
	  }
	}
      }
      #ifdef DEBUB
        printf("print simple pixels\n");
        us_dumpxyz(pmsimple, 5, 5, 0, 20, 20);
      #endif
      nbvirer=1;

      while (nbvirer != 0){ /* as long as this set is not empty loop */
	nbiter++;
	nbvirer = 0;
	for(i=0; i<GetImNPix(pm1);i++)
	  ppm2[i] = ppm1[i];
	
        pmcrt=copy_image(pm1NG);
        ppmcrt=(PIX_TYPE *)GetImPtr(pmcrt);
        pmlabelcrt=copy_image(pmlabel);
	for( i=0 ; i<ind[use] ; i++){ /* for each removable pixels */
	  pix1 = tab[use][i];
	  avirer = 1;
      #ifdef DEBUB
	  if (pix1==0){
            printf("pm2 before\n");
	    us_dumpxyz(pm2, 5, 5, 0, 20, 20);
	    printf("pmsimple before\n");
	    us_dumpxyz(pmsimple, 5, 5, 0, 20, 20);
	  }
      #endif
	  for( j=0 ; (avirer==1)&&(j<8) ; j++){  /* pr ts ses voisins */
	    pixnb = pix1+neighbours[j];
	    if ( ppmsimple[pixnb] == 1 ){
	      if( independent(pm2,pix1,pixnb,j,neighbours,pm55,neighbours55) ==0){
		avirer=0;
	      }
	      else{  /*  sinon si pas strictement indepdt */
                    #ifdef DEBUB
		if (pix1==0){
		  printf("pix1=%d\n", pix1);
		  printf("pm2 before strict independence\n");
		  us_dumpxyz(pm2, 5, 5, 0, 20, 20);
		  printf("pmsimple\n");
		  us_dumpxyz(pmsimple, 5, 5, 0, 20, 20);
		}
		    #endif
		if (strictindependent(pm2,pmsimple,pix1,pixnb,j,neighbours) == 0){
		  /* si ts ses autres voisins pas a 0 avirer=0 */

		  if (allsimplenb(pm2,pmsimple,pix1,neighbours,pixnb) !=0 ){
		    avirer=0;
		  }
				/* sinon si ronse test ok a virer=0 */
		  else{
                    #ifdef DEBUB
   		      printf("i=%d, j=%d\n",i,j);
		      us_dumpxyz(pm2, 5, 5, 0, 10, 10);
		    #endif
		    if (ronsetest(pm2,pmsimple,pix1,pmlabelcrt,neighbours) == 1){
		      avirer=0;
		    }
		    ppm2=(UCHAR *)GetImPtr(pm2);
		  }
		}
	      }
	    }
	  }
	  /*  si avirer = 1 l'enlever de pm1 */
	  if ( avirer==1 ){
	    nbvirer++;
	    nbvirerT++;
	    ppm1[pix1]=0;
	    for(j=0,minNG=PIX_MIN; j<8; j++) /* look for maximum value of lower 8-neighbours */
	      if ((ppmcrt[pix1+neighbours[j]] < ng) &&
		  (ppmcrt[pix1+neighbours[j]] > minNG) )
		minNG = ppmcrt[pix1+neighbours[j]];
	    ppm1NG[pix1]=minNG;
	    for(j=0;j<4;j++)
	      if (ppm1[pix1+neighbours[j]]==0)
		ppmlabel[pix1]=ppmlabel[pix1+neighbours[j]];
	  }
	} /* for each removable pixels */
        free_image(pmcrt);
        free_image(pmlabelcrt);
	ind[use]=0;
	for ( i=0 ; i<GetImNPix(pm1) ; i++ ){
	  ppmsimple[i]=0;
	  code = 0;
	  if (ppm1[i]==1 && panchor[i]==0){ /* check if simple */
	    code = ppm1[i-1];
	    code |= ppm1[i+1]<<1;
	    code |= ppm1[i-nx]<<2;
	    code |= ppm1[i+nx]<<3;
	    code |= ppm1[i-nx-1]<<4;
	    code |= ppm1[i+nx-1]<<5;
	    code |= ppm1[i-nx+1]<<6;
	    code |= ppm1[i+nx+1]<<7;
            #ifdef DEBUB
	      printf("DOWN UNDER code=%d\t homotab=%d\t %d\n", code, homotab[code], i);
	    #endif
	    if (homotab[code]){
	      ppmsimple[i]=1;
	      tab[use][ind[use]]= i;
	      ind[use]++;
	    }
	  }
	}
        #ifdef DEBUB
	  printf("AFTER DOWNUNDER\n");
	  us_dumpxyz(ppmsimple, 5, 5, 0, 10, 10);
	#endif
      } /*  while (nbvirer != 0) */
    } /* for NG   */
  } /* while a virerT */


  /* free allocted memory */
  free (tab[0]);
  free (tab[1]);
  free (tobetested);
  free_image(pm55);
  free_image(pm1);
  free_image(imse);
  if (pmlabel!=NULL)
    free_image(pmlabel);


  subframebox(pm1NG, box);
  subframebox(pm1NG, box);
  subframebox(imanchor, box);
  subframebox(imanchor, box);
  return(NO_ERROR);
}
#include "us_undef.h"






ERROR_TYPE oiskeleton(IMAGE *im, IMAGE *imanchor)
{

  if (szcompat(im, imanchor)!=NO_ERROR){
    (void) sprintf(buf, "oiskeleton(IMAGE *im, IMAGE *imanchor): images must be of same type and size and with non-NULL pointer to pixel data\n"); errputstr(buf);
    return ERROR;
  }
  
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(generic_oiskeleton(im, imanchor));
    break;

  case t_USHORT:
    return(us_oiskeleton(im, imanchor));
    break;

  default:
    (void) sprintf(buf, "oiskeleton(IMAGE *im, IMAGE *imanchor): invalid pixel type\n"); errputstr(buf);
    return ERROR;
  }
  return ERROR;
}





/* */
/* Order independent grey scale anchor skeleton */
/* First: 21-03-01 */
#include "g_def.h"
ERROR_TYPE generic_oiask(IMAGE *pm1NG, IMAGE *imanchor)
{
  int *tobetested=NULL;
  int *tab[2];
  int ind[2];
  int use;
  int neighbours[9];
  int neighbours55[9];
  int i,j;
  int pix1;
  int pixnb;
  int avirer;
  int nbvirer;
  int loop;
  int nbvirerT;
  int ng;
  int minNG;

  int nx, code;
  int box[6];

  int acounter;
  char fn[23];

  /* table of homotopic configurations generated automatically once for all */
  int homotab[256] = { 0,1,1,0,1,1,1,1,1,1,1,1,0,1,1,0,1,1,0,0,1,1,1,1,0,1,0,1,0,1,1,0,1,\
                       1,0,0,0,1,0,1,1,1,1,1,0,1,1,0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,1,0,1,0,\
                       1,0,1,1,1,1,0,0,1,1,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,1,0,1,1,0,0,0,0,\
		       0,0,1,0,1,0,0,1,1,0,1,1,0,0,0,0,0,0,1,0,1,0,0,0,1,0,1,1,0,1,0,1,0,\
		       0,0,1,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,1,1,0,1,0,1,0,1,1,0,0,0,0,0,0,\
		       0,0,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,0,0,0,1,0,0,0,\
		       1,1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,\
		       1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1,0};

  IMAGE *pm1        = NULL;
  IMAGE *pmsimple   = NULL;
  IMAGE *pm2        = NULL;
  IMAGE *pmlabel    = NULL;
  IMAGE *pmlabelcrt = NULL;
  IMAGE *pm55       = NULL;
  IMAGE *pm1not     = NULL;
  IMAGE *pmcrt      = NULL;
  IMAGE *imanchorcrt  = NULL;  /* current image for anchor points */

  IMAGE* imse =   create_image(t_UCHAR, 3, 3, 1);

  UCHAR *ppm1;
  UCHAR *ppm2;
  UCHAR *ppmsimple;
  UCHAR *pimse;
  PIX_TYPE *ppm1NG;
  PIX_TYPE *panchor;
  PIX_TYPE *panchorcrt;
  PIX_TYPE *ppmcrt;
  OIHTLABEL_TYPE *ppmlabel;

  int nbiter=0;

  /* avoid border problems */
  BOX_2D;
  generic_addframebox(pm1NG, box, PIX_MAX);
  generic_addframebox(pm1NG, box, 0);
  generic_addframebox(imanchor, box, 0);
  generic_addframebox(imanchor, box, 0);

  /* create neighbourhood image for labelling */
  pimse = (UCHAR *)GetImPtr(imse);
  for (i=0; i<9; i++)
    pimse[i]=1;
  pimse[0]=0;
  pimse[2]=0;
  pimse[4]=0;
  pimse[6]=0;
  pimse[8]=0;
  
  /* allocate some buffers */
  pm55        = (IMAGE *)create_image(t_UCHAR, 5, 5, 1);
  pmsimple    = (IMAGE *)create_image(t_UCHAR,GetImNx(pm1NG), GetImNy(pm1NG), GetImNz(pm1NG));
  imanchorcrt = (IMAGE *)create_image(t_UCHAR,GetImNx(imanchor), GetImNy(imanchor), GetImNz(imanchor));

  /* set shift arrays */   			   
  neighbours[4] = -GetImNx(pm1NG) -1; neighbours[2] = -GetImNx(pm1NG); neighbours[6] = -GetImNx(pm1NG) +1;
  neighbours[0] = -1;                                                  neighbours[1] = +1;
  neighbours[5] = +GetImNx(pm1NG) -1; neighbours[3] = +GetImNx(pm1NG); neighbours[7] = +GetImNx(pm1NG) +1; 
  
  neighbours55[4] = -6; neighbours55[2] = -5; neighbours55[6] = -4;
  neighbours55[0] = -1;                       neighbours55[1] = +1;
  neighbours55[5] = +4; neighbours55[3] = +5; neighbours55[7] = 6;


  tab[0]=NULL; tab[1]=NULL;
  tab[0]= (int*)malloc(GetImNPix(pm1NG)*sizeof(int));
  tab[1]=(int*)malloc(GetImNPix(pm1NG)*sizeof(int));
  tobetested=(int*)calloc(GetImNPix(pm1NG), sizeof(int)); 

  /* les niveaux de gris sont completement indep donc on fait PIX_MAX passages
     long en tps de calcul mais facile a coder a partir de N&B */
  
  pm1 = (IMAGE *)create_image(t_UCHAR,GetImNx(pm1NG), GetImNy(pm1NG), GetImNz(pm1NG));
  nbvirerT=1;
  loop=0;
  pm2 = copy_image(pm1);

  ppm1      = (UCHAR *)GetImPtr(pm1);
  ppm2      = (UCHAR *)GetImPtr(pm2);
  ppmsimple = (UCHAR *)GetImPtr(pmsimple);
  panchor   = (PIX_TYPE *)GetImPtr(imanchor);
  panchorcrt= (PIX_TYPE *)GetImPtr(imanchorcrt);
  ppm1NG    = (PIX_TYPE *)GetImPtr(pm1NG);
  nx        = GetImNx(pm1);

  acounter=0;
  while( nbvirerT != 0 ){ /* while pixels are removed */
    loop++;
    acounter++;
    nbvirerT=0;
    #ifdef DEBUB
      printf("pm1 before initial threshold \n");
      dumpxyz(pm1, 5, 5, 0, 20, 20);
    #endif
      
    for( ng=1;ng<=PIX_MAX;ng++ ){ /* process all levels */
      for( i=0,minNG=PIX_MAX; i<GetImNPix(pm1);i++){
	if( (ppm1NG[i] < minNG) &&  (ppm1NG[i] >= ng)){
	  minNG=ppm1NG[i];
	  if ( minNG==ng)
	    i=GetImNPix(pm1NG);
	}
      }
      ng=minNG;
      #ifdef DEBUB
        printf("ng=%d\n",ng);
      #endif
      
      memcpy((void *)panchorcrt, (void *)panchor, (size_t)GetImNPix(imanchor));
      for( i=0; i<GetImNPix(pm1);i++){
	if(ppm1NG[i] <ng )
	  ppm1[i]=0;
	else{
	  if (ppm1NG[i] > ng )
	    panchorcrt[i]=1;
	  ppm1[i]=1;
	}
      }
      #ifdef DEBUB
        printf("pm1 after initial threshold \n");
        dumpxyz(pm1, 5, 5, 0, 20, 20);
      #endif
      if (pmlabel!=NULL)
	free_image(pmlabel);
      pm1not=copy_image(pm1);
      negation(pm1not);
      generic_addframebox(pm1not, box, 1);
      pmlabel = to_int32(pm1not);
      i32_label(pmlabel, imse, 1, 1, 0);
      subframebox(pmlabel, box);
      free_image(pm1not);
      ppmlabel  = (OIHTLABEL_TYPE *)GetImPtr(pmlabel);


      /* initialising the set of removable pixels  ie (simple and not anchor point). */
      ind[0]=0;
      ind[1]=0;
      use=0;

      for ( i=0 ; i<GetImNPix(pm1) ; i++ ){

	ppmsimple[i]=0;
	code = 0;
	if (ppm1[i]==1 && panchorcrt[i]==0){ /* check if simple */
	  code = ppm1[i-1];
	  code |= ppm1[i+1]<<1;
	  code |= ppm1[i-nx]<<2;
	  code |= ppm1[i+nx]<<3;
	  code |= ppm1[i-nx-1]<<4;
	  code |= ppm1[i+nx-1]<<5;
	  code |= ppm1[i-nx+1]<<6;
	  code |= ppm1[i+nx+1]<<7;
          #ifdef DEBUB
	    printf("code=%d\t homotab=%d\t %d\n", code, homotab[code], i);
	  #endif
	  if (homotab[code]){
	    ppmsimple[i]=1;
	    tab[use][ind[use]]= i;
	    ind[use]++;
	  }
	}
      }
      #ifdef DEBUB
        printf("print simple pixels\n");
        dumpxyz(pmsimple, 5, 5, 0, 20, 20);
      #endif
      nbvirer=1;

      while ( nbvirer != 0){ /* as long as this set is not empty loop */
	nbiter++;
	nbvirer = 0;
	for( i=0; i<GetImNPix(pm1);i++)
	  ppm2[i] = ppm1[i];
	
        pmcrt=copy_image(pm1NG);
        ppmcrt=(PIX_TYPE *)GetImPtr(pmcrt);
        pmlabelcrt=copy_image(pmlabel);
	for( i=0 ; i<ind[use] ; i++){ /* for each removable pixels */
	  pix1 = tab[use][i];
	  avirer = 1;
      #ifdef DEBUB
	  if (pix1==0){
            printf("pm2 before\n");
	    dumpxyz(pm2, 5, 5, 0, 20, 20);
	    printf("pmsimple before\n");
	    dumpxyz(pmsimple, 5, 5, 0, 20, 20);
	  }
      #endif
	  for( j=0 ; (avirer==1)&&(j<8) ; j++){  /* pr ts ses voisins */
	    pixnb = pix1+neighbours[j];
	    if ( ppmsimple[pixnb] == 1 ){
	      if( independent(pm2,pix1,pixnb,j,neighbours,pm55,neighbours55) ==0){
		avirer=0;
	      }
	      else{  /*  sinon si pas strictement indepdt */
		/* if (pix1==0){
		  printf("pix1=%d\n", pix1);
		  printf("pm2 before strict independence\n");
		  dumpxyz(pm2, 5, 5, 0, 20, 20);
		  printf("pmsimple\n");
		  dumpxyz(pmsimple, 5, 5, 0, 20, 20);
		  } */
		if (strictindependent(pm2,pmsimple,pix1,pixnb,j,neighbours) == 0){
		  /* si ts ses autres voisins pas a 0 avirer=0 */

		  if (allsimplenb(pm2,pmsimple,pix1,neighbours,pixnb) !=0 ){
		    avirer=0;
		  }
				/* sinon si ronse test ok a virer=0 */
		  /* else{
                    #ifdef DEBUB
   		      printf("i=%d, j=%d\n",i,j);
		      dumpxyz(pm2, 5, 5, 0, 10, 10);
		    #endif
		      if (ronsetest(pm2,pmsimple,pix1,pmlabelcrt,neighbours) == 1){
		      printf("Well, Ronse's test returned 1!!!\n");
		      avirer=0;
		      }
		    ppm2=(UCHAR *)GetImPtr(pm2);
		  } */
		}
	      }
	    }
	  }
	  /*  si avirer = 1 l'enlever de pm1 */
	  if ( avirer==1 ){
	    nbvirer++;
	    nbvirerT++;
	    ppm1[pix1]=0;
	    for(j=0,minNG=PIX_MIN; j<8; j++) /* look for maximum value of lower 8-neighbours */
	      if ((ppmcrt[pix1+neighbours[j]] < ng) &&
		  (ppmcrt[pix1+neighbours[j]] > minNG) )
		minNG = ppmcrt[pix1+neighbours[j]];
	    ppm1NG[pix1]=minNG;
	    for(j=0;j<4;j++)
	      if (ppm1[pix1+neighbours[j]]==0)
		ppmlabel[pix1]=ppmlabel[pix1+neighbours[j]];
	  }
	} /* for each removable pixels */
        free_image(pmcrt);
        free_image(pmlabelcrt);
	ind[use]=0;
	for ( i=0 ; i<GetImNPix(pm1) ; i++ ){
	  ppmsimple[i]=0;
	  code = 0;
	  if (ppm1[i]==1 && panchorcrt[i]==0){ /* check if simple */
	    code = ppm1[i-1];
	    code |= ppm1[i+1]<<1;
	    code |= ppm1[i-nx]<<2;
	    code |= ppm1[i+nx]<<3;
	    code |= ppm1[i-nx-1]<<4;
	    code |= ppm1[i+nx-1]<<5;
	    code |= ppm1[i-nx+1]<<6;
	    code |= ppm1[i+nx+1]<<7;
            #ifdef DEBUB
	      printf("DOWN UNDER code=%d\t homotab=%d\t %d\n", code, homotab[code], i);
	    #endif
	    if (homotab[code]){
	      ppmsimple[i]=1;
	      tab[use][ind[use]]= i;
	      ind[use]++;
	    }
	  }
	}
        #ifdef DEBUB
	  printf("AFTER DOWNUNDER\n");
	  dumpxyz(ppmsimple, 5, 5, 0, 10, 10);
	#endif
      } /*  while (nbvirer != 0) */
      sprintf(fn,"/tmp/im%3d.tif", ng);
      if (acounter==1)
	write_tiff(pm1NG, fn);
    } /* for NG   */
  } /* while a virerT */


  printf("acounter=%d\n", acounter);
  /* liberate memory and return the result */
  free (tab[0]);
  free (tab[1]);
  free (tobetested);
  free_image(pm55);
  free_image(pm1);
  free_image(imanchorcrt);
  free_image(imse);
  if (pmlabel!=NULL)
    free_image(pmlabel);


  subframebox(pm1NG, box);
  subframebox(pm1NG, box);
  subframebox(imanchor, box);
  subframebox(imanchor, box);
  return(NO_ERROR);
}
#include "g_undef.h"



#include "us_def.h"
ERROR_TYPE us_oiask(IMAGE *pm1NG, IMAGE *imanchor)
{
  int *tobetested=NULL;
  int *tab[2];
  int ind[2];
  int use;
  int neighbours[9];
  int neighbours55[9];
  int i,j;
  int pix1;
  int pixnb;
  int avirer;
  int nbvirer;
  int loop;
  int nbvirerT;
  int ng;
  int minNG;

  int nx, code;
  int box[6];

  int acounter;
  /* char fn[9]; UNUSED! 2004-02-23 */

  /* table of homotopic configurations generated automatically once for all */
  int homotab[256] = { 0,1,1,0,1,1,1,1,1,1,1,1,0,1,1,0,1,1,0,0,1,1,1,1,0,1,0,1,0,1,1,0,1,\
                       1,0,0,0,1,0,1,1,1,1,1,0,1,1,0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,1,0,1,0,\
                       1,0,1,1,1,1,0,0,1,1,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,1,0,1,1,0,0,0,0,\
		       0,0,1,0,1,0,0,1,1,0,1,1,0,0,0,0,0,0,1,0,1,0,0,0,1,0,1,1,0,1,0,1,0,\
		       0,0,1,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,1,1,0,1,0,1,0,1,1,0,0,0,0,0,0,\
		       0,0,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,0,0,0,1,0,0,0,\
		       1,1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,\
		       1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1,0};

  IMAGE *pm1        = NULL;
  IMAGE *pmsimple   = NULL;
  IMAGE *pm2        = NULL;
  IMAGE *pmlabel    = NULL;
  IMAGE *pmlabelcrt = NULL;
  IMAGE *pm55       = NULL;
  IMAGE *pm1not     = NULL;
  IMAGE *pmcrt      = NULL;
  IMAGE *imanchorcrt  = NULL;  /* current image for anchor points */

  IMAGE* imse =   create_image(t_UCHAR, 3, 3, 1);

  UCHAR *ppm1;
  UCHAR *ppm2;
  UCHAR *ppmsimple;
  UCHAR *pimse;
  PIX_TYPE *ppm1NG;
  PIX_TYPE *panchor;
  PIX_TYPE *panchorcrt;
  PIX_TYPE *ppmcrt;
  OIHTLABEL_TYPE *ppmlabel;

  int nbiter=0;

  /* avoid border problems */
  BOX_2D;
  us_addframebox(pm1NG, box, PIX_MAX);
  us_addframebox(pm1NG, box, 0);
  us_addframebox(imanchor, box, 0);
  us_addframebox(imanchor, box, 0);

  /* create neighbourhood image for labelling */
  pimse = (UCHAR *)GetImPtr(imse);
  for (i=0; i<9; i++)
    pimse[i]=1;
  pimse[0]=0;
  pimse[2]=0;
  pimse[4]=0;
  pimse[6]=0;
  pimse[8]=0;
  
  /* allocate some buffers */
  pm55        = (IMAGE *)create_image(t_UCHAR, 5, 5, 1);
  pmsimple    = (IMAGE *)create_image(t_UCHAR,GetImNx(pm1NG), GetImNy(pm1NG), GetImNz(pm1NG));
  imanchorcrt = (IMAGE *)create_image(t_USHORT,GetImNx(imanchor), GetImNy(imanchor), GetImNz(imanchor));

  /* set shift arrays */   			   
  neighbours[4] = -GetImNx(pm1NG) -1; neighbours[2] = -GetImNx(pm1NG); neighbours[6] = -GetImNx(pm1NG) +1;
  neighbours[0] = -1;                                                  neighbours[1] = +1;
  neighbours[5] = +GetImNx(pm1NG) -1; neighbours[3] = +GetImNx(pm1NG); neighbours[7] = +GetImNx(pm1NG) +1; 
  
  neighbours55[4] = -6; neighbours55[2] = -5; neighbours55[6] = -4;
  neighbours55[0] = -1;                       neighbours55[1] = +1;
  neighbours55[5] = +4; neighbours55[3] = +5; neighbours55[7] = 6;


  tab[0]=NULL; tab[1]=NULL;
  tab[0]= (int*)malloc(GetImNPix(pm1NG)*sizeof(int));
  tab[1]=(int*)malloc(GetImNPix(pm1NG)*sizeof(int));
  tobetested=(int*)calloc(GetImNPix(pm1NG), sizeof(int)); 

  /* les niveaux de gris sont completement indep donc on fait PIX_MAX passages
     long en tps de calcul mais facile a coder a partir de N&B */

  
  pm1 = (IMAGE *)create_image(t_UCHAR,GetImNx(pm1NG), GetImNy(pm1NG), GetImNz(pm1NG));
  nbvirerT=1;
  loop=0;
  pm2 = copy_image(pm1);

  ppm1      = (UCHAR *)GetImPtr(pm1);
  ppm2      = (UCHAR *)GetImPtr(pm2);
  ppmsimple = (UCHAR *)GetImPtr(pmsimple);
  panchor   = (PIX_TYPE *)GetImPtr(imanchor);
  panchorcrt= (PIX_TYPE *)GetImPtr(imanchorcrt);
  ppm1NG    = (PIX_TYPE *)GetImPtr(pm1NG);
  nx        = GetImNx(pm1);

  acounter=0;
  while( nbvirerT != 0 ){ /* while pixels are removed */
    loop++;
    acounter++;
    nbvirerT=0;
    #ifdef DEBUB
      printf("pm1 before initial threshold \n");
      us_dumpxyz(pm1, 5, 5, 0, 20, 20);
    #endif
      
    for( ng=1;ng<=PIX_MAX;ng++ ){ /* process all levels */
      for( i=0,minNG=PIX_MAX; i<GetImNPix(pm1);i++){
	if( (ppm1NG[i] < minNG) &&  (ppm1NG[i] >= ng)){
	  minNG=ppm1NG[i];
	  if ( minNG==ng)
	    i=GetImNPix(pm1NG);
	}
      }
      ng=minNG;
      #ifdef DEBUB
        printf("ng=%d\n",ng);
      #endif
      
      memcpy((void *)panchorcrt, (void *)panchor, (size_t)GetImNPix(imanchor)*sizeof(PIX_TYPE));
      for( i=0; i<GetImNPix(pm1);i++){
	if(ppm1NG[i] <ng )
	  ppm1[i]=0;
	else{
	  if (ppm1NG[i] > ng )
	    panchorcrt[i]=1;
	  ppm1[i]=1;
	}
      }
      #ifdef DEBUB
        printf("pm1 after initial threshold \n");
        us_dumpxyz(pm1, 5, 5, 0, 20, 20);
      #endif
      if (pmlabel!=NULL)
	free_image(pmlabel);
      pm1not=copy_image(pm1);
      negation(pm1not);
      generic_addframebox(pm1not, box, 1);  /* was us_ */
      pmlabel = to_int32(pm1not);
      i32_label(pmlabel, imse, 1, 1, 0);
      subframebox(pmlabel, box);
      free_image(pm1not);
      ppmlabel  = (OIHTLABEL_TYPE *)GetImPtr(pmlabel);


      /* initialising the set of removable pixels  ie (simple and not anchor point). */
      ind[0]=0;
      ind[1]=0;
      use=0;

      for ( i=0 ; i<GetImNPix(pm1) ; i++ ){

	ppmsimple[i]=0;
	code = 0;
	if (ppm1[i]==1 && panchorcrt[i]==0){ /* check if simple */
	  code = ppm1[i-1];
	  code |= ppm1[i+1]<<1;
	  code |= ppm1[i-nx]<<2;
	  code |= ppm1[i+nx]<<3;
	  code |= ppm1[i-nx-1]<<4;
	  code |= ppm1[i+nx-1]<<5;
	  code |= ppm1[i-nx+1]<<6;
	  code |= ppm1[i+nx+1]<<7;
          #ifdef DEBUB
	    printf("code=%d\t homotab=%d\t %d\n", code, homotab[code], i);
	  #endif
	  if (homotab[code]){
	    ppmsimple[i]=1;
	    tab[use][ind[use]]= i;
	    ind[use]++;
	  }
	}
      }
      #ifdef DEBUB
        printf("print simple pixels\n");
        us_dumpxyz(pmsimple, 5, 5, 0, 20, 20);
      #endif
      nbvirer=1;

      while ( nbvirer != 0){ /* as long as this set is not empty loop */
	nbiter++;
	nbvirer = 0;
	for( i=0; i<GetImNPix(pm1);i++)
	  ppm2[i] = ppm1[i];
	
        pmcrt=copy_image(pm1NG);
        ppmcrt=(PIX_TYPE *)GetImPtr(pmcrt);
        pmlabelcrt=copy_image(pmlabel);
	for( i=0 ; i<ind[use] ; i++){ /* for each removable pixels */
	  pix1 = tab[use][i];
	  avirer = 1;
      #ifdef DEBUB
	  if (pix1==0){
            printf("pm2 before\n");
	    us_dumpxyz(pm2, 5, 5, 0, 20, 20);
	    printf("pmsimple before\n");
	    us_dumpxyz(pmsimple, 5, 5, 0, 20, 20);
	  }
      #endif
	  for( j=0 ; (avirer==1)&&(j<8) ; j++){  /* pr ts ses voisins */
	    pixnb = pix1+neighbours[j];
	    if ( ppmsimple[pixnb] == 1 ){
	      if( independent(pm2,pix1,pixnb,j,neighbours,pm55,neighbours55) ==0){
		avirer=0;
	      }
	      else{  /*  sinon si pas strictement indepdt */
		/* if (pix1==0){
		  printf("pix1=%d\n", pix1);
		  printf("pm2 before strict independence\n");
		  us_dumpxyz(pm2, 5, 5, 0, 20, 20);
		  printf("pmsimple\n");
		  us_dumpxyz(pmsimple, 5, 5, 0, 20, 20);
		  }*/
		if (strictindependent(pm2,pmsimple,pix1,pixnb,j,neighbours) == 0){
		  /* si ts ses autres voisins pas a 0 avirer=0 */

		  if (allsimplenb(pm2,pmsimple,pix1,neighbours,pixnb) !=0 ){
		    avirer=0;
		  }
				/* sinon si ronse test ok a virer=0 */
		  /*
		    Not necessary with anchor points 

		    else{
                    #ifdef DEBUB
   		      printf("i=%d, j=%d\n",i,j);
		      us_dumpxyz(pm2, 5, 5, 0, 10, 10);
		    #endif
		      if (ronsetest(pm2,pmsimple,pix1,pmlabelcrt,neighbours) == 1){
		      printf("Well, Ronse's test returned 1!!!\n");
		      avirer=0;
		      }
		    ppm2=(UCHAR *)GetImPtr(pm2);
		  } */
		}
	      }
	    }
	  }
	  /*  si avirer = 1 l'enlever de pm1 */
	  if ( avirer==1 ){
	    nbvirer++;
	    nbvirerT++;
	    ppm1[pix1]=0;
	    for(j=0,minNG=PIX_MIN; j<8; j++) /* look for maximum value of lower 8-neighbours */
	      if ((ppmcrt[pix1+neighbours[j]] < ng) &&
		  (ppmcrt[pix1+neighbours[j]] > minNG) )
		minNG = ppmcrt[pix1+neighbours[j]];
	    ppm1NG[pix1]=minNG;
	    for(j=0;j<4;j++)
	      if (ppm1[pix1+neighbours[j]]==0)
		ppmlabel[pix1]=ppmlabel[pix1+neighbours[j]];
	  }
	} /* for each removable pixels */
        free_image(pmcrt);
        free_image(pmlabelcrt);
	ind[use]=0;
	for ( i=0 ; i<GetImNPix(pm1) ; i++ ){
	  ppmsimple[i]=0;
	  code = 0;
	  if (ppm1[i]==1 && panchorcrt[i]==0){ /* check if simple */
	    code = ppm1[i-1];
	    code |= ppm1[i+1]<<1;
	    code |= ppm1[i-nx]<<2;
	    code |= ppm1[i+nx]<<3;
	    code |= ppm1[i-nx-1]<<4;
	    code |= ppm1[i+nx-1]<<5;
	    code |= ppm1[i-nx+1]<<6;
	    code |= ppm1[i+nx+1]<<7;
            #ifdef DEBUB
	      printf("DOWN UNDER code=%d\t homotab=%d\t %d\n", code, homotab[code], i);
	    #endif
	    if (homotab[code]){
	      ppmsimple[i]=1;
	      tab[use][ind[use]]= i;
	      ind[use]++;
	    }
	  }
	}
        #ifdef DEBUB
	  printf("AFTER DOWNUNDER\n");
	  us_dumpxyz(ppmsimple, 5, 5, 0, 10, 10);
	#endif
      } /*  while (nbvirer != 0) */
      /* sprintf(fn,"/tmp/im%3d.tif", ng);
      if (acounter==1)
	write_tiff(pm1NG, fn);*/
    } /* for NG   */
  } /* while a virerT */


  printf("acounter=%d\n", acounter);
  /* liberate memory and return the result */
  free (tab[0]);
  free (tab[1]);
  free (tobetested);
  free_image(pm55);
  free_image(pm1);
  free_image(imanchorcrt);
  free_image(imse);
  if (pmlabel!=NULL)
    free_image(pmlabel);

  subframebox(pm1NG, box);
  subframebox(pm1NG, box);
  subframebox(imanchor, box);
  subframebox(imanchor, box);
  return(NO_ERROR);
}
#include "us_undef.h"

ERROR_TYPE oiask(IMAGE *im, IMAGE *imanchor)
{

  if (szcompat(im, imanchor)!=NO_ERROR){
    (void) sprintf(buf,"oiask(IMAGE *im, IMAGE *imanchor): images must be of same type and size and with non-NULL pointer to pixel data\n"); errputstr(buf);
    return ERROR;
  }
  
  switch (GetImDataType(im)){
  case t_UCHAR:
    return(generic_oiask(im, imanchor));
    break;
  case t_USHORT:
    return(us_oiask(im, imanchor));
    break;
  default:
    (void) sprintf(buf, "oiask(IMAGE *im, IMAGE *imanchor): invalid pixel type\n"); errputstr(buf);
    return ERROR;
  }
  return ERROR;
}


#include "uc_def.h"
int uc_ronsetest4LPE(IMAGE *pm1, IMAGE *pmsimple, int pix1, IMAGE *pmlabel, IMAGE *pm1ori, IMAGE *pmNG, int* neighbours)
     /* remove all simple pixels as long as potentially new one appears */
     /* the anchor image indicate the pixels which can't be remove */
{
  int label=0;
  int maxmin =0;
  int i,j;
  IMAGE *seed=copy_image(pm1);
  
  PIX_TYPE *ppm1=(PIX_TYPE *)GetImPtr(pm1);
  PIX_TYPE *ppm1ori=(PIX_TYPE *)GetImPtr(pm1ori);
  PIX_TYPE *pseed=(PIX_TYPE *)GetImPtr(seed);
  PIX_TYPE *ppmsimple=(PIX_TYPE *)GetImPtr(pmsimple);
  PIX_TYPE *ppmNG=(PIX_TYPE *)GetImPtr(pmNG);
  OIHTLABEL_TYPE *ppmlabel=(OIHTLABEL_TYPE *)GetImPtr(pmlabel);


  generic_blank(seed, 0);

  
  for(i=0;i<4;i++)
    if(ppm1[pix1+neighbours[i]] == 0 )
      label = ppmlabel[pix1+neighbours[i]];
  pseed[pix1]=1;
  rdil(seed,pm1, 8, 1); /* CC(pm2,pix1,8);*/
  pseed=(PIX_TYPE *)GetImPtr(seed); /* pseed redefined in rdil !!! */
  ppm1=(PIX_TYPE *)GetImPtr(pm1); /* ppm1 redefined in rdil !!! */
  for( i=0; i<GetImNPix(pm1); i++ ){
    if( pseed[i] != 0 ){
      if (ppmsimple[i] == 0){
	free_image(seed);
	return(0);
      }
      else{
	for( j=0;j<4;j++){
	  if( ( ppm1[i+neighbours[j]] == 0 )){
	    if (ppmNG[i + neighbours[j]] > maxmin)
	      maxmin=ppmNG[i + neighbours[j]];
	    if ( ppmlabel[i + neighbours[j]] != label){
	      free_image(seed);		
	      return(0);
	    }
	  }
	}
      }
    }
  }

  /* We have a simply connected component of simple pixels */
  /* We set all pixels to the maximal value (maxmin) of the neighbours */
  for( i=0; i<GetImNPix(pm1); i++ ){
    if( pseed[i] != 0 ){
      ppmNG[i]=maxmin;
      ppm1ori[i]=0;
    }
  }
  free_image(seed);		
  
  return(1);
}
#include "uc_undef.h"





#include "us_def.h"
int us_ronsetest4LPE(IMAGE *pm1, IMAGE *pmsimple, int pix1, IMAGE *pmlabel, IMAGE *pm1ori, IMAGE *pmNG, int* neighbours)
     /* remove all simple pixels as long as potentially new one appears */
     /* the anchor image indicate the pixels which can't be remove */
{
  int label=0;
  int maxmin =0;
  int i,j;
  IMAGE *seed=copy_image(pm1);
  
  UCHAR  *ppm1=(UCHAR *)GetImPtr(pm1);
  UCHAR  *ppm1ori=(UCHAR *)GetImPtr(pm1ori);
  UCHAR  *pseed=(UCHAR *)GetImPtr(seed);
  UCHAR  *ppmsimple=(UCHAR *)GetImPtr(pmsimple);
  PIX_TYPE *ppmNG=(PIX_TYPE *)GetImPtr(pmNG);
  OIHTLABEL_TYPE *ppmlabel=(OIHTLABEL_TYPE *)GetImPtr(pmlabel);


  generic_blank(seed, 0);

  
  for(i=0;i<4;i++)
    if(ppm1[pix1+neighbours[i]] == 0 )
      label = ppmlabel[pix1+neighbours[i]];
  pseed[pix1]=1;
  rdil(seed,pm1, 8, 1); /* CC(pm2,pix1,8);*/
  pseed=(UCHAR *)GetImPtr(seed); /* pseed redefined in rdil !!! */
  ppm1=(UCHAR *)GetImPtr(pm1); /* ppm1 redefined in rdil !!! */
  for( i=0; i<GetImNPix(pm1); i++ ){
    if( pseed[i] != 0 ){
      if (ppmsimple[i] == 0){
	free_image(seed);
	return(0);
      }
      else{
	for( j=0;j<4;j++){
	  if( ( ppm1[i+neighbours[j]] == 0 )){
	    if (ppmNG[i + neighbours[j]] > maxmin)
	      maxmin=ppmNG[i + neighbours[j]];
	    if ( ppmlabel[i + neighbours[j]] != label){
	      free_image(seed);		
	      return(0);
	    }
	  }
	}
      }
    }
  }

  /* We have a simply connected component of simple pixels */
  /* We set all pixels to the maximal value (maxmin) of the neighbours */
  for( i=0; i<GetImNPix(pm1); i++ ){
    if( pseed[i] != 0 ){
      ppmNG[i]=maxmin;
      ppm1ori[i]=0;
    }
  }
  free_image(seed);		
  

  return(1);
}
#include "us_undef.h"


/*@}*/
