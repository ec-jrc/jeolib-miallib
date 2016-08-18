/* first 20100916 for building height measurement */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mialib.h"
#include "fifo.h"


#ifdef OPENMP
#include <omp.h>
#endif

/** \addtogroup group_label
 *  @{
 */

#include "u32_def.h"
IMAGE *u32_outeredgelut(IMAGE *ilbl, IMAGE *iedgelbl)
{

  /* extract the outer edge from labelled CCs given
     labelled internal boundaries of these CCs.
     The outer edges are defined as the connected component
     of internal boundary pixels connected to the infinite
     connected component of the background of the CC.
     It extracts the actual outer edges unless internal boundary matching outer edge
     is connected to internal boundaries coming from holes.
     This function is superseeded by outeredge() using
     Moore's contour tracing algorithm for actual outer edge extraction.
     Pierre Soille @ jrc.ec.europa.eu (c)
     First 20100915 (for building footprint characterisation)
  */
  G_TYPE *pg;
  IMAGE *lut;
  UINT32 *plut;
  PIX_TYPE *plbl, *pelbl, maxlbl;
  long int i;
  unsigned long int npix;

  /* get min & max values */
  pg = min_max(ilbl);
  if (pg == NULL)
    return(NULL);
  maxlbl = pg[1].u32_val;
  free((char *)pg);
  

  lut= (IMAGE *)create_image(t_UINT32, maxlbl+1, 1, 1);
  if (lut==NULL)
    return NULL;
  u32_blank(lut, 0);
  plut =(UINT32   *)GetImPtr(lut);
  plbl =(PIX_TYPE *)GetImPtr(ilbl);
  pelbl=(PIX_TYPE *)GetImPtr(iedgelbl);

  npix=GetImNPix(ilbl);

  for (i=0;i<npix;i++){
    if(plut[plbl[i]]==0){
      plut[plbl[i]]=pelbl[i];
    }
  }
#ifdef OPENMP
#pragma omp parallel for
#endif
  for (i=0;i<npix;i++){
    if (pelbl[i]!=plut[plbl[i]])
      pelbl[i]=0;
  }
  return lut;
}
#include "u32_undef.h"


IMAGE *outeredgelut(IMAGE *ilbl, IMAGE *iedgelbl)
{
  switch (GetImDataType(ilbl)&GetImDataType(iedgelbl)){

  case t_UINT32: 
    return u32_outeredgelut(ilbl, iedgelbl);
    break;

  default:
    (void)sprintf(buf, "ERROR in outeredgelut(): \
                invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
}

#include "u32_def.h"
#define MY_LUT_TYPE UINT32
#define t_MY_LUT_TYPE t_UINT32
IMAGE *u32_outeredge(IMAGE *ilbl, int graph)
{

  /* extract the outer edge from labelled CCs.  Use MSB for flagging.
     assumes border is set to zero to avoid border overflow.
     Pierre Soille @ jrc.ec.europa.eu (c)
     First 20100917 (for building footprint characterisation)

     based on Moore's contour tracing algorithm with Jacob's condition, see
     http://www.thebigblob.com/moore-neighbor-contour-tracing-algorithm-in-c/
     by Erik Smistad     (see local file moore_tracing.c)
     extended for label images as well as omp speed-up and graph.
     Additional image not actually necessary (coding in MSB is enough)
     but used to return an image with mask of outer edge pixels set to 1 (others to 0).
  */
  G_TYPE *pg;
  IMAGE *lut;
  MY_LUT_TYPE *plut;
  PIX_TYPE *plbl, maxlbl, lbl;
  IMAGE *imout;
  UCHAR *pout;
  int nx=GetImNx(ilbl);
  unsigned long int i, npix, pos;  // openMP requires signed loop index
  int maxcount=2;
  int box[BOXELEM];

  // Defines the neighborhood offset position from current position and the neighborhood
  // position we want to check next if we find a new border at checkLocationNr
  int neighborhood[8][2] = {
    {-1,7},       // red	   
    {-1-nx,7},	  // green   
    {-nx,1},	  // blue	   
    {-nx+1,1},	  // yellow  
    {1,3},	  // magenta 
    {1+nx,3},	  // cyan	   
    {nx,5},	  // white   
    {nx-1,5}	  // grey    
  };

  if (graph!=8)
    graph=4;
  if (graph==4){
    neighborhood[0][0] = -1;  // red   
    neighborhood[0][1] = 4;   	       
    neighborhood[1][0] = -nx; // green 
    neighborhood[1][1] = 1;   	       
    neighborhood[2][0] = 1;   // blue  
    neighborhood[2][1] = 2;   	       
    neighborhood[3][0] = nx;  // yellow
    neighborhood[3][1] = 3;
  }

  imout=(IMAGE *)create_image(t_UCHAR, GetImNx(ilbl), GetImNy(ilbl), 1);
  if (imout==NULL)
    return NULL;
  
  /* get min & max values */
  pg = min_max(ilbl);
  if (pg == NULL)
    return(NULL);
  maxlbl = pg[1].u32_val;
  free((char *)pg);
  
  lut= (IMAGE *)create_image(t_MY_LUT_TYPE, maxlbl+1, 1, 1);
  if (lut==NULL){
    free_image(imout);
    return NULL;
  }
  plut =(MY_LUT_TYPE *)GetImPtr(lut);
  plbl =(PIX_TYPE *)GetImPtr(ilbl);
  pout =(UCHAR *)GetImPtr(imout);
  npix =GetImNPix(ilbl);

  BOX_2D;
  u32_framebox(ilbl,box,0);

  plut[0]=1; // dummy value to speed-up next loop
  /* first collect first point of each CC in an array
     for subsequent parallel processing */
  for (i=0;i<npix;i++){
    if (plut[plbl[i]]==0){
      plut[plbl[i]]=i;
    }
  }

  /* process one cc at a time */
  //pragma omp parallel default(none)
  //shared(maxlbl,pout,plbl,plut,graph,neighborhood) private(i,lbl,pos)
  //{
#ifdef OPENMP
#pragma omp parallel for private(lbl, pos) 
#endif
  //  #pragma omp for nowait
  for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
    int checkLocationNr = 1;// The neighbor number of the location we want to check for a
                            // new border point
    long int checkPosition;      // The corresponding absolute array address of checkLocationNr
    int newCheckLocationNr; // Variable that holds the neighborhood position we want to
                            // check if we find a new border at checkLocationNr
    long int startPos = plut[i]; // Set start position
    int counter = 0;        // Counter is used for the jacobi stop criterion
    int counter2 = 0;       // Counter2 is used to determine if the point we have discovered 
                            // is one single point
    
    if (startPos!=0){
      lbl=plbl[startPos];
      //IFMSB plbl[startPos]|=PIX_MSB;     // mark pixel as border
      pout[startPos]=1;     // mark pixel as border
      pos=startPos;
  
      // Trace around the neighborhood
      while(1){
	checkPosition = pos + neighborhood[checkLocationNr-1][0];
	newCheckLocationNr = neighborhood[checkLocationNr-1][1];
 
	//IFMSB if( (plbl[checkPosition]&~PIX_MSB) == lbl) { // Next border point found
	if( plbl[checkPosition] == lbl) { // Next border point found
	  if(checkPosition == startPos){
	    counter ++;
	    // Stopping criterion (jacob)
	    if(newCheckLocationNr == 1 || counter >= maxcount) { // Close loop was 3: PiR: 2 for 8 and 1 for 4 ??? since we always start from upper left pixel (extreme pixel): NO counterexample in testouteredge.lsp on 20110928
	      //inside = trueval; // Since we are starting the search at were we first started we must set inside to true
	      break;
	    }
	  }
	  //IFMSB plbl[checkPosition]|=PIX_MSB;     // mark pixel as border
	  pout[checkPosition]=1;      // mark pixel as border
	  checkLocationNr = newCheckLocationNr;// Update which neighborhood position we should check next
	  pos = checkPosition;
	  counter2 = 0;    // Reset the counter that keeps track of how many neighbors we have visited

	}
	else{
	  // Rotate clockwise in the neighborhood
	  checkLocationNr = 1 + (checkLocationNr % graph);
	  if(counter2 > graph){
	    // If counter2 is above 8 we have traced around the neighborhood and
	    // therefore the border is a single black pixel and we can exit
	    counter2 = 0;
	    break;
	  }
	  else{
	    counter2 ++;
	  }
	}
      }    
    }
  }
  // }  /* END of parallel region */
  free_image(lut);
 
  return imout;
}
#undef MY_LUT_TYPE
#undef t_MY_LUT_TYPE
#include "u32_undef.h"


IMAGE *outeredge(IMAGE *ilbl, int graph)
{
  switch (GetImDataType(ilbl)){

  case t_UINT32: 
    return u32_outeredge(ilbl, graph);
    break;

  default:
    (void)sprintf(buf, "ERROR in outeredge(IMAGE *ilbl, int graph): \
                invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
}

#include "u32_def.h"
#define MY_LUT_TYPE UINT32
#define t_MY_LUT_TYPE t_UINT32
IMAGE *u32_outercontour(IMAGE *ilbl, int graph)
{

  /* Processing based on contour representation:
     only points with change of direction are kept.
     Use MSB for flagging.
     assumes border is set to zero to avoid border overflow.
     Pierre Soille @ jrc.ec.europa.eu (c)
     First 20100930 (for building footprint characterisation)

     based on Moore's contour tracing algorithm with Jacob's condition, see
     http://www.thebigblob.com/moore-neighbor-contour-tracing-algorithm-in-c/
     by Erik Smistad     (see local file moore_tracing.c)
     extended for label images as well as omp speed-up and graph.
     Additional image not actually necessary (coding in MSB is enough)
     but used to return an image with mask of outer edge pixels set to 1 (others to 0).

     http://www.imageprocessingplace.com/downloads_V3/root_downloads/tutorials/contour_tracing_Abeer_George_Ghuneim/moore.html
  */
  G_TYPE *pg;
  IMAGE *lut;
  MY_LUT_TYPE *plut;
  PIX_TYPE *plbl, maxlbl, lbl;
  IMAGE *imout;
  UCHAR *pout;
  int nx=GetImNx(ilbl);
  long int i, npix, pos;  // openMP requires signed loop index
  int box[BOXELEM];
  

  // Defines the neighborhood offset position from current position and the neighborhood
  // position we want to check next if we find a new border at checkLocationNr
  // 1 2 3
  // 0 x 4
  // 7 6 5
  int neighborhood[8][2] = {
    {-1,7},     // red
    {-1-nx,7},  // green
    {-nx,1},    // blue
    {-nx+1,1},  // yellow
    {1,3},      // magenta
    {1+nx,3},   // cyan
    {nx,5},     // white
    {nx-1,5}    // grey    
  };

  if (graph!=8)
    graph=4;
  if (graph==4){
    // - 1 -
    // 0 x 2
    // - 3 -
    neighborhood[0][0] = -1;  // red
    neighborhood[0][1] = 4;
    neighborhood[1][0] = -nx; // green
    neighborhood[1][1] = 1;
    neighborhood[2][0] = 1;   // blue
    neighborhood[2][1] = 2;
    neighborhood[3][0] = nx;  // yellow
    neighborhood[3][1] = 3;
  }

  imout=(IMAGE *)create_image(t_UCHAR, GetImNx(ilbl), GetImNy(ilbl), 1);
  if (imout==NULL)
    return NULL;
  
  BOX_2D;
  u32_framebox(ilbl,box,0);
  
  /* get min & max values */
  pg = min_max(ilbl);
  if (pg == NULL)
    return(NULL);
  maxlbl = pg[1].u32_val;
  free((char *)pg);
  
  lut= (IMAGE *)create_image(t_MY_LUT_TYPE, maxlbl+1, 1, 1);
  if (lut==NULL){
    free_image(imout);
    return NULL;
  }
  plut =(MY_LUT_TYPE *)GetImPtr(lut);
  plbl =(PIX_TYPE *)GetImPtr(ilbl);
  pout =(UCHAR *)GetImPtr(imout);
  npix =GetImNPix(ilbl);

  plut[0]=1; // dummy value to speed-up next loop
  /* first collect first point of each CC in an array
     for subsequent parallel processing */
  for (i=0;i<npix;i++){
    if (plut[plbl[i]]==0){
      plut[plbl[i]]=i;
    }
  }

  /* process one cc at a time */
  //pragma omp parallel default(none)	
  //shared(maxlbl,pout,plbl,plut,graph,neighborhood) private(i,lbl,pos)
  //{
#ifdef OPENMP
#pragma omp parallel for private(lbl, pos) 
#endif
  //  #pragma omp for nowait
  for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
    int checkLocationNr = 1;// The neighbor number of the location we want to check for a
                            // new border point
    long int checkPosition;      // The corresponding absolute array address of checkLocationNr
    int newCheckLocationNr; // Variable that holds the neighborhood position we want to
                            // check if we find a new border at checkLocationNr
    long int startPos = plut[i]; // Set start position
    int counter = 0;        // Counter is used for the jacobi stop criterion
    int counter2 = 0;       // Counter2 is used to determine if the point we have discovered 
                            // is one single point
    int prevCheckLocationNr = 9; // init with dummy direction
    if (startPos!=0){
      lbl=plbl[startPos];
      //IFMSB plbl[startPos]|=PIX_MSB;     // mark pixel as border
      pout[startPos]=9;     // mark pixel as border
      pos=startPos;
  
      // Trace around the neighborhood
      while(1){
	checkPosition = pos + neighborhood[checkLocationNr-1][0];
	newCheckLocationNr = neighborhood[checkLocationNr-1][1];
 
	//IFMSB if( (plbl[checkPosition]&~PIX_MSB) == lbl) { // Next border point found
	if( plbl[checkPosition] == lbl) { // Next border point found
	  if(checkPosition == startPos){


	    pout[pos]=checkLocationNr; // direction of next border point
	  
	    // set to 9 if point of change of direction
	    if (checkLocationNr!=prevCheckLocationNr){
	      pout[pos]=9;
	      pout[checkPosition]=9;
	      prevCheckLocationNr=checkLocationNr;
	    }
	  
	    counter ++;
	    // Stopping criterion (jacob)
	    if(newCheckLocationNr == 1 || counter >= 3) { // Close loop
	      //inside = trueval; // Since we are starting the search at were we first started we must set inside to true
	      break;
	    }
	  }
	  pout[pos]=checkLocationNr; // direction of next border point

	  // set to 9 if point of change of direction
	  if (checkLocationNr!=prevCheckLocationNr){
	    pout[pos]=9;
	    pout[checkPosition]=9;
	    prevCheckLocationNr=checkLocationNr;
	  }
     
	  checkLocationNr = newCheckLocationNr;// Update which neighborhood position we should check next
	  pos = checkPosition;
	  counter2 = 0;    // Reset the counter that keeps track of how many neighbors we have visited
	}
	else{
	  // Rotate clockwise in the neighborhood
	  checkLocationNr = 1 + (checkLocationNr % graph);
	  if(counter2 > graph){
	    // If counter2 is above 8 we have traced around the neighborhood and
	    // therefore the border is a single black pixel and we can exit
	    counter2 = 0;
	    break;
	  }
	  else{
	    counter2 ++;
	  }
	}
      }    
    }
  }
  // }  /* END of parallel region */
  free_image(lut);
 
  return imout;
}
#undef MY_LUT_TYPE
#undef t_MY_LUT_TYPE
#include "u32_undef.h"

IMAGE *outercontour(IMAGE *ilbl, int graph)
{
  switch (GetImDataType(ilbl)){

  case t_UINT32: 
    return u32_outercontour(ilbl, graph);
    break;

  default:
    (void)sprintf(buf, "ERROR in outercontour(IMAGE *ilbl, int graph): \
                invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
}

/*@}*/
