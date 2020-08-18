/**
 * @file   uswilk.c
 * Based on max-tree algorithm described in \cite wilkinson-roerdink2000, see also \cite meijster-wilkinson2002
 * @author (c) Michael Wilkinson, Arnold Meijster, and Jos Roerdink 18-09-2000
 * @date  
 * 
 * @brief  adapted by Pierre Soille on 20/21-09-2000 to fit miallib (see macro functions AddToNeighbour() and Link()).
 * 
 * 
 */

/* This program computes the grey scale attribute opening.
 * The input is a grey scale image (im), and its output is 
 * a grey-scale image, which is stored in an array (parent),
 * which is also used to store the trees needed for Tarjan's
 * algorithm.
 * The time complexity of this algorithm is linear in the 
 * number of pixels.
 *
 * (c) Michael Wilkinson, Arnold Meijster,and Jos Roerdink 18-09-2000
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include "miallib.h"

#ifdef DMALLOC
#include <dmalloc.h>
#endif

#if (defined(XLISP))
extern void gc();
#endif


/** @defgroup group_opclo Opening and closing
 *  Functions dealing with morphological opening and closing.
 *  @{
 */


#define MAXGREYVAL 65536

#define FALSE 0
#define TRUE  1
typedef unsigned char bool;

#define ACTIVE_ROOT -1
#define DONE_ROOT -2

typedef int greyval;   /* used for index to sorted image */
typedef unsigned short byte;

typedef void ***AuxDataType;              /* 2D array of void pointers 
                                             for auxiliary data        */
typedef greyval **Image;                  /* Guess */
typedef byte **ByteImage;                 /* Guess again */


int *SortPixelsext;              /* array storing the sorted pixels */
			
Image imext;                     /* input image */
Image parentext;                 /* output image doubling as storage 
                                    for Tarjan's trees               */
AuxDataType auxdataext;          /* Auxiliary data for attributes    */


/* The type Neighbours2D stores the number of neighbours of a pixel and the
   offsets to neighbours in two arrays. The (first) two indexes of the
   arrays are given by  (x==0) + ((x == width -1) << 1) and
   (y==0) + ((y == height -1) << 1). Thus all non boundary pixels yield
   index [0][0], all non-corner, boundary pixels [0][1], [1][0], [0][2],
   and [2][0]. The corner pixels have indices [1][1], [2][2], [1][2], and
   [2][1].
*/

typedef struct { long number[3][3],         /* number of neighbours  */
		      offsets[3][3][8];     /* offsets to neighbours */
               } Neighbours2D;
              



void Init_4_Neighbours2D ( Neighbours2D *neighbours, int  width)
/* initializes Neighbour2D type for four connectedness */
{ 
  neighbours->number[0][0] = 4;
  neighbours->offsets[0][0][0] = -1;
  neighbours->offsets[0][0][1] = +1;
  neighbours->offsets[0][0][2] = -width;
  neighbours->offsets[0][0][3] = +width;

  neighbours->number[1][0] = 3;
  neighbours->offsets[1][0][0] = +1;
  neighbours->offsets[1][0][1] = -width;
  neighbours->offsets[1][0][2] = +width;

  neighbours->number[2][0] = 3;
  neighbours->offsets[2][0][0] = -1;
  neighbours->offsets[2][0][1] = -width;
  neighbours->offsets[2][0][2] = +width;

  neighbours->number[0][1] = 3;
  neighbours->offsets[0][1][0] = -1;
  neighbours->offsets[0][1][1] = +1;
  neighbours->offsets[0][1][2] = +width;

  neighbours->number[0][2] = 3;
  neighbours->offsets[0][2][0] = -1;
  neighbours->offsets[0][2][1] = +1;
  neighbours->offsets[0][2][2] = -width;

  neighbours->number[1][1] = 2;
  neighbours->offsets[1][1][0] = +1;
  neighbours->offsets[1][1][1] = +width;

  neighbours->number[2][1] = 2;
  neighbours->offsets[2][1][0] = -1;
  neighbours->offsets[2][1][1] = +width;

  neighbours->number[1][2] = 2;
  neighbours->offsets[1][2][0] = +1;
  neighbours->offsets[1][2][1] = -width;

  neighbours->number[2][2] = 2;
  neighbours->offsets[2][2][0] = -1;
  neighbours->offsets[2][2][1] = -width;

}

void Init_8_Neighbours2D ( Neighbours2D *neighbours, int  width)
/* initializes Neighbour2D type for eight connectedness */
{ 
  neighbours->number[0][0] = 8;
  neighbours->offsets[0][0][0] = -1;
  neighbours->offsets[0][0][1] = +1;
  neighbours->offsets[0][0][2] = -width;
  neighbours->offsets[0][0][3] = +width;
  neighbours->offsets[0][0][4] = -width-1;
  neighbours->offsets[0][0][5] = +width+1;
  neighbours->offsets[0][0][6] = -width+1;
  neighbours->offsets[0][0][7] = +width-1;

  neighbours->number[1][0] = 5;
  neighbours->offsets[1][0][0] = +1;
  neighbours->offsets[1][0][1] = -width;
  neighbours->offsets[1][0][2] = +width;
  neighbours->offsets[1][0][3] = -width+1;
  neighbours->offsets[1][0][4] = +width+1;

  neighbours->number[2][0] = 5;
  neighbours->offsets[2][0][0] = -1;
  neighbours->offsets[2][0][1] = -width;
  neighbours->offsets[2][0][2] = +width;
  neighbours->offsets[2][0][3] = -width-1;
  neighbours->offsets[2][0][4] = +width-1;

  neighbours->number[0][1] = 5;
  neighbours->offsets[0][1][0] = -1;
  neighbours->offsets[0][1][1] = +1;
  neighbours->offsets[0][1][2] = +width;
  neighbours->offsets[0][1][3] = +width-1;
  neighbours->offsets[0][1][4] = +width+1;

  neighbours->number[0][2] = 5;
  neighbours->offsets[0][2][0] = -1;
  neighbours->offsets[0][2][1] = +1;
  neighbours->offsets[0][2][2] = -width;
  neighbours->offsets[0][2][3] = -width-1;
  neighbours->offsets[0][2][4] = -width+1;

  neighbours->number[1][1] = 3;
  neighbours->offsets[1][1][0] = +1;
  neighbours->offsets[1][1][1] = +width;
  neighbours->offsets[1][1][2] = +width+1;

  neighbours->number[2][1] = 3;
  neighbours->offsets[2][1][0] = -1;
  neighbours->offsets[2][1][1] = +width;
  neighbours->offsets[2][1][2] = +width-1;

  neighbours->number[1][2] = 3;
  neighbours->offsets[1][2][0] = +1;
  neighbours->offsets[1][2][1] = -width;
  neighbours->offsets[1][2][2] = -width+1;

  neighbours->number[2][2] = 3;
  neighbours->offsets[2][2][0] = -1;
  neighbours->offsets[2][2][1] = -width-1;
  neighbours->offsets[2][2][2] = -width-1;

}

#define NEIGH_INDEX(A,B) (((A)==0) + (( (A)==(B-1))<<1))

/* Returns the correct index within the arrays in the Neighbours2D
   struct type. A must be the current coordinate, B the corresponding
   array dimension.
*/
 
/********************* IMAGE I/O ********************************** */

AuxDataType CreateAuxData ( int width, int height )
/* initialize auxiliary data array */
{ AuxDataType auxdata;
  void **buf;
  int i;
#if (defined(XLISP))
    gc();
#endif
  auxdata = (AuxDataType)malloc (height*sizeof(void **));
  buf = (void **)calloc (width*height, sizeof(void *));
  if ((auxdata == NULL) || (buf == NULL))
    { 
      fprintf (stderr, "fatal error: Malloc failed in  CreateAuxData (height=%d, width=%d)\n", height, width);
      exit (-1);
    }
  auxdata[0] = buf;
  for (i=1; i<height; i++)
    auxdata[i] = auxdata[i-1]+width;
  return auxdata;
}

Image CreateImage (int width, int height)
{
  Image im;
  greyval *buf;
  int i;
#if (defined(XLISP))
    gc();
#endif
  im = (Image)malloc (height*sizeof(greyval *));
  buf = (greyval *)malloc (width*height*sizeof(greyval));
  if ((im == NULL) || (buf == NULL))
    { 
      fprintf (stderr, "fatal error: Malloc failed in CreateImage (height=%d, width=%d)\n", height, width);
      exit (-1);
    }
  im[0] = buf;
  for (i=1; i<height; i++)
    im[i] = im[i-1]+width;
  return im;
}

ByteImage CreateByteImage (int width, int height)
{
  ByteImage im;
  byte *buf;
  int i;
#if (defined(XLISP))
    gc();
#endif
  im = (ByteImage)malloc (height*sizeof(byte *));
  buf = (byte *)malloc (width*height*sizeof(byte));
  if ((im == NULL) || (buf == NULL))
    { 
      fprintf (stderr, "fatal error: Malloc failed in CreateByteImage (height=%d, width=%d)\n", height, width);
      exit (-1);
    }
  im[0] = buf;
  for (i=1; i<height; i++)
    im[i] = im[i-1]+width;
  return im;
}

Image ReadMialibIm(IMAGE *im_miallib, int *width, int *height)
{
  int i, j;
  Image im;
  byte *buf;
  im = CreateImage (*width, *height);
  buf = (byte *)GetImPtr(im_miallib);
  for (i=0; i< *height; i++)
    for (j=0; j< *width; j++, buf++)
      im[i][j]= *buf;
  return im;
}

void CreateImages(IMAGE *im_miallib, int *width, int *height)
{
  // printf("coucou1 in CreateImages\n");
  imext = ReadMialibIm (im_miallib, width, height);
  // printf("coucou2 in CreateImages\n");
  parentext    = CreateImage (*width, *height);
  // printf("coucou3 in CreateImages\n");
  printf("trying to allocate %lu bytes\n", (long unsigned int)(*width)*(*height)*sizeof(int));
  SortPixelsext = (int *)malloc ((*width)*(*height)*sizeof(int));
  // printf("coucou4 in CreateImages\n");
  return;
}

void FreeImages()
{
  free(auxdataext[0]);
  free(auxdataext);
  free(parentext[0]);
  free(parentext);
  free(imext[0]);
  free(imext);
  free(SortPixelsext);
}

void WriteMialibIm(Image im, IMAGE *im_miallibout, int width, int height)
{
  byte *buf;
  int i, j;
  buf = (byte *)GetImPtr(im_miallibout);
  for (i=0; i<height; i++)
    for (j=0; j<width; j++)
      *buf++ = im[i][j];
  return;
}

/*************** Grey Scale Attribute Opening *****************/
/************************  and Closing  ***********************/


void PixelSortforClosing (int size, greyval *im, int *SortPixels, int **idx)
/* Radix-sorts pixels in array im, and stores the results
   SortPixels. The index array idx is returned containing pointers
   idx[gv] to the ENDS of the sections containing pixels with grey 
   level gv in array SortPixels.
*/      
{
  int i, s, *hist= malloc(MAXGREYVAL * sizeof(int));
  greyval *tmp;
  /* first, we build a histogram */
  for (i=0; i<MAXGREYVAL; i++)
    hist[i] = 0;
  for (i=0, tmp=im; i<size; i++, tmp++)
    hist[*tmp]++;
  /* Now we compute offsets for the sorted array */
  s=0;
  for (i=0;i<MAXGREYVAL; i++)
    { 
       idx[i] = SortPixels+s; 
       s+=hist[i];
    }
  /* Now we do the actual sorting */
  for (i=0, tmp=im; i<size; i++, tmp++)
    *(idx[*tmp]++) = i;
  free(hist);
  return;
}


void PixelSortforOpening (int size, greyval *im, int *SortPixels, int **idx)
/* Radix-sorts pixels in array im, and stores the results
   SortPixels. The index array idx is returned containing pointers
   idx[gv] to the ENDS of the sections containing pixels with grey 
   level gv in array SortPixels.
*/      
{
  int i, s, *hist= malloc(MAXGREYVAL * sizeof(int));
  greyval *tmp;
  /* first, we build a histogram */
  for (i=0; i<MAXGREYVAL; i++)
    hist[i] = 0;
  for (i=0, tmp=im; i<size; i++, tmp++)
    hist[*tmp]++;
  /* Now we compute offsets for the sorted array */
  s=0;
  for (i=MAXGREYVAL-1;i>=0; i--)
    { 
       idx[i] = SortPixels+s; 
       s+=hist[i];
    }
  /* Now we do the actual sorting */
  for (i=0, tmp=im; i<size; i++, tmp++)
    *(idx[*tmp]++) = i;
  free(hist);
  return;
}

/* Macro AddToNeighbour is a variant of the "Link" macro below
   AddToNeighbour is used for the FIRST link. This is necessary
   to prevent us having to allocate auxiliary data which are 
   immediately discarded in the first link. Only if no linking
   takes place is a new set of auxiliary data made.
*/    

/*
             auxdata[pixel]=NewAuxData(x,y);\
             auxdata[pixel] =(*MergeAuxData)( auxdata[root], auxdata[pixel] );\
             printf("in AddToNeighbour!!!!!!!!!!!! %d\n", (int)auxdata[pixel]); \
*/


#define AddToNeighbour(p)                                         \
{                                                                 \
   root = (p);                                                    \
   while ( parent[root] >= 0)                                     \
     root = parent[root];                                         \
   newroot = root;                                                \
   if ( (im[root] == im[pixel]) ||                                \
        (parent[root] == ACTIVE_ROOT))                            \
     { if ( parent[root] == DONE_ROOT )                           \
         parent[pixel] = DONE_ROOT;                               \
       else                                                       \
	 { auxdata[pixel] = auxdata[root];                        \
           (*AddToAuxData)( auxdata[pixel],x,y );                 \
	 }                                                        \
       parent[root] = pixel;                                      \
       newroot = pixel;                                           \
     }                                                            \
   else                                                           \
     parent[pixel] = DONE_ROOT;                                   \
   r = (p);                                                       \
   while (r != root)                                              \
     { h=parent[r];                                               \
       parent[r] = newroot;                                       \
       r = h;                                                     \
     }                                                            \
}

/* The new Link function corrected by MW following comments by PS      */

#define Link(p)                                                         \
{                                                                       \
   root = (p);                                  /* Find the root of p */\
   while ( parent[root] >= 0 )                                          \
     root = parent[root];                                               \
   newroot = root;                            /* stores the new root */ \
   if (root!=pixel){                    /* not in same component yet */ \
     if ((im[root] == im[pixel]) ||              /* if same greylevel */\
          (parent[root] == ACTIVE_ROOT))         /*    or not DONE    */\
       {                                         /*  Merge components */\
         if ( parent[root] == DONE_ROOT )        /* if neighbour DONE */\
  	   { if ( parent[pixel] == ACTIVE_ROOT ) /* and pixel isn't   */\
               { DisposeAuxData(auxdata[pixel]); /* this one is DONE  */\
                 parent[pixel] = DONE_ROOT;                             \
	       }                                                        \
           }                                                            \
         else                                                           \
           if (parent[pixel] == DONE_ROOT)     /* if pixel is DONE    */\
             { if (parent[root] == ACTIVE_ROOT) /* so is the neighbour*/\
                 DisposeAuxData(auxdata[root]);                         \
	     }                                                          \
           else                                                         \
             { auxdata[pixel] =                /* otherwise MERGE DATA*/\
               (*MergeAuxData)( auxdata[root], auxdata[pixel] );        \
	     }                                                          \
         parent[root] = pixel;                 /* make link */          \
         newroot = pixel;                      /* update new root */    \
       }                                                                \
     else                           /* done processing this component */\
       if ( parent[pixel] == ACTIVE_ROOT )                              \
       { DisposeAuxData(auxdata[pixel]);                                \
         parent[pixel] = DONE_ROOT;                                     \
       }                                                                \
     r = (p);                                                           \
     while (r != root)                /* path compression starts here */\
       { h=parent[r];                                                   \
         parent[r] = newroot;                                           \
         r = h;                                                         \
       }                                      /* end path compression */\
   } \
}



void GreyAttributeClosing(double lambdaVal, int width, int height, greyval *im, greyval *parent, void **auxdata,  void *(*NewAuxData)(int, int), void (*DisposeAuxData)(void *), void (*AddToAuxData)(void *,int, int), void *(*MergeAuxData)(void *, void*), double (*Attribute)(void *), int graph)      
{


/* double lambdaVal,    /\* threshold on attribute *\/     */
/*                             int width,        /\* image width  *\/  */
/*                             int height,       /\* image height *\/ */
/* 		            greyval *im,      /\* input image  *\/ */
/*                             greyval *parent, /\* output image (also stores  */
/*                                                  Tarjan trees)            *\/ */
/*                             void **auxdata,   /\* auxilliary data for  */
/*                                                  attributes          *\/ */
/* 			    /\* last parameters are function pointers for */
/*                                creating, disposing, adding to, and merging  */
/*                                auxilliary data, and computation of attribute */
/* 			    *\/ */
/*                             void *(*NewAuxData)(int, int),  */
/*                             void (*DisposeAuxData)(void *), */
/*                             void (*AddToAuxData)(void *,int, int), */
/*                             void *(*MergeAuxData)(void *, void*), */
/*                             double (*Attribute)(void *), */
/* 			    int graph */



  int pixel,               /* pixel coordinate in form  width*y+x */
      imsize=width*height,    /* image size */
      linkmade,               /* keeps track of whether current pixel has 
                                 been linked to any neighbour
			      */
      **idx=malloc(MAXGREYVAL * sizeof(int *)), 
                              /* list of pointers to sections of the SORTED
                                 pixels containing pixels of the same 
                                 greylevel */  
      *start, *finish,        /* start and finnish of current section */ 
      curneigh, numneighs,    /* current neighbour and number of neighbours */
      NID_X, NID_Y;           /* neighbour list indexes for x and y */
  long *neigh_offsets;        /* list of offsets to neighbours */
  greyval gval;               /* current grey level */
  Neighbours2D neighbours;    /* neighbour list structure */
  int x, y,                   /* current pixel x and y */
      h, r, root,newroot,     /* auxilliary variables for root finding */
      neigh;                  /* current neighbour */
  greyval *current;           /* pointer to current pixel */


  /* Initialize neighbour list for x-connectivity */
  if (graph==8)
    Init_8_Neighbours2D(&neighbours, width);
  else
    Init_4_Neighbours2D(&neighbours, width);

  /* Sort pixels first */
  PixelSortforClosing (imsize, im, SortPixelsext, idx);
  finish=SortPixelsext;
  /* Forall pixels in increasing grey value and scan line order do Tarjan */
  for (gval=0;gval<MAXGREYVAL;gval ++)
    { /* process one grey level at a time */
      start=finish;          /* start is the previous finnish */ 
      finish=idx[gval];      /* look up finnish of current 
                                greylevel section */
      for (current=start; current<finish; current++)
	{ /* build Tarjan trees for current grey level */
          pixel = *current;
          x=pixel % width;
          y=pixel / width;
          NID_X= NEIGH_INDEX(x, width);
          NID_Y= NEIGH_INDEX(y, height);
          numneighs = neighbours.number[NID_X][NID_Y];
          neigh_offsets = neighbours.offsets[NID_X][NID_Y];
          parent[pixel] = ACTIVE_ROOT;
          linkmade = 0;
          curneigh=0; 
          do { neigh= pixel + (*neigh_offsets);         
               if ( (gval > im[neigh]) || 
                    (( neigh<pixel) && (gval == im[neigh]))
                  )
                 { AddToNeighbour(neigh);
                   linkmade=1;
                 }
               curneigh ++;
               neigh_offsets ++;
	     }
          while ( (curneigh < numneighs) && !linkmade );
          while ( curneigh < numneighs )
	    { neigh= pixel + (*neigh_offsets);
	    /* printf("curneighb=%d\n", curneigh); */
              if ( (gval > im[neigh]) || 
                   (( neigh<pixel) && (gval == im[neigh]))
		   )
		Link (neigh);
              curneigh ++;
              neigh_offsets ++;
	    }
          if (!linkmade)
            auxdata[pixel]=NewAuxData(x,y);
        }
      /* finnished building trees */
    
      for (current=start; current<finish; current++)
        { pixel = *current;
          if ( (parent[pixel] == ACTIVE_ROOT )    /* if not DONE_ROOT */ 
               &&
               ( (*Attribute)(auxdata[pixel]) >= lambdaVal) )
                                        	   /* and criterion met */
            { parent[pixel] = DONE_ROOT;          /* root is DONE_ROOT */
              (*DisposeAuxData)( auxdata[pixel] ); /* get rid of auxilliary
                                                      data */
            }     
	}
      /* Finnished one grey level */
    }
  /* done building trees */

 
  pixel = *(finish-1);                   
  if (parent[pixel] == ACTIVE_ROOT )       /* remove any remaining */
    { parent[pixel] = DONE_ROOT;         /* ACTIVE roots         */
        (*DisposeAuxData)( auxdata[pixel] );
    }     
  
  /* Forall pixels in reverse processing order do Resolve */ 
  for (current=&SortPixelsext[imsize-1]; current>=SortPixelsext; current--)
    parent[*current] = (parent[*current] < 0 ?
			 im[*current] : parent[parent[*current]]);
  free(idx);
  return;
}


void GreyAttributeOpening( double lambdaVal,    /* threshold on attribute */    
                            int width,        /* image width  */ 
                            int height,       /* image height */
		            greyval *im,      /* input image  */
                            greyval *parent, /* output image (also stores 
                                                 Tarjan trees)            */
                            void **auxdata,   /* auxilliary data for 
                                                 attributes          */
			    /* last parameters are function pointers for
                               creating, disposing, adding to, and merging 
                               auxilliary data, and computation of attribute
			    */
                            void *(*NewAuxData)(int, int), 
                            void (*DisposeAuxData)(void *),
                            void (*AddToAuxData)(void *,int, int),
                            void *(*MergeAuxData)(void *, void*),
                            double (*Attribute)(void *),
			    int graph
                          )       
{
  int pixel,               /* pixel coordinate in form  width*y+x */
      imsize=width*height,    /* image size */
      linkmade,               /* keeps track of whether current pixel has 
                                 been linked to any neighbour
			      */
      **idx=malloc(MAXGREYVAL * sizeof(int *)), 
                              /* list of pointers to sections of the SORTED
                                 pixels containing pixels of the same 
                                 greylevel */  
      *start, *finish,        /* start and finnish of current section */ 
      curneigh, numneighs,    /* current neighbour and number of neighbours */
      NID_X, NID_Y;           /* neighbour list indexes for x and y */
  long *neigh_offsets;        /* list of offsets to neighbours */
  greyval gval;               /* current grey level */
  Neighbours2D neighbours;    /* neighbour list structure */
  int x, y,                   /* current pixel x and y */
      h, r, root,newroot,     /* auxilliary variables for root finding */
      neigh;                  /* current neighbour */
  greyval *current;           /* pointer to current pixel */


  /* Initialize neighbour list for x-connectivity */
  if (graph==8)
    Init_8_Neighbours2D(&neighbours, width);
  else
    Init_4_Neighbours2D(&neighbours, width);

  /* Sort pixels first */
  PixelSortforOpening (imsize, im, SortPixelsext, idx);
  finish=SortPixelsext;
  /* Forall pixels in increasing grey value and scan line order do Tarjan */
  for (gval=MAXGREYVAL-1; gval>=0; gval-- )
    { /* process one grey level at a time */
      start=finish;          /* start is the previous finnish */ 
      finish=idx[gval];      /* look up finnish of current 
                                greylevel section */
      for (current=start; current<finish; current++)
	{ /* build Tarjan trees for current grey level */
          pixel = *current;
          x=pixel % width;
          y=pixel / width;
          NID_X= NEIGH_INDEX(x, width);
          NID_Y= NEIGH_INDEX(y, height);
          numneighs = neighbours.number[NID_X][NID_Y];
          neigh_offsets = neighbours.offsets[NID_X][NID_Y];
          parent[pixel] = ACTIVE_ROOT;
          linkmade = 0;
          curneigh=0; 
          do { neigh= pixel + (*neigh_offsets);         
               if ( (gval < im[neigh]) || 
                    (( neigh<pixel) && (gval == im[neigh]))
                  )
                 { AddToNeighbour(neigh);
                   linkmade=1;
                 }
               curneigh ++;
               neigh_offsets ++;
	     }
          while ( (curneigh < numneighs) && !linkmade );
          while ( curneigh < numneighs )
	    { neigh= pixel + (*neigh_offsets);
              if ( (gval < im[neigh]) || 
                   (( neigh<pixel) && (gval == im[neigh]))
		   )
                Link (neigh);
              curneigh ++;
              neigh_offsets ++;
	    }
          if (!linkmade)
            auxdata[pixel]=NewAuxData(x,y);
        }
      /* finnished building trees */

      for (current=start; current<finish; current++)
        { pixel = *current;
          if ( (parent[pixel] == ACTIVE_ROOT )    /* if not DONE_ROOT */ 
               &&
               ( (*Attribute)(auxdata[pixel]) >= lambdaVal) )
                                        	   /* and criterion met */
            { parent[pixel] = DONE_ROOT;          /* root is DONE_ROOT */
              (*DisposeAuxData)( auxdata[pixel] ); /* get rid of auxilliary
                                                      data */
            }     
	}
      /* Finnished one grey level */
    }
  /* done building trees */

 
  pixel = *(finish-1);                   
  if (parent[pixel] == ACTIVE_ROOT )       /* remove any remaining */
    { parent[pixel] = DONE_ROOT;         /* ACTIVE roots         */
        (*DisposeAuxData)( auxdata[pixel] );
    }     
  
  /* Forall pixels in reverse processing order do Resolve */ 
  for (current=&SortPixelsext[imsize-1]; current>=SortPixelsext; current--)
    parent[*current] = (parent[*current] < 0 ?
			 im[*current] : parent[parent[*current]]);
  free(idx);
  return;
}

/********************** Attribute functions ***************************/

/**********************************************************************/
/* typedefs and functions for surface area openings and closings */
/**********************************************************************/

typedef struct { int area;
               } SurfaceData;

void *NewSurfaceData( int x, int y )
{ 
  SurfaceData *surfacedata = (SurfaceData *) malloc(sizeof(SurfaceData));
  surfacedata->area=1;
  /* fprintf(stderr, "surfacedata in alloc=%d\n", (int) surfacedata); */
  return ((void *) surfacedata);    
}

void DisposeSurfaceData( void *surfacedata )
{
  if (surfacedata!=NULL){
    /* fprintf(stderr, "surfacedata in free=%d\n", (int) surfacedata); */
    free(surfacedata);
    surfacedata=NULL;
  }
  /* else
     fprintf(stderr, "NULL surfacedata in free=%d\n", (int) surfacedata); */
    
}

void AddToSurfaceData( void *surfacedata, int x, int y )
{
  SurfaceData *surfdata = (SurfaceData *) surfacedata;
  surfdata->area ++;
}

void *MergeSurfaceData ( void *rootdata, void *pixeldata )
{ SurfaceData *rootinert = (SurfaceData *) rootdata,
              *pixelinert = (SurfaceData *) pixeldata;
  pixelinert->area += rootinert->area;
  if(rootdata==NULL)
    fprintf(stderr, "NULL rootdata in free=%ld !!!!\n", ( long int) rootdata);
  /* free(rootdata);   replaced by following line */
  DisposeSurfaceData(rootdata);
  return pixeldata;
}

double SurfaceAttribute ( void *pixeldata )
{ SurfaceData *surface = pixeldata;
  return  ((double)(surface->area));
}


/**********************************************************************/
/*  Typedefs and functions for Minimum Enclosed Rectangle attributes  */
/**********************************************************************/

typedef struct { int x_min,        
                     x_max, 
                     y_min, 
                     y_max; 
               } EnclRectData;

void *NewEnclRectData( int x, int y )
{ EnclRectData *rectdata = (EnclRectData *)malloc(sizeof(EnclRectData));
  rectdata->x_min=x;
  rectdata->x_max=x;
  rectdata->y_min=y;
  rectdata->y_max=y;
  return ((void *) rectdata);    
}

void DisposeEnclRectData( void *rectdata )
{   
  if (rectdata!=NULL){
    free(rectdata);
    rectdata=NULL;
  }
}

void AddToEnclRectData( void *rectdata, int x, int y )
{ EnclRectData *rootdata = (EnclRectData *) rectdata;
  rootdata->x_min = (rootdata->x_min > x ) ? x : rootdata->x_min;
  rootdata->x_max = (rootdata->x_max < x ) ? x : rootdata->x_max;
  rootdata->y_min = (rootdata->y_min > y ) ? y : rootdata->y_min;
  rootdata->y_max = (rootdata->y_max < y ) ? y : rootdata->y_max;
  return; 
}

void *MergeEnclRectData( void *rootdata, void *pixeldata )
{ EnclRectData *rootrect = (EnclRectData *) rootdata,
               *pixelrect = (EnclRectData *) pixeldata;
  pixelrect->x_min = (pixelrect->x_min > rootrect->x_min) ?
                          rootrect->x_min : pixelrect->x_min;
  pixelrect->x_max = (pixelrect->x_max < rootrect->x_max) ?
                          rootrect->x_max : pixelrect->x_max;
  pixelrect->y_min = (pixelrect->y_min > rootrect->y_min) ?
                          rootrect->y_min : pixelrect->y_min;
  pixelrect->y_max = (pixelrect->y_max < rootrect->y_max) ?
                          rootrect->y_max : pixelrect->y_max;  
  DisposeEnclRectData(rootdata);
  /* free(rootdata); */
  return pixeldata;
}

double EnclRectAreaAttribute( void *pixeldata )
{ EnclRectData *rectdata = pixeldata;
  
  return (double) ((rectdata->x_max - rectdata->x_min + 1)
                   *(rectdata->y_max - rectdata->y_min + 1)
                );
}

double EnclRectDiagAttribute( void *pixeldata )
{ EnclRectData *rectdata = pixeldata;
  
  return (double) ( (rectdata->x_max - rectdata->x_min + 1)*
                    (rectdata->x_max - rectdata->x_min + 1) +

                    (rectdata->y_max - rectdata->y_min + 1)*
                    (rectdata->y_max - rectdata->y_min + 1)
                  );
}

/**********************************************************************/
/* typedefs and functions for moment of inertia openings and closings */
/**********************************************************************/

typedef struct { int area;
                 double sum_x, sum_y, 
		        sum_x2, sum_y2;    
               } InertiaData;

void *NewInertiaData( int x, int y )
{ 
  InertiaData *inertdata = (InertiaData *) malloc(sizeof(InertiaData));
  inertdata->area=1;
  inertdata->sum_x=x;
  inertdata->sum_y=y;
  inertdata->sum_x2=x*x;
  inertdata->sum_y2=y*y;
  return ((void *) inertdata);    
}

void DisposeInertiaData( void *inertdata )
{
  free(inertdata);
}

void AddToInertiaData( void *inertiadata, int x, int y )
{
  InertiaData *inertdata = (InertiaData *) inertiadata;
  inertdata->area ++;
  inertdata->sum_x += x;
  inertdata->sum_y += y;
  inertdata->sum_x2 += x*x;
  inertdata->sum_y2 += y*y;
}

void *MergeInertiaData( void *rootdata, void *pixeldata )
{ InertiaData *rootinert = (InertiaData *) rootdata,
              *pixelinert = (InertiaData *) pixeldata;
  pixelinert->area += rootinert->area;
  pixelinert->sum_x += rootinert->sum_x;
  pixelinert->sum_y += rootinert->sum_y;
  pixelinert->sum_x2 += rootinert->sum_x2;
  pixelinert->sum_y2 += rootinert->sum_y2; 
  free(rootdata);
  return pixeldata;
}

double InertiaAttribute ( void *pixeldata )
{ InertiaData *inert = pixeldata;
  return  inert->sum_x2 + inert->sum_y2 - 
          (inert->sum_x * inert->sum_x + 
           inert->sum_y * inert->sum_y) /(double)(inert->area)
          + (double)inert->area / 6.0;
}


/*********************************************************************/

IMAGE *attribute(IMAGE *im_miallib, int type, int oporclo, double lambdaVal, int graph)
{
  int width, height;
  IMAGE *im_miallibout;

  width=GetImNx(im_miallib);
  height=GetImNy(im_miallib);

  CreateImages (im_miallib, &width, &height);
  auxdataext=CreateAuxData(width,height);

  im_miallibout=(IMAGE *)create_image(5, width, height, 1);

  if (im_miallibout==NULL){
    fprintf(stderr, "Not enough memorey in *attribute\n");
    FreeImages();
    return NULL;
  }

  switch (type){
  case 0:  /* surface area */
      if (oporclo==0){
         GreyAttributeOpening (lambdaVal, width, height, imext[0], parentext[0], 
                        auxdataext[0], 
                        NewSurfaceData, DisposeSurfaceData, 
                        AddToSurfaceData, MergeSurfaceData,
                        SurfaceAttribute, graph);
      }
      else{
         GreyAttributeClosing (lambdaVal, width, height, imext[0], parentext[0], 
                        auxdataext[0], 
                        NewSurfaceData, DisposeSurfaceData, 
                        AddToSurfaceData, MergeSurfaceData,
                        SurfaceAttribute, graph);
      }
      break;
  case 1: /* Inertia */
      if (oporclo==0){
         GreyAttributeOpening (lambdaVal, width, height, imext[0], parentext[0], 
                        auxdataext[0], 
                        NewInertiaData, DisposeInertiaData, 
                        AddToInertiaData, MergeInertiaData,
                        InertiaAttribute, graph);
      }
      else{
         GreyAttributeClosing (lambdaVal, width, height, imext[0], parentext[0], 
                        auxdataext[0], 
                        NewInertiaData, DisposeInertiaData, 
                        AddToInertiaData, MergeInertiaData,
                        InertiaAttribute, graph);
      }
      break;
  case 2: /* Area of Enclose Rectangle */
      if (oporclo==0){
         GreyAttributeOpening (lambdaVal, width, height, imext[0], parentext[0], 
                        auxdataext[0], 
                        NewEnclRectData, DisposeEnclRectData, 
                        AddToEnclRectData, MergeEnclRectData,
                        EnclRectAreaAttribute, graph);
      }
      else{
         GreyAttributeClosing (lambdaVal, width, height, imext[0], parentext[0], 
                        auxdataext[0], 
                        NewEnclRectData, DisposeEnclRectData, 
                        AddToEnclRectData, MergeEnclRectData,
                        EnclRectAreaAttribute, graph);
      }
      break;
  case 3: /* Length of diagonal of Enclose Rectangle */
      if (oporclo==0){
         GreyAttributeOpening (lambdaVal, width, height, imext[0], parentext[0], 
                        auxdataext[0], 
                        NewEnclRectData, DisposeEnclRectData, 
                        AddToEnclRectData, MergeEnclRectData,
                        EnclRectAreaAttribute, graph);
      }
      else{
         GreyAttributeClosing (lambdaVal, width, height, imext[0], parentext[0], 
                        auxdataext[0], 
                        NewEnclRectData, DisposeEnclRectData, 
                        AddToEnclRectData, MergeEnclRectData,
                        EnclRectDiagAttribute, graph);
      }
      break;
    default:
	 (void)sprintf(buf, "ERROR in Attribute(): \
    			     invalid attribute type\n"); errputstr(buf);
  }
  WriteMialibIm(parentext, im_miallibout, width, height);
  FreeImages();
  return im_miallibout;

}










/* This program computes the grey scale area opening.
 * The input is a grey scale image (im), and its output is 
 * a grey-scale image (opening).
 * The time complexity of this algorithm is linear in the 
 * number of pixels.
 *
 * (c) Arnold Meijster, Michael Wilkinson
 */


ByteImage imextarea;
Image openingext;



ByteImage ReadMialibImArea(IMAGE *im_miallib, int *width, int *height)
{
  int i, j;
  ByteImage im;
  byte *buf;
  im = CreateByteImage (*width, *height);
  buf = (byte *)GetImPtr(im_miallib);
  for (i=0; i< *height; i++)
    for (j=0; j< *width; j++, buf++)
      im[i][j]= *buf;
  return im;
}

void CreateImagesArea(IMAGE *im_miallib, int *width, int *height)
{
  imextarea = ReadMialibImArea (im_miallib, width, height);
  openingext    = CreateImage (*width, *height);
  SortPixelsext = (int *)malloc ((*width)*(*height)*sizeof(int));
  return;
}


/*************** Grey Scale Area Opening *****************/

void PixelUpSort(int size, byte *im, int *SortPixels)
{
  int i, s, *idx[MAXGREYVAL], hist[MAXGREYVAL];
  byte *tmp;
  /* first, we build a histogram */
  for (i=0; i<MAXGREYVAL; i++)
    hist[i] = 0;
  for (i=0, tmp=im; i<size; i++, tmp++)
    hist[*tmp]++;
  /* Now we compute offsets for the sorted array */
  s=0;
  for (i=0;i<MAXGREYVAL; i++)
    { 
       idx[i] = SortPixels+s; 
       s+=hist[i];
    }
  /* Now we do the actual sorting */
  for (i=0, tmp=im; i<size; i++, tmp++)
    *(idx[*tmp]++) = i;
  return;
}

void PixelDownSort(int size, byte *im, int *SortPixels)
{
  int i, s, *idx[MAXGREYVAL], hist[MAXGREYVAL];
  byte *tmp;
  /* first, we build a histogram */
  for (i=0; i<MAXGREYVAL; i++)
    hist[i] = 0;
  for (i=0, tmp=im; i<size; i++, tmp++)
    hist[*tmp]++;
  /* Now we compute offsets for the sorted array */
  s=0;
  for (i=MAXGREYVAL-1;i>=0; i--)
    { 
       idx[i] = SortPixels+s; 
       s+=hist[i];
    }
  /* Now we do the actual sorting */
  for (i=0, tmp=im; i<size; i++, tmp++)
    *(idx[*tmp]++) = i;
  return;
}

#define Link1(p)                                                  \
{                                                                 \
          root = (p);                                             \
          while (opening[root] >= 0)                              \
            root = opening[root];                                 \
          newroot = root;                                         \
          if (root!=pixel)  {                                      \
            if ((-opening[root]) < lambdaVal)                         \
            {                                                     \
              opening[pixel] += opening[root];                    \
              opening[root] = pixel;                              \
              newroot = pixel;                                    \
            }                                                     \
            else                                                  \
              opening[pixel] = -lambdaVal;                           \
          } \
          r = (p);                                                \
          while (r != root)                                       \
            {                                                     \
              h=opening[r];                                       \
              opening[r] = newroot;                               \
              r = h;                                              \
            }                                                     \
}

#define Link2(p)                                                   \
{                                                                 \
          root = (p);                                             \
          while (opening[root] >= 0)                              \
            root = opening[root];                                 \
          newroot = root;                                         \
          if (root!=pixel)  {                                      \
            if ((im[root] == im[pixel]) || ((-opening[root]) < lambdaVal)) \
            {                                                     \
              opening[pixel] += opening[root];                    \
              opening[root] = pixel;                              \
              newroot = pixel;                                    \
            }                                                     \
            else                                                  \
              opening[pixel] = -lambdaVal;                           \
          } \
          r = (p);                                                \
          while (r != root)                                       \
            {                                                     \
              h=opening[r];                                       \
              opening[r] = newroot;                               \
              r = h;                                              \
            }                                                     \
}

IMAGE *GreyAreaOpening4(IMAGE *im_miallib, int lambdaVal)
{

  IMAGE *im_miallibout;
  int width=GetImNx(im_miallib);
  int height=GetImNy(im_miallib);
  greyval *opening;
  byte *im;
  
  int i, pixel,imsize=width*height, uplimit=(height-1)*width-1;
  greyval gval;
  int x, h, r, root,newroot,neigh;
  greyval *current;

  CreateImagesArea(im_miallib, &width, &height);
  opening=openingext[0];
  im=imextarea[0];
  
  im_miallibout=create_image(5, width, height, 1);

  if (im_miallibout==NULL){
    fprintf(stderr, "Not enough memorey in GreyAreaOpening4\n");
    free(openingext[0]);
    free(openingext);
    free(imextarea[0]);
    free(imextarea);
    return NULL;
  }


  /* Sort pixels first */
  PixelDownSort (imsize, im, SortPixelsext);
  /* Forall pixels in increasing grey value and scan line order do Tarjan */
  for (i=0,current=SortPixelsext; i<imsize; i++, current++)
    {
      pixel = *current;
      gval = im[pixel];
      x=pixel % width;
      opening[pixel] = -1;
      if ((x != 0) && (gval <= im[neigh=(pixel-1)]))
        Link2 (neigh);
      if ((x < width-1) && (gval < im[neigh=(pixel+1)]))
        Link1 (neigh);
      if ((pixel >= width) && (gval <= im[neigh=(pixel-width)]))
        Link2(neigh);
      if ((pixel <= uplimit) && (gval < im[neigh=(pixel+width)]))
        Link1(neigh);
    }
  /* Forall pixels in (increasing greyscale,scan line) order do Resolve */ 
  for (current=&SortPixelsext[imsize-1]; current>=SortPixelsext; current--)
    opening[*current] = (opening[*current] < 0 ?
                         im[*current] : opening[opening[*current]]);

  WriteMialibIm(openingext, im_miallibout, width, height);
  
  free(openingext[0]);
  free(openingext);
  free(imextarea[0]);
  free(imextarea);
  free(SortPixelsext);

  return im_miallibout;
}


IMAGE *GreyAreaOpening8(IMAGE *im_miallib, int lambdaVal)
{

  IMAGE *im_miallibout;
  int width=GetImNx(im_miallib);
  int height=GetImNy(im_miallib);
  greyval *opening;
  byte *im;
  
  int i, pixel,imsize=width*height, uplimit=(height-1)*width-1;
  greyval gval;
  int x, h, r, root,newroot,neigh;
  greyval *current;

  int n5=-1-width;
  int n6=-width+1;
  int n7=width-1;
  int n8=width+1;

  // printf("coucou1 in GreyAreaOpening8\n");
  CreateImagesArea(im_miallib, &width, &height);
  // printf("coucou2 in GreyAreaOpening8\n");
  opening=openingext[0];
  im=imextarea[0];
  
  // printf("coucou3 in GreyAreaOpening8\n");
  im_miallibout=(IMAGE *)create_image(5, width, height, 1);

  //printf("coucou4 in GreyAreaOpening8\n");
  if (im_miallibout==NULL){
    fprintf(stderr, "Not enough memory in GreyAreaOpening8\n");
    free(openingext[0]);
    free(openingext);
    free(imextarea[0]);
    free(imextarea);
    return NULL;
  }


  /* Sort pixels first */
  PixelDownSort (imsize, im, SortPixelsext);
  /* Forall pixels in increasing grey value and scan line order do Tarjan */
  for (i=0,current=SortPixelsext; i<imsize; i++, current++)
    {
      pixel = *current;
      gval = im[pixel];
      x=pixel % width;
      opening[pixel] = -1;

      if (x!=0){
	if (gval <= im[neigh=(pixel-1)])
          Link2 (neigh);
	if ((pixel>width) && (gval <= im[neigh=(pixel+n5)]))
          Link2 (neigh);
	if ((pixel <= uplimit) && (gval < im[neigh=(pixel+n7)]))
          Link1 (neigh);
      }

      if (x<n7){
	if (gval < im[neigh=(pixel+1)])
          Link1 (neigh);
	if ((pixel>width) && (gval <= im[neigh=(pixel+n6)]))
          Link2 (neigh);
	if ((pixel < uplimit) && (gval < im[neigh=(pixel+n8)]))
          Link1 (neigh);
      }


      if ((pixel >= width) && (gval <= im[neigh=(pixel-width)]))
        Link2(neigh);
      if ((pixel <= uplimit) && (gval < im[neigh=(pixel+width)]))
        Link1(neigh);
	
	
    }
  /* Forall pixels in (increasing greyscale,scan line) order do Resolve */ 
  for (current=&SortPixelsext[imsize-1]; current>=SortPixelsext; current--)
    opening[*current] = (opening[*current] < 0 ?
                         im[*current] : opening[opening[*current]]);

  WriteMialibIm(openingext, im_miallibout, width, height);
  
  free(openingext[0]);
  free(openingext);
  free(imextarea[0]);
  free(imextarea);
  free(SortPixelsext);

  return im_miallibout;
}



IMAGE *GreyAreaOpening(IMAGE *im_miallib, int lambdaVal, int graph)
{
  if (graph==4)
    return (GreyAreaOpening4 (im_miallib, lambdaVal));
  else if (graph==8)
    return (GreyAreaOpening8 (im_miallib, lambdaVal));
  else
    fprintf (stderr, "GreyAreaOpening: graph must be either 4 or 8\n");
  return NULL;
}
    



IMAGE *GreyAreaClosing4(IMAGE *im_miallib, int lambdaVal)
{

  IMAGE *im_miallibout;
  int width=GetImNx(im_miallib);
  int height=GetImNy(im_miallib);
  greyval *opening;
  byte *im;
  
  int i, pixel,imsize=width*height, uplimit=(height-1)*width-1;
  greyval gval;
  int x, h, r, root,newroot,neigh;
  greyval *current;


  CreateImagesArea(im_miallib, &width, &height);
  opening=openingext[0];
  im=imextarea[0];
  
  im_miallibout=create_image(5, width, height, 1);


  if (im_miallibout==NULL){
    fprintf(stderr, "Not enough memorey in GreyAreaClosing4\n");
    free(openingext[0]);
    free(openingext);
    free(imextarea[0]);
    free(imextarea);
    return NULL;
  }


  /* Sort pixels first */
  PixelUpSort (imsize, im, SortPixelsext);
  /* Forall pixels in increasing grey value and scan line order do Tarjan */
  for (i=0,current=SortPixelsext; i<imsize; i++, current++)
    {
      pixel = *current;
      gval = im[pixel];
      x=pixel % width;
      opening[pixel] = -1;

      if ((x != 0) && (gval >= im[neigh=(pixel-1)]))
        Link2 (neigh);
      if ((x < width-1) && (gval > im[neigh=(pixel+1)]))
        Link1 (neigh);
      if ((pixel >= width) && (gval >= im[neigh=(pixel-width)]))
        Link2(neigh);
      if ((pixel <= uplimit) && (gval > im[neigh=(pixel+width)]))
        Link1(neigh);

    }
  /* Forall pixels in (increasing greyscale,scan line) order do Resolve */ 
  for (current=&SortPixelsext[imsize-1]; current>=SortPixelsext; current--)
    opening[*current] = (opening[*current] < 0 ?
                         im[*current] : opening[opening[*current]]);

  WriteMialibIm(openingext, im_miallibout, width, height);
  
  free(openingext[0]);
  free(openingext);
  free(imextarea[0]);
  free(imextarea);
  free(SortPixelsext);

  return im_miallibout;
}

IMAGE *GreyAreaClosing8(IMAGE *im_miallib, int lambdaVal)
{
  IMAGE *im_miallibout;
  int width=GetImNx(im_miallib);
  int height=GetImNy(im_miallib);
  greyval *opening;
  byte *im;
  
  int i, pixel,imsize=width*height, uplimit=(height-1)*width-1;
  greyval gval;
  int x, h, r, root,newroot,neigh;
  greyval *current;

  int n5=-1-width;
  int n6=-width+1;
  int n7=width-1;
  int n8=width+1;

  CreateImagesArea(im_miallib, &width, &height);
  opening=openingext[0];
  im=imextarea[0];
  
  im_miallibout=create_image(5, width, height, 1);

  if (im_miallibout==NULL){
    fprintf(stderr, "Not enough memory in GreyAreaClosing8\n");
    free(openingext[0]);
    free(openingext);
    free(imextarea[0]);
    free(imextarea);
    return NULL;
  }

  /* Sort pixels first */
  PixelUpSort (imsize, im, SortPixelsext);
  /* Forall pixels in increasing grey value and scan line order do Tarjan */
  for (i=0,current=SortPixelsext; i<imsize; i++, current++){
      pixel = *current;
      gval = im[pixel];
      x=pixel % width;
      opening[pixel] = -1;


      if (x!=0){
	if (gval >= im[neigh=(pixel-1)])
          Link2 (neigh);
	if ((pixel>width) && (gval >= im[neigh=(pixel+n5)]))
          Link2 (neigh);
	if ((pixel <= uplimit) && (gval > im[neigh=(pixel+n7)]))
          Link1 (neigh);
      }

      if (x<n7){
	if (gval > im[neigh=(pixel+1)])
          Link1 (neigh);
	if ((pixel>width) && (gval >= im[neigh=(pixel+n6)]))
          Link2 (neigh);
	if ((pixel < uplimit) && (gval > im[neigh=(pixel+n8)]))
          Link1 (neigh);
      }


      if ((pixel >= width) && (gval >= im[neigh=(pixel-width)]))
        Link2(neigh);
      if ((pixel <= uplimit) && (gval > im[neigh=(pixel+width)]))
        Link1(neigh);
	

    }
  /* Forall pixels in (increasing greyscale,scan line) order do Resolve */ 
  for (current=&SortPixelsext[imsize-1]; current>=SortPixelsext; current--)
    opening[*current] = (opening[*current] < 0 ?
                         im[*current] : opening[opening[*current]]);

  WriteMialibIm(openingext, im_miallibout, width, height);
  
  free(openingext[0]);
  free(openingext);
  free(imextarea[0]);
  free(imextarea);
  free(SortPixelsext);

  return im_miallibout;
}


IMAGE *GreyAreaClosing(IMAGE *im_miallib, int lambdaVal, int graph)
{
  if (graph==4)
    return (GreyAreaClosing4 (im_miallib, lambdaVal));
  else if (graph==8)
    return (GreyAreaClosing8 (im_miallib, lambdaVal));
  else
    fprintf (stderr, "GreyAreaClosing: graph must be either 4 or 8\n");
  return NULL;
}
    




/*********************************************************************/
/*********************************************************************/
/****************        VERSION WITH ROI         ********************/
/*********************************************************************/
/*********************************************************************/

int MAXROIVAL  =  65534;   /* pixels outside ROI should be at 255,
			 image border MUST be at 255 !!! */
int OUTROI     = 65535;


extern ERROR_TYPE set_seq_shift();

void GreyAttributeClosingROI ( double lambdaVal,    /* threshold on attribute */    
                            int width,        /* image width  */ 
                            int height,       /* image height */
		            greyval *im,      /* input image  */
                            greyval *parent, /* output image (also stores 
                                                 Tarjan trees)            */
                            void **auxdata,   /* auxilliary data for 
                                                 attributes          */
			    /* last parameters are function pointers for
                               creating, disposing, adding to, and merging 
                               auxilliary data, and computation of attribute
			    */
                            void *(*NewAuxData)(int, int), 
                            void (*DisposeAuxData)(void *),
                            void (*AddToAuxData)(void *,int, int),
                            void *(*MergeAuxData)(void *, void*),
                            double (*Attribute)(void *),
			    int graph
                          )       
{
  int pixel,               /* pixel coordinate in form  width*y+x */
      imsize=width*height,    /* image size */
      linkmade,               /* keeps track of whether current pixel has 
                                 been linked to any neighbour
			      */
      **idx=malloc(MAXGREYVAL * sizeof(int *)), 
                              /* list of pointers to sections of the SORTED
                                 pixels containing pixels of the same 
                                 greylevel */  
      *start, *finish;        /* start and finnish of current section */ 
  greyval gval;               /* current grey level */
  int x, y,                   /* current pixel x and y */
      h, r, root,newroot,     /* auxilliary variables for root finding */
      neigh;                  /* current neighbour */
  greyval *current;           /* pointer to current pixel */

  long int shift[27];         /* array if offsets to neighbours */
  int k;



  /* take graph into account */
  if (set_seq_shift(width, height, 1, graph, shift) == ERROR)
    return ;

  

  /* Sort pixels first */
  PixelSortforClosing (imsize, im, SortPixelsext, idx);
  finish=SortPixelsext;
  /* Forall pixels in increasing grey value and scan line order do Tarjan */
  for (gval=0;gval<OUTROI;gval ++)
    { /* process one grey level at a time */
      start=finish;          /* start is the previous finnish */ 
      finish=idx[gval];      /* look up finnish of current 
                                greylevel section */
      for (current=start; current<finish; current++)
	{ /* build Tarjan trees for current grey level */
	  pixel = *current;
          x=pixel % width;
          y=pixel / width;
          parent[pixel] = ACTIVE_ROOT;
          linkmade = 0;
	  for(k=0; k<graph; k++){
	    neigh= pixel + shift[k];
	    if (im[neigh]==OUTROI)
	      continue;        
	    if ( (gval > im[neigh]) || 
		 (( neigh<pixel) && (gval == im[neigh]))
		 )
	      { AddToNeighbour(neigh);
	      linkmade=1;
	      k++;
	      break;  /* cf. was a while for Michael */
	      }
	  }
	  
          
          while ( k < graph )
	    {
	      neigh= pixel + shift[k];
	      if (im[neigh]==OUTROI)
	        continue;        
	    
             /* printf("curneighb=%d\n", curneigh); */
              if ( (gval > im[neigh]) || 
                   (( neigh<pixel) && (gval == im[neigh]))
		   )
		Link (neigh);
              k ++;
	    }
          if (!linkmade)
            auxdata[pixel]=NewAuxData(x,y);
        }
      /* finnished building trees */
    
      for (current=start; current<finish; current++)
        { pixel = *current;
          if ( (parent[pixel] == ACTIVE_ROOT )    /* if not DONE_ROOT */ 
               &&
               ( (*Attribute)(auxdata[pixel]) >= lambdaVal) )
                                        	   /* and criterion met */
            { parent[pixel] = DONE_ROOT;          /* root is DONE_ROOT */
              (*DisposeAuxData)( auxdata[pixel] ); /* get rid of auxilliary
                                                      data */
            }     
	}
      /* Finnished one grey level */
    }
  /* done building trees */

 
  pixel = *(finish-1);                   
  if (parent[pixel] == ACTIVE_ROOT )       /* remove any remaining */
    { parent[pixel] = DONE_ROOT;         /* ACTIVE roots         */
        (*DisposeAuxData)( auxdata[pixel] );
    }     
  
  /* Forall pixels in reverse processing order do Resolve */ 
  for (current=&SortPixelsext[imsize-1]; current>=SortPixelsext; current--)
    parent[*current] = (parent[*current] < 0 ?
			 im[*current] : parent[parent[*current]]);
  free(idx);
  return;
}




IMAGE *GreyAreaOpening4ROI(IMAGE *im_miallib, int lambdaVal)
{

  IMAGE *im_miallibout;
  int width=GetImNx(im_miallib);
  int height=GetImNy(im_miallib);
  greyval *opening;
  byte *im;
  
  int i, pixel,imsize=width*height;
  greyval gval, val;
  int h, r, root,newroot,neigh;
  greyval *current;

/*   int n5=-1-width; */
/*   int n6=-width+1; */
/*   int n7=width-1; */
/*   int n8=width+1; */

  CreateImagesArea(im_miallib, &width, &height);
  opening=openingext[0];
  im=imextarea[0];
  
  im_miallibout=create_image(5, width, height, 1);


  /* Sort pixels first */
  PixelDownSort (imsize, im, SortPixelsext);
  /* Forall pixels in decreasing grey value and scan line order do Tarjan */
  for (i=0,current=SortPixelsext; i<imsize; i++, current++)
    {

      pixel = *current;
      
      opening[pixel] = -1;

      gval = im[pixel];
      if (gval==OUTROI)
	continue;

      val =  im[neigh=(pixel-1)];
      if ((val != OUTROI) && (gval <= val))
	 Link2 (neigh);

      val =  im[neigh=(pixel+1)];
      if ((val != OUTROI) && (gval < val))
	 Link1 (neigh);

      val =  im[neigh=(pixel-width)];
      if ((val != OUTROI) && (gval <= val))
	 Link2 (neigh);

      val =  im[neigh=(pixel+width)];
      if ((val != OUTROI) && (gval < val))
	 Link1 (neigh);



	
    }
  /* Forall pixels in (increasing greyscale,scan line) order do Resolve */ 
  for (current=&SortPixelsext[imsize-1]; current>=SortPixelsext; current--)
    opening[*current] = (opening[*current] < 0 ?
                         im[*current] : opening[opening[*current]]);

  WriteMialibIm(openingext, im_miallibout, width, height);
  
  free(openingext[0]);
  free(openingext);
  free(imextarea[0]);
  free(imextarea);
  free(SortPixelsext);

  return im_miallibout;
}





IMAGE *GreyAreaOpening8ROI(IMAGE *im_miallib, int lambdaVal)
{

  IMAGE *im_miallibout;
  int width=GetImNx(im_miallib);
  int height=GetImNy(im_miallib);
  greyval *opening;
  byte *im;
  
  int i, pixel,imsize=width*height;
  greyval gval, val;
  int h, r, root,newroot,neigh;
  greyval *current;

  int n5=-1-width;
  int n6=-width+1;
  int n7=width-1;
  int n8=width+1;

  CreateImagesArea(im_miallib, &width, &height);
  opening=openingext[0];
  im=imextarea[0];
  
  im_miallibout=(IMAGE *)create_image(5, width, height, 1);


  /* Sort pixels first */
  PixelDownSort (imsize, im, SortPixelsext);
  /* Forall pixels in decreasing grey value and scan line order do Tarjan */
  for (i=0,current=SortPixelsext; i<imsize; i++, current++)
    {

      pixel = *current;
      
      opening[pixel] = -1;

      gval = im[pixel];
      if (gval==OUTROI)
	continue;

      val =  im[neigh=(pixel-1)];
      if ((val != OUTROI) && (gval <= val))
	 Link2 (neigh);

      val =  im[neigh=(pixel+1)];
      if ((val != OUTROI) && (gval < val))
	 Link1 (neigh);

      val =  im[neigh=(pixel-width)];
      if ((val != OUTROI) && (gval <= val))
	 Link2 (neigh);

      val =  im[neigh=(pixel+width)];
      if ((val != OUTROI) && (gval < val))
	 Link1 (neigh);

      val =  im[neigh=(pixel+n5)];
      if ((val != OUTROI) && (gval <= val))
	 Link2 (neigh);

      val =  im[neigh=(pixel+n6)];
      if ((val != OUTROI) && (gval <= val))
	 Link2 (neigh);

      val =  im[neigh=(pixel+n7)];
      if ((val != OUTROI) && (gval < val))
	 Link1 (neigh);

      val =  im[neigh=(pixel+n8)];
      if ((val != OUTROI) && (gval < val))
	 Link1 (neigh);


	
    }
  /* Forall pixels in (increasing greyscale,scan line) order do Resolve */ 
  for (current=&SortPixelsext[imsize-1]; current>=SortPixelsext; current--)
    opening[*current] = (opening[*current] < 0 ?
                         im[*current] : opening[opening[*current]]);

  WriteMialibIm(openingext, im_miallibout, width, height);
  
  free(openingext[0]);
  free(openingext);
  free(imextarea[0]);
  free(imextarea);
  free(SortPixelsext);

  return im_miallibout;
}



IMAGE *GreyAreaOpeningROI(IMAGE *im_miallib, int lambdaVal, int graph)
{
  if (graph==4)
    return (GreyAreaOpening4ROI (im_miallib, lambdaVal));
  else if (graph==8)
    return (GreyAreaOpening8ROI (im_miallib, lambdaVal));
  else
    fprintf (stderr, "GreyAreaOpening: graph must be either 4 or 8\n");
  return NULL;
}
    


IMAGE *GreyAreaClosing4ROI(IMAGE *im_miallib, int lambdaVal)
{

  IMAGE *im_miallibout;
  int width=GetImNx(im_miallib);
  int height=GetImNy(im_miallib);
  greyval *opening;
  byte *im;
  
  int i, pixel,imsize=width*height;
  greyval gval, val;
  int h, r, root,newroot,neigh;
  greyval *current;

/*   int n5=-1-width; */
/*   int n6=-width+1; */
/*   int n7=width-1; */
/*   int n8=width+1; */

  CreateImagesArea(im_miallib, &width, &height);
  opening=openingext[0];
  im=imextarea[0];
  
  im_miallibout=create_image(5, width, height, 1);


  /* Sort pixels first */
  PixelUpSort (imsize, im, SortPixelsext);
  /* Forall pixels in decreasing grey value and scan line order do Tarjan */
  for (i=0,current=SortPixelsext; i<imsize; i++, current++)
    {

      pixel = *current;
      
      opening[pixel] = -1;

      gval = im[pixel];
      if (gval==OUTROI)
	continue;

      val =  im[neigh=(pixel-1)];
      if ((val != OUTROI) && (gval >= val))
	 Link2 (neigh);

      val =  im[neigh=(pixel+1)];
      if ((val != OUTROI) && (gval > val))
	 Link1 (neigh);

      val =  im[neigh=(pixel-width)];
      if ((val != OUTROI) && (gval >= val))
	 Link2 (neigh);

      val =  im[neigh=(pixel+width)];
      if ((val != OUTROI) && (gval > val))
	 Link1 (neigh);

   
	
    }
  /* Forall pixels in (increasing greyscale,scan line) order do Resolve */ 
  for (current=&SortPixelsext[imsize-1]; current>=SortPixelsext; current--)
    opening[*current] = (opening[*current] < 0 ?
                         im[*current] : opening[opening[*current]]);

  WriteMialibIm(openingext, im_miallibout, width, height);
  
  free(openingext[0]);
  free(openingext);
  free(imextarea[0]);
  free(imextarea);
  free(SortPixelsext);

  return im_miallibout;
}


IMAGE *GreyAreaClosing8ROI(IMAGE *im_miallib, int lambdaVal)
{

  IMAGE *im_miallibout;
  int width=GetImNx(im_miallib);
  int height=GetImNy(im_miallib);
  greyval *opening;
  byte *im;
  
  int i, pixel,imsize=width*height;
  greyval gval, val;
  int h, r, root,newroot,neigh;
  greyval *current;

  int n5=-1-width;
  int n6=-width+1;
  int n7=width-1;
  int n8=width+1;

  CreateImagesArea(im_miallib, &width, &height);
  opening=openingext[0];
  im=imextarea[0];
  
  im_miallibout=create_image(5, width, height, 1);


  /* Sort pixels first */
  PixelUpSort (imsize, im, SortPixelsext);
  /* Forall pixels in decreasing grey value and scan line order do Tarjan */
  for (i=0,current=SortPixelsext; i<imsize; i++, current++)
    {

      pixel = *current;
      
      opening[pixel] = -1;

      gval = im[pixel];
      if (gval==OUTROI)
	continue;

      val =  im[neigh=(pixel-1)];
      if ((val != OUTROI) && (gval >= val))
	 Link2 (neigh);

      val =  im[neigh=(pixel+1)];
      if ((val != OUTROI) && (gval > val))
	 Link1 (neigh);

      val =  im[neigh=(pixel-width)];
      if ((val != OUTROI) && (gval >= val))
	 Link2 (neigh);

      val =  im[neigh=(pixel+width)];
      if ((val != OUTROI) && (gval > val))
	 Link1 (neigh);

      val =  im[neigh=(pixel+n5)];
      if ((val != OUTROI) && (gval >= val))
	 Link2 (neigh);

      val =  im[neigh=(pixel+n6)];
      if ((val != OUTROI) && (gval >= val))
	 Link2 (neigh);

      val =  im[neigh=(pixel+n7)];
      if ((val != OUTROI) && (gval > val))
	 Link1 (neigh);

      val =  im[neigh=(pixel+n8)];
      if ((val != OUTROI) && (gval > val))
	 Link1 (neigh);


	
    }
  /* Forall pixels in (increasing greyscale,scan line) order do Resolve */ 
  for (current=&SortPixelsext[imsize-1]; current>=SortPixelsext; current--)
    opening[*current] = (opening[*current] < 0 ?
                         im[*current] : opening[opening[*current]]);

  WriteMialibIm(openingext, im_miallibout, width, height);
  
  free(openingext[0]);
  free(openingext);
  free(imextarea[0]);
  free(imextarea);
  free(SortPixelsext);

  return im_miallibout;
}




IMAGE *GreyAreaClosingROI(IMAGE *im_miallib, int lambdaVal, int graph)
{
  if (graph==4)
    return (GreyAreaClosing4ROI (im_miallib, lambdaVal));
  else if (graph==8)
    return (GreyAreaClosing8ROI (im_miallib, lambdaVal));
  else
    fprintf (stderr, "GreyAreaClosingROI: graph must be either 4 or 8\n");
  return NULL;
}
    

/*@}*/
