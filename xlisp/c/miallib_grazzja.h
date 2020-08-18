#ifndef   	_MIAL_GRAZZJA_H_
#define   	_MIAL_GRAZZJA_H_

#ifndef OK
#define OK 1
#endif

#ifndef ERROR
#define ERROR -1
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef ZERO
#define ZERO 0
#endif

#ifndef DUMMY
#define DUMMY ZERO
#endif

#define MYBOX_1D(box,wsize)			\
  box[0]=box[1]=wsize;				\
  box[2]=box[3]=box[4]=box[5]=0;

#define MYBOX_2D(box,wsizex,wsizey)		\
  box[0]=box[1]=wsizex;				\
  box[2]=box[3]=wsizey;				\
  box[4]=box[5]=0;

#define MYBOX_3D(box,wsizex,wsizey,wsizez)	\
  box[0]=box[1]=wsizex;				\
  box[2]=box[3]=wsizey;				\
  box[4]=box[5]=wsizez;


#ifndef UNDEF
#define UNDEF 0.
#endif

#ifndef NOT
#define NOT(x) (!(x))
#endif

#ifndef MAX_GRAPH
#define MAX_GRAPH 27
#endif

#ifndef DEFAULT_GRAPH
#define DEFAULT_GRAPH 8
#endif

#ifndef MAXNUMBEROFTEXTURE
#define MAXNUMBEROFTEXTURE 15
#endif

#ifndef MAXNUMBEROFBANDS
#define MAXNUMBEROFBANDS 255
#endif

#ifndef XY2OFFSET
#define XY2OFFSET( x, y, nx ) ((y)*(nx) + (x))
#endif

#ifndef OFFSET2Y
#define OFFSET2Y( ofs, nx ) ((ofs)/(ULGINT)(nx))
#endif

#ifndef OFFSETY2X
#define OFFSETY2X( ofs, y, nx ) ((ofs) - (y)*(nx))
#endif

#ifndef MAX_SCALE
#define MAX_SCALE 100
#endif

#ifndef DIM2D
#define DIM2D 2
#endif

#ifndef DIM1D
#define DIM1D 1
#endif

#ifndef NELEMS
#define NELEMS(t) (sizeof(t) / sizeof *(t))
#endif

#ifndef Fill1D
#define Fill1D(I,x,V)   { int X; \
                            for (X=0; X<(x); X++) \
                                    I[X] = (V);}
#endif

#ifndef Fill2D
#define Fill2D(I,x,y,V) { int X, Y; \
                            for (X=0; X<(x); X++) \
                              for (Y=0; Y<(y); Y++) \
                                    I[X][Y] = (V);}
#endif

#ifndef Free
#define Free(x)   {if((x)!=NULL) {free(x); x=NULL;}}
#endif

/****************************************************************/
#ifndef  CHECK_SET_SEQ_SHIFT_WITH_BORDERS
#define  CHECK_SET_SEQ_SHIFT_WITH_BORDERS(x, y, k, nx, ny, graph)	\
  (graph == 4 &&							\
   ((y<1 && k==0)     || (x<1 && k==1)     ||				\
    (y>=ny-1 && k==2) || (x>=nx-1 && k==3)))				\
    ||	/* or */							\
  (graph == 8 &&							\
   ((y<1 && k==0)            || (x<1 && k==1)                 ||	\
    ((y<1||x>=nx-1) && k==2) || ((y<1||x<1) && k==3)          || 	\
    (x>=nx-1 && k==4)        || (y>=ny-1 && k==5)             ||	\
    ((y>=ny-1||x<1) && k==6) || ((y>=ny-1||x>=nx-1) && k==7)))	       
/* with set_seq_shift:
 *        graph = 4              graph = 8                      graph = 6  
 *      -------------          -------------                  -------------
 *      |   | 0 |   |          | 3 | 0 | 2 |           z-1    |   | 0 |   |    z+1
 *      -------------          -------------          -----   -------------   -----  
 *      | 1 |   | 3 |          | 1 |   | 4 |          | 2 |   | 1 |   | 4 |   | 5 |
 *      -------------          -------------          -----   -------------   -----
 *      |   | 2 |   |          | 6 | 5 | 7 |                  |   | 3 |   | 
 *      -------------          -------------                  -------------
 */
#endif


/****************************************************************/
#ifndef LOOPDN_COPYBUFFER
#define LOOPDN_COPYBUFFER(/*source*/ptr1, /*destination*/ptr2,		\
			  npix, ofs)					\
  LOOPDN(ofs, npix)     *(ptr2+ofs) = *(ptr1+ofs);		
#endif



/****************************************************************/
#ifndef LOOPDN_OVERLAY
#define LOOPDN_OVERLAY(/*source*/ptr1, /*destination*/ptr2,		\
		       npix, ofs, value)				\
  LOOPDN(ofs, npix) {							\
    *ptr2 = ((*ptr1 != value) ? (PIX_TYPE)*ptr1 : (PIX_TYPE)0);	\
    ptr1++, ptr2++;							\
  }
#endif


/****************************************************************/
#ifndef OVERWRITE
#define OVERWRITE(/*source*/pptr1, ptr1, /*destination*/pptr2, ptr2,	\
		  npix,nc,ofs,ic)					\
  for( ic=0; ic<nc; ic++ ) {						\
    ptr1 = pptr1[ic],  ptr2 = pptr2[ic];				\
    LOOPDN_OVERLAY(ptr1, ptr2, npix, ofs, (PIX_TYPE)0/*exact copy*/);	\
  }									
#endif
  

/****************************************************************/
#ifndef LOOPDN_ROUND
#define LOOPDN_ROUND(npix,ofs,ptr1)				\
  LOOPDN(ofs, npix) {						\
    *ptr = (PIX_TYPE)round(*ptr);				\
    ptr++;							\
  }
#endif


/****************************************************************/
#ifndef SETZERO_FRAMEBOX
#undef  SETZERO_FRAMEBOX
#endif
#define SETZERO_FRAMEBOX( imarray, nx, ny, nc, box, unibox, ic )	\
  box[0] = box[2] = unibox;						\
  box[1] = unibox + 1; box[3] = unibox + 1;				\
  box[4] = 0;	box[5] = 0;						\
  for( ic=0; ic<nc; ic++ )						\
    framebox(imarray[ic], box, (PIX_TYPE)0);



#endif 	    /* !MIAL_GRAZZJA_H_ */
