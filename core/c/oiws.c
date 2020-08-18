/*
  First Tue Feb 24 2004
*/


/** \addtogroup group_hmtsk
 *  @{
 */




#include <stdio.h>
#include <stdlib.h>
#include "miallib.h"
#include "fifo.h"
#include "pqueue.h"

#define BOX2_2D \
box[0]=box[1]=box[2]=box[3]=2; \
box[4]=box[5]=0

#include "g_def.h"
ERROR_TYPE generic_oiws(IMAGE *im)
{
    /*
      We propose to simulate an order independent flooding based on ordered
      indenpendent thinnings proceeding by increasing intensity levels.
    */
    long int i, j, ofs;
    int del; /* Bool. variable ind. whether a pixel is deletable (1) or not (0) */
    int si;  /* Bool. variable ind. whether a pixel is strictly ind. (1) or not (0) of a given simple ngb */
    int nsi; /* Bool. variable ind. whether a pixel is non-strictly ind. (1) or not (0) of all its simple ngb */
    int cfg; /* Bool. variable ind. whether there is a common foreground neighbour */
    int nsn=0; /* Bool. variable ind. whether there exists a pixel is non-simple ngb */
    int code, codengb; 
    int nx=GetImNx(im);
    long int npix=GetImNPix(im);
    int box[6];
    int homotab[256] = { 0,1,1,0,1,1,1,1,1,1,1,1,0,1,1,0,1,1,0,0,1,1,1,1,0,1,0,1,0,1,1,0,1,\
                       1,0,0,0,1,0,1,1,1,1,1,0,1,1,0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,1,0,1,0,\
                       1,0,1,1,1,1,0,0,1,1,0,1,1,0,0,0,0,0,1,1,1,1,0,0,0,1,0,1,1,0,0,0,0,\
		       0,0,1,0,1,0,0,1,1,0,1,1,0,0,0,0,0,0,1,0,1,0,0,0,1,0,1,1,0,1,0,1,0,\
		       0,0,1,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,1,1,0,1,0,1,0,1,1,0,0,0,0,0,0,\
		       0,0,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,0,0,0,1,0,0,0,\
		       1,1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,\
		       1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1,0};
    int code8[48] = { 16, 4, 4, 64, 32, 8, 8, 128,\
		      4, 16, 64, 4, 8, 32, 128, 8,\
		      1, 32, 16, 1, 2, 128, 64, 2,\
		      32, 1, 1, 16, 128, 2, 2, 64,\
		      4, 2, 1, 8,\
		      8, 2, 1, 4,\
		      2, 8, 4, 1,\
		      2, 4, 8, 1};
		      
    int code4[16]= { 4, 4, 8, 8, 4, 4, 8, 8,\
		     1, 1, 2, 2, 1, 1, 2, 2};
    int codei[8] = {1, 2, 4, 8, 16, 32, 64, 128};
    int shft[9], shftcode[129];
		     
    int *ccode8, *ccode4;
		   
    IMAGE *imflag; /* image of flags */
    UCHAR *pflag, *pcflag, *puc, *prmin, *pcrmin;
    PIX_TYPE *pim, *pcim, h, level=PIX_MIN, maxmin;
    FIFO4 *q, *q2, *q2bis, *aq, *qrmin;

    IMAGE *imrmin;
    struct pqueue *heap, *heapbis, *heaptmp;
    PQDATUM apqd[1];
    struct node *pqd;
 
    int count=0;

/*     char fname[11]; */
/*     create_lut(im); */
/*     im->lut[46]=65535; */
/*     im->lut[46+256]=0; */
/*     im->lut[46+512]=0; */
/*     im->lut[47]=0; */
/*     im->lut[47+256]=65535; */
/*     im->lut[47+512]=0; */


    heap = (struct pqueue *)pqinit(NULL, 100);  /* heap (priority queue) */
    if (heap == NULL)
	return ERROR;

    heapbis = (struct pqueue *)pqinit(NULL, 100);  /* heap (priority queue) */
    if (heapbis == NULL)
	return ERROR;


    /* avoid border effects by setting border pixels to PIX_MAX */
    BOX2_2D; /* width of 2 pixels */
    generic_framebox(im, box, PIX_MAX); 

    /* compute minima of input image */
    imrmin = (IMAGE *)minima(im, 4); /* is of UCHAR type */
    if (imrmin==NULL)
	return(ERROR);
    /* write_tiff(imrmin, "/tmp/imrmin0.tif"); */
    BOX_2D; /* width of 1 pixels */
    generic_framebox(im, box, PIX_MIN); 
    BOX2_2D; /* width of 2 pixels */
    generic_framebox(imrmin, box, 2); 
    
    /* create flag image */
    imflag = (IMAGE *)create_image(t_UCHAR,GetImNx(im), GetImNy(im), GetImNz(im));
    if (imflag==NULL)
	return(ERROR);

    /* create a queue */
    q = create_fifo4(npix/10L);
    if (q == NULL){
	(void) sprintf(buf, "oiws(): not enough memory"); errputstr(buf);
	return ERROR;
    }
    aq = create_fifo4(npix/10L);
    if (aq == NULL){
	(void) sprintf(buf, "oiws(): not enough memory"); errputstr(buf);
	return ERROR;
    }
    q2 = create_fifo4(npix/10L);
    if (q2 == NULL){
	(void) sprintf(buf, "oiws(): not enough memory"); errputstr(buf);
	return ERROR;
    }
    q2bis = create_fifo4(npix/10L);
    if (q2bis == NULL){
	(void) sprintf(buf, "oiws(): not enough memory"); errputstr(buf);
	return ERROR;
    }
    qrmin = create_fifo4(npix/10L);
    if (qrmin == NULL){
	(void) sprintf(buf, "oiws(): not enough memory"); errputstr(buf);
	return ERROR;
    }


    shft[0]=-1;         shft[1]=1;          shft[2]=-nx;       shft[3]=nx;
    shft[4]=-nx-1;      shft[5]=nx-1;       shft[6]=-nx+1;     shft[7]=nx+1;
    shft[8]=0;
    
    shftcode[1]=-1;     shftcode[2]=1;      shftcode[4]=-nx;   shftcode[8]=nx;
    shftcode[16]=-nx-1; shftcode[32]=nx-1; shftcode[64]=-nx+1; shftcode[128]=nx+1;

    /* initialise the queue with all simple pixels which neighbour a minimum */
    pim    =(PIX_TYPE *)GetImPtr(im);
    pcim   =pim;
    prmin  =(UCHAR *)GetImPtr(imrmin);
    pcrmin  =prmin;
    pflag  =(UCHAR *)GetImPtr(imflag);
    pcflag =pflag;
    for (i=0; i<npix; i++, pcim++, pcflag++, pcrmin++){
	if (*pcrmin==1){ /* check if non-minima 4-neighbours are simple */
	    for (j=0; j<4; j++){
		if (*(pcrmin+shft[j])==0){
		    *(pcrmin+shft[j])=2; /* flag as scanned */
        	    fifo4_add(qrmin, pcim+shft[j]-pim); /* for reset later on */
		    code = 0;	
		    h=*(pcim+shft[j]);
		    if (*(pcim+shft[j]-1)>=h)
			code=1;
		    if (*(pcim+shft[j]+1)>=h)
			code|=2;
		    if (*(pcim+shft[j]-nx)>=h)
			code|=4;
		    if (*(pcim+shft[j]+nx)>=h)
			code|=8;
		    if (*(pcim+shft[j]-nx-1)>=h)
			code|=16;
		    if (*(pcim+shft[j]+nx-1)>=h)
			code|=32;
		    if (*(pcim+shft[j]-nx+1)>=h)
			code|=64;
		    if (*(pcim+shft[j]+nx+1)>=h)
			code|=128;
		    if (homotab[code] || (code==0)){ /* a simple pixel at level h */
			count++;
			pqd = (PQDATUM )malloc(sizeof(struct node));
			pqd->prio = h;
			pqd->offset= (long int)(pcim+shft[j]-pim);
			pqmininsert(heap, pqd);
			*(pcflag+shft[j])=code;
		    }
		}
	    }
	}
    }
    /* write_tiff(imrmin, "/tmp/imrmin.tif");
       write_tiff(imflag, "/tmp/imflag"); */
    printf("count (number of simple border pixels at init)=%d\n", count);
    count=0;
    
    if ( pqpeek(heap, apqd) != NULL){
	level = (*apqd)->prio;
    }
 next:
    /* printf("level=%d\n", level); */
    while ( pqpeek(heap, apqd) != NULL ){
	h=(*apqd)->prio;
	if (h>level){ /* no more pixels at current level */
	    break;
	}
	pqminremove(heap, apqd);
	ofs=(*apqd)->offset;
        free((char*) *apqd);
	fifo4_add(aq, ofs); /* for reset later on */
	del=1;
	si=1;
	nsi=0;
	pcim=pim+ofs;
	pcflag=pflag+ofs;

	if (*pcflag!=0){ /* non-isolated simple pixel */
	    /* first make sure p is independent of all its simple neighbours */
	    for(i=0; i<4; i++){ /* first: scan 4-neighbours */
		puc=pcflag+shft[i];
		if (homotab[*puc] && (*(pcim+shft[i])==h) ){ /* a simple neighbour at level h */
		    codengb=*puc;
		    del=0;
		    si=0;
		    ccode8=code8+8*i;
		    for(j=0; j<4; j++){ /* 4 common 8-neighbours */
			cfg =  *pcflag & *ccode8++;
			cfg *= *puc & *ccode8;
			if (cfg){ /* common FG */
			    del=1;
			    if ( (*(puc+shftcode[*ccode8])==0) || \
				 (*(pcim+shft[i]+shftcode[*ccode8])>h) ){      /* common non-simple FG */
				si=1;
				break;
			    }
			}
			ccode8++;		    
		    }
		    if (del==1){ /* check whether P has a 4^0 neighbour connected to a 4^0 neighbour of Q */
			del=0;
			ccode4=code4+4*i;
			for(j=0; j<2; j++){ /* 2 4-neighbours of p 4-connected to a 4-neighbours of q */
			    cfg =  *pcflag & *ccode4++;
			    cfg += *puc & *ccode4;
			    if (cfg==0){ /* P has a 4^0 neighbour connected to a 4^0 neighbour of Q */
				del=1;
				break;
			    }
			    ccode4++;
			}
		    }
		    if (del==0){ /* dependent pixel */
			if ( ((*pcflag+codengb) == 3) || ((*pcflag+codengb) == 12) ){
			    nsi=0;
			    goto skip;
			}
			goto next;
		    }
		    if (si==0)
			nsi=1;
		} /* a simple neighbour */
	    } /* first: scan 4-neighbours */
	    for(i=4; i<8; i++){ /* second: scan diagonal neighbours */
		puc=pcflag+shft[i];
		if (homotab[*puc] && (*(pcim+shft[i])==h)){ /* a simple neighbour at level h */
		    codengb=*puc;
		    del=0;
		    si=0;
		    ccode8=code8+32+4*(i-4);
		    for(j=0; j<2; j++){ /* 2 common 8-neighbours */
			cfg =  *pcflag & *ccode8++;
			cfg *= *puc & *ccode8;
			if (cfg){ /* common FG */
			    del=1;
			    if ( (*(puc+shftcode[*ccode8])==0) || \
				 (*(pcim+shft[i]+shftcode[*ccode8]) > h) ){ /* common non-simple FG */
				si=1;
				break;
			    }
			}
			ccode8++;
		    }
		    if (del==0){ /* dependent pixel */
			if ( ((*pcflag+codengb) == 96 ) || ((*pcflag+codengb) == 144) ){
			    nsi=0;
			    goto skip;
			}
			goto next;
		    }
		    if (si==0)
			nsi=1;
		}
	    }
	    /* line reached -> P is independent of ALL its simple neighbours */
	    /* in addition, if nsi==0, P is strictly independent of its simple neighbours */
	
	    if (nsi==1){ /* check whether P has a non-simple FG neighbour */
		nsn=0;
		for (i=0; i<8; i++){
		    if ( (*pcflag & codei[i]) != 0){
			if ( (*(pcflag + shftcode[*pcflag & codei[i]]) == 0) || \
			     (*(pcim+shftcode[*pcflag & codei[i]]) > h)){
			    nsn=1;
			    break;
			}
		    }
		}
	    }
	}

	skip:
	if ( (nsi==0) || (nsn==0) ){ /* independent pixels (strictly or otherwise) */
	    maxmin=PIX_MIN;
	    for (j=0; j<8; j++){
		if (*pcim > *(pcim+shft[j])){
		    maxmin=MAX(maxmin,*(pcim+shft[j]));
		}
	    }
	    fifo4_add(q2, pcim-pim);
	    fifo4_add(q2bis, maxmin);
	}
	/* else: dependent pixel */
    } /* while */

    while ((ofs=fifo4_remove(aq))){ /* reset codes of pixels at current level */
	*(pflag+ofs)=0;
    }
    while ((ofs=fifo4_remove(qrmin))){ /* reset flag in rmin of pixels at current level */
	*(prmin+ofs)=0;
    }

    fifo4_lookreset(q2);
    while((ofs=fifo4_look(q2))){
	*(pim+ofs)=fifo4_remove(q2bis);
    }
/*     printf("count (number of independent simple pixels)=%d (level = %d)\n", count, level); */
/*     count=0; */

    while ((ofs = fifo4_remove(q2))){
	for (i=0; i<9; i++){ /* scan neighbours of independent pixel */
	    pcim=pim+ofs+shft[i];
	    pcflag=pflag+ofs+shft[i];
	    if ( (*pcflag==0) && (*(prmin+ofs+shft[i])==0) ){  /* (*pcim>=level) && */
		*(prmin+ofs+shft[i])=2;
        	fifo4_add(qrmin, ofs+shft[i]); /* for reset later on */
		h=*pcim;
		code=0;
		if (*(pcim-1)>=h)
		    code=1;
		if (*(pcim+1)>=h)
		    code|=2;
		if (*(pcim-nx)>=h)
		    code|=4;
		if (*(pcim+nx)>=h)
		    code|=8;
		if (*(pcim-nx-1)>=h)
		    code|=16;
		if (*(pcim+nx-1)>=h)
		    code|=32;
		if (*(pcim-nx+1)>=h)
		    code|=64;
		if (*(pcim+nx+1)>=h)
		    code|=128;
		if (homotab[code] || (code==0) ){ /* a simple pixel at level h */
		    if (h>=level){
			pqd = (PQDATUM )malloc(sizeof(struct node));
			pqd->prio = h;
			pqd->offset= (long int)(pcim-pim);
			pqmininsert(heap, pqd);
			*pcflag=code;
		    }
		    else{
			pqd = (PQDATUM )malloc(sizeof(struct node));
			pqd->prio = h;
			pqd->offset= (long int)(pcim-pim);
			pqmininsert(heapbis, pqd);
			*pcflag=code;
		    }
		}
	    }
	}
    }

/*     printf("count (number simple pixels at next iteration)=%d, level=%d\n", count, level); */
/*    count=0; */

    /* sprintf(fname, "im-%03d.tif", level);
       write_ColorMap_tiff(im, fname); */
	
    if (pqpeek(heap, apqd)!=NULL){
	level = (*apqd)->prio;
	goto next;
    }

    if (pqpeek(heapbis, apqd)!=NULL){
	level = (*apqd)->prio;
	heaptmp=heap;
	heap=heapbis;
	heapbis=heaptmp;
	goto next;
    }

    free_fifo4(q);
    free_fifo4(aq);
    free_fifo4(q2);
    free_fifo4(q2bis);
    free_fifo4(qrmin);
    free_image(imflag);
    free_pq(heap);
    free_pq(heapbis);

    return NO_ERROR;
}    
#include "g_undef.h"


ERROR_TYPE oiws(IMAGE *im)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(generic_oiws(im));
    break;

  default:
    (void)sprintf(buf, "ERROR in oiws(IMAGE *im): \
                invalid ImDataType\n"); errputstr(buf);
    return ERROR;
  }
  return ERROR;
}

/*@}*/
