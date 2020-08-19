/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2004-2020 European Union (Joint Research Centre)

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

/*
  First Tue Feb 24 2004
*/


#include <stdio.h>
#include <stdlib.h>
#include "miallib.h"
#include "fifo.h"

/** \addtogroup group_hmtsk
 *  @{
 */

/*
  The code below runs for an order independent (binary) iz
*/

#include "g_def.h"
ERROR_TYPE generic_oiiz(IMAGE *im)
{
    /*
      We propose to simulate an order independent flooding based on ordered
      indenpendent thinnings proceeding by increasing intensity levels.
    */
    long int i, j, ofs, ofs2;
    int del; /* Bool. variable ind. whether a pixel is deletable (1) or not (0) */
    int si;  /* Bool. variable ind. whether a pixel is strictly ind. (1) or not (0) of a given simple ngb */
    int nsi; /* Bool. variable ind. whether a pixel is non-strictly ind. (1) or not (0) of all its simple ngb */
    int cfg; /* Bool. variable ind. whether there is a common foreground neighbour */
    int nsn; /* Bool. variable ind. whether there exists a pixel is non-simple ngb */
    int code;
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
    int shft[8], shftcode[129];
    int dep4, dep8, cn;

    int *ccode8, *ccode4;

    int count=0;

    IMAGE *imflag; /* image of flags */
    UCHAR *pflag, *pcflag, *puc;
    PIX_TYPE *pim, *pcim, *pcim2, h, val;
    FIFO4 *q, *q2, *q3, *q4;

    /* create flag image */
    imflag = (IMAGE *)create_image(t_UCHAR,GetImNx(im), GetImNy(im), GetImNz(im));
    if (imflag==NULL)
	return(ERROR);

    /* avoid border effects by setting border pixels to PIX_MIN */
    BOX_2D;
    generic_framebox(im, box, PIX_MIN); /* PIX_MIN => never simple */

    /* create a queue */
    q = create_fifo4(npix/100L);
    if (q == NULL){
	(void) sprintf(buf, "oiiz(): not enough memory"); errputstr(buf);
	return ERROR;
    }
    q2 = create_fifo4(npix/100L);
    if (q2 == NULL){
	(void) sprintf(buf, "oiiz(): not enough memory"); errputstr(buf);
	return ERROR;
    }
    q3 = create_fifo4(npix/100L);
    if (q3 == NULL){
	(void) sprintf(buf, "oiiz(): not enough memory"); errputstr(buf);
	return ERROR;
    }
    q4 = create_fifo4(npix/100L);
    if (q4 == NULL){
	(void) sprintf(buf, "oiiz(): not enough memory"); errputstr(buf);
	return ERROR;
    }

    /* initialise the queue with all simple pixels */
    pim    =(PIX_TYPE *)GetImPtr(im);
    pcim   =pim;
    pflag  =(UCHAR *)GetImPtr(imflag);
    pcflag =pflag;
    for (i=0; i<npix; i++, pcim++, pcflag++){
	if (*pcim!=PIX_MIN){ /* check if pim is simple */
	    code = 0;
	    h=*pcim;
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
	    if (homotab[code]){
		fifo4_add(q, pcim-pim); /* insert offset position */
		*pcflag=code;
		count++;
	    }
	    else if (code == 0){ /* isolated pixel (non-simple but deletable for IZ/WS) */
		/* add in queue of ordered independent pixels */
		*pcim=0;
		/* fifo4_add(q2, pcim-pim); CHECK!!! */
	    }
	}
    }

    printf("count (number of simple border pixels at init)=%d\n", count);
    count=0;
    shft[0]=-1;         shft[1]=1;          shft[2]=-nx;       shft[3]=nx;
    shft[4]=-nx-1;      shft[5]=nx-1;       shft[6]=-nx+1;     shft[7]=nx+1;

    shftcode[1]=-1;     shftcode[2]=1;      shftcode[4]=-nx;   shftcode[8]=nx;
    shftcode[16]=-nx-1; shftcode[32]=nx-1; shftcode[64]=-nx+1; shftcode[128]=nx+1;

 next:
    fifo4_lookreset(q);
    while ((ofs = fifo4_look(q))){
	del=1;
	si=1;
	nsi=0;
	pcim=pim+ofs;
	pcflag=pflag+ofs;
	dep4=0;
	dep8=0;

	/* first make sure p is independent of all its simple neighbours */
	for(i=0; i<4; i++){ /* first: scan 4-neighbours */
	    puc=pcflag+shft[i];
	    if (homotab[*puc]){ /* a simple neighbour */
	        del=0;
	        si=0;
		ccode8=code8+8*i;
		for(j=0; j<4; j++){ /* 4 common 8-neighbours */
		    cfg =  *pcflag & *ccode8++;
		    cfg *= *puc & *ccode8;
		    if (cfg){ /* common FG */
			del=1;
			if (*(puc+shftcode[*ccode8])==0){ /* common non-simple FG */
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
		    if (si==1)
			goto next;  /* dependent for IZ */
		    else
			dep4=1;
		}
		if (si==0)
		    nsi=1;
	    } /* a simple neighbour */
	} /* first: scan 4-neighbours */
	for(i=4; i<8; i++){ /* second: scan diagonal neighbours */
	    puc=pcflag+shft[i];
	    if (homotab[*puc]){ /* a simple neighbour */
	        del=0;
		si=0;
		ccode8=code8+32+4*(i-4);
		for(j=0; j<2; j++){ /* 2 common 8-neighbours */
		    cfg =  *pcflag & *ccode8++;
		    cfg *= *puc & *ccode8;
		    if (cfg){ /* common FG */
			del=1;
			if (*(puc+shftcode[*ccode8])==0){ /* common non-simple FG */
			    si=1;
			    break;
			}
		    }
		    ccode8++;
		}
		if (del==0){ /* dependent pixel */
		    if (si==1)
			goto next;  /* dependent for IZ */
		    else
			dep8=1;
		}
		if (si==0)
		    nsi=1;
	    }
	}
	/* line reached -> P is independent of ALL its simple neighbours */
	/* in addition, if nsi==0, P is strictly independent of its simple neighbours */

	nsn=0;
	if (nsi==1){ /* check whether P has a non-simple FG neighbour */
	    for (i=0; i<8; i++){
		if ( (*pcflag & codei[i]) != 0){
		    if (*(pcflag + shftcode[*pcflag & codei[i]]) == 0){
			nsn=1;
			break;
		    }
		}
	    }
	}
	if ( (nsn==0) && ((dep4==1) || (dep8==1)) ){ /* check whether P belongs to a simply CC of simple pixels */
	    if (*pcim == 0){ /* already processed */
		printf("Already deleted\n");
		goto next;
	    }

	    printf("check whether P belongs to a simply CC of simple pixels, ofs=%ld\n", ofs);
	    fifo4_add(q3, pcim-pim);
	    fifo4_add(q4, pcim-pim);
	    *pcim=PIX_MAX; /* flag */
	    if ( (*pcflag & 138) == 0)
		cn = 1;
	    else
		cn=0;
	    if ( (*pcflag & 128) == 0)
		if ( (*pcflag & 64) )
		    cn--;
	    val=0;
	    while ((ofs2=fifo4_remove(q3))){
		pcim2=pim+ofs2;
		for (j=0; j<8; j++){
		    if ( *(pcim2+shft[j]) == 1){
			if ( *(pflag+ofs2+shft[j]) ){
			    fifo4_add(q3, pcim2+shft[j]-pim);
			    fifo4_add(q4, pcim2+shft[j]-pim);
			    *(pcim2+shft[j])=PIX_MAX;
			    if ( (*(pflag+ofs2+shft[j]) & 138) == 0)
				cn++;
			    if  ( (*(pflag+ofs2+shft[j])  & 128) == 0)
				if ( (*(pflag+ofs2+shft[j]) & 64) )
				    cn--;
			}
			else{ /* non simple pixel in CC */
			    printf("NO\n");
			    val=1;
			    fifo4_flush(q3);
			    break;
			}
		    }
		}
	    }
	    if (val==0){
		printf("cn=%d\n", cn);
	    }
	    if (cn!=1)
		val=1;
	    while ((ofs2=fifo4_remove(q4))){
		*(pim+ofs2)=val;
	    }
	}
	else if ( (nsi==0) || (nsn==0) ){ /* independent pixels (strictly or otherwise) */
	    *pcim=0;
	    fifo4_add(q2, pcim-pim);
	    count++;
	}
	/* else: dependent pixel */
    } /* while */

    while ((ofs = fifo4_remove(q))) /* reset codes */
	*(pflag+ofs)=0;

    printf("count (number of independent simple pixels)=%d\n", count);
    /* return(1); */

    count=0;
    while ((ofs = fifo4_remove(q2))){
	for (i=0; i<8; i++){ /* scan neighbours of independent pixel */
	    pcim=pim+ofs+shft[i];
	    pcflag=pflag+ofs+shft[i];
	    if ( (*pcim!=PIX_MIN) && (*pcflag==0) ){ /* *pcim==255 for non-simple pixels */
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
		if (homotab[code]){
		    fifo4_add(q, pcim-pim); /* insert offset position */
		    *pcflag=code;
		}
		/* CHECK: should never appear */
		else if (code == 0){ /* isolated pixel (non-simple but deletable for IZ/WS) */
		    /* add in queue of ordered independent pixels */
		    *pcim=0;
		    /* fifo4_add(q2, pcim-pim); CHECK THAT NOT NECESSARY!!! */
		}
	    }
	}
    }

    if (fifo4_empty(q)==0)
	goto next;

    free_fifo4(q);
    free_fifo4(q2);
    free_fifo4(q3);
    free_fifo4(q4);
    write_tiff(imflag, "/tmp/imflag.tif");
    free_image(imflag);

    return NO_ERROR;

}
#include "g_undef.h"


ERROR_TYPE oiiz(IMAGE *im)
{

  switch (GetImDataType(im)){

  case t_UCHAR:
    return(generic_oiiz(im));
    break;


  default:
    (void)sprintf(buf, "ERROR in oiiz(IMAGE *im): \
                invalid ImDataType (MUST be UCHAR)\n"); errputstr(buf);
    return ERROR;
  }
  return ERROR;
}

/*@}*/
