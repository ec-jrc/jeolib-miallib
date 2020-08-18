#include <xlglue1_grazzja.h>


/****************************************************************/
#ifdef READ_LIST  /* Read list of values with same type */
#undef READ_LIST
#endif
#define READ_LIST(array, xlist, arg, n, type)  		\
  xlist = xlgalist();			       		\
  for (n = 0; consp(xlist); xlist = cdr(xlist)) {	\
    arg = car(xlist);					\
    if(arg == NIL) break; /* exit the for loop */	\
    switch(type) {					\
    case t_FLOAT:		       			\
      array[n++]=(float)getflonum(arg);	 break;	       	\
    case t_INT32:					\
    case t_UINT32:					\
      array[n++]=(long)getfixnum(arg);	 break;		\
    case t_SHORT:					\
    case t_USHORT:					\
      array[n++]=(int)getfixnum(arg);	 break;		\
    case t_CHAR:					\
    case t_UCHAR:					\
      array[n++]=(char)getfixnum(arg);	 break;		\
    default:						\
      xlbadtype(arg); break;				\
    }							\
  }


/****************************************************************/
#ifdef READ_LIST_IMAGES  /* Read list of input multispectral images */
#undef READ_LIST_IMAGES
#endif
#define READ_LIST_IMAGES(imarray, xlist, arg, n)	\
  xlist = xlgalist();					\
  for (n = 0; consp(xlist); xlist = cdr(xlist)) {	\
    arg = car(xlist);					\
    if (imagep(arg))					\
      imarray[n++]=(IMAGE*)getimage(arg);		\
    else {						\
      xlbadtype(arg);					\
      break;						\
    }							\
  }


/****************************************************************/
#ifdef READ_IMAGE  /* Read and image */
#undef READ_IMAGE
#endif
#define READ_IMAGE(xlim)   xlim=xlgaimage();


/****************************************************************/
#ifdef READ_FLOAT  /* Read a float */
#undef READ_FLOAT
#endif
#define READ_FLOAT(par) par=(float)getflonum(xlgaflonum());


/****************************************************************/
#ifdef READ_INT  /* Read an int */
#undef READ_INT
#endif
#define READ_INT(par)   par=(int)getfixnum(xlgafixnum());


/****************************************************************/
#ifdef READ_CHAR  /* Read a char */
#undef READ_CHAR
#endif
#define READ_CHAR(par)  par=(char)getfixnum(xlgafixnum());


/****************************************************************/
#ifdef SAVE_LIST_IMAGES  /* Save list of output multispectral images */
#undef SAVE_LIST_IMAGES
#endif
#define SAVE_LIST_IMAGES( imarray, xlim, result, n, m )	\
  xlstkcheck(n+1); /* n? */				\
  xlsave(result);					\
  for( m=n-1; m>=0; m-- ) {				\
    xlsave(xlim);					\
    xlim = cvimage(imarray[m]);				\
    result = cons (xlim, result);			\
  }							\
  xlpopn(n+1); /* n? */


/****************************************************************/
#ifdef SAVE_LIST_VALUES  /* Save list of output multispectral images */
#undef SAVE_LIST_VALUES
#endif
#define SAVE_LIST_VALUES( array, xlim, result, n, m )	\
  xlstkcheck(n+1);						\
  xlsave(result);						\
  for( m=n-1; m>=0; m-- ) {					\
    xlsave(xlim);						\
    xlim = cvflonum((float)array[m]);				\
    result = cons (xlim, result);				\
  }								\
  xlpopn(n+1);



LVAL ismoothcc(){
 LVAL xlim1, xlim2, xlim3;
 int ox, oy, oz, rl;

 if (!moreargs())
   xlabort("(@smoothcc im imlbl imse ox oy oz rl)");
 /*
   \lspfunction{@}{smoothcc}{im imlbl imse ox oy oz rl}
 */

 xlim1 = xlgaimage();
 xlim2 = xlgaimage();
 xlim3 = xlgaimage();
 ox = (int) getfixnum(xlgafixnum());
 oy = (int) getfixnum(xlgafixnum());
 oz = (int) getfixnum(xlgafixnum());
 rl = (int) getfixnum(xlgafixnum());

 xllastarg();

 return(cvimage(csmoothcc((IMAGE*)getimage(xlim1),
             (IMAGE*)getimage(xlim2),
             (IMAGE*)getimage(xlim3),
             ox, oy, oz,
             rl)));
}
/****************************************************************/
LVAL itexfeat() {
/****************************************************************/
  LVAL xlim, xllab, xlmap;
  LVAL xlist, arg, result=NIL; 
  INT32 llab[SHORT_MAX];
  char afeat[MAXNUMBEROFTEXTURE];
  char gltype, weitype, flper;
  int n, nlab, nfeat;
  IMAGE **imarray_feat;
  int gltx, glty, glres, wsize;
  MIALFLOAT ascale[3], nscales;
  float sigma; int type; 

  if (!moreargs())
    xlabort("(*ctexfeat im lab ilab afeat nfeat gltype gltx glty glres weitype sigma wsize map ascale flper)");
  /*
    \lspfunction{*}{ctexfeat}{im lab ilab afeat nfeat gltype gltx glty glres weitype sigma wsize map ascale flper}
    \cfunction{\cftexfeat}
    \cfile{texture/texfeat\myext.c}
  */

  READ_IMAGE(xlim);
  READ_IMAGE(xllab);
  type = t_UINT32, READ_LIST(llab, xlist, arg, nlab, type);
  /* nlab can possibly be null: no specific label has been provided */
  type = t_UCHAR, READ_LIST(afeat, xlist, arg, n, type); 
  READ_INT(nfeat);
  READ_CHAR(gltype);
  READ_INT(gltx);
  READ_INT(glty);
  READ_INT(glres);
  READ_CHAR(weitype);
  READ_FLOAT(sigma);
  READ_INT(wsize);
  READ_IMAGE(xlmap);
  type = t_FLOAT, READ_LIST(ascale, xlist, arg, n, type); 
  if(n != 2) 
    xlabort("wrong number of scales parameters");
  READ_CHAR(flper);
  
  xllastarg();

  if((imarray_feat=texfeat((IMAGE*)getimage(xlim), (IMAGE*)getimage(xllab), llab, nlab,
			   afeat, nfeat, (char)gltype, gltx, glty, glres, (char)weitype, sigma, 
			   wsize, (IMAGE*)getimage(xlmap), ascale, (char)flper)) == NULL)
    xlabort("(*ctexfeat im lab ilab afeat nfeat gltype gltx glty glres weitype sigma wsize map ascale flper)");
  
  SAVE_LIST_IMAGES( imarray_feat, xlim, result, nfeat, n );
  
  return result;
}

