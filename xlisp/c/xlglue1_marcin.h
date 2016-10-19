

/* classification.c */
extern ERROR_TYPE classstatsinfo(IMAGE *immos, IMAGE *imin);
extern ERROR_TYPE clmindist(IMAGE *immos, IMAGE *imin, int bklabel, int mode, double thr);
extern ERROR_TYPE clparpip(IMAGE *immos, IMAGE *imin, int bklabel, int mode, double mult);
extern ERROR_TYPE clmaha(IMAGE *immos, IMAGE *imin, int bklabel, int mode, double thr);
extern ERROR_TYPE clmaxlike(IMAGE *immos, IMAGE *imin, int bklabel, int type, double thr);


/* skel.c */
extern ERROR_TYPE binODthin_noqueue(IMAGE *imin, int stype, int atype, IMAGE *imanchor);
extern ERROR_TYPE binODthin_FIFO(IMAGE *imin, int stype, int atype, IMAGE *imanchor);
extern ERROR_TYPE binOIthin_noqueue (IMAGE *imin, int stype, int atype, IMAGE *imanchor);
extern ERROR_TYPE binOIthin_FIFO (IMAGE *imin, int stype, int atype, IMAGE *imanchor);

/************ other stuff - do not copy from here to the end of file ! ***********/

extern ERROR_TYPE uc_lut(IMAGE *, IMAGE *);
extern ERROR_TYPE lvar_img(IMAGE *, int, int, int);
extern ERROR_TYPE linmix(IMAGE *, IMAGE *, int);
extern ERROR_TYPE linmeangrad(IMAGE *, int);  
