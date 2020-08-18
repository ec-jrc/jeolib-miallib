#ifndef XLGLUE1_GRAZZJA_H
#define XLGLUE1_GRAZZJA_H 

extern IMAGE* csmoothcc( IMAGE *im, IMAGE*imlbl, IMAGE *imse,
            int ox, int oy, int oz, int rl );

IMAGE** texfeat ( IMAGE *im, IMAGE* ilabel, UINT32 *llab, int nlab,
                  char *afeat, int nfeat, 
                  char gltype, int gltx, int glty, int glres, 
                  char weitype, double sigma, 
                  int wsize, IMAGE* imap, MIALFLOAT *ascale, 
                  char flper );
IMAGE** texfeatQ ( IMAGE *im, IMAGE* ilabel, UINT32 *llab, int nlab,
                   char *afeat, int nfeat, 
                   char gltype, int gltx, int glty, int glres, 
                   char weitype, double sigma, 
                   int wsize, IMAGE* imap, MIALFLOAT *ascale, 
                   char flper );

#endif
   
