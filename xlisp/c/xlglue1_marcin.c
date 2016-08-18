
/************ CLASSIFIERS *************/

LVAL ist()
{
  LVAL xlim1, xlim2;

  if (!moreargs())
    xlabort("(@st imin immos)");

/*
  \lspfunction{@}{st}{imin immos}
  \param{imin}{an image node}
  \param{lmmos}{a mosaic (class) image}
  \return{}
  \desc{Prints class statistics of multiband image 'imin', according to classes defined in 'immos'.}
  \cfunction{\cfst}
  \cfile{marcin.c}
  \authors{Marcin Iwanowski}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();

  xllastarg();

  if (uc_imst((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2))==ERROR)
    return NIL;

  return(xlim1);    
}


LVAL iclmindist()
{
  LVAL xlim1, xlim2;
  int bklab, mode;
  double thr;
  if (!moreargs())
    xlabort("(@cl_mindist immos imin bklabel &opt (thr))");

/*
  \lspfunction{@}{cl_mindist}{immos imin bklabel &opt (thr) }
  \param{immos}{mosaic image - initial classification and the result}
  \param{imin}{multispectral image}
  \param{bklabel}{label of background class (which is not considered in clasification)}
  \param{thr}{threshold for distance value}
  \return{immos}
  \desc{Minimum distance classifier.}
  \cfunction{\cfst}
  \cfile{marcin.c}
  \authors{Marcin Iwanowski}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  bklab = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    {mode = 0; thr = 1.0; /* thr doesn't matter in this case */}
  else
    { mode  = 1;
      thr  = (double)getflonum(xlgaflonum()); }
 
  xllastarg();

  if (uc_clmindist((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), bklab, mode, thr )==ERROR)
    return NIL;

  return(xlim1);    
}

LVAL iclparpip()
{
  LVAL xlim1, xlim2;
  int bklab, mode;
  double mult=0.0;
  if (!moreargs())
    xlabort("(@cl_parpip immos imin bklabel &opt (mode mult))");
 
/*
  \lspfunction{@}{cl_parpip}{immos imin bklabel &opt (mode mult)}
  \param{immos}{mosaic image - initial classification and the result}
  \param{imin}{multispectral image}
  \param{bklabel}{label of background class (which is not considered in clasification)}
  \param{mode}{type of operation}
  \param{mult}{multiplicator of standart deviation (for mode = 1,3,5)}
  \return{immos}
  \desc{Parallelpiped classifier. The variable 'mode' indictaes type of classification: mode = 0,2,4 - min and max values among all the members of pixels which belong to every class ('mult' doesn't play a role in this case), mode = 1,3,5 - in each dimension - mean value +/- the multiplication (by parameter 'mult') of standard deviation, mode = 0,1 - pixels from inseparability regions are classified to the last class matched, mode = 2,3 - pixels from inseparability regions are not classified, mode = 4,5 - returns number of classes to which each image point belong.}
  \cfunction{\cfst}
  \cfile{marcin.c}
  \authors{Marcin Iwanowski}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  bklab = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    {mode = 1; mult = 1.0;}
  else
    {
     mode  = (int)getfixnum(xlgafixnum());
     if ((mode&1) == 1) mult  = (double)getflonum(xlgaflonum());  /* only for +/- stddev i.e. mode == 1,3,5 */
    }
  xllastarg();

  if (uc_clparpip((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), bklab, mode, mult )==ERROR)
    return NIL;

  return(xlim1);    
}

LVAL iclmaha()
{
  LVAL xlim1, xlim2;
  int bklab, mode;
  double thr;
  if (!moreargs())
    xlabort("(@cl_maha immos imin bklabel)");

/*
  \lspfunction{@}{cl_maha}{immos imin bklabel &opt (thr)}
  \param{immos}{mosaic image - initial classification and the result}
  \param{imin}{multispectral image} 
  \param{bklabel}{label of background class (which is not considered in clasification)}
  \param{thr}{threshold for distance value}
  \return{immos}
  \desc{Mahalonobis distance classifier.}
  \cfunction{\cfst}
  \cfile{marcin.c}
  \authors{Marcin Iwanowski}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  bklab = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    {mode = 0; thr = 1.0; /* thr doesn't matter in this case */ }
  else
    {
      mode  = 1;
      thr  = (double)getflonum(xlgaflonum());
    }
  xllastarg();

  if (uc_clmaha((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), bklab, mode, thr )==ERROR)
    return NIL;

  return(xlim1);    
}


LVAL iclmaxlike()
{
  LVAL xlim1, xlim2;
  int bklab,type;
  double thr=0.0;
  if (!moreargs())
    xlabort("(@cl_maxlike immos imin bklabel &opt (type thr)");

/*
  \lspfunction{@}{cl_maxlike}{immos imin bklabel &opt (type thr)}
  \param{immos}{mosaic image - initial classification and the result}
  \param{imin}{multispectral image}
  \param{bklabel}{label of background class (which is not considered in clasification)}
  \param{type}{type of operation}
  \param{thr}{discriminant fuction threshold (for type = 4,5,6)}
  \return{immos}
  \desc{Maximum likelihood classifier. Variable 'mode' - indicates a way of computing the classification: mode  = 0 - no threshold ('th' not used), mode = 1 - distance threshold ('th' indicates threshold = power of 2 of maximum acceptable Mahalonobis distance)}
  \cfunction{\cfst}
  \cfile{marcin.c}
  \authors{Marcin Iwanowski}
*/

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  bklab = (int)getfixnum(xlgafixnum());
  if (!moreargs())
    {type = 0; thr = 1.0; /* thr doesn't matter in this case */ }
  else
    {
     type  = (int)getfixnum(xlgafixnum());
     if (type >3) thr  = (double)getflonum(xlgaflonum());
    }
  xllastarg();
 
  if (uc_clmaxlike((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), bklab, type, thr )==ERROR)
    return NIL;

  return(xlim1);    
}

/********************** END OF CLASSIFIERS ********************/


/********************** BINARY SKELETONISATION ***************/

LVAL ibinskelOD ()
{
  LVAL xlim1; 
  int type, alg;
  if (!moreargs())  xlabort("(@binskelOD imin type &opt (alg))");
  /*
   \lspfunction{@}{binskelOD}{imin type &opt (alg)}
   \param{imin}{input (binary shape) and output (skeleton) image}
   \param{type}{type of skeletonisation}
   \param{alg}{type of algorithm}
   \return{imin}
   \desc{Computes order dependent skeleton of imin. The image 'imin' is a binary image (with 0 and 1 values). The variable 'type' controls the type of pixels which are removed during the thinning process: 0 - 8-simple; 1 - 4-simple; 2 - (4,8)-simple; 3 - 8-b-simple; 4 - 4-b-simple; 5 - (4,8)-b-simple. The varaible 'arg' stands for the type of algorithm used: 0 - fast queue based (default); 1 - classical (slow, multiple image scannings).}
   \cfunction{\cfbinODthin_FIFO(binODthin_noqueue)}
   \cfile{skel.c}
  \authors{Marcin Iwanowski}
  */

  xlim1 = xlgaimage();
  type = (int)getfixnum(xlgafixnum());
  if (type < 0) type = 0;
  if (type > 5) type = 5;
  if (!moreargs()) alg = 0;  
  else alg = (int)getfixnum(xlgafixnum());
  xllastarg();
  if (alg == 0) 
   {if (binODthin_FIFO((IMAGE *)getimage(xlim1), type, 0, NULL) == ERROR) return NIL;}
  else 
   {if (binODthin_noqueue((IMAGE *)getimage(xlim1), type, 0, NULL) == ERROR) return NIL;}  
  return(xlim1); 
}

LVAL ibinskelOI ()
{
  LVAL xlim1; 
  int type, alg;
  if (!moreargs())  xlabort("(@binskelOI imin type &opt (alg))");
  /*
   \lspfunction{@}{binskelOI}{imin type &opt (alg)}
   \param{imin}{input (binary shape) and output (skeleton) image}
   \param{type}{type of skeletonisation}
   \param{alg}{type of algorithm}
   \return{imin}
   \desc{Computes order independent skeleton of 'imin'. The image 'imin' is a binary image (with 0 and 1 values). The variable 'type' controls the type of pixels which are removed during the thinning process: 0 - 8-simple; 1 - 4-simple; 2 - (4,8)-simple; 3 - 8-b-simple; 4 - 4-b-simple; 5 - (4,8)-b-simple. The variable 'arg' stands for the type of algorithm used: 0 - fast queue based (default); 1 - classical (slow, multiple image scannings).}
   \cfunction{\cfbinOIthin_FIFO(binOIthin_noqueue)}
   \cfile{skel.c}
  \authors{Marcin Iwanowski}
  */
  xlim1 = xlgaimage();
  type = (int)getfixnum(xlgafixnum());
  if (type < 0) type = 0;
  if (type > 5) type = 5;
  if (!moreargs()) alg = 0; 
  else alg = (int)getfixnum(xlgafixnum());
  xllastarg();
  if (alg == 0) 
   {if (binOIthin_FIFO( (IMAGE *)getimage(xlim1), type, 0, NULL) == ERROR) return NIL;}
  else 
   {if (binOIthin_noqueue( (IMAGE *)getimage(xlim1), type, 0, NULL) == ERROR) return NIL;}  
  return(xlim1); 
}

LVAL ibinanchorskelOD ()
{
  LVAL xlim1,xlim2; 
  int type, alg;
  if (!moreargs())  xlabort("(@binanchorskelOD imin imanchor type &opt (alg))");
  /*
   \lspfunction{@}{binanchorskelOD}{imin imanchor type &opt (alg)}
   \param{imin}{input (binary shape) and output (skeleton) image}
   \param{imanchor}{input anchor image (1 - anchor pixel, 0 - otherwise)}
   \param{type}{type of skeletonisation}
   \param{alg}{type of algorithm}
   \return{imin}
   \desc{Computes order dependent anchored skeleton of 'imin'. The image 'imin'is a binary image (with 0 and 1 values). Variable 'type' controls the type of pixels which are removed during the thinning process: 0 - 8-simple; 1 - 4-simple; 2 - (4,8)-simple; 3 - 8-b-simple; 4 - 4-b-simple; 5 - (4,8)-b-simple. The variable 'arg' stands for the type of algorithm used: 0 - fast queue based (default); 1 - classical (slow, multiple image scannings).}
   \cfunction{\cfbinODthin_FIFO(binODthin_noqueue)}
   \cfile{skel.c}
  \authors{Marcin Iwanowski}
  */
  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  type = (int)getfixnum(xlgafixnum());
  if (type < 0) type = 0;
  if (type > 5) type = 5;
  if (!moreargs()) alg = 0; 
   else alg = (int)getfixnum(xlgafixnum());
  xllastarg();
  if (alg == 0) 
   {if (binODthin_FIFO( (IMAGE *)getimage(xlim1), type, 1, (IMAGE *)getimage(xlim2)) == ERROR) return NIL;}
  else 
   {if (binODthin_noqueue( (IMAGE *)getimage(xlim1), type, 1, (IMAGE *)getimage(xlim2)) == ERROR) return NIL;}  
  return(xlim1); 
}

LVAL ibinanchorskelOI ()
{
  LVAL xlim1,xlim2; 
  int type, alg;
  if (!moreargs())  xlabort("(@binanchorskelOI imin imanchor type &opt (alg))");
  /*
   \lspfunction{@}{binanchorskelOI}{imin imanchor type &opt (alg)}
   \param{imin}{input (binary shape) and output (skeleton) image}
   \param{imanchor}{input anchor image (1 - anchor pixel, 0 - otherwise)}
   \param{type}{type of skeletonisation}
   \param{alg}{type of algorithm}
   \return{imin}
   \desc{Computes order independent anchored skeleton of imin. Image 'imin' is a binary image (with 0 and 1 values). The variable 'type' controls the type of pixels which are removed during the thinning process: 0 - 8-simple; 1 - 4-simple; 2 - (4,8)-simple; 3 - 8-b-simple; 4 - 4-b-simple; 5 - (4,8)-b-simple. The variable 'arg' stands for the type of algorithm used: 0 - fast queue based (default); 1 - classical (slow, multiple image scannings).}
   \cfunction{\cfbinOIthin_FIFO(binOIthin_noqueue)}
   \cfile{skel.c}
  \authors{Marcin Iwanowski}
  */
  xlim1 = xlgaimage();  
  xlim2 = xlgaimage();
  type = (int)getfixnum(xlgafixnum());
  if (type < 0) type = 0;
  if (type > 5) type = 5;
  if (!moreargs()) alg = 0; 
   else alg = (int)getfixnum(xlgafixnum());
  xllastarg();
  if (alg == 0) 
   {if (binOIthin_FIFO( (IMAGE *)getimage(xlim1), type, 1, (IMAGE *)getimage(xlim2)) == ERROR) return NIL;}
  else 
   {if (binOIthin_noqueue( (IMAGE *)getimage(xlim1), type, 1, (IMAGE *)getimage(xlim2)) == ERROR) return NIL;}  
  return(xlim1); 
}

/********************* END OF BINARY SKELETONISATION  **********************/
