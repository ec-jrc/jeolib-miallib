

/* unwrap.c */
LVAL iunwrap()
{
  LVAL xlim1;
  int graph;

  if (!moreargs())
    xlabort("(@unwrap im graph)");

  xlim1 = xlgaimage();
  graph = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (unwrap((IMAGE *)getimage(xlim1), graph)==ERROR)
    return NIL;

  return(xlim1);
}



/* ngb.c */
LVAL imult8()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*mult8 im)");
/*
  \lspfunction{*}{mult8}{im}
  \param{im}{an image node (UCHAR)}
  \return{im}
  \desc{}
  \cfunction{\cfmultEIGHT}
  \cfile{test.c}
  \example{}{}
*/
  xlim1 = xlgaimage();

  xllastarg();

  return(cvimage(mult8((IMAGE *)getimage(xlim1))));
}



LVAL iomeca()
{
  LVAL xlim1;

  if (!moreargs())
    xlabort("(*omeca im)");
  xlim1 = xlgaimage();

  xllastarg();

  return(cvimage(omeca((IMAGE *)getimage(xlim1))));
}

LVAL iclip()
{
  LVAL xlim1, xlim2;
  int graph, n;

  if (!moreargs())
    xlabort("(*clip im lutim graph n)");

  xlim1 = xlgaimage();
  xlim2 = xlgaimage();
  graph = (int)getfixnum(xlgafixnum());
  n = (int)getfixnum(xlgafixnum());
  
  xllastarg();

  return (cvimage(clip((IMAGE *)getimage(xlim1), (IMAGE *)getimage(xlim2), graph, n)));
}

LVAL i3d22d()
{
  LVAL xlim1;
  int npx, npy;

  if (!moreargs())
    xlabort("(@3d22d im npx npy); npx=number of planes in x, npy= ... in y");

  xlim1 = xlgaimage();
  npx = (int) getfixnum(xlgafixnum());
  npy = (int) getfixnum(xlgafixnum());

  xllastarg();

  if (a3d22d((IMAGE *)getimage(xlim1), npx, npy)==ERROR)
    return NIL;

  return(xlim1);
}

LVAL idestripe()
{
  LVAL xlim1;
  int graph;

  if (!moreargs())
    xlabort("(@destripe im graph)");
/*
  \lspfunction{@}{destripe}{im graph}
  \param{im}{an image node}
  \param{graph}{integer for graph-connectivity}
  \return{im}
  \desc{}
  \cfunction{int destripe(IMAGE *im, int graph)}
  \cfile{test.c}
  \example{}{}
*/
  xlim1 = xlgaimage();
  graph= (int) getfixnum(xlgafixnum());
  xllastarg();

  if (destripe((IMAGE *)getimage(xlim1), graph) == ERROR)
    return NIL;
  return xlim1;
}



/* */



LVAL imcsort()
{
  LVAL arg, xlist;
  IMAGE *imarray[maxNumberOfBands];
  int nc=0;

  if (!moreargs())
    xlabort("(*mcsort (list im1 [im2] [...] [imn]))\n");
/*
  \lspfunction{*}{mcsort}{listim}
  \param{listim}{a list of image nodes}
  \return{a new image coding the input images}
  \desc{in development!}
  \cfunction{\cfmcsort}
  \cfile{mcsort.c}
*/
  xlist = xlgalist();

  for (nc = 0; consp(xlist); xlist = cdr(xlist)){
    arg = car(xlist);
    if (imagep(arg))
      imarray[nc++]=(IMAGE *)getimage(arg);
    else{
      xlbadtype(arg);
      break;
    }
  }

  xllastarg();

  return(cvimage(mcsort((IMAGE **)imarray, nc)));
}

LVAL imcsortlut()
{
  LVAL arg, xlist;
  IMAGE *imarray[maxNumberOfBands];
  IMAGE *lutarray[maxNumberOfBands];
  int ncim=0, nclut=0;

  if (!moreargs())
    xlabort("(*mcsortlut (list im1 [im2] [...] [imn]) (list lut1 [lut2] [...] [lutn]))\n");
/*
  \lspfunction{*}{mcsortlut}{listim listlut}
  \param{listim}{a list of image nodes}
  \param{lutim}{a list of corresponding look-up-tables}
  \return{a new image coding the input images}
  \desc{in development!}
  \cfunction{\cfmcsort}
  \cfile{mcsort.c}
*/
  xlist = xlgalist();

  for (ncim = 0; consp(xlist); xlist = cdr(xlist)){
    arg = car(xlist);
    if (imagep(arg))
      imarray[ncim++]=(IMAGE *)getimage(arg);
    else{
      xlbadtype(arg);
      break;
    }
  }
  xlist = xlgalist();

  for (nclut = 0; consp(xlist); xlist = cdr(xlist)){
    arg = car(xlist);
    if (imagep(arg))
      lutarray[nclut++]=(IMAGE *)getimage(arg);
    else{
      xlbadtype(arg);
      break;
    }
  }

  xllastarg();

  if (ncim != nclut)
    xlabort("mcsortlut: number of images and luts must be identical");

  return(cvimage(mcsortlut((IMAGE **)imarray, (IMAGE **)lutarray,ncim)));
}
