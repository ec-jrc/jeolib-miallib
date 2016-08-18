/* vos fonctions d'interfacage comme dans xlglue1.c */

#ifdef EERIE

LVAL iSetStructElem()
{
  LVAL arg;

  if (!moreargs())
    xlabort("*SetStructElem Image\n");

  arg = xlgaimage();
  xllastarg();
  SetStructElem((IMAGE *)getimage(arg));
  return(s_true);
}

LVAL iSetRank()
{
  double R;

  if(!moreargs())
    xlabort("*SetRank Rank\n");
  
  R = (double)(getflonum(xlgaflonum()));
  xllastarg();
  SetRang(R);
  return(s_true);
}

LVAL iSetOrigin()
{
  int X, Y;

  if(!moreargs())
    xlabort("*SetOrigin X Y\n");
  
  X = (int)getfixnum(xlgafixnum());
  Y = (int)getfixnum(xlgafixnum());
  xllastarg();
  SetOrigin(X,Y);
  return(s_true);
}

LVAL iSetLineCenter()
{
  int C;

  if(!moreargs())
    xlabort("*SetLineCenter Offset\nOffset >= 0 & <line length");
  
  C = (int)getfixnum(xlgafixnum());
  xllastarg();
  SetCenterLine(C);
  return(s_true);
}

LVAL iApplyFilter()
{
  LVAL Arg1, Arg2;
  int Op;

  if(!moreargs())
    xlabort("*ApplyFilter Src Dest Op\nOp = 1 => EROSION\nop = 2 => DILATATION\nOp = 3 => MEDIAN\nOp = 4 => RANK\n");
  
  Arg1 = xlgaimage();
  Arg2 = xlgaimage();
  Op = (int)getfixnum(xlgafixnum());
  xllastarg();
  Apply((IMAGE *)getimage(Arg1), (IMAGE *)getimage(Arg2), Op);
  return(Arg2);
}

LVAL iApplyLineFilter()
{
  LVAL Arg1, Arg2;
  int Op, DX, DY, N;

  if(!moreargs())
    xlabort("*ApplyLineFilter Src Dest DX DY N Op\nOp = 1 => EROSION\nop = 2 => DILATATION\nOp = 3 => MEDIAN\nOp = 4 => RANK\n");
  
  Arg1 = xlgaimage();
  Arg2 = xlgaimage();
  DX = (int)getfixnum(xlgafixnum());
  DY = (int)getfixnum(xlgafixnum());
  N = (int)getfixnum(xlgafixnum());
  Op = (int)getfixnum(xlgafixnum());
  xllastarg();
  ApplyLine((IMAGE *)getimage(Arg1), (IMAGE *)getimage(Arg2), DX, DY, N, Op);
  return(Arg2);
}


#endif /* ifdef EERIE */
