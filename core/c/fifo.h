
/*
 ** FIFO type definition
 */



typedef struct {
  long int *qp;     /* Pointer to circular queue    */
  long int *qps;    /* Pointer to storage position  */
  long int *qpr;    /* Pointer to retrieve position */
  long int *qplast; /* Pointer to last+1 element    */
  long int *qpl;    /* Pointer to look position     */
  long int mod;    /* Modulation factor            */
} FIFO4;

#define  FICT_PIX  1

extern FIFO4 *create_fifo4(long int);
extern void  fifo4_add(FIFO4 *, long int);
extern long int  fifo4_remove(FIFO4 *);
extern long  int fifo4_look(FIFO4 *);
extern void fifo4_lookreset(FIFO4 *);
extern long  int fifo4_empty(FIFO4 *);
extern void  fifo4_increase(FIFO4 *);
extern void  free_fifo4(FIFO4 *);
extern void fifo4_flush(FIFO4 *);
