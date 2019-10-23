/**
 * @file   shm.c
 * @author Pierre SOILLE
 * @date   Wed Sep 25 14:30:49 2013
 * 
 * @details
 * 
 * 
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <semaphore.h>

#include "mialib.h"


/** \addtogroup group_mem
 *  @{
 */

/*
** inspired by
** http://publib.boulder.ibm.com/infocenter/iseries/v5r3/topic/apis/apiexusmem.htm
** https://www.ibm.com/support/knowledgecenter/ssw_i5_54/apiref/apiexusmem.htm
**
** use shell command ipcs to monitor shared memory segments
**
** first: 20130925
**
** by Pierre.Soille@jrc.ec.europa.eu
*/



IMAGE *shmatimage(key_t shmkey, size_t nx, size_t ny, size_t nz, size_t nbyte, int type)
{
  void *shm_address;
  int shmid;
  size_t bpp;
  IMAGE *im;

  switch(type){
  case t_UCHAR:
    bpp=1;
    break;
  case t_USHORT:
  case t_SHORT:
    bpp=2;
    break;
  case t_UINT32:
  case t_INT32:
  case t_FLOAT:
    bpp=4;
    break;
  case t_DOUBLE:
    bpp=8;
    break;
  default:
    printf("shmatimage(): invalid type(=%d), detaching shared memory ...\n", type);
    return NULL;
  }

  if(nbyte<nx*ny*nz*bpp){
    printf("shmatimage(): error, nbyte is strictly less than nx*ny*nz*bpp!\n");
    return NULL;
  }

  /* Get the already created shared memory ID associated with key. */
  /* If the shared memory ID does not exist, then it will not be   */
  /* created, and an error will occur.                             */
  shmid = shmget(shmkey, nbyte, 0666);
  if (shmid == -1){
    printf("shmatimage(): shmget() failed for shmkey\n");
    return NULL;
  }

  /* Attach the shared memory segment to the client process.       */
  shm_address = shmat(shmid, NULL, 0);
  if ( shm_address==NULL ){
    printf("shmatimage(): shmat() failed\n");
    return NULL;
  }

  im = (IMAGE *)calloc((size_t)1, sizeof(IMAGE));
  SetImDataType(im, type);
  /* dirty trick used for shm originating from MatLab SharedMatrix procedure.
     It does not work fully: the 6 first pixels after releasing the memory
     to MatLab are always seen as zeros by Matlab!  20131008
     In practice, nbyte is expected to be equal to nx*ny*nz*bpp */
  if(nbyte>nx*ny*nz*bpp)
    printf("shmatimage(): warning, nbyte is larger than nx*ny*nz*bpp.  Assuming padding at the start of shared memory\n");
  SetImNByte(im,nx*ny*nz*bpp);
  SetImPtr(im, (char *)shm_address+nbyte-nx*ny*nz*bpp);
  SetImNx(im,nx);
  SetImNy(im,ny);
  SetImNz(im,nz);   

  return im;
}

ERROR_TYPE shmdtimage(void *shm_address, int semkey_flag, key_t semkey)
{
  void *sem_address;
  int semid;
  int rc;

  /* Detach the shared memory segment from the current process.    */
  rc = shmdt(shm_address);
  if (rc==-1)      {
    printf("shdtimage(): shmdt() failed\n");
    return -1;
  }

  /* if semkey_flag !=0                                            */
  /* Get the already created shared memory ID associated with key. */
  /* If the shared memory ID does not exist, then it will not be   */
  /* created, and an error will occur.                             */

  if (semkey_flag){
    semid = shmget(semkey,sizeof(sem_t), 0666);
    if (semid == -1){
      printf("shmdtimage(): shmget() failed for semkey\n");
      return ERROR;
    }

    /* Attach the shared memory segment of semaphore to the client process */
    sem_address = (sem_t*) shmat(semid, NULL, 0);
    if ( sem_address==NULL ){
      printf("shmatimage(): shmat() failed\n");
      return ERROR;
    }

    rc=sem_post(sem_address);

    if (rc == -1){
      printf("sem_post(): sem_post() failed\n");
      return -1;
    }
  }
  return NO_ERROR;
}

/* unused code at the moment, based on *named* semaphores */
#define NUMSEMS 2

IMAGE *shmatimage_named_semaphores(key_t semkey, key_t shmkey, size_t nx, size_t ny, size_t nz, size_t nbyte, int type)
{
    struct sembuf operations[2];
    void         *shm_address;
    int semid, shmid, rc;

    IMAGE *im;

    /* Get the already created semaphore ID associated with key.     */
    /* If the semaphore set does not exist, then it will not be      */
    /* created, and an error will occur.                             */
    semid = semget( semkey, NUMSEMS, 0666);
    if ( semid == -1 ){
        printf("shmatimage: semget() failed\n");
        return NULL;
      }


    /* Get the already created shared memory ID associated with key. */
    /* If the shared memory ID does not exist, then it will not be   */
    /* created, and an error will occur.                             */

    shmid = shmget(shmkey, nbyte, 0666);
    if (shmid == -1){
      printf("main: shmget() failed\n");
      return NULL;
    }

    /* Attach the shared memory segment to the client process.       */
    shm_address = shmat(shmid, NULL, 0);
    if ( shm_address==NULL ){
      printf("shmatimage: shmat() failed\n");
      return NULL;
    }

    /* First, check to see if the first semaphore is a zero.  If it  */
    /* is not, it is busy right now.  The semop() command will wait  */
    /* for the semaphore to reach zero before running the semop().   */
    /* When it is zero, increment the first semaphore to show that   */
    /* the shared memory segment is busy.                            */
    operations[0].sem_num = 0;
                                    /* Operate on the first sem      */
    operations[0].sem_op =  0;
                                    /* Wait for the value to be=0    */
    operations[0].sem_flg = 0;
                                    /* Allow a wait to occur         */

    operations[1].sem_num = 0;
                                    /* Operate on the first sem      */
    operations[1].sem_op =  1;
                                    /* Increment the semval by one   */
    operations[1].sem_flg = 0;
                                    /* Allow a wait to occur         */

    rc = semop( semid, operations, 2 );
    if (rc == -1){
      printf("shmatimage: semop() failed\n");
      return NULL;
    }

    im = (IMAGE *)calloc((size_t)1, sizeof(IMAGE));

    SetImDataType(im, type);
    SetImNByte(im,nbyte);
    SetImPtr(im,shm_address);
    SetImNx(im,nx);
    SetImNy(im,ny);
    SetImNz(im,nz);   

    return im;
}

ERROR_TYPE shmdtimage_named_semaphores(key_t semkey, void *shm_address)
{
  struct sembuf operations[2];
  int semid;
  int rc;

  /* Release the shared memory segment by decrementing the in-use  */
  /* semaphore (the first one).  Increment the second semaphore to */
  /* show that the client is finished with it.                     */
  operations[0].sem_num = 0;
  /* Operate on the first sem      */
  operations[0].sem_op =  -1;
  /* Decrement the semval by one   */
  operations[0].sem_flg = 0;
  /* Allow a wait to occur         */
  operations[1].sem_num = 1;
  /* Operate on the second sem     */
  operations[1].sem_op =  1;
  /* Increment the semval by one   */
  operations[1].sem_flg = 0;
  /* Allow a wait to occur         */

  /* Get the already created semaphore ID associated with key.     */
  /* If the semaphore set does not exist, then it will not be      */
  /* created, and an error will occur.                             */
  semid = semget( semkey, NUMSEMS, 0666);
  if ( semid == -1 ){
    printf("shmdtimage: semget() failed\n");
    return -1;
  }
 
  rc = semop( semid, operations, 2 );

  if (rc == -1)      {
    printf("shdtimage: semop() failed\n");
    return -1;
  }

  /* Detach the shared memory segment from the current process.    */
  rc = shmdt(shm_address);
  if (rc==-1)      {
    sprintf(buf, "shdtimage: shmdt() failed\n"); errputstr(buf);
    return -1;
  }

  return NO_ERROR;
}

/*@}*/
