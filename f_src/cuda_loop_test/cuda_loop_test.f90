program main

!******************************************************************************/
!
!! MAIN is the main program for CUDA_LOOP_TEST.
!
!  Discussion:
!
!    CUDA_LOOP_TEST demonstrates CUDA_LOOP.
!
!    A CUDA kernel "kernel()" is invoked by a command of the form
!      
!      kernel << blocks, threads >> ( args )
!
!    where blocks and threads are each vectors of up to 3 values,
!    listing the number of blocks and number of threads to be used.
!
!    If a problem involves N tasks, then tasks are allotted to 
!    specific CUDA processes in an organized fashion.  Some processes
!    may get no tasks, one task, or multiple tasks.  
!
!    Each process is given variables that can be used to determine
!    the tasks to be performed:
!
!      gridDim.x, gridDim.y, gridDim.z: the block dimensions as
!      given by the user in "blocks"
!
!      blockDim.x, blockDim.y, blockDim.z: the thread dimensions as
!      given by the user in "threads"
!
!      blockIdx.x, blockIdx.y, blockId.z: the block indices for this process.
!
!      threadIdx.x, threadIdx.y, threadIdx.z: the thread indices for this process.
!
!    Essentially, a process can determine its linear index K by:
!
!      K = threadIdx.x
!        +  blockdim.x  * threadIdx.y
!        +  blockDim.x  *  blockDim.y  * threadIdx.z
!        +  blockDim.x  *  blockDim.y  *  blockDim.z  * blockIdx.x
!        +  blockDim.x  *  blockDim.y  *  blockDim.z  *  gridDim.x  * blockIdx.y
!        +  blockDim.x  *  blockDim.y  *  blockDim.z  *  gridDim.x  *  gridDim.y  * blockIdx.z
!
!    Set task T = K.
!
!    while ( T < N )
!      carry out task T
!      T = T + blockDim.x * blockDim.y * blockDim.z * gridDim.x * gridDim.y * gridDim.z.
!
!    This program suggests how a specific set of block and thread parameters 
!    would determine the assignment of individual tasks to CUDA processes.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 March 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Local, integer ( kind = 4 ) BLOCKS(3), the CUDA block values.  These 
!    should be nonnegative.  Typically, the third entry is 1.  Generally, the 
!    first two values cannot be greater than 35,535.
!
!    Local, int THREADS(3), the CUDA thread values.  These should be 
!    nonnegative.  Typically, there is a maximum value imposed on these 
!    quantities, which depends on the GPU model.
!
!    Local, int N, the number of tasks to be carried out.
!
  implicit none

  integer ( kind = 4 ) blocks(3)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) threads(3)

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CUDA_LOOP_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Simulate the way CUDA breaks into iterative task, using'
  write ( *, '(a)' ) '  blocks and threads.'
!
!  Linear array of blocks and threads.
!  Essentially, blocks = your hands and threads = your fingers.
!  Now count up to 23..
!
  blocks(1) = 2
  blocks(2) = 1
  blocks(3) = 1
  threads(1) = 5
  threads(2) = 1
  threads(3) = 1
  n = 23
 
  call cuda_loop ( blocks, threads, n )
!
!  Unit arrays of blocks and threads.
!  Waste your GPU by having a single block and thread do everything.
!
  blocks(1) = 1
  blocks(2) = 1
  blocks(3) = 1
  threads(1) = 1
  threads(2) = 1
  threads(3) = 1
  n = 23

  call cuda_loop ( blocks, threads, n )
!
!  2D block array, 3D thread array.
!  More processes than tasks.
!
  blocks(1) = 2
  blocks(2) = 3
  blocks(3) = 1
  threads(1) = 2
  threads(2) = 1
  threads(3) = 4
  n = 40
 
  call cuda_loop ( blocks, threads, n )
!
!  One block, 8 threads.
! 
  blocks(1) = 1
  blocks(2) = 1
  blocks(3) = 1
  threads(1) = 2
  threads(2) = 2
  threads(3) = 2
  n = 23
 
  call cuda_loop ( blocks, threads, n )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CUDA_LOOP_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
