subroutine cuda_loop ( blocks, threads, n )

!*****************************************************************************80
!
!! CUDA_LOOP simulates the behavior of a CUDA loop.
!
!  Discussion:
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
!      given by the user in "blocks";
!
!      blockDim.x, blockDim.y, blockDim.z: the thread dimensions as
!      given by the user in "threads";
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
!      carry out task T;
!      T = T + blockDim.x * blockDim.y * blockDim.z * gridDim.x * gridDim.y * gridDim.z.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) BLOCKS[3], the CUDA block values.  These 
!    should be nonnegative.  Typically, the third entry is 1.  Generally, 
!    the first two values cannot be greater than 35,535.
!
!    Input, integer ( kind = 4 ) THREADS[3], the CUDA thread values.  These 
!    should be nonnegative.  Typically, there is a maximum value imposed on 
!    these quantities, which depends on the GPU model.
!
!    Input, integer ( kind = 4 ) N, the number of tasks to be carried out.
!
  implicit none

  integer ( kind = 4 ) blockDimx
  integer ( kind = 4 ) blockDimy
  integer ( kind = 4 ) blockDimz
  integer ( kind = 4 ) blockIdx
  integer ( kind = 4 ) blockIdy
  integer ( kind = 4 ) blockIdz
  integer ( kind = 4 ) blocks(3)
  integer ( kind = 4 ) chunk
  integer ( kind = 4 ) gridDimx
  integer ( kind = 4 ) gridDimy
  integer ( kind = 4 ) gridDimz
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) k2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) t
  integer ( kind = 4 ) threadIdx
  integer ( kind = 4 ) threadIdy
  integer ( kind = 4 ) threadIdz
  integer ( kind = 4 ) threads(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CUDA_LOOP:'
  write ( *, '(a)' ) '  Simulate the assignment of N tasks to the blocks'
  write ( *, '(a)' ) '  and threads of a GPU using CUDA.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of tasks is ', n
  write ( *, '(a,i4,a,i4,a,i4,a)' ) &
    '  BLOCKS:  { ', blocks(1), ',', blocks(2), ',', blocks(3), '}'
  write ( *, '(a,i4,a,i4,a,i4,a)' ) &
    '  THREADS: { ', threads(1), ',', threads(2), ',', threads(3), '}'

  k1 = 0

  blockDimx = threads(1)
  blockDimy = threads(2)
  blockDimz = threads(3)

  gridDimx = blocks(1)
  gridDimy = blocks(2)
  gridDimz = blocks(3)

  chunk = blocks(2) * blocks(1) * threads(3) * threads(2) * threads(1)
  write ( *, '(a,i6)' ) '  Total threads = ', chunk
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Process   Process (bx,by,bz) (tx,ty,tz)  Tasks...'
  write ( *, '(a)' ) '  Increment Formula'
  write ( *, '(a)' ) ''

  do blockIdz = 0, gridDimz - 1
    do blockIdy = 0, gridDimy - 1
      do blockIdx = 0, gridDimx - 1
        do threadIdz = 0, blockDimz - 1
          do threadIdy = 0, blockDimy - 1
            do threadIdx = 0, blockDimx - 1
              t = k1
              k2 = &
                threadIdx &
                + blockDimx * threadIdy &
                + blockDimx * blockDimy * threadIdz &
                + blockDimx * blockDimy * blockDimz * blockIdx &
                + blockDimx * blockDimy * blockDimz * gridDimx * blockIdy &
                + blockDimx * blockDimy * blockDimz * gridDimx * gridDimy * blockIdz

              write ( *, '(2x,i7,2x,i7,a,i2,a,i2,a,i2,a,i2,a,i2,a,i2,a)', &
                advance = 'no' ) &
                k1, k2, ': (',blockIdx, ',', blockIdy, ',', blockIdz, ') (', &
                threadIdx, ',', threadIdy, ',', threadIdz, ')'
              do while ( t < n )
                write ( *, '(i3)', advance = 'no' ) t
                t = t + chunk
              end do
              write ( *, '(a)' ) ''
              k1 = k1 + 1
            end do
          end do
        end do
      end do
    end do
  end do

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end

