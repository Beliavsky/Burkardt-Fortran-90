program main

!*****************************************************************************80
!
!! MAIN is the main program for MXM_OPENMP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 October 2011
!
!  Author:
!
!    John Burkardt
!
  use omp_lib

  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) angle
  real ( kind = 8 ), allocatable :: b(:,:)
  real ( kind = 8 ), allocatable :: c(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) s
  integer ( kind = 4 ) thread_num
  real ( kind = 8 ) wtime

  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MXM_OPENMP:'
  write ( *, '(a)' ) '  FORTRAN90/OpenMP version'
  write ( *, '(a)' ) '  Compute matrix product C = A * B.'

  thread_num = omp_get_max_threads ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) &
    '  The number of processors available = ', omp_get_num_procs ( )
  write ( *, '(a,i8)' ) '  The number of threads available    = ', thread_num

  n = 2000
  write ( *, '(a,i8)' ) '  The matrix order N                 = ', n
  allocate ( a(1:n,1:n) )
  allocate ( b(1:n,1:n) )
  allocate ( c(1:n,1:n) )
!
!  Loop 1: Evaluate A.
!
  s = 1.0D+00 / sqrt ( real ( n, kind = 8 ) )

  wtime = omp_get_wtime ( )

!$omp parallel shared ( a, b, c, n, s ) private ( angle, i, j, k )

  !$omp do
  do i = 1, n
    do j = 1, n
      angle = 2.0D+00 * pi * ( i - 1 ) * ( j - 1 ) / real ( n, kind = 8 )
      a(i,j) = s * ( sin ( angle ) + cos ( angle ) ) 
    end do
  end do
  !$omp end do
!
!  Loop 2: Copy A into B.
!
  !$omp do
  do i = 1, n
    do j = 1, n
      b(i,j) = a(i,j)
    end do
  end do
  !$omp end do
!
!  Loop 3: Compute C = A * B.
!
  !$omp do
  do i = 1, n
    do j = 1, n
      c(i,j) = 0.0D+00
      do k = 1, n
        c(i,j) = c(i,j) + a(i,k) * b(k,j)
      end do
    end do
  end do
  !$omp end do

!$omp end parallel

  wtime = omp_get_wtime ( ) - wtime
  write ( *, '(a,g14.6)' ) '  Elapsed seconds = ', wtime
  write ( *, '(a,g14.6)' ) '  C(100,100)  = ', c(100,100)
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( b )
  deallocate ( c )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MXM_OPENMP:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
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
