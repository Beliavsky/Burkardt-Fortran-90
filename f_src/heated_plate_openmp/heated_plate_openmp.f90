program main

!*****************************************************************************80
!
!! MAIN is the main program for HEATED_PLATE_OPENMP.
!
!  Discussion:
!
!    This code solves the steady state heat equation on a rectangular region.
!
!    The sequential version of this program needs approximately
!    18/eps iterations to complete. 
!
!
!    The physical region, and the boundary conditions, are suggested
!    by this diagram;
!
!                   W = 0
!             +------------------+
!             |                  |
!    W = 100  |                  | W = 100
!             |                  |
!             +------------------+
!                   W = 100
!
!    The region is covered with a grid of M by N nodes, and an N by N
!    array W is used to record the temperature.  The correspondence between
!    array indices and locations in the region is suggested by giving the
!    indices of the four corners:
!
!                  I = 0
!          [0][0]-------------[0][N-1]
!             |                  |
!      J = 0  |                  |  J = N-1
!             |                  |
!        [M-1][0]-----------[M-1][N-1]
!                  I = M-1
!
!    The steady state solution to the discrete heat equation satisfies the
!    following condition at an interior grid point:
!
!      W[Central] = (1/4) * ( W[North] + W[South] + W[East] + W[West] )
!
!    where "Central" is the index of the grid point, "North" is the index
!    of its immediate neighbor to the "north", and so on.
!   
!    Given an approximate solution of the steady state heat equation, a
!    "better" solution is given by replacing each interior point by the
!    average of its 4 neighbors - in other words, by using the condition
!    as an ASSIGNMENT statement:
!
!      W[Central]  <=  (1/4) * ( W[North] + W[South] + W[East] + W[West] )
!
!    If this process is repeated often enough, the difference between 
!    successive estimates of the solution will go to zero.
!
!    This program carries out such an iteration, using a tolerance specified by
!    the user, and writes the final estimate of the solution to a file that can
!    be used for graphic processing.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 October 2011
!
!  Author:
!
!    Original FORTRAN90 version by Michael Quinn.
!    This version by John Burkardt.
!
!  Reference:
!
!    Michael Quinn,
!    Parallel Programming in C with MPI and OpenMP,
!    McGraw-Hill, 2004,
!    ISBN13: 978-0071232654,
!    LC: QA76.73.C15.Q55.
!
!  Local parameters:
!
!    Local, real ( kind = 8 ) DIFF, the norm of the change in the solution from 
!    one iteration to the next.
!
!    Local, real ( kind = 8 ) MEAN, the average of the boundary values, used 
!    to initialize the values of the solution in the interior.
!
!    Local, real ( kind = 8 ) U(M,N), the solution at the previous iteration.
!
!    Local, real ( kind = 8 ) W(M,N), the solution computed at the latest 
!    iteration.
!
  use omp_lib

  implicit none

  integer ( kind = 4 ), parameter :: m = 500
  integer ( kind = 4 ), parameter :: n = 500

  real ( kind = 8 ) diff
  real ( kind = 8 ) :: eps = 0.001D+00
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iterations
  integer ( kind = 4 ) iterations_print
  integer ( kind = 4 ) j
  real ( kind = 8 ) mean
  real ( kind = 8 ) u(m,n)
  real ( kind = 8 ) w(m,n)
  real ( kind = 8 ) wtime

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HEATED_PLATE_OPENMP'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) &
    '  A program to solve for the steady state temperature distribution'
  write ( *, '(a)' ) '  over a rectangular plate.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8,a,i8,a)' ) '  Spatial grid of ', m, ' by ', n, ' points.'
  write ( *, '(a,g14.6)' ) &
    '  The iteration will repeat until the change is <= ', eps
  write ( *, '(a,i8)' ) &
    '  The number of processors available = ', omp_get_num_procs ( )
  write ( *, '(a,i8)' ) &
    '  The number of threads available    = ', omp_get_max_threads ( )
!
!  Set the boundary values, which don't change.
!
!  OpenMP Note:
!  You CANNOT set MEAN to zero inside the parallel region.
!
  mean = 0.0D+00
!
!$omp parallel shared ( w ) private ( i, j ) 

  !$omp do
  do i = 2, m - 1
    w(i,1) = 100.0D+00
    w(i,n) = 100.0D+00
  end do
  !$omp end do

  !$omp do
  do j = 1, n
    w(m,j) = 100.0D+00
    w(1,j) =   0.0D+00
  end do
  !$omp end do
!
!  Average the boundary values, to come up with a reasonable
!  initial value for the interior.
!
  !$omp do reduction ( + : mean )
  do i = 2, m - 1
    mean = mean + w(i,1) + w(i,n)
  end do
  !$omp end do

  !$omp do reduction ( + : mean )
  do j = 1, n
    mean = mean + w(1,j) + w(m,j)
  end do
  !$omp end do

!$omp end parallel
!
!  OpenMP note:
!  You cannot normalize MEAN inside the parallel region.  It
!  only gets its correct value once you leave the parallel region.
!  So we interrupt the parallel region, set MEAN, and go back in.
!
  mean = mean / dble ( 2 * m + 2 * n - 4 )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  MEAN = ', mean
!
!  Initialize the interior solution to the mean value.
!
!$omp parallel shared ( mean, w ) private ( i, j )

  !$omp do
  do j = 2, n - 1
    do i = 2, m - 1
      w(i,j) = mean
    end do
  end do
  !$omp end do

!$omp end parallel
!
!  Iterate until the  new solution W differs from the old solution U
!  by no more than EPS.
!
  iterations = 0
  iterations_print = 1

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' Iteration  Change'
  write ( *, '(a)' ) ' '

  wtime = omp_get_wtime ( )

  diff = eps

  do while ( eps <= diff )
!
!  OpenMP node: You CANNOT set DIFF to 0.0 inside the parallel region.
!
    diff = 0.0D+00

!$omp parallel shared ( u, w ) private ( i, j ) 

    !$omp do
    do j = 1, n
      do i = 1, m
        u(i,j) = w(i,j)
      end do
    end do
    !$omp end do

    !$omp do
    do j = 2, n - 1
      do i = 2, m - 1
        w(i,j) = 0.25D+00 * ( u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1) )
      end do
    end do
    !$omp end do

    !$omp do reduction ( max : diff )
    do j = 1, n
      do i = 1, m
        diff = max ( diff, abs ( u(i,j) - w(i,j) ) )
      end do
    end do
    !$omp end do

!$omp end parallel

    iterations = iterations + 1

    if ( iterations == iterations_print ) then
      write ( *, '(2x,i8,2x,g14.6)' ) iterations, diff
      iterations_print = 2 * iterations_print
    end if

  end do

  wtime = omp_get_wtime ( ) - wtime

  write ( *, '(a)' ) ' '
  write ( *, '(2x,i8,2x,g14.6)' ) iterations, diff
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Error tolerance achieved.'
  write ( *, '(a,g14.6)' ) '  Wall clock time = ', wtime
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HEATED_PLATE_OPENMP:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! timestamp prints the current YMDHMS date as a time stamp.
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
