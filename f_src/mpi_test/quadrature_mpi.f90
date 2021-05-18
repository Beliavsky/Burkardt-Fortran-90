program main

!*****************************************************************************80
!
!! MAIN is the main program for QUADRATURE.
!
!  Discussion:
!
!    QUADRATURE estimates an integral using quadrature.
!
!    The integral of F(X) = 4 / ( 1 + X * X ) from 0 to 1 is PI.
!
!    We break up the interval [0,1] into N subintervals, evaluate
!    F(X) at the midpoint of each subinterval, and multiply the
!    sum of these values by N to get an estimate for the integral.
!
!    If we have M processes available because we are using MPI, then
!    we can ask processes 0, 1, 2, ... M-1 to handle the subintervals
!    in the following order:
!
!          0      1       2            M-1  <-- Process numbers begin at 0
!     ------ ------  ------  -----  ------
!          1      2       3    ...       M
!        M+1    M+2     M+3    ...     2*M
!      2*M+1    2*M+2 2*M+3    ...     3*M
!                              
!    and so on up to subinterval N.  The partial sums collected by 
!    each process are then sent to the master process to be added 
!    together to get the estimated integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 October 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!    Snir, Otto, Huss-Lederman, Walker, Dongarra,
!    MPI - The Complete Reference,
!    Volume 1, The MPI Core,
!    second edition,
!    MIT Press, 1998.
!
  use mpi

  real ( kind = 8 ) f
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) id
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) my_part
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_part
  integer ( kind = 4 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) q_diff
  real ( kind = 8 ), parameter :: q_exact = 3.141592653589793238462643D+00
  real ( kind = 8 ) q_part
  real ( kind = 8 ) sum2
  real ( kind = 8 ) wtime_diff
  real ( kind = 8 ) wtime_end
  real ( kind = 8 ) wtime_start
  real ( kind = 8 ) x
!
!  Initialize MPI.
!
  call MPI_Init ( ierr )
!
!  Get this process's ID.
!
  call MPI_Comm_rank ( MPI_COMM_WORLD, id, ierr )
!
!  Find out how many processes are available.
!
  call MPI_Comm_size ( MPI_COMM_WORLD, p, ierr )

  if ( id == 0 ) then
    call timestamp ( )
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'QUADRATURE_MPI:'
    write ( *, '(a)' ) '  FORTRAN90 version'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  An MPI example program to estimate an integral.'
    write ( *, '(a,i8)' ) '  The number of processes is ', p

    wtime_start = MPI_Wtime ( )

  end if

  write ( *, '(a,i8,a)' ) 'Process ', id, ' is active.'
!
!  Assume that the master process just got the value of N from the user.
!  Here, we'll use an assignment statement.
!
  if ( id == 0 ) then
    n = 1000
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'QUADRATURE - Master process:'
    write ( *, '(a,i8,a)' ) '  Number of intervals being used is ', n
  end if
!
!  The master process broadcasts the value of N to all other processes.
!
  call MPI_Bcast ( n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr )
!
!  Every process, including the master, now adds up its terms.
!
!  There are N evaluation points total.
!  Each evaluation point is in the center of an interval of width 1/N.
!
  h = 1.0D+00 / real ( n, kind = 8 )

  q_part = 0.0D+00
  n_part = 0

  do i = id + 1, n, p

    x = real ( 2 * i - 1, kind = 8 ) &
      / real ( 2 * n,     kind = 8 )

    n_part = n_part + 1
    q_part = q_part + f ( x )

  end do

  q_part = q_part * h

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) 'QUADRATURE - Process ', id
  write ( *, '(a,i8)' ) '  Points used ', n_part
  write ( *, '(a,g24.16)' ) '  Estimate: ', q_part
!
!  Each process sends its value of Q_PART to the master process, to
!  be summed in Q.
!
  call MPI_Reduce ( q_part, q, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, &
    MPI_COMM_WORLD, ierr )
!
!  The master process scales the sum by H and prints the answer.
!
  if ( id == 0 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'QUADRATURE - Master process:'
    write ( *, '(a,g24.16)' ) '  Integral estimate  ', q
    write ( *, '(a,g24.16)' ) '  Exact value is     ', q_exact
    q_diff = abs ( q_exact - q )
    write ( *, '(a,g24.16)' ) '  Error is           ', q_diff

    wtime_end = MPI_Wtime ( )
    wtime_diff = wtime_end - wtime_start

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Elapsed wall clock seconds = ', wtime_diff

  end if
!
!  Finish up.
!
  call MPI_Finalize ( ierr )

  if ( id == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'QUADRATURE - Master process:'
    write ( *, '(a)' ) '  Normal end of execution.'
    write ( *, '(a)' ) ' '
    call timestamp ( )
  end if

  stop
end
function f ( x )

!*****************************************************************************80
!
!! F is the function we are integrating.
!
!  Discussion:
!
!    Integral ( 0 <= X <= 1 ) 4/(1+X*X) dX = PI
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) F, the value of the function.
!
  implicit none

  real ( kind = 8 ) f
  real ( kind = 8 ) x

  f = 4.0D+00 / ( 1.0D+00 + x * x )

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
