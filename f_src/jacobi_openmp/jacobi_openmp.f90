program main

!*****************************************************************************80
!
!! MAIN is the main program for JACOBI_OPENMP.
!
!  Discussion:
!
!    JACOBI_OPENMP carries out a Jacobi iteration with OpenMP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 January 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 50000

  real ( kind = 8 ) b(n)
  real ( kind = 8 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it
  integer ( kind = 4 ) m
  real ( kind = 8 ) r
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xnew(n)

  m = 5000

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'JACOBI_OPENMP:'
  write ( *, '(a)' ) '  Fortran90/OpenMP version'
  write ( *, '(a)' ) '  Jacobi iteration to solve A*x=b.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of variables  N = ', n
  write ( *, '(a,i8)' ) '  Number of iterations M = ', m

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  IT     l2(dX)    l2(resid)'
  write ( *, '(a)' ) ''

!$omp parallel private ( i )
!
!  Set up the right hand side.
!
!$omp do
  do i = 1, n - 1
    b(i) = 0.0D+00
  end do
!$omp end do
  b(n) = real ( n + 1, kind = 8 )
!
!  Initialize the solution estimate to 0.
!  Exact solution is (1,2,3,...,N).
!
!$omp do
  do i = 1, n
    x(i) = 0.0D+00
  end do
!$omp end do
!
!$omp end parallel
!
!  Iterate M times.
!
  do it = 1, m

!$omp parallel private ( i, t )
!
!  Jacobi update.
!
!$omp do
    do i = 1, n
      xnew(i) = b(i) 
      if ( 1 < i ) then
        xnew(i) = xnew(i) + x(i-1)
      end if
      if ( i < n ) then
        xnew(i) = xnew(i) + x(i+1)
      end if
      xnew(i) = xnew(i) / 2.0D+00
    end do
!$omp end do
!
!  Difference.
!
    d = 0.0D+00
!$omp do reduction ( + : d )
    do i = 1, n
      d = d + ( x(i) - xnew(i) ) ** 2
    end do
!$omp end do
!
!  Overwrite old solution.
!
!$omp do
    do i = 1, n
      x(i) = xnew(i)
    end do
!$omp end do
!
!  Residual.
!
    r = 0.0D+00
!$omp do reduction ( + : r )
    do i = 1, n
      t = b(i) - 2.0D+00 * x(i)
      if ( 1 < i ) then
        t = t + x(i-1)
      end if
      if ( i < n ) then
        t = t + x(i+1)
      end if
      r = r + t * t
    end do
!$omp end do

!$omp master
    if ( it <= 10 .or. m - 10 <= it ) then
      write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) it, sqrt ( d ), sqrt ( r )
    end if
    if ( it == 10 ) then
      write ( *, '(a)' ) '  Omitting intermediate results.'
    end if
!$omp end master

!$omp end parallel

  end do
!
!  Write part of final estimate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Part of final solution estimate:'
  write ( *, '(a)' ) ''
  do i = 1, 10
    write ( *, '(2x,i8,2x,g14.6)' ) i, x(i)
  end do
  write ( *, '(a)' ) '...'
  do i = n - 10, n
    write ( *, '(2x,i8,2x,g14.6)' ) i, x(i)
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'JACOBI_OPENMP:'
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

