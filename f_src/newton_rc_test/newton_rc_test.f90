program main

!*****************************************************************************80
!
!! newton_rc_test tests newton_rc().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'newton_rc_test():'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test newton_rc(), to solve nonlinear equations'
  write ( *, '(a)' ) '  using reverse communication.'

  call newton_rc_test1 ( )
  call newton_rc_test2 ( )
  call newton_rc_test3 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'newton_rc_test():'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine newton_rc_test1 ( )

!*****************************************************************************80
!
!! newton_rc_test1 calls newton_rc() for 1 equation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 1

  real ( kind = 8 ) fx(n)
  real ( kind = 8 ) fxtol
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ido
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'newton_rc_test1'
  write ( *, '(a)' ) '  Use newton_rc() to solve a system of 1 nonlinear equation.'
!
!  Initialization.
!
  ido = 0
  fxtol = 0.000001D+00
  x(1:n) = 4.7D+00
  call test1 ( n, x, fx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Initial Values:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  X       FX'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, x(i), fx(i)
  end do
!
!  The solution loop.
!
  do

    call newton_rc ( fx, ido, n, x )

    if ( ido == 0 ) then

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Convergence:'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I  X       FX'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, x(i), fx(i)
      end do

      exit

    else if ( ido == 1 .or. ido == 2 ) then

      call test1 ( n, x, fx )

    else

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'newton_rc_test1:'
      write ( *, '(a)' ) '  An error seems to have occurred.'
      write ( *, '(a,i6)' ) '  Stopping with ido = ', ido
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I  X       FX'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, x(i), fx(i)
      end do

      exit

    end if

  end do

  return
end
subroutine test1 ( n, x, fx )

!*****************************************************************************80
!
!! test1 evaluates a nonlinear function of n variables.
!
!  Discussion:
!
!    This is Kepler's equation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) e
  real ( kind = 8 ) fx(n)
  real ( kind = 8 ) m
  real ( kind = 8 ) x(n)

  e = 0.8D+00
  m = 5.0D+00
  fx(1) = x(1) - m - e * sin ( x(1) )

  return
end
subroutine newton_rc_test2 ( )

!*****************************************************************************80
!
!! newton_rc_test2 calls newton_rc() for 4 equations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) fx(n)
  real ( kind = 8 ) fxtol
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ido
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'newton_rc_test2'
  write ( *, '(a)' ) '  Use newton_rc() to solve a system of 4 nonlinear equations.'
!
!  Initialization.
!
  ido = 0
  fxtol = 0.000001D+00
  x(1:n) = 0.0D+00
  call test2 ( n, x, fx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Initial Values:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  X       FX'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, x(i), fx(i)
  end do
!
!  The solution loop.
!
  do

    call newton_rc ( fx, ido, n, x )

    if ( ido == 0 ) then

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Convergence:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  X       FX'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, x(i), fx(i)
      end do

      exit

    else if ( ido == 1 .or. ido == 2 ) then

      call test2 ( n, x, fx )

    else

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'newton_rc_test2:'
      write ( *, '(a)' ) '  An error seems to have occurred.'
      write ( *, '(a,i6)' ) '  Stopping with ido = ', ido
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I  X       FX'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, x(i), fx(i)
      end do

      exit

    end if

  end do

  return
end
subroutine test2 ( n, x, fx )

!*****************************************************************************80
!
!! test2 evaluates a nonlinear function of n variables.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fx(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, n
    fx(i) = ( x(i) - i )**2
  end do

  return
end
subroutine newton_rc_test3 ( )

!*****************************************************************************80
!
!! newton_rc_test3 calls newton_rc() for 8 equations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 8

  real ( kind = 8 ) fx(n)
  real ( kind = 8 ) fxtol
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ido
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'newton_rc_test3'
  write ( *, '(a)' ) '  Use newton_rc() to solve a system of 8 nonlinear equations.'
!
!  Initialization.
!
  ido = 0
  fxtol = 0.000001D+00
  x(1:n) = 0.0D+00
  call test3 ( n, x, fx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Initial Values:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  X       FX'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, x(i), fx(i)
  end do
!
!  The solution loop.
!
  do

    call newton_rc ( fx, ido, n, x )

    if ( ido == 0 ) then

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Convergence:'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I  X       FX'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, x(i), fx(i)
      end do

      exit

    else if ( ido == 1 .or. ido == 2 ) then

      call test3 ( n, x, fx )

    else

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'newton_rc_test3:'
      write ( *, '(a)' ) '  An error seems to have occurred.'
      write ( *, '(a,i6)' ) '  Stopping with ido = ', ido
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I  X       FX'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, x(i), fx(i)
      end do

      exit

    end if

  end do

  return
end
subroutine test3 ( n, x, f )

!*****************************************************************************80
!
!! P13_FX evaluates the function for problem 13.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of variables and equations.
!
!    real ( kind = 8 ) X(N), the evaluation point.
!
!  Output:
!
!    real ( kind = 8 ) F(N), the function value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(n)

  do k = 1, n

    f(k) = ( 3.0D+00 - 2.0D+00 * x(k) ) * x(k) + 1.0D+00

    if ( 1 < k ) then
      f(k) = f(k) - x(k-1)
    end if

    if ( k < n ) then
      f(k) = f(k) - 2.0D+00 * x(k+1)
    end if

  end do

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! timestamp() prints the current YMDHMS date as a time stamp.
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

