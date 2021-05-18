subroutine u_init ( n, x, u0 )

!*****************************************************************************80
!
!! U_INIT supplies the initial value of U at each node.
!
!  Modified:
!
!    02 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Input, real ( kind = 8 ) X(N), the X coordinates of the nodes.
!
!    Output, real ( kind = 8 ) U0(N), the initial value of U at the nodes.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) u0(n)
  real ( kind = 8 ) x(n)

  u0(1:n) = exp ( - ( x(1:n) - real ( 100.0, kind = 8 ) )**2 ) &
    / real ( 5.0, kind = 8 )

  return
end
subroutine v_init ( n, x, v0 )

!*****************************************************************************80
!
!! V_INIT supplies the initial value of V at each node.
!
!  Modified:
!
!    02 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Input, real ( kind = 8 ) X(N), the X coordinates of the nodes.
!
!    Output, real ( kind = 8 ) U0(N), the initial value of U at the nodes.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) v0(n)
  real ( kind = 8 ) x(n)

  call r8_fake_use ( x(1) )

  v0(1:n) = real ( 2.0, kind = 8 ) / real ( 5.0, kind = 8 )

  return
end
subroutine r8_fake_use ( x )

!*****************************************************************************80
!
!! r8_fake_use pretends to use a variable.
!
!  Discussion:
!
!    Some compilers will issue a warning if a variable is unused.
!    Sometimes there's a good reason to include a variable in a program,
!    but not to use it.  Calling this function with that variable as
!    the argument will shut the compiler up.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the variable to be "used".
!
  implicit none

  real ( kind = 8 ) x

  if ( x /= x ) then
    write ( *, '(a)' ) '  r8_fake_use: variable is NAN.'
  end if

  return
end

