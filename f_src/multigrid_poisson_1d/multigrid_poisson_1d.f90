subroutine ctof ( nc, uc, nf, uf )

!*****************************************************************************80
!                                                    
!! CTOF transfers data from a coarse to a finer grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Hager,
!    Applied Numerical Linear Algebra,
!    Prentice-Hall, 1988,
!    ISBN13: 978-0130412942,
!    LC: QA184.H33.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NC, the number of coarse nodes.
!
!    Input, real ( kind = 8 ) UC(NC), the coarse correction data.
!
!    Input, integer ( kind = 4 ) NF, the number of fine nodes.
!
!    Input/output, real ( kind = 8 ) UF(NF), on input, the fine grid data.
!    On output, the data has been updated with prolonged coarse 
!    correction data.
!
  implicit none

  integer ( kind = 4 ) nc
  integer ( kind = 4 ) nf

  integer ( kind = 4 ) ic
  integer ( kind = 4 ) if
  real ( kind = 8 ) uc(nc)
  real ( kind = 8 ) uf(nf)

  do ic = 1, nc
    if = 2 * ic - 1
    uf(if) = uf(if) + uc(ic)
  end do

  do ic = 1, nc - 1
    if = 2 * ic
    uf(if) = uf(if) + 0.5D+00 * ( uc(ic) + uc(ic+1) )
  end do

  return
end
subroutine ftoc ( nf, uf, rf, nc, uc, rc )

!*****************************************************************************80
!                                                    
!! FTOC transfers data from a fine grid to a coarser grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Hager,
!    Applied Numerical Linear Algebra,
!    Prentice-Hall, 1988,
!    ISBN13: 978-0130412942,
!    LC: QA184.H33.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NF, the number of fine nodes.
!
!    Input, real ( kind = 8 ) UF(NF), the fine data.
!
!    Input, real ( kind = 8 ) RF(NF), the right hand side for the fine grid.
!
!    Input, integer ( kind = 4 ) NC, the number of coarse nodes.
!
!    Output, real ( kind = 8 ) UC(NC), the coarse grid data, set to zero.
!
!    Output, real ( kind = 8 ) RC(NC), the right hand side for the coarse grid.
!
  implicit none

  integer ( kind = 4 ) nc
  integer ( kind = 4 ) nf

  integer ( kind = 4 ) ic
  integer ( kind = 4 ) if
  real ( kind = 8 ) rc(nc)
  real ( kind = 8 ) rf(nf)
  real ( kind = 8 ) uc(nc)
  real ( kind = 8 ) uf(nf)

  uc(1) = 0.0D+00
  rc(1) = 0.0D+00
  do ic = 2, nc - 1
    if = 2 * ic - 1
    uc(ic) = 0.0D+00
    rc(ic) = 4.0D+00 * ( rf(if) + uf(if-1) - 2.0D+00 * uf(if) + uf(if+1) )
  end do
  uc(nc) = 0.0D+00
  rc(nc) = 0.0D+00

  return
end
subroutine gauss_seidel ( n, r, u, dif_l1 )

!*****************************************************************************80
!                                                    
!! GAUSS_SEIDEL carries out one step of a Gauss-Seidel iteration.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Hager,
!    Applied Numerical Linear Algebra,
!    Prentice-Hall, 1988,
!    ISBN13: 978-0130412942,
!    LC: QA184.H33.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of unknowns.
!
!    Input, real ( kind = 8 ) R(N), the right hand side.
!
!    Input/output, real ( kind = 8 ) U(N), the estimated solution.
!
!    Output, real ( kind = 8 ) DIF_L1, the L1 norm of the difference between the
!    input and output solution estimates.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) dif_l1
  integer ( kind = 4 ) i
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) u_old

  dif_l1 = 0.0D+00

  do i = 2, n - 1
    u_old = u(i)
    u(i) = 0.5D+00 * ( u(i-1) + u(i+1) + r(i) )
    dif_l1 = dif_l1 + abs ( u(i) - u_old )
  end do

  return
end
function i4_log_2 ( i )

!*****************************************************************************80
!
!! I4_LOG_2 returns the integer part of the logarithm base 2 of an I4.
!
!  Discussion:
!
!    For positive I4_LOG_2(I), it should be true that
!      2^I4_LOG_2(X) <= |I| < 2^(I4_LOG_2(I)+1).
!    The special case of I4_LOG_2(0) returns -HUGE().
!
!    An I4 is an integer ( kind = 4 ) value.
!
!  Example:
!
!     I  I4_LOG_2
!
!     0  -1
!     1,  0
!     2,  1
!     3,  1
!     4,  2
!     5,  2
!     6,  2
!     7,  2
!     8,  3
!     9,  3
!    10,  3
!   127,  6
!   128,  7
!   129,  7
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the number whose logarithm base 2
!    is desired.
!
!    Output, integer ( kind = 4 ) I4_LOG_2, the integer part of the
!    logarithm base 2 of the absolute value of I.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_abs
  integer ( kind = 4 ) i4_log_2
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647

  if ( i == 0 ) then

    i4_log_2 = - i4_huge

  else

    i4_log_2 = 0

    i_abs = abs ( i )

    do while ( 2 <= i_abs )
      i_abs = i_abs / 2
      i4_log_2 = i4_log_2 + 1
    end do

  end if

  return
end
subroutine monogrid_poisson_1d ( n, a, b, ua, ub, force, exact, it_num, u )

!*****************************************************************************80
!                                                    
!! MONOGRID_POISSON_1D solves a 1D PDE, using the Gauss-Seidel method.
!
!  Discussion:
!
!    This routine solves a 1D boundary value problem of the form
!
!      - U''(X) = F(X) for A < X < B,
!
!    with boundary conditions U(A) = UA, U(B) = UB.
!
!    The Gauss-Seidel method is used. 
!
!    This routine is provided primarily for comparison with the
!    multigrid solver.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Hager,
!    Applied Numerical Linear Algebra,
!    Prentice-Hall, 1988,
!    ISBN13: 978-0130412942,
!    LC: QA184.H33.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of intervals.
!
!    Input, real ( kind = 8 ) A, B, the endpoints.
!
!    Input, real ( kind = 8 ) UA, UB, the boundary values at the endpoints.
!
!    Input, external real ( kind = 8 ) FORCE, the name of the function 
!    which evaluates the right hand side.
!
!    Input, external real ( kind = 8 ) EXACT, the name of the function 
!    which evaluates the exact solution.
!
!    Output, integer ( kind = 4 ) IT_NUM, the number of iterations.
!
!    Output, real ( kind = 8 ) U(N+1), the computed solution.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) d1
  real ( kind = 8 ), external :: exact
  real ( kind = 8 ), external :: force
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_num
  real ( kind = 8 ) r(n+1)
  real ( kind = 8 ) tol
  real ( kind = 8 ) u(n+1)
  real ( kind = 8 ) ua
  real ( kind = 8 ) ub
  real ( kind = 8 ) x(n+1)
!
!  Initialization.
!
  tol = 0.0001D+00
!
!  Set the nodes.
!
  call r8vec_linspace ( n + 1, a, b, x )
!
!  Set the right hand side.
!
  r(1) = ua
  h = ( b - a ) / real ( n, kind = 8 )
  do i = 2, n
    r(i) = h**2 * force ( x(i) )
  end do
  r(n+1) = ub

  u(1:n+1) = 0.0D+00

  it_num = 0
!
!  Gauss-Seidel iteration.
!
  do

    it_num = it_num + 1

    call gauss_seidel ( n + 1, r, u, d1 )

    if ( d1 <= tol ) then
      exit
    end if

  end do

  return
end
subroutine multigrid_poisson_1d ( n, a, b, ua, ub, force, exact, it_num, u )

!*****************************************************************************80
!                                                    
!! MULTIGRID_POISSON_1D solves a 1D PDE using the multigrid method.
!
!  Discussion:
!
!    This routine solves a 1D boundary value problem of the form
!
!      - U''(X) = F(X) for A < X < B,
!
!    with boundary conditions U(A) = UA, U(B) = UB.
!
!    The multigrid method is used. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by William Hager.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Hager,
!    Applied Numerical Linear Algebra,
!    Prentice-Hall, 1988,
!    ISBN13: 978-0130412942,
!    LC: QA184.H33.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of intervals.
!    N must be a power of 2.
!
!    Input, real ( kind = 8 ) A, B, the endpoints.
!
!    Input, real ( kind = 8 ) UA, UB, the boundary values at the endpoints.
!
!    Input, external real ( kind = 8 ) FORCE, the name of the function 
!    which evaluates the right hand side.
!
!    Input, external real ( kind = 8 ) EXACT, the name of the function 
!    which evaluates the exact solution.
!
!    Output, integer ( kind = 4 ) IT_NUM, the number of iterations.
!
!    Output, real ( kind = 8 ) U(N+1), the computed solution.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) d0
  real ( kind = 8 ) d1
  real ( kind = 8 ), external :: exact
  real ( kind = 8 ), external :: force
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_log_2
  integer ( kind = 4 ) it
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) ll
  integer ( kind = 4 ) m
  integer ( kind = 4 ) nl
  real ( kind = 8 ), allocatable :: r(:)
  real ( kind = 8 ) tol
  real ( kind = 8 ) u(n+1)
  real ( kind = 8 ) ua
  real ( kind = 8 ) ub
  real ( kind = 8 ) utol
  real ( kind = 8 ), allocatable :: uu(:)
  real ( kind = 8 ) x(n+1)
!
!  Determine if we have enough storage.
!
  k = i4_log_2 ( n )

  if ( n /= 2**k ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTIGRID_POISSON_1D - Fatal error!'
    write ( *, '(a)' ) '  N is not a power of 2.'
    stop 1
  end if

  nl = n + n + k - 2

  allocate ( uu(1:nl) )
  allocate ( r(1:nl) )
!
!  Initialization.
!
  it = 4
  it_num = 0
  tol = 0.0001D+00
  utol = 0.7D+00
  m = n
!
!  Set the nodes.
!
  call r8vec_linspace ( n + 1, a, b, x )
! 
!  Set the right hand side.
! 
  r(1) = ua
  h = ( b - a ) / real ( n, kind = 8 )
  do i = 2, n
    r(i) = h**2 * force ( x(i) )
  end do
  r(n+1) = ub

  uu(1:nl) = 0.0D+00
!
!  L points to first entry of solution
!  LL points to penultimate entry.
!
  l = 1
  ll = n
! 
!  Gauss-Seidel iteration
!
  d1 = 0.0D+00
  j = 0

  do

    d0 = d1
    j = j + 1
    call gauss_seidel ( n + 1, r(l), uu(l), d1 )
    it_num = it_num + 1
!
!  Do at least 4 iterations at each level.
!
    if ( j < it ) then

      cycle
!
!  Enough iterations, satisfactory decrease, on finest grid, exit.
!
    else if ( d1 < tol .and. n == m ) then

      exit
!
!  Enough iterations, satisfactory convergence, go finer.
!
    else if ( d1 < tol ) then

      call ctof ( n + 1, uu(l), n + n + 1, uu(l-1-n-n) )

      n = n + n
      ll = l - 2
      l = l - 1 - n
      j = 0
!
!  Enough iterations, slow convergence, 2 < N, go coarser.
!
    else if ( utol * d0 <= d1 .and. 2 < n ) then

      call ftoc ( n + 1, uu(l), r(l), (n/2)+1, uu(l+n+1), r(l+n+1) )

      n = n / 2
      l = ll + 2
      ll = ll + n + 1
      j = 0

    end if

  end do

  u(1:n+1) = uu(1:n+1)

  deallocate ( r )
  deallocate ( uu )

  return
end
subroutine r8vec_linspace ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_LINSPACE creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
!
!    In other words, the interval is divided into N-1 even subintervals,
!    and the endpoints of intervals are used as the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the first and last entries.
!
!    Output, real ( kind = 8 ) X(N), a vector of linearly spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n
      x(i) = ( real ( n - i,     kind = 8 ) * a   &
             + real (     i - 1, kind = 8 ) * b ) &
             / real ( n     - 1, kind = 8 )
    end do

  end if

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
