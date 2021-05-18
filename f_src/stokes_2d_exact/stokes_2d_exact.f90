subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical ( kind = 4 ) lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
subroutine grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

!*****************************************************************************80
!
!! GRID_2D returns a regular 2D grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X_NUM, the number of X values to use.
!
!    Input, real ( kind = 8 ) X_LO, X_HI, the range of X values.
!
!    Input, integer ( kind = 4 ) Y_NUM, the number of Y values to use.
!
!    Input, real ( kind = 8 ) Y_LO, Y_HI, the range of Y values.
!
!    Output, real ( kind = 8 ) X(X_NUM,Y_NUM), Y(X_NUM,Y_NUM), 
!    the coordinates of the grid.
!
  implicit none

  integer ( kind = 4 ) x_num
  integer ( kind = 4 ) y_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(x_num,y_num)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo
  real ( kind = 8 ) xi
  real ( kind = 8 ) y(x_num,y_num)
  real ( kind = 8 ) y_hi
  real ( kind = 8 ) y_lo
  real ( kind = 8 ) yj

  if ( x_num == 1 ) then
    x(1:x_num,1:y_num) = ( x_lo + x_hi ) / 2.0D+00
  else
    do i = 1, x_num
      xi = ( real ( x_num - i,     kind = 8 ) * x_lo   &
           + real (         i - 1, kind = 8 ) * x_hi ) &
           / real ( x_num     - 1, kind = 8 )
      x(i,1:y_num) = xi
    end do
  end if

  if ( y_num == 1 ) then
    y(1:x_num,1:y_num) = ( y_lo + y_hi ) / 2.0D+00
  else
    do j = 1, y_num
      yj = ( real ( y_num - j,     kind = 8 ) * y_lo   &
           + real (         j - 1, kind = 8 ) * y_hi ) &
           / real ( y_num     - 1, kind = 8 )
      y(1:x_num,j) = yj
    end do
  end if

  return
end
function r8vec_norm_l2 ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM_L2 returns the L2 norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L2 norm is defined as:
!
!      R8VEC_NORM_L2 = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector whose L2 norm is desired.
!
!    Output, real ( kind = 8 ) R8VEC_NORM_L2, the L2 norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm_l2

  r8vec_norm_l2 = sqrt ( sum ( a(1:n)**2 ) )

  return
end
subroutine r8vec_uniform_ab ( n, a, b, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_AB returns a scaled pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Each dimension ranges from A to B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

  end do

  return
end
subroutine resid_stokes1 ( n, x, y, ur, vr, pr )

!*****************************************************************************80
!
!! RESID_STOKES1 returns residuals of the exact Stokes solution #1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Junping Wang, Yanqiu Wang, Xiu Ye,
!    A robust numerical method for Stokes equations based on divergence-free
!    H(div) finite element methods,
!    SIAM Journal on Scientific Computing,
!    Volume 31, Number 4, 2009, pages 2784-2802.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    Output, real ( kind = 8 ) UR(N), VR(N), PR(N), the residuals in the U,
!    V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) p
  real ( kind = 8 ) pr(n)
  real ( kind = 8 ) px
  real ( kind = 8 ) py
  real ( kind = 8 ) u
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) ux
  real ( kind = 8 ) uxx
  real ( kind = 8 ) uy
  real ( kind = 8 ) uyy
  real ( kind = 8 ) v
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) vx
  real ( kind = 8 ) vxx
  real ( kind = 8 ) vy
  real ( kind = 8 ) vyy
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Get the right hand sides.
!
  call rhs_stokes1 ( n, x, y, f, g, h )
!
!  Form the functions and derivatives.
!
  do i = 1, n

    u = - 2.0D+00 &
          * x(i) ** 2 * ( x(i) - 1.0D+00 ) ** 2  &
          * y(i) * ( y(i) - 1.0D+00 ) * ( 2.0D+00 * y(i) - 1.0D+00 )

    ux = - 2.0D+00  &
          * ( 4.0D+00 * x(i) ** 3 - 6.0D+00 * x(i) ** 2  &
          + 2.0D+00 * x(i) ) &
          * y(i) * ( y(i) - 1.0D+00 ) * ( 2.0D+00 * y(i) - 1.0D+00 )

    uxx = - 2.0D+00  &
          * ( 12.0D+00 * x(i) ** 2 - 12.0D+00 * x(i) + 2.0D+00 ) &
          * ( 2.0D+00 * y(i) ** 3 - 3.0D+00 * y(i) **2 + y(i) )

    uy = - 2.0D+00  &
          * x(i) ** 2 * ( x(i) - 1.0D+00 ) ** 2  &
          * ( 6.0D+00 * y(i) ** 2 - 3.0D+00 * y(i) + 1.0D+00 )

    uyy = - 2.0D+00  &
          * ( x(i) ** 4 - 2.0D+00 * x(i) ** 3 + x(i) ** 2 ) &
          * ( 12.0D+00 * y(i) - 6.0D+00 )

    v =   2.0D+00  &
          * x(i) * ( x(i) - 1.0D+00 ) * ( 2.0D+00 * x(i) - 1.0D+00 ) &
          * y(i) ** 2 * ( y(i) - 1.0D+00 ) ** 2 

    vx =   2.0D+00  &
          * ( 6.0D+00 * x(i) ** 2 - 6.0D+00 * x(i) + 1.0D+00 ) &
          * y(i) ** 2 * ( y(i) - 1.0D+00 ) ** 2 

    vxx =   2.0D+00  &
          * ( 12.0D+00 * x(i) - 6.0D+00 ) &
          * y(i) ** 2 * ( y(i) - 1.0D+00 ) ** 2 

    vy =   2.0D+00  &
          * x(i) * ( x(i) - 1.0D+00 ) * ( 2.0D+00 * x(i) - 1.0D+00 ) &
          * ( 4.0D+00 * y(i) ** 3 - 6.0D+00 * y(i) ** 2  &
          + 2.0D+00 * y(i) )

    vyy =   2.0D+00  &
          * x(i) * ( x(i) - 1.0D+00 ) * ( 2.0D+00 * x(i) - 1.0D+00 ) &
          * ( 12.0D+00 * y(i) ** 2 - 12.0D+00 * y(i) + 2.0D+00 )

    p = 0.0D+00
    px = 0.0D+00
    py = 0.0D+00

    ur(i) = px - ( uxx + uyy ) - f(i)
    vr(i) = py - ( vxx + vyy ) - g(i)
    pr(i) = ux + vy - h(i)

  end do

  return
end
subroutine resid_stokes2 ( n, x, y, ur, vr, pr )

!*****************************************************************************80
!
!! RESID_STOKES2 returns residuals of the exact Stokes solution #2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Junping Wang, Yanqiu Wang, Xiu Ye,
!    A robust numerical method for Stokes equations based on divergence-free
!    H(div) finite element methods,
!    SIAM Journal on Scientific Computing,
!    Volume 31, Number 4, 2009, pages 2784-2802.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    Output, real ( kind = 8 ) UR(N), VR(N), PR(N), the residuals in the U,
!    V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) p
  real ( kind = 8 ) pr(n)
  real ( kind = 8 ) px
  real ( kind = 8 ) py
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) u
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) ux
  real ( kind = 8 ) uxx
  real ( kind = 8 ) uy
  real ( kind = 8 ) uyy
  real ( kind = 8 ) v
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) vx
  real ( kind = 8 ) vxx
  real ( kind = 8 ) vy
  real ( kind = 8 ) vyy
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Get the right hand sides.
!
  call rhs_stokes2 ( n, x, y, f, g, h )

  do i = 1, n

    u =   2.0D+00 &
          * sin ( 2.0D+00 * r8_pi * x(i) ) &
          * cos ( 2.0D+00 * r8_pi * y(i) )

    ux =   4.0D+00 * r8_pi &
          * cos ( 2.0D+00 * r8_pi * x(i) ) &
          * cos ( 2.0D+00 * r8_pi * y(i) )

    uxx = - 8.0D+00 * r8_pi ** 2 &
          * sin ( 2.0D+00 * r8_pi * x(i) ) &
          * cos ( 2.0D+00 * r8_pi * y(i) )

    uy = - 4.0D+00 * r8_pi &
          * sin ( 2.0D+00 * r8_pi * x(i) ) &
          * sin ( 2.0D+00 * r8_pi * y(i) )

    uyy = - 8.0D+00 * r8_pi ** 2 &
          * sin ( 2.0D+00 * r8_pi * x(i) ) &
          * cos ( 2.0D+00 * r8_pi * y(i) )

    v = - 2.0D+00 &
          * cos ( 2.0D+00 * r8_pi * x(i) ) &
          * sin ( 2.0D+00 * r8_pi * y(i) )

    vx =   4.0D+00 * r8_pi &
          * sin ( 2.0D+00 * r8_pi * x(i) ) &
          * sin ( 2.0D+00 * r8_pi * y(i) )

    vxx =   8.0D+00 * r8_pi ** 2 &
          * cos ( 2.0D+00 * r8_pi * x(i) ) &
          * sin ( 2.0D+00 * r8_pi * y(i) )

    vy = - 4.0D+00 * r8_pi &
          * cos ( 2.0D+00 * r8_pi * x(i) ) &
          * cos ( 2.0D+00 * r8_pi * y(i) )

    vyy =   8.0D+00 * r8_pi ** 2 &
          * cos ( 2.0D+00 * r8_pi * x(i) ) &
          * sin ( 2.0D+00 * r8_pi * y(i) )

    p = x(i) ** 2 + y(i) ** 2

    px = 2.0D+00 * x(i)
    py = 2.0D+00 * y(i)

    ur(i) = px - ( uxx + uyy ) - f(i)
    vr(i) = py - ( vxx + vyy ) - g(i)
    pr(i) = ux + uy - h(i)

  end do

  return
end
subroutine resid_stokes3 ( n, x, y, ur, vr, pr )

!*****************************************************************************80
!
!! RESID_STOKES3 returns residuals of the exact Stokes solution #3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Howard Elman, Alison Ramage, David Silvester,
!    Finite Elements and Fast Iterative Solvers with
!    Applications in Incompressible Fluid Dynamics,
!    Oxford, 2005,
!    ISBN: 978-0198528678,
!    LC: QA911.E39.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    Output, real ( kind = 8 ) UR(N), VR(N), PR(N), the residuals in the U,
!    V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) p
  real ( kind = 8 ) pr(n)
  real ( kind = 8 ) px
  real ( kind = 8 ) py
  real ( kind = 8 ) u
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) ux
  real ( kind = 8 ) uxx
  real ( kind = 8 ) uy
  real ( kind = 8 ) uyy
  real ( kind = 8 ) v
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) vx
  real ( kind = 8 ) vxx
  real ( kind = 8 ) vy
  real ( kind = 8 ) vyy
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Get the right hand sides.
!
  call rhs_stokes3 ( n, x, y, f, g, h )
!
!  Form the functions and derivatives.
!
  do i = 1, n

    u =   20.0D+00 * x(i) * y(i) ** 3
    ux = 20.0D+00 * y(i) ** 3
    uxx = 0.0D+00
    uy = 60.0D+00 * x(i) * y(i) ** 2
    uyy = 120.0D+00 * x(i) * y(i)

    v = 5.0D+00 * ( x(i) ** 4  - y(i) ** 4 )
    vx = 20.0D+00 * x(i) ** 3
    vxx = 60.0D+00 * x(i) ** 2
    vy = - 20.0D+00 * y(i) ** 3
    vyy = - 60.0D+00 * y(i) ** 2

    p =   60.0D+00 * x(i) ** 2 * y(i) - 20.0D+00 * y(i) ** 3 + 10.0D+00
    px = 120.0D+00 * x(i) * y(i)
    py =  60.0D+00 * x(i) ** 2 - 60.0D+00 * y(i) ** 2

    ur(i) = px - ( uxx + uyy ) - f(i)
    vr(i) = py - ( vxx + vyy ) - g(i)
    pr(i) = ux + vy - h(i)

  end do

  return
end
subroutine rhs_stokes1 ( n, x, y, f, g, h )

!*****************************************************************************80
!
!! RHS_STOKES1 returns the right hand sides of the exact Stokes solution #1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Junping Wang, Yanqiu Wang, Xiu Ye,
!    A robust numerical method for Stokes equations based on divergence-free
!    H(div) finite element methods,
!    SIAM Journal on Scientific Computing,
!    Volume 31, Number 4, 2009, pages 2784-2802.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    Output, real ( kind = 8 ) F(N), G(N), H(N), the right hand sides in the U,
!    V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  do i = 1, n

    f(i) = + 2.0D+00 &
          * ( 12.0D+00 * x(i) ** 2 - 12.0D+00 * x(i) + 2.0D+00 ) &
          * ( 2.0D+00 * y(i) ** 3 - 3.0D+00 * y(i) **2 + y(i) ) &
          + 2.0D+00 &
          * ( x(i) ** 4 - 2.0D+00 * x(i) ** 3 + x(i) ** 2 ) &
          * ( 12.0D+00 * y(i) - 6.0D+00 )

    g(i) = - 2.0D+00 &
          * ( 12.0D+00 * x(i) - 6.0D+00 ) &
          * ( y(i) ** 4 - 2.0D+00 * y(i) ** 3 + y(i) ** 2 ) &
          - 2.0D+00 &
          * ( 2.0D+00 * x(i) ** 3 - 3.0D+00 * x(i) ** 2 + x(i) ) &
          * ( 12.0D+00 * y(i) ** 2 - 12.0D+00 * y(i) + 2.0D+00 )

    h(i) = 0.0D+00

  end do

  return
end
subroutine rhs_stokes2 ( n, x, y, f, g, h )

!*****************************************************************************80
!
!! RHS_STOKES2 returns the right hand sides of the exact Stokes solution #2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Junping Wang, Yanqiu Wang, Xiu Ye,
!    A robust numerical method for Stokes equations based on divergence-free
!    H(div) finite element methods,
!    SIAM Journal on Scientific Computing,
!    Volume 31, Number 4, 2009, pages 2784-2802.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    Output, real ( kind = 8 ) F(N), G(N), H(N), the right hand sides in the U,
!    V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) p
  real ( kind = 8 ) px
  real ( kind = 8 ) py
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) u
  real ( kind = 8 ) ux
  real ( kind = 8 ) uxx
  real ( kind = 8 ) uy
  real ( kind = 8 ) uyy
  real ( kind = 8 ) v
  real ( kind = 8 ) vx
  real ( kind = 8 ) vxx
  real ( kind = 8 ) vy
  real ( kind = 8 ) vyy
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  do i = 1, n

    u =   2.0D+00 &
          * sin ( 2.0D+00 * r8_pi * x(i) ) &
          * cos ( 2.0D+00 * r8_pi * y(i) )

    ux =   4.0D+00 * r8_pi &
          * cos ( 2.0D+00 * r8_pi * x(i) ) &
          * cos ( 2.0D+00 * r8_pi * y(i) )

    uxx = - 8.0D+00 * r8_pi ** 2 &
          * sin ( 2.0D+00 * r8_pi * x(i) ) &
          * cos ( 2.0D+00 * r8_pi * y(i) )

    uy = - 4.0D+00 * r8_pi &
          * sin ( 2.0D+00 * r8_pi * x(i) ) &
          * sin ( 2.0D+00 * r8_pi * y(i) )

    uyy = - 8.0D+00 * r8_pi ** 2 &
          * sin ( 2.0D+00 * r8_pi * x(i) ) &
          * cos ( 2.0D+00 * r8_pi * y(i) )

    v = - 2.0D+00 &
          * cos ( 2.0D+00 * r8_pi * x(i) ) &
          * sin ( 2.0D+00 * r8_pi * y(i) )

    vx =   4.0D+00 * r8_pi &
          * sin ( 2.0D+00 * r8_pi * x(i) ) &
          * sin ( 2.0D+00 * r8_pi * y(i) )

    vxx =   8.0D+00 * r8_pi ** 2 &
          * cos ( 2.0D+00 * r8_pi * x(i) ) &
          * sin ( 2.0D+00 * r8_pi * y(i) )

    vy = - 4.0D+00 * r8_pi &
          * cos ( 2.0D+00 * r8_pi * x(i) ) &
          * cos ( 2.0D+00 * r8_pi * y(i) )

    vyy =   8.0D+00 * r8_pi ** 2 &
          * cos ( 2.0D+00 * r8_pi * x(i) ) &
          * sin ( 2.0D+00 * r8_pi * y(i) )

    p = x(i) ** 2 + y(i) ** 2

    px = 2.0D+00 * x(i)
    py = 2.0D+00 * y(i)

    f(i) = px - ( uxx + uyy )
    g(i) = py - ( vxx + vyy )
    h(i) = ux + vy

  end do

  return
end
subroutine rhs_stokes3 ( n, x, y, f, g, h )

!*****************************************************************************80
!
!! RHS_STOKES3 returns the right hand sides of the exact Stokes solution #3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Howard Elman, Alison Ramage, David Silvester,
!    Finite Elements and Fast Iterative Solvers with
!    Applications in Incompressible Fluid Dynamics,
!    Oxford, 2005,
!    ISBN: 978-0198528678,
!    LC: QA911.E39.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    Output, real ( kind = 8 ) F(N), G(N), H(N), the right hand sides in the U,
!    V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  f(1:n) = 0.0D+00
  g(1:n) = 0.0D+00
  h(1:n) = 0.0D+00

  return
end
subroutine stokes_gnuplot ( header, n, x, y, u, v, s )

!*****************************************************************************80
!
!! STOKES_GNUPLOT writes the Stokes vector field to files for GNUPLOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) HEADER, a header to be used to name the files.
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of the 
!    evaluation points.
!
!    Input, real ( kind = 8 ) U(N), V(N), the velocity components.
!
!    Input, real ( kind = 8 ) S, a scale factor for the velocity vectors.
!
  implicit none

  integer ( kind = 4 ) n

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = * ) header
  integer ( kind = 4 ) i
  character ( len = 255 ) plot_filename
  real ( kind = 8 ) s
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Write the data file.
!
  data_filename = trim ( header ) // '_data.txt'

  call get_unit ( data_unit )

  open ( unit = data_unit, file = data_filename, status = 'replace' )

  do i = 1, n
    write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x(i), y(i), s * u(i), s * v(i)
  end do

  close ( unit = data_unit )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Data written to "' // trim ( data_filename ) // '".'
!
!  Write the command file.
!
  command_filename = trim ( header ) // '_commands.txt'
  call get_unit ( command_unit )

  plot_filename = trim ( header ) // '.png'

  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '#  ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '#  Add titles and labels.'
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set title "Stokes flow"'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '#  Add grid lines.'
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set size ratio -1'
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '#  Timestamp the plot.'
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2:3:4 with vectors \'
  write ( command_unit, '(a)' ) '  head filled lt 2 linecolor rgb "blue"'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Commands written to "' // &
    trim ( command_filename ) // '".'

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
subroutine uvp_stokes1 (  n, x, y, u, v, p )

!*****************************************************************************80
!
!! UVP_STOKES1 evaluates the exact Stokes solution #1.
!
!  Discussion:
!
!    The solution is defined over the unit square [0,1]x[0,1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Junping Wang, Yanqiu Wang, Xiu Ye,
!    A robust numerical method for Stokes equations based on divergence-free
!    H(div) finite element methods,
!    SIAM Journal on Scientific Computing,
!    Volume 31, Number 4, 2009, pages 2784-2802.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    Output, real ( kind = 8 ) U(N), V(N), P(N), the velocity components and
!    pressure at each of the points.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  do i = 1, n

    u(i) = - 2.0D+00 &
          * x(i) ** 2 * ( x(i) - 1.0D+00 ) ** 2 &
          * y(i) * ( y(i) - 1.0D+00 ) * ( 2.0D+00 * y(i) - 1.0D+00 )

    v(i) =   2.0D+00 &
          * x(i) * ( x(i) - 1.0D+00 ) * ( 2.0D+00 * x(i) - 1.0D+00 ) &
          * y(i) ** 2 * ( y(i) - 1.0D+00 ) ** 2 

    p(i) = 0.0D+00

  end do

  return
end
subroutine uvp_stokes2 (  n, x, y, u, v, p )

!*****************************************************************************80
!
!! UVP_STOKES2 evaluates the exact Stokes solution #2.
!
!  Discussion:
!
!    The solution is defined over the unit square [0,1]x[0,1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Junping Wang, Yanqiu Wang, Xiu Ye,
!    A robust numerical method for Stokes equations based on divergence-free
!    H(div) finite element methods,
!    SIAM Journal on Scientific Computing,
!    Volume 31, Number 4, 2009, pages 2784-2802.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    Output, real ( kind = 8 ) U(N), V(N), P(N), the velocity components and
!    pressure at each of the points.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) p(n)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  do i = 1, n

    u(i) =   2.0D+00 &
          * sin ( 2.0D+00 * r8_pi * x(i) ) &
          * cos ( 2.0D+00 * r8_pi * y(i) )


    v(i) = - 2.0D+00 &
          * cos ( 2.0D+00 * r8_pi * x(i) ) &
          * sin ( 2.0D+00 * r8_pi * y(i) )

    p(i) = x(i) ** 2 + y(i) ** 2

  end do

  return
end
subroutine uvp_stokes3 (  n, x, y, u, v, p )

!*****************************************************************************80
!
!! UVP_STOKES3 evaluates the exact Stokes solution #3.
!
!  Discussion:
!
!    The solution is defined over the unit square [-1,+1]x[-1,+1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Howard Elman, Alison Ramage, David Silvester,
!    Finite Elements and Fast Iterative Solvers with
!    Applications in Incompressible Fluid Dynamics,
!    Oxford, 2005,
!    ISBN: 978-0198528678,
!    LC: QA911.E39.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    Output, real ( kind = 8 ) U(N), V(N), P(N), the velocity components and
!    pressure at each of the points.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  do i = 1, n

    u(i) =   20.0D+00 * x(i) * y(i) ** 3
    v(i) =    5.0D+00 * ( x(i) ** 4  - y(i) ** 4 )
    p(i) =   60.0D+00 * x(i) ** 2 * y(i) - 2.0D+00 * y(i) ** 3 + 10.0D+00

  end do

  return
end
