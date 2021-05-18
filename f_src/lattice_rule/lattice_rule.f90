function e_01_2d ( dim_num, a, b )

!*****************************************************************************80
!
!! E_01_2D is the exact integral of 2D test function #1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, real ( kind = 8 ) A(DIM_NUM), B(DIM_NUM), the integration limits.
!
!    Output, real ( kind = 8 ) E_01_2D, the integral of the function 
!    over the limits.
!
  implicit none

  integer ( kind = 4 ) dim_num

  real ( kind = 8 ) a(dim_num)
  real ( kind = 8 ) b(dim_num)
  real ( kind = 8 ) e_01_2d

  e_01_2d = 1.0D+00

  return
end
function f_01_2d ( dim_num, x )

!*****************************************************************************80
!
!! F_01_2D is the 2D test function #1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, real ( kind = 8 ) X(DIM_NUM), the point where the function 
!    is to be evaluated.
!
!    Output, real ( kind = 8 ) F_01_2D, the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) dim_num

  real ( kind = 8 ), parameter :: e = 2.718281828459045D+00
  real ( kind = 8 ) f_01_2d
  real ( kind = 8 ) x(dim_num)

  f_01_2d = x(2) * exp ( x(1) * x(2) ) / ( e - 2.0D+00 )

  return
end
function f2 ( x )

!*****************************************************************************80
!
!! F2 evaluates a function of a scalar used in defining P2(Q).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the value of the argument.
!
!    Output, real ( kind = 8 ) F2, the value of F2(X).
!
  implicit none

  real ( kind = 8 ) f2
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  f2 = 1.0D+00 + 2.0D+00 * pi**2 * ( x * x - x + 1.0D+00 / 6.0D+00 )

  return
end
function f20_s ( dim_num, x )

!*****************************************************************************80
!
!! F20_S evaluates a function of a vector used in defining P2(Q).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, real ( kind = 8 ) X(DIM_NUM), the value of the argument.
!
!    Output, real ( kind = 8 ) F20_S, the value of F20_S(X).
!
  implicit none

  integer ( kind = 4 ) dim_num

  real ( kind = 8 ) f2
  real ( kind = 8 ) f20_s
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(dim_num)

  f20_s = 1.0D+00
  do i = 1, dim_num
    f20_s = f20_s * ( 1.0D+00 + ( f2 ( x(i) ) - 1.0D+00 ) )
  end do

  f20_s = f20_s - 1.0D+00

  return
end
function fibonacci ( k )

!*****************************************************************************80
!
!! FIBONACCI returns the Fibonacci number of given index.
!
!  Example:
!
!    K   Fibonacci
!
!    0   0
!    1   1
!    2   1
!    3   2
!    4   3
!    5   5
!    6   8
!    7  13
!    8  21
!    9  34
!   10  55
!   11  89
!   12 144
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, the index of the Fibonacci number to 
!    be used.  K must be at least 1.
!
!    Output, integer ( kind = 4 ) FIBONACCI, the value of the K-th 
!    Fibonacci number.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) fibonacci
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kk

  if ( k < 0 ) then
    fibonacci = - huge ( k )
    return
  else if ( k == 0 ) then
    fibonacci = 0
    return
  else if ( k == 1 ) then
    fibonacci = 1
    return
  end if

  c = 0
  b = 0
  a = 1

  do kk = 2, k

    c = b
    b = a
    a = c + b

  end do

  fibonacci = a

  return
end
subroutine fibonacci_lattice_b ( k, f, quad )

!*****************************************************************************80
!
!! FIBONACCI_LATTICE_B applies an optimal Fibonacci lattice rule in 2D.
!
!  Discussion:
!
!    This routine may be applied to integrands which are not periodic.
!
!    When K is odd, this is the same as the symmetric Fibonacci lattice
!    integration rule.  But when K is even, a correction is made to the
!    corner weights which is expected to improve the results.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, the index of the Fibonacci number
!    to be used.  K must be at least 3.
!
!    Input, external, real ( kind = 8 ) F, the name of the user-supplied routine
!    which evaluates the function, of the form:
!    function f ( dim_num, x )
!    integer ( kind = 4 ) dim_num
!    real f
!    real x(dim_num)
!    f = ...
!
!    Output, real ( kind = 8 ) QUAD, the estimated integral.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) delta
  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) fibonacci
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) quad
  real ( kind = 8 ) quad1
  real ( kind = 8 ) quad2
  integer ( kind = 4 ) rank
  real ( kind = 8 ) w(0:1,0:1)
  real ( kind = 8 ) x(1:dim_num)
  integer ( kind = 4 ) z(1:dim_num)

  quad = 0.0D+00

  m = fibonacci ( k )
  n = fibonacci ( k - 1 )
!
!  Get the corner weights.
!
  if ( mod ( k, 2 ) == 1 ) then

    w(0:1,0:1) = 1.0D+00 / real ( 4 * m, kind = 8 )

  else

     delta = 0.0D+00
     do j = 1, m-1
       delta = delta + real ( j * mod ( j * n, m ), kind = 8 ) &
                     / real ( m**2, kind = 8 )
     end do
     w(0,0) = 0.25D+00 - delta / real ( m, kind = 8 )

     delta = 0.0D+00
     do j = 1, m-1
       delta = delta + real ( j * ( m - mod ( j * n, m ) ), kind = 8 ) &
                     / real ( m**2, kind = 8 )
     end do
     w(0,1) = 0.25D+00 - delta / real ( m, kind = 8 )

     w(1,0) = w(0,1)
     w(1,1) = w(0,0)

  end if
!
!  Get all the corner values.
!
  rank = 0
  quad1 = 0.0D+00

  do

    call tuple_next ( 0, 1, dim_num, rank, z )

    if ( rank == 0 ) then
      exit
    end if

    x(1:dim_num) = real ( z, kind = 8 )
    quad1 = quad1 + w(z(1),z(2)) * f ( dim_num, x )

  end do
!
!  Get the interior values.
!
  z(1) = 1
  z(2) = fibonacci ( k - 1 )

  quad2 = 0.0D+00
  do j = 1, m-1
    x(1:dim_num) = mod ( real ( j * z(1:dim_num), kind = 8 ) &
      / real ( m, kind = 8 ), 1.0D+00 )
    quad2 = quad2 + f ( dim_num, x )
  end do

  quad = quad1 + quad2 / real ( m, kind = 8 )

  return
end
subroutine fibonacci_lattice_q ( k, f, quad )

!*****************************************************************************80
!
!! FIBONACCI_LATTICE_Q applies a Fibonacci lattice integration rule in 2D.
!
!  Discussion:
!
!    Because this is a standard lattice rule, it is really only suited
!    for functions which are periodic, of period 1, in both X and Y.
!
!    The related routines FIBONACCI_LATTICE_S and FIBONACCI_LATTICE_B
!    may be helpful in cases where the integrand does not satisfy this
!    requirement.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, the index of the Fibonacci number
!    to be used.  K must be at least 3.
!
!    Input, external, real ( kind = 8 ) F, the name of the user-supplied routine
!    which evaluates the function, of the form:
!    function f ( dim_num, x )
!    integer ( kind = 4 ) dim_num
!    real f
!    real x(dim_num)
!    f = ...
!
!    Output, real ( kind = 8 ) QUAD, the estimated integral.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) fibonacci
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  real ( kind = 8 ) quad
  real ( kind = 8 ) x(1:dim_num)
  integer ( kind = 4 ) z(1:dim_num)

  quad = 0.0D+00

  m = fibonacci ( k )

  z(1) = 1
  z(2) = fibonacci ( k - 1 )

  do j = 0, m-1
    x(1:dim_num) = mod ( real ( j * z(1:dim_num), kind = 8 ) &
                 / real ( m, kind = 8 ), 1.0D+00 )
    quad = quad + f ( dim_num, x )
  end do

  quad = quad / real ( m, kind = 8 )

  return
end
subroutine fibonacci_lattice_q_nodes ( k, x )

!*****************************************************************************80
!
!! FIBONACCI_LATTICE_Q_NODES returns Fibonacci lattice nodes in 2D.
!
!  Discussion:
!
!    Because this is a standard lattice rule, it is really only suited
!    for functions which are periodic, of period 1, in both X and Y.
!
!    The number of nodes returned is 
!
!      M = fibonacci ( k ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, the index of the Fibonacci number to 
!    be used.  K must be at least 3.
!
!    Output, real ( kind = 8 ) X(2,M), the nodes.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ), parameter :: dim_num = 2

  integer ( kind = 4 ) fibonacci
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(dim_num,*)
  integer ( kind = 4 ) z(dim_num)

  m = fibonacci ( k )

  z(1) = 1
  z(2) = fibonacci ( k - 1 )

  do j = 0, m-1
    x(1:dim_num,j+1) = mod ( real ( j * z(1:dim_num), kind = 8 ) &
                     / real ( m,          kind = 8 ), 1.0D+00 )
  end do

  return
end
subroutine fibonacci_lattice_q1 ( k, f, quad )

!*****************************************************************************80
!
!! FIBONACCI_LATTICE_Q1 applies a Fibonacci lattice integration rule in 2D.
!
!  Discussion:
!
!    This is a modification of the algorithm in FIBONACCI_LATTICE_Q.
!    It uses a nonlinear transformation on the integrand, which makes
!    the lattice rule more suitable for nonperiodic integrands.
!
!    The transformation replaces the integration variable X by
!
!      PHI(X) = 3*X^2 - 2*X^3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, the index of the Fibonacci number 
!    to be used.  K must be at least 3.
!
!    Input, external, real ( kind = 8 ) F, the name of the user-supplied routine
!    which evaluates the function, of the form:
!    function f ( dim_num, x )
!    integer ( kind = 4 ) dim_num
!    real f
!    real x(dim_num)
!    f = ...
!
!    Output, real ( kind = 8 ) QUAD, the estimated integral.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) dphi
  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) fibonacci
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  real ( kind = 8 ) quad
  real ( kind = 8 ) x(1:dim_num)
  real ( kind = 8 ) y(1:dim_num)
  integer ( kind = 4 ) z(1:dim_num)

  quad = 0.0D+00

  m = fibonacci ( k )

  z(1) = 1
  z(2) = fibonacci ( k - 1 )

  do j = 0, m-1
    x(1:dim_num) = mod ( real ( j * z(1:dim_num), kind = 8 ) &
      / real ( m, kind = 8 ), 1.0D+00 )
    dphi = 1.0D+00
    do i = 1, dim_num
      y(i) = ( 3.0D+00 - 2.0D+00 * x(i) ) * x(i)**2
      dphi = dphi * 6.0D+00 * ( 1.0D+00 - x(i) ) * x(i)
    end do
    quad = quad + f ( dim_num, y ) * dphi
  end do

  quad = quad / real ( m, kind = 8 )

  return
end
subroutine fibonacci_lattice_q2 ( k, f, quad )

!*****************************************************************************80
!
!! FIBONACCI_LATTICE_Q2 applies a Fibonacci lattice integration rule in 2D.
!
!  Discussion:
!
!    This is a modification of the algorithm in FIBONACCI_LATTICE_Q.
!    It uses a nonlinear transformation on the integrand, which makes
!    the lattice rule more suitable for nonperiodic integrands.
!
!    The transformation replaces the integration variable X by
!
!      PHI(X) = 3*X^3 *( 10 - 15 * X + 6 * X^2 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, the index of the Fibonacci number
!    to be used.  K must be at least 3.
!
!    Input, external, real ( kind = 8 ) F, the name of the user-supplied routine
!    which evaluates the function, of the form:
!    function f ( dim_num, x )
!    integer ( kind = 4 ) dim_num
!    real f
!    real x(dim_num)
!    f = ...
!
!    Output, real ( kind = 8 ) QUAD, the estimated integral.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) dphi
  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) fibonacci
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  real ( kind = 8 ) quad
  real ( kind = 8 ) x(1:dim_num)
  real ( kind = 8 ) y(1:dim_num)
  integer ( kind = 4 ) z(1:dim_num)

  quad = 0.0D+00

  m = fibonacci ( k )

  z(1) = 1
  z(2) = fibonacci ( k - 1 )

  do j = 0, m-1
    x(1:dim_num) = mod ( real ( j * z(1:dim_num), kind = 8 ) &
      / real ( m, kind = 8 ), 1.0D+00 )
    dphi = 1.0D+00
    do i = 1, dim_num
      y(i) = ( 10.0D+00 - 15.0D+00 * x(i) + 6.0D+00 * x(i)**2 ) * x(i)**3
      dphi = dphi * 30.0D+00 * ( 1.0D+00 - x(i) )**2 * x(i)**2
    end do
    quad = quad + f ( dim_num, y ) * dphi
  end do

  quad = quad / real ( m, kind = 8 )

  return
end
subroutine fibonacci_lattice_q3 ( k, f, quad )

!*****************************************************************************80
!
!! FIBONACCI_LATTICE_Q3 applies a Fibonacci lattice integration rule in 2D.
!
!  Discussion:
!
!    This is a modification of the algorithm in FIBONACCI_LATTICE_Q.
!    It uses a nonlinear transformation on the integrand, which makes
!    the lattice rule more suitable for nonperiodic integrands.
!
!    The transformation replaces the integration variable X by
!
!      PHI(X) = X - sin ( 2 * PI * X ) / ( 2 * PI )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, the index of the Fibonacci number.
!    K must be at least 3.
!
!    Input, external, real ( kind = 8 ) F, the name of the user-supplied routine
!    which evaluates the function, of the form:
!    function f ( dim_num, x )
!    integer ( kind = 4 ) dim_num
!    real f
!    real x(dim_num)
!    f = ...
!
!    Output, real ( kind = 8 ) QUAD, the estimated integral.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) dphi
  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) fibonacci
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  real ( kind = 8 ) quad
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) two_pi
  real ( kind = 8 ) x(1:dim_num)
  real ( kind = 8 ) y(1:dim_num)
  integer ( kind = 4 ) z(1:dim_num)

  quad = 0.0D+00

  m = fibonacci ( k )

  z(1) = 1
  z(2) = fibonacci ( k - 1 )

  two_pi = 2.0D+00 * pi

  do j = 0, m-1
    x(1:dim_num) = mod ( real ( j * z(1:dim_num), kind = 8 ) &
      / real ( m, kind = 8 ), 1.0D+00 )
    dphi = 1.0D+00
    do i = 1, dim_num
      y(i) = x(i) - sin ( two_pi * x(i) ) / two_pi
      dphi = dphi * ( 1.0D+00 - cos ( two_pi * x(i) ) )
    end do
    quad = quad + f ( dim_num, y ) * dphi
  end do

  quad = quad / real ( m, kind = 8 )

  return
end
subroutine fibonacci_lattice_t ( k, f, quad )

!*****************************************************************************80
!
!! FIBONACCI_LATTICE_T applies a symmetric Fibonacci lattice rule in 2D.
!
!  Discussion:
!
!    This routine may be applied to integrands which are not periodic.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, the index of the Fibonacci number.
!    K must be at least 3.
!
!    Input, external, real ( kind = 8 ) F, the name of the user-supplied routine
!    which evaluates the function, of the form:
!    function f ( dim_num, x )
!    integer ( kind = 4 ) dim_num
!    real f
!    real x(dim_num)
!    f = ...
!
!    Output, real ( kind = 8 ) QUAD, the estimated integral.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) fibonacci
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  real ( kind = 8 ) quad
  real ( kind = 8 ) quad1
  real ( kind = 8 ) quad2
  integer ( kind = 4 ) rank
  real ( kind = 8 ) w
  real ( kind = 8 ) x(1:dim_num)
  integer ( kind = 4 ) z(1:dim_num)

  quad = 0.0D+00

  m = fibonacci ( k )
!
!  Get all the corner values.
!
  rank = 0
  quad1 = 0.0D+00
  w = 1.0D+00 / real ( 2**dim_num, kind = 8 )

  do 

    call tuple_next ( 0, 1, dim_num, rank, z )

    if ( rank == 0 ) then
      exit
    end if

    x(1:dim_num) = real ( z, kind = 8 )
    quad1 = quad1 + w * f ( dim_num, x )

  end do
!
!  Get the interior values.
!
  z(1) = 1
  z(2) = fibonacci ( k - 1 )

  quad2 = 0.0D+00
  do j = 1, m-1
    x(1:dim_num) = mod ( real ( j * z(1:dim_num), kind = 8 ) &
      / real ( m, kind = 8 ), 1.0D+00 )
    quad2 = quad2 + f ( dim_num, x )
  end do

  quad = ( quad1 + quad2 ) / real ( m, kind = 8 )

  return
end
subroutine find_z20 ( dim_num, m, z )

!*****************************************************************************80
!
!! FIND_Z20 finds the appropriate Z vector to minimize P2(QS).
!
!  Discussion:
!
!    For the method of good lattice points, a number of points M, and
!    a single generator vector Z is chosen.  The integrand is assumed
!    to be periodic of period 1 in each argument, and is evaluated at
!    each of the points X(I)(1:DIM_NUM) = I * Z(1:DIM_NUM) / M, 
!    for I = 0 to M-1.  The integral is then approximated by the average
!    of these values.
!
!    Assuming that DIM_NUM and M are known, and that the integrand is not
!    known beforehand, the accuracy of the method depends entirely
!    on the choice of Z.  One method of choosing Z is to search for
!    the Z among all candidates which minimizes a particular quantity
!    P_ALPHA(QS).
!
!    We only need to look at vectors Z of the form
!    (1, L, L^2, ..., L^(DIM_NUM-1)),
!    for L = 1 to M/2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) M, the number of points to be used.
!
!    Output, integer ( kind = 4 ) Z(DIM_NUM), the optimal vector.
!
  implicit none

  integer ( kind = 4 ) dim_num

  real ( kind = 8 ), external :: f20_s
  integer ( kind = 4 ) i
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  real ( kind = 8 ) q0
  real ( kind = 8 ) q0_min
  integer ( kind = 4 ) value
  integer ( kind = 4 ) z(dim_num)
  integer ( kind = 4 ) z_min(dim_num)

  q0_min = huge ( q0_min )

  do l = 1, m / 2

    value = 1
    do i = 1, dim_num
      z(i) = value
      value = mod ( value * l, m )
    end do
!
!  Use this Z and the lattice integral method Q0 of order M,
!  to approximate the integral of P2.
!
    call lattice ( dim_num, m, z, f20_s, q0 )
!
!  If this result is the smallest so far, save the corresponding Z.
!
    if ( q0 < q0_min ) then
      q0_min = q0
      z_min(1:dim_num) = z(1:dim_num)
    end if

  end do
!
!  Return the best Z.
!
  z(1:dim_num) = z_min(1:dim_num)

  return
end
subroutine gray_next ( n, switch )

!*****************************************************************************80
!
!! GRAY_NEXT generates the next Gray code by switching one item at a time.
!
!  Discussion:
!
!    On the first call only, the user must set SWITCH = -N.
!    This initializes the routine to the Gray code for N zeroes.
!
!    Each time it is called thereafter, it returns in SWITCH the index
!    of the item to be switched in the Gray code.  The sign of SWITCH
!    indicates whether the item is to be added or subtracted (or
!    whether the corresponding bit should become 1 or 0).  When
!    SWITCH is equal to N+1 on output, all the Gray codes have been
!    generated.
!
!    The routine has internal memory that is set up on call with
!    SWITCH = -N, and released on final return.
!
!  Example:
!
!    N  Switch         Subset in/out   Binary Number
!                      Interpretation  Interpretation
!   --  ---------      --------------  --------------
!
!    4   -4 / 0         0 0 0 0         0
!
!        +1             1 0 0 0         1
!          +2           1 1 0 0         3
!        -1             0 1 0 0         2
!            +3         0 1 1 0         6
!        +1             1 1 1 0         7
!          -2           1 0 1 0         5
!        -1             0 0 1 0         4
!              +4       0 0 1 1        12
!        +1             1 0 1 1        13
!          +2           1 1 1 1        15
!        -1             0 1 1 1        14
!            -3         0 1 0 1        10
!        +1             1 1 0 1        11
!          -2           1 0 0 1         9
!        -1             0 0 0 1         8
!              -4       0 0 0 0         0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6,
!    LC: QA164.N54.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the total set from which
!    subsets will be drawn.
!
!    Input/output, integer ( kind = 4 ) SWITCH.  This is an input item only
!    on the first call for a particular sequence of Gray codes,
!    at which time it must be set to -N.  This corresponds to
!    all items being excluded, or all bits being 0, in the Gray code.
!    On output, SWITCH indicates which of the N items must be "changed",
!    and the sign indicates whether the item is to be added or removed
!    (or the bit is to become 1 or 0).  Note that on return from the
!    first call only, SWITCH retains its value of -N.
!
  implicit none

  integer ( kind = 4 ), save, allocatable, dimension ( : ) :: a
  integer ( kind = 4 ) i
  integer ( kind = 4 ), save :: k = 0
  integer ( kind = 4 ) n
  integer ( kind = 4 ), save :: n_save = 0
  integer ( kind = 4 ) switch

  if ( n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GRAY_NEXT - Fatal error!'
    write ( *, '(a)' ) '  Input value of N <= 0.'
    stop
  end if

  if ( switch == -n ) then

    if ( allocated(a) ) then
      deallocate ( a )
    end if

    allocate ( a(1:n) )
    a(1:n) = 0

    n_save = n
    k = 1
    switch = 0

    return
  end if

  if ( n /= n_save ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GRAY_NEXT - Fatal error!'
    write ( *, '(a)' ) '  Input value of N has changed from definition value.'
    stop
  end if
!
!  First determine WHICH item is to be changed.
!
  if ( mod ( k, 2 ) == 1 ) then

    switch = 1

  else

    do i = 1, n_save
      if ( a(i) == 1 ) then
        switch = i + 1
        exit
      end if
    end do

  end if
!
!  Take care of the terminal case SWITCH = N + 1.
!
  if ( switch == n + 1 ) then
    switch = n
  end if
!
!  Now determine HOW the item is to be changed.
!
  if ( a(switch) == 0 ) then
    a(switch) = 1
  else if ( a(switch) == 1 ) then
    a(switch) = 0
    switch = -switch
  end if
!
!  Update the counter.
!
  k = k + 1
!
!  If the output SWITCH = -N_SAVE, then we're done.
!
  if ( switch == -n_save ) then

    deallocate ( a )
    n_save = 0
    k = 0

  end if

  return
end
subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) big
  integer ( kind = 4 ) i
  character ( len = * )  title

  if ( title /= ' ' ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  big = maxval ( abs ( a(1:n) ) )

  write ( *, '(a)' ) ' '
  if ( big < 1000 ) then
    do i = 1, n
      write ( *, '(i6,1x,i4)' ) i, a(i)
    end do
  else if ( big < 1000000 ) then
    do i = 1, n
      write ( *, '(i6,1x,i7)' ) i, a(i)
    end do
  else
    do i = 1, n
      write ( *, '(i6,i11)' ) i, a(i)
    end do
  end if

  return
end
subroutine lattice ( dim_num, m, z, f, quad )

!*****************************************************************************80
!
!! LATTICE applies a lattice integration rule.
!
!  Discussion:
!
!    Because this is a standard lattice rule, it is really only suited
!    for functions which are periodic, of period 1, in both X and Y.
!
!    For a suitable F, and a given value of M (the number of lattice points),
!    the performance of the routine is affected by the choice of the
!    generator vector Z.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) M, the order (number of points) of the rule.
!
!    Input, integer ( kind = 4 ) Z(DIM_NUM), the generator vector.  Typically, 
!    the elements of Z satisfy 1 <= Z(1:DIM_NUM) < M, and are relatively 
!    prime to M.  This is easy to guarantee if M is itself a prime number.
!
!    Input, external, real ( kind = 8 ) F, the name of the user-supplied routine
!    which evaluates the function, of the form:
!    function f ( dim_num, x )
!    integer ( kind = 4 ) dim_num
!    real f
!    real x(dim_num)
!    f = ...
!
!    Output, real ( kind = 8 ) QUAD, the estimated integral.
!
  implicit none

  integer ( kind = 4 ) dim_num

  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  real ( kind = 8 ) quad
  real ( kind = 8 ) x(1:dim_num)
  integer ( kind = 4 ) z(1:dim_num)

  quad = 0.0D+00

  do j = 0, m-1
    x(1:dim_num) = mod ( real ( j * z(1:dim_num), kind = 8 ) &
      / real ( m, kind = 8 ), 1.0D+00 )
    quad = quad + f ( dim_num, x )
  end do

  quad = quad / real ( m, kind = 8 )

  return
end
subroutine lattice_np0 ( dim_num, m, z, f, quad )

!*****************************************************************************80
!
!! LATTICE_NP0 applies a lattice integration rule to a nonperiodic function.
!
!  Discussion:
!
!    This routine attempts to modify a lattice rule, suitable for use
!    with a periodic function, for use with a nonperiodic function F(X),
!    essentially by applying the lattice rule to the function
!
!      G(X) = ( F(X) + F(1-X) ) / 2
!
!    This is the rule in 1 dimension.  In two dimensions, we have
!
!      G(X,Y) = ( F(X,Y) + F(X,1-Y) + F(1-X,Y) + F(1-X,1-Y) ) / 4
!
!    with the obvious generalizations to higher dimensions.  
!
!    Drawbacks of this approach include:
!
!    * in dimension DIM_NUM, we must evaluate the function F at 
!      2**DIM_NUM points for every single evaluation of G;
!
!    * the function G, regarded as a periodic function, is continuous,
!      but not generally differentiable, at 0 and 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Seymour Haber,
!    Parameters for Integrating Periodic Functions of Several Variables,
!    Mathematics of Computation,
!    Volume 41, Number 163, July 1983, pages 115-129.
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) M, the order (number of points) of the rule.
!
!    Input, integer ( kind = 4 ) Z(DIM_NUM), the generator vector.  Typically, 
!    the elements of Z satisfy 1 <= Z(1:DIM_NUM) < M, and are relatively prime 
!    to M.  This is easy to guarantee if M is itself a prime number.
!
!    Input, external, real ( kind = 8 ) F, the name of the user-supplied routine
!    which evaluates the function, of the form:
!    function f ( dim_num, x )
!    integer ( kind = 4 ) dim_num
!    real f
!    real x(dim_num)
!    f = ...
!
!    Output, real ( kind = 8 ) QUAD, the estimated integral.
!
  implicit none

  integer ( kind = 4 ) dim_num

  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  real ( kind = 8 ) quad
  integer ( kind = 4 ) switch
  real ( kind = 8 ) x(1:dim_num)
  real ( kind = 8 ) y(1:dim_num)
  integer ( kind = 4 ) z(1:dim_num)

  quad = 0.0D+00

  do j = 0, m-1

    x(1:dim_num) = mod ( real ( j * z(1:dim_num), kind = 8 ) &
      / real ( m, kind = 8 ), 1.0D+00 )
!
!  Generate all DIM_NUM-tuples for which the I-th element is X(I) or 1-X(I).
!
    switch = -dim_num

    do

      call gray_next ( dim_num, switch )

      if ( switch == -dim_num ) then
        exit
      end if

      if ( switch == 0 ) then
        y(1:dim_num) = x(1:dim_num)
      else
        y( abs ( switch ) ) = 1.0D+00 - y( abs ( switch ) )
      end if

      quad = quad + f ( dim_num, y )

    end do

  end do

  quad = quad / real ( 2**dim_num * m, kind = 8 )

  return
end
subroutine lattice_np1 ( dim_num, m, z, f, quad )

!*****************************************************************************80
!
!! LATTICE_NP1 applies a lattice integration rule to a nonperiodic function.
!
!  Discussion:
!
!    This routine applies the transformation function
!
!      PHI(T) = 3*T^2 - 2*T^3
!
!    to a nonperiodic integrand to make it suitable for treatment
!    by a lattice rule.
!
!    For a suitable F, and a given value of M (the number of lattice points),
!    the performance of the routine is affected by the choice of the
!    generator vector Z.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) M, the order (number of points) of the rule.
!
!    Input, integer ( kind = 4 ) Z(DIM_NUM), the generator vector.  Typically, 
!    the elements of Z satisfy 1 <= Z(1:DIM_NUM) < M, and are relatively prime 
!    to M.  This is easy to guarantee if M is itself a prime number.
!
!    Input, external, real ( kind = 8 ) F, the name of the user-supplied routine
!    which evaluates the function, of the form:
!    function f ( dim_num, x )
!    integer ( kind = 4 ) dim_num
!    real f
!    real x(dim_num)
!    f = ...
!
!    Output, real ( kind = 8 ) QUAD, the estimated integral.
!
  implicit none

  integer ( kind = 4 ) dim_num

  real ( kind = 8 ) dphi
  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  real ( kind = 8 ) quad
  real ( kind = 8 ) x(1:dim_num)
  real ( kind = 8 ) y(1:dim_num)
  integer ( kind = 4 ) z(1:dim_num)

  quad = 0.0D+00

  do j = 0, m-1
    x(1:dim_num) = mod ( real ( j * z(1:dim_num), kind = 8 ) &
      / real ( m, kind = 8 ), 1.0D+00 )
    dphi = 1.0D+00
    do i = 1, dim_num
      y(i) = ( 3.0D+00 - 2.0D+00 * x(i) ) * x(i)**2
      dphi = dphi * 6.0D+00 * ( 1.0D+00 - x(i) ) * x(i)
    end do
    quad = quad + f ( dim_num, y ) * dphi
  end do

  quad = quad / real ( m, kind = 8 )

  return
end
subroutine lattice_print ( dim_num, m, z, title )

!*****************************************************************************80
!
!! LATTICE_PRINT prints the points in a lattice rule.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) M, the number of points to use.
!
!    Input, integer ( kind = 4 ) Z(DIM_NUM), the generator vector.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) dim_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  character ( len = * ) title
  integer ( kind = 4 ) y(dim_num)
  integer ( kind = 4 ) z(dim_num)

  if ( title /= ' ' ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '

  do i = 0, m-1
    y(1:dim_num) = mod ( i * z(1:dim_num), m )
    write ( *, '(i4,4x,10i4)') i+1, y(1:dim_num)
  end do

  return
end
subroutine monte_carlo ( dim_num, m, f, quad )

!*****************************************************************************80
!
!! MONTE_CARLO applies a Monte Carlo integration rule.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ian Sloan, Stephen Joe,
!    Lattice Methods for Multiple Integration,
!    Oxford, 1994,
!    ISBN: 0198534728,
!    LC: QA311.S56
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) M, the number of points to use.
!
!    Input, external, real ( kind = 8 ) F, the name of the user-supplied routine
!    which evaluates the function, of the form:
!    function f ( dim_num, x )
!    integer ( kind = 4 ) dim_num
!    real f
!    real x(dim_num)
!    f = ...
!
!    Output, real ( kind = 8 ) QUAD, the estimated integral.
!
  implicit none

  integer ( kind = 4 ) dim_num

  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  real ( kind = 8 ) quad
  real ( kind = 8 ) x(1:dim_num)

  quad = 0.0D+00

  do j = 1, m
    call random_number ( harvest = x(1:dim_num) )
    quad = quad + f ( dim_num, x )
  end do

  quad = quad / real ( m, kind = 8 )

  return
end
function prime ( n )

!*****************************************************************************80
!
!! PRIME returns any of the first PRIME_MAX prime numbers.
!
!  Discussion:
!
!    PRIME_MAX is 1600, and the largest prime stored is 13499.
!
!    Thanks to Bart Vandewoestyne for pointing out a typo, 18 February 2005.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964, pages 870-873.
!
!    Daniel Zwillinger,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996, pages 95-98.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the index of the desired prime number.
!    In general, is should be true that 0 <= N <= PRIME_MAX.
!    N = -1 returns PRIME_MAX, the index of the largest prime available.
!    N = 0 is legal, returning PRIME = 1.
!
!    Output, integer ( kind = 4 ) PRIME, the N-th prime.  If N is out of range,
!    PRIME is returned as -1.
!
  implicit none

  integer ( kind = 4 ), parameter :: prime_max = 1600

  integer ( kind = 4 ), save :: icall = 0
  integer ( kind = 4 ) n
  integer ( kind = 4 ), save, dimension ( prime_max ) :: npvec
  integer ( kind = 4 ) prime

  if ( icall == 0 ) then

    icall = 1

    npvec(1:100) = (/ &
        2,    3,    5,    7,   11,   13,   17,   19,   23,   29, &
       31,   37,   41,   43,   47,   53,   59,   61,   67,   71, &
       73,   79,   83,   89,   97,  101,  103,  107,  109,  113, &
      127,  131,  137,  139,  149,  151,  157,  163,  167,  173, &
      179,  181,  191,  193,  197,  199,  211,  223,  227,  229, &
      233,  239,  241,  251,  257,  263,  269,  271,  277,  281, &
      283,  293,  307,  311,  313,  317,  331,  337,  347,  349, &
      353,  359,  367,  373,  379,  383,  389,  397,  401,  409, &
      419,  421,  431,  433,  439,  443,  449,  457,  461,  463, &
      467,  479,  487,  491,  499,  503,  509,  521,  523,  541 /)

    npvec(101:200) = (/ &
      547,  557,  563,  569,  571,  577,  587,  593,  599,  601, &
      607,  613,  617,  619,  631,  641,  643,  647,  653,  659, &
      661,  673,  677,  683,  691,  701,  709,  719,  727,  733, &
      739,  743,  751,  757,  761,  769,  773,  787,  797,  809, &
      811,  821,  823,  827,  829,  839,  853,  857,  859,  863, &
      877,  881,  883,  887,  907,  911,  919,  929,  937,  941, &
      947,  953,  967,  971,  977,  983,  991,  997, 1009, 1013, &
     1019, 1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069, &
     1087, 1091, 1093, 1097, 1103, 1109, 1117, 1123, 1129, 1151, &
     1153, 1163, 1171, 1181, 1187, 1193, 1201, 1213, 1217, 1223 /)

    npvec(201:300) = (/ &
     1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283, 1289, 1291, &
     1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373, &
     1381, 1399, 1409, 1423, 1427, 1429, 1433, 1439, 1447, 1451, &
     1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499, 1511, &
     1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583, &
     1597, 1601, 1607, 1609, 1613, 1619, 1621, 1627, 1637, 1657, &
     1663, 1667, 1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733, &
     1741, 1747, 1753, 1759, 1777, 1783, 1787, 1789, 1801, 1811, &
     1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877, 1879, 1889, &
     1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973, 1979, 1987 /)

    npvec(301:400) = (/ &
     1993, 1997, 1999, 2003, 2011, 2017, 2027, 2029, 2039, 2053, &
     2063, 2069, 2081, 2083, 2087, 2089, 2099, 2111, 2113, 2129, &
     2131, 2137, 2141, 2143, 2153, 2161, 2179, 2203, 2207, 2213, &
     2221, 2237, 2239, 2243, 2251, 2267, 2269, 2273, 2281, 2287, &
     2293, 2297, 2309, 2311, 2333, 2339, 2341, 2347, 2351, 2357, &
     2371, 2377, 2381, 2383, 2389, 2393, 2399, 2411, 2417, 2423, &
     2437, 2441, 2447, 2459, 2467, 2473, 2477, 2503, 2521, 2531, &
     2539, 2543, 2549, 2551, 2557, 2579, 2591, 2593, 2609, 2617, &
     2621, 2633, 2647, 2657, 2659, 2663, 2671, 2677, 2683, 2687, &
     2689, 2693, 2699, 2707, 2711, 2713, 2719, 2729, 2731, 2741 /)

    npvec(401:500) = (/ &
     2749, 2753, 2767, 2777, 2789, 2791, 2797, 2801, 2803, 2819, &
     2833, 2837, 2843, 2851, 2857, 2861, 2879, 2887, 2897, 2903, &
     2909, 2917, 2927, 2939, 2953, 2957, 2963, 2969, 2971, 2999, &
     3001, 3011, 3019, 3023, 3037, 3041, 3049, 3061, 3067, 3079, &
     3083, 3089, 3109, 3119, 3121, 3137, 3163, 3167, 3169, 3181, &
     3187, 3191, 3203, 3209, 3217, 3221, 3229, 3251, 3253, 3257, &
     3259, 3271, 3299, 3301, 3307, 3313, 3319, 3323, 3329, 3331, &
     3343, 3347, 3359, 3361, 3371, 3373, 3389, 3391, 3407, 3413, &
     3433, 3449, 3457, 3461, 3463, 3467, 3469, 3491, 3499, 3511, &
     3517, 3527, 3529, 3533, 3539, 3541, 3547, 3557, 3559, 3571 /)

    npvec(501:600) = (/ &
     3581, 3583, 3593, 3607, 3613, 3617, 3623, 3631, 3637, 3643, &
     3659, 3671, 3673, 3677, 3691, 3697, 3701, 3709, 3719, 3727, &
     3733, 3739, 3761, 3767, 3769, 3779, 3793, 3797, 3803, 3821, &
     3823, 3833, 3847, 3851, 3853, 3863, 3877, 3881, 3889, 3907, &
     3911, 3917, 3919, 3923, 3929, 3931, 3943, 3947, 3967, 3989, &
     4001, 4003, 4007, 4013, 4019, 4021, 4027, 4049, 4051, 4057, &
     4073, 4079, 4091, 4093, 4099, 4111, 4127, 4129, 4133, 4139, &
     4153, 4157, 4159, 4177, 4201, 4211, 4217, 4219, 4229, 4231, &
     4241, 4243, 4253, 4259, 4261, 4271, 4273, 4283, 4289, 4297, &
     4327, 4337, 4339, 4349, 4357, 4363, 4373, 4391, 4397, 4409 /)

    npvec(601:700) = (/ &
     4421, 4423, 4441, 4447, 4451, 4457, 4463, 4481, 4483, 4493, &
     4507, 4513, 4517, 4519, 4523, 4547, 4549, 4561, 4567, 4583, &
     4591, 4597, 4603, 4621, 4637, 4639, 4643, 4649, 4651, 4657, &
     4663, 4673, 4679, 4691, 4703, 4721, 4723, 4729, 4733, 4751, &
     4759, 4783, 4787, 4789, 4793, 4799, 4801, 4813, 4817, 4831, &
     4861, 4871, 4877, 4889, 4903, 4909, 4919, 4931, 4933, 4937, &
     4943, 4951, 4957, 4967, 4969, 4973, 4987, 4993, 4999, 5003, &
     5009, 5011, 5021, 5023, 5039, 5051, 5059, 5077, 5081, 5087, &
     5099, 5101, 5107, 5113, 5119, 5147, 5153, 5167, 5171, 5179, &
     5189, 5197, 5209, 5227, 5231, 5233, 5237, 5261, 5273, 5279 /)

    npvec(701:800) = (/ &
     5281, 5297, 5303, 5309, 5323, 5333, 5347, 5351, 5381, 5387, &
     5393, 5399, 5407, 5413, 5417, 5419, 5431, 5437, 5441, 5443, &
     5449, 5471, 5477, 5479, 5483, 5501, 5503, 5507, 5519, 5521, &
     5527, 5531, 5557, 5563, 5569, 5573, 5581, 5591, 5623, 5639, &
     5641, 5647, 5651, 5653, 5657, 5659, 5669, 5683, 5689, 5693, &
     5701, 5711, 5717, 5737, 5741, 5743, 5749, 5779, 5783, 5791, &
     5801, 5807, 5813, 5821, 5827, 5839, 5843, 5849, 5851, 5857, &
     5861, 5867, 5869, 5879, 5881, 5897, 5903, 5923, 5927, 5939, &
     5953, 5981, 5987, 6007, 6011, 6029, 6037, 6043, 6047, 6053, &
     6067, 6073, 6079, 6089, 6091, 6101, 6113, 6121, 6131, 6133 /)

    npvec(801:900) = (/ &
     6143, 6151, 6163, 6173, 6197, 6199, 6203, 6211, 6217, 6221, &
     6229, 6247, 6257, 6263, 6269, 6271, 6277, 6287, 6299, 6301, &
     6311, 6317, 6323, 6329, 6337, 6343, 6353, 6359, 6361, 6367, &
     6373, 6379, 6389, 6397, 6421, 6427, 6449, 6451, 6469, 6473, &
     6481, 6491, 6521, 6529, 6547, 6551, 6553, 6563, 6569, 6571, &
     6577, 6581, 6599, 6607, 6619, 6637, 6653, 6659, 6661, 6673, &
     6679, 6689, 6691, 6701, 6703, 6709, 6719, 6733, 6737, 6761, &
     6763, 6779, 6781, 6791, 6793, 6803, 6823, 6827, 6829, 6833, &
     6841, 6857, 6863, 6869, 6871, 6883, 6899, 6907, 6911, 6917, &
     6947, 6949, 6959, 6961, 6967, 6971, 6977, 6983, 6991, 6997 /)

    npvec(901:1000) = (/ &
     7001, 7013, 7019, 7027, 7039, 7043, 7057, 7069, 7079, 7103, &
     7109, 7121, 7127, 7129, 7151, 7159, 7177, 7187, 7193, 7207, &
     7211, 7213, 7219, 7229, 7237, 7243, 7247, 7253, 7283, 7297, &
     7307, 7309, 7321, 7331, 7333, 7349, 7351, 7369, 7393, 7411, &
     7417, 7433, 7451, 7457, 7459, 7477, 7481, 7487, 7489, 7499, &
     7507, 7517, 7523, 7529, 7537, 7541, 7547, 7549, 7559, 7561, &
     7573, 7577, 7583, 7589, 7591, 7603, 7607, 7621, 7639, 7643, &
     7649, 7669, 7673, 7681, 7687, 7691, 7699, 7703, 7717, 7723, &
     7727, 7741, 7753, 7757, 7759, 7789, 7793, 7817, 7823, 7829, &
     7841, 7853, 7867, 7873, 7877, 7879, 7883, 7901, 7907, 7919 /)

    npvec(1001:1100) = (/ &
     7927, 7933, 7937, 7949, 7951, 7963, 7993, 8009, 8011, 8017, &
     8039, 8053, 8059, 8069, 8081, 8087, 8089, 8093, 8101, 8111, &
     8117, 8123, 8147, 8161, 8167, 8171, 8179, 8191, 8209, 8219, &
     8221, 8231, 8233, 8237, 8243, 8263, 8269, 8273, 8287, 8291, &
     8293, 8297, 8311, 8317, 8329, 8353, 8363, 8369, 8377, 8387, &
     8389, 8419, 8423, 8429, 8431, 8443, 8447, 8461, 8467, 8501, &
     8513, 8521, 8527, 8537, 8539, 8543, 8563, 8573, 8581, 8597, &
     8599, 8609, 8623, 8627, 8629, 8641, 8647, 8663, 8669, 8677, &
     8681, 8689, 8693, 8699, 8707, 8713, 8719, 8731, 8737, 8741, &
     8747, 8753, 8761, 8779, 8783, 8803, 8807, 8819, 8821, 8831 /)

    npvec(1101:1200) = (/ &
     8837, 8839, 8849, 8861, 8863, 8867, 8887, 8893, 8923, 8929, &
     8933, 8941, 8951, 8963, 8969, 8971, 8999, 9001, 9007, 9011, &
     9013, 9029, 9041, 9043, 9049, 9059, 9067, 9091, 9103, 9109, &
     9127, 9133, 9137, 9151, 9157, 9161, 9173, 9181, 9187, 9199, &
     9203, 9209, 9221, 9227, 9239, 9241, 9257, 9277, 9281, 9283, &
     9293, 9311, 9319, 9323, 9337, 9341, 9343, 9349, 9371, 9377, &
     9391, 9397, 9403, 9413, 9419, 9421, 9431, 9433, 9437, 9439, &
     9461, 9463, 9467, 9473, 9479, 9491, 9497, 9511, 9521, 9533, &
     9539, 9547, 9551, 9587, 9601, 9613, 9619, 9623, 9629, 9631, &
     9643, 9649, 9661, 9677, 9679, 9689, 9697, 9719, 9721, 9733 /)

    npvec(1201:1300) = (/ &
     9739, 9743, 9749, 9767, 9769, 9781, 9787, 9791, 9803, 9811, &
     9817, 9829, 9833, 9839, 9851, 9857, 9859, 9871, 9883, 9887, &
     9901, 9907, 9923, 9929, 9931, 9941, 9949, 9967, 9973,10007, &
    10009,10037,10039,10061,10067,10069,10079,10091,10093,10099, &
    10103,10111,10133,10139,10141,10151,10159,10163,10169,10177, &
    10181,10193,10211,10223,10243,10247,10253,10259,10267,10271, &
    10273,10289,10301,10303,10313,10321,10331,10333,10337,10343, &
    10357,10369,10391,10399,10427,10429,10433,10453,10457,10459, &
    10463,10477,10487,10499,10501,10513,10529,10531,10559,10567, &
    10589,10597,10601,10607,10613,10627,10631,10639,10651,10657 /)

    npvec(1301:1400) = (/ &
    10663,10667,10687,10691,10709,10711,10723,10729,10733,10739, &
    10753,10771,10781,10789,10799,10831,10837,10847,10853,10859, &
    10861,10867,10883,10889,10891,10903,10909,10937,10939,10949, &
    10957,10973,10979,10987,10993,11003,11027,11047,11057,11059, &
    11069,11071,11083,11087,11093,11113,11117,11119,11131,11149, &
    11159,11161,11171,11173,11177,11197,11213,11239,11243,11251, &
    11257,11261,11273,11279,11287,11299,11311,11317,11321,11329, &
    11351,11353,11369,11383,11393,11399,11411,11423,11437,11443, &
    11447,11467,11471,11483,11489,11491,11497,11503,11519,11527, &
    11549,11551,11579,11587,11593,11597,11617,11621,11633,11657 /)

    npvec(1401:1500) = (/ &
    11677,11681,11689,11699,11701,11717,11719,11731,11743,11777, &
    11779,11783,11789,11801,11807,11813,11821,11827,11831,11833, &
    11839,11863,11867,11887,11897,11903,11909,11923,11927,11933, &
    11939,11941,11953,11959,11969,11971,11981,11987,12007,12011, &
    12037,12041,12043,12049,12071,12073,12097,12101,12107,12109, &
    12113,12119,12143,12149,12157,12161,12163,12197,12203,12211, &
    12227,12239,12241,12251,12253,12263,12269,12277,12281,12289, &
    12301,12323,12329,12343,12347,12373,12377,12379,12391,12401, &
    12409,12413,12421,12433,12437,12451,12457,12473,12479,12487, &
    12491,12497,12503,12511,12517,12527,12539,12541,12547,12553 /)

   npvec(1501:1600) = (/ &
    12569,12577,12583,12589,12601,12611,12613,12619,12637,12641, &
    12647,12653,12659,12671,12689,12697,12703,12713,12721,12739, &
    12743,12757,12763,12781,12791,12799,12809,12821,12823,12829, &
    12841,12853,12889,12893,12899,12907,12911,12917,12919,12923, &
    12941,12953,12959,12967,12973,12979,12983,13001,13003,13007, &
    13009,13033,13037,13043,13049,13063,13093,13099,13103,13109, &
    13121,13127,13147,13151,13159,13163,13171,13177,13183,13187, &
    13217,13219,13229,13241,13249,13259,13267,13291,13297,13309, &
    13313,13327,13331,13337,13339,13367,13381,13397,13399,13411, &
    13417,13421,13441,13451,13457,13463,13469,13477,13487,13499 /)

  end if

  if ( n == -1 ) then
    prime = prime_max
  else if ( n == 0 ) then
    prime = 1
  else if ( n <= prime_max ) then
    prime = npvec(n)
  else
    prime = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PRIME - Fatal error!'
    write ( *, '(a,i6)' ) '  Illegal prime index N = ', n
    write ( *, '(a,i6)' ) '  N should be between 1 and PRIME_MAX =', prime_max
    stop
  end if

  return
end
subroutine r8mat_transpose_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT prints a R8MAT, transposed.
!
!  Discussion:
!
!    A R8MAT is a two dimensional matrix of double precision real values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT_SOME prints some of a R8MAT, transposed.
!
!  Discussion:
!
!    A R8MAT is a two dimensional matrix of double precision real values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

    i2hi = i2lo + incx - 1
    i2hi = min ( i2hi, m )
    i2hi = min ( i2hi, ihi )

    inc = i2hi + 1 - i2lo

    write ( *, '(a)' ) ' '

    do i = i2lo, i2hi
      i2 = i + 1 - i2lo
      write ( ctemp(i2), '(i8,6x)' ) i
    end do

    write ( *, '(''  Row   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Col'
    write ( *, '(a)' ) ' '

    j2lo = max ( jlo, 1 )
    j2hi = min ( jhi, n )

    do j = j2lo, j2hi

      do i2 = 1, inc
        i = i2lo - 1 + i2
        write ( ctemp(i2), '(g14.6)' ) a(i,j)
      end do

      write ( *, '(i5,1x,5a14)' ) j, ( ctemp(i), i = 1, inc )

    end do

  end do

  return
end
subroutine r8vec_uniform_01 ( n, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of real ( kind = 8 ) values.
!
!    For now, the input quantity SEED is an integer variable.
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
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = real ( seed, kind = 8 ) * 4.656612875D-10

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
subroutine tuple_next ( m1, m2, n, rank, x )

!*****************************************************************************80
!
!! TUPLE_NEXT computes the next element of a tuple space.
!
!  Discussion:
!
!    The elements are N vectors.  Each entry is constrained to lie
!    between M1 and M2.  The elements are produced one at a time.
!    The first element is
!      (M1,M1,...,M1),
!    the second element is
!      (M1,M1,...,M1+1),
!    and the last element is
!      (M2,M2,...,M2)
!    Intermediate elements are produced in lexicographic order.
!
!  Example:
!
!    N = 2, M1 = 1, M2 = 3
!
!    INPUT        OUTPUT
!    -------      -------
!    Rank  X      Rank   X
!    ----  ---    -----  ---
!    0     * *    1      1 1
!    1     1 1    2      1 2
!    2     1 2    3      1 3
!    3     1 3    4      2 1
!    4     2 1    5      2 2
!    5     2 2    6      2 3
!    6     2 3    7      3 1
!    7     3 1    8      3 2
!    8     3 2    9      3 3
!    9     3 3    0      0 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M1, M2, the minimum and maximum entries.
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input/output, integer ( kind = 4 ) RANK, counts the elements.
!    On first call, set RANK to 0.  Thereafter, the output value of RANK
!    will indicate the order of the element returned.  When there are no
!    more elements, RANK will be returned as 0.
!
!    Input/output, integer ( kind = 4 ) X(N), on input the previous tuple.
!    On output, the next tuple.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) m1
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) x(n)

  if ( m2 < m1 ) then
    rank = 0
    return
  end if

  if ( rank <= 0 ) then

    x(1:n) = m1
    rank = 1

  else

    rank = rank + 1
    i = n

    do

      if ( x(i) < m2 ) then
        x(i) = x(i) + 1
        exit
      end if

      x(i) = m1

      if ( i == 1 ) then
        rank = 0
        x(1:n) = m1
        exit
      end if

      i = i - 1

    end do

  end if

  return
end
