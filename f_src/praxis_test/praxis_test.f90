program main

!*****************************************************************************80
!
!! MAIN is the main program for praxis_test.
!
!  Discussion:
!
!    praxis_test tests praxis.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'praxis_test'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test praxis.'
!
!  Minimization tests.
!
  call beale_test ( )
  call box_test ( )
  call chebyquad_test ( )
  call cube_test ( )
  call helix_test ( )
  call hilbert_test ( )
  call powell3d_test ( )
  call rosenbrock_test ( )
  call singular_test ( )
  call tridiagonal_test ( )
  call watson_test ( )
  call wood_test ( )
!
!  Utility tests.
!
  call minfit_test ( )
  call svsort_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'praxis_test'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine beale_test ( )

!*****************************************************************************80
!
!! BEALE_TEST calls PRAXIS for the Beale function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 2

  real ( kind = 8 ), external :: beale_f
  real ( kind = 8 ) h0
  real ( kind = 8 ) pr
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ) t0
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BEALE_TEST'
  write ( *, '(a)' ) '  The Beale function.'

  t0 = 0.00001D+00
  h0 = 0.25D+00
  prin = 0

  x(1:n) = (/ 0.1D+00, 0.1D+00 /)

  call r8vec_print ( n, x, '  Initial point:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', beale_f ( x, n )

  pr = praxis ( t0, h0, n, prin, x, beale_f )

  call r8vec_print ( n, x, '  Computed minimizer:' )
  
  write ( *, '(a,g14.6)' ) '  Function value = ', beale_f ( x, n )

  return
end
function beale_f ( x, n )

!*****************************************************************************80
!
!! BEALE_F evaluates the Beale function.
!
!  Discussion:
!
!    The function is the sum of the squares of three functions.
!
!    This function has a valley approaching the line X(2) = 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    E Beale,
!    On an Iterative Method for Finding a Local Minimum of a Function
!    of More than One Variable,
!    Technical Report 25, Statistical Techniques Research Group,
!    Princeton University, 1958.
!
!    Richard Brent,
!    Algorithms for Finding Zeros and Extrema of Functions Without
!    Calculating Derivatives,
!    Stanford University Technical Report STAN-CS-71-198.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the evaluation point.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Output, real ( kind = 8 ) BEALE_F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) beale_f
  real ( kind = 8 ), parameter :: c1 = 1.5D+00
  real ( kind = 8 ), parameter :: c2 = 2.25D+00
  real ( kind = 8 ), parameter :: c3 = 2.625D+00
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  real ( kind = 8 ) fx3
  real ( kind = 8 ) x(n)

  fx1 = c1 - x(1) * ( 1.0D+00 - x(2)      )
  fx2 = c2 - x(1) * ( 1.0D+00 - x(2) ** 2 )
  fx3 = c3 - x(1) * ( 1.0D+00 - x(2) ** 3 )

  beale_f = fx1 * fx1 + fx2 * fx2 + fx3 * fx3

  return
end
subroutine box_test ( )

!*****************************************************************************80
!
!! BOX_TEST calls PRAXIS for the Box function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ), external :: box_f
  real ( kind = 8 ) h0
  real ( kind = 8 ) pr
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ) t0
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BOX_TEST'
  write ( *, '(a)' ) '  The Box function.'

  t0 = 0.00001D+00
  h0 = 20.0D+00
  prin = 0

  x(1:n) = (/ 0.0D+00, 10.0D+00, 20.0D+00 /)

  call r8vec_print ( n, x, '  Initial point:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', box_f ( x, n )

  pr = praxis ( t0, h0, n, prin, x, box_f )

  call r8vec_print ( n, x, '  Computed minimizer:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', box_f ( x, n )

  return
end
function box_f ( x, n )

!*****************************************************************************80
!
!! BOX_F evaluates the Box function.
!
!  Discussion:
!
!    The function is formed by the sum of squares of 10 separate terms.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the evaluation point.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Output, real ( kind = 8 ) BOX_F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) box_f
  real ( kind = 8 ) c
  real ( kind = 8 ) fx
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  box_f = 0.0D+00

  do i = 1, 10

    c = - real ( i, kind = 8 ) / 10.0D+00

    fx = exp ( c * x(1) ) - exp ( c * x(2) ) - x(3) * &
      ( exp ( c ) - exp ( 10.0D+00 * c ) )
   
    box_f = box_f + fx * fx

  end do

  return
end
subroutine chebyquad_test ( )

!*****************************************************************************80
!
!! CHEBYQUAD_TEST calls PRAXIS for the Chebyquad function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 8

  real ( kind = 8 ), external :: chebyquad_f
  real ( kind = 8 ) h0
  integer ( kind = 4 ) i
  real ( kind = 8 ) pr
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ) t0
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CHEBYQUAD_TEST'
  write ( *, '(a)' ) '  The Chebyquad function.'

  t0 = 0.00001D+00
  h0 = 0.1D+00
  prin = 0

  do i = 1, n
    x(i) = real ( i, kind = 8 ) / real ( n + 1, kind = 8 )
  end do

  call r8vec_print ( n, x, '  Initial point:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', chebyquad_f ( x, n )

  pr = praxis ( t0, h0, n, prin, x, chebyquad_f )

  call r8vec_print ( n, x, '  Computed minimizer:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', chebyquad_f ( x, n )

  return
end
function chebyquad_f ( x, n )

!*****************************************************************************80
!
!! CHEBYQUAD_F evaluates the Chebyquad function.
!
!  Discussion:
!
!    The function is formed by the sum of squares of N separate terms.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the evaluation point.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Output, real ( kind = 8 ) CHEBYQUAD_F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) chebyquad_f
  real ( kind = 8 ) fvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) t
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) th
  real ( kind = 8 ) x(n)

  fvec(1:n) = 0.0D+00

  do j = 1, n

    t1 = 1.0D+00
    t2 = 2.0D+00 * x(j) - 1.0D+00
    t = 2.0D+00 * t2

    do i = 1, n
      fvec(i) = fvec(i) + t2
      th = t * t2 - t1
      t1 = t2
      t2 = th
    end do

  end do

  do i = 1, n
    fvec(i) = fvec(i) / real ( n, kind = 8 )
    if ( mod ( i, 2 ) == 0 ) then
      fvec(i) = fvec(i) + 1.0D+00 / real ( i * i - 1, kind = 8 )
    end if
  end do
!
!  Compute F.
!
  chebyquad_f = sum ( fvec(1:n) ** 2 )

  return
end
subroutine cube_test ( )

!*****************************************************************************80
!
!! CUBE_TEST calls PRAXIS for the Cube function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 2

  real ( kind = 8 ), external :: cube_f
  real ( kind = 8 ) h0
  real ( kind = 8 ) pr
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ) t0
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CUBE_TEST'
  write ( *, '(a)' ) '  The Cube function.'

  t0 = 0.00001D+00
  h0 = 1.0D+00
  prin = 0

  x(1:n) = (/ -1.2D+00, -1.0D+00 /)

  call r8vec_print ( n, x, '  Initial point:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', cube_f ( x, n )

  pr = praxis ( t0, h0, n, prin, x, cube_f )

  call r8vec_print ( n, x, '  Computed minimizer:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', cube_f ( x, n )

  return
end
function cube_f ( x, n )

!*****************************************************************************80
!
!! CUBE_F evaluates the Cube function.
!
!  Discussion:
!
!    The function is the sum of the squares of two functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the evaluation point.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Output, real ( kind = 8 ) CUBE_F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) cube_f
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  real ( kind = 8 ) x(n)

  fx1 = 10.0D+00 * ( x(2) - x(1) ** 3 )
  fx2 = 1.0D+00 - x(1)

  cube_f = fx1 ** 2 + fx2 ** 2

  return
end
subroutine helix_test ( )

!*****************************************************************************80
!
!! HELIX_TEST calls PRAXIS for the Fletcher-Powell Helix function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ), external :: helix_f
  real ( kind = 8 ) h0
  real ( kind = 8 ) pr
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ) t0
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HELIX_TEST'
  write ( *, '(a)' ) '  The Fletcher-Powell Helix function.'

  t0 = 0.00001D+00
  h0 = 1.0D+00
  prin = 0

  x(1:n) = (/ -1.0D+00, 0.0D+00, 0.0D+00 /)

  call r8vec_print ( n, x, '  Initial point:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', helix_f ( x, n )

  pr = praxis ( t0, h0, n, prin, x, helix_f )

  call r8vec_print ( n, x, '  Computed minimizer:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', helix_f ( x, n )

  return
end
function helix_f ( x, n )

!*****************************************************************************80
!
!! HELIX_F evaluates the Helix function.
!
!  Discussion:
!
!    The function is the sum of the squares of three functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the evaluation point.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Output, real ( kind = 8 ) HELIX_F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) helix_f
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  real ( kind = 8 ) fx3
  real ( kind = 8 ) r
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) theta
  real ( kind = 8 ) x(n)

  r = sqrt ( x(1) ** 2 + x(2) ** 2 )

  if ( 0.0D+00 <= x(1) ) then
    theta = 0.5D+00 * atan2 ( x(2), x(1) ) / r8_pi
  else if ( x(1) < 0.0D+00 ) then
    theta = 0.5D+00 * ( atan2 ( x(2), x(1) ) + r8_pi ) / r8_pi
  end if

  fx1 = 10.0D+00 * ( x(3) - 10.0D+00 * theta )
  fx2 = 10.0D+00 * ( r - 1.0D+00 )
  fx3 = x(3)

  helix_f = fx1 * fx1 + fx2 * fx2 + fx3 * fx3

  return
end
subroutine hilbert_test ( )

!*****************************************************************************80
!
!! HILBERT_TEST calls PRAXIS for the Hilbert function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) h0
  real ( kind = 8 ), external :: hilbert_f
  real ( kind = 8 ) pr
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ) t0
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HILBERT_TEST'
  write ( *, '(a)' ) '  The Hilbert function.'

  t0 = 0.00001D+00
  h0 = 10.0D+00
  prin = 0

  x(1:n) = 1.0D+00

  call r8vec_print ( n, x, '  Initial point:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', hilbert_f ( x, n )

  pr = praxis ( t0, h0, n, prin, x, hilbert_f )

  call r8vec_print ( n, x, '  Computed minimizer:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', hilbert_f ( x, n )

  return
end
function hilbert_f ( x, n )

!*****************************************************************************80
!
!! HILBERT_F evaluates the Hilbert function.
!
!  Discussion:
!
!    The function is a positive definite quadratic function of the form
!
!      f(x) = x' A x
!
!    where A is the Hilbert matrix, A(I,J) = 1/(I+J-1).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the evaluation point.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Output, real ( kind = 8 ) HILBERT_F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) hilbert_f
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)

  hilbert_f = 0.0D+00

  do i = 1, n
    do j = 1, n
      hilbert_f = hilbert_f + x(i) * x(j) / real ( i + j - 1, kind = 8 )
    end do
  end do

  return
end
subroutine powell3d_test ( )

!*****************************************************************************80
!
!! POWELL3D_TEST calls PRAXIS for the Powell 3D function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) h0
  real ( kind = 8 ), external :: powell3d_f
  real ( kind = 8 ) pr
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ) t0
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POWELL3D_TEST'
  write ( *, '(a)' ) '  The Powell 3D function.'

  t0 = 0.00001D+00
  h0 = 1.0D+00
  prin = 0

  x(1:n) = (/ 0.0D+00, 1.0D+00, 2.0D+00 /)

  call r8vec_print ( n, x, '  Initial point:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', powell3d_f ( x, n )

  pr = praxis ( t0, h0, n, prin, x, powell3d_f )

  call r8vec_print ( n, x, '  Computed minimizer:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', powell3d_f ( x, n )

  return
end
function powell3d_f ( x, n )

!*****************************************************************************80
!
!! POWELL3D_F evaluates the Powell 3D function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    M J D Powell,
!    An Efficient Method for Finding the Minimum of a Function of
!    Several Variables Without Calculating Derivatives,
!    Computer Journal, 
!    Volume 7, Number 2, pages 155-162, 1964.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the evaluation point.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Output, real ( kind = 8 ) POWELL3D_F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) powell3d_f
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)

  powell3d_f = 3.0D+00 - 1.0D+00 / ( 1.0D+00 + ( x(1) - x(2) ) ** 2 ) &
    - sin ( 0.5D+00 * r8_pi * x(2) * x(3) ) &
    - exp ( - ( ( x(1) - 2.0D+00 * x(2) + x(3) ) / x(2) ) ** 2 )

  return
end
subroutine rosenbrock_test ( )

!*****************************************************************************80
!
!! ROSENBROCK_TEST calls PRAXIS for the Rosenbrock function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 2

  real ( kind = 8 ) h0
  real ( kind = 8 ) pr
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ), external :: rosenbrock_f
  real ( kind = 8 ) t0
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ROSENBROCK_TEST'
  write ( *, '(a)' ) '  The Rosenbrock function.'

  t0 = 0.00001D+00
  h0 = 1.0D+00
  prin = 0

  x(1:n) = (/ -1.2D+00, 1.0D+00 /)

  call r8vec_print ( n, x, '  Initial point:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', rosenbrock_f ( x, n )

  pr = praxis ( t0, h0, n, prin, x, rosenbrock_f )

  call r8vec_print ( n, x, '  Computed minimizer:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', rosenbrock_f ( x, n )

  return
end
function rosenbrock_f ( x, n )

!*****************************************************************************80
!
!! ROSENBROCK_F evaluates the Rosenbrock function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the evaluation point.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Output, real ( kind = 8 ) ROSENBROCK_F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  real ( kind = 8 ) rosenbrock_f
  real ( kind = 8 ) x(n)

  rosenbrock_f = 0.0D+00
  do j = 1, n
    if ( mod ( j, 2 ) == 1 ) then
      rosenbrock_f = rosenbrock_f + ( 1.0D+00 - x(j) ) ** 2
    else
      rosenbrock_f = rosenbrock_f + 100.0D+00 * ( x(j) - x(j-1) ** 2 ) ** 2
    end if
  end do

  return
end
subroutine singular_test ( )

!*****************************************************************************80
!
!! SINGULAR_TEST calls PRAXIS for the Powell Singular function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) h0
  real ( kind = 8 ) pr
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ), external :: singular_f
  real ( kind = 8 ) t0
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SINGULAR_TEST'
  write ( *, '(a)' ) '  The Powell Singular function.'

  t0 = 0.00001D+00
  h0 = 1.0D+00
  prin = 0

  x(1:n) = (/ 3.0D+00, -1.0D+00, 0.0D+00, 1.0D+00 /)

  call r8vec_print ( n, x, '  Initial point:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', singular_f ( x, n )

  pr = praxis ( t0, h0, n, prin, x, singular_f )

  call r8vec_print ( n, x, '  Computed minimizer:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', singular_f ( x, n )

  return
end
function singular_f ( x, n )

!*****************************************************************************80
!
!! SINGULAR_F evaluates the Powell Singular function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the evaluation point.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Output, real ( kind = 8 ) SINGULAR_F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  real ( kind = 8 ) f3
  real ( kind = 8 ) f4
  integer ( kind = 4 ) j
  real ( kind = 8 ) singular_f
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xjp1
  real ( kind = 8 ) xjp2
  real ( kind = 8 ) xjp3

  singular_f = 0.0D+00

  do j = 1, n, 4

    if ( j + 1 <= n ) then
      xjp1 = x(j+1)
    else
      xjp1 = 0.0D+00
    end if

    if ( j + 2 <= n ) then
      xjp2 = x(j+2)
    else
      xjp2 = 0.0D+00
    end if

    if ( j + 3 <= n ) then
      xjp3 = x(j+3)
    else
      xjp3 = 0.0D+00
    end if
 
    f1 = x(j) + 10.0D+00 * xjp1

    if ( j + 1 <= n ) then
      f2 = xjp2 - xjp3
    else
      f2 = 0.0D+00
    end if

    if ( j + 2 <= n ) then
      f3 = xjp1 - 2.0D+00 * xjp2
    else
      f3 = 0.0D+00
    end if

    if ( j + 3 <= n ) then
      f4 = x(j) - xjp3
    else
      f4 = 0.0D+00
    end if

    singular_f = singular_f &
      +            f1 ** 2 &
      +  5.0D+00 * f2 ** 2 &
      +            f3 ** 4 &
      + 10.0D+00 * f4 ** 4

  end do

  return
end
subroutine tridiagonal_test ( )

!*****************************************************************************80
!
!! TRIDIAGONAL_TEST calls PRAXIS for the Tridiagonal function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) h0
  real ( kind = 8 ) pr
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ) t0
    real ( kind = 8 ), external :: tridiagonal_f
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIDIAGONAL_TEST'
  write ( *, '(a)' ) '  The Tridiagonal function.'

  t0 = 0.00001D+00
  h0 = 8.0D+00
  prin = 0

  x(1:n) = 0.0D+00

  call r8vec_print ( n, x, '  Initial point:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', tridiagonal_f ( x, n )

  pr = praxis ( t0, h0, n, prin, x, tridiagonal_f )

  call r8vec_print ( n, x, '  Computed minimizer:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', tridiagonal_f ( x, n )

  return
end
function tridiagonal_f ( x, n )

!*****************************************************************************80
!
!! TRIDIAGONAL_F evaluates the tridiagonal function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the evaluation point.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Output, real ( kind = 8 ) TRIDIAGONAL_F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) tridiagonal_f
  real ( kind = 8 ) x(n)

  tridiagonal_f = x(1) ** 2 + 2.0D+00 * sum ( x(2:n) ** 2 )

  do i = 1, n - 1
    tridiagonal_f = tridiagonal_f - 2.0D+00 * x(i) * x(i+1)
  end do

  tridiagonal_f = tridiagonal_f - 2.0D+00 * x(1)

  return
end
subroutine watson_test ( )

!*****************************************************************************80
!
!! WATSON_TEST calls PRAXIS for the Watson function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ) h0
  real ( kind = 8 ) pr
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ) t0
  real ( kind = 8 ) watson_f
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WATSON_TEST'
  write ( *, '(a)' ) '  The Watson function.'

  t0 = 0.00001D+00
  h0 = 1.0D+00
  prin = 0

  x(1:n) = 0.0D+00

  call r8vec_print ( n, x, '  Initial point:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', watson_f ( x, n )

  pr = praxis ( t0, h0, n, prin, x, watson_f )

  call r8vec_print ( n, x, '  Computed minimizer:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', watson_f ( x, n )

  return
end
function watson_f ( x, n )

!*****************************************************************************80
!
!! WATSON_F evaluates the Watson function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the evaluation point.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Output, real ( kind = 8 ) WATSON_F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) s1
  real ( kind = 8 ) s2
  real ( kind = 8 ) watson_f
  real ( kind = 8 ) x(n)

  watson_f = 0.0D+00

  do i = 1, 29

    s1 = 0.0D+00
    d = 1.0D+00
    do j = 2, n
      s1 = s1 + real ( j - 1, kind = 8 ) * d * x(j)
      d = d * real ( i, kind = 8 ) / 29.0D+00
    end do

    s2 = 0.0D+00
    d = 1.0D+00
    do j = 1, n
      s2 = s2 + d * x(j)
      d = d * real ( i, kind = 8 ) / 29.0D+00
    end do

    watson_f = watson_f + ( s1 - s2 ** 2 - 1.0D+00 ) ** 2

  end do

  watson_f = watson_f + x(1) ** 2 + ( x(2) - x(1) ** 2 - 1.0D+00 ) ** 2

  return
end
subroutine wood_test ( )

!*****************************************************************************80
!
!! WOOD_TEST calls PRAXIS for the Wood function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) h0
  real ( kind = 8 ) pr
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ) t0
  real ( kind = 8 ) wood_f
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WOOD_TEST'
  write ( *, '(a)' ) '  The Wood function.'

  t0 = 0.00001D+00
  h0 = 10.0D+00
  prin = 0

  x(1:n) = (/ -3.0D+00, -1.0D+00, -3.0D+00, -1.0D+00 /)

  call r8vec_print ( n, x, '  Initial point:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', wood_f ( x, n )

  pr = praxis ( t0, h0, n, prin, x, wood_f )

  call r8vec_print ( n, x, '  Computed minimizer:' )

  write ( *, '(a,g14.6)' ) '  Function value = ', wood_f ( x, n )

  return
end
function wood_f ( x, n )

!*****************************************************************************80
!
!! WOOD_F evaluates the Wood function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the evaluation point.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Output, real ( kind = 8 ) WOOD_F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  real ( kind = 8 ) f3
  real ( kind = 8 ) f4
  real ( kind = 8 ) f5
  real ( kind = 8 ) f6
  real ( kind = 8 ) wood_f
  real ( kind = 8 ) x(n)

  f1 = x(2) - x(1) ** 2
  f2 = 1.0D+00 - x(1)
  f3 = x(4) - x(3) ** 2
  f4 = 1.0D+00 - x(3)
  f5 = x(2) + x(4) - 2.0D+00
  f6 = x(2) - x(4)

  wood_f = &
      100.0D+00 * f1 ** 2 & 
    +             f2 ** 2 &
    +  90.0D+00 * f3 ** 2 &
    +             f4 ** 2 &
    +  10.0D+00 * f5 ** 2 &
    +   0.1D+00 * f6 ** 2

  return
end
subroutine minfit_test ( )

!*****************************************************************************80
!
!! MINFIT_TEST tests MINFIT, which is a sort of SVD computation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) a2(n,n)
  real ( kind = 8 ) d(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) tol

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MINFIT_TEST'
  write ( *, '(a)' ) '  MINFIT computes part of the SVD of a matrix A.'
  write ( *, '(a)' ) '    SVD: A = U * D * V'''
  write ( *, '(a)' ) '  MINFIT is given A, and returns the diagonal D'
  write ( *, '(a)' ) '  and the orthogonal matrix V.'
 
  a(1:n,1:n) = 0.0D+00
  do i = 1, n
    a(i,i) = 2.0D+00
  end do
  do i = 1, n - 1
    a(i,i+1) = -1.0D+00
  end do
  do i = 2, n
    a(i,i-1) = -1.0D+00
  end do

  call r8mat_print ( n, n, a, '  The matrix A:' )

  tol = sqrt ( epsilon ( tol ) )

  call minfit ( n, tol, a, d )

  call r8mat_print ( n, n, a, '  The vector V:' )

  call r8vec_print ( n, d, '  The singular values D:' )
!
!  Because A is positive definite symmetric, the "missing" matrix V = U.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Because A is positive definite symmetric,'
  write ( *, '(a)' ) '  we can reconstruct it as A = V * D * V'''

  do i = 1, n
    do j = 1, n
      a2(i,j) = 0.0D+00
      do k = 1, n
        a2(i,j) = a2(i,j) + a(i,k) * d(k) * a(j,k)
      end do
    end do
  end do

  call r8mat_print ( n, n, a2, '  The product A2 = V * D * V''' );

  return
end
subroutine svsort_test ( )

!*****************************************************************************80
!
!! SVSORT_TEST tests SVSORT, which sorts singular value information.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) d(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) v(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SVSORT_TEST'
  write ( *, '(a)' ) '  SVSORT sorts a vector D, and the corresponding columns'
  write ( *, '(a)' ) '  of a matrix V.'
  
! call random_number ( harvest = d(1:n) )

  seed = 123456789
  do i = 1, n
    d(i) = r8_uniform_01 ( seed )
  end do

  do i = 1, n
    do j = 1, n
      v(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  First row = entries of D.'
  write ( *, '(a)' ) '  Corresponding columns of V below.'
  write ( *, '(a)' ) ''
  write ( *, '(5g14.6)' ) d(1:n)
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(5g14.6)' ) v(i,1:n)
  end do

  call svsort ( n, d, v )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  After sorting D and rearranging V:'
  write ( *, '(a)' ) ''
  write ( *, '(5g14.6)' ) d(1:n)
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(5g14.6)' ) v(i,1:n)
  end do

  return
end

