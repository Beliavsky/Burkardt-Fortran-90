subroutine butcher_b ( m, l, u )

!*****************************************************************************80
!
!! BUTCHER_B returns the bounds in the butcher problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = (/ -1.0D+00, -0.1D+00, -0.1D+00, -1.0D+00, -0.1D+00,  -0.1D+00 /)
  u(1:m) = (/  0.0D+00, +0.9D+00, +0.5D+00, -0.1D+00, -0.05D+00, -0.003D+00 /)

  return
end
subroutine butcher_f ( m, n, x, value )

!*****************************************************************************80
!
!! BUTCHER_F returns the function in the butcher problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
      x(6,1:n) * x(2,1:n) ** 2 &
    + x(5,1:n) * x(3,1:n) ** 2 &
    - x(1,1:n) * x(4,1:n) ** 2 &
    + x(4,1:n) ** 3 &
    + x(4,1:n) ** 2 &
    - 1.0D+00 / 3.0D+00 * x(1,1:n) &
    + 4.0D+00 / 3.0D+00 * x(4,1:n)  )
 
  return
end
subroutine butcher_m ( m )

!*****************************************************************************80
!
!! BUTCHER_M returns the number of variables in the butcher problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 6

  return
end
subroutine camel_b ( m, l, u )

!*****************************************************************************80
!
!! CAMEL_B returns the bounds in the camel problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -3.0D+00
  u(1:m) = +3.0D+00

  return
end
subroutine camel_f ( m, n, x, value )

!*****************************************************************************80
!
!! CAMEL_F returns the function in the camel problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
      4.0D+00           * x(1,1:n) ** 2             &
    - 2.1D+00           * x(1,1:n) ** 4             &
    + 1.0D+00 / 3.0D+00 * x(1,1:n) ** 6             &
    +                     x(1,1:n)       * x(2,1:n) &
    - 4.0D+00           * x(2,1:n) ** 2             &
    + 4.0D+00           * x(2,1:n) ** 4 )

  return
end
subroutine camel_m ( m )

!*****************************************************************************80
!
!! CAMEL_M returns the number of variables in the camel problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 2

  return
end
subroutine camera_b ( m, l, u )

!*****************************************************************************80
!
!! CAMERA_B returns the bounds in the camera problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -100.0D+00
  u(1:m) = +100.0D+00

  return
end
subroutine camera_f ( m, n, x, value )

!*****************************************************************************80
!
!! CAMERA_F returns the function in the camera problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
    - 6.8D+00 * x(1,1:n) * x(4,1:n) &
    - 3.2D+00 * x(1,1:n) * x(5,1:n) &
    + 1.3D+00 * x(1,1:n) * x(6,1:n) &
    + 5.1D+00 * x(1,1:n)             &
    - 3.2D+00 * x(2,1:n) * x(4,1:n) &
    - 4.8D+00 * x(2,1:n) * x(5,1:n) &
    - 0.7D+00 * x(2,1:n) * x(6,1:n) &
    - 7.1D+00 * x(2,1:n)             &
    + 1.3D+00 * x(3,1:n) * x(4,1:n) &
    - 0.7D+00 * x(3,1:n) * x(5,1:n) &
    + 9.0D+00 * x(3,1:n) * x(6,1:n) &
    -           x(3,1:n) &
    + 5.1D+00 * x(4,1:n) &
    - 7.1D+00 * x(5,1:n) &
    -           x(6,1:n) &
    + 2.6D+00 )

  return
end
subroutine camera_m ( m )

!*****************************************************************************80
!
!! CAMERA_M returns the number of variables in the camera problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 6

  return
end
subroutine caprasse_b ( m, l, u )

!*****************************************************************************80
!
!! CAPRASSE_B returns the bounds in the caprasse problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -0.5D+00
  u(1:m) = +0.5D+00

  return
end
subroutine caprasse_f ( m, n, x, value )

!*****************************************************************************80
!
!! CAPRASSE_F returns the function in the caprasse problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
    -            x(1,1:n)       * x(3,1:n) ** 3                 &
    +  4.0D+00 * x(2,1:n)       * x(3,1:n) ** 2 * x(4,1:n)      &
    +  4.0D+00 * x(1,1:n)       * x(3,1:n)      * x(4,1:n) ** 2 &
    +  2.0D+00 * x(2,1:n)       * x(4,1:n) ** 3                 &
    +  4.0D+00 * x(1,1:n)       * x(3,1:n)                      &
    +  4.0D+00 * x(3,1:n) ** 2                                  &
    - 10.0D+00 * x(2,1:n)       * x(4,1:n)                      &
    - 10.0D+00 * x(4,1:n) ** 2                                  &
    +  2.0D+00 )

  return
end
subroutine caprasse_m ( m )

!*****************************************************************************80
!
!! CAPRASSE_M returns the number of variables in the caprasse problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 4

  return
end
subroutine cyclic5_b ( m, l, u )

!*****************************************************************************80
!
!! CYCLIC5_B returns the bounds in the cyclic5 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -10.0D+00
  u(1:m) = +10.0D+00

  return
end
subroutine cyclic5_f ( m, n, x, value )

!*****************************************************************************80
!
!! CYCLIC5_F returns the function in the cyclic5 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
      x(1,1:n) * x(2,1:n) * x(3,1:n) * x(4,1:n) &
    + x(1,1:n) * x(2,1:n) * x(3,1:n) * x(5,1:n) &
    + x(1,1:n) * x(2,1:n) * x(4,1:n) * x(5,1:n) &
    + x(1,1:n) * x(3,1:n) * x(4,1:n) * x(5,1:n) &
    + x(2,1:n) * x(3,1:n) * x(4,1:n) * x(5,1:n) )

  return
end
subroutine cyclic5_m ( m )

!*****************************************************************************80
!
!! CYCLIC5_M returns the number of variables in the cyclic5 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 5

  return
end
subroutine cyclic7_b ( m, l, u )

!*****************************************************************************80
!
!! CYCLIC7_B returns the bounds in the cyclic7 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -1.0D+00
  u(1:m) = +1.0D+00

  return
end
subroutine cyclic7_f ( m, n, x, value )

!*****************************************************************************80
!
!! CYCLIC7_F returns the function in the cyclic7 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
      x(1,1:n) * x(2,1:n) * x(3,1:n) * x(4,1:n) * x(5,1:n) * x(6,1:n)            &
    + x(1,1:n) * x(2,1:n) * x(3,1:n) * x(4,1:n) * x(5,1:n)            * x(7,1:n) &
    + x(1,1:n) * x(2,1:n) * x(3,1:n) * x(4,1:n)            * x(6,1:n) * x(7,1:n) &
    + x(1,1:n) * x(2,1:n) * x(3,1:n)            * x(5,1:n) * x(6,1:n) * x(7,1:n) &
    + x(1,1:n) * x(2,1:n)            * x(4,1:n) * x(5,1:n) * x(6,1:n) * x(7,1:n) &
    + x(1,1:n)            * x(3,1:n) * x(4,1:n) * x(5,1:n) * x(6,1:n) * x(7,1:n) &
    +            x(2,1:n) * x(3,1:n) * x(4,1:n) * x(5,1:n) * x(6,1:n) * x(7,1:n) )

  return
end
subroutine cyclic7_m ( m )

!*****************************************************************************80
!
!! CYCLIC7_M returns the number of variables in the cyclic7 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 7

  return
end
subroutine cyclic8_b ( m, l, u )

!*****************************************************************************80
!
!! CYCLIC8_B returns the bounds in the cyclic8 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -1.0D+00
  u(1:m) = +1.0D+00

  return
end
subroutine cyclic8_f ( m, n, x, value )

!*****************************************************************************80
!
!! CYCLIC8_F returns the function in the cyclic8 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
      x(1,1:n) * x(2,1:n) * x(3,1:n) * x(4,1:n) * x(5,1:n) * x(6,1:n) * x(7,1:n)             &
    + x(1,1:n) * x(2,1:n) * x(3,1:n) * x(4,1:n) * x(5,1:n) * x(6,1:n)             * x(8,1:n) &
    + x(1,1:n) * x(2,1:n) * x(3,1:n) * x(4,1:n) * x(5,1:n)             * x(7,1:n) * x(8,1:n) &
    + x(1,1:n) * x(2,1:n) * x(3,1:n) * x(4,1:n)             * x(6,1:n) * x(7,1:n) * x(8,1:n) &
    + x(1,1:n) * x(2,1:n) * x(3,1:n)             * x(5,1:n) * x(6,1:n) * x(7,1:n) * x(8,1:n) &
    + x(1,1:n) * x(2,1:n)             * x(4,1:n) * x(5,1:n) * x(6,1:n) * x(7,1:n) * x(8,1:n) &
    + x(1,1:n)             * x(3,1:n) * x(4,1:n) * x(5,1:n) * x(6,1:n) * x(7,1:n) * x(8,1:n) &
    +             x(2,1:n) * x(3,1:n) * x(4,1:n) * x(5,1:n) * x(6,1:n) * x(7,1:n) * x(8,1:n) )

  return
end
subroutine cyclic8_m ( m )

!*****************************************************************************80
!
!! CYCLIC8_M returns the number of variables in the cyclic8 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 8

  return
end
subroutine goldstein_price_b ( m, l, u )

!*****************************************************************************80
!
!! GOLDSTEIN_PRICE_B returns the bounds in the goldstein_price problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -2.0D+00
  u(1:m) = +2.0D+00

  return
end
subroutine goldstein_price_f ( m, n, x, value )

!*****************************************************************************80
!
!! GOLDSTEIN_PRICE_F returns the function in the goldstein_price problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  g(1:n) = ( &
    1.0D+00 + ( x(1,1:n) + x(2,1:n) + 1.0D+00 ) ** 2 &
    * ( 19.0D+00 - 14.0D+00 * x(1,1:n) + 3.0D+00 * x(1,1:n) ** 2 &
    - 14.0D+00 * x(2,1:n) + 6.0D+00 * x(1,1:n) * x(2,1:n) &
    + 3.0D+00 * x(2,1:n) ** 2 ) )

  h(:n) = ( &
    30.0D+00 + ( 2.0D+00 * x(1,1:n) - 3.0D+00 * x(2,1:n) ) ** 2 &
    * ( 18.0D+00 - 32.0D+00 * x(1,1:n) + 12.0D+00 * x(1,1:n) ** 2 &
    + 48.0D+00 * x(2,1:n) - 36.0D+00 * x(1,1:n) * x(2,1:n) &
    + 27.0D+00 * x(2,1:n) ** 2 ) )

  value(1:n) = ( g(1:n) * h(1:n) )

  return
end
subroutine goldstein_price_m ( m )

!*****************************************************************************80
!
!! GOLDSTEIN_PRICE_M returns the number of variables in the goldstein_price problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 2

  return
end
subroutine hairer_b ( m, l, u )

!*****************************************************************************80
!
!! HAIRER_B returns the bounds in the hairer problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = (/ +2.0D+00, +2.0D+00, +2.0D+00, -5.0D+00, -5.0D+00, -5.0D+00 /)
  u(1:m) = (/ +5.0D+00, +5.0D+00, +5.0D+00, -2.0D+00, -2.0D+00, -2.0D+00 /)

  return
end
subroutine hairer_f ( m, n, x, value )

!*****************************************************************************80
!
!! HAIRER_F returns the function in the hairer problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
    +           x(3,1:n) ** 3 * x(4,1:n) &
    +           x(2,1:n) ** 3 * x(5,1:n) &
    +           x(1,1:n) ** 3 * x(6,1:n) &
    - 0.25D+00 )

  return
end
subroutine hairer_m ( m )

!*****************************************************************************80
!
!! HAIRER_M returns the number of variables in the hairer problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 6

  return
end
subroutine heart_b ( m, l, u )

!*****************************************************************************80
!
!! HEART_B returns the bounds in the heart problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = (/ -0.1D+00,  0.4D+00, -0.7D+00, -0.7D+00, +0.1D+00, -0.1D+00, -0.3D+00, -1.1D+00 /)
  u(1:m) = (/  0.4D+00, +1.0D+00, -0.4D+00, +0.4D+00, +0.2D+00, +0.2D+00, +1.1D+00, -0.3D+00 /)

  return
end
subroutine heart_f ( m, n, x, value )

!*****************************************************************************80
!
!! HEART_F returns the function in the heart problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
    +           x(1,1:n) * x(6,1:n) ** 3                 &
    - 3.0D+00 * x(1,1:n) * x(6,1:n)      * x(7,1:n) ** 2 &
    +           x(3,1:n) * x(7,1:n) ** 3                 &
    - 3.0D+00 * x(3,1:n) * x(7,1:n)      * x(6,1:n) ** 2 &
    +           x(2,1:n) * x(5,1:n) ** 3                 &
    - 3.0D+00 * x(2,1:n) * x(5,1:n)      * x(8,1:n) ** 2 &
    +           x(4,1:n) * x(8,1:n) ** 3                 &
    - 3.0D+00 * x(4,1:n) * x(8,1:n)      * x(5,1:n) ** 2 &
    + 0.9563453D+00 )
 
  return
end
subroutine heart_m ( m )

!*****************************************************************************80
!
!! HEART_M returns the number of variables in the heart problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 8

  return
end
subroutine himmelblau_b ( m, l, u )

!*****************************************************************************80
!
!! HIMMELBLAU_B returns the bounds in the himmelblau problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -5.0D+00
  u(1:m) = +5.0D+00

  return
end
subroutine himmelblau_f ( m, n, x, value )

!*****************************************************************************80
!
!! HIMMELBLAU_F returns the function in the himmelblau problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  g(1:n) = x(1,1:n) ** 2 + x(2,1:n)      - 11.0D+00
  h(1:n) = x(1,1:n)      + x(2,1:n) ** 2 -  7.0D+00
  value(1:n) = ( g(1:n) ** 2 + h(1:n) ** 2 )

  return
end
subroutine himmelblau_m ( m )

!*****************************************************************************80
!
!! HIMMELBLAU_M returns the number of variables in the himmelblau problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 2

  return
end
subroutine hunecke_b ( m, l, u )

!*****************************************************************************80
!
!! HUNECKE_B returns the bounds in the hunecke problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = (/  0.0D+00, +2.0D+00, -2.0D+00, +1.0D+00, -2.0D+00 /)
  u(1:m) = (/ +1.0D+00, +3.0D+00, -1.0D+00, +3.0D+00, -1.0D+00 /)

  return
end
subroutine hunecke_f ( m, n, x, value )

!*****************************************************************************80
!
!! HUNECKE_F returns the function in the hunecke problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
    +           x(2,1:n) ** 6  * x(3,1:n)                                                   &
    +           x(2,1:n)       * x(3,1:n) ** 6                                              &
    +           x(1,1:n) ** 2  * x(2,1:n) ** 4 * x(5,1:n)                                 &
    - 3.0D+00 * x(1,1:n)       * x(2,1:n) ** 2 * x(3,1:n) ** 2 * x(4,1:n)      * x(5,1:n) &
    +           x(3,1:n) ** 4  * x(4,1:n) ** 2 * x(5,1:n)                                 &
    -           x(1,1:n) ** 3  * x(3,1:n)      * x(4,1:n)      * x(5,1:n) ** 2            &
    -           x(1,1:n)       * x(2,1:n)      * x(4,1:n) ** 3 * x(5,1:n) ** 2            &
    +           x(2,1:n)       * x(3,1:n)      * x(5,1:n) ** 5 )

  return
end
subroutine hunecke_m ( m )

!*****************************************************************************80
!
!! HUNECKE_M returns the number of variables in the hunecke problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 5

  return
end
subroutine kearfott_b ( m, l, u )

!*****************************************************************************80
!
!! KEARFOTT_B returns the bounds in the kearfott problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -2.0D+00
  u(1:m) = +2.0D+00

  return
end
subroutine kearfott_f ( m, n, x, value )

!*****************************************************************************80
!
!! KEARFOTT_F returns the function in the kearfott problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = 0.0D+00

  do i = 1, m - 1
    value = value &
      + ( x(i,1:n) ** 2 - x(i+1,1:n) ) ** 2 &
      + ( x(m,1:n) ** 2 - x(i,1:n)   ) ** 2
  end do

  return
end
subroutine kearfott_m ( m )

!*****************************************************************************80
!
!! KEARFOTT_M returns the number of variables in the kearfott problem.
!
!  Discussion
!
!    Actually, the function can be defined for any 2 <= M.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 4

  return
end
subroutine lv3_b ( m, l, u )

!*****************************************************************************80
!
!! LV3_B returns the bounds in the lv3 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -1.5D+00
  u(1:m) = +2.0D+00

  return
end
subroutine lv3_f ( m, n, x, value )

!*****************************************************************************80
!
!! LV3_F returns the function in the lv3 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
    -           x(1,1:n) * x(2,1:n) ** 2 &
    +           x(1,1:n) * x(3,1:n) ** 2 &
    - 1.1D+00 * x(1,1:n) &
    + 1.0D+00 )
 
  return
end
subroutine lv3_m ( m )

!*****************************************************************************80
!
!! LV3_M returns the number of variables in the lv3 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 3

  return
end
subroutine lv4_b ( m, l, u )

!*****************************************************************************80
!
!! LV4_B returns the bounds in the lv4 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -2.0D+00
  u (1:m)= +2.0D+00

  return
end
subroutine lv4_f ( m, n, x, value )

!*****************************************************************************80
!
!! LV4_F returns the function in the lv4 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
    -           x(1,1:n) * x(2,1:n) ** 2 &
    +           x(1,1:n) * x(3,1:n) ** 2 &
    +           x(1,1:n) * x(4,1:n) ** 2 &
    - 1.1D+00 * x(1,1:n) &
    + 1.0D+00 )
 
  return
end
subroutine lv4_m ( m )

!*****************************************************************************80
!
!! LV4_M returns the number of variables in the lv4 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 4

  return
end
subroutine magnetism6_b ( m, l, u )

!*****************************************************************************80
!
!! MAGNETISM6_B returns the bounds in the magnetism6 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -5.0D+00
  u(1:m) = +5.0D+00

  return
end
subroutine magnetism6_f ( m, n, x, value )

!*****************************************************************************80
!
!! MAGNETISM6_F returns the function in the magnetism6 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
      2.0D+00 * x(1,1:n) ** 2 &
    + 2.0D+00 * x(2,1:n) ** 2 &
    + 2.0D+00 * x(3,1:n) ** 2 &
    + 2.0D+00 * x(4,1:n) ** 2 &
    + 2.0D+00 * x(5,1:n) ** 2 &
    +           x(6,1:n) ** 2 &
    -           x(6,1:n) )
 
  return
end
subroutine magnetism6_m ( m )

!*****************************************************************************80
!
!! MAGNETISM6_M returns the number of variables in the magnetism6 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 6

  return
end
subroutine magnetism7_b ( m, l, u )

!*****************************************************************************80
!
!! MAGNETISM7_B returns the bounds in the magnetism7 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -1.0D+00
  u(1:m) = +1.0D+00

  return
end
subroutine magnetism7_f ( m, n, x, value )

!*****************************************************************************80
!
!! MAGNETISM7_F returns the function in the magnetism7 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
                x(1,1:n) ** 2 &
    + 2.0D+00 * x(2,1:n) ** 2 &
    + 2.0D+00 * x(3,1:n) ** 2 &
    + 2.0D+00 * x(4,1:n) ** 2 &
    + 2.0D+00 * x(5,1:n) ** 2 &
    + 2.0D+00 * x(6,1:n) ** 2 &
    + 2.0D+00 * x(7,1:n) ** 2 &
    -           x(1,1:n) )
 
  return
end
subroutine magnetism7_m ( m )

!*****************************************************************************80
!
!! MAGNETISM7_M returns the number of variables in the magnetism7 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 7

  return
end
subroutine quadratic_b ( m, l, u )

!*****************************************************************************80
!
!! QUADRATIC_B returns the bounds in the quadratic problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) =  -99.99D+00
  u(1:m) = +100.00D+00

  return
end
subroutine quadratic_f ( m, n, x, value )

!*****************************************************************************80
!
!! QUADRATIC_F returns the function in the quadratic problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  r = - 2.0D+00

  value(1:n) = - r

  do i = 1, m
    value(1:n) = value(1:n) + x(i,1:n) ** 2
  end do

  return
end
subroutine quadratic_m ( m )

!*****************************************************************************80
!
!! QUADRATIC_M returns the number of variables in the quadratic problem.
!
!  Discussion
!
!    Actually, the function can be defined for any 1 <= M.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 8

  return
end
subroutine r8mat_uniform_abvec ( m, n, a, b, seed, r )

!*****************************************************************************80
!
!! R8MAT_UNIFORM_ABVEC returns a scaled pseudorandom R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A(I) <= R(I,J) <= B(I)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 October 2012
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
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    Input, real ( kind = 8 ) A(M), B(M), the lower and upper limits.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(M,N), the array of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m)
  real ( kind = 8 ) b(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_UNIFORM_ABVEC - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if

      r(i,j) = a(i) + ( b(i) - a(i) ) * real ( seed, kind = 8 ) &
        * 4.656612875D-10

    end do
  end do

  return
end
subroutine rd_b ( m, l, u )

!*****************************************************************************80
!
!! RD_B returns the bounds in the rd problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -5.0D+00
  u(1:m) = +5.0D+00

  return
end
subroutine rd_f ( m, n, x, value )

!*****************************************************************************80
!
!! RD_F returns the function in the rd problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
    -                   x(1,1:n) &
    + 2.0D+00         * x(2,1:n) &
    -                   x(3,1:n) &
    - 0.835634534D+00 * x(2,1:n) & 
    - 0.835634534D+00 * x(2,1:n) ** 2 )

  return
end
subroutine rd_m ( m )

!*****************************************************************************80
!
!! RD_M returns the number of variables in the rd problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 3

  return
end
subroutine reimer5_b ( m, l, u )

!*****************************************************************************80
!
!! REIMER5_B returns the bounds in the reimer5 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -1.0D+00
  u(1:m) = +1.0D+00

  return
end
subroutine reimer5_f ( m, n, x, value )

!*****************************************************************************80
!
!! REIMER5_F returns the function in the reimer5 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
    - 1.0D+00 &
    + 2.0D+00 * x(1,1:n) ** 6 &
    - 2.0D+00 * x(2,1:n) ** 6 &
    + 2.0D+00 * x(3,1:n) ** 6 &
    - 2.0D+00 * x(4,1:n) ** 6 &
    + 2.0D+00 * x(5,1:n) ** 6 )

  return
end
subroutine reimer5_m ( m )

!*****************************************************************************80
!
!! REIMER5_M returns the number of variables in the reimer5 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 5

  return
end
subroutine reimer6_b ( m, l, u )

!*****************************************************************************80
!
!! REIMER6_B returns the bounds in the reimer6 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -5.0D+00
  u(1:m) = +5.0D+00

  return
end
subroutine reimer6_f ( m, n, x, value )

!*****************************************************************************80
!
!! REIMER6_F returns the function in the reimer6 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
    - 1.0D+00 &
    + 2.0D+00 * x(1,1:n) ** 7 &
    - 2.0D+00 * x(2,1:n) ** 7 &
    + 2.0D+00 * x(3,1:n) ** 7 &
    - 2.0D+00 * x(4,1:n) ** 7 &
    + 2.0D+00 * x(5,1:n) ** 7 &
    - 2.0D+00 * x(6,1:n) ** 7 )

  return
end
subroutine reimer6_m ( m )

!*****************************************************************************80
!
!! REIMER6_M returns the number of variables in the reimer6 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 6

  return
end
subroutine rosenbrock_b ( m, l, u )

!*****************************************************************************80
!
!! ROSENBROCK_B returns the bounds in the rosenbrock problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) =  -5.0D+00
  u(1:m) = +10.0D+00

  return
end
subroutine rosenbrock_f ( m, n, x, value )

!*****************************************************************************80
!
!! ROSENBROCK_F returns the function in the rosenbrock problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = 0.0D+00

  do i = 1, m - 1
    value = value &
      + 100.0D+00 * ( x(i,1:n) - x(i+1,1:n) ) ** 2 &
      +             ( x(i,1:n) - 1.0D+00    ) ** 2
  end do

  return
end
subroutine rosenbrock_m ( m )

!*****************************************************************************80
!
!! ROSENBROCK_M returns the number of variables in the rosenbrock problem.
!
!  Discussion
!
!    Actually, the function can be defined for any 2 <= M.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 4

  return
end
subroutine schwefel_b ( m, l, u )

!*****************************************************************************80
!
!! SCHWEFEL_B returns the bounds in the schwefel problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -10.0D+00
  u(1:m) = +10.0D+00

  return
end
subroutine schwefel_f ( m, n, x, value )

!*****************************************************************************80
!
!! SCHWEFEL_F returns the function in the schwefel problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
      ( x(1,1:n) - x(2,1:n) ** 2 ) ** 2 &
    + ( x(2,1:n) - 1.0D+00       ) ** 2 &
    + ( x(1,1:n) - x(3,1:n) ** 2 ) ** 2 &
    + ( x(3,1:n) - 1.0D+00       ) ** 2 )

  return
end
subroutine schwefel_m ( m )

!*****************************************************************************80
!
!! SCHWEFEL_M returns the number of variables in the schwefel problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cesar Munoz, Anthony Narkawicz,
!    Formalization of Bernstein polynomials and applications to global 
!    optimization,
!    Journal of Automated Reasoning,
!    Volume 51, Number 2, 2013, pages 151-196.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 3

  return
end
subroutine smith1_b ( m, l, u )

!*****************************************************************************80
!
!! SMITH1_B returns the bounds in the smith1 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Andrew Smith,
!    Fast construction of constant bound functions for sparse polynomials,
!    Journal of Global Optimization,
!    Volume 43, 2009, pages 445-458.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = (/ +1.0D+00, +2.0D+00, +4.0D+00, -5.0D+00,  +2.0D+00 /)
  u(1:m) = (/ +2.0D+00, +3.0D+00, +6.0D+00, -2.0D+00, +10.0D+00 /)

  return
end
subroutine smith1_f ( m, n, x, value )

!*****************************************************************************80
!
!! SMITH1_F returns the function in the smith1 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Andrew Smith,
!    Fast construction of constant bound functions for sparse polynomials,
!    Journal of Global Optimization,
!    Volume 43, 2009, pages 445-458.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
      3.0D+00 * x(1,1:n) ** 2 * x(2,1:n) ** 3 * x(3,1:n) ** 4 &
    +           x(1,1:n) ** 3 * x(2,1:n)      * x(3,1:n) ** 3 &
    - 5.0D+00 * x(1,1:n)      * x(2,1:n)      * x(4,1:n) ** 5 &
    +           x(3,1:n)      * x(4,1:n)      * x(5,1:n) ** 3 )
 
  return
end
subroutine smith1_m ( m )

!*****************************************************************************80
!
!! SMITH1_M returns the number of variables in the smith1 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Andrew Smith,
!    Fast construction of constant bound functions for sparse polynomials,
!    Journal of Global Optimization,
!    Volume 43, 2009, pages 445-458.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 5

  return
end
subroutine smith2_b ( m, l, u )

!*****************************************************************************80
!
!! SMITH2_B returns the bounds in the smith2 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!  Reference:
!
!    Andrew Smith,
!    Fast construction of constant bound functions for sparse polynomials,
!    Journal of Global Optimization,
!    Volume 43, 2009, pages 445-458.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = +1.0D+00
  u(1:m) = +2.0D+00

  return
end
subroutine smith2_f ( m, n, x, value )

!*****************************************************************************80
!
!! SMITH2_F returns the function in the smith2 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Andrew Smith,
!    Fast construction of constant bound functions for sparse polynomials,
!    Journal of Global Optimization,
!    Volume 43, 2009, pages 445-458.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
       3.0D+00  * x(1,1:n)      * x(2,1:n) ** 5                 &
    +  2.0D+00  * x(1,1:n) ** 4 * x(2,1:n)                      &
    -  8.0D+00  * x(1,1:n) ** 2 * x(3,1:n) ** 6 * x(4,1:n) ** 2 &
    -             x(1,1:n)      * x(4,1:n) ** 8                 &
    +  3.0D+00  * x(2,1:n) ** 3 * x(5,1:n)                      &
    - 10.0D+00  * x(4,1:n) ** 5 * x(5,1:n) ** 5 * x(6,1:n) ** 5 &
    -  0.01D+00 * x(5,1:n) ** 2 * x(6,1:n) ** 2                 &
    +  4.0D+00  * x(5,1:n) ** 3 * x(7,1:n) ** 4 )
 
  return
end
subroutine smith2_m ( m )

!*****************************************************************************80
!
!! SMITH2_M returns the number of variables in the smith2 problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Andrew Smith,
!    Fast construction of constant bound functions for sparse polynomials,
!    Journal of Global Optimization,
!    Volume 43, 2009, pages 445-458.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 7

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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
subroutine virasoro_b ( m, l, u )

!*****************************************************************************80
!
!! VIRASORO_B returns the bounds in the virasoro problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -1.0D+00
  u(1:m) = +1.0D+00

  return
end
subroutine virasoro_f ( m, n, x, value )

!*****************************************************************************80
!
!! VIRASORO_F returns the function in the virasoro problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
    - 2.0D+00 * x(1,1:n)      * x(4,1:n) &
    + 2.0D+00 * x(1,1:n)      * x(7,1:n) &
    - 2.0D+00 * x(2,1:n)      * x(5,1:n) &
    + 2.0D+00 * x(2,1:n)      * x(7,1:n) &
    - 2.0D+00 * x(3,1:n)      * x(6,1:n) &
    + 2.0D+00 * x(3,1:n)      * x(7,1:n) &
    + 2.0D+00 * x(4,1:n)      * x(7,1:n) &
    + 2.0D+00 * x(5,1:n)      * x(7,1:n) &
    + 8.0D+00 * x(6,1:n)      * x(7,1:n) &
    - 6.0D+00 * x(6,1:n)      * x(8,1:n) &
    + 8.0D+00 * x(7,1:n) ** 2            &
    + 6.0D+00 * x(7,1:n)      * x(8,1:n) &
    -           x(7,1:n) )

  return
end
subroutine virasoro_m ( m )

!*****************************************************************************80
!
!! VIRASORO_M returns the number of variables in the virasoro problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 8

  return
end
subroutine wright_b ( m, l, u )

!*****************************************************************************80
!
!! WRIGHT_B returns the bounds in the wright problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) = -0.5D+00
  u(1:m) = +0.5D+00

  return
end
subroutine wright_f ( m, n, x, value )

!*****************************************************************************80
!
!! WRIGHT_F returns the function in the wright problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  value(1:n) = ( &
      x(5,1:n) ** 2 &
    + x(1,1:n) &
    + x(2,1:n) &
    + x(3,1:n) &
    + x(4,1:n) &
    - x(5,1:n) &
    - 10.0D+00 )

  return
end
subroutine wright_m ( m )

!*****************************************************************************80
!
!! WRIGHT_M returns the number of variables in the wright problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 5

  return
end
subroutine zakharov_b ( m, l, u )

!*****************************************************************************80
!
!! ZAKHAROV_B returns the bounds in the zakharov problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Output, real ( kind = 8 ) L(M), U(M), the lower and upper bounds.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) l(m)
  real ( kind = 8 ) u(m)

  l(1:m) =  -5.0D+00
  u(1:m) = +10.0D+00

  return
end
subroutine zakharov_f ( m, n, x, value )

!*****************************************************************************80
!
!! ZAKHAROV_F returns the function in the zakharov problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of variables.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(M,N), the points.
!
!    Output, real ( kind = 8 ) VALUE(N), the value of the function at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) s1(n)
  real ( kind = 8 ) s2(n)
  real ( kind = 8 ) value(n)
  real ( kind = 8 ) x(m,n)

  s1(1:n) = 0.0D+00
  s2(1:n) = 0.0D+00

  do i = 1, m
    s1(1:n) = s1(1:n) + x(i,1:n) **  2
    s2(1:n) = s2(1:n) + 0.5D+00 * real ( i, kind = 8 ) * x(i,1:n)
  end do

  value(1:n) = s1(1:n) + s2(1:n) ** 2 + s2(1:n) ** 4

  return
end
subroutine zakharov_m ( m )

!*****************************************************************************80
!
!! ZAKHAROV_M returns the number of variables in the zakharov problem.
!
!  Discussion
!
!    Actually, the function can be defined for any 1 <= M.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sashwati Ray, PSV Nataraj,
!    An efficient algorithm for range computation of polynomials using the
!    Bernstein form,
!    Journal of Global Optimization,
!    Volume 45, 2009, pages 403-426.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, the number of variables.
!
  implicit none

  integer ( kind = 4 ) m

  m = 5

  return
end

