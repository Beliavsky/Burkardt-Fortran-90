subroutine a_log ( rule, a )

!*****************************************************************************80
!
!! A_LOG returns the value of A for an Alpert rule for log singular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule, between 1 and 10.
!
!    Output, integer ( kind = 4 ) A, the value of A.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ), dimension ( 10 ) :: a_vec = (/ &
    1, 2, 2, 3, 3, &
    5, 6, 7, 9, 10 /)
  integer ( kind = 4 ) rule

  if ( rule < 1 .or. 10 < rule ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'A_LOG - Fatal error!'
    write ( *, '(a)' ) '  Input value of RULE is not between 1 and 10.'
    stop 1
  end if

  a = a_vec(rule)
  
  return
end
subroutine a_power ( rule, a )

!*****************************************************************************80
!
!! A_POWER returns A for an Alpert rule for power singular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule, between 1 and 12.
!
!    Output, integer ( kind = 4 ) A, the value of A.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ), dimension ( 12 ) :: a_vec = (/ &
    1,  2,  2,  2,  2, &
    3,  4,  5,  6,  8, &
    9, 10 /)
  integer ( kind = 4 ) rule

  if ( rule < 1 .or. 12 < rule ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'A_POWER - Fatal error!'
    write ( *, '(a)' ) '  Input value of RULE is not between 1 and 12.'
    stop 1
  end if

  a = a_vec(rule)
  
  return
end
subroutine a_regular ( rule, a )

!*****************************************************************************80
!
!! A_REGULAR returns the value of A for an Alpert rule for regular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule, between 1 and 12.
!
!    Output, integer ( kind = 4 ) A, the value of A.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ), dimension ( 12 ) :: a_vec = (/ &
     1,  2,  2,  3,  3, &
     4,  5,  7,  9, 10, &
    12, 14 /)
  integer ( kind = 4 ) rule

  if ( rule < 1 .or. 12 < rule ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'A_REGULAR - Fatal error!'
    write ( *, '(a)' ) '  Input value of RULE is not between 1 and 12.'
    stop 1
  end if

  a = a_vec(rule)
  
  return
end
subroutine integral_log ( value )

!*****************************************************************************80
!
!! INTEGRAL_LOG evaluates the test integral with logarithmic singularity.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VALUE, the integral of the test integrand 
!    from 0 to 1.
!
!
!  1/200 (-Sin[3/10] + Sin[2003/10] - SinIntegral[200])
!
  implicit none

  real ( kind = 8 ) value

  value = -0.012771107587415899716D+00

  return
end
subroutine integral_power ( value )

!*****************************************************************************80
!
!! INTEGRAL_POWER evaluates the test integral with power singularity.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VALUE, the integral of the test integrand 
!    from 0 to 1.
!
!  ( 20 * sqrt ( pi ) * FresnelC(20/sqrt(pi)) - sin(3/10) + sin(2003/10))/200
!
  implicit none

  real ( kind = 8 ) value

  value = 0.079321002746971411182D+00

  return
end
subroutine integral_regular ( value )

!*****************************************************************************80
!
!! INTEGRAL_REGULAR evaluates the regular test integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VALUE, the integral of the test integrand 
!    from 0 to 1.
!
!
!  1/200 (-Sin[3/10] + Sin[200] + Sin[2003/10])
!
  implicit none

  real ( kind = 8 ) value

  value = ( - sin ( 0.3D+00 ) + sin ( 200.0D+00 ) &
    + sin ( 200.3D+00 ) ) / 200.0D+00

  return
end
subroutine integrand_log ( n, x, f )

!*****************************************************************************80
!
!! INTEGRAND_LOG evaluates the test integrand with logarithmic singularity.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), the evaluation points.
!
!    Output, real ( kind = 8 ) F(N), the integrand at the evaluation points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) x(n)

  f(1:n) = cos ( 200.0D+00 * x(1:n) ) * log ( x(1:n) ) &
    + cos ( 200.0D+00 * x(1:n) + 0.3D+00 )

  return
end
subroutine integrand_power ( n, x, f )

!*****************************************************************************80
!
!! INTEGRAND_POWER evaluates the test integrand with power singularity.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), the evaluation points.
!
!    Output, real ( kind = 8 ) F(N), the integrand at the evaluation points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) x(n)

  f(1:n) = cos ( 200.0D+00 * x(1:n) ) * x(1:n) ** ( - 0.5D+00 ) &
    + cos ( 200.0D+00 * x(1:n) + 0.3D+00 )

  return
end
subroutine integrand_regular ( n, x, f )

!*****************************************************************************80
!
!! INTEGRAND_REGULAR evaluates the regular test integrand.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), the evaluation points.
!
!    Output, real ( kind = 8 ) F(N), the integrand at the evaluation points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) x(n)

  f(1:n) = cos ( 200.0D+00 * x(1:n) ) + cos ( 200.0D+00 * x(1:n) + 0.3D+00 )

  return
end
subroutine j_log ( rule, j )

!*****************************************************************************80
!
!! J_LOG returns the value of J for an Alpert rule for log singular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule, between 1 and 10.
!
!    Output, integer ( kind = 4 ) J, the value of J.
!
  implicit none
 
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( 10 ) :: j_vec = (/ &
     1,  2,  3,  4,  5, &
     7, 10, 11, 14, 15 /)
  integer ( kind = 4 ) rule

  if ( rule < 1 .or. 10 < rule ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'J_LOG - Fatal error!'
    write ( *, '(a)' ) '  Input value of RULE is not between 1 and 10.'
    stop 1
  end if

  j = j_vec(rule)
  
  return
end
subroutine j_power ( rule, j )

!*****************************************************************************80
!
!! J_POWER returns J for an Alpert rule for power singular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule, between 1 and 12.
!
!    Output, integer ( kind = 4 ) J, the value of J.
!
  implicit none

  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( 12 ) :: j_vec = (/ &
      1,  2,  2,  3,  3, &
      4,  6,  8, 10, 12, &
     14, 16 /)
  integer ( kind = 4 ) rule

  if ( rule < 1 .or. 12 < rule ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'J_POWER - Fatal error!'
    write ( *, '(a)' ) '  Input value of RULE is not between 1 and 12.'
    stop 1
  end if

  j = j_vec(rule)
  
  return
end
subroutine j_regular ( rule, j )

!*****************************************************************************80
!
!! J_REGULAR returns the value of J for an Alpert rule for regular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule, between 1 and 12.
!
!    Output, integer ( kind = 4 ) J, the value of J.
!
  implicit none

  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( 12 ) :: j_vec = (/ &
     1,  2,  2,  3,  3, &
     4,  6,  8, 10, 12, &
    14, 16 /)
  integer ( kind = 4 ) rule

  if ( rule < 1 .or. 12 < rule ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'J_REGULAR - Fatal error!'
    write ( *, '(a)' ) '  Input value of RULE is not between 1 and 12.'
    stop 1
  end if

  j = j_vec(rule)
  
  return
end
subroutine num_log ( num )

!*****************************************************************************80
!
!! NUM_LOG returns the number of Alpert rules for log singular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) NUM, the number of rules.
!
  implicit none

  integer ( kind = 4 ) num

  num = 10

  return
end
subroutine num_power ( num )

!*****************************************************************************80
!
!! NUM_POWER returns the number of Alpert rules for power singular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) NUM, the number of rules.
!
  implicit none

  integer ( kind = 4 ) num

  num = 12

  return
end
subroutine num_regular ( num )

!*****************************************************************************80
!
!! NUM_REGULAR returns the number of Alpert rules for regular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) NUM, the number of rules.
!
  implicit none

  integer ( kind = 4 ) num

  num = 12

  return
end
subroutine order_log ( rule, order )

!*****************************************************************************80
!
!! ORDER_LOG returns the order of an Alpert rule for log singular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule, between 1 and 10.
!
!    Output, integer ( kind = 4 ) ORDER, the order of the rule.
!
  implicit none

  integer ( kind = 4 ) order
  integer ( kind = 4 ), dimension ( 10 ) :: order_vec = (/ &
     2,  3,  4,  5,  6, &
     8, 10, 12, 14, 16 /)
  integer ( kind = 4 ) rule

  if ( rule < 1 .or. 10 < rule ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ORDER_LOG - Fatal error!'
    write ( *, '(a)' ) '  Input value of RULE is not between 1 and 10.'
    stop 1
  end if

  order = order_vec(rule)
  
  return
end
subroutine order_power ( rule, order )

!*****************************************************************************80
!
!! ORDER_POWER returns the order of an Alpert rule for power singular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule, between 1 and 10.
!
!    Output, real ( kind = 8 ) ORDER, the order of the rule.
!
  implicit none

  real ( kind = 8 ) order
  real ( kind = 8 ), dimension ( 12 ) :: order_vec = (/ &
     1.5,  2.0,  2.5,  3.0,  3.5, &
     4.0,  6.0,  8.0, 10.0, 12.0, &
    14.0, 16.0 /)
  integer ( kind = 4 ) rule

  if ( rule < 1 .or. 12 < rule ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ORDER_POWER - Fatal error!'
    write ( *, '(a)' ) '  Input value of RULE is not between 1 and 12.'
    stop 1
  end if

  order = order_vec(rule)
  
  return
end
subroutine order_regular ( rule, order )

!*****************************************************************************80
!
!! ORDER_REGULAR returns the order of an Alpert rule for regular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule, between 1 and 12.
!
!    Output, integer ( kind = 4 ) ORDER, the order of the rule.
!
  implicit none

  integer ( kind = 4 ) order
  integer ( kind = 4 ), dimension ( 12 ) :: order_vec = (/ &
     3,  4,  5,  6,  7, &
     8, 12, 16, 20, 24, &
    28, 32 /)
  integer ( kind = 4 ) rule

  if ( rule < 1 .or. 12 < rule ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ORDER_REGULAR - Fatal error!'
    write ( *, '(a)' ) '  Input value of RULE is not between 1 and 12.'
    stop 1
  end if

  order = order_vec(rule)
  
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
subroutine r8vec_uniform_01 ( n, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
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
    stop 1
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
subroutine rule_log ( rule, j, x, w )

!*****************************************************************************80
!
!! RULE_LOG returns an Alpert rule for log singular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Bradley Alpert,
!    Hybrid Gauss-Trapezoidal Quadrature Rules,
!    SIAM Journal on Scientific Computing,
!    Volume 20, Number 5, pages 1551-1584, 1999.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule, between 1 and 10.
!
!    Input, integer ( kind = 4 ) J, the number of points in the rule.
!
!    Output, real ( kind = 8 ) X(J), W(J), the points and weights for the rule.
!
  implicit none

  integer ( kind = 4 ) j

  integer ( kind = 4 ) rule
  real ( kind = 8 ) w(j)
  real ( kind = 8 ) x(j)

   if ( rule < 1 .or. 10 < rule ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'RULE_LOG - Fatal error!'
    write ( *, '(a)' ) '  Input value of RULE is not between 1 and 10.'
    stop 1
  end if

  if ( rule == 1 ) then

    x(1) = 1.591549430918953D-01
         
    w(1) = 5.0D-01

  else if ( rule == 2 ) then

    x(1) = 1.150395811972836D-01
    x(2) = 9.365464527949632D-01

    w(1) = 3.913373788753340D-01
    w(2) = 1.108662621124666D+00

  else if ( rule == 3 ) then

    x(1) = 2.379647284118974D-02
    x(2) = 2.935370741501914D-01
    x(3) = 1.023715124251890D+00

    w(1) = 8.795942675593887D-02
    w(2) = 4.989017152913699D-01
    w(3) = 9.131388579526912D-01

  else if ( rule == 4 ) then

    x(1) = 2.339013027203800D-02
    x(2) = 2.854764931311984D-01
    x(3) = 1.005403327220700D+00
    x(4) = 1.994970303994294D+00

    w(1) = 8.609736556158105D-02
    w(2) = 4.847019685417959D-01
    w(3) = 9.152988869123725D-01
    w(4) = 1.013901778984250D+00

  else if ( rule == 5 ) then

    x(1) = 4.004884194926570D-03
    x(2) = 7.745655373336686D-02
    x(3) = 3.972849993523248D-01
    x(4) = 1.075673352915104D+00
    x(5) = 2.003796927111872D+00

    w(1) = 1.671879691147102D-02
    w(2) = 1.636958371447360D-01
    w(3) = 4.981856569770637D-01
    w(4) = 8.372266245578912D-01
    w(5) = 9.841730844088381D-01

  else if ( rule == 6 ) then

    x(1) = 6.531815708567918D-03
    x(2) = 9.086744584657729D-02
    x(3) = 3.967966533375878D-01
    x(4) = 1.027856640525646D+00
    x(5) = 1.945288592909266D+00
    x(6) = 2.980147933889640D+00
    x(7) = 3.998861349951123D+00

    w(1) = 2.462194198995203D-02
    w(2) = 1.701315866854178D-01
    w(3) = 4.609256358650077D-01
    w(4) = 7.947291148621894D-01
    w(5) = 1.008710414337933D+00
    w(6) = 1.036093649726216D+00
    w(7) = 1.004787656533285D+00

  else if ( rule == 7 ) then

    x(1) = 1.175089381227308D-03
    x(2) = 1.877034129831289D-02
    x(3) = 9.686468391426860D-02
    x(4) = 3.004818668002884D-01
    x(5) = 6.901331557173356D-01
    x(6) = 1.293695738083659D+00
    x(7) = 2.090187729798780D+00
    x(8) = 3.016719313149212D+00
    x(9) = 4.001369747872486D+00
    x(10) = 5.000025661793423D+00

    w(1) = 4.560746882084207D-03
    w(2) = 3.810606322384757D-02
    w(3) = 1.293864997289512D-01
    w(4) = 2.884360381408835D-01
    w(5) = 4.958111914344961D-01
    w(6) = 7.077154600594529D-01
    w(7) = 8.741924365285083D-01
    w(8) = 9.661361986515218D-01
    w(9) = 9.957887866078700D-01
    w(10) = 9.998665787423845D-01

  else if ( rule == 8 ) then

    x(1) = 1.674223682668368D-03
    x(2) = 2.441110095009738D-02
    x(3) = 1.153851297429517D-01
    x(4) = 3.345898490480388D-01
    x(5) = 7.329740531807683D-01
    x(6) = 1.332305048525433D+00
    x(7) = 2.114358752325948D+00
    x(8) = 3.026084549655318D+00
    x(9) = 4.003166301292590D+00
    x(10) = 5.000141170055870D+00
    x(11) = 6.000001002441859D+00

    w(1) = 6.364190780720557D-03
    w(2) = 4.723964143287529D-02
    w(3) = 1.450891158385963D-01
    w(4) = 3.021659470785897D-01
    w(5) = 4.984270739715340D-01
    w(6) = 6.971213795176096D-01
    w(7) = 8.577295622757315D-01
    w(8) = 9.544136554351155D-01
    w(9) = 9.919938052776484D-01
    w(10) = 9.994621875822987D-01
    w(11) = 9.999934408092805D-01

  else if ( rule == 9 ) then

    x(1) = 9.305182368545380D-04
    x(2) = 1.373832458434617D-02
    x(3) = 6.630752760779359D-02
    x(4) = 1.979971397622003D-01
    x(5) = 4.504313503816532D-01
    x(6) = 8.571888631101634D-01
    x(7) = 1.434505229617112D+00
    x(8) = 2.175177834137754D+00
    x(9) = 3.047955068386372D+00
    x(10) = 4.004974906813428D+00
    x(11) = 4.998525901820967D+00
    x(12) = 5.999523015116678D+00
    x(13) = 6.999963617883990D+00
    x(14) = 7.999999488130134D+00

    w(1) = 3.545060644780164D-03
    w(2) = 2.681514031576498D-02
    w(3) = 8.504092035093420D-02
    w(4) = 1.854526216643691D-01
    w(5) = 3.251724374883192D-01
    w(6) = 4.911553747260108D-01
    w(7) = 6.622933417369036D-01
    w(8) = 8.137254578840510D-01
    w(9) = 9.235595514944174D-01
    w(10) = 9.821609923744658D-01
    w(11) = 1.000047394596121D+00
    w(12) = 1.000909336693954D+00
    w(13) = 1.000119534283784D+00
    w(14) = 1.000002835746089D+00

  else if ( rule == 10 ) then

    x(1) = 8.371529832014113D-04
    x(2) = 1.239382725542637D-02
    x(3) = 6.009290785739468D-02
    x(4) = 1.805991249601928D-01
    x(5) = 4.142832599028031D-01
    x(6) = 7.964747731112430D-01
    x(7) = 1.348993882467059D+00
    x(8) = 2.073471660264395D+00
    x(9) = 2.947904939031494D+00
    x(10) = 3.928129252248612D+00
    x(11) = 4.957203086563112D+00
    x(12) = 5.986360113977494D+00
    x(13) = 6.997957704791519D+00
    x(14) = 7.999888757524622D+00
    x(15) = 8.999998754306120D+00

    w(1) = 3.190919086626234D-03
    w(2) = 2.423621380426338D-02
    w(3) = 7.740135521653088D-02
    w(4) = 1.704889420286369D-01
    w(5) = 3.029123478511309D-01
    w(6) = 4.652220834914617D-01
    w(7) = 6.401489637096768D-01
    w(8) = 8.051212946181061D-01
    w(9) = 9.362411945698647D-01
    w(10) = 1.014359775369075D+00
    w(11) = 1.035167721053657D+00
    w(12) = 1.020308624984610D+00
    w(13) = 1.004798397441514D+00
    w(14) = 1.000395017352309D+00
    w(15) = 1.000007149422537D+00

  end if

  return
end
subroutine rule_power ( rule, j, x, w )

!*****************************************************************************80
!
!! RULE_POWER returns an Alpert rule for power singular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Bradley Alpert,
!    Hybrid Gauss-Trapezoidal Quadrature Rules,
!    SIAM Journal on Scientific Computing,
!    Volume 20, Number 5, pages 1551-1584, 1999.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule, between 1 and 12.
!
!    Input, integer ( kind = 4 ) J, the number of points in the rule.
!
!    Output, real ( kind = 8 ) X(J), W(J), the points and weights for the rule.
!
  implicit none

  integer ( kind = 4 ) j

  integer ( kind = 4 ) rule
  real ( kind = 8 ) w(j)
  real ( kind = 8 ) x(j)

  if ( rule < 1 .or. 12 < rule ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'RULE_POWER - Fatal error!'
    write ( *, '(a)' ) '  Input value of RULE is not between 1 and 12.'
    stop 1
  end if

  if ( rule == 1 ) then

    x(1) = 1.172258571393266D-01
         
    w(1) = 5.000000000000000D-01

  else if ( rule == 2 ) then

    x(1) = 9.252112715421378D-02
    x(2) = 1.000000000000000D-00

    w(1) = 4.198079625266162D-01
    w(2) = 1.080192037473384D+00

  else if ( rule == 3 ) then

    x(1) = 6.023873796408450D-02
    x(2) = 8.780704050676215D-01

    w(1) = 2.858439990420468D-01
    w(2) = 1.214156000957953D+00

  else if ( rule == 4 ) then

    x(1) = 7.262978413470474D-03
    x(2) = 2.246325512521893D-01
    x(3) = 1.000000000000000D+00

    w(1) = 3.907638767531813D-02
    w(2) = 4.873484056646474D-01
    w(3) = 9.735752066600344D-01

  else if ( rule == 5 ) then

    x(1) = 1.282368909458828D-02
    x(2) = 2.694286346792474D-01
    x(3) = 1.018414523786358D+00

    w(1) = 6.363996663105925D-02
    w(2) = 5.077434578043636D-01
    w(3) = 9.286165755645772D-01

  else if ( rule == 6 ) then

    x(1) = 1.189242434021285D-02
    x(2) = 2.578220434738662D-01
    x(3) = 1.007750064585281D+00
    x(4) = 2.000000000000000D+00

    w(1) = 5.927215035616424D-02
    w(2) = 4.955981740306228D-01
    w(3) = 9.427131290628058D-01
    w(4) = 1.002416546550407D+00

  else if ( rule == 7 ) then

    x(1) = 3.317925942699451D-03
    x(2) = 8.283019705296352D-02
    x(3) = 4.136094925726231D-01
    x(4) = 1.088744373688402D+00
    x(5) = 2.006482101852379D+00
    x(6) = 3.000000000000000D+00

    w(1) = 1.681780929883469D-02
    w(2) = 1.755244404544475D-01
    w(3) = 5.039350503858001D-01
    w(4) = 8.266241339680867D-01
    w(5) = 9.773065848981277D-01
    w(6) = 9.997919809947032D-01

  else if ( rule == 8 ) then

    x(1) = 1.214130606523435D-03
    x(2) = 3.223952700027058D-02
    x(3) = 1.790935383649920D-01
    x(4) = 5.437663805244631D-01
    x(5) = 1.176116628396759D+00
    x(6) = 2.031848210716014D+00
    x(7) = 3.001961225690812D+00
    x(8) = 4.000000000000000D+00

    w(1) = 6.199844884297793D-03
    w(2) = 7.106286791720044D-02
    w(3) = 2.408930104410471D-01
    w(4) = 4.975929263668960D-01
    w(5) = 7.592446540441226D-01
    w(6) = 9.322446399614420D-01
    w(7) = 9.928171438160095D-01
    w(8) = 9.999449125689846D-01

  else if ( rule == 9 ) then

    x(1) = 1.745862989163252D-04
    x(2) = 8.613670540457314D-03
    x(3) = 6.733385088703690D-02
    x(4) = 2.514488774733840D-01
    x(5) = 6.341845573737690D-01
    x(6) = 1.248404055083152D+00
    x(7) = 2.065688031953401D+00
    x(8) = 3.009199358662542D+00
    x(9) = 4.000416269690208D+00
    x(10) = 5.000000000000000D+00

    w(1) = 1.016950985948944D-03
    w(2) = 2.294670686517670D-02
    w(3) = 1.076657968022888D-01
    w(4) = 2.734577662465576D-01
    w(5) = 4.978815591924992D-01
    w(6) = 7.256208919565360D-01
    w(7) = 8.952638690320078D-01
    w(8) = 9.778157465381624D-01
    w(9) = 9.983390781399277D-01
    w(10) = 9.999916342408948D-01

  else if ( rule == 10 ) then

    x(1) = 5.710218427206990D-04
    x(2) = 1.540424351115548D-02
    x(3) = 8.834248407196555D-02
    x(4) = 2.824462054509770D-01
    x(5) = 6.574869892305580D-01
    x(6) = 1.246541060977993D+00
    x(7) = 2.039218495130811D+00
    x(8) = 2.979333487049800D+00
    x(9) = 3.985772595393049D+00
    x(10) = 4.997240804311428D+00
    x(11) = 5.999868793951190D+00
    x(12) = 7.000000000000000D+00

    w(1) = 2.921018926912141D-03
    w(2) = 3.431130611256885D-02
    w(3) = 1.224669495638615D-01
    w(4) = 2.761108242022520D-01
    w(5) = 4.797809643010337D-01
    w(6) = 6.966555677271379D-01
    w(7) = 8.790077941972658D-01
    w(8) = 9.868622449294327D-01
    w(9) = 1.015142389688201D+00
    w(10) = 1.006209712632210D+00
    w(11) = 1.000528829922287D+00
    w(12) = 1.000002397796838D+00

  else if ( rule == 11 ) then

    x(1) = 3.419821460249725D-04
    x(2) = 9.296593430187960D-03
    x(3) = 5.406214771755252D-02
    x(4) = 1.763945096508648D-01
    x(5) = 4.218486605653738D-01
    x(6) = 8.274022895884040D-01
    x(7) = 1.410287585637014D+00
    x(8) = 2.160997505238153D+00
    x(9) = 3.043504749358223D+00
    x(10) = 4.005692579069439D+00
    x(11) = 4.999732707905968D+00
    x(12) = 5.999875191971098D+00
    x(13) = 6.999994560568667D+00
    x(14) = 8.000000000000000D+00

    w(1) = 1.750957243202047D-03
    w(2) = 2.080726584287380D-02
    w(3) = 7.586830616433430D-02
    w(4) = 1.766020526671851D-01
    w(5) = 3.206624362072232D-01
    w(6) = 4.934405290553812D-01
    w(7) = 6.707497030698472D-01
    w(8) = 8.244959025366557D-01
    w(9) = 9.314646742162802D-01
    w(10) = 9.845768443163154D-01
    w(11) = 9.992852769154770D-01
    w(12) = 1.000273112957723D+00
    w(13) = 1.000022857402321D+00
    w(14) = 1.000000081405180D+00

  else if ( rule == 12 ) then

    x(1) = 2.158438988280793D-04
    x(2) = 5.898432743709196D-03
    x(3) = 3.462795956896131D-02
    x(4) = 1.145586495070213D-01
    x(5) = 2.790344218856415D-01
    x(6) = 5.600113798653321D-01
    x(7) = 9.814091242883119D-01
    x(8) = 1.553594853974655D+00
    x(9) = 2.270179114036658D+00
    x(10) = 3.108234601715371D+00
    x(11) = 4.032930893996553D+00
    x(12) = 5.006803270228157D+00
    x(13) = 6.000815466735179D+00
    x(14) = 7.000045035079542D+00
    x(15) = 8.000000738923901D+00
    x(16) = 9.000000000000000D+00

    w(1) = 1.105804873501181D-03
    w(2) = 1.324499944707956D-02
    w(3) = 4.899842307592144D-02
    w(4) = 1.165326192868815D-01
    w(5) = 2.178586693194957D-01
    w(6) = 3.481766016945031D-01
    w(7) = 4.964027915911545D-01
    w(8) = 6.469026189623831D-01
    w(9) = 7.823688971783889D-01
    w(10) = 8.877772445893361D-01
    w(11) = 9.551665077035583D-01
    w(12) = 9.876285579741800D-01
    w(13) = 9.979929183863017D-01
    w(14) = 9.998470620634641D-01
    w(15) = 9.999962891645340D-01
    w(16) = 9.999999946893169D-01

  end if

  return
end
subroutine rule_regular ( rule, j, x, w )

!*****************************************************************************80
!
!! RULE_REGULAR returns an Alpert rule for regular functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Bradley Alpert,
!    Hybrid Gauss-Trapezoidal Quadrature Rules,
!    SIAM Journal on Scientific Computing,
!    Volume 20, Number 5, pages 1551-1584, 1999.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) RULE, the index of the rule, between 1 and 12.
!
!    Input, integer ( kind = 4 ) J, the number of points in the rule.
!
!    Output, real ( kind = 8 ) X(J), W(J), the points and weights for the rule.
!
  implicit none

  integer ( kind = 4 ) j

  integer ( kind = 4 ) rule
  real ( kind = 8 ) w(j)
  real ( kind = 8 ) x(j)

  if ( rule < 1 .or. 12 < rule ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'RULE_REGULAR - Fatal error!'
    write ( *, '(a)' ) '  Input value of RULE is not between 1 and 12.'
    stop 1
  end if

  if ( rule == 1 ) then

    x(1) = 1.666666666666667D-01

    w(1) = 5.000000000000000D-01

  else if ( rule == 2 ) then

    x(1) = 2.000000000000000D-01
    x(2) = 1.000000000000000D+00

    w(1) = 5.208333333333333D-01
    w(2) = 9.791666666666667D-01

  else if ( rule == 3 ) then

    x(1) = 2.245784979812614D-01
    x(2) = 1.013719374359164D+00

    w(1) = 5.540781643606372D-01
    w(2) = 9.459218356393628D-01

  else if ( rule == 4 ) then

    x(1) = 2.250991042610971D-01
    x(2) = 1.014269060987992D+00
    x(3) = 2.000000000000000D+00

    w(1) = 5.549724327164180D-01
    w(2) = 9.451317411845473D-01
    w(3) = 9.998958260990347D-01

  else if ( rule == 5 ) then

    x(1) = 2.180540672543505D-01
    x(2) = 1.001181873031216D+00
    x(3) = 1.997580526418033D+00

    w(1) = 5.408088967208193D-01
    w(2) = 9.516615045823566D-01
    w(3) = 1.007529598696824D+00

  else if ( rule == 6 ) then

    x(1) = 2.087647422032129D-01
    x(2) = 9.786087373714483D-01
    x(3) = 1.989541386579751D+00
    x(4) = 3.000000000000000D+00

    w(1) = 5.207988277246498D-01
    w(2) = 9.535038018555888D-01
    w(3) = 1.024871626402471D+00
    w(4) = 1.000825744017291D+00

  else if ( rule == 7 ) then

    x(1) = 7.023955461621939D-02
    x(2) = 4.312297857227970D-01
    x(3) = 1.117752734518115D+00
    x(4) = 2.017343724572518D+00
    x(5) = 3.000837842847590D+00
    x(6) = 4.000000000000000D+00

    w(1) = 1.922315977843698D-01
    w(2) = 5.348399530514687D-01
    w(3) = 8.170209442488760D-01
    w(4) = 9.592111521445966D-01
    w(5) = 9.967143408044999D-01
    w(6) = 9.999820119661890D-01

  else if ( rule == 8 ) then

    x(1) = 9.919337841451028D-02
    x(2) = 5.076592669645529D-01
    x(3) = 1.184972925827278D+00
    x(4) = 2.047493467134072D+00
    x(5) = 3.007168911869310D+00
    x(6) = 4.000474996776184D+00
    x(7) = 5.000007879022339D+00
    x(8) = 6.000000000000000D+00

    w(1) = 2.528198928766921D-01
    w(2) = 5.550158230159486D-01
    w(3) = 7.852321453615224D-01
    w(4) = 9.245915673876714D-01
    w(5) = 9.839350200445296D-01
    w(6) = 9.984463448413151D-01
    w(7) = 9.999592378464547D-01
    w(8) = 9.999999686258662D-01

  else if ( rule == 9 ) then

    x(1) = 9.209200446233291D-02
    x(2) = 4.752021947758861D-01
    x(3) = 1.124687945844539D+00
    x(4) = 1.977387385642367D+00
    x(5) = 2.953848957822108D+00
    x(6) = 3.976136786048776D+00
    x(7) = 4.994354281979877D+00
    x(8) = 5.999469539335291D+00
    x(9) = 6.999986704874333D+00
    x(10) = 8.000000000000000D+00

    w(1) = 2.351836144643984D-01
    w(2) = 5.248820509085946D-01
    w(3) = 7.634026409869887D-01
    w(4) = 9.284711336658351D-01
    w(5) = 1.010969886587741D+00
    w(6) = 1.024959725311073D+00
    w(7) = 1.010517534639652D+00
    w(8) = 1.001551595797932D+00
    w(9) = 1.000061681794188D+00
    w(10) = 1.000000135843597D+00

  else if ( rule == 10 ) then

    x(1) = 6.001064731474805D-02
    x(2) = 3.149685016229433D-01
    x(3) = 7.664508240518316D-01
    x(4) = 1.396685781342510D+00
    x(5) = 2.175195903206602D+00
    x(6) = 3.062320575880355D+00
    x(7) = 4.016440988792476D+00
    x(8) = 5.002872064275734D+00
    x(9) = 6.000285453310164D+00
    x(10) = 7.000012964962529D+00
    x(11) = 8.000000175554469D+00
    x(12) = 9.000000000000000D+00

    w(1) = 1.538932104518340D-01
    w(2) = 3.551058128559424D-01
    w(3) = 5.449200036280007D-01
    w(4) = 7.104078497715549D-01
    w(5) = 8.398780940253654D-01
    w(6) = 9.272767950890611D-01
    w(7) = 9.750605697371132D-01
    w(8) = 9.942629650823470D-01
    w(9) = 9.992421778421898D-01
    w(10) = 9.999534370786161D-01
    w(11) = 9.999990854912925D-01
    w(12) = 9.999999989466828D-01

  else if ( rule == 11 ) then

    x(1) = 6.234360533194102D-02
    x(2) = 3.250286721702614D-01
    x(3) = 7.837350794282182D-01
    x(4) = 1.415673112616924D+00
    x(5) = 2.189894250061313D+00
    x(6) = 3.070053877483040D+00
    x(7) = 4.018613756218047D+00
    x(8) = 5.002705902035397D+00
    x(9) = 5.999929741810400D+00
    x(10) = 6.999904720846024D+00
    x(11) = 7.999986894843540D+00
    x(12) = 8.999999373380393D+00
    x(13) = 9.999999992002911D+00
    x(14) = 1.100000000000000D+01

    w(1) = 1.595975279734157D-01
    w(2) = 3.637046028193864D-01
    w(3) = 5.498753177297441D-01
    w(4) = 7.087986792086956D-01
    w(5) = 8.335172275501195D-01
    w(6) = 9.204446510608518D-01
    w(7) = 9.710881776552090D-01
    w(8) = 9.933296578555239D-01
    w(9) = 9.994759087910050D-01
    w(10) = 1.000133030254421D+00
    w(11) = 1.000032915011460D+00
    w(12) = 1.000002261653775D+00
    w(13) = 1.000000042393520D+00
    w(14) = 1.000000000042872D+00

  else if ( rule == 12 ) then

    x(1) = 5.899550614325259D-02
    x(2) = 3.082757062227814D-01
    x(3) = 7.463707253079130D-01
    x(4) = 1.355993726494664D+00
    x(5) = 2.112943217346336D+00
    x(6) = 2.987241496545946D+00
    x(7) = 3.944798920961176D+00
    x(8) = 4.950269202842798D+00
    x(9) = 5.972123043117706D+00
    x(10) = 6.989783558137742D+00
    x(11) = 7.997673019512965D+00
    x(12) = 8.999694932747039D+00
    x(13) = 9.999979225211805D+00
    x(14) = 1.099999938266130D+01
    x(15) = 1.199999999462073D+01
    x(16) = 1.300000000000000D+01

    w(1) = 1.511076023874179D-01
    w(2) = 3.459395921169090D-01
    w(3) = 5.273502805146873D-01
    w(4) = 6.878444094543021D-01
    w(5) = 8.210319140034114D-01
    w(6) = 9.218382875515803D-01
    w(7) = 9.873027487553060D-01
    w(8) = 1.018251913441155D+00
    w(9) = 1.021933430349293D+00
    w(10) = 1.012567983413513D+00
    w(11) = 1.004052289554521D+00
    w(12) = 1.000713413344501D+00
    w(13) = 1.000063618302950D+00
    w(14) = 1.000002486385216D+00
    w(15) = 1.000000030404477D+00
    w(16) = 1.000000000020760D+00

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
