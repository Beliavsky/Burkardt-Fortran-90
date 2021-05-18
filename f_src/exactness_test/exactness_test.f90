program main

!*****************************************************************************80
!
!! MAIN is the main program for EXACTNESS_TEST.
!
!  Discussion:
!
!    EXACTNESS_TEST tests the EXACTNESS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EXACTNESS_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the EXACTNESS library.'

  call chebyshev1_exactness_test ( )
  call chebyshev2_exactness_test ( )
  call chebyshev3_exactness_test ( )
  call clenshaw_curtis_exactness_test ( )
  call fejer1_exactness_test ( )
  call fejer2_exactness_test ( )
  call gegenbauer_exactness_test ( )
  call hermite_exactness_test ( )
  call hermite_1_exactness_test ( )
  call laguerre_exactness_test ( )
  call laguerre_1_exactness_test ( )
  call legendre_exactness_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EXACTNESS_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine chebyshev1_exactness_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV1_EXACTNESS_TEST tests rules for the Chebyshev1 integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CHEBYSHEV1_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Gauss-Chebyshev1 rules for the Chebyshev1 integral.'
  write ( *, '(a)' ) '  Density function rho(x) = 1/sqrt(1-x^2).'
  write ( *, '(a)' ) '  Region: -1 <= x <= +1.'
  write ( *, '(a)' ) '  Exactness: 2N-1.'

  do n = 1, 5
    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call chebyshev1_set ( n, x, w )
    p_max = 2 * n
    call chebyshev1_exactness ( n, x, w, p_max )
    deallocate ( x )
    deallocate ( w )
  end do

  return
end
subroutine chebyshev1_set ( n, x, w )

!*****************************************************************************80
!
!! CHEBYSHEV1_SET sets a Chebyshev Type 1 quadrature rule.
!
!  Discussion:
!
!    The integral:
!
!      integral ( -1 <= x <= 1 ) f(x) / sqrt ( 1 - x * x ) dx
!
!    The quadrature rule:
!
!      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    N must be between 1 and 10.
!
!    Output, real ( kind = 8 ) X(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then
    x( 1) =   0.0D+00
    w( 1) =    3.141592653589793D+00
  else if ( n == 2 ) then
    x( 1) =  -0.7071067811865475D+00
    x( 2) =   0.7071067811865476D+00
    w( 1) =    1.570796326794897D+00
    w( 2) =    1.570796326794897D+00
  else if ( n == 3 ) then  
    x( 1) =  -0.8660254037844387D+00
    x( 2) =   0.0D+00
    x( 3) =   0.8660254037844387D+00
    w( 1) =    1.047197551196598D+00
    w( 2) =    1.047197551196598D+00
    w( 3) =    1.047197551196598D+00
  else if ( n == 4 ) then
    x( 1) =  -0.9238795325112867D+00    
    x( 2) =  -0.3826834323650897D+00    
    x( 3) =   0.3826834323650898D+00    
    x( 4) =   0.9238795325112867D+00    
    w( 1) =   0.7853981633974483D+00    
    w( 2) =   0.7853981633974483D+00    
    w( 3) =   0.7853981633974483D+00
    w( 4) =   0.7853981633974483D+00
  else if ( n == 5 ) then
    x( 1) =  -0.9510565162951535D+00    
    x( 2) =  -0.5877852522924730D+00
    x( 3) =   0.0D+00
    x( 4) =   0.5877852522924731D+00    
    x( 5) =   0.9510565162951535D+00    
    w( 1) =   0.6283185307179586D+00    
    w( 2) =   0.6283185307179586D+00    
    w( 3) =   0.6283185307179586D+00    
    w( 4) =   0.6283185307179586D+00    
    w( 5) =   0.6283185307179586D+00
  else if ( n == 6 ) then
    x( 1) =  -0.9659258262890682D+00    
    x( 2) =  -0.7071067811865475D+00    
    x( 3) =  -0.2588190451025206D+00    
    x( 4) =   0.2588190451025207D+00    
    x( 5) =   0.7071067811865476D+00    
    x( 6) =   0.9659258262890683D+00    
    w( 1) =   0.5235987755982988D+00    
    w( 2) =   0.5235987755982988D+00    
    w( 3) =   0.5235987755982988D+00    
    w( 4) =   0.5235987755982988D+00    
    w( 5) =   0.5235987755982988D+00    
    w( 6) =   0.5235987755982988D+00
  else if ( n == 7 ) then
    x( 1) =  -0.9749279121818237D+00    
    x( 2) =  -0.7818314824680295D+00    
    x( 3) =  -0.4338837391175581D+00    
    x( 4) =   0.0D+00
    x( 5) =   0.4338837391175582D+00    
    x( 6) =   0.7818314824680298D+00    
    x( 7) =   0.9749279121818236D+00    
    w( 1) =   0.4487989505128276D+00    
    w( 2) =   0.4487989505128276D+00    
    w( 3) =   0.4487989505128276D+00    
    w( 4) =   0.4487989505128276D+00    
    w( 5) =   0.4487989505128276D+00    
    w( 6) =   0.4487989505128276D+00    
    w( 7) =   0.4487989505128276D+00
  else if ( n == 8 ) then
    x( 1) =  -0.9807852804032304D+00    
    x( 2) =  -0.8314696123025453D+00    
    x( 3) =  -0.5555702330196020D+00    
    x( 4) =  -0.1950903220161282D+00    
    x( 5) =   0.1950903220161283D+00    
    x( 6) =   0.5555702330196023D+00    
    x( 7) =   0.8314696123025452D+00    
    x( 8) =   0.9807852804032304D+00    
    w( 1) =   0.3926990816987241D+00    
    w( 2) =   0.3926990816987241D+00    
    w( 3) =   0.3926990816987241D+00    
    w( 4) =   0.3926990816987241D+00    
    w( 5) =   0.3926990816987241D+00    
    w( 6) =   0.3926990816987241D+00    
    w( 7) =   0.3926990816987241D+00    
    w( 8) =   0.3926990816987241D+00
  else if ( n == 9 ) then
    x( 1) =  -0.9848077530122080D+00    
    x( 2) =  -0.8660254037844385D+00    
    x( 3) =  -0.6427876096865394D+00    
    x( 4) =  -0.3420201433256685D+00
    x( 5) =   0.0D+00
    x( 6) =   0.3420201433256688D+00    
    x( 7) =   0.6427876096865394D+00    
    x( 8) =   0.8660254037844387D+00    
    x( 9) =   0.9848077530122080D+00
    w( 1) =   0.3490658503988659D+00    
    w( 2) =   0.3490658503988659D+00    
    w( 3) =   0.3490658503988659D+00    
    w( 4) =   0.3490658503988659D+00    
    w( 5) =   0.3490658503988659D+00    
    w( 6) =   0.3490658503988659D+00    
    w( 7) =   0.3490658503988659D+00    
    w( 8) =   0.3490658503988659D+00    
    w( 9) =   0.3490658503988659D+00 
  else if ( n == 10 ) then
    x( 1) =  -0.9876883405951377D+00    
    x( 2) =  -0.8910065241883678D+00    
    x( 3) =  -0.7071067811865475D+00    
    x( 4) =  -0.4539904997395467D+00    
    x( 5) =  -0.1564344650402306D+00    
    x( 6) =   0.1564344650402309D+00    
    x( 7) =   0.4539904997395468D+00    
    x( 8) =   0.7071067811865476D+00    
    x( 9) =   0.8910065241883679D+00    
    x(10) =   0.9876883405951378D+00    
    w( 1) =   0.3141592653589793D+00    
    w( 2) =   0.3141592653589793D+00    
    w( 3) =   0.3141592653589793D+00    
    w( 4) =   0.3141592653589793D+00    
    w( 5) =   0.3141592653589793D+00    
    w( 6) =   0.3141592653589793D+00    
    w( 7) =   0.3141592653589793D+00    
    w( 8) =   0.3141592653589793D+00    
    w( 9) =   0.3141592653589793D+00
    w(10) =   0.3141592653589793D+00
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHEBYSHEV1_SET - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal value of N = ', n
    write ( *, '(a)' ) '  Legal values are 1 through 10.'
    stop 1
  end if

  return
end
subroutine chebyshev2_exactness_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV2_EXACTNESS_TEST tests rules for the Chebyshev2 integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CHEBYSHEV2_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Gauss-Chebyshev2 rules for the Chebyshev2 integral.'
  write ( *, '(a)' ) '  Density function rho(x) = sqrt(1-x^2).'
  write ( *, '(a)' ) '  Region: -1 <= x <= +1.'
  write ( *, '(a)' ) '  Exactness: 2N-1.'

  do n = 1, 5
    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call chebyshev2_set ( n, x, w )
    p_max = 2 * n
    call chebyshev2_exactness ( n, x, w, p_max )
    deallocate ( x )
    deallocate ( w )
  end do

  return
end
subroutine chebyshev2_set ( n, x, w )

!*****************************************************************************80
!
!! CHEBYSHEV2_SET sets a Chebyshev Type 2 quadrature rule.
!
!  Discussion:
!
!    The integral:
!
!      integral ( -1 <= x <= 1 ) f(x) * sqrt ( 1 - x * x ) dx
!
!    The quadrature rule:
!
!      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    N must be between 1 and 10.
!
!    Output, real ( kind = 8 ) X(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then
    x( 1) =   0.0D+00
    w( 1) =    1.570796326794897D+00
  else if ( n == 2 ) then
    x( 1) =  -0.5000000000000000D+00    
    x( 2) =   0.5000000000000000D+00    
    w( 1) =   0.7853981633974484D+00    
    w( 2) =   0.7853981633974481D+00 
  else if ( n == 3 ) then   
    x( 1) =  -0.7071067811865475D+00    
    x( 2) =   0.0D+00
    x( 3) =   0.7071067811865476D+00    
    w( 1) =   0.3926990816987243D+00    
    w( 2) =   0.7853981633974483D+00    
    w( 3) =   0.3926990816987240D+00
  else if ( n == 4 ) then   
    x( 1) =  -0.8090169943749473D+00    
    x( 2) =  -0.3090169943749473D+00    
    x( 3) =   0.3090169943749475D+00    
    x( 4) =   0.8090169943749475D+00    
    w( 1) =   0.2170787134227061D+00    
    w( 2) =   0.5683194499747424D+00    
    w( 3) =   0.5683194499747423D+00    
    w( 4) =   0.2170787134227060D+00
  else if ( n == 5 ) then   
    x( 1) =  -0.8660254037844387D+00   
    x( 2) =  -0.5000000000000000D+00    
    x( 3) =   0.0D+00
    x( 4) =   0.5000000000000000D+00    
    x( 5) =   0.8660254037844387D+00    
    w( 1) =   0.1308996938995747D+00    
    w( 2) =   0.3926990816987242D+00    
    w( 3) =   0.5235987755982988D+00    
    w( 4) =   0.3926990816987240D+00    
    w( 5) =   0.1308996938995747D+00
  else if ( n == 6 ) then  
    x( 1) =  -0.9009688679024190D+00    
    x( 2) =  -0.6234898018587335D+00    
    x( 3) =  -0.2225209339563143D+00    
    x( 4) =   0.2225209339563144D+00    
    x( 5) =   0.6234898018587336D+00    
    x( 6) =   0.9009688679024191D+00    
    w( 1) =   0.08448869089158863D+00
    w( 2) =   0.2743330560697779D+00    
    w( 3) =   0.4265764164360819D+00    
    w( 4) =   0.4265764164360819D+00    
    w( 5) =   0.2743330560697778D+00    
    w( 6) =   0.08448869089158857D+00
  else if ( n == 7 ) then
    x( 1) =  -0.9238795325112867D+00    
    x( 2) =  -0.7071067811865475D+00    
    x( 3) =  -0.3826834323650897D+00    
    x( 4) =   0.0D+00
    x( 5) =   0.3826834323650898D+00    
    x( 6) =   0.7071067811865476D+00    
    x( 7) =   0.9238795325112867D+00    
    w( 1) =   0.05750944903191316D+00
    w( 2) =   0.1963495408493621D+00    
    w( 3) =   0.3351896326668110D+00    
    w( 4) =   0.3926990816987241D+00    
    w( 5) =   0.3351896326668110D+00    
    w( 6) =   0.1963495408493620D+00    
    w( 7) =   0.05750944903191313D+00
  else if ( n == 8 ) then
    x( 1) =  -0.9396926207859083D+00    
    x( 2) =  -0.7660444431189779D+00    
    x( 3) =  -0.5000000000000000D+00    
    x( 4) =  -0.1736481776669303D+00    
    x( 5) =   0.1736481776669304D+00    
    x( 6) =   0.5000000000000000D+00    
    x( 7) =   0.7660444431189780D+00    
    x( 8) =   0.9396926207859084D+00    
    w( 1) =   0.04083294770910712D+00
    w( 2) =   0.1442256007956728D+00    
    w( 3) =   0.2617993877991495D+00    
    w( 4) =   0.3385402270935190D+00    
    w( 5) =   0.3385402270935190D+00    
    w( 6) =   0.2617993877991494D+00    
    w( 7) =   0.1442256007956727D+00    
    w( 8) =   0.04083294770910708D+00
  else if ( n == 9 ) then
    x( 1) =  -0.9510565162951535D+00    
    x( 2) =  -0.8090169943749473D+00    
    x( 3) =  -0.5877852522924730D+00    
    x( 4) =  -0.3090169943749473D+00    
    x( 5) =   0.0D+00
    x( 6) =   0.3090169943749475D+00    
    x( 7) =   0.5877852522924731D+00    
    x( 8) =   0.8090169943749475D+00    
    x( 9) =   0.9510565162951535D+00    
    w( 1) =   0.02999954037160818D+00
    w( 2) =   0.1085393567113530D+00    
    w( 3) =   0.2056199086476263D+00    
    w( 4) =   0.2841597249873712D+00    
    w( 5) =   0.3141592653589793D+00    
    w( 6) =   0.2841597249873711D+00    
    w( 7) =   0.2056199086476263D+00    
    w( 8) =   0.1085393567113530D+00    
    w( 9) =   0.02999954037160816D+00
  else if ( n == 10 ) then
    x( 1) =  -0.9594929736144974D+00    
    x( 2) =  -0.8412535328311811D+00    
    x( 3) =  -0.6548607339452850D+00    
    x( 4) =  -0.4154150130018863D+00    
    x( 5) =  -0.1423148382732850D+00    
    x( 6) =   0.1423148382732851D+00    
    x( 7) =   0.4154150130018864D+00    
    x( 8) =   0.6548607339452851D+00    
    x( 9) =   0.8412535328311812D+00    
    x(10) =   0.9594929736144974D+00    
    w( 1) =   0.02266894250185884D+00
    w( 2) =   0.08347854093418908D+00
    w( 3) =   0.1631221774548166D+00    
    w( 4) =   0.2363135602034873D+00    
    w( 5) =   0.2798149423030966D+00    
    w( 6) =   0.2798149423030965D+00    
    w( 7) =   0.2363135602034873D+00    
    w( 8) =   0.1631221774548166D+00    
    w( 9) =   0.08347854093418902D+00
    w(10) =   0.02266894250185884D+00
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHEBYSHEV2_SET - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal value of N = ', n
    write ( *, '(a)' ) '  Legal values are 1 through 10.'
    stop 1
  end if

  return
end
subroutine chebyshev3_exactness_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV3_EXACTNESS_TEST tests rules for the Chebyshev1 integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CHEBYSHEV3_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Gauss-Chebyshev3 rules for the Chebyshev1 integral.'
  write ( *, '(a)' ) '  Density function rho(x) = 1/sqrt(1-x^2).'
  write ( *, '(a)' ) '  Region: -1 <= x <= +1.'
  write ( *, '(a)' ) '  Exactness: 2N-3.'

  do n = 1, 5
    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call chebyshev3_set ( n, x, w )
    if ( n == 1 ) then
      p_max = 2
    else
      p_max = 2 * n - 3 + 1
    end if
    call chebyshev1_exactness ( n, x, w, p_max )
    deallocate ( x )
    deallocate ( w )
  end do

  return
end
subroutine chebyshev3_set ( n, x, w )

!*****************************************************************************80
!
!! CHEBYSHEV3_SET sets a Chebyshev Type 3 quadrature rule.
!
!  Discussion:
!
!    The integral:
!
!      integral ( -1 <= x <= 1 ) f(x) / sqrt ( 1 - x * x ) dx
!
!    The quadrature rule:
!
!      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    N must be between 1 and 10.
!
!    Output, real ( kind = 8 ) X(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then
    x( 1) =    0.000000000000000D+00
    w( 1) =    3.141592653589793D+00
  else if ( n == 2 ) then    
    x( 1) =   -1.000000000000000D+00    
    x( 2) =    1.000000000000000D+00    
    w( 1) =    1.570796326794897D+00    
    w( 2) =    1.570796326794897D+00
  else if ( n == 3 ) then  
    x( 1) =   -1.000000000000000D+00    
    x( 2) =   0.0D+00
    x( 3) =    1.000000000000000D+00    
    w( 1) =   0.7853981633974483D+00    
    w( 2) =    1.570796326794897D+00    
    w( 3) =   0.7853981633974483D+00
  else if ( n == 4 ) then    
    x( 1) =   -1.000000000000000D+00    
    x( 2) =  -0.5000000000000000D+00    
    x( 3) =   0.5000000000000000D+00    
    x( 4) =    1.000000000000000D+00    
    w( 1) =   0.5235987755982988D+00    
    w( 2) =    1.047197551196598D+00    
    w( 3) =    1.047197551196598D+00    
    w( 4) =   0.5235987755982988D+00
  else if ( n == 5 ) then    
    x( 1) =   -1.000000000000000D+00    
    x( 2) =  -0.7071067811865475D+00    
    x( 3) =   0.0D+00
    x( 4) =   0.7071067811865476D+00    
    x( 5) =    1.000000000000000D+00    
    w( 1) =   0.3926990816987241D+00    
    w( 2) =   0.7853981633974483D+00    
    w( 3) =   0.7853981633974483D+00    
    w( 4) =   0.7853981633974483D+00    
    w( 5) =   0.3926990816987241D+00
  else if ( n == 6 ) then   
    x( 1) =   -1.000000000000000D+00    
    x( 2) =  -0.8090169943749473D+00    
    x( 3) =  -0.3090169943749473D+00    
    x( 4) =   0.3090169943749475D+00    
    x( 5) =   0.8090169943749475D+00    
    x( 6) =    1.000000000000000D+00    
    w( 1) =   0.3141592653589793D+00    
    w( 2) =   0.6283185307179586D+00    
    w( 3) =   0.6283185307179586D+00    
    w( 4) =   0.6283185307179586D+00    
    w( 5) =   0.6283185307179586D+00    
    w( 6) =   0.3141592653589793D+00
  else if ( n == 7 ) then    
    x( 1) =   -1.000000000000000D+00    
    x( 2) =  -0.8660254037844387D+00    
    x( 3) =  -0.5000000000000000D+00    
    x( 4) =   0.0D+00
    x( 5) =   0.5000000000000001D+00    
    x( 6) =   0.8660254037844387D+00    
    x( 7) =    1.000000000000000D+00    
    w( 1) =   0.2617993877991494D+00    
    w( 2) =   0.5235987755982988D+00    
    w( 3) =   0.5235987755982988D+00    
    w( 4) =   0.5235987755982988D+00    
    w( 5) =   0.5235987755982988D+00    
    w( 6) =   0.5235987755982988D+00    
    w( 7) =   0.2617993877991494D+00
  else if ( n == 8 ) then    
    x( 1) =   -1.000000000000000D+00    
    x( 2) =  -0.9009688679024190D+00    
    x( 3) =  -0.6234898018587335D+00    
    x( 4) =  -0.2225209339563143D+00    
    x( 5) =   0.2225209339563144D+00    
    x( 6) =   0.6234898018587336D+00    
    x( 7) =   0.9009688679024191D+00    
    x( 8) =    1.000000000000000D+00    
    w( 1) =   0.2243994752564138D+00    
    w( 2) =   0.4487989505128276D+00    
    w( 3) =   0.4487989505128276D+00    
    w( 4) =   0.4487989505128276D+00    
    w( 5) =   0.4487989505128276D+00    
    w( 6) =   0.4487989505128276D+00    
    w( 7) =   0.4487989505128276D+00    
    w( 8) =   0.2243994752564138D+00
  else if ( n == 9 ) then    
    x( 1) =   -1.000000000000000D+00    
    x( 2) =  -0.9238795325112867D+00    
    x( 3) =  -0.7071067811865475D+00    
    x( 4) =  -0.3826834323650897D+00    
    x( 5) =   0.0D+00
    x( 6) =   0.3826834323650898D+00    
    x( 7) =   0.7071067811865476D+00    
    x( 8) =   0.9238795325112867D+00    
    x( 9) =    1.000000000000000D+00    
    w( 1) =   0.1963495408493621D+00    
    w( 2) =   0.3926990816987241D+00    
    w( 3) =   0.3926990816987241D+00    
    w( 4) =   0.3926990816987241D+00    
    w( 5) =   0.3926990816987241D+00    
    w( 6) =   0.3926990816987241D+00    
    w( 7) =   0.3926990816987241D+00    
    w( 8) =   0.3926990816987241D+00    
    w( 9) =   0.1963495408493621D+00
  else if ( n == 10 ) then    
    x( 1) =   -1.000000000000000D+00    
    x( 2) =  -0.9396926207859083D+00    
    x( 3) =  -0.7660444431189779D+00    
    x( 4) =  -0.5000000000000000D+00    
    x( 5) =  -0.1736481776669303D+00    
    x( 6) =   0.1736481776669304D+00    
    x( 7) =   0.5000000000000001D+00    
    x( 8) =   0.7660444431189780D+00    
    x( 9) =   0.9396926207859084D+00    
    x(10) =    1.000000000000000D+00    
    w( 1) =   0.1745329251994329D+00    
    w( 2) =   0.3490658503988659D+00    
    w( 3) =   0.3490658503988659D+00    
    w( 4) =   0.3490658503988659D+00    
    w( 5) =   0.3490658503988659D+00    
    w( 6) =   0.3490658503988659D+00    
    w( 7) =   0.3490658503988659D+00 
    w( 8) =   0.3490658503988659D+00    
    w( 9) =   0.3490658503988659D+00    
    w(10) =   0.1745329251994329D+00    
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHEBYSHEV3_SET - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal value of N = ', n
    write ( *, '(a)' ) '  Legal values are 1 through 10.'
    stop 1
  end if

  return
end
subroutine clenshaw_curtis_exactness_test ( )

!*****************************************************************************80
!
!! CLENSHAW_CURTIS_EXACTNESS_TEST tests Clenshaw-Curtis rules for the Legendre integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CLENSHAW_CURTIS_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Clenshaw-Curtis rules for the Legendre integral.'
  write ( *, '(a)' ) '  Density function rho(x) = 1.'
  write ( *, '(a)' ) '  Region: -1 <= x <= +1.'
  write ( *, '(a)' ) '  Exactness: N   for N odd,'
  write ( *, '(a)' ) '             N-1 for N even.'

  do n = 1, 5
    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call clenshaw_curtis_set ( n, x, w )
    if ( mod ( n, 2 ) == 1 ) then
      p_max = n + 1
    else
      p_max = n
    end if
    call legendre_exactness ( n, x, w, p_max )
    deallocate ( x )
    deallocate ( w )
  end do

  return
end
subroutine clenshaw_curtis_set ( n, x, w )

!*****************************************************************************80
!
!! CLENSHAW_CURTIS_SET sets a Clenshaw-Curtis quadrature rule.
!
!  Discussion:
!
!    The integral:
!
!      integral ( -1 <= x <= 1 ) f(x) dx
!
!    The quadrature rule:
!
!      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
!
!    The abscissas for the rule of order N can be regarded
!    as the cosines of equally spaced angles between 180 and 0 degrees:
!
!      X(I) = cos ( ( I - 1 ) * PI / ( N - 1 ) )
!
!    except for the basic case N = 1, when
!
!      X(1) = 0.
!
!    A Clenshaw-Curtis rule that uses N points will integrate
!    exactly all polynomials of degrees 0 through N-1.  If N
!    is odd, then by symmetry the polynomial of degree N will
!    also be integrated exactly.
!
!    If the value of N is increased in a sensible way, then
!    the new set of abscissas will include the old ones.  One such
!    sequence would be N(K) = 2*K+1 for K = 0, 1, 2, &
!    Thus, in the table below, the abscissas for order 9 include
!    those for order 5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Charles Clenshaw, Alan Curtis,
!    A Method for Numerical Integration on an Automatic Computer,
!    Numerische Mathematik,
!    Volume 2, Number 1, December 1960, pages 197-205.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    N must be between 1 and 17, 33, 65 or 129.
!
!    Output, real ( kind = 8 ) X(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) =  0.00000000000000000000D+00
    w(1) =  2.00000000000000000000D+00

  else if ( n == 2 ) then

    x(1) = -1.00000000000000000000D+00
    x(2) =  1.00000000000000000000D+00

    w(1) =  1.00000000000000000000D+00
    w(2) =  1.00000000000000000000D+00

  else if ( n == 3 ) then

    x(1) = -1.00000000000000000000D+00
    x(2) =  0.00000000000000000000D+00
    x(3) =  1.00000000000000000000D+00

    w(1) =  0.33333333333333333333D+00
    w(2) =  1.33333333333333333333D+00
    w(3) =  0.33333333333333333333D+00

  else if ( n == 4 ) then

    x(1) = -1.00000000000000000000D+00
    x(2) = -0.50000000000000000000D+00
    x(3) =  0.50000000000000000000D+00
    x(4) =  1.00000000000000000000D+00

    w(1) =  0.11111111111111111111D+00
    w(2) =  0.88888888888888888889D+00
    w(3) =  0.88888888888888888889D+00
    w(4) =  0.11111111111111111111D+00

  else if ( n == 5 ) then

    x(1) = -1.00000000000000000000D+00
    x(2) = -0.70710678118654752440D+00
    x(3) =  0.00000000000000000000D+00
    x(4) =  0.70710678118654752440D+00
    x(5) =  1.00000000000000000000D+00

    w(1) =  0.06666666666666666667D+00
    w(2) =  0.53333333333333333333D+00
    w(3) =  0.80000000000000000000D+00
    w(4) =  0.53333333333333333333D+00
    w(5) =  0.06666666666666666667D+00

  else if ( n == 6 ) then

    x(1) = -1.00000000000000000000D+00
    x(2) = -0.80901699437494742410D+00
    x(3) = -0.30901699437494742410D+00
    x(4) =  0.30901699437494742410D+00
    x(5) =  0.80901699437493732410D+00
    x(6) =  1.00000000000000000000D+00

    w(1) =  0.04000000000000000000D+00
    w(2) =  0.36074304120001121619D+00
    w(3) =  0.59925695879998878381D+00
    w(4) =  0.59925695879998878381D+00
    w(5) =  0.36074304120001121619D+00
    w(6) =  0.04000000000000000000D+00

  else if ( n == 7 ) then

    x(1) = -1.00000000000000000000D+00
    x(2) = -0.86602540378443864676D+00
    x(3) = -0.50000000000000000000D+00
    x(4) =  0.00000000000000000000D+00
    x(5) =  0.50000000000000000000D+00
    x(6) =  0.86602540378443864676D+00
    x(7) =  1.00000000000000000000D+00

    w(1) =  0.02857142857142857143D+00
    w(2) =  0.25396825396825396825D+00
    w(3) =  0.45714285714285714286D+00
    w(4) =  0.52063492063492063492D+00
    w(5) =  0.45714285714285714286D+00
    w(6) =  0.25396825396825396825D+00
    w(7) =  0.02857142857142857143D+00

  else if ( n == 8 ) then

    x(1) = -1.00000000000000000000D+00
    x(2) = -0.90096886790241912624D+00
    x(3) = -0.62348980185873353053D+00
    x(4) = -0.22252093395631440429D+00
    x(5) =  0.22252093395631440429D+00
    x(6) =  0.62348980185873353053D+00
    x(7) =  0.90096886790241910624D+00
    x(8) =  1.00000000000000000000D+00

    w(1) =  0.02040816326530612245D+00
    w(2) =  0.19014100721820835178D+00
    w(3) =  0.35224242371815911533D+00
    w(4) =  0.43720840579832641044D+00
    w(5) =  0.43720840579832641044D+00
    w(6) =  0.35224242371815911533D+00
    w(7) =  0.19014100721820835178D+00
    w(8) =  0.02040816326530612245D+00

  else if ( n == 9 ) then

    x(1) = -1.00000000000000000000D+00
    x(2) = -0.92387953251128675613D+00
    x(3) = -0.70710678118654752440D+00
    x(4) = -0.38268343236508977173D+00
    x(5) =  0.00000000000000000000D+00
    x(6) =  0.38268343236508977173D+00
    x(7) =  0.70710678118654752440D+00
    x(8) =  0.92387953251128675613D+00
    x(9) =  1.00000000000000000000D+00

    w(1) =  0.01587301587301587302D+00
    w(2) =  0.14621864921601815501D+00
    w(3) =  0.27936507936507936508D+00
    w(4) =  0.36171785872048978150D+00
    w(5) =  0.39365079365079365079D+00
    w(6) =  0.36171785872048978150D+00
    w(7) =  0.27936507936507936508D+00
    w(8) =  0.14621864921601815501D+00
    w(9) =  0.01587301587301587302D+00

  else if ( n == 10 ) then

    x(1)  = -1.00000000000000000000D+00
    x(2)  = -0.93969262078590838405D+00
    x(3)  = -0.76604444311897903520D+00
    x(4)  = -0.50000000000000000000D+00
    x(5)  = -0.17364817766693034885D+00
    x(6)  =  0.17364817766693034885D+00
    x(7)  =  0.50000000000000000000D+00
    x(8)  =  0.76604444311897903520D+00
    x(9)  =  0.93969262078590838405D+00
    x(10) =  1.00000000000000000000D+00

    w(1)  =  0.01234567901234567901D+00
    w(2)  =  0.11656745657203712296D+00
    w(3)  =  0.22528432333810440813D+00
    w(4)  =  0.30194003527336860670D+00
    w(5)  =  0.34386250580414418320D+00
    w(6)  =  0.34386250580414418320D+00
    w(7)  =  0.30194003527336860670D+00
    w(8)  =  0.22528432333810440813D+00
    w(9)  =  0.11656745657203712296D+00
    w(10) =  0.01234567901234567901D+00

  else

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CLENSHAW_CURTIS - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of N.'
    stop 1

  end if

  return
end
subroutine fejer1_exactness_test ( )

!*****************************************************************************80
!
!! FEJER1_EXACTNESS_TEST tests Fejer Type 1 rules for the Legendre integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FEJER1_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Fejer Type 1 rules for the Legendre integral.'
  write ( *, '(a)' ) '  Density function rho(x) = 1.'
  write ( *, '(a)' ) '  Region: -1 <= x <= +1.'
  write ( *, '(a)' ) '  Exactness: N   for N odd,'
  write ( *, '(a)' ) '             N-1 for N even.'

  do n = 1, 5
    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call fejer1_set ( n, x, w )
    if ( mod ( n, 2 ) == 1 ) then
      p_max = n + 1
    else
      p_max = n
    end if
    call legendre_exactness ( n, x, w, p_max )
    deallocate ( x )
    deallocate ( w )
  end do

  return
end
subroutine fejer1_set ( n, x, w )

!*****************************************************************************80
!
!! FEJER1_SET sets abscissas and weights for Fejer type 1 quadrature.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!    Walter Gautschi,
!    Numerical Quadrature in the Presence of a Singularity,
!    SIAM Journal on Numerical Analysis,
!    Volume 4, Number 3, 1967, pages 357-362.
!
!    Joerg Waldvogel,
!    Fast Construction of the Fejer and Clenshaw-Curtis Quadrature Rules,
!    BIT Numerical Mathematics,
!    Volume 43, Number 1, 2003, pages 1-18.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    N should be between 1 and 10.
!
!    Output, real ( kind = 8 ) X(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1)   =  0.000000000000000D+00
    w(1) =  2.000000000000000D+00

  else if ( n == 2 ) then

    x(1) =   -0.7071067811865475D+00
    x(2) =    0.7071067811865475D+00

    w(1) =  1.000000000000000D+00
    w(2) =  1.000000000000000D+00

  else if ( n == 3 ) then

    x( 1) =  -0.8660254037844387D+00
    x( 2) =   0.0000000000000000D+00
    x( 3) =   0.8660254037844387D+00

    w(1) =  0.4444444444444444D+00
    w(2) =  1.1111111111111111D+00
    w(3) =  0.4444444444444444D+00

  else if ( n == 4 ) then

    x( 1) =  -0.9238795325112867D+00
    x( 2) =  -0.3826834323650897D+00
    x( 3) =   0.3826834323650898D+00
    x( 4) =   0.9238795325112867D+00

    w( 1) = 0.2642977396044841D+00
    w( 2) = 0.7357022603955158D+00
    w( 3) = 0.7357022603955158D+00
    w( 4) = 0.2642977396044841D+00

  else if ( n == 5 ) then

    x( 1) =  -0.9510565162951535D+00
    x( 2) =  -0.5877852522924730D+00
    x( 3) =   0.0000000000000000D+00
    x( 4) =   0.5877852522924731D+00
    x( 5) =   0.9510565162951535D+00

    w( 1) = 0.1677812284666835D+00
    w( 2) = 0.5255521048666498D+00
    w( 3) = 0.6133333333333333D+00
    w( 4) = 0.5255521048666498D+00
    w( 5) = 0.1677812284666835D+00

  else if ( n == 6 ) then

    x( 1) =  -0.9659258262890682D+00
    x( 2) =  -0.7071067811865475D+00
    x( 3) =  -0.2588190451025206D+00
    x( 4) =   0.2588190451025207D+00
    x( 5) =   0.7071067811865476D+00
    x( 6) =   0.9659258262890683D+00

    w( 1) = 0.1186610213812358D+00
    w( 2) = 0.3777777777777778D+00
    w( 3) = 0.5035612008409863D+00
    w( 4) = 0.5035612008409863D+00
    w( 5) = 0.3777777777777778D+00
    w( 6) = 0.1186610213812358D+00

  else if ( n == 7 ) then

    x( 1) =  -0.9749279121818237D+00
    x( 2) =  -0.7818314824680295D+00
    x( 3) =  -0.4338837391175581D+00
    x( 4) =   0.0000000000000000D+00
    x( 5) =   0.4338837391175582D+00
    x( 6) =   0.7818314824680298D+00
    x( 7) =   0.9749279121818236D+00

    w( 1) = 0.08671618072672234D+00
    w( 2) = 0.2878313947886919D+00
    w( 3) = 0.3982415401308441D+00
    w( 4) = 0.4544217687074830D+00
    w( 5) = 0.3982415401308441D+00
    w( 6) = 0.2878313947886919D+00
    w( 7) = 0.08671618072672234D+00

  else if ( n == 8 ) then

    x( 1) =  -0.9807852804032304D+00
    x( 2) =  -0.8314696123025453D+00
    x( 3) =  -0.5555702330196020D+00
    x( 4) =  -0.1950903220161282D+00
    x( 5) =   0.1950903220161283D+00
    x( 6) =   0.5555702330196023D+00
    x( 7) =   0.8314696123025452D+00
    x( 8) =   0.9807852804032304D+00

    w( 1) = 0.06698294569858981D+00
    w( 2) = 0.2229879330145788D+00
    w( 3) = 0.3241525190645244D+00
    w( 4) = 0.3858766022223071D+00
    w( 5) = 0.3858766022223071D+00
    w( 6) = 0.3241525190645244D+00
    w( 7) = 0.2229879330145788D+00
    w( 8) = 0.06698294569858981D+00

  else if ( n == 9 ) then

    x( 1) =  -0.9848077530122080D+00
    x( 2) =  -0.8660254037844385D+00
    x( 3) =  -0.6427876096865394D+00
    x( 4) =  -0.3420201433256685D+00
    x( 5) =   0.0000000000000000D+00
    x( 6) =   0.3420201433256688D+00
    x( 7) =   0.6427876096865394D+00
    x( 8) =   0.8660254037844387D+00
    x( 9) =   0.9848077530122080D+00

    w( 1) = 0.05273664990990676D+00
    w( 2) = 0.1791887125220458D+00
    w( 3) = 0.2640372225410044D+00
    w( 4) = 0.3308451751681364D+00
    w( 5) = 0.3463844797178130D+00
    w( 6) = 0.3308451751681364D+00
    w( 7) = 0.2640372225410044D+00
    w( 8) = 0.1791887125220458D+00
    w( 9) = 0.05273664990990676D+00

  else if ( n == 10 ) then

    x( 1) =  -0.9876883405951377D+00
    x( 2) =  -0.8910065241883678D+00
    x( 3) =  -0.7071067811865475D+00
    x( 4) =  -0.4539904997395467D+00
    x( 5) =  -0.1564344650402306D+00
    x( 6) =   0.1564344650402309D+00
    x( 7) =   0.4539904997395468D+00
    x( 8) =   0.7071067811865476D+00
    x( 9) =   0.8910065241883679D+00
    x(10) =   0.9876883405951378D+00

    w( 1) = 0.04293911957413078D+00
    w( 2) = 0.1458749193773909D+00
    w( 3) = 0.2203174603174603D+00
    w( 4) = 0.2808792186638755D+00
    w( 5) = 0.3099892820671425D+00
    w( 6) = 0.3099892820671425D+00
    w( 7) = 0.2808792186638755D+00
    w( 8) = 0.2203174603174603D+00
    w( 9) = 0.1458749193773909D+00
    w(10) = 0.04293911957413078D+00

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FEJER1_SET - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal value of N = ', n
    write ( *, '(a)' ) '  Legal values are 1 through 10.'
    stop 1

  end if

  return
end
subroutine fejer2_exactness_test ( )

!*****************************************************************************80
!
!! FEJER2_EXACTNESS_TEST tests Fejer Type 2 rules for the Legendre integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FEJER2_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Fejer Type 2 rules for the Legendre integral.'
  write ( *, '(a)' ) '  Density function rho(x) = 1.'
  write ( *, '(a)' ) '  Region: -1 <= x <= +1.'
  write ( *, '(a)' ) '  Exactness: N   for N odd,'
  write ( *, '(a)' ) '             N-1 for N even.'

  do n = 1, 5
    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call fejer2_set ( n, x, w )
    if ( mod ( n, 2 ) == 1 ) then
      p_max = n + 1
    else
      p_max = n
    end if
    call legendre_exactness ( n, x, w, p_max )
    deallocate ( x )
    deallocate ( w )
  end do

  return
end
subroutine fejer2_set ( n, x, w )

!*****************************************************************************80
!
!! FEJER2_SET sets abscissas and weights for Fejer type 2 quadrature.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!    Walter Gautschi,
!    Numerical Quadrature in the Presence of a Singularity,
!    SIAM Journal on Numerical Analysis,
!    Volume 4, Number 3, 1967, pages 357-362.
!
!    Joerg Waldvogel,
!    Fast Construction of the Fejer and Clenshaw-Curtis Quadrature Rules,
!    BIT Numerical Mathematics,
!    Volume 43, Number 1, 2003, pages 1-18.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    N should be between 1 and 10.
!
!    Output, real ( kind = 8 ) X(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1)   =  0.000000000000000D+00
    w(1) =  2.000000000000000D+00

  else if ( n == 2 ) then

    x(1) =   -0.5000000000000000D+00
    x(2) =    0.5000000000000000D+00

    w(1) =  1.0000000000000000D+00
    w(2) =  1.0000000000000000D+00

  else if ( n == 3 ) then

    x( 1) =  -0.7071067811865476D+00
    x( 2) =   0.0000000000000000D+00
    x( 3) =   0.7071067811865476D+00

    w(1) =  0.6666666666666666D+00
    w(2) =  0.6666666666666666D+00
    w(3) =  0.6666666666666666D+00

  else if ( n == 4 ) then

    x( 1) =  -0.8090169943749475D+00
    x( 2) =  -0.3090169943749475D+00
    x( 3) =   0.3090169943749475D+00
    x( 4) =   0.8090169943749475D+00

    w( 1) = 0.4254644007500070D+00
    w( 2) = 0.5745355992499930D+00
    w( 3) = 0.5745355992499930D+00
    w( 4) = 0.4254644007500070D+00

  else if ( n == 5 ) then

    x( 1) =  -0.8660254037844387D+00
    x( 2) =  -0.5000000000000000D+00
    x( 3) =   0.0000000000000000D+00
    x( 4) =   0.5000000000000000D+00
    x( 5) =   0.8660254037844387D+00

    w( 1) = 0.3111111111111111D+00
    w( 2) = 0.4000000000000000D+00
    w( 3) = 0.5777777777777777D+00
    w( 4) = 0.4000000000000000D+00
    w( 5) = 0.3111111111111111D+00

  else if ( n == 6 ) then

    x( 1) =  -0.9009688679024191D+00
    x( 2) =  -0.6234898018587336D+00
    x( 3) =  -0.2225209339563144D+00
    x( 4) =   0.2225209339563144D+00
    x( 5) =   0.6234898018587336D+00
    x( 6) =   0.9009688679024191D+00

    w( 1) = 0.2269152467244296D+00
    w( 2) = 0.3267938603769863D+00
    w( 3) = 0.4462908928985841D+00
    w( 4) = 0.4462908928985841D+00
    w( 5) = 0.3267938603769863D+00
    w( 6) = 0.2269152467244296D+00

  else if ( n == 7 ) then

    x( 1) =  -0.9238795325112867D+00
    x( 2) =  -0.7071067811865476D+00
    x( 3) =  -0.3826834323650898D+00
    x( 4) =   0.0000000000000000D+00
    x( 5) =   0.3826834323650898D+00
    x( 6) =   0.7071067811865476D+00
    x( 7) =   0.9238795325112867D+00

    w( 1) = 0.1779646809620499D+00
    w( 2) = 0.2476190476190476D+00
    w( 3) = 0.3934638904665215D+00
    w( 4) = 0.3619047619047619D+00
    w( 5) = 0.3934638904665215D+00
    w( 6) = 0.2476190476190476D+00
    w( 7) = 0.1779646809620499D+00

  else if ( n == 8 ) then

    x( 1) =  -0.9396926207859084D+00
    x( 2) =  -0.7660444431189780D+00
    x( 3) =  -0.5000000000000000D+00
    x( 4) =  -0.1736481776669304D+00
    x( 5) =   0.1736481776669304D+00
    x( 6) =   0.5000000000000000D+00
    x( 7) =   0.7660444431189780D+00
    x( 8) =   0.9396926207859084D+00

    w( 1) = 0.1397697435050225D+00
    w( 2) = 0.2063696457302284D+00
    w( 3) = 0.3142857142857143D+00
    w( 4) = 0.3395748964790348D+00
    w( 5) = 0.3395748964790348D+00
    w( 6) = 0.3142857142857143D+00
    w( 7) = 0.2063696457302284D+00
    w( 8) = 0.1397697435050225D+00

  else if ( n == 9 ) then

    x( 1) =  -0.9510565162951535D+00
    x( 2) =  -0.8090169943749475D+00
    x( 3) =  -0.5877852522924731D+00
    x( 4) =  -0.3090169943749475D+00
    x( 5) =   0.0000000000000000D+00
    x( 6) =   0.3090169943749475D+00
    x( 7) =   0.5877852522924731D+00
    x( 8) =   0.8090169943749475D+00
    x( 9) =   0.9510565162951535D+00

    w( 1) = 0.1147810750857217D+00
    w( 2) = 0.1654331942222276D+00
    w( 3) = 0.2737903534857068D+00
    w( 4) = 0.2790112502222170D+00
    w( 5) = 0.3339682539682539D+00
    w( 6) = 0.2790112502222170D+00
    w( 7) = 0.2737903534857068D+00
    w( 8) = 0.1654331942222276D+00
    w( 9) = 0.1147810750857217D+00

  else if ( n == 10 ) then

    x( 1) =  -0.9594929736144974D+00
    x( 2) =  -0.8412535328311812D+00
    x( 3) =  -0.6548607339452851D+00
    x( 4) =  -0.4154150130018864D+00
    x( 5) =  -0.1423148382732851D+00
    x( 6) =   0.1423148382732851D+00
    x( 7) =   0.4154150130018864D+00
    x( 8) =   0.6548607339452851D+00
    x( 9) =   0.8412535328311812D+00
    x(10) =   0.9594929736144974D+00

    w( 1) = 0.09441954173982806D+00
    w( 2) = 0.1411354380109716D+00
    w( 3) = 0.2263866903636005D+00
    w( 4) = 0.2530509772156453D+00
    w( 5) = 0.2850073526699544D+00
    w( 6) = 0.2850073526699544D+00
    w( 7) = 0.2530509772156453D+00
    w( 8) = 0.2263866903636005D+00
    w( 9) = 0.1411354380109716D+00
    w(10) = 0.09441954173982806D+00

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FEJER2_SET - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal value of N = ', n
    write ( *, '(a)' ) '  Legal values are 1 through 10.'
    stop 1

  end if

  return
end
subroutine gegenbauer_exactness_test ( )

!*****************************************************************************80
!
!! GEGENBAUER_EXACTNESS_TEST tests Gauss-Gegenbauer rules for the Gegenbauer integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) lambda
  integer ( kind = 4 ) n
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  lambda = 1.75D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GEGENBAUER_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Gauss-Gegenbauer rules for the Gegenbauer integral.'
  write ( *, '(a)' ) '  Density function rho(x) = (1-x^2)^(lambda-1/2).'
  write ( *, '(a,g14.6)' ) '  Lambda = ', lambda
  write ( *, '(a)' ) '  Region: -1 <= x <= +1.'
  write ( *, '(a)' ) '  Exactness: 2*N-1.'

  do n = 1, 5

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    if ( n == 1 ) then
      x = (/ &
        0.0000000000000000D+00 /)
      w = (/ &
        1.2485988353771993D+00 /)
    else if ( n == 2 ) then
      x = (/ &
       -0.4264014327112208D+00, &
        0.4264014327112208D+00 /)
      w = (/ &
        0.6242994176885995D+00, &
        0.6242994176885995D+00 /)
    else if ( n == 3 ) then
      x = (/ &
       -0.6324555320336757D+00, &
        0.0000000000000000D+00, &
        0.6324555320336757D+00 /)
      w = (/ &
        0.2837724625857273D+00, &
        0.6810539102057455D+00, &
        0.2837724625857273D+00 /)
    else if ( n == 4 ) then
      x = (/ &
       -0.7455376618816977D+00, &
       -0.2752317970082527D+00, &
        0.2752317970082527D+00, &
        0.7455376618816980D+00 /)
      w = (/ &
        0.1379302690657785D+00, &
        0.4863691486228214D+00, &
        0.4863691486228208D+00, &
        0.1379302690657786D+00 /)
    else if ( n == 5 ) then
      x = (/ &
       -0.8137803260309515D+00, &
       -0.4553315257658559D+00, &
        0.0000000000000001D+00, &
        0.4553315257658557D+00, &
        0.8137803260309517D+00 /)
      w = (/ &
        0.0725955752894624D+00, &
        0.3156051535278124D+00, &
        0.4721973777426502D+00, &
        0.3156051535278118D+00, &
        0.0725955752894624D+00 /)
    end if
    p_max = 2 * n
    call gegenbauer_exactness ( n, x, w, p_max, lambda )
    deallocate ( x )
    deallocate ( w )
  end do

  return
end
subroutine hermite_exactness_test ( )

!*****************************************************************************80
!
!! HERMITE_EXACTNESS_TEST tests Gauss-Hermite rules for the Hermite integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HERMITE_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Gauss-Hermite rules for the Hermite integral.'
  write ( *, '(a)' ) '  Density function rho(x) = exp(-x^2).'
  write ( *, '(a)' ) '  Region: -oo < x < +oo.'
  write ( *, '(a)' ) '  Exactness: 2N-1.'

  do n = 1, 5
    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call hermite_set ( n, x, w )
    p_max = 2 * n
    call hermite_exactness ( n, x, w, p_max )
    deallocate ( x )
    deallocate ( w )
  end do

  return
end
subroutine hermite_set ( n, x, w )

!*****************************************************************************80
!
!! HERMITE_SET sets abscissas and weights for Hermite quadrature.
!
!  Discussion:
!
!    The integral:
!
!      integral ( -oo < x < +oo ) f(x) * rho(x) dx
!
!    The weight function:
!
!      rho(x) = exp ( - x * x )
!
!    The quadrature rule:
!
!      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
!
!    Mathematica can numerically estimate the abscissas of the rule
!    of order N to P digits by the command:
!
!      NSolve (/ HermiteH (/ n, x ] == 0, x, p ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Vladimir Krylov,
!    Approximate Calculation of Integrals,
!    Dover, 2006,
!    ISBN: 0486445798,
!    LC: QA311.K713.
!
!    Arthur Stroud, Don Secrest,
!    Gaussian Quadrature Formulas,
!    Prentice Hall, 1966,
!    LC: QA299.4G3S7.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996,
!    ISBN: 0-8493-2479-3,
!    LC: QA47.M315.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    N must be between 1 and 20, 31/32/33, 63/64/65, 127/128/129.
!
!    Output, real ( kind = 8 ) X(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = 0.0D+00

    w(1) = 1.77245385090551602729816748334D+00

  else if ( n == 2 ) then

    x(1) = - 0.707106781186547524400844362105D+00
    x(2) =   0.707106781186547524400844362105D+00

    w(1) = 0.886226925452758013649083741671D+00
    w(2) = 0.886226925452758013649083741671D+00

  else if ( n == 3 ) then

    x(1) = - 0.122474487139158904909864203735D+01
    x(2) =   0.0D+00
    x(3) =   0.122474487139158904909864203735D+01

    w(1) = 0.295408975150919337883027913890D+00
    w(2) = 0.118163590060367735153211165556D+01
    w(3) = 0.295408975150919337883027913890D+00

  else if ( n == 4 ) then

    x(1) = - 0.165068012388578455588334111112D+01
    x(2) = - 0.524647623275290317884060253835D+00
    x(3) =   0.524647623275290317884060253835D+00
    x(4) =   0.165068012388578455588334111112D+01

    w(1) = 0.813128354472451771430345571899D-01
    w(2) = 0.804914090005512836506049184481D+00
    w(3) = 0.804914090005512836506049184481D+00
    w(4) = 0.813128354472451771430345571899D-01

  else if ( n == 5 ) then

    x(1) = - 0.202018287045608563292872408814D+01
    x(2) = - 0.958572464613818507112770593893D+00
    x(3) =   0.0D+00
    x(4) =   0.958572464613818507112770593893D+00
    x(5) =   0.202018287045608563292872408814D+01

    w(1) = 0.199532420590459132077434585942D-01
    w(2) = 0.393619323152241159828495620852D+00
    w(3) = 0.945308720482941881225689324449D+00
    w(4) = 0.393619323152241159828495620852D+00
    w(5) = 0.199532420590459132077434585942D-01

  else if ( n == 6 ) then

    x(1) = - 0.235060497367449222283392198706D+01
    x(2) = - 0.133584907401369694971489528297D+01
    x(3) = - 0.436077411927616508679215948251D+00
    x(4) =   0.436077411927616508679215948251D+00
    x(5) =   0.133584907401369694971489528297D+01
    x(6) =   0.235060497367449222283392198706D+01

    w(1) = 0.453000990550884564085747256463D-02
    w(2) = 0.157067320322856643916311563508D+00
    w(3) = 0.724629595224392524091914705598D+00
    w(4) = 0.724629595224392524091914705598D+00
    w(5) = 0.157067320322856643916311563508D+00
    w(6) = 0.453000990550884564085747256463D-02

  else if ( n == 7 ) then

    x(1) = - 0.265196135683523349244708200652D+01
    x(2) = - 0.167355162876747144503180139830D+01
    x(3) = - 0.816287882858964663038710959027D+00
    x(4) =   0.0D+00
    x(5) =   0.816287882858964663038710959027D+00
    x(6) =   0.167355162876747144503180139830D+01
    x(7) =   0.265196135683523349244708200652D+01

    w(1) = 0.971781245099519154149424255939D-03
    w(2) = 0.545155828191270305921785688417D-01
    w(3) = 0.425607252610127800520317466666D+00
    w(4) = 0.810264617556807326764876563813D+00
    w(5) = 0.425607252610127800520317466666D+00
    w(6) = 0.545155828191270305921785688417D-01
    w(7) = 0.971781245099519154149424255939D-03

  else if ( n == 8 ) then

    x(1) = - 0.293063742025724401922350270524D+01
    x(2) = - 0.198165675669584292585463063977D+01
    x(3) = - 0.115719371244678019472076577906D+01
    x(4) = - 0.381186990207322116854718885584D+00
    x(5) =   0.381186990207322116854718885584D+00
    x(6) =   0.115719371244678019472076577906D+01
    x(7) =   0.198165675669584292585463063977D+01
    x(8) =   0.293063742025724401922350270524D+01

    w(1) = 0.199604072211367619206090452544D-03
    w(2) = 0.170779830074134754562030564364D-01
    w(3) = 0.207802325814891879543258620286D+00
    w(4) = 0.661147012558241291030415974496D+00
    w(5) = 0.661147012558241291030415974496D+00
    w(6) = 0.207802325814891879543258620286D+00
    w(7) = 0.170779830074134754562030564364D-01
    w(8) = 0.199604072211367619206090452544D-03

  else if ( n == 9 ) then

    x(1) = - 0.319099320178152760723004779538D+01
    x(2) = - 0.226658058453184311180209693284D+01
    x(3) = - 0.146855328921666793166701573925D+01
    x(4) = - 0.723551018752837573322639864579D+00
    x(5) =   0.0D+00
    x(6) =   0.723551018752837573322639864579D+00
    x(7) =   0.146855328921666793166701573925D+01
    x(8) =   0.226658058453184311180209693284D+01
    x(9) =   0.319099320178152760723004779538D+01

    w(1) = 0.396069772632643819045862946425D-04
    w(2) = 0.494362427553694721722456597763D-02
    w(3) = 0.884745273943765732879751147476D-01
    w(4) = 0.432651559002555750199812112956D+00
    w(5) = 0.720235215606050957124334723389D+00
    w(6) = 0.432651559002555750199812112956D+00
    w(7) = 0.884745273943765732879751147476D-01
    w(8) = 0.494362427553694721722456597763D-02
    w(9) = 0.396069772632643819045862946425D-04

  else if ( n == 10 ) then

    x(1) =  - 0.343615911883773760332672549432D+01
    x(2) =  - 0.253273167423278979640896079775D+01
    x(3) =  - 0.175668364929988177345140122011D+01
    x(4) =  - 0.103661082978951365417749191676D+01
    x(5) =  - 0.342901327223704608789165025557D+00
    x(6) =    0.342901327223704608789165025557D+00
    x(7) =    0.103661082978951365417749191676D+01
    x(8) =    0.175668364929988177345140122011D+01
    x(9) =    0.253273167423278979640896079775D+01
    x(10) =   0.343615911883773760332672549432D+01

    w(1) =  0.764043285523262062915936785960D-05
    w(2) =  0.134364574678123269220156558585D-02
    w(3) =  0.338743944554810631361647312776D-01
    w(4) =  0.240138611082314686416523295006D+00
    w(5) =  0.610862633735325798783564990433D+00
    w(6) =  0.610862633735325798783564990433D+00
    w(7) =  0.240138611082314686416523295006D+00
    w(8) =  0.338743944554810631361647312776D-01
    w(9) =  0.134364574678123269220156558585D-02
    w(10) = 0.764043285523262062915936785960D-05

  else

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'HERMITE_SET - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of N.'
    stop 1

  end if

  return
end
subroutine hermite_1_exactness_test ( )

!*****************************************************************************80
!
!! HERMITE_1_EXACTNESS_TEST tests Gauss-Hermite rules for the Hermite integral.
!
!  Discussion:
!
!    Instead of the usual density rho(x)=exp(-x*x), these rules apply to
!    approximating the integral:
!      I(f) = integral ( -oo < x < +oo ) f(x) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HERMITE_1_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Gauss-Hermite rules for the Hermite integral.'
  write ( *, '(a)' ) '  Density function rho(x) = 1.'
  write ( *, '(a)' ) '  Region: -oo < x < +oo.'
  write ( *, '(a)' ) '  Exactness: 2N-1.'

  do n = 1, 5
    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call hermite_1_set ( n, x, w )
!
!  Standardize the rule by multiplying every weight w(i) by exp(-x(i)^2).
!
    w(1:n) = exp ( - x(1:n) * x(1:n) ) * w(1:n)
!
!  Now test the rule in standard form.
!
    p_max = 2 * n
    call hermite_exactness ( n, x, w, p_max )
    deallocate ( x )
    deallocate ( w )
  end do

  return
end
subroutine hermite_1_set ( n, x, w )

!*****************************************************************************80
!
!! HERMITE_1_SET sets abscissas and weights for Hermite quadrature.
!
!  Discussion:
!
!    This routine is for the case with unit density:
!      integral ( -oo < x < +oo ) f(x) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    N must be between 1 and 10.
!
!    Output, real ( kind = 8 ) X(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = 0.0D+00

    w(1) = 1.7724538509055161D+00

  else if ( n == 2 ) then

    x(1) = - 0.707106781186547524400844362105D+00
    x(2) =   0.707106781186547524400844362105D+00

    w(1) = 1.4611411826611391D+00
    w(2) = 1.4611411826611391D+00

  else if ( n == 3 ) then

    x(1) = - 0.122474487139158904909864203735D+01
    x(2) =   0.0D+00
    x(3) =   0.122474487139158904909864203735D+01

    w(1) = 1.3239311752136438D+00 
    w(2) = 1.1816359006036774D+00
    w(3) = 1.3239311752136438D+00

  else if ( n == 4 ) then

    x(1) = - 0.165068012388578455588334111112D+01
    x(2) = - 0.524647623275290317884060253835D+00
    x(3) =   0.524647623275290317884060253835D+00
    x(4) =   0.165068012388578455588334111112D+01

    w(1) = 1.2402258176958150D+00
    w(2) = 1.0599644828949693D+00
    w(3) = 1.0599644828949693D+00
    w(4) = 1.2402258176958150D+00

  else if ( n == 5 ) then

    x(1) = - 0.202018287045608563292872408814D+01
    x(2) = - 0.958572464613818507112770593893D+00
    x(3) =   0.0D+00
    x(4) =   0.958572464613818507112770593893D+00
    x(5) =   0.202018287045608563292872408814D+01

    w(1) = 1.1814886255359869D+00
    w(2) = 0.98658099675142830D+00
    w(3) = 0.94530872048294190D+00
    w(4) = 0.98658099675142830D+00
    w(5) = 1.1814886255359869D+00

  else if ( n == 6 ) then

    x(1) = - 0.235060497367449222283392198706D+01
    x(2) = - 0.133584907401369694971489528297D+01
    x(3) = - 0.436077411927616508679215948251D+00
    x(4) =   0.436077411927616508679215948251D+00
    x(5) =   0.133584907401369694971489528297D+01
    x(6) =   0.235060497367449222283392198706D+01

    w(1) = 1.1369083326745253D+00
    w(2) = 0.93558055763118075D+00
    w(3) = 0.87640133443623058D+00
    w(4) = 0.87640133443623058D+00
    w(5) = 0.93558055763118075D+00
    w(6) = 1.1369083326745253D+00

  else if ( n == 7 ) then

    x(1) = - 0.265196135683523349244708200652D+01
    x(2) = - 0.167355162876747144503180139830D+01
    x(3) = - 0.816287882858964663038710959027D+00
    x(4) =   0.0D+00
    x(5) =   0.816287882858964663038710959027D+00
    x(6) =   0.167355162876747144503180139830D+01
    x(7) =   0.265196135683523349244708200652D+01

    w(1) = 1.1013307296103216D+00
    w(2) = 0.89718460022518409D+00
    w(3) = 0.82868730328363926D+00
    w(4) = 0.81026461755680734D+00
    w(5) = 0.82868730328363926D+00
    w(6) = 0.89718460022518409D+00
    w(7) = 1.1013307296103216D+00

  else if ( n == 8 ) then

    x(1) = - 0.293063742025724401922350270524D+01
    x(2) = - 0.198165675669584292585463063977D+01
    x(3) = - 0.115719371244678019472076577906D+01
    x(4) = - 0.381186990207322116854718885584D+00
    x(5) =   0.381186990207322116854718885584D+00
    x(6) =   0.115719371244678019472076577906D+01
    x(7) =   0.198165675669584292585463063977D+01
    x(8) =   0.293063742025724401922350270524D+01

    w(1) = 1.0719301442479805D+00
    w(2) = 0.86675260656338138D+00
    w(3) = 0.79289004838640131D+00
    w(4) = 0.76454412865172916D+00
    w(5) = 0.76454412865172916D+00
    w(6) = 0.79289004838640131D+00
    w(7) = 0.86675260656338138D+00
    w(8) = 1.0719301442479805D+00

  else if ( n == 9 ) then

    x(1) = - 0.319099320178152760723004779538D+01
    x(2) = - 0.226658058453184311180209693284D+01
    x(3) = - 0.146855328921666793166701573925D+01
    x(4) = - 0.723551018752837573322639864579D+00
    x(5) =   0.0D+00
    x(6) =   0.723551018752837573322639864579D+00
    x(7) =   0.146855328921666793166701573925D+01
    x(8) =   0.226658058453184311180209693284D+01
    x(9) =   0.319099320178152760723004779538D+01

    w(1) = 1.0470035809766838D+00
    w(2) = 0.84175270147867043D+00
    w(3) = 0.76460812509455023D+00
    w(4) = 0.73030245274509220D+00
    w(5) = 0.72023521560605097D+00
    w(6) = 0.73030245274509220D+00
    w(7) = 0.76460812509455023D+00
    w(8) = 0.84175270147867043D+00
    w(9) = 1.0470035809766838D+00

  else if ( n == 10 ) then

    x(1) =  - 0.343615911883773760332672549432D+01
    x(2) =  - 0.253273167423278979640896079775D+01
    x(3) =  - 0.175668364929988177345140122011D+01
    x(4) =  - 0.103661082978951365417749191676D+01
    x(5) =  - 0.342901327223704608789165025557D+00
    x(6) =    0.342901327223704608789165025557D+00
    x(7) =    0.103661082978951365417749191676D+01
    x(8) =    0.175668364929988177345140122011D+01
    x(9) =    0.253273167423278979640896079775D+01
    x(10) =   0.343615911883773760332672549432D+01

    w(1) = 1.0254516913657352D+00
    w(2) = 0.82066612640481640D+00
    w(3) = 0.74144193194356511D+00
    w(4) = 0.70329632310490608D+00
    w(5) = 0.68708185395127341D+00
    w(6) = 0.68708185395127341D+00
    w(7) = 0.70329632310490608D+00
    w(8) = 0.74144193194356511D+00
    w(9) = 0.82066612640481640D+00
    w(10) = 1.0254516913657352D+00

  else

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'HERMITE_1_SET - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of N.'
    stop 1

  end if

  return
end
subroutine laguerre_exactness_test ( )

!*****************************************************************************80
!
!! LAGUERRE_EXACTNESS_TEST tests Gauss-Laguerre rules for the Laguerre integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LAGUERRE_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Gauss-Laguerre rules for the Laguerre integral.'
  write ( *, '(a)' ) '  Density function rho(x) = exp(-x).'
  write ( *, '(a)' ) '  Region: 0 <= x < +oo.'
  write ( *, '(a)' ) '  Exactness: 2N-1.'

  do n = 1, 5
    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call laguerre_set ( n, x, w )
    p_max = 2 * n
    call laguerre_exactness ( n, x, w, p_max )
    deallocate ( x )
    deallocate ( w )
  end do

  return
end
subroutine laguerre_set ( n, x, w )

!*****************************************************************************80
!
!! LAGUERRE_SET sets abscissas and weights for Laguerre quadrature.
!
!  Discussion:
!
!    The abscissas are the zeroes of the Laguerre polynomial L(N)(X).
!
!    The integral:
!
!      integral ( 0 <= x < +oo ) exp ( -x ) * f(x) dx
!
!    The quadrature rule:
!
!      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
!
!    The integral:
!
!      integral ( 0 <= x < +oo ) f(x) dx
!
!    The quadrature rule:
!
!      sum ( 1 <= i <= n ) w(i) * exp ( x(i) ) * f ( x(i) )
!
!    Mathematica can numerically estimate the abscissas for the
!    n-th order polynomial to p digits of precision by the command:
!
!      NSolve (/ LaguerreL(/n,x] == 0, x, p ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Vladimir Krylov,
!    Approximate Calculation of Integrals,
!    Dover, 2006,
!    ISBN: 0486445798,
!    LC: QA311.K713.
!
!    Arthur Stroud, Don Secrest,
!    Gaussian Quadrature Formulas,
!    Prentice Hall, 1966,
!    LC: QA299.4G3S7.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996,
!    ISBN: 0-8493-2479-3,
!    LC: QA47.M315.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    N must be between 1 and 10.
!
!    Output, real ( kind = 8 ) X(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) =  1.00000000000000000000000000000D+00

    w(1) =  1.00000000000000000000000000000D+00

  else if ( n == 2 ) then

    x(1) = 0.585786437626904951198311275790D+00
    x(2) = 3.41421356237309504880168872421D+00

    w(1) = 0.85355339059327376220042218105D+00
    w(2) = 0.146446609406726237799577818948D+00

  else if ( n == 3 ) then

    x(1) = 0.415774556783479083311533873128D+00
    x(2) = 2.29428036027904171982205036136D+00
    x(3) = 6.28994508293747919686641576551D+00

    w(1) = 0.71109300992917301544959019114D+00
    w(2) = 0.27851773356924084880144488846D+00
    w(3) = 0.010389256501586135748964920401D+00

  else if ( n == 4 ) then

    x(1) = 0.322547689619392311800361459104D+00
    x(2) = 1.74576110115834657568681671252D+00
    x(3) = 4.53662029692112798327928538496D+00
    x(4) = 9.39507091230113312923353644342D+00

    w(1) = 0.60315410434163360163596602382D+00
    w(2) = 0.35741869243779968664149201746D+00
    w(3) = 0.03888790851500538427243816816D+00
    w(4) = 0.0005392947055613274501037905676D+00

  else if ( n == 5 ) then

    x(1) = 0.263560319718140910203061943361D+00
    x(2) = 1.41340305910651679221840798019D+00
    x(3) = 3.59642577104072208122318658878D+00
    x(4) = 7.08581000585883755692212418111D+00
    x(5) = 12.6408008442757826594332193066D+00

    w(1) = 0.52175561058280865247586092879D+00
    w(2) = 0.3986668110831759274541333481D+00
    w(3) = 0.0759424496817075953876533114D+00
    w(4) = 0.00361175867992204845446126257D+00
    w(5) = 0.00002336997238577622789114908455D+00

  else if ( n == 6 ) then

    x(1) = 0.222846604179260689464354826787D+00
    x(2) = 1.18893210167262303074315092194D+00
    x(3) = 2.99273632605931407769132528451D+00
    x(4) = 5.77514356910451050183983036943D+00
    x(5) = 9.83746741838258991771554702994D+00
    x(6) = 15.9828739806017017825457915674D+00

    w(1) = 0.45896467394996359356828487771D+00
    w(2) = 0.4170008307721209941133775662D+00
    w(3) = 0.1133733820740449757387061851D+00
    w(4) = 0.01039919745314907489891330285D+00
    w(5) = 0.000261017202814932059479242860D+00
    w(6) = 8.98547906429621238825292053D-07

  else if ( n == 7 ) then

    x(1) = 0.193043676560362413838247885004D+00
    x(2) = 1.02666489533919195034519944317D+00
    x(3) = 2.56787674495074620690778622666D+00
    x(4) = 4.90035308452648456810171437810D+00
    x(5) = 8.18215344456286079108182755123D+00
    x(6) = 12.7341802917978137580126424582D+00
    x(7) = 19.3957278622625403117125820576D+00

    w(1) = 0.40931895170127390213043288002D+00
    w(2) = 0.4218312778617197799292810054D+00
    w(3) = 0.1471263486575052783953741846D+00
    w(4) = 0.0206335144687169398657056150D+00
    w(5) = 0.00107401014328074552213195963D+00
    w(6) = 0.0000158654643485642012687326223D+00
    w(7) = 3.17031547899558056227132215D-08

  else if ( n == 8 ) then

    x(1) = 0.170279632305100999788861856608D+00
    x(2) = 0.903701776799379912186020223555D+00
    x(3) = 2.25108662986613068930711836697D+00
    x(4) = 4.26670017028765879364942182690D+00
    x(5) = 7.04590540239346569727932548212D+00
    x(6) = 10.7585160101809952240599567880D+00
    x(7) = 15.7406786412780045780287611584D+00
    x(8) = 22.8631317368892641057005342974D+00

    w(1) = 0.36918858934163752992058283938D+00
    w(2) = 0.4187867808143429560769785813D+00
    w(3) = 0.175794986637171805699659867D+00
    w(4) = 0.033343492261215651522132535D+00
    w(5) = 0.0027945362352256725249389241D+00
    w(6) = 0.00009076508773358213104238501D+00
    w(7) = 8.4857467162725315448680183D-07
    w(8) = 1.04800117487151038161508854D-09

  else if ( n == 9 ) then

    x(1) = 0.152322227731808247428107073127D+00
    x(2) = 0.807220022742255847741419210952D+00
    x(3) = 2.00513515561934712298303324701D+00
    x(4) = 3.78347397333123299167540609364D+00
    x(5) = 6.20495677787661260697353521006D+00
    x(6) = 9.37298525168757620180971073215D+00
    x(7) = 13.4662369110920935710978818397D+00
    x(8) = 18.8335977889916966141498992996D+00
    x(9) = 26.3740718909273767961410072937D+00

    w(1) = 0.336126421797962519673467717606D+00
    w(2) = 0.411213980423984387309146942793D+00
    w(3) = 0.199287525370885580860575607212D+00
    w(4) = 0.0474605627656515992621163600479D+00
    w(5) = 0.00559962661079458317700419900556D+00
    w(6) = 0.000305249767093210566305412824291D+00
    w(7) = 6.59212302607535239225572284875D-06
    w(8) = 4.1107693303495484429024104033D-08
    w(9) = 3.29087403035070757646681380323D-11

  else if ( n == 10 ) then

    x(1) = 0.137793470540492430830772505653D+00
    x(2) = 0.729454549503170498160373121676D+00
    x(3) = 1.80834290174031604823292007575D+00
    x(4) = 3.40143369785489951448253222141D+00
    x(5) = 5.55249614006380363241755848687D+00
    x(6) = 8.33015274676449670023876719727D+00
    x(7) = 11.8437858379000655649185389191D+00
    x(8) = 16.2792578313781020995326539358D+00
    x(9) = 21.9965858119807619512770901956D+00
    x(10) = 29.9206970122738915599087933408D+00

    w(1) = 0.30844111576502014154747083468D+00
    w(2) = 0.4011199291552735515157803099D+00
    w(3) = 0.218068287611809421588648523D+00
    w(4) = 0.062087456098677747392902129D+00
    w(5) = 0.009501516975181100553839072D+00
    w(6) = 0.0007530083885875387754559644D+00
    w(7) = 0.00002825923349599565567422564D+00
    w(8) = 4.249313984962686372586577D-07
    w(9) = 1.839564823979630780921535D-09
    w(10) = 9.911827219609008558377547D-13

  else

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGUERRE_SET - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of N.'
    stop 1

  end if

  return
end
subroutine laguerre_1_exactness_test ( )

!*****************************************************************************80
!
!! LAGUERRE_1_EXACTNESS_TEST tests Gauss-Laguerre rules for the Laguerre integral with rho=1.
!
!  Discussion:
!
!    Instead of the usual density rho(x)=exp(-x), these rules apply to
!    approximating the integral:
!      I(f) = integral ( 0 <= x < +oo ) f(x) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LAGUERRE_1_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Gauss-Laguerre quadrature rules for the Laguerre integral.'
  write ( *, '(a)' ) '  Density function rho(x) = 1.'
  write ( *, '(a)' ) '  Region: 0 <= x < +oo.'
  write ( *, '(a)' ) '  Exactness: 2N-1.'

  do n = 1, 5
    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call laguerre_1_set ( n, x, w )
!
!  Standardize the rule by multiplying every weight w(i) by exp(-x(i)).
!
    w(1:n) = exp ( - x(1:n) ) * w(1:n)
!
!  Now test the rule in standard form.
!
    p_max = 2 * n
    call laguerre_exactness ( n, x, w, p_max )
    deallocate ( x )
    deallocate ( w )
  end do

  return
end
subroutine laguerre_1_set ( n, x, w )

!*****************************************************************************80
!
!! LAGUERRE_1_SET sets abscissas and weights for Laguerre quadrature.
!
!  Discussion:
!
!    This routine is specialized for the case where the density function is 1.
!
!    The integral is:
!      I(f) = integral ( 0 <= x < +oo ) f(x) dx
!    The quadrature rule is
!      Q(f) = sum ( 1 <= i <= n ) w(i) * f ( x(i) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    N must be between 1 and 10.
!
!    Output, real ( kind = 8 ) X(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) =  1.00000000000000000000000000000D+00

    w(1) =  2.7182818284590451D+00

  else if ( n == 2 ) then

    x(1) = 0.585786437626904951198311275790D+00
    x(2) = 3.41421356237309504880168872421D+00

    w(1) = 1.5333260331194167D+00
    w(2) = 4.4509573350545928D+00

  else if ( n == 3 ) then

    x(1) = 0.415774556783479083311533873128D+00
    x(2) = 2.29428036027904171982205036136D+00
    x(3) = 6.28994508293747919686641576551D+00

    w(1) = 1.0776928592709207D+00
    w(2) = 2.7621429619015876D+00
    w(3) = 5.6010946254344267D+00

  else if ( n == 4 ) then

    x(1) = 0.322547689619392311800361459104D+00
    x(2) = 1.74576110115834657568681671252D+00
    x(3) = 4.53662029692112798327928538496D+00
    x(4) = 9.39507091230113312923353644342D+00

    w(1) = 0.83273912383788917D+00
    w(2) = 2.0481024384542965D+00
    w(3) = 3.6311463058215168D+00
    w(4) = 6.4871450844076604D+00

  else if ( n == 5 ) then

    x(1) = 0.263560319718140910203061943361D+00
    x(2) = 1.41340305910651679221840798019D+00
    x(3) = 3.59642577104072208122318658878D+00
    x(4) = 7.08581000585883755692212418111D+00
    x(5) = 12.6408008442757826594332193066D+00

    w(1) = 0.67909404220775038D+00
    w(2) = 1.6384878736027471D+00
    w(3) = 2.7694432423708375D+00
    w(4) = 4.3156569009208940D+00
    w(5) = 7.2191863543544450D+00

  else if ( n == 6 ) then

    x(1) = 0.222846604179260689464354826787D+00
    x(2) = 1.18893210167262303074315092194D+00
    x(3) = 2.99273632605931407769132528451D+00
    x(4) = 5.77514356910451050183983036943D+00
    x(5) = 9.83746741838258991771554702994D+00
    x(6) = 15.9828739806017017825457915674D+00

    w(1) = 0.57353550742273818D+00
    w(2) = 1.3692525907123045D+00
    w(3) = 2.2606845933826722D+00
    w(4) = 3.3505245823554555D+00
    w(5) = 4.8868268002108213D+00
    w(6) = 7.8490159455958279D+00

  else if ( n == 7 ) then

    x(1) = 0.193043676560362413838247885004D+00
    x(2) = 1.02666489533919195034519944317D+00
    x(3) = 2.56787674495074620690778622666D+00
    x(4) = 4.90035308452648456810171437810D+00
    x(5) = 8.18215344456286079108182755123D+00
    x(6) = 12.7341802917978137580126424582D+00
    x(7) = 19.3957278622625403117125820576D+00

    w(1) = 0.49647759753997234D+00
    w(2) = 1.1776430608611976D+00
    w(3) = 1.9182497816598063D+00
    w(4) = 2.7718486362321113D+00
    w(5) = 3.8412491224885148D+00
    w(6) = 5.3806782079215330D+00
    w(7) = 8.4054324868283103D+00

  else if ( n == 8 ) then

    x(1) = 0.170279632305100999788861856608D+00
    x(2) = 0.903701776799379912186020223555D+00
    x(3) = 2.25108662986613068930711836697D+00
    x(4) = 4.26670017028765879364942182690D+00
    x(5) = 7.04590540239346569727932548212D+00
    x(6) = 10.7585160101809952240599567880D+00
    x(7) = 15.7406786412780045780287611584D+00
    x(8) = 22.8631317368892641057005342974D+00

    w(1) = 0.43772341049291136D+00
    w(2) = 1.0338693476655976D+00
    w(3) = 1.6697097656587756D+00
    w(4) = 2.3769247017585995D+00
    w(5) = 3.2085409133479259D+00
    w(6) = 4.2685755108251344D+00
    w(7) = 5.8180833686719184D+00
    w(8) = 8.9062262152922216D+00

  else if ( n == 9 ) then

    x(1) = 0.152322227731808247428107073127D+00
    x(2) = 0.807220022742255847741419210952D+00
    x(3) = 2.00513515561934712298303324701D+00
    x(4) = 3.78347397333123299167540609364D+00
    x(5) = 6.20495677787661260697353521006D+00
    x(6) = 9.37298525168757620180971073215D+00
    x(7) = 13.4662369110920935710978818397D+00
    x(8) = 18.8335977889916966141498992996D+00
    x(9) = 26.3740718909273767961410072937D+00

    w(1) = 0.39143112431563987D+00
    w(2) = 0.92180502852896307D+00
    w(3) = 1.4801279099429154D+00
    w(4) = 2.0867708075492613D+00
    w(5) = 2.7729213897119713D+00
    w(6) = 3.5916260680922663D+00
    w(7) = 4.6487660021402037D+00
    w(8) = 6.2122754197471348D+00
    w(9) = 9.3632182377057980D+00

  else if ( n == 10 ) then

    x(1) = 0.137793470540492430830772505653D+00
    x(2) = 0.729454549503170498160373121676D+00
    x(3) = 1.80834290174031604823292007575D+00
    x(4) = 3.40143369785489951448253222141D+00
    x(5) = 5.55249614006380363241755848687D+00
    x(6) = 8.33015274676449670023876719727D+00
    x(7) = 11.8437858379000655649185389191D+00
    x(8) = 16.2792578313781020995326539358D+00
    x(9) = 21.9965858119807619512770901956D+00
    x(10) = 29.9206970122738915599087933408D+00

    w(1) = 0.35400973860699630D+00
    w(2) = 0.83190230104358065D+00
    w(3) = 1.3302885617493283D+00
    w(4) = 1.8630639031111309D+00
    w(5) = 2.4502555580830108D+00
    w(6) = 3.1227641551351848D+00
    w(7) = 3.9341526955615240D+00
    w(8) = 4.9924148721930299D+00
    w(9) = 6.5722024851307994D+00
    w(10) = 9.7846958403746243D+00

  else

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAGUERRE_1_SET - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of N.'
    stop 1

  end if

  return
end
subroutine legendre_exactness_test ( )

!*****************************************************************************80
!
!! LEGENDRE_EXACTNESS_TEST tests Gauss-Legendre rules for the Legendre integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LEGENDRE_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Gauss-Legendre rules for the Legendre integral.'
  write ( *, '(a)' ) '  Density function rho(x) = 1.'
  write ( *, '(a)' ) '  Region: -1 <= x <= +1.'
  write ( *, '(a)' ) '  Exactness: 2*N-1.'

  do n = 1, 5
    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call legendre_set ( n, x, w )
    p_max = 2 * n
    call legendre_exactness ( n, x, w, p_max )
    deallocate ( x )
    deallocate ( w )
  end do

  return
end
subroutine legendre_set ( n, x, w )

!*****************************************************************************80
!
!! LEGENDRE_SET sets abscissas and weights for Gauss-Legendre quadrature.
!
!  Discussion:
!
!    The integral:
!
!      integral ( -1 <= x <= 1 ) f(x) dx
!
!    The quadrature rule:
!
!      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
!
!    The quadrature rule is exact for polynomials through degree 2*N-1.
!
!    The abscissas are the zeroes of the Legendre polynomial P(N)(X).
!
!    Mathematica can compute the abscissas and weights of a Gauss-Legendre
!    rule of order N for the interval (/A,B] with P digits of precision
!    by the commands:
!
!    Needs(/"NumericalDifferentialEquationAnalysis`"]
!    GaussianQuadratureWeights (/ n, a, b, p ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Vladimir Krylov,
!    Approximate Calculation of Integrals,
!    Dover, 2006,
!    ISBN: 0486445798,
!    LC: QA311.K713.
!
!    Arthur Stroud, Don Secrest,
!    Gaussian Quadrature Formulas,
!    Prentice Hall, 1966,
!    LC: QA299.4G3S7.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996,
!    ISBN: 0-8493-2479-3,
!    LC: QA47.M315.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    N must be between 1 and 10.
!
!    Output, real ( kind = 8 ) X(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = 0.000000000000000000000000000000D+00

    w(1) = 2.000000000000000000000000000000D+00

  else if ( n == 2 ) then

    x(1) = -0.577350269189625764509148780502D+00
    x(2) = 0.577350269189625764509148780502D+00

    w(1) = 1.000000000000000000000000000000D+00
    w(2) = 1.000000000000000000000000000000D+00

  else if ( n == 3 ) then

    x(1) = -0.774596669241483377035853079956D+00
    x(2) = 0.000000000000000000000000000000D+00
    x(3) = 0.774596669241483377035853079956D+00

    w(1) = 0.555555555555555555555555555556D+00
    w(2) = 0.888888888888888888888888888889D+00
    w(3) = 0.555555555555555555555555555556D+00

  else if ( n == 4 ) then

    x(1) = -0.861136311594052575223946488893D+00
    x(2) = -0.339981043584856264802665759103D+00
    x(3) = 0.339981043584856264802665759103D+00
    x(4) = 0.861136311594052575223946488893D+00

    w(1) = 0.347854845137453857373063949222D+00
    w(2) = 0.652145154862546142626936050778D+00
    w(3) = 0.652145154862546142626936050778D+00
    w(4) = 0.347854845137453857373063949222D+00

  else if ( n == 5 ) then

    x(1) = -0.906179845938663992797626878299D+00
    x(2) = -0.538469310105683091036314420700D+00
    x(3) = 0.000000000000000000000000000000D+00
    x(4) = 0.538469310105683091036314420700D+00
    x(5) = 0.906179845938663992797626878299D+00

    w(1) = 0.236926885056189087514264040720D+00
    w(2) = 0.478628670499366468041291514836D+00
    w(3) = 0.568888888888888888888888888889D+00
    w(4) = 0.478628670499366468041291514836D+00
    w(5) = 0.236926885056189087514264040720D+00

  else if ( n == 6 ) then

    x(1) = -0.932469514203152027812301554494D+00
    x(2) = -0.661209386466264513661399595020D+00
    x(3) = -0.238619186083196908630501721681D+00
    x(4) = 0.238619186083196908630501721681D+00
    x(5) = 0.661209386466264513661399595020D+00
    x(6) = 0.932469514203152027812301554494D+00

    w(1) = 0.171324492379170345040296142173D+00
    w(2) = 0.360761573048138607569833513838D+00
    w(3) = 0.467913934572691047389870343990D+00
    w(4) = 0.467913934572691047389870343990D+00
    w(5) = 0.360761573048138607569833513838D+00
    w(6) = 0.171324492379170345040296142173D+00

  else if ( n == 7 ) then

    x(1) = -0.949107912342758524526189684048D+00
    x(2) = -0.741531185599394439863864773281D+00
    x(3) = -0.405845151377397166906606412077D+00
    x(4) = 0.000000000000000000000000000000D+00
    x(5) = 0.405845151377397166906606412077D+00
    x(6) = 0.741531185599394439863864773281D+00
    x(7) = 0.949107912342758524526189684048D+00

    w(1) = 0.129484966168869693270611432679D+00
    w(2) = 0.279705391489276667901467771424D+00
    w(3) = 0.381830050505118944950369775489D+00
    w(4) = 0.417959183673469387755102040816D+00
    w(5) = 0.381830050505118944950369775489D+00
    w(6) = 0.279705391489276667901467771424D+00
    w(7) = 0.129484966168869693270611432679D+00

  else if ( n == 8 ) then

    x(1) = -0.960289856497536231683560868569D+00
    x(2) = -0.796666477413626739591553936476D+00
    x(3) = -0.525532409916328985817739049189D+00
    x(4) = -0.183434642495649804939476142360D+00
    x(5) = 0.183434642495649804939476142360D+00
    x(6) = 0.525532409916328985817739049189D+00
    x(7) = 0.796666477413626739591553936476D+00
    x(8) = 0.960289856497536231683560868569D+00

    w(1) = 0.101228536290376259152531354310D+00
    w(2) = 0.222381034453374470544355994426D+00
    w(3) = 0.313706645877887287337962201987D+00
    w(4) = 0.362683783378361982965150449277D+00
    w(5) = 0.362683783378361982965150449277D+00
    w(6) = 0.313706645877887287337962201987D+00
    w(7) = 0.222381034453374470544355994426D+00
    w(8) = 0.101228536290376259152531354310D+00

  else if ( n == 9 ) then

    x(1) = -0.968160239507626089835576203D+00
    x(2) = -0.836031107326635794299429788D+00
    x(3) = -0.613371432700590397308702039D+00
    x(4) = -0.324253423403808929038538015D+00
    x(5) = 0.000000000000000000000000000D+00
    x(6) = 0.324253423403808929038538015D+00
    x(7) = 0.613371432700590397308702039D+00
    x(8) = 0.836031107326635794299429788D+00
    x(9) = 0.968160239507626089835576203D+00

    w(1) = 0.081274388361574411971892158111D+00
    w(2) = 0.18064816069485740405847203124D+00
    w(3) = 0.26061069640293546231874286942D+00
    w(4) = 0.31234707704000284006863040658D+00
    w(5) = 0.33023935500125976316452506929D+00
    w(6) = 0.31234707704000284006863040658D+00
    w(7) = 0.26061069640293546231874286942D+00
    w(8) = 0.18064816069485740405847203124D+00
    w(9) = 0.081274388361574411971892158111D+00

  else if ( n == 10 ) then

    x(1) = -0.973906528517171720077964012D+00
    x(2) = -0.865063366688984510732096688D+00
    x(3) = -0.679409568299024406234327365D+00
    x(4) = -0.433395394129247190799265943D+00
    x(5) = -0.148874338981631210884826001D+00
    x(6) = 0.148874338981631210884826001D+00
    x(7) = 0.433395394129247190799265943D+00
    x(8) = 0.679409568299024406234327365D+00
    x(9) = 0.865063366688984510732096688D+00
    x(10) = 0.973906528517171720077964012D+00

    w(1) = 0.066671344308688137593568809893D+00
    w(2) = 0.14945134915058059314577633966D+00
    w(3) = 0.21908636251598204399553493423D+00
    w(4) = 0.26926671930999635509122692157D+00
    w(5) = 0.29552422471475287017389299465D+00
    w(6) = 0.29552422471475287017389299465D+00
    w(7) = 0.26926671930999635509122692157D+00
    w(8) = 0.21908636251598204399553493423D+00
    w(9) = 0.14945134915058059314577633966D+00
    w(10) = 0.066671344308688137593568809893D+00

  else

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LEGENDRE_SET - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of N.'
    stop 1

  end if

  return
end

