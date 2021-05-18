program main

!*****************************************************************************80
!
!! MAIN is the main probram for LINE_CVT_LLOYD_TEST.
!
!  Discussion:
!
!    LINE_CVT_LLOYD_TEST tests the line_cvt_lloyd library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LINE_CVT_LLOYD_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LINE_CVT_LLOYD library.'

  call test01 ( )
  call test02 ( )
!
!  Repeat, using sorted initial points.
!
  call test03 ( )
  call test04 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LINE_CVT_LLOYD_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! LINE_CVT_LLOYD_TEST01 tests the unconstrained computation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 25

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) h
  character ( len = 255 ) header
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LINE_CVT_LLOYD_TEST01:'
  write ( *, '(a)' ) '  Test the unconstrained computation.'

  a = 0.0D+00
  b = 1.0D+00
  it_num = 200
  seed = 123456789
  call r8vec_uniform_ab ( n, a, b, seed, x )
  header = 'test01'

  write ( *, '(a)' ) ''
  write ( *, '(a,i3,a,f8.4,a,f8.4,a)' ) &
    '  Use ', n, ' points in the interval [', a, ',', b, ']'
  write ( *, '(a,i4)' ) '  Number of iterations to take is ', it_num
  write ( *, '(a)' ) '  Call this calculation "'// trim ( header ) // '"'
  h = ( b - a ) / real ( n - 1, kind = 8 )
  write ( *, '(a,g14.6)' ) '  Expect a uniform spacing of ', h

  call r8vec_print ( n, x, '  Initial generators:' )

  call line_cvt_lloyd ( n, a, b, it_num, header, x )

  call r8vec_print ( n, x, '  Final generators:' )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! LINE_CVT_LLOYD_TEST02 tests the constrained computation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 25

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) h
  character ( len = 255 ) header
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LINE_CVT_LLOYD_TEST02:'
  write ( *, '(a)' ) '  Test the constrained computation.'

  a = 0.0D+00
  b = 1.0D+00
  it_num = 200
  seed = 123456789
  call r8vec_uniform_ab ( n, a, b, seed, x )
  header = 'test02'

  write ( *, '(a)' ) ''
  write ( *, '(a,i3,a,f8.4,a,f8.4,a)' ) &
    '  Use ', n, ' points in the interval [', a, ',', b, ']'
  write ( *, '(a,i4)' ) '  Number of iterations to take is ', it_num
  write ( *, '(a)' ) '  Call this calculation "'// trim ( header ) // '"'
  h = ( b - a ) / real ( n, kind = 8 )
  write ( *, '(a,g14.6)' ) '  Expect a uniform spacing of ', h

  call r8vec_print ( n, x, '  Initial generators:' )

  call line_ccvt_lloyd ( n, a, b, it_num, header, x )

  call r8vec_print ( n, x, '  Final generators:' )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! LINE_CVT_LLOYD_TEST03 tests the unconstrained computation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 25

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) h
  character ( len = 255 ) header
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LINE_CVT_LLOYD_TEST03:'
  write ( *, '(a)' ) '  Test the unconstrained computation.'
  write ( *, '(a)' ) '  SORT the random initial values before use.'

  a = 0.0D+00
  b = 1.0D+00
  it_num = 200
  seed = 123456789
  call r8vec_uniform_ab ( n, a, b, seed, x )
  call r8vec_sort_insert_a ( n, x )
  header = 'test03'

  write ( *, '(a)' ) ''
  write ( *, '(a,i3,a,f8.4,a,f8.4,a)' ) &
    '  Use ', n, ' points in the interval [', a, ',', b, ']'
  write ( *, '(a,i4)' ) '  Number of iterations to take is ', it_num
  write ( *, '(a)' ) '  Call this calculation "'// trim ( header ) // '"'
  h = ( b - a ) / real ( n - 1, kind = 8 )
  write ( *, '(a,g14.6)' ) '  Expect a uniform spacing of ', h

  call r8vec_print ( n, x, '  Initial generators:' )

  call line_cvt_lloyd ( n, a, b, it_num, header, x )

  call r8vec_print ( n, x, '  Final generators:' )

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! LINE_CVT_LLOYD_TEST04 tests the constrained computation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 25

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) h
  character ( len = 255 ) header
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LINE_CVT_LLOYD_TEST04:'
  write ( *, '(a)' ) '  Test the constrained computation.'
  write ( *, '(a)' ) '  SORT the initial points before use.'

  a = 0.0D+00
  b = 1.0D+00
  it_num = 200
  seed = 123456789
  call r8vec_uniform_ab ( n, a, b, seed, x )
  call r8vec_sort_insert_a ( n, x )
  header = 'test04'

  write ( *, '(a)' ) ''
  write ( *, '(a,i3,a,f8.4,a,f8.4,a)' ) &
    '  Use ', n, ' points in the interval [', a, ',', b, ']'
  write ( *, '(a,i4)' ) '  Number of iterations to take is ', it_num
  write ( *, '(a)' ) '  Call this calculation "'// trim ( header ) // '"'
  h = ( b - a ) / real ( n, kind = 8 )
  write ( *, '(a,g14.6)' ) '  Expect a uniform spacing of ', h

  call r8vec_print ( n, x, '  Initial generators:' )

  call line_ccvt_lloyd ( n, a, b, it_num, header, x )

  call r8vec_print ( n, x, '  Final generators:' )

  return
end
