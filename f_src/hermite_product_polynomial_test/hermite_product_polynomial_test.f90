program main

!*****************************************************************************80
!
!! MAIN is the main program for HPP_TEST.
!
!  Discussion:
!
!    HPP_TEST tests the HERMITE_PRODUCT_POLYNOMIAL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HPP_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the HERMITE_PRODUCT_POLYNOMIAL library.'

  call hpp_test01 ( )
  call hpp_test015 ( )
  call hpp_test02 ( )
  call hpp_test03 ( )
  call hpp_test04 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HPP_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine hpp_test01 ( )

!*****************************************************************************80
!
!! HPP_TEST01 tests routines for the GRLEX ordering of compositions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 2

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank1
  integer ( kind = 4 ) rank2
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) x(k)
  integer ( kind = 4 ) x_sum
  integer ( kind = 4 ) x_sum_old

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HPP_TEST01:'
  write ( *, '(a)' ) '  COMP_NEXT_GRLEX is given a composition, and computes the '
  write ( *, '(a)' ) '  next composition in grlex order.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank   Sum   Components'

  x(1:k) = 0
  x_sum_old = -1
  rank = 1

  do

    x_sum = sum ( x(1:k) )

    if ( x_sum_old < x_sum ) then
      x_sum_old = x_sum
      write ( *, '(a)' ) ''
    end if

    write ( *, '(2x,i4,2x,i4,a)', advance = 'no' ) rank, x_sum, ':'
    do i = 1, k
      write ( *, '(i4)', advance = 'no' ) x(i)
    end do
    write ( *, '(a)' ) ''

    if ( 20 <= rank ) then
      exit
    end if

    call comp_next_grlex ( k, x )
    rank = rank + 1

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  COMP_UNRANK_GRLEX is given a rank and returns the'
  write ( *, '(a)' ) '  corresponding set of multinomial exponents.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank   Sum   Components'
  write ( *, '(a)' ) ''

  seed = 123456789

  do test = 1, 5
    rank = i4_uniform_ab ( 1, 20, seed )
    call comp_unrank_grlex ( k, rank, x )
    x_sum = sum ( x(1:k) )
    write ( *, '(2x,i4,2x,i4,a)', advance = 'no' ) rank, x_sum, ':'
    do i = 1, k
      write ( *, '(i4)', advance = 'no' ) x(i)
    end do
    write ( *, '(a)' ) ''
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  COMP_RANDOM_GRLEX randomly selects a composition'
  write ( *, '(a)' ) '  between given lower and upper ranks.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank   Sum   Components'
  write ( *, '(a)' ) ''

  seed = 123456789
  rank1 = 5
  rank2 = 20

  do test = 1, 5
    call comp_random_grlex ( k, rank1, rank2, seed, x, rank )
    x_sum = sum ( x(1:k) )
    write ( *, '(2x,i4,2x,i4,a)', advance = 'no' ) rank, x_sum, ':'
    do i = 1, k
      write ( *, '(i4)', advance = 'no' ) x(i)
    end do
    write ( *, '(a)' ) ''
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  COMP_RANK_GRLEX returns the rank of a given composition.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank   Sum   Components'
  write ( *, '(a)' ) ''

  x = (/ 4, 0 /)
  call comp_rank_grlex ( k, x, rank )
  x_sum = sum ( x(1:k) )
  write ( *, '(2x,i4,2x,i4,a)', advance = 'no' ) rank, x_sum, ':'
  do i = 1, k
    write ( *, '(i4)', advance = 'no' ) x(i)
  end do
  write ( *, '(a)' ) ''

  x = (/ 11, 5 /)
  call comp_rank_grlex ( k, x, rank )
  x_sum = sum ( x(1:k) )
  write ( *, '(2x,i4,2x,i4,a)', advance = 'no' ) rank, x_sum, ':'
  do i = 1, k
    write ( *, '(i4)', advance = 'no' ) x(i)
  end do
  write ( *, '(a)' ) ''

  return
end
subroutine hpp_test015 ( )

!*****************************************************************************80
!
!! HPP_TEST015 tests HP_COEFFICIENTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: c(:)
  integer ( kind = 4 ), allocatable :: e(:)
  integer ( kind = 4 ), allocatable :: f(:)
  integer ( kind = 4 ) l(1)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  integer ( kind = 4 ) o_max
  character ( len = 255 ) title

  m = 1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HPP_TEST015:'
  write ( *, '(a)' ) '  HEP_COEFFICIENTS computes the coefficients and'
  write ( *, '(a)' ) '  exponents of the Hermite polynomial He(n,x).'

  do n = 1, 5

    allocate ( c(1:(n+2)/2) )
    allocate ( e(1:(n+2)/2) )
    allocate ( f(1:(n+2)/2) )

    call hep_coefficients ( n, o, c, f )

    l(1) = n
    o_max = o

    call hepp_to_polynomial ( m, l, o_max, o, c, e )

    write ( *, '(a)' ) ''
    write ( title, '(a,i1,a)' ) '  He(', n, ',x) ='

    call polynomial_print ( m, o, c, e, title )

    deallocate ( c )
    deallocate ( e )
    deallocate ( f )

  end do

  return
end
subroutine hpp_test02 ( )

!*****************************************************************************80
!
!! HPP_TEST02 tests HP_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) e
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) o
  real ( kind = 8 ) x
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2

  m = 1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HPP_TEST02:'
  write ( *, '(a)' ) '  HEP_VALUES stores values of'
  write ( *, '(a)' ) '  the Hermite polynomial He(n,x).'
  write ( *, '(a)' ) '  HEP_VALUE evaluates a Hermite polynomial.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '                        Tabulated                 Computed'
  write ( *, '(a)', advance = 'no' ) &
    '     O        X          He(O,X)                   He(O,X)'
  write ( *, '(a)' ) '                   Error'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call hep_values ( n_data, o, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    call hep_value ( m, o, x, fx2 )

    e = fx1 - fx2

    write ( *, '(2x,i4,2x,f12.8,2x,g24.16,2x,g24.16,2x,g8.2)' ) &
      o, x, fx1, fx2, e

  end do

  return
end
subroutine hpp_test03 ( )

!*****************************************************************************80
!
!! HPP_TEST03 tests HEPP_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 1

  real ( kind = 8 ), allocatable :: c(:)
  integer ( kind = 4 ), allocatable :: e(:)
  integer ( kind = 4 ) l(m)
  integer ( kind = 4 ) o
  integer ( kind = 4 ) o_max
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) seed
  real ( kind = 8 ) v1(n)
  real ( kind = 8 ) v2(n)
  real ( kind = 8 ) x(m,n)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HPP_TEST03:'
  write ( *, '(a)' ) '  HEPP_VALUE evaluates a Hermite product polynomial.'
  write ( *, '(a)' ) '  POLYNOMIAL_VALUE evaluates a polynomial.'

  xlo = -1.0D+00
  xhi = +1.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( m, xlo, xhi, seed, x )

  write ( *, '(a)' ) ''
  write ( *, '(a,3g14.6)' ) '  Evaluate at X = ', x(1:3,1)
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank  I1  I2  I3:  He(I1,X1)*He(I2,X2)*He(I3,X3)    P(X1,X2,X3)'
  write ( *, '(a)' ) ''

  do rank = 1, 20

    call comp_unrank_grlex ( m, rank, l )
!
!  Evaluate the HePP directly.
!
    call hepp_value ( m, n, l, x, v1 )
!
!  Convert the HePP to a polynomial.
!
    o_max = product ( ( l(1:m) + 2 ) / 2 )

    allocate ( c(1:o_max) )
    allocate ( e(1:o_max) )

    call hepp_to_polynomial ( m, l, o_max, o, c, e )
!
!  Evaluate the polynomial.
!
    call polynomial_value ( m, o, c, e, n, x, v2 )
!
!  Compare results.
!
    write ( *, '(2x,i4,2x,i2,2x,i2,2x,i2,2x,g14.6,2x,g14.6)' ) &
      rank, l(1:m), v1(1), v2(1)

    deallocate ( c )
    deallocate ( e )

  end do

  return
end
subroutine hpp_test04 ( )

!*****************************************************************************80
!
!! HPP_TEST04 tests HEPP_TO_POLYNOMIAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2

  real ( kind = 8 ), allocatable :: c(:)
  integer ( kind = 4 ), allocatable :: e(:)
  integer ( kind = 4 ) l(m)
  character ( len = 255 ) label
  integer ( kind = 4 ) o
  integer ( kind = 4 ) o_max
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HPP_TEST04:'
  write ( *, '(a)' ) '  HEPP_TO_POLYNOMIAL is given a Hermite product polynomial'
  write ( *, '(a)' ) '  and determines its polynomial representation.'

  write ( *, '(a)' ) ''
  write ( *, '(a,i2)' ) '  Using spatial dimension M = ', m

  do rank = 1, 11

    call comp_unrank_grlex ( m, rank, l )

    o_max = product ( ( l(1:m) + 2 ) / 2 )

    allocate ( c(1:o_max) )
    allocate ( e(1:o_max) )

    call hepp_to_polynomial ( m, l, o_max, o, c, e )

    write ( label, '(a,i2,a,i2,a,i2,a)' ) &
      '  HePP #', rank, & 
      ' = L(', l(1), &
      ',X)*L(', l(2), &
      ',Y) ='

    write ( *, '(a)' ) ''
    call polynomial_print ( m, o, c, e, label )

    deallocate ( c )
    deallocate ( e )

  end do

  return
end
