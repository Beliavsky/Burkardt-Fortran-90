program main

!*****************************************************************************80
!
!! MAIN is the main program for legendre_product_polynomial_test.
!
!  Discussion:
!
!    legendre_product_polynomial_test tests the LEGENDRE_PRODUCT_POLYNOMIAL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 November 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'legendre_product_polynomial_test:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LEGENDRE_PRODUCT_POLYNOMIAL library.'

  call i4_choose_test ( )
  call i4_uniform_ab_test ( )

  call i4vec_permute_test ( )
  call i4vec_print_test ( )
  call i4vec_sort_heap_index_a_test ( )
  call i4vec_sum_test ( )
  call i4vec_uniform_ab_test ( )

  call r8vec_permute_test ( )
  call r8vec_print_test ( )
  call r8vec_uniform_ab_test ( )

  call perm_uniform_test ( )

  call comp_enum_test ( )
  call comp_next_grlex_test ( )
  call comp_random_grlex_test ( )
  call comp_rank_grlex_test ( )
  call comp_unrank_grlex_test ( )

  call mono_next_grlex_test ( )
  call mono_print_test ( )
  call mono_rank_grlex_test ( )
  call mono_unrank_grlex_test ( )
  call mono_upto_enum_test ( )
  call mono_upto_next_grlex_test ( )
  call mono_upto_random_test ( )
  call mono_value_test ( )

  call polynomial_compress_test ( )
  call polynomial_print_test ( )
  call polynomial_sort_test ( )
  call polynomial_value_test ( )

  call lp_coefficients_test ( )
  call lp_value_test ( )
  call lp_values_test ( )

  call lpp_to_polynomial_test ( )
  call lpp_value_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'legendre_product_polynomial_test:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine comp_enum_test ( )

!*****************************************************************************80
!
!! COMP_ENUM_TEST tests COMP_ENUM;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 October 2014
!
!  Author:
!
!    John Burkardt
! 
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  integer ( kind = 4 ) num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMP_ENUM_TEST'
  write ( *, '(a)' ) '  COMP_ENUM counts compositions;'
  write ( *, '(a)' ) ''
  do n = 0, 10
    do k = 1, 10
      call comp_enum ( n, k, num )
      write ( *, '(2x,i6)', advance = 'no' ) num
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine comp_next_grlex_test ( )

!*****************************************************************************80
!
!! COMP_NEXT_GRLEX_TEST tests COMP_NEXT_GRLEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: kc = 3

  integer ( kind = 4 ) i4vec_sum
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) xc(kc)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMP_NEXT_GRLEX_TEST'
  write ( *, '(a)' ) '  A COMP is a composition of an integer N into K parts.'
  write ( *, '(a)' ) '  Each part is nonnegative.  The order matters.'
  write ( *, '(a)' ) '  COMP_NEXT_GRLEX determines the next COMP in'
  write ( *, '(a)' ) '  graded lexicographic (grlex) order.'
  
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank:     NC       COMP    '
  write ( *, '(a)' ) '  ----:     --   ------------'

  do rank = 1, 71

    if ( rank == 1 ) then
      xc(1:kc) = 0
    else
      call comp_next_grlex ( kc, xc )
    end if

    nc = i4vec_sum ( kc, xc(1:kc) )

    write ( *, '(3x,i3,a)', advance = 'no' ) rank, ': '
    write ( *, '(4x,i2,a)', advance = 'no' ) nc, ' = '
    do j = 1, kc - 1
      write ( *, '(i2,a)', advance = 'no' ) xc(j), ' + '
    end do
    write ( *, '(i2)', advance = 'yes' ) xc(kc)
!
!  When XC(1) == NC, we have completed the compositions associated with
!  a particular integer, and are about to advance to the next integer.
!
    if ( xc(1) == nc ) then
      write ( *, '(a)' ) '  ----:     --   ------------'
    end if

  end do

  return
end
subroutine comp_random_grlex_test ( )

!*****************************************************************************80
!
!! COMP_RANDOM_GRLEX_TEST tests COMP_RANDOM_GRLEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: kc = 3

  integer ( kind = 4 ) i4vec_sum
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank1
  integer ( kind = 4 ) rank2
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) xc(kc)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMP_RANDOM_GRLEX_TEST'
  write ( *, '(a)' ) '  A COMP is a composition of an integer N into K parts.'
  write ( *, '(a)' ) '  Each part is nonnegative.  The order matters.'
  write ( *, '(a)' ) '  COMP_RANDOM_GRLEX selects a random COMP in'
  write ( *, '(a)' ) '  graded lexicographic (grlex) order between'
  write ( *, '(a)' ) '  indices RANK1 and RANK2.'
  write ( *, '(a)' ) ''

  rank1 = 20
  rank2 = 60
  seed = 123456789

  do test = 1, 5

    call comp_random_grlex ( kc, rank1, rank2, seed, xc, rank )
    nc = i4vec_sum ( kc, xc )

    write ( *, '(a,i3,a)', advance = 'no' ) '   ', rank, ': '
    write ( *, '(a,i2,a)', advance = 'no' ) '    ', nc, ' = '
    do j = 1, kc - 1
      write ( *, '(i2,a)', advance = 'no' ) xc(j), ' + '
    end do
    write ( *, '(i2)' ) xc(kc)
  end do

  return
end
subroutine comp_rank_grlex_test ( )

!*****************************************************************************80
!
!! COMP_RANK_GRLEX_TEST tests COMP_RANK_GRLEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: kc = 3

  integer ( kind = 4 ) rank1
  integer ( kind = 4 ) rank2
  integer ( kind = 4 ) rank3
  integer ( kind = 4 ) rank4
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) xc(kc)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMP_RANK_GRLEX_TEST'
  write ( *, '(a)' ) '  A COMP is a composition of an integer N into K parts.'
  write ( *, '(a)' ) '  Each part is nonnegative.  The order matters.'
  write ( *, '(a)' ) '  COMP_RANK_GRLEX determines the rank of a COMP'
  write ( *, '(a)' ) '  from its parts.'
     
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        Actual  Inferred'
  write ( *, '(a)' ) '  Test    Rank      Rank'
  write ( *, '(a)' ) ''

  rank1 = 20
  rank2 = 60
  seed = 123456789

  do test = 1, 5
    call comp_random_grlex ( kc, rank1, rank2, seed, xc, rank3 )
    call comp_rank_grlex ( kc, xc, rank4 )
    write ( *, '(2x,i4,2x,i6,2x,i8)' ) test, rank3, rank4
  end do

  return
end
subroutine comp_unrank_grlex_test ( )

!*****************************************************************************80
!
!! COMP_UNRANK_GRLEX_TEST tests COMP_UNRANK_GRLEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: kc = 3

  integer ( kind = 4 ) i4vec_sum
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) rank1
  integer ( kind = 4 ) xc(kc)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMP_UNRANK_GRLEX_TEST'
  write ( *, '(a)' ) '  A COMP is a composition of an integer N into K parts.'
  write ( *, '(a)' ) '  Each part is nonnegative.  The order matters.'
  write ( *, '(a)' ) '  COMP_UNRANK_GRLEX determines the parts'
  write ( *, '(a)' ) '  of a COMP from its rank.'
     
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank: ->  NC       COMP'
  write ( *, '(a)' ) '  ----:     --   ------------'

  do rank1 = 1, 71
    call comp_unrank_grlex ( kc, rank1, xc )
    nc = i4vec_sum ( kc, xc )

    write ( *, '(3x,i3,a)', advance = 'no' ) rank1, ': '
    write ( *, '(4x,i2,a)', advance = 'no' ) nc, ' = '
    do j = 1, kc - 1
      write ( *, '(i2,a)', advance = 'no' ) xc(j), ' + '
    end do
    write ( *, '(i2)', advance = 'yes' ) xc(kc)
!
!  When XC(1) == NC, we have completed the compositions associated with
!  a particular integer, and are about to advance to the next integer.
!
    if ( xc(1) == nc ) then
      write ( *, '(a)' ) '  ----:     --   ------------'
    end if

  end do

  return
end
subroutine i4_choose_test ( )

!*****************************************************************************80
!
!! I4_CHOOSE_TEST tests I4_CHOOSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) cnk
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_CHOOSE_TEST'
  write ( *, '(a)' ) '  I4_CHOOSE evaluates C(N,K).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         N         K       CNK'
 
  do n = 0, 4
    write ( *, '(a)' ) ' '
    do k = 0, n
      cnk = i4_choose ( n, k )
      write ( *, '(2x,i8,2x,i8,2x,i8)' ) n, k, cnk
    end do
  end do
 
  return
end
subroutine i4_uniform_ab_test ( )

!*****************************************************************************80
!
!! I4_UNIFORM_AB_TEST tests I4_UNIFORM_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: a = -100
  integer ( kind = 4 ), parameter :: b = 200
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  I4_UNIFORM_AB computes pseudorandom values '
  write ( *, '(a)' ) '  in an interval [A,B].'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  The lower endpoint A = ', a
  write ( *, '(a,i12)' ) '  The upper endpoint B = ', b
  write ( *, '(a,i12)' ) '  The initial seed is ', seed
  write ( *, '(a)' ) ' '

  do i = 1, 20

    j = i4_uniform_ab ( a, b, seed )

    write ( *, '(2x,i8,2x,i8)' ) i, j

  end do

  return
end
subroutine i4vec_permute_test ( )

!*****************************************************************************80
!
!! I4VEC_PERMUTE_TEST tests I4VEC_PERMUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 12

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = n
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4VEC_PERMUTE_TEST'
  write ( *, '(a)' ) '  I4VEC_PERMUTE reorders an I4VEC'
  write ( *, '(a)' ) '  according to a given permutation.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  A, before rearrangement:' )

  call perm_uniform ( n, seed, p )

  call i4vec_print ( n, p, '  Permutation vector P:' )

  call i4vec_permute ( n, p, a )

  call i4vec_print ( n, a, '  A, after rearrangement:' )

  return
end
subroutine i4vec_sort_heap_index_a_test ( )

!*****************************************************************************80
!
!! I4VEC_SORT_HEAP_INDEX_A_TEST tests I4VEC_SORT_HEAP_INDEX_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4VEC_SORT_HEAP_INDEX_A_TEST'
  write ( *, '(a)' ) '  I4VEC_SORT_HEAP_INDEX_A creates an ascending'
  write ( *, '(a)' ) '  sort index for an I4VEC.'

  seed = 123456789

  b = 0
  c = 3 * n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Unsorted array A:' )

  call i4vec_sort_heap_index_a ( n, a, indx )

  call i4vec_print ( n, indx, '  Index vector INDX:' )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  I, INDX(I), A(INDX(I))'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(3i8)' ) i, indx(i), a(indx(i))
  end do

  return
end
subroutine i4vec_sum_test ( )

!*****************************************************************************80
!
!! I4VEC_SUM_TEST tests I4VEC_SUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) i4vec_sum
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) s
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4VEC_SUM_TEST'
  write ( *, '(a)' ) '  I4VEC_SUM sums the entries of an I4VEC.'

  lo = 0
  hi = 10
  seed = 123456789

  call i4vec_uniform_ab ( n, lo, hi, seed, a )
  call i4vec_print ( n, a, '  The vector:' )

  s = i4vec_sum ( n, a )
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The vector entries sum to ', s

  return
end
subroutine i4vec_uniform_ab_test ( )

!*****************************************************************************80
!
!! I4VEC_UNIFORM_AB_TEST tests I4VEC_UNIFORM_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ), parameter :: a = -100
  integer ( kind = 4 ), parameter :: b = 200
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) v(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4VEC_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  I4VEC_UNIFORM_AB computes pseudorandom values '
  write ( *, '(a)' ) '  in an interval [A,B].'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  The lower endpoint A = ', a
  write ( *, '(a,i12)' ) '  The upper endpoint B = ', b
  write ( *, '(a,i12)' ) '  The initial seed is ', seed

  call i4vec_uniform_ab ( n, a, b, seed, v )

  call i4vec_print ( n, v, '  The random vector:' )

  return
end
subroutine mono_next_grlex_test ( )

!*****************************************************************************80
!
!! MONO_NEXT_GRLEX_TEST tests MONO_NEXT_GRLEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MONO_NEXT_GRLEX_TEST'
  write ( *, '(a)' ) '  MONO_NEXT_GRLEX returns the next monomial'
  write ( *, '(a)' ) '  in graded lexicographic order.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i2)' ) '  Let M =  ', m

  a = 0
  b = 3
  seed = 123456789

  do i = 1, 10

    call i4vec_uniform_ab ( m, a, b, seed, x )
    write ( *, '(a)' ) ' '
    write ( *, '(2x,4i2)' ) x(1:m)

    do j = 1, 5
      call mono_next_grlex ( m, x )
      write ( *, '(2x,4i2)' ) x(1:m)
    end do

  end do

  return
end
subroutine mono_print_test ( )

!*****************************************************************************80
!
!! MONO_PRINT_TEST tests MONO_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 November 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), dimension ( 1 ) :: f1 = (/ 5 /)
  integer ( kind = 4 ), dimension ( 1 ) :: f2 = (/ - 5 /)
  integer ( kind = 4 ), dimension ( 4 ) :: f3 = (/ 2, 1, 0, 3 /)
  integer ( kind = 4 ), dimension ( 3 ) :: f4 = (/ 17, -3, 199 /)
  integer ( kind = 4 ) m

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MONO_PRINT_TEST'
  write ( *, '(a)' ) '  MONO_PRINT can print out a monomial.'
  write ( *, '(a)' ) ''

  m = 1
  call mono_print ( m, f1, '  Monomial [5]:' )

  m = 1
  call mono_print ( m, f2, '  Monomial [5]:' )

  m = 4
  call mono_print ( m, f3, '  Monomial [2,1,0,3]:' )

  m = 3
  call mono_print ( m, f4, '  Monomial [17,-3,199]:' )

  return
end
subroutine mono_rank_grlex_test ( )

!*****************************************************************************80
!
!! MONO_RANK_GRLEX_TEST tests MONO_RANK_GRLEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: test_num = 8

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) test
  integer ( kind = 4 ) x(m)
  integer ( kind = 4 ), dimension ( m, test_num ) :: x_test = reshape ( (/ &
    0, 0, 0, &
    1, 0, 0, &
    0, 0, 1, &
    0, 2, 0, &
    1, 0, 2, &
    0, 3, 1, &
    3, 2, 1, &
    5, 2, 1 /), (/ m, test_num /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MONO_RANK_GRLEX_TEST'
  write ( *, '(a)' ) '  MONO_RANK_GRLEX returns the rank of a monomial in '
  write ( *, '(a)' ) '  the sequence of all monomials in M dimensions.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Print a monomial sequence with ranks assigned.'

  n = 4

  write ( *, '(a)' ) ''
  write ( *, '(a,i2)' ) '  Let M = ', m
  write ( *, '(a,i2)' ) '      N = ', n
  write ( *, '(a)' ) ''

  x = (/ 0, 0, 0 /)
  i = 1

  do

    write ( *, '(2x,i3,4x,3i2)' ) i, x(1:m)

    if ( x(1) == n ) then
      exit
    end if

    call mono_upto_next_grlex ( m, n, x )
    i = i + 1

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now, given a monomial, retrieve its rank in the sequence:'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    x(1:m) = x_test(1:m,test)
    call mono_rank_grlex ( m, x, rank )
    write ( *, '(2x,i3,4x,3i2)' ) rank, x(1:m)
  end do

  return
end
subroutine mono_unrank_grlex_test ( )

!*****************************************************************************80
!
!! MONO_RANK_GRLEX_TEST tests MONO_UNRANK_GRLEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) , parameter :: m = 3

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_max
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) test_num
  integer ( kind = 4 ) x(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MONO_UNRANK_GRLEXT_TEST'
  write ( *, '(a)' ) '  MONO_UNRANK_GRLEX is given a rank, and returns'
  write ( *, '(a)' ) '  the corresponding monomial in the sequence of '
  write ( *, '(a)' ) '  all monomials in M dimensions in grlex order.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  For reference, print a monomial sequence with ranks.'

  n = 4
  rank_max = mono_upto_enum ( m, n )

  write ( *, '(a)' ) ''
  write ( *, '(a,i3)' ) '  Let M = ', m
  write ( *, '(a,i3)' ) '      N = ', n
  write ( *, '(a)' ) ''

  x(1:m) = 0

  i = 1

  do

    write ( *, '(2x,i3,4x,3i2)' ) i, x(1:m)

    if ( x(1) == n ) then
      exit
    end if

    call mono_upto_next_grlex ( m, n, x )
    i = i + 1

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,i3)' ) '  Now choose random ranks between 1 and ', rank_max
  write ( *, '(a)' ) ''

  seed = 123456789
  test_num = 5

  do test = 1, test_num

    rank = i4_uniform_ab ( 1, rank_max, seed )    
    call mono_unrank_grlex ( m, rank, x )
    write ( *, '(2x,i3,4x,3i2)' ) rank, x(1:m)

  end do

  return
end
subroutine mono_upto_enum_test ( )

!*****************************************************************************80
!
!! MONO_UPTO_ENUM_TEST tests MONO_UPTO_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) v

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MONO_UPTO_ENUM_TEST'
  write ( *, '(a)' ) '  MONO_UPTO_ENUM can enumerate the number of monomials'
  write ( *, '(a)' ) '  in M variables, of total degree 0 up to N.'

  write ( *, '(a)' ) ''
  write ( *, '(a)', advance = 'no' ) '    N:'
  do n = 0, 8
    write ( *, '(2x,i4)', advance = 'no' ) n
  end do
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '   M +------------------------------------------------------'
  do m = 1, 8
    write ( *, '(2x,i2,a)', advance = 'no' ) m, ' |'
    do n = 0, 8
      v = mono_upto_enum ( m, n )
      write ( *, '(1x,i5)', advance = 'no' ) v
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine mono_upto_next_grlex_test ( )

!*****************************************************************************80
!
!! MONO_UPTO_NEXT_GRLEX_TEST tests MONO_UPTO_NEXT_GRLEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) x(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MONO_UPTO_NEXT_GRLEX_TEST'
  write ( *, '(a)' ) '  MONO_UPTO_NEXT_GRLEX can list the monomials'
  write ( *, '(a)' ) '  in M variables, of total degree up to N,'
  write ( *, '(a)' ) '  one at a time, in graded lexicographic order.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  We start the process with (0,0,...,0,0).'
  write ( *, '(a)' ) '  The process ends with (N,0,...,0,0)'

  n = 4

  write ( *, '(a)' ) ''
  write ( *, '(a,i2)' ) '  Let M = ', m
  write ( *, '(a,i2)' ) '      N = ', n
  write ( *, '(a)' ) ''

  x = (/ 0, 0, 0 /)
  i = 1

  do

    write ( *, '(2x,i2,4x,3i2)' ) i, x(1:m)

    if ( x(1) == n ) then
      exit
    end if

    call mono_upto_next_grlex ( m, n, x )
    i = i + 1

  end do

  return
end
subroutine mono_upto_random_test ( )

!*****************************************************************************80
!
!! MONO_UPTO_RANDOM_TEST tests MONO_UPTO_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 November 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) , parameter :: m = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) test_num
  integer ( kind = 4 ) x(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MONO_UPTO_RANDOM_TEST'
  write ( *, '(a)' ) '  MONO_UPTO_RANDOM selects at random a monomial'
  write ( *, '(a)' ) '  in M dimensions of total degree no greater than N.'

  n = 4

  write ( *, '(a)' ) ''
  write ( *, '(a,i3)' ) '  Let M = ', m
  write ( *, '(a,i3)' ) '      N = ', n
  write ( *, '(a)' ) ''

  seed = 123456789
  test_num = 5

  do test = 1, test_num
    call mono_upto_random ( m, n, seed, rank, x )
    write ( *, '(2x,i3,4x,3i2)' ) rank, x(1:m)
  end do

  return
end
subroutine mono_value_test ( )

!*****************************************************************************80
!
!! MONO_VALUE_TEST tests MONO_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: nx = 2

  integer ( kind = 4 ) f(m)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) test_num
  real ( kind = 8 ) v(nx)
  real ( kind = 8 ), dimension ( m, nx ) :: x = reshape ( (/ &
     1.0D+00, 2.0D+00, 3.0D+00, &
    -2.0D+00, 4.0D+00, 1.0D+00 /), (/ m, nx /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MONO_VALUE_TEST'
  write ( *, '(a)' ) '  MONO_VALUE evaluates a monomial.'

  n = 6

  write ( *, '(a)' ) ''
  write ( *, '(a,i3)' ) '  Let M = ', m
  write ( *, '(a,i3)' ) '      N = ', n

  seed = 123456789
  test_num = 5

  do test = 1, test_num

    call mono_upto_random ( m, n, seed, rank, f )
    write ( *, '(a)' ) ''
    call mono_print ( m, f, '  M(X) = ' )
    call mono_value ( m, nx, f, x, v )
    do j = 1, nx
      write ( *, '(a,f4.0,a,f4.0,a,f4.0,a,g14.6)' ) &
        '  M(', x(1,j), ',', x(2,j), ',', x(3,j), ') = ', v(j)
    end do
    
  end do

  return
end
subroutine perm_uniform_test ( )

!*****************************************************************************80
!
!! PERM_UNIFORM_TEST tests PERM_UNIFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PERM_UNIFORM_TEST'
  write ( *, '(a)' ) '  PERM_UNIFORM randomly selects a permutation.'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, 5

    call perm_uniform ( n, seed, p )
    write ( *, '(2x,10i4)' ) p(1:n)

  end do

  return
end
subroutine i4vec_print_test ( )

!*****************************************************************************80
!
!! I4VEC_PRINT_TEST tests I4VEC_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 November 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ), dimension ( n ) :: a = (/ &
    91, 92, 93, 94 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4VEC_PRINT_TEST'
  write ( *, '(a)' ) '  I4VEC_PRINT prints an I4VEC'

  call i4vec_print ( n, a, '  The I4VEC:' )

  return
end
subroutine lp_coefficients_test ( )

!*****************************************************************************80
!
!! LP_COEFFICIENTS_TEST tests LP_COEFFICIENTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  real ( kind = 8 ) c(n_max+1)
  integer ( kind = 4 ) e(n_max+1)
  integer ( kind = 4 ) f(n_max+1)
  character ( len = 255 ) label
  integer ( kind = 4 ) m 
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LP_COEFFICIENTS_TEST'
  write ( *, '(a)' ) &
    '  LP_COEFFICIENTS: coefficients of Legendre polynomial P(n,x).'
  write ( *, '(a)' ) ''
  
  do n = 0, n_max

    call lp_coefficients ( n, o, c, f )

    m = 1
    e(1:o) = f(1:o) + 1

    write ( label, '(a,i2,a)' ) '  P(', n, ',x) ='
    call polynomial_print ( m, o, c, e, label )

   end do

  return
end
subroutine lp_value_test ( )

!*****************************************************************************80
!
!! LP_VALUE_TEST tests LP_VALUE.
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
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) o
  real ( kind = 8 ) x
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2

  n = 1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LP_VALUE_TEST:'
  write ( *, '(a)' ) '  LP_VALUE evaluates a Legendre polynomial.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '                        Tabulated                 Computed'
  write ( *, '(a)', advance = 'no' ) &
    '     O        X           L(O,X)                    L(O,X)'
  write ( *, '(a)' ) '                   Error'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call lp_values ( n_data, o, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    call lp_value ( n, o, x, fx2 )

    e = fx1 - fx2

    write ( *, '(2x,i4,2x,f12.8,2x,g24.16,2x,g24.16,2x,g8.2)' ) &
      o, x, fx1, fx2, e

  end do

  return
end
subroutine lp_values_test ( )

!*****************************************************************************80
!
!! LP_VALUES_TEST tests LP_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) o
  real ( kind = 8 ) x
  real ( kind = 8 ) fx

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LP_VALUES_TEST:'
  write ( *, '(a)' ) '  LP_VALUES stores values of'
  write ( *, '(a)' ) '  the Legendre polynomial P(o,x).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '                        Tabulated'
  write ( *, '(a)' ) '     O        X           L(O,X)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call lp_values ( n_data, o, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(2x,i4,2x,f12.8,2x,g24.16)' ) o, x, fx

  end do

  return
end
subroutine lpp_to_polynomial_test ( )

!*****************************************************************************80
!
!! LPP_TO_POLYNOMIAL_TEST tests LPP_TO_POLYNOMIAL.
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
  write ( *, '(a)' ) 'LPP_TO_POLYNOMIAL_TEST:'
  write ( *, '(a)' ) '  LPP_TO_POLYNOMIAL is given a Legendre product polynomial'
  write ( *, '(a)' ) '  and determines its polynomial representation.'

  write ( *, '(a)' ) ''
  write ( *, '(a,i2)' ) '  Using spatial dimension M = ', m

  do rank = 1, 11

    call comp_unrank_grlex ( m, rank, l )

    o_max = product ( ( l(1:m) + 2 ) / 2 )

    allocate ( c(1:o_max) )
    allocate ( e(1:o_max) )

    call lpp_to_polynomial ( m, l, o_max, o, c, e )

    write ( label, '(a,i2,a,i2,a,i2,a)' ) &
      '  LPP #', rank, & 
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
subroutine lpp_value_test ( )

!*****************************************************************************80
!
!! LPP_VALUE_TEST tests LPP_VALUE.
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
  write ( *, '(a)' ) 'LPP_VALUE_TEST:'
  write ( *, '(a)' ) '  LPP_VALUE evaluates a Legendre product polynomial.'

  xlo = -1.0D+00
  xhi = +1.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( m, xlo, xhi, seed, x )

  write ( *, '(a)' ) ''
  write ( *, '(a,3g14.6)' ) '  Evaluate at X = ', x(1:3,1)
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank  I1  I2  I3:  L(I1,X1)*L(I2,X2)*L(I3,X3)    P(X1,X2,X3)'
  write ( *, '(a)' ) ''

  do rank = 1, 20

    call comp_unrank_grlex ( m, rank, l )
!
!  Evaluate the LPP directly.
!
    call lpp_value ( m, n, l, x, v1 )
!
!  Convert the LPP to a polynomial.
!
    o_max = product ( ( l(1:m) + 2 ) / 2 )

    allocate ( c(1:o_max) )
    allocate ( e(1:o_max) )

    call lpp_to_polynomial ( m, l, o_max, o, c, e )
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
subroutine polynomial_compress_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_COMPRESS_TEST tests POLYNOMIAL_COMPRESS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer, parameter :: m = 3
  integer, parameter :: o = 10

  real ( kind = 8 ), save, dimension ( o ) :: c = (/ &
    7.0, - 5.0, 5.0, 9.0, 11.0, 3.0, 6.0, 0.0, - 13.0, 1.0E-20 /)
  real ( kind = 8 ) c2(o)
  integer ( kind = 4 ), save, dimension ( o ) :: e = (/ &
    1, 2, 2, 4, 5, 5, 5, 12, 33, 35 /)
  integer ( kind = 4 ) e2(o)
  integer ( kind = 4 ) o2
  character ( len = 255 ) title

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_COMPRESS_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_COMPRESS compresses a polynomial.'

  write ( *, '(a)' ) ''
  title = '  Uncompressed P(X) ='
  call polynomial_print ( m, o, c, e, title )

  call polynomial_compress ( o, c, e, o2, c2, e2 )

  write ( *, '(a)' ) ''
  title = '  Compressed P(X) ='
  call polynomial_print ( m, o2, c2, e2, title )

  return
end
subroutine polynomial_print_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_PRINT_TEST tests POLYNOMIAL_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: o = 6

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) e(o)
  character ( len = 80 ) title

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_PRINT_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_PRINT prints a polynomial.'

  c = (/ 7.0, - 5.0, 9.0, 11.0, 0.0, - 13.0 /)
  e = (/ 1, 2, 4, 5, 12, 33 /)
  title = '  P1(X) ='

  write ( *, '(a)' ) ''
  call polynomial_print ( m, o, c, e, title )

  return
end
subroutine polynomial_sort_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_SORT_TEST tests POLYNOMIAL_SORT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: o = 6

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) e(o)
  character ( len = 80 ) title

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_SORT_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_SORT sorts a polynomial by exponent index..'

  c = (/ 0.0, 9.0, -5.0, - 13.0, 7.0, 11.0 /)
  e = (/ 12, 4, 2, 33, 1, 5 /)

  write ( *, '(a)' ) ''
  title = '  Unsorted polynomial:'
  call polynomial_print ( m, o, c, e, title )

  call polynomial_sort ( o, c, e )

  write ( *, '(a)' ) ''
  title = '  Sorted polynomial:'
  call polynomial_print ( m, o, c, e, title )

  return
end
subroutine polynomial_value_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_VALUE_TEST tests POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: nx = 2
  integer ( kind = 4 ), parameter :: o = 6

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) e(o)
  integer ( kind = 4 ) j
  real ( kind = 8 ) p(nx)
  character ( len = 80 ) title
  real ( kind = 8 ) x(m,nx)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_VALUE_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_VALUE evaluates a polynomial.'

  write ( *, '(a)' ) ''
  c = (/ 7.0, - 5.0, 9.0, 11.0, 0.0, - 13.0 /)
  e = (/ 1, 2, 4, 5, 12, 33 /)
  title = '  P(X) ='
  call polynomial_print ( m, o, c, e, title )

  x(1:m,1) = (/ 1.0, 2.0, 3.0 /)
  x(1:m,2) = (/ -2.0, 4.0, 1.0 /)
  call polynomial_value ( m, o, c, e, nx, x, p )
  write ( *, '(a)' ) ''
  do j = 1, nx
    write ( *, '(a,f10.4,a,f10.4,a,f10.4,a,g14.6)' ) &
      '  P(', x(1,j), ',', x(2,j), ',', x(3,j), ') = ', p(j)
  end do

  return
end

subroutine r8vec_permute_test ( )

!*****************************************************************************80
!
!! R8VEC_PERMUTE_TEST tests R8VEC_PERMUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ), dimension ( n ) :: p = (/ 2, 4, 5, 1, 3 /)
  real ( kind = 8 ), dimension (n) :: x = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_PERMUTE_TEST'
  write ( *, '(a)' ) '  R8VEC_PERMUTE permutes a R8VEC in place.'

  call r8vec_print ( n, x, '  Original array X[]' )

  call i4vec_print ( n, p, '  Permutation vector P[]' )

  call r8vec_permute ( n, p, x )

  call r8vec_print ( n, x, '  Permuted array X[P[]]:' )

  return
end
subroutine r8vec_print_test ( )

!*****************************************************************************80
!
!! R8VEC_PRINT_TEST tests R8VEC_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ), dimension ( n ) :: a = (/ &
    123.456D+00, 0.000005D+00, -1.0D+06, 3.14159265D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_PRINT_TEST'
  write ( *, '(a)' ) '  R8VEC_PRINT prints an R8VEC.'

  call r8vec_print ( n, a, '  The R8VEC:' )

  return
end
subroutine r8vec_uniform_ab_test ( )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_AB_TEST tests R8VEC_UNIFORM_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ), parameter :: a = 10.0D+00
  real ( kind = 8 ), parameter :: b = 20.0D+00
  real ( kind = 8 ) r(n)
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 3

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  R8VEC_UNIFORM_AB returns a random R8VEC '
  write ( *, '(a)' ) '  with entries in a given range [ A, B ]'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  For this problem:'
  write ( *, '(a,g14.6)' ) '  A = ', a
  write ( *, '(a,g14.6)' ) '  B = ', b

  seed = 123456789

  do test = 1, test_num

    write ( *, '(a)' ) ' '
    write ( *, '(a,i12)' ) '  Input SEED = ', seed

    call r8vec_uniform_ab ( n, a, b, seed, r )

    call r8vec_print ( n, r, '  Random R8VEC:' )

  end do

  return
end

