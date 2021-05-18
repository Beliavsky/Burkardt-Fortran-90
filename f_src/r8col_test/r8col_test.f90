program main

!*****************************************************************************80
!
!! MAIN is the main program for R8COL_TEST.
!
!  Discussion:
!
!    R8COL_TEST tests the R8COL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the R8COL library.'

  call i4_log_10_test ( )

  call i4vec_print_test ( )

  call r8col_find_test ( )
  call r8col_insert_test ( )
  call r8col_max_test ( )
  call r8col_mean_test ( )
  call r8col_min_test ( )
  call r8col_part_quick_a_test ( )
  call r8col_permute_test ( )
  call r8col_sort_heap_a_test ( )
  call r8col_sort_heap_index_a_test ( )
  call r8col_sort_quick_a_test ( )
  call r8col_sorted_tol_unique_test ( )
  call r8col_sorted_tol_unique_count_test ( )
  call r8col_sorted_tol_undex_test ( )
  call r8col_sortr_a_test ( )
  call r8col_sum_test ( )
  call r8col_swap_test ( )
  call r8col_to_r8vec_test ( )
  call r8col_tol_undex_test ( )
  call r8col_undex_test ( )
  call r8col_unique_count_test ( )
  call r8col_variance_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine i4_log_10_test ( )

!*****************************************************************************80
!
!! I4_LOG_10_TEST tests I4_LOG_10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 13

  integer ( kind = 4 ) i4_log_10
  integer ( kind = 4 ) test
  integer ( kind = 4 ) x
  integer ( kind = 4 ), dimension ( test_num ) :: x_test = (/ &
    0, 1, 2, 3, 9, 10, 11, 99, 101, -1, -2, -3, -9 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_LOG_10_TEST'
  write ( *, '(a)' ) '  I4_LOG_10: whole part of log base 10,'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  X, I4_LOG_10'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    x = x_test(test)
    write ( *, '( 2x, i8, i12 )' ) x, i4_log_10 ( x )
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
subroutine r8col_find_test ( )

!*****************************************************************************80
!
!! R8COL_FIND_TEST tests R8COL_FIND.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4
  integer ( kind = 4 ), parameter :: m = 3

  real ( kind = 8 ) dtab(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icol
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8vec(m)

  k = 1
  do i = 1, m
    do j = 1, n

      dtab(i,j) = real ( k, kind = 8 )

      if ( j == 3 ) then
        r8vec(i) = real ( k, kind = 8 )
      end if

      k = k + 1

    end do
  end do

  call r8col_find ( m, n, dtab, r8vec, icol )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_FIND_TEST'
  write ( *, '(a)' ) '  For an R8COL;'
  write ( *, '(a)' ) '  R8COL_FIND seeks a column matching given data.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  R8COL_FIND returns ICOL = ', icol

  return
end
subroutine r8col_insert_test ( )

!*****************************************************************************80
!
!! R8COL_INSERT_TEST tests R8COL_INSERT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n_max = 10

  real ( kind = 8 ), dimension (m,n_max) :: a = reshape ( (/ &
    2.0D+00,  6.0D+00, 10.0D+00, &
    4.0D+00,  8.0D+00, 12.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00 /), (/ m, n_max /) )
  integer ( kind = 4 ) col
  real ( kind = 8 ), dimension(m) :: r8vec1 = (/ 3.0D+00, 7.0D+00, 11.0D+00 /)
  real ( kind = 8 ), dimension(m) :: r8vec2 = (/ 3.0D+00, 4.0D+00, 18.0D+00 /)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_INSERT_TEST'
  write ( *, '(a)' ) '  For an R8COL;'
  write ( *, '(a)' ) '  R8COL_INSERT inserts new columns.'

  n = 4

  call r8col_print ( m, n, a, '  The unsorted matrix:' )

  call r8col_sort_heap_a ( m, n, a )

  call r8col_print ( m, n, a, '  The sorted matrix:' )

  call r8vec_print ( m, r8vec1, '  New column:' )

  call r8col_insert ( n_max, m, n, a, r8vec1, col )

  if ( col < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  The data was already in column ', abs ( col )
  else
    call r8col_print ( m, n, a, '  The updated matrix:' )
  end if

  call r8vec_print ( m, r8vec2, '  New column:' )

  call r8col_insert ( n_max, m, n, a, r8vec2, col )

  if ( col < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  The data was already in column ', abs ( col )
  else
    call r8col_print ( m, n, a, '  The updated matrix:' )
  end if

  return
end
subroutine r8col_max_test ( )

!*****************************************************************************80
!
!! R8COL_MAX_TEST tests R8COL_MAX;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) amax(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_MAX_TEST'
  write ( *, '(a)' ) '  R8COL_MAX computes maximums of an R8COL;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
    end do
  end do

  call r8col_print ( m, n, a, '  The array:' )

  call r8col_max ( m, n, a, amax )

  call r8vec_print ( n, amax, '  The column maximums:' )

  return
end
subroutine r8col_mean_test ( )

!*****************************************************************************80
!
!! R8COL_MEAN_TEST tests R8COL_MEAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) mean(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_MEAN_TEST'
  write ( *, '(a)' ) '  For an R8COL;'
  write ( *, '(a)' ) '  R8COL_MEAN computes means;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
    end do
  end do

  call r8col_print ( m, n, a, '  The array:' )

  call r8col_mean ( m, n, a, mean )

  call r8vec_print ( n, mean, '  The column means:' )

  return
end
subroutine r8col_min_test ( )

!*****************************************************************************80
!
!! R8COL_MIN_TEST tests R8COL_MIN;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) amin(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_MIN_TEST'
  write ( *, '(a)' ) '  R8COL_MIN computes minimums of an R8COL;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
    end do
  end do

  call r8col_print ( m, n, a, '  The array:' )

  call r8col_min ( m, n, a, amin )

  call r8vec_print ( n, amin, '  The column minimums:' )

  return
end
subroutine r8col_part_quick_a_test ( )

!*****************************************************************************80
!
!! R8COL_PART_QUICK_A_TEST tests R8COL_PART_QUICK_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ), parameter :: n = 8

  real ( kind = 8 ), dimension ( m, n ) :: a = reshape ( (/ &
     2.0D+00, 4.0D+00, &
     8.0D+00, 8.0D+00, &
     6.0D+00, 2.0D+00, &
     0.0D+00, 2.0D+00, &
    10.0D+00, 6.0D+00, &
    10.0D+00, 0.0D+00, &
     0.0D+00, 6.0D+00, &
     5.0D+00, 8.0D+00 /), (/ m, n /) )
  integer ( kind = 4 ) l
  integer ( kind = 4 ) r

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_PART_QUICK_A_TEST'
  write ( *, '(a)' ) '  For an R8COL;'
  write ( *, '(a)' ) '  R8COL_PART_QUICK_A partitions the matrix.'

  call r8col_print ( m, n, a, '  The matrix:' )

  l = 2
  r = 4
  call r8col_part_quick_a ( m, n, a, l, r )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  L = ', l
  write ( *, '(a,i4)' ) '  R = ', r

  call r8col_print ( m, n, a, '  The partitioned matrix:' )

  return
end
subroutine r8col_permute_test ( )

!*****************************************************************************80
!
!! R8COL_PERMUTE_TEST tests R8COL_PERMUTE.
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

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ), dimension (m,n) :: a = reshape ( (/ &
    11.0D+00, 21.0D+00, 31.0D+00, &
    12.0D+00, 22.0D+00, 32.0D+00, &
    13.0D+00, 23.0D+00, 33.0D+00, &
    14.0D+00, 24.0D+00, 34.0D+00, &
    15.0D+00, 25.0D+00, 35.0D+00 /), (/ m, n /) )
  integer ( kind = 4 ), dimension ( n ) :: perm = (/ 2, 4, 5, 1, 3 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_PERMUTE_TEST'
  write ( *, '(a)' ) '  R8COL_PERMUTE permutes an R8COL in place.'

  call r8col_print ( m, n, a, '  A (unpermuted)' )

  call i4vec_print ( n, perm, '  The (column) permutation vector:' )

  call r8col_permute ( m, n, perm, a )

  call r8col_print ( m, n, a, '  A (permuted)' )

  return
end
subroutine r8col_sort_heap_a_test ( )

!*****************************************************************************80
!
!! R8COL_SORT_HEAP_A_TEST tests R8COL_SORT_HEAP_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n_max = 10

  real ( kind = 8 ), dimension (m,n_max) :: a = reshape ( (/ &
    2.0D+00,  6.0D+00, 10.0D+00, &
    4.0D+00,  8.0D+00, 12.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00 /), (/ m, n_max /) )
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_SORT_HEAP_A_TEST'
  write ( *, '(a)' ) '  For an R8COL;'
  write ( *, '(a)' ) '  R8COL_SORT_HEAP_A does an ascending heap sort'

  n = 4

  call r8col_print ( m, n, a, '  The unsorted matrix:' )

  call r8col_sort_heap_a ( m, n, a )

  call r8col_print ( m, n, a, '  The sorted matrix:' )

  return
end
subroutine r8col_sort_heap_index_a_test ( )

!*****************************************************************************80
!
!! R8COL_SORT_HEAP_INDEX_A_TEST tests R8COL_SORT_HEAP_INDEX_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 15

  real ( kind = 8 ), dimension (m,n) :: a = reshape ( (/ &
    2.0D+00,  6.0D+00, 10.0D+00, &
    4.0D+00,  8.0D+00, 12.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  6.0D+00,  0.0D+00, &
    3.0D+00,  4.0D+00, 18.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  6.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    1.0D+00,  5.0D+00,  9.1D+00, &
    1.0D+00,  5.1D+00,  9.0D+00 /), (/ m, n /) )
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_SORT_HEAP_INDEX_A_TEST'
  write ( *, '(a)' ) '  R8COL_SORT_HEAP_INDEX_A computes an index vector which'
  write ( *, '(a)' ) '  ascending sorts an R8COL.'

  call r8col_transpose_print ( m, n, a, '  The unsorted R8COL (transposed):' )

  call r8col_sort_heap_index_a ( m, n, a, indx )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The implicitly sorted R8COL (transposed)'
  write ( *, '(a)' ) ' '

  do j = 1, n
    j2 = indx(j)
    write ( *, '(2x,i4,a,2x,f10.1,2x,f10.1,2x,f10.1)' ) j2, ':', a(1:m,j2)
  end do

  return
end
subroutine r8col_sort_quick_a_test ( )

!*****************************************************************************80
!
!! R8COL_SORT_QUICK_A_TEST tests R8COL_SORT_QUICK_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ), parameter :: b = 0.0D+00
  real ( kind = 8 ), parameter :: c = 10.0D+00
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_SORT_QUICK_A_TEST'
  write ( *, '(a)' ) '  For an R8COL;'
  write ( *, '(a)' ) '  R8COL_SORT_QUICK_A does a quicksort.'

  seed = 123456789

  call r8col_uniform_ab ( m, n, b, c, seed, a )

  call r8col_print ( m, n, a, '  The unsorted matrix:' )

  call r8col_sort_quick_a ( m, n, a )

  call r8col_print ( m, n, a, '  The sorted matrix:' )

  return
end
subroutine r8col_sorted_tol_unique_test ( )

!*****************************************************************************80
!
!! R8COL_SORTED_TOL_UNIQUE_TEST tests R8COL_SORTED_TOL_UNIQUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 22

  real ( kind = 8 ), dimension (m,n) :: a = reshape ( (/ &
    1.9D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    4.0D+00,  8.0D+00, 12.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  6.0D+00,  0.0D+00, &
    2.0D+00,  0.0D+00, 10.1D+00, &
    2.0D+00,  0.1D+00, 10.0D+00, &
    3.0D+00,  4.0D+00, 18.0D+00, &
    1.9D+00,  8.0D+00, 10.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  6.0D+00, 10.0D+00, &
    2.1D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    2.0D+00,  0.0D+00, 10.1D+00, &
    1.0D+00,  5.0D+00,  9.1D+00, &
    1.0D+00,  5.1D+00,  9.0D+00 /), (/ m, n /) )
  real ( kind = 8 ) tol
  integer ( kind = 4 ) unique_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_SORTED_TOL_UNIQUE_TEST'
  write ( *, '(a)' ) '  R8COL_SORTED_TOL_UNIQUE finds tolerably unique columns'
  write ( *, '(a)' ) '  in a sorted R8COL.'

  call r8col_transpose_print ( m, n, a, '  The unsorted R8COL (transposed):' )

  call r8col_sort_heap_a ( m, n, a )

  call r8col_transpose_print ( m, n, a, '  The sorted R8COL (transposed):' )

  tol = 0.25D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Using tolerance = ', tol

  call r8col_sorted_tol_unique ( m, n, a, tol, unique_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of tolerably unique columns is ', unique_num

  call r8col_transpose_print ( m, unique_num, a, &
    '  The sorted tolerably unique R8COL (transposed):' )

  return
end
subroutine r8col_sorted_tol_unique_count_test ( )

!*****************************************************************************80
!
!! R8COL_SORTED_TOL_UNIQUE_COUNT_TEST tests R8COL_SORTED_TOL_UNIQUE_COUNT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 22

  real ( kind = 8 ), dimension (m,n) :: a = reshape ( (/ &
    1.9D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    4.0D+00,  8.0D+00, 12.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  6.0D+00,  0.0D+00, &
    2.0D+00,  0.0D+00, 10.1D+00, &
    2.0D+00,  0.1D+00, 10.0D+00, &
    3.0D+00,  4.0D+00, 18.0D+00, &
    1.9D+00,  8.0D+00, 10.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  6.0D+00, 10.0D+00, &
    2.1D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    2.0D+00,  0.0D+00, 10.1D+00, &
    1.0D+00,  5.0D+00,  9.1D+00, &
    1.0D+00,  5.1D+00,  9.0D+00 /), (/ m, n /) )
  real ( kind = 8 ) tol
  integer ( kind = 4 ) unique_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_SORTED_TOL_UNIQUE_COUNT_TEST'
  write ( *, '(a)' ) '  R8COL_SORTED_TOL_UNIQUE_COUNT counts tolerably '
  write ( *, '(a)' ) '  unique columns in a sorted R8COL.'

  call r8col_transpose_print ( m, n, a, '  The unsorted R8COL (transposed):' )

  call r8col_sort_heap_a ( m, n, a )

  call r8col_transpose_print ( m, n, a, '  The sorted R8COL (transposed):' )

  tol = 0.25D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Using tolerance = ', tol

  call r8col_sorted_tol_unique_count ( m, n, a, tol, unique_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of tolerably unique columns is ', unique_num

  return
end
subroutine r8col_sorted_tol_undex_test ( )

!*****************************************************************************80
!
!! R8COL_SORTED_TOL_UNDEX_TEST tests R8COL_SORTED_TOL_UNDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 22

  real ( kind = 8 ), dimension (m,n) :: a = reshape ( (/ &
    1.9D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    4.0D+00,  8.0D+00, 12.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  6.0D+00,  0.0D+00, &
    2.0D+00,  0.0D+00, 10.1D+00, &
    2.0D+00,  0.1D+00, 10.0D+00, &
    3.0D+00,  4.0D+00, 18.0D+00, &
    1.9D+00,  8.0D+00, 10.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  6.0D+00, 10.0D+00, &
    2.1D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    2.0D+00,  0.0D+00, 10.1D+00, &
    1.0D+00,  5.0D+00,  9.1D+00, &
    1.0D+00,  5.1D+00,  9.0D+00 /), (/ m, n /) )
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: au
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n_unique
  real ( kind = 8 ) tol
  integer ( kind = 4 ), allocatable, dimension ( : ) :: undx
  integer ( kind = 4 ) xdnu(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_SORTED_TOL_UNDEX_TEST'
  write ( *, '(a)' ) '  R8COL_SORTED_TOL_UNDEX produces index vectors which '
  write ( *, '(a)' ) '  create a sorted list of the tolerably unique columns'
  write ( *, '(a)' ) '  of a sorted R8COL,'
  write ( *, '(a)' ) '  and a map from the original R8COL to the (implicit)'
  write ( *, '(a)' ) '  R8COL of sorted tolerably unique elements.'

  call r8col_transpose_print ( m, n, a, '  The unsorted R8COL (transposed):' )

  call r8col_sort_heap_a ( m, n, a )

  call r8col_transpose_print ( m, n, a, '  The sorted R8COL (transposed):' )

  tol = 0.25D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Tolerance for equality = ', tol

  call r8col_sorted_tol_unique_count ( m, n, a, tol, n_unique )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of tolerably unique columns is ', n_unique

  allocate ( au(1:m,1:n_unique) )
  allocate ( undx(1:n_unique) )

  call r8col_sorted_tol_undex ( m, n, a, n_unique, tol, undx, xdnu )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  XDNU points to the representative for each item.'
  write ( *, '(a)' ) '  UNDX selects the representatives.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I  XDNU  UNDX'
  write ( *, '(a)' ) ' '
  do i = 1, n_unique
    write ( *, '(2x,i4,2x,i4,2x,i4)' ) i, xdnu(i), undx(i)
  end do
  do i = n_unique + 1, n
    write ( *, '(2x,i4,2x,i4)'       ) i, xdnu(i)
  end do

  do j = 1, n_unique
    au(1:m,j) = a(1:m,undx(j))
  end do

  call r8col_transpose_print ( m, n_unique, au, &
    '  The tolerably unique R8COL (transposed):' )

  deallocate ( au )
  deallocate ( undx )

  return
end
subroutine r8col_sortr_a_test ( )

!*****************************************************************************80
!
!! R8COL_SORTR_A_TEST tests R8COL_SORTR_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 10
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ), parameter :: b = 0.0D+00
  real ( kind = 8 ), parameter :: c = 10.0D+00
  integer ( kind = 4 ) key
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_SORTR_A_TEST'
  write ( *, '(a)' ) '  R8COL_SORTR_A is given an array, and reorders'
  write ( *, '(a)' ) '  it so that a particular column is sorted.'
  write ( *, '(a)' ) ' '

  key = 2
  write ( *, '(a,i8)' ) '  Here, the special column is ', key

  seed = 123456789

  call r8col_uniform_ab ( m, n, b, c, seed, a )

  call r8col_print ( m, n, a, '  Unsorted array:' )

  call r8col_sortr_a ( m, n, a, key )

  call r8col_print ( m, n, a, '  Sorted array:' )

  return
end
subroutine r8col_sum_test ( )

!*****************************************************************************80
!
!! R8COL_SUM_TEST tests R8COL_SUM;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) colsum(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_SUM_TEST'
  write ( *, '(a)' ) '  For an R8COL;'
  write ( *, '(a)' ) '  R8COL_SUM computes sums;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
    end do
  end do

  call r8col_print ( m, n, a, '  The array:' )

  call r8col_sum ( m, n, a, colsum )

  call r8vec_print ( n, colsum, '  The column sums:' )

  return
end
subroutine r8col_swap_test ( )

!*****************************************************************************80
!
!! R8COL_SWAP_TEST tests R8COL_SWAP;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) icol1
  integer ( kind = 4 ) icol2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_SWAP_TEST'
  write ( *, '(a)' ) '  R8COL_SWAP swaps two columns of an R8COL;'

  call r8col_indicator ( m, n, a )

  call r8col_print ( m, n, a, '  The array:' )

  icol1 = 1
  icol2 = 3

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8,a,i8)' ) '  Swap columns ', icol1, ' and ', icol2

  call r8col_swap ( m, n, a, icol1, icol2 )

  call r8col_print ( m, n, a, '  The updated matrix:' )

  return
end
subroutine r8col_to_r8vec_test ( )

!*****************************************************************************80
!
!! R8COL_TO_R8VEC_TEST tests R8COL_TO_R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(m*n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_TO_R8VEC_TEST'
  write ( *, '(a)' ) '  R8COL_TO_R8VEC converts an array of columns to a vector.'
  write ( *, '(a)' ) ' '

  do i = 1, m
    do j = 1, n
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call r8col_print ( m, n, a, '  The array of columns:' )

  call r8col_to_r8vec ( m, n, a, x )

  call r8vec_print ( m*n, x, '  The resulting vector of columns:' )

  return
end
subroutine r8col_tol_undex_test ( )

!*****************************************************************************80
!
!! R8COL_TOL_UNDEX_TEST tests R8COL_TOL_UNDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 22

  real ( kind = 8 ), dimension (m,n) :: a = reshape ( (/ &
    1.9D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    4.0D+00,  8.0D+00, 12.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  6.0D+00,  0.0D+00, &
    2.0D+00,  0.0D+00, 10.1D+00, &
    2.0D+00,  0.1D+00, 10.0D+00, &
    3.0D+00,  4.0D+00, 18.0D+00, &
    1.9D+00,  8.0D+00, 10.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  6.0D+00, 10.0D+00, &
    2.1D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    2.0D+00,  0.0D+00, 10.1D+00, &
    1.0D+00,  5.0D+00,  9.1D+00, &
    1.0D+00,  5.1D+00,  9.0D+00 /), (/ m, n /) )
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: au
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n_unique
  real ( kind = 8 ) tol
  integer ( kind = 4 ), allocatable, dimension ( : ) :: undx
  integer ( kind = 4 ), allocatable, dimension ( : ) :: xdnu

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_TOL_UNDEX_TEST'
  write ( *, '(a)' ) '  R8COL_TOL_UNDEX produces index vectors which '
  write ( *, '(a)' ) '  create a sorted list of the tolerably unique columns'
  write ( *, '(a)' ) '  of an R8COL,'
  write ( *, '(a)' ) '  and a map from the original R8COL to the (implicit)'
  write ( *, '(a)' ) '  R8COL of sorted tolerably unique elements.'

  call r8col_transpose_print ( m, n, a, '  The unsorted R8COL (transposed):' )

  tol = 0.25D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Tolerance for equality = ', tol

  call r8col_tol_unique_count ( m, n, a, tol, n_unique )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of tolerably unique columns is ', n_unique

  allocate ( au(1:m,1:n_unique) )
  allocate ( undx(1:n_unique) )
  allocate ( xdnu(1:n) )

  call r8col_tol_undex ( m, n, a, n_unique, tol, undx, xdnu )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  XDNU points to the representative for each item.'
  write ( *, '(a)' ) '  UNDX selects the representatives.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I  XDNU  UNDX'
  write ( *, '(a)' ) ' '
  do i = 1, n_unique
    write ( *, '(2x,i4,2x,i4,2x,i4)' ) i, xdnu(i), undx(i)
  end do
  do i = n_unique + 1, n
    write ( *, '(2x,i4,2x,i4)'       ) i, xdnu(i)
  end do

  do j = 1, n_unique
    au(1:m,j) = a(1:m,undx(j))
  end do

  call r8col_transpose_print ( m, n_unique, au, &
    '  The tolerably unique R8COL (transposed):' )

  deallocate ( au )
  deallocate ( undx )
  deallocate ( xdnu )

  return
end
subroutine r8col_undex_test ( )

!*****************************************************************************80
!
!! R8COL_UNDEX_TEST tests R8COL_UNDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 15

  real ( kind = 8 ), dimension (m,n) :: a = reshape ( (/ &
    2.0D+00,  6.0D+00, 10.0D+00, &
    4.0D+00,  8.0D+00, 12.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  6.0D+00,  0.0D+00, &
    3.0D+00,  4.0D+00, 18.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  6.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    1.0D+00,  5.0D+00,  9.1D+00, &
    1.0D+00,  5.1D+00,  9.0D+00 /), (/ m, n /) )
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: au
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n_unique
  integer ( kind = 4 ), allocatable, dimension ( : ) :: undx
  integer ( kind = 4 ) xdnu(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_UNDEX_TEST'
  write ( *, '(a)' ) '  R8COL_UNDEX produces index vectors which create a sorted'
  write ( *, '(a)' ) '  list of the unique columns of an (unsorted) R8COL,'
  write ( *, '(a)' ) '  and a map from the original R8COL to the (implicit)'
  write ( *, '(a)' ) '  R8COL of sorted unique elements.'

  call r8col_transpose_print ( m, n, a, '  The R8COL (transposed):' )

  call r8col_unique_count ( m, n, a, n_unique )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of unique columns is ', n_unique

  allocate ( au(1:m,1:n_unique) )
  allocate ( undx(1:n_unique) )

  call r8col_undex ( m, n, a, n_unique, undx, xdnu )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  XDNU points to the representative for each item.'
  write ( *, '(a)' ) '  UNDX selects the representatives.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I  XDNU  UNDX'
  write ( *, '(a)' ) ' '
  do i = 1, n_unique
    write ( *, '(2x,i4,2x,i4,2x,i4)' ) i, xdnu(i), undx(i)
  end do
  do i = n_unique + 1, n
    write ( *, '(2x,i4,2x,i4)'       ) i, xdnu(i)
  end do

  do j = 1, n_unique
    au(1:m,j) = a(1:m,undx(j))
  end do

  call r8col_transpose_print ( m, n_unique, au, &
    '  The Unique R8COL (transposed):' )

  deallocate ( au )
  deallocate ( undx )

  return
end
subroutine r8col_unique_count_test ( )

!*****************************************************************************80
!
!! R8COL_UNIQUE_COUNT_TEST tests R8COL_UNIQUE_COUNT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 15

  real ( kind = 8 ), dimension (m,n) :: a = reshape ( (/ &
    2.0D+00,  6.0D+00, 10.0D+00, &
    4.0D+00,  8.0D+00, 12.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  6.0D+00,  0.0D+00, &
    3.0D+00,  4.0D+00, 18.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, &
    0.0D+00,  6.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    3.0D+00,  7.0D+00, 11.0D+00, &
    2.0D+00,  0.0D+00, 10.0D+00, &
    2.0D+00,  6.0D+00, 10.0D+00, &
    1.0D+00,  5.0D+00,  9.0D+00, &
    1.0D+00,  5.0D+00,  9.1D+00, &
    1.0D+00,  5.1D+00,  9.0D+00 /), (/ m, n /) )
  integer ( kind = 4 ) unique_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_UNIQUE_COUNT_TEST'
  write ( *, '(a)' ) '  R8COL_UNIQUE_COUNT counts unique columns'
  write ( *, '(a)' ) '  in an unsorted R8COL.'

  call r8col_transpose_print ( m, n, a, '  The R8COL (transposed):' )

  call r8col_unique_count ( m, n, a, unique_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of unique columns is ', unique_num

  return
end
subroutine r8col_variance_test ( )

!*****************************************************************************80
!
!! R8COL_VARIANCE_TEST tests R8COL_VARIANCE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) variance(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8COL_VARIANCE_TEST'
  write ( *, '(a)' ) '  R8COL_VARIANCE computes variances of an R8COL;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
    end do
  end do

  call r8col_print ( m, n, a, '  The array:' )

  call r8col_variance ( m, n, a, variance )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Column       variance:'
  write ( *, '(a)' ) ' '

  do j = 1, n
    write ( *, '(2x,i8,2x,f10.4)' ) j, variance(j)
  end do

  return
end
