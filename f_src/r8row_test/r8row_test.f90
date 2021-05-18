program main

!*****************************************************************************80
!
!! MAIN is the main program for R8ROW_TEST.
!
!  Discussion:
!
!    R8ROW_TEST tests the R8ROW library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the R8ROW library.'

  call i4_log_10_test ( )

  call i4mat_print_test ( )
  call i4mat_print_some_test ( )

  call i4vec_print_test ( )

  call r8row_compare_test ( )
  call r8row_indicator_test ( )
  call r8row_max_test ( )
  call r8row_mean_test ( )
  call r8row_min_test ( )
  call r8row_part_quick_a_test ( )
  call r8row_print_test ( )
  call r8row_print_some_test ( )
  call r8row_running_average_test ( )
  call r8row_running_sum_test ( )
  call r8row_sort_heap_a_test ( )
  call r8row_sort_heap_index_a_test ( )
  call r8row_sort_quick_a_test ( )
  call r8row_sum_test ( )
  call r8row_swap_test ( )
  call r8row_to_r8vec_test ( )
  call r8row_transpose_print_test ( )
  call r8row_transpose_print_some_test ( );
  call r8row_uniform_ab_test ( )
  call r8row_variance_test ( )

  call r8vec_print_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_TEST'
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
subroutine i4mat_print_test ( )

!*****************************************************************************80
!
!! I4MAT_PRINT_TEST tests I4MAT_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4MAT_PRINT_TEST'
  write ( *, '(a)' ) '  I4MAT_PRINT prints an I4MAT.'

  do j = 1, n
    do i = 1, m
      a(i,j) = i * 10 + j
    end do
  end do

  call i4mat_print ( m, n, a, '  The matrix:' )

  return
end
subroutine i4mat_print_some_test ( )

!*****************************************************************************80
!
!! I4MAT_PRINT_SOME_TEST tests I4MAT_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4MAT_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  I4MAT_PRINT_SOME prints some of an I4MAT.'

  do j = 1, n
    do i = 1, m
      a(i,j) = i * 10 + j
    end do
  end do

  call i4mat_print_some ( m, n, a, 2, 1, 4, 2, &
    '  The I4MAT, rows 2:4, cols 1:2:' );

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
subroutine r8row_compare_test ( )

!*****************************************************************************80
!
!! R8ROW_COMPARE_TEST tests R8ROW_COMPARE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  integer ( kind = 4 ), allocatable :: c(:,:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8ROW_COMPARE_TEST'
  write ( *, '(a)' ) '  R8ROW_COMPARE compares rows of an R8ROW,'
  write ( *, '(a)' ) '  returning -1, 0 or +1 for comparison.'

  m = 6
  n = 5
 
  allocate ( a(1:m,1:n) )
  do j = 1, n
    do i = 1, m
      a(i,j) = mod ( i + j, 3 )
    end do
  end do

  call r8row_print ( m, n, a, '  Matrix A:' )

  allocate ( c(1:m,1:m) )
  do j = 1, m
    do i = 1, m
      call r8row_compare ( m, n, a, i, j, value )
      c(i,j) = value
    end do
  end do

  call i4mat_print ( m, m, c, '  C(I,J) = Row I compare Row J:' )

  deallocate ( a )
  deallocate ( c )

  return
end
subroutine r8row_indicator_test ( )

!*****************************************************************************80
!
!! R8ROW_INDICATOR_TEST tests R8ROW_INDICATOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_INDICATOR_TEST'
  write ( *, '(a)' ) '  R8ROW_INDICATOR returns an R8ROW indicator matrix.'

  call r8row_indicator ( m, n, a )

  call r8row_print ( m, n, a, '  The indicator matrix:' )

  return
end
subroutine r8row_max_test ( )

!*****************************************************************************80
!
!! R8ROW_MAX_TEST tests R8ROW_MAX.
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
  real ( kind = 8 ) amax(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_MAX_TEST'
  write ( *, '(a)' ) '  R8ROW_MAX computes row maximums of an R8ROW.'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
    end do
  end do

  call r8row_print ( m, n, a, '  The original matrix:' )

  call r8row_max ( m, n, a, amax )

  call r8vec_print ( m, amax, '  The row maximums:' )

  return
end
subroutine r8row_mean_test ( )

!*****************************************************************************80
!
!! R8ROW_MEAN_TEST tests R8ROW_MEAN.
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
  real ( kind = 8 ) mean(m)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_MEAN_TEST'
  write ( *, '(a)' ) '  R8ROW_MEAN computes row means of an R8ROW.'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
    end do
  end do

  call r8row_print ( m, n, a, '  The original matrix:' )

  call r8row_mean ( m, n, a, mean )

  call r8vec_print ( m, mean, '  The row means:' )

  return
end
subroutine r8row_min_test ( )

!*****************************************************************************80
!
!! R8ROW_MIN_TEST tests R8ROW_MIN;
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
  real ( kind = 8 ) amin(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_MIN_TEST'
  write ( *, '(a)' ) '  R8ROW_MIN computes row minimums of an R8ROW.'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
    end do
  end do

  call r8row_print ( m, n, a, '  The original matrix:' )

  call r8row_min ( m, n, a, amin )

  call r8vec_print ( m, amin, '  The row minimums:' )

  return
end
subroutine r8row_part_quick_a_test ( )

!*****************************************************************************80
!
!! R8ROW_PART_QUICK_A_TEST tests R8ROW_PART_QUICK_A.
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

  integer ( kind = 4 ), parameter :: m = 8
  integer ( kind = 4 ), parameter :: n = 2

  real ( kind = 8 ), dimension ( m, n ) :: a = reshape ( (/ &
     2.0D+00, 8.0D+00, 6.0D+00, 0.0D+00, 10.0D+00, &
    10.0D+00, 0.0D+00, 5.0D+00, &
     4.0D+00, 8.0D+00, 2.0D+00, 2.0D+00,  6.0D+00, &
     0.0D+00, 6.0D+00, 8.0D+00 /), (/ m, n /) )
  integer ( kind = 4 ) l
  integer ( kind = 4 ) r

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_PART_QUICK_A_TEST'
  write ( *, '(a)' ) '  R8ROW_PART_QUICK_A partitions an R8ROW matrix.'

  call r8row_print ( m, n, a, '  The matrix:' )

  call r8row_part_quick_a ( m, n, a, l, r )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  L = ', l
  write ( *, '(a,i4)' ) '  R = ', r

  call r8row_print ( m, n, a, '  The partitioned matrix:' )

  return
end
subroutine r8row_print_test ( )

!*****************************************************************************80
!
!! R8ROW_PRINT_TEST tests R8ROW_PRINT.
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

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_PRINT_TEST'
  write ( *, '(a)' ) '  R8ROW_PRINT prints an R8ROW.'

  do j = 1, n
    do i = 1, m
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call r8row_print ( m, n, a, '  The R8ROW:' )

  return
end
subroutine r8row_print_some_test ( )

!*****************************************************************************80
!
!! R8ROW_PRINT_SOME_TEST tests R8ROW_PRINT_SOME.
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

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8ROW_PRINT_SOME prints some of an R8ROW.'

  do j = 1, n
    do i = 1, m
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call r8row_print_some ( m, n, a, 2, 1, 4, 2, &
    '  The R8ROW, rows 2:4, cols 1:2:' )

  return
end
subroutine r8row_running_average_test ( )

!*****************************************************************************80
!
!! R8ROW_RUNNING_AVERAGE_TEST tests R8ROW_RUNNING_AVERAGE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:,:)
  real ( kind = 8 ), allocatable :: s(:,:)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8ROW_RUNNING_AVERAGE_TEST'
  write ( *, '(a)' ) '  R8ROW_RUNNING_AVERAGE returns M sets of running averages'
  write ( *, '(a)' ) '  of an MxN R8ROW.'

  m = 5
  n = 10
  a = -5.0D+00
  b = +10.0D+00
  seed = 123456789

  allocate ( r(1:m,1:n) )
  call r8row_uniform_ab ( m, n, a, b, seed, r )

  call r8row_print ( m, n, r, '  Random R8ROW:' )

  allocate ( s(1:m,1:n+1) )
  call r8row_running_average ( m, n, r, s )

  call r8row_print ( m, n + 1, s, '  Running averages:' )

  deallocate ( r )
  deallocate ( s )

  return
end
subroutine r8row_running_sum_test ( )

!*****************************************************************************80
!
!! R8ROW_RUNNING_SUM_TEST tests R8ROW_RUNNING_SUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:,:)
  real ( kind = 8 ), allocatable :: s(:,:)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8ROW_RUNNING_SUM_TEST'
  write ( *, '(a)' ) '  R8ROW_RUNNING_SUM returns the M running sums of an R8ROW.'

  m = 5
  n = 10
  a = -5.0D+00
  b = +10.0D+00
  seed = 123456789

  allocate ( r(1:m,1:n) )
  call r8row_uniform_ab ( m, n, a, b, seed, r )

  call r8row_print ( m, n, r, '  Random R8ROW:' )

  allocate ( s(1:m,1:n+1) )
  call r8row_running_sum ( m, n, r, s )

  call r8row_print ( m, n + 1, s, '  Running sums:' )

  deallocate ( r )
  deallocate ( s )

  return
end
subroutine r8row_sort_heap_a_test ( )

!*****************************************************************************80
!
!! R8ROW_SORT_HEAP_A_TEST tests R8ROW_SORT_HEAP_A.
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

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ), dimension (m,n) :: a = reshape ( (/ &
     2.0D+00,  4.0D+00, 1.0D+00,  3.0D+00, &
     6.0D+00,  8.0D+00, 5.0D+00,  7.0D+00, &
    10.0D+00, 12.0D+00, 9.0D+00, 11.0D+00 /), (/ m, n /) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_SORT_HEAP_A_TEST'
  write ( *, '(a)' ) '  R8ROW_SORT_HEAP_A ascending heap sorts of an R8ROW.'

  call r8row_print ( m, n, a, '  The unsorted matrix:' )

  call r8row_sort_heap_a ( m, n, a )

  call r8row_print ( m, n, a, '  The sorted matrix:' )

  return
end
subroutine r8row_sort_heap_index_a_test ( )

!*****************************************************************************80
!
!! R8ROW_SORT_HEAP_INDEX_A_TEST tests R8ROW_SORT_HEAP_INDEX_A.
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

  integer ( kind = 4 ), parameter :: m = 15
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ), dimension (m,n) :: a = reshape ( (/ &
    2.0D+00,  4.0D+00,  1.0D+00,  3.0D+00,  2.0D+00, &
    3.0D+00,  0.0D+00,  0.0D+00,  2.0D+00,  3.0D+00, &
    2.0D+00,  2.0D+00,  1.0D+00,  1.0D+00,  1.0D+00, &
    6.0D+00,  8.0D+00,  5.0D+00,  7.0D+00,  6.0D+00, &
    4.0D+00,  0.0D+00,  6.0D+00,  6.0D+00,  7.0D+00, &
    0.0D+00,  6.0D+00,  5.0D+00,  5.0D+00,  5.1D+00, &
   10.0D+00, 12.0D+00,  9.0D+00, 11.0D+00,  0.0D+00, &
   18.0D+00,  0.0D+00, 10.0D+00, 10.0D+00, 11.0D+00, &
   10.0D+00, 10.0D+00,  9.0D+00,  9.1D+00,  9.0D+00 /), (/ m, n /) )
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) indx(m)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_SORT_HEAP_INDEX_A_TEST'
  write ( *, '(a)' ) '  R8ROW_SORT_HEAP_INDEX_A computes an index vector which'
  write ( *, '(a)' ) '  ascending sorts an R8ROW.'

  call r8row_transpose_print ( m, n, a, '  The unsorted R8ROW:' )

  call r8row_sort_heap_index_a ( m, n, a, indx )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The implicitly sorted R8ROW'
  write ( *, '(a)' ) ' '

  do i = 1, m
    i2 = indx(i)
    write ( *, '(2x,i4,a,2x,f10.1,2x,f10.1,2x,f10.1)' ) i2, ':', a(i2,1:n)
  end do

  return
end
subroutine r8row_sort_quick_a_test ( )

!*****************************************************************************80
!
!! R8ROW_SORT_QUICK_A_TEST tests R8ROW_SORT_QUICK_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2012
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
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_SORT_QUICK_A_TEST'
  write ( *, '(a)' ) '  R8ROW_SORT_QUICK_A does a quicksort of an R8ROW.'

  seed = 123456789

  call r8row_uniform_ab ( m, n, b, c, seed, a )

  call r8row_print ( m, n, a, '  The unsorted matrix:' )

  call r8row_sort_quick_a ( m, n, a )

  call r8row_print ( m, n, a, '  The sorted matrix:' )

  return
end
subroutine r8row_sum_test ( )

!*****************************************************************************80
!
!! R8ROW_SUM_TEST tests R8ROW_SUM;
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
  real ( kind = 8 ) rowsum(m)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_SUM_TEST'
  write ( *, '(a)' ) '  R8ROW_SUM computes row sums of an R8ROW;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
    end do
  end do

  call r8row_print ( m, n, a, '  The original matrix:' )

  call r8row_sum ( m, n, a, rowsum )

  call r8vec_print ( m, rowsum, '  The row sums:' )

  return
end
subroutine r8row_swap_test ( )

!*****************************************************************************80
!
!! R8ROW_SWAP_TEST tests R8ROW_SWAP;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 September 2005
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
  integer ( kind = 4 ) row1
  integer ( kind = 4 ) row2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_SWAP_TEST'
  write ( *, '(a)' ) '  R8ROW_SWAP swaps two rows of an R8ROW;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
    end do
  end do

  call r8row_print ( m, n, a, '  The original matrix:' )

  row1 = 1
  row2 = 3

  write ( *, '(a)' ) ' '
  write ( *, '(a,i3,a,i3)' ) '  Swap rows ', row1, ' and ', row2

  call r8row_swap ( m, n, a, row1, row2 )

  call r8row_print ( m, n, a, '  The modified matrix:' )

  return
end
subroutine r8row_to_r8vec_test ( )

!*****************************************************************************80
!
!! R8ROW_TO_R8VEC_TEST tests R8ROW_TO_R8VEC.
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
  write ( *, '(a)' ) 'R8ROW_TO_R8VEC_TEST'
  write ( *, '(a)' ) '  R8ROW_TO_R8VEC converts an R8ROW into an R8VEC.'

  do i = 1, m
    do j = 1, n
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call r8row_print ( m, n, a, '  The array of rows:' )

  call r8row_to_r8vec ( m, n, a, x )

  call r8vec_print ( m*n, x, '  The resulting vector of rows:' )

  return
end
subroutine r8row_transpose_print_test ( )

!*****************************************************************************80
!
!! R8ROW_TRANSPOSE_PRINT_TEST tests R8ROW_TRANSPOSE_PRINT;
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

  integer ( kind = 4 ), parameter :: m = 7
  integer ( kind = 4 ), parameter :: n = 12

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_TRANSPOSE_PRINT_TEST'
  write ( *, '(a)' ) '  R8ROW_TRANSPOSE_PRINT prints a R8ROW,'
  write ( *, '(a)' ) '  transposed.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix row order M =    ', m
  write ( *, '(a,i8)' ) '  Matrix column order N = ', n
!
!  Set the matrix.
!
  do i = 1, m
    do j = 1, n
      a(i,j) = real ( i * 100 + j, kind = 8 )
    end do
  end do

  call r8row_transpose_print ( m, n, a, '  The transposed matrix A:' )

  return
end
subroutine r8row_transpose_print_some_test ( )

!*****************************************************************************80
!
!! R8ROW_TRANSPOSE_PRINT_SOME_TEST tests R8ROW_TRANSPOSE_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_TRANSPOSE_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8ROW_TRANSPOSE_PRINT_SOME prints some of an R8ROW,'
  write ( *, '(a)' ) '  transposed.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Matrix row order M =    ', m
  write ( *, '(a,i8)' ) '  Matrix column order N = ', n

  call r8row_indicator ( m, n, a )

  call r8row_print ( m, n, a, '  The matrix A:' )

  call r8row_transpose_print_some ( m, n, a, 1, 2, 3, 3, &
    '  The transposed matrix A, rows 1:3, cols 2:3:' )

  return
end
subroutine r8row_uniform_ab_test ( )

!*****************************************************************************80
!
!! R8ROW_UNIFORM_AB_TEST tests R8ROW_UNIFORM_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ), dimension (m,n) :: a
  real ( kind = 8 ), parameter :: b = 2.0D+00
  real ( kind = 8 ), parameter :: c = 10.0D+00
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  R8ROW_UNIFORM_AB sets an R8ROW to random values in [A,B].'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  call r8row_uniform_ab ( m, n, b, c, seed, a )
!
!  Print out the matrix to be inverted.
!
  call r8row_print ( m, n, a, '  The random matrix:' )

  return
end
subroutine r8row_variance_test ( )

!*****************************************************************************80
!
!! R8ROW_VARIANCE_TEST tests R8ROW_VARIANCE.
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
  real ( kind = 8 ) variance(m)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ROW_VARIANCE_TEST'
  write ( *, '(a)' ) '  R8ROW_VARIANCE computes variances of an R8ROW.'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
    end do
  end do

  call r8row_print ( m, n, a, '  The original matrix:' )

  call r8row_variance ( m, n, a, variance )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Row variances:'
  write ( *, '(a)' ) ' '
  do i = 1, m
    write ( *, '(2x,i3,3x,f10.4)' ) i, variance(i)
  end do

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


