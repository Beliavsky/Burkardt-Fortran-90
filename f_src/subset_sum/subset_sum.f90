subroutine backup_one ( n, u, t )

!*****************************************************************************80
!
!! BACKUP_ONE seeks the last 1 in the subarray U(1:T-1).
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    15 July 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the full size of the U array.
!
!    Input, integer ( kind = 4 ) U(N), the array to be checked.
!
!    Input/output, integer ( kind = 4 ) T; on input a value between 1 and N; 
!    on output, the highest index in U, between 1 and TOLD-1,
!    for which U is 1.  If no such value is found, T is -1.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) t
  integer ( kind = 4 ) u(n)

  do i = t - 1, 1, -1
    if ( u(i) == 1 ) then
      t = i
      return
    end if
  end do

  t = -1;

  return
end
subroutine i4_to_digits_binary ( i, n, c )

!*****************************************************************************80
!
!! I4_TO_DIGITS_BINARY produces the binary digits of an I4.
!
!  Discussion:
!
!    An I4 is an integer.
!
!  Example:
!
!     I    N     C               Binary
!    --  ---   ---         ------------
!     0    1   0                      0
!     0    2   0, 0                  00
!     1    3   1, 0, 0              100
!     2    3   0, 1, 0              010
!     3    3   1, 1, 0              011
!     4    3   0, 0, 1              100
!     8    3   0, 0, 0           (1)000
!     8    5   0, 0, 0, 1, 0      01000
!    -8    5   0, 0, 0, 1, 0  (-) 01000
!
!     0    3   0, 0, 0
!     1    3   1, 0, 0
!     2    3   0, 1, 0
!     3    3   1, 1, 0
!     4    3   0, 0, 1
!     5    3   1, 0, 1
!     6    3   0, 1, 1
!     7    3   1, 1, 1
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    09 December 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, an integer to be represented.
!
!    Input, integer ( kind = 4 ) N, the number of binary digits to produce.
!
!    Output, integer ( kind = 4 ) C(N), the first N binary digits of I,
!    with C(1) being the units digit.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) c(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_copy
  integer ( kind = 4 ) j

  i_copy = abs ( i )

  do j = 1, n

    c(j) = mod ( i_copy, 2 )
    i_copy = i_copy / 2

  end do

  return
end
subroutine subset_sum_count ( n, w, t, ind_min, ind_max, solution_num )

!*****************************************************************************80
!
!! SUBSET_SUM_COUNT counts solutions to the subset sum problem in a range.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    09 December 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the set.
!
!    Input, integer ( kind = 4 ) W(N), a set of weights.  The length of this
!    array must be no more than 31.
!
!    Input, integer ( kind = 4 ) T, the target value.
!
!    Input, integer ( kind = 4 ) IND_MIN, IND_MAX, the lower and upper
!    limits to be searched.  0 <= IND_MIN <= IND_MAX <= (2^N)-1.
!
!    Output, integer ( kind = 4 ) SOLUTION_NUM, the number of distinct
!    solutions of the subset sum problem found within the given range.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) c(n)
  integer ( kind = 4 ) ind
  integer ( kind = 4 ) ind_max
  integer ( kind = 4 ) ind_max2
  integer ( kind = 4 ) ind_min
  integer ( kind = 4 ) ind_min2
  integer ( kind = 4 ) solution_num
  integer ( kind = 4 ) t
  integer ( kind = 4 ) w(n)
!
!  Check the data.
!
  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SUBSET_SUM_COUNT - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    stop
  end if

  if ( 31 < n ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SUBSET_SUM_COUNT - Fatal error!'
    write ( *, '(a)' ) '  31 < N.'
    stop
  end if

  ind_min2 = max ( ind_min, 0 )
  ind_max2 = min ( ind_max, ( 2 ** n ) - 1 )
!
!  Run through the range.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Searching from IND_MIN = ', ind_min2
  write ( *, '(a,i8)' ) '  through IND_MAX = ', ind_max2

  solution_num = 0

  do ind = ind_min2, ind_max2
!
!  Convert INDEX into vector of indices in W.
!
    call i4_to_digits_binary ( ind, n, c )
!
!  If the sum of those weights matches the target, return combination.
!
    if ( dot_product ( c, w ) == t ) then
      solution_num = solution_num + 1
    end if

  end do

  return
end
subroutine subset_sum_count_test ( n, w, t, ind_min, ind_max )

!*****************************************************************************80
!
!! SUBSET_SUM_COUNT_TEST tests SUBSET_SUM_COUNT.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    10 November 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of weights.
!
!    Input, integer ( kind = 4 ) W(N), a set of weights.  The length of this
!    array must be no more than 31.
!
!    Input, integer ( kind = 4 ) T, the target value.
!
!    Input, integer ( kind = 4 ) IND_MIN, IND_MAX, the lower and upper
!    limits to be searched.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ind_max
  integer ( kind = 4 ) ind_min
  integer ( kind = 4 ) solution_num
  integer ( kind = 4 ) t
  integer ( kind = 4 ) w(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SUBSET_SUM_COUNT_TEST:'
  write ( *, '(a)' ) '  SUBSET_SUM_COUNT_TEST counts solutions'
  write ( *, '(a)' ) '  to the subset sum problem.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Target value T = ', t
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   I       W(I)'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i2,2x,i8)' ) i, w(i)
  end do

  call subset_sum_count ( n, w, t, ind_min, ind_max, solution_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of solutions = ', solution_num

  return
end
subroutine subset_sum_count_tests ( )

!*****************************************************************************80
!
!! SUBSET_SUM_COUNT_TESTS tests SUBSET_SUM_COUNT_TEST.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    10 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) ind_max
  integer ( kind = 4 ) ind_min
  integer ( kind = 4 ) n
  integer ( kind = 4 ) t
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 9
  integer ( kind = 4 ), allocatable :: w(:)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SUBSET_SUM_COUNT_TESTS:'
  write ( *, '(a)' ) '  SUBSET_SUM_COUNT_TEST calls SUBSET_SUM_COUNT with'
  write ( *, '(a)' ) '  a particular set of problem data.'
!
!  Simply count solutions.
!
  do test = 1, test_num

    if ( test == 1 ) then
      n = 8
      allocate ( w(1:n) )
      w = (/ 15, 22, 14, 26, 32, 9, 16, 8 /)
      t = 53
      ind_min = 0
      ind_max = 2 ** n - 1
    else if ( test == 2 ) then
      n = 8
      allocate ( w(1:n) )
      w = (/ 15, 22, 14, 26, 32, 9, 16, 8 /)
      t = 53
      ind_min = 68
      ind_max = 2 ** n - 1
    else if ( test == 3 ) then
      n = 8
      allocate ( w(1:n) )
      w = (/ 15, 22, 14, 26, 32, 9, 16, 8 /)
      t = 53
      ind_min = 167
      ind_max = 2 ** n - 1
    else if ( test == 4 ) then
      n = 10
      allocate ( w(1:n) )
      w = (/ 267,  493,  869,  961, 1000, 1153, 1246, 1598, 1766, 1922 /)
      t = 5842
      ind_min = 0
      ind_max = 2 ** n - 1
    else if ( test == 5 ) then
      n = 21
      allocate ( w(1:n) )
      w = (/  518533, 1037066, 2074132, 1648264, 796528, &
             1593056,  686112, 1372224,  244448, 488896, &
              977792, 1955584, 1411168,  322336, 644672, &
             1289344,   78688,  157376,  314752, 629504, &
             1259008 /)
      t = 2463098
      ind_min = 0
      ind_max = 2 ** n - 1
    else if ( test == 6 ) then
      n = 10
      allocate ( w(1:n) )
      w = (/ 41, 34, 21, 20,  8,  7,  7,  4,  3,  3 /)
      t = 50
      ind_min = 0
      ind_max = 2 ** n - 1
    else if ( test == 7 ) then
      n = 9
      allocate ( w(1:n) )
      w = (/ 81, 80, 43, 40, 30, 26, 12, 11, 9 /)
      t = 100
      ind_min = 0
      ind_max = 2 ** n - 1
    else if ( test == 8 ) then
      n = 6
      allocate ( w(1:n) )
      w = (/ 1, 2, 4, 8, 16, 32 /)
      t = 22
      ind_min = 0
      ind_max = 2 ** n - 1
    else if ( test == 9 ) then
      n = 10
      allocate ( w(1:n) )
      w = (/ 25, 27, 3, 12, 6, 15, 9, 30, 21, 19 /)
      t = 50
      ind_min = 0
      ind_max = 2 ** n - 1
    end if

    call subset_sum_count_test ( n, w, t, ind_min, ind_max )

    deallocate ( w )

  end do

  return
end
subroutine subset_sum_find ( n, w, t, ind_min, ind_max, ind, c )

!*****************************************************************************80
!
!! SUBSET_SUM_FIND seeks a subset of a set that has a given sum.
!
!  Discussion:
!
!    This function tries to compute a target value as the sum of
!    a selected subset of a given set of weights.
!
!    This function works by brute force, that is, it tries every
!    possible subset to see if it sums to the desired value.
!
!    Given N weights, every possible selection can be described by
!    one of the N-digit binary numbers from 0 to 2^N-1.
!
!    This function includes a range, which allows the user to
!    control which subsets are to be checked.  Thus, if there are
!    N weights, specifying a range of [ 0, 2^N-1] indicates that
!    all subsets should be checked.  On the other hand, this full
!    range could be broken down into smaller subranges, each of
!    which could be checked independently.
!
!    It is possible that, in the given range, there may be multiple
!    solutions of the problem.  This function will only return
!    one such solution, if found.  However, the function may be called
!    again, with an appropriate restriction of the range, to continue
!    the search for other solutions.
!
!  Example:
!
!    w = [ 1, 2, 4, 8, 16, 32 ];
!    t = 22;
!    r = [ 0, 2^6 - 1 ];
!
!    call subset_sum_find ( w, t, r, c, ind )
!
!    c = [ 2, 3, 5 ]
!    index = 22
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    09 December 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the set.
!
!    Input, integer ( kind = 4 ) W(N), a set of weights.  The length of this
!    array must be no more than 31.
!
!    Input, integer ( kind = 4 ) T, the target value.
!
!    Input, integer ( kind = 4 ) IND_MIN, IND_MAX, the lower and upper
!    limits to be searched.  0 <= IND_MIN <= IND_MAX <= (2^N)-1.
!
!    Output, integer ( kind = 4 ) IND, the index of the solution.
!    If IND is -1, no solution was found in the range.
!
!    Output, integer ( kind = 4 ) C(N), indicates the solution, assuming
!    that IND is not -1.  In that case, the sum T is made by selecting
!    those weights W(I) for which C(I) is 1.  In fact,
!    T = sum ( 1 <= I <= N ) C(I) * W(I).
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) c(n)
  integer ( kind = 4 ) ind
  integer ( kind = 4 ) ind_max
  integer ( kind = 4 ) ind_max2
  integer ( kind = 4 ) ind_min
  integer ( kind = 4 ) ind_min2
  integer ( kind = 4 ) t
  integer ( kind = 4 ) w(n)
!
!  Check the data.
!
  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SUBSET_SUM_FIND - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    stop
  end if

  if ( 31 < n ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SUBSET_SUM_FIND - Fatal error!'
    write ( *, '(a)' ) '  31 < N.'
    stop
  end if

  ind_min2 = max ( ind_min, 0 )
  ind_max2 = min ( ind_max, ( 2 ** n ) - 1 )
!
!  Run through the range.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Searching from IND_MIN = ', ind_min2
  write ( *, '(a,i8)' ) '  through IND_MAX = ', ind_max2

  do ind = ind_min2, ind_max2
!
!  Convert INDEX into vector of indices in W.
!
    call i4_to_digits_binary ( ind, n, c )
!
!  If the sum of those weights matches the target, return combination.
!
    if ( dot_product ( c, w ) == t ) then
      return
    end if

  end do

  ind = - 1

  return
end
subroutine subset_sum_find_test ( n, w, t, ind_min, ind_max, ind )

!*****************************************************************************80
!
!! SUBSET_SUM_FIND_TEST tests SUBSET_SUM_FIND.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    10 November 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of weights.
!
!    Input, integer ( kind = 4 ) W(N), a set of weights.  The length of this
!    array must be no more than 31.
!
!    Input, integer ( kind = 4 ) T, the target value.
!
!    Input, integer ( kind = 4 ) IND_MIN, IND_MAX, the lower and upper
!    limits to be searched.
!
!    Output, integer ( kind = 4 ) IND, the index of a solution, if found,
!    or the value -1 otherwise.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) c(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ind
  integer ( kind = 4 ) ind_max
  integer ( kind = 4 ) ind_min
  integer ( kind = 4 ) t
  integer ( kind = 4 ) w(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SUBSET_SUM_FIND_TEST:'
  write ( *, '(a)' ) '  SUBSET_SUM_FIND seeks a subset of W that sums to T.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Target value T = ', t
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   I       W(I)'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i2,2x,i8)' ) i, w(i)
  end do

  call subset_sum_find ( n, w, t, ind_min, ind_max, ind, c )

  if ( ind == -1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  No solution was found.'
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' )  '  Solution index = ', ind
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   I       W(I)  C(I)'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i2,2x,i8,5x,i1)' ) i, w(i), c(i)
  end do

  return
end
subroutine subset_sum_find_tests ( )

!*****************************************************************************80
!
!! SUBSET_SUM_FIND_TESTS tests SUBSET_SUM_FIND_TEST.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    18 May 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) ind
  integer ( kind = 4 ) ind_max
  integer ( kind = 4 ) ind_min
  integer ( kind = 4 ) n
  integer ( kind = 4 ) t
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 9
  integer ( kind = 4 ), allocatable :: w(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SUBSET_SUM_FIND_TESTS:'
  write ( *, '(a)' ) '  SUBSET_SUM_FIND_TEST calls SUBSET_SUM_FIND with'
  write ( *, '(a)' ) '  a particular set of problem data.'
!
!  Find individual solutions.
!
  do test = 1, test_num

    if ( test == 1 ) then
      n = 8
      allocate ( w(1:n) )
      w = (/ 15, 22, 14, 26, 32, 9, 16, 8 /)
      t = 53
      ind_min = 0
      ind_max = 2 ** n - 1
    else if ( test == 2 ) then
      n = 8
      allocate ( w(1:n) )
      w = (/ 15, 22, 14, 26, 32, 9, 16, 8 /)
      t = 53
      ind_min = ind + 1
      ind_max = 2 ** n - 1
    else if ( test == 3 ) then
      n = 8
      allocate ( w(1:n) )
      w = (/ 15, 22, 14, 26, 32, 9, 16, 8 /)
      t = 53
      ind_min = ind + 1
      ind_max = 2 ** n - 1
    else if ( test == 4 ) then
      n = 10
      allocate ( w(1:n) )
      w = (/ 267,  493,  869,  961, 1000, 1153, 1246, 1598, 1766, 1922 /)
      t = 5842
      ind_min = 0
      ind_max = 2 ** n - 1
    else if ( test == 5 ) then
      n = 21
      allocate ( w(1:n) )
      w = (/  518533, 1037066, 2074132, 1648264, 796528, &
             1593056,  686112, 1372224,  244448, 488896, &
              977792, 1955584, 1411168,  322336, 644672, &
             1289344,   78688,  157376,  314752, 629504, &
             1259008 /)
      t = 2463098
      ind_min = 0
      ind_max = 2 ** n - 1
    else if ( test == 6 ) then
      n = 10
      allocate ( w(1:n) )
      w = (/ 41, 34, 21, 20,  8,  7,  7,  4,  3,  3 /)
      t = 50
      ind_min = 0
      ind_max = 2 ** n - 1
    else if ( test == 7 ) then
      n = 9
      allocate ( w(1:n) )
      w = (/ 81, 80, 43, 40, 30, 26, 12, 11, 9 /)
      t = 100
      ind_min = 0
      ind_max = 2 ** n - 1
    else if ( test == 8 ) then
      n = 6
      allocate ( w(1:n) )
      w = (/ 1, 2, 4, 8, 16, 32 /)
      t = 22
      ind_min = 0
      ind_max = 2 ** n - 1
    else if ( test == 9 ) then
      n = 10
      allocate ( w(1:n) )
      w = (/ 25, 27, 3, 12, 6, 15, 9, 30, 21, 19 /)
      t = 50
      ind_min = 0
      ind_max = 2 ** n - 1
    end if

    call subset_sum_find_test ( n, w, t, ind_min, ind_max, ind )

    deallocate ( w )

  end do

  return
end
subroutine subset_sum_next ( s, n, v, more, u, t )
 
!*****************************************************************************80
!
!! SUBSET_SUM_NEXT seeks, one at a time, subsets of V that sum to S.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    15 July 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) S, the desired sum.
!
!    Input, integer ( kind = 4 ) N, the number of values.
!
!    Input, integer ( kind = 4 ) V(N), the values.
!    These must be nonnegative, and sorted in ascending order.  
!    Duplicate values are allowed.
!
!    Input/output, logical MORE, should be set to FALSE before the first call.
!    Thereafter, on output, MORE is TRUE if a solution is being returned,
!    and FALSE if there are no more solutions.
!
!    Input/output, integer ( kind = 4 ) U(N), should be set to 0 before the 
!    first call.  On output with MORE TRUE, U indexes the selected entries
!    of V that form a solution.
!
!    Input/output, integer ( kind = 4 ) T, should be set to 0 before the first 
!    call.  On output, if MORE is true, T is the highest index of the selected 
!    values, although this is of little interest to the user.
!
  implicit none

  integer ( kind = 4 ) n

  logical more
  integer ( kind = 4 ) s
  integer ( kind = 4 ) su
  integer ( kind = 4 ) t
  integer ( kind = 4 ) u(n)
  integer ( kind = 4 ) v(n)

  if ( .not. more ) then
  
    t = 0;
    u(1:n) = 0
    
  else
  
    more = .false.
    u(t) = 0

    call backup_one ( n, u, t )
      
    if ( t < 1 ) then
      return
    end if

    u(t) = 0
    t = t + 1
    u(t) = 1
      
  end if
    
  do

    su = dot_product ( u, v )
  
    if ( su < s .and. t < n ) then

      t = t + 1;
      u(t) = 1;        

    else if ( su == s ) then

      more = .true.
      return

    else

      u(t) = 0

      call backup_one ( n, u, t )
      
      if ( t < 1 ) then
        exit
      end if

      u(t) = 0
      t = t + 1
      u(t) = 1
      
    end if
        
  end do

  return
end
subroutine subset_sum_next_test ( s, n, v )

!*****************************************************************************80
!
!! SUBSET_SUM_NEXT_TEST tests the SUBSET_SUM_NEXT library.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    15 July 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) S, the desired sum.
!
!    Input, integer ( kind = 4 ) N, the number of values.
!
!    Input, integer ( kind = 4 ) V(N), the values.
!    These must be nonnegative, and sorted in ascending order.  
!    Duplicate values are allowed.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  logical more
  logical plus
  integer ( kind = 4 ) s
  integer ( kind = 4 ) t
  integer ( kind = 4 ) u(n)
  integer ( kind = 4 ) v(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_SUM_NEXT_TEST:'
  write ( *, '(a)' ) '  SUBSET_SUM_NEXT finds the "next" subset of the values V'
  write ( *, '(a)' ) '  which sum to the desired total S.'

  more = .false.
  u(1:n) = 0
  t = 0
  
  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Desired sum S = ', s
  write ( *, '(a,i3)' ) '  Number of targets = ', n
  write ( *, '(a)', advance = 'no' ) '  Targets:'
  do i = 1, n
    write ( *, '(1x,i6)', advance = 'no' ) v(i)
  end do
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) ''

  k = 0
  
  do
    call subset_sum_next ( s, n, v, more, u, t )
    if ( .not. more ) then
      exit
    end if
    k = k + 1
    write ( *, '(2x,i3,a,2x,i6,a)', advance = 'no' ) k, ':', s, ' = '
    plus = .false.
    do i = 1, n
      if ( u(i) /= 0 ) then
        if ( plus ) then
          write ( *, '(a)', advance = 'no' ) ' +'
        end if
        write ( *, '(i6)', advance = 'no' ) v(i)
        plus = .true.
      end if
    end do
    write ( *, '(a)' ) ''
  end do
  
  return
end
subroutine subset_sum_next_tests ( )

!*****************************************************************************80
!
!! SUBSET_SUM_NEXT_TESTS calls SUBSET_SUM_NEXT_TEST with various values.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    15 July 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ), allocatable :: v(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_SUM_NEXT_TESTS:'
  write ( *, '(a)' ) '  SUBSET_SUM_NEXT_TEST solves the subset sum problem'
  write ( *, '(a)' ) '  for specific values of S, N and V.'
  
  s = 9
  n = 5
  allocate ( v(1:n) )
  v = (/ 1, 2, 3, 5, 7 /)
  call subset_sum_next_test ( s, n, v )
  deallocate ( v )
  
  s = 8
  n = 9
  allocate ( v(1:n) )
  v = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)
  call subset_sum_next_test ( s, n, v )
  deallocate ( v )
!
!  What happens with a repeated target?
!
  s = 8
  n = 9
  allocate ( v(1:n) )
  v = (/ 1, 2, 3, 3, 5, 6, 7, 8, 9 /)
  call subset_sum_next_test ( s, n, v )
  deallocate ( v )
!
!  What happens with a target that needs all the values?
!
  s = 18
  n = 5
  allocate ( v(1:n) )
  v = (/ 1, 2, 3, 5, 7 /)
  call subset_sum_next_test ( s, n, v )
  deallocate ( v )
!
!  A larger S.
!
  s = 5842
  n = 10
  allocate ( v(1:n) )
  v = (/ 267, 493, 869, 961, 1000, 1153, 1246, 1598, 1766, 1922 /)
  call subset_sum_next_test ( s, n, v )
  deallocate ( v )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_SUM_NEXT_TESTS:'
  write ( *, '(a)' ) '  Normal end of execution.'
  
  return
end
subroutine subset_sum_table ( t, n, w, table )

!*****************************************************************************80
!
!! SUBSET_SUM_TABLE sets a subset sum table.
!
!  Discussion:
!
!    The subset sum problem seeks to construct the value T by summing a 
!    subset of the values W.
!
!    This function seeks a solution by constructing a table TABLE of length T,
!    so that TABLE(I) = J means that the sum I can be constructed, and that
!    the last member of the sum is an entry of W equal to J.
!
!  Example:
!
!    w = [ 1, 2, 4, 8, 16, 32 ];
!    t = 22;
!
!    table = subset_sum ( w, t, r )
!    table = [ 1, 2, 2, 4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 8, 8, 
!      16, 16, 16, 16, 16, 16, 16 ]
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    09 November 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) T, the target value.
!
!    Input, integer ( kind = 4 ) N, the number of weights.
!
!    Input, integer ( kind = 4 ) W(N), the weights.
!
!    Output, integer ( kind = 4 ) TABLE(T), the subset sum table.  TABLE(I) is 
!    0 if the target value I cannot be formed.  It is J if the value I can 
!    be formed, with the last term in the sum being the value J.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) t

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) table(t)
  integer ( kind = 4 ) w(n)

  table(1:t) = 0

  do i = 1, n
    do j = t - w(i), 0, -1

      if ( j == 0 ) then
        if ( table(w(i)) == 0 ) then
          table(w(i)) = w(i)
        end if
      else if ( table(j) /= 0 .and. table(j+w(i)) == 0 ) then
        table(j+w(i)) = w(i)
      end if

    end do

  end do
  
  return
end
subroutine subset_sum_table_test ( t, n, w )

!*****************************************************************************80
!
!! SUBSET_SUM_TABLE_TEST tests SUBSET_SUM_TABLE.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    10 November 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) T, the target value.
!
!    Input, integer ( kind = 4 ) N, the number of weights.
!
!    Input, integer ( kind = 4 ) W(N), a set of weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) t

  integer ( kind = 4 ) i
  integer ( kind = 4 ) list(t)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) table(t)
  integer ( kind = 4 ) w(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SUBSET_SUM_TABLE_TEST:'
  write ( *, '(a)' ) '  SUBSET_SUM_TABLE seeks a subset of W that sums to T.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Target value T = ', t
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   I       W(I)'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i2,2x,i8)' ) i, w(i)
  end do

  call subset_sum_table ( t, n, w, table )

  write ( *, '(a)' ) ''

  if ( table(t) == 0 ) then
    write ( *, '(a)' ) '  No solution was found.'
  else
    call subset_sum_table_to_list ( t, table, m, list )
    write ( *, '(i8)', advance = 'no' ) t
    write ( *, '(a)', advance = 'no' ) ' = '
    do i = 1, m
      if ( 1 < i ) then
        write ( *, '(a)', advance = 'no' ) ' + '
      end if
      write ( *, '(i8)', advance = 'no' ) list(i)
    end do
    write ( *, '(a)' ) ''
  end if

  return
end
subroutine subset_sum_table_tests ( )

!*****************************************************************************80
!
!! SUBSET_SUM_TABLE_TESTS tests SUBSET_SUM_TABLE_TEST.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    10 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) t
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 7
  integer ( kind = 4 ), allocatable :: w(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SUBSET_SUM_TABLE_TESTS:'
  write ( *, '(a)' ) '  SUBSET_SUM_TABLE_TEST calls SUBSET_SUM_TABLE with'
  write ( *, '(a)' ) '  a particular set of problem data.'
!
!  Find individual solutions.
!
  do test = 1, test_num

    if ( test == 1 ) then
      t = 53
      n = 8
      allocate ( w(1:n) )
      w = (/ 15, 22, 14, 26, 32, 9, 16, 8 /)
    else if ( test == 2 ) then
      t = 5842
      n = 10
      allocate ( w(1:n) )
      w = (/ 267,  493,  869,  961, 1000, 1153, 1246, 1598, 1766, 1922 /)
    else if ( test == 3 ) then
      t = 2463098
      n = 21
      allocate ( w(1:n) )
      w = (/  518533, 1037066, 2074132, 1648264, 796528, &
             1593056,  686112, 1372224,  244448, 488896, &
              977792, 1955584, 1411168,  322336, 644672, &
             1289344,   78688,  157376,  314752, 629504, &
             1259008 /)
    else if ( test == 4 ) then
      t = 50
      n = 10
      allocate ( w(1:n) )
      w = (/ 41, 34, 21, 20,  8,  7,  7,  4,  3,  3 /)
    else if ( test == 5 ) then
      t = 100
      n = 9
      allocate ( w(1:n) )
      w = (/ 81, 80, 43, 40, 30, 26, 12, 11, 9 /)
    else if ( test == 6 ) then
      t = 22
      n = 6
      allocate ( w(1:n) )
      w = (/ 1, 2, 4, 8, 16, 32 /)
    else if ( test == 7 ) then
      t = 50
      n = 10
      allocate ( w(1:n) )
      w = (/ 25, 27, 3, 12, 6, 15, 9, 30, 21, 19 /)
    end if

    call subset_sum_table_test ( t, n, w )

    deallocate ( w )

  end do

  return
end
subroutine subset_sum_table_to_list ( t, table, m, list )

!*****************************************************************************80
!
!! SUBSET_SUM_TABLE_TO_LIST converts a subset sum table to a list.
!
!  Discussion:
!
!    The subset sum problem seeks to construct the value T by summing a 
!    subset of the values W.
!
!    This function takes a table computed by subset_sum_table() and converts
!    it to the corresponding list of values that form the sum.
!
!  Example:
!
!    t = 22
!    n = 6
!    w = [ 1, 2, 4, 8, 16, 32 ]
!    call subset_sum ( t, n, w, table )
!    table = [ 1, 2, 2, 4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 8, 8, 
!      16, 16, 16, 16, 16, 16, 16 ]
!
!    call subset_sum_table_to_list ( t, table, m, list )
!    m = 3
!    list = [ 2, 4, 16 ]
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    10 November 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) T, the target value.
!
!    Input, integer ( kind = 4 ) TABLE(T), the subset sum table.
!
!    Output, integer ( kind = 4 ) M, the number of items in the list.
!    If M == 0, then no solution was found and the list is empty.
!
!    Output, integer ( kind = 4 ) LIST(T), contains the M items in the
!    list of weights that sum to T.
!    
  implicit none

  integer ( kind = 4 ) t

  integer ( kind = 4 ) i
  integer ( kind = 4 ) list(t)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) table(t)

  list(1:t) = 0

  i = t
  m = 0
  do while ( 0 < i )
    m = m + 1
    list(m) = table(i)
    i = i - table(i)
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
!    I don't care what you do with this code.
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
