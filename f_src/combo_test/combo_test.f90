program main

!*****************************************************************************80
!
!! MAIN is the main program for COMBO_TEST.
!
!  Discussion:
!
!    COMBO_TEST tests the COMBO library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMBO_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the COMBO library.'

  call backtrack_test ( )

  call bal_seq_check_test ( )
  call bal_seq_enum_test ( )
  call bal_seq_rank_test ( )
  call bal_seq_successor_test ( )
  call bal_seq_to_tableau_test ( )
  call bal_seq_unrank_test ( )

  call bell_numbers_test ( )

  call cycle_check_test ( )
  call cycle_to_perm_test ( )

  call dist_enum_test ( )
  call dist_next_test ( )

  call edge_check_test ( )
  call edge_degree_test ( )
  call edge_enum_test ( )

  call gray_code_check_test ( )
  call gray_code_enum_test ( )
  call gray_code_rank_test ( )
  call gray_code_successor_test ( )
  call gray_code_unrank_test ( )

  call i4_choose_test ( )
  call i4_factorial_test ( )
  call i4_fall_test ( )

  call i4vec_backtrack_test ( )
  call i4vec_dot_product_test ( )
  call i4vec_part1_test ( )
  call i4vec_part2_test ( )
  call i4vec_search_binary_a_test ( )
  call i4vec_search_binary_d_test ( )
  call i4vec_sort_insert_a_test ( )
  call i4vec_sort_insert_d_test ( )
  call i4vec_uniform_ab_test ( )

  call knapsack_01_test ( )
  call knapsack_rational_test ( )
  call knapsack_reorder_test ( )

  call ksubset_colex_check_test ( )
  call ksubset_colex_rank_test ( )
  call ksubset_colex_successor_test ( )
  call ksubset_colex_unrank_test ( )
  call ksubset_enum_test ( )
  call ksubset_lex_check_test ( )
  call ksubset_lex_rank_test ( )
  call ksubset_lex_successor_test ( )
  call ksubset_lex_unrank_test ( )
  call ksubset_revdoor_rank_test ( )
  call ksubset_revdoor_successor_test ( )
  call ksubset_revdoor_unrank_test ( )

  call marriage_test ( )

  call mountain_test ( )

  call npart_enum_test ( )

  call npart_rsf_lex_random_test ( )
  call npart_rsf_lex_rank_test ( )
  call npart_rsf_lex_successor_test ( )
  call npart_rsf_lex_unrank_test ( )

  call npart_sf_lex_successor_test ( )
  call npart_table_test ( )

  call part_enum_test ( )
  call part_rsf_check_test ( )
  call part_sf_check_test ( )
  call part_sf_conjugate_test ( )
  call part_sf_majorize_test ( )
  call part_successor_test ( )
  call part_table_test ( )

  call partn_enum_test ( )
  call partn_sf_check_test ( )
  call partn_successor_test ( )

  call partition_greedy_test ( )

  call perm_check_test ( )
  call perm_enum_test ( )
  call perm_inv_test ( )
  call perm_lex_rank_test ( )
  call perm_lex_successor_test ( )
  call perm_lex_unrank_test ( )
  call perm_mul_test ( )
  call perm_parity_test ( )
  call perm_print_test ( )
  call perm_random_test ( );
  call perm_tj_rank_test ( )
  call perm_tj_successor_test ( )
  call perm_tj_unrank_test ( )
  call perm_to_cycle_test ( )

  call pruefer_check_test ( )
  call pruefer_enum_test ( )
  call pruefer_rank_test ( )
  call pruefer_successor_test ( )
  call pruefer_to_tree_test ( )
  call pruefer_unrank_test ( )

  call queens_test ( )

  call r8_choose_test ( )
  call r8_gamma_log_test ( )

  call r8vec_backtrack_test ( )

  call rgf_check_test ( )
  call rgf_enum_test ( )
  call rgf_g_table_test ( )
  call rgf_rank_test ( )
  call rgf_successor_test ( )
  call rgf_to_setpart_test ( )
  call rgf_unrank_test ( )

  call setpart_check_test ( )
  call setpart_enum_test ( )
  call setpart_to_rgf_test ( )

  call stirling_numbers1_test ( )
  call stirling_numbers2_test ( )

  call subset_check_test ( )
  call subset_colex_rank_test ( )
  call subset_colex_successor_test ( )
  call subset_colex_unrank_test ( )
  call subset_complement_test ( )
  call subset_distance_test ( )
  call subset_enum_test ( )
  call subset_intersect_test ( )
  call subset_lex_rank_test ( )
  call subset_lex_successor_test ( )
  call subset_lex_unrank_test ( )
  call subset_random_test ( )
  call subset_union_test ( )
  call subset_weight_test ( )
  call subset_xor_test ( )

  call subsetsum_swap_test ( )

  call tableau_check_test ( )
  call tableau_enum_test ( )
  call tableau_to_bal_seq_test ( )

  call tree_check_test ( )
  call tree_enum_test ( )
  call tree_rank_test ( )
  call tree_successor_test ( )
  call tree_to_pruefer_test ( )
  call tree_unrank_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMBO_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' )  ''
  call timestamp ( )

  stop 0
end
subroutine backtrack_test ( )

!*****************************************************************************80
!
!! BACKTRACK_TEST tests BACKTRACK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 8
  integer ( kind = 4 ), parameter :: maxstack = n * n

  integer ( kind = 4 ) iarray(n)
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) istack(maxstack)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) nstack

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BACKTRACK_TEST'
  write ( *, '(a)' ) '  BACKTRACK supervises a backtrack search.'
  write ( *, '(a)' ) '  Here, we search for an arrangement of nonattacking'
  write ( *, '(a)' ) '  queens on a chessboard.'
  write ( *, '(a)' ) ''

  indx = 0

  do

    call backtrack ( n, iarray, indx, k, nstack, istack, maxstack )

    if ( indx == 1 ) then

      write ( *, '(19i4)' ) iarray(1:n)

    else if ( indx == 2 ) then

      call queens ( n, iarray, k, nstack, istack, maxstack )

    else

      exit

    end if

  end do

  return
end
subroutine bal_seq_check_test ( )

!*****************************************************************************80
!
!! BAL_SEQ_CHECK_TEST tests BAL_SEQ_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical ( kind = 4 ) check
  integer ( kind = 4 ) n
  integer ( kind = 4 ) test
  integer ( kind = 4 ) t(10)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BAL_SEQ_CHECK TEST'
  write ( *, '(a)' ) '  BAL_SEQ_CHECK checks N and T(1:2*N).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Check?   N    T(1:2*N)'
  write ( *, '(a)' ) ''
  
  do test = 1, 3

    n = 5

    if ( test == 1 ) then
      t(1:2*n) = (/ 0, 0, 1, 0, 1, 0, 0, 1, 1, 1 /)
    else if ( test == 2 ) then
      t(1:2*n) = (/ 1, 1, 0, 1, 0, 1, 1, 0, 0, 0 /)
    else if ( test == 3 ) then
      t(1:2*n) = (/ 0, 0, 1, 0, 1, 0, 0, 1, 0, 1 /)
    end if

    call bal_seq_check ( n, t, check )

    write ( *, '(6x,l2,2x,i2)', advance = 'no' ) check, n
    call i4vec_transpose_print ( n, t, '' )

  end do

  return
end
subroutine bal_seq_enum_test ( )

!*****************************************************************************80
!
!! BAL_SEQ_ENUM_TEST tests BAL_SEQ_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) bal_seq_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BAL_SEQ_ENUM_TEST'
  write ( *, '(a)' ) '  BAL_SEQ_ENUM enumerates balanced sequences of N items.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N     #'
  write ( *, '(a)' ) ''

  do n = 0, 10
    call bal_seq_enum ( n, bal_seq_num )
    write ( *, '(2x,i2,2x,i6)' ) n, bal_seq_num
  end do

  return
end
subroutine bal_seq_rank_test ( )

!*****************************************************************************80
!
!! BAL_SEQ_RANK_TEST tests BAL_SEQ_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) t(10)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BAL_SEQ_RANK_TEST'
  write ( *, '(a)' ) '  BAL_SEQ_RANK ranks a balanced sequence of N items.'

  n = 5
  t = (/ 0, 0, 1, 0, 1, 1, 0, 0, 1, 1 /)

  call bal_seq_rank ( n, t, rank )

  call i4vec_print ( 2 * n, t, '  Element to be ranked:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Computed rank: ', rank

  return
end
subroutine bal_seq_successor_test ( )

!*****************************************************************************80
!
!! BAL_SEQ_SUCCESSOR_TEST tests BAL_SEQ_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(10)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BAL_SEQ_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  BAL_SEQ_SUCCESSOR lists balanced sequences of N items, one at a time.'

  n = 5
  rank = -1

  do

    rank_old = rank

    call bal_seq_successor ( n, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(4x,i3,2x,10i2)' ) rank, t(1:2*n)

  end do

  return
end
subroutine bal_seq_to_tableau_test ( )

!*****************************************************************************80
!
!! BAL_SEQ_TO_TABLEAU_TEST tests BAL_SEQ_TO_TABLEAU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) rank
  integer ( kind = 4 ) t(2*n)
  integer ( kind = 4 ) tab(2,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BAL_SEQ_TO_TABLEAU_TEST'
  write ( *, '(a)' ) '  BAL_SEQ_TO_TABLEAU converts a balanced'
  write ( *, '(a)' ) '  sequence to a tableau;'
!
!  Pick a "random" balanced sequence.
!
  rank = 7

  call bal_seq_unrank ( rank, n, t )

  call i4vec_transpose_print ( 2 * n, t, '  Balanced sequence:' )
!
!  Convert to a tableau.
!
  call bal_seq_to_tableau ( n, t, tab )

  call i4mat_print ( 2, 4, tab, '  Tableau:' )

  return
end
subroutine bal_seq_unrank_test ( )

!*****************************************************************************80
!
!! BAL_SEQ_UNRANK_TEST tests BAL_SEQ_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) t(10)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BAL_SEQ_UNRANK_TEST'
  write ( *, '(a)' ) '  BAL_SEQ_UNRANK unranks a balanced sequence of N items.'

  rank = 21
  n = 5

  call bal_seq_unrank ( rank, n, t )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The element of rank ', rank
  write ( *, '(a)' ) ''
  write ( *, '(4x,10i2)' ) t(1:2*n)

  return
end
subroutine bell_numbers_test ( )

!*****************************************************************************80
!
!! BELL_NUMBERS_TEST tests BELL_NUMBERS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 July 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: b(:)
  integer ( kind = 4 ) bn
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BELL_NUMBERS_TEST'
  write ( *, '(a)' ) '  BELL_NUMBERS computes Bell numbers.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N        BELL(N)    BELL_NUMBERS(N)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bell_values ( n_data, n, bn )

    if ( n_data == 0 ) then
      exit
    end if

    allocate ( b(0:n) )

    call bell_numbers ( n, b )

    write ( *, '(2x,i8,2x,i12,2x,i12)' ) n, bn, b(n)

    deallocate ( b )

  end do

  return
end
subroutine cycle_check_test ( )

!*****************************************************************************80
!
!! CYCLE_CHECK_TEST tests CYCLE_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 January 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical ( kind = 4 ) check
  integer ( kind = 4 ) i
  integer ( kind = 4 ), allocatable :: indx(:)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncycle
  integer ( kind = 4 ), allocatable :: t(:)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CYCLE_CHECK TEST'
  write ( *, '(a)' ) '  CYCLE_CHECK checks a permutation in cycle form.'
  
  do test = 1, 6

    if ( test == 1 ) then
      n = 0
      ncycle = 3
      allocate ( t(1:8) )
      allocate ( indx(1:ncycle) )
      t = (/ 5, 1, 3, 8, 6, 2, 4, 7 /)
      indx = (/ 1, 4, 3 /)
    else if ( test == 2 ) then
      n = 8
      ncycle = 0
      allocate ( t(1:n) )
      allocate ( indx(1:3) )
      t = (/ 5, 1, 3, 8, 6, 2, 4, 7 /)
      indx = (/ 1, 4, 3 /)
    else if ( test == 3 ) then
      n = 8
      ncycle = 3
      allocate ( t(1:n) )
      allocate ( indx(1:ncycle) )
      t = (/ 5, 1, 3, 8, 6, 2, 4, 7 /)
      indx = (/ 1, 4, 2 /)
    else if ( test == 4 ) then
      n = 8
      ncycle = 3
      allocate ( t(1:n) )
      allocate ( indx(1:ncycle) )
      t = (/ 5, 1, 3, 12, 6, 2, 4, 7 /)
      indx = (/ 1, 4, 3 /)
    else if ( test == 5 ) then
      n = 8
      ncycle = 3
      allocate ( t(1:n) )
      allocate ( indx(1:ncycle) )
      t = (/ 5, 1, 3, 8, 5, 2, 4, 7 /)
      indx = (/ 1, 4, 3 /)
    else if ( test == 6 ) then
      n = 8
      ncycle = 3
      allocate ( t(1:n) )
      allocate ( indx(1:ncycle) )
      t = (/ 5, 1, 3, 8, 6, 2, 4, 7 /)
      indx = (/ 1, 4, 3 /)
    end if

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Permutation in cycle form:'
    write ( *, '(a,i4)' ) '  Number of cycles is ', ncycle
    write ( *, '(a)' ) ''
    jlo = 0
    do i = 1, ncycle
      write ( *, '(a)' ) '    '
      do j = jlo + 1, jlo + indx(i)
        write ( *, '(2x,i4)', advance = 'no' ) t(j)
      end do
      write ( *, '(a)' ) ''
      jlo = jlo + indx(i)
    end do

    call cycle_check ( n, ncycle, t, indx, check )
    write ( *, '(a,l)' ) '  Check = ', check
    
    deallocate ( indx )
    deallocate ( t )

  end do

  return
end
subroutine cycle_to_perm_test ( )

!*****************************************************************************80
!
!! CYCLE_TO_PERM_TEST tests CYCLE_TO_PERM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 7
  integer ( kind = 4 ), parameter :: ncycle = 3

  integer ( kind = 4 ) i
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ), dimension ( ncycle ) :: index = (/ 5, 1, 1 /)
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ), dimension ( n ) :: t = (/ 4, 2, 5, 3, 1, 6, 7 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CYCLE_TO_PERM_TEST'
  write ( *, '(a)' ) '  CYCLE_TO_PERM converts a permutation from'
  write ( *, '(a)' ) '  cycle to array form;'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Cycle form:'
  write ( *, '(a,i8)' ) '  Number of cycles is ', ncycle
  write ( *, '(a)' ) ''
  jlo = 0
  do i = 1, ncycle
    write ( *, '(4x,20i4)' ) t(jlo+1:jlo+index(i))
    jlo = jlo + index(i)
  end do
!
!  Convert the set partition back to an RGF.
!
  call cycle_to_perm ( n, ncycle, t, index, p )

  call perm_print ( n, p, '  Corresponding permutation:' )

  return
end
subroutine dist_enum_test ( )

!*****************************************************************************80
!
!! DIST_ENUM_TEST tests DIST_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) dist_num
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DIST_ENUM_TEST'
  write ( *, '(a)' ) '  DIST_ENUM enumerates distributions of N indistinguishable'
  write ( *, '(a)' ) '  objects among M distinguishable slots:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      N:      0       1       2       3       4       5'
  write ( *, '(a)' ) '   M'
  do m = 0, 10
    write ( *, '(2x,i2,a)', advance = 'no' ) m, ':  '
    do n = 0, 5
      call dist_enum ( m, n, dist_num )
      write ( *, '(2x,i6)', advance = 'no' ) dist_num
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine dist_next_test ( )

!*****************************************************************************80
!
!! DIST_NEXT_TEST tests DIST_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) idist
  integer ( kind = 4 ) leftmost
  integer ( kind = 4 ) m
  logical ( kind = 4 ) more
  integer ( kind = 4 ) num_dist
  integer ( kind = 4 ) q(k)

  m = 5
  more = .false.

  call dist_enum ( k, m, num_dist )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DIST_NEXT_TEST'
  write ( *, '(a)' ) '  DIST_NEXT produces the next'
  write ( *, '(a)' ) '  distribution of M indistinguishable'
  write ( *, '(a)' ) '  objects among K distinguishable slots.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Number of:'
  write ( *, '(a,i8)' ) '    indistinguishable objects = ', m
  write ( *, '(a,i8)' ) '    distinguishable slots =     ', k
  write ( *, '(a,i8)' ) '    distributions is            ', num_dist
  write ( *, '(a)' ) ''

  idist = 0
  leftmost = 0

  do

    call dist_next ( k, m, q, leftmost, more )

    if ( .not. more ) then
      exit
    end if

    idist = idist + 1
    write ( *, '(4x,6i5)' ) idist, q(1:k)

  end do

  return
end
subroutine edge_check_test ( )

!*****************************************************************************80
!
!! EDGE_CHECK_TEST tests EDGE_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical ( kind = 4 ) check
  integer ( kind = 4 ) edge_num
  integer ( kind = 4 ), allocatable :: edge_list(:)
  integer ( kind = 4 ), dimension ( 2 * 3 ) :: edge_list1 = (/ &
    1, 2, &
    2, 3, &
    3, 1 /)
  integer ( kind = 4 ), dimension ( 2 * 3 ) :: edge_list2 = (/ &
    1, 2, &
    2, 3, &
    3, 1 /)
  integer ( kind = 4 ), dimension ( 2 * 3 ) :: edge_list3 = (/ &
    1, 2, &
    2, 3, &
    3, 4 /)
  integer ( kind = 4 ), dimension ( 2 * 3 ) :: edge_list4 = (/ &
    1, 2, &
    2, 2, &
    3, 1 /)
  integer ( kind = 4 ), dimension ( 2 * 3 ) :: edge_list5 = (/ &
    1, 2, &
    2, 3, &
    2, 1 /)
  integer ( kind = 4 ), dimension ( 2 * 3 ) :: edge_list6 = (/ &
    1, 2, &
    2, 3, &
    3, 1 /)
  integer ( kind = 4 ) node_num
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EDGE_CHECK TEST'
  write ( *, '(a)' ) '  EDGE_CHECK checks a graph described by edges.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Check?  Nodes  Edges    EdgeList'
  write ( *, '(a)' ) ''
  
  do test = 1, 6

    if ( test == 1 ) then

      node_num = -5
      edge_num = 3
      allocate ( edge_list(2*edge_num) )
      edge_list(1:2*edge_num) = edge_list1(1:2*edge_num)

    else if ( test == 2 ) then

      node_num = 3
      edge_num = 3
      allocate ( edge_list(2*edge_num) )
      edge_list(1:2*edge_num) = edge_list2(1:2*edge_num)

      edge_num = -1

    else if ( test == 3 ) then

      node_num = 3
      edge_num = 3
      allocate ( edge_list(2*edge_num) )
      edge_list(1:2*edge_num) = edge_list3(1:2*edge_num)

    else if ( test == 4 ) then

      node_num = 3
      edge_num = 3
      allocate ( edge_list(2*edge_num) )
      edge_list(1:2*edge_num) = edge_list4(1:2*edge_num)

    else if ( test == 5 ) then

      node_num = 3
      edge_num = 3
      allocate ( edge_list(2*edge_num) )
      edge_list(1:2*edge_num) = edge_list5(1:2*edge_num)

    else if ( test == 6 ) then

      node_num = 3
      edge_num = 3
      allocate ( edge_list(2*edge_num) )
      edge_list(1:2*edge_num) = edge_list6(1:2*edge_num)

    end if

    write ( *, '(a)' ) ''
    call edge_check ( node_num, edge_num, edge_list, check )
    write ( *, '(2x,l,2x,i2,2x,i2)' ) check, node_num, edge_num
    call i4mat_print ( 2, edge_num, edge_list, '  Edge list of graph:' )

    deallocate ( edge_list )

  end do

  return
end
subroutine edge_degree_test ( )

!*****************************************************************************80
!
!! EDGE_DEGREE_TEST tests EDGE_DEGREE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d(5)
  integer ( kind = 4 ), dimension ( 2, 5 ) :: edge = reshape ( (/ &
    1, 2, &
    2, 3, &
    2, 4, &
    3, 4, &
    4, 5 /), (/ 2, 5 /) )
  integer ( kind = 4 ) edge_num
  integer ( kind = 4 ) node_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EDGE_DEGREE_TEST'
  write ( *, '(a)' ) '  EDGE_DEGREE determines the degree of each node in a graph.'

  node_num = 5
  edge_num = 5

  call i4mat_print ( 2, edge_num, edge, '  The edge array:' )

  call edge_degree ( node_num, edge_num, edge, d )

  call i4vec_print ( node_num, d, '  The degree vector:' )

  return
end
subroutine edge_enum_test ( )

!*****************************************************************************80
!
!! EDGE_ENUM_TEST tests EDGE_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) edge_num
  integer ( kind = 4 ) node_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EDGE_ENUM_TEST'
  write ( *, '(a)' ) '  EDGE_ENUM enumerates the maximum number of edges'
  write ( *, '(a)' ) '  possible in a graph of NODE_NUM nodes.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   NODE_NUM    EDGE_NUM(max)'
  write ( *, '(a)' ) ''

  do node_num = 1, 10
    call edge_enum ( node_num, edge_num )
    write ( *, '(9x,i2,6x,i6)' ) node_num, edge_num
  end do

  return
end
subroutine gray_code_check_test ( )

!*****************************************************************************80
!
!! GRAY_CODE_CHECK_TEST tests GRAY_CODE_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) t(n)
  integer ( kind = 4 ) test
  
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GRAY_CODE_CHECK TEST'
  write ( *, '(a)' ) '  GRAY_CODE_CHECK checks N and T(1:N).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Check?   N    T(1:N)'
  write ( *, '(a)' ) ''
  
  do test = 1, 3

    if ( test == 1 ) then
      t = (/ 0, 1, 1, 0, 1 /)
    else if ( test == 2 ) then
      t = (/ 1, 0, 7, 1, 0 /)
    else if ( test == 3 ) then
      t = (/ 1, 1, 1, 1, 1 /)
    end if

    call gray_code_check ( n, t, ierror )
    write ( *, '(6x,i2,2x,i2,2x,5(i2))' ) ierror, n, t(1:n)

  end do

  return
end
subroutine gray_code_enum_test ( )

!*****************************************************************************80
!
!! GRAY_CODE_ENUM_TEST tests GRAY_CODE_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ngray

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GRAY_CODE_ENUM_TEST'
  write ( *, '(a)' ) '  GRAY_CODE_ENUM enumerates Gray codes'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N    Enum(N)'
  write ( *, '(a)' ) ''
  do n = 0, 10
    call gray_code_enum ( n, ngray )
    write ( *, '(2x,i2,2x,i6)' ) n, ngray
  end do

  return
end
subroutine gray_code_rank_test ( )

!*****************************************************************************80
!
!! GRAY_CODE_RANK_TEST tests GRAY_CODE_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) rank
  integer ( kind = 4 ), dimension ( n ) :: t = (/ 1, 1, 0, 0, 0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GRAY_CODE_RANK_TEST'
  write ( *, '(a)' ) '  GRAY_CODE_RANK ranks Gray codes.'

  call gray_code_rank ( n, t, rank )

  call i4vec_print ( n, t, '  Element to be ranked:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Computed rank: ', rank

  return
end
subroutine gray_code_successor_test ( )

!*****************************************************************************80
!
!! GRAY_CODE_SUCCESSOR_TEST tests GRAY_CODE_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GRAY_CODE_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  GRAY_CODE_SUCCESSOR lists Gray codes one by one.'

  rank = -1

  do

    rank_old = rank

    call gray_code_successor ( n, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(4x,6i5)' ) rank, t(1:n)

  end do

  return
end
subroutine gray_code_unrank_test ( )

!*****************************************************************************80
!
!! GRAY_CODE_UNRANK_TEST tests GRAY_CODE_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) ngray
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) t(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GRAY_CODE_UNRANK_TEST'
  write ( *, '(a)' ) '  GRAY_CODE_UNRANK unranks a Gray code.'

  call gray_code_enum ( n, ngray )

  rank = ngray / 2

  call gray_code_unrank ( rank, n, t )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The element of rank ', rank
  write ( *, '(a)' ) ''
  write ( *, '(4x,6i5)' ) t(1:n)

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
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_CHOOSE_TEST'
  write ( *, '(a)' ) '  I4_CHOOSE computes binomial coefficients.'

  do i = -1, 5
    do j = -1, 5
      write ( *, '(3i8)' ) i, j, i4_choose ( i, j )
    end do
  end do

  return
end
subroutine i4_factorial_test ( )

!*****************************************************************************80
!
!! I4_FACTORIAL_TEST tests I4_FACTORIAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i4_factorial
  integer ( kind = 4 ) fx
  integer ( kind = 4 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_FACTORIAL_TEST:'
  write ( *, '(a)' ) '  I4_FACTORIAL evaluates the factorial function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     X       Exact F       FACTORIAL(X)'
  write ( *, '(a)' ) ''

  n = 0

  do

    call i4_factorial_values ( n, x, fx )

    if ( n == 0 ) then
      exit
    end if

    if ( x <= 0.0D+00 ) then
      cycle
    end if

    fx2 = i4_factorial ( x )

    write ( *, '(i4,2i12)' ) x, fx, fx2

  end do

  return
end
subroutine i4_fall_test ( )

!*****************************************************************************80
!
!! I4_FALL_TEST tests I4_FALL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) f1
  integer ( kind = 4 ) f2
  integer ( kind = 4 ) i4_fall
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_FALL_TEST:'
  write ( *, '(a)' ) '  I4_FALL evaluates the falling factorial function:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         M         N      Exact         I4_FALL(M,N)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call i4_fall_values ( n_data, m, n, f1 )

    if ( n_data == 0 ) then
      exit
    end if

    f2 = i4_fall ( m, n )

    write ( *, '(2x,i8,2x,i8,2x,i12,2x,i12)' ) m, n, f1, f2

  end do

  return
end
subroutine i4vec_backtrack_test ( )

!*****************************************************************************80
!
!! I4VEC_BACKTRACK_TEST tests I4VEC_BACKTRACK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxstack = 100
  integer ( kind = 4 ), parameter :: n = 8

  integer ( kind = 4 ) found_num
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) k
  integer ( kind = 4 ) ncan(n)
  integer ( kind = 4 ) nstack
  integer ( kind = 4 ) stacks(maxstack)
  integer ( kind = 4 ) t
  integer ( kind = 4 ) total
  integer ( kind = 4 ), dimension ( n ) :: w = (/ &
    15, 22, 14, 26, 32, 9, 16, 8 /)
  integer ( kind = 4 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_BACKTRACK_TEST'
  write ( *, '(a)' ) '  I4VEC_BACKTRACK uses backtracking, seeking a vector X'
  write ( *, '(a)' ) '  of N values which satisfies some condition.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  In this demonstration, we have 8 integers W(I).'
  write ( *, '(a)' ) '  We seek all subsets that sum to 53.'
  write ( *, '(a)' ) '  X(I) is 0 or 1 if the entry is skipped or used.'
  write ( *, '(a)' ) ''

  t = 53

  x(1:n) = 0
  indx = 0
  k = 0
  nstack = 0
  ncan(1:n) = 0

  found_num = 0

  do

    call i4vec_backtrack ( n, x, indx, k, nstack, stacks, maxstack, ncan )

    if ( indx == 1 ) then

      found_num = found_num + 1
      write ( *, '(2x,i2,3x)', advance = 'no' ) found_num

      total = dot_product ( w(1:n), x(1:n) )
      write ( *, '(2x,i3,a1,2x)', advance = 'no' ) total, ':'

      do i = 1, n
        if ( x(i) == 1 ) then
          write ( *, '(2x,i2)', advance = 'no' ) w(i)
        end if
      end do
      write ( *, '(a)' ) ''
!
!  Given that we've chose X(1:K-1), what are our choices for X(K)?
!
!    if T < TOTAL, 
!      no choices
!    if T = TOTAL, 
!      X(K) = 0
!    if T > TOTAL and K < N, 
!      X(k) = 0
!      If ( W(K)+TOTAL <= T ) X(K) = 1
!    If T > TOTAL and K = N,
!      If ( W(K) + TOTAL) = T ) X(K) = 1
!
    elseif ( indx == 2 ) then

      total = dot_product ( w(1:k-1), x(1:k-1) )

      if ( t < total ) then

        ncan(k) = 0

      else if ( t == total ) then

        ncan(k) = ncan(k) + 1
        nstack = nstack + 1
        stacks(nstack) = 0

      else if ( total < t .and. k < n ) then

        ncan(k) = ncan(k) + 1
        nstack = nstack + 1
        stacks(nstack) = 0

        if ( total + w(k) <= t ) then
          ncan(k) = ncan(k) + 1
          nstack = nstack + 1
          stacks(nstack) = 1
        end if

      else if ( total < t .and. k == n ) then

        if ( total + w(k) == t ) then
          ncan(k) = ncan(k) + 1
          nstack = nstack + 1
          stacks(nstack) = 1
        end if

      end if

    else

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Done!'
      exit

    end if

  end do

  return
end
subroutine i4vec_dot_product_test ( )

!*****************************************************************************80
!
!! I4VEC_DOT_PRODUCT_TEST tests I4VEC_DOT_PRODUCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) d
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) i4vec_dot_product
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4VEC_DOT_PRODUCT_TEST'
  write ( *, '(a)' ) '  I4VEC_DOT_PRODUCT computes the dot product of two I4VECs.'

  lo = 0
  hi = 10
  seed = 123456789

  call i4vec_uniform_ab ( n, lo, hi, seed, a )
  call i4vec_print ( n, a, '  The vector A:' )

  call i4vec_uniform_ab ( n, lo, hi, seed, b )
  call i4vec_print ( n, b, '  The vector B:' )

  d = i4vec_dot_product ( n, a, b )
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The dot product is ', d

  return
end
subroutine i4vec_part1_test ( )

!*****************************************************************************80
!
!! I4VEC_PART1_TEST tests I4VEC_PART1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npart = 5

  
  integer ( kind = 4 ) n
  integer ( kind = 4 ) x(npart)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_PART1_TEST:'
  write ( *, '(a)' ) '  I4VEC_PART1 partitions an integer N into NPART parts.'

  n = 17

  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Partition N = ', n, ' into NPART = ', npart, ' parts:'
  write ( *, '(a)' ) ''

  call i4vec_part1 ( n, npart, x )

  call i4vec_print ( npart, x, '  The partition:' )

  return
end
subroutine i4vec_part2_test ( )

!*****************************************************************************80
!
!! I4VEC_PART2_TEST tests I4VEC_PART2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npart = 5

  
  integer ( kind = 4 ) n
  integer ( kind = 4 ) x(npart)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_PART2_TEST:'
  write ( *, '(a)' ) '  I4VEC_PART2 partitions an integer N into NPART parts.'

  n = 17

  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Partition N = ', n, ' into NPART = ', npart, ' parts:'
  write ( *, '(a)' ) ''

  call i4vec_part2 ( n, npart, x )

  call i4vec_print ( npart, x, '  The partition:' )

  return
end
subroutine i4vec_search_binary_a_test ( )

!*****************************************************************************80
!
!! I4VEC_SEARCH_BINARY_A_TEST tests I4VEC_SEARCH_BINARY_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) index

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SEARCH_BINARY_A_TEST'
  write ( *, '(a)' ) '  I4VEC_SEARCH_BINARY_A searches a ascending sorted vector.'

  a(1:n) = (/ 0, 1, 1, 2, 3, 4, 5, 6, 7, 8 /)

  call i4vec_print ( n, a, '  Ascending sorted array:' )

  b = 5

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Now search for an instance of the value ', b

  call i4vec_search_binary_a ( n, a, b, index )

  write ( *, '(a)' ) ''
  if ( index == -1 ) then
    write ( *, '(a)' ) '  The value does not occur.'
  else
    write ( *, '(a,i8)' ) '  The value occurs at index = ', index
  end if

  return
end
subroutine i4vec_search_binary_d_test ( )

!*****************************************************************************80
!
!! I4VEC_SEARCH_BINARY_D_TEST tests I4VEC_SEARCH_BINARY_D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) index

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SEARCH_BINARY_D_TEST'
  write ( *, '(a)' ) '  I4VEC_SEARCH_BINARY_D searches a descending '
  write ( *, '(a)' ) ' sorted vector.'

  a(1:n) = (/ 8, 7, 6, 5, 4, 3, 2, 1, 1, 0 /)

  call i4vec_print ( n, a, '  Descending sorted array:' )

  b = 5

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Now search for an instance of the value ', b

  call i4vec_search_binary_d ( n, a, b, index )

  write ( *, '(a)' ) ''
  if ( index == 0 ) then
    write ( *, '(a)' ) '  The value does not occur.'
  else
    write ( *, '(a,i8)' ) '  The value occurs at index = ', index
  end if

  return
end
subroutine i4vec_sort_insert_a_test ( )

!*****************************************************************************80
!
!! I4VEC_SORT_INSERT_A_TEST tests I4VEC_SORT_INSERT_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORT_INSERT_A_TEST'
  write ( *, '(a)' ) '  I4VEC_SORT_INSERT_A ascending sorts an I4VEC;'

  a(1:n) = (/ 6, 7, 1, 0, 4, 3, 2, 1, 5, 8 /)

  call i4vec_print ( n, a, '  Before ascending sort:' )

  call i4vec_sort_insert_a ( n, a )

  call i4vec_print ( n, a, '  After ascending sort:' )

  return
end
subroutine i4vec_sort_insert_d_test ( )

!*****************************************************************************80
!
!! I4VEC_SORT_INSERT_D_TEST tests I4VEC_SORT_INSERT_D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORT_INSERT_D_TEST'
  write ( *, '(a)' ) '  I4VEC_SORT_INSERT_D descending sorts an I4VEC.'

  a(1:n) = (/ 6, 7, 1, 0, 4, 3, 2, 1, 5, 8 /)

  call i4vec_print ( n, a, '  Before descending sort:' )

  call i4vec_sort_insert_d ( n, a )

  call i4vec_print ( n, a, '  After descending sort:' )

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
subroutine knapsack_01_test ( )

!*****************************************************************************80
!
!! KNAPSACK_01_TEST tests KNAPSACK_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  real ( kind = 8 ) :: mass
  real ( kind = 8 ) :: mass_limit = 26.0
  real ( kind = 8 ), dimension ( n ) :: p = (/ &
    24.0, 13.0, 23.0, 15.0, 16.0 /)
  real ( kind = 8 ) :: profit
  real ( kind = 8 ), dimension ( n ) :: w = (/ &
    12.0,  7.0, 11.0,  8.0,  9.0 /)
  real ( kind = 8 ), dimension ( n ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KNAPSACK_01_TEST'
  write ( *, '(a)' ) '  KNAPSACK_01 solves the 0/1 knapsack problem.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Object, Profit, Mass, "Profit Density"'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i6,2x,f7.3,2x,f7.3,2x,f7.3)' ) i, p(i), w(i), p(i)/w(i)
  end do

  call knapsack_reorder ( n, p, w )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  After reordering by Profit Density:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Object, Profit, Mass, "Profit Density"'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i6,2x,f7.3,2x,f7.3,2x,f7.3)' ) i, p(i), w(i), p(i) / w(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,f7.3)' ) '  Total mass restriction is ', mass_limit

  call knapsack_01 ( n, mass_limit, p, w, x, mass, profit )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Object, Density, Choice, Profit, Mass'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(i6,f7.3,f7.3,2f7.3)' ) i, p(i)/w(i), x(i), &
      x(i) * p(i), x(i) * w(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,2f7.3)' ) '  Total:            ', profit, mass

  return
end
subroutine knapsack_rational_test ( )

!*****************************************************************************80
!
!! KNAPSACK_RATIONAL_TEST tests KNAPSACK_RATIONAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  real ( kind = 8 ) :: mass
  real ( kind = 8 ) :: mass_limit = 26.0
  real ( kind = 8 ), dimension ( n ) :: p = (/ &
    24.0, 13.0, 23.0, 15.0, 16.0 /)
  real ( kind = 8 ) :: profit
  real ( kind = 8 ), dimension ( n ) :: w = (/ &
    12.0,  7.0, 11.0,  8.0,  9.0 /)
  real ( kind = 8 ), dimension ( n ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KNAPSACK_RATIONAL_TEST'
  write ( *, '(a)' ) '  KNAPSACK_RATIONAL solves the rational knapsack problem.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Object, Profit, Mass, "Profit Density"'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(i6,3f7.3)' ) i, p(i), w(i), p(i) / w(i)
  end do

  call knapsack_reorder ( n, p, w )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  After reordering by Profit Density:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Object, Profit, Mass, "Profit Density"'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(i6,3f7.3)' ) i, p(i), w(i), p(i) / w(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,f7.3)' ) '  Total mass restriction is ', mass_limit

  call knapsack_rational ( n, mass_limit, p, w, x, mass, profit )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Object, Density, Choice, Profit, Mass'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(i6,f7.3,f7.3,2f7.3)' ) i, p(i) / w(i), x(i), &
      x(i) * p(i), x(i) * w(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,2f7.3)' ) '  Total:            ', profit, mass

  return
end
subroutine knapsack_reorder_test ( )

!*****************************************************************************80
!
!! KNAPSACK_REORDER_TEST tests KNAPSACK_REORDER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  real ( kind = 8 ), dimension ( n ) :: p = (/ &
    24.0, 13.0, 23.0, 15.0, 16.0 /)
  real ( kind = 8 ), dimension ( n ) :: w = (/ &
    12.0,  7.0, 11.0,  8.0,  9.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KNAPSACK_REORDER_TEST'
  write ( *, '(a)' ) '  KNAPSACK_REORDER reorders the knapsack data.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Object, Profit, Mass, "Profit Density"'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i6,2x,f7.3,2x,f7.3,2x,f7.3)' ) i, p(i), w(i), p(i)/w(i)
  end do

  call knapsack_reorder ( n, p, w )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  After reordering by Profit Density:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Object, Profit, Mass, "Profit Density"'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i6,2x,f7.3,2x,f7.3,2x,f7.3)' ) i, p(i), w(i), p(i) / w(i)
  end do

  return
end
subroutine ksubset_colex_check_test ( )

!*****************************************************************************80
!
!! KSUBSET_COLEX_CHECK_TEST tests KSUBSET_COLEX_CHECK.
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

  logical ( kind = 4 ) check;
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s(3)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUBSET_COLEX_CHECK TEST'
  write ( *, '(a)' ) '  KSUBSET_COLEX_CHECK checks a K subset of an N set.'
  
  do test = 1, 7

    if ( test == 1 ) then
      k = -1
      n = 5
      s = (/ -1, -1, -1 /)
    else if ( test == 2 ) then
      k = 3
      n = 0
      s = (/ 5, 3, 2 /)
    else if ( test == 3 ) then
      k = 3
      n = 5
      s = (/ 5, 2, 3 /)
    else if ( test == 4 ) then
      k = 3
      n = 5
      s = (/ 7, 3, 2 /)
    else if ( test == 5 ) then
      k = 3
      n = 5
      s = (/ 5, 3, 2 /)
    else if ( test == 6 ) then
      k = 0
      n = 5
      s = (/ -1, -1, -1 /)
    else if ( test == 7 ) then
      k = 0
      n = 0
      s = (/ -1, -1, -1 /)
    end if

    call ksubset_colex_check ( k, n, s, check )
    call i4vec_transpose_print ( k, s, '  Subset:' )
    write ( *, '(a,i4,a,i4)' ) '  N = ', n, ', K = ', k
    write ( *, '(a,l)' ) '  Check = ', check

  end do

  return
end
subroutine ksubset_colex_rank_test ( )

!*****************************************************************************80
!
!! KSUBSET_COLEX_RANK_TEST tests KSUBSET_COLEX_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ), dimension ( k ) :: t = (/ 5, 3, 1 /)

  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUBSET_COLEX_RANK_TEST'
  write ( *, '(a)' ) '  KSUBSET_COLEX_RANK ranks'
  write ( *, '(a)' ) '  K-subsets of an N set,'
  write ( *, '(a)' ) '  using the colexicographic ordering.'

  call i4vec_transpose_print ( k, t, '  The element to be ranked:' )

  call ksubset_colex_rank ( k, n, t, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The rank is computed as ', rank

  return
end
subroutine ksubset_colex_successor_test ( )

!*****************************************************************************80
!
!! KSUBSET_COLEX_SUCCESSOR_TEST tests KSUBSET_COLEX_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(k)

  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUBSET_COLEX_SUCCESSOR_TEST:'
  write ( *, '(a)' ) '  KSUBSET_COLEX_SUCCESSOR lists'
  write ( *, '(a)' ) '  K-subsets of an N set,'
  write ( *, '(a)' ) '  using the colexicographic ordering.'

  rank = -1

  do

    rank_old = rank

    call ksubset_colex_successor ( k, n, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(4x,6i5)' ) rank, t(1:k)

  end do

  return
end
subroutine ksubset_colex_unrank_test ( )

!*****************************************************************************80
!
!! KSUBSET_COLEX_UNRANK_TEST tests KSUBSET_COLEX_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) t(k)

  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUBSET_COLEX_UNRANK_TEST'
  write ( *, '(a)' ) '  KSUBSET_COLEX_UNRANK unranks'
  write ( *, '(a)' ) '  K-subsets of an N set'
  write ( *, '(a)' ) '  using the colexicographic ordering.'

  rank = 5

  call ksubset_colex_unrank ( rank, k, n, t )

  call i4vec_transpose_print ( k, t, '  The element of rank 5:' )

  return
end
subroutine ksubset_enum_test ( )

!*****************************************************************************80
!
!! KSUBSET_ENUM_TEST tests KSUBSET_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) ksubset_num
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUBSET_ENUM_TEST'
  write ( *, '(a)' ) '  KSUBSET_ENUM enumerates K subsets of an N set.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      K:      0       1       2       3       4       5'
  write ( *, '(a)' ) '   N'
  do n = 0, 10
    write ( *, '(2x,i2,a)', advance = 'no' ) n, ':  '
    do k = 0, min ( n, 5 )
      call ksubset_enum ( k, n, ksubset_num )
      write ( *, '(2x,i6)', advance = 'no' ) ksubset_num
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine ksubset_lex_check_test ( )

!*****************************************************************************80
!
!! KSUBSET_LEX_CHECK_TEST tests KSUBSET_LEX_CHECK.
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

  logical ( kind = 4 ) check;
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s(3)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUBSET_LEX_CHECK TEST'
  write ( *, '(a)' ) '  KSUBSET_LEX_CHECK checks a K subset of an N set.'
  
  do test = 1, 7

    if ( test == 1 ) then
      k = -1
      n = 5
      s = (/ -1, -1, -1 /)
    else if ( test == 2 ) then
      k = 3
      n = 0
      s = (/ 2, 3, 5 /)
    else if ( test == 3 ) then
      k = 3
      n = 5
      s = (/ 3, 2, 5 /)
    else if ( test == 4 ) then
      k = 3
      n = 5
      s = (/ 2, 3, 7 /)
    else if ( test == 5 ) then
      k = 3
      n = 5
      s = (/ 2, 3, 5 /)
    else if ( test == 6 ) then
      k = 0
      n = 5
      s = (/ -1, -1, -1 /)
    else if ( test == 7 ) then
      k = 0
      n = 0
      s = (/ -1, -1, -1 /)
    end if

    call ksubset_lex_check ( k, n, s, check )
    call i4vec_transpose_print ( k, s, '  Subset:' )
    write ( *, '(a,i4,a,i4)' ) '  N = ', n, ', K = ', k
    write ( *, '(a,l)' ) '  Check = ', check

  end do

  return
end
subroutine ksubset_lex_rank_test ( )

!*****************************************************************************80
!
!! KSUBSET_LEX_RANK tests KSUBSET_LEX_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ), dimension ( k ) :: t = (/ 1, 4, 5 /)

  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUBSET_LEX_RANK'
  write ( *, '(a)' ) '  KSUBSET_LEX_RANK ranks K-subsets of an N set,'
  write ( *, '(a)' ) '  using the lexicographic ordering.'

  call ksubset_lex_rank ( k, n, t, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The rank of the element:'
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) t(1:k)
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  is computed as ', rank

  return
end
subroutine ksubset_lex_successor_test ( )

!*****************************************************************************80
!
!! KSUBSET_LEX_SUCCESSOR_TEST tests KSUBSET_LEX_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(k)

  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUBSET_LEX_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  KSUBSET_LEX_SUCCESSOR lists K-subsets of an N set,'
  write ( *, '(a)' ) '  using the lexicographic ordering.'

  rank = -1

  do

    rank_old = rank

    call ksubset_lex_successor ( k, n, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, t(1:k)

  end do

  return
end
subroutine ksubset_lex_unrank_test ( )

!*****************************************************************************80
!
!! KSUBSET_LEX_UNRANK_TEST tests KSUBSET_LEX_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) t(k)

  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUBSET_LEX_UNRANK_TEST'
  write ( *, '(a)' ) '  KSUBSET_LEX_UNRANK unranks K-subsets of an N set'
  write ( *, '(a)' ) '  using the lexicographic ordering.'

  rank = 5

  call ksubset_lex_unrank ( rank, k, n, t )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The element of rank ', rank
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) t(1:k)

  return
end
subroutine ksubset_revdoor_rank_test ( )

!*****************************************************************************80
!
!! KSUBSET_REVDOOR_RANK_TEST tests KSUBSET_REVDOOR_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ), dimension ( k ) :: t = (/ 2, 4, 5 /)

  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUBSET_REVDOOR_RANK_TEST'
  write ( *, '(a)' ) '  KSUBSET_REVDOOR_RANK ranks K-subsets of an N set,'
  write ( *, '(a)' ) '  using the revolving door ordering.'
  write ( *, '(a)' ) ''

  call ksubset_revdoor_rank ( k, n, t, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The rank of the element:'
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) t(1:k)
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  is computed as ', rank

  return
end
subroutine ksubset_revdoor_successor_test ( )

!*****************************************************************************80
!
!! KSUBSET_REVDOOR_SUCCESOR_TEST tests KSUBSET_REVDOOR_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(k)

  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUBSET_REVDOOR_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  KSUBSET_REVDOOR_SUCCESSOR lists'
  write ( *, '(a)' ) '  K-subsets of an N set,'
  write ( *, '(a)' ) '  using the revolving door ordering.'
  write ( *, '(a)' ) ''

  rank = -1

  do

    rank_old = rank

    call ksubset_revdoor_successor ( k, n, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, t(1:k)

  end do

  return
end
subroutine ksubset_revdoor_unrank_test ( )

!*****************************************************************************80
!
!! KSUBSET_REVDOOR_UNRANK_TEST tests KSUBSET_REVDOOR_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) t(k)

  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUBSET_REVDOOR_UNRANK_TEST'
  write ( *, '(a)' ) '  KSUBSET_REVDOOR_UNRANK unranks K-subsets of an N set,'
  write ( *, '(a)' ) '  using the revolving door ordering.'
  write ( *, '(a)' ) ''

  rank = 5

  call ksubset_revdoor_unrank ( rank, k, n, t )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The element of rank ', rank
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) t(1:k)

  return
end
subroutine marriage_test ( )

!*****************************************************************************80
!
!! MARRIAGE_TEST tests MARRIAGE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) fiancee(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) next(n)
  integer ( kind = 4 ) prefer(n,n)
  integer ( kind = 4 ) rank(n,n)
!
!  PREFER(M,W) is the index of women W on man M's list.
!
  prefer(1:5,1:5) = reshape ( (/ &
    2, 1, 2, 1, 5, &
    5, 2, 3, 3, 3, &
    1, 3, 5, 2, 2, &
    3, 4, 4, 4, 1, &
    4, 5, 1, 5, 4  &
    /), (/ 5, 5 /) )
!
!  RANK(W,M) is the index of man M on woman W's list.
!
  rank(1:5,1:5) = reshape ( (/ &
    2, 4, 1, 4, 5, &
    4, 3, 3, 2, 2, &
    5, 5, 4, 1, 3, &
    3, 1, 2, 3, 1, &
    1, 2, 5, 5, 4  &
   /), (/ 5, 5 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MARRIAGE_TEST'
  write ( *, '(a)' ) '  MARRIAGE arranges a set of stable marriages'
  write ( *, '(a)' ) '  given a set of preferences.'

  call marriage ( n, prefer, rank, fiancee, next )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Man, Wife''s rank, Wife'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(3i8)' ) i, next(i), prefer(i,next(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Woman, Husband''s rank, Husband'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(3i8)' ) i, rank(i,fiancee(i)), fiancee(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Correct result:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  M:W 1  2  3  4  5'
  write ( *, '(a)' ) '   1  +  .  .  .  .'
  write ( *, '(a)' ) '   2  .  .  .  +  .'
  write ( *, '(a)' ) '   3  .  .  .  .  +'
  write ( *, '(a)' ) '   4  .  .  +  .  .'
  write ( *, '(a)' ) '   5  .  +  .  .  .'

  return
end
subroutine mountain_test ( )

!*****************************************************************************80
!
!! MOUNTAIN_TEST tests MOUNTAIN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) mxy
  integer ( kind = 4 ) row(0:2*n)
  integer ( kind = 4 ) x
  integer ( kind = 4 ) y

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MOUNTAIN_TEST'
  write ( *, '(a)' ) '  MOUNTAIN computes mountain numbers.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   Y    MXY'
  write ( *, '(a)' ) ''

  do y = 0, n
    do x = 0, 2*n
      call mountain ( n, x, y, mxy )
      row(x) = mxy
    end do
    write ( *, '(2x,i2,3x,11i4)' ) y, row(0:2*n)
  end do

  return
end
subroutine npart_enum_test ( )

!*****************************************************************************80
!
!! NPART_ENUM_TEST tests NPART_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) npart_num
  integer ( kind = 4 ) part_num
  
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NPART_ENUM_TEST'
  write ( *, '(a)' ) '  NPART_ENUM enumerates partitions of N into PART_NUM parts.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   PART_NUM:  1       2       3       4       5       6'
  write ( *, '(a)' ) '   N'
  do n = 0, 10
    write ( *, '(2x,i2,a)', advance = 'no' ) n, ':  '
    do part_num = 1, min ( n, 6 )
      call npart_enum ( n, part_num, npart_num )
      write ( *, '(2x,i6)', advance = 'no' ) npart_num
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine npart_rsf_lex_random_test ( )

!*****************************************************************************80
!
!! NPART_RSF_LEX_RANDOM_TEST tests NPART_RSF_LEX_RANDOM;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npart = 3

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) t(npart)

  n = 12
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NPART_RSF_LEX_RANDOM_TEST'
  write ( *, '(a)' ) '  NPART_RSF_LEX_RANDOM produces random examples.'
  write ( *, '(a)' ) '  of partitions of N with NPART parts'
  write ( *, '(a)' ) '  in reverse standard form.'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call npart_rsf_lex_random ( n, npart, seed, t )

    write ( *, '(6i5)' ) t(1:npart)

  end do

  return
end
subroutine npart_rsf_lex_rank_test ( )

!*****************************************************************************80
!
!! NPART_RSF_LEX_RANK_TEST tests NPART_RSF_LEX_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npart = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ), dimension ( npart ) :: t = (/ 1, 5, 6 /)

  n = 12

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NPART_RSF_LEX_RANK_TEST'
  write ( *, '(a)' ) '  NPART_RSF_LEX_RANK ranks partitions of N with NPART parts'
  write ( *, '(a)' ) '  in reverse standard form.'

  call npart_rsf_lex_rank ( n, npart, t, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The rank of the element:'
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) t(1:npart)
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  is computed as ', rank

  return
end
subroutine npart_rsf_lex_successor_test ( )

!*****************************************************************************80
!
!! NPART_RSF_LEX_SUCCESSOR_TEST tests NPART_RSF_LEX_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npart = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(npart)

  n = 12

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NPART_RSF_LEX_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  NPART_RSF_LEX_SUCCESSOR lists partitions of N with NPART parts'
  write ( *, '(a)' ) '  in reverse standard form.'

  rank = -1

  do

    rank_old = rank

    call npart_rsf_lex_successor ( n, npart, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, t(1:npart)

  end do

  return
end
subroutine npart_rsf_lex_unrank_test ( )

!*****************************************************************************80
!
!! NPART_RSF_LEX_UNRANK_TEST tests NPART_RSF_LEX_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npart = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) t(npart)

  n = 12

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NPART_RSF_LEX_UNRANK_TEST'
  write ( *, '(a)' ) '  NPART_RSF_LEX_UNRANK unranks partitions of N with NPART parts'
  write ( *, '(a)' ) '  in reverse standard form.'

  rank = 4

  call npart_rsf_lex_unrank ( rank, n, npart, t )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The element of rank ', rank
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) t(1:npart)

  return
end
subroutine npart_sf_lex_successor_test ( )

!*****************************************************************************80
!
!! NPART_SF_LEX_SUCCESSOR_TEST tests NPART_SF_LEX_SUCCESSOR;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: npart = 3

  integer ( kind = 4 ) n
  integer ( kind = 4 ) npartitions
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(npart)

  n = 12

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NPART_SF_LEX_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  NPART_SF_LEX_SUCCESSOR lists'
  write ( *, '(a)' ) '  partitions of N with NPART parts'
  write ( *, '(a)' ) '  in standard form.'

  call npart_enum ( n, npart, npartitions )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  For N = ', n
  write ( *, '(a,i8)' ) '  and NPART = ', npart
  write ( *, '(a,i8)' ) '  the number of partitions is ', npartitions
  write ( *, '(a)' ) ''
!
!  List.
!
  rank = -1

  do

    rank_old = rank

    call npart_sf_lex_successor ( n, npart, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, t(1:npart)

  end do

  return
end
subroutine npart_table_test ( )

!*****************************************************************************80
!
!! NPART_TABLE_TEST tests NPART_TABLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxn = 10
  integer ( kind = 4 ), parameter :: maxpart = 5

  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(0:maxn,0:maxpart)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NPART_TABLE_TEST'
  write ( *, '(a)' ) '  NPART_TABLE tabulates partitions'
  write ( *, '(a)' ) '  of N with NPART parts;'

  call npart_table ( maxn, maxpart, maxn, p )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I P(I,0) P(I,1) P(I,2) P(I,3) P(I,4) P(I,5)'
  write ( *, '(a)' ) ''

  do i = 0, maxn
    write ( *, '(11i5)' ) i, p(i,0:maxpart)
  end do

  return
end
subroutine part_enum_test ( )

!*****************************************************************************80
!
!! PART_ENUM_TEST tests PART_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) part_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PART_ENUM_TEST'
  write ( *, '(a)' ) '  PART_ENUM enumerates partitions of N.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N     #'
  write ( *, '(a)' ) ''

  do n = 0, 10
    call part_enum ( n, part_num )
    write ( *, '(2x,i2,2x,i6)' ) n, part_num
  end do

  return
end
subroutine part_rsf_check_test ( )

!*****************************************************************************80
!
!! PART_RSF_CHECK_TEST tests PART_RSF_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: a(:)
  logical ( kind = 4 ) check
  integer ( kind = 4 ) n
  integer ( kind = 4 ) npart
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PART_RSF_CHECK TEST'
  write ( *, '(a)' ) '  PART_RSF_CHECK checks a reverse standard form partition.'
  
  do test = 1, 6

    if ( test == 1 ) then
      n = 0
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 1, 4, 4, 6 /)
    else if ( test == 2 ) then
      n = 15
      npart = 0
      allocate ( a(1:4) )
      a = (/ 1, 4, 4, 6 /)
    else if ( test == 3 ) then
      n = 15
      npart = 4
      allocate ( a(1:npart) )
      a = (/ -9, 4, 4, 16 /)
    else if ( test == 4 ) then
      n = 15
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 6, 4, 4, 1 /)
    else if ( test == 5 ) then
      n = 15
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 1, 4, 5, 6 /)
    else if ( test == 6 ) then
      n = 15
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 1, 4, 4, 6 /)
    end if

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Partition in RSF form.'
    write ( *, '(a,i4)' ) '  Partition of N = ', n
    write ( *, '(a,i4)' ) '  Number of parts NPART = ', npart
    call i4vec_transpose_print ( npart, a, '' )
    call part_rsf_check ( n, npart, a, check )
    write ( *, '(a,l)' ) '  Check = ', check
    deallocate ( a )

  end do

  return
end
subroutine part_sf_check_test ( )

!*****************************************************************************80
!
!! PART_SF_CHECK_TEST tests PART_SF_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: a(:)
  logical ( kind = 4 ) check
  integer ( kind = 4 ) n
  integer ( kind = 4 ) npart
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PART_SF_CHECK TEST'
  write ( *, '(a)' ) '  PART_SF_CHECK checks a standard form partition.'
  
  do test = 1, 6

    if ( test == 1 ) then
      n = 0
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 6, 4, 4, 1 /)
    else if ( test == 2 ) then
      n = 15
      npart = 0
      allocate ( a(1:4) )
      a = (/ 6, 4, 4, 1 /)
    else if ( test == 3 ) then
      n = 15
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 16, 4, 4, -9 /)
    else if ( test == 4 ) then
      n = 15
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 1, 4, 4, 6 /)
    else if ( test == 5 ) then
      n = 15
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 6, 5, 4, 1 /)
    else if ( test == 6 ) then
      n = 15
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 6, 4, 4, 1 /)
    end if

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Partition in RSF form.'
    write ( *, '(a,i4)' ) '  Partition of N = ', n
    write ( *, '(a,i4)' ) '  Number of parts NPART = ', npart
    call i4vec_transpose_print ( npart, a, '' )
    call part_sf_check ( n, npart, a, check )
    write ( *, '(a,l)' ) '  Check = ', check
    deallocate ( a )

  end do

  return
end
subroutine part_successor_test ( )

!*****************************************************************************80
!
!! PART_SUCCESSOR_TEST tests PART_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 8

  integer ( kind = 4 ) npart
  integer ( kind = 4 ) npartitions
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PART_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  PART_SUCCESSOR produces partitions of N.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Partitions of N = ', n

  call part_enum ( n, npartitions )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  For N = ', n
  write ( *, '(a,i8)' ) '  the number of partitions is ', npartitions
  write ( *, '(a)' ) ''
!
!  List.
!
  rank = -1

  do

    rank_old = rank

    call part_successor ( n, npart, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(2x,i2,3x,10i3)' ) rank, t(1:npart)

  end do

  return
end
subroutine part_sf_conjugate_test ( )

!*****************************************************************************80
!
!! PART_SF_CONJUGATE_TEST tests PART_SF_CONJUGATE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 8

  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) npart
  integer ( kind = 4 ) npartb
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PART_SF_CONJUGATE_TEST'
  write ( *, '(a)' ) '  PART_SF_CONJUGATE produces the conjugate of a partition.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Partitions of N = ', n
!
!  List.
!
  rank = -1

  do

    rank_old = rank

    call part_successor ( n, npart, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(2x,i2,4x,10i3)' ) rank, t(1:npart)
    call part_sf_conjugate ( n, npart, t, npartb, b )
    write ( *, '(2x,a4,2x,10i3)' ) 'Con:', b(1:npartb)

  end do

  return
end
subroutine part_sf_majorize_test ( )

!*****************************************************************************80
!
!! PART_SF_MAJORIZE_TEST tests PART_SF_MAJORIZE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 8

  integer ( kind = 4 ), parameter, dimension ( n ) :: a = (/ 2, 2, 2, 1, 1, 0, 0, 0 /)
  integer ( kind = 4 ), parameter, dimension ( n ) :: b = (/ 3, 1, 1, 1, 1, 1, 0, 0 /)
  integer ( kind = 4 ), parameter, dimension ( n ) :: c = (/ 2, 2, 1, 1, 1, 1, 0, 0 /)
  integer ( kind = 4 ), parameter :: nparta = 5
  integer ( kind = 4 ), parameter :: npartb = 6
  integer ( kind = 4 ), parameter :: npartc = 6
  integer ( kind = 4 ) result

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PART_SF_MAJORIZE_TEST'
  write ( *, '(a)' ) '  PART_SF_MAJORIZE determines if one partition'
  write ( *, '(a)' ) '  majorizes another.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Partitions of N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(2x,a,2x,10i3)' ) 'A:', a(1:nparta)
  write ( *, '(2x,a,2x,10i3)' ) 'B:', b(1:npartb)
  write ( *, '(2x,a,2x,10i3)' ) 'C:', c(1:npartc)

  call part_sf_majorize ( n, nparta, a, npartb, b, result )
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  A compare B: ', result
  call part_sf_majorize ( n, npartb, b, npartc, c, result )
  write ( *, '(a,i8)' ) '  B compare C: ', result
  call part_sf_majorize ( n, npartc, c, nparta, a, result )
  write ( *, '(a,i8)' ) '  C compare A: ', result
  call part_sf_majorize ( n, npartc, c, npartc, c, result )
  write ( *, '(a,i8)' ) '  C compare C: ', result

  return
end
subroutine part_table_test ( )

!*****************************************************************************80
!
!! PART_TABLE_TEST tests PART_TABLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxn = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(0:maxn)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PART_TABLE_TEST'
  write ( *, '(a)' ) '  PART_TABLE tabulates partitions of N.'

  call part_table ( maxn, p )

  write ( *, '(a)' ) ''

  do i = 0, maxn
    write ( *, '(2x,i2,2x,i4)' ) i, p(i)
  end do

  return
end
subroutine partn_enum_test ( )

!*****************************************************************************80
!
!! PARTN_ENUM_TEST tests PARTN_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) nmax
  integer ( kind = 4 ) partn_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PARTN_ENUM_TEST'
  write ( *, '(a)' ) '  PARTN_ENUM enumerates partitions of N with maximum part NMAX.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   NMAX:      1       2       3       4       5       6'
  write ( *, '(a)' ) '   N'

  do n = 0, 10
    write ( *, '(2x,i2,a)', advance = 'no' ) n, ':  '
    do nmax = 1, min ( n, 6 )
      call partn_enum ( n, nmax, partn_num )
      write ( *, '(2x,i6)', advance = 'no' ) partn_num
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine partn_sf_check_test ( )

!*****************************************************************************80
!
!! PARTN_SF_CHECK_TEST tests PARTN_SF_CHECK.
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

  integer ( kind = 4 ), allocatable :: a(:)
  logical ( kind = 4 ) check
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nmax
  integer ( kind = 4 ) npart
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PARTN_SF_CHECK TEST'
  write ( *, '(a)' ) '  PARTN_SF_CHECK checks a standard form partition'
  write ( *, '(a)' ) '  of N with largest entry NMAX.'
  
  do test = 1, 7

    if ( test == 1 ) then
      n = 0
      nmax = 6
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 6, 4, 4, 1 /)
    else if ( test == 2 ) then
      n = 15
      nmax = 6
      npart = 0
      allocate ( a(1:npart) )
      a = (/ 6, 4, 4, 1 /)
    else if ( test == 3 ) then
      n = 15
      nmax = 6
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 6, 6, 6, -3 /)
    else if ( test == 4 ) then
      n = 15
      nmax = 6
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 8, 4, 2, 1 /)
    else if ( test == 5 ) then
      n = 15
      nmax = 6
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 1, 4, 4, 6 /)
    else if ( test == 6 ) then
      n = 15
      nmax = 6
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 6, 5, 4, 1 /)
    else if ( test == 7 ) then
      n = 15
      nmax = 6
      npart = 4
      allocate ( a(1:npart) )
      a = (/ 6, 4, 4, 1 /)
    end if

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Partition in SF form.'
    write ( *, '(a,i4)' ) '  Partition of N = ', n
    write ( *, '(a,i4)' ) '  Maximum entry NMAX = ', nmax
    write ( *, '(a,i4)' ) '  Number of parts NPART = ', npart
    call i4vec_transpose_print ( npart, a, '' )
    call partn_sf_check ( n, nmax, npart, a, check )
    write ( *, '(a,l)' ) '  Check = ', check

    deallocate ( a )

  end do

  return
end
subroutine partn_successor_test ( )

!*****************************************************************************80
!
!! PARTN_SUCCESSOR_TEST tests PARTN_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11

  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) nmax
  integer ( kind = 4 ) npart
  integer ( kind = 4 ) npart2
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PARTN_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  PARTN_SUCCESSOR lists partitions of N with maximum element NMAX:'

  nmax = 4
  rank = -1

  do

    rank_old = rank

    call partn_successor ( n, nmax, npart, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(2x,i2,3x,15i3)' ) rank, t(1:npart)

  end do
!
!  List conjugates.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Repeat, but list RSF conjugated partitions.'
  write ( *, '(a)' ) ''
  rank = -1

  do

    rank_old = rank

    call partn_successor ( n, nmax, npart, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    call part_sf_conjugate ( n, npart, t, npart2, b )
    call i4vec_reverse ( npart2, b )
    write ( *, '(2x,i2,3x,15i3)' ) rank, b(1:npart2)

  end do

  return
end
subroutine partition_greedy_test ( )

!*****************************************************************************80
!
!! PARTITION_GREEDY_TEST tests PARTITION_GREEDY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) sums(2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PARTITION_GREEDY_TEST'
  write ( *, '(a)' ) '  PARTITION_GREEDY partitions an integer vector into'
  write ( *, '(a)' ) '  two subsets with nearly equal sum.'

  a = (/ 2, 10, 3, 8, 5, 7, 9, 5, 3, 2 /)

  call partition_greedy ( n, a, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'Data set #1 partitioned:'
  write ( *, '(a)' ) ''

  sums(1:2) = 0

  do i = 1, n

    if ( indx(i) == 1 ) then
      sums(1) = sums(1) + a(i)
      write ( *, '(2x,i6)' ) a(i)
    else
      write ( *, '(2x,6x,i6)' ) a(i)
      sums(2) = sums(2) + a(i)
    end if

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Sums:'
  write ( *, '(a)' ) ''
  write ( *, '(2i6)' ) sums(1), sums(2)

  a = (/ 771, 121, 281, 854, 885, 734, 486, 1003, 83, 62 /)

  call partition_greedy ( n, a, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Data set #2 partitioned:'
  write ( *, '(a)' ) ''

  sums(1:2) = 0

  do i = 1, n

    if ( indx(i) == 1 ) then
      sums(1) = sums(1) + a(i)
      write ( *, '(i6)' ) a(i)
    else
      write ( *, '(6x,i6)' ) a(i)
      sums(2) = sums(2) + a(i)
    end if

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Sums:'
  write ( *, '(a)' ) ''
  write ( *, '(2i6)' ) sums(1), sums(2)

  return
end
subroutine perm_check_test ( )

!*****************************************************************************80
!
!! PERM_CHECK_TEST tests PERM_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical ( kind = 4 ) check
  integer ( kind = 4 ) n
  integer ( kind = 4 ), allocatable :: s(:)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_CHECK TEST'
  write ( *, '(a)' ) '  PERM_CHECK checks a permutation.'
  
  do test = 1, 3

    if ( test == 1 ) then

      n = 5
      allocate ( s(1:n) )
      s = (/ 5, 1, 8, 3, 4 /)

    else if ( test == 2 ) then

      n = 5
      allocate ( s(1:n) )
      s = (/ 5, 1, 4, 3, 4 /)

    else if ( test == 3 ) then

      n = 5
      allocate ( s(1:n) )
      s = (/ 5, 1, 2, 3, 4 /)

    end if

    call perm_check ( n, s, check )
    call i4vec_transpose_print ( n, s, '  Permutation:' )
    write ( *, '(a,l)' ) '  Check = ', check

    deallocate ( s )

  end do

  return
end
subroutine perm_enum_test ( )

!*****************************************************************************80
!
!! PERM_ENUM_TEST tests PERM_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) perm_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_ENUM_TEST'
  write ( *, '(a)' ) '  PERM_ENUM enumerates permutations of N items.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N       #'
  write ( *, '(a)' ) ''

  do n = 0, 10
    call perm_enum ( n, perm_num )
    write ( *, '(2x,i2,2x,i8)' ) n, perm_num
  end do

  return
end
subroutine perm_inv_test ( )

!*****************************************************************************80
!
!! PERM_INV_TEST tests PERM_INV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ), dimension ( n ) :: p = (/ 3, 1, 2, 4 /)
  integer ( kind = 4 ) q(n)
  integer ( kind = 4 ) r(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_INV_TEST'
  write ( *, '(a)' ) '  PERM_INV inverts a permutation of the integers:'

  call perm_print ( n, p, '  The permutation P is ' )
!
!  Invert.
!
  call perm_inv ( n, p, q )

  call perm_print ( n, q, '  The inverse permutation Q is ' )
!
!  Multiply.
!
  call perm_mul ( n, p, q, r )

  call perm_print ( n, r, '  The product R = P * Q is ' )

  return
end
subroutine perm_lex_rank_test ( )

!*****************************************************************************80
!
!! PERM_LEX_RANK_TEST tests PERM_LEX_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ), dimension ( n ) :: pi = (/ 3, 1, 2, 4 /)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_LEX_RANK_TEST'
  write ( *, '(a)' ) '  PERM_LEX_RANK ranks permutations of the integers,'
  write ( *, '(a)' ) '  using the lexicographic ordering.'
 
  call perm_print ( n, pi, '  The element:' )

  call perm_lex_rank ( n, pi, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The rank is computed as ', rank

  return
end
subroutine perm_lex_successor_test ( )

!*****************************************************************************80
!
!! PERM_LEX_SUCCESSOR_TEST tests PERM_LEX_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) pi(n)
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_LEX_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  PERM_LEX_SUCCESSOR lists permutations of the integers,'
  write ( *, '(a)' ) '  using the lexicographic ordering.'
  write ( *, '(a)' ) ''

  rank = -1

  do

    rank_old = rank

    call perm_lex_successor ( n, pi, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, pi(1:n)

  end do

  return
end
subroutine perm_lex_unrank_test ( )

!*****************************************************************************80
!
!! PERM_LEX_UNRANK_TEST tests PERM_LEX_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) pi(n)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_LEX_UNRANK_TEST'
  write ( *, '(a)' ) '  PERM_LEX_UNRANK unranks permutations of the integers,'
  write ( *, '(a)' ) '  using the lexicographic ordering.'
  write ( *, '(a)' ) ''

  rank = 12

  call perm_lex_unrank ( rank, n, pi )

  call perm_print ( n, pi, '  The element of rank 12:' )

  return
end
subroutine perm_mul_test ( )

!*****************************************************************************80
!
!! PERM_MUL_TEST tests PERM_MUL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ), dimension ( n ) :: p = (/ 3, 1, 2, 4 /)
  integer ( kind = 4 ), dimension ( n ) :: q = (/ 2, 3, 1, 4 /)
  integer ( kind = 4 ) r(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_MUL_TEST'
  write ( *, '(a)' ) '  PERM_MUL multiplies two permutations.'

  call perm_print ( n, p, '  The permutation P:' )

  call perm_print ( n, q, '  The permutation Q:' )
!
!  Multiply.
!
  call perm_mul ( n, p, q, r )

  call perm_print ( n, r, '  The product R = P * Q is ' )

  return
end
subroutine perm_parity_test ( )

!*****************************************************************************80
!
!! PERM_PARITY_TEST tests PERM_PARITY.
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
  integer ( kind = 4 ) p(5)
  integer ( kind = 4 ) parity
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_PARITY_TEST'
  write ( *, '(a)' ) '  PERM_PARITY computes the parity of a permutation.'

  n = 5
  seed = 123456789

  do test = 1, 5
    call perm_random ( n, seed, p )
    call perm_print ( n, p, '  The permutation P:' )
    call perm_parity ( n, p, parity )
    write ( *, '(a)' ) ''
    write ( *, '(a,i4)' ) '  The parity is ', parity
  end do

  return
end
subroutine perm_print_test ( )

!*****************************************************************************80
!
!! PERM_PRINT_TEST tests PERM_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ), dimension ( n ) ::  p = (/ 7, 2, 4, 1, 5, 3, 6 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_PRINT_TEST'
  write ( *, '(a)' ) '  PERM_PRINT prints a permutation of (1,...,N).'

  call perm_print ( n, p, '  The 1-based permutation:' )
  
  return
end
subroutine perm_random_test ( )

!*****************************************************************************80
!
!! PERM_RANDOM_TEST tests PERM_RANDOM;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_RANDOM_TEST'
  write ( *, '(a)' ) '  PERM_RANDOM produces a random permutation of (1,...,N);'
  write ( *, '(a,i8)' ) '  For this test, N = ', n
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 5
    call perm_random ( n, seed, p )
    call i4vec_transpose_print (  n, p, "" )
  end do
 
  return
end
subroutine perm_tj_rank_test ( )

!*****************************************************************************80
!
!! PERM_TJ_RANK_TEST tests PERM_TJ_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ), dimension ( n ) :: pi = (/ 4, 3, 2, 1 /)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_TJ_RANK_TEST'
  write ( *, '(a)' ) '  PERM_TJ_RANK ranks permutations of the integers'
  write ( *, '(a)' ) '  using the Trotter-Johnson ordering.'

  call perm_print ( n, pi, '  The element to be ranked:' )

  call perm_tj_rank ( n, pi, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The rank is computed as ', rank

  return
end
subroutine perm_tj_successor_test ( )

!*****************************************************************************80
!
!! PERM_TJ_SUCCESSOR_TEST tests PERM_TJ_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) pi(n)
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_TJ_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  PERM_TJ_SUCCESSOR lists permutations of the integers'
  write ( *, '(a)' ) '  using the Trotter-Johnson ordering.'

  rank = -1

  do

    rank_old = rank

    call perm_tj_successor ( n, pi, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, pi(1:n)

  end do

  return
end
subroutine perm_tj_unrank_test ( )

!*****************************************************************************80
!
!! PERM_TJ_UNRANK_TEST tests PERM_TJ_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) pi(n)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_TJ_UNRANK_TEST'
  write ( *, '(a)' ) '  PERM_TJ_UNRANK unranks permutations of the integers'
  write ( *, '(a)' ) '  using the Trotter-Johnson ordering:'

  rank = 12

  call perm_tj_unrank ( rank, n, pi )

  call perm_print ( n, pi, '  The element of rank 12:' )

  return
end
subroutine perm_to_cycle_test ( )

!*****************************************************************************80
!
!! PERM_TO_CYCLE_TEST tests PERM_TO_CYCLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ) i
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) index(n)
  integer ( kind = 4 ) ncycle
  integer ( kind = 4 ), dimension ( n ) :: p = (/ 4, 5, 1, 2, 3, 6, 7 /)
  integer ( kind = 4 ) t(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_TO_CYCLE'
  write ( *, '(a)' ) '  PERM_TO_CYCLE converts a permutation from'
  write ( *, '(a)' ) '  array to cycle form.'

  call perm_print ( n, p, '  "Random" permutation:' )
!
!  Convert the permutation to cycle form.
!
  call perm_to_cycle ( n, p, ncycle, t, index )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Corresponding cycle form:'
  write ( *, '(a,i8)' ) '  Number of cycles is ', ncycle
  write ( *, '(a)' ) ''
  jlo = 0
  do i = 1, ncycle
    write ( *, '(4x,20i4)' ) t(jlo+1:jlo+index(i))
    jlo = jlo + index(i)
  end do

  return
end
subroutine pruefer_check_test ( )

!*****************************************************************************80
!
!! PRUEFER_CHECK_TEST tests PRUEFER_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical ( kind = 4 ) check
  integer ( kind = 4 ) n
  integer ( kind = 4 ) test
  integer ( kind = 4 ), allocatable :: p(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PRUEFER_CHECK TEST'
  write ( *, '(a)' ) '  PRUEFER_CHECK checks a Pruefer code.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Check?   N    T(1:2*N)'
  write ( *, '(a)' ) ''
  
  do test = 1, 4

    if ( test == 1 ) then
      n = 2
      allocate ( p(1:n-2) )
    else if ( test == 2 ) then
      n = 3
      allocate ( p(1:n-2) )
      p(1:n-2) = (/ 1 /)
    else if ( test == 3 ) then
      n = 4
      allocate ( p(1:n-2) )
      p(1:n-2) = (/ 5, 2 /)
    else if ( test == 4 ) then
      n = 5
      allocate ( p(1:n-2) )
      p(1:n-2) = (/ 5, 1, 3 /)
    end if

    call pruefer_check ( n, p, check )

    write ( *, '(6x,l2,2x,i2)', advance = 'no' ) check, n
    call i4vec_transpose_print ( n - 2, p, '' )

    deallocate ( p )

  end do

  return
end
subroutine pruefer_enum_test ( )

!*****************************************************************************80
!
!! PRUEFER_ENUM_TEST tests PRUEFER_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) pruefer_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PRUEFER_ENUM_TEST'
  write ( *, '(a)' ) '  PRUEFER_ENUM enumerates trees on N nodes, using the Pruefer code'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N         #'
  write ( *, '(a)' ) ''

  do n = 0, 10
    call pruefer_enum ( n, pruefer_num )
    write ( *, '(2x,i2,2x,i10)' ) n, pruefer_num
  end do

  return
end
subroutine pruefer_rank_test ( )

!*****************************************************************************80
!
!! PRUEFER_RANK_TEST tests PRUEFER_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ), dimension ( n - 2 ) :: p = (/ 3, 1 /)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PRUEFER_RANK_TEST'
  write ( *, '(a)' ) '  PRUEFER_RANK ranks Pruefer codes.'

  call pruefer_rank ( n, p, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The rank of the element:'
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) p(1:n-2)
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  is computed as ', rank

  return
end
subroutine pruefer_successor_test ( )

!*****************************************************************************80
!
!! PRUEFER_SUCCESSOR_TEST tests PRUEFER_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) p(n-2)
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PRUEFER_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  PRUEFER_SUCCESSOR lists Pruefer codes.'

  rank = -1

  do

    rank_old = rank

    call pruefer_successor ( n, p, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, p(1:n-2)

  end do

  return
end
subroutine pruefer_to_tree_test ( )

!*****************************************************************************80
!
!! PRUEFER_TO_TREE_TEST tests PRUEFER_TO_TREE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) p(n-2)
  integer ( kind = 4 ) pruefer_num
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) t(2,n-1)
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PRUEFER_TO_TREE_TEST'
  write ( *, '(a)' ) '  PRUEFER_TO_TREE converts a Pruefer code to a tree;'

  call pruefer_enum ( n, pruefer_num )

  i4_lo = 0
  i4_hi = pruefer_num - 1

  do test = 1, test_num
!
!  Pick a "random" Pruefer code.
!
    rank = i4_uniform_ab ( i4_lo, i4_hi, seed )

    call pruefer_unrank ( rank, n, p )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Random Pruefer code of rank ', rank
    write ( *, '(6i5)' ) p(1:n-2)
!
!  Convert the Pruefer code to a tree.
!
    call pruefer_to_tree ( n, p, t )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Edge list for the corresponding tree:'
    write ( *, '(a)' ) ''
    do j = 1, n - 1
      write ( *, '(6i5)' ) j, t(1:2,j)
    end do
!
!  Convert the tree to a Pruefer code.
!
    call tree_to_pruefer ( n, t, p )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Corresponding Pruefer code:'
    write ( *, '(6i5)' ) p(1:n-2)

  end do

  return
end
subroutine pruefer_unrank_test ( )

!*****************************************************************************80
!
!! PRUEFER_UNRANK_TEST tests PRUEFER_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) p(n-2)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PRUEFER_UNRANK_TEST'
  write ( *, '(a)' ) '  PRUEFER_UNRANK unranks Pruefer codes.'

  rank = 8

  call pruefer_unrank ( rank, n, p )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The element of rank ', rank
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) p(1:n-2)

  return
end
subroutine queens_test ( )

!*****************************************************************************80
!
!! QUEENS_TEST tests QUEENS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 8
  integer ( kind = 4 ), parameter :: maxstack = n * n

  integer ( kind = 4 ) iarray(n)
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) istack(maxstack)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) nstack

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUEENS_TEST'
  write ( *, '(a)' ) '  QUEENS produces nonattacking queens'
  write ( *, '(a)' ) '  on a chessboard using a backtrack search.'
  write ( *, '(a)' ) ''

  indx = 0

  do

    call backtrack ( n, iarray, indx, k, nstack, istack, maxstack )

    if ( indx == 1 ) then

      write ( *, '(19i4)' ) iarray(1:n)

    else if ( indx == 2 ) then

      call queens ( n, iarray, k, nstack, istack, maxstack )

    else

      exit

    end if

  end do

  return
end
subroutine r8_choose_test ( )

!*****************************************************************************80
!
!! R8_CHOOSE_TEST tests R8_CHOOSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cnk
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_choose

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_CHOOSE_TEST'
  write ( *, '(a)' ) '  R8_CHOOSE evaluates C(N,K).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         N         K       CNK'
 
  do n = 0, 5
    write ( *, '(a)' ) ' '
    do k = 0, n
      cnk = r8_choose ( n, k )
      write ( *, '(2x,i8,2x,i8,2x,g14.6)' ) n, k, cnk
    end do
  end do
 
  return
end
subroutine r8_gamma_log_test ( )

!*****************************************************************************80
!
!! R8_GAMMA_LOG_TEST tests R8_GAMMA_LOG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_GAMMA_LOG_TEST:'
  write ( *, '(a)' ) '  R8_GAMMA_LOG computes the Log(Gamma()) function.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '          X            GAMMA_LOG(X)   R8_GAMMA_LOG(X)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call gamma_log_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_gamma_log ( x )

    write ( *, '(2x,f14.6,2x,g24.16,2x,g24.16)' ) x, fx1, fx2

  end do

  return
end
subroutine r8vec_backtrack_test ( )

!*****************************************************************************80
!
!! R8VEC_BACKTRACK_TEST tests R8VEC_BACKTRACK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxstack = 100
  integer ( kind = 4 ), parameter :: n = 8

  integer ( kind = 4 ) found_num
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) k
  integer ( kind = 4 ) ncan(n)
  integer ( kind = 4 ) nstack
  real ( kind = 8 ) stacks(maxstack)
  real ( kind = 8 ) t
  real ( kind = 8 ) total
  real ( kind = 8 ), dimension ( n ) :: w = (/ &
    15.0, 22.0, 14.0, 26.0, 32.0, 9.0, 16.0, 8.0 /)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_BACKTRACK_TEST'
  write ( *, '(a)' ) '  R8VEC_BACKTRACK uses backtracking, seeking a vector X'
  write ( *, '(a)' ) '  of N values which satisfies some condition.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  In this demonstration, we have 8 integers W(I).'
  write ( *, '(a)' ) '  We seek all subsets that sum to 53.'
  write ( *, '(a)' ) '  X(I) is 0 or 1 if the entry is skipped or used.'
  write ( *, '(a)' ) ''

  t = 53

  x(1:n) = 1.0D+00
  indx = 0
  k = 0
  nstack = 0
  ncan(1:n) = 0

  found_num = 0

  do

    call r8vec_backtrack ( n, x, indx, k, nstack, stacks, maxstack, ncan )

    if ( indx == 1 ) then

      found_num = found_num + 1
      write ( *, '(2x,i2,3x)', advance = 'no' ) found_num

      total = dot_product ( w(1:n), x(1:n) )
      write ( *, '(2x,f6.2,a1,2x)', advance = 'no' ) total, ':'

      do i = 1, n
        if ( x(i) == 1.0 ) then
          write ( *, '(2x,f6.2)', advance = 'no' ) w(i)
        end if
      end do
      write ( *, '(a)' ) ''
!
!  Given that we've chose X(1:K-1), what are our choices for X(K)?
!
!    if T < TOTAL, 
!      no choices
!    if T = TOTAL, 
!      X(K) = 0
!    if T > TOTAL and K < N, 
!      X(k) = 0
!      If ( W(K)+TOTAL <= T ) X(K) = 1
!    If T > TOTAL and K = N,
!      If ( W(K) + TOTAL) = T ) X(K) = 1
!
    elseif ( indx == 2 ) then

      total = dot_product ( w(1:k-1), x(1:k-1) )

      if ( t < total ) then

        ncan(k) = 0

      else if ( t == total ) then

        ncan(k) = ncan(k) + 1
        nstack = nstack + 1
        stacks(nstack) = 0.0D+00

      else if ( total < t .and. k < n ) then

        ncan(k) = ncan(k) + 1
        nstack = nstack + 1
        stacks(nstack) = 0.0D+00

        if ( total + w(k) <= t ) then
          ncan(k) = ncan(k) + 1
          nstack = nstack + 1
          stacks(nstack) = 1.0D+00
        end if

      else if ( total < t .and. k == n ) then

        if ( total + w(k) == t ) then
          ncan(k) = ncan(k) + 1
          nstack = nstack + 1
          stacks(nstack) = 1.0D+00
        end if

      end if

    else

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Done!'
      exit

    end if

  end do

  return
end
subroutine rgf_check_test ( )

!*****************************************************************************80
!
!! RGF_CHECK_TEST tests RGF_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical ( kind = 4 ) check
  integer ( kind = 4 ), allocatable :: f(:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RGF_CHECK TEST'
  write ( *, '(a)' ) '  RGF_CHECK checks a restricted growth function.'
  
  do test = 1, 4

    if ( test == 1 ) then
      m = -1
      allocate ( f(1:1) )
      f = (/ -1 /)
    else if ( test == 2 ) then
      m = 7
      allocate ( f(1:m) )
      f = (/ 0, 1, 2, 3, 4, 5, 6 /)
    else if ( test == 3 ) then
      m = 7
      allocate ( f(1:m) )
      f = (/ 1, 3, 5, 8, 9, 10, 12 /)
    else if ( test == 4 ) then
      m = 7
      allocate ( f(1:m) )
      f = (/ 1, 2, 3, 1, 4, 5, 4 /)
    end if

    call i4vec_transpose_print ( m, f, '  RGF:' )
    call rgf_check ( m, f, check )
    write ( *, '(a,l)' ) '  Check = ', check
    deallocate ( f )

  end do

  return
end
subroutine rgf_enum_test ( )

!*****************************************************************************80
!
!! RGF_ENUM_TEST tests RGF_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rgf_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RGF_ENUM_TEST'
  write ( *, '(a)' ) '  RGF_ENUM enumerates restricted growth functions.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N     #'
  write ( *, '(a)' ) ''

  do n = 0, 10
    call rgf_enum ( n, rgf_num )
    write ( *, '(2x,i2,2x,i6)' ) n, rgf_num
  end do

  return
end
subroutine rgf_g_table_test ( )

!*****************************************************************************80
!
!! RGF_G_TABLE_TEST tests RGF_G_TABLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 July 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: MMAX = 8

  integer ( kind = 4 ) d(0:MMAX,0:MMAX)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m

  m = 6

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RGF_G_TABLE_TEST'
  write ( *, '(a)' ) '  RGF_G_TABLE tabulates generalized restricted'
  write ( *, '(a)' ) '  growth functions.'
  write ( *, '(a)' ) ''

  call rgf_g_table ( m, MMAX, d )

  do i = 0, m
    write ( *, '(7i6)' ) d(i,0:m-i)
  end do

  return
end
subroutine rgf_rank_test ( )

!*****************************************************************************80
!
!! RGF_RANK_TEST tests RGF_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4

  integer ( kind = 4 ), dimension ( m ) :: f = (/ 1, 2, 1, 3 /)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RGF_RANK_TEST'
  write ( *, '(a)' ) '  RGF_RANK ranks restricted growth functions.'

  call rgf_rank ( m, f, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The rank of the element:'
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) f(1:m)
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  is computed as ', rank

  return
end
subroutine rgf_successor_test ( )

!*****************************************************************************80
!
!! RGF_SUCCESSOR_TEST tests RGF_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4

  integer ( kind = 4 ) f(m)
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RGF_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  RGF_SUCCESSOR lists restricted growth functions.'

  rank = -1

  do

    rank_old = rank

    call rgf_successor ( m, f, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, f(1:m)

  end do

  return
end
subroutine rgf_to_setpart_test ( )

!*****************************************************************************80
!
!! RGF_TO_SETPART_TEST tests RGF_TO_SETPART.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 8

  integer ( kind = 4 ) i
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ), dimension ( m ) :: f = (/ 1, 1, 1, 1, 1, 2, 1, 3 /)
  integer ( kind = 4 ) index(m)
  integer ( kind = 4 ) nsub
  integer ( kind = 4 ) s(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RGF_TO_SETPART_TEST'
  write ( *, '(a)' ) '  RGF_TO_SETPART converts a balanced'
  write ( *, '(a)' ) '  sequence to a restricted growth function;'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Restricted growth function:'
  write ( *, '(a)' ) ''
  write ( *, '(8i2)' ) f(1:m)
!
!  Convert the RGF to a set partition.
!
  call rgf_to_setpart ( m, f, nsub, s, index )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Corresponding set partition'
  write ( *, '(a)' ) ''
  jlo = 1
  do i = 1, nsub
    write ( *, '(8i4)' ) s(jlo:index(i))
    jlo = index(i) + 1
  end do

  return
end
subroutine rgf_unrank_test ( )

!*****************************************************************************80
!
!! RGF_UNRANK_TEST tests RGF_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4

  integer ( kind = 4 ) f(m)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RGF_UNRANK_TEST'
  write ( *, '(a)' ) '  RGF_UNRANK unranks restricted growth functions.'

  rank = 7

  call rgf_unrank ( rank, m, f )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The element of rank ', rank
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) f(1:m)

  return
end
subroutine setpart_check_test ( )

!*****************************************************************************80
!
!! SETPART_CHECK_TEST tests SETPART_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical ( kind = 4 ) check
  integer ( kind = 4 ) i
  integer ( kind = 4 ), allocatable :: indx(:)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) m
  integer ( kind = 4 ) nsub
  integer ( kind = 4 ), allocatable :: s(:)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SETPART_CHECK TEST'
  write ( *, '(a)' ) '  SETPART_CHECK checks a set partition.'
  
  do test = 1, 6

    if ( test == 1 ) then
      m = 0
      nsub = 3
      allocate ( s(1:8) )
      allocate ( indx(1:nsub) )
      s = (/ 3, 6, 1, 4, 7, 2, 5, 8 /)
      indx = (/ 2, 5, 8 /)
    else if ( test == 2 ) then
      m = 8
      nsub = 0
      allocate ( s(1:m) )
      allocate ( indx(1:3) )
      s = (/ 3, 6, 1, 4, 7, 2, 5, 8 /)
      indx = (/ 2, 5, 8 /)
    else if ( test == 3 ) then
      m = 8
      nsub = 3
      allocate ( s(1:m) )
      allocate ( indx(1:nsub) )
      s = (/ 3, 6, 1, 4, 7, 2, 5, 8 /)
      indx = (/ 2, 8, 5 /)
    else if ( test == 4 ) then
      m = 8
      nsub = 3
      allocate ( s(1:m) )
      allocate ( indx(1:nsub) )
      s = (/ 3, 6, 1, 4, 9, 2, 5, 8 /)
      indx = (/ 2, 5, 8 /)
    else if ( test == 5 ) then
      m = 8
      nsub = 3
      allocate ( s(1:m) )
      allocate ( indx(1:nsub) )
      s = (/ 3, 6, 1, 4, 6, 2, 5, 8 /)
      indx = (/ 2, 5, 8 /)
    else if ( test == 6 ) then
      m = 8
      nsub = 3
      allocate ( s(1:m) )
      allocate ( indx(1:nsub) )
      s = (/ 3, 6, 1, 4, 7, 2, 5, 8 /)
      indx = (/ 2, 5, 8 /)
    end if

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  The set partition'
    write ( *, '(a,i4)' ) '  M = ', m
    write ( *, '(a,i4)' ) '  NSUB = ', nsub
    write ( *, '(a)' ) ''
    jlo = 1
    do i = 1, nsub
      do j = jlo, indx(i)
        write ( *, '(i4)', advance = 'no' ) s(j)
      end do
      write ( *, '(a)' ) ''
      jlo = indx(i) + 1
    end do

    call setpart_check ( m, nsub, s, indx, check )
    write ( *, '(a,l)' ) '  Check = ', check

    deallocate ( indx )
    deallocate ( s )
    
  end do

  return
end
subroutine setpart_enum_test ( )

!*****************************************************************************80
!
!! SETPART_ENUM_TEST tests SETPART_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) npart

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SETPART_ENUM'
  write ( *, '(a)' ) '  SETPART_ENUM enumerates set partitions.'
  write ( *, '(a)' ) ''
!
!  Enumerate.
!
  do n = 1, 6
    call setpart_enum ( n, npart )
    write ( *, '(i6,i6)' ) n, npart
  end do

  return
end
subroutine setpart_to_rgf_test ( )

!*****************************************************************************80
!
!! SETPART_TO_RGF_TEST tests SETPART_TO_RGF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 8

  integer ( kind = 4 ) i
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) f(m)
  integer ( kind = 4 ), dimension ( 3 ) :: index = (/ 6, 7, 8 /)
  integer ( kind = 4 ) :: nsub = 3
  integer ( kind = 4 ), dimension ( m ) :: s = (/ 1, 2, 3, 4, 5, 7, 6, 8 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SETPART_TO_RGF_TEST'
  write ( *, '(a)' ) '  SETPART_TO_RGF converts a restricted growth'
  write ( *, '(a)' ) '  function to a balanced sequence.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The set partition'
  write ( *, '(a)' ) ''
  jlo = 1
  do i = 1, nsub
    write ( *, '(8i4)' ) s(jlo:index(i))
    jlo = index(i) + 1
  end do
!
!  Convert the set partition to an RGF.
!
  call setpart_to_rgf ( m, nsub, s, index, f )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Corresponding restricted growth function:'
  write ( *, '(a)' ) ''
  write ( *, '(8i2)' ) f(1:m)

  return
end
subroutine stirling_numbers1_test ( )

!*****************************************************************************80
!
!! STIRLING_NUMBERS1_TEST tests STIRLING_NUMBERS1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxm = 6
  integer ( kind = 4 ), parameter :: maxn = 6

  integer ( kind = 4 ) i
  integer ( kind = 4 ) s(0:maxm,0:maxn)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'STIRLING_NUMBERS1_TEST'
  write ( *, '(a)' ) '  STIRLING_NUMBERS1 computes a table of Stirling'
  write ( *, '(a)' ) '  numbers of the first kind.'

  call stirling_numbers1 ( maxm, maxn, s )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I S(I,0) S(I,1) S(I,2) S(I,3) S(I,4) S(I,5)'
  write ( *, '(a)' ) ''

  do i = 0, maxm
    write ( *, '(11i5)' ) i, s(i,0:maxn)
  end do

  return
end
subroutine stirling_numbers2_test ( )

!*****************************************************************************80
!
!! STIRLING_NUMBERS2_TEST tests STIRLING_NUMBERS2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxm = 6
  integer ( kind = 4 ), parameter :: maxn = 6

  integer ( kind = 4 ) i
  integer ( kind = 4 ) s(0:maxm,0:maxn)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'STIRLING_NUMBERS2_TEST'
  write ( *, '(a)' ) '  STIRLING_NUMBERS2 computes a table of Stirling'
  write ( *, '(a)' ) '  numbers of the second kind.'

  call stirling_numbers2 ( maxm, maxn, s )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I S(I,0) S(I,1) S(I,2) S(I,3) S(I,4) S(I,5)'
  write ( *, '(a)' ) ''

  do i = 0, maxm
    write ( *, '(11i5)' ) i, s(i,0:maxn)
  end do

  return
end
subroutine subset_check_test ( )

!*****************************************************************************80
!
!! SUBSET_CHECK_TEST tests SUBSET_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical ( kind = 4 ) check
  integer ( kind = 4 ) n
  integer ( kind = 4 ), allocatable :: s(:)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) '\n';
  write ( *, '(a)' ) 'SUBSET_CHECK TEST'
  write ( *, '(a)' ) '  SUBSET_CHECK checks a subset.'
  
  do test = 1, 3

    if ( test == 1 ) then

      n = 0
      allocate ( s(1:1) )
      s = (/ 0 /)
 
    else if ( test == 2 ) then

      n = 3
      allocate ( s(1:n) )
      s = (/ 1, 2, 0 /)

    else if ( test == 3 ) then

      n = 5
      allocate ( s(1:n) )
      s = (/ 1, 0, 0, 1, 0 /)
 
    end if

    call subset_check ( n, s, check )
    call i4vec_transpose_print ( n, s, '  Subset:' )
    write ( *, '(a,l5)' ) '  Check = ', check
 
    deallocate ( s )

  end do

  return
end
subroutine subset_colex_rank_test ( )

!*****************************************************************************80
!
!! SUBSET_COLEX_RANK_TEST tests SUBSET_COLEX_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) rank
  integer ( kind = 4 ), dimension ( n ) :: t = (/ 0, 1, 0, 1, 0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_COLEX_RANK_TEST'
  write ( *, '(a)' ) '  SUBSET_COLEX_RANK ranks all subsets of a set,'
  write ( *, '(a)' ) '  using the colexicographic ordering.'

  call subset_colex_rank ( n, t, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The rank of the element:'
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) t(1:n)
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  is computed as ', rank

  return
end
subroutine subset_colex_successor_test ( )

!*****************************************************************************80
!
!! SUBSET_COLEX_SUCCESSOR_TEST tests SUBSET_COLEX_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_COLEX_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  SUBSET_COLEX_SUCCESSOR lists all subsets of a set,'
  write ( *, '(a)' ) '  using the colexicographic ordering.'

  rank = -1

  do

    rank_old = rank

    call subset_colex_successor ( n, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, t(1:n)

  end do

  return
end
subroutine subset_colex_unrank_test ( )

!*****************************************************************************80
!
!! SUBSET_COLEX_UNRANK_TEST tests SUBSET_COLEX_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) rank
  integer ( kind = 4 ) t(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_COLEX_UNRANK_TEST'
  write ( *, '(a)' ) '  SUBSET_COLEX_UNRANK unranks all subsets of a set,'
  write ( *, '(a)' ) '  using the colexicographic ordering.'

  rank = 10

  call subset_colex_unrank ( rank, n, t )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The element of rank ', rank
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) t(1:n)

  return
end
subroutine subset_complement_test ( )

!*****************************************************************************80
!
!! SUBSET_COMPLEMENT_TEST tests SUBSET_COMPLEMENT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) s1(5)
  integer ( kind = 4 ) s2(5)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_COMPLEMENT_TEST'
  write ( *, '(a)' ) '  SUBSET_COMPLEMENT returns the complement of a subset.'

  n = 5
  seed = 123456789

  call subset_random ( n, seed, s1 )
  call i4vec_transpose_print ( n, s1, '  Subset S1:            ' )

  call subset_complement ( n, s1, s2 )
  call i4vec_transpose_print ( n, s2, '  S2 = complement of S1:' )

  return
end
subroutine subset_distance_test ( )

!*****************************************************************************80
!
!! SUBSET_DISTANCE_TEST tests SUBSET_DISTANCE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) distance
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s1(5)
  integer ( kind = 4 ) s2(5)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_DISTANCE_TEST'
  write ( *, '(a)' ) '  SUBSET_DISTANCE computes the distance between subsets.'

  n = 5
  seed = 123456789

  call subset_random ( n, seed, s1 )
  call i4vec_transpose_print ( n, s1, '  Subset S1:' )

  call subset_random ( n, seed, s2 )
  call i4vec_transpose_print ( n, s2, '  Subset S2:' )

  call subset_distance ( n, s1, s2 )
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Distance = ', distance

  return
end
subroutine subset_enum_test ( )

!*****************************************************************************80
!
!! SUBSET_ENUM_TEST tests SUBSET_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) subset_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_ENUM_TEST'
  write ( *, '(a)' ) '  SUBSET_ENUM enumerates subsets of a set of N items.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N     #'
  write ( *, '(a)' ) ''

  do n = 0, 10
    call subset_enum ( n, subset_num )
    write ( *, '(2x,i2,2x,i6)' ) n, subset_num
  end do

  return
end
subroutine subset_intersect_test ( )

!*****************************************************************************80
!
!! SUBSET_INTERSECT_TEST tests SUBSET_INTERSECT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) s1(7)
  integer ( kind = 4 ) s2(7)
  integer ( kind = 4 ) s3(7)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_INTERSECT_TEST'
  write ( *, '(a)' ) '  SUBSET_INTERSECT computes the intersection of subsets.'

  n = 7
  seed = 123456789

  call subset_random ( n, seed, s1 )
  call i4vec_transpose_print ( n, s1, '  Subset S1:' )

  call subset_random ( n, seed, s2 )
  call i4vec_transpose_print ( n, s2, '  Subset S2:' )

  call subset_intersect ( n, s1, s2, s3 )
  call i4vec_transpose_print ( n, s3, '  Intersect:' )

  return
end
subroutine subset_lex_rank_test ( )

!*****************************************************************************80
!
!! SUBSET_LEX_RANK_TEST tests SUBSET_LEX_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) rank
  integer ( kind = 4 ), dimension ( n ) :: t = (/ 0, 1, 0, 1, 0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_LEX_RANK_TEST'
  write ( *, '(a)' ) '  SUBSET_LEX_RANK ranks all subsets of a set,'
  write ( *, '(a)' ) '  using the lexicographic ordering.'

  call subset_lex_rank ( n, t, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The rank of the element:'
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) t(1:n)
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  is computed as ', rank

  return
end
subroutine subset_lex_successor_test ( )

!*****************************************************************************80
!
!! SUBSET_LEX_SUCCESSOR_TEST tests SUBSET_LEX_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_LEX_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  SUBSET_LEX_SUCCESSOR lists all subsets of a set,'
  write ( *, '(a)' ) '  using the lexicographic ordering.'

  rank = -1

  do

    rank_old = rank

    call subset_lex_successor ( n, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, t(1:n)

  end do

  return
end
subroutine subset_lex_unrank_test ( )

!*****************************************************************************80
!
!! SUBSET_LEX_UNRANK_TEST tests SUBSET_LEX_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) rank
  integer ( kind = 4 ) t(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_LEX_UNRANK_TEST'
  write ( *, '(a)' ) '  SUBSET_LEX_UNRANK unranks all subsets of a set,'
  write ( *, '(a)' ) '  using the lexicographic ordering.'

  rank = 10

  call subset_lex_unrank ( rank, n, t )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The element of rank ', rank
  write ( *, '(a)' ) ''
  write ( *, '(6i5)' ) t(1:n)

  return
end
subroutine subset_random_test ( )

!*****************************************************************************80
!
!! SUBSET_RANDOM_TEST tests SUBSET_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ), allocatable :: s(:)
  integer ( kind = 4 ) seed

  write ( *, '(a)' )
  write ( *, '(a)' ) 'SUBSET_RANDOM_TEST'
  write ( *, '(a)' ) '  SUBSET_RANDOM returns a random subset.'

  n = 5
  allocate ( s(1:n) )
  seed = 123456789

  do i = 1, 10
    call subset_random ( n, seed, s )
    call i4vec_transpose_print ( n, s, '' )
  end do

  deallocate ( s )

  return
end
subroutine subset_union_test ( )

!*****************************************************************************80
!
!! SUBSET_UNION_TEST tests SUBSET_UNION.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) s1(7)
  integer ( kind = 4 ) s2(7)
  integer ( kind = 4 ) s3(7)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_UNION_TEST'
  write ( *, '(a)' ) '  SUBSET_UNION computes the union of subsets.'

  n = 7
  seed = 123456789

  call subset_random ( n, seed, s1 )
  call i4vec_transpose_print ( n, s1, '  Subset S1:' )

  call subset_random ( n, seed, s2 )
  call i4vec_transpose_print ( n, s2, '  Subset S2:' )

  call subset_union ( n, s1, s2, s3 )
  call i4vec_transpose_print ( n, s3, '  Union:    ' )

  return
end
subroutine subset_weight_test ( )

!*****************************************************************************80
!
!! SUBSET_WEIGHT_TEST tests SUBSET_WEIGHT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) s(5)
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) weight

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_WEIGHT_TEST'
  write ( *, '(a)' ) '  SUBSET_WEIGHT returns the weight of a subset.'

  n = 5
  seed = 123456789

  call subset_random ( n, seed, s )
  call i4vec_transpose_print ( n, s, '  Subset S:' )

  call subset_weight ( n, s, weight )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Subset weight is ', weight

  return
end
subroutine subset_xor_test ( )

!*****************************************************************************80
!
!! SUBSET_XOR_TEST tests SUBSET_XOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) s1(7)
  integer ( kind = 4 ) s2(7)
  integer ( kind = 4 ) s3(7)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_XOR_TEST'
  write ( *, '(a)' ) '  SUBSET_XOR computes the exclusive OR of subsets.'

  n = 7
  seed = 123456789

  call subset_random ( n, seed, s1 )
  call i4vec_transpose_print ( n, s1, '  Subset S1:' )

  call subset_random ( n, seed, s2 )
  call i4vec_transpose_print ( n, s2, '  Subset S2:' )

  call subset_xor ( n, s1, s2, s3 )
  call i4vec_transpose_print ( n, s3, '  XOR:      ' )

  return
end
subroutine subsetsum_swap_test ( )

!*****************************************************************************80
!
!! SUBSETSUM_SWAP_TEST tests SUBSETSUM_SWAP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) index(n)
  integer ( kind = 4 ) sum_achieved
  integer ( kind = 4 ) sum_desired

  sum_desired = 17

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSETSUM_SWAP_TEST'
  write ( *, '(a)' ) '  SUBSETSUM_SWAP seeks a solution of the subset'
  write ( *, '(a)' ) '  sum problem using pair swapping.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The desired sum is ', sum_desired

  a(1:7) = (/ 12, 8, 11, 30, 8, 3, 7 /)

  call subsetsum_swap ( n, a, sum_desired, index, sum_achieved )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    A(I), INDEX(I)'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,2i5)' ) a(i), index(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The achieved sum is ', sum_achieved

  return
end
subroutine tableau_check_test ( )

!*****************************************************************************80
!
!! TABLEAU_CHECK_TEST tests TABLEAU_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical check
  integer ( kind = 4 ) n
  integer ( kind = 4 ), allocatable :: t(:,:)
!
!  Moronic compiler won't allow an empty array!
!
  integer ( kind = 4 ) :: t1(2,1) = reshape ( (/ &
    99, 99 /), (/ 2, 1 /) )
  integer ( kind = 4 ) :: t2(2,4) = reshape ( (/ &
    1, 2, &
    2, 4, &
    3, 7, &
    4, 9 /), (/ 2, 4 /) )
  integer ( kind = 4 ) :: t3(2,4) = reshape ( (/ &
    1, 2, &
    3, 4, &
    5, 5, &
    3, 3 /), (/ 2, 4 /) )
  integer ( kind = 4 ) :: t4(2,4) = reshape ( (/ &
    1, 2, &
    3, 4, &
    4, 5, &
    5, 3 /), (/ 2, 4 /) )
  integer ( kind = 4 ) :: t5(2,4) = reshape ( (/ &
    1, 3, &
    3, 4, &
    6, 7, &
    7, 8 /), (/ 2, 4 /) )
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TABLEAU_CHECK TEST'
  write ( *, '(a)' ) '  TABLEAU_CHECK checks a 2xN tableau.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Check?'
  write ( *, '(a)' ) ''
  
  do test = 1, 5
    if ( test == 1 ) then
      n = 0
      allocate ( t(1:2,1:n) )
      t(1:2,1:n) = t1(1:2,1:n)
    else if ( test == 2 ) then
      n = 4
      allocate ( t(1:2,1:n) )
      t(1:2,1:n) = t2(1:2,1:n)
    else if ( test == 3 ) then
      n = 4
      allocate ( t(1:2,1:n) )
      t(1:2,1:n) = t3(1:2,1:n)
    else if ( test == 4 ) then
      n = 4
      allocate ( t(1:2,1:n) )
      t(1:2,1:n) = t4(1:2,1:n)
    else if ( test == 5 ) then
      n = 4
      allocate ( t(1:2,1:n) )
      t(1:2,1:n) = t5(1:2,1:n)
    end if

    write ( *, '(a)' ) ''
    call tableau_check ( n, t, check )
    write ( *, '(a,l)' ) '      Check = ', check
    call i4mat_print ( 2, n, t, '  Tableau:' )
    deallocate ( t )
  end do

  return
end
subroutine tableau_enum_test ( )

!*****************************************************************************80
!
!! TABLEAU_ENUM_TEST tests TABLEAU_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) tableau_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TABLEAU_ENUM_TEST'
  write ( *, '(a)' ) '  TABLEAU_ENUM enumerates tableaus on N nodes.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N       #'
  write ( *, '(a)' ) ''

  do n = 0, 10
    call tableau_enum ( n, tableau_num )
    write ( *, '(2x,i2,2x,i6)' ) n, tableau_num
  end do

  return
end
subroutine tableau_to_bal_seq_test ( )

!*****************************************************************************80
!
!! TABLEAU_TO_BAL_SEQ_TEST tests TABLEAU_TO_BAL_SEQ.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) t(2*n)
  integer ( kind = 4 ), dimension ( 2, 4 ) :: tab = reshape ( (/ &
    1, 3, 2, 4, 5, 7, 6, 8 /), (/ 2, 4 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TABLEAU_TO_BAL_SEQ_TEST'
  write ( *, '(a)' ) '  TABLEAU_TO_BAL_SEQ converts a tableau'
  write ( *, '(a)' ) '  to a balanced sequence.'

  call i4mat_print ( 2, n, tab, '  Tableau:' )

  call tableau_to_bal_seq ( n, tab, t )

  call i4vec_transpose_print ( 2 * n, t, '  Balanced sequence:' )

  return
end
subroutine tree_check_test ( )

!*****************************************************************************80
!
!! TREE_CHECK_TEST tests TREE_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical ( kind = 4 ) check
  integer ( kind = 4 ) n
  integer ( kind = 4 ), allocatable :: t(:,:)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TREE_CHECK TEST'
  write ( *, '(a)' ) '  TREE_CHECK checks a tree.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Check?'
  write ( *, '(a)' ) ''
  
  do test = 1, 4

    if ( test == 1 ) then
      n = 0
      allocate ( t(2,1) )
    else if ( test == 2 ) then
      n = 3
      allocate ( t(2,n-1) )
      t = reshape ( (/ 1, 2, 2, 3 /), (/ 2, n - 1 /) )
    else if ( test == 3 ) then
      n = 5
      allocate ( t(2,n-1) )
      t = reshape ( (/ 1, 2, 3, 4, 4, 5, 5, 3 /), (/ 2, n - 1 /) )
    else if ( test == 4 ) then
      n = 6
      allocate ( t(2,n-1) )
      t = reshape ( (/ 1, 3, 2, 3, 3, 4, 4, 5, 5, 6 /), (/ 2, n - 1 /) )
    end if

    write ( *, '(a)' ) ''
    call tree_check ( n, t, check )
    write ( *, '(a,l)' ) '      Check = ', check
    call i4mat_print ( 2, n - 1, t, '  Tree:' );
    deallocate ( t )

  end do

  return
end
subroutine tree_enum_test ( )

!*****************************************************************************80
!
!! TREE_ENUM_TEST tests TREE_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) tree_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TREE_ENUM_TEST'
  write ( *, '(a)' ) '  TREE_ENUM enumerates trees on N nodes.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N         #'
  write ( *, '(a)' ) ''

  do n = 0, 10
    call tree_enum ( n, tree_num )
    write ( *, '(2x,i2,2x,i10)' ) n, tree_num
  end do

  return
end
subroutine tree_rank_test ( )

!*****************************************************************************80
!
!! TREE_RANK_TEST tests TREE_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) rank
  integer ( kind = 4 ), dimension ( 2, n - 1 ) :: t = reshape ( (/ &
    4, 3, 3, 1, 2, 1 /), (/ 2, n - 1 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TREE_RANK_TEST'
  write ( *, '(a)' ) '  TREE_RANK ranks trees.'

  call tree_rank ( n, t, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The rank of the element:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5i5)' ) t(1,1:n-1)
  write ( *, '(2x,5i5)' ) t(2,1:n-1)
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  is computed as ', rank

  return
end
subroutine tree_successor_test ( )

!*****************************************************************************80
!
!! TREE_SUCCESSOR_TEST tests TREE_SUCCESSOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_old
  integer ( kind = 4 ) t(2,n-1)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TREE_SUCCESSOR_TEST'
  write ( *, '(a)' ) '  TREE_SUCCESOR lists trees.'

  rank = -1

  do

    rank_old = rank

    call tree_successor ( n, t, rank )

    if ( rank <= rank_old ) then
      exit
    end if

    write ( *, '(i5,2x,5i5)' ) rank, t(1,1:n-1)
    write ( *, '(5x,2x,5i5)' )       t(2,1:n-1)

  end do

  return
end
subroutine tree_to_pruefer_test ( )

!*****************************************************************************80
!
!! TREE_TO_PRUEFER_TEST tests TREE_TO_PRUEFER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) p(n-2)
  integer ( kind = 4 ) pruefer_num
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) t(2,n-1)
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TREE_TO_PRUEFER_TEST'
  write ( *, '(a)' ) '  TREE_TO_PRUEFER converts a tree to a Pruefer code.'

  call pruefer_enum ( n, pruefer_num )

  i4_lo = 0
  i4_hi = pruefer_num - 1

  do test = 1, test_num
!
!  Pick a "random" Pruefer code.
!
    rank = i4_uniform_ab ( i4_lo, i4_hi, seed )

    call pruefer_unrank ( rank, n, p )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Random Pruefer code of rank ', rank
    write ( *, '(6i5)' ) p(1:n-2)
!
!  Convert the Pruefer code to a tree.
!
    call pruefer_to_tree ( n, p, t )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Edge list for the corresponding tree:'
    write ( *, '(a)' ) ''
    do j = 1, n - 1
      write ( *, '(6i5)' ) j, t(1:2,j)
    end do
!
!  Convert the tree to a Pruefer code.
!
    call tree_to_pruefer ( n, t, p )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Corresponding Pruefer code:'
    write ( *, '(6i5)' ) p(1:n-2)

  end do

  return
end
subroutine tree_unrank_test ( )

!*****************************************************************************80
!
!! TREE_UNRANK_TEST tests TREE_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) rank
  integer ( kind = 4 ) t(2,n-1)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TREE_UNRANK_TEST'
  write ( *, '(a)' ) '  TREE_UNRANK unranks trees.'

  rank = 8

  call tree_unrank ( rank, n, t )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The element of rank ', rank
  write ( *, '(a)' ) ''
  write ( *, '(2x,5i5)' ) t(1,1:n-1)
  write ( *, '(2x,5i5)' ) t(2,1:n-1)

  return
end
