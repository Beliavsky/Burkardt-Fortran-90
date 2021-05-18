program main

!*****************************************************************************80
!
!! MAIN is the main program for I4LIB_TEST.
!
!  Discussion:
!
!    I4LIB_TEST tests the I4LIB library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4LIB_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the I4LIB library.'

  call i4_abs_test ( )
  call i4_and_test ( )
  call i4_bclr_test ( )
  call i4_bit_hi1_test ( )
  call i4_bit_lo0_test ( )
  call i4_bit_lo1_test ( )
  call i4_bit_reverse_test ( )
  call i4_bset_test ( )
  call i4_btest_test ( )
  call i4_ceiling_test ( )
  call i4_characteristic_test ( )
  call i4_choose_test ( )
  call i4_choose_check_test ( )
  call i4_choose_log_test ( )
  call i4_div_rounded_test ( )
  call i4_divp_test ( )
  call i4_factorial_test ( )
  call i4_factorial_log_test ( )
  call i4_factorial2_test ( )
  call i4_fall_test ( )
  call i4_floor_test ( )
  call i4_gcd_test ( )
  call i4_huge_test ( )
  call i4_huge_normalizer_test ( )
  call i4_is_even_test ( )
  call i4_is_odd_test ( )
  call i4_is_power_of_2_test ( )
  call i4_is_power_of_10_test ( )
  call i4_is_prime_test ( )
  call i4_lcm_test ( )
  call i4_lcm_12n_test ( )
  call i4_log_10_test ( )
  call i4_log_2_test ( )
  call i4_log_i4_test ( )
  call i4_log_r8_test ( )
  call i4_mant_test ( )
  call i4_max_test ( )
  call i4_min_test ( )
  call i4_moddiv_test ( )
  call i4_modp_test ( )
  call i4_normal_ab_test ( )
  call i4_not_test ( )
  call i4_or_test ( )
  call i4_power_test ( )
  call i4_rise_test ( )
  call i4_sign_test ( )
  call i4_sign3_test ( )
  call i4_swap_test ( )
  call i4_to_halton_test ( )
  call i4_to_pascal_test ( )
  call i4_to_pascal_degree_test ( )
  call i4_to_triangle_lower_test ( )
  call i4_uniform_ab_test ( )
  call i4_walsh_1d_test ( )
  call i4_width_test ( )
  call i4_wrap_test ( )
  call i4_xor_test ( ) 

  call i4block_print_test ( )

  call i4col_find_item_test ( )
  call i4col_find_pair_wrap_test ( )
  call i4col_sort_a_test ( )
  call i4col_sort_d_test ( )
  call i4col_sort2_a_test ( )
  call i4col_sorted_singleton_count_test ( )
  call i4col_sorted_unique_count_test ( )

  call i4mat_elim_test ( )
  call i4mat_indicator_test ( )
  call i4mat_l1_inverse_test ( )
  call i4mat_max_test ( )
  call i4mat_max_index_test ( )
  call i4mat_min_test ( )
  call i4mat_min_index_test ( )
  call i4mat_perm_uniform_test ( )
  call i4mat_print_test ( )
  call i4mat_print_some_test ( )
  call i4mat_product_elementwise_test ( )
  call i4mat_rank_test ( )
  call i4mat_ref_test ( )
  call i4mat_row_reduce_test ( )
  call i4mat_row_swap_test ( )
  call i4mat_rref_test ( )
  call i4mat_rref_system_test ( )
  call i4mat_transpose_test ( )
  call i4mat_u_solve_test ( )
  call i4mat_u1_inverse_test ( )
  call i4mat_width_test ( )

  call i4row_max_test ( )
  call i4row_mean_test ( )
  call i4row_min_test ( )
  call i4row_sort_a_test ( )
  call i4row_sort_d_test ( )
  call i4row_sort2_d_test ( )
  call i4row_sum_test ( )
  call i4row_swap_test ( )
  call i4row_variance_test ( )

  call i4rows_to_i4mat_test ( )

  call i4vec_add_test ( )
  call i4vec_amax_test ( )
  call i4vec_amax_index_test ( )
  call i4vec_amin_test ( )
  call i4vec_amin_index_test ( )
  call i4vec_aminz_test ( )
  call i4vec_aminz_index_test ( )
  call i4vec_ascend_sub_test ( )
  call i4vec_binary_next_test ( )
  call i4vec_bracket_test ( )
  call i4vec_choose_test ( )
  call i4vec_concatenate_test ( )
  call i4vec_cum_test ( )
  call i4vec_cum0_test ( )
  call i4vec_decrement_test ( )
  call i4vec_direct_product_test ( )
  call i4vec_direct_product2_test ( )
  call i4vec_distances_test ( )
  call i4vec_dot_product_test ( )
  call i4vec_frac_test ( )
  call i4vec_heap_a_test ( )
  call i4vec_heap_d_test ( )
  call i4vec_heap_d_extract_test ( )
  call i4vec_heap_d_insert_test ( )
  call i4vec_heap_d_max_test ( )
  call i4vec_histogram_test ( )
  call i4vec_identity_row_test ( )
  call i4vec_increment_test ( )
  call i4vec_index_test ( )
  call i4vec_index_delete_all_test ( )
  call i4vec_index_delete_dupes_test ( )
  call i4vec_index_delete_one_test ( )
  call i4vec_index_insert_test ( )
  call i4vec_index_insert_unique_test ( )
  call i4vec_index_order_test ( )
  call i4vec_index_search_test ( )
  call i4vec_indexed_heap_d_test ( )
  call i4vec_indexed_heap_d_extract_test ( )
  call i4vec_indexed_heap_d_insert_test ( )
  call i4vec_indexed_heap_d_max_test ( )
  call i4vec_indicator0_test ( )
  call i4vec_indicator1_test ( )
  call i4vec_insert_test ( )
  call i4vec_is_ascending_test ( )
  call i4vec_is_binary_test ( )
  call i4vec_is_descending_test ( )
  call i4vec_is_pairwise_prime_test ( )
  call i4vec_max_test ( )
  call i4vec_max_index_test ( )
  call i4vec_max_index_last_test ( )
  call i4vec_max_last_test ( )
  call i4vec_mean_test ( )
  call i4vec_median_test ( )
  call i4vec_merge_a_test ( )
  call i4vec_min_test ( )
  call i4vec_min_index_test ( )
  call i4vec_nonzero_count_test ( )
  call i4vec_nonzero_first_test ( )
  call i4vec_order_type_test ( )
  call i4vec_part_test ( )
  call i4vec_part_quick_a_test ( )
  call i4vec_permute_test ( )
  call i4vec_permute_uniform_test ( )
  call i4vec_print_test ( )
  call i4vec_red_test ( )
  call i4vec_reverse_test ( )
  call i4vec_run_count_test ( )
  call i4vec_search_binary_a_test ( )
  call i4vec_sort_bubble_a_test ( )
  call i4vec_sort_heap_a_test ( )
  call i4vec_sort_heap_d_test ( )
  call i4vec_sort_heap_index_a_test ( )
  call i4vec_sort_heap_index_d_test ( )
  call i4vec_sort_insert_a_test ( )
  call i4vec_sort_quick_a_test ( )
  call i4vec_sort_shell_a_test ( )
  call i4vec_sorted_undex_test ( )
  call i4vec_sorted_unique_test ( )
  call i4vec_sorted_unique_count_test ( )
  call i4vec_sorted_unique_hist_test ( )
  call i4vec_sum_test ( )
  call i4vec_sum_vec_test ( )
  call i4vec_transpose_print_test ( )
  call i4vec_undex_test ( )

  call i4vec_uniform_ab_test ( )
  call i4vec_unique_count_test ( )
  call i4vec_unique_index_test ( )
  call i4vec_value_index_test ( )
  call i4vec_variance_test ( )
  call i4vec_width_test ( )

  call i4vec2_print_test ( )
  call i4vec2_sort_a_test ( )
  call i4vec2_sort_d_test ( )
  call i4vec2_sorted_unique_test ( )

  call ksub_next4_test ( )

  call pascal_to_i4_test ( )

  call perm0_check_test ( )
  call perm0_uniform_test ( )
  call perm1_check_test ( )
  call perm1_uniform_test ( )

  call permutation_symbol_test ( )

  call prime_test ( )

  call triangle_lower_to_i4_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4LIB_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine i4_abs_test ( )

!*****************************************************************************80
!
!! I4_ABS_TEST tests I4_ABS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) i4_abs
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_ABS_TEST'
  write ( *, '(a)' ) '  I4_ABS returns the absolute value of an I4.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       A       B=I4_ABS(A)'
  write ( *, '(a)' ) ''

  i4_lo = -100
  i4_hi = +100
  seed = 123456789
  do i = 1, 10
    a = i4_uniform_ab ( i4_lo, i4_hi, seed )
    b = i4_abs ( a )
    write ( *, '(2x,i8,2x,i8)' ) a, b
  end do

  return
end
subroutine i4_and_test ( )

!*****************************************************************************80
!
!! I4_AND_TEST tests I4_AND.
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

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_and
  integer ( kind = 4 ) :: i4_hi = 100
  integer ( kind = 4 ) :: i4_lo = 0
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_AND_TEST'
  write ( *, '(a)' ) '  I4_AND returns the bitwise AND of'
  write ( *, '(a)' ) '  two integers.'
  write ( *, '(a)' ) '  Compare with FORTRAN90 intrinsic IAND.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I         J    I4_AND      IAND'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    i = i4_uniform_ab ( i4_lo, i4_hi, seed )
    j = i4_uniform_ab ( i4_lo, i4_hi, seed )
    k = i4_and ( i, j )
    l = iand ( i, j )
    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k, l
  end do

  return
end
subroutine i4_bclr_test ( )

!*****************************************************************************80
!
!! I4_BCLR_TEST tests I4_BCLR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 2

  integer ( kind = 4 ) i4
  integer ( kind = 4 ), dimension ( test_num ) :: i4_test = (/ &
    101, -31 /)
  integer ( kind = 4 ) i4_bclr
  integer ( kind = 4 ) j
  integer ( kind = 4 ) pos
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_BCLR_TEST'
  write ( *, '(a)' ) '  I4_BCLR sets a given bit to 0.'

  do test = 1, test_num

    i4 = i4_test(test)

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Working on I4 = ', i4
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '       Pos     I4_BCLR(I4,POS)'
    write ( *, '(a)' ) ''

    do pos = 0, 31
  
      j = i4_bclr ( i4, pos )

      write ( *, '(2x,i8,2x,i12)' ) pos, j

    end do

  end do

  return
end
subroutine i4_bit_hi1_test ( )

!*****************************************************************************80
!
!! I4_BIT_HI1_TEST tests I4_BIT_HI1.
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

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) i4_bit_hi1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_BIT_HI1_TEST'
  write ( *, '(a)' ) '  I4_BIT_HI1 returns the location of the high 1 bit.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       I  I4_BIT_HI1(I)'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    i = i4_uniform_ab ( 0, 100, seed )
    j = i4_bit_hi1 ( i )
    write ( *, '(2x,i8,2x,i8)' ) i, j
  end do

  return
end
subroutine i4_bit_lo0_test ( )

!*****************************************************************************80
!
!! I4_BIT_LO0_TEST tests I4_BIT_LO0.
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

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) i4_bit_lo0
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_BIT_LO0_TEST'
  write ( *, '(a)' ) '  I4_BIT_LO0 returns the location of the low 0 bit.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       I  I4_BIT_LO0(I)'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    i = i4_uniform_ab ( 0, 100, seed )
    j = i4_bit_lo0 ( i )
    write ( *, '(2x,i8,2x,i8)' ) i, j
  end do

  return
end
subroutine i4_bit_lo1_test ( )

!*****************************************************************************80
!
!! I4_BIT_LO1_TEST tests I4_BIT_LO1.
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

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) i4_bit_lo1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_BIT_LO1_TEST'
  write ( *, '(a)' ) '  I4_BIT_LO1 returns the location of the low 1 bit.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       I  I4_BIT_LO1(I)'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    i = i4_uniform_ab ( 0, 100, seed )
    j = i4_bit_lo1 ( i )
    write ( *, '(2x,i8,2x,i8)' ) i, j
  end do

  return
end
subroutine i4_bit_reverse_test ( )

!*****************************************************************************80
!
!! I4_BIT_REVERSE_TEST tests I4_BIT_REVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_hi
  integer ( kind = 4 ) i4_bit_reverse
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_BIT_REVERSE_TEST'
  write ( *, '(a)' ) '  I4_BIT_REVERSE bit reverses I with respect to 2^J'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I         J  I4_BIT_REVERSE(I,J)'
  write ( *, '(a)' ) ''

  do j = 0, 4
    i_hi = 2 ** j - 1
    do i = 0, i_hi
      k = i4_bit_reverse ( i, j )
      write ( *, '(2x,i8,2x,i8,2x,i8)' ) i, j, k
    end do
  end do

  return
end
subroutine i4_bset_test ( )

!*****************************************************************************80
!
!! I4_BSET_TEST tests I4_BSET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 2

  integer ( kind = 4 ) i4
  integer ( kind = 4 ), dimension ( test_num ) :: i4_test = (/ &
    101, -31 /)
  integer ( kind = 4 ) i4_bset
  integer ( kind = 4 ) j
  integer ( kind = 4 ) pos
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_BSET_TEST'
  write ( *, '(a)' ) '  I4_BSET sets a given bit to 1.'

  do test = 1, test_num

    i4 = i4_test(test)

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Working on I4 = ', i4
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '       Pos     I4_BSET(I4,POS)'
    write ( *, '(a)' ) ''

    do pos = 0, 31
  
      j = i4_bset ( i4, pos )

      write ( *, '(2x,i8,2x,i12)' ) pos, j

    end do

  end do

  return
end
subroutine i4_btest_test ( )

!*****************************************************************************80
!
!! I4_BTEST_TEST tests I4_BTEST.
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

  integer ( kind = 4 ), parameter :: test_num = 2

  integer ( kind = 4 ) i4
  integer ( kind = 4 ), dimension ( test_num ) :: i4_test = (/ &
    101, -31 /)
  logical i4_btest
  logical j
  integer ( kind = 4 ) pos
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_BTEST_TEST'
  write ( *, '(a)' ) '  I4_BTEST reports whether a given bit is 0 or 1.'

  do test = 1, test_num

    i4 = i4_test(test)

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Analyze the integer I4 = ', i4
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '       Pos     I4_BTEST(I4,POS)'
    write ( *, '(a)' ) ''

    do pos = 0, 31
  
      j = i4_btest ( i4, pos )

      write ( *, '(2x,i8,13x,l1)' ) pos, j

    end do

  end do

  return
end
subroutine i4_ceiling_test ( )

!*****************************************************************************80
!
!! I4_CEILING_TEST tests I4_CEILING.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4
  integer ( kind = 4 ) i4_ceiling
  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed

  r8_lo = -100.0D+00
  r8_hi =  100.0D+00
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_CEILING_TEST'
  write ( *, '(a)' ) '  I4_CEILING evaluates the "ceiling" of an R8.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      R8    I4_CEILING(R8)'
  write ( *, '(a)' ) ''

  do i = 1, 10
    r8 = r8_uniform_ab ( r8_lo, r8_hi, seed )
    i4 = i4_ceiling ( r8 )
    write ( *, '(2x,f8.4,12x,i4)' ) r8, i4
  end do
 
  return
end
subroutine i4_characteristic_test ( )

!*****************************************************************************80
!
!! I4_CHARACTERISTIC_TEST tests I4_CHARACTERISTIC.
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

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_characteristic

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_CHARACTERISTIC_TEST'
  write ( *, '(a)' ) '  I4_CHARACTERISTIC computes the characteristic'
  write ( *, '(a)' ) '  of an integer Q, which is  '
  write ( *, '(a)' ) '    Q if Q is prime;'
  write ( *, '(a)' ) '    P, if Q = P^N for some prime P;'
  write ( *, '(a)' ) '    0, if Q is negative, 0, 1, or the product of '
  write ( *, '(a)' ) '      more than 1 distinct prime.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I  I4_CHARACTERISTIC'
  write ( *, '(a)' ) ''

  do i = 1, 50
    write ( *, '(2x,i2,13x,i4)' ) i, i4_characteristic ( i )
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_CHOOSE_TEST'
  write ( *, '(a)' ) '  I4_CHOOSE evaluates C(N,K).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         K       CNK'
 
  do n = 0, 4
    write ( *, '(a)' ) ''
    do k = 0, n
      cnk = i4_choose ( n, k )
      write ( *, '(2x,i8,2x,i8,2x,i8)' ) n, k, cnk
    end do
  end do
 
  return
end
subroutine i4_choose_check_test ( )

!*****************************************************************************80
!
!! I4_CHOOSE_CHECK_TEST tests I4_CHOOSE_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical ( kind = 4 ) check
  integer ( kind = 4 ) cnk
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_choose
  logical ( kind = 4 ) i4_choose_check
  integer ( kind = 4 ) k
  integer ( kind = 4 ), save :: k_test(4) = (/ &
    3, 999, 3, 10 /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ), save :: n_test(4) = (/ &
    10, 1000, 100, 100 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_CHOOSE_CHECK_TEST'
  write ( *, '(a)' ) '  I4_CHOOSE_CHECK checks whether C(N,K)'
  write ( *, '(a)' ) '  can be computed with integer arithmetic'
  write ( *, '(a)' ) '  or not.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     N     K    CHECK?    I4_CHOOSE'
  write ( *, '(a)' ) ''
 
  do i = 1, 4
    n = n_test(i)
    k = k_test(i)
    check = i4_choose_check ( n, k )
    if ( check ) then
      cnk = i4_choose ( n, k )
      write ( *, '(2x,i4,2x,i4,8x,l1,8x,i6)' ) n, k, check, cnk
    else
      write ( *, '(2x,i4,2x,i4,8x,l1,a)' ) n, k, check, '   Not computable'
    end if
  end do
 
  return
end
subroutine i4_choose_log_test ( )

!*****************************************************************************80
!
!! I4_CHOOSE_LOG_TEST tests I4_CHOOSE_LOG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) cnk
  real ( kind = 8 ) elcnk
  integer ( kind = 4 ) i4_choose
  real ( kind = 8 ) i4_choose_log
  integer ( kind = 4 ) k
  real ( kind = 8 ) lcnk
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_CHOOSE_LOG_TEST'
  write ( *, '(a)' ) '  I4_CHOOSE_LOG evaluates log(C(N,K)).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         K        lcnk           elcnk           CNK'
 
  do n = 0, 4
    write ( *, '(a)' ) ''
    do k = 0, n
      lcnk = i4_choose_log ( n, k )
      elcnk = exp ( lcnk )
      cnk = i4_choose ( n, k )
      write ( *, '(2x,i8,2x,i8,2x,g14.6,2x,g14.6,2x,i8)' ) n, k, lcnk, elcnk, cnk
    end do
  end do
 
  return
end
subroutine i4_div_rounded_test ( )

!*****************************************************************************80
!
!! I4_DIV_ROUNDED_TEST tests I4_DIV_ROUNDED.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 October 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ), parameter :: a_hi =  100
  integer ( kind = 4 ), parameter :: a_lo = -100
  integer ( kind = 4 ) b
  integer ( kind = 4 ), parameter :: b_hi =  10
  integer ( kind = 4 ), parameter :: b_lo = -10
  real ( kind = 8 ) c0
  integer ( kind = 4 ) c1
  integer ( kind = 4 ) c2
  integer ( kind = 4 ) c3
  integer ( kind = 4 ) c4
  integer ( kind = 4 ) i4_div_rounded
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 20

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_DIV_ROUNDED_TEST'
  write ( *, '(a)' ) '  I4_DIV_ROUNDED performs rounded integer division.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  C0 = real ( a ) / real ( b )'
  write ( *, '(a)' ) '  C1 = I4_DIV_ROUNDED ( A, B )'
  write ( *, '(a)' ) '  C2 = nint ( real ( a ) / real ( b ) )'
  write ( *, '(a)' ) '  C3 = A / B'
  write ( *, '(a)' ) '  C4 = int ( real ( a ) / real ( b ) )'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  C1 and C2 should be equal;'
  write ( *, '(a)' ) '  C3 and C4 should be equal.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     A     B           C0         C1    C2      C3    C4'
  write ( *, '(a)' ) ''

  seed = 123456789

  do test = 1, test_num
    a = i4_uniform_ab ( a_lo, a_hi, seed )
    b = i4_uniform_ab ( b_lo, b_hi, seed )
    if ( b == 0 ) then
      b = 7
    end if
    c0 = real ( a, kind = 8 ) / real ( b, kind = 8 )
    c1 = i4_div_rounded ( a, b )
    c2 = nint ( real ( a, kind = 8 ) / real ( b, kind = 8 ) )
    c3 = a / b
    c4 = int ( real ( a, kind = 8 ) / real ( b, kind = 8 ) )
    write ( *, '(2x,i4,2x,i4,4x,f14.6,2x,i4,2x,i4,4x,i4,2x,i4)' ) &
      a, b, c0, c1, c2, c3, c4
  end do

  return
end
subroutine i4_divp_test ( )

!*****************************************************************************80
!
!! I4_DIVP_TEST tests I4_DIVP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 October 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ), parameter :: a_hi =  100
  integer ( kind = 4 ), parameter :: a_lo = -100
  integer ( kind = 4 ) b
  integer ( kind = 4 ), parameter :: b_hi =  10
  integer ( kind = 4 ), parameter :: b_lo = -10
  integer ( kind = 4 ) c
  integer ( kind = 4 ) d
  integer ( kind = 4 ) i4_divp
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 20

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_DIVP_TEST'
  write ( *, '(a)' ) '  I4_DIVP(A,B) returns the smallest multiplier of J'
  write ( *, '(a)' ) '  that is less than I'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     A     B     C     D'
  write ( *, '(a)' ) ''

  seed = 123456789

  do test = 1, test_num
    a = i4_uniform_ab ( a_lo, a_hi, seed )
    b = i4_uniform_ab ( b_lo, b_hi, seed )
    if ( b == 0 ) then
      b = 7
    end if
    c = i4_divp ( a, b )
    d = c * b
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,i4)' ) a, b, c, d
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
!    05 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) f1
  integer ( kind = 4 ) f2
  integer ( kind = 4 ) i4_factorial
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_FACTORIAL_TEST:'
  write ( *, '(a)' ) '  I4_FACTORIAL evaluates the factorial function:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N      Exact         I4_FACTORIAL(N)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call i4_factorial_values ( n_data, n, f1 )

    if ( n_data == 0 ) then
      exit
    end if

    f2 = i4_factorial ( n )

    write ( *, '(2x,i8,2x,i12,2x,i12)' ) n, f1, f2

  end do

  return
end
subroutine i4_factorial_log_test ( )

!*****************************************************************************80
!
!! I4_FACTORIAL_LOG_TEST tests I4_FACTORIAL_LOG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) elfact
  integer ( kind = 4 ) fact
  real ( kind = 8 ) i4_factorial_log
  real ( kind = 8 ) lfact
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_FACTORIAL_LOG_TEST:'
  write ( *, '(a)' ) '  I4_FACTORIAL_LOG evaluates log(N!):'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N       lfact          elfact              fact'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call i4_factorial_values ( n_data, n, fact )

    if ( n_data == 0 ) then
      exit
    end if

    lfact = i4_factorial_log ( n )
    elfact = exp ( lfact )

    write ( *, '(2x,i8,2x,g14.6,2x,g14.6,2x,i12)' ) n, lfact, elfact, fact

  end do

  return
end
subroutine i4_factorial2_test ( )

!*****************************************************************************80
!
!! I4_FACTORIAL2_TEST tests I4_FACTORIAL2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) f1
  integer ( kind = 4 ) f2
  integer ( kind = 4 ) i4_factorial2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_FACTORIAL2_TEST:'
  write ( *, '(a)' ) '  I4_FACTORIAL2 evaluates the double factorial function:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N      Exact         I4_FACTORIAL2(N)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call i4_factorial2_values ( n_data, n, f1 )

    if ( n_data == 0 ) then
      exit
    end if

    f2 = i4_factorial2 ( n )

    write ( *, '(2x,i8,2x,i12,2x,i12)' ) n, f1, f2

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
subroutine i4_floor_test ( )

!*****************************************************************************80
!
!! I4_FLOOR_TEST tests I4_FLOOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4
  integer ( kind = 4 ) i4_floor
  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed

  r8_lo = -100.0D+00
  r8_hi =  100.0D+00
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_FLOOR_TEST'
  write ( *, '(a)' ) '  I4_FLOOR evaluates the "floor" of a real number.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      R8      I4_FLOOR(R8)'
  write ( *, '(a)' ) ''

  do i = 1, 10
    r8 = r8_uniform_ab ( r8_lo, r8_hi, seed )
    i4 = i4_floor ( r8 )
    write ( *, '(2x,f8.4,12x,i4)' ) r8, i4
  end do
 
  return
end
subroutine i4_gcd_test ( )

!*****************************************************************************80
!
!! I4_GCD_TEST tests I4_GCD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 7

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_gcd
  integer ( kind = 4 ), dimension(test_num) :: i_test = (/ &
    36, 49, 0, 12, 36, 1, 91 /)
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension(test_num) :: j_test = (/ &
    30, -7, 71, 12, 49, 42, 28 /)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_GCD_TEST'
  write ( *, '(a)' ) '  I4_GCD computes the greatest common factor,'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I       J  I4_GCD'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    i = i_test(test)
    j = j_test(test)
    write ( *, '(2x,3i8)') i, j, i4_gcd ( i, j )
  end do

  return
end
subroutine i4_huge_test ( )

!*****************************************************************************80
!
!! I4_HUGE_TEST tests I4_HUGE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i4_huge
  integer ( kind = 4 ) dummy

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_HUGE_TEST'
  write ( *, '(a)' ) '  I4_HUGE returns a huge integer.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i12)' ) '  I4_HUGE() = ', i4_huge ( )
  write ( *, '(a,i12)' ) '  HUGE(1) =   ', huge ( dummy )

  return
end
subroutine i4_huge_normalizer_test ( )

!*****************************************************************************80
!
!! I4_HUGE_NORMALIZER_TEST tests I4_HUGE_NORMALIZER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i4
  integer ( kind = 4 ) i4_huge
  real ( kind = 8 ) i4_huge_normalizer
  real ( kind = 8 ) r8
  real ( kind = 8 ) value

  i4 = i4_huge ( )
  r8 = i4_huge_normalizer ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_HUGE_NORMALIZER_TEST'
  write ( *, '(a)' ) '  I4_HUGE_NORMALIZER returns 1/(I4_HUGE+1).'
  write ( *, '(a)' ) ''
  write ( *, '(a,i12)' ) '  I4_HUGE() = ', i4
  write ( *, '(a,g14.6)' ) '  I4_HUGE_NORMALIZER() = ', r8

  value = real ( i4, kind = 8 ) * r8

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  I4_HUGE * I4_HUGE_NORMALIZER = ', value

  return
end
subroutine i4_is_even_test ( )

!*****************************************************************************80
!
!! I4_IS_EVEN_TEST tests I4_IS_EVEN.
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

  integer ( kind = 4 ) i
  logical ( kind = 4 ) i4_is_even

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_IS_EVEN_TEST'
  write ( *, '(a)' ) '  I4_IS_EVEN reports whether an I4 is even.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I     I4_IS_EVEN(I)'
  write ( *, '(a)' ) ''

  do i = -2, 25
    write ( *, '(2x,i8,2x,l1)' ) i, i4_is_even ( i )
  end do

  return
end
subroutine i4_is_odd_test ( )

!*****************************************************************************80
!
!! I4_IS_ODD_TEST tests I4_IS_ODD.
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

  integer ( kind = 4 ) i
  logical ( kind = 4 ) i4_is_odd

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_IS_ODD_TEST'
  write ( *, '(a)' ) '  I4_IS_ODD reports whether an I4 is odd.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I     I4_IS_ODD(I)'
  write ( *, '(a)' ) ''

  do i = -2, 25
    write ( *, '(2x,i8,2x,l1)' ) i, i4_is_odd ( i )
  end do

  return
end
subroutine i4_is_power_of_2_test ( )

!*****************************************************************************80
!
!! I4_IS_POWER_OF_2_TEST tests I4_IS_POWER_OF_2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  logical ( kind = 4 ) i4_is_power_of_2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_IS_POWER_OF_2_TEST'
  write ( *, '(a)' ) '  I4_IS_POWER_OF_2 reports whether an I4 is a power of 2.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I     I4_IS_POWER_OF_2(I)'
  write ( *, '(a)' ) ''

  do i = -4, 25
    write ( *, '(2x,i6,2x,l1)' ) i, i4_is_power_of_2 ( i )
  end do

  return
end
subroutine i4_is_power_of_10_test ( )

!*****************************************************************************80
!
!! I4_IS_POWER_OF_10_TEST tests I4_IS_POWER_OF_10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  logical ( kind = 4 ) i4_is_power_of_10

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_IS_POWER_OF_10_TEST'
  write ( *, '(a)' ) '  I4_IS_POWER_OF_10 reports whether an I4 is a power of 10.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I     I4_IS_POWER_OF_10(I)'
  write ( *, '(a)' ) ''

  do i = 97, 103
    write ( *, '(2x,i6,2x,l1)' ) i, i4_is_power_of_10 ( i )
  end do

  return
end
subroutine i4_is_prime_test ( )

!*****************************************************************************80
!
!! I4_IS_PRIME_TEST tests I4_IS_PRIME.
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

  integer ( kind = 4 ) i
  logical ( kind = 4 ) i4_is_prime

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_IS_PRIME_TEST'
  write ( *, '(a)' ) '  I4_IS_PRIME reports whether an I4 is prime.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I     I4_IS_PRIME(I)'
  write ( *, '(a)' ) ''

  do i = -2, 25
    write ( *, '(2x,i8,2x,l1)' ) i, i4_is_prime ( i )
  end do

  return
end
subroutine i4_lcm_test ( )

!*****************************************************************************80
!
!! I4_LCM_TEST tests I4_LCM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 7

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_lcm
  integer ( kind = 4 ), dimension(test_num) :: i_test = (/ &
    36, 49, 0, 12, 36, 1, 91 /)
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension(test_num) :: j_test = (/ &
    30, -7, 71, 12, 49, 42, 28 /)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_LCM_TEST'
  write ( *, '(a)' ) '  I4_LCM computes the least common multiple.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I     J   I4_LCM'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    i = i_test(test)
    j = j_test(test)
    write ( *, '(2x,4i8)') i, j, i4_lcm ( i, j )
  end do

  return
end
subroutine i4_lcm_12n_test ( )

!*****************************************************************************80
!
!! I4_LCM_12N_TEST tests I4_LCM_12N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 August 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none


  integer ( kind = 4 ) i4_lcm_12n
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_LCM_12N_TEST'
  write ( *, '(a)' ) '  I4_LCM_12N computes the least common multiple'
  write ( *, '(a)' ) '  of integer 1 through N'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     N   I4_LCM_12N'
  write ( *, '(a)' ) ''

  do n = 1, 10
    write ( *, '(2x,i2,2x,i10)') n, i4_lcm_12n ( n )
  end do

  return
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_LOG_10_TEST'
  write ( *, '(a)' ) '  I4_LOG_10: whole part of log base 10,'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  X, I4_LOG_10'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    x = x_test(test)
    write ( *, '( 2x, i8, i12 )' ) x, i4_log_10 ( x )
  end do

  return
end
subroutine i4_log_2_test ( )

!*****************************************************************************80
!
!! I4_LOG_2_TEST tests I4_LOG_2.
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

  integer ( kind = 4 ), parameter :: test_num = 17

  integer ( kind = 4 ) i4_log_2
  integer ( kind = 4 ) test
  integer ( kind = 4 ) x
  integer ( kind = 4 ), dimension ( test_num ) :: x_test = (/ &
      0,    1,    2,    3,    9, &
     10,   11,   99,  101,   -1, &
     -2,   -3,   -9, 1000, 1023, &
   1024, 1025 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_LOG_2_TEST'
  write ( *, '(a)' ) '  I4_LOG_2: whole part of log base 2.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X     I4_LOG_2'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    x = x_test(test)
    write ( *, '( 2x, i8, i12 )' ) x, i4_log_2 ( x )
  end do

  return
end
subroutine i4_log_i4_test ( )

!*****************************************************************************80
!
!! I4_LOG_I4_TEST tests I4_LOG_I4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer i4
  integer i4_log_i4
  integer j4

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_LOG_I4_TEST'
  write ( *, '(a)' ) '  I4_LOG_I4: logarithm of I4 base J4,'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        I4        J4 I4_LOG_I4'
  write ( *, '(a)' ) ''

  do j4 = 2, 5
    do i4 = 0, 10
      write ( *, '(2x, i8, 2x, i8, 2x, i8 )' ) i4, j4, i4_log_i4 ( i4, j4 )
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine i4_log_r8_test ( )

!*****************************************************************************80
!
!! I4_LOG_R8_TEST tests I4_LOG_R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 10

  real ( kind = 8 ) b
  real ( kind = 8 ), dimension(test_num) :: b_test = (/ &
    2.0D+00, 3.0D+00,  4.0D+00,  5.0D+00,   6.0D+00, &
    7.0D+00, 8.0D+00, 16.0D+00, 32.0D+00, 256.0D+00 /)
  integer ( kind = 4 ) i4_log_r8
  integer ( kind = 4 ) test
  integer ( kind = 4 ) x

  x = 16

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_LOG_R8_TEST'
  write ( *, '(a)' ) '  I4_LOG_R8: whole part of log base B,'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         X          B      I4_LOG_R8'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    b = b_test(test)

    write ( *, '(2x,i8,g14.6,i12)' ) x, b, i4_log_r8 ( x, b )

  end do

  return
end
subroutine i4_mant_test ( )

!*****************************************************************************80
!
!! I4_MANT_TEST tests I4_MANT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) s
  real ( kind = 8 ) x

  x = -314.159D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_MANT_TEST'
  write ( *, '(a)' ) '  I4_MANT decomposes an integer,'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Number to be decomposed is X = ', x

  call i4_mant ( x, s, j, k, l )

  write ( *, '(a)' ) ''
  write ( *, '(a,i24,a,i24,a,i24,a,i8)' ) &
    '  I4_MANT: X = ', s, ' * (', j, '/', k, ') * 2 ^ ', l

  return
end
subroutine i4_max_test ( )

!*****************************************************************************80
!
!! I4_MAX_TEST tests I4_MAX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) i4_max
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_MAX_TEST'
  write ( *, '(a)' ) '  I4_MAX returns the maximum of two I4''s.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       A       B      C=I4_MAX(A,B)'
  write ( *, '(a)' ) ''

  i4_lo = -100
  i4_hi = +100
  seed = 123456789
  do i = 1, 10
    a = i4_uniform_ab ( i4_lo, i4_hi, seed )
    b = i4_uniform_ab ( i4_lo, i4_hi, seed )
    c = i4_max ( a, b )
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) a, b, c
  end do

  return
end
subroutine i4_min_test ( )

!*****************************************************************************80
!
!! I4_MIN_TEST tests I4_MIN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) i4_min
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_MIN_TEST'
  write ( *, '(a)' ) '  I4_MIN returns the minimum of two I4''s.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       A       B      C=I4_MIN(A,B)'
  write ( *, '(a)' ) ''

  i4_lo = -100
  i4_hi = +100
  seed = 123456789
  do i = 1, 10
    a = i4_uniform_ab ( i4_lo, i4_hi, seed )
    b = i4_uniform_ab ( i4_lo, i4_hi, seed )
    c = i4_min ( a, b )
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) a, b, c
  end do

  return
end
subroutine i4_moddiv_test ( )

!*****************************************************************************80
!
!! I4_MODDIV_TEST tests I4_MODDIV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 4

  integer ( kind = 4 ) d
  integer ( kind = 4 ), dimension ( test_num ) :: d_vec = (/ &
    50, -50, 50, -50 /)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( test_num ) :: n_vec = (/ &
    107, 107, -107, -107 /)
  integer ( kind = 4 ) r
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_MODDIV_TEST'
  write ( *, '(a)' ) '  I4_MODDIV factors a number'
  write ( *, '(a)' ) '  into a multiple M and remainder R.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    Number   Divisor  Multiple Remainder'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    n = n_vec(test)
    d = d_vec(test)
    call i4_moddiv ( n, d, m, r )
    write ( *, '(2x,4i10)' ) n, d, m, r
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Repeat using FORTRAN MOD:'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    n = n_vec(test)
    d = d_vec(test)
    m = n / d
    r = mod ( n, d )
    write ( *, '(2x,4i10)' ) n, d, m, r
  end do

  return
end
subroutine i4_modp_test ( )

!*****************************************************************************80
!
!! I4_MODP_TEST tests I4_MODP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 4

  integer ( kind = 4 ) d
  integer ( kind = 4 ), dimension ( test_num ) :: d_vec = (/ &
    50, -50, 50, -50 /)
  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( test_num ) :: n_vec = (/ &
    107, 107, -107, -107 /)
  integer ( kind = 4 ) r
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_MODP_TEST'
  write ( *, '(a)' ) '  I4_MODP factors a number'
  write ( *, '(a)' ) '  into a multiple and a positive remainder.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    Number   Divisor  Multiple Remainder'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    n = n_vec(test)
    d = d_vec(test)
    r = i4_modp ( n, d )
    m = ( n - r ) / d
    write ( *, '(2x,4i10)' ) n, d, m, r
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Repeat using FORTRAN MOD:'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    n = n_vec(test)
    d = d_vec(test)
    r = mod ( n, d )
    m = ( n - r ) / d
    write ( *, '(2x,4i10)' ) n, d, m, r
  end do

  return
end
subroutine i4_not_test ( )

!*****************************************************************************80
!
!! I4_NOT_TEST tests I4_NOT.
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

  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: i4_hi = 100
  integer ( kind = 4 ) :: i4_lo = 0
  integer ( kind = 4 ) i4_not
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_NOT_TEST'
  write ( *, '(a)' ) '  I4_NOT returns the NOT of an integer I with respect to'
  write ( *, '(a)' ) '  a maximum integer J.'
  write ( *, '(a)' ) '  Compare with FORTRAN90 intrinsic NOT.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I         J    I4_NOT      NOT + J + 1'
  write ( *, '(a)' ) ''

  j = 255

  do test = 1, test_num
    i = i4_uniform_ab ( i4_lo, i4_hi, seed )
    k = i4_not ( i, j )
    l = j + not ( i ) + 1
    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k, l
  end do

  return
end
subroutine i4_or_test ( )

!*****************************************************************************80
!
!! I4_OR_TEST tests I4_OR.
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

  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: i4_hi = 100
  integer ( kind = 4 ) :: i4_lo = 0
  integer ( kind = 4 ) i4_or
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_OR_TEST'
  write ( *, '(a)' ) '  I4_OR returns the bitwise inclusive OR of'
  write ( *, '(a)' ) '  two integers.'
  write ( *, '(a)' ) '  Compare with FORTRAN90 intrinsic IOR.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I         J    I4_OR      IOR'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    i = i4_uniform_ab ( i4_lo, i4_hi, seed )
    j = i4_uniform_ab ( i4_lo, i4_hi, seed )
    k = i4_or ( i, j )
    l = ior ( i, j )
    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k, l
  end do

  return
end
subroutine i4_power_test ( )

!*****************************************************************************80
!
!! I4_POWER_TEST tests I4_POWER.
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

  integer ( kind = 4 ) i
  integer ( kind = 4 ), dimension ( 7 ) :: i_test = (/ &
    0, 1, 2, 3, 10, -1, -2 /)
  integer ( kind = 4 ) i4_power
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( 7 ) :: j_test = (/ &
    1, 2, 3, 3, 3, 4, 5 /)
  integer ( kind = 4 ) test
  integer ( kind = 4 ) test_num

  test_num = 7

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_POWER_TEST'
  write ( *, '(a)' ) '  I4_POWER computes I^J'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I       J  I4_POWER(I,J)'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    i = i_test(test)
    j = j_test(test)
    write ( *, '(2x,i2,2x,i2,2x,i6)' ) i, j, i4_power ( i, j )
  end do

  return
end
subroutine i4_rise_test ( )

!*****************************************************************************80
!
!! I4_RISE_TEST tests I4_RISE.
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
  integer ( kind = 4 ) i4_rise
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_RISE_TEST:'
  write ( *, '(a)' ) '  I4_RISE evaluates the rising factorial function:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         M         N      Exact         I4_RISE(M,N)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call i4_rise_values ( n_data, m, n, f1 )

    if ( n_data == 0 ) then
      exit
    end if

    f2 = i4_rise ( m, n )

    write ( *, '(2x,i8,2x,i8,2x,i12,2x,i12)' ) m, n, f1, f2

  end do

  return
end
subroutine i4_sign_test ( )

!*****************************************************************************80
!
!! I4_SIGN_TEST tests I4_SIGN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 5

  integer ( kind = 4 ) i4
  integer ( kind = 4 ) i4_sign
  integer ( kind = 4 ), dimension(test_num) :: i4_vec = (/ -10, -7, 0, 5, 9 /)
  integer ( kind = 4 ) s
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_SIGN_TEST'
  write ( *, '(a)' ) '  I4_SIGN returns the sign of an I4.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I4  I4_SIGN(I4)'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    i4 = i4_vec(test)
    s = i4_sign ( i4 )
    write ( *, '(2x,i4,2x,i11)' ) i4, s
  end do

  return
end
subroutine i4_sign3_test ( )

!*****************************************************************************80
!
!! I4_SIGN3_TEST tests I4_SIGN3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 5

  integer ( kind = 4 ) i4
  integer ( kind = 4 ) i4_sign3
  integer ( kind = 4 ), dimension(test_num) :: i4_vec = (/ -10, -7, 0, 5, 9 /)
  integer ( kind = 4 ) s
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_SIGN3_TEST'
  write ( *, '(a)' ) '  I4_SIGN3 returns the three-way sign of an I4.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I4  I4_SIGN3(I4)'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    i4 = i4_vec(test)
    s = i4_sign3 ( i4 )
    write ( *, '(2x,i4,2x,i11)' ) i4, s
  end do

  return
end
subroutine i4_swap_test ( )

!*****************************************************************************80
!
!! I4_SWAP_TEST tests I4_SWAP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_SWAP_TEST'
  write ( *, '(a)' ) '  I4_SWAP swaps two I4''s.'

  i = 1
  j = 202

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Before swapping: '
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  I = ', i
  write ( *, '(a,i8)' ) '  J = ', j

  call i4_swap ( i, j )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  After swapping: '
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  I = ', i
  write ( *, '(a,i8)' ) '  J = ', j

  return
end
subroutine i4_to_halton_test ( )

!*****************************************************************************80
!
!! I4_TO_HALTON_TEST tests I4_TO_HALTON. 
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

  integer ( kind = 4 ), parameter :: dim_num = 3

  integer ( kind = 4 ), dimension ( dim_num ) :: base = (/ 2, 3, 5 /)
  integer ( kind = 4 ), dimension ( dim_num ) :: leap = (/ 1, 1, 1 /)
  integer ( kind = 4 ) n
  real ( kind = 8 ) r(dim_num)
  integer ( kind = 4 ), dimension ( dim_num ) :: seed = (/ 0, 0, 0 /)
  integer ( kind = 4 ) step

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_TO_HALTON_TEST'
  write ( *, '(a)' ) '  I4_TO_HALTON computes a Halton sequence.'
  write ( *, '(a)' ) '  The user specifies all data explicitly.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  In this test, we call I4_TO_HALTON repeatedly.'
  write ( *, '(a)' ) '  We use distinct primes as bases.'

  n = 10

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I    R(0)      R(1)      R(2)'
  write ( *, '(a)' ) ''

  do step = 0, n

    call i4_to_halton ( dim_num, step, seed, leap, base, r );
    write ( *, '(2x,i2,3(2x,f8.4))' ) step, r(1:3)

  end do

  return
end
subroutine i4_to_pascal_test ( )

!*****************************************************************************80
!
!! I4_TO_PASCAL_TEST tests I4_TO_PASCAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_TO_PASCAL_TEST'
  write ( *, '(a)' ) '  I4_TO_PASCAL converts a linear index to'
  write ( *, '(a)' ) '  Pascal triangle indices.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     K  =>   I     J'
  write ( *, '(a)' ) ''

  do k = 1, 20

    call i4_to_pascal ( k, i, j )

    write ( *, '(2x,i4,4x,i4,2x,i4)' ) k, i, j

  end do

  return
end
subroutine i4_to_pascal_degree_test ( )

!*****************************************************************************80
!
!! I4_TO_PASCAL_DEGREE_TEST tests I4_TO_PASCAL_DEGREE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_TO_PASCAL_DEGREE_TEST'
  write ( *, '(a)' ) '  I4_TO_PASCAL_DEGREE converts a linear index to'
  write ( *, '(a)' ) '  the degree of the corresponding Pascal triangle indices.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     K  =>   D'
  write ( *, '(a)' ) ''

  do k = 1, 20

    call i4_to_pascal_degree ( k, d )

    write ( *, '(2x,i4,4x,i4)' ) k, d

  end do

  return
end
subroutine i4_to_triangle_lower_test ( )

!*****************************************************************************80
!
!! I4_TO_TRIANGLE_LOWER_TEST tests I4_TO_TRIANGLE_LOWER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_TO_TRIANGLE_LOWER_TEST'
  write ( *, '(a)' ) '  I4_TO_TRIANGLE_LOWER converts a linear index to a'
  write ( *, '(a)' ) '  lower triangular one.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     K  =>   I   J'
  write ( *, '(a)' ) ''

  do k = 0, 20

    call i4_to_triangle_lower ( k, i, j )

    write ( *, '(2x,i4,4x,i4,i4)' ) k, i, j

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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  I4_UNIFORM_AB computes pseudorandom values '
  write ( *, '(a)' ) '  in an interval [A,B].'

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a,i12)' ) '  The lower endpoint A = ', a
  write ( *, '(a,i12)' ) '  The upper endpoint B = ', b
  write ( *, '(a,i12)' ) '  The initial seed is ', seed
  write ( *, '(a)' ) ''

  do i = 1, 20

    j = i4_uniform_ab ( a, b, seed )

    write ( *, '(2x,i8,2x,i8)' ) i, j

  end do

  return
end
subroutine i4_walsh_1d_test ( )

!*****************************************************************************80
!
!! I4_WALSH_1D_TEST tests I4_WALSH_1D.
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

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_walsh_1d
  integer ( kind = 4 ) w0
  integer ( kind = 4 ) wm1
  integer ( kind = 4 ) wm2
  integer ( kind = 4 ) wm3
  integer ( kind = 4 ) wp1
  integer ( kind = 4 ) wp2
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_WALSH_1D_TEST'
  write ( *, '(a)' ) '  I4_WALSH_1D evaluates 1D Walsh functions:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X       +2  +1   0  -1  -2  -3'
  write ( *, '(a)' ) ''

  do i = 0, 32

    x = real ( i, kind = 8 ) / 4.0D+00

    wp2 = i4_walsh_1d ( x,  2 )
    wp1 = i4_walsh_1d ( x,  1 )
    w0  = i4_walsh_1d ( x,  0 )
    wm1 = i4_walsh_1d ( x, -1 )
    wm2 = i4_walsh_1d ( x, -2 )
    wm3 = i4_walsh_1d ( x, -3 )

    write ( *, '(2x,f10.6,6i4)' ) x, wp2, wp1, w0, wm1, wm2, wm3

  end do

  return
end
subroutine i4_width_test ( )

!*****************************************************************************80
!
!! I4_WIDTH_TEST tests I4_WIDTH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 13

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_width
  integer ( kind = 4 ) x
  integer ( kind = 4 ) :: x_test(test_num) = (/ &
    0, 1, 2, 3, 9, 10, 11, 99, 101, -1, -2, -3, -9 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_WIDTH_TEST'
  write ( *, '(a)' ) '  I4_WIDTH determines the printing "width" of an I4.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '            I4      I4_WIDTH'
  write ( *, '(a)' ) ''

  do i = 1, test_num
    x = x_test(i)
    write ( *, '(2x,i12,2x,i12)' ) x, i4_width ( x )
  end do

  return
end
subroutine i4_wrap_test ( )

!*****************************************************************************80
!
!! I4_WRAP_TEST tests I4_WRAP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_wrap
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo

  ilo = 4
  ihi = 8

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_WRAP_TEST'
  write ( *, '(a)' ) '  I4_WRAP forces an integer to lie within given limits.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  ILO = ', ilo
  write ( *, '(a,i8)' ) '  IHI = ', ihi
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  I4_WRAP(I)'
  write ( *, '(a)' ) ''

  do i = -10, 20
    write ( *, '(2x,2i8)' ) i, i4_wrap ( i, ilo, ihi )
  end do

  return
end
subroutine i4_xor_test ( )

!*****************************************************************************80
!
!! I4_XOR_TEST tests I4_XOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: i4_hi = 100
  integer ( kind = 4 ) :: i4_lo = 0
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) i4_xor
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_XOR_TEST'
  write ( *, '(a)' ) '  I4_XOR returns the bitwise exclusive OR of'
  write ( *, '(a)' ) '  two integers.'
  write ( *, '(a)' ) '  Compare with FORTRAN90 intrinsic IEOR.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I         J    I4_XOR      IEOR'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    i = i4_uniform_ab ( i4_lo, i4_hi, seed )
    j = i4_uniform_ab ( i4_lo, i4_hi, seed )
    k = i4_xor ( i, j )
    l = ieor ( i, j )
    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k, l
  end do

  return
end
subroutine i4block_print_test ( )

!*****************************************************************************80
!
!! I4BLOCK_PRINT_TEST tests I4BLOCK_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: l = 4
  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 2
  integer ( kind = 4 ), dimension(l,m,n) :: x = reshape ( (/ &
        1,  2,  3,   4,  1, &
        4,  9, 16,   1,  8, &
       27, 64,  2,   4,  6, &
        8,  2,  8,  18, 32, &
        2, 16, 54, 128 /), (/ l, m, n /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4BLOCK_PRINT_TEST'
  write ( *, '(a)' ) '  I4BLOCK_PRINT prints an I4BLOCK.'

  call i4block_print ( l, m, n, x, '  The 3D array:' )

  return
end
subroutine i4col_find_item_test ( )

!*****************************************************************************80
!
!! I4COL_FIND_ITEM_TEST tests I4COL_FIND_ITEM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 August 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4
  integer ( kind = 4 ), parameter :: test_num = 3

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) col
  integer ( kind = 4 ) i
  integer ( kind = 4 ) item
  integer ( kind = 4 ), dimension ( test_num ) :: item_test = (/ &
    34, 12, 90 /)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) row
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4COL_FIND_ITEM_TEST'
  write ( *, '(a)' ) '  I4COL_FIND_ITEM finds the first occurrence of'
  write ( *, '(a)' ) '  an item in an integer array of columns.'

  do i = 1, m
    do j = 1, n
      a(i,j) = 10 * i + j
    end do
  end do

  call i4mat_print ( m, n, a, '  The matrix of columns:' )

  do test = 1, test_num

    item = item_test(test)

    call i4col_find_item ( m, n, a, item, row, col )

    write ( *, '(a,i8,a,i8,a,i8)' ) '  Item ', item, '  occurs in row ', &
      row, ' and column ', col

  end do

  return
end
subroutine i4col_find_pair_wrap_test ( )

!*****************************************************************************80
!
!! I4COL_FIND_PAIR_WRAP_TEST tests I4COL_FIND_PAIR_WRAP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 August 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4
  integer ( kind = 4 ), parameter :: test_num = 5

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) col
  integer ( kind = 4 ) i
  integer ( kind = 4 ) item1
  integer ( kind = 4 ), dimension ( test_num ) :: item1_test = (/ &
    22, 32, 22, 54, 54 /)
  integer ( kind = 4 ) item2
  integer ( kind = 4 ), dimension ( test_num ) :: item2_test = (/ &
    32, 22, 23, 14, 11 /)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) row
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4COL_FIND_PAIR_WRAP_TEST'
  write ( *, '(a)' ) '  I4COL_FIND_PAIR_WRAP finds the first occurrence of'
  write ( *, '(a)' ) '  a pair of item in an integer array of columns.'
  write ( *, '(a)' ) '  Items in the array are ordered by column, and'
  write ( *, '(a)' ) '  wraparound is allowed.'

  do i = 1, m
    do j = 1, n
      a(i,j) = 10 * i + j
    end do
  end do

  call i4mat_print ( m, n, a, '  The matrix of columns:' )

  do test = 1, test_num

    item1 = item1_test(test)
    item2 = item2_test(test)

    call i4col_find_pair_wrap ( m, n, a, item1, item2, row, col )

    write ( *, '(a,i8,a,i8,a,i8,a,i8)' ) '  Item ', item1, &
      ' followed by item ', item2, ' occurs in row ', &
      row, ' and column ', col

  end do

  return
end
subroutine i4col_sort_a_test ( )

!*****************************************************************************80
!
!! I4COL_SORT_A_TEST tests I4COL_SORT_A.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) :: b = 1
  integer ( kind = 4 ) :: c = 10
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4COL_SORT_A_TEST'
  write ( *, '(a)' ) '  I4COL_SORT_A ascending sorts an integer array'
  write ( *, '(a)' ) '  as a table of columns.'

  seed = 123456789

  call i4mat_uniform_ab ( m, n, b, c, seed, a )

  call i4mat_print ( m, n, a, '  The original matrix:' )

  call i4col_sort_a ( m, n, a )

  call i4mat_print ( m, n, a, '  Descending sorted:' )

  return
end
subroutine i4col_sort_d_test ( )

!*****************************************************************************80
!
!! I4COL_SORT_D_TEST tests I4COL_SORT_D.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) :: b = 1
  integer ( kind = 4 ) :: c = 10
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4COL_SORT_D_TEST'
  write ( *, '(a)' ) '  I4COL_SORT_D descending sorts an integer array'
  write ( *, '(a)' ) '  as a table of columns.'

  seed = 123456789

  call i4mat_uniform_ab ( m, n, b, c, seed, a )

  call i4mat_print ( m, n, a, '  The original matrix:' )

  call i4col_sort_d ( m, n, a )

  call i4mat_print ( m, n, a, '  Descending sorted:' )

  return
end
subroutine i4col_sort2_a_test ( )

!*****************************************************************************80
!
!! I4COL_SORT2_A_TEST tests I4COL_SORT2_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = 20
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4COL_SORT2_A_TEST'
  write ( *, '(a)' ) '  For a rectangular integer matrix:'
  write ( *, '(a)' ) '  I4COL_SORT2_D sorts the elements of the columns.'

  call i4mat_uniform_ab ( m, n, b, c, seed, a )

  call i4mat_print ( m, n, a, '  The matrix:' )

  call i4col_sort2_a ( m, n, a )

  call i4mat_print ( m, n, a, '  The element-sorted column matrix:' )

  return
end
subroutine i4col_sorted_singleton_count_test ( )

!*****************************************************************************80
!
!! I4COL_SORTED_SINGLETON_COUNT_TEST tests I4COL_SORTED_SINGLETON_COUNT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) singleton_num
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4COL_SORTED_SINGLETON_COUNT_TEST'
  write ( *, '(a)' ) '  I4COL_SORTED_SINGLETON_COUNT counts singletons'
  write ( *, '(a)' ) '  in a sorted I4COL;'

  seed = 123456789

  do test = 1, test_num

    b = 0
    c = 3

    call i4mat_uniform_ab ( m, n, b, c, seed, a )

    call i4col_sort_a ( m, n, a )

    call i4mat_print ( m, n, a, '  Ascending sorted ICOL:' )

    call i4col_sorted_singleton_count ( m, n, a, singleton_num )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8,a)' ) '  Number of singletons = ', singleton_num

  end do

  return
end
subroutine i4col_sorted_unique_count_test ( )

!*****************************************************************************80
!
!! I4COL_SORTED_UNIQUE_COUNT_TEST tests I4COL_SORTED_UNIQUE_COUNT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) unique_num
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4COL_SORTED_UNIQUE_COUNT_TEST'
  write ( *, '(a)' ) '  I4COL_SORTED_UNIQUE_COUNT counts the unique entries'
  write ( *, '(a)' ) '  of a sorted I4COL;'

  seed = 123456789

  do test = 1, test_num

    b = 0
    c = 3

    call i4mat_uniform_ab ( m, n, b, c, seed, a )

    call i4col_sort_a ( m, n, a )

    call i4mat_print ( m, n, a, '  Ascending sorted I4COL:' )

    call i4col_sorted_unique_count ( m, n, a, unique_num )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8,a)' ) '  Number of unique entries = ', unique_num

  end do

  return
end
subroutine i4mat_elim_test ( )

!*****************************************************************************80
!
!! I4MAT_ELIM_TEST tests I4MAT_ELIM.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) factor
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 3

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_ELIM_TEST'
  write ( *, '(a)' ) '  I4MAT_ELIM does exact Gauss elimination.'

  do test = 1, test_num

    if ( test == 1 ) then

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = k
        end do
      end do

    else if ( test == 2 ) then

      factor = 8 * 7 * 6 * 5 * 4 * 3 * 2

      do i = 1, m
        do j = 1, n
          a(i,j) = factor / ( i + j - 1 )
        end do
      end do

    else if ( test == 3 ) then

      do i = 1, m
        do j = 1, n
          a(i,j) = i * j
        end do
      end do

    end if

    call i4mat_print ( m, n, a, '  The original matrix:' )

    call i4mat_elim ( m, n, a )

    call i4mat_print ( m, n, a, '  The matrix returned by I4MAT_ELIM:' )

  end do

  return
end
subroutine i4mat_indicator_test ( )

!*****************************************************************************80
!
!! I4MAT_INDICATOR_TEST tests I4MAT_INDICATOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_INDICATOR_TEST'
  write ( *, '(a)' ) '  I4MAT_INDICATOR returns an indicator matrix.'

  call i4mat_indicator ( m, n, a )

  call i4mat_print ( m, n, a, '  The indicator matrix:' )

  return
end
subroutine i4mat_l1_inverse_test ( )

!*****************************************************************************80
!
!! I4MAT_L1_INVERSE_TEST tests I4MAT_L1_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6
!
!  Each row of this definition is a COLUMN of the matrix.
!
  integer ( kind = 4 ), dimension ( n, n ) :: a = reshape ( (/ &
     1,  2,  0,  5,  0, 75, &
     0,  1,  0,  0,  0,  0, &
     0,  0,  1,  3,  0,  0, &
     0,  0,  0,  1,  0,  6, &
     0,  0,  0,  0,  1,  4, &
     0,  0,  0,  0,  0,  1 /), (/ n, n /) )
  integer ( kind = 4 ) b(n,n)
  integer ( kind = 4 ) c(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_L1_INVERSE_TEST'
  write ( *, '(a)' ) '  I4MAT_L1_INVERSE inverts a unit lower triangular matrix.'

  call i4mat_print ( n, n, a, '  The original matrix:' )

  call i4mat_l1_inverse ( n, a, b )

  call i4mat_print ( n, n, b, '  The inverse matrix:' )

  c(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )

  call i4mat_print ( n, n, c, '  The product:' )

  return
end
subroutine i4mat_max_test ( )

!*****************************************************************************80
!
!! I4MAT_MAX_TEST tests I4MAT_MAX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = 10
  integer ( kind = 4 ) i4mat_max
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_MAX_TEST'
  write ( *, '(a)' ) '  I4MAT_MAX returns the maximum;'

  seed = 123456789

  call i4mat_uniform_ab ( m, n, b, c, seed, a )

  call i4mat_print ( m, n, a, '  Random array:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Maximum value is ', i4mat_max ( m, n, a )

  return
end
subroutine i4mat_max_index_test ( )

!*****************************************************************************80
!
!! I4MAT_MAX_INDEX_TEST tests I4MAT_MAX_INDEX.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = 10
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_MAX_INDEX_TEST'
  write ( *, '(a)' ) '  I4MAT_MAX_INDEX locates the maximum;'

  seed = 123456789

  call i4mat_uniform_ab ( m, n, b, c, seed, a )

  call i4mat_print ( m, n, a, '  Random array:' )

  write ( *, '(a)' ) ''
  call i4mat_max_index ( m, n, a, i, j )
  write ( *, '(a,2i8)' ) '  Maximum I,J indices            ', i, j

  return
end
subroutine i4mat_min_test ( )

!*****************************************************************************80
!
!! I4MAT_MIN_TEST tests I4MAT_MIN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = 10
  integer ( kind = 4 ) i4mat_min
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_MIN_TEST'
  write ( *, '(a)' ) '  I4MAT_MIN returns the minimum;'

  seed = 123456789

  call i4mat_uniform_ab ( m, n, b, c, seed, a )

  call i4mat_print ( m, n, a, '  Random array:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Minimum value is ', i4mat_min ( m, n, a )

  return
end
subroutine i4mat_min_index_test ( )

!*****************************************************************************80
!
!! I4MAT_MIN_INDEX_TEST tests I4MAT_MIN_INDEX.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = 10
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_MIN_INDEX_TEST'
  write ( *, '(a)' ) '  I4MAT_MIN_INDEX locates the minimum;'

  seed = 123456789

  call i4mat_uniform_ab ( m, n, b, c, seed, a )

  call i4mat_print ( m, n, a, '  Random array:' )

  write ( *, '(a)' ) ''
  call i4mat_min_index ( m, n, a, i, j )
  write ( *, '(a,2i8)' ) '  Minimum I,J indices            ', i, j

  return
end
subroutine i4mat_perm_uniform_test ( )

!*****************************************************************************80
!
!! I4MAT_PERM_UNIFORM_TEST tests I4MAT_PERM_UNIFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_PERM_UNIFORM_TEST'
  write ( *, '(a)' ) '  I4MAT_PERM_UNIFORM applies a random permutation'
  write ( *, '(a)' ) '  to a square integer matrix.'

  seed = 123456789

  do i = 1, n
    do j = 1, n
      a(i,j) = 10 * i + j
    end do
  end do

  call i4mat_print ( n, n, a, '  The original matrix:' )

  call i4mat_perm_uniform ( n, a, seed )

  call i4mat_print ( n, n, a, '  The permuted matrix:' )

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

  write ( *, '(a)' ) ''
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

  write ( *, '(a)' ) ''
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
subroutine i4mat_product_elementwise_test ( )

!*****************************************************************************80
!
!! I4MAT_PRODUCT_ELEMENTWISE_TEST tests I4MAT_PRODUCT_ELEMENTWISE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(m,n)
  integer ( kind = 4 ) i4mat_product_elementwise
  integer ( kind = 4 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_PRODUCT_ELEMENTWISE_TEST'
  write ( *, '(a)' ) '  I4MAT_PRODUCT_ELEMENTWISE computes the elementwise'
  write ( *, '(a)' ) '  product of two I4MATs.'

  a = reshape ( (/ 1, 4, 2, 5, 3, 6 /), (/ m, n /) ) 
  b = reshape ( (/ 1, 2, 3, 4, 5, 6 /), (/ m, n /) )

  call i4mat_print ( m, n, a, '  A:' )
  call i4mat_print ( m, n, b, '  B:' )

  t = i4mat_product_elementwise ( m, n, a, b )
 
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Elementwise product = ', t

  return
end
subroutine i4mat_rank_test ( )

!*****************************************************************************80
!
!! I4MAT_RANK_TEST tests I4MAT_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ), dimension ( 3, 3 ) :: a1 = reshape ( (/ &
    1, 4, 7, &
    2, 5, 8, &
    3, 6, 9 /), (/ 3, 3 /) )
  integer ( kind = 4 ), dimension ( 3, 3 ) :: a2 = reshape ( (/ &
    1, 4, 7, &
    2, 5, 8, &
    3, 6, 0 /), (/ 3, 3 /) )
  integer ( kind = 4 ), dimension ( 4, 3 ) :: a3 = reshape ( (/ &
    1,  4,  7, 10, &
    2,  5,  8, 11, &
    3,  6,  0, 12 /), (/ 4, 3 /) )
  integer ( kind = 4 ), dimension ( 3, 4 ) :: a4 = reshape ( (/ &
    1, 4, 7, &
    2, 5, 8, &
    3, 6, 9, &
    7, 8, 3 /), (/ 3, 4 /) )
  integer ( kind = 4 ), dimension ( 5, 3 ) :: a5 = reshape ( (/ &
    1, 4, 7, 10, 3, &
    2, 5, 8, 11, 3, &
    3, 6, 9, 12, 3 /), (/ 5, 3 /) )
  integer ( kind = 4 ), dimension ( 3, 2 ) :: a6 = reshape ( (/ &
    0, 0, 0, &
    0, 0, 0 /), (/ 3, 2 /) )
  integer ( kind = 4 ) rank_a

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_RANK_TEST'
  write ( *, '(a)' ) '  I4MAT_RANK computes the rank of an integer matrix.'

  m = 3
  n = 3
  call i4mat_print ( m, n, a1, '  Matrix A1:' )
  call i4mat_rank ( m, n, a1, rank_a )
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The rank is ', rank_a

  m = 3
  n = 3
  call i4mat_print ( m, n, a2, '  Matrix A2:' )
  call i4mat_rank ( m, n, a2, rank_a )
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The rank is ', rank_a

  m = 4
  n = 3
  call i4mat_print ( m, n, a3, '  Matrix A3:' )
  call i4mat_rank ( m, n, a3, rank_a )
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The rank is ', rank_a

  m = 3
  n = 4
  call i4mat_print ( m, n, a4, '  Matrix A4:' )
  call i4mat_rank ( m, n, a4, rank_a )
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The rank is ', rank_a

  m = 5
  n = 3
  call i4mat_print ( m, n, a5, '  Matrix A5:' )
  call i4mat_rank ( m, n, a5, rank_a )
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The rank is ', rank_a

  m = 3
  n = 2
  call i4mat_print ( m, n, a6, '  Matrix A6:' )
  call i4mat_rank ( m, n, a6, rank_a )
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The rank is ', rank_a

  return
end
subroutine i4mat_ref_test ( )

!*****************************************************************************80
!
!! I4MAT_REF_TEST tests I4MAT_REF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) det

  a = reshape ( (/ &
    1, -2, 3, -1, &
    3, -6, 9, -3, &
    0,  0, 0,  0, &
    2, -2, 0,  1, &
    6, -8, 6,  0, &
    3,  3, 6,  9, &
    1,  1, 2,  3 /), (/ m, n /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_REF_TEST'
  write ( *, '(a)' ) '  I4MAT_REF computes the '
  write ( *, '(a)' ) '  integer row echelon form (IREF) of an I4MAT.'

  call i4mat_print ( m, n, a, '  Input A:' )

  call i4mat_ref ( m, n, a, det )

  write ( *, '(a)' ) 
  write ( *, '(a,g14.6)' ) '  The pseudo-determinant = ', det

  call i4mat_print ( m, n, a, '  IREF of A:' )

  return
end
subroutine i4mat_row_reduce_test ( )

!*****************************************************************************80
!
!! I4MAT_ROW_REDUCE_TEST tests I4MAT_ROW_REDUCE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2018
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_ROW_REDUCE_TEST'
  write ( *, '(a)' ) '  I4MAT_ROW_REDUCE divides out any common factors in the'
  write ( *, '(a)' ) '  entries of a row of an I4MAT.'

  a = reshape ( (/ &
    12,   4, -12, 30, 0, &
    88,   8,  88, 18, 4, &
     9, 192,  94, 42, 8 /), (/ 5, 3 /) )

  call i4mat_print ( m, n, a, '  Original matrix:' )

  do i = m, 1, -1
    call i4mat_row_reduce ( m, n, i, a )
    call i4mat_print ( m, n, a, '  After reducing a row:' )
  end do

  return
end
subroutine i4mat_row_swap_test ( )

!*****************************************************************************80
!
!! I4MAT_ROW_SWAP_TEST tests I4MAT_ROW_SWAP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_ROW_SWAP_TEST'
  write ( *, '(a)' ) '  I4MAT_ROW_SWAP swaps two rows of an I4MAT.'

  do i = 1, m
    do j = 1, n
      a(i,j) = 10 * i + j
    end do
  end do

  call i4mat_print ( m, n, a, '  Input A:' )

  i1 = 2
  i2 = 5
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2)' ) '  Swap rows ', i1, ' and ', i2

  call i4mat_row_swap ( m, n, a, i1, i2 )

  call i4mat_print ( m, n, a, '  Modified matrix:' )

  return
end
subroutine i4mat_rref_test ( )

!*****************************************************************************80
!
!! I4MAT_RREF_TEST tests I4MAT_RREF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) det

  a = reshape ( (/ &
    1, -2, 3, -1, &
    3, -6, 9, -3, &
    0,  0, 0,  0, &
    2, -2, 0,  1, &
    6, -8, 6,  0, &
    3,  3, 6,  9, &
    1,  1, 2,  3 /), (/ m, n /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_RREF_TEST'
  write ( *, '(a)' ) '  I4MAT_RREF computes the '
  write ( *, '(a)' ) '  integer row reduced echelon form (IRREF) of an I4MAT.'

  call i4mat_print ( m, n, a, '  Input A:' )

  call i4mat_rref ( m, n, a, det )

  write ( *, '(a)' ) 
  write ( *, '(a,g14.6)' ) '  The pseudo-determinant = ', det

  call i4mat_print ( m, n, a, '  IREF of A:' )

  return
end
subroutine i4mat_rref_system_test ( )

!*****************************************************************************80
!
!! I4MAT_RREF_SYSTEM_TEST tests I4MAT_RREF_SYSTEM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: a1(:,:)
  integer ( kind = 4 ), allocatable :: a2(:,:)
  integer ( kind = 4 ), allocatable :: a3(:,:)
  integer ( kind = 4 ), allocatable :: b2(:)
  integer ( kind = 4 ), allocatable :: b3(:)
  integer ( kind = 4 ) det
  integer ( kind = 4 ), allocatable :: freedom(:)
  integer ( kind = 4 ) freedom_num
  integer ( kind = 4 ), allocatable :: i4rows(:)
  logical incon
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_RREF_SYSTEM_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  I4MAT_RREF_SYSTEM computes the linear system associated'
  write ( *, '(a)' ) '  with an integer reduced row echelon form of an I4MAT.'
!
!  "Wide" matrix.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Look at a "wide" matrix:'

  m = 4
  n = 7

  allocate ( i4rows(1:m*n) )
  i4rows(1:m*n) = (/ &
    1,  3, 0,  2,  6, 3, 1, &
   -2, -6, 0, -2, -8, 3, 1, &
    3,  9, 0,  0,  6, 6, 2, &
   -1, -3, 0,  1,  0, 9, 3 /)

  allocate ( a1(1:m,1:n) )
  call i4rows_to_i4mat ( m, n, i4rows, a1 )
 
  call i4mat_print ( m, n, a1, '  Input A1:' )

  allocate ( a2(1:m,1:n) )
  a2(1:m,1:n) = a1(1:m,1:n)
  call i4mat_rref ( m, n, a2, det )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The pseudo-determinant = ', det

  call i4mat_print ( m, n, a2, '  A2, the IRREF of A1:' )

  allocate ( b2(1:m) )
  b2(1:m-1) = 1
  b2(m) = 0
  call i4vec_print ( m, b2, '  B2, the right hand side:' )

  allocate ( a3(1:n,1:n) )
  allocate ( b3(1:n) )
  allocate ( freedom(1:n) )

  call i4mat_rref_system ( m, n, a2, b2, a3, b3, incon, freedom_num, freedom )

  write ( *, '(a)' ) ''
  if ( incon ) then
    write ( *, '(a)' ) '  The original system is INCONSISTENT.'
  else
    write ( *, '(a)' ) '  The original system is CONSISTENT.'
  end if

  call i4mat_print ( n, n, a3, '  A3, the augmented IRREF:' )
  call i4vec_print ( n, b3, '  B3, the augmented RHS:' )
  call i4vec_print ( freedom_num, freedom, '  Indices of degrees of freedom.' )

  deallocate ( a1 )
  deallocate ( a2 )
  deallocate ( a3 )
  deallocate ( b2 )
  deallocate ( b3 )
  deallocate ( freedom )
  deallocate ( i4rows )
!
!  "Tall" matrix.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Look at a "tall" matrix:'

  m = 7
  n = 4

  allocate ( i4rows(1:m*n) )
  i4rows(1:m*n) = (/ &
    1, -2, 3, -1, &
    3, -6, 9, -3, &
    0,  0, 0,  0, &
    2, -2, 0,  1, &
    6, -8, 6,  0, &
    3,  3, 6,  9, &
    1,  1, 2,  3 /)

  allocate ( a1(1:m,1:n) )
  call i4rows_to_i4mat ( m, n, i4rows, a1 )
 
  call i4mat_print ( m, n, a1, '  Input A1:' )

  allocate ( a2(1:m,1:n) )
  a2(1:m,1:n) = a1(1:m,1:n)
  call i4mat_rref ( m, n, a2, det )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The pseudo-determinant = ', det

  call i4mat_print ( m, n, a2, '  A2, the IRREF of A1:' )

  allocate ( b2(1:m) )
  b2(1:m) = 1
  call i4vec_print ( m, b2, '  B2, the right hand side:' )

  allocate ( a3(1:n,1:n) )
  allocate ( b3(1:n) )
  allocate ( freedom(1:n) )

  call i4mat_rref_system ( m, n, a2, b2, a3, b3, incon, freedom_num, freedom )

  write ( *, '(a)' ) ''
  if ( incon ) then
    write ( *, '(a)' ) '  The original system is INCONSISTENT.'
  else
    write ( *, '(a)' ) '  The original system is CONSISTENT.'
  end if

  call i4mat_print ( n, n, a3, '  A3, the augmented IRREF:' )
  call i4vec_print ( n, b3, '  B3, the augmented RHS:' )
  call i4vec_print ( freedom_num, freedom, '  Indices of degrees of freedom.' )

  deallocate ( a1 )
  deallocate ( a2 )
  deallocate ( a3 )
  deallocate ( b2 )
  deallocate ( b3 )
  deallocate ( freedom )
  deallocate ( i4rows )

  return
end
subroutine i4mat_transpose_test ( )

!*****************************************************************************80
!
!! I4MAT_TRANSPOSE_TEST tests I4MAT_TRANSPOSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(m*n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_TRANSPOSE_TEST'
  write ( *, '(a)' ) '  I4MAT_TRANSPOSE transposes a matrix.'

  call i4mat_indicator ( m, n, a )

  call i4mat_print ( m, n, a, '  The matrix:' )

  call i4mat_transpose ( m, n, a )

  call i4mat_print ( n, m, a, '  The transposed matrix:' )

  return
end
subroutine i4mat_u_solve_test ( )

!*****************************************************************************80
!
!! I4MAT_U_SOLVE_TEST tests I4MAT_U_SOLVE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4
!
!  Each row of this definition is a COLUMN of the matrix.
!
  integer ( kind = 4 ), dimension ( n, n ) :: a = reshape ( (/ &
    1, 0, 0,  0, &
    2, 3, 0,  0, &
    4, 5, 6,  0, &
    7, 8, 9, 10 /), (/ n, n /) )
  integer ( kind = 4 ), dimension ( n ) :: b = (/ &
    45, 53, 54, 40 /)
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) rnorm
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_U_SOLVE_TEST'
  write ( *, '(a)' ) '  I4MAT_U_SOLVE solves an upper triangular system.'

  call i4mat_print ( n, n, a, '  Input matrix A:' )

  call i4vec_print ( n, b, '  Right hand side b:' )

  call i4mat_u_solve ( n, a, b, x )

  call r8vec_print ( n, x, '  Computed solution x:' )

  r(1:n) = matmul ( a(1:n,1:n), x(1:n) ) - b(1:n)

  rnorm = sqrt ( sum ( r(1:n) ** 2 ) )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Norm of A*x-b = ', rnorm

  return
end
subroutine i4mat_u1_inverse_test ( )

!*****************************************************************************80
!
!! I4MAT_U1_INVERSE_TEST tests I4MAT_U1_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6
!
!  Each row of this definition is a COLUMN of the matrix.
!
  integer ( kind = 4 ), dimension ( n, n ) :: a = reshape ( (/ &
    1,  0,  0,  0,  0,  0, &
    2,  1,  0,  0,  0,  0, &
    0,  0,  1,  0,  0,  0, &
    5,  0,  3,  1,  0,  0, &
    0,  0,  0,  0,  1,  0, &
   75,  0,  0,  6,  4,  1 /), (/ n, n /) )
  integer ( kind = 4 ) b(n,n)
  integer ( kind = 4 ) c(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_U1_INVERSE_TEST'
  write ( *, '(a)' ) '  I4MAT_U1_INVERSE inverts a unit upper triangular matrix.'

  call i4mat_print ( n, n, a, '  The original matrix:' )

  call i4mat_u1_inverse ( n, a, b )

  call i4mat_print ( n, n, b, '  The inverse matrix:' )

  c(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )

  call i4mat_print ( n, n, c, '  The product:' )

  return
end
subroutine i4mat_width_test ( )

!*****************************************************************************80
!
!! I4MAT_WIDTH_TEST tests I4MAT_WIDTH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m1 = 3
  integer ( kind = 4 ), parameter :: m2 = 3
  integer ( kind = 4 ), parameter :: n1 = 3
  integer ( kind = 4 ), parameter :: n2 = 3

  integer ( kind = 4 ) :: a1(m1,n1) = reshape ( (/ &
    11, 211, 3111, &
    12, 222, 3222, &
    13, 233, 3333 /), (/ 3, 3 /) )
  integer ( kind = 4 ) :: a2(m2,n2) = reshape ( (/ &
    10,    23, 45, &
    42, -1000, 63, &
    77,    63, 90 /), (/ 3, 3 /) )
  integer ( kind = 4 ) i4mat_width

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_WIDTH_TEST'
  write ( *, '(a)' ) '  I4MAT_WIDTH determines the printing "width" of an I4MAT.'

  call i4mat_print ( m1, n1, a1, '  Matrix A1:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Printing width of A1 = ', i4mat_width ( m1, n1, a1 )

  call i4mat_print ( m2, n2, a2, '  Matrix A2:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Printing width of A2 = ', i4mat_width ( m2, n2, a2 )

  return
end
subroutine i4row_max_test ( )

!*****************************************************************************80
!
!! I4ROW_MAX_TEST tests I4ROW_MAX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) amax(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4ROW_MAX_TEST'
  write ( *, '(a)' ) '  I4ROW_MAX computes row maximums;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = k
    end do
  end do

  call i4mat_print ( m, n, a, '  The matrix:' )

  call i4row_max ( m, n, a, amax )

  call i4vec_print ( m, amax, '  The row maximums:' )

  return
end
subroutine i4row_mean_test ( )

!*****************************************************************************80
!
!! I4ROW_MEAN_TEST tests I4ROW_MEAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) mean(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4ROW_MEAN_TEST'
  write ( *, '(a)' ) '  I4ROW_MEAN computes row means;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = k
    end do
  end do

  call i4mat_print ( m, n, a, '  The matrix:' )

  call i4row_mean ( m, n, a, mean )

  call r8vec_print ( m, mean, '  The row means:' )

  return
end
subroutine i4row_min_test ( )

!*****************************************************************************80
!
!! I4ROW_MIN_TEST tests I4ROW_MIN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) amin(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4ROW_MIN_TEST'
  write ( *, '(a)' ) '  I4ROW_MIN computes row minimums;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = k
    end do
  end do

  call i4mat_print ( m, n, a, '  The matrix:' )

  call i4row_min ( m, n, a, amin )

  call i4vec_print ( m, amin, '  The row minimums:' )

  return
end
subroutine i4row_sort_a_test ( )

!*****************************************************************************80
!
!! I4ROW_SORT_A_TEST tests I4ROW_SORT_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 10
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = 10
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4ROW_SORT_A_TEST'
  write ( *, '(a)' ) '  For a rectangular integer matrix:'
  write ( *, '(a)' ) '  I4ROW_SORT_A sorts the rows;'

  seed = 123456789

  call i4mat_uniform_ab ( m, n, b, c, seed, a )

  call i4mat_print ( m, n, a, '  The original matrix:' )

  call i4row_sort_a ( m, n, a )

  call i4mat_print ( m, n, a, '  The row-sorted matrix:' )

  return
end
subroutine i4row_sort_d_test ( )

!*****************************************************************************80
!
!! I4ROW_SORT_D_TEST tests I4ROW_SORT_D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4ROW_SORT_D_TEST'
  write ( *, '(a)' ) '  For a rectangular integer matrix:'
  write ( *, '(a)' ) '  I4ROW_SORT_D sorts the rows;'

  seed = 123456789

  do i = 1, m
    do j = 1, n
      a(i,j) = 10 * i + j
    end do
  end do

  call i4mat_print ( m, n, a, '  The original matrix:' )

  call i4mat_perm2_uniform ( m, n, a, seed )

  call i4mat_print ( m, n, a, '  The matrix, permuted by I4MAT_PERM2_UNIFORM:' )

  call i4row_sort_d ( m, n, a )

  call i4mat_print ( m, n, a, '  The row-sorted matrix:' )

  return
end
subroutine i4row_sort2_d_test ( )

!*****************************************************************************80
!
!! I4ROW_SORT2_D_TEST tests I4ROW_SORT2_D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 6
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4ROW_SORT2_D_TEST'
  write ( *, '(a)' ) '  For a rectangular integer matrix:'
  write ( *, '(a)' ) '  I4ROW_SORT2_D sorts the elements of the rows.'

  seed = 123456789

  do i = 1, m
    do j = 1, n
      a(i,j) = 10 * i + j
    end do
  end do

  call i4mat_print ( m, n, a, '  The original matrix:' )

  call i4mat_perm2_uniform ( m, n, a, seed )

  call i4mat_print ( m, n, a, '  The matrix, permuted by I4MAT_PERM2_UNIFORM:' )

  call i4row_sort2_d ( m, n, a )

  call i4mat_print ( m, n, a, '  The element-sorted row-sorted matrix:' )

  return
end
subroutine i4row_sum_test ( )

!*****************************************************************************80
!
!! I4ROW_SUM_TEST tests I4ROW_SUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) rowsum(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4ROW_SUM_TEST'
  write ( *, '(a)' ) '  I4ROW_SUM computes row sums;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = k
    end do
  end do

  call i4mat_print ( m, n, a, '  The matrix:' )

  call i4row_sum ( m, n, a, rowsum )

  call i4vec_print ( m, rowsum, '  The row sums:' )

  return
end
subroutine i4row_swap_test ( )

!*****************************************************************************80
!
!! I4ROW_SWAP_TEST tests I4ROW_SWAP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) row1
  integer ( kind = 4 ) row2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4ROW_SWAP_TEST'
  write ( *, '(a)' ) '  For an integer matrix of rows,'
  write ( *, '(a)' ) '  I4ROW_SWAP swaps two rows;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = k
    end do
  end do

  call i4mat_print ( m, n, a, '  The matrix:' )

  row1 = 1
  row2 = 3

  write ( *, '(a)' ) ''
  write ( *, '(a,i8,a,i8)' ) '  Swap rows ', row1, ' and ', row2
  write ( *, '(a)' ) ''

  call i4row_swap ( m, n, a, row1, row2 )

  call i4mat_print ( m, n, a, '  The new matrix:' )

  return
end
subroutine i4row_variance_test ( )

!*****************************************************************************80
!
!! I4ROW_VARIANCE_TEST tests I4ROW_VARIANCE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) variance(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4ROW_VARIANCE_TEST'
  write ( *, '(a)' ) '  I4ROW_VARIANCE computes row variances;'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = k
    end do
  end do

  call i4mat_print ( m, n, a, '  The matrix:' )

  call i4row_variance ( m, n, a, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Row variances:'
  write ( *, '(a)' ) ''
  do i = 1, m
    write ( *, '(2x,i3,2x,f10.4)' ) i, variance(i)
  end do

  return
end
subroutine i4rows_to_i4mat_test ( )

!*****************************************************************************80
!
!! I4ROWS_TO_I4MAT_TEST tests I4ROWS_TO_I4MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) i4mat(m,n)
  integer ( kind = 4 ), dimension ( m * n ) :: i4rows = (/ &
    11, 12, 13, 14, &
    21, 22, 23, 24, &
    31, 32, 33, 34 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4ROWS_TO_I4MAT_TEST'
  write ( *, '(a)' ) '  I4ROWS_TO_I4MAT allows an I4MAT to be initialized'
  write ( *, '(a)' ) '  by data stored ROW-WISE in a vector.'

  call i4vec_print ( m * n, i4rows, '  The data vector:' )

  call i4rows_to_i4mat ( m, n, i4rows, i4mat )

  call i4mat_print ( m, n, i4mat, '  The data copied into an array:' )

  return
end
subroutine i4vec_add_test ( )

!*****************************************************************************80
!
!! I4VEC_ADD_TEST tests I4VEC_ADD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) c(n)
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_ADD_TEST'
  write ( *, '(a)' ) '  I4VEC_ADD adds two I4VEC''s'

  seed = 123456789

  lo = - n
  hi = n

  call i4vec_uniform_ab ( n, lo, hi, seed, a )
  call i4vec_uniform_ab ( n, lo, hi, seed, b )
  call i4vec_add ( n, a, b, c )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I     A     B     C'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(4i6)' ) i, a(i), b(i), c(i)
  end do

  return
end
subroutine i4vec_amax_test ( )

!*****************************************************************************80
!
!! I4VEC_AMAX_TEST tests I4VEC_AMAX.
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) aval
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_AMAX_TEST'
  write ( *, '(a)' ) '  For an integer vector:'
  write ( *, '(a)' ) '  I4VEC_AMAX:   maximum absolute entry;'

  seed = 123456789

  b = -10
  c = 5

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  call i4vec_amax ( n, a, aval )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Maximum absolute value: ', aval

  return
end
subroutine i4vec_amax_index_test ( )

!*****************************************************************************80
!
!! I4VEC_AMAX_INDEX_TEST tests I4VEC_AMAX_INDEX.
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_AMAX_INDEX_TEST'
  write ( *, '(a)' ) '  For an integer vector:'
  write ( *, '(a)' ) '  I4VEC_AMAX_INDEX:  index of maximum absolute entry;'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call i4vec_amax_index ( n, a, ival )

  write ( *, '(a,i8)' ) '  Maximum abs index:        ', ival

  return
end
subroutine i4vec_amin_test ( )

!*****************************************************************************80
!
!! I4VEC_AMIN_TEST tests I4VEC_AMIN.
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) aval
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_AMIN_TEST'
  write ( *, '(a)' ) '  For an integer vector:'
  write ( *, '(a)' ) '  I4VEC_AMIN:   minimum absolute entry;'
  write ( *, '(a)' ) ''

  seed = 123456789

  b = -10
  c = 5

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call i4vec_amin ( n, a, aval )

  write ( *, '(a,i8)' ) '  Minimum absolute value: ', aval

  return
end
subroutine i4vec_amin_index_test ( )

!*****************************************************************************80
!
!! I4VEC_AMIN_INDEX_TEST tests I4VEC_AMIN_INDEX.
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_AMIN_INDEX_TEST'
  write ( *, '(a)' ) '  For an integer vector:'
  write ( *, '(a)' ) '  I4VEC_AMIN_INDEX:  index minimum absolute entry;'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call i4vec_amin_index ( n, a, ival )

  write ( *, '(a,i8)' ) '  Minimum abs index:      ', ival

  return
end
subroutine i4vec_aminz_test ( )

!*****************************************************************************80
!
!! I4VEC_AMINZ_TEST tests I4VEC_AMINZ.
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) aval
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_AMINZ_TEST'
  write ( *, '(a)' ) '  For an I4VEC:'
  write ( *, '(a)' ) '  I4VEC_AMINZ:  minimum nonzero absolute entry;'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call i4vec_aminz ( n, a, aval )

  write ( *, '(a,i8)' ) '  Minimum abs nonzero:      ', aval

  return
end
subroutine i4vec_aminz_index_test ( )

!*****************************************************************************80
!
!! I4VEC_AMINZ_INDEX_TEST tests I4VEC_AMINZ_INDEX.
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_AMINZ_INDEX_TEST'
  write ( *, '(a)' ) '  For an I4VEC:'
  write ( *, '(a)' ) '  I4VEC_AMINZ_INDEX: index of minimum nonzero absolute entry;'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call i4vec_aminz_index ( n, a, ival )

  write ( *, '(a,i8)' ) '  Minimum abs nonzero index:', ival

  return
end
subroutine i4vec_ascend_sub_test ( )

!*****************************************************************************80
!
!! I4VEC_ASCEND_SUB_TEST tests I4VEC_ASCEND_SUB
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

  integer ( kind = 4 ), parameter :: n = 14

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) :: b = 1
  integer ( kind = 4 ) :: c = 10
  integer ( kind = 4 ) length
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) sub(n)
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 6

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_ASCEND_SUB_TEST'
  write ( *, '(a)' ) '  I4VEC_ASCEND_SUB computes a longest ascending'
  write ( *, '(a)' ) '  subsequence of an I4VEC.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  do test = 1, test_num
    call i4vec_uniform_ab ( n, b, c, seed, a )
    call i4vec_print ( n, a, '  The vector to be tested:' )
    call i4vec_ascend_sub ( n, a, length, sub )
    call i4vec_print ( length, sub, '  A longest ascending subsequence:' )
  end do

  return
end
subroutine i4vec_binary_next_test ( )

!*****************************************************************************80
!
!! I4VEC_BINARY_NEXT_TEST tests I4VEC_BINARY_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) bvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_BINARY_NEXT_TEST'
  write ( *, '(a)' ) '  I4VEC_BINARY_NEXT generates the next binary vector.'
  write ( *, '(a)' ) ''
 
  bvec(1:n) = 0

  do

    call i4vec_transpose_print ( n, bvec, '  ' )

    if ( all ( bvec(1:n) == 1 ) ) then
      exit
    end if

    call i4vec_binary_next ( n, bvec )
 
  end do

  return
end
subroutine i4vec_bracket_test ( )

!*****************************************************************************80
!
!! I4VEC_BRACKET_TEST tests I4VEC_BRACKET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20
  integer ( kind = 4 ), parameter :: test_num = 6

  integer ( kind = 4 ) a(n_max)
  integer ( kind = 4 ), dimension (test_num) :: atest = (/ &
    -10, 2, 9, 10, 20, 24 /)
  integer ( kind = 4 ) aval
  integer ( kind = 4 ) i
  integer ( kind = 4 ) left
  integer ( kind = 4 ) n
  integer ( kind = 4 ) right
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_BRACKET_TEST'
  write ( *, '(a)' ) '  I4VEC_BRACKET finds a pair of entries in a'
  write ( *, '(a)' ) '  sorted I4VEC which bracket a value.'

  n = 10
  do i = 1, n
    a(i) = 2 * i
  end do
  a(6) = a(5)

  call i4vec_print ( n, a, '  Sorted array:' )

  do test = 1, test_num

    aval = atest(test)

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Search for AVAL = ', aval

    call i4vec_bracket ( n, a, aval, left, right )

    write ( *, '(a,i8)' ) '  Left = ', left
    write ( *, '(a,i8)' ) '  Right = ', right

    if ( 1 <= left ) then
      write ( *, '(a,i8)' ) '  A(LEFT)=', a(left)
    end if

    if ( 1 <= right ) then
      write ( *, '(a,i8)' ) '  A(RIGHT) = ', a(right)
    end if

  end do

  return
end
subroutine i4vec_choose_test ( )

!*****************************************************************************80
!
!! I4VEC_CHOOSE_TEST tests I4VEC_CHOOSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5

  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) i4vec_choose
  integer ( kind = 4 ) j(m)
  integer ( kind = 4 ) k(m)
  integer ( kind = 4 ) mm
  integer ( kind = 4 ) n(m)
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) v1
  integer ( kind = 4 ) v2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_CHOOSE_TEST:'
  write ( *, '(a)' ) '  I4VEC_CHOOSE computes the generalized binomial coefficient.'

  seed = 12345678

  call i4vec_uniform_ab ( m, 0, 6, seed, j )
  call i4vec_uniform_ab ( m, 0, 6, seed, k )
  n(1:m) = j(1:m) + k(1:m)

  call i4vec_transpose_print ( m, n, "  N:" );
  call i4vec_transpose_print ( m, k, "  K:" );

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   M        V1        V2'
  write ( *, '(a)' ) ''
  v1 = 1
  do mm = 0, m
 
    if ( mm == 0 ) then
      v1 = 1
    else
      v1 = v1 * i4_choose ( n(mm), k(mm) )
    end if
    v2 = i4vec_choose ( mm, n, k );
    write ( *, '(2x,i2,2x,i8,2x,i8)' ) mm, v1, v2

  end do

  return
end
subroutine i4vec_concatenate_test ( )

!*****************************************************************************80
!
!! I4VEC_CONCATENATE_TEST tests I4VEC_CONCATENATE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 5
  integer ( kind = 4 ), parameter :: n2 = 3
  integer ( kind = 4 ), parameter :: n3 = n1 + n2

  integer ( kind = 4 ), dimension ( n1 ) :: a1 = (/ &
    91, 31, 71, 51, 31 /)
  integer ( kind = 4 ), dimension ( n2 ) :: a2 = (/ &
    42, 22, 12 /)
  integer ( kind = 4 ) a3(n3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_CONCATENATE_TEST'
  write ( *, '(a)' ) '  I4VEC_CONCATENATE concatenates two I4VECs'

  call i4vec_print ( n1, a1, '  Array 1:' )
  call i4vec_print ( n2, a2, '  Array 2:' )
  call i4vec_concatenate ( n1, a1, n2, a2, a3 )
  call i4vec_print ( n3, a3, '  Array 3 = Array 1 + Array 2:' )

  return
end
subroutine i4vec_cum_test ( )

!*****************************************************************************80
!
!! I4VEC_CUM_TEST tests I4VEC_CUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 December 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) a_cum(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_CUM_TEST'
  write ( *, '(a)' ) '  For an integer vector:'
  write ( *, '(a)' ) '  I4VEC_CUM:   cumulative sum;'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  call i4vec_cum ( n, a, a_cum )

  call i4vec_print ( n, a_cum, '  Cumulative sums:' )

  return
end
subroutine i4vec_cum0_test ( )

!*****************************************************************************80
!
!! I4VEC_CUM0_TEST tests I4VEC_CUM0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 December 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) a_cum0(0:n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_CUM0_TEST'
  write ( *, '(a)' ) '  For an integer vector:'
  write ( *, '(a)' ) '  I4VEC_CUM0:  cumulative sum, zero based;'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  call i4vec_cum0 ( n, a, a_cum0 )

  call i4vec_print ( n + 1, a_cum0, '  0-based Cumulative sums:' )

  return
end
subroutine i4vec_decrement_test ( )

!*****************************************************************************80
!
!! I4VEC_DECREMENT_TEST tests I4VEC_DECREMENT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) seed
  integer ( kind = 4 ) v(n)
  integer ( kind = 4 ) v_hi
  integer ( kind = 4 ) v_lo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_DECREMENT_TEST'
  write ( *, '(a)' ) '  I4VEC_DECREMENT decrements an I4VEC.'

  v_lo = -5
  v_hi = 10
  seed = 123456789
  call i4vec_uniform_ab ( n, v_lo, v_hi, seed, v )
  call i4vec_print ( n, v, '  The I4VEC:' )
  call i4vec_decrement ( n, v )
  call i4vec_print ( n, v, '  The I4VEC after decrementing:' )

  return
end
subroutine i4vec_direct_product_test ( )

!*****************************************************************************80
!
!! I4VEC_DIRECT_PRODUCT_TEST tests I4VEC_DIRECT_PRODUCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: factor_num = 3
  integer ( kind = 4 ), parameter :: point_num = 24

  integer ( kind = 4 ) factor_index
  integer ( kind = 4 ) factor_order
  integer ( kind = 4 ), allocatable, dimension ( : ) :: factor_value
  integer ( kind = 4 ) j
  integer ( kind = 4 ) x(factor_num,point_num)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_DIRECT_PRODUCT_TEST'
  write ( *, '(a)' ) '  I4VEC_DIRECT_PRODUCT forms the entries of a'
  write ( *, '(a)' ) '  direct product of a given number of I4VEC factors.'

  x(1:factor_num,1:point_num) = 0

  do factor_index = 1, factor_num

    if ( factor_index == 1 ) then
      factor_order = 4
      allocate ( factor_value(1:factor_order) )
      factor_value = (/ 1, 2, 3, 4 /)
    else if ( factor_index == 2 ) then
      factor_order = 3
      allocate ( factor_value(1:factor_order) )
      factor_value = (/ 50, 60, 70 /)
    else if ( factor_index == 3 ) then
      factor_order = 2
      allocate ( factor_value(1:factor_order) )
      factor_value = (/ 800, 900 /)
    end if

    call i4vec_direct_product ( factor_index, factor_order, factor_value,  &
      factor_num, point_num, x )

    deallocate ( factor_value )

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     J     X(1)  X(2)  X(3)'
  write ( *, '(a)' ) ''

  do j = 1, point_num
    write ( *, '(2x,i4,4x,i4,2x,i4,2x,i4)' ) j, x(1:factor_num,j)
  end do

  return
end
subroutine i4vec_direct_product2_test ( )

!*****************************************************************************80
!
!! I4VEC_DIRECT_PRODUCT2_TEST tests I4VEC_DIRECT_PRODUCT2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: factor_num = 3
  integer ( kind = 4 ), parameter :: point_num = 24

  integer ( kind = 4 ) factor_index
  integer ( kind = 4 ) factor_order
  integer ( kind = 4 ), allocatable, dimension ( : ) :: factor_value
  integer ( kind = 4 ) w(point_num)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_DIRECT_PRODUCT2_TEST'
  write ( *, '(a)' ) '  I4VEC_DIRECT_PRODUCT2 forms the entries of a'
  write ( *, '(a)' ) '  direct product of a given number of I4VEC factors.'

  w(1:point_num) = 1

  do factor_index = 1, factor_num

    if ( factor_index == 1 ) then
      factor_order = 4
      allocate ( factor_value(1:factor_order) )
      factor_value = (/ 2, 3, 5, 7 /)
    else if ( factor_index == 2 ) then
      factor_order = 3
      allocate ( factor_value(1:factor_order) )
      factor_value = (/ 11, 13, 17 /)
    else if ( factor_index == 3 ) then
      factor_order = 2
      allocate ( factor_value(1:factor_order) )
      factor_value = (/ 19, 21 /)
    end if

    call i4vec_direct_product2 ( factor_index, factor_order, factor_value,  &
      factor_num, point_num, w )

    deallocate ( factor_value )

  end do

  call i4vec_print ( point_num, w, '  Product W:' )

  return
end
subroutine i4vec_distances_test ( )

!*****************************************************************************80
!
!! I4VEC_DISTANCES_TEST tests I4VEC_DISTANCES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 January 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5;

  integer ( kind = 4 ), allocatable :: d(:)
  integer ( kind = 4 ) :: locate(5) = (/ 0, 3, 10, 20, 100 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_DISTANCES_TEST'
  write ( *, '(a)' )  '  I4VEC_DISTANCES computes the pairwise distances'
  write ( *, '(a)' )  '  between elements of an I4VEC.'

  allocate ( d(1:n*(n-1)/2) )

  call i4vec_distances ( n, locate, d )

  call i4vec_print ( n, locate, '  Locations:' )
  call i4vec_print ( n * ( n - 1 ) / 2, d, '  Distances:' )

  deallocate ( d )

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

  write ( *, '(a)' ) ''
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
subroutine i4vec_frac_test ( )

!*****************************************************************************80
!
!! I4VEC_FRAC_TEST tests I4VEC_FRAC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) afrac
  integer ( kind = 4 ) :: b = 1
  integer ( kind = 4 ) :: c = 2 * n
  integer ( kind = 4 ) k
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_FRAC_TEST'
  write ( *, '(a)' ) '  I4VEC_FRAC: K-th smallest entry in an I4VEC.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  The array to search:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Fractile    Value'
  write ( *, '(a)' ) ''

  do k = 1, n, n / 2

    call i4vec_frac ( n, a, k, afrac )

    write ( *, '(2x,2i8)' ) k, afrac

  end do

  return
end
subroutine i4vec_heap_a_test ( )

!*****************************************************************************80
!
!! I4VEC_HEAP_A_TEST tests I4VEC_HEAP_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = n
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_HEAP_A_TEST'
  write ( *, '(a)' ) '  For an I4VEC,'
  write ( *, '(a)' ) '  I4VEC_HEAP_A puts into ascending heap form.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Unsorted array:' )

  call i4vec_heap_a ( n, a )

  call i4vec_print ( n, a, '  Ascending heap form:' )

  return
end
subroutine i4vec_heap_d_test ( )

!*****************************************************************************80
!
!! I4VEC_HEAP_D_TEST tests I4VEC_HEAP_D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_HEAP_D_TEST'
  write ( *, '(a)' ) '  For an I4VEC,'
  write ( *, '(a)' ) '  I4VEC_HEAP_D puts into descending heap form.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  do i = 1, n
    a(i) = i4_uniform_ab ( 0, n, seed )
  end do

  call i4vec_print ( n, a, '  Unsorted array:' )

  call i4vec_heap_d ( n, a )

  call i4vec_print ( n, a, '  Descending heap form:' )

  return
end
subroutine i4vec_heap_d_extract_test ( )

!*****************************************************************************80
!
!! I4VEC_HEAP_D_EXTRACT_TEST tests I4VEC_HEAP_D_EXTRACT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  integer ( kind = 4 ) a(n_max)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) val

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_HEAP_D_EXTRACT_TEST'
  write ( *, '(a)' ) '  For a descending heap of integers,'
  write ( *, '(a)' ) '  I4VEC_HEAP_D_EXTRACT extracts the maximum value;'

  n = 0

  seed = 123456789

  do i = 1, n_max

    b = 0
    c = 10

    val = i4_uniform_ab ( b, c, seed )

    call i4vec_heap_d_insert ( n, a, val )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Inserting value          ', val

    call i4vec_heap_d_max ( n, a, val )

    write ( *, '(a,i8)' ) '  Current maximum value is ', val

  end do

  call i4vec_print ( n, a, '  Current heap as a vector:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now extract the maximum several times.'
  write ( *, '(a)' ) ''

  do i = 1, 5
    call i4vec_heap_d_extract ( n, a, val )
    write ( *, '(a,i8)' ) '  Extracting maximum element = ', val
  end do

  call i4vec_print ( n, a, '  Current heap as a vector:' )

  return
end
subroutine i4vec_heap_d_insert_test ( )

!*****************************************************************************80
!
!! I4VEC_HEAP_D_INSERT_TEST tests I4VEC_HEAP_D_INSERT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  integer ( kind = 4 ) a(n_max)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) val

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_HEAP_D_INSERT_TEST'
  write ( *, '(a)' ) '  For a descending heap of integers,'
  write ( *, '(a)' ) '  I4VEC_HEAP_D_INSERT inserts a value into the heap.'

  n = 0

  seed = 123456789

  do i = 1, n_max

    b = 0
    c = 10

    val = i4_uniform_ab ( b, c, seed )

    call i4vec_heap_d_insert ( n, a, val )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Inserting value          ', val

    call i4vec_heap_d_max ( n, a, val )

    write ( *, '(a,i8)' ) '  Current maximum value is ', val

  end do

  call i4vec_print ( n, a, '  Current heap as a vector:' )

  return
end
subroutine i4vec_heap_d_max_test ( )

!*****************************************************************************80
!
!! I4VEC_HEAP_D_MAX_TEST tests I4VEC_HEAP_D_MAX
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  integer ( kind = 4 ) a(n_max)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) val

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_HEAP_D_MAX_TEST'
  write ( *, '(a)' ) '  For a descending heap of integers,'
  write ( *, '(a)' ) '  I4VEC_HEAP_D_MAX reports the maximum value.'

  n = 0

  seed = 123456789

  do i = 1, n_max

    b = 0
    c = 10

    val = i4_uniform_ab ( b, c, seed )

    call i4vec_heap_d_insert ( n, a, val )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Inserting value          ', val

    call i4vec_heap_d_max ( n, a, val )

    write ( *, '(a,i8)' ) '  Current maximum value is ', val

  end do

  return
end
subroutine i4vec_histogram_test ( )

!*****************************************************************************80
!
!! I4VEC_HISTOGRAM_TEST tests I4VEC_HISTOGRAM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 1000

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ), allocatable, dimension ( : ) :: histo_gram
  integer ( kind = 4 ) histo_num
  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_HISTOGRAM_TEST'
  write ( *, '(a)' ) '  I4VEC_HISTOGRAM histograms an I4VEC.'

  call i4vec_uniform_ab ( n, 0, 25, seed, a )

  histo_num = 20
  allocate ( histo_gram(0:histo_num) )

  call i4vec_histogram ( n, a, histo_num, histo_gram )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Histogram of data from 0 to ', histo_num
  write ( *, '(a)' ) ''

  do i = 0, histo_num
    if ( 0 < histo_gram(i) ) then
      write ( *, '(2x,i8,2x,i8)' ) i, histo_gram(i)
    end if
  end do

  return
end
subroutine i4vec_identity_row_test ( )

!*****************************************************************************80
!
!! I4VEC_IDENTITY_ROW_TEST tests I4VEC_IDENTITY_ROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_IDENTITY_ROW_TEST'
  write ( *, '(a)' ) '  I4VEC_IDENTITY_ROW returns a row of the identity matrix.'
  write ( *, '(a)' ) ''
  do i = 0, n + 1
    call i4vec_identity_row ( n, i, a )
    write ( *, '(i2,a,5i2)' ) i, ':', a(1:n)
  end do

  return
end
subroutine i4vec_increment_test ( )

!*****************************************************************************80
!
!! I4VEC_INCREMENT_TEST tests I4VEC_INCREMENT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) seed
  integer ( kind = 4 ) v(n)
  integer ( kind = 4 ) v_hi
  integer ( kind = 4 ) v_lo

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INCREMENT_TEST'
  write ( *, '(a)' ) '  I4VEC_INCREMENT increments an I4VEC.'

  v_lo = -5
  v_hi = 10
  seed = 123456789
  call i4vec_uniform_ab ( n, v_lo, v_hi, seed, v )
  call i4vec_print ( n, v, '  The I4VEC:' )
  call i4vec_increment ( n, v )
  call i4vec_print ( n, v, '  The I4VEC after incrementing:' )


  return
end
subroutine i4vec_index_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEX_TEST tests I4VEC_INDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 November 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) aval
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i4vec_index
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEX_TEST'
  write ( *, '(a)' ) '  For an I4VEC:'
  write ( *, '(a)' ) '  I4VEC_INDEX:              first index of given value;'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  aval = a(n/2)
  write ( *, '(a)' ) ''
  j = i4vec_index ( n, a, aval )
  write ( *, '(a,i8,a,i8)' ) '  Index of first occurrence of ', aval, ' is ', j

  aval = aval + 1
  j = i4vec_index ( n, a, aval )
  write ( *, '(a,i8,a,i8)' ) '  Index of first occurrence of ', aval, ' is ', j

  return
end
subroutine i4vec_index_delete_all_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEX_DELETE_ALL_TEST tests I4VEC_INDEX_DELETE_ALL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 25

  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEX_DELETE_ALL'
  write ( *, '(a)' ) '  For an index sorted array of integers.'
  write ( *, '(a)' ) '  I4VEC_INDEX_DELETE_ALL deletes all copies of a'
  write ( *, '(a)' ) '  particular value.'

  xval = 8
  call i4vec_index_insert ( n, x, indx, xval )

  xval = 7
  call i4vec_index_insert ( n, x, indx, xval )

  b = 0
  c = 20
  seed = 123456789

  do i = 1, 20
    xval = i4_uniform_ab ( b, c, seed )
    write ( *, '(4x,i3)' ) xval
    call i4vec_index_insert ( n, x, indx, xval )
  end do

  xval = 7
  call i4vec_index_insert ( n, x, indx, xval )

  xval = 8
  call i4vec_index_insert ( n, x, indx, xval )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,i3,9x,i3)' ) i, indx(i), x(i), x(indx(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Call I4VEC_INDEX_DELETE_ALL to delete values of 7:'

  xval = 7
  call i4vec_index_delete_all ( n, x, indx, xval )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,i3,9x,i3)' ) i, indx(i), x(i), x(indx(i))
  end do

  return
end
subroutine i4vec_index_delete_dupes_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEX_DELETE_DUPES_TEST tests I4VEC_INDEX_DELETE_DUPES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 25

  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEX_DELETE_DUPES'
  write ( *, '(a)' ) '  I4VEC_INDEX_DELETE_DUPES deletes duplicates.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate some random values:'
  write ( *, '(a)' ) ''

  xval = 8
  call i4vec_index_insert ( n, x, indx, xval )

  xval = 7
  call i4vec_index_insert ( n, x, indx, xval )

  b = 0
  c = 20
  seed = 123456789

  do i = 1, 20
    xval = i4_uniform_ab ( b, c, seed )
    write ( *, '(4x,i3)' ) xval
    call i4vec_index_insert ( n, x, indx, xval )
  end do

  xval = 7
  call i4vec_index_insert ( n, x, indx, xval )

  xval = 8
  call i4vec_index_insert ( n, x, indx, xval )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,i3,9x,i3)' ) i, indx(i), x(i), x(indx(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Call I4VEC_INDEX_DELETE_DUPES to delete duplicates:'

  call i4vec_index_delete_dupes ( n, x, indx, n, x, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of unique entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,i3)' ) i, indx(i), x(i)
  end do

  return
end
subroutine i4vec_index_delete_one_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEX_DELETE_ONE_TEST tests I4VEC_INDEX_DELETE_ONE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 25

  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEX_DELETE_ONE_TEST'
  write ( *, '(a)' ) '  For an index sorted array of integers.'
  write ( *, '(a)' ) '  I4VEC_INDEX_DELETE_ONE deletes one copies of a'
  write ( *, '(a)' ) '  particular value.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate some random values:'
  write ( *, '(a)' ) ''

  xval = 8
  call i4vec_index_insert ( n, x, indx, xval )

  xval = 7
  call i4vec_index_insert ( n, x, indx, xval )

  b = 0
  c = 20
  seed = 123456789

  do i = 1, 20
    xval = i4_uniform_ab ( b, c, seed )
    write ( *, '(4x,i3)' ) xval
    call i4vec_index_insert ( n, x, indx, xval )
  end do

  xval = 7
  call i4vec_index_insert ( n, x, indx, xval )

  xval = 8
  call i4vec_index_insert ( n, x, indx, xval )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,i3,9x,i3)' ) i, indx(i), x(i), x(indx(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Call I4VEC_INDEX_DELETE_ONE to delete a value of 8:'

  xval = 8
  call i4vec_index_delete_one ( n, x, indx, xval, n, x, indx )

  return
end
subroutine i4vec_index_insert_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEX_INSERT_TEST tests I4VEC_INDEX_INSERT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 25

  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEX_INSERT_TEST'
  write ( *, '(a)' ) '  I4VEC_INDEX_INSERT inserts values into an'
  write ( *, '(a)' ) '  index sorted array of integers.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate some random values:'
  write ( *, '(a)' ) ''

  xval = 8
  call i4vec_index_insert ( n, x, indx, xval )

  xval = 7
  call i4vec_index_insert ( n, x, indx, xval )

  b = 0
  c = 20
  seed = 123456789

  do i = 1, 20
    xval = i4_uniform_ab ( b, c, seed )
    write ( *, '(4x,i3)' ) xval
    call i4vec_index_insert ( n, x, indx, xval )
  end do

  xval = 7
  call i4vec_index_insert ( n, x, indx, xval )

  xval = 8
  call i4vec_index_insert ( n, x, indx, xval )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,i3,9x,i3)' ) i, indx(i), x(i), x(indx(i))
  end do

  return
end
subroutine i4vec_index_insert_unique_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEX_INSERT_UNIQUE_TEST tests I4VEC_INDEX_INSERT_UNIQUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEX_INSERT_UNIQUE_TEST'
  write ( *, '(a)' ) '  I4VEC_INDEX_INSERT_UNIQUE inserts unique values into an'
  write ( *, '(a)' ) '  index sorted array.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate some random values:'

  b = 0
  c = n_max
  seed = 123456789

  do i = 1, n_max
    xval = i4_uniform_ab ( b, c, seed )
    call i4vec_index_insert_unique ( n, x, indx, xval )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I   INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,i3,9x,i3)' ) i, indx(i), x(i), x(indx(i))
  end do

  return
end
subroutine i4vec_index_order_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEX_ORDER_TEST tests I4VEC_INDEX_ORDER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEX_ORDER_TEST'
  write ( *, '(a)' ) '  I4VEC_INDEX_ORDER sorts an index sorted array.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate some random values:'
  write ( *, '(a)' ) ''

  b = 0
  c = 20
  seed = 123456789

  do i = 1, 20
    xval = i4_uniform_ab ( b, c, seed )
    write ( *, '(4x,i3)' ) xval
    call i4vec_index_insert_unique ( n, x, indx, xval )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of unique entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,i3,9x,i3)' ) i, indx(i), x(i), x(indx(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now call I4VEC_INDEX_ORDER to carry out the sorting:'

  call i4vec_index_order ( n, x, indx )

  call i4vec_print ( n, x, '  X:' )

  return
end
subroutine i4vec_index_search_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEX_SEARCH_TEST tests I4VEC_INDEX_SEARCH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) equal
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEX_SEARCH_TEST'
  write ( *, '(a)' ) '  I4VEC_INDEX_SEARCH searches for an entry with '
  write ( *, '(a)' ) '  a given value.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate some random values:'

  b = 0
  c = n_max
  seed = 123456789

  do i = 1, n_max
    xval = i4_uniform_ab ( b, c, seed )
    call i4vec_index_insert_unique ( n, x, indx, xval )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I   INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,i3,9x,i3)' ) i, indx(i), x(i), x(indx(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Results of search for given XVAL:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  XVAL  Less Equal More'
  write ( *, '(a)' ) ''

  do xval = 0, 20
    call i4vec_index_search ( n, x, indx, xval, less, equal, more )
    write ( *, '(2x,i3,3x,i3,3x,i3,3x,i3)' ) xval, less, equal, more
  end do

  return
end
subroutine i4vec_indexed_heap_d_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEXED_HEAP_D_TEST tests I4VEC_INDEXED_HEAP_D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 20
  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) :: a(m) = (/ &
    101, 102, 103, 104, 105, 106, 107, 108, 109, 110, &
    111, 112, 113, 114, 115, 116, 117, 118, 119, 120 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: indx(n) = (/ &
    1, 11, 17, 5, 7, 13, 15, 3, 19, 9 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEXED_HEAP_D_TEST'
  write ( *, '(a)' ) '  I4VEC_INDEXED_HEAP_D creates a descending heap'
  write ( *, '(a)' ) '  from an indexed I4VEC.'
!
!  Print before.
!
  call i4vec_print ( m, a, '  The data vector:' )
  call i4vec_print ( n, indx, '  The index vector:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX):'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
  end do
!
!  Heap the data.
!
  call i4vec_indexed_heap_d ( n, a, indx )
!
!  Print afterwards.  Only INDX should change.
!
  call i4vec_print ( m, a, '  The data vector (should NOT change):' )
  call i4vec_print ( n, indx, '  The index vector (may change):' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) is now a descending heap:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
  end do

  return
end
subroutine i4vec_indexed_heap_d_extract_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEXED_HEAP_D_EXTRACT_TEST tests I4VEC_INDEXED_HEAP_D_EXTRACT
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 20
  integer ( kind = 4 ), parameter :: n_max = 20

  integer ( kind = 4 ) a(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) indx_extract
  integer ( kind = 4 ) indx_insert
  integer ( kind = 4 ) indx_max
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEXED_HEAP_D_EXTRACT'
  write ( *, '(a)' ) '  For an indexed I4VEC,'
  write ( *, '(a)' ) '  I4VEC_INDEXED_HEAP_D_EXTRACT extracts the maximum value;'
!
!  Set the data array.  To keep things easy, we will use the indicator vector.
!
  call i4vec_indicator1 ( m, a )
!
!  The index array will initially be a random subset of the numbers 1 to M,
!  in random order.
!
  n = 5
  indx(1:11) = (/ 9, 2, 8, 14, 5, 7, 15, 1, 19, 20, 3 /)

  call i4vec_print ( m, a, '  The data vector:' )
  call i4vec_print ( n, indx, '  The index vector:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX):'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
  end do
!
!  Create a descending heap from the indexed array.
!
  call i4vec_indexed_heap_d ( n, a, indx )

  call i4vec_print ( n, indx, '  The index vector after heaping:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after heaping:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
  end do
!
!  Insert five entries, and monitor the maximum.
!
  do i = 1, 5

    indx_insert = indx(n+1)

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Inserting value          ', a(indx_insert)

    call i4vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

    call i4vec_indexed_heap_d_max ( n, a, indx, indx_max )

    write ( *, '(a,i8)' ) '  Current maximum is ', a(indx_max)

  end do
  call i4vec_print ( m, a, '  The data vector after insertions:' )
  call i4vec_print ( n, indx, '  The index vector after insertions:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after insertions:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
  end do
!
!  Extract the first 5 largest elements.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now extract the maximum several times.'
  write ( *, '(a)' ) ''

  do i = 1, 5
    call i4vec_indexed_heap_d_extract ( n, a, indx, indx_extract )
    write ( *, '(a,i8,a,i8)' ) '  Extracting maximum element A(', &
      indx_extract,') = ', a(indx_extract)
  end do

  call i4vec_print ( m, a, '  The data vector after extractions:' )
  call i4vec_print ( n, indx, '  The index vector after extractions:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after extractions:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
  end do

  return
end
subroutine i4vec_indexed_heap_d_insert_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEXED_HEAP_D_INSERT_TEST tests I4VEC_INDEXED_HEAP_D_INSERT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 20
  integer ( kind = 4 ), parameter :: n_max = 20

  integer ( kind = 4 ) a(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) indx_insert
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEXED_HEAP_D_INSERT_TEST'
  write ( *, '(a)' ) '  For an indexed I4VEC,'
  write ( *, '(a)' ) '  I4VEC_INDEXED_HEAP_D_INSERT inserts a value into the heap.'
!
!  Set the data array.  To keep things easy, we will use the indicator vector.
!
  call i4vec_indicator1 ( m, a )
!
!  The index array will initially be a random subset of the numbers 1 to M,
!  in random order.
!
  n = 5
  indx(1:11) = (/ 9, 2, 8, 14, 5, 7, 15, 1, 19, 20, 3 /)

  call i4vec_print ( m, a, '  The data vector:' )
  call i4vec_print ( n, indx, '  The index vector:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX):'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
  end do
!
!  Create a descending heap from the indexed array.
!
  call i4vec_indexed_heap_d ( n, a, indx )

  call i4vec_print ( n, indx, '  The index vector after heaping:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after heaping:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
  end do
!
!  Insert five entries
!
  do i = 1, 5

    indx_insert = indx(n+1)

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Inserting value          ', a(indx_insert)

    call i4vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

  end do

  call i4vec_print ( m, a, '  The data vector after insertions:' )
  call i4vec_print ( n, indx, '  The index vector after insertions:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after insertions:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
  end do

  return
end
subroutine i4vec_indexed_heap_d_max_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEXED_HEAP_D_MAX_TEST tests I4VEC_INDEXED_HEAP_D_MAX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 20
  integer ( kind = 4 ), parameter :: n_max = 20

  integer ( kind = 4 ) a(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) indx_insert
  integer ( kind = 4 ) indx_max
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEXED_HEAP_D_MAX_TEST'
  write ( *, '(a)' ) '  For an indexed I4VEC,'
  write ( *, '(a)' ) '  I4VEC_INDEXED_HEAP_D_MAX reports the maximum value.'
!
!  Set the data array.  To keep things easy, we will use the indicator vector.
!
  call i4vec_indicator1 ( m, a )
!
!  The index array will initially be a random subset of the numbers 1 to M,
!  in random order.
!
  n = 5
  indx(1:11) = (/ 9, 2, 8, 14, 5, 7, 15, 1, 19, 20, 3 /)

  call i4vec_print ( m, a, '  The data vector:' )
  call i4vec_print ( n, indx, '  The index vector:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX):'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
  end do
!
!  Create a descending heap from the indexed array.
!
  call i4vec_indexed_heap_d ( n, a, indx )

  call i4vec_print ( n, indx, '  The index vector after heaping:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after heaping:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
  end do
!
!  Insert five entries, and monitor the maximum.
!
  do i = 1, 5

    indx_insert = indx(n+1)

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Inserting value          ', a(indx_insert)

    call i4vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

    call i4vec_indexed_heap_d_max ( n, a, indx, indx_max )

    write ( *, '(a,i8)' ) '  Current maximum is ', a(indx_max)

  end do

  return
end
subroutine i4vec_indicator0_test ( )

!*****************************************************************************80
!
!! I4VEC_INDICATOR0_TEST tests I4VEC_INDICATOR0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDICATOR0_TEST'
  write ( *, '(a)' ) '  I4VEC_INDICATOR0 returns a 0-based indicator vector.'

  call i4vec_indicator0 ( n, a )

  call i4vec_print ( n, a, '  The "indicator0" vector:' )

  return
end
subroutine i4vec_indicator1_test ( )

!*****************************************************************************80
!
!! I4VEC_INDICATOR1_TEST tests I4VEC_INDICATOR1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDICATOR1_TEST'
  write ( *, '(a)' ) '  I4VEC_INDICATOR1 returns a 1-based indicator vector.'

  call i4vec_indicator1 ( n, a )

  call i4vec_print ( n, a, '  The "indicator1" vector:' )

  return
end
subroutine i4vec_insert_test ( )

!*****************************************************************************80
!
!! I4VEC_INSERT_TEST tests I4VEC_INSERT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20
  integer ( kind = 4 ), parameter :: test_num = 6

  integer ( kind = 4 ) a(n_max)
  integer ( kind = 4 ), dimension (test_num) :: atest = (/ &
    -10, 2, 9, 10, 20, 24 /)
  integer ( kind = 4 ) aval
  integer ( kind = 4 ) i
  integer ( kind = 4 ) left
  integer ( kind = 4 ) n
  integer ( kind = 4 ) right
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INSERT_TEST'
  write ( *, '(a)' ) '  I4VEC_INSERT inserts a value into a vector.'

  n = 10
  do i = 1, n
    a(i) = 2 * i
  end do
  a(6) = a(5)

  call i4vec_print ( n, a, '  Sorted array:' )

  do test = 1, test_num

    aval = atest(test)

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Search for AVAL = ', aval

    call i4vec_bracket ( n, a, aval, left, right )

    write ( *, '(a,i8)' ) '  Left = ', left
    write ( *, '(a,i8)' ) '  Right = ', right

    if ( 1 <= left ) then
      write ( *, '(a,i8)' ) '  A(LEFT)=', a(left)
    end if

    if ( 1 <= right ) then
      write ( *, '(a,i8)' ) '  A(RIGHT) = ', a(right)
    end if
!
!  Insert the value.
!
    if ( left == -1 ) then
      left = 0
    end if

    if ( left == right ) then

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  No insertion necessary.'

    else

      call i4vec_insert ( n, a, left+1, aval )

      n = n + 1

      call i4vec_print ( n, a, '  Sorted, augmented array:' )

    end if

  end do

  return
end
subroutine i4vec_is_ascending_test ( )

!*****************************************************************************80
!
!! I4VEC_IS_ASCENDING_TEST tests I4VEC_IS_ASCENDING.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4
  integer ( kind = 4 ), parameter :: test_num = 6

  logical ( kind = 4 ) i4vec_is_ascending
  integer ( kind = 4 ) test
  integer ( kind = 4 ) x(n)
!
!  Each ROW of this definition is a COLUMN of the matrix.
!
  integer ( kind = 4 ), dimension(n,test_num) :: x_test = reshape ( (/ &
    1, 3, 2, 4, &
    2, 2, 2, 2, &
    1, 2, 2, 4, &
    1, 2, 3, 4, &
    4, 4, 3, 1, &
    9, 7, 3, 0 /), (/ n, test_num /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_IS_ASCENDING_TEST'
  write ( *, '(a)' ) '  I4VEC_IS_ASCENDING determines if an I4VEC ascends.'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    x(1:n) = x_test(1:n,test)

    call i4vec_print ( n, x, '  Test vector:' )

    write ( *, '(a,l1)' ) '  I4VEC_IS_ASCENDING =  ', &
      i4vec_is_ascending ( n, x )

  end do

  return
end
subroutine i4vec_is_binary_test ( )

!*****************************************************************************80
!
!! I4VEC_IS_BINARY_TEST tests I4VEC_IS_BINARY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical i4vec_is_binary
  integer ( kind = 4 ) n
  integer ( kind = 4 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_IS_BINARY_TEST'
  write ( *, '(a)' ) '  I4VEC_IS_BINARY is TRUE if an I4VEC only contains'
  write ( *, '(a)' ) '  0 or 1 entries.'

  n = 3
  allocate ( x(1:n) )
  x = (/ 0, 0, 0 /)
  write ( *, '(a)' ) ''
  call i4vec_transpose_print ( n, x, '  X:' )
  if ( i4vec_is_binary ( n, x ) ) then
    write ( *, '(a)' ) '  X is binary.'
  else
    write ( *, '(a)' ) '  X is NOT binary.'
  end if
  deallocate ( x )

  n = 3
  allocate ( x(1:n) )
  x = (/ 1, 0, 1 /)
  write ( *, '(a)' ) ''
  call i4vec_transpose_print ( n, x, '  X:' )
  if ( i4vec_is_binary ( n, x ) ) then
    write ( *, '(a)' ) '  X is binary.'
  else
    write ( *, '(a)' ) '  X is NOT binary.'
  end if
  deallocate ( x )

  n = 3
  allocate ( x(1:n) )
  x = (/ 0, 2, 1 /)
  write ( *, '(a)' ) ''
  call i4vec_transpose_print ( n, x, '  X:' )
  if ( i4vec_is_binary ( n, x ) ) then
    write ( *, '(a)' ) '  X is binary.'
  else
    write ( *, '(a)' ) '  X is NOT binary.'
  end if
  deallocate ( x )

  return
end
subroutine i4vec_is_descending_test ( )

!*****************************************************************************80
!
!! I4VEC_IS_DESCENDING_TEST tests I4VEC_IS_DESCENDING.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4
  integer ( kind = 4 ), parameter :: test_num = 6

  logical ( kind = 4 ) i4vec_is_descending
  integer ( kind = 4 ) test
  integer ( kind = 4 ) x(n)
!
!  Each ROW of this definition is a COLUMN of the matrix.
!
  integer ( kind = 4 ), dimension(n,test_num) :: x_test = reshape ( (/ &
    1, 3, 2, 4, &
    2, 2, 2, 2, &
    1, 2, 2, 4, &
    1, 2, 3, 4, &
    4, 4, 3, 1, &
    9, 7, 3, 0 /), (/ n, test_num /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_IS_DESCENDING_TEST'
  write ( *, '(a)' ) '  I4VEC_IS_DESCENDING determines if an I4VEC descends.'

  do test = 1, test_num

    x(1:n) = x_test(1:n,test)

    call i4vec_print ( n, x, '  Test vector:' )

    write ( *, '(a,l1)' ) '  I4VEC_IS_DESCENDING = ', &
      i4vec_is_descending ( n, x )

  end do

  return
end
subroutine i4vec_is_pairwise_prime_test ( )

!*****************************************************************************80
!
!! I4VEC_IS_PAIRWISE_PRIME_TEST tests I4VEC_IS_PAIRWISE_PRIME.
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

  integer ( kind = 4 ), parameter :: n = 4
  integer ( kind = 4 ), parameter :: test_num = 6

  logical ( kind = 4 ) i4vec_is_pairwise_prime
  integer ( kind = 4 ) test
  integer ( kind = 4 ) x(n)
!
!  Each ROW of the definition is a COLUMN of the matrix.
!
  integer ( kind = 4 ), dimension ( n, test_num ) :: x_test = reshape ( (/ &
     1,  3,  2,  4, &
     2,  2,  2,  2, &
     5,  7, 12, 29, &
     1, 13,  1, 11, &
     1,  4,  9, 16, &
     6, 35, 13, 77 /), (/ n, test_num /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_IS_PAIRWISE_PRIME_TEST'
  write ( *, '(a)' ) '  I4VEC_IS_PAIRWISE_PRIME determines if an I4VEC'
  write ( *, '(a)' ) '  is pairwise prime.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '              Pairwise'
  write ( *, '(a)' ) '  Row Vector     Prime?'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    x(1:n) = x_test(1:n,test)

    write ( *, '(2x,4i3,3x,l1)' ) x(1:n), i4vec_is_pairwise_prime ( n, x )

  end do

  return
end
subroutine i4vec_max_test ( )

!*****************************************************************************80
!
!! I4VEC_MAX_TEST tests I4VEC_MAX.
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

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) i4vec_max
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(n)
  integer ( kind = 4 ) x_max

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_MAX_TEST'
  write ( *, '(a)' ) '  For an I4VEC:'
  write ( *, '(a)' ) '  I4VEC_MAX:           maximum entry;'

  a = 1
  b = 30
  seed = 123456789

  call i4vec_uniform_ab ( n, a, b, seed, x )

  call i4vec_print ( n, x, '  Input vector:' )

  x_max = i4vec_max ( n, x )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Maximum:                  ', x_max

  return
end
subroutine i4vec_max_index_test ( )

!*****************************************************************************80
!
!! I4VEC_MAX_INDEX_TEST tests I4VEC_MAX_INDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 November 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_MAX_INDEX_TEST'
  write ( *, '(a)' ) '  For an I4VEC:'
  write ( *, '(a)' ) '  I4VEC_MAX_INDEX:          a maximal index;'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call i4vec_max_index ( n, a, ival )
  write ( *, '(a,i8)' ) '  Maximum index:            ', ival

  return
end
subroutine i4vec_max_index_last_test ( )

!*****************************************************************************80
!
!! I4VEC_MAX_INDEX_LAST_TEST tests I4VEC_MAX_INDEX, I4VEC_MAX_INDEX_LAST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 November 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) i4vec_max_index_last
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_MAX_INDEX_LAST_TEST'
  write ( *, '(a)' ) '  For an I4VEC:'
  write ( *, '(a)' ) '  I4VEC_MAX_INDEX_LAST:     last maximal index;'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  ival = i4vec_max_index_last ( n, a )
  write ( *, '(a,i8)' ) '  Last maximum index:       ', ival

  return
end
subroutine i4vec_max_last_test ( )

!*****************************************************************************80
!
!! I4VEC_MAX_LAST_TEST tests I4VEC_MAX_LAST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 January 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) i4vec_max_last
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(n)
  integer ( kind = 4 ) x_max

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_MAX_LAST_TEST'
  write ( *, '(a)' ) '  I4VEC_MAX_LAST identifies the largest element in an'
  write ( *, '(a)' ) '  I4VEC, and moves it to the final entry.'

  seed = 123456789

  do i = 1, n
    x(i) = i4_uniform_ab ( 1, 30, seed )
  end do

  call i4vec_print ( n, x, '  Input vector:' )

  x_max = i4vec_max_last ( n, x )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Maximum:                  ', x_max

  call i4vec_print ( n, x, '  Output vector:' )

  return
end
subroutine i4vec_mean_test ( )

!*****************************************************************************80
!
!! I4VEC_MEAN_TEST tests I4VEC_MEAN.
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

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  real ( kind = 8 ) mean
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_MEAN_TEST'
  write ( *, '(a)' ) '  For an I4VEC:'
  write ( *, '(a)' ) '  I4VEC_MEAN:          mean value;'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  call i4vec_mean ( n, a, mean )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Mean:                     ', mean

  return
end
subroutine i4vec_median_test ( )

!*****************************************************************************80
!
!! I4VEC_MEDIAN_TEST tests I4VEC_MEDIAN.
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

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) median
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_MEDIAN_TEST'
  write ( *, '(a)' ) '  For an I4VEC:'
  write ( *, '(a)' ) '  I4VEC_MEDIAN:        median value;'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  call i4vec_median ( n, a, median )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Median:                   ', median

  return
end
subroutine i4vec_merge_a_test ( )

!*****************************************************************************80
!
!! I4VEC_MERGE_A_TEST tests I4VEC_MERGE_A.
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

  integer ( kind = 4 ), parameter :: n1 = 10
  integer ( kind = 4 ), parameter :: n2 = 10

  integer ( kind = 4 ) a1(n1)
  integer ( kind = 4 ) a2(n2)
  integer ( kind = 4 ) a3(n1+n2)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) n3
  integer ( kind = 4 ) search_val
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_MERGE_A_TEST'
  write ( *, '(a)' ) '  I4VEC_MERGE_A merges two ascending-sorted I4VECs;'

  seed = 123456789

  b = 0
  c = n1

  call i4vec_uniform_ab ( n1, b, c, seed, a1 )

  search_val = a1(1)

  call i4vec_sort_heap_a ( n1, a1 )

  b = 0
  c = n2

  call i4vec_uniform_ab ( n2, b, c, seed, a2 )

  call i4vec_sort_heap_a ( n2, a2 )

  call i4vec_print ( n1, a1, '  Input vector A1:' )

  call i4vec_print ( n2, a2, '  Input vector A2:' )

  call i4vec_merge_a ( n1, a1, n2, a2, n3, a3 )

  call i4vec_print ( n3, a3, '  Merged vector A3:' )

  return
end
subroutine i4vec_min_test ( )

!*****************************************************************************80
!
!! I4VEC_MIN_TEST tests I4VEC_MIN.
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

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) i4vec_min
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(n)
  integer ( kind = 4 ) x_min

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_MIN_TEST'
  write ( *, '(a)' ) '  For an I4VEC:'
  write ( *, '(a)' ) '  I4VEC_MIN:           minimum entry;'

  a = 1
  b = 30
  seed = 123456789

  call i4vec_uniform_ab ( n, a, b, seed, x )

  call i4vec_print ( n, x, '  Input vector:' )

  x_min = i4vec_min ( n, x )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Minimum:                  ', x_min

  return
end
subroutine i4vec_min_index_test ( )

!*****************************************************************************80
!
!! I4VEC_MIN_INDEX_TEST tests I4VEC_MIN_INDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 November 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_MIN_INDEX_TEST'
  write ( *, '(a)' ) '  For an I4VEC:'
  write ( *, '(a)' ) '  I4VEC_MIN_INDEX:          a minimal index;'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call i4vec_min_index ( n, a, ival )
  write ( *, '(a,i8)' ) '  Minimum index:            ', ival

  return
end
subroutine i4vec_nonzero_count_test ( )

!*****************************************************************************80
!
!! I4VEC_NONZERO_COUNT_TEST tests I4VEC_NONZERO_COUNT.
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

  integer ( kind = 4 ), parameter :: n = 15

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i4vec_nonzero_count
  integer ( kind = 4 ) nonzero
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_NONZERO_COUNT_TEST'
  write ( *, '(a)' ) '  For an I4VEC:'
  write ( *, '(a)' ) '  I4VEC_NONZERO_COUNT: number of nonzeroes;'

  seed = 123456789

  b = -3
  c = 4

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  nonzero = i4vec_nonzero_count ( n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of nonzeroes :     ', nonzero

  return
end
subroutine i4vec_nonzero_first_test ( )

!*****************************************************************************80
!
!! I4VEC_NONZERO_FIRST_TEST tests I4VEC_NONZERO_FIRST.
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) a_save(n)
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) nz
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_NONZERO_FIRST_TEST'
  write ( *, '(a)' ) '  For an I4VEC:'
  write ( *, '(a)' ) '  I4VEC_NONZERO_FIRST left shifts the nonzero entries'
  write ( *, '(a)' ) '  of an I4VEC so they appear first.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '  ----------Before--------------    ----------After---------------'
  write ( *, '(a)' ) ''
  seed = 123456789

  ilo = -1
  ihi = +2

  do test = 1, test_num

    call i4vec_uniform_ab ( n, ilo, ihi, seed, a )
    a_save(1:n) = a(1:n)
    call i4vec_nonzero_first ( n, a, nz, indx )
    write ( *, '(2x,10i3,4x,10i3)' ) a_save(1:n), a(1:n)

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The value NZ counts the nonzeros, and'
  write ( *, '(a)' ) '  the vector INDX indicates the original positions:'
  write ( *, '(a)' ) ''

  call i4vec_uniform_ab ( n, ilo, ihi, seed, a )
  a_save(1:n) = a(1:n)
  call i4vec_nonzero_first ( n, a, nz, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Original vector:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,10i3)' ) a_save(1:n)
  write ( *, '(a)' ) ''
  write ( *, '(a,i2)' ) '  Number of nonzeros NZ = ', nz
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Shifted vector:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,10i3)' ) a(1:n)
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Index vector:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,10i3)' ) indx(1:n)

  return
end
subroutine i4vec_order_type_test ( )

!*****************************************************************************80
!
!! I4VEC_ORDER_TYPE_TEST tests I4VEC_ORDER_TYPE.
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

  integer ( kind = 4 ), parameter :: n = 4
  integer ( kind = 4 ), parameter :: test_num = 6

  integer ( kind = 4 ) j
  integer ( kind = 4 ) order
  integer ( kind = 4 ) test
  integer ( kind = 4 ) x(n)
!
!  Each ROW of the definition is a COLUMN of the matrix.
!
  integer ( kind = 4 ), dimension ( n, test_num ) :: x_test = reshape ( (/ &
    1, 3, 2, 4, &
    2, 2, 2, 2, &
    1, 2, 2, 4, &
    1, 2, 3, 4, &
    4, 4, 3, 1, &
    9, 7, 3, 0 /), (/ n, test_num /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_ORDER_TYPE_TEST'
  write ( *, '(a)' ) '  I4VEC_ORDER_TYPE classifies an I4VEC as'
  write ( *, '(a)' ) '  -1: no order'
  write ( *, '(a)' ) '   0: all equal;'
  write ( *, '(a)' ) '   1: ascending;'
  write ( *, '(a)' ) '   2: strictly ascending;'
  write ( *, '(a)' ) '   3: descending;'
  write ( *, '(a)' ) '   4: strictly descending.'

  do test = 1, test_num

    x(1:n) = x_test(1:n,test)

    call i4vec_order_type ( n, x, order )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  The following vector has order type ', order
    write ( *, '(a)' ) ''
    do j = 1, n
      write ( *, '(2x,i8,i8)' ) j, x(j)
    end do

  end do

  return
end
subroutine i4vec_part_test ( )

!*****************************************************************************80
!
!! I4VEC_PART_TEST tests I4VEC_PART.
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

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) nval

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_PART_TEST'
  write ( *, '(a)' ) '  I4VEC_PART partitions an integer.'

  nval = 17
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  NVAL = ', nval

  call i4vec_part ( n, nval, a )

  call i4vec_print ( n, a, '  Partitioned:' )

  nval = -49
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  NVAL = ', nval

  call i4vec_part ( n, nval, a )

  call i4vec_print ( n, a, '  Partitioned:' )

  return
end
subroutine i4vec_part_quick_a_test ( )

!*****************************************************************************80
!
!! I4VEC_PART_QUICK_A_TEST tests I4VEC_PART_QUICK_A.
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
  integer ( kind = 4 ) l
  integer ( kind = 4 ) r
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_PART_QUICK_A_TEST'
  write ( *, '(a)' ) '  I4VEC_PART_QUICK_A reorders an I4VEC'
  write ( *, '(a)' ) '  as part of a quick sort.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Before rearrangement:' )

  call i4vec_part_quick_a ( n, a, l, r )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rearranged array'
  write ( *, '(a,i8)' ) '  Left index =  ', l
  write ( *, '(a,i8)' ) '  Key index =   ', l + 1
  write ( *, '(a,i8)' ) '  Right index = ', r

  call i4vec_print ( l,     a(1:l),   '  Left half:' )
  call i4vec_print ( 1,     a(l+1),   '  Key:' )
  call i4vec_print ( n-l-1, a(l+2:n), '  Right half:' )

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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_PERMUTE_TEST'
  write ( *, '(a)' ) '  I4VEC_PERMUTE reorders an I4VEC'
  write ( *, '(a)' ) '  according to a given permutation.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  A, before rearrangement:' )

  call perm1_uniform ( n, seed, p )

  call i4vec_print ( n, p, '  Permutation vector P:' )

  call i4vec_permute ( n, p, a )

  call i4vec_print ( n, a, '  A, after rearrangement:' )

  return
end
subroutine i4vec_permute_uniform_test ( )

!*****************************************************************************80
!
!! I4VEC_PERMUTE_UNIFORM_TEST tests I4VEC_PERMUTE_UNIFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_PERMUTE_UNIFORM_TEST'
  write ( *, '(a)' ) '  I4VEC_PERMUTE_UNIFORM randomly reorders an I4VEC.'

  do i = 1, n
    a(i) = 100 + i
  end do

  seed = 123456789

  call i4vec_print ( n, a, '  A, before permutation:' )

  call i4vec_permute_uniform ( n, a, seed )

  call i4vec_print ( n, a, '  A, after random permutation:' )

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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_PRINT_TEST'
  write ( *, '(a)' ) '  I4VEC_PRINT prints an I4VEC'

  call i4vec_print ( n, a, '  The I4VEC:' )

  return
end
subroutine i4vec_red_test ( )

!*****************************************************************************80
!
!! I4VEC_RED_TEST tests I4VEC_RED.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) factor
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_RED_TEST'
  write ( *, '(a)' ) '  I4VEC_RED divides out any common factors in the'
  write ( *, '(a)' ) '  entries of an I4VEC.'

  a = reshape ( (/ &
    12,   4, -12, 30, 0, &
    88,   8,  88, 18, 4, &
     9, 192,  94, 42, 8 /), (/ 5, 3 /) )

  call i4mat_print ( m, n, a, '  Apply I4VEC_RED to each row of this matrix:' )

  do i = 1, m
    call i4vec_red ( n, a(i,1:n), factor )
  end do

  call i4mat_print ( m, n, a, '  Reduced matrix:' )

  return
end
subroutine i4vec_reverse_test ( )

!*****************************************************************************80
!
!! I4VEC_REVERSE_TEST tests I4VEC_REVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 June 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = 3 * n
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_REVERSE_TEST'
  write ( *, '(a)' ) '  I4VEC_REVERSE reverses an I4VEC.'

  seed = 123456789

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Original vector:' )

  call i4vec_reverse ( n, a )

  call i4vec_print ( n, a, '  Reversed:' )

  a(1:n) = a(n:1:-1)

  call i4vec_print ( n, a, '  Re-reversed array using a(1:n) = a(n:1:-1):' )

  return
end
subroutine i4vec_run_count_test ( )

!*****************************************************************************80
!
!! I4VEC_RUN_COUNT_TEST tests I4VEC_RUN_COUNT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) run_count
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_RUN_COUNT_TEST'
  write ( *, '(a)' ) '  I4VEC_RUN_COUNT counts runs in an I4VEC'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) ' Run Count        Sequence'
  write ( *, '(a)' ) ''

  seed = 123456789

  do test = 1, test_num

    call i4vec_uniform_ab ( n, 0, 1, seed, a )

    call i4vec_run_count ( n, a, run_count )

    write ( *, '(2x,i8,8x,20i2)' ) run_count, a(1:n)

  end do

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
  integer ( kind = 4 ) index
  integer ( kind = 4 ) search_val
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SEARCH_BINARY_A_TEST'
  write ( *, '(a)' ) '  For ascending order:'
  write ( *, '(a)' ) '  I4VEC_SEARCH_BINARY_A searches an I4VEC for a value;'

  seed = 123456789

  b = 0
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  search_val = a(1)

  call i4vec_sort_heap_a ( n, a )

  call i4vec_print ( n, a, '  Input vector A:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Search the array A for the value ', search_val

  call i4vec_search_binary_a ( n, a, search_val, index )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  SEARCH RESULT:'
  if ( 0 < index ) then
    write ( *, '(a,i8)' ) '    The value occurs in index ', index
  else
    write ( *, '(a)' ) '    The value does not occur in the array.'
  end if

  return
end
subroutine i4vec_sort_bubble_a_test ( )

!*****************************************************************************80
!
!! I4VEC_SORT_BUBBLE_A_TEST tests I4VEC_SORT_BUBBLE_A.
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
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = 3 * n
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORT_BUBBLE_A_TEST'
  write ( *, '(a)' ) '  For an I4VEC,'
  write ( *, '(a)' ) '  I4VEC_SORT_BUBBLE_A ascending sorts,'

  seed = 123456789

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Unsorted:' )

  call i4vec_sort_bubble_a ( n, a )

  call i4vec_print ( n, a, '  Ascending sorted:' )

  return
end
subroutine i4vec_sort_heap_a_test ( )

!*****************************************************************************80
!
!! I4VEC_SORT_HEAP_A_TEST tests I4VEC_SORT_HEAP_A.
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
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORT_HEAP_A_TEST'
  write ( *, '(a)' ) '  I4VEC_SORT_HEAP_A ascending sorts an I4VEC,'

  seed = 123456789

  do i = 1, n
    a(i) = i4_uniform_ab ( 0, 3 * n, seed )
  end do

  call i4vec_print ( n, a, '  Unsorted:' )

  call i4vec_sort_heap_a ( n, a )

  call i4vec_print ( n, a, '  Ascending sorted:' )

  return
end
subroutine i4vec_sort_heap_d_test ( )

!*****************************************************************************80
!
!! I4VEC_SORT_HEAP_D_TEST tests I4VEC_SORT_HEAP_D.
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
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = 3 * n
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORT_HEAP_D_TEST'
  write ( *, '(a)' ) '  I4VEC_SORT_HEAP_D descending sorts an I4VEC.'

  seed = 123456789

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Unsorted:' )

  call i4vec_sort_heap_d ( n, a )

  call i4vec_print ( n, a, '  Descending sorted:' )

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

  write ( *, '(a)' ) ''
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, INDX(I), A(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(3i8)' ) i, indx(i), a(indx(i))
  end do

  return
end
subroutine i4vec_sort_heap_index_d_test ( )

!*****************************************************************************80
!
!! I4VEC_SORT_HEAP_INDEX_D_TEST tests I4VEC_SORT_HEAP_INDEX_D.
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORT_HEAP_INDEX_D_TEST'
  write ( *, '(a)' ) '  I4VEC_SORT_HEAP_INDEX_D creates a descending'
  write ( *, '(a)' ) '  sort index for an I4VEC.'

  seed = 123456789

  b = 0
  c = 3 * n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Unsorted array:' )

  call i4vec_sort_heap_index_d ( n, a, indx )

  call i4vec_print ( n, indx, '  Index vector INDX:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now use the index array to carry out the'
  write ( *, '(a)' ) '  permutation implicitly.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, INDX(I), A(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,3i8)' ) i, indx(i), a(indx(i))
  end do

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
!    25 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORT_INSERT_A_TEST'
  write ( *, '(a)' ) '  I4VEC_SORT_INSERT_A sorts an I4VEC.'

  seed = 123456789

  b = 0
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Unsorted array:' )

  call i4vec_sort_insert_a ( n, a )

  call i4vec_print ( n, a, '  Sorted array:' )

  return
end
subroutine i4vec_sort_quick_a_test ( )

!*****************************************************************************80
!
!! I4VEC_SORT_QUICK_A_TEST tests I4VEC_SORT_QUICK_A.
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
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORT_QUICK_A_TEST'
  write ( *, '(a)' ) '  I4VEC_SORT_QUICK_A quicksorts an I4VEC.'

  seed = 123456789

  b = 0
  c = 3 * n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Unsorted array:' )

  call i4vec_sort_quick_a ( n, a )

  call i4vec_print ( n, a, '  Sorted array:' )

  return
end
subroutine i4vec_sort_shell_a_test ( )

!*****************************************************************************80
!
!! I4VEC_SORT_SHELL_A_TEST tests I4VEC_SORT_SHELL_A.
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
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORT_SHELL_A_TEST'
  write ( *, '(a)' ) '  I4VEC_SORT_SHELL_A Shell sorts an I4VEC.'

  seed = 123456789

  b = 0
  c = 3 * n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Unsorted array:' )

  call i4vec_sort_shell_a ( n, a )

  call i4vec_print ( n, a, '  Sorted array:' )

  return
end
subroutine i4vec_sorted_undex_test ( )

!*****************************************************************************80
!
!! I4VEC_SORTED_UNDEX_TEST tests I4VEC_SORTED_UNDEX.
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

  integer ( kind = 4 ), parameter :: x_num = 9

  integer ( kind = 4 ) i
  integer ( kind = 4 ), allocatable, dimension ( : ) :: undx
  integer ( kind = 4 ) x_unique_num
  integer ( kind = 4 ), dimension ( x_num ) :: x_val = (/ &
    11, 11, 11, 22, 22, 33, 33, 55, 55 /)
  integer ( kind = 4 ), allocatable, dimension ( : ) :: xu_val
  integer ( kind = 4 ) xdnu(x_num)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORTED_UNDEX_TEST'
  write ( *, '(a)' ) '  I4VEC_SORTED_UNDEX produces index vectors which create a sorted'
  write ( *, '(a)' ) '  list of the unique elements of a sorted I4VEC,'
  write ( *, '(a)' ) '  and a map from the original vector to the (implicit)'
  write ( *, '(a)' ) '  vector of sorted unique elements.'

  call i4vec_print ( x_num, x_val, '  The vector X:' )

  call i4vec_sorted_unique_count ( x_num, x_val, x_unique_num )

  allocate ( undx(1:x_unique_num) )
  allocate ( xu_val(1:x_unique_num) )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of unique entries in X is ', x_unique_num

  call i4vec_sorted_undex ( x_num, x_val, x_unique_num, undx, xdnu )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  UNDX can be used to list the unique elements of X'
  write ( *, '(a)' ) '  in sorted order.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  UNDX   X(UNDX)'
  write ( *, '(a)' ) ''

  do i = 1, x_unique_num
    write ( *, '(2x,i4,2x,i4,2x,i8)' ) i, undx(i), x_val(undx(i))
  end do

  xu_val(1:x_unique_num) = x_val(undx(1:x_unique_num))

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  UNDX can be used to created XU, a copy of X'
  write ( *, '(a)' ) '  containing only the unique elements, in sorted order.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  UNDX XU(I)'
  write ( *, '(a)' ) ''
  do i = 1, x_unique_num
    write ( *, '(2x,i4,2x,i4,2x,i4)' ) i, undx(i), xu_val(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  XDNU can be used to match each element of X with one of the'
  write ( *, '(a)' ) '  unique elements'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  XDNU  X(I)   XU(XDNU(I))'
  write ( *, '(a)' ) ''

  do i = 1, x_num
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,i12)' ) i, xdnu(i), x_val(i), xu_val(xdnu(i))
  end do

  deallocate ( undx )
  deallocate ( xu_val )

  return
end
subroutine i4vec_sorted_unique_test ( )

!*****************************************************************************80
!
!! I4VEC_SORTED_UNIQUE_TEST tests I4VEC_SORTED_UNIQUE.
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

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) unique_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORTED_UNIQUE_TEST'
  write ( *, '(a)' ) '  I4VEC_SORTED_UNIQUE finds unique entries in a sorted array.'

  seed = 123456789

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_sort_heap_a ( n, a )

  call i4vec_print ( n, a, '  Input vector:' )

  call i4vec_sorted_unique ( n, a, unique_num )

  call i4vec_print ( unique_num, a, '  Unique entries:' )

  return
end
subroutine i4vec_sorted_unique_count_test (  )

!*****************************************************************************80
!
!! I4VEC_SORTED_UNIQUE_COUNT_TEST tests I4VEC_SORTED_UNIQUE_COUNT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20
  
  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) a_unique
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) seed

  b = 0
  c = n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORTED_UNIQUE_COUNT_TEST'
  write ( *, '(a)' ) &
    '  I4VEC_SORTED_UNIQUE_COUNT counts unique entries in a sorted I4VEC.'

  seed = 123456789

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_sort_heap_a ( n, a )

  call i4vec_print ( n, a, '  Input vector:' )

  call i4vec_sorted_unique_count ( n, a, a_unique )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of unique entries is ', a_unique

  return
end
subroutine i4vec_sorted_unique_hist_test ( )

!*****************************************************************************80
!
!! I4VEC_SORTED_UNIQUE_HIST_TEST tests I4VEC_SORTED_UNIQUE_HIST.
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

  integer ( kind = 4 ), parameter :: unique_max = 20
  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) acount(unique_max)
  integer ( kind = 4 ) auniq(unique_max)
  integer ( kind = 4 ) :: b = 0
  integer ( kind = 4 ) :: c = 3 * n
  integer ( kind = 4 ) unique_num
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORTED_UNIQUE_HIST_TEST'
  write ( *, '(a)' ) '  For an I4VEC,'
  write ( *, '(a)' ) '  I4VEC_SORTED_UNIQUE_HIST makes a histogram '
  write ( *, '(a)' ) '  of unique entries.'

  seed = 123456789

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Unsorted:' )

  call i4vec_sort_bubble_a ( n, a )

  call i4vec_print ( n, a, '  Ascending sorted:' )

  call i4vec_sorted_unique_hist ( n, a, unique_max, unique_num, auniq, acount );

  write ( *, '(a)' ) ''
  write ( *, '(a,i8,a)' ) '  I4VEC_UNIQ3 counts ',  unique_num, ' unique entries.'

  call i4vec2_print ( unique_num, auniq, acount, '  Value and Multiplicity' )

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

  write ( *, '(a)' ) ''
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
subroutine i4vec_sum_vec_test ( )

!*****************************************************************************80
!
!! I4VEC_SUM_VEC_TEST tests I4VEC_SUM_VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) c(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SUM_VEC_TEST'
  write ( *, '(a)' ) '  I4VEC_SUM_VEC does a pairwise sum of two I4VEC''s.'

  seed = 123456789

  call i4vec_uniform_ab ( n, 0, 10, seed, a )
  call i4vec_transpose_print ( n, a, '  A:' )

  call i4vec_uniform_ab ( n, 0, 10, seed, b )
  call i4vec_transpose_print ( n, b, '  B:' )

  call i4vec_sum_vec ( n, a, b, c )
  call i4vec_transpose_print ( n, c, '  C = A + B:' )

  return
end
subroutine i4vec_transpose_print_test ( )

!*****************************************************************************80
!
!! I4VEC_TRANSPOSE_PRINT_TEST tests I4VEC_TRANSPOSE_PRINT.
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

  integer ( kind = 4 ), parameter :: n = 12

  integer ( kind = 4 ) a(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_TRANSPOSE_PRINT_TEST'
  write ( *, '(a)' ) '  I4VEC_TRANSPOSE_PRINT prints an I4VEC'
  write ( *, '(a)' ) '  with 5 entries to a row, and an optional title.'

  call i4vec_indicator1 ( n, a )

  call i4vec_print ( n, a, '  Output from I4VEC_PRINT:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Call I4VEC_TRANSPOSE_PRINT with a short title:'

  call i4vec_transpose_print ( n, a, '  My array:  ' )

  return
end
subroutine i4vec_undex_test ( )

!*****************************************************************************80
!
!! I4VEC_UNDEX_TEST tests I4VEC_UNDEX.
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

  integer ( kind = 4 ), parameter :: x_num = 9

  integer ( kind = 4 ) i
  integer ( kind = 4 ), allocatable, dimension ( : ) :: undx
  integer ( kind = 4 ) x_unique_num
  integer ( kind = 4 ), dimension ( x_num ) :: x_val = (/ &
    33, 55, 11, 11, 55, 33, 22, 22, 11 /)
  integer ( kind = 4 ), allocatable, dimension ( : ) :: xu_val
  integer ( kind = 4 ) xdnu(x_num)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_UNDEX_TEST'
  write ( *, '(a)' ) '  I4VEC_UNDEX produces index vectors which create a sorted'
  write ( *, '(a)' ) '  list of the unique elements of an (unsorted) I4VEC,'
  write ( *, '(a)' ) '  and a map from the original vector to the (implicit)'
  write ( *, '(a)' ) '  vector of sorted unique elements.'

  call i4vec_print ( x_num, x_val, '  The vector X:' )

  call i4vec_unique_count ( x_num, x_val, x_unique_num )

  allocate ( undx(1:x_unique_num) )
  allocate ( xu_val(1:x_unique_num) )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of unique entries in X is ', x_unique_num

  call i4vec_undex ( x_num, x_val, x_unique_num, undx, xdnu )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  UNDX can be used to list the unique elements of X'
  write ( *, '(a)' ) '  in sorted order.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  UNDX   X(UNDX)'
  write ( *, '(a)' ) ''

  do i = 1, x_unique_num
    write ( *, '(2x,i4,2x,i4,2x,i8)' ) i, undx(i), x_val(undx(i))
  end do

  xu_val(1:x_unique_num) = x_val(undx(1:x_unique_num))

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  UNDX can be used to created XU, a copy of X'
  write ( *, '(a)' ) '  containing only the unique elements, in sorted order.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  UNDX XU(I)'
  write ( *, '(a)' ) ''
  do i = 1, x_unique_num
    write ( *, '(2x,i4,2x,i4,2x,i4)' ) i, undx(i), xu_val(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  XDNU can be used to match each element of X with one of the'
  write ( *, '(a)' ) '  unique elements'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  XDNU  X(I)   XU(XDNU(I))'
  write ( *, '(a)' ) ''

  do i = 1, x_num
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,i12)' ) i, xdnu(i), x_val(i), xu_val(xdnu(i))
  end do

  deallocate ( undx )
  deallocate ( xu_val )

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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  I4VEC_UNIFORM_AB computes pseudorandom values '
  write ( *, '(a)' ) '  in an interval [A,B].'

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a,i12)' ) '  The lower endpoint A = ', a
  write ( *, '(a,i12)' ) '  The upper endpoint B = ', b
  write ( *, '(a,i12)' ) '  The initial seed is ', seed

  call i4vec_uniform_ab ( n, a, b, seed, v )

  call i4vec_print ( n, v, '  The random vector:' )

  return
end
subroutine i4vec_unique_count_test (  )

!*****************************************************************************80
!
!! I4VEC_UNIQUE_COUNT_TEST tests I4VEC_UNIQUE_COUNT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20
  
  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) a_unique
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) seed

  b = 0
  c = n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_UNIQUE_COUNT_TEST'
  write ( *, '(a)' ) '  I4VEC_UNIQUE_COUNT counts unique entries in an I4VEC.'

  seed = 123456789

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  call i4vec_unique_count ( n, a, a_unique )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of unique entries is ', a_unique

  return
end
subroutine i4vec_unique_index_test ( )

!*****************************************************************************80
!
!! I4VEC_UNIQUE_INDEX_TEST tests I4VEC_UNIQUE_INDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2008
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
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) unique_index(n)

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_UNIQUE_INDEX_TEST'
  write ( *, '(a)' ) '  I4VEC_UNIQUE_INDEX, for each entry in an I4VEC'
  write ( *, '(a)' ) '  indexes the unique elements.'

  b = 1
  c = 5

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_unique_index ( n, a, unique_index )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I      A(I)    UNIQUE'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) i, a(i), unique_index(i)
  end do

  return
end
subroutine i4vec_value_index_test ( )

!*****************************************************************************80
!
!! I4VEC_VALUE_INDEX_TEST tests I4VEC_VALUE_INDEX.
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

  integer ( kind = 4 ), parameter :: max_index = 3
  integer ( kind = 4 ), parameter :: n = 25

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) n_index
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) value
  integer ( kind = 4 ) value_index(max_index)

  seed = 123456789

  value = 3

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_VALUE_INDEX_TEST'
  write ( *, '(a)' ) '  I4VEC_VALUE_INDEX indexes entries equal to'
  write ( *, '(a)' ) '  a given value.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The desired value is ', value
  write ( *, '(a,i8)' ) '  Maximum number of indices to find is ', max_index

  b = 1
  c = 5

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector A:' )

  call i4vec_value_index ( n, a, value, max_index, n_index, value_index )

  call i4vec_print ( n_index, value_index, &
    '  Indices of entries equal to given value: ' )

  return
end
subroutine i4vec_variance_test ( )

!*****************************************************************************80
!
!! I4VEC_VARIANCE_TEST tests I4VEC_VARIANCE.
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) seed
  real ( kind = 8 ) variance

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_VARIANCE_TEST'
  write ( *, '(a)' ) '  I4VEC_VARIANCE computes the variance of an I4VEC.'

  seed = 123456789

  b = -n
  c = n

  call i4vec_uniform_ab ( n, b, c, seed, a )

  call i4vec_print ( n, a, '  Input vector:' )

  call i4vec_variance ( n, a, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Variance:                 ', variance

  return
end
subroutine i4vec_width_test ( )

!*****************************************************************************80
!
!! I4VEC_WIDTH_TEST tests I4VEC_WIDTH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 13

  integer ( kind = 4 ) i4vec_width
  integer ( kind = 4 ) :: x(n) = (/ &
    0, 1, 2, 3, 9, 10, 11, 99, 101, -1, -2, -3, -9 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_WIDTH_TEST'
  write ( *, '(a)' ) '  I4VEC_WIDTH determines the printing "width" of an I4VEC.'
  
  call i4vec_print ( n, x, '  The I4VEC:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The printing width is ', i4vec_width ( n, x )

  return
end
subroutine i4vec2_print_test ( )

!*****************************************************************************80
!
!! I4VEC2_PRINT_TEST tests I4VEC2_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(0:n)
  integer ( kind = 4 ) b(0:n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC2_PRINT_TEST'
  write ( *, '(a)' ) '  I4VEC2_PRINT prints a pair of I4VECs'

  do i = 0, n
    a(i) = ( i * ( i + 1 ) ) / 2
    b(i) = ( i * ( i + 1 ) * ( 2 * i + 1 ) ) / 6
  end do

  call i4vec2_print ( n + 1, a, b, '  I, sum of I, sum of I^2:' )

  return
end
subroutine i4vec2_sort_a_test ( )

!*****************************************************************************80
!
!! I4VEC2_SORT_A_TEST tests I4VEC2_SORT_A.
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) ivec(n)
  integer ( kind = 4 ) jvec(n)
  integer ( kind = 4 ) seed

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC2_SORT_A_TEST'
  write ( *, '(a)' ) '  For a pair of I4VECs:'
  write ( *, '(a)' ) '  I4VEC2_SORT_A ascending sorts;'

  b = 1
  c = 3

  call i4vec_uniform_ab ( n, b, c, seed, ivec )

  call i4vec_uniform_ab ( n, b, c, seed, jvec )

  ivec(3) = ivec(1)
  jvec(3) = jvec(1)

  ivec(5) = ivec(2)
  jvec(5) = jvec(2)

  ivec(9) = ivec(1)
  jvec(9) = jvec(1)

  call i4vec2_print ( n, ivec, jvec, '  The array:' )

  call i4vec2_sort_a ( n, ivec, jvec )

  call i4vec2_print ( n, ivec, jvec, '  After ascending sort:' )

  return
end
subroutine i4vec2_sort_d_test ( )

!*****************************************************************************80
!
!! I4VEC2_SORT_D_TEST tests I4VEC2_SORT_D.
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) ivec(n)
  integer ( kind = 4 ) jvec(n)
  integer ( kind = 4 ) seed

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC2_SORT_D_TEST'
  write ( *, '(a)' ) '  For a pair of I4VECs:'
  write ( *, '(a)' ) '  I4VEC2_SORT_D descending sorts;'

  b = 1
  c = 3

  call i4vec_uniform_ab ( n, b, c, seed, ivec )

  call i4vec_uniform_ab ( n, b, c, seed, jvec )

  ivec(3) = ivec(1)
  jvec(3) = jvec(1)

  ivec(5) = ivec(2)
  jvec(5) = jvec(2)

  ivec(9) = ivec(1)
  jvec(9) = jvec(1)

  call i4vec2_print ( n, ivec, jvec, '  The array:' )

  call i4vec2_sort_d ( n, ivec, jvec )

  call i4vec2_print ( n, ivec, jvec, '  After descending sort:' )

  return
end
subroutine i4vec2_sorted_unique_test ( )

!*****************************************************************************80
!
!! I4VEC2_SORTED_UNIQUE_TEST tests I4VEC2_SORTED_UNIQUE.
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) ivec(n)
  integer ( kind = 4 ) jvec(n)
  integer ( kind = 4 ) unique_num
  integer ( kind = 4 ) seed

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORTED_UNIQUE_TEST'
  write ( *, '(a)' ) '  For a pair of I4VECs:'
  write ( *, '(a)' ) '  I4VEC2_SORTED_UNIQUE counts unique entries.'

  b = 1
  c = 3

  call i4vec_uniform_ab ( n, b, c, seed, ivec )

  call i4vec_uniform_ab ( n, b, c, seed, jvec )

  ivec(3) = ivec(1)
  jvec(3) = jvec(1)

  ivec(5) = ivec(2)
  jvec(5) = jvec(2)

  ivec(9) = ivec(1)
  jvec(9) = jvec(1)

  call i4vec2_print ( n, ivec, jvec, '  The array:' )

  call i4vec2_sort_a ( n, ivec, jvec )

  call i4vec2_print ( n, ivec, jvec, '  After ascending sort:' )

  call i4vec2_sorted_unique ( n, ivec, jvec, unique_num )

  call i4vec2_print ( unique_num, ivec, jvec, '  After UNIQ:' )

  return
end
subroutine ksub_next4_test ( )

!*****************************************************************************80
!
!! KSUB_NEXT4_TEST tests KSUB_NEXT4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) a(k)
  logical done
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_NEXT4_TEST'
  write ( *, '(a)' ) '  KSUB_NEXT4 generates K subsets of an N set.'
  write ( *, '(a,i8)' ) '  N = ', n
  write ( *, '(a,i8)' ) '  K=  ', k
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank    Subset'
  write ( *, '(a)' ) ''

  done = .true.
  rank = 0
 
  do
 
    call ksub_next4 ( n, k, a, done )
 
    if ( done ) then
      exit
    end if

    rank = rank + 1
    write ( *, '(2x,i4,4x,3i4)' ) rank, a(1:k)

  end do
 
  return
end
subroutine pascal_to_i4_test ( )

!*****************************************************************************80
!
!! PASCAL_TO_I4_TEST tests PASCAL_TO_I4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PASCAL_TO_I4_TEST'
  write ( *, '(a)' ) '  PASCAL_TO_I4 converts Pascal triangle indices to a'
  write ( *, '(a)' ) '  linear index.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I     J =>    K'
  write ( *, '(a)' ) ''

  do d = 0, 4
    do i = d, 0, -1
      j = d - i
      call pascal_to_i4 ( i, j, k )
      write ( *, '(2x,i4,2x,i4,4x,i4)' ) i, j, k
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine perm0_check_test ( )

!*****************************************************************************80
!
!! PERM0_CHECK_TEST tests PERM0_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) ierror
  integer ( kind = 4 ), dimension ( n ) :: p1 = (/ &
    5, 2, 3, 4, 1 /)
  integer ( kind = 4 ), dimension ( n ) :: p2 = (/ &
    4, 1, 3, 0, 2 /)
  integer ( kind = 4 ), dimension ( n ) :: p3 = (/ &
    0, 2, 1, 3, 2 /)
  integer ( kind = 4 ) perm0_check

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM0_CHECK_TEST'
  write ( *, '(a)' ) '  PERM0_CHECK checks a permutation of 0,...,N-1.'
  write ( *, '(a)' ) ''

  call i4vec_transpose_print ( n, p1, '  Permutation 1:' )
  ierror = perm0_check ( n, p1 )

  call i4vec_transpose_print ( n, p2, '  Permutation 2:' )
  ierror = perm0_check ( n, p2 )

  call i4vec_transpose_print ( n, p3, '  Permutation 3:' )
  ierror = perm0_check ( n, p3 )

  return
end
subroutine perm0_uniform_test ( )

!*****************************************************************************80
!
!! PERM0_UNIFORM_TEST tests PERM0_UNIFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 May 2015
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM0_UNIFORM_TEST'
  write ( *, '(a)' ) '  PERM0_UNIFORM randomly selects a permutation of 0,...,N-1.'
  write ( *, '(a)' ) ''

  seed = 123456789

  do test = 1, 5

    call perm0_uniform ( n, seed, p )
    write ( *, '(2x,10i4)' ) p(1:n)

  end do

  return
end
subroutine perm1_check_test ( )

!*****************************************************************************80
!
!! PERM1_CHECK_TEST tests PERM1_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) ierror
  integer ( kind = 4 ), dimension ( n ) :: p1 = (/ &
    5, 2, 3, 4, 1 /) 
  integer ( kind = 4 ), dimension ( n ) :: p2 = (/ &
    4, 1, 3, 0, 2 /)
  integer ( kind = 4 ), dimension ( n ) :: p3 = (/ &
    0, 2, 1, 3, 2 /)
  integer ( kind = 4 ) perm1_check

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_CHECK_TEST'
  write ( *, '(a)' ) '  PERM1_CHECK checks a permutation of 1,...,N.'
  write ( *, '(a)' ) ''

  call i4vec_transpose_print ( n, p1, '  Permutation 1:' )
  ierror = perm1_check ( n, p1 )

  call i4vec_transpose_print ( n, p2, '  Permutation 2:' )
  ierror = perm1_check ( n, p2 )

  call i4vec_transpose_print ( n, p3, '  Permutation 3:' )
  ierror = perm1_check ( n, p3 )

  return
end
subroutine perm1_uniform_test ( )

!*****************************************************************************80
!
!! PERM1_UNIFORM_TEST tests PERM1_UNIFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 May 2015
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_UNIFORM_TEST'
  write ( *, '(a)' ) '  PERM1_UNIFORM randomly selects a permutation of 1,...,N.'
  write ( *, '(a)' ) ''

  seed = 123456789

  do test = 1, 5

    call perm1_uniform ( n, seed, p )
    write ( *, '(2x,10i4)' ) p(1:n)

  end do

  return
end
subroutine permutation_symbol_test ( )

!*****************************************************************************80
!
!! PERMUTATION_SYMBOL_TEST tests PERMUTATION_SYMBOL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) permutation_symbol
  integer ( kind = 4 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERMUTATION_SYMBOL_TEST'
  write ( *, '(a)' ) '  PERMUTATION_SYMBOL evaluates the Levi-Civita permutation symbol.'

  a = (/ 1, 2, 3, 4, 5 /)
  call i4vec_transpose_print ( n, a, '  Input vector:' )
  value = permutation_symbol ( n, a )
  write ( *, '(a,i2)' ) '  Levi-Civita permutation symbol = ', value

  a = (/ 4, 2, 3, 1, 5 /)
  call i4vec_transpose_print ( n, a, '  Input vector:' )
  value = permutation_symbol ( n, a )
  write ( *, '(a,i2)' ) '  Levi-Civita permutation symbol = ', value

  a = (/ 1, 2, 3, 4, 2 /)
  call i4vec_transpose_print ( n, a, '  Input vector:' )
  value = permutation_symbol ( n, a )
  write ( *, '(a,i2)' ) '  Levi-Civita permutation symbol = ', value
 
  return
end
subroutine prime_test ( )

!*****************************************************************************80
!
!! PRIME_TEST tests PRIME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) prime
  integer ( kind = 4 ) prime_max

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PRIME_TEST'
  write ( *, '(a)' ) '  PRIME returns primes from a table.'

  n = -1
  prime_max = prime ( n )
  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of primes stored is ', prime_max
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I    Prime(I)'
  write ( *, '(a)' ) ''
  do i = 1, 10
    write ( *, '(4x,i4,2x,i6)' ) i, prime(i)
  end do
  write ( *, '(a)' ) ''
  do i = prime_max - 10, prime_max
    write ( *, '(4x,i4,2x,i6)' ) i, prime(i)
  end do
  
  return
end
subroutine triangle_lower_to_i4_test ( )

!*****************************************************************************80
!
!! TRIANGLE_LOWER_TO_I4_TEST tests TRIANGLE_LOWER_TO_I4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_LOWER_TO_I4_TEST'
  write ( *, '(a)' ) '  TRIANGLE_LOWER_TO_I4 converts a lower triangular index'
  write ( *, '(a)' ) '  to a linear one.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I, J ==> K'
  write ( *, '(a)' ) ''

  do i = 1, 4
    do j = 1, i
      call triangle_lower_to_i4 ( i, j, k )
      write ( *, '(2x,i4,i4,4x,i4)' )  i, j, k
    end do
  end do

  return
end
