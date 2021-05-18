program main

!*****************************************************************************80
!
!! MAIN is the main program for SUBSET_TEST.
!
!  Discussion:
!
!    SUBSET_TEST tests SUBSET.
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

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test SUBSET.'

  call asm_enum_test ( )
  call asm_triangle_test ( )
  call bell_test ( )
  call catalan_test ( )
  call catalan_row_next_test ( )
  call cfrac_to_rat_test ( )
  call cfrac_to_rfrac_test ( )
  call change_greedy_test ( )
  call change_next_test ( )
  call chinese_check_test ( )
  call chinese_to_i4_test ( )
  call comb_next_test ( )
  call comb_row_next_test ( )
  call comb_unrank_test ( )
  call comp_enum_test ( )
  call comp_next_test ( )
  call comp_next_grlex_test ( )
  call comp_random_test( )
  call comp_random_grlex_test ( )
  call comp_rank_grlex_test ( )
  call comp_to_ksub_test ( )
  call comp_unrank_grlex_test ( )
  call compnz_enum_test ( )
  call compnz_next_test ( )
  call compnz_random_test ( )
  call compnz_to_ksub_test ( )
  call congruence_test ( )
  call count_pose_random_test ( )
  call debruijn_test ( )
  call dec_add_test ( )
  call dec_div_test ( )
  call dec_mul_test ( )
  call dec_round_test ( )
  call dec_to_r8_test ( )
  call dec_to_rat_test ( )
  call dec_to_s_test ( )
  call dec_width_test ( )
  call decmat_det_test ( )
  call decmat_print_test ( )
  call derange_enum_test ( )
  call derange_enum2_test ( )
  call derange_enum3_test ( )
  call derange1_back_next_test ( )
  call derange1_check_test ( )
  call derange1_weed_next_test ( )
  call digraph_arc_euler_test ( )
  call digraph_arc_print_test ( )
  call diophantine_test ( )
  call diophantine_solution_minimize_test ( )
  call dvec_add_test ( )
  call dvec_complementx_test ( )
  call dvec_mul_test ( )
  call dvec_print_test ( )
  call dvec_sub_test ( )
  call dvec_to_i4_test ( )
  call equiv_next_test ( )
  call equiv_next2_test ( )
  call equiv_print_test ( )
  call equiv_print2_test ( )
  call equiv_random_test ( )
  call euler_row_test ( )
  call frobenius_number_order2_test ( )
  call gray_next_test ( )
  call gray_rank1_test ( )
  call gray_rank2_test ( )
  call gray_unrank1_test ( )
  call gray_unrank2_test ( )
  call i4_bclr_test ( )
  call i4_bset_test ( )
  call i4_btest_test ( )
  call i4_choose_test ( )
  call i4_factor_test ( )
  call i4_fall_test ( )
  call i4_gcd_test ( )
  call i4_huge_test ( )
  call i4_log_10_test ( )
  call i4_modp_test ( )
  call i4_moebius_test ( )
  call i4_partition_conj_test ( )
  call i4_partition_count_test ( )
  call i4_partition_count2_test ( )
  call i4_partition_next_test ( )
  call i4_partition_next2_test ( )
  call i4_partition_print_test ( )
  call i4_partition_random_test ( )
  call i4_partitions_next_test ( )
  call i4_rise_test ( )
  call i4_sqrt_test ( )
  call i4_sqrt_cf_test ( )
  call i4_to_chinese_test ( )
  call i4_to_dvec_test ( )
  call i4_to_i4poly_test ( )
  call i4_to_van_der_corput_test ( )
  call i4mat_01_rowcolsum_test ( )
  call i4mat_01_rowcolsum2_test ( )
  call i4mat_u1_inverse_test ( )
  call i4mat_perm1_test ( )
  call i4mat_2perm1_test ( )
  call i4poly_test ( )
  call i4poly_add_test ( )
  call i4poly_cyclo_test ( )
  call i4poly_degree_test ( )
  call i4poly_dif_test ( )
  call i4poly_div_test ( )
  call i4poly_mul_test ( )
  call i4poly_print_test ( )
  call i4poly_to_i4_test ( )
  call i4vec_backtrack_test ( )
  call i4vec_descends_test ( )
  call i4vec_frac_test ( )
  call i4vec_index_test ( )
  call i4vec_maxloc_last_test ( )
  call i4vec_pairwise_prime_test ( )
  call i4vec_reverse_test ( )
  call i4vec_sort_bubble_a_test ( )
  call i4vec_sort_heap_index_d_test ( )
  call i4vec_transpose_print_test ( )
  call i4vec_uniform_ab_test ( )
  call index_box_next_2d_test ( )
  call index_box_next_3d_test ( )
  call index_box2_next_2d_test ( )
  call index_box2_next_3d_test ( )
  call index_next0_test ( )
  call index_next1_test ( )
  call index_next2_test ( )
  call index_rank0_test ( )
  call index_rank1_test ( )
  call index_rank2_test ( )
  call index_unrank0_test ( )
  call index_unrank1_test ( )
  call index_unrank2_test ( )
  call inverse_mod_n_test ( )
  call inversion_to_perm1_test ( )
  call involute_enum_test ( )
  call jfrac_to_rfrac_test ( )
  call josephus_test ( )
  call ksub_next_test ( )
  call ksub_next2_test ( )
  call ksub_next3_test ( )
  call ksub_next4_test ( )
  call ksub_random_test ( )
  call ksub_random2_test ( )
  call ksub_random3_test ( )
  call ksub_random4_test ( )
  call ksub_random5_test ( )
  call ksub_rank_test ( )
  call ksub_to_comp_test ( )
  call ksub_to_compnz_test ( )
  call ksub_unrank_test ( )
  call l4vec_next_test ( )
  call matrix_product_opt_test ( )
  call moebius_matrix_test ( )
  call monomial_count_test ( )
  call monomial_counts_test ( )
  call morse_thue_test ( )
  call multinomial_coef1_test ( )
  call multinomial_coef2_test ( )
  call multiperm_enum_test ( )
  call multiperm_next_test ( )
  call network_flow_max_test ( )
  call nim_sum_test ( )
  call padovan_test ( )
  call pell_basic_test ( )
  call pell_next_test ( )
  call pent_enum_test ( )
  call perm_ascend_test ( )
  call perm_fixed_enum_test ( )
  call perm0_check_test ( )
  call perm0_print_test ( )
  call perm1_break_count_test ( )
  call perm1_canon_to_cycle_test ( )
  call perm1_check_test ( )
  call perm1_cycle_test ( )
  call perm1_cycle_to_canon_test ( )
  call perm1_cycle_to_index_test ( )
  call perm1_distance_test ( )
  call perm1_free_test ( )
  call perm1_index_to_cycle_test ( )
  call perm1_inverse_test ( )
  call perm1_inverse2_test ( )
  call perm1_inverse3_test ( )
  call perm1_lex_next_test ( )
  call perm1_mul_test ( )
  call perm1_next_test ( )
  call perm1_next2_test ( )
  call perm1_next3_test ( )
  call perm1_print_test ( )
  call perm1_random_test ( )
  call perm1_random2_test ( )
  call perm1_rank_test ( )
  call perm1_sign_test ( )
  call perm1_to_equiv_test ( )
  call perm1_to_inversion_test ( )
  call perm1_to_ytb_test ( )
  call perm1_unrank_test ( )
  call perrin_test ( )
  call pord_check_test ( )
  call power_mod_test ( )
  call power_series1_test ( )
  call power_series2_test ( )
  call power_series3_test ( )
  call power_series4_test ( )
  call prime_test ( )
  call pythag_triple_next_test ( )
  call r8_agm_test ( )
  call r8_choose_test ( )
  call r8_fall_test ( )
  call r8_rise_test ( )
  call r8_to_cfrac_test ( )
  call r8_to_dec_test ( )
  call r8_to_rat_test ( )
  call r8mat_det_test ( )
  call r8mat_perm1_test ( )
  call r8mat_2perm1_test ( )
  call r8mat_permanent_test ( )
  call r8poly_test ( )
  call r8poly_f2p_test ( )
  call r8poly_fval_test ( )
  call r8poly_n2p_test ( )
  call r8poly_nval_test ( )
  call r8poly_nx_test ( )
  call r8poly_p2f_test ( )
  call r8poly_p2n_test ( )
  call r8poly_p2t_test ( )
  call r8poly_print_test ( )
  call r8poly_pval_test ( )
  call r8poly_t2p_test ( )
  call r8vec_backtrack_test ( )
  call r8vec_frac_test ( )
  call r8vec_mirror_next_test ( )
  call rat_add_test ( )
  call rat_div_test ( )
  call rat_farey_test ( )
  call rat_farey2_test ( )
  call rat_mul_test ( )
  call rat_normalize_test ( )
  call rat_sum_formula_test ( )
  call rat_to_cfrac_test ( )
  call rat_to_dec_test ( )
  call rat_to_r8_test ( )
  call rat_to_s_test ( )
  call rat_width_test ( )
  call ratmat_det_test ( )
  call ratmat_print_test ( )
  call regro_next_test ( )
  call rfrac_to_cfrac_test ( )
  call rfrac_to_jfrac_test ( )
  call schroeder_test ( )
  call sort_heap_external_test ( )
  call subcomp_next_test ( )
  call subcompnz_next_test ( )
  call subcompnz2_next_test ( )
  call subset_by_size_next_test ( )
  call subset_gray_next_test ( )
  call subset_gray_rank_test ( )
  call subset_gray_unrank_test ( )
  call subset_lex_next_test ( )
  call subset_random_test ( )
  call subtriangle_next_test ( )
  call thue_binary_next_test ( )
  call thue_ternary_next_test ( )
  call triang_test ( )
  call tuple_next_test ( )
  call tuple_next_fast_test ( )
  call tuple_next_ge_test ( )
  call tuple_next2_test ( )
  call ubvec_add_test ( )
  call ubvec_print_test ( )
  call ubvec_to_ui4_test ( )
  call ubvec_xor_test ( )
  call ui4_to_ubvec_test ( )
  call vec_colex_next_test ( )
  call vec_colex_next2_test ( )
  call vec_colex_next3_test ( )
  call vec_gray_next_test ( )
  call vec_gray_rank_test ( )
  call vec_gray_unrank_test ( )
  call vec_lex_next_test ( )
  call vec_random_test ( )
  call vector_constrained_next_test ( )
  call vector_constrained_next2_test ( )
  call vector_constrained_next3_test ( )
  call vector_constrained_next4_test ( )
  call vector_constrained_next5_test ( )
  call vector_constrained_next6_test ( )
  call vector_constrained_next7_test ( )
  call vector_next_test ( )
  call ytb_enum_test ( )
  call ytb_next_test ( )
  call ytb_random_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop
end
subroutine asm_enum_test ( )

!*****************************************************************************80
!
!! ASM_ENUM_TEST tests ASM_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 7

  integer ( kind = 4 ) asm_num
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ASM_ENUM_TEST'
  write ( *, '(a)' ) '  ASM_ENUM returns the number of alternating sign'
  write ( *, '(a)' ) '  matrices of a given order.'

  write ( *, '(a)' ) ''
  do n = 0, n_max
    call asm_enum ( n, asm_num )
    write ( *, '(2x,i2,2x,i8)' ) n, asm_num
  end do

  return
end
subroutine asm_triangle_test ( )

!*****************************************************************************80
!
!! ASM_TRIANGLE_TEST tests ASM_TRIANGLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 7

  integer ( kind = 4 ) a(n_max+1)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ASM_TRIANGLE_TEST'
  write ( *, '(a)' ) '  ASM_TRIANGLE returns a row of the alternating sign'
  write ( *, '(a)' ) '  matrix triangle.'
  write ( *, '(a)' ) ''

  do n = 0, n_max
    call asm_triangle ( n, a )
    write ( *, '(2x,i2,2x,8i8)' ) n, a(1:n+1)
  end do

  return
end
subroutine bell_test ( )

!*****************************************************************************80
!
!! BELL_TEST tests BELL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) c
  integer ( kind = 4 ) c2(0:10)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BELL_TEST'
  write ( *, '(a)' ) '  BELL computes Bell numbers.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  N  exact C(I)  computed C(I)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bell_values ( n_data, n, c )

    if ( n_data == 0 ) then
      exit
    end if

    call bell ( n, c2 )

    write ( *, '(2x,i4,2i10)' ) n, c, c2(n)

  end do

  return
end
subroutine catalan_test ( )

!*****************************************************************************80
!
!! CATALAN_TEST tests CATALAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) c
  integer ( kind = 4 ) c2(0:10)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CATALAN_TEST'
  write ( *, '(a)' ) '  CATALAN computes Catalan numbers.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  N  exact C(I)  computed C(I)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call catalan_values ( n_data, n, c )

    if ( n_data == 0 ) then
      exit
    end if

    call catalan ( n, c2 )

    write ( *, '(2x,i4,2i8)' ) n, c, c2(n)

  end do

  return
end
subroutine catalan_row_next_test ( )

!*****************************************************************************80
!
!! CATALAN_ROW_NEXT_TEST tests CATALAN_ROW_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) c(0:n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ido

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CATALAN_ROW_NEXT_TEST'
  write ( *, '(a)' ) '  CATALAN_ROW_NEXT computes a row of Catalan''s triangle.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  First, compute row 7:'

  ido = 0
  i = 7
  call catalan_row_next ( ido, i, c )
  write ( *, '(2x,i2,2x,11i6)' ) i, c(0:i)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now compute rows one at a time:'
  write ( *, '(a)' ) ''

  ido = 0

  do i = 0, n
    call catalan_row_next ( ido, i, c )
    ido = 1
    write ( *, '(2x,i2,2x,11i6)' ) i, c(0:i)
  end do

  return
end
subroutine cfrac_to_rat_test ( )

!*****************************************************************************80
!
!! CFRAC_TO_RAT_TEST tests CFRAC_TO_RAT.
!
!  Discussion:
!
!    Compute the continued fraction form of 4096/15625.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 10

  integer ( kind = 4 ) a(m)
  integer ( kind = 4 ) bot
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) n
  integer ( kind = 4 ) p(m)
  integer ( kind = 4 ) q(m)
  integer ( kind = 4 ) top

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CFRAC_TO_RAT_TEST'
  write ( *, '(a)' ) '  CFRAC_TO_RAT continued fraction => fraction.'
  write ( *, '(a)' ) ''
  top = 4096
  bot = 15625
  write ( *, '(a,i8,a,i8)' ) '  Regular fraction is ', top, ' / ', bot
 
  call rat_to_cfrac ( top, bot, m, n, a, ierror )
 
  call i4vec_print ( n, a, '  Continued fraction coefficients:' )

  call cfrac_to_rat ( n, a, p, q )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The continued fraction convergents.'
  write ( *, '(a)' ) '  The last row contains the value of the continued'
  write ( *, '(a)' ) '  fraction, written as a common fraction.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, P(I), Q(I), P(I)/Q(I)'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,i3,2i8,g14.6)' ) i, p(i), q(i), &
      real ( p(i), kind = 8 ) / real ( q(i), kind = 8 )
  end do
 
  return
end
subroutine cfrac_to_rfrac_test ( )

!*****************************************************************************80
!
!! CFRAC_TO_RFRAC_TEST tests CFRAC_TO_RFRAC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxm = 10

  real ( kind = 8 ) g(2*maxm)
  real ( kind = 8 ) h(2*maxm)
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) m
  real ( kind = 8 ) p(maxm)
  real ( kind = 8 ) q(maxm+1)

  m = 3

  p(1:3) = (/ 1.0D+00, 1.0D+00, 2.0D+00 /)
  q(1:4) = (/ 1.0D+00, 3.0D+00, 1.0D+00, 1.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CFRAC_TO_RFRAC_TEST'
  write ( *, '(a)' ) '  CFRAC_TO_RFRAC: continued fraction to ratio.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rational polynomial fraction coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(a,5f12.4)' ) '  P:  ', p(1:m)
  write ( *, '(a,5f12.4)' ) '  Q:  ', q(1:m+1)
 
  call rfrac_to_cfrac ( m, p, q, h, ierror )
 
  call r8vec_print ( 2*m, h, '  Continued fraction coefficients:' )

  g(1:2*m) = 1.0D+00

  call cfrac_to_rfrac ( 2*m, g, h, p, q )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Recovered rational polynomial:'
  write ( *, '(a)' ) ''
  write ( *, '(a,5f12.4)' ) '  P:  ', p(1:m)
  write ( *, '(a,5f12.4)' ) '  Q:  ', q(1:m+1)
 
  return
end
subroutine change_greedy_test ( )

!*****************************************************************************80
!
!! CHANGE_GREEDY_TEST tests CHANGE_GREEDY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: coin_num = 6

  integer ( kind = 4 ) change(100)
  integer ( kind = 4 ) change_num
  integer ( kind = 4 ), dimension ( coin_num ) :: coin_value = (/ &
    1, 5, 10, 25, 50, 100 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) total
  integer ( kind = 4 ) total2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHANGE_GREEDY_TEST'
  write ( *, '(a)' ) '  CHANGE_GREEDY makes change using the biggest'
  write ( *, '(a)' ) '  coins first.'

  total = 73

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The total for which change is to be made: ', total
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The available coins are:'
  write ( *, '(a)' ) ''
  do i = 1, coin_num
    write ( *, '(2x,i8)' ) coin_value(i)
  end do

  call change_greedy ( total, coin_num, coin_value, change_num, change )

  write ( *, '(a)' ) ''
  write ( *, '(2x,i8,4x,(20i3))' ) change_num, change(1:change_num)
  total2 = sum ( coin_value(change(1:change_num) ) )
  write ( *, '(2x,i8,4x,(20i3))' ) total2, coin_value(change(1:change_num) )

  return
end
subroutine change_next_test ( )

!*****************************************************************************80
!
!! CHANGE_NEXT_TEST tests CHANGE_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: coin_num = 6

  integer ( kind = 4 ) change(100)
  integer ( kind = 4 ) change_num
  integer ( kind = 4 ), dimension ( coin_num ) :: coin_value = (/ 1, 5, 10, 25, 50, 100 /)
  logical done
  integer ( kind = 4 ) i
  integer ( kind = 4 ) total

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHANGE_NEXT_TEST'
  write ( *, '(a)' ) '  CHANGE_NEXT displays the next possible way to make'
  write ( *, '(a)' ) '  change for a given total'

  total = 50

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The total for which change is to be made: ', total
  write ( *, '(a)' ) ''

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The available coins are:'
  write ( *, '(a)' ) ''
  do i = 1, coin_num
    write ( *, '(2x,i8)' ) coin_value(i)
  end do

  done = .true.
  i = 0

  do

    call change_next ( total, coin_num, coin_value, change_num, change, done )

    if ( done .or. 9 < i ) then
      exit
    end if

    i = i + 1
    write ( *, '(a)' ) ''
    write ( *, '(i3, ":")' ) i
    write ( *, '(2x,25i3)' ) coin_value(change(1:change_num) )

  end do

  return
end
subroutine chinese_check_test ( )

!*****************************************************************************80
!
!! CHINESE_CHECK_TEST tests CHINESE_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) ierror
  integer ( kind = 4 ), dimension ( n ) :: m1 = (/ 1, 3,  8, 25 /)
  integer ( kind = 4 ), dimension ( n ) :: m2 = (/ 1, 3, -8, 25 /)
  integer ( kind = 4 ), dimension ( n ) :: m3 = (/ 1, 3,  1, 25 /)
  integer ( kind = 4 ), dimension ( n ) :: m4 = (/ 1, 3,  8, 24 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHINESE_CHECK_TEST'
  write ( *, '(a)' ) '  CHINESE_CHECK checks a set of moduluses for suitability'
  write ( *, '(a)' ) '  with the Chinese Remainder representation.'

  call i4vec_print ( n, m1, '  Modulus set #1:' )
  call chinese_check ( n, m1, ierror )
  write ( *, '(a,i4)' ) '  Error flag = ', ierror

  call i4vec_print ( n, m2, '  Modulus set #2:' )
  call chinese_check ( n, m2, ierror )
  write ( *, '(a,i4)' ) '  Error flag = ', ierror

  call i4vec_print ( n, m3, '  Modulus set #3:' )
  call chinese_check ( n, m3, ierror )
  write ( *, '(a,i4)' ) '  Error flag = ', ierror

  call i4vec_print ( n, m4, '  Modulus set #4:' )
  call chinese_check ( n, m4, ierror )
  write ( *, '(a,i4)' ) '  Error flag = ', ierror

  return
end
subroutine chinese_to_i4_test ( )

!*****************************************************************************80
!
!! CHINESE_TO_I4_TEST tests CHINESE_TO_I4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ), dimension ( n ) :: m = (/ 3, 4, 5, 7 /)
  integer ( kind = 4 ) r(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHINESE_TO_I4_TEST'
  write ( *, '(a)' ) '  CHINESE_TO_I4 computes an integer with the given'
  write ( *, '(a)' ) '  Chinese Remainder representation.'

  call i4vec_print ( n, m, '  The moduli:' )

  j = 37

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The number being analyzed is ', j

  call i4_to_chinese ( j, n, m, r )

  call i4vec_print ( n, r, '  The remainders:' )

  call chinese_to_i4 ( n, m, r, j2 )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The reconstructed number is ', j2

  call i4_to_chinese ( j2, n, m, r )

  call i4vec_print ( n, r, '  The remainders of the reconstructed number are:' )

  return
end
subroutine comb_next_test ( )

!*****************************************************************************80
!
!! COMB_NEXT_TEST tests COMB_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 April 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: a(:)
  logical done
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n

  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMB_NEXT_TEST'
  write ( *, '(a)' ) '  COMB_NEXT produces combinations.'

  do k = 1, n

    allocate ( a(1:k) )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Combinations of size K = ', k
    write ( *, '(a)' ) ''

    done = .true.

    do

      call comb_next ( n, k, a, done )
 
      if ( done ) then
        exit
      end if

      write ( *, '(2x,5i3)' ) a(1:k)

    end do

    deallocate ( a )

  end do

  return
end
subroutine comb_row_next_test ( )

!*****************************************************************************80
!
!! COMB_ROW_NEXT_TEST tests COMB_ROW_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  integer ( kind = 4 ) c(0:n_max)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMB_ROW_NEXT_TEST'
  write ( *, '(a)' ) '  COMB_ROW computes a row of Pascal''s triangle.'
  write ( *, '(a)' ) ''
 
  do n = 0, n_max
    call comb_row_next ( n, c )
    write ( *, '(2x,i2,2x,11i5)' ) n, c(0:n)
  end do
 
  return
end
subroutine comb_unrank_test ( )

!*****************************************************************************80
!
!! COMB_UNRANK_TEST tests COMB_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) cnk
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ), parameter :: m = 10
  integer ( kind = 4 ) rank

  cnk = i4_choose ( m, n )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMB_UNRANK_TEST'
  write ( *, '(a)' ) '  COMB_UNRANK returns a combination of N things'
  write ( *, '(a)' ) '  out of M, given the lexicographic rank.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The total set size is M = ', m
  write ( *, '(a,i8)' ) '  The subset size is N =    ', n
  write ( *, '(a,i8)' ) '  The number of combinations of N out of M is ', cnk
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   Rank	  Combination'
  write ( *, '(a)' ) ''
 
  do rank = 1, 3
    call comb_unrank ( m, n, rank, a )
    write ( *, '(2x,i3,3x,5i4)' ) rank, a(1:n)
  end do
 
  do rank = 6, 8
    call comb_unrank ( m, n, rank, a )
    write ( *, '(2x,i3,3x,5i4)' ) rank, a(1:n)
  end do
 
  do rank = 250, 252
    call comb_unrank ( m, n, rank, a )
    write ( *, '(2x,i3,3x,5i4)' ) rank, a(1:n)
  end do
 
  return
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
subroutine comp_next_test ( )

!*****************************************************************************80
!
!! COMP_NEXT_TEST tests COMP_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ih
  integer ( kind = 4 ) it
  logical more
  integer ( kind = 4 ) n

  n = 6
  more = .false.

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMP_NEXT_TEST'
  write ( *, '(a)' ) '  COMP_NEXT generates compositions.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Seeking all compositions of N = ', n
  write ( *, '(a,i8,a)' ) '  using ', k, ' parts.'

  i = 0

  do

    call comp_next ( n, k, a, more, ih, it )

    i = i + 1
    write ( *, '(2x,i4,2x,8i4)' ) i, a(1:k)

    if ( .not. more )  then
      exit
    end if

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
subroutine comp_random_test ( )

!*****************************************************************************80
!
!! COMP_RANDOM_TEST tests COMP_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 5

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ), parameter :: test_num = 5

  n = 10
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMP_RANDOM_TEST'
  write ( *, '(a)' ) '  COMP_RANDOM generates random compositions.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Seeking random compositions of N = ', n
  write ( *, '(a,i8,a)' ) '  using ', k, ' parts.'
  write ( *, '(a)' ) ''

  do i = 1, test_num
    call comp_random ( n, k, seed, a )
    write ( *, '(2x,8i4)' ) a(1:k)
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
subroutine comp_to_ksub_test ( )

!*****************************************************************************80
!
!! COMP_TO_KSUB_TEST tests COMP_TO_KSUB.
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

  integer ( kind = 4 ) ac(5)
  integer ( kind = 4 ) as(4)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) ks
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) ns
  integer ( kind = 4 ) seed
  
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMP_TO_KSUB_TEST'
  write ( *, '(a)' ) &
    '  COMP_TO_KSUB returns the K subset corresponding to a composition.'

  nc = 10
  kc = 5
  seed = 123456789

  do i = 1, 5

    write ( *, '(a)' ) ''

    call comp_random ( nc, kc, seed, ac )
    write ( *, '(a,5(i4))' ) '  COMP:', ac(1:kc)

    call comp_to_ksub ( nc, kc, ac, ns, ks, as )
    write ( *, '(a,4(i4))' ) '  KSUB:', as(1:ks)

    call ksub_to_comp ( ns, ks, as, nc, kc, ac )
    write ( *, '(a,5(i4))' ) '  COMP:', ac(1:kc)
    
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
subroutine compnz_enum_test ( )

!*****************************************************************************80
!
!! COMPNZ_ENUM_TEST tests COMPNZ_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  integer ( kind = 4 ) number

  n = 6
  k = 3
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMPNZ_ENUM_TEST'
  write ( *, '(a)' ) '  COMPNZ_ENUM enumerates compositions with nonzero parts.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Seeking all compositions of N = ', n
  write ( *, '(a,i8,a)' ) '  using ', k, ' nonzero parts.'

  call compnz_enum ( n, k, number )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The number of these compositions is ', number

  return
end
subroutine compnz_next_test ( )

!*****************************************************************************80
!
!! COMPNZ_NEXT_TEST tests COMPNZ_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) h
  integer ( kind = 4 ) i
  logical more
  integer ( kind = 4 ) n
  integer ( kind = 4 ) t

  n = 6
  more = .false.
  h = 0
  t = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMPNZ_NEXT_TEST'
  write ( *, '(a)' ) '  COMPNZ_NEXT generates compositions with nonzero parts.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Seeking all compositions of N = ', n
  write ( *, '(a,i8,a)' ) '  using ', k, ' nonzero parts.'

  i = 0

  do

    call compnz_next ( n, k, a, more, h, t )

    i = i + 1

    write ( *, '(2x,i4,2x,8i4)' ) i, a(1:k)

    if ( .not. more )  then
      exit
    end if

  end do
 
  return
end
subroutine compnz_random_test ( )

!*****************************************************************************80
!
!! COMPNZ_RANDOM_TEST tests COMPNZ_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 December 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 5

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ), parameter :: test_num = 5

  n = 10
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMPNZ_RANDOM_TEST'
  write ( *, '(a)' ) '  COMPNZ_RANDOM generates random compositions'
  write ( *, '(a)' ) '  with nonzero parts.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Seeking random compositions of N = ', n
  write ( *, '(a,i8,a)' ) '  using ', k, ' nonzero parts.'
  write ( *, '(a)' ) ''

  do i = 1, test_num
    call compnz_random ( n, k, seed, a )
    write ( *, '(2x,8i4)' ) a(1:k)
  end do
 
  return
end
subroutine compnz_to_ksub_test ( )

!*****************************************************************************80
!
!! COMPNZ_TO_KSUB_TEST tests COMPNZ_TO_KSUB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) ac(5)
  integer ( kind = 4 ) as(4)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) ks
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) ns
  integer ( kind = 4 ) seed
  
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMPNZ_TO_KSUB_TEST'
  write ( *, '(a)' ) '  COMPNZ_TO_KSUB returns the K subset corresponding '
  write ( *, '(a)' ) '  to a nonzero composition.'

  nc = 10
  kc = 5
  seed = 123456789

  do i = 1, 5

    write ( *, '(a)' ) ''

    call compnz_random ( nc, kc, seed, ac )
    write ( *, '(a,5(i4))' ) '  COMPNZ:', ac(1:kc)

    call compnz_to_ksub ( nc, kc, ac, ns, ks, as )
    write ( *, '(a,4(i4))' ) '  KSUB:  ', as(1:ks)

    call ksub_to_compnz ( ns, ks, as, nc, kc, ac )
    write ( *, '(a,5(i4))' ) '  COMPNZ:', ac(1:kc)
    
  end do

  return
end
subroutine congruence_test ( )

!*****************************************************************************80
!
!! CONGRUENCE_TEST tests CONGRUENCE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 21

  integer ( kind = 4 ) a
  integer ( kind = 4 ), dimension ( test_num ) :: a_test = (/ &
     1027,  1027,  1027,  1027, -1027, &
    -1027, -1027, -1027,     6,     0, &
        0,     0,     1,     1,     1, &
     1024,     0,     0,     5,     2, &
        7 /)
  integer ( kind = 4 ) b
  integer ( kind = 4 ), dimension ( test_num ) :: b_test = (/ &
      712,   712,  -712,  -712,   712, &
      712,  -712,  -712,     8,     0, &
        1,     1,     0,     0,     1, &
   -15625,     0,     3,     0,     4, &
       19 /)
  integer ( kind = 4 ) c
  integer ( kind = 4 ), dimension ( test_num ) :: c_test = (/ &
        7,    -7,     7,    -7,     7, &
       -7,     7,    -7,    50,     0, &
        0,     1,     0,     1,     0, &
    11529,     1,    11,    19,     7, &
        1 /)
  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) result
  integer ( kind = 4 ) test
  integer ( kind = 4 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CONGRUENCE_TEST'
  write ( *, '(a)' ) '  CONGRUENCE solves a congruence equation:'
  write ( *, '(a)' ) '    A * X = C mod ( B )'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '   I        A         B         C         X     Mod ( A*X-C,B)'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    a = a_test(test)
    b = b_test(test)
    c = c_test(test)

    call congruence ( a, b, c, ierror, x )

    if ( ierror /= 0 ) then
      write ( *, '(2x,i2,2x,3i10,a,i10)' ) test, a, b, c, &
        ' Error code = ', ierror
    else
      if ( b /= 0 ) then
        result = i4_modp ( a * x - c, b )
      else
        result = 0
      end if
      write ( *, '(2x,i2,2x,5i10)' ) test, a, b, c, x, result
    end if

  end do

  return
end
subroutine count_pose_random_test ( )

!*****************************************************************************80
!
!! COUNT_POSE_RANDOM_TEST tests COUNT_POSE_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) blocks(6)
  integer ( kind = 4 ) goal
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COUNT_POSE_RANDOM_TEST'
  write ( *, '(a)' ) '  COUNT_POSE_RANDOM poses a random problem for '
  write ( *, '(a)' ) '  the game "The Count is Good".'

  seed = 123456789

  do i = 1, 5

    call count_pose_random ( seed, blocks, goal )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Problem #', i
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '    The goal number:'
    write ( *, '(a)' ) ''
    write ( *, '(6x,i8)' ) goal
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '    The available numbers are '
    write ( *, '(a)' ) ''
    write ( *, '(6x,6i4)' ) blocks(1:6)

  end do

  return
end
subroutine debruijn_test ( )

!*****************************************************************************80
!
!! DEBRUIJN_TEST tests DEBRUIJN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 3

  integer ( kind = 4 ) m
  integer ( kind = 4 ), parameter, dimension ( test_num ) :: m_test = (/ &
    2, 3, 2 /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter, dimension ( test_num ) :: n_test = (/ &
    3, 3, 4 /)
  integer ( kind = 4 ), dimension (27) :: string
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DEBRUIJN_TEST'
  write ( *, '(a)' ) '  DEBRUIJN computes a de Bruijn string.'

  do test = 1, test_num

    m = m_test(test)
    n = n_test(test)

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  The alphabet size is M = ', m
    write ( *, '(a,i8)' ) '  The string length is N = ', n

    call debruijn ( m, n, string )

    write ( *, '(a)' ) ''
    write ( *, '(4x,80i1)' ) string(1:m**n)

  end do

  return
end
subroutine dec_add_test ( )

!*****************************************************************************80
!
!! DEC_ADD_TEST tests DEC_ADD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) abot
  integer ( kind = 4 ) atop
  integer ( kind = 4 ) bbot
  integer ( kind = 4 ) btop
  integer ( kind = 4 ) cbot
  integer ( kind = 4 ) ctop
  integer ( kind = 4 ) dec_digit
  integer ( kind = 4 ) ierror
  character ( len = 15 ) string

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DEC_ADD_TEST'
  write ( *, '(a)' ) '  DEC_ADD adds two decimals.'

  dec_digit = 3

  atop = 128
  abot = - 1
  btop = 438
  bbot = - 2

  call dec_add ( atop, abot, btop, bbot, dec_digit, ctop, cbot, ierror )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of decimal places is ', dec_digit
  write ( *, '(a)' ) ''
  call dec_to_s ( atop, abot, string )
  write ( *, '(a)' ) '  A = ' // trim ( string )
  call dec_to_s ( btop, bbot, string )
  write ( *, '(a)' ) '  B = ' // trim ( string )
  call dec_to_s ( ctop, cbot, string )
  write ( *, '(a)' ) '  C = ' // trim ( string )
 
  return
end
subroutine dec_div_test ( )

!*****************************************************************************80
!
!! DEC_DIV_TEST tests DEC_DIV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) abot
  integer ( kind = 4 ) atop
  integer ( kind = 4 ) bbot
  integer ( kind = 4 ) btop
  integer ( kind = 4 ) cbot
  integer ( kind = 4 ) ctop
  integer ( kind = 4 ) dec_digit
  integer ( kind = 4 ) ierror
  character ( len = 15 ) string

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DEC_DIV_TEST'
  write ( *, '(a)' ) '  DEC_DIV divides two decimals.'

  dec_digit = 3

  atop = 523
  abot = -1
  btop = 134
  bbot = 2

  call dec_div ( atop, abot, btop, bbot, dec_digit, ctop, cbot, ierror )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of decimal places is ', dec_digit
  write ( *, '(a)' ) ''
  call dec_to_s ( atop, abot, string )
  write ( *, '(a)' ) '  A = ' // trim ( string )
  call dec_to_s ( btop, bbot, string )
  write ( *, '(a)' ) '  B = ' // trim ( string )
  call dec_to_s ( ctop, cbot, string )
  write ( *, '(a)' ) '  C = ' // trim ( string )
 
  return
end
subroutine dec_mul_test ( )

!*****************************************************************************80
!
!! DEC_MUL_TEST tests DEC_MUL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) abot
  integer ( kind = 4 ) atop
  integer ( kind = 4 ) bbot
  integer ( kind = 4 ) btop
  integer ( kind = 4 ) cbot
  integer ( kind = 4 ) ctop
  integer ( kind = 4 ) dec_digit
  character ( len = 15 ) string

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DEC_MUL_TEST'
  write ( *, '(a)' ) '  DEC_MUL multiplies two decimals.'

  dec_digit = 2

  atop = 14
  abot = - 4
  btop = 16
  bbot = 2

  call dec_mul ( atop, abot, btop, bbot, dec_digit, ctop, cbot )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of decimal places is ', dec_digit
  write ( *, '(a)' ) ''
  call dec_to_s ( atop, abot, string )
  write ( *, '(a)' ) '  A = ' // trim ( string )
  call dec_to_s ( btop, bbot, string )
  write ( *, '(a)' ) '  B = ' // trim ( string )
  call dec_to_s ( ctop, cbot, string )
  write ( *, '(a)' ) '  C = ' // trim ( string )
 
  return
end
subroutine dec_round_test ( )

!*****************************************************************************80
!
!! DEC_ROUND_TEST tests DEC_ROUND.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 7

  integer ( kind = 4 ), dimension ( test_num ) :: d_test = (/ & 
      1, 2, 3, 4, 2, 3, 4 /)
  integer ( kind = 4 ) dec_digit
  integer ( kind = 4 ) exponent
  integer ( kind = 4 ), dimension ( test_num ) :: exponent_test = (/ &
     -1,  -1, -1, -1, 2, 2, 2 /)
  integer ( kind = 4 ) mantissa
  integer ( kind = 4 ), dimension ( test_num ) :: mantissa_test = (/ &
    523, 523, 523, 523, 6340, 6340, 6340 /)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DEC_ROUND_TEST'
  write ( *, '(a)' ) '  DEC_ROUND "rounds" a decimal to a number of digits.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '           -----Before-------  -----After--------'
  write ( *, '(a)' ) '  Digits   Mantissa  Exponent  Mantissa  Exponent'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    dec_digit = d_test(test)

    mantissa = mantissa_test(test)
    exponent = exponent_test(test)

    call dec_round ( mantissa, exponent, dec_digit, mantissa, exponent )

    write ( *, '(2x,3i8,4x,2i8)' ) &
      dec_digit, mantissa_test(test), exponent_test(test), mantissa, exponent

  end do

  return
end
subroutine dec_to_r8_test ( )

!*****************************************************************************80
!
!! DEC_TO_R8_TEST tests DEC_TO_R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) dec_digit
  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DEC_TO_R8_TEST'
  write ( *, '(a)' ) '  DEC_TO_R8 converts a decimal to a real number.'

  dec_digit = 5

  write ( *, '(a)' ) ''
  write ( *, '(a,i3)' ) '  The number of decimal digits is ', dec_digit

  r8_lo = -10.0D+00
  r8_hi = +10.0D+00
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     R   =>  A * 10^B  =>  R2'
  write ( *, '(a)' ) ''

  do i = 1, 10
    r = r8_uniform_ab ( r8_lo, r8_hi, seed )
    call r8_to_dec ( r, dec_digit, a, b )
    call dec_to_r8 ( a, b, r2 )
    write ( *, '(2x,f10.6,2x,i8,2x,i8,2x,f10.6)' ) r, a, b, r2
  end do

  return
end
subroutine dec_to_rat_test ( )

!*****************************************************************************80
!
!! DEC_TO_RAT_TEST tests DEC_TO_RAT.
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

  integer ( kind = 4 ) exponent
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) mantissa
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) r3
  integer ( kind = 4 ) rat_bot
  integer ( kind = 4 ) rat_bot2
  integer ( kind = 4 ) rat_top
  integer ( kind = 4 ) rat_top2
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DEC_TO_RAT_TEST'
  write ( *, '(a)' ) '  DEC_TO_RAT decimal => fraction.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  In this test, choose the top and bottom'
  write ( *, '(a)' ) '  of a rational at random, and compute the'
  write ( *, '(a)' ) '  equivalent real number.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Then convert to decimal, and the equivalent real.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Then convert back to rational and the equivalent real.'
  
  seed = 123456789

  do i = 1, 10

    rat_top = i4_uniform_ab ( -1000, 1000, seed )

    rat_bot = i4_uniform_ab (     1, 1000, seed )

    r1 = real ( rat_top, kind = 8 ) / real ( rat_bot, kind = 8 )

    call rat_to_dec ( rat_top, rat_bot, mantissa, exponent )

    r2 = real ( mantissa, kind = 8 ) * 10.0D+00**( exponent )
 
    call dec_to_rat ( mantissa, exponent, rat_top2, rat_bot2 )
    r3 = real ( rat_top2, kind = 8 ) / real ( rat_bot2, kind = 8 )

    write ( *, '(a)' ) ''
    write ( *, '(2x,f10.6,a,i12,a,i12)' ) r1, '=', rat_top, '/', rat_bot
    write ( *, '(2x,f10.6,a,i12,a,i12)' ) r2, '=', mantissa, '*10^', exponent
    write ( *, '(2x,f10.6,a,i12,a,i12)' ) r3, '=', rat_top2, '/', rat_bot2

  end do
 
  return
end
subroutine dec_to_s_test ( )

!*****************************************************************************80
!
!! DEC_TO_S_TEST tests DEC_TO_S.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) exponent
  integer ( kind = 4 ) mantissa
  character ( len = 100 ) s

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DEC_TO_S_TEST'
  write ( *, '(a)' ) '  DEC_TO_S prints a decimal value.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Mantissa  Exponent  String'
  write ( *, '(a)' ) ''

  mantissa = 523
  exponent = -1
  call dec_to_s ( mantissa, exponent, s )
  write ( *, '(2x,i8,2x,i8,2x,a)' ) mantissa, exponent, trim ( s )

  mantissa = 134
  exponent = 2
  call dec_to_s ( mantissa, exponent, s )
  write ( *, '(2x,i8,2x,i8,2x,a)' ) mantissa, exponent, trim ( s )

  mantissa = -134
  exponent = 2
  call dec_to_s ( mantissa, exponent, s )
  write ( *, '(2x,i8,2x,i8,2x,a)' ) mantissa, exponent, trim ( s )

  mantissa = 0
  exponent = 10
  call dec_to_s ( mantissa, exponent, s )
  write ( *, '(2x,i8,2x,i8,2x,a)' ) mantissa, exponent, trim ( s )

  do exponent = -8, 3
    mantissa = 123456
    call dec_to_s ( mantissa, exponent, s )
    write ( *, '(2x,i8,2x,i8,2x,a)' ) mantissa, exponent, trim ( s )
  end do

  return
end
subroutine dec_width_test ( )

!*****************************************************************************80
!
!! DEC_WIDTH_TEST tests DEC_WIDTH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) dec_width
  integer ( kind = 4 ) exponent
  integer ( kind = 4 ) i
  integer ( kind = 4 ) mantissa

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DEC_WIDTH_TEST'
  write ( *, '(a)' ) '  DEC_WIDTH determines the "width" of a decimal.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Mantissa  Exponent  Width'
  write ( *, '(a)' ) ''

  mantissa = 523
  exponent = -1
  i = dec_width ( mantissa, exponent )
  write ( *, '(2x,i8,2x,i8,2x,i8)' ) mantissa, exponent, i

  mantissa = 134
  exponent = 2
  i = dec_width ( mantissa, exponent )
  write ( *, '(2x,i8,2x,i8,2x,i8)' ) mantissa, exponent, i

  mantissa = -134
  exponent = 2
  i = dec_width ( mantissa, exponent )
  write ( *, '(2x,i8,2x,i8,2x,i8)' ) mantissa, exponent, i

  mantissa = 0
  exponent = 10
  i = dec_width ( mantissa, exponent )
  write ( *, '(2x,i8,2x,i8,2x,i8)' ) mantissa, exponent, i

  do exponent = -8, 3
    mantissa = 123456
    i = dec_width ( mantissa, exponent )
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) mantissa, exponent, i
  end do

  return
end
subroutine decmat_det_test ( )

!*****************************************************************************80
!
!! DECMAT_DET_TEST tests DECMAT_DET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n3 = 3
  integer ( kind = 4 ), parameter :: n4 = 4

  integer ( kind = 4 ) dec_digit
  integer ( kind = 4 ) i
  integer ( kind = 4 ) a3(n3,n3)
  integer ( kind = 4 ) a4(n4,n4)
  integer ( kind = 4 ) b3(n3,n3)
  integer ( kind = 4 ) b4(n4,n4)
  integer ( kind = 4 ) dbot
  integer ( kind = 4 ) dtop
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DECMAT_DET_TEST'
  write ( *, '(a)' ) '  DECMAT_DET: determinant of a decimal matrix.'
  write ( *, '(a)' ) ''
 
  dec_digit = 7

  k = 0
  do i = 1, n3
    do j = 1, n3
      k = k + 1
      a3(i,j) = k
    end do
  end do

  b3(1:n3,1:n3) = 0
 
  call decmat_print ( n3, n3, a3, b3, '  The 123/456/789 matrix:' )

  call decmat_det ( n3, a3, b3, dec_digit, dtop, dbot, ierror )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Determinant of the 123/456/789 matrix'
  write ( *, '(2x,i8,a,i8)' ) dtop, ' * 10** ', dbot
 
  do i = 1, n4
    do j = 1, n4
      r = 1.0D+00 / real ( i + j, kind = 8 )
      call r8_to_dec ( r, dec_digit, a4(i,j), b4(i,j) )
    end do
  end do
 
  call decmat_print ( n4, n4, a4, b4, '  The Hilbert matrix:' )

  call decmat_det ( n4, a4, b4, dec_digit, dtop, dbot, ierror )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Determinant of the Hilbert matrix:'
  write ( *, '(2x,i8,a,i8)' ) dtop, ' * 10 ** ', dbot 

  do i = 1, n3
    do j = 1, n3
      if ( i == j ) then
        a3(i,j) = 2
      else if ( i == j + 1 .or. i == j - 1 ) then
        a3(i,j) = -1
      else
        a3(i,j) = 0
      end if
      b3(i,j) = 0
    end do
  end do
 
  call decmat_print ( n3, n3, a3, b3, '  The -1,2,-1 matrix:' )

  call decmat_det ( n3, a3, b3, dec_digit, dtop, dbot, ierror )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Determinant of the -1,2,-1 matrix:'
  write ( *, '(2x,i8,a,i8)' ) dtop, ' * 10 ** ', dbot
 
  return
end
subroutine decmat_print_test ( )

!*****************************************************************************80
!
!! DECMAT_PRINT_TEST tests DECMAT_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(m,n)
  integer ( kind = 4 ) dec_digit
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DECMAT_PRINT_TEST'
  write ( *, '(a)' ) '  DECMAT_PRINT prints a decimal matrix.'

  dec_digit = 7

  do i = 1, m
    do j = 1, n
      r = 1.0D+00 / real ( i + j, kind = 8 )
      call r8_to_dec ( r, dec_digit, a(i,j), b(i,j) )
    end do
  end do
 
  call decmat_print ( m, n, a, b, '  The Hilbert matrix:' )
 
  return
end
subroutine derange_enum_test ( )

!*****************************************************************************80
!
!! DERANGE_ENUM_TEST tests DERANGE_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) derange_enum
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DERANGE_ENUM_TEST'
  write ( *, '(a)' ) '  DERANGE_ENUM counts derangements;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  N    # of derangements'
  write ( *, '(a)' ) ''

  do i = 0, n
    write ( *, '(2x,i8,2x,3i10)' ) i, derange_enum(i)
  end do

  return
end
subroutine derange_enum2_test ( )

!*****************************************************************************80
!
!! DERANGE_ENUM2 tests DERANGE_ENUM2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) d(0:n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DERANGE_ENUM2_TEST'
  write ( *, '(a)' ) '  DERANGE_ENUM2 counts derangements.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  N    # of derangements'
  write ( *, '(a)' ) ''

  call derange_enum2 ( n, d )

  do i = 0, n
    write ( *, '(2x,i8,2x,i10)' ) i, d(i)
  end do

  return
end
subroutine derange_enum3_test ( )

!*****************************************************************************80
!
!! DERANGE_ENUM3_TEST tests DERANGE_ENUM3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) derange_enum3
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DERANGE_ENUM3_TEST'
  write ( *, '(a)' ) '  DERANGE_ENUM3 counts derangements.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  N    # of derangements'
  write ( *, '(a)' ) ''

  do i = 0, n
    write ( *, '(2x,i8,2x,i10)' ) i, derange_enum3(i)
  end do

  return
end
subroutine derange1_back_next_test ( )

!*****************************************************************************80
!
!! DERANGE1_BACK_NEXT_TEST tests DERANGE1_BACK_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  logical more
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DERANGE1_BACK_NEXT_TEST'
  write ( *, '(a)' ) '  DERANGE1_BACK_NEXT generates derangements'
  write ( *, '(a)' ) '  using backtracking.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Here, we seek all derangments of order N = ', n
  write ( *, '(a)' ) ''

  more = .false.
  rank = 0

  do

    call derange1_back_next ( n, a, more )

    if ( .not. more ) then
      exit
    end if

    rank = rank + 1
    write ( *, '(2x,i4,4x,8i4)' ) rank, a(1:n)

  end do

  return
end
subroutine derange1_check_test ( )

!*****************************************************************************80
!
!! DERANGE1_CHECK_TEST tests DERANGE1_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ), dimension ( n, 5 ) :: a_test = reshape ( (/ &
    2, 3, 4, 5, 1, &
    2, 5, 3, 1, 4, &
    2, 3, 4, 1, 4, &
    0, 3, 4, 5, 1, &
    1, 4, 9, 2, 3  &
    /), (/ n, 5 /) )
  logical check
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DERANGE1_CHECK_TEST'
  write ( *, '(a)' ) '  DERANGE1_CHECK_checks whether a vector of N objects'
  write ( *, '(a)' ) '  represents a derangement of (1,...,N).'
 
  do j = 1, 5
 
    do i = 1, n
      a(i) = a_test(i,j)
    end do

    call i4vec_transpose_print ( n, a, '  Potential derangement:' )
    call derange1_check ( n, a, check )
    write ( *, '(a,l1)' ) '  CHECK = ', check

  end do

  return
end
subroutine derange1_weed_next_test ( )

!*****************************************************************************80
!
!! DERANGE1_WEED_NEXT_TEST tests DERANGE1_WEED_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) maxder
  logical more
  integer ( kind = 4 ) numder

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DERANGE1_WEED_NEXT_TEST'
  write ( *, '(a)' ) '  DERANGE1_WEED_NEXT generates derangements of (1,...,N)'
  write ( *, '(a)' ) '  by generating ALL permutations, and "weeding out"'
  write ( *, '(a)' ) '  the ones that are not derangements.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Here, we seek all derangements of order N = ', n
  write ( *, '(a)' ) ''

  more = .false.
  i = 0
 
  do

    call derange1_weed_next ( n, a, more, maxder, numder )

    i = i + 1
    write ( *, '(2x,i4,4x,8i4)' ) i, a(1:n)

    if ( .not. more ) then
      exit
    end if
 
  end do

  return
end
subroutine digraph_arc_euler_test ( )

!*****************************************************************************80
!
!! DIGRAPH_ARC_EULER_TEST calls DIGRAPH_ARC_EULER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nedge = 7
  integer ( kind = 4 ), parameter :: nnode = 5

  integer ( kind = 4 ) i
  integer ( kind = 4 ) in
  integer ( kind = 4 ), dimension ( nedge ) :: inode = (/ 2, 1, 2, 1, 3, 5, 4 /)
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( nedge ) :: jnode = (/ 5, 4, 3, 2, 1, 1, 2 /)
  integer ( kind = 4 ) jp1
  logical success
  integer ( kind = 4 ) trail(nedge)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DIGRAPH_ARC_EULER_TEST'
  write ( *, '(a)' ) '  DIGRAPH_ARC_EULER finds an Euler circuit of a digraph.'

  call digraph_arc_print ( nedge, inode, jnode, &
    '  The arc list of the digraph:' )

  call digraph_arc_euler ( nnode, nedge, inode, jnode, success, trail )

  if ( success ) then

    call i4vec_print ( nedge, trail, '  The edge list of the Euler circuit:' )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  The node list of the Euler circuit:'
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '    I  Edge  Node'
    write ( *, '(a)' ) ''

    do i = 1, nedge

      j = trail(i)

      if ( i == nedge ) then
        jp1 = trail(1)
      else
        jp1 = trail(i+1)
      end if

      if ( jnode(j) == inode(jp1) ) then
        in = jnode(j)
      else
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'The circuit has failed!'
        exit
      end if

      write ( *, '(2x,3i8)' ) i, j, in

    end do

  else

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  The digraph is not eulerian.'
    write ( *, '(a)' ) ''

  end if

  return
end
subroutine digraph_arc_print_test ( )

!*****************************************************************************80
!
!! DIGRAPH_ARC_PRINT_TEST calls DIGRAPH_ARC_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nedge = 7
  integer ( kind = 4 ), parameter :: nnode = 5

  integer ( kind = 4 ), dimension ( nedge ) :: inode = (/ 2, 1, 2, 1, 3, 5, 4 /)
  integer ( kind = 4 ), dimension ( nedge ) :: jnode = (/ 5, 4, 3, 2, 1, 1, 2 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DIGRAPH_ARC_PRINT_TEST'
  write ( *, '(a)' ) '  DIGRAPH_ARC_PRINT prints a digraph.'

  call digraph_arc_print ( nedge, inode, jnode, &
    '  The arc list of the digraph:' )

  return
end
subroutine diophantine_test ( )

!*****************************************************************************80
!
!! DIOPHANTINE_TEST tests DIOPHANTINE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 20

  integer ( kind = 4 ) a
  integer ( kind = 4 ), dimension ( test_num ) :: a_test = (/ &
     1027,  1027,  1027, 1027, -1027, &
    -1027, -1027, -1027,    6,     0, &
        0,     0,     1,    1,     1, &
     1024,     0,     0,    5,     2 /)
  integer ( kind = 4 ) b
  integer ( kind = 4 ), dimension ( test_num) ::  b_test = (/ &
       712,   712, -712, -712, 712, &
       712,  -712, -712,    8,   0, &
         1,     1,    0,    0,   1, &
    -15625,     0,    3,    0,   4 /)
  integer ( kind = 4 ) c
  integer ( kind = 4 ), dimension ( test_num) ::  c_test = (/ &
         7,    -7,    7,   -7,   7, &
        -7,     7,   -7,   50,   0, &
         0,     1,    0,    1,   0, &
     11529,     1,   11,   19,   7 /)
  integer ( kind = 4 ) error
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) test
  integer ( kind = 4 ) x
  integer ( kind = 4 ) y

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DIOPHANTINE_TEST'
  write ( *, '(a)' ) '  DIOPHANTINE solves a Diophantine equation:'
  write ( *, '(a)' ) '    A * X + B * Y = C'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        A         B         C         X     Y     Error'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    a = a_test(test)
    b = b_test(test)
    c = c_test(test)

    call diophantine ( a, b, c, ierror, x, y )

    if ( ierror /= 0 ) then
      write ( *, '(2x,3i10,a,i10)' ) a, b, c, ' Error code = ', ierror
    else
      error = a * x + b * y - c
      write ( *, '(2x,6i10)' ) a, b, c, x, y, error
    end if

  end do

  return
end
subroutine diophantine_solution_minimize_test ( )

!*****************************************************************************80
!
!! DIOPHANTINE_SOLUTION_MINIMIZE_TEST tests DIOPHANTINE_SOLUTION_MINIMIZE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) r
  integer ( kind = 4 ) x
  integer ( kind = 4 ) y

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DIOPHANTINE_SOLUTION_MINIMIZE_TEST'
  write ( *, '(a)' ) '  DIOPHANTINE_SOLUTION_MINIMIZE computes a minimal'
  write ( *, '(a)' ) '  Euclidean norm solution of a Diophantine equation:'
  write ( *, '(a)' ) '    A * X + B * Y = C'

  a = 4096
  b = - 15625
  c = 46116
  x = 665499996
  y = 174456828

  r = a * x + b * y - c

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Coefficients:'
  write ( *, '(a,i12)' ) '    A = ', a
  write ( *, '(a,i12)' ) '    B = ', b
  write ( *, '(a,i12)' ) '    C = ', c
  write ( *, '(a)' ) '  Solution:'
  write ( *, '(a,i12)' ) '    X = ', x
  write ( *, '(a,i12)' ) '    Y = ', y
  write ( *, '(a)' ) '  Residual R = A * X + B * Y - C:'
  write ( *, '(a,i12)' ) '    R = ', r

  call diophantine_solution_minimize ( a, b, x, y )

  r = a * x + b * y - c

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The minimized solution:'
  write ( *, '(a,i12)' ) '    X = ', x
  write ( *, '(a,i12)' ) '    Y = ', y
  write ( *, '(a)' ) '  Residual R = A * X + B * Y - C:'
  write ( *, '(a,i12)' ) '    R = ', r

  x = 15621
  y = 4092

  r = a * x + b * y - c

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The minimal positive solution:'
  write ( *, '(a,i12)' ) '    X = ', x
  write ( *, '(a,i12)' ) '    Y = ', y
  write ( *, '(a)' ) '  Residual R = A * X + B * Y - C:'
  write ( *, '(a,i12)' ) '    R = ', r

  return
end
subroutine dvec_add_test ( )

!*****************************************************************************80
!
!! DVEC_ADD_TEST tests DVEC_ADDB;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) dvec1(n)
  integer ( kind = 4 ) dvec2(n)
  integer ( kind = 4 ) dvec3(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DVEC_ADD_TEST'
  write ( *, '(a)' ) '  DVEC_ADD adds decimal vectors representing integers;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        I        J        K = I + J'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    
    i = i4_uniform_ab ( -100, 100, seed )
    j = i4_uniform_ab ( -100, 100, seed )

    write ( *, '(a)' ) ''

    write ( *, '(2x,i8,2x,i8)' ) i, j

    k = i + j
    l = i - j

    write ( *, '(a20,2x,i8)' ) '  Directly:         ', k

    call i4_to_dvec ( i, n, dvec1 )
    call i4_to_dvec ( j, n, dvec2 )

    call dvec_add ( n, dvec1, dvec2, dvec3 )
    call dvec_to_i4 ( n, dvec3, k )

    write ( *, '(a20,2x,i8)' ) '  DVEC_ADD', k

  end do

  return
end
subroutine dvec_complementx_test ( )

!*****************************************************************************80
!
!! DVEC_COMPLEMENTX_TEST tests DVEC_COMPLEMENTX;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) dvec1(n)
  integer ( kind = 4 ) dvec2(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DVEC_COMPLEMENTX_TEST'
  write ( *, '(a)' ) '  DVEC_COMPLEMENTX returns the ten''s complement'
  write ( *, '(a)' ) '  of a (signed) decimal vector;'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    
    i = i4_uniform_ab ( -100, 100, seed )

    call i4_to_dvec ( i, n, dvec1 )

    call dvec_complementx ( n, dvec1, dvec2 )

    call dvec_to_i4 ( n, dvec2, j )

    write ( *, '(a)' ) ''
    write ( *, '(a,2x,i8)' ) '  I = ', i
    write ( *, '(a,2x,i8)' ) '  J = ', j
    call dvec_print ( n, dvec1, '' )
    call dvec_print ( n, dvec2, '' )

  end do

  return
end
subroutine dvec_mul_test ( )

!*****************************************************************************80
!
!! DVEC_MUL_TEST tests DVEC_MUL;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) dvec1(n)
  integer ( kind = 4 ) dvec2(n)
  integer ( kind = 4 ) dvec3(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ) test2
  integer ( kind = 4 ), parameter :: test_num = 10
  integer ( kind = 4 ), parameter :: test2_num = 2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DVEC_MUL_TEST'
  write ( *, '(a)' ) '  DVEC_MUL multiplies decimal vectors '
  write ( *, '(a)' ) '  representing integers;'

  do test2 = 1, test2_num

    if ( test2 == 1 ) then

      n2 = n

    else if ( test2 == 2 ) then

      n2 = 6

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  NOW REPEAT THE TEST...'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  but use too few digits to represent big products.'
      write ( *, '(a)' ) '  This corresponds to an "overflow".'
      write ( *, '(a)' ) '  The result here should get the final decimal'
      write ( *, '(a)' ) '  digits correctly, though.'
    end if

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '        I        J        K = I * J'
    write ( *, '(a)' ) ''

    do test = 1, test_num
      
      i = i4_uniform_ab ( -1000, 1000, seed )
      j = i4_uniform_ab ( -1000, 1000, seed )

      write ( *, '(a)' ) ''

      write ( *, '(2x,i8,2x,i8)' ) i, j

      k = i * j

      write ( *, '(a20,2x,i8)' ) '  Directly:         ', k

      call i4_to_dvec ( i, n2, dvec1 )
      call i4_to_dvec ( j, n2, dvec2 )

      call dvec_mul ( n2, dvec1, dvec2, dvec3 )
      call dvec_to_i4 ( n2, dvec3, k )

      write ( *, '(a20,2x,i8)' ) '  DVEC_MUL          ', k

    end do

  end do

  return
end
subroutine dvec_print_test ( )

!*****************************************************************************80
!
!! DVEC_PRINT_TEST tests DVEC_PRINT;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), dimension ( 20 ) :: dvec = (/ &
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, &
    3, 4, 1, 7, 7, 5, 5, 0, 0, 9 /)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DVEC_PRINT_TEST'
  write ( *, '(a)' ) '  DVEC_PRINT prints a (signed) decimal vector;'

  n = 20
  call dvec_print ( n, dvec, '  The DVEC:' )

 return
end
subroutine dvec_sub_test ( )

!*****************************************************************************80
!
!! DVEC_SUB_TEST tests DVEC_SUB;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) dvec1(n)
  integer ( kind = 4 ) dvec2(n)
  integer ( kind = 4 ) dvec4(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DVEC_SUB_TEST'
  write ( *, '(a)' ) '  DVEC_SUB subtracts decimal vectors representing integers;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        I        J        L = I - J'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    
    i = i4_uniform_ab ( -100, 100, seed )
    j = i4_uniform_ab ( -100, 100, seed )

    write ( *, '(a)' ) ''

    write ( *, '(2x,i8,2x,i8)' ) i, j

    l = i - j

    write ( *, '(a20,2x,i8)' ) '  Directly:         ', l

    call i4_to_dvec ( i, n, dvec1 )
    call i4_to_dvec ( j, n, dvec2 )

    call dvec_sub ( n, dvec1, dvec2, dvec4 )
    call dvec_to_i4 ( n, dvec4, l )

    write ( *, '(a20,2x,i8,2x,i8)' ) '  DVEC_SUB', l

  end do

  return
end
subroutine dvec_to_i4_test ( )

!*****************************************************************************80
!
!! DVEC_TO_I4_TEST tests DVEC_TO_I4;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ) dvec(n)
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DVEC_TO_I4_TEST'
  write ( *, '(a)' ) '  DVEC_TO_I4 converts a DVEC to an I4'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        I4 => DVEC => I4'
  write ( *, '(a)' ) ''

  seed = 123456789
  i1 = i4_uniform_ab ( -10000, 10000, seed )
  call i4_to_dvec ( i1, n, dvec )
  call dvec_to_i4 ( n, dvec, i2 )

  write ( *, '(2x,i6,2x,6i2,2x,i6)' ) i1, dvec(n:1:-1), i2

  return
end
subroutine equiv_next_test ( )

!*****************************************************************************80
!
!! EQUIV_NEXT_TEST tests EQUIV_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) jarray(n)
  logical more
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EQUIV_NEXT_TEST'
  write ( *, '(a)' ) '  EQUIV_NEXT generates all partitions of a set.'
  write ( *, '(a)' ) ''
  write ( *, '(a,6i4)' ) '  Rank/element:', ( i, i = 1, n )
  write ( *, '(a)' ) ''
 
  rank = 0
  more = .false.
 
  do
 
    call equiv_next ( n, nc, jarray, a, more )
 
    rank = rank + 1
    write ( *, '(2x,15i4)' ) rank, a(1:n)
 
    if ( .not. more ) then
      exit
    end if

  end do

  return
end
subroutine equiv_next2_test ( )

!*****************************************************************************80
!
!! EQUIV_NEXT2_TEST tests EQUIV_NEXT2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(n)
  logical done
  integer ( kind = 4 ) i
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EQUIV_NEXT2_TEST'
  write ( *, '(a)' ) '  EQUIV_NEXT2 generates all partitions of a set.'
  write ( *, '(a,i8)' ) '  Here, N = ', n
  rank = 0
  done = .true.
  write ( *, '(a)' ) ''
  write ( *, '(a,6i4)' ) '  Rank/element:', ( i, i = 1, n )
  write ( *, '(a)' ) ''
 
  do

    call equiv_next2 ( n, a, done )

    if ( done ) then
      exit
    end if

    rank = rank + 1
    write ( *, '(2x,i4,10x,6i4)' ) rank, a(1:n)

  end do

  return 
end
subroutine equiv_print_test ( )

!*****************************************************************************80
!
!! EQUIV_PRINT_TEST tests EQUIV_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) npart
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EQUIV_PRINT_TEST'
  write ( *, '(a)' ) '  EQUIV_PRINT prints a set partition.'
 
  seed = 123456789

  do i = 1, 5
 
    call equiv_random ( n, seed, npart, a )

    call equiv_print ( n, a, '  The set partition:' )
 
  end do
 
  return
end
subroutine equiv_print2_test ( )

!*****************************************************************************80
!
!! EQUIV_PRINT2_TEST tests EQUIV_PRINT2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) npart
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EQUIV_PRINT2_TEST'
  write ( *, '(a)' ) '  EQUIV_PRINT2 prints a set partition.'
 
  seed = 123456789

  do i = 1, 5
 
    call equiv_random ( n, seed, npart, a )

    call equiv_print2 ( n, a, '  The set partition:' )
 
  end do
 
  return
end
subroutine equiv_random_test ( )

!*****************************************************************************80
!
!! EQUIV_RANDOM_TEST tests EQUIV_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 May 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) npart
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EQUIV_RANDOM_TEST'
  write ( *, '(a)' ) '  EQUIV_RANDOM selects a random set partition.'
 
  seed = 123456789

  do i = 1, 5
 
    call equiv_random ( n, seed, npart, a )

    call equiv_print2 ( n, a, '  The set partition:' )
 
  end do
 
  return
end
subroutine euler_row_test ( )

!*****************************************************************************80
!
!! EULER_ROW_TEST tests EULER_ROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nmax = 9

  integer ( kind = 4 ) ieuler(0:nmax)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EULER_ROW_TEST'
  write ( *, '(a)' ) '  EULER_ROW gets rows of Euler''s triangle.'
  write ( *, '(a)' ) ''

  do n = 0, nmax
    call euler_row ( n, ieuler )
    write ( *, '(2x,10i7)' ) ieuler(0:n)
  end do
 
  return
end
subroutine frobenius_number_order2_test ( )

!*****************************************************************************80
!
!! FROBENIUS_NUMBER_ORDER2_TEST tests FROBENIUS_NUMBER_ORDER2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) c1
  integer ( kind = 4 ) c2
  integer ( kind = 4 ) f1
  integer ( kind = 4 ) f2
  integer ( kind = 4 ) frobenius_number_order2
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FROBENIUS_NUMBER_ORDER2_TEST'
  write ( *, '(a)' ) &
    '  FROBENIUS_NUMBER_ORDER2 computes Frobenius numbers of order 2.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        C1        C1   exact F  comput F'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call frobenius_number_order2_values ( n_data, c1, c2, f1 )

    if ( n_data == 0 ) then
      exit
    end if

    f2 = frobenius_number_order2 ( c1, c2 )

    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) c1, c2, f1, f2

  end do

  return
end
subroutine gray_next_test ( )

!*****************************************************************************80
!
!! GRAY_NEXT_TEST tests GRAY_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) change
  integer ( kind = 4 ) g(n)
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GRAY_NEXT_TEST'
  write ( *, '(a)' ) '  GRAY_NEXT returns the index of the single item'
  write ( *, '(a)' ) '  to be changed in order to get the next Gray code.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   K  Change  Gray Code'
  write ( *, '(a)' ) ''

  g(1:n) = 0

  change = -n
  k = 0
  a(1:n) = 0

  do

    call gray_next ( n, change, k, a )

    if ( change == -n ) then
      exit
    else if ( change == 0 ) then
      g(1:n) = 0
    else
      g(abs(change)) = 1 - g(abs(change))
    end if

    write ( *, '(2x,i2,2x,i8,2x,4i1)' ) k, change, g(1:n)
    
  end do

  return
end
subroutine gray_rank1_test ( )

!*****************************************************************************80
!
!! GRAY_RANK1_TEST tests GRAY_RANK1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) gray
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GRAY_RANK1_TEST'
  write ( *, '(a)' ) '  GRAY_RANK1 ranks a Gray code;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    R  =                         RANK'
  write ( *, '(a)' ) '    G  =            GRAY_UNRANK1(RANK)'
  write ( *, '(a)' ) '    R2 = GRAY_RANK1(GRAY_UNRANK1(RANK))'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         R         G         R2'
  write ( *, '(a)' ) ''
 
  do rank = 0, 24
    call gray_unrank1 ( rank, gray )
    call gray_rank1 ( gray, rank2 )
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) rank, gray, rank2
  end do

  return
end
subroutine gray_rank2_test ( )

!*****************************************************************************80
!
!! GRAY_RANK2_TEST tests GRAY_RANK2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) gray
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GRAY_RANK2_TEST'
  write ( *, '(a)' ) '  GRAY_RANK2 ranks a Gray code;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    R  =                         RANK'
  write ( *, '(a)' ) '    G  =            GRAY_UNRANK2(RANK)'
  write ( *, '(a)' ) '    R2 = GRAY_RANK2(GRAY_UNRANK2(RANK))'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         R         G         R2'
  write ( *, '(a)' ) ''
 
  do rank = 0, 24
    call gray_unrank2 ( rank, gray )
    call gray_rank2 ( gray, rank2 )
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) rank, gray, rank2
  end do

  return
end
subroutine gray_unrank1_test ( )

!*****************************************************************************80
!
!! GRAY_UNRANK1_TEST tests GRAY_UNRANK1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) gray
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GRAY_UNRANK1_TEST'
  write ( *, '(a)' ) '  GRAY_UNRANK1 unranks a Gray code.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    R  =                         RANK'
  write ( *, '(a)' ) '    G  =            GRAY_UNRANK1(RANK)'
  write ( *, '(a)' ) '    R2 = GRAY_RANK1(GRAY_UNRANK1(RANK))'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         R         G         R2'
  write ( *, '(a)' ) ''
 
  do rank = 0, 24
    call gray_unrank1 ( rank, gray )
    call gray_rank1 ( gray, rank2 )
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) rank, gray, rank2
  end do

  return
end
subroutine gray_unrank2_test ( )

!*****************************************************************************80
!
!! GRAY_UNRANK2_TEST tests GRAY_UNRANK2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) gray
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GRAY_UNRANK2_TEST'
  write ( *, '(a)' ) '  GRAY_UNRANK2 unranks a Gray code.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    R  =                         RANK'
  write ( *, '(a)' ) '    G  =            GRAY_UNRANK2(RANK)'
  write ( *, '(a)' ) '    R2 = GRAY_RANK2(GRAY_UNRANK2(RANK))'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         R         G         R2'
  write ( *, '(a)' ) ''
 
  do rank = 0, 24
    call gray_unrank2 ( rank, gray )
    call gray_rank2 ( gray, rank2 )
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) rank, gray, rank2
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_BCLR_TEST'
  write ( *, '(a)' ) '  I4_BCLR sets a given bit to 0.'

  do test = 1, test_num

    i4 = i4_test(test)

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Working on I4 = ', i4
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '       Pos     I4_BCLR(I4,POS)'
    write ( *, '(a)' ) ' '

    do pos = 0, 31
  
      j = i4_bclr ( i4, pos )

      write ( *, '(2x,i8,2x,i12)' ) pos, j

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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_BSET_TEST'
  write ( *, '(a)' ) '  I4_BSET sets a given bit to 1.'

  do test = 1, test_num

    i4 = i4_test(test)

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Working on I4 = ', i4
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '       Pos     I4_BSET(I4,POS)'
    write ( *, '(a)' ) ' '

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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_BTEST_TEST'
  write ( *, '(a)' ) '  I4_BTEST reports whether a given bit is 0 or 1.'

  do test = 1, test_num

    i4 = i4_test(test)

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Analyze the integer I4 = ', i4
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '       Pos     I4_BTEST(I4,POS)'
    write ( *, '(a)' ) ' '

    do pos = 0, 31
  
      j = i4_btest ( i4, pos )

      write ( *, '(2x,i8,13x,l1)' ) pos, j

    end do

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
subroutine i4_factor_test ( )

!*****************************************************************************80
!
!! I4_FACTOR_TEST tests I4_FACTOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter  :: factor_max = 10

  integer ( kind = 4 ) factor(factor_max)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nfactor
  integer ( kind = 4 ) nleft
  integer ( kind = 4 ) power(factor_max)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_FACTOR_TEST'
  write ( *, '(a)' ) '  I4_FACTOR factors an integer,'

  n = ( 2 ** 2 ) * 17 * 37

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The integer is ', n

  call i4_factor ( n, factor_max, nfactor, factor, power, nleft )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Prime representation:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I  FACTOR(I)  POWER(I)'
  write ( *, '(a)' ) ''
  if ( abs ( nleft ) /= 1 ) then
    write ( *, '(2x,i8,i8,a)' ) 0, nleft, ' (UNFACTORED PORTION)'
  end if

  do i = 1, nfactor
    write ( *, '(2x,3i8)' ) i, factor(i), power(i)
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
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_gcd
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_GCD_TEST'
  write ( *, '(a)' ) '  I4_GCD computes the greatest common divisor'
  write ( *, '(a)' ) '  of two integers.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I       J    I4_GCD(I,J)'
  write ( *, '(a)' ) ''

  seed = 123456789

  do k = 1, 15
    i = i4_uniform_ab ( -5, 15, seed )
    j = i4_uniform_ab ( 1, 15, seed )
    write ( *, '(2x,3i8)' ) i, j, i4_gcd ( i, j )
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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_HUGE_TEST'
  write ( *, '(a)' ) '  I4_HUGE returns a huge integer.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  I4_HUGE() = ', i4_huge ( )
  write ( *, '(a,i12)' ) '  HUGE(1) =   ', huge ( dummy )

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
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 21

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_log_10
  integer ( kind = 4 ), dimension ( n ) :: x = (/ &
      0,    1,    2,  3,  9, 10, 11,  99, 100, 101, &
    999, 1000, 1001, -1, -2, -3, -9, -10, -11, -99, &
   -101 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_LOG_10_TEST'
  write ( *, '(a)' ) '  I4_LOG_10: whole part of log base 10,'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         X        I4_LOG_10'
  write ( *, '(a)' ) ''

  do i = 1, n

    write ( *, '(2x,i8,i12)' ) x(i), i4_log_10 ( x(i) )

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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_MODP_TEST'
  write ( *, '(a)' ) '  I4_MODP factors a number'
  write ( *, '(a)' ) '  into a multiple and a positive remainder.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Number   Divisor  Multiple Remainder'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    n = n_vec(test)
    d = d_vec(test)
    r = i4_modp ( n, d )
    m = ( n - r ) / d
    write ( *, '(2x,4i10)' ) n, d, m, r
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Repeat using FORTRAN MOD:'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    n = n_vec(test)
    d = d_vec(test)
    r = mod ( n, d )
    m = ( n - r ) / d
    write ( *, '(2x,4i10)' ) n, d, m, r
  end do

  return
end
subroutine i4_moebius_test ( )

!*****************************************************************************80
!
!! I4_MOEBIUS_TEST tests I4_MOEBIUS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) c1
  integer ( kind = 4 ) c2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_MOEBIUS_TEST:'
  write ( *, '(a)' ) '  I4_MOEBIUS evaluates the Moebius function:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N      Exact         I4_MOEBIUS(N)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call moebius_values ( n_data, n, c1 )

    if ( n_data == 0 ) then
      exit
    end if

    call i4_moebius ( n, c2 )

    write ( *, '(2x,i8,2x,i12,2x,i12)' ) n, c1, c2

  end do

  return
end
subroutine i4_partition_conj_test ( )

!*****************************************************************************80
!
!! I4_PARTITION_CONJ_TEST tests I4_PARTITION_CONJ.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 14
  integer ( kind = 4 ), parameter :: npart1 = 4

  integer ( kind = 4 ) a1(npart1)
  integer ( kind = 4 ) a2(n)
  integer ( kind = 4 ) mult1(npart1)
  integer ( kind = 4 ) mult2(n)
  integer ( kind = 4 ) npart2

  a1(1:npart1) = (/ 2, 5, 1, 4 /)
  mult1(1:npart1) = (/ 1, 1, 3, 1 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_PARTITION_CONJ_TEST'
  write ( *, '(a)' ) '  I4_PARTITION_CONJ conjugates an integer partition.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Original partition:'
  write ( *, '(a)' ) ''

  call i4_partition_print ( n, npart1, a1, mult1 )

  call i4_partition_conj ( n, a1, mult1, npart1, a2, mult2, npart2 )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Conjugate partition:'
  write ( *, '(a)' ) ''

  call i4_partition_print ( n, npart2, a2, mult2 )

  return
end
subroutine i4_partition_count_test ( )

!*****************************************************************************80
!
!! I4_PARTITION_COUNT_TEST tests I4_PARTITION_COUNT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) p
  integer ( kind = 4 ) p2(0:n_max)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_PARTITION_COUNT_TEST'
  write ( *, '(a)' ) '  I4_PARTITION_COUNT counts partitions of an integer.'

  n_data = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N     Exact     Count'
  write ( *, '(a)' ) ''

  do

    call i4_partition_count_values ( n_data, n, p )

    if ( n_data == 0 ) then
      exit
    end if

    call i4_partition_count ( n, p2 )
 
    write ( *, '(2x,i4,2i10)' ) n, p, p2(n)

  end do

  return
end
subroutine i4_partition_count2_test ( )

!*****************************************************************************80
!
!! I4_PARTITION_COUNT2_TEST tests I4_PARTITION_COUNT2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) p
  integer ( kind = 4 ) p2(0:n_max)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_PARTITION_COUNT2_TEST'
  write ( *, '(a)' ) '  I4_PARTITION_COUNT2 counts partitions of an integer.'

  n_data = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N     Exact     Count'
  write ( *, '(a)' ) ''

  do

    call i4_partition_count_values ( n_data, n, p )

    if ( n_data == 0 ) then
      exit
    end if

    call i4_partition_count2 ( n, p2 )
 
    write ( *, '(2x,i4,2i10)' ) n, p, p2(n)

  end do

  return
end
subroutine i4_partition_next_test ( )

!*****************************************************************************80
!
!! I4_PARTITION_NEXT_TEST tests I4_PARTITION_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ) a(n)
  logical done
  integer ( kind = 4 ) mult(n)
  integer ( kind = 4 ) npart
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_PARTITION_NEXT_TEST'
  write ( *, '(a)' ) '  I4_PARTITION_NEXT generates partitions of an integer.'
  write ( *, '(a,i8)' ) '  Here N = ', n
  write ( *, '(a)' ) ''

  rank = 0
  done = .true.
 
  do

    call i4_partition_next ( n, npart, a, mult, done )
 
    if ( done ) then
      exit 
    end if

    rank = rank + 1

    call i4_partition_print ( n, npart, a, mult )

  end do
 
  return
end
subroutine i4_partition_next2_test ( )

!*****************************************************************************80
!
!! I4_PARTITION_NEXT2_TEST tests I4_PARTITION_NEXT2.
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

  integer ( kind = 4 ) a(7)
  logical more
  integer ( kind = 4 ) mult(7)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) npart

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_PARTITION_NEXT2_TEST'
  write ( *, '(a)' ) '  I4_PARTITION_NEXT2 produces partitions of an integer.'
  write ( *, '(a)' ) ''

  n = 7
  more = .false.

  do

    call i4_partition_next2 ( n, npart, a, mult, more )

    call i4_partition_print ( n, npart, a, mult )

    if ( .not. more ) then
      exit
    end if

  end do

  return
end
subroutine i4_partition_print_test ( )

!*****************************************************************************80
!
!! I4_PARTITION_PRINT_TEST tests I4_PARTITION_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 14
  integer ( kind = 4 ), parameter :: npart = 4

  integer ( kind = 4 ) a(npart)
  integer ( kind = 4 ) mult(npart)

  a(1:npart) = (/ 2, 5, 1, 4 /)
  mult(1:npart) = (/ 1, 1, 3, 1 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_PARTITION_PRINT_TEST'
  write ( *, '(a)' ) '  I4_PARTITION_PRINT prints an integer partition.'
  write ( *, '(a)' ) ''

  call i4_partition_print ( n, npart, a, mult )

  return
end
subroutine i4_partition_random_test ( )

!*****************************************************************************80
!
!! I4_PARTITION_RANDOM_TEST tests I4_PARTITION_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 8

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) mult(n)
  integer ( kind = 4 ) npart
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) table(0:n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_PARTITION_RANDOM_TEST'
  write ( *, '(a)' ) '  I4_PARTITION_RANDOM generates a random partition.'
  write ( *, '(a)' ) ''

  seed = 123456789
!
!  Get the partition table.
!
  call i4_partition_count2 ( n, table )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The number of partitions of N'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       N    Number of partitions'
  write ( *, '(a)' ) ''

  do j = 0, n
    write ( *, '(2x,i8,4x,i8)' ) j, table(j)
  end do

  write ( *, '(a)' ) ''

  do i = 1, 5

    call i4_partition_random ( n, table, seed, a, mult, npart )

    call i4_partition_print ( n, npart, a, mult )

  end do
 
  return
end
subroutine i4_partitions_next_test ( )

!*****************************************************************************80
!
!! I4_PARTITIONS_NEXT_TEST tests I4_PARTITIONS_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: s = 3

  integer ( kind = 4 ) i
  integer ( kind = 4 ) m(s)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_PARTITIONS_NEXT_TEST'
  write ( *, '(a)' ) '  I4_PARTITIONS_NEXT produces the next'
  write ( *, '(a)' ) '  nondecreasing partitions of an integer, and'
  write ( *, '(a)' ) '  if necessary, increments the integer to keep on going.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I Sum    Partition'
  write ( *, '(a)' ) ''
  i = 0

  m(1:3) = (/ 0, 0, 0 /)

  write ( *, '(2x,i2,2x,i2,4x,10i2)' ) i, sum ( m ), m(1:s)

  do i = 1, 15

    call i4_partitions_next ( s, m )

    write ( *, '(2x,i2,2x,i2,4x,10i2)' ) i, sum ( m ), m(1:s)

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  You can start from any legal partition.'
  write ( *, '(a)' ) '  Here, we restart at ( 2, 1, 0 ).'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I Sum    Partition'
  write ( *, '(a)' ) ''

  i = 0
  m = (/ 2, 1, 0 /)

  write ( *, '(2x,i2,2x,i2,4x,10i2)' ) i, sum ( m ), m(1:s)

  do i = 1, 15

    call i4_partitions_next ( s, m )

    write ( *, '(2x,i2,2x,i2,4x,10i2)' ) i, sum ( m ), m(1:s)

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
subroutine i4_sqrt_test ( )

!*****************************************************************************80
!
!! I4_SQRT_TEST tests I4_SQRT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) q
  integer ( kind = 4 ) r

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_SQRT_TEST'
  write ( *, '(a)' ) '  I4_SQRT computes the square root of an integer.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       N  Sqrt(N) Remainder'
  write ( *, '(a)' ) ''

  do n = -5, 20

    call i4_sqrt ( n, q, r )
    write ( *, '(2x,3i9)' ) n, q, r

  end do

  return
end
subroutine i4_sqrt_cf_test ( )

!*****************************************************************************80
!
!! I4_SQRT_CF_TEST tests I4_SQRT_CF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: max_term = 100

  integer ( kind = 4 ) b(0:max_term)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_term

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_SQRT_CF_TEST'
  write ( *, '(a)' ) '  I4_SQRT_CF computes the continued fraction form'
  write ( *, '(a)' ) '  of the square root of an integer.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N  Period  Whole  Repeating Part'
  write ( *, '(a)' ) ''
  do n = 1, 20

    call i4_sqrt_cf ( n, max_term, n_term, b )
    write ( *, '(2x,i5,3x,i5,2x,i5,10i5)' ) n, n_term, b(0), b(1:n_term)

  end do

  return
end
subroutine i4_to_chinese_test ( )

!*****************************************************************************80
!
!! I4_TO_CHINESE_TEST tests I4_TO_CHINESE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ), dimension ( n ) :: m = (/ 3, 4, 5, 7 /)
  integer ( kind = 4 ) r(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_TO_CHINESE_TEST'
  write ( *, '(a)' ) '  I4_TO_CHINESE computes the Chinese Remainder'
  write ( *, '(a)' ) '  representation of an integer.'

  call i4vec_print ( n, m, '  The moduli:' )

  j = 37

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The number being analyzed is ', j

  call i4_to_chinese ( j, n, m, r )

  call i4vec_print ( n, r, '  The remainders:' )

  call chinese_to_i4 ( n, m, r, j2 )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The reconstructed number is ', j2

  call i4_to_chinese ( j2, n, m, r )

  call i4vec_print ( n, r, '  The remainders of the reconstructed number are:' )

  return
end
subroutine i4_to_dvec_test ( )

!*****************************************************************************80
!
!! I4_TO_DVEC_TEST tests I4_TO_DVEC;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ) dvec(n)
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_TO_DVEC_TEST'
  write ( *, '(a)' ) '  I4_TO_DVEC converts a DVEC to an I4'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        I4 => DVEC => I4'
  write ( *, '(a)' ) ''

  seed = 123456789
  i1 = i4_uniform_ab ( -10000, 10000, seed )
  call i4_to_dvec ( i1, n, dvec )
  call dvec_to_i4 ( n, dvec, i2 )

  write ( *, '(2x,i6,2x,6i2,2x,i6)' ) i1, dvec(n:1:-1), i2

  return
end
subroutine i4_to_i4poly_test ( )

!*****************************************************************************80
!
!! I4_TO_I4POLY_TEST tests I4_TO_I4POLY;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: degree_max = 5
  integer ( kind = 4 ), parameter :: test_num = 9

  integer ( kind = 4 ) a(0:degree_max)
  integer ( kind = 4 ) base
  integer ( kind = 4 ), dimension ( test_num ) :: base_test = (/ &
   2, 2, 2, 3, 4, 5, 6, 23, 24 /)
  integer ( kind = 4 ) degree
  integer ( kind = 4 ) intval
  integer ( kind = 4 ) intval2
  integer ( kind = 4 ), dimension ( test_num ) :: intval_test = (/ &
   1, 6, 23, 23, 23, 23, 23, 23, 23 /)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_TO_I4POLY_TEST'
  write ( *, '(a)' ) '  I4_TO_I4POLY converts an integer to a polynomial'
  write ( *, '(a)' ) '  in a given base;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       I    BASE  DEGREE  Coefficients'
  write ( *, '(a)' ) ''
  do test = 1, test_num
    intval = intval_test(test)
    base = base_test(test)
    call i4_to_i4poly ( intval, base, degree_max, degree, a )
    write ( *, '(2x,i4,2x,i4,2x,i4,6x,10i8)' ) &
      intval, base, degree, a(degree:0:-1)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now let I4_TO_I4POLY convert I to a polynomial,'
  write ( *, '(a)' ) '  use I4POLY_TO_I4 to evaluate it, and compare.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       I    I2'
  write ( *, '(a)' ) ''
  do test = 1, test_num
    intval = intval_test(test)
    base = base_test(test)
    call i4_to_i4poly ( intval, base, degree_max, degree, a )
    call i4poly_to_i4 ( degree, a, base, intval2 )
    write ( *, '(2x,i8,2x,i8)' ) intval, intval2
  end do

  return
end
subroutine i4_to_van_der_corput_test ( )

!*****************************************************************************80
!
!! I4_TO_VAN_DER_CORPUT_TEST tests I4_TO_VAN_DER_CORPUT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_prime = 5

  real ( kind = 8 ) h(n_prime)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) :: n_test = 10
  integer ( kind = 4 ) p
  integer ( kind = 4 ) prime

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_TO_VAN_DER_CORPUT_TEST'
  write ( *, '(a)' ) '  I4_TO_VAN_DER_CORPUT computes the elements '
  write ( *, '(a)' ) '  of a van der Corput sequence.'
  write ( *, '(a)' ) '  The sequence depends on the prime numbers used '
  write ( *, '(a)' ) '  as a base.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Bases:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5(8x,i8))' ) ( prime(j), j = 1, n_prime )
  write ( *, '(a)' ) ''

  do i = 1, n_test
    do j = 1, n_prime
      p = prime(j)
      call i4_to_van_der_corput ( i, p, h(j) )
    end do
    write ( *, '(2x,i3,5g14.6)' ) i, h(1:n_prime)
  end do

  return
end
subroutine i4mat_01_rowcolsum_test ( )

!*****************************************************************************80
!
!! I4MAT_01_ROWCOLSUM_TEST tests I4MAT_01_ROWCOLSUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 13
  integer ( kind = 4 ), parameter :: n = 17

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ), dimension ( n ) :: c = &
    (/  4,  4, 11, 10, 10,  8,  9, 10,  8,  9,  3, 10,  4,  7,  9,  3,  3 /)
  integer ( kind = 4 ), dimension ( n ) :: c2
  integer ( kind = 4 ), dimension ( n ) :: c3
  integer ( kind = 4 ), dimension ( n ) :: cperm
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( m ) :: r = &
    (/ 14, 13, 14, 10, 12,  2, 10,  1, 10, 11,  6,  2, 17 /)
  integer ( kind = 4 ), dimension ( m ) :: r2
  integer ( kind = 4 ), dimension ( m ) :: r3
  integer ( kind = 4 ), dimension ( m ) :: rperm

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_01_ROWCOLSUM_TEST'
  write ( *, '(a)' ) '  I4MAT_01_ROWCOLSUM constructs a 01 matrix with'
  write ( *, '(a)' ) '  given row and column sums.'

  call i4vec_print ( m, r, '  The rowsum vector R:' )
  call i4vec_sort_heap_index_d ( m, r, rperm )

  do i = 1, m
    r2(i) = r(rperm(i))
  end do

  call i4vec_print ( n, c, '  The columnsum vector C: ' )
  call i4vec_sort_heap_index_d ( n, c, cperm )

  do i = 1, n
    c2(i) = c(cperm(i))
  end do

  call i4mat_01_rowcolsum ( m, n, r2, c2, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) &
      '  I4MAT_01_ROWCOLSUM returned error flag IERROR = ', ierror
    return
  end if
!
!  Invert the R and C permutations.
!
  call i4mat_2perm1 ( m, n, a, rperm, cperm )

  call i4mat_print ( m, n, a, '  The rowcolsum matrix:' )

  do i = 1, m
    r3(i) = sum ( a(i,1:n) )
  end do

  call i4vec_print ( m, r3, '  Computed row sums' )

  do j = 1, n
    c3(j) = sum ( a(1:m,j) )
  end do

  call i4vec_print ( n, c3, '  Computed column sums:' )

  return
end
subroutine i4mat_01_rowcolsum2_test ( )

!*****************************************************************************80
!
!! I4MAT_01_ROWCOLSUM2_TEST tests I4MAT_01_ROWCOLSUM2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ), dimension ( n ) :: c = (/ &
    2, 1, 2, 2, 2 /)
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ), dimension ( m ) :: r = (/ &
    2, 1, 3, 1, 2 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_01_ROWCOLSUM2_TEST'
  write ( *, '(a)' ) '  I4MAT_01_ROWCOLSUM2 constructs a 01 matrix with'
  write ( *, '(a)' ) '  given row and column sums.'

  call i4vec_print ( m, r, '  The rowsum vector:' )
  call i4vec_print ( n, c, '  The columnsum vector: ' )

  call i4mat_01_rowcolsum2 ( m, n, r, c, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) &
      '  I4MAT_01_ROWCOLSUM2 returned error flag IERROR = ', ierror
    write ( *, '(a)' ) '  The matrix returned is not an exact solution.'
  end if

  call i4mat_print ( m, n, a, '  The rowcolsum matrix:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now repeat, with data for which there is not'
  write ( *, '(a)' ) '  a solution.  The program will try its best anyway.'

  c = (/ 1, 4, 1, 5, 1 /)
  r = (/ 1, 3, 4, 1, 3 /)

  call i4vec_print ( m, r, '  The rowsum vector:' )
  call i4vec_print ( n, c, '  The columnsum vector: ' )

  call i4mat_01_rowcolsum2 ( m, n, r, c, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) &
      '  I4MAT_01_ROWCOLSUM2 returned error flag IERROR = ', ierror
    write ( *, '(a)' ) '  The matrix returned is not an exact solution.'
  end if

  call i4mat_print ( m, n, a, '  The rowcolsum matrix:' )

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
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ), dimension (n,n) :: a = reshape ( &
  (/ &
    1, 0, 0, 0, 0, 0, &
    1, 1, 0, 0, 0, 0, &
    0, 0, 1, 0, 0, 0, &
    0, 0, 1, 1, 0, 0, &
    0, 0, 0, 0, 1, 0, &
   75, 0, 0, 0, 1, 1 /), (/ n, n /) )
  integer ( kind = 4 ) b(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_U1_INVERSE_TEST'
  write ( *, '(a)' ) '  I4MAT_U1_INVERSE inverts a unit upper triangular matrix.'

  call i4mat_print ( n, n, a, '  The input matrix:' )
 
  call i4mat_u1_inverse ( n, a, b )
 
  call i4mat_print ( n, n, b, '  The inverse:' )
 
  return
end
subroutine i4mat_perm1_test ( )

!*****************************************************************************80
!
!! I4MAT_PERM1_TEST test I4MAT_PERM1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 9

  integer ( kind = 4 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( n ) :: p = (/ 2,3,9,6,7,8,5,4,1 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_PERM1_TEST'
  write ( *, '(a)' ) '  I4MAT_PERM1 reorders an integer matrix in place.'
  write ( *, '(a)' ) '  The rows and columns use the same permutation.'

  do i = 1, n
    do j = 1, n
      a(i,j) = i * 10 + j
    end do
  end do

  call i4mat_print ( n, n, a, '  The input matrix:' )
 
  call perm1_print ( n, p, '  The row and column permutation:' )
 
  call i4mat_perm1 ( n, a, p )
 
  call i4mat_print ( n, n, a, '  The permuted matrix:' )
 
  return
end
subroutine i4mat_2perm1_test ( )

!*****************************************************************************80
!
!! I4MAT_2PERM1_TEST test I4MAT_2PERM1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 9
  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), dimension ( m ) :: p = (/ 2,3,9,6,7,8,5,4,1 /)
  integer ( kind = 4 ), dimension ( n ) :: q = (/ 3,4,5,6,7,1,2 /)
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_2PERM1_TEST'
  write ( *, '(a)' ) '  I4MAT_2PERM1 reorders an integer matrix in place.'
  write ( *, '(a)' ) '  Rows and columns use different permutations.'

  do i = 1, m
    do j = 1, n
      a(i,j) = i * 10 + j
    end do
  end do
 
  call i4mat_print ( m, n, a, '  The input matrix:' )
 
  call perm1_print ( m, p, '  The row permutation:' )

  call perm1_print ( n, q, '  The column permutation:' )
 
  call i4mat_2perm1 ( m, n, a, p, q )
 
  call i4mat_print ( m, n, a, '  The permuted matrix:' )

  return
end
subroutine i4poly_test ( )

!*****************************************************************************80
!
!! I4POLY_TEST test I4POLY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6
  integer ( kind = 4 ), parameter :: test_num = 6

  integer ( kind = 4 ), dimension ( n ) :: a
  integer ( kind = 4 ) iopt
  integer ( kind = 4 ) test
  integer ( kind = 4 ) val
  integer ( kind = 4 ) x0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4POLY_TEST'
  write ( *, '(a)' ) '  I4POLY converts between power sum, factorial'
  write ( *, '(a)' ) '  and Taylor forms, and can evaluate a polynomial'
  write ( *, '(a)' ) ''
 
  do test = 1, test_num

    if ( test == 1 ) then
      iopt = -3
    else if ( test == 2 ) then
      iopt = -2
    else if ( test == 3 ) then
      iopt = -1
      x0 = 2
    else if ( test == 4 ) then
      iopt = 0
      x0 = 2
    else if ( test == 5 ) then
      iopt = 6
      x0 = 2
    else if ( test == 6 ) then
      iopt = 6
      x0 = -2
    end if

    a = (/ 0, 0, 0, 0, 0, 1 /)

    if ( test == 1 ) then
      write ( *, '(a)' ) '  All calls have input A as follows'
      write ( *, '(10i4)' ) a(1:n)
    end if
 
    call i4poly ( n, a, x0, iopt, val )
 
    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Option IOPT = ', iopt
    if ( -1 <= iopt ) then
      write ( *, '(a,i8)' ) '  X0 = ', x0
    end if
    if ( iopt == -3 .or. iopt == -2 .or. 0 < iopt ) then
      write ( *, '(a)' ) '  Output array = '
      write ( *, '(2x,10i4)' ) a(1:n)
    end if
    if ( iopt == -1 .or. iopt == 0 ) then
      write ( *, '(a,i8)' ) '  Value = ', val
    end if
 
  end do

  return
end
subroutine i4poly_add_test ( )

!*****************************************************************************80
!
!! I4POLY_ADD_TEST tests I4POLY_ADD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a(0:5)
  integer ( kind = 4 ) b(0:5)
  integer ( kind = 4 ) c(0:5)
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nb
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) nc2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4POLY_ADD_TEST'
  write ( *, '(a)' ) '  I4POLY_ADD adds two I4POLY''s.'

  na = 5
  a(0:na) = (/ 0, 1, 2, 3, 4, 5 /)
  nb = 5
  b(0:nb) = (/ 1, -2, 7, 8, 0, -5 /)

  call i4poly_add ( na, a, nb, b, c )

  call i4poly_print ( na, a, '  Polynomial A:' )

  call i4poly_print ( nb, b, '  Polynomial B:' )

  nc = max ( na, nb )

  call i4poly_degree ( nc, c, nc2 )

  call i4poly_print ( nc2, c, '  Polynomial C = A+B:' )

  return
end
subroutine i4poly_cyclo_test ( )

!*****************************************************************************80
!
!! I4POLY_CYCLO_TEST tests I4POLY_CYCLO.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: max_n = 10

  integer ( kind = 4 ) phi(0:max_n)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4POLY_CYCLO_TEST'
  write ( *, '(a)' ) '  I4POLY_CYCLO computes cyclotomic polynomials.'

  do n = 0, max_n

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  N = ', n
    write ( *, '(a)' ) ''
    call i4poly_cyclo ( n, phi )

    call i4poly_print ( n, phi, '  The cyclotomic polynomial:' )

  end do

  return
end
subroutine i4poly_degree_test ( )

!*****************************************************************************80
!
!! I4POLY_DEGREE_TEST tests I4POLY_DEGREE.
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

  integer ( kind = 4 ) a(0:10)
  integer ( kind = 4 ) degree
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4POLY_DEGREE_TEST'
  write ( *, '(a)' ) '  I4POLY_DEGREE returns the degree of an I4POLY.'

  n = 10
  a(0:n) = (/ 0, 1, 2, 0, 4, 0, 6, 7, 0, 0, 0 /)

  call i4poly_print ( n, a, '  The polynomial:' )

  call i4poly_degree ( n, a, degree )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The polynomial degree is ', degree

  return
end
subroutine i4poly_dif_test ( )

!*****************************************************************************80
!
!! I4POLY_DIF_TEST tests I4POLY_DIF.
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

  integer ( kind = 4 ) a(0:10)
  integer ( kind = 4 ) b(0:10)
  integer ( kind = 4 ) d
  integer ( kind = 4 ) na
  integer ( kind = 4 ), parameter :: test_num = 2
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4POLY_DIF_TEST'
  write ( *, '(a)' ) '  I4POLY_DIF computes derivatives of an I4POLY.'
  write ( *, '(a)' ) ''
!
!  1: Differentiate X^3 + 2*X^2 - 5*X - 6 once.
!  2: Differentiate X^4 + 3*X^3 + 2*X^2 - 2  3 times.
!
  do test = 1, test_num

    if ( test == 1 ) then
      na = 3
      d = 1
      a(0:na) = (/ -6, -5, 2, 1 /)
    else if ( test == 2 ) then
      na = 4
      d = 3
      a(0:na) = (/ -2, 5, 2, 3, 1 /)
    end if

    call i4poly_print ( na, a, '  The polynomial A:' )

    write ( *, '(a)' ) ''
    write ( *, '(a,i2,a)' ) '  Differentiate A ', d, ' times.'

    call i4poly_dif ( na, a, d, b )
 
    call i4poly_print ( na - d, b, '  The derivative, B:' )

  end do

  return
end
subroutine i4poly_div_test ( )

!*****************************************************************************80
!
!! I4POLY_DIV_TEST tests I4POLY_DIV.
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

  integer ( kind = 4 ) a(0:10)
  integer ( kind = 4 ) b(0:10)
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nb
  integer ( kind = 4 ) nq
  integer ( kind = 4 ) nr
  integer ( kind = 4 ), parameter :: test_num = 2
  integer ( kind = 4 ) q(0:10)
  integer ( kind = 4 ) r(0:10)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4POLY_DIV_TEST'
  write ( *, '(a)' ) '  I4POLY_DIV computes the quotient and'
  write ( *, '(a)' ) '  remainder for polynomial division.'
  write ( *, '(a)' ) ''
!
!  1: Divide X^3 + 2*X^2 - 5*X - 6  by X-2.  
!     Quotient is 3+4*X+X^2, remainder is 0.
!
!  2: Divide X^4 + 3*X^3 + 2*X^2 - 2  by  X^2 + X - 3.
!     Quotient is X^2 + 2*X + 3, remainder 8*X + 7.
!
  do test = 1, test_num

    if ( test == 1 ) then
      na = 3
      a(0:na) = (/ -6, -5, 2, 1 /)
      nb = 1
      b(0:nb) = (/ -2, 1 /)
    else if ( test == 2 ) then
      na = 4
      a(0:na) = (/ -2, 5, 2, 3, 1 /)
      nb = 2
      b(0:nb) = (/ -3, 1, 1 /)
    end if

    call i4poly_print ( na, a, '  The polynomial to be divided, A:' )
    call i4poly_print ( nb, b, '  The divisor polynomial, B:' )

    call i4poly_div ( na, a, nb, b, nq, q, nr, r )
 
    call i4poly_print ( nq, q, '  The quotient polynomial, Q:' )
    call i4poly_print ( nr, r, '  The remainder polynomial, R:' )

  end do

  return
end
subroutine i4poly_mul_test ( )

!*****************************************************************************80
!
!! I4POLY_MUL_TEST tests I4POLY_MUL.
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

  integer ( kind = 4 ), parameter :: maxn = 5

  integer ( kind = 4 ) a(0:maxn)
  integer ( kind = 4 ) b(0:maxn)
  integer ( kind = 4 ) c(0:maxn)
  integer ( kind = 4 ) test
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nb
  integer ( kind = 4 ), parameter :: test_num = 2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4POLY_MUL_TEST'
  write ( *, '(a)' ) '  I4POLY_MUL multiplies two polynomials.'
  write ( *, '(a)' ) ''
!
!  1: Multiply (1+X) times (1-X).  Answer is 1-X**2.
!  2: Multiply (1+2*X+3*X**2) by (1-2*X). Answer is 1 + 0*X - X**2 - 6*X**3
!
  do test = 1, test_num

    if ( test == 1 ) then
      na = 1
      a(0:na) = (/ 1, 1 /)
      nb = 1
      b(0:nb) = (/ 1, -1 /)
    else if ( test == 2 ) then
      na = 2
      a(0:na) = (/ 1, 2, 3 /)
      nb = 1
      b(0:nb) = (/ 1, -2 /)
    end if

    call i4poly_mul ( na, a, nb, b, c )

    call i4poly_print ( na, a, '  The factor A:' )

    call i4poly_print ( nb, b, '  The factor B:' )

    call i4poly_print ( na+nb, c, '  The product C = A*B:' )

  end do

  return
end
subroutine i4poly_print_test ( )

!*****************************************************************************80
!
!! I4POLY_PRINT_TEST tests I4POLY_PRINT.
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

  integer ( kind = 4 ) a(0:4)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4POLY_PRINT_TEST'
  write ( *, '(a)' ) '  I4POLY_PRINT prints an I4POLY.'

  n = 4
  a(0:n) = (/ -2, 5, 2, 3, 1 /)

  call i4poly_print ( n, a, '  The polynomial:' )

  return
end
subroutine i4poly_to_i4_test ( )

!*****************************************************************************80
!
!! I4POLY_TO_I4_TEST tests I4POLY_TO_I4;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: degree_max = 5
  integer ( kind = 4 ), parameter :: test_num = 9

  integer ( kind = 4 ) a(0:degree_max)
  integer ( kind = 4 ) base
  integer ( kind = 4 ), dimension ( test_num ) :: base_test = (/ &
   2, 2, 2, 3, 4, 5, 6, 23, 24 /)
  integer ( kind = 4 ) degree
  integer ( kind = 4 ) intval
  integer ( kind = 4 ) intval2
  integer ( kind = 4 ), dimension ( test_num ) :: intval_test = (/ &
   1, 6, 23, 23, 23, 23, 23, 23, 23 /)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4POLY_TO_I4_TEST'
  write ( *, '(a)' ) '  I4POLY_TO_I4 evaluates an integer polynomial.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       I    BASE  DEGREE  Coefficients'
  write ( *, '(a)' ) ''
  do test = 1, test_num
    intval = intval_test(test)
    base = base_test(test)
    call i4_to_i4poly ( intval, base, degree_max, degree, a )
    write ( *, '(2x,i4,2x,i4,2x,i4,6x,10i8)' ) &
      intval, base, degree, a(degree:0:-1)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now let I4_TO_I4POLY convert I to a polynomial,'
  write ( *, '(a)' ) '  use I4POLY_TO_I4 to evaluate it, and compare.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       I    I2'
  write ( *, '(a)' ) ''
  do test = 1, test_num
    intval = intval_test(test)
    base = base_test(test)
    call i4_to_i4poly ( intval, base, degree_max, degree, a )
    call i4poly_to_i4 ( degree, a, base, intval2 )
    write ( *, '(2x,i8,2x,i8)' ) intval, intval2
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

    call i4vec_backtrack ( n, maxstack, x, indx, k, nstack, stacks, ncan )

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
subroutine i4vec_descends_test ( )

!*****************************************************************************80
!
!! I4VEC_DESCENDS_TEST tests I4VEC_DESCENDS;
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

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(n)
  logical descends
  integer ( kind = 4 ) i
  logical i4vec_descends
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_DESCENDS_TEST'
  write ( *, '(a)' ) '  I4VEC_DESCENDS is true if an integer vector decreases.'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 5

    call i4vec_uniform_ab ( n, 1, n, seed, a )

    call i4vec_print ( n, a, '  The integer array to search:' )
 
    descends = i4vec_descends ( n, a )

    if ( descends ) then
      write ( *, '(a)' ) '  The preceding vector is descending.'
    else
      write ( *, '(a)' ) '  The preceding vector is not descending.'
    end if

  end do

  return
end
subroutine i4vec_frac_test ( )

!*****************************************************************************80
!
!! I4VEC_FRAC_TEST tests I4VEC_FRAC;
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) afrac
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_FRAC_TEST'
  write ( *, '(a)' ) '  I4VEC_FRAC: K-th smallest integer vector entry.'
  write ( *, '(a)' ) ''

  seed = 123456789

  call i4vec_uniform_ab ( n, 1, 2*n, seed, a )

  call i4vec_print ( n, a, '  The integer array to search:' )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       K       K-th smallest'
  write ( *, '(a)' ) ''

  do k = 1, n

    call i4vec_frac ( n, a, k, afrac )
    write ( *, '(2x,i8,2x,i8)' ) k, afrac

  end do

  return
end
subroutine i4vec_index_test ( )

!*****************************************************************************80
!
!! I4VEC_INDEX_TEST tests I4VEC_INDEX;
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

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) aval
  integer ( kind = 4 ) first
  integer ( kind = 4 ) i4vec_index
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_INDEX_TEST'
  write ( *, '(a)' ) '  I4VEC_INDEX returns the index of the first occurrence'
  write ( *, '(a)' ) '  of a given value in an integer vector.'
  write ( *, '(a)' ) ''

  seed = 123456789

  call i4vec_uniform_ab ( n, 1, n/2, seed, a )

  aval = a(n/2)

  call i4vec_print ( n, a, '  The integer array to search:' )
 
  first = i4vec_index ( n, a, aval )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The value searched for is ', aval
  write ( *, '(a,i8)' ) '  The index of first occurrence is ', first

  return
end
subroutine i4vec_maxloc_last_test ( )

!*****************************************************************************80
!
!! I4VEC_MAXLOC_LAST_TEST tests I4VEC_MAXLOC_LAST;
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

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i4vec_maxloc_last
  integer ( kind = 4 ) last
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_MAXLOC_LAST_TEST'
  write ( *, '(a)' ) '  I4VEC_MAXLOC_LAST: index of the last maximal'
  write ( *, '(a)' ) '  entry in an integer vector.'
  write ( *, '(a)' ) ''

  seed = 123456789

  call i4vec_uniform_ab ( n, 1, n/4, seed, a )

  call i4vec_print ( n, a, '  The integer array to search:' )
 
  last = i4vec_maxloc_last ( n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Index of last maximal entry is ', last

  return
end
subroutine i4vec_pairwise_prime_test ( )

!*****************************************************************************80
!
!! I4VEC_PAIRWISE_PRIME_TEST tests I4VEC_PAIRWISE_PRIME;
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

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  logical i4vec_pairwise_prime
  logical pairwise_prime
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_PAIRWISE_PRIME_TEST'
  write ( *, '(a)' ) '  I4VEC_PAIRWISE_PRIME is true if an integer vector'
  write ( *, '(a)' ) '  is pairwise prime.'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 5

    call i4vec_uniform_ab ( n, 1, n, seed, a )

    call i4vec_print ( n, a, '  The integer array to check:' )
 
    pairwise_prime = i4vec_pairwise_prime ( n, a )

    if ( pairwise_prime ) then
      write ( *, '(a)' ) '  The preceding vector is pairwise prime.'
    else
      write ( *, '(a)' ) '  The preceding vector is not pairwise prime.'
    end if

  end do

  return
end
subroutine i4vec_reverse_test ( )

!*****************************************************************************80
!
!! I4VEC_REVERSE_TEST tests I4VEC_REVERSE;
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

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'i4VEC_REVERSE_TEST'
  write ( *, '(a)' ) '  I4VEC_REVERSE reverses an integer vector.'
  write ( *, '(a)' ) ''

  seed = 123456789

  call i4vec_uniform_ab ( n, 1, n, seed, a )

  call i4vec_print ( n, a, '  The integer array:' )
 
  call i4vec_reverse ( n, a )

  call i4vec_print ( n, a, '  The reversed integer array:' )

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
!    29 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORT_BUBBLE_A_TEST'
  write ( *, '(a)' ) '  I4VEC_SORT_BUBBLE_A ascending sorts an integer vector'
  write ( *, '(a)' ) '  using bubble sort.'
  write ( *, '(a)' ) ''

  seed = 123456789

  call i4vec_uniform_ab ( n, 0, 3*n, seed, a )

  call i4vec_print ( n, a, '  Unsorted array:' )

  call i4vec_sort_bubble_a ( n, a )

  call i4vec_print ( n, a, '  Sorted array:' )

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
!    29 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_SORT_HEAP_INDEX_D_TEST'
  write ( *, '(a)' ) '  I4VEC_SORT_HEAP_INDEX_D descending index-sorts'
  write ( *, '(a)' ) '  an integer vector using heap sort.'
  write ( *, '(a)' ) ''

  seed = 123456789

  call i4vec_uniform_ab ( n, 0, 3*n, seed, a )

  call i4vec_print ( n, a, '  Unsorted array:' )

  call i4vec_sort_heap_index_d ( n, a, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       I    INDX    A(INDX)'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,3i8)' ) i, indx(i), a(indx(i))
  end do

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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4VEC_TRANSPOSE_PRINT_TEST'
  write ( *, '(a)' ) '  I4VEC_TRANSPOSE_PRINT prints an I4VEC'
  write ( *, '(a)' ) '  with 5 entries to a row, and an optional title.'

  call i4vec_indicator1 ( n, a )

  call i4vec_transpose_print ( n, a, &
    '  Array printed by I4VEC_TRANSPOSE_PRINT:  ' )

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
subroutine index_box_next_2d_test ( )

!*****************************************************************************80
!
!! INDEX_BOX_NEXT_2D_TEST tests INDEX_BOX_NEXT_2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  logical more
  integer ( kind = 4 ), parameter :: n1 = 5
  integer ( kind = 4 ), parameter :: n2 = 3
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_BOX_NEXT_2D_TEST'
  write ( *, '(a)' ) '  INDEX_BOX_NEXT_2D produces IJ indices that'
  write ( *, '(a)' ) '  lie on the surface of a box in 2D.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The box has logical dimensions:'
  write ( *, '(3x,3i3)' ) n1, n2
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   #    I   J'
  write ( *, '(a)' ) ''

  more = .false.
  n = 0

  do

    call index_box_next_2d ( n1, n2, i, j, more )

    if ( .not. more ) then
      exit
    end if

    n = n + 1
    write ( *, '(2x,4i3)' ) n, i, j

  end do

  return
end
subroutine index_box_next_3d_test ( )

!*****************************************************************************80
!
!! INDEX_BOX_NEXT_3D_TEST tests INDEX_BOX_NEXT_3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  logical more
  integer ( kind = 4 ), parameter :: n1 = 5
  integer ( kind = 4 ), parameter :: n2 = 3
  integer ( kind = 4 ), parameter :: n3 = 4
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_BOX_NEXT_3D_TEST'
  write ( *, '(a)' ) '  INDEX_BOX_NEXT_3D produces IJK indices that'
  write ( *, '(a)' ) '  lie on the surface of a box.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The box has logical dimensions:'
  write ( *, '(3x,3i3)' ) n1, n2, n3
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     #    I   J   K'
  write ( *, '(a)' ) ''

  more = .false.
  n = 0

  do

    call index_box_next_3d ( n1, n2, n3, i, j, k, more )

    if ( .not. more ) then
      exit
    end if

    n = n + 1
    write ( *, '(2x,4i3)' ) n, i, j, k

  end do

  return
end
subroutine index_box2_next_2d_test ( )

!*****************************************************************************80
!
!! INDEX_BOX2_NEXT_2D_TEST tests INDEX_BOX2_NEXT_2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: ic = 10
  integer ( kind = 4 ) j
  integer ( kind = 4 ), parameter :: jc = 20
  logical more
  integer ( kind = 4 ), parameter :: n1 = 4
  integer ( kind = 4 ), parameter :: n2 = 3
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_BOX2_NEXT_2D_TEST'
  write ( *, '(a)' ) '  INDEX_BOX2_NEXT_2D produces IJ indices that'
  write ( *, '(a)' ) '  lie on the surface of a box2 in 2D.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The box has half-widths:'
  write ( *, '(3x,3i3)' ) n1, n2
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  and has center cell:'
  write ( *, '(3x,2i3)' ) ic, jc
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     #    I   J'
  write ( *, '(a)' ) ''

  more = .false.
  n = 0

  do

    call index_box2_next_2d ( n1, n2, ic, jc, i, j, more )

    if ( .not. more ) then
      exit
    end if

    n = n + 1
    write ( *, '(2x,4i3)' ) n, i, j

  end do

  return
end
subroutine index_box2_next_3d_test ( )

!*****************************************************************************80
!
!! INDEX_BOX2_NEXT_3D_TEST tests INDEX_BOX2_NEXT_3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: ic = 10
  integer ( kind = 4 ) j
  integer ( kind = 4 ), parameter :: jc = 20
  integer ( kind = 4 ) k
  integer ( kind = 4 ), parameter :: kc = 30
  logical more
  integer ( kind = 4 ), parameter :: n1 = 5
  integer ( kind = 4 ), parameter :: n2 = 3
  integer ( kind = 4 ), parameter :: n3 = 4
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_BOX2_NEXT_3D_TEST'
  write ( *, '(a)' ) '  INDEX_BOX2_NEXT_3D produces IJK indices that'
  write ( *, '(a)' ) '  lie on the surface of a box.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The box has half widths:'
  write ( *, '(3x,3i3)' ) n1, n2, n3
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  and central cell:'
  write ( *, '(3x,3i3)' ) ic, jc, kc
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  We will only print a PORTION of the data!'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   #    I   J   K'
  write ( *, '(a)' ) ''

  more = .false.
  n = 0

  do

    call index_box2_next_3d ( n1, n2, n3, ic, jc, kc, i, j, k, more )

    if ( .not. more ) then
      exit
    end if

    n = n + 1

    if ( n <= 10 .or. 370 <= n ) then
      write ( *, '(2x,4i3)' ) n, i, j, k
    end if

  end do

  return
end
subroutine index_next0_test ( )

!*****************************************************************************80
!
!! INDEX_NEXT0_TEST tests INDEX_NEXT0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ), parameter :: hi = 3
  logical more

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_NEXT0_TEST'
  write ( *, '(a)' ) '  INDEX_NEXT0 generates all indices of an'
  write ( *, '(a)' ) '  array of given shape, with'
  write ( *, '(a)' ) '  lower limit 1 and given upper limit.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of index entries = ', n
  write ( *, '(a,i8)' ) '  Coordinate maximum HI =   ', hi
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Index arrays:'
  write ( *, '(a)' ) ''

  more = .false.

  do

    call index_next0 ( n, hi, a, more )

    write ( *, '(2x,3i4)' ) a(1:n)

    if ( .not. more ) then
      exit
    end if

  end do

  return
end
subroutine index_next1_test ( )

!*****************************************************************************80
!
!! INDEX_NEXT1_TEST tests INDEX_NEXT1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ), dimension ( n ) :: hi = (/ 4, 2, 3 /)
  logical more

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_NEXT1_TEST'
  write ( *, '(a)' ) '  INDEX_NEXT1 generates all indices of an'
  write ( *, '(a)' ) '  array of given shape, with'
  write ( *, '(a)' ) '  lower limit 1 and given upper limits.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of index entries = ', n

  call i4vec_print ( n, hi, '  Coordinate maximum indices:' )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Index arrays:'
  write ( *, '(a)' ) ''

  more = .false.

  do

    call index_next1 ( n, hi, a, more )

    write ( *, '(2x,3i4)' ) a(1:n)

    if ( .not. more ) then
      exit
    end if

  end do

  return
end
subroutine index_next2_test ( )

!*****************************************************************************80
!
!! INDEX_NEXT2_TEST tests INDEX_NEXT2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ), dimension ( n ) :: hi = (/ 11, -3, 1 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), dimension ( n ) :: lo = (/ 10, -5, 0 /)
  logical more

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_NEXT2_TEST'
  write ( *, '(a)' ) '  INDEX_NEXT2 generates all indices of an'
  write ( *, '(a)' ) '  array of given shape with given'
  write ( *, '(a)' ) '  lower and upper limits.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of index entries = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Coordinate, Maximum Index'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(3i10)' ) i, lo(i), hi(i)
  end do
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Index arrays:'
  write ( *, '(a)' ) ''

  more = .false.

  do

    call index_next2 ( n, lo, hi, a, more )

    write ( *, '(2x,3i4)' ) a(1:n)

    if ( .not. more ) then
      exit
    end if

  end do

  return
end
subroutine index_rank0_test ( )

!*****************************************************************************80
!
!! INDEX_RANK0_TEST tests INDEX_RANK0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ), dimension ( n ) :: a = (/ 3, 1, 2 /)
  integer ( kind = 4 ), parameter :: hi = 3
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_RANK0_TEST'
  write ( *, '(a)' ) '  INDEX_RANK0 ranks an index with'
  write ( *, '(a)' ) '  lower limit 1 and given upper limit.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of index entries = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Coordinate maximum Index = ', hi
  write ( *, '(a)' ) ''

  call i4vec_print ( n, a, '  The index array:' )

  call index_rank0 ( n, hi, a, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The rank of this object is ', rank

  return
end
subroutine index_rank1_test ( )

!*****************************************************************************80
!
!! INDEX_RANK1_TEST tests INDEX_RANK1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ), dimension ( n ) :: a = (/ 4, 1, 2 /)
  integer ( kind = 4 ), dimension ( n ) :: hi = (/ 4, 2, 3 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_RANK1_TEST'
  write ( *, '(a)' ) '  INDEX_RANK1 ranks an index with'
  write ( *, '(a)' ) '  lower limit 1 and given upper limits.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of index entries = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Coordinate, Maximum Index'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,2i10)' ) i, hi(i)
  end do
 
  call i4vec_print ( n, a, '  The index array:' )

  call index_rank1 ( n, hi, a, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The rank of this object is ', rank

  return
end
subroutine index_rank2_test ( )

!*****************************************************************************80
!
!! INDEX_RANK2_TEST tests INDEX_RANK2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ), dimension ( n ) :: a = (/ 1, 11, 5 /)
  integer ( kind = 4 ), dimension ( n ) :: hi = (/ 2, 11, 6 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), dimension ( n ) :: lo = (/ 1, 10, 4 /)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_RANK2_TEST'
  write ( *, '(a)' ) '  INDEX_RANK2 ranks an index with given'
  write ( *, '(a)' ) '  lower and upper limits.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of index entries = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Coordinate, Minimum index, Maximum Index'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,3i10)' ) i, lo, hi(i)
  end do
 
  call i4vec_print ( n, a, '  The index array:' )

  call index_rank2 ( n, lo, hi, a, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The rank of this object is ', rank

  return
end
subroutine index_unrank0_test ( )

!*****************************************************************************80
!
!! INDEX_UNRANK0_TEST tests INDEX_UNRANK0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ), parameter :: hi = 3
  integer ( kind = 4 ) maxrank
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_UNRANK0_TEST'
  write ( *, '(a)' ) '  INDEX_UNRANK0 unranks a multi-index.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The multi-index has dimension ', n
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The upper limit is HI = ', hi
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank, Multi-Index:'
  write ( *, '(a)' ) ''
 
  maxrank = hi**n

  do rank = 1, maxrank
    call index_unrank0 ( n, hi, rank, a )
    write ( *, '(2x,i3,3i8)' ) rank, a(1:n)
  end do
 
  return
end
subroutine index_unrank1_test ( )

!*****************************************************************************80
!
!! INDEX_UNRANK1_TEST tests INDEX_UNRANK1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ), dimension ( n ) :: hi = (/ 4, 2, 3 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) maxrank
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_UNRANK1_TEST'
  write ( *, '(a)' ) '  INDEX_UNRANK1 unranks a multi-index.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The multi-index has dimension ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The upper limits are:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,2i10)' ) i, hi(i)
  end do
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank, Multi-Index:'
  write ( *, '(a)' ) ''
 
  maxrank = product ( hi )

  do rank = 1, maxrank
    call index_unrank1 ( n, hi, rank, a )
    write ( *, '(2x,i3,3i8)' ) rank, a(1:n)
  end do
 
  return
end
subroutine index_unrank2_test ( )

!*****************************************************************************80
!
!! INDEX_UNRANK2_TEST tests INDEX_UNRANK2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ), dimension ( n ) :: hi = (/ 2, 11, 6 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), dimension (n) :: lo = (/ 1, 10, 4 /)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INDEX_UNRANK2_TEST'
  write ( *, '(a)' ) '  INDEX_UNRANK2 unranks a multi-index.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The multi-index has dimension ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The lower and upper limits are:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,3i10)' ) i, lo(i), hi(i)
  end do
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank, Multi-Index:'
  write ( *, '(a)' ) ''
 
  rank = 7

  call index_unrank2 ( n, lo, hi, rank, a )
  write ( *, '(2x,i3,3i8)' ) rank, a(1:n)
 
  return
end
subroutine inversion_to_perm1_test ( )

!*****************************************************************************80
!
!! INVERSION_TO_PERM1_TEST tests INVERSION_TO_PERM1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ins(n)
  integer ( kind = 4 ) perm(n)
  integer ( kind = 4 ) perm2(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INVERSION_TO_PERM1_TEST'
  write ( *, '(a)' ) '  INVERSION_TO_PERM1: inversion => permutation (1,...,N).'
  write ( *, '(a)' ) ''

  perm(1:n) = (/ 3, 5, 1, 4, 2 /)

  call perm1_to_inversion ( n, perm, ins )

  call inversion_to_perm1 ( n, ins, perm2 )

  write ( *, '(2x,6i3)' ) ( i, i = 1, n )
  write ( *, '(2x,6i3)' ) perm(1:n)
  write ( *, '(2x,6i3)' ) ins(1:n)
  write ( *, '(2x,6i3)' ) perm2(1:n)
 
  return
end
subroutine involute_enum_test ( )

!*****************************************************************************80
!
!! INVOLUTE_ENUM_TEST tests INVOLUTE_ENUM;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  real ( kind = 8 ) prob
  real ( kind = 8 ) r8_factorial
  integer ( kind = 4 ) s(0:n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INVOLUTE_ENUM_TEST'
  write ( *, '(a)' ) '  INVOLUTE_ENUM counts involutions;'
  write ( *, '(a)' ) ''

  call involute_enum ( n, s )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N    Number   Probability'
  write ( *, '(a)' ) ''

  do i = 0, n
    prob = real ( s(i), kind = 8 ) / r8_factorial ( i )
    write ( *, '(2x,i8,2x,i8,2x,g14.6)' ) i, s(i), prob
  end do

  return
end
subroutine inverse_mod_n_test ( )

!*****************************************************************************80
!
!! INVERSE_MOD_N_TEST tests INVERSE_MOD_N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 November 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) b
  integer ( kind = 4 ) n
  integer ( kind = 4 ) y
  integer ( kind = 4 ) z

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INVERSE_MOD_N_TEST'
  write ( *, '(a)' ) '  INVERSE_MOD_N seeks Y, the inverse of B mod N,'
  write ( *, '(a)' ) '  so that mod ( B * Y, N ) = 1, but returns 0'
  write ( *, '(a)' ) '  if the inverse does not exist.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     B     N     Y     Z = mod ( B * Y, N )'
  write ( *, '(a)' ) ''

  do n = 1, 10
    do b = 1, n - 1
      call inverse_mod_n ( b, n, y )
      z = mod ( b * y, n )
      write ( *, '(2x,i4,2x,i4,2x,i4,2x,i4)' ) b, n, y, z
    end do
  end do

  return
end
subroutine jfrac_to_rfrac_test ( )

!*****************************************************************************80
!
!! JFRAC_TO_RFRAC_TEST tests JFRAC_TO_RFRAC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 October 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxm = 10

  integer ( kind = 4 ) m
  real ( kind = 8 ) p(maxm)
  real ( kind = 8 ) q(maxm)
  real ( kind = 8 ) r(maxm)
  real ( kind = 8 ) s(maxm)
  integer ( kind = 4 ) seed
!
!  Generate the data, but force Q(M+1) to be 1.  
!  That will make it easier to see that the two operations are inverses
!  of each other.  JFRAC_TO_RFRAC is free to scale its output, and chooses
!  a scaling in which Q(M+1) is 1.
!
  seed = 123456789
  m = 6
  call r8vec_uniform_01 ( m, seed, p )
  call r8vec_uniform_01 ( m + 1, seed, q )

  q(1:m+1) = q(1:m+1) / q(m+1)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'JFRAC_TO_RFRAC_TEST'
  write ( *, '(a)' ) '  JFRAC_TO_RFRAC converts a J fraction'
  write ( *, '(a)' ) '  to a rational polynomial fraction.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The original rational polynomial coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) p(1:m)
  write ( *, '(2x,5g14.6)' ) q(1:m+1)
 
  call rfrac_to_jfrac ( m, p, q, r, s )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The J fraction coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) r(1:m)
  write ( *, '(2x,5g14.6)' ) s(1:m)
 
  call jfrac_to_rfrac ( m, r, s, p, q )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The recovered rational polynomial:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) p(1:m)
  write ( *, '(2x,5g14.6)' ) q(1:m+1)

  return
end
subroutine josephus_test ( )

!*****************************************************************************80
!
!! JOSEPHUS_TEST tests JOSEPHUS.
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

  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'JOSEPHUS_TEST'
  write ( *, '(a)' ) '  JOSEPHUS solves Josephus problems.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    N    M    K	 X'
  write ( *, '(a)' ) ''

  m = 3
  n = 41
  k = 41
  call josephus ( n, m, k, x )
  write ( *, '(2x,4i5)' ) n, m, k, x

  m = -38
  n = 41
  k = 41
  call josephus ( n, m, k, x )

  write ( *, '(2x,4i5)' ) n, m, k, x

  m = 3
  n = 41
  k = 40
  call josephus ( n, m, k, x )

  write ( *, '(2x,4i5)' ) n, m, k, x

  m = 2
  n = 64
  k = 64
  call josephus ( n, m, k, x )

  write ( *, '(2x,4i5)' ) n, m, k, x

  m = 2
  n = 1000
  k = 1000
  call josephus ( n, m, k, x )

  write ( *, '(2x,4i5)' ) n, m, k, x

  return
end
subroutine ksub_next_test ( )

!*****************************************************************************80
!
!! KSUB_NEXT_TEST tests KSUB_NEXT.
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

  integer ( kind = 4 ) a(3)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) m2
  logical more
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank


  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_NEXT_TEST'
  write ( *, '(a)' ) '  KSUB_NEXT generates all K subsets of an N set'
  write ( *, '(a)' ) '  in lexicographic order.'
  write ( *, '(a)' ) ''

  n = 5
  k = 3
  a(1:k) = 0
  more = .false.
  m = 0
  m2 = 0

  rank = 0
 
  do

    call ksub_next ( n, k, a, more, m, m2 )

    rank = rank + 1
    write ( *, '(2x,i4,4x,8i4)' ) rank, a(1:k)

    if ( .not. more ) then
      exit
    end if

  end do
 
  return
end
subroutine ksub_next2_test ( )

!*****************************************************************************80
!
!! KSUB_NEXT2_TEST tests KSUB_NEXT2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) in
  integer ( kind = 4 ) iout
  logical more
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_NEXT2_TEST'
  write ( *, '(a)' ) '  KSUB_NEXT2 generates the next K subset of an'
  write ( *, '(a)' ) '  N set by the revolving door method.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank  Subset  Added  Removed'
  write ( *, '(a)' ) ''
!
!  KSUB_NEXT2 doesn't have a good way of stopping.  
!  We will save the starting subset, and stop when the
!  new subset is the same as the starting one.
!
  in = 0
  iout = 0
  rank = 0
 
  call i4vec_indicator1 ( k, a )
 
  do
 
    rank = rank + 1
    write ( *, '(2x,i4,2x,3i2,3x,i2,7x,i2)' ) rank, a(1:k), in, iout
 
    call ksub_next2 ( n, k, a, in, iout )
 
    more = .false.

    do i = 1, k
      if ( a(i) /= i ) then
        more = .true.
      end if
    end do

    if ( .not. more ) then
      exit
    end if

  end do
 
  return
end
subroutine ksub_next3_test ( )

!*****************************************************************************80
!
!! KSUB_NEXT3_TEST tests KSUB_NEXT3.
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
  integer ( kind = 4 ) in
  integer ( kind = 4 ) iout
  logical more
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_NEXT3_TEST'
  write ( *, '(a)' ) '  KSUB_NEXT3 generates all K subsets of an N set'
  write ( *, '(a)' ) '  using the revolving door method.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank    Subset  Added Removed'
  write ( *, '(a)' ) ''

  rank = 0
  more = .false.
 
  do

    call ksub_next3 ( n, k, a, more, in, iout )

    rank = rank + 1
    write ( *, '(2x,i4,2x,3i2,3x,i2,7x,i2)' ) rank, a(1:k), in, iout

    if ( .not. more ) then
      exit
    end if

  end do

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
subroutine ksub_random_test ( )

!*****************************************************************************80
!
!! KSUB_RANDOM_TEST tests KSUB_RANDOM.
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
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_RANDOM_TEST'
  write ( *, '(a)' ) '  KSUB_RANDOM generates a random K subset of an N set.'
  write ( *, '(a,i8)' ) '  Set size is N =    ', n
  write ( *, '(a,i8)' ) '  Subset size is K = ', k
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10

    call ksub_random ( n, k, seed, a )

    write ( *, '(2x,8i3)' ) a(1:k)

  end do
 
  return
end
subroutine ksub_random2_test ( )

!*****************************************************************************80
!
!! KSUB_RANDOM2_TEST tests KSUB_RANDOM2.
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
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_RANDOM2_TEST'
  write ( *, '(a)' ) '  KSUB_RANDOM2 generates a random K subset of an N set.'
  write ( *, '(a,i8)' ) '  Set size is N =    ', n
  write ( *, '(a,i8)' ) '  Subset size is K = ', k
  write ( *, '(a)' ) ''
 
  seed = 123456789

  do i = 1, 10
    call ksub_random2 ( n, k, seed, a )
    write ( *, '(2x,8i3)' ) a(1:k)
  end do
 
  return
end
subroutine ksub_random3_test ( )

!*****************************************************************************80
!
!! KSUB_RANDOM3_TEST tests KSUB_RANDOM3.
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
  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_RANDOM3_TEST'
  write ( *, '(a)' ) '  KSUB_RANDOM3 generates a random K-subset of an N-set.'
  write ( *, '(a,i8)' ) '  Set size is N =    ', n
  write ( *, '(a,i8)' ) '  Subset size is K = ', k
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10
    call ksub_random3 ( n, k, seed, b )
    l = 0
    do j = 1, n
      if ( b(j) == 1 ) then
        l = l + 1
        a(l) = j
      end if
    end do
    write ( *, '(2x,15i3)' ) a(1:k)
  end do
 
  return
end
subroutine ksub_random4_test ( )

!*****************************************************************************80
!
!! KSUB_RANDOM4_TEST tests KSUB_RANDOM4.
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
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_RANDOM4_TEST'
  write ( *, '(a)' ) '  KSUB_RANDOM4 generates a random K subset of an N set.'
  write ( *, '(a,i8)' ) '  Set size is N =    ', n
  write ( *, '(a,i8)' ) '  Subset size is K = ', k
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10

    call ksub_random4 ( n, k, seed, a )

    write ( *, '(2x,8i3)' ) a(1:k)

  end do
 
  return
end
subroutine ksub_random5_test ( )

!*****************************************************************************80
!
!! KSUB_RANDOM5_TEST tests KSUB_RANDOM5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 June 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_RANDOM5_TEST'
  write ( *, '(a)' ) '  KSUB_RANDOM5 generates a random K subset of an N set.'
  write ( *, '(a,i8)' ) '  Set size is N =    ', n
  write ( *, '(a,i8)' ) '  Subset size is K = ', k
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10

    call ksub_random5 ( n, k, seed, a )

    write ( *, '(2x,8i3)' ) a(1:k)

  end do
 
  return
end
subroutine ksub_rank_test ( )

!*****************************************************************************80
!
!! KSUB_RANK_TEST tests KSUB_RANK.
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

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ), dimension ( k ) :: a = (/ 1, 3, 5 /)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_RANK_TEST'
  write ( *, '(a)' ) '  KSUB_RANK: rank of a K subset of an N set.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  For N = ', n
  write ( *, '(a,i8)' ) '  and K = ', k
  write ( *, '(a)' ) '  the subset is:'
  write ( *, '(5i4)' ) a(1:k)
 
  call ksub_rank ( k, a, rank )
 
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The rank is ', rank
 
  return
end
subroutine ksub_to_comp_test ( )

!*****************************************************************************80
!
!! KSUB_TO_COMP_TEST tests COMP_TO_KSUB and KSUB_TO_COMP.
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

  integer ( kind = 4 ) ac(5)
  integer ( kind = 4 ) as(4)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) ks
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) ns
  integer ( kind = 4 ) seed
  
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_TO_COMP_TEST'
  write ( *, '(a)' ) &
    '  COMP_TO_KSUB returns the K subset corresponding to a composition.'
  write ( *, '(a)' ) &
    '  KSUB_TO_COMP returns the composition corresponding to a K subset.'

  nc = 10
  kc = 5
  seed = 123456789

  do i = 1, 5

    write ( *, '(a)' ) ''

    call comp_random ( nc, kc, seed, ac )
    write ( *, '(a,5(i4))' ) '  COMP:', ac(1:kc)

    call comp_to_ksub ( nc, kc, ac, ns, ks, as )
    write ( *, '(a,4(i4))' ) '  KSUB:', as(1:ks)

    call ksub_to_comp ( ns, ks, as, nc, kc, ac )
    write ( *, '(a,5(i4))' ) '  COMP:', ac(1:kc)
    
  end do

  return
end
subroutine ksub_to_compnz_test ( )

!*****************************************************************************80
!
!! KSUB_TO_COMPNZ_TEST tests KSUB_TO_COMPNZ.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) ac(5)
  integer ( kind = 4 ) as(4)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) ks
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) ns
  integer ( kind = 4 ) seed
  
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_TO_COMPNZ_TEST'
  write ( *, '(a)' ) '  KSUB_TO_COMPNZ returns the nonzero composition'
  write ( *, '(a)' ) '  corresponding to a K subset.'

  ns = 14
  ks = 4
  seed = 123456789

  do i = 1, 5

    write ( *, '(a)' ) ''

    call ksub_random2 ( ns, ks, seed, as )
    write ( *, '(a,4(i4))' ) '  KSUB:  ', as(1:ks)

    call ksub_to_compnz ( ns, ks, as, nc, kc, ac )
    write ( *, '(a,5(i4))' ) '  COMPNZ:', ac(1:kc)

    call compnz_to_ksub ( nc, kc, ac, ns, ks, as )
    write ( *, '(a,4(i4))' ) '  KSUB:  ', as(1:ks)

  end do

  return
end
subroutine ksub_unrank_test ( )

!*****************************************************************************80
!
!! KSUB_UNRANK_TEST tests KSUB_UNRANK.
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
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ) rank

  rank = 8
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_UNRANK_TEST'
  write ( *, '(a)' ) '  KSUB_UNRANK: find the K-subset of an N set'
  write ( *, '(a)' ) '  of a given rank.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  N is ', n
  write ( *, '(a,i8)' ) '  K is ', k
  write ( *, '(a,i8)' ) '  and the desired rank is ', rank
 
  call ksub_unrank ( k, rank, a )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The subset of the given rank is:'
  write ( *, '(2x,5i4)' ) a(1:k)
 
  return
end
subroutine l4vec_next_test ( )

!*****************************************************************************80
!
!! L4VEC_NEXT_TEST tests L4VEC_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  logical ( kind = 4 ) l4vec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'L4VEC_NEXT_TEST'
  write ( *, '(a)' ) '  L4VEC_NEXT generates logical vectors.'
  write ( *, '(a)' ) ''
 
  l4vec(1:n) = .false.

  do

    write ( *, '(2x,3l1)' ) l4vec(1:n)

    if ( all ( l4vec(1:n) ) ) then
      exit
    end if

    call l4vec_next ( n, l4vec )
 
  end do

  return
end
subroutine matrix_product_opt_test ( )

!*****************************************************************************80
!
!! MATRIX_PRODUCT_OPT_TEST tests MATRIX_PRODUCT_OPT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ) cost
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order(n-1)
  integer ( kind = 4 ), dimension ( n+1) :: rank = (/ 4, 2, 3, 1, 2, 2, 3 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MATRIX_PRODUCT_OPT_TEST'
  write ( *, '(a)' ) '  MATRIX_PRODUCT_OPT seeks the optimal order'
  write ( *, '(a)' ) '  for a chain of matrix products.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Matrix ranks:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I         R         C'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) i, rank(i), rank(i+1)
  end do

  call matrix_product_opt ( n, rank, cost, order )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Optimal cost is ', cost

  call i4vec_print ( n-1, order, '  Ordering:' )

  return
end
subroutine moebius_matrix_test ( )

!*****************************************************************************80
!
!! MOEBIUS_MATRIX_TEST tests MOEBIUS_MATRIX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11

  integer ( kind = 4 ), dimension ( n, n ) :: ih = reshape (&
    (/ &
    0,0,1,1,0,0,0,0,0,0,0, &
    0,0,0,0,0,0,0,1,0,0,0, &
    0,1,0,0,0,0,0,0,0,0,0, &
    0,1,0,0,0,0,0,0,0,0,0, &
    0,0,0,1,0,0,0,0,0,0,0, &
    1,0,0,0,1,0,0,0,1,0,0, &
    0,0,0,0,0,1,0,0,0,1,1, &
    0,0,0,0,0,0,0,0,0,0,0, &
    0,0,1,1,0,0,0,0,0,0,0, &
    1,0,0,0,0,0,0,0,1,0,0, &
    0,0,0,0,0,0,0,0,1,0,0 /), (/ n, n /) )
  integer ( kind = 4 ) matrix(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MOEBIUS_MATRIX_TEST'
  write ( *, '(a)' ) '  MOEBIUS_MATRIX computes the Moebius matrix.'
 
  call i4mat_print ( n, n, ih, '  The input matrix:' )

  call moebius_matrix ( n, ih, matrix )
 
  call i4mat_print ( n, n, matrix, '  The Moebius matrix:' )
 
  return
end
subroutine monomial_count_test ( )

!*****************************************************************************80
!
!! MONOMIAL_COUNT_TEST tests MONOMIAL_COUNT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: degree_max = 9

  integer ( kind = 4 ) dim
  integer ( kind = 4 ) total

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MONOMIAL_COUNT_TEST'
  write ( *, '(a)' ) '  MONOMIAL_COUNT counts the number of monomials of'
  write ( *, '(a)' ) '  degrees 0 through DEGREE_MAX in a space of dimension DIM.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  DIM    Count'
  write ( *, '(a)' ) ''

  do dim = 1, 6

    call monomial_count ( degree_max, dim, total )
    write ( *, '(2x,i4,2x,i8)' ) dim, total

  end do

  return
end
subroutine monomial_counts_test ( )

!*****************************************************************************80
!
!! MONOMIAL_COUNTS_TEST tests MONOMIAL_COUNTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: degree_max = 9

  integer ( kind = 4 ) counts(0:degree_max)
  integer ( kind = 4 ) degree
  integer ( kind = 4 ) dim

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MONOMIAL_COUNTS_TEST'
  write ( *, '(a)' ) '  MONOMIAL_COUNTS counts the number of monomials of'
  write ( *, '(a)' ) '  each degree 0 through DEGREE_MAX in a space of dimension DIM.'

  do dim = 1, 6

    call monomial_counts ( degree_max, dim, counts )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  DIM = ', dim
    write ( *, '(a)' ) ''
    do degree = 0, degree_max
      write ( *, '(2x,i8,2x,i8)' ) degree, counts(degree)
    end do
    write ( *, '(a)' ) ''
    write ( *, '(2x,a8,2x,i8)' ) &
      '   Total', sum ( counts(0:degree_max) )

  end do

  return
end
subroutine morse_thue_test ( )

!*****************************************************************************80
!
!! MORSE_THUE_TEST tests MORSE_THUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 100

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) s(0:n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MORSE_THUE_TEST'
  write ( *, '(a)' ) '  MORSE_THUE computes the Morse-Thue numbers.'
  write ( *, '(a)' ) ''

  do i = 0, n
    call morse_thue ( i, s(i) )
  end do

  do ilo = 0, n, 10
    ihi = min ( ilo + 9, n )
    write ( *, '(4x,40i1)' ) s(ilo:ihi)
  end do

  return
end
subroutine multinomial_coef1_test ( )

!*****************************************************************************80
!
!! MULTINOMIAL_COEF1_TEST tests MULTINOMIAL_COEF12.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxfactor = 5

  integer ( kind = 4 ) factor(maxfactor)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncomb1
  integer ( kind = 4 ) nfactor

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MULTINOMIAL_COEF1_TEST'
  write ( *, '(a)' ) '  MULTINOMIAL_COEF1 computes multinomial'
  write ( *, '(a)' ) '  coefficients using the Gamma function;'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Line 10 of the BINOMIAL table:'
  write ( *, '(a)' ) ''

  n = 10
  nfactor = 2

  do i = 0, n

    factor(1) = i
    factor(2) = n - i

    call multinomial_coef1 ( nfactor, factor, ncomb1 )

    write ( *, '(2x,i4,i4,3x,i5)' ) factor(1), factor(2), ncomb1

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Level 5 of the TRINOMIAL coefficients:'

  n = 5
  nfactor = 3

  do i = 0, n

    factor(1) = i

    write ( *, '(a)' ) ''

    do j = 0, n - factor(1)

      factor(2) = j
      factor(3) = n - factor(1) - factor(2)

      call multinomial_coef1 ( nfactor, factor, ncomb1 )

      write ( *, '(2x,i4,i4,i4,3x,i5)' ) factor(1), factor(2), factor(3), &
        ncomb1

    end do

  end do

  return
end
subroutine multinomial_coef2_test ( )

!*****************************************************************************80
!
!! MULTINOMIAL_COEF2_TEST tests MULTINOMIAL_COEF2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxfactor = 5

  integer ( kind = 4 ) factor(maxfactor)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncomb2
  integer ( kind = 4 ) nfactor

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MULTINOMIAL_COEF2_TEST'
  write ( *, '(a)' ) '  MULTINOMIAL_COEF2 computes multinomial'
  write ( *, '(a)' ) '  coefficients directly.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Line 10 of the BINOMIAL table:'
  write ( *, '(a)' ) ''

  n = 10
  nfactor = 2

  do i = 0, n

    factor(1) = i
    factor(2) = n - i

    call multinomial_coef2 ( nfactor, factor, ncomb2 )

    write ( *, '(2x,i4,i4,3x,i5)' ) factor(1), factor(2), ncomb2

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Level 5 of the TRINOMIAL coefficients:'

  n = 5
  nfactor = 3

  do i = 0, n

    factor(1) = i

    write ( *, '(a)' ) ''

    do j = 0, n - factor(1)

      factor(2) = j
      factor(3) = n - factor(1) - factor(2)

      call multinomial_coef2 ( nfactor, factor, ncomb2 )

      write ( *, '(2x,i4,i4,i4,3x,i5)' ) factor(1), factor(2), factor(3), &
        ncomb2

    end do

  end do

  return
end
subroutine multiperm_enum_test ( )

!*****************************************************************************80
!
!! MULTIPERM_ENUM_TEST tests MULTIPERM_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) counts(n)
  integer ( kind = 4 ), parameter :: i4_1 = 1
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) k
  integer ( kind = 4 ) number
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ) :: test_num = 5
  
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MULTIPERM_ENUM_TEST'
  write ( *, '(a)' ) '  MULTIPERM_ENUM enumerates multipermutations.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  N is the number of objects to be permuted.'
  write ( *, '(a)' ) '  K is the number of distinct types of objects.'
  write ( *, '(a)' ) '  COUNTS is the number of objects of each type.'
  write ( *, '(a)' ) '  NUMBER is the number of multipermutations.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Number       N       K       Counts(1:K)'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    k = i4_uniform_ab ( i4_1, n, seed )

    call compnz_random ( n, k, seed, counts )

    call multiperm_enum ( n, k, counts, number )

    write ( *, '(2x,i6,2x,i6,2x,i6,5(2x,i4))' ) number, n, k, counts(1:k)

  end do
  
  return
end
subroutine multiperm_next_test ( )

!*****************************************************************************80
!
!! MULTIPERM_NEXT_TEST tests MULTIPERM_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ), dimension ( n ) :: a
  logical more
  integer ( kind = 4 ) tally
  
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MULTIPERM_NEXT_TEST'
  write ( *, '(a)' ) '  MULTIPERM_NEXT computes multipermutations in'
  write ( *, '(a)' ) '  lexical order.'
  write ( *, '(a)' ) ''

  tally = 0
  a(1:n) = (/ 1, 2, 2, 3, 3, 3 /)
  more = .true.
 
  do while ( more )

    tally = tally + 1   

    write ( *, '(2x,i4,2x,6(2x,i2))' ) tally, a(1:n)
 
    call multiperm_next ( n, a, more )
            
  end do
  
  return
end
subroutine network_flow_max_test ( )

!*****************************************************************************80
!
!! NETWORK_FLOW_MAX_TEST tests NETWORK_FLOW_MAX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: i4_2 = 2
  integer ( kind = 4 ), parameter :: nnode = 6
  integer ( kind = 4 ), parameter :: nedge = 20

  integer ( kind = 4 ) i
  integer ( kind = 4 ) icut(nnode)
  integer ( kind = 4 ), dimension ( 2, nedge ) :: icpflo = reshape ( &
    (/ 3,0,7,0,2,0,5,0,4,0,1,0,4,0,2,0,8,0,3,0, &
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 /), (/ i4_2, nedge /) )
  integer ( kind = 4 ), dimension ( 2, nedge ) :: iendpt = reshape ( &
    (/ 1,2, 1,3, 2,3, 2,4, 2,5, 3,4, 3,5, 4,5, 4,6, 5,6, &
    2,1, 3,1, 3,2, 4,2, 5,2, 4,3, 5,3, 5,4, 6,4, 6,5 /), (/ i4_2, nedge /) )
  integer ( kind = 4 ) node_flow(nnode)
  integer ( kind = 4 ) :: sink = 6
  integer ( kind = 4 ) :: source = 1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NETWORK_FLOW_MAX_TEST'
  write ( *, '(a)' ) '  NETWORK_FLOW_MAX finds the maximum flow on a network.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The source is node ', source
  write ( *, '(a,i8)' ) '  The sink is node   ', sink
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Endpoint array:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,20i3)' ) iendpt(1,1:nedge)
  write ( *, '(2x,20i3)' ) iendpt(2,1:nedge)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Input edge capacity array:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,20i3)' ) icpflo(1,1:nedge)
 
  call network_flow_max ( nnode, nedge, iendpt, icpflo, source, &
    sink, icut, node_flow )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Reordered endpoint array:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,20i3)' ) ( iendpt(1,i), i = 1, nedge )
  write ( *, '(2x,20i3)' ) ( iendpt(2,i), i = 1, nedge )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Output edge capacity/flow array:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,20i3)' ) ( icpflo(1,i), i = 1, nedge )
  write ( *, '(2x,20i3)' ) ( icpflo(2,i), i = 1, nedge )

  call i4vec_print ( nnode, icut, '  Minimal node cut vector:' )

  call i4vec_print ( nnode, node_flow, '  Nodal flow vector:' )

  return
end
subroutine nim_sum_test ( )

!*****************************************************************************80
!
!! NIM_SUM_TEST tests NIM_SUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 32

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i1vec(n)
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i2vec(n)
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) i3vec(n)
  integer ( kind = 4 ), parameter :: ihi = 1000
  integer ( kind = 4 ), parameter :: ilo = 0
  integer ( kind = 4 ), parameter :: test_num = 5
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NIM_SUM_TEST'
  write ( *, '(a)' ) '  NIM_SUM computes the Nim sum of two integers.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I    J    Nim(I+J)'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, test_num

    i1 = i4_uniform_ab ( ilo, ihi, seed )
    call ui4_to_ubvec ( i1, n, i1vec )

    i2 = i4_uniform_ab ( ilo, ihi, seed )
    call ui4_to_ubvec ( i2, n, i2vec )

    call nim_sum ( i1, i2, i3 )
    call ui4_to_ubvec ( i3, n, i3vec )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  I1, I2, I3 in decimal:'
    write ( *, '(a)' ) ''
    write ( *, '(i5)' ) i1
    write ( *, '(i5)' ) i2
    write ( *, '(i5)' ) i3
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  I1, I2, I3 in binary:'
    write ( *, '(a)' ) ''
    call ubvec_print ( n, i1vec, '' )
    call ubvec_print ( n, i2vec, '' )
    call ubvec_print ( n, i3vec, '' )

  end do

  return
end
subroutine padovan_test ( )

!*****************************************************************************80
!
!! PADOVAN_TEST tests PADOVAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 15

  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PADOVAN_TEST'
  write ( *, '(a)' ) '  PADOVAN computes the Padovan numbers.'
  write ( *, '(a)' ) ''

  call padovan ( n, p )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       N        P(N)'
  write ( *, '(a)' ) ''

  do i = 0, n - 1
    write ( *, '(2x,i8,2x,i10)' ) i, p(i+1)
  end do

  return
end
subroutine pell_basic_test ( )

!*****************************************************************************80
!
!! PELL_BASIC_TEST tests PELL_BASIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) q
  integer ( kind = 4 ) r
  integer ( kind = 4 ) x0
  integer ( kind = 4 ) y0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PELL_BASIC_TEST'
  write ( *, '(a)' ) '  PELL_BASIC solves the basic Pell equation.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       D       X        Y         R'
  write ( *, '(a)' ) ''

  do d = 2, 20

    call i4_sqrt ( d, q, r )

    if ( r /= 0 ) then

      call pell_basic ( d, x0, y0 )

      r = x0 ** 2 - d * y0 ** 2

      write ( *, '(2x,4i9)' ) d, x0, y0, r

    end if

  end do

  return
end
subroutine pell_next_test ( )

!*****************************************************************************80
!
!! PELL_NEXT_TEST tests PELL_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) q
  integer ( kind = 4 ) r
  integer ( kind = 4 ) x0
  integer ( kind = 4 ) x1
  integer ( kind = 4 ) y0
  integer ( kind = 4 ) y1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PELL_NEXT_TEST'
  write ( *, '(a)' ) '  PELL_NEXT computes the "next" solution of the Pell equation.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       D       X        Y         R'
  write ( *, '(a)' ) ''

  do d = 2, 20

    call i4_sqrt ( d, q, r )

    if ( r /= 0 ) then

      call pell_basic ( d, x0, y0 )

      r = x0**2 - d * y0**2

      write ( *, '(2x,4i9)' ) d, x0, y0, r

      call pell_next ( d, x0, y0, x0, y0, x1, y1 )

      r = x1**2 - d * y1**2

      write ( *, '(2x,9x,3i9)' ) x1, y1, r

    end if

  end do

  return
end
subroutine pent_enum_test ( )

!*****************************************************************************80
!
!! PENT_ENUM_TEST tests PENT_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) pi

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PENT_ENUM_TEST'
  write ( *, '(a)' ) '  PENT_ENUM counts points in pentagons.'
  write ( *, '(a)' ) ''

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  N    Pent(N)'
  write ( *, '(a)' ) ''

  do i = 0, n
    call pent_enum ( i, pi )
    write ( *, '(2x,2i10)' ) i, pi
  end do

  return
end
subroutine perm_ascend_test ( )

!*****************************************************************************80
!
!! PERM_ASCEND_TEST tests PERM_ASCEND.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 9

  integer ( kind = 4 ) length
  integer ( kind = 4 ), dimension ( n ) :: p = (/ 2,3,9,6,7,8,5,4,1 /)
  integer ( kind = 4 ) subseq(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_ASCEND_TEST'
  write ( *, '(a)' ) '  PERM_ASCEND determines the length of the longest'
  write ( *, '(a)' ) '  increasing subsequence in a permutation.'

  call perm1_print ( n, p, '  The permutation:' )

  call perm_ascend ( n, p, length, subseq )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) &
    '  The length of the longest increasing subsequence is ', length

  call i4vec_print ( length, subseq, '  A longest increasing subsequence:' )

  return
end
subroutine perm_fixed_enum_test ( )

!*****************************************************************************80
!
!! PERM_FIXED_ENUM_TEST tests PERM_FIXED_ENUM.
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) fnm
  integer ( kind = 4 ) m

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM_FIXED_ENUM_TEST'
  write ( *, '(a)' ) '  PERM_FIXED_ENUM enumerates the permutations'
  write ( *, '(a)' ) '  of N objects that leave M unchanged.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  For this test, N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  M    F(N,M)'
  write ( *, '(a)' ) ''

  do m = 0, n

    call perm_fixed_enum ( n, m, fnm )
    write ( *, '(2x,i3,2x,i8)' ) m, fnm

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

  call perm0_print ( n, p1, '  Permutation 1:' )
  ierror = perm0_check ( n, p1 )

  call perm0_print ( n, p2, '  Permutation 2:' )
  ierror = perm0_check ( n, p2 )

  call perm0_print ( n, p3, '  Permutation 3:' )
  ierror = perm0_check ( n, p3 )

  return
end
subroutine perm0_print_test ( )

!*****************************************************************************80
!
!! PERM0_PRINT_TEST tests PERM0_PRINT.
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

  integer ( kind = 4 ), dimension ( n ) ::  p = (/ 6, 1, 3, 0, 4, 2, 5 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM0_PRINT_TEST'
  write ( *, '(a)' ) '  PERM0_PRINT prints a permutation.'

  call perm0_print ( n, p, '  The 0-based permutation:' )
  
  return
end
subroutine perm1_break_count_test ( )

!*****************************************************************************80
!
!! PERM1_BREAK_COUNT_TEST tests PERM1_BREAK_COUNT.
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

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ) break_count
  integer ( kind = 4 ), dimension ( n ) :: p = (/ 4, 5, 2, 1, 6, 3 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_BREAK_COUNT_TEST'
  write ( *, '(a)' ) '  PERM1_BREAK_COUNT counts breaks in a permutation of (1,...,N).'
 
  call perm1_print ( n, p, '  The permutation:' )
 
  call perm1_break_count ( n, p, break_count )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The number of breaks is ', break_count

  return
end
subroutine perm1_canon_to_cycle_test ( )

!*****************************************************************************80
!
!! PERM1_CANON_TO_CYCLE_TEST tests PERM1_CANON_TO_CYCLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ), dimension ( n ) :: p1 = (/ 4, 5, 2, 1, 6, 3 /)
  integer ( kind = 4 ), dimension ( n ) :: p2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_CANON_TO_CYCLE_TEST'
  write ( *, '(a)' ) '  PERM1_CANON_TO_CYCLE converts a permutation of (1,...,N) from'
  write ( *, '(a)' ) '  canonical to cycle form.'
 
  call perm1_print ( n, p1, '  The permutation in canonical form:' )
 
  call perm1_canon_to_cycle ( n, p1, p2 )

  call perm1_print ( n, p2, '  The permutation in cycle form:' )
 
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
  write ( *, '(a)' ) '  PERM1_CHECK checks a permutation of (1,...,N).'
  write ( *, '(a)' ) ''

  call perm1_print ( n, p1, '  Permutation 1:' )
  ierror = perm1_check ( n, p1 )

  call perm1_print ( n, p2, '  Permutation 2:' )
  ierror = perm1_check ( n, p2 )

  call perm1_print ( n, p3, '  Permutation 3:' )
  ierror = perm1_check ( n, p3 )

  return
end
subroutine perm1_cycle_test ( )

!*****************************************************************************80
!
!! PERM1_CYCLE_TEST tests PERM1_CYCLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 9

  integer ( kind = 4 ) iopt
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) ncycle
  integer ( kind = 4 ), dimension ( n ) :: p = (/ 2,3,9,6,7,8,5,4,1 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_CYCLE_TEST'
  write ( *, '(a)' ) '  PERM1_CYCLE analyzes a permutation of (1,...,N).'
 
  call perm1_print ( n, p, '  The permutation:' )
 
  iopt = 1
  call perm1_cycle ( n, iopt, p, isgn, ncycle )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  NCYCLE = ', ncycle
  write ( *, '(a,i8)' ) '  ISGN =   ', isgn

  call perm1_print ( n, p, '  The permutation in cycle form:' )
 
  return
end
subroutine perm1_cycle_to_canon_test ( )

!*****************************************************************************80
!
!! PERM1_CYCLE_TO_CANON_TEST tests PERM1_CYCLE_TO_CANON.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ), dimension ( n ) :: p1 = (/ -6, 3, 1, -5, 4, -2 /)
  integer ( kind = 4 ), dimension ( n ) :: p2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_CYCLE_TO_CANON_TEST'
  write ( *, '(a)' ) '  PERM1_CYCLE_TO_CANON converts a permutation of (1,...,N) from'
  write ( *, '(a)' ) '  cycle to canonical form.'
 
  call perm1_print ( n, p1, '  The permutation in cycle form:' )
 
  call perm1_cycle_to_canon ( n, p1, p2 )

  call perm1_print ( n, p2, '  The permutation in canonical form:' )
 
  return
end
subroutine perm1_cycle_to_index_test ( )

!*****************************************************************************80
!
!! PERM1_CYCLE_TO_INDEX_TEST tests PERM1_CYCLE_TO_INDEX.
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

  integer ( kind = 4 ), parameter :: n = 9

  integer ( kind = 4 ), dimension ( n ) :: p1 = (/ 2,3,9,6,7,8,5,4,1 /)
  integer ( kind = 4 ), dimension ( n ) :: p2
  integer ( kind = 4 ), dimension ( n ) :: p3

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_CYCLE_TO_INDEX_TEST'
  write ( *, '(a)' ) '  PERM1_CYCLE_TO_INDEX converts a permutation of (1,...,N) from'
  write ( *, '(a)' ) '  cycle to standard index form.'
 
  call perm1_print ( n, p1, '  The standard index form permutation:' )
 
  call perm1_index_to_cycle ( n, p1, p2 )

  call perm1_print ( n, p2, '  The permutation in cycle form:' )

  call perm1_cycle_to_index ( n, p2, p3 )
 
  call perm1_print ( n, p3, '  The standard index form permutation:' )
 
  return
end
subroutine perm1_distance_test ( )

!*****************************************************************************80
!
!! PERM1_DISTANCE_TEST tests PERM1_DISTANCE
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) k11
  integer ( kind = 4 ) k12
  integer ( kind = 4 ) k13
  integer ( kind = 4 ) k21
  integer ( kind = 4 ) k23
  integer ( kind = 4 ) p1(n)
  integer ( kind = 4 ) p2(n)
  integer ( kind = 4 ) p3(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_DISTANCE_TEST'
  write ( *, '(a)' ) '  PERM1_DISTANCE computes the Ulam metric distance'
  write ( *, '(a)' ) '  between two permutations of (1,...,N).'

  seed = 123456789

  call perm1_random2 ( n, seed, p1 )
  call perm1_print ( n, p1, '  Permutation P1' )
  call perm1_random2 ( n, seed, p2 )
  call perm1_print ( n, p2, '  Permutation P2' )
  call perm1_random2 ( n, seed, p3 )
  call perm1_print ( n, p3, '  Permutation P3' )

  call perm1_distance ( n, p1, p1, k11 )
  call perm1_distance ( n, p1, p2, k12 )
  call perm1_distance ( n, p2, p1, k21 )
  call perm1_distance ( n, p1, p3, k13 )
  call perm1_distance ( n, p2, p3, k23 )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  K(P1,P1) should be 0.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  K(P1,P1) = ', k11
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  K(P1,P2) should equal K(P2,P1).'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  K(P1,P2) = ', k12
  write ( *, '(a,i8)' ) '  K(P2,P1) = ', k21
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  K(P1,P3) <= K(P1,P2) + K(P2,P3).'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  K(P1,P3) = ', k13
  write ( *, '(a,i8)' ) '  K(P1,P2) = ', k12
  write ( *, '(a,i8)' ) '  K(P2,P3) = ', k23
  write ( *, '(a,i8)' ) '  K(P1,P2) + K(P2,P3) = ', k12 + k23

  return
end
subroutine perm1_free_test ( )

!*****************************************************************************80
!
!! PERM1_FREE_TEST tests PERM1_FREE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) ifree(n)
  integer ( kind = 4 ) ipart(n)
  integer ( kind = 4 ) nfree
  integer ( kind = 4 ) npart
  integer ( kind = 4 ), dimension ( n ) :: p = (/ &
    5, 2, 3, 4, 1 /) 

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_FREE_TEST'
  write ( *, '(a)' ) &
    '  PERM1_FREE returns the unused values in a partial permutation of (1,...,N).'

  do npart = 0, n
    ipart(1:npart) = p(1:npart)
    nfree = n - npart
    call perm1_free ( npart, ipart, nfree, ifree )
    call i4vec_transpose_print ( npart, ipart, '  Partial permutation:' )
    call i4vec_transpose_print ( nfree, ifree, '  Values not yet used:' )
  end do

  return
end
subroutine perm1_index_to_cycle_test ( )

!*****************************************************************************80
!
!! PERM1_INDEX_TO_CYCLE_TEST tests PERM1_INDEX_TO_CYCLE.
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

  integer ( kind = 4 ), parameter :: n = 9

  integer ( kind = 4 ), dimension ( n ) :: p1 = (/ 2,3,9,6,7,8,5,4,1 /)
  integer ( kind = 4 ), dimension ( n ) :: p2
  integer ( kind = 4 ), dimension ( n ) :: p3

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_INDEX_TO_CYCLE_TEST'
  write ( *, '(a)' ) '  PERM1_INDEX_TO_CYCLE converts a permutation of (1,...,N) from'
  write ( *, '(a)' ) '  standard index to cycle form.'
 
  call perm1_print ( n, p1, '  The standard index form permutation:' )
 
  call perm1_index_to_cycle ( n, p1, p2 )

  call perm1_print ( n, p2, '  The permutation in cycle form:' )

  call perm1_cycle_to_index ( n, p2, p3 )
 
  call perm1_print ( n, p3, '  The standard index form permutation:' )
 
  return
end
subroutine perm1_inverse_test ( )

!*****************************************************************************80
!
!! PERM1_INVERSE_TEST tests PERM1_INVERSE;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ), dimension ( n ) :: p1 = (/ 4, 3, 5, 1, 7, 6, 2 /)
  integer ( kind = 4 ) p2(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_INVERSE_TEST'
  write ( *, '(a)' ) '  PERM1_INVERSE inverts a permutation of (1,...,N)'
  write ( *, '(a)' ) ''

  call perm1_print ( n, p1, '  The original permutation:' )
 
  call perm1_inverse ( n, p1, p2 )
 
  call perm1_print ( n, p2, '  The inverted permutation:' )
 
  return
end
subroutine perm1_inverse2_test ( )

!*****************************************************************************80
!
!! PERM1_INVERSE2_TEST tests PERM1_INVERSE2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ), dimension ( n ) :: p = (/ 4, 3, 5, 1, 7, 6, 2 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_INVERSE2_TEST'
  write ( *, '(a)' ) '  PERM1_INVERSE2 inverts a permutation of (1,...,N).'

  call perm1_print ( n, p, '  The original permutation:' )
 
  call perm1_inverse2 ( n, p )
 
  call perm1_print ( n, p, '  The inverted permutation:' )
 
  return
end
subroutine perm1_inverse3_test ( )

!*****************************************************************************80
!
!! PERM1_INVERSE3_TEST tests PERM1_INVERSE3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ), dimension ( n ) :: p = (/ 4, 3, 5, 1, 7, 6, 2 /)
  integer ( kind = 4 ), dimension ( n ) :: p_inv

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_INVERSE3_TEST'
  write ( *, '(a)' ) '  PERM1_INVERSE3 inverts a permutation of (1,...,N).'

  call perm1_print ( n, p, '  The original permutation:' )
 
  call perm1_inverse3 ( n, p, p_inv )
 
  call perm1_print ( n, p_inv, '  The inverted permutation:' )
 
  return
end
subroutine perm1_lex_next_test ( )

!*****************************************************************************80
!
!! PERM1_LEX_NEXT_TEST tests PERM1_LEX_NEXT.
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

  integer ( kind = 4 ), parameter :: n = 4

  logical more
  integer ( kind = 4 ) p(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_LEX_NEXT_TEST'
  write ( *, '(a)' ) '  PERM1_LEX_NEXT generates permutations of (1,...,N).'
  write ( *, '(a)' ) ''
  more = .false.
 
  do

    call perm1_lex_next ( n, p, more )

    if ( .not. more ) then
      exit
    end if

    call perm1_print ( n, p, '' )

  end do
 
  return
end
subroutine perm1_mul_test ( )

!*****************************************************************************80
!
!! PERM1_MUL_TEST tests PERM1_MUL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) p1(n)
  integer ( kind = 4 ) p2(n)
  integer ( kind = 4 ) p3(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_MUL_TEST'
  write ( *, '(a)' ) '  PERM1_MUL multiplies two permutations of (1,...,N).'
  write ( *, '(a)' ) ''

  seed = 123456789

  call perm1_random ( n, seed, p1 )
  call perm1_random ( n, seed, p2 )

  call perm1_print ( n, p1, '  Permutation P1:' )

  call perm1_print ( n, p2, '  Permutation P2:' )

  call perm1_mul ( n, p1, p2, p3 )

  call perm1_print ( n, p3, '  Product permutation:' )

  return
end
subroutine perm1_next_test ( )

!*****************************************************************************80
!
!! PERM1_NEXT_TEST tests PERM1_NEXT.
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

  integer ( kind = 4 ), parameter :: n = 4

  logical even
  logical more
  integer ( kind = 4 ) p(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_NEXT_TEST'
  write ( *, '(a)' ) '  PERM1_NEXT generates permutations of (1,...,N).'
  write ( *, '(a)' ) ''
  more = .false.
 
  do

    call perm1_next ( n, p, more, even )

    call perm1_print ( n, p, '' )

    if ( .not. more ) then
      exit
    end if
 
  end do

  return
end
subroutine perm1_next2_test ( )

!*****************************************************************************80
!
!! PERM1_NEXT2_TEST tests PERM1_NEXT2.
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

  integer ( kind = 4 ), parameter :: n = 4

  logical done
  integer ( kind = 4 ) iactiv(n)
  integer ( kind = 4 ) idir(n)
  integer ( kind = 4 ) invers(n)
  integer ( kind = 4 ) p(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_NEXT2_TEST'
  write ( *, '(a)' ) '  PERM1_NEXT2 generates permutations of (1,...,N).'
  write ( *, '(a)' ) ''
  done = .true.
 
  do

    call perm1_next2 ( n, p, done, iactiv, idir, invers )
 
    if ( done ) then
      exit
    end if

    call perm1_print ( n, p, '' )

  end do
 
  return
end
subroutine perm1_next3_test ( )

!*****************************************************************************80
!
!! PERM1_NEXT3_TEST tests PERM1_NEXT3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 November 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  logical more
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_NEXT3_TEST'
  write ( *, '(a)' ) '  PERM1_NEXT3 generates permutations of (1,...,N).'
  write ( *, '(a)' ) ''

  more = .false.
  rank = 0
 
  do

    call perm1_next3 ( n, p, more, rank )

    if ( .not. more ) then
      exit
    end if

    call perm1_print ( n, p, '' )

  end do
 
  return
end
subroutine perm1_print_test ( )

!*****************************************************************************80
!
!! PERM1_PRINT_TEST tests PERM1_PRINT.
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
  write ( *, '(a)' ) 'PERM1_PRINT_TEST'
  write ( *, '(a)' ) '  PERM1_PRINT prints a permutation of (1,...,N).'

  call perm1_print ( n, p, '  The 1-based permutation:' )
  
  return
end
subroutine perm1_random_test ( )

!*****************************************************************************80
!
!! PERM1_RANDOM_TEST tests PERM1_RANDOM;
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

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_RANDOM_TEST'
  write ( *, '(a)' ) '  PERM1_RANDOM produces a random permutation of (1,...,N);'
  write ( *, '(a,i8)' ) '  For this test, N = ', n
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 5
    call perm1_random ( n, seed, p )
    call perm1_print ( n, p, '' )
  end do
 
  return
end
subroutine perm1_random2_test ( )

!*****************************************************************************80
!
!! PERM1_RANDOM2_TEST tests PERM1_RANDOM2.
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

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_RANDOM2_TEST'
  write ( *, '(a)' ) '  PERM1_RANDOM2 produces a random permutation of (1,...,N).'
  write ( *, '(a,i8)' ) '  For this test, N = ', n
  write ( *, '(a)' ) ''
 
  seed = 123456789

  do i = 1, 5
    call perm1_random2 ( n, seed, p )
    call perm1_print ( n, p, '' )
  end do
 
  return
end
subroutine perm1_sign_test ( )

!*****************************************************************************80
!
!! PERM1_SIGN_TEST tests PERM1_SIGN.
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

  integer ( kind = 4 ), parameter :: n = 4

  logical more
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) p_sign

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_SIGN_TEST'
  write ( *, '(a)' ) '  PERM1_SIGN computes the sign of a permutation of (1,...,N).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  RANK  SIGN  Permutation'
  write ( *, '(a)' ) ''

  more = .false.
  rank = 0 

  do

    call perm1_lex_next ( n, p, more )
    call perm1_sign ( n, p, p_sign )

    if ( .not. more ) then
      exit
    end if

    write ( *, '(2x,i4,2x,i4,2x,10i4)' ) rank, p_sign, p(1:n)

    rank = rank + 1

  end do
 
  return
end
subroutine perm1_rank_test ( )

!*****************************************************************************80
!
!! PERM1_RANK_TEST tests PERM1_RANK.
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

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ), dimension ( n ) :: p = (/ 1, 4, 2, 3 /)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_RANK_TEST'
  write ( *, '(a)' ) '  PERM1_RANK ranks a permutation of (1,...,N).'

  call perm1_print ( n, p, '  The permutation:' )
 
  call perm1_rank ( n, p, rank )
 
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The rank is:', rank
 
  return
end
subroutine perm1_to_equiv_test ( )

!*****************************************************************************80
!
!! PERM1_TO_EQUIV_TEST tests PERM1_TO_EQUIV.
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

  integer ( kind = 4 ), parameter :: n = 9

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) jarray(n)
  integer ( kind = 4 ) npart
  integer ( kind = 4 ), dimension ( n ) :: p = (/ 2,3,9,6,7,8,5,4,1 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_TO_EQUIV_TEST'
  write ( *, '(a)' ) '  PERM1_TO_EQUIV returns the set partition'
  write ( *, '(a)' ) '  or equivalence classes determined by a'
  write ( *, '(a)' ) '  permutation of (1,...,N).'

  call perm1_print ( n, p, '  The input permutation:' )
 
  call perm1_to_equiv ( n, p, npart, jarray, a )

  call equiv_print ( n, a, '  The partition:' )
 
  return
end
subroutine perm1_to_inversion_test ( )

!*****************************************************************************80
!
!! PERM1_TO_INVERSION_TEST tests PERM1_TO_INVERSION.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ins(n)
  integer ( kind = 4 ) perm(n)
  integer ( kind = 4 ) perm2(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_TO_INVERSION_TEST'
  write ( *, '(a)' ) '  PERM1_TO_INVERSION: permutation (1,...,N) => inversion.'
  write ( *, '(a)' ) ''

  perm(1:n) = (/ 3, 5, 1, 4, 2 /)

  call perm1_to_inversion ( n, perm, ins )

  call inversion_to_perm1 ( n, ins, perm2 )

  write ( *, '(2x,6i3)' ) ( i, i = 1, n )
  write ( *, '(2x,6i3)' ) perm(1:n)
  write ( *, '(2x,6i3)' ) ins(1:n)
  write ( *, '(2x,6i3)' ) perm2(1:n)
 
  return
end
subroutine perm1_to_ytb_test ( )

!*****************************************************************************80
!
!! PERM1_TO_YTB_TEST tests PERM1_TO_YTB.
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

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) lambda(n)
  integer ( kind = 4 ), dimension ( n ) ::  p = (/ 7, 2, 4, 1, 5, 3, 6 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_TO_YTB_TEST'
  write ( *, '(a)' ) '  PERM1_TO_YTB converts a permutation of (1,...,N) to a'
  write ( *, '(a)' ) '  Young table.'

  call perm1_print ( n, p, '  The permutation:' )
 
  call perm1_to_ytb ( n, p, lambda, a )

  call ytb_print ( n, a, '  The Young table:' )
 
  return
end
subroutine perm1_unrank_test ( )

!*****************************************************************************80
!
!! PERM1_UNRANK_TEST tests PERM1_UNRANK.
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

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERM1_UNRANK_TEST'
  write ( *, '(a)' ) '  PERM1_UNRANK, given a rank, computes the'
  write ( *, '(a)' ) '  corresponding permutation of (1,...,N).'
  write ( *, '(a)' ) ''
  rank = 6
  write ( *, '(a,i8)' ) '  The requested rank is ', rank
 
  call perm1_unrank ( n, rank, p )
 
  call perm1_print ( n, p, '  The permutation:' )
 
  return
end
subroutine perrin_test ( )

!*****************************************************************************80
!
!! PERRIN_TEST tests PERRIN.
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

  integer ( kind = 4 ), parameter :: n = 15

  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PERRIN_TEST'
  write ( *, '(a)' ) '  PERRIN computes the Perrin numbers.'

  call perrin ( n, p )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     N        P(N)'
  write ( *, '(a)' ) ''

  do i = 0, n-1
    write ( *, '(2x,i8,i10)' ) i, p(i+1)
  end do

  return
end
subroutine pord_check_test ( )

!*****************************************************************************80
!
!! PORD_CHECK_TEST tests PORD_CHECK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ), dimension ( n, n ) :: a = reshape ( &
  (/ &
    1,0,1,0,1,0,1,0,0,1, &
    0,1,0,0,1,0,0,0,0,0, &
    0,0,1,0,1,0,1,0,0,1, &
    0,1,1,1,1,1,1,1,0,1, &
    0,0,0,0,1,0,0,0,0,0, &
    0,1,0,0,1,1,1,0,0,0, &
    0,0,0,0,1,0,1,0,0,0, &
    0,1,0,0,1,1,1,1,0,1, &
    0,0,0,0,0,0,0,0,0,0, &
    0,0,0,0,1,0,1,0,0,1 /), (/ n, n /) )
  integer ( kind = 4 ) ierror

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PORD_CHECK_TEST'
  write ( *, '(a)' ) '  PORD_CHECK checks a partial ordering.'

  call i4mat_print ( n, n, a, '  The partial ordering matrix:' )
 
  call pord_check ( n, a, ierror )
 
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  CHECK FLAG = ', ierror
  write ( *, '(a)' ) '  0 means no error.'
  write ( *, '(a)' ) '  1 means illegal value of N.'
  write ( *, '(a)' ) '  2 means some A(I,J) and A(J,I) are both nonzero.'
 
  return
end
subroutine power_mod_test ( )

!*****************************************************************************80
!
!! POWER_MOD_TEST tests POWER_MOD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POWER_MOD_TEST'
  write ( *, '(a)' ) '  POWER_MOD computes the remainder of a power'
  write ( *, '(a)' ) '  of an integer modulo another integer.'

  a = 7
  n = 50
  m = 11

  call power_mod ( a, n, m, x )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  A = ', a
  write ( *, '(a,i8)' ) '  N = ', n
  write ( *, '(a,i8)' ) '  M = ', m
  write ( *, '(a,i8)' ) '  mod ( A**N, M ) = ', x

  a = 3
  n = 118
  m = 119

  call power_mod ( a, n, m, x )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  A = ', a
  write ( *, '(a,i8)' ) '  N = ', n
  write ( *, '(a,i8)' ) '  M = ', m
  write ( *, '(a,i8)' ) '  mod ( A**N, M ) = ', x

  return
end
subroutine power_series1_test ( )

!*****************************************************************************80
!
!! POWER_SERIES1_TEST tests POWER_SERIES1;
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

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POWER_SERIES1_TEST'
  write ( *, '(a)' ) '  POWER_SERIES1 composes a power series;'

  alpha = 7.0D+00
 
  a(1) = 1.0D+00
  a(2:n) = 0.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Power series of G(x) = (1+F(x))**alpha'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  N = ', n
  write ( *, '(a,g14.6)' ) '  ALPHA = ', alpha
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Series for F(x):'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) a(1:n)
 
  call power_series1 ( n, alpha, a, b )
 
  write ( *, '(a)' ) '  Series for G(x):'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) b(1:n)
 
  return
end
subroutine power_series2_test ( )

!*****************************************************************************80
!
!! POWER_SERIES2_TEST tests POWER_SERIES2;
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

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POWER_SERIES2_TEST'
  write ( *, '(a)' ) '  POWER_SERIES2 composes a power series;'
  write ( *, '(a)' ) '  Here we compute the power series of G(x) = exp(F(x))-1'
  write ( *, '(a,i8)' ) '  The number of terms is N = ', n

  a(1) = -4.0D+00
  a(2:n) = 0.0D+00
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Series for F(x):'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) a(1:n)
 
  call power_series2 ( n, a, b )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Series for G(x):'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) b(1:n)
 
  return
end
subroutine power_series3_test ( )

!*****************************************************************************80
!
!! POWER_SERIES3_TEST tests POWER_SERIES3;
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

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POWER_SERIES3_TEST'
  write ( *, '(a)' ) '  POWER_SERIES3 composes a power series;'
 
  a(1) = 1.0D+00
  a(2) = 1.0D+00
  a(3:n) = 0.0D+00
 
  b(1) = 1.0D+00
  b(2) = 1.0D+00
  b(3:n) = 0.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Power series of H(x) = G(F(x))'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of terms, N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Series for F(x):'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) a(1:n)
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Series for G(x):'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) b(1:n)
 
  call power_series3 ( n, a, b, c )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Series for H(x):'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) c(1:n)
 
  return
end
subroutine power_series4_test ( )

!*****************************************************************************80
!
!! POWER_SERIES4_TEST tests POWER_SERIES4.
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

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c(n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POWER_SERIES4_TEST'
  write ( *, '(a)' ) '  POWER_SERIES4 composes a power series;'

  do i = 1, n
    a(i) = 1.0D+00 / real ( i, kind = 8 )
  end do

  b(1) = 1.0D+00
  b(2:n) = 0.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Power series of H(x) = G(1/F(x))'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Series for F(x):'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) a(1:n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Series for G(x):'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) b(1:n)
 
  call power_series4 ( n, a, b, c )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Series for H(x):'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) c(1:n)
 
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
subroutine pythag_triple_next_test ( )

!*****************************************************************************80
!
!! PYTHAG_TRIPLE_NEXT_TEST tests PYTHAG_TRIPLE_NEXT
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

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) c
  integer ( kind = 4 ) d
  integer ( kind = 4 ) e
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PYTHAG_TRIPLE_NEXT_TEST'
  write ( *, '(a)' ) '  PYTHAG_TRIPLE_NEXT computes the "next"'
  write ( *, '(a)' ) '  Pythagorean triple.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I   J   A   B   C  A^2+B^2   C^2'
  write ( *, '(a)' ) ''

  i = 0
  j = 0

  do k = 0, 20
    call pythag_triple_next ( i, j, a, b, c )
    d = a * a + b * b
    e = c * c
    write ( *, '(2x,5i4,2i8)' ) i, j, a, b, c, d, e
  end do

  return
end
subroutine r8_agm_test ( )

!*****************************************************************************80
!
!! R8_AGM_TEST tests R8_AGM
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

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_agm
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_AGM_TEST'
  write ( *, '(a)' ) '  R8_AGM computes the arithmetic-geometric mean (AGM)'
  write ( *, '(a)' ) '  of two nonnegative real numbers.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    X        Y    R8_AGM(X,Y)'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10
    j = i4_uniform_ab ( 1, 10, seed )
    x = real ( j, kind = 8 )
    j = i4_uniform_ab ( 1, 10, seed )
    y = real ( j, kind = 8 )
    z = r8_agm ( x, y )
    write ( *, '(2x,f8.4,2x,f8.4,2x,f8.4)' ) x, y, z
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
!    02 June 2007
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CHOOSE_TEST'
  write ( *, '(a)' ) '  R8_CHOOSE evaluates C(N,K).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       N         K    CNK'
  write ( *, '(a)' ) ''

  do n = 0, 4
    do k = 0, n
      cnk = r8_choose ( n, k )
      write ( *, '(2x,i8,2x,i8,g14.6)' ) n, k, cnk
    end do
  end do

  return
end
subroutine r8_fall_test ( )

!*****************************************************************************80
!
!! R8_FALL_TEST tests R8_FALL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_fall
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_FALL_TEST'
  write ( *, '(a)' ) '  R8_FALL computes the falling factorial function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '    X          N                Exact' // &
    '                  Computed'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call r8_fall_values ( n_data, x, n, f1 )

    if ( n_data == 0 ) then
      exit
    end if

    f2 = r8_fall ( x, n );

    write ( *, '(2x,f8.4,2x,i4,2x,g24.16,2x,g24.16)' ) x, n, f1, f2

  end do
     
  return
end
subroutine r8_rise_test ( )

!*****************************************************************************80
!
!! R8_RISE_TEST tests R8_RISE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_rise
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_RISE_TEST'
  write ( *, '(a)' ) '  R8_RISE computes the rising factorial function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '    X          N                Exact' // &
    '                  Computed'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call r8_rise_values ( n_data, x, n, f1 )

    if ( n_data == 0 ) then
      exit
    end if

    f2 = r8_rise ( x, n );

    write ( *, '(2x,f8.4,2x,i4,2x,g24.16,2x,g24.16)' ) x, n, f1, f2

  end do
     
  return
end
subroutine r8_to_cfrac_test ( )

!*****************************************************************************80
!
!! R8_TO_CFRAC_TEST tests R8_TO_CFRAC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) a(0:n)
  real ( kind = 8 ) error
  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(-1:n)
  integer ( kind = 4 ) q(-1:n)
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_pi
  real ( kind = 8 ) temp

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_TO_CFRAC_TEST'
  write ( *, '(a)' ) '  R8_TO_CFRAC converts a real number to a'
  write ( *, '(a)' ) '  a sequence of continued fraction convergents.'

  r = 2.0D+00 * r8_pi ( )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Use the real number R = ', r

  call r8_to_cfrac ( r, n, a, p, q )

  write ( *, '(a)' ) ''

  do i = 0, n
    temp = real ( p(i), kind = 8 ) / real ( q(i), kind = 8 )
    error = r - temp
    write ( *, '(2x,3i12,2g14.6)' ) a(i), p(i), q(i), temp, error
  end do

  return
end
subroutine r8_to_dec_test ( )

!*****************************************************************************80
!
!! R8_TO_DEC_TEST tests R8_TO_DEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) dec_digit
  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_TO_DEC_TEST'
  write ( *, '(a)' ) '  R8_TO_DEC converts a real number to a decimal;'
 
  dec_digit = 5

  write ( *, '(a)' ) ''
  write ( *, '(a,i3)' ) '  The number of decimal digits is ', dec_digit

  r8_lo = -10.0D+00
  r8_hi = +10.0D+00
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     R   =>  A * 10^B  =>  R2'
  write ( *, '(a)' ) ''

  do i = 1, 10
    r = r8_uniform_ab ( r8_lo, r8_hi, seed )
    call r8_to_dec ( r, dec_digit, a, b )
    call dec_to_r8 ( a, b, r2 )
    write ( *, '(2x,f10.6,2x,i8,2x,i8,2x,f10.6)' ) r, a, b, r2
  end do

  return
end
subroutine r8_to_rat_test ( )

!*****************************************************************************80
!
!! R8_TO_RAT_TEST tests R8_TO_RAT.
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

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: ndig = 4
  real ( kind = 8 ) r
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_TO_RAT_TEST'
  write ( *, '(a)' ) '  R8_TO_RAT converts a real number to a rational;'
  write ( *, '(a)' ) ''
  write ( *, '(a,i3)' ) '  The maximum number of digits allowed is ', ndig

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     R   =>  A / B  =>  R2'
  write ( *, '(a)' ) ''

  do i = 1, 10
    r = r8_uniform_01 ( seed )
    r = 10.0D+00 * ( r - 0.25D+00 )
    call r8_to_rat ( r, ndig, a, b )
    call rat_to_r8 ( a, b, r2 )
    write ( *, '(2x,f10.6,i8,2x,i8,f10.6)' ) r, a, b, r2
  end do

  return
end
subroutine r8mat_det_test ( )

!*****************************************************************************80
!
!! R8MAT_DET_TEST tests R8MAT_DET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n3 = 3
  integer ( kind = 4 ), parameter :: n4 = 4

  real ( kind = 8 ) a3(n3,n3)
  real ( kind = 8 ) a4(n4,n4)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_DET_TEST'
  write ( *, '(a)' ) '  R8MAT_DET: determinant of a real matrix.'
  write ( *, '(a)' ) ''
 
  k = 0
  do i = 1, n3
    do j = 1, n3
      k = k+1
      a3(i,j) = real ( k, kind = 8 )
    end do
  end do
 
  call r8mat_print ( n3, n3, a3, '  The 123/456/789 matrix:' )

  call r8mat_det ( n3, a3, det )
 
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Determinant of the 123/456/789 matrix is ', det
 
  do i = 1, n4
    do j = 1, n4
      a4(i,j) = 1.0D+00 / real ( i + j, kind = 8 )
    end do
  end do
 
  call r8mat_print ( n4, n4, a4, '  The Hilbert matrix:' )

  call r8mat_det ( n4, a4, det )
 
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Determinant of the Hilbert matrix is ', det
 
  do i = 1, n3
    do j = 1, n3
      if ( i == j ) then
        a3(i,j) = 2.0D+00
      else if ( i == j+1 .or. i == j-1 ) then
        a3(i,j) = -1.0D+00
      else
        a3(i,j) = 0.0D+00
      end if
    end do
  end do
 
  call r8mat_print ( n3, n3, a3, '  The -1,2,-1 matrix:' )

  call r8mat_det ( n3, a3, det )
 
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Determinant of the -1,2,-1 matrix is ', det
 
  return
end
subroutine r8mat_perm1_test ( )

!*****************************************************************************80
!
!! R8MAT_PERM1_TEST tests R8MAT_PERM1.
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

  integer ( kind = 4 ), parameter :: n = 9

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), dimension ( n ) :: p = (/ 2,3,9,6,7,8,5,4,1 /)
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_PERM1_TEST'
  write ( *, '(a)' ) '  R8MAT_PERM1 reorders a real matrix in place.'
  write ( *, '(a)' ) '  The rows and columns use the same permutation.'
 
  do i = 1, n
    do j = 1, n
      a(i,j) = real ( i * 10 + j, kind = 8 )
    end do
  end do
 
  call r8mat_print ( n, n, a, '  The original matrix' )
 
  call perm1_print ( n, p, '  The row and column permutation:' )
 
  call r8mat_perm1 ( n, a, p )
 
  call r8mat_print ( n, n, a, '  The permuted matrix' )
 
  return
end
subroutine r8mat_2perm1_test ( )

!*****************************************************************************80
!
!! R8MAT_2PERM1_TEST tests R8MAT_2PERM1.
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

  integer ( kind = 4 ), parameter :: m = 9
  integer ( kind = 4 ), parameter :: n = 7

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( m ) :: p = (/ 2, 3, 9, 6, 7, 8, 5, 4, 1 /)
  integer ( kind = 4 ), dimension ( n ) :: q = (/ 3, 4, 5, 6, 7, 1, 2 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_2PERM1_TEST'
  write ( *, '(a)' ) '  R8MAT_2PERM1 reorders a real matrix in place.'
  write ( *, '(a)' ) '  Rows and columns use different permutations.'
 
  do i = 1, m
    do j = 1, n
      a(i,j) = real ( i * 10 + j, kind = 8 )
    end do
  end do
 
  call r8mat_print ( m, n, a, '  The original matrix' )
 
  call perm1_print ( m, p, '  The row permutation:' )
 
  call perm1_print ( n, q, '  The column permutation:' )

  call r8mat_2perm1 ( m, n, a, p, q )
 
  call r8mat_print ( m, n, a, '  The permuted matrix' )
 
  return
end
subroutine r8mat_permanent_test ( )

!*****************************************************************************80
!
!! R8MAT_PERMANENT_TEST tests R8MAT_PERMANENT.
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

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) perm

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_PERMANENT_TEST'
  write ( *, '(a)' ) '  R8MAT_PERMANENT: the matrix permanent function.'
  write ( *, '(a)' ) '  We will analyze matrices with 0 diagonal and'
  write ( *, '(a)' ) '  1 on all offdiagonals.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Order	    Permanent.'
  write ( *, '(a)' ) ''
 
  do n = 2, 12
 
    allocate ( a(1:n,1:n) )

    a(1:n,1:n) = 1.0D+00

    do i = 1, n
      a(i,i) = 0.0D+00
    end do
 
    call r8mat_permanent ( n, a, perm )
 
    write ( *, '(7x,i2,8x,g18.10)' ) n, perm

    deallocate ( a )
 
  end do
 
  return
end
subroutine r8poly_test ( )

!*****************************************************************************80
!
!! R8POLY_TEST test R8POLY.
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

  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ), dimension ( n ) :: a
  integer ( kind = 4 ) iopt
  integer ( kind = 4 ) test
  real ( kind = 8 ) val
  real ( kind = 8 ) x0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_TEST'
  write ( *, '(a)' ) '  R8POLY converts between power sum, factorial'
  write ( *, '(a)' ) '  and Taylor forms, and can evaluate a polynomial'
  write ( *, '(a)' ) ''
 
  do test = 1, 6

    if ( test == 1 ) then
      iopt = -3
    else if ( test == 2 ) then
      iopt = -2
    else if ( test == 3 ) then
      iopt = -1
      x0 = 2.0D+00
    else if ( test == 4 ) then
      iopt = 0
      x0 = 2.0D+00
    else if ( test == 5 ) then
      iopt = 6
      x0 = 2.0D+00
    else if ( test == 6 ) then
      iopt = 6
      x0 = -2.0D+00
    end if

    a(1:n) = (/ 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 1.0D+00 /)

    if ( test == 1 ) then
      write ( *, '(a)' ) '  All calls have input A as follows'
      write ( *, '(2x,6f7.2)' ) a(1:n)
    end if
 
    call r8poly ( n, a, x0, iopt, val )
 
    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Option IOPT = ', iopt
    if ( -1 <= iopt ) then
      write ( *, '(a,g14.6)' ) '  X0 = ', x0
    end if

    if ( iopt == -3 .or. iopt == -2 .or. 0 < iopt ) then
      write ( *, '(a)' ) '  Output array = '
      write ( *, '(2x,6f7.2)' ) a(1:n)
    end if

    if ( iopt == -1 .or. iopt == 0 ) then
      write ( *, '(a,g14.6)' ) '  Value = ', val
    end if
 
  end do

  return
end
subroutine r8poly_f2p_test ( )

!*****************************************************************************80
!
!! R8POLY_F2P_TEST tests R8POLY_F2P.
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

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n)

  call r8vec_indicator1 ( n, a )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_F2P_TEST'
  write ( *, '(a)' ) '  R8POLY_F2P: factorial => power sum.'

  call r8poly_print ( n-1, a, '  The power sum polynomial:' )
 
  call r8poly_p2f ( n, a )
 
  call r8vec_print ( n, a, '  The factorial polynomial coefficients:' )
 
  call r8poly_f2p ( n, a )
 
  call r8poly_print ( n-1, a, '  The recovered power sum polynomial:' )
 
  return
end
subroutine r8poly_fval_test ( )

!*****************************************************************************80
!
!! R8POLY_FVAL_TEST tests R8POLY_FVAL.
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) val
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_FVAL_TEST'
  write ( *, '(a)' ) '  R8POLY_FVAL evaluates a polynomial in factorial form.'

  call r8vec_indicator1 ( n, a )
 
  call r8vec_print ( n, a, '  The factorial polynomial coefficients:' )

  x = 2.0D+00

  call r8poly_fval ( n, a, x, val )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6)' ) '  R8POLY (', x, ' ) = ', val
  write ( *, '(a)' ) '  The correct value is 11.'
 
  return
end
subroutine r8poly_n2p_test ( )

!*****************************************************************************80
!
!! R8POLY_N2P_TEST tests R8POLY_N2P.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a2(n)

  call r8vec_indicator1 ( n, a )

  a2(1:n) = 2.0D+00 * a(1:n)
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_N2P_TEST'
  write ( *, '(a)' ) '  R8POLY_N2P: Newton => power sum;'

  call r8poly_print ( n-1, a, '  The power sum polynomial:' )
 
  call r8poly_p2n ( n, a, a2 )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Newton polynomial coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(6f12.4)' ) a(1:n)
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Newton polynomial abscissas:'
  write ( *, '(a)' ) ''
  write ( *, '(6f12.4)' ) a2(1:n)
 
  call r8poly_n2p ( n, a, a2 )
 
  call r8poly_print ( n-1, a, '  The recovered power sum polynomial:' )

  return
end
subroutine r8poly_nval_test ( )

!*****************************************************************************80
!
!! R8POLY_NVAL_TEST tests R8POLY_NVAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a2(n)
  real ( kind = 8 ) val
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_NVAL_TEST'
  write ( *, '(a)' ) '  R8POLY_NVAL evaluates a Newton polynomial.'

  call r8vec_indicator1 ( n, a )

  a2(1:n-1) = a(1:n-1) - 1.0D+00
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Newton polynomial coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,6f12.4)' ) a(1:n)
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Newton polynomial abscissas:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,6f12.4)' ) a2(1:n-1)
 
  x = 2.0D+00
 
  call r8poly_nval ( n, a, a2, x, val )
 
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6)' ) '  R8POLY ( ', x,' ) = ', val
  write ( *, '(a)' ) '  The correct value is 11.'
 
  return
end
subroutine r8poly_nx_test ( )

!*****************************************************************************80
!
!! R8POLY_NX_TEST tests R8POLY_NX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) x
  real ( kind = 8 ) xarray(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_NX_TEST'
  write ( *, '(a)' ) '  R8POLY_NX replaces one abscissa in a Newton polynomial.'

  call r8vec_indicator1 ( n, a )
  call r8vec_indicator1 ( n, xarray )
 
  call r8vec_print ( n, a, '  Newton polynomial coefficients:' )
  call r8vec_print ( n, xarray, '  Newton polynomial abscissas:' )
!
!  Shift the X array by inserting X=0.
!
  x = 0.0D+00
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Replace one abscissa by X = ', x

  call r8poly_nx ( n, a, xarray, x )
!
!  Report the new polynomial form.
!
  call r8vec_print ( n, a, '  Revised Newton polynomial coefficients:' )
  call r8vec_print ( n, xarray, '  Revised Newton polynomial abscissas:' )

  return
end
subroutine r8poly_p2f_test ( )

!*****************************************************************************80
!
!! R8POLY_P2F_TEST tests R8POLY_P2F.
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

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n)

  call r8vec_indicator1 ( n, a )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_P2F_TEST'
  write ( *, '(a)' ) '  R8POLY_P2F: power sum => factorial;'

  call r8poly_print ( n-1, a, '  The power sum polynomial:' )
 
  call r8poly_p2f ( n, a )
 
  call r8vec_print ( n, a, '  The factorial polynomial coefficients:' )
 
  call r8poly_f2p ( n, a )
 
  call r8poly_print ( n-1, a, '  The recovered power sum polynomial:' )
 
  return
end
subroutine r8poly_p2n_test ( )

!*****************************************************************************80
!
!! R8POLY_P2N_TEST tests R8POLY_P2N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a2(n)

  call r8vec_indicator1 ( n, a )

  a2(1:n) = 2.0D+00 * a(1:n)
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_P2N_TEST'
  write ( *, '(a)' ) '  R8POLY_P2N: Power sum => Newton.'

  call r8poly_print ( n-1, a, '  The power sum polynomial:' )
 
  call r8poly_p2n ( n, a, a2 )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Newton polynomial coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(6f12.4)' ) a(1:n)
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Newton polynomial abscissas:'
  write ( *, '(a)' ) ''
  write ( *, '(6f12.4)' ) a2(1:n)
 
  call r8poly_n2p ( n, a, a2 )
 
  call r8poly_print ( n-1, a, '  The recovered power sum polynomial:' )

  return
end
subroutine r8poly_p2t_test ( )

!*****************************************************************************80
!
!! R8POLY_P2T_TEST tests R8POLY_P2T.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) x

  call r8vec_indicator1 ( n, a )

  x = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_P2T_TEST'
  write ( *, '(a)' ) '  R8POLY_P2T: Power sum => Taylor.'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Taylor expansion point is X = ', x
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The Taylor coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,6f12.4)' ) a(1:n)

  call r8poly_t2p ( n, a, x )

  call r8poly_print ( n-1, a, '  The power sum polynomial:' )

  call r8poly_p2t ( n, a, x )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The recovered Taylor coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,6f12.4)' ) a(1:n)
 
  return
end 
subroutine r8poly_print_test ( )

!*****************************************************************************80
!
!! R8POLY_PRINT_TEST tests R8POLY_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a(0:4)
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_PRINT_TEST'
  write ( *, '(a)' ) '  R8POLY_PRINT prints an R8POLY.'

  n = 4
  a(0:n) = (/ -2.0, 5.1, 2.2, 3.3, 1.4 /)

  call r8poly_print ( n, a, '  The polynomial:' )

  return
end
subroutine r8poly_pval_test ( )

!*****************************************************************************80
!
!! R8POLY_PVAL_TEST tests R8POLY_PVAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) i
  real ( kind = 8 ) a(0:n)
  real ( kind = 8 ) val
  real ( kind = 8 ) x

  do i = 0, n
    a(i) = real ( i + 1, kind = 8 )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_PVAL_TEST'
  write ( *, '(a)' ) '  R8POLY_PVAL evaluates a polynomial'
  write ( *, '(a)' ) '  in power sum form.'

  call r8poly_print ( n, a, '  The polynomial to be evaluated:' )

  x = 2.0D+00
 
  call r8poly_pval ( n, a, x, val )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  At X = ', x
  write ( *, '(a,g14.6)' ) '  Computed polynomial value is ', val
  write ( *, '(a)' ) '  Correct value is 129.'
 
  return
end
subroutine r8poly_t2p_test ( )

!*****************************************************************************80
!
!! R8POLY_T2P_TEST tests R8POLY_T2P.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) x

  call r8vec_indicator1 ( n, a )

  x = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8POLY_T2P_TEST'
  write ( *, '(a)' ) '  R8POLY_T2P: Taylor => Power sum;'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Taylor expansion point is X = ', x
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The Taylor coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,6f12.4)' ) a(1:n)

  call r8poly_t2p ( n, a, x )

  call r8poly_print ( n-1, a, '  The power sum polynomial:' )

  call r8poly_p2t ( n, a, x )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The recovered Taylor coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,6f12.4)' ) a(1:n)
 
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

    call r8vec_backtrack ( n, maxstack, x, indx, k, nstack, stacks, ncan )

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
subroutine r8vec_frac_test ( )

!*****************************************************************************80
!
!! R8VEC_FRAC_TEST tests R8VEC_FRAC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ), parameter :: ahi = 10.0D+00
  real ( kind = 8 ), parameter :: alo = 0.0D+00
  real ( kind = 8 ) afrac
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_FRAC_TEST'
  write ( *, '(a)' ) '  R8VEC_FRAC: K-th smallest real vector entry;'

  seed = 123456789

  call r8vec_uniform ( n, alo, ahi, seed, a )

  call r8vec_print ( n, a, '  The real array to search: ' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Frac   R8VEC_FRAC'
  write ( *, '(a)' ) ''

  do k = 1, n

    call r8vec_frac ( n, a, k, afrac )
    write ( *, '(2x,i4,2x,g14.6)' ) k, afrac

  end do

  return
end
subroutine r8vec_mirror_next_test ( )

!*****************************************************************************80
!
!! R8VEC_MIRROR_NEXT_TEST tests R8VEC_MIRROR_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(n)
  logical done

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MIRROR_NEXT_TEST'
  write ( *, '(a)' ) '  R8VEC_MIRROR_NEXT generates all sign variations'
  write ( *, '(a)' ) '  of a real vector.'

  a(1:n) = (/ 1.0D+00, 2.0D+00, 3.0D+00 /)

  do

    call r8vec_print ( n, a, '  Next vector:' )

    call r8vec_mirror_next ( n, a, done )

    if ( done ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Done.'
      exit
    end if

  end do

  a(1:n) = (/ 1.0D+00, 0.0D+00, 3.0D+00 /)

  do

    call r8vec_print ( n, a, '  Next vector:' )

    call r8vec_mirror_next ( n, a, done )

    if ( done ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Done.'
      exit
    end if

  end do

  return
end
subroutine rat_add_test ( )

!*****************************************************************************80
!
!! RAT_ADD_TEST tests RAT_ADD.
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

  integer ( kind = 4 ) abot
  integer ( kind = 4 ) atop
  integer ( kind = 4 ) bbot
  integer ( kind = 4 ) btop
  integer ( kind = 4 ) cbot
  integer ( kind = 4 ) ctop
  integer ( kind = 4 ) ierror
  character ( len = 22 ) string

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAT_ADD_TEST'
  write ( *, '(a)' ) '  RAT_ADD adds two rationals.'

  atop = 3
  abot = 4
  btop = 10
  bbot = 7

  call rat_add ( atop, abot, btop, bbot, ctop, cbot, ierror )

  write ( *, '(a)' ) ''
  call rat_to_s ( atop, abot, string )
  write ( *, '(a)' ) '  A = ' // trim ( string )
  call rat_to_s ( btop, bbot, string )
  write ( *, '(a)' ) '  B = ' // trim ( string )
  call rat_to_s ( ctop, cbot, string )
  write ( *, '(a)' ) '  C = A + B = ' // trim ( string )
 
  return
end
subroutine rat_div_test ( )

!*****************************************************************************80
!
!! RAT_DIV_TEST tests RAT_DIV.
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

  integer ( kind = 4 ) abot
  integer ( kind = 4 ) atop
  integer ( kind = 4 ) bbot
  integer ( kind = 4 ) btop
  integer ( kind = 4 ) cbot
  integer ( kind = 4 ) ctop
  integer ( kind = 4 ) ierror
  character ( len = 22 ) string

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAT_DIV_TEST'
  write ( *, '(a)' ) '  RAT_DIV divides two rationals.'

  atop = 3
  abot = 4
  btop = 10
  bbot = 7

  call rat_div ( atop, abot, btop, bbot, ctop, cbot, ierror )

  write ( *, '(a)' ) ''
  call rat_to_s ( atop, abot, string )
  write ( *, '(a)' ) '  A = ' // trim ( string )
  call rat_to_s ( btop, bbot, string )
  write ( *, '(a)' ) '  B = ' // trim ( string )
  call rat_to_s ( ctop, cbot, string )
  write ( *, '(a)' ) '  C = A / B = ' // trim ( string )
 
  return
end
subroutine rat_farey_test ( )

!*****************************************************************************80
!
!! RAT_FAREY_TEST tests RAT_FAREY.
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

  integer ( kind = 4 ), parameter :: max_frac = 20

  integer ( kind = 4 ) a(max_frac)
  integer ( kind = 4 ) b(max_frac)
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) n
  integer ( kind = 4 ) num_frac

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAT_FAREY_TEST'
  write ( *, '(a)' ) '  RAT_FAREY computes a row of the Farey fraction table.'

  do n = 1, 7

    call rat_farey ( n, max_frac, num_frac, a, b )
 
    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Row ', n
    write ( *, '(a,i8)' ) '  Number of fractions: ', num_frac

    do ilo = 1, num_frac, 20
      ihi = min ( ilo+20-1, num_frac )
      write ( *, '(a)' ) ''
      write ( *, '(2x,20i3)' ) a(ilo:ihi)
      write ( *, '(2x,20i3)' ) b(ilo:ihi)
    end do

  end do

  return
end
subroutine rat_farey2_test ( )

!*****************************************************************************80
!
!! RAT_FAREY2_TEST tests RAT_FAREY2.
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

  integer ( kind = 4 ), parameter :: max_n = 4

  integer ( kind = 4 ) a(2**max_n+1)
  integer ( kind = 4 ) b(2**max_n+1)
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAT_FAREY2_TEST'
  write ( *, '(a)' ) '  RAT_FAREY2 computes a row of the Farey fraction table.'

  do n = 0, max_n

    call rat_farey2 ( n, a, b )
 
    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Row ', n+1

    do ilo = 1, 2**n+1, 20
      ihi = min ( ilo+20-1, 2**n+1 )
      write ( *, '(a)' ) ''
      write ( *, '(2x,20i3)' ) a(ilo:ihi)
      write ( *, '(2x,20i3)' ) b(ilo:ihi)
    end do

  end do

  return
end
subroutine rat_mul_test ( )

!*****************************************************************************80
!
!! RAT_MUL_TEST tests RAT_MUL.
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

  integer ( kind = 4 ) abot
  integer ( kind = 4 ) atop
  integer ( kind = 4 ) bbot
  integer ( kind = 4 ) btop
  integer ( kind = 4 ) cbot
  integer ( kind = 4 ) ctop
  integer ( kind = 4 ) ierror
  character ( len = 22 ) string

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAT_MUL_TEST'
  write ( *, '(a)' ) '  RAT_MUL multiplies two rationals.'

  atop = 3
  abot = 4
  btop = 10
  bbot = 7

  call rat_mul ( atop, abot, btop, bbot, ctop, cbot, ierror )

  write ( *, '(a)' ) ''
  call rat_to_s ( atop, abot, string )
  write ( *, '(a)' ) '  A = ' // trim ( string )
  call rat_to_s ( btop, bbot, string )
  write ( *, '(a)' ) '  B = ' // trim ( string )
  call rat_to_s ( ctop, cbot, string )
  write ( *, '(a)' ) '  C = A * B = ' // trim ( string )
 
  return
end
subroutine rat_normalize_test ( )

!*****************************************************************************80
!
!! RAT_NORMALIZE_TEST tests RAT_NORMALIZE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) :: rat_num = 7

  integer ( kind = 4 ) a1
  integer ( kind = 4 ) a2
  integer ( kind = 4 ) b1
  integer ( kind = 4 ) b2
  integer ( kind = 4 ) i
  integer ( kind = 4 ), dimension ( 7 ) :: rat_bot = (/ &
    4, 1000,  1,  4,   7, -15, -11 /)
  integer ( kind = 4 ), dimension ( 7 ) :: rat_top = (/ &
    3, 1,    20,  8, -10,   9, -11 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAT_NORMALIZE_TEST'
  write ( *, '(a)' ) '  RAT_NORMALIZE normalizes a rational.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '           A           B             A             B'
  write ( *, '(a)' ) '                                 normalized     normalized'
  write ( *, '(a)' ) ''

  do i = 1, rat_num
    a1 = rat_top(i)
    b1 = rat_bot(i)
    a2 = a1
    b2 = b1
    call rat_normalize ( a2, b2 )
    write ( *, '(2x,i10,2x,i10,4x,i10,2x,i10)' ) a1, b1, a2, b2
  end do

  return
end

subroutine rat_sum_formula_test ( )

!*****************************************************************************80
!
!! RAT_SUM_FORMULA_TEST tests RAT_SUM_FORMULA.
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

  integer ( kind = 4 ), parameter :: n = 6
  integer ( kind = 4 ) a(0:n,n+1)
  integer ( kind = 4 ) b(0:n,n+1)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAT_SUM_FORMULA_TEST'
  write ( *, '(a)' ) '  RAT_SUM_FORMULA computes the coefficients for the'
  write ( *, '(a)' ) '  formulas for the sums of powers of integers.'
  
  call rat_sum_formula ( n, a, b )

  call ratmat_print ( n+1, n+1, a, b, '  Power Sum Coefficients:' )

  return
end
subroutine rat_to_cfrac_test ( )

!*****************************************************************************80
!
!! RAT_TO_CFRAC_TEST tests RAT_TO_CFRAC.
!
!  Discussion:
!
!    Compute the continued fraction form of 4096/15625.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 10

  integer ( kind = 4 ) a(m)
  integer ( kind = 4 ) bot
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) n
  integer ( kind = 4 ) p(m)
  integer ( kind = 4 ) q(m)
  integer ( kind = 4 ) top

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAT_TO_CFRAC_TEST'
  write ( *, '(a)' ) '  RAT_TO_CFRAC fraction => continued fraction,'
  write ( *, '(a)' ) ''
  top = 4096
  bot = 15625
  write ( *, '(a,i8,a,i8)' ) '  Regular fraction is ', top, ' / ', bot
 
  call rat_to_cfrac ( top, bot, m, n, a, ierror )
 
  call i4vec_print ( n, a, '  Continued fraction coefficients:' )

  call cfrac_to_rat ( n, a, p, q )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The continued fraction convergents.'
  write ( *, '(a)' ) '  The last row contains the value of the continued'
  write ( *, '(a)' ) '  fraction, written as a common fraction.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, P(I), Q(I), P(I)/Q(I)'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,i3,2i8,g14.6)' ) i, p(i), q(i), &
      real ( p(i), kind = 8 ) / real ( q(i), kind = 8 )
  end do
 
  return
end
subroutine rat_to_dec_test ( )

!*****************************************************************************80
!
!! RAT_TO_DEC_TEST tests RAT_TO_DEC.
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

  integer ( kind = 4 ) exponent
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) mantissa
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) r3
  integer ( kind = 4 ) rat_bot
  integer ( kind = 4 ) rat_bot2
  integer ( kind = 4 ) rat_top
  integer ( kind = 4 ) rat_top2
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAT_TO_DEC_TEST'
  write ( *, '(a)' ) '  RAT_TO_DEC fraction => decimal,'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  In this test, choose the top and bottom'
  write ( *, '(a)' ) '  of a rational at random, and compute the'
  write ( *, '(a)' ) '  equivalent real number.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Then convert to decimal, and the equivalent real.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Then convert back to rational and the equivalent real.'
  
  seed = 123456789

  do i = 1, 10

    rat_top = i4_uniform_ab ( -1000, 1000, seed )

    rat_bot = i4_uniform_ab (     1, 1000, seed )

    r1 = real ( rat_top, kind = 8 ) / real ( rat_bot, kind = 8 )

    call rat_to_dec ( rat_top, rat_bot, mantissa, exponent )

    r2 = real ( mantissa, kind = 8 ) * 10.0D+00**( exponent )
 
    call dec_to_rat ( mantissa, exponent, rat_top2, rat_bot2 )
    r3 = real ( rat_top2, kind = 8 ) / real ( rat_bot2, kind = 8 )

    write ( *, '(a)' ) ''
    write ( *, '(2x,f10.6,a,i12,a,i12)' ) r1, '=', rat_top, '/', rat_bot
    write ( *, '(2x,f10.6,a,i12,a,i12)' ) r2, '=', mantissa, '*10^', exponent
    write ( *, '(2x,f10.6,a,i12,a,i12)' ) r3, '=', rat_top2, '/', rat_bot2

  end do
 
  return
end
subroutine rat_to_r8_test ( )

!*****************************************************************************80
!
!! RAT_TO_R8_TEST tests RAT_TO_R8.
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

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: ndig = 4
  real ( kind = 8 ) r
  real ( kind = 8 ) r2
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAT_TO_R8_TEST'
  write ( *, '(a)' ) '  RAT_TO_R8 converts a rational to a real number.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i3)' ) '  The maximum number of digits allowed is ', ndig

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     R   =>  A / B  =>  R2'
  write ( *, '(a)' ) ''

  do i = 1, 10
    r = r8_uniform_01 ( seed )
    r = 10.0D+00 * ( r - 0.25D+00 )
    call r8_to_rat ( r, ndig, a, b )
    call rat_to_r8 ( a, b, r2 )
    write ( *, '(2x,f10.6,i8,2x,i8,f10.6)' ) r, a, b, r2
  end do

  return
end
subroutine rat_to_s_test ( )

!*****************************************************************************80
!
!! RAT_TO_S_TEST tests RAT_TO_S.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) :: rat_num = 7

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ), dimension ( 7 ) :: rat_bot = (/ &
    4, 1000,  1,  4,   7, -15, -11 /)
  integer ( kind = 4 ), dimension ( 7 ) :: rat_top = (/ &
    3, 1,    20,  8, -10,   9, -11 /)
  character ( len = 255 ) s

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAT_TO_S_TEST'
  write ( *, '(a)' ) '  RAT_TO_S converts a rational to a string.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '           A           B    A/B'
  write ( *, '(a)' ) ''

  do i = 1, rat_num
    a = rat_top(i)
    b = rat_bot(i)
    call rat_to_s ( a, b, s );
    write ( *, '(2x,i10,2x,i10,6x,a)' ) a, b, trim ( s )
  end do

  return
end
subroutine rat_width_test ( )

!*****************************************************************************80
!
!! RAT_WIDTH_TEST tests RAT_WIDTH.
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

  integer ( kind = 4 ), parameter :: n_test = 17

  integer ( kind = 4 ) a
  integer ( kind = 4 ), dimension ( n_test ) :: a_test = (/ &
    1000, 1000, 1000, 1000, 1000, 1, -1, -10, -100, -1000, &
    1, 10, 100, 1000, 10000, 17, 4000000 /)
  integer ( kind = 4 ) b
  integer ( kind = 4 ), dimension ( n_test ) :: b_test = (/ &
    3, 40, 500, 6000, 70000, 1, 200, 200, 200, 200, &
   -200, -200, -200, -200, -200, 3000, 4000000 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) rat_width
  integer ( kind = 4 ) width

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAT_WIDTH_TEST'
  write ( *, '(a)' ) '  RAT_WIDTH determines the "width" of a rational.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Top    Bottom  Width'
  write ( *, '(a)' ) ''

  do i = 1, n_test
    a = a_test(i)
    b = b_test(i)
    width = rat_width ( a, b )
    write ( *, '(2x,3i8)' ) a, b, width
  end do

  return
end
subroutine ratmat_det_test ( )

!*****************************************************************************80
!
!! RATMAT_DET_TEST tests RATMAT_DET.
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

  integer ( kind = 4 ), parameter :: n3 = 3

  integer ( kind = 4 ) a3(n3,n3)
  integer ( kind = 4 ) b3(n3,n3)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) idbot
  integer ( kind = 4 ) idtop
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RATMAT_DET_TEST'
  write ( *, '(a)' ) '  RATMAT_DET: determinant of a rational matrix.'
  write ( *, '(a)' ) ''
 
  k = 0
  do i = 1, n3
    do j = 1, n3
      k = k + 1
      a3(i,j) = k
    end do
  end do

  b3(1:n3,1:n3) = 1
 
  call ratmat_print ( n3, n3, a3, b3, '  The 123/456/789 matrix:' )

  call ratmat_det ( n3, a3, b3, idtop, idbot, ierror )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Determinant of the 123/456/789 matrix:'
  write ( *, '(2x,i8,a,i8)' ) idtop, ' / ', idbot
 
  do i = 1, n3
    do j = 1, n3
      a3(i,j) = 1
      b3(i,j) = i + j
    end do
  end do
 
  call ratmat_print ( n3, n3, a3, b3, '  The Hilbert matrix:' )

  call ratmat_det ( n3, a3, b3, idtop, idbot, ierror )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Determinant of the Hilbert matrix:'
  write ( *, '(2x,i8,a,i8)' ) idtop, ' / ', idbot
 
  do i = 1, n3
    do j = 1, n3
      if ( i == j ) then
        a3(i,j) = 2
      else if ( i == j+1 .or. i == j-1 ) then
        a3(i,j) = -1
      else
        a3(i,j) = 0
      end if
      b3(i,j) = 1
    end do
  end do
 
  call ratmat_print ( n3, n3, a3, b3, '  The -1 2 -1 matrix:' )

  call ratmat_det ( n3, a3, b3, idtop, idbot, ierror )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Determinant of the -1,2,-1 matrix:'
  write ( *, '(2x,i8,a,i8)' ) idtop, ' / ', idbot
 
  return
end
subroutine ratmat_print_test ( )

!*****************************************************************************80
!
!! RATMAT_PRINT_TEST tests RATMAT_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RATMAT_PRINT_TEST'
  write ( *, '(a)' ) '  RATMAT_PRINT prints a rational matrix.'

  do i = 1, m
    do j = 1, n
      a(i,j) = 1
      b(i,j) = i + j
    end do
  end do
 
  call ratmat_print ( m, n, a, b, '  The Hilbert matrix:' )

  return
end
subroutine regro_next_test ( )

!*****************************************************************************80
!
!! REGRO_NEXT_TEST tests REGRO_NEXT.
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

  integer ( kind = 4 ), parameter :: n = 4

  logical done
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) v(n)
  integer ( kind = 4 ) vmax(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'REGRO_NEXT_TEST'
  write ( *, '(a)' ) '  REGRO_NEXT generates all restricted growth '
  write ( *, '(a)' ) '  functions.'
  write ( *, '(a)' ) ''

  rank = 0

  done = .true.
 
  do

    call regro_next ( n, v, vmax, done )

    if ( done ) then
      exit
    end if

    rank = rank + 1
    write ( *, '(2x,5i3)' ) rank, v(1:n)

  end do
 
  return
end
subroutine rfrac_to_cfrac_test ( )

!*****************************************************************************80
!
!! RFRAC_TO_CFRAC_TEST tests RFRAC_TO_CFRAC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxm = 10

  real ( kind = 8 ) g(2*maxm)
  real ( kind = 8 ) h(2*maxm)
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) m
  real ( kind = 8 ) p(maxm)
  real ( kind = 8 ) q(maxm+1)

  m = 3

  p(1:3) = (/ 1.0D+00, 1.0D+00, 2.0D+00 /)
  q(1:4) = (/ 1.0D+00, 3.0D+00, 1.0D+00, 1.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RFRAC_TO_CFRAC_TEST'
  write ( *, '(a)' ) '  RFRAC_TO_CFRAC: ratio to continued fration.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rational polynomial fraction coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(a,5f12.4)' ) '  P:  ', p(1:m)
  write ( *, '(a,5f12.4)' ) '  Q:  ', q(1:m+1)
 
  call rfrac_to_cfrac ( m, p, q, h, ierror )
 
  call r8vec_print ( 2*m, h, '  Continued fraction coefficients:' )

  g(1:2*m) = 1.0D+00

  call cfrac_to_rfrac ( 2*m, g, h, p, q )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Recovered rational polynomial:'
  write ( *, '(a)' ) ''
  write ( *, '(a,5f12.4)' ) '  P:  ', p(1:m)
  write ( *, '(a,5f12.4)' ) '  Q:  ', q(1:m+1)
 
  return
end
subroutine rfrac_to_jfrac_test ( )

!*****************************************************************************80
!
!! RFRAC_TO_JFRAC_TEST tests RFRAC_TO_JFRAC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 October 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxm = 10

  integer ( kind = 4 ) m
  real ( kind = 8 ) p(maxm)
  real ( kind = 8 ) q(maxm)
  real ( kind = 8 ) r(maxm)
  real ( kind = 8 ) s(maxm)
  integer ( kind = 4 ) seed
!
!  Generate the data, but force Q(M+1) to be 1.  
!  That will make it easier to see that the two operations are inverses
!  of each other.  JFRAC_TO_RFRAC is free to scale its output, and chooses
!  a scaling in which Q(M+1) is 1.
!
  seed = 123456789
  m = 6
  call r8vec_uniform_01 ( m, seed, p )
  call r8vec_uniform_01 ( m + 1, seed, q )

  q(1:m+1) = q(1:m+1) / q(m+1)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RFRAC_TO_JFRAC_TEST'
  write ( *, '(a)' ) '  RFRAC_TO_JFRAC converts a rational polynomial'
  write ( *, '(a)' ) '  fraction to a J fraction.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The original rational polynomial coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) p(1:m)
  write ( *, '(2x,5g14.6)' ) q(1:m+1)
 
  call rfrac_to_jfrac ( m, p, q, r, s )
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The J fraction coefficients:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) r(1:m)
  write ( *, '(2x,5g14.6)' ) s(1:m)
 
  call jfrac_to_rfrac ( m, r, s, p, q )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The recovered rational polynomial:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,5g14.6)' ) p(1:m)
  write ( *, '(2x,5g14.6)' ) q(1:m+1)

  return
end
subroutine schroeder_test ( )

!*****************************************************************************80
!
!! SCHROEDER_TEST tests SCHROEDER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) s(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SCHROEDER_TEST'
  write ( *, '(a)' ) '  SCHROEDER computes the Schroeder numbers.'

  call schroeder ( n, s )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       N        S(N)'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,i8,i10)' ) i, s(i)
  end do

  return
end
subroutine sort_heap_external_test ( )

!*****************************************************************************80
!
!! SORT_HEAP_EXTERNAL_TEST tests SORT_HEAP_EXTERNAL.
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

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) k
  integer ( kind = 4 ) k0
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SORT_HEAP_EXTERNAL_TEST'
  write ( *, '(a)' ) '  SORT_HEAP_EXTERNAL sorts objects externally.'

  seed = 123456789

  call i4vec_uniform_ab ( n, 1, n, seed, a )
 
  call i4vec_print ( n, a, '  Unsorted array:' )
 
  indx = 0
  i = 0
  j = 0
  isgn = 0
  i1 = 0
  j1 = 0
  k0 = 0
  k1 = 0
  n1 = 0

  do

    call sort_heap_external ( n, indx, i, j, isgn, i1, j1, k0, k1, n1 )
 
    if ( indx < 0 ) then
      isgn = 1
      if ( a(i) <= a(j) ) then
        isgn = -1
      end if
    else if ( 0 < indx ) then
      k    = a(i)
      a(i) = a(j)
      a(j) = k
    else
      exit
    end if

  end do

  call i4vec_print ( n, a, '  Sorted array:' )
 
  return
end
subroutine subcomp_next_test ( )

!*****************************************************************************80
!
!! SUBCOMP_NEXT_TEST tests SUBCOMP_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) count
  integer ( kind = 4 ) h
  logical more
  logical more2
  integer ( kind = 4 ) :: n = 6
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBCOMP_NEXT_TEST'
  write ( *, '(a)' ) '  SUBCOMP_NEXT generates subcompositions.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Seek all subcompositions of N = ', n
  write ( *, '(a,i8,a)' ) '  using K = ', k, ' parts.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     #   Sum'
  write ( *, '(a)' ) ''

  more = .false.
  count = 0

  do

    call subcomp_next ( n, k, a, more, h, t, n2, more2 )

    count = count + 1
    write ( *, '(2x,i4,2x,i4,2x,8i4)' ) count, sum ( a(1:k) ), a(1:k)

    if ( .not. more )  then
      exit
    end if

  end do
 
  return
end
subroutine subcompnz_next_test ( )

!*****************************************************************************80
!
!! SUBCOMPNZ_NEXT_TEST tests SUBCOMPNZ_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) count
  integer ( kind = 4 ) h
  logical more
  logical more2
  integer ( kind = 4 ) :: n = 6
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBCOMPNZ_NEXT_TEST'
  write ( *, '(a)' ) '  SUBCOMPNZ_NEXT generates subcompositions'
  write ( *, '(a)' ) '  using nonzero parts.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Seek all subcompositions of N = ', n
  write ( *, '(a,i8,a)' ) '  using K = ', k, ' nonzero parts.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     #   Sum'
  write ( *, '(a)' ) ''

  more = .false.
  count = 0

  do

    call subcompnz_next ( n, k, a, more, h, t, n2, more2 )

    count = count + 1
    write ( *, '(2x,i4,2x,i4,2x,8i4)' ) count, sum ( a(1:k) ), a(1:k)

    if ( .not. more )  then
      exit
    end if

  end do
 
  return
end
subroutine subcompnz2_next_test ( )

!*****************************************************************************80
!
!! SUBCOMPNZ2_NEXT_TEST tests SUBCOMPNZ2_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) count
  integer ( kind = 4 ) h
  logical more
  logical more2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) :: n_hi = 7
  integer ( kind = 4 ) :: n_lo = 5
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBCOMPNZ2_NEXT_TEST'
  write ( *, '(a)' ) '  SUBCOMPNZ2_NEXT generates subcompositions'
  write ( *, '(a)' ) '  using nonzero parts.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Seek all subcompositions of N'
  write ( *, '(a,i8,a)' ) '  using K = ', k, ' nonzero parts.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8,a,i8)' ) '  N ranges from ', n_lo, ' to ', n_hi
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     #     N'
  write ( *, '(a)' ) ''

  more = .false.
  count = 0

  do

    call subcompnz2_next ( n_lo, n_hi, k, a, more, h, t, n2, more2 )

    count = count + 1
    n = sum ( a(1:k) )
    write ( *, '(2x,i4,2x,i4,2x,8i4)' ) count, n, a(1:k)

    if ( .not. more )  then
      exit
    end if

  end do
 
  return
end
subroutine subset_by_size_next_test ( )

!*****************************************************************************80
!
!! SUBSET_BY_SIZE_NEXT_TEST tests SUBSET_BY_SIZE_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) m2
  logical more
  logical more2
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) subsize

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_BY_SIZE_NEXT_TEST'
  write ( *, '(a)' ) '  SUBSET_BY_SIZE_NEXT generates all subsets of an N set.'
  write ( *, '(a)' ) ''

  subsize = 0
  more = .false.
  more2 = .false.
  m = 0
  m2 = 0

  rank = 0

  do

    call subset_by_size_next ( n, a, subsize, more, more2, m, m2 )

    rank = rank + 1

    if ( 0 < subsize ) then
      write ( *, '(2x,i4,4x,5i2)' ) rank, a(1:subsize)
    else
      write ( *, '(2x,i4,4x,a)' ) rank, 'The empty set'
    end if

    if ( .not. more ) then
      exit
    end if

  end do

  return
end
subroutine subset_gray_next_test ( )

!*****************************************************************************80
!
!! SUBSET_GRAY_NEXT_TEST tests SUBSET_GRAY_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) iadd
  logical more
  integer ( kind = 4 ) ncard
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_GRAY_NEXT_TEST'
  write ( *, '(a)' ) '  SUBSET_GRAY_NEXT generates all subsets of an N set.'
  write ( *, '(a)' ) '  using the Gray code ordering:'
  write ( *, '(a)' ) '  0 0 1 0 1 means the subset contains 3 and 5.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Gray code'
  write ( *, '(a)' ) ''
 
  rank = 0
  more = .false.
 
  do
 
    call subset_gray_next ( n, a, more, ncard, iadd )

    rank = rank + 1 
    write ( *, '(2x,i4,4x,5i2)' ) rank, a(1:n)

    if ( .not. more ) then
      exit
    end if

  end do

  return
end
subroutine subset_gray_rank_test ( )

!*****************************************************************************80
!
!! SUBSET_GRAY_RANK_TEST tests SUBSET_GRAY_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ), dimension ( n ) :: a = (/ 1, 0, 1, 1, 0 /)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_GRAY_RANK_TEST'
  write ( *, '(a)' ) '  SUBSET_GRAY_RANK returns rank of a subset of an N set'
  write ( *, '(a)' ) '  using the Gray code ordering.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  For N = ', n
  write ( *, '(a)' ) '  the subset is:'
  write ( *, '(2x,5i2)' ) a(1:n)
 
  call subset_gray_rank ( n, a, rank )
 
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The rank is ', rank
 
  return
end
subroutine subset_gray_unrank_test ( )

!*****************************************************************************80
!
!! SUBSET_GRAY_UNRANK_TEST tests SUBSET_GRAY_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) rank

  rank = 8
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_GRAY_UNRANK_TEST'
  write ( *, '(a)' ) '  SUBSET_GRAY_UNRANK finds the subset of an N set'
  write ( *, '(a)' ) '  of a given rank under the Gray code ordering.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  N is ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank   Subset'
  write ( *, '(a)' ) ''

  do rank = 1, 10
 
    call subset_gray_unrank ( rank, n, a )

    write ( *, '(2x,i4,4x,5i2)' ) rank, a(1:n)

  end do
 
  return
end
subroutine subset_lex_next_test ( )

!*****************************************************************************80
!
!! SUBSET_LEX_NEXT_TEST tests SUBSET_LEX_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: ndim = 3

  integer ( kind = 4 ) a(ndim)
  integer ( kind = 4 ) k
  logical ltest
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_LEX_NEXT_TEST'
  write ( *, '(a)' ) '  SUBSET_LEX_NEXT generates all subsets of an N set.'
  write ( *, '(a)' ) '  The user can impose a restriction on the'
  write ( *, '(a)' ) '  maximum size of the subsets.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Here, we require the subsets to be no larger'
  write ( *, '(a,i8)' ) '  than ', ndim

  n = 5
  k = 0
 
  do
 
    ltest = ( k == ndim )

    call subset_lex_next ( n, ltest, ndim, k, a )
 
    if ( 0 < k ) then
      write ( *, '(2x,6i2)' ) a(1:k)
    else
      write ( *, '(a)' ) '  The empty set.'
    end if
 
    if ( k == 0 ) then
      exit
    end if

  end do
 
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
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBSET_RANDOM_TEST'
  write ( *, '(a)' ) '  SUBSET_RANDOM picks a subset at random.'
  write ( *, '(a,i8)' ) '  The number of elements in the main set is ', n
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 5
    call subset_random ( n, seed, a )
    write ( *, '(2x,40i2)' ) a(1:n)
  end do
 
  return
end
subroutine subtriangle_next_test ( )

!*****************************************************************************80
!
!! SUBTRIANGLE_NEXT_TEST tests SUBTRIANGLE_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j3
  logical more 
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rank

  n = 4
  rank = 0

  more = .false.
  i1 = 0
  j1 = 0
  i2 = 0
  j2 = 0
  i3 = 0
  j3 = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SUBTRIANGLE_NEXT_TEST'
  write ( *, '(a)' ) '  SUBTRIANGLE_NEXT generates the indices of subtriangles'
  write ( *, '(a)' ) '  in a triangle whose edges were divided into N subedges.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  For this test, N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank    I1  J1    I2  J2    I3  J3'
  write ( *, '(a)' ) ''

  do

    call subtriangle_next ( n, more, i1, j1, i2, j2, i3, j3 )

    rank = rank + 1

    write ( *, '(2x,i4,4x,i2,2x,i2,4x,i2,2x,i2,4x,i2,2x,i2)' ) &
      rank, i1, j1, i2, j2, i3, j3

    if ( .not. more ) then
      exit
    end if

  end do

  return
end
subroutine thue_binary_next_test ( )

!*****************************************************************************80
!
!! THUE_BINARY_NEXT_TEST tests THUE_BINARY_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 100

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) thue(n_max)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'THUE_BINARY_NEXT_TEST'
  write ( *, '(a)' ) '  THUE_BINARY_NEXT returns the next Thue binary sequence.'
  write ( *, '(a)' ) ''

  n = 1
  thue(1) = 0
  write ( *, '(2x,i4,4x,80i1)' ) n, thue(1:n)

  do i = 1, 6
    call thue_binary_next ( n, thue )
    write ( *, '(2x,i4,4x,80i1)' ) n, thue(1:n)
  end do

  return
end
subroutine thue_ternary_next_test ( )

!*****************************************************************************80
!
!! THUE_TERNARY_NEXT_TEST tests THUE_TERNARY_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 100

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) thue(n_max)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'THUE_TERNARY_NEXT_TEST'
  write ( *, '(a)' ) '  THUE_TERNARY_NEXT returns the next '
  write ( *, '(a)' ) '  Thue ternary sequence.'
  write ( *, '(a)' ) ''

  n = 1
  thue(1) = 1
  write ( *, '(2x,i4,4x,80i1)' ) n, thue(1:n)

  do i = 1, 5
    call thue_ternary_next ( n, thue )
    write ( *, '(2x,i4,4x,80i1)' ) n, thue(1:n)
  end do

  return
end
subroutine triang_test ( )

!*****************************************************************************80
!
!! TRIANG_TEST tests TRIANG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ), dimension ( n, n ) :: a = reshape ( &
  (/ &
    1,0,1,0,1,0,1,0,0,1, &
    0,1,0,0,1,0,0,0,0,0, &
    0,0,1,0,1,0,1,0,0,1, &
    0,1,1,1,1,1,1,1,0,1, &
    0,0,0,0,1,0,0,0,0,0, &
    0,1,0,0,1,1,1,0,0,0, &
    0,0,0,0,1,0,1,0,0,0, &
    0,1,0,0,1,1,1,1,0,1, &
    0,0,0,0,0,0,0,0,0,0, &
    0,0,0,0,1,0,1,0,0,1 /), (/ n, n /) )
  integer ( kind = 4 ) p(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANG_TEST'
  write ( *, '(a)' ) '  TRIANG relabels elements for a partial ordering,'

  call i4mat_print ( n, n, a, '  The input matrix:' )
 
  call triang ( n, a, p )
 
  call perm1_print ( n, p, '  The new ordering:' )

  call i4mat_2perm1 ( n, n, a, p, p )
 
  call i4mat_print ( n, n, a, '  The reordered matrix:' )
 
  return
end
subroutine tuple_next_test ( )

!*****************************************************************************80
!
!! TUPLE_NEXT_TEST tests TUPLE_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 2

  integer ( kind = 4 ), parameter :: m1 = 2
  integer ( kind = 4 ), parameter :: m2 = 4
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TUPLE_NEXT_TEST'
  write ( *, '(a)' ) '  TUPLE_NEXT returns the next "tuple", that is,'
  write ( *, '(a)' ) '  a vector of N integers, each between M1 and M2.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  M1 = ', m1
  write ( *, '(a,i8)' ) '  M2 = ', m2
  write ( *, '(a,i8)' ) '  N =  ', n
  write ( *, '(a)' ) ''

  rank = 0

  do

    call tuple_next ( m1, m2, n, rank, x )

    if ( rank == 0 ) then
      exit
    end if

    write ( *, '(2x,i4,2x,10i3)' ) rank, x(1:n)

  end do

  return
end
subroutine tuple_next_fast_test ( )

!*****************************************************************************80
!
!! TUPLE_NEXT_FAST_TEST tests TUPLE_NEXT_FAST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 2

  integer ( kind = 4 ) base(n)
  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) rank_max
  integer ( kind = 4 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TUPLE_NEXT_FAST_TEST'
  write ( *, '(a)' ) '  TUPLE_NEXT_FAST returns the next "tuple", that is,'
  write ( *, '(a)' ) '  a vector of N integers, each between 1 and M.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  M = ', m
  write ( *, '(a,i8)' ) '  N = ', n
  write ( *, '(a)' ) ''
!
!  Initialize.
!
  rank = -1
  call tuple_next_fast ( m, n, rank, base, x )
!
!  Request the RANK-th tuple.
!
  rank_max = m ** n - 1

  do rank = 0, rank_max

    call tuple_next_fast ( m, n, rank, base, x )

    write ( *, '(2x,i4,2x,10i3)' ) rank, x(1:n)

  end do

  return
end
subroutine tuple_next_ge_test ( )

!*****************************************************************************80
!
!! TUPLE_NEXT_GE_TEST tests TUPLE_NEXT_GE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TUPLE_NEXT_GE_TEST'
  write ( *, '(a)' ) '  TUPLE_NEXT_GE returns the next "tuple", that is,'
  write ( *, '(a)' ) '  a vector of N integers, each between 1 and M,'
  write ( *, '(a)' ) '  with the constraint that the entries be'
  write ( *, '(a)' ) '  nondecreasing.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  M = ', m
  write ( *, '(a,i8)' ) '  N = ', n
  write ( *, '(a)' ) ''

  rank = 0

  do

    call tuple_next_ge ( m, n, rank, x )

    if ( rank == 0 ) then
      exit
    end if

    write ( *, '(2x,i4,2x,10i3)' ) rank, x(1:n)

  end do

  return
end
subroutine tuple_next2_test ( )

!*****************************************************************************80
!
!! TUPLE_NEXT2_TEST tests TUPLE_NEXT2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ), parameter, dimension ( n ) :: xmin = (/ 2, 3, 8 /)
  integer ( kind = 4 ), parameter, dimension ( n ) :: xmax = (/ 4, 3, 5 /)
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TUPLE_NEXT2_TEST'
  write ( *, '(a)' ) '  TUPLE_NEXT2 returns the next "tuple", that is,'
  write ( *, '(a)' ) '  a vector of N integers.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The minimum tuple is '
  write ( *, '(2x,5i8)' ) xmin(1:n)
  write ( *, '(a)' ) '  The maximum tuple is '
  write ( *, '(2x,5i8)' ) xmax(1:n)
  write ( *, '(a)' ) ''

  rank = 0

  do

    call tuple_next2 ( n, xmin, xmax, rank, x )

    if ( rank == 0 ) then
      exit
    end if

    write ( *, '(2x,i4,2x,10i3)' ) rank, x(1:n)

  end do

  return
end
subroutine ubvec_add_test ( )

!*****************************************************************************80
!
!! UBVEC_ADD_TEST tests UBVEC_ADD;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) bvec1(n)
  integer ( kind = 4 ) bvec2(n)
  integer ( kind = 4 ) bvec3(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_ADD_TEST'
  write ( *, '(a)' ) '  UBVEC_ADD adds unsigned binary vectors'
  write ( *, '(a)' ) '  representing unsigned integers;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        I        J        K = I + J'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    
    i = i4_uniform_ab ( 0, 100, seed )
    j = i4_uniform_ab ( 0, 100, seed )

    write ( *, '(a)' ) ''

    write ( *, '(2x,i8,2x,i8)' ) i, j

    k = i + j

    write ( *, '(a20,2x,i8)' ) '  Directly:         ', k

    call ui4_to_ubvec ( i, n, bvec1 )
    call ui4_to_ubvec ( j, n, bvec2 )

    call ubvec_add ( n, bvec1, bvec2, bvec3 )
    call ubvec_to_ui4 ( n, bvec3, k )

    write ( *, '(a20,2x,i8)' ) '  UBVEC_ADD         ', k

  end do

  return
end
subroutine ubvec_print_test ( )

!*****************************************************************************80
!
!! UBVEC_PRINT_TEST tests UBVEC_PRINT.
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

  integer ( kind = 4 ), dimension ( n ) :: ubvec = (/ &
    1, 0, 0, 1, 0, 1, 1, 1, 0, 0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_PRINT_TEST'
  write ( *, '(a)' ) '  UBVEC_PRINT prints an unsigned binary vector.'

  call ubvec_print ( n, ubvec, '  UBVEC:' )

  return
end
subroutine ubvec_to_ui4_test ( )

!*****************************************************************************80
!
!! UBVEC_TO_UI4_TEST tests UBVEC_TO_UI4;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) bvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_TO_UI4_TEST'
  write ( *, '(a)' ) '  UBVEC_TO_UI4 converts an unsigned binary vector'
  write ( *, '(a)' ) '  to an unsigned integer;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I --> BVEC  -->  I'
  write ( *, '(a)' ) ''
  do i = 0, 10
    call ui4_to_ubvec ( i, n, bvec )
    call ubvec_to_ui4 ( n, bvec, i2 )
    write ( *, '(2x,i3,2x,10i1,2x,i3)' ) i, bvec(1:n), i2
  end do

  return
end
subroutine ubvec_xor_test ( )

!*****************************************************************************80
!
!! UBVEC_XOR_TEST tests UBVEC_XOR;
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

  integer ( kind = 4 ) bvec1(n)
  integer ( kind = 4 ) bvec2(n)
  integer ( kind = 4 ) bvec3(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_XOR_TEST'
  write ( *, '(a)' ) '  UBVEC_XOR computes the exclusive OR of two'
  write ( *, '(a)' ) '  unsigned binary vectors representing unsigned integers;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        I        J        K = I XOR J'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    
    i = i4_uniform_ab ( 0, 100, seed )
    j = i4_uniform_ab ( 0, 100, seed )

    call ui4_to_ubvec ( i, n, bvec1 )
    call ui4_to_ubvec ( j, n, bvec2 )
    call ubvec_xor ( n, bvec1, bvec2, bvec3 )

    call ubvec_to_ui4 ( n, bvec3, k )

    write ( *, '(2x,i8,2x,i8,2x,i8)' ) i, j, k

  end do

  return
end
subroutine ui4_to_ubvec_test ( )

!*****************************************************************************80
!
!! UI4_TO_UBVEC_TEST tests UI4_TO_UBVEC;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) bvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UI4_TO_UBVEC_TEST'
  write ( *, '(a)' ) '  UI4_TO_UBVEC converts an unsigned integer to an '
  write ( *, '(a)' ) '  unsigned binary vector;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I --> BVEC  -->  I'
  write ( *, '(a)' ) ''
  do i = 0, 10
    call ui4_to_ubvec ( i, n, bvec )
    call ubvec_to_ui4 ( n, bvec, i2 )
    write ( *, '(2x,i3,2x,10i1,2x,i3)' ) i, bvec(1:n), i2
  end do

  return
end
subroutine vec_colex_next_test ( )

!*****************************************************************************80
!
!! VEC_COLEX_NEXT_TEST tests VEC_COLEX_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  integer ( kind = 4 ) a(dim_num)
  integer ( kind = 4 ) base
  logical more

  base = 3
  more = .false.

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VEC_COLEX_NEXT_TEST'
  write ( *, '(a)' ) '  VEC_COLEX_NEXT generates all DIM_NUM-vectors'
  write ( *, '(a,i8)' ) '  in colex order in a given base BASE.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The dimension DIM_NUM = ', dim_num
  write ( *, '(a,i8)' ) '  The base BASE =         ', base
  write ( *, '(a)' ) ''
 
  do

    call vec_colex_next ( dim_num, base, a, more )

    if ( .not. more ) then
      exit
    end if

    write ( *, '(2x,3i4)' ) a(1:dim_num)

  end do

  return
end
subroutine vec_colex_next2_test ( )

!*****************************************************************************80
!
!! VEC_COLEX_NEXT2_TEST tests VEC_COLEX_NEXT2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  integer ( kind = 4 ) a(dim_num)
  integer ( kind = 4 ), dimension(dim_num) :: base = (/ 2, 1, 3 /)
  logical more

  more = .false.

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VEC_COLEX_NEXT2_TEST'
  write ( *, '(a)' ) '  VEC_COLEX_NEXT2 generates all DIM_NUM-vectors'
  write ( *, '(a,i8)' ) '  in colex order in given bases BASE(1:DIM_NUM).'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The dimension DIM_NUM = ', dim_num
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The base vector:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,3i4)' ) base(1:dim_num)
  write ( *, '(a)' ) ''

  do

    call vec_colex_next2 ( dim_num, base, a, more )

    if ( .not. more ) then
      exit
    end if

    write ( *, '(2x,3i4)' ) a(1:dim_num)

  end do

  return
end
subroutine vec_colex_next3_test ( )

!*****************************************************************************80
!
!! VEC_COLEX_NEXT3_TEST tests VEC_COLEX_NEXT3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 August 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  integer ( kind = 4 ) a(dim_num)
  integer ( kind = 4 ), dimension(dim_num) :: base = (/ 2, 1, 3 /)
  logical more

  more = .false.

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VEC_COLEX_NEXT3_TEST'
  write ( *, '(a)' ) '  VEC_COLEX_NEXT3 generates all DIM_NUM-vectors'
  write ( *, '(a,i8)' ) '  in colex order in given bases BASE(1:DIM_NUM).'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The dimension DIM_NUM = ', dim_num
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The base vector:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,3i4)' ) base(1:dim_num)
  write ( *, '(a)' ) ''

  do

    call vec_colex_next3 ( dim_num, base, a, more )

    if ( .not. more ) then
      exit
    end if

    write ( *, '(2x,3i4)' ) a(1:dim_num)

  end do

  return
end
subroutine vec_gray_next_test ( )

!*****************************************************************************80
!
!! VEC_GRAY_NEXT_TEST tests VEC_GRAY_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) active(n)
  integer ( kind = 4 ), dimension ( n ) :: base = (/ 2, 2, 1, 4 /)
  integer ( kind = 4 ) change
  integer ( kind = 4 ) dir(n)
  logical done
  integer ( kind = 4 ) prod
  integer ( kind = 4 ) rank

  prod = product ( base )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VEC_GRAY_NEXT_TEST'
  write ( *, '(a)' ) '  VEC_GRAY_NEXT generates product space elements.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The number of components is ', n
  write ( *, '(a,i8)' ) '  The number of elements is ', prod
  write ( *, '(a)' ) '  Each component has its own number of degrees of'
  write ( *, '(a)' ) '  freedom.'
  write ( *, '(a)' ) ''
  write ( *, '(a,6i4)' ) '  Rank Change     ', base(1:n)
  write ( *, '(a)' ) ''
  rank = 0
  done = .true.
 
  do
 
    rank = rank + 1
 
    call vec_gray_next ( n, base, a, done, active, dir, change )
 
    if ( done ) then
      exit
    end if

    write ( *, '(2x,i4,2x,i4,2x,4x,6i4)' ) rank, change, a(1:n)

  end do
 
  return
end
subroutine vec_gray_rank_test ( )

!*****************************************************************************80
!
!! VEC_GRAY_RANK_TEST tests VEC_GRAY_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ), dimension ( n ) :: base = (/ 2, 2, 1, 4 /)
  integer ( kind = 4 ) prod
  integer ( kind = 4 ) rank

  prod = product ( base )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VEC_GRAY_RANK_TEST'
  write ( *, '(a)' ) '  VEC_GRAY_RANK ranks product space elements.'
  write ( *, '(a)' ) '  VEC_GRAY_UNRANK unranks them.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The number of components is ', n
  write ( *, '(a,i8)' ) '  The number of elements is ', prod
  write ( *, '(a)' ) '  Each component has its own number of degrees of'
  write ( *, '(a)' ) '  freedom, which, for this example, are:'
  write ( *, '(a)' ) ''
  write ( *, '(a,6i4)' ) '  Rank Change     ', base(1:n)
  write ( *, '(a)' ) ''

  a(1:n) = base(1:n) / 2

  call vec_gray_rank ( n, base, a, rank )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  VEC_GRAY_RANK reports the element '
  write ( *, '(a)' ) ''
  write ( *, '(4x,3x,6i4)' ) a(1:n)
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  has rank ', rank

  return
end
subroutine vec_gray_unrank_test ( )

!*****************************************************************************80
!
!! VEC_GRAY_UNRANK_TEST tests VEC_GRAY_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ), dimension ( n ) :: base = (/ 2, 2, 1, 4 /)
  integer ( kind = 4 ) prod
  integer ( kind = 4 ) rank

  prod = product ( base )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VEC_GRAY_UNRANK_TEST'
  write ( *, '(a)' ) '  VEC_GRAY_UNRANK unranks product space elements.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The number of components is ', n
  write ( *, '(a,i8)' ) '  The number of elements is ', prod
  write ( *, '(a)' ) '  Each component has its own number of degrees of'
  write ( *, '(a)' ) '  freedom, which, for this example, are:'
  write ( *, '(a)' ) ''
  write ( *, '(a,6i4)' ) '  Rank Change     ', base(1:n)
  write ( *, '(a)' ) ''

  rank = 7
  call vec_gray_unrank ( n, base, rank, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  VEC_GRAY_UNRANK reports the element of rank ', rank
  write ( *, '(a)' ) '  is:'
  write ( *, '(a)' ) ''
  write ( *, '(4x,3x,6i4)' ) a(1:n)
  write ( *, '(a)' ) ''

  return
end
subroutine vec_lex_next_test ( )

!*****************************************************************************80
!
!! VEC_LEX_NEXT_TEST tests VEC_LEX_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) base
  logical more

  base = 3
  more = .false.
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VEC_LEX_NEXT_TEST'
  write ( *, '(a)' ) '  VEC_LEX_NEXT generates all N-vectors'
  write ( *, '(a,i8)' ) '  in a given base.  Here we use base ', base
  write ( *, '(a)' ) ''
 
  do

    call vec_lex_next ( n, base, a, more )

    if ( .not. more ) then
      exit
    end if

    write ( *, '(2x,3i4)' ) a(1:n)

  end do

  return
end
subroutine vec_random_test ( )

!*****************************************************************************80
!
!! VEC_RANDOM_TEST tests VEC_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) base
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  base = 3
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VEC_RANDOM_TEST'
  write ( *, '(a)' ) '  VEC_RANDOM generates a random N-vector'
  write ( *, '(a)' ) '  in a given base.'
  write ( *, '(a,i8)' ) '  Here, we use base ', base
  write ( *, '(a)' ) ''

  do i = 1, 5
    call vec_random ( n, base, seed, a )
    write ( *, '(2x,3i4)' ) a(1:n)
  end do
 
  return
end
subroutine vector_constrained_next_test ( )

!*****************************************************************************80
!
!! VECTOR_CONSTRAINED_NEXT_TEST tests VECTOR_CONSTRAINED_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 March 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) constraint
  integer ( kind = 4 ) i
  logical more
  integer ( kind = 4 ) x(n)
  integer ( kind = 4 ) x_max(n)
  integer ( kind = 4 ) x_min(n)
  integer ( kind = 4 ) x_prod

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VECTOR_CONSTRAINED_NEXT_TEST'
  write ( *, '(a)' ) '  VECTOR_CONSTRAINED_NEXT:'
  write ( *, '(a)' ) '  Consider vectors:'
  write ( *, '(a)' ) '    X_MIN(1:N) <= X(1:N) <= X_MAX(1:N),'
  write ( *, '(a)' ) '  Set'
  write ( *, '(a)' ) '    P = Product X_MAX(1:N)'
  write ( *, '(a)' ) '  Accept only vectors for which:'
  write ( *, '(a)' ) '    sum ( (X(1:N)-1) * P / X_MAX(1:N) ) <= P'

  more = .false.
  x_min(1:n) = (/ 2, 2, 1 /)
  x_max(1:n) = (/ 4, 5, 3 /)

  write ( *, '(a)' ) ''
  write ( *, '(a,3i4)' ) '  X_MIN:', x_min(1:n)
  write ( *, '(a,3i4)' ) '  X_MAX:', x_max(1:n)

  i = 0
  x_prod = product ( x_max(1:n) )

  write ( *, '(a)' ) ''
  write ( *, '(a,i12)' ) '  Maximum allowed CONSTRAINT = P = ', x_prod
  write ( *, '(a)' ) ''

  do

    call vector_constrained_next ( n, x_min, x_max, x, constraint, more )

    if ( .not. more ) then
      exit
    end if

    i = i + 1
    write ( *, '(2x,i8,2x,i12,2x,i8,2x,i8,2x,i8)' ) i, constraint, x(1:n)

  end do

  return
end
subroutine vector_constrained_next2_test ( )

!*****************************************************************************80
!
!! VECTOR_CONSTRAINED_NEXT2_TEST tests VECTOR_CONSTRAINED_NEXT2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 March 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 3

  integer ( kind = 4 ) constraint
  integer ( kind = 4 ) i
  logical more
  integer ( kind = 4 ) n
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ) x_max(n_max)
  integer ( kind = 4 ) x_min(n_max)
  integer ( kind = 4 ) x_prod

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VECTOR_CONSTRAINED_NEXT2_TEST'
  write ( *, '(a)' ) '  VECTOR_CONSTRAINED_NEXT2:'
  write ( *, '(a)' ) '  Consider vectors:'
  write ( *, '(a)' ) '    X_MIN(1:N) <= X(1:N) <= X_MAX(1:N),'
  write ( *, '(a)' ) '  Set'
  write ( *, '(a)' ) '    P = Product X_MAX(1:N)'
  write ( *, '(a)' ) '  Accept only vectors for which:'
  write ( *, '(a)' ) '    sum ( X(1:N) * P / X_MAX(1:N) ) <= P'

  x_min(1:n_max) = (/ 1, 1, 1 /)
  x_max(1:n_max) = (/ 5, 6, 4 /)

  do n = 2, n_max

    more = .false.

    write ( *, '(a)' ) ''
    write ( *, '(a,3i4)' ) '  X_MIN:', x_min(1:n)
    write ( *, '(a,3i4)' ) '  X_MAX:', x_max(1:n)

    i = 0
    x_prod = product ( x_max(1:n) )

    write ( *, '(a)' ) ''
    write ( *, '(a,i12)' ) '  Maximum allowed CONSTRAINT = P = ', x_prod
    write ( *, '(a)' ) ''

    do

      call vector_constrained_next2 ( n, x_min, x_max, x, constraint, more )

      if ( .not. more ) then
        exit
      end if

      i = i + 1
      write ( *, '(2x,i8,2x,i12,2x,i8,2x,i8,2x,i8)' ) i, constraint, x(1:n)

    end do

  end do

  return
end
subroutine vector_constrained_next3_test ( )

!*****************************************************************************80
!
!! VECTOR_CONSTRAINED_NEXT3_TEST tests VECTOR_CONSTRAINED_NEXT3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 April 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 3

  real ( kind = 8 ) constraint
  integer ( kind = 4 ) i
  logical more
  integer ( kind = 4 ) n
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ) x_max(n_max)
  integer ( kind = 4 ) x_min(n_max)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VECTOR_CONSTRAINED_NEXT3_TEST'
  write ( *, '(a)' ) '  VECTOR_CONSTRAINED_NEXT3:'
  write ( *, '(a)' ) '  Consider vectors:'
  write ( *, '(a)' ) '    X_MIN(1:N) <= X(1:N) <= X_MAX(1:N),'
  write ( *, '(a)' ) '  Set'
  write ( *, '(a)' ) '    CONSTRAINT = sum ( X(1:N) / X_MAX(1:N) )'
  write ( *, '(a)' ) '  Accept only vectors for which:'
  write ( *, '(a)' ) '    CONSTRAINT <= 1'

  x_min(1:n_max) = (/ 1, 1, 1 /)
  x_max(1:n_max) = (/ 5, 6, 4 /)

  do n = 2, n_max

    more = .false.

    write ( *, '(a)' ) ''
    write ( *, '(a,3i4)' ) '  X_MIN:', x_min(1:n)
    write ( *, '(a,3i4)' ) '  X_MAX:', x_max(1:n)
    write ( *, '(a)' ) ''

    i = 0

    do

      call vector_constrained_next3 ( n, x_min, x_max, x, constraint, more )

      if ( .not. more ) then
        exit
      end if

      i = i + 1
      write ( *, '(2x,i8,2x,g14.6,2x,i8,2x,i8,2x,i8)' ) i, constraint, x(1:n)

    end do

  end do

  return
end
subroutine vector_constrained_next4_test ( )

!*****************************************************************************80
!
!! VECTOR_CONSTRAINED_NEXT4_TEST tests VECTOR_CONSTRAINED_NEXT4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 May 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 3

  real ( kind = 8 ), dimension ( n_max ) :: alpha = (/ &
    4.0D+00, 3.0D+00, 5.0D+00 /)
  integer ( kind = 4 ) i
  logical more
  integer ( kind = 4 ) n
  real ( kind = 8 ) :: q = 20.0D+00
  real ( kind = 8 ) total
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ), dimension ( n_max ) :: x_max = (/ &
    2, 6, 4 /)
  integer ( kind = 4 ), dimension ( n_max ) :: x_min = (/ &
    1, 0, 1 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VECTOR_CONSTRAINED_NEXT4_TEST'
  write ( *, '(a)' ) '  VECTOR_CONSTRAINED_NEXT4:'
  write ( *, '(a)' ) '  Consider vectors:'
  write ( *, '(a)' ) '    X_MIN(1:N) <= X(1:N) <= X_MAX(1:N),'
  write ( *, '(a)' ) '  Set'
  write ( *, '(a)' ) '    TOTAL = sum ( ALPHA(1:N) * X(1:N) )'
  write ( *, '(a)' ) '  Accept only vectors for which:'
  write ( *, '(a)' ) '    TOTAL <= Q'

  do n = 2, n_max

    more = .false.

    write ( *, '(a)' ) ''
    write ( *, '(a,3g14.6)' ) '  ALPHA:', alpha(1:n)
    write ( *, '(a, g14.6)' ) '  Q:    ', q
    write ( *, '(a,3i4)'    ) '  X_MIN:', x_min(1:n)
    write ( *, '(a,3i4)'    ) '  X_MAX:', x_max(1:n)
    write ( *, '(a)' ) ''

    i = 0

    do

      call vector_constrained_next4 ( n, alpha, x_min, x_max, x, q, more )

      if ( .not. more ) then
        exit
      end if

      total = dot_product ( alpha(1:n), real ( x(1:n), kind = 8 ) )
      i = i + 1
      write ( *, '(2x,i8,2x,g14.6,2x,i8,2x,i8,2x,i8)' ) i, total, x(1:n)

    end do

  end do

  return
end
subroutine vector_constrained_next5_test ( )

!*****************************************************************************80
!
!! VECTOR_CONSTRAINED_NEXT5_TEST tests VECTOR_CONSTRAINED_NEXT5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) base
  integer ( kind = 4 ) i
  logical more
  integer ( kind = 4 ) sum_max
  integer ( kind = 4 ) sum_min
  integer ( kind = 4 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VECTOR_CONSTRAINED_NEXT5_TEST'
  write ( *, '(a)' ) '  VECTOR_CONSTRAINED_NEXT5:'
  write ( *, '(a)' ) '  Generate integer vectors X such that:'
  write ( *, '(a)' ) '    SUM_MIN <= sum ( X(1:N) ) <= SUM_MAX,'
  write ( *, '(a)' ) '  We require every X(I) to be at least 1.'

  sum_min = 5
  sum_max = 7
  base = 0
  more = .false.

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  N =       ', n
  write ( *, '(a,i8)' ) '  SUM_MIN = ', sum_min
  write ( *, '(a,i8)' ) '  SUM_MAX = ', sum_max
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         #        X(1)      X(2)      X(3)'
  write ( *, '(a)' ) ''

  i = 0

  do

    call vector_constrained_next5 ( n, x, sum_min, sum_max, base, more )

    if ( .not. more ) then
      exit
    end if

    i = i + 1
    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, x(1:n)

  end do

  return
end
subroutine vector_constrained_next6_test ( )

!*****************************************************************************80
!
!! VECTOR_CONSTRAINED_NEXT6_TEST tests VECTOR_CONSTRAINED_NEXT6.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 3

  real ( kind = 8 ), dimension ( n_max ) :: alpha = (/ &
    4.0D+00, 3.0D+00, 5.0D+00 /)
  integer ( kind = 4 ) i
  logical more
  integer ( kind = 4 ) n
  real ( kind = 8 ) :: q_max = 20.0D+00
  real ( kind = 8 ) :: q_min = 16.0D+00
  real ( kind = 8 ) total
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ), dimension ( n_max ) :: x_max = (/ &
    2, 6, 4 /)
  integer ( kind = 4 ), dimension ( n_max ) :: x_min = (/ &
    1, 0, 1 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VECTOR_CONSTRAINED_NEXT6_TEST'
  write ( *, '(a)' ) '  VECTOR_CONSTRAINED_NEXT6:'
  write ( *, '(a)' ) '  Consider vectors:'
  write ( *, '(a)' ) '    X_MIN(1:N) <= X(1:N) <= X_MAX(1:N),'
  write ( *, '(a)' ) '  Set'
  write ( *, '(a)' ) '    TOTAL = sum ( ALPHA(1:N) * X(1:N) )'
  write ( *, '(a)' ) '  Accept only vectors for which:'
  write ( *, '(a)' ) '    Q_MIN <= TOTAL <= Q_MAX'

  do n = 2, n_max

    more = .false.

    write ( *, '(a)' ) ''
    write ( *, '(a,3g14.6)' ) '  ALPHA:', alpha(1:n)
    write ( *, '(a, g14.6)' ) '  Q_MIN:', q_min
    write ( *, '(a, g14.6)' ) '  Q_MAX:', q_max
    write ( *, '(a,3i4)'    ) '  X_MIN:', x_min(1:n)
    write ( *, '(a,3i4)'    ) '  X_MAX:', x_max(1:n)
    write ( *, '(a)' ) ''

    i = 0

    do

      call vector_constrained_next6 ( n, alpha, x_min, x_max, x, q_min, &
        q_max, more )

      if ( .not. more ) then
        exit
      end if

      total = dot_product ( alpha(1:n), real ( x(1:n), kind = 8 ) )
      i = i + 1
      write ( *, '(2x,i8,2x,g14.6,2x,i8,2x,i8,2x,i8)' ) i, total, x(1:n)

    end do

  end do

  return
end
subroutine vector_constrained_next7_test ( )

!*****************************************************************************80
!
!! VECTOR_CONSTRAINED_NEXT7_TEST tests VECTOR_CONSTRAINED_NEXT7.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 May 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 3

  real ( kind = 8 ), dimension ( n_max ) :: alpha = (/ &
    4.0D+00, 3.0D+00, 5.0D+00 /)
  integer ( kind = 4 ) i
  logical more
  integer ( kind = 4 ) n
  real ( kind = 8 ) :: q_max = 20.0D+00
  real ( kind = 8 ) :: q_min = 16.0D+00
  real ( kind = 8 ) total
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ), dimension ( n_max ) :: x_max = (/ &
    2, 6, 4 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VECTOR_CONSTRAINED_NEXT7_TEST'
  write ( *, '(a)' ) '  VECTOR_CONSTRAINED_NEXT7:'
  write ( *, '(a)' ) '  Consider vectors:'
  write ( *, '(a)' ) '    0 <= X(1:N) <= X_MAX(1:N),'
  write ( *, '(a)' ) '  Set'
  write ( *, '(a)' ) '    TOTAL = sum ( ALPHA(1:N) * X(1:N) )'
  write ( *, '(a)' ) '  Accept only vectors for which:'
  write ( *, '(a)' ) '    Q_MIN <= TOTAL <= Q_MAX'

  do n = 2, n_max

    more = .false.

    write ( *, '(a)' ) ''
    write ( *, '(a,3g14.6)' ) '  ALPHA:', alpha(1:n)
    write ( *, '(a, g14.6)' ) '  Q_MIN:', q_min
    write ( *, '(a, g14.6)' ) '  Q_MAX:', q_max
    write ( *, '(a,3i4)'    ) '  X_MAX:', x_max(1:n)
    write ( *, '(a)' ) ''

    i = 0

    do

      call vector_constrained_next7 ( n, alpha, x_max, x, q_min, &
        q_max, more )

      if ( .not. more ) then
        exit
      end if

      total = dot_product ( alpha(1:n), real ( x(1:n), kind = 8 ) )
      i = i + 1
      write ( *, '(2x,i8,2x,g14.6,2x,i8,2x,i8,2x,i8)' ) i, total, x(1:n)

    end do

  end do

  return
end
subroutine vector_next_test ( )

!*****************************************************************************80
!
!! VECTOR_NEXT_TEST tests VECTOR_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 July 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 3

  integer ( kind = 4 ) i
  logical              more
  integer ( kind = 4 ) n
  integer ( kind = 4 ) x(n_max)
  integer ( kind = 4 ), dimension ( n_max ) :: x_max = (/ &
    2, 6, 4 /)
  integer ( kind = 4 ), dimension ( n_max ) :: x_min = (/ &
    1, 4, 3 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VECTOR_NEXT_TEST'
  write ( *, '(a)' ) '  VECTOR_NEXT:'
  write ( *, '(a)' ) '  Generate all vectors X such that:'
  write ( *, '(a)' ) '    X_MIN(1:N) <= X(1:N) <= X_MAX(1:N),'

  do n = 2, n_max

    more = .false.

    write ( *, '(a)' ) ''
    write ( *, '(2x,a4,4x,i8,2x,i8,2x,i8)' ) 'XMIN', x_min(1:n)

    i = 0

    do

      call vector_next ( n, x_min, x_max, x, more )

      if ( .not. more ) then
        exit
      end if

      i = i + 1
      write ( *, '(2x,i4,4x,i8,2x,i8,2x,i8)' ) i, x(1:n)

    end do

    write ( *, '(2x,a4,4x,i8,2x,i8,2x,i8)' ) 'XMAX', x_max(1:n)

  end do

  return
end
subroutine ytb_enum_test ( )

!*****************************************************************************80
!
!! YTB_ENUM_TEST tests YTB_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) pi

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'YTB_ENUM_TEST'
  write ( *, '(a)' ) '  YTB_ENUM counts Young table.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  N    YTB(N)'
  write ( *, '(a)' ) ''

  do i = 0, n
    call ytb_enum ( i, pi )
    write ( *, '(2x,2i10)' ) i, pi
  end do

  return
end
subroutine ytb_next_test ( )

!*****************************************************************************80
!
!! YTB_NEXT_TEST tests YTB_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ), dimension ( n ) :: lambda = (/ 3, 2, 1, 0, 0, 0 /)
  logical more

  a(1:n) = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'YTB_NEXT_TEST'
  write ( *, '(a)' ) '  YTB_NEXT generates Young tables.'
  write ( *, '(a)' ) ''
  more = .false.
 
  do
 
    call ytb_next ( n, lambda, a, more )
 
    call ytb_print ( n, a, '' )
 
    if ( .not. more ) then
      exit
    end if

  end do

  return
end
subroutine ytb_random_test ( )

!*****************************************************************************80
!
!! YTB_RANDOM_TEST tests YTB_RANDOM.
!
!  Discussion:
!
!    I4_LOG_10 ( I ) + 1 is the number of decimal digits in I.
!
!  Modified:
!
!    05 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), dimension ( n ) :: lambda = (/ 3, 2, 1, 0, 0, 0 /)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'YTB_RANDOM_TEST'
  write ( *, '(a)' ) '  YTB_RANDOM generates a random Young table'

  seed = 123456789

  do i = 1, 5
 
    call ytb_random ( n, lambda, seed, a )

    call ytb_print ( n, a, '' )
 
  end do
 
  return
end
