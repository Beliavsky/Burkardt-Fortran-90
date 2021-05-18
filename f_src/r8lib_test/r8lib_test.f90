program main

!*****************************************************************************80
!
!! r8lib_test() tests r8lib().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8lib_test()'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test r8lib().'

  call i4int_to_r8int_test ( )

  call perm0_check_test ( )
  call perm0_uniform_test ( )

  call perm1_check_test ( )
  call perm1_uniform_test ( )

  call r8_abs_test ( )
  call r8_acos_test ( )
  call r8_acosh_test ( )
  call r8_asin_test ( )
  call r8_asinh_test ( )
  call r8_atan_test ( )
  call r8_atanh_test ( )
  call r8_big_test ( )

  call r8_cas_test ( )
  call r8_ceiling_test ( )
  call r8_choose_test ( )
  call r8_chop_test ( )
  call r8_cosd_test ( )
  call r8_cot_test ( )
  call r8_cotd_test ( )
  call r8_csc_test ( )
  call r8_cscd_test ( )
  call r8_cube_root_test ( )

  call r8_degrees_test ( )
  call r8_diff_test ( )
  call r8_digit_test ( )
  call r8_divide_i4_test ( )

  call r8_e_test ( )
  call r8_epsilon_test ( )
  call r8_epsilon_compute_test ( )
  call r8_exp_test ( )

  call r8_factorial_test ( )
  call r8_factorial_stirling_test ( )
  call r8_factorial2_test ( )
  call r8_fall_test ( )
  call r8_floor_test ( )
  call r8_fractional_test ( )

  call r8_gamma_test ( )
  call r8_gamma_log_test ( )

  call r8_huge_test ( )
!
!  Note that R8_IS_INF_TEST can cause IEEE warning flags to be set.
!
  if ( .false. ) then
    call r8_is_inf_test ( )
  end if
!
!  Note that R8_IS_NAN_TEST can cause IEEE warning flags to be set.
!
  if ( .false. ) then
    call r8_is_nan_test ( )
  end if

  call r8_log_2_test ( )
  call r8_log_10_test ( )
  call r8_log_b_test ( )

  call r8_mant_test ( )
  call r8_max_test ( )
  call r8_min_test ( )
  call r8_mod_test ( )
  call r8_modp_test ( )
  call r8_mop_test ( )

  call r8_nint_test ( )
  call r8_normal_01_test ( )
  call r8_normal_ab_test ( )
!
!  Note that R8_NTH_ROOT_TEST can cause IEEE warning flags to be set.
!
  if ( .false. ) then
    call r8_nth_root_test ( )
  end if

  call r8_pi_test ( )
  call r8_power_test ( )
  call r8_power_fast_test ( )

  call r8_radians_test ( )
  call r8_relu_test ( )
  call r8_rise_test ( )
  call r8_round2_test ( )
  call r8_roundb_test ( )
  call r8_roundx_test ( )

  call r8_secd_test ( )
  call r8_sech_test ( )
  call r8_sigmoid_test ( )
  call r8_sign_test ( )
  call r8_sign_match_test ( )
  call r8_sign3_test ( )
  call r8_sincos_sum_test ( )
  call r8_sind_test ( )
  call r8_softplus_test ( )
  call r8_swap_test ( )
  call r8_swap3_test ( )

  call r8_tand_test ( )
  call r8_tiny_test ( )
  call r8_to_i4_test ( )
  call r8_to_r8_discrete_test ( )

  call r8_uniform_01_test ( )
  call r8_uniform_ab_test ( )

  call r8_walsh_1d_test ( )
  call r8_wrap_test ( )

  call r82col_print_part_test ( )

  call r82row_order_type_test ( )
  call r82row_part_quick_a_test ( )
  call r82row_print_part_test ( )
  call r82row_sort_heap_index_a_test ( )
  call r82row_sort_quick_a_test ( )

  call r83col_print_part_test ( )

  call r83row_print_part_test ( )

  call r8block_expand_linear_test ( )
  call r8block_print_test ( )

  call r8r8vec_index_insert_unique_test ( )

  call r8r8r8vec_index_insert_unique_test ( )

  call r8int_to_i4int_test ( )

  call r8mat_cholesky_factor_test ( )
  call r8mat_cholesky_factor_upper_test ( )
  call r8mat_cholesky_inverse_test ( )
  call r8mat_cholesky_solve_test ( )
  call r8mat_cholesky_solve_upper_test ( )
  call r8mat_det_2d_test ( )
  call r8mat_det_3d_test ( )
  call r8mat_det_4d_test ( )
  call r8mat_det_5d_test ( )
  call r8mat_expand_linear_test ( )
  call r8mat_expand_linear2_test ( )
  call r8mat_fs_test ( )
  call r8mat_fss_test ( )
  call r8mat_givens_post_test ( )
  call r8mat_givens_pre_test ( )
  call r8mat_hess_test ( )
  call r8mat_house_axh_test ( )
  call r8mat_house_form_test ( )
  call r8mat_house_post_test ( )
  call r8mat_house_pre_test ( )
  call r8mat_indicator_test ( )
  call r8mat_inverse_2d_test ( )
  call r8mat_inverse_3d_test ( )
  call r8mat_inverse_4d_test ( )
  call r8mat_is_integer_test ( )
  call r8mat_jac_test ( )
  call r8mat_kronecker_test ( )
  call r8mat_l_inverse_test ( )
  call r8mat_l_print_test ( )
  call r8mat_l_solve_test ( )
  call r8mat_l1_inverse_test ( )
  call r8mat_lt_solve_test ( )
  call r8mat_lu_test ( )
  call r8mat_max_test ( )
  call r8mat_max_index_test ( )
  call r8mat_maxcol_minrow_test ( )
  call r8mat_maxrow_mincol_test ( )
  call r8mat_min_test ( )
  call r8mat_min_index_test ( )
  call r8mat_mincol_maxrow_test ( )
  call r8mat_minrow_maxcol_test ( )
  call r8mat_mm_test ( )
  call r8mat_mv_test ( )
  call r8mat_nint_test ( )
  call r8mat_nonzeros_test ( )
  call r8mat_norm_fro_test ( )
  call r8mat_norm_fro_affine_test ( )
  call r8mat_norm_l1_test ( )
  call r8mat_nullspace_test ( )
  call r8mat_nullspace_size_test ( )
  call r8mat_orth_uniform_test ( )
  call r8mat_plot_test ( )
  call r8mat_power_method_test ( )
  call r8mat_print_test ( )
  call r8mat_print_some_test ( )
  call r8mat_product_elementwise_test ( )
  call r8mat_ref_test ( )
  call r8mat_rref_test ( )
  call r8mat_scale_01_test ( )
  call r8mat_scale_ab_test ( )
  call r8mat_solve_test ( )
  call r8mat_solve_2d_test ( )
  call r8mat_solve_3d_test ( )
  call r8mat_solve2_test ( )
  call r8mat_standardize_test ( )
  call r8mat_symm_jacobi_test ( )
  call r8mat_to_r8plu_test ( )
  call r8mat_trace_test ( )
  call r8mat_transpose_test ( )
  call r8mat_transpose_in_place_test ( )
  call r8mat_transpose_new_test ( )
  call r8mat_transpose_print_test ( )
  call r8mat_transpose_print_some_test ( )
  call r8mat_u_inverse_test ( )
  call r8mat_u_solve_test ( )
  call r8mat_u1_inverse_test ( )
  call r8mat_uniform_ab_test ( )
  call r8mat_ut_solve_test ( )

  call r8plu_det_test ( )
  call r8plu_inverse_test ( )
  call r8plu_mul_test ( )
  call r8plu_solve_test ( )
  call r8plu_to_r8mat_test ( )

  call r8rows_to_r8mat_test ( )

  call r8slmat_print_test ( )

  call r8vec_amax_test ( )
  call r8vec_amin_test ( )
  call r8vec_binary_next_test ( )
  call r8vec_bracket_test ( )
  call r8vec_bracket2_test ( )
  call r8vec_bracket3_test ( )
  call r8vec_bracket5_test ( )
  call r8vec_cheby_extreme_test ( )
  call r8vec_cheby_zero_test ( )
  call r8vec_concatenate_test ( )
  call r8vec_correlation_test ( )
  call r8vec_convolution_test ( )
  call r8vec_convolution_circ_test ( )
  call r8vec_dif_test ( )
  call r8vec_diff_norm_li_test ( )
  call r8vec_direct_product_test ( )
  call r8vec_direct_product2_test ( )
  call r8vec_dot_product_test ( )
  call r8vec_even_test ( )
  call r8vec_even2_test ( )
  call r8vec_even3_test ( )
  call r8vec_expand_linear_test ( )
  call r8vec_frac_test ( )
  call r8vec_heap_d_extract_test ( )
  call r8vec_heap_d_insert_test ( )
  call r8vec_heap_d_max_test ( )
  call r8vec_histogram_test ( )
  call r8vec_house_column_test ( )
  call r8vec_identity_row_test ( )
  call r8vec_index_insert_test ( )
  call r8vec_index_delete_all_test ( )
  call r8vec_index_delete_dupes_test ( )
  call r8vec_index_delete_one_test ( )
  call r8vec_index_insert_unique_test ( )
  call r8vec_index_order_test ( )
  call r8vec_index_search_test ( )
  call r8vec_index_sorted_range_test ( )
  call r8vec_indexed_heap_d_test ( )
  call r8vec_indexed_heap_d_extract_test ( )
  call r8vec_indexed_heap_d_insert_test ( )
  call r8vec_indexed_heap_d_max_test ( )
  call r8vec_indicator0_test ( )
  call r8vec_indicator1_test ( )
  call r8vec_is_binary_test ( )
  call r8vec_is_distinct_test ( )
  call r8vec_is_integer_test ( )
  call r8vec_legendre_test ( )
  call r8vec_linspace_test ( )
  call r8vec_linspace2_test ( )
  call r8vec_max_test ( )
  call r8vec_max_abs_index_test ( )
  call r8vec_max_index_test ( )
  call r8vec_mean_test ( )
  call r8vec_mean_geometric_test ( )
  call r8vec_mean_running_test ( )
  call r8vec_mean_update_test ( )
  call r8vec_median_test ( )
  call r8vec_midspace_test ( )
  call r8vec_min_test ( )
  call r8vec_min_index_test ( )
  call r8vec_mirror_next_test ( )
  call r8vec_mirror_ab_next_test ( )
  call r8vec_nint_test ( )
  call r8vec_norm_test ( ) 
  call r8vec_norm_affine_test ( )
  call r8vec_norm_l0_test ( )
  call r8vec_norm_l1_test ( )
  call r8vec_norm_l2_test ( )
  call r8vec_norm_li_test ( )
  call r8vec_normal_01_test ( )
  call r8vec_normal_ab_test ( )
  call r8vec_normalize_l1_test ( )
  call r8vec_order_type_test ( )
  call r8vec_permute_test ( )
  call r8vec_permute_cyclic_test ( )
  call r8vec_permute_uniform_test ( )
  call r8vec_polarize_test ( )
  call r8vec_print_test ( )
  call r8vec_print_part_test ( )
  call r8vec_print_some_test ( )
  call r8vec_reverse_test ( )
  call r8vec_rotate_test ( )
  call r8vec_rsquared_test ( )
  call r8vec_rsquared_adjusted_test ( )
  call r8vec_scale_01_test ( )
  call r8vec_scale_ab_test ( )
  call r8vec_search_binary_a_test ( )
  call r8vec_sign3_running_test ( )
  call r8vec_smooth_test ( )
  call r8vec_softmax_test ( )
  call r8vec_sort_bubble_a_test ( )
  call r8vec_sort_heap_a_test ( )
  call r8vec_sort_heap_d_test ( )
  call r8vec_sort_heap_index_a_test ( )
  call r8vec_sort_heap_index_d_test ( )
  call r8vec_sort_heap_mask_a_test ( )
  call r8vec_sort_insert_a_test ( )
  call r8vec_sort_insert_index_a_test ( )
  call r8vec_sort_quick_a_test ( )
  call r8vec_sorted_merge_a_test ( )
  call r8vec_sorted_nearest_test ( )
  call r8vec_sorted_range_test ( )
  call r8vec_sorted_split_test ( )
  call r8vec_sorted_undex_test ( )
  call r8vec_sorted_unique_test ( )
  call r8vec_sorted_unique_count_test ( )
  call r8vec_sorted_unique_hist_test ( )
  call r8vec_split_test ( )
  call r8vec_standardize_test ( )
  call r8vec_std_test ( )
  call r8vec_std_sample_test ( )
  call r8vec_std_sample_update_test ( )
  call r8vec_std_update_test ( )
  call r8vec_sum_test ( )
  call r8vec_sum_running_test ( )
  call r8vec_transpose_print_test ( )
  call r8vec_undex_test ( )
  call r8vec_uniform_01_test ( )
  call r8vec_uniform_ab_test ( )
  call r8vec_uniform_unit_test ( )
  call r8vec_variance_test ( )
  call r8vec_variance_circular_test ( )
  call r8vec_variance_sample_test ( )
  call r8vec_variance_sample_update_test ( )
  call r8vec_variance_update_test ( )
  call r8vec2_print_test ( )
  call r8vec2_print_some_test ( )
  call r8vec2_sort_a_test ( )
  call r8vec2_sort_d_test ( )
  call r8vec2_sort_heap_index_a_test ( )
  call r8vec2_sorted_unique_test ( )
  call r8vec2_sorted_unique_index_test ( )
  call r8vec2_sum_max_index_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8lib_test()'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine i4int_to_r8int_test ( )

!*****************************************************************************80
!
!! I4INT_TO_R8_INT_TEST tests I4INT_TO_R8INT;
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

  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) ir
  real ( kind = 8 ) r
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) :: rhi = 200.0D+00
  real ( kind = 8 ) rhi2
  real ( kind = 8 ) :: rlo = 100.0D+00
  real ( kind = 8 ) rlo2
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) :: test_num = 10

  ilo = 1
  ihi = 11

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4INT_TO_R8INT_TEST'
  write ( *, '(a)' ) '  For data in an interval,'
  write ( *, '(a)' ) '  I4INT_TO_R8INT converts an integer to a real;'
  write ( *, '(a)' ) ''
  write ( *, '(a,2i8)' ) '  Integer interval: ', ilo, ihi
  write ( *, '(a,2g14.6)' ) '  Real interval: ', rlo, rhi
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       R             I(R)  R(I(R))'
  write ( *, '(a)' ) ''

  seed = 123456789

  rlo2 = rlo - 15.0D+00
  rhi2 = rhi + 15.0D+00

  do test = 1, test_num
    r = r8_uniform_ab ( rlo2, rhi2, seed )
    call r8int_to_i4int ( rlo, rhi, r, ilo, ihi, ir )
    call i4int_to_r8int ( ilo, ihi, ir, rlo, rhi, r2 )
    write ( *, '(2x,g14.6,i8,g14.6)' ) r, ir, r2
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
  write ( *, '(a)' ) '  PERM0_UNIFORM randomly selects a permutation of 0...N-1'
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
  write ( *, '(a)' ) '  PERM1_UNIFORM randomly selects a permutation of 1...N'
  write ( *, '(a)' ) ''

  seed = 123456789

  do test = 1, 5

    call perm1_uniform ( n, seed, p )
    write ( *, '(2x,10i4)' ) p(1:n)

  end do

  return
end
subroutine r8_abs_test ( )

!*****************************************************************************80
!
!! R8_ABS_TEST tests R8_ABS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_abs
  real ( kind = 8 ) r8_absolute
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) :: r8_hi = +5.0D+00
  real ( kind = 8 ) :: r8_lo = -5.0D+00
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) :: test_num = 10

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ABS_TEST'
  write ( *, '(a)' ) '  R8_ABS returns the absolute value of an R8.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X         R8_ABS(X)'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    r8 = r8_uniform_ab ( r8_lo, r8_hi, seed )
    r8_absolute = r8_abs ( r8 )
    write ( *, '(2x,f10.6,2x,f10.6)' ) r8, r8_absolute
  end do

  return
end
subroutine r8_acos_test ( )

!*****************************************************************************80
!
!! R8_ACOS_TEST tests R8_ACOS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) c
  real ( kind = 8 ) r8_acos
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ACOS_TEST'
  write ( *, '(a)' ) '  R8_ACOS computes the arc-cosine of an angle.' 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       C            R8_ACOS(C)        ACOS(C)'
  write ( *, '(a)' ) ''

  do test = -1, 13

    c = real ( test - 6, kind = 8 ) / real ( 6, kind = 8 )

    if ( -1.0D+00 <= c .and. c <= 1.0D+00 ) then
      write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) &
        c, r8_acos ( c ), acos ( c )
    else
      write ( *, '(2x,g14.6,2x,g14.6)' ) &
        c, r8_acos ( c )
    end if

  end do

  return
end
subroutine r8_acosh_test ( )

!*****************************************************************************80
!
!! R8_ACOSH_TEST tests R8_ACOSH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) r8_acosh
  integer ( kind = 4 ) test
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ACOSH_TEST'
  write ( *, '(a)' ) '  R8_ACOSH computes the arc-hyperbolic-cosine of an angle.' 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            A=R8_ACOSH(X)    COSH(A)'
  write ( *, '(a)' ) ''

  do test = 0, 8

    x = 1.0D+00 + real ( test, kind = 8 ) / 2.0D+00
    a = r8_acosh ( x )
    x2 = cosh ( a )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, a, x2

  end do

  return
end
subroutine r8_asin_test ( )

!*****************************************************************************80
!
!! R8_ASIN_TEST tests R8_ASIN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_asin
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ASIN_TEST'
  write ( *, '(a)' ) '  R8_ASIN computes the inverse sine'
  write ( *, '(a)' ) '  of a given value.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X       R8_ASIN(X)     SIN(R8_ASIN(X))'
  write ( *, '(a)' ) ''

  do i = 0, 10
    x = 1.0D+00 + real ( i, kind = 8 ) / 5.0D+00
    a = r8_asin ( x )
    x2 = sin ( a )
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, a, x2
  end do

  return
end
subroutine r8_asinh_test ( )

!*****************************************************************************80
!
!! R8_ASINH_TEST tests R8_ASINH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_asinh
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ASINH_TEST'
  write ( *, '(a)' ) '  R8_ASINH computes the inverse hyperbolic sine'
  write ( *, '(a)' ) '  of a given value.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X       R8_ASINH(X)     SINH(R8_ASINH(X))'
  write ( *, '(a)' ) ''

  do i = 0, 10
    x = 1.0D+00 + real ( i, kind = 8 ) / 5.0D+00
    a = r8_asinh ( x )
    x2 = sinh ( a )
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, a, x2
  end do

  return
end
subroutine r8_atan_test ( )

!*****************************************************************************80
!
!! R8_ATAN_TEST tests R8_ATAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 8

  real ( kind = 8 ) r8_atan
  integer ( kind = 4 ) test
  real ( kind = 8 ) x
  real ( kind = 8 ), dimension ( test_num ) :: xtest = (/ &
     1.0D+00,  1.0D+00,  0.0D+00, -1.0D+00, &
    -1.0D+00, -1.0D+00,  0.0D+00,  1.0D+00 /)
  real ( kind = 8 ) y
  real ( kind = 8 ), dimension ( test_num ) :: ytest = (/ &
     0.0D+00,  1.0D+00,  1.0D+00,  1.0D+00, &
     0.0D+00, -1.0D+00, -1.0D+00, -1.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ATAN_TEST'
  write ( *, '(a)' ) '  R8_ATAN computes the arc-tangent given Y and X;'
  write ( *, '(a)' ) '  ATAN2 is the system version of this routine.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X               Y          ATAN2(Y,X)    R8_ATAN(Y,X)'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    x = xtest(test)
    y = ytest(test)
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, y, atan2 ( y, x ), r8_atan ( y, x )
  end do

  return
end
subroutine r8_atanh_test ( )

!*****************************************************************************80
!
!! R8_ATANH_TEST tests R8_ATANH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_atanh
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ATANH_TEST'
  write ( *, '(a)' ) '  R8_ATANH computes the inverse hyperbolic tangent'
  write ( *, '(a)' ) '  of a given value.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X       R8_ATANH(X)     TANH(R8_ATANH(X))'
  write ( *, '(a)' ) ''

  do i = -2, 9
    x = real ( i, kind = 8 ) / 10.0D+00
    a = r8_atanh ( x )
    x2 = tanh ( a )
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, a, x2
  end do

  return
end
subroutine r8_big_test ( )

!*****************************************************************************80
!
!! R8_BIG_TEST tests R8_BIG.
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

  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_big

  r8 = 1.0D+00
  r8 = huge ( r8 )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BIG_TEST:'
  write ( *, '(a)' ) '  R8_BIG returns a "big" R8;'
  write ( *, '(a)' ) ''
  write ( *, '(a,g24.16)' ) '    R8_BIG ( ) =       ', r8_big ( )
  write ( *, '(a,g24.16)' ) '    HUGE ( 1.0D+00 ) = ', r8

  return
end
subroutine r8_cas_test ( )

!*****************************************************************************80
!
!! R8_CAS_TEST tests R8_CAS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) r8_cas
  real ( kind = 8 ) r8_pi
  integer ( kind = 4 ), parameter :: test_num = 12
  integer ( kind = 4 ) test
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CAS_TEST'
  write ( *, '(a)' ) '  R8_CAS evaluates the casine of a number.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        X           R8_CAS ( X )'
  write ( *, '(a)' ) ''
  do test = 0, test_num
    x = r8_pi ( ) * real ( test, kind = 8 ) / real ( test_num, kind = 8 )
    write ( *, '(2x,g14.6,2x,g14.6)' ) x, r8_cas ( x )
  end do

  return
end
subroutine r8_ceiling_test ( )

!*****************************************************************************80
!
!! R8_CEILING_TEST tests R8_CEILING.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_ceiling
  real ( kind = 8 ) rval
  real ( kind = 8 ) rval2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CEILING_TEST'
  write ( *, '(a)' ) '  R8_CEILING rounds a value up.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X        R8_CEILING(X)'
  write ( *, '(a)' ) ''

  do i = -6, 6
    rval = real ( i, kind = 8 ) / 5.0D+00
    rval2 = r8_ceiling ( rval )
    write ( *, '(2x,g14.6,2x,g14.6)' ) rval, rval2
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CHOOSE_TEST'
  write ( *, '(a)' ) '  R8_CHOOSE evaluates C(N,K).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         N         K       CNK'
 
  do n = 0, 5
    write ( *, '(a)' ) ''
    do k = 0, n
      cnk = r8_choose ( n, k )
      write ( *, '(2x,i8,2x,i8,2x,g14.6)' ) n, k, cnk
    end do
  end do
 
  return
end
subroutine r8_chop_test ( )

!*****************************************************************************80
!
!! R8_CHOP_TEST tests R8_CHOP.
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

  integer ( kind = 4 ) place
  real ( kind = 8 ) r8_chop
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CHOP_TEST'
  write ( *, '(a)' ) '  R8_CHOP truncates an R8 to a given number of binary places.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    Places        R8'
  write ( *, '(a)' ) ''

  do place = 0, 31
    x = r8_chop ( place, r8_pi )
    write ( *, '(2x,i8,2x,g24.16)' ) place, x
  end do
 
  return
end
subroutine r8_cosd_test ( )

!*****************************************************************************80
!
!! R8_COSD_TEST tests R8_COSD.
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

  real ( kind = 8 ) angle
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_cosd

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_COSD_TEST'
  write ( *, '(a)' ) '  R8_COSD computes the cosine of an angle'
  write ( *, '(a)' ) '  given in degrees.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  ANGLE    R8_COSD(ANGLE)'
  write ( *, '(a)' )
 
  do i = 0, 360, 15
    angle = real ( i, kind = 8 )
    write ( *, '(2x,f8.2,2x,g14.6)' ) angle, r8_cosd ( angle )
  end do
 
  return
end
subroutine r8_cot_test ( )

!*****************************************************************************80
!
!! R8_COT_TEST tests R8_COT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) angle
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_cot
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_COT_TEST'
  write ( *, '(a)' ) '  R8_COT computes the cotangent of an angle.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  ANGLE    R8_COT(ANGLE)'
  write ( *, '(a)' )
 
  do i = 0, 24
    angle = r8_pi * real ( i, kind = 8 ) / 12.0D+00
    if ( mod ( i, 12 ) == 0 ) then
      write ( *, '(2x,f8.2,2x,a)' ) angle, '  Undefined'
    else
      write ( *, '(2x,f8.2,2x,g14.6)' ) angle, r8_cot ( angle )
    end if
  end do
 
  return
end
subroutine r8_cotd_test ( )

!*****************************************************************************80
!
!! R8_COTD_TEST tests R8_COTD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) angle
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_cotd

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_COTD_TEST'
  write ( *, '(a)' ) '  R8_COTD computes the cotangent of an angle'
  write ( *, '(a)' ) '  given in degrees.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  ANGLE    R8_COTD(ANGLE)'
  write ( *, '(a)' )
 
  do i = 0, 360, 15
    angle = real ( i, kind = 8 )
    if ( mod ( i, 180 ) == 0 ) then
      write ( *, '(2x,f8.2,2x,a)' ) angle, '  Undefined'
    else
      write ( *, '(2x,f8.2,2x,g14.6)' ) angle, r8_cotd ( angle )
    end if
  end do
 
  return
end
subroutine r8_csc_test ( )

!*****************************************************************************80
!
!! R8_CSC_TEST tests R8_CSC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) angle
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_csc
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CSC_TEST'
  write ( *, '(a)' ) '  R8_CSC computes the cosecant of an angle.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  ANGLE    R8_CSC(ANGLE)'
  write ( *, '(a)' )
 
  do i = 0, 24
    angle = real ( i, kind = 8 ) * r8_pi / 12.0D+00
    if ( mod ( i, 12 ) == 0 ) then
      write ( *, '(2x,f8.2,2x,a)' ) angle, '  Undefined'
    else
      write ( *, '(2x,f8.2,2x,g14.6)' ) angle, r8_csc ( angle )
    end if
  end do
 
  return
end
subroutine r8_cscd_test ( )

!*****************************************************************************80
!
!! R8_CSCD_TEST tests R8_CSCD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) angle
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_cscd

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CSCD_TEST'
  write ( *, '(a)' ) '  R8_CSCD computes the cosecant of an angle'
  write ( *, '(a)' ) '  given in degrees.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  ANGLE    R8_CSCD(ANGLE)'
  write ( *, '(a)' )
 
  do i = 0, 360, 15
    angle = real ( i, kind = 8 )
    if ( mod ( i, 180 ) == 0 ) then
      write ( *, '(2x,f8.2,2x,a)' ) angle, '  Undefined'
    else
      write ( *, '(2x,f8.2,2x,g14.6)' ) angle, r8_cscd ( angle )
    end if
  end do
 
  return
end
subroutine r8_cube_root_test ( )

!*****************************************************************************80
!
!! R8_CUBE_ROOT_TEST tests R8_CUBE_ROOT.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_cube_root
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x1
  real ( kind = 8 ) y
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CUBE_ROOT_TEST'
  write ( *, '(a)' ) '  R8_CUBE_ROOT computes the cube root of an R8.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X               Y               Y^3'
  write ( *, '(a)' ) ''

  a = -10.0D+00
  b = +10.0D+00
  seed = 123456789

  do i = 1, 10
    x1 = r8_uniform_ab ( a, b, seed )
    y = r8_cube_root ( x1 )
    x2 = y ** 3
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x1, y, x2
  end do

  return
end
subroutine r8_degrees_test ( )

!*****************************************************************************80
!
!! R8_DEGREES_TEST tests R8_DEGREES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) degrees
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_degrees
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) radians

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_DEGREES_TEST'
  write ( *, '(a)' ) '  R8_DEGREES converts an angle from radians to degrees.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  ANGLE  R8_DEGREES(ANGLE)'
  write ( *, '(a)' )
 
  do i = 0, 24
    radians = real ( i, kind = 8 ) * r8_pi / 12.0D+00
    degrees = r8_degrees ( radians )
    write ( *, '(2x,g14.6,2x,g14.6)' ) radians, degrees
  end do
 
  return
end
subroutine r8_diff_test ( )

!*****************************************************************************80
!
!! R8_DIFF_TEST tests R8_DIFF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 15

  integer ( kind = 4 ) ndig
  real ( kind = 8 ) r8_diff
  integer ( kind = 4 ) test
  real ( kind = 8 ) x
  real ( kind = 8 ), dimension ( test_num ) :: y_test = (/ &
    0.0625D+00, 0.125D+00, 0.25D+00, 0.50D+00,  0.874D+00, &
    0.876D+00,  0.90D+00,  0.95D+00, 0.99D+00,  1.0D+00, &
    1.01D+00,   1.05D+00,  1.10D+00, 3.0D+00,  10.0D+00 /)
  real ( kind = 8 ) y

  ndig = 3
  x = 1.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_DIFF_TEST'
  write ( *, '(a)' ) '  R8_DIFF computes a difference X-Y to a given'
  write ( *, '(a)' ) '  number of binary places.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8,a)' ) '  For this test, we use ', ndig, ' binary places.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X         Y        X-Y     R8_DIFF(X,Y)'
  write ( *, '(a)' ) ''
  do test = 1, test_num
    y = y_test(test)
    write ( *, '(4f10.4)' ) x, y, x-y, r8_diff ( x, y, ndig )
  end do

  return
end
subroutine r8_digit_test ( )

!*****************************************************************************80
!
!! R8_DIGIT_TEST tests R8_DIGIT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: maxdig = 20

  integer ( kind = 4 ) i
  integer ( kind = 4 ) digit(-2:maxdig)
  integer ( kind = 4 ) idigit
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  x = r8_pi

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_DIGIT_TEST'
  write ( *, '(a)' ) '  R8_DIGIT extracts decimal digits.'
  write ( *, '(a)' ) ''
  write ( *, '(a,g24.16)' ) '  Here, we get digits of ', x
  write ( *, '(a)' ) ''

  do idigit = -2, maxdig
    call r8_digit ( x, idigit, digit(idigit) )
  end do

  write ( *, '(2x,25i3)' ) ( i, i = -2, maxdig )
  write ( *, '(2x,25i3)' ) digit(-2:maxdig)

  return
end
subroutine r8_divide_i4_test ( )

!*****************************************************************************80
!
!! R8_DIVIDE_I4_TEST rests R8_DIVIDE_I4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i4_uniform_ab
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_divide_i4
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_DIVIDE_I4_TEST'
  write ( *, '(a)' ) '  R8_DIVIDE_I4 computes an integer ratio'
  write ( *, '(a)' ) '  using real arithmetic.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I1    I2      R8_DIVIDE_I4'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10

    i1 = i4_uniform_ab ( -100, +100, seed )
    i2 = i4_uniform_ab ( -100, +100, seed )
    if ( i2 == 0 ) then
      i2 = 1
    end if

    r = r8_divide_i4 ( i1, i2 )
    write ( *, '(2x,i4,2x,i4,2x,g14.6)' ) i1, i2, r

  end do

  return
end
subroutine r8_e_test ( )

!*****************************************************************************80
!
!! R8_E_TEST tests R8_E.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_e
  real ( kind = 8 ) value1
  real ( kind = 8 ) value2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_E_TEST'
  write ( *, '(a)' ) '  R8_E returns the value of E.'
  write ( *, '(a)' ) '  Compare E to (1+1/n)^n'
  value1 = r8_e ( )
  write ( *, '(a,g24.16)' ) '  R8_E =      ', value1
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        N     Estimate      Error'
  write ( *, '(a)' ) ''

  n = 1
  do i = 0, 20
    value2 = ( real ( n + 1, kind = 8 ) / real ( n, kind = 8 ) ) ** n
    write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) n, value2, abs ( value1 - value2 )
    n = n * 2
  end do

  return
end
subroutine r8_epsilon_test ( )

!*****************************************************************************80
!
!! R8_EPSILON_TEST tests R8_EPSILON.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) r8_epsilon
  real ( kind = 8 ) r
  real ( kind = 8 ) s

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_EPSILON_TEST'
  write ( *, '(a)' ) '  R8_EPSILON returns the R8 machine precision.'
  write ( *, '(a)' ) ''

  r = r8_epsilon ( )
  write ( *, '(a,g24.16)' ) '  R = R8_EPSILON()         = ', r

  s = ( 1.0D+00 + r ) - 1.0D+00
  write ( *, '(a,g24.16)' ) '  ( 1 + R ) - 1            = ', s

  s = ( 1.0D+00 + ( r / 2.0D+00 ) ) - 1.0D+00
  write ( *, '(a,g24.16)' ) '  ( 1 + (R/2) ) - 1        = ', s

  return
end
subroutine r8_epsilon_compute_test ( )

!*****************************************************************************80
!
!! R8_EPSILON_COMPUTE_TEST tests R8_EPSILON_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) r8_epsilon_compute
  real ( kind = 8 ) r
  real ( kind = 8 ) s

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_EPSILON_COMPUTE_TEST'
  write ( *, '(a)' ) '  R8_EPSILON_COMPUTE computes the R8 machine precision.'
  write ( *, '(a)' ) ''

  r = r8_epsilon_compute ( )
  write ( *, '(a,g24.16)' ) '  R = R8_EPSILON_COMPUTE() = ', r

  s = ( 1.0D+00 + r ) - 1.0D+00
  write ( *, '(a,g24.16)' ) '  ( 1 + R ) - 1            = ', s

  s = ( 1.0D+00 + ( r / 2.0D+00 ) ) - 1.0D+00
  write ( *, '(a,g24.16)' ) '  ( 1 + (R/2) ) - 1        = ', s

  return
end
subroutine r8_exp_test ( )

!*****************************************************************************80
!
!! R8_EXP_TEST tests R8_EXP.
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

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_exp
  real ( kind = 8 ) value
  real ( kind = 8 ) x
  real ( kind = 8 ), dimension ( 11 ) :: x_test = (/ &
    -100.0D+00, -75.0D+00, -50.0D+00, -25.0D+00, -1.0D+00, &
       0.0D+00,  +1.0D+00,  25.0D+00,  50.0D+00, 75.0D+00, &
     100.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_EXP_TEST'
  write ( *, '(a)' ) '  R8_EXP evaluates the exponential function of R8.'
  write ( *, '(a)' ) '  It truncates very small or large arguments.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X         R8_EXP(X)'
  write ( *, '(a)' ) ''

  do i = 1, 11
    x = x_test(i)
    value = r8_exp ( x )
    write ( *, '(2x,g10.6,2x,g10.6)' ) x, value
  end do

  return
end
subroutine r8_factorial_test ( )

!*****************************************************************************80
!
!! R8_FACTORIAL_TEST tests R8_FACTORIAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 December 2014
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
  real ( kind = 8 ) r8_factorial

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_FACTORIAL_TEST'
  write ( *, '(a)' ) '  R8_FACTORIAL computes the factorial function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '    N                Exact' // &
    '                  Computed'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call r8_factorial_values ( n_data, n, f1 )

    if ( n_data == 0 ) then
      exit
    end if

    f2 = r8_factorial ( n )

    write ( *, '(2x,i4,2x,g24.16,2x,g24.16)' ) n, f1, f2

  end do
     
  return
end
subroutine r8_factorial_stirling_test ( )

!*****************************************************************************80
!
!! R8_FACTORIAL_STIRLING_TEST tests R8_FACTORIAL_STIRLING.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_factorial_stirling

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_FACTORIAL_STIRLING_TEST'
  write ( *, '(a)' ) '  R8_FACTORIAL_STIRLING computes Stirling''s'
  write ( *, '(a)' ) '  approximate factorial function;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  N        Factorial      Factorial'
  write ( *, '(a)' ) '           Stirling'
  write ( *, '(a)' ) ''

  f2 = 1.0D+00
  do i = 1, 20
    f1 = r8_factorial_stirling ( i )
    f2 = f2 * real ( i, kind = 8 )
    write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) i, f1, f2
  end do

  return
end
subroutine r8_factorial2_test ( )

!*****************************************************************************80
!
!! R8_FACTORIAL2_TEST tests R8_FACTORIAL2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 December 2014
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
  real ( kind = 8 ) r8_factorial2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_FACTORIAL2_TEST'
  write ( *, '(a)' ) '  R8_FACTORIAL2 computes the double factorial function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '    N                Exact' // &
    '                  Computed'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call r8_factorial2_values ( n_data, n, f1 )

    if ( n_data == 0 ) then
      exit
    end if

    f2 = r8_factorial2 ( n )

    write ( *, '(2x,i4,2x,g24.16,2x,g24.16)' ) n, f1, f2

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

    f2 = r8_fall ( x, n )

    write ( *, '(2x,f8.4,2x,i4,2x,g24.16,2x,g24.16)' ) x, n, f1, f2

  end do
     
  return
end
subroutine r8_floor_test ( )

!*****************************************************************************80
!
!! R8_FLOOR_TEST tests R8_FLOOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_floor
  real ( kind = 8 ) rval
  real ( kind = 8 ) rval2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_FLOOR_TEST'
  write ( *, '(a)' ) '  R8_FLOOR rounds a value down.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X        R8_FLOOR(X)'
  write ( *, '(a)' ) ''

  do i = -6, 6
    rval = real ( i, kind = 8 ) / 5.0D+00
    rval2 = r8_floor ( rval )
    write ( *, '(2x,g14.6,2x,g14.6)' ) rval, rval2
  end do

  return
end
subroutine r8_fractional_test ( )

!*****************************************************************************80
!
!! R8_FRACTIONAL_TEST tests R8_FRACTIONAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fractional
  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_fractional
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) :: r8_hi = 5.0D+00
  real ( kind = 8 ) :: r8_lo = -3.0D+00
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) :: test_num = 10

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_FRACTIONAL_TEST'
  write ( *, '(a)' ) '  R8_FRACTIONAL returns the fractional part of an R8.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    X    R8_FRACTIONAL(X)'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    r8 = r8_uniform_ab ( r8_lo, r8_hi, seed )
    fractional = r8_fractional ( r8 )
    write ( *, '(2x,f10.6,2x,f10.6)' ) r8, fractional
  end do

  return
end
subroutine r8_gamma_test ( )

!*****************************************************************************80
!
!! R8_GAMMA_TEST tests R8_GAMMA.
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
  real ( kind = 8 ) r8_gamma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_GAMMA_TEST:'
  write ( *, '(a)' ) '  R8_GAMMA computes the Gamma functions.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '          X            GAMMA(X)               R8_GAMMA(X)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call gamma_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_gamma ( x )

    write ( *, '(2x,f14.6,2x,g24.16,2x,g24.16)' ) x, fx1, fx2

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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_GAMMA_LOG_TEST:'
  write ( *, '(a)' ) '  R8_GAMMA_LOG computes the Log(Gamma()) function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '          X            GAMMA_LOG(X)   R8_GAMMA_LOG(X)'
  write ( *, '(a)' ) ''

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
subroutine r8_huge_test ( )

!*****************************************************************************80
!
!! R8_HUGE_TEST tests R8_HUGE.
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

  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_huge

  r8 = 1.0D+00
  r8 = huge ( r8 )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_HUGE_TEST'
  write ( *, '(a)' ) '  R8_HUGE returns a "huge" R8;'
  write ( *, '(a)' ) ''
  write ( *, '(a,g24.16)' ) '    R8_HUGE ( ) =      ', r8_huge ( )
  write ( *, '(a,g24.16)' ) '    HUGE ( 1.0D+00 ) = ', r8

  return
end
subroutine r8_is_inf_test ( )

!*****************************************************************************80
!
!! R8_IS_INF_TEST tests R8_IS_INF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8
  logical ( kind = 4 ) r8_is_inf

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_IS_INF_TEST'
  write ( *, '(a)' ) '  R8_IS_INF reports whether an R8 is infinite.'
  write ( *, '(a)' ) ''

  r8 = 1.0D+00
  write ( *, '(a,l1)' ) '  R8_IS_INF(1.0) = ', r8_is_inf ( r8 )

  r1 = 1.0D+00
  r2 = 0.0D+00
  r8 = r1 / r2
  write ( *, '(a,l1)' ) '  R8_IS_INF(1.0/0.0) = ', r8_is_inf ( r8 )

  r1 = 0.0D+00
  r2 = 0.0D+00
  r8 = r1 / r2
  write ( *, '(a,l1)' ) '  R8_IS_INF(0.0/0.0) = ', r8_is_inf ( r8 )

  r1 = 0.0D+00
  r2 = 0.0D+00
  r8 = r1 ** r2
  write ( *, '(a,l1)' ) '  R8_IS_INF(0^0) = ', r8_is_inf ( r8 )

  r1 = -2.0D+00
  r8 = acos ( r1 )
  write ( *, '(a,l1)' ) '  R8_IS_INF(acos(-2)) = ', r8_is_inf ( r8 )

  r1 = 1000.0D+00
  r8 = exp ( r1 )
  write ( *, '(a,l1)' ) '  R8_IS_INF(exp(1000)) = ', r8_is_inf ( r8 )

  r1 = 0.0D+00
  r8 = log ( r1 )
  write ( *, '(a,l1)' ) '  R8_IS_INF(log(0)) = ', r8_is_inf ( r8 )

  r1 = -1.0D+00
  r8 = sqrt ( r1 )
  write ( *, '(a,l1)' ) '  R8_IS_INF(sqrt(-1)) = ', r8_is_inf ( r8 )

  return
end
subroutine r8_is_nan_test ( )

!*****************************************************************************80
!
!! R8_IS_NAN_TEST tests R8_IS_NAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8
  logical ( kind = 4 ) r8_is_nan

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_IS_NAN_TEST'
  write ( *, '(a)' ) '  R8_IS_NAN reports whether an R8 is a NaN.'
  write ( *, '(a)' ) ''

  r8 = 1.0D+00
  write ( *, '(a,l1)' ) '  R8_IS_NAN(1.0) = ', r8_is_nan ( r8 )

  r1 = 1.0D+00
  r2 = 0.0D+00
  r8 = r1 / r2
  write ( *, '(a,l1)' ) '  R8_IS_NAN(1.0/0.0) = ', r8_is_nan ( r8 )

  r1 = 0.0D+00
  r2 = 0.0D+00
  r8 = r1 / r2
  write ( *, '(a,l1)' ) '  R8_IS_NAN(0.0/0.0) = ', r8_is_nan ( r8 )

  r1 = 0.0D+00
  r2 = 0.0D+00
  r8 = r1 ** r2
  write ( *, '(a,l1)' ) '  R8_IS_NAN(0^0) = ', r8_is_nan ( r8 )

  r1 = -2.0D+00
  r8 = acos ( r1 )
  write ( *, '(a,l1)' ) '  R8_IS_NAN(acos(-2)) = ', r8_is_nan ( r8 )

  r1 = 1000.0D+00
  r8 = exp ( r1 )
  write ( *, '(a,l1)' ) '  R8_IS_NAN(exp(1000)) = ', r8_is_nan ( r8 )

  r1 = 0.0D+00
  r8 = log ( r1 )
  write ( *, '(a,l1)' ) '  R8_IS_NAN(log(0)) = ', r8_is_nan ( r8 )

  r1 = -1.0D+00
  r8 = sqrt ( r1 )
  write ( *, '(a,l1)' ) '  R8_IS_NAN(sqrt(-1)) = ', r8_is_nan ( r8 )

  return
end
subroutine r8_log_2_test ( )

!*****************************************************************************80
!
!! R8_LOG_2_TEST tests R8_LOG_2.
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

  integer ( kind = 4 ), parameter :: test_num = 18

  real ( kind = 8 ) r8_log_2
  integer ( kind = 4 ) test
  real ( kind = 8 ) x
  real ( kind = 8 ), dimension(test_num) :: x_test = (/ &
    0.0D+00,  1.0D+00,  2.0D+00,   3.0D+00,  9.0D+00, &
   10.0D+00, 11.0D+00, 99.0D+00, 101.0D+00, -1.0D+00, &
   -2.0D+00, -3.0D+00, -9.0D+00,   0.5D+00,  0.33D+00, &
    0.25D+00, 0.20D+00, 0.01D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_LOG_2_TEST'
  write ( *, '(a)' ) '  R8_LOG_2 computes the logarithm base 2.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    X         R8_LOG_2'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    x = x_test(test)
    write ( *, '( 2g14.6 )' ) x, r8_log_2 ( x )
  end do

  return
end
subroutine r8_log_10_test ( )

!*****************************************************************************80
!
!! R8_LOG_10_TEST tests R8_LOG_10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 18

  real ( kind = 8 ) r8_log_10
  integer ( kind = 4 ) test
  real ( kind = 8 ) x
  real ( kind = 8 ), dimension(test_num) :: x_test = (/ &
    0.0D+00,  1.0D+00,  2.0D+00,   3.0D+00,  9.0D+00, &
   10.0D+00, 11.0D+00, 99.0D+00, 101.0D+00, -1.0D+00, &
   -2.0D+00, -3.0D+00, -9.0D+00,   0.5D+00,  0.33D+00, &
    0.25D+00, 0.20D+00, 0.01D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_LOG_10_TEST'
  write ( *, '(a)' ) '  R8_LOG_10 computes the logarithm base 10.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    X         R8_LOG_10'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    x = x_test(test)
    write ( *, '( 2g14.6 )' ) x, r8_log_10 ( x )
  end do

  return
end
subroutine r8_log_b_test ( )

!*****************************************************************************80
!
!! R8_LOG_B_TEST tests R8_LOG_B.
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

  integer ( kind = 4 ), parameter :: test_num = 10

  real ( kind = 8 ) b
  real ( kind = 8 ), dimension(test_num) :: b_test = (/ &
    2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 6.0D+00, &
    7.0D+00, 8.0D+00, 16.0D+00, 32.0D+00, 256.0D+00 /)
  real ( kind = 8 ) r8_log_b
  integer ( kind = 4 ) test
  real ( kind = 8 ) x

  x = 16.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_LOG_B_TEST'
  write ( *, '(a)' ) '  R8_LOG_B computes the logarithm base B.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        X           B       R8_LOG_B'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    b = b_test(test)

    write ( *, '( 2x,3g14.6, i12 )' ) x, b, r8_log_b ( x, b )

  end do

  return
end
subroutine r8_mant_test ( )

!*****************************************************************************80
!
!! R8_MANT_TEST tests R8_MANT.
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

  integer ( kind = 4 ) is
  integer ( kind = 4 ) l
  real ( kind = 8 ) r
  real ( kind = 8 ) x

  x = -314.159D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_MANT_TEST'
  write ( *, '(a)' ) '  R8_MANT decomposes a value.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Number to be decomposed:'
  write ( *, '(2x,g14.6)' ) x

  call r8_mant ( x, is, r, l )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8,a,g14.6,a,i8)' ) &
    '  R8_MANT: X = ', is, ' * ', r, ' * 2 ^ ', l

  return
end
subroutine r8_max_test ( )

!*****************************************************************************80
!
!! R8_MAX_TEST tests R8_MAX.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_max
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_MAX_TEST'
  write ( *, '(a)' ) '  R8_MAX returns the maximum of two R8''s.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       A       B      C=R8_MAX(A,B)'
  write ( *, '(a)' ) ''

  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  do i = 1, 10
    a = r8_uniform_ab ( r8_lo, r8_hi, seed )
    b = r8_uniform_ab ( r8_lo, r8_hi, seed )
    c = r8_max ( a, b )
    write ( *, '(2x,f8.4,2x,f8.4,2x,f8.4)' ) a, b, c
  end do

  return
end
subroutine r8_min_test ( )

!*****************************************************************************80
!
!! R8_MIN_TEST tests R8_MIN.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8_min
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_MIN_TEST'
  write ( *, '(a)' ) '  R8_MIN returns the minimum of two R8''s.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       A       B      C=R8_MIN(A,B)'
  write ( *, '(a)' ) ''

  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789
  do i = 1, 10
    a = r8_uniform_ab ( r8_lo, r8_hi, seed )
    b = r8_uniform_ab ( r8_lo, r8_hi, seed )
    c = r8_min ( a, b )
    write ( *, '(2x,f8.4,2x,f8.4,2x,f8.4)' ) a, b, c
  end do

  return
end
subroutine r8_mod_test ( )

!*****************************************************************************80
!
!! R8_MOD_TEST tests R8_MOD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) r8_mod
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) :: x_hi =  10.0D+00
  real ( kind = 8 ) :: x_lo = -10.0D+00
  real ( kind = 8 ) y
  real ( kind = 8 ) z1
  real ( kind = 8 ) z2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_MOD_TEST'
  write ( *, '(a)' ) '  R8_MOD returns the remainder after division.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X         Y     MOD(X,Y)    R8_MOD(X,Y)'
  write ( *, '(a)' ) ''

  seed = 123456789

  do test = 1, test_num

    x = r8_uniform_ab ( x_lo, x_hi, seed )
    y = r8_uniform_ab ( x_lo, x_hi, seed )

    z1 =    mod ( x, y )
    z2 = r8_mod ( x, y )

    write ( * , '(2x,f8.4,2x,f8.4,2x,f8.4,2x,f8.4)' ) x, y, z1, z2

  end do

  return
end
subroutine r8_modp_test ( )

!*****************************************************************************80
!
!! R8_MODP_TEST tests R8_MODP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) r8_modp
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) :: x_hi =  10.0D+00
  real ( kind = 8 ) :: x_lo = -10.0D+00
  real ( kind = 8 ) y
  real ( kind = 8 ) z1
  real ( kind = 8 ) z2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_MODP_TEST'
  write ( *, '(a)' ) '  R8_MODP returns the remainder after division.'
  write ( *, '(a)' ) '  Unlike the FORTRAN MOD, R8_MODP ( X, Y ) is positive.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X       Y      MOD(X,Y)  R8_MODP(X,Y)'
  write ( *, '(a)' ) ''

  seed = 123456789

  do test = 1, test_num

    x = r8_uniform_ab ( x_lo, x_hi, seed )
    y = r8_uniform_ab ( x_lo, x_hi, seed )

    z1 =    mod  ( x, y )
    z2 = r8_modp ( x, y )

    write ( * , '(2x,f8.4,2x,f8.4,2x,f8.4,2x,f8.4)' ) x, y, z1, z2

  end do

  return
end
subroutine r8_mop_test ( )

!*****************************************************************************80
!
!! R8_MOP_TEST tests R8_MOP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i4
  integer ( kind = 4 ) i4_max
  integer ( kind = 4 ) i4_min
  integer ( kind = 4 ) i4_uniform_ab
  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_mop
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_MOP_TEST'
  write ( *, '(a)' ) '  R8_MOP evaluates (-1.0)^I4 as an R8.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I4  R8_MOP(I4)'
  write ( *, '(a)' ) ''

  i4_min = -100
  i4_max = +100
  seed = 123456789

  do test = 1, 10
    i4 = i4_uniform_ab ( i4_min, i4_max, seed )
    r8 = r8_mop ( i4 )
    write ( *, '(2x,i4,2x,f4.1)' ) i4, r8
  end do

  return
end
subroutine r8_nint_test ( )

!*****************************************************************************80
!
!! R8_NINT_TEST tests R8_NINT
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

  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) r8_nint
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_NINT_TEST'
  write ( *, '(a)' ) '  R8_NINT produces the nearest integer to an R8.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X      R8_NINT(X)'
  write ( *, '(a)' ) ''

  b = -10.0D+00
  c = +10.0D+00

  do test = 1, test_num
    x = r8_uniform_ab ( b, c, seed )
    write ( *, '(2x,f10.4,2x,i8)' ) x, r8_nint ( x )
  end do

  return;
end
subroutine r8_normal_01_test ( )

!*****************************************************************************80
!
!! R8_NORMAL_01_TEST tests R8_NORMAL_01.
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

  real ( kind = 8 ) r8_normal_01
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 20
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_NORMAL_01_TEST'
  write ( *, '(a)' ) '  R8_NORMAL_01 generates normally distributed'
  write ( *, '(a)' ) '  random values.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed
  write ( *, '(a)' ) ''

  do test = 1, test_num

    x = r8_normal_01 ( seed )
    write ( *, '(2x,g14.6)' ) x

  end do

  return
end
subroutine r8_normal_ab_test ( )

!*****************************************************************************80
!
!! R8_NORMAL_AB_TEST tests R8_NORMAL_AB.
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

  real ( kind = 8 ) mu
  real ( kind = 8 ) r8_normal_ab
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) sigma
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 20
  real ( kind = 8 ) x

  mu = 10.0D+00
  sigma = 4.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_NORMAL_AB_TEST'
  write ( *, '(a)' ) '  R8_NORMAL_AB generates normally distributed random'
  write ( *, '(a)' ) '  values with mean MU and standard deviation SIGMA.'
  write ( *, '(a,i12)' ) '  Initial random number seed = ', seed
  write ( *, '(a,g14.6)' ) '  MU    = ', mu
  write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
  write ( *, '(a)' ) ''

  do test = 1, test_num

    x = r8_normal_ab ( mu, sigma, seed )
    write ( *, '(2x,g14.6)' ) x

  end do

  return
end
subroutine r8_nth_root_test ( )

!*****************************************************************************80
!
!! R8_NTH_ROOT_TEST tests R8_NTH_ROOT.
!
!  Discussion:
!
!    Some of these tests will return Infinity or NaN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_nth_root
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_NTH_ROOT_TEST'
  write ( *, '(a)' ) '  R8_NTH_ROOT computes the nth root of an R8.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         X        -3        -2        -1         0         1         2         3'
  write ( *, '(a)' )''

  do i = -3, 3

    x = real ( i, kind = 8 )
    write ( *, '(2x,f8.4)', advance = 'no' ) x

    do n = -3, 3
      y = r8_nth_root ( x, n )
      write ( *, '(2x,f8.4)', advance = 'no' ) y
    end do

    write ( *, '(a)' ) ''

  end do

  return
end
subroutine r8_pi_test ( )

!*****************************************************************************80
!
!! R8_PI_TEST tests R8_PI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 May 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) four
  real ( kind = 8 ) one
  real ( kind = 8 ) r8_pi
  real ( kind = 8 ) v1
  real ( kind = 8 ) v2

  four = real ( 4, kind = 8 )
  one = real ( 1, kind = 8 )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_PI_TEST'
  write ( *, '(a)' ) '  R8_PI returns the value of PI.'
  write ( *, '(a)' ) ''
  v1 = r8_pi ( )
  write ( *, '(a,g24.16)' ) '  R8_PI =     ', v1
  v2 = four * atan ( one )
  write ( *, '(a,g24.16)' ) '  4*atan(1) = ', v2

  return
end
subroutine r8_power_test ( )

!*****************************************************************************80
!
!! R8_POWER_TEST tests R8_POWER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) r8_power
  integer ( kind = 4 ) i
  integer ( kind = 4 ) p
  real ( kind = 8 ) r
  real ( kind = 8 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_POWER_TEST'
  write ( *, '(a)' ) '  R8_POWER computes R^P.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       R            P    R^P'
  write ( *, '(a)' ) ''

  do i = -5, 5

    r = 2.0D+00
    p = i
    value = r8_power ( r, p )
    write ( *, '(2x,g14.6,i5,g14.6,i5)' ) r, p, value

  end do

  return
end
subroutine r8_power_fast_test ( )

!*****************************************************************************80
!
!! R8_POWER_FAST_TEST tests R8_POWER_FAST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) mults
  integer ( kind = 4 ) p
  real ( kind = 8 ) r
  real ( kind = 8 ) rp

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_POWER_FAST_TEST'
  write ( *, '(a)' ) '  R8_POWER_FAST computes R^P, economizing on'
  write ( *, '(a)' ) '  multiplications.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       R           P      R^P        Mults'
  write ( *, '(a)' ) ''

  do i = -10, 40

    r = 2.0D+00
    p = i
    call r8_power_fast ( r, p, rp, mults )
    write ( *, '(2x,g14.6,i5,g14.6,i5)' ) r, p, rp, mults

  end do

  return
end
subroutine r8_radians_test ( )

!*****************************************************************************80
!
!! R8_RADIANS_TEST tests R8_RADIANS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) degrees
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_radians
  real ( kind = 8 ) radians

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_RADIANS_TEST'
  write ( *, '(a)' ) '  R8_RADIANS converts an angle from degrees to radians.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  ANGLE  R8_RADIANS(ANGLE)'
  write ( *, '(a)' )
 
  do i = 0, 24
    degrees = real ( i, kind = 8 ) * 360.0D+00 / 24.0D+00
    radians = r8_radians ( degrees )
    write ( *, '(2x,g14.6,2x,g14.6)' ) degrees, radians
  end do
 
  return
end
subroutine r8_relu_test ( )

!*****************************************************************************80
!
!! R8_RELU_TEST tests R8_RELU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 January 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_relu
  real ( kind = 8 ) value
  real ( kind = 8 ) x
  real ( kind = 8 ), dimension ( 25 ) :: x_test = (/ &
    -500.0D+00,   -50.0D+00,    -5.0D+00,    -4.0D+00,    -3.0D+00, &
      -2.0D+00,    -1.0D+00,    -0.5D+00,    -0.05D+00,    -0.005D+00, &
      -0.0005D+00,  0.0D+00,     0.0005D+00,  0.005D+00,    0.05D+00, &
       0.5D+00,     1.0D+00,     2.0D+00,     3.0D+00,      4.0D+00, &
       5.0D+00,    50.0D+00,   500.0D+00,  5000.0D+00,  50000.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_RELU_TEST'
  write ( *, '(a)' ) '  R8_RELU evaluates the ReLU function of an R8.'
  write ( *, '(a)' ) '  This is max(x,0).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X             R8_RELU(X)'
  write ( *, '(a)' ) ''

  do i = 1, 25
    x = x_test(i)
    value = r8_relu ( x );
    write ( *, '(2x,g14.6,2x,g14.6)' ) x, value
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
subroutine r8_round2_test ( )

!*****************************************************************************80
!
!! R8_ROUND2_TEST tests R8_ROUND2.
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

  integer ( kind = 4 ) i
  integer ( kind = 4 ) nplace
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) xround

  x = r8_pi

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ROUND2_TEST'
  write ( *, '(a)' ) '  R8_ROUND2 rounds a number to a'
  write ( *, '(a)' ) '  specified number of base 2 digits.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test effect on PI:'
  write ( *, '(a,g24.16)' ) '  X = ', x
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    NPLACE  XROUND'
  write ( *, '(a)' ) ''

  do i = 0, 20
    nplace = i
    call r8_round2 ( nplace, x, xround )
    write ( *, '(2x,i8,g24.16)' ) i, xround
  end do

  return
end
subroutine r8_roundb_test ( )

!*****************************************************************************80
!
!! R8_ROUNDB_TEST tests R8_ROUNDB.
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

  integer ( kind = 4 ) base
  integer ( kind = 4 ) i
  integer ( kind = 4 ) nplace
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) xround

  base = 3
  x = r8_pi

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ROUNDB_TEST'
  write ( *, '(a)' ) '  R8_ROUNDB rounds a number to a '
  write ( *, '(a)' ) '  specified number of base BASE digits.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Here, we will use BASE = ',base
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test effect on PI:'
  write ( *, '(a,g24.16)' ) '  X = ', x
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    NPLACE  XROUND'
  write ( *, '(a)' ) ''

  do i = 0, 20
    nplace = i
    call r8_roundb ( base, nplace, x, xround )
    write ( *, '(2x,i8,g24.16)' ) i, xround
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Try with a negative base:'
  x = 121.0D+00
  base = -3
  nplace = 3
  write ( *, '(a)' ) ''
  write ( *, '(a,g24.16)' ) '  Input quantity is X = ', x
  write ( *, '(a,i8)' ) '  to be rounded in base ', base

  do nplace = 1, 5

    call r8_roundb ( base, nplace, x, xround )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8,a,g24.16)' ) '  Output value to ', nplace, &
      ' places is ', xround

  end do

  return
end
subroutine r8_roundx_test ( )

!*****************************************************************************80
!
!! R8_ROUNDX_TEST tests R8_ROUNDX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) nplace
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) xround

  seed = 123456789
  x = r8_pi

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ROUNDX_TEST'
  write ( *, '(a)' ) '  R8_ROUNDX rounds a number to a '
  write ( *, '(a)' ) '  specified number of decimal digits.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test effect on PI:'
  write ( *, '(a,g24.16)' ) '  X = ', x
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  NPLACE  XROUND'
  write ( *, '(a)' ) ''

  do i = 0, 10
    nplace = i
    call r8_roundx ( nplace, x, xround )
    write ( *, '(2x,i8,g24.16)' ) i, xround
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test effect on random values:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  NPLACE  X     XROUND'
  write ( *, '(a)' ) ''

  do i = 1, 5

    x = r8_uniform_01 ( seed )

    write ( *, '(a)' ) ''

    do nplace = 0, 10, 2
      call r8_roundx ( nplace, x, xround )
      write ( *, '(2x,i8,2x,g24.16,2x,g24.16)' ) nplace, x, xround
    end do

  end do

  return
end
subroutine r8_secd_test ( )

!*****************************************************************************80
!
!! R8_SECD_TEST tests R8_SECD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) angle
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_secd

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SECD_TEST'
  write ( *, '(a)' ) '  R8_SECD computes the secant of an angle'
  write ( *, '(a)' ) '  given in degrees.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  ANGLE    R8_SECD(ANGLE)'
  write ( *, '(a)' )
 
  do i = 0, 360, 15
    angle = real ( i, kind = 8 )
    if ( mod ( i + 90, 180 ) == 0 ) then
      write ( *, '(2x,f8.2,2x,a)' ) angle, '  Undefined'
    else
      write ( *, '(2x,f8.2,2x,g14.6)' ) angle, r8_secd ( angle )
    end if
  end do
 
  return
end
subroutine r8_sech_test ( )

!*****************************************************************************80
!
!! R8_SECH_TEST tests R8_SECH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_sech
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SECH_TEST'
  write ( *, '(a)' ) '  R8_SECH computes the hyperbolic secant.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  X        R8_SECH(X)'
  write ( *, '(a)' )
 
  do i = 0, 20
    x = real ( i - 10, kind = 8 ) / 10.0D+00
    write ( *, '(2x,f8.2,2x,g14.6)' ) x, r8_sech ( x )
  end do
 
  return
end
subroutine r8_sigmoid_test ( )

!*****************************************************************************80
!
!! r8_sigmoid_test tests r8_sigmoid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) l
  real ( kind = 8 ) m
  real ( kind = 8 ) r8_sigmoid
  real ( kind = 8 ) value
  real ( kind = 8 ) x
  real ( kind = 8 ), dimension ( 11 ) :: x_test = (/ &
    -4.00D+00, -2.00D+00, -1.00D+00, -0.50D+00, -0.25D+00, &
     0.00D+00, +0.25D+00,  0.50D+00,  1.00D+00,  2.00D+00, &
     4.00D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8_sigmoid_test'
  write ( *, '(a)' ) '  r8_sigmoid evaluates the sigmoid function of R8.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X         R8_SIGMOID(L,B,M,X)'
  write ( *, '(a)' ) ''

  l = 1.0D+00
  b = 0.0D+00
  m = 1.0D+00

  do i = 1, 11
    x = x_test(i)
    value = r8_sigmoid ( l, b, m, x )
    write ( *, '(2x,g14.6,2x,g14.6)' ) x, value
  end do

  return
end
subroutine r8_sign_test ( )

!*****************************************************************************80
!
!! R8_SIGN_TEST tests R8_SIGN.
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

  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_sign
  real ( kind = 8 ), parameter, dimension ( test_num ) :: r8_test = (/ &
    -1.25D+00, -0.25D+00, 0.0D+00, +0.5D+00, +9.0D+00 /)
  real ( kind = 8 ) s
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SIGN_TEST'
  write ( *, '(a)' ) '  R8_SIGN returns the sign of an R8.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    R8    R8_SIGN(R8)'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    r8 = r8_test(test)
    s = r8_sign ( r8 )
    write ( *, '(2x,f8.4,2x,f8.0)' ) r8, s
  end do

  return
end
subroutine r8_sign_match_test ( )

!*****************************************************************************80
!
!! R8_SIGN_MATCH_TEST tests R8_SIGN_MATCH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  logical match
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  logical r8_sign_match
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SIGN_MATCH_TEST'
  write ( *, '(a)' ) '  R8_SIGN_MATCH reports whether signs match'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        R1          R2       R8_SIGN_MATCH(R1,R2)'
  write ( *, '(a)' ) ''

  seed = 123456789
  do i = 1, 10
    r1 = r8_uniform_ab ( -5.0D+00, +5.0D+00, seed )
    r2 = r8_uniform_ab ( -5.0D+00, +5.0D+00, seed )
    match = r8_sign_match ( r1, r2 )
    write ( *, '(2x,f10.4,2x,f10.4,8x,l1)' ) r1, r2, match
  end do

  return
end
subroutine r8_sign3_test ( )

!*****************************************************************************80
!
!! R8_SIGN3_TEST tests R8_SIGN3.
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

  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_sign3
  real ( kind = 8 ), parameter, dimension ( test_num ) :: r8_test = (/ &
    -1.25D+00, -0.25D+00, 0.0D+00, +0.5D+00, +9.0D+00 /)
  real ( kind = 8 ) s
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SIGN3_TEST'
  write ( *, '(a)' ) '  R8_SIGN3 returns the three-way sign of an R8.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    R8    R8_SIGN3(R8)'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    r8 = r8_test(test)
    s = r8_sign3 ( r8 )
    write ( *, '(2x,f8.4,2x,f8.0)' ) r8, s
  end do

  return
end
subroutine r8_sincos_sum_test ( )

!*****************************************************************************80
!
!! R8_SINCOS_SUM_TEST tests R8_SINCOS_SUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) f
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SINCOS_SUM_TEST'
  write ( *, '(a)' ) '  R8_SINCOS_SUM simplifies a linear sine and cosine sum'

  seed = 123456789
  a = r8_uniform_ab ( -5.0D+00, +5.0D+00, seed )
  b = r8_uniform_ab ( -5.0D+00, +5.0D+00, seed )
  c = r8_uniform_ab ( -5.0D+00, +5.0D+00, seed )

  call r8_sincos_sum ( a, b, d, e, f )

  write ( *, '(a)' ) ''
  write ( *, '(a,g12.4,a,g12.4,a,g12.4,a,g12.4,a)' ) '   ', a, ' * sin ( ', c, ' * x ) + ', b, ' * cos ( ', c, ' * x )'
  write ( *, '(a,g12.4,a,g12.4,a,g12.4,a)' )         ' = ', d, ' * sin ( ', c, ' * x + ', e, ' )'
  write ( *, '(a,g12.4,a,g12.4,a,g12.4,a)' )         ' = ', d, ' * cos ( ', c, ' * x + ', f, ' )'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I        X        form 1        form 2        form 3'
  write ( *, '(a)' ) ''

  do i = 0, 10
    x = real ( i, kind = 8 ) * r8_pi / 10.0D+00
    y1 = a * sin ( c * x ) + b * cos ( c * x )
    y2 = d * sin ( c * x + e )
    y3 = d * cos ( c * x + f )
    write ( *, '(2x,i2,2x,f10.4,2x,g12.6,2x,g12.6,2x,g12.6)' ) &
      i, x, y1, y2, y3
  end do

  return
end
subroutine r8_sind_test ( )

!*****************************************************************************80
!
!! R8_SIND_TEST tests R8_SIND.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) angle
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_sind

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SIND_TEST'
  write ( *, '(a)' ) '  R8_SIND computes the sine of an angle'
  write ( *, '(a)' ) '  given in degrees.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  ANGLE    R8_SIND(ANGLE)'
  write ( *, '(a)' )
 
  do i = 0, 360, 15
    angle = real ( i, kind = 8 )
    write ( *, '(2x,f8.2,2x,g14.6)' ) angle, r8_sind ( angle )
  end do
 
  return
end
subroutine r8_softplus_test ( )

!*****************************************************************************80
!
!! R8_SOFTPLUS_TEST tests R8_SOFTPLUS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_softplus
  real ( kind = 8 ) value
  real ( kind = 8 ) x
  real ( kind = 8 ), dimension ( 25 ) :: x_test = (/ &
    -500.0D+00,   -50.0D+00,    -5.0D+00,    -4.0D+00,    -3.0D+00, &
      -2.0D+00,    -1.0D+00,    -0.5D+00,    -0.05D+00,    -0.005D+00, &
      -0.0005D+00,  0.0D+00,     0.0005D+00,  0.005D+00,    0.05D+00, &
       0.5D+00,     1.0D+00,     2.0D+00,     3.0D+00,      4.0D+00, &
       5.0D+00,    50.0D+00,   500.0D+00,  5000.0D+00,  50000.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SOFTPLUS_TEST'
  write ( *, '(a)' ) '  R8_SOFTPLUS evaluates the softplus function of an R8.'
  write ( *, '(a)' ) '  This is a smoothed version of max(x,0).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      X         R8_SOFTPLUS(X)'
  write ( *, '(a)' ) ''

  do i = 1, 25
    x = x_test(i)
    value = r8_softplus ( x );
    write ( *, '(2x,g14.6,2x,g14.6)' ) x, value
  end do

  return
end
subroutine r8_swap_test ( )

!*****************************************************************************80
!
!! R8_SWAP_TEST tests R8_SWAP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) x
  real ( kind = 8 ) y

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SWAP_TEST'
  write ( *, '(a)' ) '  R8_SWAP swaps two reals.'

  x = 1.0D+00
  y = 3.141592653589793D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Before swapping:'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '    X = ', x
  write ( *, '(a,g14.6)' ) '    Y = ', y

  call r8_swap ( x, y )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  After swapping:'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '    X = ', x
  write ( *, '(a,g14.6)' ) '    Y = ', y

  return
end
subroutine r8_swap3_test ( )

!*****************************************************************************80
!
!! R8_SWAP3_TEST tests R8_SWAP3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 January 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SWAP3_TEST'
  write ( *, '(a)' ) '  R8_SWAP3 swaps three reals.'

  x = 1.0D+00
  y = 3.141592653589793D+00
  z = 1952.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '            X      Y      Z'
  write ( *, '(a)' ) ''
  write ( *, '(a,3g14.6)' ) '  Before: ', x, y, z

  do i = 1, 3

    call r8_swap3 ( x, y, z )
    write ( *, '(a,i2,3g14.6)' ) '  Swap  ', i, x, y, z

  end do

  return
end
subroutine r8_tand_test ( )

!*****************************************************************************80
!
!! R8_TAND_TEST tests R8_TAND.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) angle
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_tand

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_TAND_TEST'
  write ( *, '(a)' ) '  R8_TAND computes the tangent of an angle'
  write ( *, '(a)' ) '  given in degrees.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  ANGLE    R8_TAND(ANGLE)'
  write ( *, '(a)' )
 
  do i = 0, 360, 15
    angle = real ( i, kind = 8 )
    if ( mod ( i + 90, 180 ) == 0 ) then
      write ( *, '(2x,f8.2,2x,a)' ) angle, '  Undefined'
    else
      write ( *, '(2x,f8.2,2x,g14.6)' ) angle, r8_tand ( angle )
    end if
  end do
 
  return
end
subroutine r8_tiny_test ( )

!*****************************************************************************80
!
!! R8_TINY_TEST tests R8_TINY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) r8
  real ( kind = 8 ) r8_tiny

  r8 = 1.0D+00
  r8 = tiny ( r8 )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_TINY_TEST'
  write ( *, '(a)' ) '  R8_TINY returns a "tiny" R8;'
  write ( *, '(a)' ) ''
  write ( *, '(a,g24.16)' ) '    R8_TINY ( ) =      ', r8_tiny ( )
  write ( *, '(a,g24.16)' ) '    TINY ( 1.0D+00 ) = ', r8

  return
end
subroutine r8_to_r8_discrete_test ( )

!*****************************************************************************80
!
!! R8_TO_R8_DISCRETE_TEST tests R8_TO_R8_DISCRETE.
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

  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) :: ndx = 19
  real ( kind = 8 ) r
  real ( kind = 8 ) rd
  real ( kind = 8 ) :: rhi = 10.0D+00
  real ( kind = 8 ) rhi2
  real ( kind = 8 ) :: rlo = 1.0D+00
  real ( kind = 8 ) rlo2
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 15

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_TO_R8_DISCRETE_TEST'
  write ( *, '(a)' ) '  R8_TO_R8_DISCRETE maps numbers to a discrete set'
  write ( *, '(a)' ) '  of equally spaced numbers in an interval.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of discrete values = ', ndx
  write ( *, '(a,2g14.6)' ) '  Real interval: ', rlo, rhi
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        R         RD'
  write ( *, '(a)' ) ''

  seed = 123456789

  rlo2 = rlo - 2.0D+00
  rhi2 = rhi + 2.0D+00

  do test = 1, test_num
    r = r8_uniform_ab ( rlo2, rhi2, seed )
    call r8_to_r8_discrete ( r, rlo, rhi, ndx, rd )
    write ( *, '(2x,g14.6,g14.6)' ) r, rd
  end do

  return
end
subroutine r8_to_i4_test ( )

!*****************************************************************************80
!
!! R8_TO_I4_TEST tests R8_TO_I4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) ix
  integer ( kind = 4 ) ixmax
  integer ( kind = 4 ) ixmin
  real ( kind = 8 ) x
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_TO_I4_TEST'
  write ( *, '(a)' ) '  R8_TO_I4 finds an integer IX in [IXMIN,IXMAX]'
  write ( *, '(a)' ) '  corresponding to X in [XMIN,XMAX].'

  xmin = 2.5D+00
  x = 3.5D+00
  xmax = 5.5D+00

  ixmin = 10
  ixmax = 40

  call r8_to_i4 ( xmin, xmax, x, ixmin, ixmax, ix )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6,a,g14.6)' ) &
    '  XMIN ',  xmin, '   X = ',  x, '  XMAX = ', xmax
  write ( *, '(a,i14,a,i14,a,i14)' ) &
    ' IXMIN ', ixmin, '  IX = ', ix, ' IXMAX = ', ixmax

  return
end
subroutine r8_uniform_01_test ( )

!*****************************************************************************80
!
!! R8_UNIFORM_01_TEST tests R8_UNIFORM_01
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

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ), parameter :: n = 1000
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  R8_UNIFORM_01 samples a uniform random'
  write ( *, '(a)' ) '  distribution in [0,1].'

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a,i12)' ) '  Starting with seed = ', seed

  do i = 1, n
    x(i) = r8_uniform_01 ( seed )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  First few values:'
  write ( *, '(a)' ) ''
  do i = 1, 5
    write ( *, '(2x,i8,2x,g14.6)' ) i, x(i)
  end do

  mean = 0.0D+00
  do i = 1, n
    mean = mean + x(i)
  end do
  mean = mean / real ( n, kind = 8 )
 
  variance = 0.0D+00
  do i = 1, n
    variance = variance + ( x(i) - mean ) ** 2
  end do
  variance = variance / real ( n, kind = 8 )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of values computed was N = ', n
  write ( *, '(a,g14.6)' ) '  Average value was ', mean
  write ( *, '(a,g14.6)' ) '  Minimum value was ', minval ( x(1:n) )
  write ( *, '(a,g14.6)' ) '  Maximum value was ', maxval ( x(1:n) )
  write ( *, '(a,g14.6)' ) '  Variance was ', variance

  return
end
subroutine r8_uniform_ab_test ( )

!*****************************************************************************80
!
!! R8_UNIFORM_AB_TEST tests R8_UNIFORM_AB.
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

  real ( kind = 8 ), parameter :: b = 10.0D+00
  real ( kind = 8 ), parameter :: c = 20.0D+00
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  R8_UNIFORM returns random values in a given range:'
  write ( *, '(a)' ) '  [ B, C ]'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  For this problem:'
  write ( *, '(a,g14.6)' ) '  B = ', b
  write ( *, '(a,g14.6)' ) '  C = ', c
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10
    r = r8_uniform_ab ( b, c, seed )
    write ( *, '(2x,g14.6)' ) r
  end do

  return
end
subroutine r8_walsh_1d_test ( )

!*****************************************************************************80
!
!! R8_WALSH_1D_TEST tests R8_WALSH_1D;
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

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_walsh_1d
  real ( kind = 8 ) w0
  real ( kind = 8 ) wm1
  real ( kind = 8 ) wm2
  real ( kind = 8 ) wm3
  real ( kind = 8 ) wp1
  real ( kind = 8 ) wp2
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_WALSH_1D_TEST'
  write ( *, '(a)' ) '  R8_WALSH_1D evaluates 1D Walsh functions:'
  write ( *, '(a)' ) ''
  write ( *, * ) 'X  W(+2) W(+1) W(0) W(-1) W(-2) W(-3)'
  write ( *, '(a)' ) ''

  do i = 0, 32

    x = real ( i, kind = 8 ) / 4.0D+00

    wp2 = r8_walsh_1d ( x,  2 )
    wp1 = r8_walsh_1d ( x,  1 )
    w0  = r8_walsh_1d ( x,  0 )
    wm1 = r8_walsh_1d ( x, -1 )
    wm2 = r8_walsh_1d ( x, -2 )
    wm3 = r8_walsh_1d ( x, -3 )

    write ( *, '(2x,f10.6,6f4.1)' ) x, wp2, wp1, w0, wm1, wm2, wm3

  end do

  return
end
subroutine r8_wrap_test ( )

!*****************************************************************************80
!
!! R8_WRAP_TEST tests R8_WRAP;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 July 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), parameter :: a = - 2.0D+00
  real ( kind = 8 ), parameter :: b = 12.0D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) r8_wrap
  real ( kind = 8 ), parameter :: rhi = 6.5D+00
  real ( kind = 8 ), parameter :: rlo = 3.0D+00
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 20

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_WRAP_TEST'
  write ( *, '(a)' ) '  R8_WRAP "wraps" an R8 to lie within an interval:'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6)' ) '  Wrapping interval is ', rlo, ', ', rhi
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      R      R8_WRAP ( R )'
  write ( *, '(a)' ) ''
  seed = 123456789

  do test = 1, test_num

    r = r8_uniform_ab ( a, b, seed )
    r2 = r8_wrap ( r, rlo, rhi )
    write ( *, '(2x,g14.6,2x,g14.6)' ) r, r2

  end do

  return
end
subroutine r82col_print_part_test ( )

!*****************************************************************************80
!
!! R82COL_PRINT_PART_TEST tests R82COL_PRINT_PART.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) max_print
!
!  FORTRAN 2D arrays are listed in row order.
!
  real ( kind = 8 ), dimension ( n, 2 ) :: v = reshape ( (/ &
    11.0,  21.0, 31.0, 41.0, 51.0, 61.0, 71.0, 81.0, 91.0, 101.0, &
    12.0,  22.0, 32.0, 42.0, 52.0, 62.0, 72.0, 82.0, 92.0, 102.0 /), (/ n, 2 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R82COL_PRINT_PART_TEST'
  write ( *, '(a)' ) '  R82COL_PRINT_PART prints part of an R82COL.'

  max_print = 2
  call r82col_print_part ( n, v, max_print, '  Output with MAX_PRINT = 2' )

  max_print = 5
  call r82col_print_part ( n, v, max_print, '  Output with MAX_PRINT = 5' )

  max_print = 25
  call r82col_print_part ( n, v, max_print, '  Output with MAX_PRINT = 25' )

  return
end
subroutine r82row_order_type_test ( )

!*****************************************************************************80
!
!! R82ROW_ORDER_TYPE_TEST tests R82ROW_ORDER_TYPE.
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

  integer ( kind = 4 ), parameter :: n = 4
  integer ( kind = 4 ), parameter :: test_num = 10

  integer ( kind = 4 ) order
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  character ( len = 40 ) title
  real ( kind = 8 ) x(2,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R82ROW_ORDER_TYPE_TEST'
  write ( *, '(a)' ) '  R82ROW_ORDER_TYPE classifies a R8VEC as'
  write ( *, '(a)' ) '  -1: no order'
  write ( *, '(a)' ) '   0: all equal;'
  write ( *, '(a)' ) '   1: ascending;'
  write ( *, '(a)' ) '   2: strictly ascending;'
  write ( *, '(a)' ) '   3: descending;'
  write ( *, '(a)' ) '   4: strictly descending.'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    call r8mat_uniform_01 ( 2, n, seed, x )

    x(1:2,1:n) = real ( nint ( 3.0D+00 * x(1:2,1:n) ), kind = 8 )

    call r82row_order_type ( n, x, order )

    write ( title, '(a,i8)' ) '  Order type = ', order

    call r82row_print ( n, x, title )

  end do

  return
end
subroutine r82row_part_quick_a_test ( )

!*****************************************************************************80
!
!! R82ROW_PART_QUICK_A_TEST tests R82ROW_PART_QUICK_A.
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

  real ( kind = 8 ) a(2,n)
  real ( kind = 8 ) :: b = 0.0D+00
  real ( kind = 8 ) :: c = 10.0D+00
  integer ( kind = 4 ) l
  integer ( kind = 4 ) r
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R82ROW_PART_QUICK_A_TEST'
  write ( *, '(a)' ) '  R82ROW_PART_QUICK_A reorders an R82ROW'
  write ( *, '(a)' ) '  as part of a quick sort.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  call r8mat_uniform_ab ( 2, n, b, c, seed, a )

  call r82row_print ( n, a, '  Before rearrangement:' )

  call r82row_part_quick_a ( n, a, l, r )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rearranged array'
  write ( *, '(a,i8)' ) '  Left index =  ', l
  write ( *, '(a,i8)' ) '  Key index =   ', l+1
  write ( *, '(a,i8)' ) '  Right index = ', r
  write ( *, '(a)' ) ''

  call r82row_print ( l,     a(1:2,1:l),   '  Left half:' )
  call r82row_print ( 1,     a(1:2,l+1),   '  Key:' )
  call r82row_print ( n-l-1, a(1:2,l+2:n), '  Right half:' )

  return
end
subroutine r82row_print_part_test ( )

!*****************************************************************************80
!
!! R82ROW_PRINT_PART_TEST tests R82ROW_PRINT_PART.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) max_print
!
!  FORTRAN arrays are listed in row order.
!
  real ( kind = 8 ), dimension ( 2, n ) :: v = reshape ( (/ &
     11.0,  21.0, &
     12.0,  22.0, &
     13.0,  23.0, &
     14.0,  24.0, &
     15.0,  25.0, &
     16.0,  26.0, &
     17.0,  27.0, &
     18.0,  28.0, &
     19.0,  29.0, &
     20.0,  30.0 /), (/ 2, n /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R82ROW_PRINT_PART_TEST'
  write ( *, '(a)' ) '  R82ROW_PRINT_PART prints part of an R82ROW'
  write ( *, '(a)' ) '  as a list of columns (that is, transposed).'

  max_print = 2
  call r82row_print_part ( n, v, max_print, '  Output with MAX_PRINT = 2' )

  max_print = 5
  call r82row_print_part ( n, v, max_print, '  Output with MAX_PRINT = 5' )

  max_print = 25
  call r82row_print_part ( n, v, max_print, '  Output with MAX_PRINT = 25' )

  return
end
subroutine r82row_sort_heap_index_a_test ( )

!*****************************************************************************80
!
!! R82ROW_SORT_HEAP_INDEX_A_TEST tests R82ROW_SORT_HEAP_INDEX_A.
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

  real ( kind = 8 ) a(2,n)
  real ( kind = 8 ) :: b = 0.0D+00
  real ( kind = 8 ) :: c = 10.0D+00
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R82ROW_SORT_HEAP_INDEX_A_TEST'
  write ( *, '(a)' ) '  R82ROW_SORT_HEAP_INDEX_A index sorts an R82ROW'
  write ( *, '(a)' ) '  using heapsort.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  call r8mat_uniform_ab ( 2, n, b, c, seed, a )
!
!  Give a few elements the same first component.
!
  a(1,3) = a(1,5)
  a(1,4) = a(1,12)
!
!  Give a few elements the same second component.
!
  a(2,6) = a(2,1)
  a(2,2) = a(2,9)
!
!  Make two entries equal.
!
  a(1:2,7) = a(1:2,11)

  call r82row_print ( n, a, '  Before rearrangement:' )

  call r82row_sort_heap_index_a ( n, a, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I Index A(Index)'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,i8,i8,g14.6,g14.6)' ) i, indx(i), a(1:2,indx(i))
  end do

  call r82row_permute ( n, indx, a )

  call r82row_print ( n, a, '  After rearrangement by R82ROW_PERMUTE:' )

  return
end
subroutine r82row_sort_quick_a_test ( )

!*****************************************************************************80
!
!! R82ROW_SORT_QUICK_A_TEST tests R82ROW_SORT_QUICK_A.
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

  integer ( kind = 4 ), parameter :: n = 12

  real ( kind = 8 ) a(2,n)
  real ( kind = 8 ) :: b = 0.0D+00
  real ( kind = 8 ) :: c = 10.0D+00
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) ' R82ROW_SORT_QUICK_A_TEST'
  write ( *, '(a)' ) '  R82ROW_SORT_QUICK_A sorts an R82ROW'
  write ( *, '(a)' ) '  using quick sort.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  call r8mat_uniform_ab ( 2, n, b, c, seed, a )
!
!  Give a few elements the same first component.
!
  a(1,3) = a(1,5)
  a(1,4) = a(1,12)
!
!  Give a few elements the same second component.
!
  a(2,6) = a(2,1)
  a(2,2) = a(2,9)
!
!  Make two entries equal.
!
  a(1:2,7) = a(1:2,11)

  call r82row_print ( n, a, '  Before rearrangement:' )

  call r82row_sort_quick_a ( n, a )

  call r82row_print ( n, a, '  Sorted array:' )

  return
end
subroutine r83col_print_part_test ( )

!*****************************************************************************80
!
!! R83COL_PRINT_PART_TEST tests R83COL_PRINT_PART.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) max_print
!
!  FORTRAN 2D arrays are listed in row order.
!
  real ( kind = 8 ), dimension ( n, 3 ) :: v = reshape ( (/ &
    11.0,  21.0, 31.0, 41.0, 51.0, 61.0, 71.0, 81.0, 91.0, 101.0, &
    12.0,  22.0, 32.0, 42.0, 52.0, 62.0, 72.0, 82.0, 92.0, 102.0, &
    13.0,  23.0, 33.0, 43.0, 53.0, 63.0, 73.0, 83.0, 93.0, 103.0 /), (/ n, 3 /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R83COL_PRINT_PART_TEST'
  write ( *, '(a)' ) '  R83COL_PRINT_PART prints part of an R83COL.'

  max_print = 2
  call r83col_print_part ( n, v, max_print, '  Output with MAX_PRINT = 2' )

  max_print = 5
  call r83col_print_part ( n, v, max_print, '  Output with MAX_PRINT = 5' )

  max_print = 25
  call r83col_print_part ( n, v, max_print, '  Output with MAX_PRINT = 25' )

  return
end
subroutine r83row_print_part_test ( )

!*****************************************************************************80
!
!! R83ROW_PRINT_PART_TEST tests R83ROW_PRINT_PART.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) max_print
!
!  FORTRAN arrays are listed in row order.
!
  real ( kind = 8 ), dimension ( 3, n ) :: v = reshape ( (/ &
     11.0,  21.0, 31.0, &
     12.0,  22.0, 32.0, &
     13.0,  23.0, 33.0, &
     14.0,  24.0, 34.0, &
     15.0,  25.0, 35.0, &
     16.0,  26.0, 36.0, &
     17.0,  27.0, 37.0, &
     18.0,  28.0, 38.0, &
     19.0,  29.0, 39.0, &
     20.0,  30.0, 40.0 /), (/ 3, n /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R83ROW_PRINT_PART_TEST'
  write ( *, '(a)' ) '  R83ROW_PRINT_PART prints part of an R83ROW'
  write ( *, '(a)' ) '  as a list of columns (that is, transposed).'

  max_print = 2
  call r83row_print_part ( n, v, max_print, '  Output with MAX_PRINT = 2' )

  max_print = 5
  call r83row_print_part ( n, v, max_print, '  Output with MAX_PRINT = 5' )

  max_print = 25
  call r83row_print_part ( n, v, max_print, '  Output with MAX_PRINT = 25' )

  return
end
subroutine r8block_expand_linear_test ( )

!*****************************************************************************80
!
!! R8BLOCK_EXPAND_LINEAR_TEST tests R8BLOCK_EXPAND_LINEAR.
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

  integer ( kind = 4 ), parameter :: l = 4
  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 2
  integer ( kind = 4 ), parameter :: lfat = 1
  integer ( kind = 4 ), parameter :: mfat = 2
  integer ( kind = 4 ), parameter :: nfat = 1
  integer ( kind = 4 ), parameter :: l2 = ( l - 1 ) * ( lfat + 1 ) + 1
  integer ( kind = 4 ), parameter :: m2 = ( m - 1 ) * ( mfat + 1 ) + 1
  integer ( kind = 4 ), parameter :: n2 = ( n - 1 ) * ( nfat + 1 ) + 1
  real ( kind = 8 ), dimension(l,m,n) :: x = reshape ( (/ &
        1.0D+00,  2.0D+00,  3.0D+00,   4.0D+00,  1.0D+00, &
        4.0D+00,  9.0D+00, 16.0D+00,   1.0D+00,  8.0D+00, &
       27.0D+00, 64.0D+00,  2.0D+00,   4.0D+00,  6.0D+00, &
        8.0D+00,  2.0D+00,  8.0D+00,  18.0D+00, 32.0D+00, &
        2.0D+00, 16.0D+00, 54.0D+00, 128.0D+00 /), (/ l, m, n /) )
  real ( kind = 8 ) xfat(l2,m2,n2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8BLOCK_EXPAND_LINEAR_TEST'
  write ( *, '(a)' ) '  R8BLOCK_EXPAND_LINEAR linearly interpolates new data'
  write ( *, '(a)' ) '  between old values in a 3D block.'

  call r8block_print ( l, m, n, x, '  Original block:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  LFAT = ', lfat
  write ( *, '(a,i8)' ) '  MFAT = ', mfat
  write ( *, '(a,i8)' ) '  NFAT = ', nfat

  call r8block_expand_linear ( l, m, n, x, lfat, mfat, nfat, xfat )

  call r8block_print ( l2, m2, n2, xfat, '  Fattened block:' )

  return
end
subroutine r8block_print_test ( )

!*****************************************************************************80
!
!! R8BLOCK_PRINT_TEST tests R8BLOCK_PRINT.
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
  real ( kind = 8 ), dimension(l,m,n) :: x = reshape ( (/ &
        1.0D+00,  2.0D+00,  3.0D+00,   4.0D+00,  1.0D+00, &
        4.0D+00,  9.0D+00, 16.0D+00,   1.0D+00,  8.0D+00, &
       27.0D+00, 64.0D+00,  2.0D+00,   4.0D+00,  6.0D+00, &
        8.0D+00,  2.0D+00,  8.0D+00,  18.0D+00, 32.0D+00, &
        2.0D+00, 16.0D+00, 54.0D+00, 128.0D+00 /), (/ l, m, n /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8BLOCK_PRINT_TEST'
  write ( *, '(a)' ) '  R8BLOCK_PRINT prints an R8BLOCK.'

  call r8block_print ( l, m, n, x, '  The 3D array:' )

  return
end
subroutine r8r8vec_index_insert_unique_test ( )

!*****************************************************************************80
!
!! R8R8VEC_INDEX_INSERT_UNIQUE_TEST tests R8R8VEC_INDEX_INSERT_UNIQUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 30

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) :: x_max = 4.0D+00
  real ( kind = 8 ) :: x_min = 1.0D+00
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(n_max)
  real ( kind = 8 ) :: y_max = 3.0D+00
  real ( kind = 8 ) :: y_min = 1.0D+00
  real ( kind = 8 ) yval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8R8VEC_INDEX_INSERT_UNIQUE_TEST'
  write ( *, '(a)' ) '  R8R8VEC_INDEX_INSERT_UNIQUE inserts unique values into'
  write ( *, '(a)' ) '  an index sorted array.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8,a)' ) '  Generate ', n_max, ' random values:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Index    XVAL    YVAL'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, n_max

    xval = r8_uniform_ab ( x_min, x_max, seed )
    xval = real ( nint ( xval ), kind = 8 )
    yval = r8_uniform_ab ( y_min, y_max, seed )
    yval = real ( nint ( yval ), kind = 8 )

    call r8r8vec_index_insert_unique ( n_max, n, x, y, indx, xval, yval, &
      ival, ierror )

    write ( *, '(2x,i3,6x,f6.2,9x,f6.2)' ) ival, xval, yval

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Vector of unique X Y values:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  X(I)   Y(I)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,f6.2,9x,f6.2)' ) i, x(i), y(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  X, Y sorted by index'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(INDX(I))  Y(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2,9x,f6.2)' ) i, indx(i), &
      x(indx(i)), y(indx(i))
  end do

  return
end
subroutine r8r8r8vec_index_insert_unique_test ( )

!*****************************************************************************80
!
!! R8R8VEC_INDEX_INSERT_UNIQUE_TEST tests R8R8R8VEC_INDEX_INSERT_UNIQUE.
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

  integer ( kind = 4 ), parameter :: n_max = 30

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(n_max)
  real ( kind = 8 ) yval
  real ( kind = 8 ) z(n_max)
  real ( kind = 8 ) zval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8R8R8VEC_INDEX_INSERT_UNIQUE_TEST'
  write ( *, '(a)' ) '  R8R8R8VEC_INDEX_INSERT_UNIQUE inserts unique values'
  write ( *, '(a)' ) '  into an index sorted array.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of random values to generate = ', n_max
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    XVAL    YVAL  ZVAL  Index'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, n_max

    xval = r8_uniform_ab ( 1.0D+00, 4.0D+00, seed )
    xval = real ( nint ( xval ), kind = 8 )
    yval = r8_uniform_ab ( 1.0D+00, 3.0D+00, seed )
    yval = real ( nint ( yval ), kind = 8 )
    zval = r8_uniform_ab ( 1.0D+00, 4.0D+00, seed )
    zval = real ( nint ( zval ), kind = 8 )

    call r8r8r8vec_index_insert_unique ( n_max, n, x, y, z, indx, &
      xval, yval, zval, ival, ierror )

    write ( *, '(2x,i3,6x,f6.2,9x,f6.2,9x,f6.2)' ) ival, xval, yval, zval

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Vector of unique X Y Z values:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  X(I)   Y(I)    Z(I)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,f6.2,9x,f6.2,9x,f6.2)' ) i, x(i), y(i), z(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  X Y Z sorted by index:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2,9x,f6.2)' ) i, indx(i), &
      x(indx(i)), y(indx(i)), z(indx(i))
  end do

  return
end
subroutine r8int_to_i4int_test ( )

!*****************************************************************************80
!
!! R8INT_TO_I4INT_TEST tests R8INT_TO_I4INT.
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

  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) ir
  real ( kind = 8 ) r
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_uniform_ab
  real ( kind = 8 ) :: rhi = 200.0D+00
  real ( kind = 8 ) rhi2
  real ( kind = 8 ) :: rlo = 100.0D+00
  real ( kind = 8 ) rlo2
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) :: test_num = 10

  ilo = 1
  ihi = 11

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8INT_TO_I4INT_TEST'
  write ( *, '(a)' ) '  For data in an interval,'
  write ( *, '(a)' ) '  R8INT_TO_I4INT converts a real to an integer.'
  write ( *, '(a)' ) ''
  write ( *, '(a,2i8)' ) '  Integer interval: ', ilo, ihi
  write ( *, '(a,2g14.6)' ) '  Real interval: ', rlo, rhi
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  R   I(R)  R(I(R))'
  write ( *, '(a)' ) ''

  seed = 123456789

  rlo2 = rlo - 15.0D+00
  rhi2 = rhi + 15.0D+00

  do test = 1, test_num
    r = r8_uniform_ab ( rlo2, rhi2, seed )
    call r8int_to_i4int ( rlo, rhi, r, ilo, ihi, ir )
    call i4int_to_r8int ( ilo, ihi, ir, rlo, rhi, r2 )
    write ( *, '(2x,g14.6,i8,g14.6)' ) r, ir, r2
  end do

  return
end
subroutine r8mat_cholesky_factor_test ( )

!*****************************************************************************80
!
!! R8MAT_CHOLESKY_FACTOR_TEST tests R8MAT_CHOLESKY_FACTOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) d(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) j
  real ( kind = 8 ) l(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_CHOLESKY_FACTOR_TEST'
  write ( *, '(a)' ) '  R8MAT_CHOLESKY_FACTOR determines the'
  write ( *, '(a)' ) '  lower triangular Cholesky factorization'
  write ( *, '(a)' ) '  of a positive definite symmetric matrix,'

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 2.0D+00
      else if ( abs ( i - j ) == 1 ) then
        a(i,j) = -1.0D+00
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  call r8mat_print ( n, n, a, '  Matrix to be factored:' )
!
!  Compute the Cholesky factor.
!
  call r8mat_cholesky_factor ( n, a, l, ierror )
  call r8mat_print ( n, n, l, '  Cholesky factor L:' )
  d(1:n,1:n) = matmul ( l(1:n,1:n), transpose ( l(1:n,1:n) ) )
  call r8mat_print ( n, n, d, '  Product L * L'':' )

  return
end
subroutine r8mat_cholesky_factor_upper_test ( )

!*****************************************************************************80
!
!! R8MAT_CHOLESKY_FACTOR_UPPER_TEST tests R8MAT_CHOLESKY_FACTOR_UPPER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) d(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  real ( kind = 8 ) r(n,n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_CHOLESKY_FACTOR_UPPER_TEST'
  write ( *, '(a)' ) '  R8MAT_CHOLESKY_FACTOR_UPPER determines the'
  write ( *, '(a)' ) '  upper triangular Cholesky factorization'
  write ( *, '(a)' ) '  of a positive definite symmetric matrix,'

  seed = 123456789
  call r8mat_uniform_01 ( n, n, seed, a )
  a = ( a + transpose ( a ) ) / 2.0D+00
  do i = 1, n
    a(i,i) = a(i,i) + real ( n, kind = 8 )
  end do

  call r8mat_print ( n, n, a, '  Matrix A to be factored:' )
!
!  Compute the Cholesky factor.
!
  call r8mat_cholesky_factor_upper ( n, a, r, ierror )
  call r8mat_print ( n, n, r, '  Upper Cholesky factor R:' )
  d(1:n,1:n) = matmul ( transpose ( r(1:n,1:n) ), r(1:n,1:n) )
  call r8mat_print ( n, n, d, '  Product R'' * R:' )

  return
end
subroutine r8mat_cholesky_inverse_test ( )

!*****************************************************************************80
!
!! R8MAT_CHOLESKY_INVERSE_TEST tests R8MAT_CHOLESKY_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) a2(n,n)
  real ( kind = 8 ) a3(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_CHOLESKY_INVERSE_TEST'
  write ( *, '(a)' ) '  For a positive definite symmetric matrix,'
  write ( *, '(a)' ) '  R8MAT_CHOLESKY_INVERSE computes the inverse.'

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 2.0D+00
      else if ( abs ( i - j ) == 1 ) then
        a(i,j) = -1.0D+00
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  call r8mat_print ( n, n, a, '  Matrix to be inverted:' )

  a2(1:n,1:n) = a(1:n,1:n)

  call r8mat_cholesky_inverse ( n, a2 )

  call r8mat_print ( n, n, a2, '  Inverse matrix:' )

  a3(1:n,1:n) = matmul ( a2(1:n,1:n), a(1:n,1:n) )
  
  call r8mat_print ( n, n, a3, '  Product inv(A) * A:' )

  return
end
subroutine r8mat_cholesky_solve_test ( )

!*****************************************************************************80
!
!! R8MAT_CHOLESKY_SOLVE_TEST tests R8MAT_CHOLESKY_SOLVE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) d(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) j
  real ( kind = 8 ) l(n,n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_CHOLESKY_SOLVE_TEST'
  write ( *, '(a)' ) '  For a positive definite symmetric matrix,'
  write ( *, '(a)' ) '  R8MAT_CHOLESKY_SOLVE solves a linear system'
  write ( *, '(a)' ) '  using the lower Cholesky factorization.'

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 2.0D+00
      else if ( abs ( i - j ) == 1 ) then
        a(i,j) = -1.0D+00
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  call r8mat_print ( n, n, a, '  Matrix to be factored:' )
!
!  Compute the Cholesky factor.
!
  call r8mat_cholesky_factor ( n, a, l, ierror )
  call r8mat_print ( n, n, l, '  Cholesky factor L:' )
  d(1:n,1:n) = matmul ( l(1:n,1:n), transpose ( l(1:n,1:n) ) )
  call r8mat_print ( n, n, d, '  Product L * L'':' )
!
!  Solve a linear system.
!
  b(1:n-1) = 0.0D+00
  b(n) = real ( n + 1, kind = 8 )
  call r8vec_print ( n, b, '  Right hand side:' )

  call r8mat_cholesky_solve ( n, l, b, x )

  call r8vec_print ( n, x, '  Computed solution:' )

  return
end
subroutine r8mat_cholesky_solve_upper_test ( )

!*****************************************************************************80
!
!! R8MAT_CHOLESKY_SOLVE_UPPER_TEST tests R8MAT_CHOLESKY_SOLVE_UPPER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) d(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) j
  real ( kind = 8 ) r(n,n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_CHOLESKY_SOLVE_UPPER_TEST'
  write ( *, '(a)' ) '  For a positive definite symmetric matrix,'
  write ( *, '(a)' ) '  R8MAT_CHOLESKY_SOLVE_UPPER solves a linear system'
  write ( *, '(a)' ) '  using the upper Cholesky factorization.'

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 2.0D+00
      else if ( abs ( i - j ) == 1 ) then
        a(i,j) = -1.0D+00
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  call r8mat_print ( n, n, a, '  Matrix to be factored:' )
!
!  Compute the Cholesky factor.
!
  call r8mat_cholesky_factor_upper ( n, a, r, ierror )
  call r8mat_print ( n, n, r, '  Cholesky factor R:' )
  d(1:n,1:n) = matmul ( transpose ( r(1:n,1:n) ), r(1:n,1:n) )
  call r8mat_print ( n, n, d, '  Product R'' * R:' )
!
!  Solve a linear system.
!
  b(1:n-1) = 0.0D+00
  b(n) = real ( n + 1, kind = 8 )
  call r8vec_print ( n, b, '  Right hand side:' )

  call r8mat_cholesky_solve_upper ( n, r, b, x )

  call r8vec_print ( n, x, '  Computed solution:' )

  return
end
subroutine r8mat_det_2d_test ( )

!*****************************************************************************80
!
!! R8MAT_DET_2D_TEST tests R8MAT_DET_2D;
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

  integer ( kind = 4 ), parameter :: n = 2

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_det_2d
  real ( kind = 8 ), dimension ( n ) :: x = (/ &
    1.0D+00, 10.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_DET_2D_TEST'
  write ( *, '(a)' ) '  R8MAT_DET_2D: determinant of a 2 by 2 matrix;'

  call r8mat_vand2 ( n, x, a )

  det = r8mat_det_2d ( a )

  call r8mat_print ( n, n, a, '  Matrix:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  R8MAT_DET_2D computes determinant:', det
!
!  Special formula for the determinant of a Vandermonde matrix:
!
  det = 1.0D+00
  do i = 1, n
    do j = 1, i-1
      det = det * ( x(i) - x(j) )
    end do
  end do
  write ( *, '(a,g14.6)' ) '  Exact determinant is ', det

  return
end
subroutine r8mat_det_3d_test ( )

!*****************************************************************************80
!
!! R8MAT_DET_3D_TESt tests R8MAT_DET_3D;
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

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_det_3d
  real ( kind = 8 ), dimension ( n ) :: x = (/ &
    1.0D+00, 10.0D+00, 4.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_DET_3D_TEST'
  write ( *, '(a)' ) '  R8MAT_DET_3D: determinant of a 3 by 3 matrix;'

  call r8mat_vand2 ( n, x, a )
  det = r8mat_det_3d ( a )

  call r8mat_print ( n, n, a, '  Matrix:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  R8MAT_DET_3D computes determinant:', det
!
!  Special formula for the determinant of a Vandermonde matrix:
!
  det = 1.0D+00
  do i = 1, n
    do j = 1, i - 1
      det = det * ( x(i) - x(j) )
    end do
  end do
  write ( *, '(a,g14.6)' ) '  Exact determinant is ', det

  return
end
subroutine r8mat_det_4d_test ( )

!*****************************************************************************80
!
!! R8MAT_DET_4D_TEST tests R8MAT_DET_4D;
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

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_det_4d
  real ( kind = 8 ), dimension ( n ) :: x = (/ &
    1.0D+00, 10.0D+00, 4.0D+00, 2.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_DET_4D_TEST'
  write ( *, '(a)' ) '  R8MAT_DET_4D determinant of a 4 by 4 matrix;'

  call r8mat_vand2 ( n, x, a )
  det = r8mat_det_4d ( a )

  call r8mat_print ( n, n, a, '  Matrix:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  R8MAT_DET_4D computes determinant:', det
!
!  Special formula for the determinant of a Vandermonde matrix:
!
  det = 1.0D+00
  do i = 1, n
    do j = 1, i - 1
      det = det * ( x(i) - x(j) )
    end do
  end do
  write ( *, '(a,g14.6)' ) '  Exact determinant is ', det

  return
end
subroutine r8mat_det_5d_test ( )

!*****************************************************************************80
!
!! R8MAT_DET_5D_TEST tests R8MAT_DET_5D;
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_det_5d
  real ( kind = 8 ), dimension ( n ) :: x = (/ &
    1.0D+00, 10.0D+00, 4.0D+00, 2.0D+00, 3.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_DET_5D_TEST'
  write ( *, '(a)' ) '  R8MAT_DET_5D: determinant of 5 by 5 matrix.'

  call r8mat_vand2 ( n, x, a )
  det = r8mat_det_5d ( a )

  call r8mat_print ( n, n, a, '  Matrix:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  R8MAT_DET_5D computes determinant: ', det
!
!  Special formula for the determinant of a Vandermonde matrix:
!
  det = 1.0D+00
  do i = 1, n
    do j = 1, i - 1
      det = det * ( x(i) - x(j) )
    end do
  end do
  write ( *, '(a,g14.6)' ) '  Exact determinant is ', det

  return
end
subroutine r8mat_expand_linear_test ( )

!*****************************************************************************80
!
!! R8MAT_EXPAND_LINEAR_TEST tests R8MAT_EXPAND_LINEAR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 3
  integer ( kind = 4 ), parameter :: mfat = 2
  integer ( kind = 4 ), parameter :: nfat = 1
  integer ( kind = 4 ), parameter :: m2 = ( m - 1 ) * ( mfat + 1 ) + 1
  integer ( kind = 4 ), parameter :: n2 = ( n - 1 ) * ( nfat + 1 ) + 1
!
!  Each row of this definition is a COLUMN of the matrix.
!
  real ( kind = 8 ), dimension(m,n) :: x = reshape ( (/ &
    1.0D+00, 2.0D+00,  3.0D+00,  4.0D+00, &
    1.0D+00, 4.0D+00,  9.0D+00, 16.0D+00, &
    1.0D+00, 8.0D+00, 27.0D+00, 64.0D+00 /), (/ m, n /) )
  real ( kind = 8 ) xfat(m2,n2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_EXPAND_LINEAR_TEST'
  write ( *, '(a)' ) '  R8MAT_EXPAND_LINEAR linearly interpolates new data'
  write ( *, '(a)' ) '  between old values in a matrix.'

  call r8mat_print ( m, n, x, '  Original matrix:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  MFAT = ', mfat
  write ( *, '(a,i8)' ) '  NFAT = ', nfat

  call r8mat_expand_linear ( m, n, x, mfat, nfat, xfat )

  call r8mat_print ( m2, n2, xfat, '  Fattened matrix:' )

  return
end
subroutine r8mat_expand_linear2_test ( )

!*****************************************************************************80
!
!! R8MAT_EXPAND_LINEAR2_TEST tests R8MAT_EXPAND_LINEAR2.
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
  integer ( kind = 4 ), parameter :: m2 = 10
  integer ( kind = 4 ), parameter :: n = 2
  integer ( kind = 4 ), parameter :: n2 = 5

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) a2(m2,n2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_EXPAND_LINEAR2_TEST'
  write ( *, '(a)' ) '  R8MAT_EXPAND_LINEAR2 fills in a large array by'
  write ( *, '(a)' ) '  interpolating data from a small array.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Original matrix has dimensions:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,2i8)' ) m, n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Expanded matrix has dimensions:'
  write ( *, '(a)' ) ''
  write ( *, '(2x,2i8)' ) m2, n2

  do i = 1, m
    do j = 1, n
      a(i,j) = 10.0D+00 * real ( i, kind = 8 ) + real ( j, kind = 8 )
    end do
  end do

  call r8mat_print ( m, n, a, '  The little matrix A:' )

  call r8mat_expand_linear2 ( m, n, a, m2, n2, a2 )

  call r8mat_print ( m2, n2, a2, '  Expanded array A2:' )

  return
end
subroutine r8mat_fs_test ( )

!*****************************************************************************80
!
!! R8MAT_FS_TEST tests R8MAT_FS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 January 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_FS_TEST'
  write ( *, '(a)' ) '  For a matrix in general storage,'
  write ( *, '(a)' ) '  R8MAT_FS factors and solves a linear system.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  call r8mat_uniform_01 ( n, n, seed, a )
!
!  Set the desired solutions.
!
  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do
  call r8mat_mv ( n, n, a, x, b )
!
!  Factor and solve the system.
!
  call r8mat_fs ( n, a, b, info )
      
  if ( info /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'R8MAT_FS_TEST - Fatal error!'
    write ( *, '(a)' ) '  R8MAT_FS reports the matrix is singular.'
    return
  end if

  call r8vec_print ( n, b, '  Solution:' )

  return
end
subroutine r8mat_fss_test ( )

!*****************************************************************************80
!
!! r8mat_fss_test tests r8mat_fss().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 February 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10
  integer ( kind = 4 ), parameter :: nb = 3

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,nb)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n,nb)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8mat_fss_test'
  write ( *, '(a)' ) '  r8mat_fss() factors a square matrix and'
  write ( *, '(a)' ) '  solves multiple linear systems.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  call r8mat_uniform_01 ( n, n, seed, a )
!
!  X contains three columns of desired solutions.
!
  x(1:n,1) = 1.0D+00
  do i = 1, n
    x(i,2) = real ( i, kind = 8 )
  end do
  do i = 1, n
    x(i,3) = real ( 1 + mod ( i - 1, 3 ), kind = 8 )
  end do

  call r8mat_print ( n, nb, x, '  Desired solutions:' )
!
!  Set B = A * X.
!
  b = matmul ( a, x )
!
!  Call r8mat_fss to factor and solve the system.
!
  call r8mat_fss ( n, a, nb, b, info )
  
  if ( info /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'R8MAT_FSS_TEST - Fatal error!'
    write ( *, '(a)' ) '  R8MAT_FSS reports the matrix is singular.'
    return
  end if

  call r8mat_print ( n, nb, b, '  Computed solutions:' )

  return
end
subroutine r8mat_givens_post_test ( )

!*****************************************************************************80
!
!! R8MAT_GIVENS_POST_TEST tests R8MAT_GIVENS_POST.
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

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) ag(n,n)
  integer ( kind = 4 ) col
  real ( kind = 8 ) g(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) row

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_GIVENS_POST_TEST'
  write ( *, '(a)' ) '  R8MAT_GIVENS_POST computes a Givens ' // &
    'postmultiplier rotation matrix.'

  do i = 1, n
    do j = 1, n
      a(i,j) = real ( i ** ( j - 1 ), kind = 8 )
    end do
  end do

  call r8mat_print ( n, n, a, '  Matrix A:' )

  row = 3
  col = 2

  write ( *, '(a)' ) ''
  write ( *, '(a,2i8)' ) '  I, J=', row, col

  call r8mat_givens_post ( n, a, row, col, g )

  call r8mat_print ( n, n, g, '  G' )

  ag(1:n,1:n) = matmul ( a(1:n,1:n), g(1:n,1:n) )

  call r8mat_print ( n, n, ag, '  A*G' )

  return
end
subroutine r8mat_givens_pre_test ( )

!*****************************************************************************80
!
!! R8MAT_GIVENS_PRE_TEST tests R8MAT_GIVENS_PRE.
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

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) col
  real ( kind = 8 ) g(n,n)
  real ( kind = 8 ) ga(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) row

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_GIVENS_PRE_TEST'
  write ( *, '(a)' ) '  R8MAT_GIVENS_PRE computes a Givens ' // &
    'premultiplier rotation matrix.'

  do i = 1, n
    do j = 1, n
      a(i,j) = real ( i**(j-1), kind = 8 )
    end do
  end do

  call r8mat_print ( n, n, a, '  Matrix A:' )

  row = 3
  col = 2

  write ( *, '(a)' ) ''
  write ( *, '(a,2i8)' ) '  I, J=', row, col

  call r8mat_givens_pre ( n, a, row, col, g )

  call r8mat_print ( n, n, g, '  G' )

  ga(1:n,1:n) = matmul ( g(1:n,1:n), a(1:n,1:n) )

  call r8mat_print ( n, n, ga, '  G*A' )

  return
end
subroutine r8mat_hess_test ( )

!*****************************************************************************80
!
!! R8MAT_HESS_TEST tests R8MAT_HESS.
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

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) h(n,n)
  external r8mat_hess_f
  real ( kind = 8 ), dimension ( n ) :: x = (/ &
    1.0D+00, 2.0D+00, 3.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_HESS_TEST'
  write ( *, '(a)' ) '  R8MAT_HESS estimates the Hessian matrix'
  write ( *, '(a)' ) '  of a scalar function.'

  call r8mat_hess ( r8mat_hess_f, n, x, h )

  call r8mat_print ( n, n, h, '  Estimated jacobian:' )

  call r8mat_hess_exact ( n, x, h )

  call r8mat_print ( n, n, h, '  Exact jacobian:' )

  return
end
subroutine r8mat_hess_f ( n, x, f )

!*****************************************************************************80
!
!! R8MAT_HESS_F is a sample nonlinear function for treatment by R8MAT_HESS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of parameters.
!
!    Input, real ( kind = 8 ) X(N), the parameter values.
!
!    Output, real ( kind = 8 ) F, the function value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f
  real ( kind = 8 ) x(n)

  f = x(1)**2 + x(1) * x(2) + x(2) * cos ( 10.0D+00 * x(3) )

  return
end
subroutine r8mat_hess_exact ( n, x, h )

!*****************************************************************************80
!
!! R8MAT_HESS_EXACT is the exact Hessian of R8MAT_HESS_F.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of parameters.
!
!    Input, real ( kind = 8 ) X(N), the parameter values.
!
!    Output, real ( kind = 8 ) H(N,N), the Hessian values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) h(n,n)
  real ( kind = 8 ) x(n)

  h(1,1) = 2.0D+00
  h(1,2) = 1.0D+00
  h(1,3) = 0.0D+00

  h(2,1) = 1.0D+00
  h(2,2) = 0.0D+00
  h(2,3) = - 10.0D+00 * sin ( 10.0D+00 * x(3) )

  h(3,1) = 0.0D+00
  h(3,2) = - 10.0D+00 * sin ( 10.0D+00 * x(3) )
  h(3,3) = - 100.0D+00 * x(2) * cos ( 10.0D+00 * x(3) )

  return
end
subroutine r8mat_house_axh_test ( )

!*****************************************************************************80
!
!! R8MAT_HOUSE_AXH_TEST tests R8MAT_HOUSE_AXH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) a_col(n)
  real ( kind = 8 ) ah(n,n)
  real ( kind = 8 ) h(n,n)
  real ( kind = 8 ) ha(n,n)
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  integer ( kind = 4 ) seed
  real ( kind = 8 ) v(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_HOUSE_AXH_TEST'
  write ( *, '(a)' ) '  R8MAT_HOUSE_AXH multiplies a matrix A times a'
  write ( *, '(a)' ) '  compact Householder matrix.'

  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789

  call r8mat_uniform_ab ( n, n, r8_lo, r8_hi, seed, a )

  call r8mat_print ( n, n, a, '  Matrix A:' )
!
!  Request V, the compact form of the Householder matrix H
!  such that H*A packs column 3 of A.
!
  k = 3
  a_col(1:n) = a(1:n,k)
  call r8vec_house_column ( n, a_col, k, v )

  call r8vec_print ( n, v, '  Compact vector V so H*A packs column 3:' )

  call r8mat_house_form ( n, v, h )

  call r8mat_print ( n, n, h, '  Householder matrix H:' )
!
!  Compute A*H.
!
  call r8mat_house_axh ( n, a, v, ah )

  call r8mat_print ( n, n, ah, '  Indirect product A*H:' )
!
!  Compare with a direct calculation of A*H.
!
  ah = matmul ( a(1:n,1:n), h(1:n,1:n) )
  call r8mat_print ( n, n, ah, '  Direct product A*H:' )
!
!  Compute H*A to verify packing.
!
  ha = matmul ( h(1:n,1:n), a(1:n,1:n) )
  call r8mat_print ( n, n, ha, '  Product H*A has packed column 3:' )

  return
end
subroutine r8mat_house_form_test ( )

!*****************************************************************************80
!
!! R8MAT_HOUSE_FORM_TEST tests R8MAT_HOUSE_FORM.
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) h(n,n)
  real ( kind = 8 ), dimension ( n ) :: v = (/ &
    0.0D+00, 0.0D+00, 1.0D+00, 2.0D+00, 3.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_HOUSE_FORM_TEST'
  write ( *, '(a)' ) '  R8MAT_HOUSE_FORM forms a Householder'
  write ( *, '(a)' ) '  matrix from its compact form.'

  call r8vec_print ( n, v, '  Compact vector form V:' )

  call r8mat_house_form ( n, v, h )

  call r8mat_print ( n, n, h, '  Householder matrix H:' )

  return
end
subroutine r8mat_house_post_test ( )

!*****************************************************************************80
!
!! R8MAT_HOUSE_POST_TEST tests R8MAT_HOUSE_POST.
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) ah(n,n)
  real ( kind = 8 ), parameter :: b = 0.0D+00
  real ( kind = 8 ), parameter :: c = 5.0D+00
  real ( kind = 8 ) h(n,n)
  integer ( kind = 4 ) row
  integer ( kind = 4 ) col
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_HOUSE_POST_TEST'
  write ( *, '(a)' ) '  R8MAT_HOUSE_POST computes a Householder'
  write ( *, '(a)' ) '  postmultiplier;'

  seed = 123456789

  call r8mat_uniform_ab ( n, n, b, c, seed, a )

  call r8mat_print ( n, n, a, '  Matrix A:' )

  row = 2
  col = 3

  write ( *, '(a)' ) ''
  write ( *, '(a,2i8)' ) '  I, J=', row, col

  call r8mat_house_post ( n, a, row, col, h )

  call r8mat_print ( n, n, h, '  Householder matrix H:' )

  ah(1:n,1:n) = matmul ( a(1:n,1:n), h(1:n,1:n) )

  call r8mat_print ( n, n, ah, '  Product A*H:' )

  return
end
subroutine r8mat_house_pre_test ( )

!*****************************************************************************80
!
!! R8MAT_HOUSE_PRE_TEST tests R8MAT_HOUSE_PRE.
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ), parameter :: b = 0.0D+00
  real ( kind = 8 ), parameter :: c = 5.0D+00
  integer ( kind = 4 ) col
  real ( kind = 8 ) h(n,n)
  real ( kind = 8 ) ha(n,n)
  integer ( kind = 4 ) row
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_HOUSE_PRE_TEST'
  write ( *, '(a)' ) '  R8MAT_HOUSE_PRE computes a Householder premultiplier;'

  seed = 123456789

  call r8mat_uniform_ab ( n, n, b, c, seed, a )

  call r8mat_print ( n, n, a, '  Matrix A:' )

  row = 2
  col = 3

  write ( *, '(a)' ) ''
  write ( *, '(a,2i8)' ) '  I, J=', row, col

  call r8mat_house_pre ( n, a, row, col, h )

  call r8mat_print ( n, n, h, '  Householder matrix H:' )

  ha(1:n,1:n) = matmul ( h(1:n,1:n), a(1:n,1:n) )

  call r8mat_print ( n, n, ha, '  Product H*A:' )

  return
end
subroutine r8mat_indicator_test ( )

!*****************************************************************************80
!
!! R8MAT_INDICATOR_TEST tests R8MAT_INDICATOR.
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
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_INDICATOR_TEST'
  write ( *, '(a)' ) '  R8MAT_INDICATOR returns an indicator matrix.'

  call r8mat_indicator ( m, n, a )

  call r8mat_print ( m, n, a, '  The indicator matrix:' )

  return
end
subroutine r8mat_inverse_2d_test ( )

!*****************************************************************************80
!
!! R8MAT_INVERSE_2D_TEST tests R8MAT_INVERSE_2D.
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

  integer ( kind = 4 ), parameter :: n = 2
!
!  Each ROW of this definion is a COLUMN of the matrix.
!
  real ( kind = 8 ), dimension(n,n) :: a = reshape ( (/ &
    1.0D+00, 3.0D+00, &
    2.0D+00, 4.0D+00 /), (/ 2, 2 /) )
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) c(n,n)
  real ( kind = 8 ) det

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_INVERSE_2D_TEST'
  write ( *, '(a)' ) '  R8MAT_INVERSE_2D inverts a 2 by 2 matrix.'

  call r8mat_print ( n, n, a, '  Matrix A to invert:' )
!
!  Compute the inverse matrix.
!
  call r8mat_inverse_2d ( a, b, det )

  if ( det == 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  The input matrix was singular, no inverse'
    write ( *, '(a)' ) '  could be computed.'
    return
  end if

  call r8mat_print ( n, n, b, '  Inverse matrix B:' )

  c(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )

  call r8mat_print ( n, n, c, '  Product C = A * B:' )

  return
end
subroutine r8mat_inverse_3d_test ( )

!*****************************************************************************80
!
!! R8MAT_INVERSE_3D_TEST tests R8MAT_INVERSE_3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3
!
!  Each ROW of this definion is a COLUMN of the matrix.
!
  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
    1.0D+00, 4.0D+00, 7.0D+00, &
    2.0D+00, 5.0D+00, 8.0D+00, &
    3.0D+00, 6.0D+00, 0.0D+00 /), (/ n, n /) )
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) c(n,n)
  real ( kind = 8 ) det

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_INVERSE_3D_TEST'
  write ( *, '(a)' ) '  R8MAT_INVERSE_3D inverts a 3 by 3 matrix.'

  call r8mat_print ( n, n, a, '  Matrix A to be inverted:' )
!
!  Compute the inverse matrix.
!
  call r8mat_inverse_3d ( a, b, det )

  if ( det == 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  The input matrix was singular, no inverse'
    write ( *, '(a)' ) '  could be computed.'
    return
  end if

  call r8mat_print ( n, n, b, '  Inverse matrix B:' )

  c(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )

  call r8mat_print ( n, n, c, '  Product C = A * B:' )

  return
end
subroutine r8mat_inverse_4d_test ( )

!*****************************************************************************80
!
!! R8MAT_INVERSE_4D_TEST tests R8MAT_INVERSE_4D.
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

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) c(n,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_INVERSE_4D_TEST'
  write ( *, '(a)' ) '  R8MAT_INVERSE_4D inverts a 4 x 4 matrix.'

  do i = 1, n
    do j = 1, n

      if ( i <= j ) then
        a(i,j) = real ( n + 1 - j, kind = 8 )
      else if ( j == i - 1 ) then
        a(i,j) = n - j
      else
        a(i,j) = 0.0D+00
      end if

    end do
  end do

  call r8mat_print ( n, n, a, '  Matrix A to be inverted:' )

  call r8mat_inverse_4d ( a, b, det )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Determinant is ', det

  call r8mat_print ( n, n, b, '  Inverse B:' )

  c(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )

  call r8mat_print ( n, n, c, '  Product C = A * B:' )

  return
end
subroutine r8mat_is_integer_test ( )

!*****************************************************************************80
!
!! R8MAT_IS_INTEGER_TEST tests R8MAT_IS_INTEGER.
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

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(m,n)
  logical r8mat_is_integer

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_IS_INTEGER_TEST'
  write ( *, '(a)' ) '  R8MAT_IS_INTEGER is TRUE if every entry of an R8MAT'
  write ( *, '(a)' ) '  is an integer.'

  a = reshape ( (/ &
    1.0D+00, 4.0D+00, &
    2.0D+00, 5.0D+00, &
    3.0D+00, 6.0D+00 /), (/ 2, 3 /) )

  call r8mat_print ( m, n, a, '  Example 1: Obviously integer' )
  if ( r8mat_is_integer ( m, n, a ) ) then
    write ( *, '(a)' ) '  A is an integer matrix.'
  else
    write ( *, '(a)' ) '  A is NOT an integer matrix.'
  end if

  a(2,3) = 6.5D+00

  call r8mat_print ( m, n, a, '  Example 2: Obviously NOT integer' )
  if ( r8mat_is_integer ( m, n, a ) ) then
    write ( *, '(a)' ) '  A is an integer matrix.'
  else
    write ( *, '(a)' ) '  A is NOT an integer matrix.'
  end if

  a(2,3) = 6.0D+00
  a(2,2) = 5.00000001D+00

  call r8mat_print ( m, n, a, '  Example 3: Not obviously not integer' )
  if ( r8mat_is_integer ( m, n, a ) ) then
    write ( *, '(a)' ) '  A is an integer matrix.'
  else
    write ( *, '(a)' ) '  A is NOT an integer matrix.'
  end if

  a(2,2) = 5.0D+00
  a(2,3) = 300000000.2D+00
  call r8mat_print ( m, n, a, '  Example 4: Not obviously not integer' )
  if ( r8mat_is_integer ( m, n, a ) ) then
    write ( *, '(a)' ) '  A is an integer matrix.'
  else
    write ( *, '(a)' ) '  A is NOT an integer matrix.'
  end if

  return
end
subroutine r8mat_jac_test ( )

!*****************************************************************************80
!
!! R8MAT_JAC_TEST tests R8MAT_JAC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) :: eps = 0.00001D+00
  real ( kind = 8 ) fprime(m,n)
  external r8mat_jac_f
  real ( kind = 8 ), dimension ( n ) :: x = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_JAC_TEST'
  write ( *, '(a)' ) '  R8MAT_JAC estimates the M by N jacobian matrix'
  write ( *, '(a)' ) '  of a nonlinear function.'

  call r8mat_jac ( m, n, eps, r8mat_jac_f, x, fprime )

  call r8mat_print ( m, n, fprime, '  Estimated jacobian:' )

  call r8mat_jac_exact ( m, n, x, fprime )

  call r8mat_print (  m, n, fprime, '  Exact jacobian:' )

  return
end
subroutine r8mat_jac_f ( m, n, x, f )

!*****************************************************************************80
!
!! R8MAT_JAC_F is a sample nonlinear function for treatment by R8MAT_JAC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of functions.
!
!    Input, integer ( kind = 4 ) N, the number of parameters.
!
!    Input, real ( kind = 8 ) X(N), the parameter values.
!
!    Output, real ( kind = 8 ) F(M), the function values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) f(m)
  real ( kind = 8 ) x(n)

  f(1) = sin ( x(1) * x(2) )
  f(2) = sqrt ( 1.0D+00 + x(1) ** 2 ) + x(3)
  f(3) = x(1) + 2.0D+00 * x(2) + 3.0D+00 * x(3) + 4.0D+00 * x(4)

  return
end
subroutine r8mat_jac_exact ( m, n, x, fprime )

!*****************************************************************************80
!
!! R8MAT_JAC_EXACT is the exact jacobian of R8MAT_JAC_F.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of functions.
!
!    Input, integer ( kind = 4 ) N, the number of parameters.
!
!    Input, real ( kind = 8 ) X(N), the parameter values.
!
!    Output, real ( kind = 8 ) FPRIME(M,N), the jacobian values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) fprime(m,n)
  real ( kind = 8 ) x(n)

  fprime(1,1) = cos ( x(1) * x(2) ) * x(2)
  fprime(1,2) = cos ( x(1) * x(2) ) * x(1)
  fprime(1,3) = 0.0D+00
  fprime(1,4) = 0.0D+00

  fprime(2,1) = x(1) / sqrt ( 1.0D+00 + x(1)**2 )
  fprime(2,2) = 0.0D+00
  fprime(2,3) = 1.0D+00
  fprime(2,4) = 0.0D+00

  fprime(3,1) = 1.0D+00
  fprime(3,2) = 2.0D+00
  fprime(3,3) = 3.0D+00
  fprime(3,4) = 4.0D+00

  return
end
subroutine r8mat_kronecker_test ( )

!*****************************************************************************80
!
!! R8MAT_KRONECKER_TEST tests R8MAT_KRONECKER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m1 = 2
  integer ( kind = 4 ), parameter :: m2 = 3
  integer ( kind = 4 ), parameter :: m = m1 * m2
  integer ( kind = 4 ), parameter :: n1 = 3
  integer ( kind = 4 ), parameter :: n2 = 2
  integer ( kind = 4 ), parameter :: n = n1 * n2

  real ( kind = 8 ), dimension (m1,n1) :: a = reshape ( (/ &
    1.0D+00, 4.0D+00, &
    2.0D+00, 5.0D+00, &
    3.0D+00, 6.0D+00 /), (/ 2, 3 /) )
  real ( kind = 8 ), dimension (m2,n2) :: b = reshape ( (/ &
    7.0D+00,  9.0D+00, 11.0D+00, &
    8.0D+00, 10.0D+00, 12.0D+00 /), (/ 3, 2 /) )
  real ( kind = 8 ) c(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_KRONECKER_TEST'
  write ( *, '(a)' ) '  R8MAT_KRONECKER computes the Kronecker product'
  write ( *, '(a)' ) '  of two matrices.'

  call r8mat_print ( m1, n1, a, '  Factor matrix A:' )
  call r8mat_print ( m2, n2, b, '  Factor matrix B:' )

  call r8mat_kronecker ( m1, n1, a, m2, n2, b, c )

  call r8mat_print ( m, n, c, '  Kronecker product C = kron(A,B)' )

  return
end
subroutine r8mat_l_inverse_test ( )

!*****************************************************************************80
!
!! R8MAT_L_INVERSE_TEST tests R8MAT_L_INVERSE.
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

  integer ( kind = 4 ), parameter :: n = 4
!
!  Each row of this definition is a COLUMN of the matrix.
!
  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
    1.0D+00, 2.0D+00, 4.0D+00,  7.0D+00, &
    0.0D+00, 3.0D+00, 5.0D+00,  8.0D+00, &
    0.0D+00, 0.0D+00, 6.0D+00,  9.0D+00, &
    0.0D+00, 0.0D+00, 0.0D+00, 10.0D+00 /), (/ n, n /) )
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) c(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_L_INVERSE_TEST'
  write ( *, '(a)' ) '  R8MAT_L_INVERSE inverts a lower triangular matrix.'

  call r8mat_print ( n, n, a, '  Matrix A to be inverted:' )

  call r8mat_l_inverse ( n, a, b )

  call r8mat_print ( n, n, b, '  Inverse matrix B:' )

  c(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )

  call r8mat_print ( n, n, c, '  Product C = A * B:' )

  return
end
subroutine r8mat_l_print_test ( )

!*****************************************************************************80
!
!! R8MAT_L_PRINT_TEST tests R8MAT_L_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), dimension(28) :: a1 = (/ &
    11.0D+00, 21.0D+00, 31.0D+00, 41.0D+00, 51.0D+00, 61.0D+00, 71.0D+00, &
              22.0D+00, 32.0D+00, 42.0D+00, 52.0D+00, 62.0D+00, 72.0D+00, &
                        33.0D+00, 43.0D+00, 53.0D+00, 63.0D+00, 73.0D+00, &
                                  44.0D+00, 54.0D+00, 64.0D+00, 74.0D+00, &
                                            55.0D+00, 65.0D+00, 75.0D+00, &
                                                      66.0D+00, 76.0D+00, &
                                                                77.0D+00 /)
  real ( kind = 8 ), dimension(18) :: a2 = (/ &
    11.0D+00, 21.0D+00, 31.0D+00, 41.0D+00, 51.0D+00, 61.0D+00, 71.0D+00, &
              22.0D+00, 32.0D+00, 42.0D+00, 52.0D+00, 62.0D+00, 72.0D+00, &
                        33.0D+00, 43.0D+00, 53.0D+00, 63.0D+00, 73.0D+00 /)
  real ( kind = 8 ), dimension(10) :: a3 = (/ &
    11.0D+00, 21.0D+00, 31.0D+00, 41.0D+00, &
              22.0D+00, 32.0D+00, 42.0D+00, &
                        33.0D+00, 43.0D+00, &
                                  44.0D+00 /)
  integer ( kind = 4 ), parameter :: m1 = 7
  integer ( kind = 4 ), parameter :: m2 = 7
  integer ( kind = 4 ), parameter :: m3 = 4
  integer ( kind = 4 ), parameter :: n1 = 7
  integer ( kind = 4 ), parameter :: n2 = 3
  integer ( kind = 4 ), parameter :: n3 = 7

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_L_PRINT_TEST'
  write ( *, '(a)' ) '  R8MAT_L_PRINT prints a lower triangular matrix'
  write ( *, '(a)' ) '  stored compactly.  Only the (possibly) nonzero '
  write ( *, '(a)' ) '  elements are printed.'

  call r8mat_l_print ( m1, n1, a1, '  A 7 by 7 matrix.' )

  call r8mat_l_print ( m2, n2, a2, '  A 7 by 3 matrix.' )

  call r8mat_l_print ( m3, n3, a3, '  A 4 by 7 matrix.' )

  return
end
subroutine r8mat_l_solve_test ( )

!*****************************************************************************80
!
!! R8MAT_L_SOLVE_TEST tests R8MAT_L_SOLVE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2016
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
  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
    1.0D+00, 2.0D+00, 4.0D+00,  7.0D+00, &
    0.0D+00, 3.0D+00, 5.0D+00,  8.0D+00, &
    0.0D+00, 0.0D+00, 6.0D+00,  9.0D+00, &
    0.0D+00, 0.0D+00, 0.0D+00, 10.0D+00 /), (/ n, n /) )
  real ( kind = 8 ), dimension ( n ) :: b = (/ &
    1.0D+00, 8.0D+00, 32.0D+00, 90.0D+00 /)
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) r8vec_norm
  real ( kind = 8 ) rnorm
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_L_SOLVE_TEST'
  write ( *, '(a)' ) '  R8MAT_L_SOLVE solves a lower triangular system.'

  call r8mat_print ( n, n, a, '  Input matrix A:' )

  call r8vec_print ( n, b, '  Right hand side b:' )

  call r8mat_l_solve ( n, a, b, x )

  call r8vec_print ( n, x, '  Computed solution x:' )

  r(1:n) = matmul ( a(1:n,1:n), x(1:n) ) - b(1:n)

  rnorm = r8vec_norm ( n, r )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Norm of A*x-b = ', rnorm

  return
end
subroutine r8mat_l1_inverse_test ( )

!*****************************************************************************80
!
!! R8MAT_L1_INVERSE_TESt tests R8MAT_L1_INVERSE.
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
  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
     1.0D+00, 2.0D+00, 0.0D+00, 5.0D+00, 0.0D+00, 75.0D+00, &
     0.0D+00, 1.0D+00, 0.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, &
     0.0D+00, 0.0D+00, 1.0D+00, 3.0D+00, 0.0D+00,  0.0D+00, &
     0.0D+00, 0.0D+00, 0.0D+00, 1.0D+00, 0.0D+00,  6.0D+00, &
     0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 1.0D+00,  4.0D+00, &
     0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00,  1.0D+00 /), (/ n, n /) )
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) c(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_L1_INVERSE_TEST'
  write ( *, '(a)' ) '  R8MAT_L1_INVERSE inverts a unit lower triangular matrix.'

  call r8mat_print ( n, n, a, '  Matrix A to be inverted:' )

  call r8mat_l1_inverse ( n, a, b )

  call r8mat_print ( n, n, b, '  Inverse matrix B:' )

  c(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )

  call r8mat_print ( n, n, c, '  Product C = A * B:' )

  return
end
subroutine r8mat_lt_solve_test ( )

!*****************************************************************************80
!
!! R8MAT_LT_SOLVE_TEST tests R8MAT_LT_SOLVE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2015
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
  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
    1.0D+00, 2.0D+00, 4.0D+00,  7.0D+00, &
    0.0D+00, 3.0D+00, 5.0D+00,  8.0D+00, &
    0.0D+00, 0.0D+00, 6.0D+00,  9.0D+00, &
    0.0D+00, 0.0D+00, 0.0D+00, 10.0D+00 /), (/ n, n /) )
  real ( kind = 8 ), dimension ( n ) :: b = (/ &
    45.0D+00, 53.0D+00, 54.0D+00, 40.0D+00 /)
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) r8vec_norm
  real ( kind = 8 ) rnorm
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_LT_SOLVE_TEST'
  write ( *, '(a)' ) '  R8MAT_LT_SOLVE solves a transposed lower triangular system.'

  call r8mat_print ( n, n, a, '  Input matrix A:' )

  call r8vec_print ( n, b, '  Right hand side b:' )

  call r8mat_lt_solve ( n, a, b, x )

  call r8vec_print ( n, x, '  Computed solution x:' )

  r(1:n) = matmul ( transpose ( a(1:n,1:n) ), x(1:n) ) - b(1:n)

  rnorm = r8vec_norm ( n, r )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Norm of A''*x-b = ', rnorm

  return
end
subroutine r8mat_lu_test ( )

!*****************************************************************************80
!
!! R8MAT_LU_TEST tests R8MAT_LU.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) l(m,m)
  real ( kind = 8 ) p(m,m)
  real ( kind = 8 ) plu(m,n)
  real ( kind = 8 ) u(m,n)
  real ( kind = 8 ), dimension ( n ) :: x = (/ &
    1.0D+00, 10.0D+00, 4.0D+00, 2.0D+00, 3.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_LU_TEST'
  write ( *, '(a)' ) '  R8MAT_LU computes the LU factors of a matrix.'

  call r8mat_vand2 ( n, x, a )

  call r8mat_print ( m, n, a, '  Matrix to be factored:' )

  call r8mat_lu ( m, n, a, l, p, u )

  call r8mat_print ( m, m, p, '  P factor:' )

  call r8mat_print ( m, m, l, '  L factor:' )

  call r8mat_print ( m, n, u, '  U factor:' )

  plu(1:m,1:n) = matmul ( transpose ( p(1:m,1:m) ), &
    matmul ( l(1:m,1:m), u(1:m,1:n) ) )

  call r8mat_print ( m, n, plu, '  P''*L*U:' )

  return
end
subroutine r8mat_max_test ( )

!*****************************************************************************80
!
!! R8MAT_MAX_TEST tests R8MAT_MAX.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ), parameter :: b = 0.0D+00
  real ( kind = 8 ), parameter :: c = 10.0D+00
  real ( kind = 8 ) r8mat_max
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_MAX_TEST'
  write ( *, '(a)' ) '  For an R8MAT,'
  write ( *, '(a)' ) '  R8MAT_MAX computes the maximum value;'

  seed = 123456789

  call r8mat_uniform_ab ( m, n, b, c, seed, a )

  call r8mat_print ( m, n, a, '  Random array:' )

  t = r8mat_max ( m, n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Maximum value = ', t

  return
end
subroutine r8mat_max_index_test ( )

!*****************************************************************************80
!
!! R8MAT_MAX_INDEX_TEST tests R8MAT_MAX_INDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ), parameter :: b = 0.0D+00
  real ( kind = 8 ), parameter :: c = 10.0D+00
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_MAX_INDEX_TEST'
  write ( *, '(a)' ) '  For an R8MAT:'
  write ( *, '(a)' ) '  R8MAT_MAX_INDEX locates the maximum entry;'

  seed = 123456789

  call r8mat_uniform_ab ( m, n, b, c, seed, a )

  call r8mat_print ( m, n, a, '  Random array:' )

  call r8mat_max_index ( m, n, a, i, j )

  write ( *, '(a)' ) ''
  write ( *, '(a,2i8)' ) '  Maximum I,J indices            ', i, j
 
  return
end
subroutine r8mat_maxcol_minrow_test ( )

!*****************************************************************************80
!
!! R8MAT_MAXCOL_MINROW_TEST tests R8MAT_MAXCOL_MINROW.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ), parameter :: b = 0.0D+00
  real ( kind = 8 ), parameter :: c = 10.0D+00
  real ( kind = 8 ) r8mat_maxcol_minrow
  integer ( kind = 4 ) seed
  real ( kind = 8 ) temp

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_MAXCOL_MINROW_TEST'
  write ( *, '(a)' ) '  R8MAT_MAXCOL_MINROW computes the maximum over'
  write ( *, '(a)' ) '  columns of the mininum over rows;'

  seed = 123456789

  call r8mat_uniform_ab ( m, n, b, c, seed, a )

  call r8mat_print ( m, n, a, '  Random array:' )

  temp = r8mat_maxcol_minrow ( m, n, a )

  write ( *, '(a,2g14.6)' ) '  MAXCOL_MINROW = ', temp

  return
end
subroutine r8mat_maxrow_mincol_test ( )

!*****************************************************************************80
!
!! R8MAT_MAXROW_MINCOL_TEST tests R8MAT_MAXROW_MINCOL.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ), parameter :: b = 0.0D+00
  real ( kind = 8 ), parameter :: c = 10.0D+00
  real ( kind = 8 ) r8mat_maxrow_mincol
  integer ( kind = 4 ) seed
  real ( kind = 8 ) temp

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_MAXROW_MINCOL_TEST'
  write ( *, '(a)' ) '  R8MAT_MAXROW_MINCOL computes the maximum over'
  write ( *, '(a)' ) '  rows of the mininum over columns;'

  seed = 123456789

  call r8mat_uniform_ab ( m, n, b, c, seed, a )

  call r8mat_print ( m, n, a, '  Random array:' )

  temp = r8mat_maxrow_mincol ( m, n, a )

  write ( *, '(a,2g14.6)' ) '  MAXROW_MINCOL = ', temp

  return
end
subroutine r8mat_min_test ( )

!*****************************************************************************80
!
!! R8MAT_MIN_TEST tests R8MAT_MIN.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ), parameter :: b = 0.0D+00
  real ( kind = 8 ), parameter :: c = 10.0D+00
  real ( kind = 8 ) r8mat_min
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_MIN_TEST'
  write ( *, '(a)' ) '  R8MAT_MIN computes the minimum value of an R8MAT'

  seed = 123456789

  call r8mat_uniform_ab ( m, n, b, c, seed, a )

  call r8mat_print ( m, n, a, '  Random array:' )

  t = r8mat_min ( m, n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Minimum value = ', t

  return
end
subroutine r8mat_min_index_test( )

!*****************************************************************************80
!
!! R8MAT_MIN_INDEX_TEST tests R8MAT_MIN_INDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ), parameter :: b = 0.0D+00
  real ( kind = 8 ), parameter :: c = 10.0D+00
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_MIN_INDEX_TEST'
  write ( *, '(a)' ) '  For an R8MAT:'
  write ( *, '(a)' ) '  R8MAT_MIN_INDEX locates the minimum entry;'

  seed = 123456789

  call r8mat_uniform_ab ( m, n, b, c, seed, a )

  call r8mat_print ( m, n, a, '  Random array:' )

  call r8mat_max_index ( m, n, a, i, j )

  write ( *, '(a)' ) ''
  write ( *, '(a,2i8)' ) '  Minimum I,J indices            ', i, j

  return
end
subroutine r8mat_mincol_maxrow_test ( )

!*****************************************************************************80
!
!! R8MAT_MINCOL_MAXROW_TEST tests R8MAT_MINCOL_MAXROW.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ), parameter :: b = 0.0D+00
  real ( kind = 8 ), parameter :: c = 10.0D+00
  real ( kind = 8 ) r8mat_mincol_maxrow
  integer ( kind = 4 ) seed
  real ( kind = 8 ) temp

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_MINCOL_MAXROW_TEST'
  write ( *, '(a)' ) '  R8MAT_MINCOL_MAXROW computes the minimum over'
  write ( *, '(a)' ) '  columns of the maxinum over rows;'

  seed = 123456789

  call r8mat_uniform_ab ( m, n, b, c, seed, a )

  call r8mat_print ( m, n, a, '  Random array:' )

  temp = r8mat_mincol_maxrow ( m, n, a )

  write ( *, '(a,2g14.6)' ) '  MINCOL_MAXROW = ', temp

  return
end
subroutine r8mat_minrow_maxcol_test ( )

!*****************************************************************************80
!
!! R8MAT_MINROW_MAXCOL_TEST tests R8MAT_MINROW_MAXCOL.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ), parameter :: b = 0.0D+00
  real ( kind = 8 ), parameter :: c = 10.0D+00
  real ( kind = 8 ) r8mat_minrow_maxcol
  integer ( kind = 4 ) seed
  real ( kind = 8 ) temp

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_MINROW_MAXCOL_TEST'
  write ( *, '(a)' ) '  R8MAT_MINROW_MAXCOL computes the minimum over'
  write ( *, '(a)' ) '  rows of the maxinum over columns;'
  write ( *, '(a)' ) ''

  seed = 123456789

  call r8mat_uniform_ab ( m, n, b, c, seed, a )

  call r8mat_print ( m, n, a, '  Random array:' )

  temp = r8mat_minrow_maxcol ( m, n, a )

  write ( *, '(a,2g14.6)' ) '  MINROW_MAXCOL = ', temp

  return
end
subroutine r8mat_mm_test ( )

!*****************************************************************************80
!
!! R8MAT_MM_TEST tests R8MAT_MM.
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

  integer ( kind = 4 ), parameter :: n1 = 4
  integer ( kind = 4 ), parameter :: n2 = 3
  integer ( kind = 4 ), parameter :: n3 = n1

  real ( kind = 8 ) a(n1,n2)
  real ( kind = 8 ) b(n2,n3)
  real ( kind = 8 ) c(n1,n3)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_MM_TEST'
  write ( *, '(a)' ) '  R8MAT_MM computes a matrix-matrix product C = A * B.'

  do i = 1, n1
    do j = 1, n2

      if ( j == 1 ) then
        a(i,j) = 1.0D+00
      else if ( i == 1 ) then
        a(i,j) = 0.0D+00
      else
        a(i,j) = a(i-1,j-1) + a(i-1,j)
      end if

    end do
  end do

  b(1:n2,1:n3) = transpose ( a(1:n1,1:n2) )

  call r8mat_mm ( n1, n2, n3, a, b, c )

  call r8mat_print ( n1, n2, a, '  A:' )
  call r8mat_print ( n2, n3, b, '  B:' )
  call r8mat_print ( n1, n3, c, '  C = A*B:' )

  return
end
subroutine r8mat_mv_test ( )

!*****************************************************************************80
!
!! R8MAT_MV_TEST tests R8MAT_MV.
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

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 2

  real ( kind = 8 ), dimension ( m, n ) :: a = reshape ( (/ &
    1.0, 2.0, 3.0, 4.0, &
    1.0, 1.0, 1.0, 1.0 /), (/ m, n /) )
  real ( kind = 8 ) b(m)
  real ( kind = 8 ), dimension ( n ) :: x = (/ 1.0, 2.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_MV_TEST'
  write ( *, '(a)' ) '  R8MAT_MV computes a matrix-vector product b = A * x;'

  call r8mat_mv ( m, n, a, x, b )

  call r8mat_print ( m, n, a, '  A:' )
  call r8vec_print ( n, x, '  X:' )
  call r8vec_print ( m, b, '  B = A*X:' )

  return
end
subroutine r8mat_nint_test ( )

!*****************************************************************************80
!
!! R8MAT_NINT_TEST tests R8MAT_NINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_NINT_TEST'
  write ( *, '(a)' ) '  R8MAT_NINT rounds an R8MAT.'

  x1 = -5.0D+00
  x2 = +5.0D+00
  seed = 123456789
  call r8mat_uniform_ab ( m, n, x1, x2, seed, a )
  call r8mat_print ( m, n, a, '  Matrix A:' )
  call r8mat_nint ( m, n, a )
  call r8mat_print ( m, n, a, '  Rounded matrix A:' )

  return
end
subroutine r8mat_nonzeros_test ( )

!*****************************************************************************80
!
!! R8MAT_NONZEROS_TEST tests R8MAT_NONZEROS.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) c1
  integer ( kind = 4 ) c2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) r8mat_nonzeros

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_NONZEROS_TEST'
  write ( *, '(a)' ) '  R8MAT_NONZEROS counts nonzeros in an R8MAT.'

  c1 = 0
  do i = 1, m
    do j = 1, n
      if ( mod ( i, 2 ) == 0 .and. mod (  j, 2 ) == 0 ) then
        a(i,j) = 1.0D+00
        c1 = c1 + 1
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  call r8mat_print ( m, n, a, '  Matrix A:' )

  c2 = r8mat_nonzeros ( m, n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Expected nonzeros = ', c1
  write ( *, '(a,i4)' ) '  Computed nonzeros = ', c2

  return
end
subroutine r8mat_norm_fro_test ( )

!*****************************************************************************80
!
!! R8MAT_NORM_FRO_TEST tests R8MAT_NORM_FRO.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8mat_norm_fro
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_NORM_FRO_TEST'
  write ( *, '(a)' ) '  R8MAT_NORM_FRO computes a Frobenius norm of an R8MAT;'

  t1 = 0.0D+00
  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      a(i,j) = real ( k, kind = 8 )
      t1 = t1 + real ( k * k, kind = 8 )
    end do
  end do

  t1 = sqrt ( t1 )

  call r8mat_print ( m, n, a, '  A:' )

  t2 = r8mat_norm_fro ( m, n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Expected norm = ', t1
  write ( *, '(a,g14.6)' ) '  Computed norm = ', t2

  return
end
subroutine r8mat_norm_fro_affine_test ( )

!*****************************************************************************80
!
!! R8MAT_NORM_FRO_AFFINE_TEST tests R8MAT_NORM_FRO_AFFINE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) b(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_norm_fro_affine
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_NORM_FRO_AFFINE_TEST'
  write ( *, '(a)' ) '  R8MAT_NORM_FRO_AFFINE computes the Frobenius norm'
  write ( *, '(a)' ) '  of the difference of two R8MAT''s.'

  seed = 123456789
  call r8mat_uniform_01 ( m, n, seed, a )
  call r8mat_uniform_01 ( m, n, seed, b )

  t1 = 0.0D+00
  do i = 1, m
    do j = 1, n
      t1 = t1 + ( a(i,j) - b(i,j) ) ** 2
    end do
  end do
  t1 = sqrt ( t1 )

  t2 = r8mat_norm_fro_affine ( m, n, a, b )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Expected norm = ', t1
  write ( *, '(a,g14.6)' ) '  Computed norm = ', t2

  return
end
subroutine r8mat_norm_l1_test ( )

!*****************************************************************************80
!
!! R8MAT_NORM_L1_TEST tests R8MAT_NORM_L1.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_norm_l1
  integer seed
  real ( kind = 8 ) t
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_NORM_L1_TEST'
  write ( *, '(a)' ) '  R8MAT_NORM_L1 computes the L1 norm of an R8MAT;'

  x1 = -5.0D+00
  x2 = +5.0D+00
  seed = 123456789
  call r8mat_uniform_ab ( m, n, x1, x2, seed, a )
  call r8mat_nint ( m, n, a )

  call r8mat_print ( m, n, a, '  A:' )

  t = r8mat_norm_l1 ( m, n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Computed norm = ', t

  return
end
subroutine r8mat_nullspace_test ( )

!*****************************************************************************80
!
!! R8MAT_NULLSPACE_TEST tests R8MAT_NULLSPACE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 7

  real ( kind = 8 ), dimension ( m, n ) :: a = reshape ( (/ &
    1.0, -2.0, 3.0, -1.0, &
    3.0, -6.0, 9.0, -3.0, &
    0.0,  0.0, 0.0,  0.0, &
    2.0, -2.0, 0.0,  1.0, &
    6.0, -8.0, 6.0,  0.0, &
    3.0,  3.0, 6.0,  9.0, &
    1.0,  1.0, 2.0,  3.0 /), (/ m, n /) )
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: ax
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: nullspace
  integer ( kind = 4 ) nullspace_size

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_NULLSPACE_TEST'
  write ( *, '(a)' ) '  R8MAT_NULLSPACE computes the nullspace of a matrix.'

  call r8mat_print ( m, n, a, '  Input A:' )

  call r8mat_nullspace_size ( m, n, a, nullspace_size )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Nullspace size is ', nullspace_size

  allocate ( nullspace(1:n,1:nullspace_size) )

  call r8mat_nullspace ( m, n, a, nullspace_size, nullspace )

  call r8mat_print ( n, nullspace_size, nullspace, '  Nullspace vectors:' )

  allocate ( ax(1:m,1:nullspace_size) )

  ax = matmul ( a, nullspace )

  call r8mat_print ( m, nullspace_size, ax, &
    '  Product A * Nullspace vectors:' )

  deallocate ( ax )
  deallocate ( nullspace )

  return
end
subroutine r8mat_nullspace_size_test ( )

!*****************************************************************************80
!
!! R8MAT_NULLSPACE_SIZE_TEST tests R8MAT_NULLSPACE_SIZE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 7

  real ( kind = 8 ), dimension ( m, n ) :: a = reshape ( (/ &
    1.0, -2.0, 3.0, -1.0, &
    3.0, -6.0, 9.0, -3.0, &
    0.0,  0.0, 0.0,  0.0, &
    2.0, -2.0, 0.0,  1.0, &
    6.0, -8.0, 6.0,  0.0, &
    3.0,  3.0, 6.0,  9.0, &
    1.0,  1.0, 2.0,  3.0 /), (/ m, n /) )
  integer ( kind = 4 ) nullspace_size

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_NULLSPACE_SIZE_TEST'
  write ( *, '(a)' ) &
    '  R8MAT_NULLSPACE_SIZE computes the size of the nullspace of a matrix.'

  call r8mat_print ( m, n, a, '  Input A:' )

  call r8mat_nullspace_size ( m, n, a, nullspace_size )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Nullspace size is ', nullspace_size

  return
end
subroutine r8mat_orth_uniform_test ( )

!*****************************************************************************80
!
!! R8MAT_ORTH_UNIFORM_TEST tests R8MAT_ORTH_UNIFORM.
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) ata(n,n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_ORTH_UNIFORM_TEST'
  write ( *, '(a)' ) '  R8MAT_ORTH_UNIFORM computes a random orthogonal matrix.'

  seed = 123456789

  call r8mat_orth_uniform ( n, seed, a )

  call r8mat_print ( n, n, a, '  Random orthogonal matrix A' )

  ata(1:n,1:n) = matmul ( transpose ( a(1:n,1:n) ), a(1:n,1:n) )

  call r8mat_print ( n, n, ata, '  A''*A should be identity:' )

  return
end
subroutine r8mat_plot_test ( )

!*****************************************************************************80
!
!! R8MAT_PLOT_TEST tests R8MAT_PLOT.
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

  integer ( kind = 4 ), parameter :: m = 10
  integer ( kind = 4 ), parameter :: n = 100

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) im1
  integer ( kind = 4 ) ip1

  a(1:m,1:n) = 0.0D+00

  do i = 1, m

    a(i,i) = -2.0D+00

    if ( i+1 <= n ) then
      ip1 = i+1
    else
      ip1 = 1
    end if

    a(i,ip1) = 1.0D+00

    if ( 1 <= i-1 ) then
      im1 = i-1
    else
      im1 = n
    end if

    a(i,im1) = 1.0D+00

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_PLOT_TEST'
  write ( *, '(a)' ) '  R8MAT_PLOT prints a symbolic picture of a matrix.'
  write ( *, '(a)' ) '  Typically, '
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    "-" for negative, '
  write ( *, '(a)' ) '    " " for zero, and'
  write ( *, '(a)' ) '    "+" for positive entries'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  or'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    "X" for nonzero and, '
  write ( *, '(a)' ) '    " " for zero.'
  write ( *, '(a)' ) ''

  call r8mat_plot ( m, n, a, '  A plot of the matrix:' )

  return
end
subroutine r8mat_power_method_test ( )

!*****************************************************************************80
!
!! R8MAT_POWER_METHOD_TEST tests R8MAT_POWER_METHOD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) av(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r
  real ( kind = 8 ) v(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_POWER_METHOD_TEST'
  write ( *, '(a)' ) '  R8MAT_POWER_METHOD applies the power method'
  write ( *, '(a)' ) '  to a matrix.'

  do i = 1, n
    do j = 1, n
      if ( j == i - 1 .or. j == i + 1 ) then
        a(i,j) = -1.0D+00
      else if ( j == i ) then
        a(i,j) = 2.0D+00
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  v(1:n) = 0.0D+00

  call r8mat_power_method ( n, a, r, v )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Estimated eigenvalue = ', r

  call r8vec_print ( n, v, '  Estimated eigenvector V:' )

  av(1:n) = matmul ( a(1:n,1:n), v(1:n) )

  call r8vec_print ( n, av, '  Value of A*V:' )

  return
end
subroutine r8mat_print_test ( )

!*****************************************************************************80
!
!! R8MAT_PRINT_TEST tests R8MAT_PRINT.
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_PRINT_TEST'
  write ( *, '(a)' ) '  R8MAT_PRINT prints an R8MAT.'

  do j = 1, n
    do i = 1, m
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call r8mat_print ( m, n, a, '  The R8MAT:' )

  return
end
subroutine r8mat_print_some_test ( )

!*****************************************************************************80
!
!! R8MAT_PRINT_SOME_TEST tests R8MAT_PRINT_SOME.
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8MAT_PRINT_SOME prints some of an R8MAT.'

  do j = 1, n
    do i = 1, m
      a(i,j) = real ( 10 * i + j, kind = 8 )
    end do
  end do

  call r8mat_print_some ( m, n, a, 2, 1, 4, 2, &
    '  The R8MAT, rows 2:4, cols 1:2:' )

  return
end
subroutine r8mat_product_elementwise_test ( )

!*****************************************************************************80
!
!! R8MAT_PRODUCT_ELEMENTWISE_TEST tests R8MAT_PRODUCT_ELEMENTWISE.
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

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) b(m,n)
  real ( kind = 8 ) r8mat_product_elementwise
  real ( kind = 8 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_PRODUCT_ELEMENTWISE_TEST'
  write ( *, '(a)' ) '  R8MAT_PRODUCT_ELEMENTWISE computes the elementwise\'
  write ( *, '(a)' ) '  product of two I4MATs.'

  a = reshape ( (/ 1.0, 4.0, 2.0, 5.0, 3.0, 6.0 /), (/ m, n /) ) 
  b = reshape ( (/ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 /), (/ m, n /) )

  call r8mat_print ( m, n, a, '  A:' )
  call r8mat_print ( m, n, b, '  B:' )

  t = r8mat_product_elementwise ( m, n, a, b )
 
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Elementwise product = ', t

  return
end
subroutine r8mat_ref_test ( )

!*****************************************************************************80
!
!! R8MAT_REF_TEST tests R8MAT_REF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 7

  real ( kind = 8 ), dimension ( m, n ) :: a = reshape ( (/ &
    1.0, -2.0, 3.0, -1.0, &
    3.0, -6.0, 9.0, -3.0, &
    0.0,  0.0, 0.0,  0.0, &
    2.0, -2.0, 0.0,  1.0, &
    6.0, -8.0, 6.0,  0.0, &
    3.0,  3.0, 6.0,  9.0, &
    1.0,  1.0, 2.0,  3.0 /), (/ m, n /) )
  real ( kind = 8 ) det

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_REF_TEST'
  write ( *, '(a)' ) '  R8MAT_REF computes the row echelon form of a matrix.'

  call r8mat_print ( m, n, a, '  Input A:' )

  call r8mat_ref ( m, n, a, det )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Pseudo-determinant = ', det

  call r8mat_print ( m, n, a, '  REF form:' )

  return
end
subroutine r8mat_rref_test ( )

!*****************************************************************************80
!
!! R8MAT_RREF_TEST tests R8MAT_RREF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 7

  real ( kind = 8 ), dimension ( m, n ) :: a = reshape ( (/ &
    1.0, -2.0, 3.0, -1.0, &
    3.0, -6.0, 9.0, -3.0, &
    0.0,  0.0, 0.0,  0.0, &
    2.0, -2.0, 0.0,  1.0, &
    6.0, -8.0, 6.0,  0.0, &
    3.0,  3.0, 6.0,  9.0, &
    1.0,  1.0, 2.0,  3.0 /), (/ m, n /) )
  real ( kind = 8 ) det

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_RREF_TEST'
  write ( *, '(a)' ) '  R8MAT_RREF computes the reduced row echelon form of a matrix.'

  call r8mat_print ( m, n, a, '  Input A:' )

  call r8mat_rref ( m, n, a, det )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Pseudo-determinant = ', det

  call r8mat_print ( m, n, a, '  RREF form:' )

  return
end
subroutine r8mat_scale_01_test ( )

!*****************************************************************************80
!
!! R8MAT_SCALE_01_TEST tests R8MAT_SCALE_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 October 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 10
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mu(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma(n)
  real ( kind = 8 ) x(m,n)
  real ( kind = 8 ) xmax(n)
  real ( kind = 8 ) xmin(n)
  real ( kind = 8 ) xs(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_SCALE_01_TEST'
  write ( *, '(a)' ) '  R8MAT_SCALE_01 shifts and scales an R8MAT so that'
  write ( *, '(a)' ) '  every column has min 0 and max 1.'

  a = -5.0D+00
  b = 15.0D+00
  seed = 123456789

  call r8mat_uniform_ab ( m, n, a, b, seed, x )
 
  call r8mat_print ( m, n, x, '  Matrix X:' )
  call r8mat_mean_columns ( m, n, x, mu )
  call r8mat_std_columns ( m, n, x, sigma )
  call r8mat_max_columns ( m, n, x, xmax )
  call r8mat_min_columns ( m, n, x, xmin )
  write ( *, '(a)' ) ''
  write ( *, '(a,3g14.6)' ) '  mean(X) = ', mu(1:n)
  write ( *, '(a,3g14.6)' ) '  std(X)  = ', sigma(1:n)
  write ( *, '(a,3g14.6)' ) '  max(X)  = ', xmax(1:n)
  write ( *, '(a,3g14.6)' ) '  min(X)  = ', xmin(1:n)

  call r8mat_scale_01 ( m, n, x, xs )

  call r8mat_print ( m, n, xs, '  Matrix XS:' )
  call r8mat_mean_columns ( m, n, xs, mu )
  call r8mat_std_columns ( m, n, xs, sigma )
  call r8mat_max_columns ( m, n, xs, xmax )
  call r8mat_min_columns ( m, n, xs, xmin )
  write ( *, '(a)' ) ''
  write ( *, '(a,3g14.6)' ) '  mean(XS) = ', mu(1:n)
  write ( *, '(a,3g14.6)' ) '  std(XS)  = ', sigma(1:n)
  write ( *, '(a,3g14.6)' ) '  max(XS)  = ', xmax(1:n)
  write ( *, '(a,3g14.6)' ) '  min(XS)  = ', xmin(1:n)

  return
end
subroutine r8mat_scale_ab_test ( )

!*****************************************************************************80
!
!! R8MAT_SCALE_AB_TEST tests R8MAT_SCALE_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 October 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 10
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mu(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma(n)
  real ( kind = 8 ) x(m,n)
  real ( kind = 8 ) xmax(n)
  real ( kind = 8 ) xmin(n)
  real ( kind = 8 ) xs(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_SCALE_AB_TEST'
  write ( *, '(a)' ) '  R8MAT_SCALE_AB shifts and scales an R8MAT so that'
  write ( *, '(a)' ) '  every column has min A and max B.'

  a = -5.0D+00
  b = 15.0D+00
  seed = 123456789

  call r8mat_uniform_ab ( m, n, a, b, seed, x )
 
  call r8mat_print ( m, n, x, '  Matrix X:' )
  call r8mat_mean_columns ( m, n, x, mu )
  call r8mat_std_columns ( m, n, x, sigma )
  call r8mat_max_columns ( m, n, x, xmax )
  call r8mat_min_columns ( m, n, x, xmin )
  write ( *, '(a)' ) ''
  write ( *, '(a,3g14.6)' ) '  mean(X) = ', mu(1:n)
  write ( *, '(a,3g14.6)' ) '  std(X)  = ', sigma(1:n)
  write ( *, '(a,3g14.6)' ) '  max(X)  = ', xmax(1:n)
  write ( *, '(a,3g14.6)' ) '  min(X)  = ', xmin(1:n)

  a = -1.0D+00
  b = +1.0D+00
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  New scale interval = [ ', a, ',', b, ']'
  call r8mat_scale_ab ( m, n, x, a, b, xs )

  call r8mat_print ( m, n, xs, '  Matrix XS:' )
  call r8mat_mean_columns ( m, n, xs, mu )
  call r8mat_std_columns ( m, n, xs, sigma )
  call r8mat_max_columns ( m, n, xs, xmax )
  call r8mat_min_columns ( m, n, xs, xmin )
  write ( *, '(a)' ) ''
  write ( *, '(a,3g14.6)' ) '  mean(XS) = ', mu(1:n)
  write ( *, '(a,3g14.6)' ) '  std(XS)  = ', sigma(1:n)
  write ( *, '(a,3g14.6)' ) '  max(XS)  = ', xmax(1:n)
  write ( *, '(a,3g14.6)' ) '  min(XS)  = ', xmin(1:n)

  return
end
subroutine r8mat_solve_test ( )

!*****************************************************************************80
!
!! R8MAT_SOLVE_TEST tests R8MAT_SOLVE.
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

  integer ( kind = 4 ), parameter :: n = 3
  integer ( kind = 4 ), parameter :: rhs_num = 2
!
!  Each row of this definition is a COLUMN of the matrix.
!
  real ( kind = 8 ), dimension (n,n+rhs_num) :: a = reshape ( &
    (/ 1.0D+00,  4.0D+00,  7.0D+00, &
       2.0D+00,  5.0D+00,  8.0D+00, &
       3.0D+00,  6.0D+00,  0.0D+00, &
      14.0D+00, 32.0D+00, 23.0D+00, &
       7.0D+00, 16.0D+00,  7.0D+00 /), &
    (/ n, n+rhs_num /) )
  integer ( kind = 4 ) info

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_SOLVE_TEST'
  write ( *, '(a)' ) '  R8MAT_SOLVE solves linear systems.'
!
!  Print out the matrix to be inverted.
!
  call r8mat_print ( n, n+rhs_num, a, '  The linear system:' )
!
!  Solve the systems.
!
  call r8mat_solve ( n, rhs_num, a, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  The input matrix was singular.'
    write ( *, '(a)' ) '  The solutions could not be computed.'
    write ( *, '(a)' ) ''
    return
  end if

  call r8mat_print ( n, rhs_num, a(1:n,n+1:n+rhs_num), &
    '  The computed solutions' )

  return
end
subroutine r8mat_solve_2d_test ( )

!*****************************************************************************80
!
!! R8MAT_SOLVE_2D_TEST tests R8MAT_SOLVE_2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 November 2005
!
!  Author:
!
!    John Burkardt
!
  integer ( kind = 4 ), parameter :: n = 2

  real ( kind = 8 ), dimension (n,n) :: a
  real ( kind = 8 ), dimension ( n ) :: b
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 5
  real ( kind = 8 ), dimension ( n ) :: x
  real ( kind = 8 ), dimension ( n ) :: x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_SOLVE_2D_TEST'
  write ( *, '(a)' ) '  R8MAT_SOLVE_2D solves 2D linear systems.'

  seed = 123456789

  do test = 1, test_num

    call r8mat_uniform_01 ( n, n, seed, a )
    call r8vec_uniform_01 ( n, seed, x )
    b(1:n) = matmul ( a(1:n,1:n), x(1:n) )

    call r8mat_solve_2d ( a, b, det, x2 )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Solution / Computed:'
    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,g14.6,2x,g14.6)' ) x(i), x2(i)
    end do

  end do

  return
end
subroutine r8mat_solve_3d_test ( )

!*****************************************************************************80
!
!! R8MAT_SOLVE_3D_TEST tests R8MAT_SOLVE_3D.
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
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ), dimension (n,n) :: a
  real ( kind = 8 ), dimension ( n ) :: b
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 5
  real ( kind = 8 ), dimension ( n ) :: x
  real ( kind = 8 ), dimension ( n ) :: x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_SOLVE_3D_TEST'
  write ( *, '(a)' ) '  R8MAT_SOLVE_3D solves 3D linear systems.'

  seed = 123456789

  do test = 1, test_num

    call r8mat_uniform_01 ( n, n, seed, a )
    call r8vec_uniform_01 ( n, seed, x )
    b(1:n) = matmul ( a(1:n,1:n), x(1:n) )

    call r8mat_solve_3d ( a, b, det, x2 )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Solution / Computed:'
    write ( *, '(a)' ) ''

    do i = 1, n
      write ( *, '(2x,g14.6,2x,g14.6)' ) x(i), x2(i)
    end do

  end do

  return
end
subroutine r8mat_solve2_test ( )

!*****************************************************************************80
!
!! R8MAT_SOLVE2_TEST tests R8MAT_SOLVE2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 4

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  real ( kind = 8 ), dimension ( 2, 2 ) :: a1 = reshape ( (/ &
    1.0D+00, 3.0D+00, &
    2.0D+00, 4.0D+00 /), (/ 2, 2 /) )
  real ( kind = 8 ), dimension ( 3, 3 ) :: a2 = reshape ( (/ &
    2.0D+00, 1.0D+00, 1.0D+00, &
    1.0D+00, 1.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, 1.0D+00 /), (/ 3, 3 /) )
  real ( kind = 8 ), dimension ( 4, 4 ) :: a3 = reshape ( (/ &
    1.0D+00, 2.0D+00, 1.0D+00, 3.0D+00, &
    0.0D+00, 1.0D+00, 2.0D+00, 1.0D+00, &
    0.0D+00, 0.0D+00, 3.0D+00, 2.0D+00, &
    1.0D+00, 3.0D+00, 0.0D+00, 1.0D+00 /), (/ 4, 4 /) )
  real ( kind = 8 ), dimension ( 3, 3 ) :: a4 = reshape ( (/ &
    2.0D+00, 1.0D+00, 3.0D+00, &
    4.0D+00, 2.0D+00, 6.0D+00, &
    1.0D+00, 4.0D+00, 5.0D+00 /), (/ 3, 3 /) )
  real ( kind = 8 ), allocatable, dimension ( : ) :: b
  real ( kind = 8 ), dimension ( 2 ) :: b1 = (/ &
    5.0D+00, 11.0D+00 /)
  real ( kind = 8 ), dimension ( 3 ) :: b2 = (/ &
    4.0D+00, 2.0D+00, 2.0D+00 /)
  real ( kind = 8 ), dimension ( 4 ) :: b3 = (/ &
    5.0D+00, 11.0D+00, 16.0D+00, 15.0D+00 /)
  real ( kind = 8 ), dimension ( 3 ) :: b4 = (/ &
    13.0D+00, 17.0D+00, 20.0D+00 /)
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( test_num ) :: n_test = (/ 2, 3, 4, 3 /)
  integer ( kind = 4 ) test
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_SOLVE2_TEST'
  write ( *, '(a)' ) '  R8MAT_SOLVE2 is a linear solver.'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    n = n_test ( test )

    allocate ( a(1:n,1:n) )
    allocate ( b(1:n) )
    allocate ( x(1:n) )

    if ( test == 1 ) then
      a(1:n,1:n) = a1(1:n,1:n)
      b(1:n) = b1(1:n)
    else if ( test == 2 ) then
      a(1:n,1:n) = a2(1:n,1:n)
      b(1:n) = b2(1:n)
    else if ( test == 3 ) then
      a(1:n,1:n) = a3(1:n,1:n)
      b(1:n) = b3(1:n)
    else if ( test == 4 ) then
      a(1:n,1:n) = a4(1:n,1:n)
      b(1:n) = b4(1:n)
    end if

    call r8vec_print ( n, b, '  Right hand side:' )

    call r8mat_solve2 ( n, a, b, x, ierror )

    write ( *, '(a)' ) ''
    if ( ierror == 0 ) then
      write ( *, '(a)' ) '  The system is nonsingular.'
    else if ( ierror == 1 ) then
      write ( *, '(a)' ) '  The system is singular, but consistent.'
    else if ( ierror == 2 ) then
      write ( *, '(a)' ) '  The system is singular and inconsistent.'
    end if

    call r8vec_print ( n, x, '  Computed solution:' )

    deallocate ( a )
    deallocate ( b )
    deallocate ( x )

  end do

  return
end
subroutine r8mat_standardize_test ( )

!*****************************************************************************80
!
!! R8MAT_STANDARDIZE_TEST tests R8MAT_STANDARDIZE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 10
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mu(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma(n)
  real ( kind = 8 ) x(m,n)
  real ( kind = 8 ) xmax(n)
  real ( kind = 8 ) xmin(n)
  real ( kind = 8 ) xs(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_STANDARDIZE_TEST'
  write ( *, '(a)' ) '  R8MAT_STANDARDIZE shifts and scales an R8MAT so that'
  write ( *, '(a)' ) '  every column has zero mean and unit standard deviation.'

  a = -5.0D+00
  b = 15.0D+00
  seed = 123456789

  call r8mat_uniform_ab ( m, n, a, b, seed, x )
 
  call r8mat_print ( m, n, x, '  Matrix X:' )
  call r8mat_mean_columns ( m, n, x, mu )
  call r8mat_std_columns ( m, n, x, sigma )
  call r8mat_max_columns ( m, n, x, xmax )
  call r8mat_min_columns ( m, n, x, xmin )
  write ( *, '(a)' ) ''
  write ( *, '(a,3g14.6)' ) '  mean(X) = ', mu(1:n)
  write ( *, '(a,3g14.6)' ) '  std(X)  = ', sigma(1:n)
  write ( *, '(a,3g14.6)' ) '  max(X)  = ', xmax(1:n)
  write ( *, '(a,3g14.6)' ) '  min(X)  = ', xmin(1:n)

  call r8mat_standardize ( m, n, x, xs )

  call r8mat_print ( m, n, xs, '  Matrix XS:' )
  call r8mat_mean_columns ( m, n, xs, mu )
  call r8mat_std_columns ( m, n, xs, sigma )
  call r8mat_max_columns ( m, n, xs, xmax )
  call r8mat_min_columns ( m, n, xs, xmin )
  write ( *, '(a)' ) ''
  write ( *, '(a,3g14.6)' ) '  mean(XS) = ', mu(1:n)
  write ( *, '(a,3g14.6)' ) '  std(XS)  = ', sigma(1:n)
  write ( *, '(a,3g14.6)' ) '  max(XS)  = ', xmax(1:n)
  write ( *, '(a,3g14.6)' ) '  min(XS)  = ', xmin(1:n)

  return
end
subroutine r8mat_symm_jacobi_test ( )

!*****************************************************************************80
!
!! R8MAT_SYMM_JACOBI_TEST tests R8MAT_SYMM_JACOBI;
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) q(n,n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_SYMM_JACOBI_TEST'
  write ( *, '(a)' ) '  For a symmetric R8MAT:'
  write ( *, '(a)' ) '  R8MAT_SYMM_JACOBI diagonalizes;'
!
!  Choose the eigenvalues.
!
  call r8vec_indicator1 ( n, x )
!
!  Choose the eigenvectors.
!
  seed = 123456789
  call r8mat_orth_uniform ( n, seed, q )
!
!  Compute A = Q*X*Q.
!
  call r8mat_symm_eigen ( n, x, q, a )

  call r8mat_print ( n, n, a, '  Matrix to diagonalize:' )

  call r8mat_symm_jacobi ( n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Computed Eigenvalues:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,g14.6)' ) a(i,i)
  end do

  return
end
subroutine r8mat_to_r8plu_test ( )

!*****************************************************************************80
!
!! R8MAT_TO_R8PLU_TEST tests R8MAT_TO_R8PLU.
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) a2(n,n)
  real ( kind = 8 ) :: b = 0.0D+00
  real ( kind = 8 ) :: c = 1.0D+00
  integer ( kind = 4 ) info
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_TO_R8PLU_TEST'
  write ( *, '(a)' ) '  R8MAT_TO_R8PLU determines the compressed PLU factors'
  write ( *, '(a)' ) '  of a R8MAT.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  call r8mat_uniform_ab ( n, n, b, c, seed, a )

  call r8mat_print ( n, n, a, '  The matrix A:' )
!
!  Factor the matrix.
!
  call r8mat_to_r8plu ( n, a, pivot, lu, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Warning!'
    write ( *, '(a)' ) '  R8MAT_TO_R8PLU declares the matrix is singular!'
    write ( *, '(a,i8)' ) '  The value of INFO is ', info
  end if
!
!  Display the gory details.
!
  call i4vec_print ( n, pivot, '  The pivot vector P:' )

  call r8mat_print ( n, n, lu, '  The compressed LU factors:' )
!
!  Recover the matrix from the PLU factors.
!
  call r8plu_to_r8mat ( n, pivot, lu, a2 )

  call r8mat_print ( n, n, a2, '  The recovered matrix A2:' )

  return
end
subroutine r8mat_trace_test ( )

!*****************************************************************************80
!
!! R8MAT_TRACE_TEST tests R8MAT_TRACE.
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

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_trace
  real ( kind = 8 ) trace

  do i = 1, n
    do j = 1, n

      if ( i <= j ) then
        a(i,j) = real ( n + 1 - j, kind = 8 )
      else if ( j == i - 1 ) then
        a(i,j) = real ( n - j, kind = 8 )
      else
        a(i,j) = 0.0D+00
      end if

    end do
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_TRACE_TEST'
  write ( *, '(a)' ) '  R8MAT_TRACE computes the trace of a matrix'

  call r8mat_print ( n, n, a, '  Matrix:' )

  trace = r8mat_trace ( n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Trace is ', trace

  return
end
subroutine r8mat_transpose_test ( )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_TEST tests R8MAT_TRANSPOSE.
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
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m*n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_TRANSPOSE_TEST'
  write ( *, '(a)' ) '  R8MAT_TRANSPOSE transposes an R8MAT.'

  call r8mat_indicator ( m, n, a )
  call r8mat_print ( m, n, a, '  Matrix A:' )

  call r8mat_transpose ( m, n, a )
  call r8mat_print ( n, m, a, '  Transposed matrix At:' )

  return
end
subroutine r8mat_transpose_in_place_test ( )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_IN_PLACE_TEST tests R8MAT_TRANSPOSE_IN_PLACE.
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_TRANSPOSE_IN_PLACE_TEST'
  write ( *, '(a)' ) '  R8MAT_TRANSPOSE_IN_PLACE transposes an R8MAT.'

  call r8mat_indicator ( n, n, a )
  call r8mat_print ( n, n, a, '  Matrix A:' )

  call r8mat_transpose_in_place ( n, a )
  call r8mat_print ( n, n, a, '  Transposed matrix At:' )

  return
end
subroutine r8mat_transpose_new_test ( )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_NEW_TEST tests R8MAT_TRANSPOSE_NEW.
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
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) at(n,m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_TRANSPOSE_NEW_TEST'
  write ( *, '(a)' ) '  R8MAT_TRANSPOSE_NEW transposes an R8MAT.'

  call r8mat_indicator ( m, n, a )
  call r8mat_print ( m, n, a, '  Matrix A:' )

  call r8mat_transpose_new ( m, n, a, at )
  call r8mat_print ( n, m, at, '  Transposed matrix At:' )

  return
end
subroutine r8mat_transpose_print_test ( )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT_TEST tests R8MAT_TRANSPOSE_PRINT;
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_TRANSPOSE_PRINT_TEST'
  write ( *, '(a)' ) '  R8MAT_TRANSPOSE_PRINT prints an R8MAT,'
  write ( *, '(a)' ) '  transposed.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Matrix row order M =    ', m
  write ( *, '(a,i8)' ) '  Matrix column order N = ', n

  call r8mat_indicator ( m, n, a )

  call r8mat_print ( m, n, a, '  The matrix A:' )

  call r8mat_transpose_print ( m, n, a, '  The transposed matrix A:' )

  return
end
subroutine r8mat_transpose_print_some_test ( )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT_SOME_TEST tests R8MAT_TRANSPOSE_PRINT_SOME.
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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_TRANSPOSE_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT,'
  write ( *, '(a)' ) '  transposed.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Matrix row order M =    ', m
  write ( *, '(a,i8)' ) '  Matrix column order N = ', n

  call r8mat_indicator ( m, n, a )

  call r8mat_print ( m, n, a, '  The matrix A:' )

  call r8mat_transpose_print_some ( m, n, a, 1, 2, 3, 3, &
    '  The transposed matrix A, rows 1:3, cols 2:3:' )

  return
end
subroutine r8mat_u_inverse_test ( )

!*****************************************************************************80
!
!! R8MAT_U_INVERSE_TEST tests R8MAT_U_INVERSE.
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

  integer ( kind = 4 ), parameter :: n = 4
!
!  Each row of this definition is a COLUMN of the matrix.
!
  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
    1.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, &
    2.0D+00, 3.0D+00, 0.0D+00,  0.0D+00, &
    4.0D+00, 5.0D+00, 6.0D+00,  0.0D+00, &
    7.0D+00, 8.0D+00, 9.0D+00, 10.0D+00 /), (/ n, n /) )
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) c(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_U_INVERSE_TEST'
  write ( *, '(a)' ) '  R8MAT_U_INVERSE inverts an upper triangular matrix.'

  call r8mat_print ( n, n, a, '  Input matrix A' )

  call r8mat_u_inverse ( n, a, b )

  call r8mat_print ( n, n, b, '  Inverse matrix B:' )

  c(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )

  call r8mat_print ( n, n, c, '  Product C = A * B:' )

  return
end
subroutine r8mat_u_solve_test ( )

!*****************************************************************************80
!
!! R8MAT_U_SOLVE_TEST tests R8MAT_U_SOLVE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2015
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
  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
    1.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, &
    2.0D+00, 3.0D+00, 0.0D+00,  0.0D+00, &
    4.0D+00, 5.0D+00, 6.0D+00,  0.0D+00, &
    7.0D+00, 8.0D+00, 9.0D+00, 10.0D+00 /), (/ n, n /) )
  real ( kind = 8 ), dimension ( n ) :: b = (/ &
    45.0D+00, 53.0D+00, 54.0D+00, 40.0D+00 /)
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) r8vec_norm
  real ( kind = 8 ) rnorm
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_U_SOLVE_TEST'
  write ( *, '(a)' ) '  R8MAT_U_SOLVE solves an upper triangular system.'

  call r8mat_print ( n, n, a, '  Input matrix A:' )

  call r8vec_print ( n, b, '  Right hand side b:' )

  call r8mat_u_solve ( n, a, b, x )

  call r8vec_print ( n, x, '  Computed solution x:' )

  r(1:n) = matmul ( a(1:n,1:n), x(1:n) ) - b(1:n)

  rnorm = r8vec_norm ( n, r )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Norm of A*x-b = ', rnorm

  return
end
subroutine r8mat_u1_inverse_test ( )

!*****************************************************************************80
!
!! R8MAT_U1_INVERSE_TEST tests R8MAT_U1_INVERSE.
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
  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
    1.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, &
    2.0D+00, 1.0D+00, 0.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, &
    0.0D+00, 0.0D+00, 1.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, &
    5.0D+00, 0.0D+00, 3.0D+00, 1.0D+00, 0.0D+00,  0.0D+00, &
    0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 1.0D+00,  0.0D+00, &
   75.0D+00, 0.0D+00, 0.0D+00, 6.0D+00, 4.0D+00,  1.0D+00 /), (/ n, n /) )
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) c(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_U1_INVERSE_TEST'
  write ( *, '(a)' ) '  R8MAT_U1_INVERSE inverts a unit upper triangular matrix.'

  call r8mat_print ( n, n, a, '  Input matrix A' )

  call r8mat_u1_inverse (  n, a, b )

  call r8mat_print ( n, n, b, '  Inverse matrix B:' )

  c(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )

  call r8mat_print ( n, n, c, '  Product C = A * B:' )

  return
end
subroutine r8mat_uniform_ab_test ( )

!*****************************************************************************80
!
!! R8MAT_UNIFORM_AB_TEST tests R8MAT_UNIFORM_AB.
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

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ), dimension (m,n) :: a
  real ( kind = 8 ), parameter :: b = 2.0D+00
  real ( kind = 8 ), parameter :: c = 10.0D+00
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  R8MAT_UNIFORM_AB sets an R8MAT to random values in [A,B].'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  call r8mat_uniform_ab ( m, n, b, c, seed, a )
!
!  Print out the matrix to be inverted.
!
  call r8mat_print ( m, n, a, '  The random matrix:' )

  return
end
subroutine r8mat_ut_solve_test ( )

!*****************************************************************************80
!
!! R8MAT_UT_SOLVE_TEST tests R8MAT_UT_SOLVE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2015
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
  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
    1.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, &
    2.0D+00, 3.0D+00, 0.0D+00,  0.0D+00, &
    4.0D+00, 5.0D+00, 6.0D+00,  0.0D+00, &
    7.0D+00, 8.0D+00, 9.0D+00, 10.0D+00 /), (/ n, n /) )
  real ( kind = 8 ), dimension ( n ) :: b = (/ &
    1.0D+00, 8.0D+00, 32.0D+00, 90.0D+00 /)
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) r8vec_norm
  real ( kind = 8 ) rnorm
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8MAT_UT_SOLVE_TEST'
  write ( *, '(a)' ) '  R8MAT_UT_SOLVE solves a transposed upper triangular system.'

  call r8mat_print ( n, n, a, '  Input matrix A:' )

  call r8vec_print ( n, b, '  Right hand side b:' )

  call r8mat_ut_solve ( n, a, b, x )

  call r8vec_print ( n, x, '  Computed solution x:' )

  r(1:n) = matmul ( transpose ( a(1:n,1:n) ), x(1:n) ) - b(1:n)

  rnorm = r8vec_norm ( n, r )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Norm of A''*x-b = ', rnorm

  return
end
subroutine r8plu_det_test ( )

!*****************************************************************************80
!
!! R8PLU_DET_TEST tests R8PLU_DET;
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) info
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8PLU_DET_TEST'
  write ( *, '(a)' ) '  R8PLU_DET determines the determinant of a matrix'
  write ( *, '(a)' ) '  from its compressed PLU factors.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  call r8mat_uniform_01 ( n, n, seed, a )

  call r8mat_print ( n, n, a, '  The matrix A:' )
!
!  Factor the matrix.
!
  call r8mat_to_r8plu ( n, a, pivot, lu, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Fatal error!'
    write ( *, '(a)' ) '  R8MAT_TO_R8PLU declares the matrix is singular!'
    write ( *, '(a,i8)' ) '  The value of INFO is ', info
    return
  end if
!
!  Compute the determinant.
!
  call r8plu_det ( n, pivot, lu, det )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The determinant = ', det

  return
end
subroutine r8plu_inverse_test ( )

!*****************************************************************************80
!
!! R8PLU_INVERSE_TEST tests R8PLU_INVERSE;
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) c(n,n)
  integer ( kind = 4 ) info
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8PLU_INVERSE_TEST'
  write ( *, '(a)' ) '  R8PLU_INVERSE determines the inverse of a matrix'
  write ( *, '(a)' ) '  from its compressed PLU factors.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  call r8mat_uniform_01 ( n, n, seed, a )

  call r8mat_print ( n, n, a, '  The matrix A:' )
!
!  Factor the matrix.
!
  call r8mat_to_r8plu ( n, a, pivot, lu, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'R8PLU_INVERSE_TEST - Warning!'
    write ( *, '(a)' ) '  R8MAT_TO_R8PLU declares the matrix is singular!'
    write ( *, '(a,i8)' ) '  The value of INFO is ', info
    return
  end if
!
!  Compute the inverse.
!
  call r8plu_inverse ( n, pivot, lu, b )

  call r8mat_print ( n, n, b, '  The inverse matrix B:' )
!
!  Compute the product C = A * B.
!
  c(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )

  call r8mat_print ( n, n, c, '  The product matrix C = A * B:' )

  return
end
subroutine r8plu_mul_test ( )

!*****************************************************************************80
!
!! R8PLU_MUL_TEST tests R8PLU_MUL;
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8PLU_MUL_TEST'
  write ( *, '(a)' ) '  R8PLU_MUL computes the product A*x=b'
  write ( *, '(a)' ) '  using the compressed PLU factors of A.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  call r8mat_uniform_01 ( n, n, seed, a )

  call r8mat_print ( n, n, a, '  The matrix A:' )
!
!  Set the right hand side B1.
!
  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do

  b(1:n) = matmul ( a(1:n,1:n), x(1:n) )

  call r8vec_print ( n, b, '  The right hand side B (computed from A):' )
!
!  Factor the matrix.
!
  call r8mat_to_r8plu ( n, a, pivot, lu, info )
!
!  Compute the matrix-vector product.
!
  call r8plu_mul ( n, pivot, lu, x, b )

  call r8vec_print ( n, b, '  The right hand side B (computed from PLU):' )

  return
end
subroutine r8plu_solve_test ( )

!*****************************************************************************80
!
!! R8PLU_SOLVE_TEST tests R8PLU_SOLVE;
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8PLU_SOLVE_TEST'
  write ( *, '(a)' ) '  R8PLU_SOLVE solves a linear system A*x=b'
  write ( *, '(a)' ) '  using the compressed PLU factors of A.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  call r8mat_uniform_01 ( n, n, seed, a )

  call r8mat_print ( n, n, a, '  The matrix A:' )
!
!  Set the right hand side.
!
  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do
  b(1:n) = matmul ( a(1:n,1:n), x(1:n) )
  x(1:n) = 0.0D+00

  call r8vec_print ( n, b, '  The right hand side B:' )
!
!  Factor the matrix.
!
  call r8mat_to_r8plu ( n, a, pivot, lu, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Fatal error!'
    write ( *, '(a)' ) '  R8MAT_TO_R8PLU declares the matrix is singular!'
    write ( *, '(a,i8)' ) '  The value of INFO is ', info
    return
  end if
!
!  Solve the system.
!
  call r8plu_solve ( n, pivot, lu, b, x )

  call r8vec_print ( n, x, '  The computed solution X:' )

  return
end
subroutine r8plu_to_r8mat_test ( )

!*****************************************************************************80
!
!! R8PLU_TO_R8MAT_TEST tests R8PLU_TO_R8MAT;
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) a2(n,n)
  real ( kind = 8 ) :: b = 0.0D+00
  real ( kind = 8 ) :: c = 1.0D+00
  integer ( kind = 4 ) info
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8PLU_TO_R8MAT_TEST'
  write ( *, '(a)' ) '  R8PLU_TO_R8MAT determines the original matrix from'
  write ( *, '(a)' ) '  the compressed PLU factors.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
  call r8mat_uniform_ab ( n, n, b, c, seed, a )

  call r8mat_print ( n, n, a, '  The matrix A:' )
!
!  Factor the matrix.
!
  call r8mat_to_r8plu ( n, a, pivot, lu, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Warning!'
    write ( *, '(a)' ) '  R8MAT_TO_R8PLU declares the matrix is singular!'
    write ( *, '(a,i8)' ) '  The value of INFO is ', info
  end if
!
!  Display the gory details.
!
  call i4vec_print ( n, pivot, '  The pivot vector P:' )

  call r8mat_print ( n, n, lu, '  The compressed LU factors:' )
!
!  Recover the matrix from the PLU factors.
!
  call r8plu_to_r8mat ( n, pivot, lu, a2 )

  call r8mat_print ( n, n, a2, '  The recovered matrix A2:' )

  return
end
subroutine r8rows_to_r8mat_test ( )

!*****************************************************************************80
!
!! R8ROWS_TO_I4MAT_TEST tests R8ROWS_TO_I4MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) r8mat(m,n)
  real ( kind = 8 ), dimension ( m * n ) :: r8rows = (/ &
    11.0D+00, 12.0D+00, 13.0D+00, 14.0D+00, &
    21.0D+00, 22.0D+00, 23.0D+00, 24.0D+00, &
    31.0D+00, 32.0D+00, 33.0D+00, 34.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8ROWS_TO_R8MAT_TEST'
  write ( *, '(a)' ) '  R8ROWS_TO_R8MAT allows an R8MAT to be initialized'
  write ( *, '(a)' ) '  by data stored ROW-WISE in a vector.'

  call r8vec_print ( m * n, r8rows, '  The data vector:' )

  call r8rows_to_i4mat ( m, n, r8rows, r8mat )

  call r8mat_print ( m, n, r8mat, '  The data copied into an array:' )

  return
end
subroutine r8slmat_print_test ( )

!*****************************************************************************80
!
!! R8SLMAT_PRINT_TEST tests R8SLMAT_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 3

  real ( kind = 8 ), allocatable, dimension ( : ) :: a
  real ( kind = 8 ), dimension ( 21 ) :: a1 = (/ &
    21.0D+00, 31.0D+00, 41.0D+00, 51.0D+00, 61.0D+00, 71.0D+00, &
              32.0D+00, 42.0D+00, 52.0D+00, 62.0D+00, 72.0D+00, &
                        43.0D+00, 53.0D+00, 63.0D+00, 73.0D+00, &
                                  54.0D+00, 64.0D+00, 74.0D+00, &
                                            65.0D+00, 75.0D+00, &
                                                      76.0D+00 /)
  real ( kind = 8 ), dimension ( 15 ) :: a2 = (/ &
    21.0D+00, 31.0D+00, 41.0D+00, 51.0D+00, 61.0D+00, 71.0D+00, &
              32.0D+00, 42.0D+00, 52.0D+00, 62.0D+00, 72.0D+00, &
                        43.0D+00, 53.0D+00, 63.0D+00, 73.0D+00 /)
  real ( kind = 8 ), dimension ( 6 ) :: a3 = (/ &
    21.0D+00, 31.0D+00, 41.0D+00, &
              32.0D+00, 42.0D+00, &
                        43.0D+00 /)
  integer ( kind = 4 ) m
  integer ( kind = 4 ), dimension ( test_num ) :: m_test = (/ 7, 7, 4 /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ), dimension ( test_num ) :: n_test = (/ 7, 3, 7 /)
  integer ( kind = 4 ) size
  integer ( kind = 4 ), dimension ( test_num ) :: size_test = (/ 21, 15, 6 /)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8SLMAT_PRINT_TEST'
  write ( *, '(a)' ) '  R8SLMAT_PRINT prints a strictly lower triangular matrix'
  write ( *, '(a)' ) '  stored compactly.  Only the (possibly) nonzero '
  write ( *, '(a)' ) '  elements are printed.'

  do test = 1, test_num

    m = m_test(test)
    n = n_test(test)
    size = size_test(test)

    allocate ( a(1:size) )

    if ( test == 1 ) then
      a(1:size) = a1(1:size)
    else if ( test == 2 ) then
      a(1:size) = a2(1:size)
    else if ( test == 3 ) then
      a(1:size) = a3(1:size)
    end if

    call r8slmat_print ( m, n, a, '  R8SLMAT' )

    deallocate ( a )

  end do

  return
end
subroutine r8vec_amax_test ( )

!*****************************************************************************80
!
!! R8VEC_AMAX_TEST tests R8VEC_AMAX;
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

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) aval
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8vec_amax
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_AMAX_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_AMAX:      maximum magnitude entry;'

  r8_lo = -5.0D+00
  r8_hi = +5.0D+00

  seed = 123456789

  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  aval = r8vec_amax ( n, a )
  write ( *, '(a,g14.6)' ) '  Maximum absolute:         ', aval

  return
end
subroutine r8vec_amin_test ( )

!*****************************************************************************80
!
!! R8VEC_AMIN_TEST tests R8VEC_AMIN;
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

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) aval
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8vec_amin
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_AMIN_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_AMIN:      minimum magnitude entry.'

  b = - real ( n, kind = 8 )
  c = real ( n, kind = 8 )

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  aval = r8vec_amin ( n, a )
  write ( *, '(a,g14.6)' ) '  Minimum absolute:         ', aval

  return
end
subroutine r8vec_binary_next_test ( )

!*****************************************************************************80
!
!! R8VEC_BINARY_NEXT_TEST tests R8VEC_BINARY_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) bvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_BINARY_NEXT_TEST'
  write ( *, '(a)' ) '  R8VEC_BINARY_NEXT generates the next binary vector.'
  write ( *, '(a)' ) ''
 
  bvec(1:n) = 0.0D+00

  do

    call r8vec_transpose_print ( n, bvec, '  ' )

    if ( all ( bvec(1:n) == 1.0D+00 ) ) then
      exit
    end if

    call r8vec_binary_next ( n, bvec )
 
  end do

  return
end
subroutine r8vec_bracket_test ( )

!*****************************************************************************80
!
!! R8VEC_BRACKET_TEST tests R8VEC_BRACKET.
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
  integer ( kind = 4 ), parameter :: test_num = 6

  integer ( kind = 4 ) left
  integer ( kind = 4 ) right
  integer ( kind = 4 ) test
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), dimension ( test_num ) :: xtest = (/ &
    -10.0D+00, 1.0D+00, 4.5D+00, 5.0D+00, 10.0D+00, 12.0D+00 /)
  real ( kind = 8 ) xval

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_BRACKET_TEST'
  write ( *, '(a)' ) '  R8VEC_BRACKET finds a pair of entries in a'
  write ( *, '(a)' ) '  sorted R8VEC which bracket a value.'

  call r8vec_indicator1 ( n, x )
  x(6) = x(5)

  call r8vec_print ( n, x, '  Sorted array:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    LEFT             RIGHT'
  write ( *, '(a)' ) '  X(LEFT)   XVAL   X(RIGHT)'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    xval = xtest(test)

    call r8vec_bracket ( n, x, xval, left, right )

    write ( *, '(i14,14x,i14)' ) left, right

    if ( 1 <= left .and. 1 <= right ) then
      write ( *, '(2x,3g14.6)' ) x(left), xval, x(right)
    else if ( left < 1 .and. 1 <= right ) then
      write ( *, '(2x,14x,2g14.6)' )          xval, x(right)
    else if ( 1 <= left .and. right < 1 ) then
      write ( *, '(2x,2g14.6)' ) x(left), xval
    else if ( left < 1 .and. right < 1 ) then
      write ( *, '(2x,14x,g14.6)' )          xval
    end if

  end do

  return
end
subroutine r8vec_bracket2_test ( )

!*****************************************************************************80
!
!! R8VEC_BRACKET2_TEST tests R8VEC_BRACKET2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 6

  integer ( kind = 4 ) left
  integer ( kind = 4 ) n
  integer ( kind = 4 ) right
  integer ( kind = 4 ) start
  integer ( kind = 4 ) test
  real ( kind = 8 ) x(10)
  real ( kind = 8 ), dimension ( test_num ) :: xtest = (/ &
    -10.0D+00, 1.0D+00, 4.5D+00, 5.0D+00, 10.0D+00, 12.0D+00 /)
  real ( kind = 8 ) xval

  n = 10

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_BRACKET2_TEST'
  write ( *, '(a)' ) '  R8VEC_BRACKET2 finds a pair of entries in a'
  write ( *, '(a)' ) '  sorted R8VEC which bracket a value.'

  call r8vec_indicator1 ( n, x )
  x(6) = x(5)

  call r8vec_print ( n, x, '  Sorted array:' )

  left = 0

  do test = 1, test_num

    xval = xtest(test)

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  Search for XVAL = ', xval

    if ( 0 < left ) then
      start = left
    else
      start = ( n + 1 ) / 2
    end if

    write ( *, '(a,i8)' ) '  Start = ', start

    call r8vec_bracket2 ( n, x, xval, start, left, right )

    write ( *, '(a,i8)' ) '  Left = ', left
    write ( *, '(a,i8)' ) '  Right = ', right

    if ( 1 <= left ) then
      write ( *, '(a,g14.6)' ) '  X(LEFT)=', x(left)
    end if

    if ( 1 <= right ) then
      write ( *, '(a,g14.6)' ) '  X(RIGHT) = ', x(right)
    end if

  end do

  return
end
subroutine r8vec_bracket3_test ( )

!*****************************************************************************80
!
!! R8VEC_BRACKET3_TEST tests R8VEC_BRACKET3.
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

  integer ( kind = 4 ), parameter :: test_num = 6

  integer ( kind = 4 ) left
  integer ( kind = 4 ) n
  integer ( kind = 4 ) test
  real ( kind = 8 ) x(10)
  real ( kind = 8 ), dimension ( test_num ) :: x_test = (/ &
    -10.0D+00, 1.0D+00, 4.5D+00, 5.0D+00, 10.0D+00, 12.0D+00 /)
  real ( kind = 8 ) xval

  n = 10

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_BRACKET3_TEST'
  write ( *, '(a)' ) '  R8VEC_BRACKET3 finds a pair of entries in a'
  write ( *, '(a)' ) '  sorted R8VEC which bracket a value.'

  call r8vec_indicator1 ( n, x )
  x(6) = x(5)

  call r8vec_print ( n, x, '  Sorted array:' )

  left = ( n + 1 ) / 2

  do test = 1, test_num

    xval = x_test(test)

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  Search for XVAL = ', xval

    write ( *, '(a,i8)' ) '  Starting guess for interval is = ', left

    call r8vec_bracket3 ( n, x, xval, left )

    write ( *, '(a)' ) '  Nearest interval:'
    write ( *, '(a,i8,a,g14.6)' ) '    X[', left,' ]= ', x(left)
    write ( *, '(a,i8,a,g14.6)' ) '    X[', left+1, ' ]= ', x(left+1)

  end do

  return
end
subroutine r8vec_bracket5_test ( )

!*****************************************************************************80
!
!! R8VEC_BRACKET5_TEST tests R8VEC_BRACKET5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 October 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10
  integer ( kind = 4 ), parameter :: test_num = 6

  integer ( kind = 4 ) left
  integer ( kind = 4 ) r8vec_bracket5
  integer ( kind = 4 ) right
  integer ( kind = 4 ) test
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), dimension ( test_num ) :: xtest = (/ &
    -10.0D+00, 1.0D+00, 4.5D+00, 5.0D+00, 10.0D+00, 12.0D+00 /)
  real ( kind = 8 ) xval

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_BRACKET5_TEST'
  write ( *, '(a)' ) '  R8VEC_BRACKET5 finds a pair of entries in a'
  write ( *, '(a)' ) '  sorted R8VEC which bracket a value.'

  call r8vec_indicator1 ( n, x )
  x(6) = x(5)

  call r8vec_print ( n, x, '  Sorted array:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        LEFT                   RIGHT'
  write ( *, '(a)' ) '      X(LEFT)       XVAL     X(RIGHT)'
  write ( *, '(a)' ) '' 

  do test = 1, test_num

    xval = xtest(test)

    left = r8vec_bracket5 ( n, x, xval )

    if ( left == -1 ) then
      write ( *, '(2x,i10)' ) left
      write ( *, '(2x,10x,2x,f10.4,2x,a)' ) xval, '(Not bracketed!)'
    else
      right = left + 1
      write ( *, '(2x,i10,2x,10x,2x,i10)' ) left, right
      write ( *, '(2x,f10.4,2x,f10.4,2x,f10.4)' ) x(left), xval, x(right)
    end if

  end do

  return
end
subroutine r8vec_cheby_extreme_test ( )

!*****************************************************************************80
!
!! R8VEC_CHEBY_EXTREME_TEST tests R8VEC_CHEBY_EXTREME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_CHEBY_EXTREME_TEST'
  write ( *, '(a)' ) '  R8VEC_CHEBY_EXTREME computes N Chebyshev Extreme points in [R1,R2].'

  r1 = -1.0D+00
  r2 = +1.0D+00
  n = 5
  allocate ( r(1:n) )

  call r8vec_cheby_extreme ( n, r1, r2, r )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4,a,g14.6,a,g14.6)' ) '  N = ', n, '  R1 = ', r1, '  R2 = ', r2

  call r8vec_print ( n, r, '  Chebyshev points:' )

  deallocate ( r )

  r1 =   0.0D+00
  r2 = +10.0D+00
  n = 7

  allocate ( r(1:n) )

  call r8vec_cheby_extreme ( n, r1, r2, r )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4,a,g14.6,a,g14.6)' ) '  N = ', n, '  R1 = ', r1, '  R2 = ', r2

  call r8vec_print ( n, r, '  Chebyshev points:' )

  deallocate ( r )

  return
end
subroutine r8vec_cheby_zero_test ( )

!*****************************************************************************80
!
!! R8VEC_CHEBY_ZERO_TEST tests R8VEC_CHEBY_ZERO.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_CHEBY_ZERO_TEST'
  write ( *, '(a)' ) '  R8VEC_CHEBY1SPACE computes Chebyshev Zero points in [R1,R2].'

  r1 = -1.0D+00
  r2 = +1.0D+00
  n = 5
  allocate ( r(1:n) )

  call r8vec_cheby_zero ( n, r1, r2, r )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4,a,g14.6,a,g14.6)' ) '  N = ', n, '  R1 = ', r1, '  R2 = ', r2

  call r8vec_print ( n, r, '  Chebyshev points:' )

  deallocate ( r )

  r1 =   0.0D+00
  r2 = +10.0D+00
  n = 7

  allocate ( r(1:n) )

  call r8vec_cheby_zero ( n, r1, r2, r )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4,a,g14.6,a,g14.6)' ) '  N = ', n, '  R1 = ', r1, '  R2 = ', r2

  call r8vec_print ( n, r, '  Chebyshev points:' )

  deallocate ( r )

  return
end
subroutine r8vec_concatenate_test ( )

!*****************************************************************************80
!
!! R8VEC_CONCATENATE_TEST tests R8VEC_CONCATENATE.
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

  real ( kind = 8 ), dimension ( n1 ) :: a1 = (/ &
    91.1, 31.2, 71.3, 51.4, 31.5 /)
  real ( kind = 8 ), dimension ( n2 ) :: a2 = (/ &
    42.6, 22.7, 12.8 /)
  real ( kind = 8 ) a3(n3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_CONCATENATE_TEST'
  write ( *, '(a)' ) '  R8VEC_CONCATENATE concatenates two R8VECs'

  call r8vec_print ( n1, a1, '  Array 1:' )
  call r8vec_print ( n2, a2, '  Array 2:' )
  call r8vec_concatenate ( n1, a1, n2, a2, a3 )
  call r8vec_print ( n3, a3, '  Array 3 = Array 1 + Array 2:' )

  return
end
subroutine r8vec_convolution_test ( )

!*****************************************************************************80
!
!! R8VEC_CONVOLUTION_TEST tests R8VEC_CONVOLUTION
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 May 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ), dimension(m) :: x = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00 /)
  real ( kind = 8 ), dimension(n) :: y = (/ &
   -1.0D+00, 5.0D+00, 3.0D+00 /)
  real ( kind = 8 ) z(m+n-1)
  real ( kind = 8 ), dimension (m+n-1) :: z_correct = (/ &
    -1.0D+00, 3.0D+00, 10.0D+00, 17.0D+00, 29.0D+00, 12.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_CONVOLUTION_TEST'
  write ( *, '(a)' ) '  R8VEC_CONVOLUTION computes the convolution'
  write ( *, '(a)' ) '  of two vectors.'

  call r8vec_print ( m, x, '  The factor X:' )
  call r8vec_print ( n, y, '  The factor Y:' )

  call r8vec_convolution ( m, x, n, y, z )

  call r8vec_print ( m + n - 1, z, '  The convolution z = x star y:' )

  call r8vec_print ( m + n - 1, z_correct, '  Correct answer:' )

  return
end
subroutine r8vec_convolution_circ_test ( )

!*****************************************************************************80
!
!! R8VEC_CONVOLUTION_CIRT_TEST tests R8VEC_CONVOLUTION_CIRC
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

  real ( kind = 8 ), dimension(n) :: x = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00 /)
  real ( kind = 8 ), dimension(n) :: y = (/ &
    1.0D+00, 2.0D+00, 4.0D+00, 8.0D+00 /)
  real ( kind = 8 ) z(n)
  real ( kind = 8 ), dimension ( n ) :: z_correct = (/ &
    37.0D+00, 44.0D+00, 43.0D+00, 26.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_CONVOLUTION_CIRC_TEST'
  write ( *, '(a)' ) '  R8VEC_CONVOLUTION_CIRC computes the circular convolution'
  write ( *, '(a)' ) '  of two R8VECs.'

  call r8vec_print ( n, x, '  The factor X:' )
  call r8vec_print ( n, y, '  The factor Y:' )

  call r8vec_convolution_circ ( n, x, y, z )

  call r8vec_print ( n, z, '  The circular convolution z = x CC y:' )

  call r8vec_print ( n, z_correct, '  Correct answer:' )

  return
end
subroutine r8vec_correlation_test ( )

!*****************************************************************************80
!
!! r8vec_correlation_test tests r8vec_correlation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 August 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ) r
  real ( kind = 8 ), dimension(n) :: v1 = (/ &
    43.0D+00, 21.0D+00, 25.0D+00, 42.0D+00, 57.0D+00, 59.0D+00 /)
  real ( kind = 8 ), dimension(n) :: v2 = (/ &
    99.0D+00, 65.0D+00, 79.0D+00, 75.0D+00, 87.0D+00, 81.0D+00 /)


  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8vec_correlation_test:'
  write ( *, '(a)' ) '  r8vec_correlation computes the correlation of two R8VEC''s.'

  call r8vec_print ( n, v1, '  Vector V1:' )
  call r8vec_print ( n, v2, '  Vector V2:' )

  call r8vec_correlation ( n, v1, v2, r )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  V1 V2 Correlation coefficient r = ', r

  return
end
subroutine r8vec_dif_test ( )

!*****************************************************************************80
!
!! R8VEC_DIF_TEST tests R8VEC_DIF.
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

  real ( kind = 8 ) cof(0:n)
  real ( kind = 8 ) fdif
  real ( kind = 8 ) :: h = 0.01D+00
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec_dif_f
  real ( kind = 8 ) :: x = 1.0D+00
  real ( kind = 8 ) xi

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_DIF_TEST'
  write ( *, '(a)' ) '  R8VEC_DIF estimates derivatives.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Estimate the derivative of order N = ', n
  write ( *, '(a,g14.6)' ) '  Using H = ', h
  write ( *, '(a,g14.6)' ) '  at argument X = ', x
!
!  Get the coefficients.
!
  call r8vec_dif ( n, h, cof )

  call r8vec_print ( n + 1, cof, '  The difference coefficients:' )

  fdif = 0.0D+00
  do i = 0, n
    xi = x + real ( 2 * i - n, kind = 8 ) * h
    fdif = fdif + cof(i) * r8vec_dif_f ( xi )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Estimate is FDIF = ', fdif

  return
end
function r8vec_dif_f ( x )

!*****************************************************************************80
!
!! R8VEC_DIF_F evaluates the function used in R8VEC_DIF_TEST.
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

  real ( kind = 8 ) r8vec_dif_f
  real ( kind = 8 ) x

  r8vec_dif_f = exp ( x )

  return
end
subroutine r8vec_diff_norm_li_test ( )

!*****************************************************************************80
!
!! R8VEC_DIFF_NORM_LI_TEST tests R8VEC_DIFF_NORM_LI.
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

  real ( kind = 8 ) diff
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8vec_diff_norm_li
  integer ( kind = 4 ) seed
  real ( kind = 8 ) v1(n)
  real ( kind = 8 ) v2(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_DIFF_NORM_LI_TEST'
  write ( *, '(a)' ) '  R8VEC_DIFF_NORM_LI computes the L-infinity'
  write ( *, '(a)' ) '  norm of the difference of two R8VEC''s'

  r8_lo = -10.0D+00
  r8_hi = +10.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v1 )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v2 )

  call r8vec_print ( n, v1, '  Vector V1:' )
  call r8vec_print ( n, v2, '  Vector V2:' )

  diff = r8vec_diff_norm_li ( n, v1, v2 )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  L-Infinity norm of V1-V2 = ', diff

  return
end
subroutine r8vec_direct_product_test ( )

!*****************************************************************************80
!
!! R8VEC_DIRECT_PRODUCT_TEST tests R8VEC_DIRECT_PRODUCT.
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
  real ( kind = 8 ), allocatable, dimension ( : ) :: factor_value
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(factor_num,point_num)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_DIRECT_PRODUCT_TEST'
  write ( *, '(a)' ) '  R8VEC_DIRECT_PRODUCT forms the entries of a'
  write ( *, '(a)' ) '  direct product of a given number of R8VEC factors.'

  x(1:factor_num,1:point_num) = 0.0D+00

  do factor_index = 1, factor_num

    if ( factor_index == 1 ) then
      factor_order = 4
      allocate ( factor_value(1:factor_order) )
      factor_value = (/ 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00 /)
    else if ( factor_index == 2 ) then
      factor_order = 3
      allocate ( factor_value(1:factor_order) )
      factor_value = (/ 50.0D+00, 60.0D+00, 70.0D+00 /)
    else if ( factor_index == 3 ) then
      factor_order = 2
      allocate ( factor_value(1:factor_order) )
      factor_value = (/ 800.0D+00, 900.0D+00 /)
    end if

    call r8vec_direct_product ( factor_index, factor_order, factor_value,  &
      factor_num, point_num, x )

    deallocate ( factor_value )

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     J         X(1)      X(2)      X(3)'
  write ( *, '(a)' ) ''

  do j = 1, point_num
    write ( *, '(2x,i4,4x,f8.1,2x,f8.1,2x,f8.1)' ) j, x(1:factor_num,j)
  end do

  return
end
subroutine r8vec_direct_product2_test ( )

!*****************************************************************************80
!
!! R8VEC_DIRECT_PRODUCT2_TEST tests R8VEC_DIRECT_PRODUCT2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 May 2007
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
  real ( kind = 8 ), allocatable, dimension ( : ) :: factor_value
  integer ( kind = 4 ) j
  real ( kind = 8 ) w(point_num)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC)_DIRECT_PRODUCT2_TEST'
  write ( *, '(a)' ) '  R8VEC_DIRECT_PRODUCT2 forms the entries of a'
  write ( *, '(a)' ) '  direct product of a given number of R8VEC factors.'

  w(1:point_num) = 1.0D+00

  do factor_index = 1, factor_num

    if ( factor_index == 1 ) then
      factor_order = 4
      allocate ( factor_value(1:factor_order) )
      factor_value = (/ 2.0D+00, 3.0D+00, 5.0D+00, 7.0D+00 /)
    else if ( factor_index == 2 ) then
      factor_order = 3
      allocate ( factor_value(1:factor_order) )
      factor_value = (/ 11.0D+00, 13.0D+00, 17.0D+00 /)
    else if ( factor_index == 3 ) then
      factor_order = 2
      allocate ( factor_value(1:factor_order) )
      factor_value = (/ 19.0D+00, 21.0D+00 /)
    end if

    call r8vec_direct_product2 ( factor_index, factor_order, factor_value,  &
      factor_num, point_num, w )

    deallocate ( factor_value )

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     J         W(J)'
  write ( *, '(a)' ) ''

  do j = 1, point_num
    write ( *, '(2x,i4,4x,f8.1)' ) j, w(j)
  end do

  return
end
subroutine r8vec_dot_product_test ( )

!*****************************************************************************80
!
!! R8VEC_DOT_PRODUCT_TEST tests R8VEC_DOT_PRODUCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) r8vec_dot_product
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: v1(:)
  real ( kind = 8 ), allocatable :: v2(:)
  real ( kind = 8 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_DOT_PRODUCT_TEST:'
  write ( *, '(a)' ) '  R8VEC_DOT_PRODUCT computes the dot product of two R8VEC''s.'

  n = 10
  allocate ( v1(1:n) )
  allocate ( v2(1:n) )
  seed = 123456789
  call r8vec_uniform_01 ( n, seed, v1 )
  call r8vec_uniform_01 ( n, seed, v2 )
  call r8vec2_print ( n, v1, v2, "  V1 and V2:" )

  value = r8vec_dot_product ( n, v1, v2 )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  V1 dot V2 = ', value

  deallocate ( v1 )
  deallocate ( v2 )

  return
end
subroutine r8vec_even_test ( )

!*****************************************************************************80
!
!! R8VEC_EVEN_TEST tests R8VEC_EVEN.
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

  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo

  xlo = 0.0D+00
  xhi = 99.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_EVEN_TEST'
  write ( *, '(a)' ) '  R8VEC_EVEN computes an R8VEC containing N evenly spaced'
  write ( *, '(a)' ) '  values between XLO and XHI.'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  XLO = ', xlo
  write ( *, '(a,g14.6)' ) '  XHI = ', xhi
  write ( *, '(a,i8)' ) '  while N = ', n

  call r8vec_even ( n, xlo, xhi, x )

  call r8vec_print ( n, x, '  Resulting array:' )

  return
end
subroutine r8vec_even2_test ( )

!*****************************************************************************80
!
!! R8VEC_EVEN2_TEST tests R8VEC_EVEN2.
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

  integer ( kind = 4 ), parameter :: nold = 5
  integer ( kind = 4 ), parameter :: maxval = 20

  integer ( kind = 4 ) i
  integer ( kind = 4 ) istar
  integer ( kind = 4 ) jstar
  integer ( kind = 4 ), dimension(nold-1) :: nfill = (/ 4, 3, 5, 0 /)
  integer ( kind = 4 ) nval
  real ( kind = 8 ), dimension(nold) :: xold = (/ &
    0.0D+00, 1.0D+00, 5.0D+00, 2.0D+00, 0.0D+00 /)
  real ( kind = 8 ) xval(maxval)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_EVEN2_TEST'
  write ( *, '(a)' ) '  R8VEC_EVEN2 interpolates a specified number of '
  write ( *, '(a)' ) '  points pairs of values in a vector.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Input data:'
  write ( *, '(a)' ) ''
  do i = 1, nold
    write ( *, '(2x,g14.6)' ) xold(i)
    if ( i < nold ) then
      write ( *, '(2x,a,i10,a)' ) '(', nfill(i), ')'
    end if
  end do

  call r8vec_even2 ( maxval, nfill, nold, xold, nval, xval )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Resulting vector:'
  write ( *, '(a)' ) ''

  istar = 1
  jstar = 1
  do i = 1, nval

    if ( i == istar ) then

      write ( *, '(2x,a1,g14.6)' ) '*', xval(i)

      if ( jstar < nold ) then
        istar = istar + nfill(jstar) + 1
        jstar = jstar + 1
      end if

    else

      write ( *, '(2x,g14.6)' ) xval(i)

    end if

  end do

  return
end
subroutine r8vec_even3_test ( )

!*****************************************************************************80
!
!! R8VEC_EVEN3_TEST tests R8VEC_EVEN3.
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

  integer ( kind = 4 ), parameter :: nold = 4
  integer ( kind = 4 ), parameter :: nval = 12

  real ( kind = 8 ), dimension(nold) :: xold = (/ &
    0.0D+00, 5.1D+00, 7.0D+00, 10.0D+00 /)
  real ( kind = 8 ) xval(nval)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_EVEN3_TEST'
  write ( *, '(a)' ) '  R8VEC_EVEN3 tries to evenly interpolate new data'
  write ( *, '(a)' ) '  between old values.'

  call r8vec_print ( nold, xold, '  Original vector:' )

  call r8vec_even3 ( nold, nval, xold, xval )

  call r8vec_print ( nval, xval, '  New vector:' )

  return
end
subroutine r8vec_expand_linear_test ( )

!*****************************************************************************80
!
!! R8VEC_EXPAND_LINEAR_TEST tests R8VEC_EXPAND_LINEAR.
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

  integer ( kind = 4 ), parameter :: n = 6
  integer ( kind = 4 ), parameter :: fat = 3
  integer ( kind = 4 ), parameter :: nfat = ( n - 1 ) * ( fat + 1 ) + 1

  real ( kind = 8 ), dimension(n) :: x =  (/ &
    16.0D+00, 4.0D+00, 0.0D+00, 4.0D+00, 16.0D+00, 36.0D+00 /)
  real ( kind = 8 ) xfat(nfat)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_EXPAND_LINEAR_TEST'
  write ( *, '(a)' ) '  R8VEC_EXPAND_LINEAR linearly interpolates new data'
  write ( *, '(a)' ) '  between old values.'

  call r8vec_print ( n, x, '  Original vector:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Expansion factor is ', fat

  call r8vec_expand_linear ( n, x, fat, xfat )

  call r8vec_print ( nfat, xfat, '  Fattened vector:' )

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
!    16 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) afrac
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_FRAC_TEST'
  write ( *, '(a)' ) '  R8VEC_FRAC: K-th smallest R8VEC entry;'

  seed = 123456789

  call r8vec_uniform_01 ( n, seed, a )

  call r8vec_print ( n, a, '  Array to search:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Fractile  Value '
  write ( *, '(a)' ) ''

  do k = 1, n, n / 2

    call r8vec_frac ( n, a, k, afrac )

    write ( *, '(2x,i8,2x,g14.6)' ) k, afrac

  end do

  return
end
subroutine r8vec_heap_d_extract_test ( )

!*****************************************************************************80
!
!! R8VEC_HEAP_D_EXTRACT_TEST tests R8VEC_HEAP_D_EXTRACT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  real ( kind = 8 ) a(n_max)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_HEAP_D_EXTRACT_TEST'
  write ( *, '(a)' ) '  For a heap descending sorted R8VEC,'
  write ( *, '(a)' ) '  R8VEC_HEAP_D_EXTRACT extracts the maximum value;'

  n = 0

  seed = 123456789

  do i = 1, n_max

    b = 0.0D+00
    c = 10.0D+00

    value = r8_uniform_ab ( b, c, seed )

    call r8vec_heap_d_insert ( n, a, value )

    write ( *, '(a)' ) ''
    write ( *, '(a,f10.4)' ) '  Inserting value          ', value

    call r8vec_heap_d_max ( n, a, value )

    write ( *, '(a,f10.4)' ) '  Current maximum value is ', value

  end do

  call r8vec_print ( n, a, '  Current heap as a vector:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now extract the maximum several times.'
  write ( *, '(a)' ) ''

  do i = 1, 5
    call r8vec_heap_d_extract ( n, a, value )
    write ( *, '(a,f10.4)' ) '  Extracting maximum element = ', value
  end do

  call r8vec_print ( n, a, '  Current heap as a vector:' )

  return
end
subroutine r8vec_heap_d_insert_test ( )

!*****************************************************************************80
!
!! R8VEC_HEAP_D_INSERT_TEST tests R8VEC_HEAP_D_INSERT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  real ( kind = 8 ) a(n_max)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_HEAP_D_INSERT_TEST'
  write ( *, '(a)' ) '  For a heap descending sorted R8VEC,'
  write ( *, '(a)' ) '  R8VEC_HEAP_D_INSERT inserts a value into the heap.'

  n = 0

  seed = 123456789

  do i = 1, n_max

    b = 0.0D+00
    c = 10.0D+00

    value = r8_uniform_ab ( b, c, seed )

    call r8vec_heap_d_insert ( n, a, value )

    write ( *, '(a)' ) ''
    write ( *, '(a,f10.4)' ) '  Inserting value          ', value

    call r8vec_heap_d_max ( n, a, value )

    write ( *, '(a,f10.4)' ) '  Current maximum value is ', value

  end do

  call r8vec_print ( n, a, '  Current heap as a vector:' )

  return
end
subroutine r8vec_heap_d_max_test ( )

!*****************************************************************************80
!
!! R8VEC_HEAP_D_MAX_TEST tests R8VEC_HEAP_D_MAX
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  real ( kind = 8 ) a(n_max)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ) value

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_HEAP_D_MAX_TEST'
  write ( *, '(a)' ) '  For a heap descending sorted R8VEC,'
  write ( *, '(a)' ) '  R8VEC_HEAP_D_MAX reports the maximum value.'

  n = 0

  seed = 123456789

  do i = 1, n_max

    b = 0.0D+00
    c = 10.0D+00

    value = r8_uniform_ab ( b, c, seed )

    call r8vec_heap_d_insert ( n, a, value )

    write ( *, '(a)' ) ''
    write ( *, '(a,f10.4)' ) '  Inserting value          ', value

    call r8vec_heap_d_max ( n, a, value )

    write ( *, '(a,f10.4)' ) '  Current maximum value is ', value

  end do

  call r8vec_print ( n, a, '  Current heap as a vector:' )

  return
end
subroutine r8vec_histogram_test ( )

!*****************************************************************************80
!
!! R8VEC_HISTOGRAM_TEST tests R8VEC_HISTOGRAM.
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

  integer ( kind = 4 ), parameter :: histo_num = 20
  integer ( kind = 4 ), parameter :: n = 1000

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) :: a_hi
  real ( kind = 8 ) :: a_lo
  real ( kind = 8 ) bin_hi
  real ( kind = 8 ) bin_lo
  integer ( kind = 4 ), dimension (0:histo_num+1) :: histo_gram
  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_HISTOGRAM_TEST'
  write ( *, '(a)' ) '  R8VEC_HISTOGRAM histograms an R8VEC.'

  do test = 1, test_num

    if ( test == 1 ) then

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Uniform data:'

      a_lo =  0.0D+00
      a_hi = +1.0D+00
      call r8vec_uniform_01 ( n, seed, a )

    else if ( test == 2 ) then

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Normal data:'
      a_lo = -3.0D+00
      a_hi = +3.0D+00
      call r8vec_normal_01 ( n, seed, a )

    end if

    call r8vec_histogram ( n, a, a_lo, a_hi, histo_num, histo_gram )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Histogram of data:'
    write ( *, '(a)' ) ''

    do i = 0, histo_num+1

      if ( i == 0 ) then

        write ( *, '(2x,10x,2x,f10.4,2x,i8)' ) a_lo, histo_gram(i)

      else if ( i <= histo_num ) then

        bin_lo = ( real ( histo_num - i + 1, kind = 8 ) * a_lo   &
                 + real (             i - 1, kind = 8 ) * a_hi ) &
                 / real ( histo_num,         kind = 8 )

        bin_hi = ( real ( histo_num - i,     kind = 8 ) * a_lo   &
                 + real (             i,     kind = 8 ) * a_hi ) &
                 / real ( histo_num,         kind = 8 )

        write ( *, '(2x,f10.4,2x,f10.4,2x,i8)' ) bin_lo, bin_hi, histo_gram(i)

      else if ( i == histo_num+1 ) then

        write ( *, '(2x,f10.4,2x,10x,2x,i8)' ) a_hi, histo_gram(i)

      end if

    end do

  end do

  return
end
subroutine r8vec_house_column_test ( )

!*****************************************************************************80
!
!! R8VEC_HOUSE_COLUMN_TEST tests R8VEC_HOUSE_COLUMN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) a_col(n)
  real ( kind = 8 ) h(n,n)
  real ( kind = 8 ) ha(n,n)
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  integer ( kind = 4 ) seed
  real ( kind = 8 ) v(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_HOUSE_COLUMN_TEST'
  write ( *, '(a)' ) '  R8VEC_HOUSE_COLUMN returns the compact form of'
  write ( *, '(a)' ) '  a Householder matrix that "packs" a column'
  write ( *, '(a)' ) '  of a matrix.'
!
!  Get a random matrix.
!
  r8_lo = -5.0D+00
  r8_hi = +5.0D+00
  seed = 123456789

  call r8mat_uniform_ab ( n, n, r8_lo, r8_hi, seed, a )

  call r8mat_print ( n, n, a, '  Matrix A:' )
!
!  Zero out subdiagonal columns 1 through N-1.
!
  do k = 1, n - 1

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Working on column K = ', k

    a_col(1:n) = a(1:n,k)
    call r8vec_house_column ( n, a_col, k, v )

    call r8mat_house_form ( n, v, h )

    call r8mat_print ( n, n, h, '  Householder matrix H:' )

    ha(1:n,1:n) = matmul ( h(1:n,1:n), a(1:n,1:n) )

    call r8mat_print ( n, n, ha, '  Product H*A:' )
!
!  If we set A := HA, then we can successively convert A to upper
!  triangular form.
!
    a(1:n,1:n) = ha(1:n,1:n)

  end do

  return
end
subroutine r8vec_identity_row_test ( )

!*****************************************************************************80
!
!! R8VEC_IDENTITY_ROW_TEST tests R8VEC_IDENTITY_ROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_IDENTITY_ROW_TEST'
  write ( *, '(a)' ) '  R8VEC_IDENTITY_ROW returns a row of the identity matrix.'
  write ( *, '(a)' ) ''
  do i = 0, n + 1
    call r8vec_identity_row ( n, i, a )
    write ( *, '(i2,a,5f4.1)' ) i, ':', a(1:n)
  end do

  return
end
subroutine r8vec_index_insert_test ( )

!*****************************************************************************80
!
!! R8VEC_INDEX_INSERT_TEST tests R8VEC_INDEX_INSERT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 25

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDEX_INSERT_TEST'
  write ( *, '(a)' ) '  R8VEC_INDEX_INSERT inserts values into an'
  write ( *, '(a)' ) '  index sorted array.'

  xval = 8.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  xval = 7.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  seed = 123456789

  do i = 1, 20
    xval = r8_uniform_ab ( 0.0D+00, 20.0D+00, seed )
    xval = real ( nint ( xval ), kind = 8 )
    write ( *, '(4x,f6.2)' ) xval
    call r8vec_index_insert ( n, x, indx, xval )
  end do

  xval = 7.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  xval = 8.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) i, indx(i), x(i), x(indx(i))
  end do

  return
end
subroutine r8vec_index_delete_all_test ( )

!*****************************************************************************80
!
!! R8VEC_INDEX_DELETE_ALL_TEST tests R8VEC_INDEX_DELETE_ALL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 25

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDEX_DELETE_ALL_TEST'
  write ( *, '(a)' ) '  R8VEC_INDEX_DELETE_ALL deletes all copies of a'
  write ( *, '(a)' ) '  particular value.'

  xval = 8.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  xval = 7.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  seed = 123456789

  do i = 1, 20
    xval = r8_uniform_ab ( 0.0D+00, 20.0D+00, seed )
    xval = real ( nint ( xval ), kind = 8 )
    write ( *, '(4x,f6.2)' ) xval
    call r8vec_index_insert ( n, x, indx, xval )
  end do

  xval = 7.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  xval = 8.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) i, indx(i), x(i), x(indx(i))
  end do

  xval = 7.0D+00
  call r8vec_index_delete_all ( n, x, indx, xval )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) i, indx(i), x(i), x(indx(i))
  end do

  return
end
subroutine r8vec_index_delete_dupes_test ( )

!*****************************************************************************80
!
!! R8VEC_INDEX_DELETE_DUPES_TEST tests R8VEC_INDEX_DELETE_DUPES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 25

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDEX_DELETE_DUPES_TEST'
  write ( *, '(a)' ) '  R8VEC_INDEX_DELETE_DUPES deletes duplicates.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate some random values:'
  write ( *, '(a)' ) ''

  xval = 8.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  xval = 7.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  seed = 123456789

  do i = 1, 20
    xval = r8_uniform_ab ( 0.0D+00, 20.0D+00, seed )
    xval = real ( nint ( xval ), kind = 8 )
    write ( *, '(4x,f6.2)' ) xval
    call r8vec_index_insert ( n, x, indx, xval )
  end do

  xval = 7.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  xval = 8.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) i, indx(i), x(i), x(indx(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Call R8VEC_INDEX_DELETE_DUPES to delete duplicates:'

  call r8vec_index_delete_dupes ( n, x, indx, n, x, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of unique entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,f6.2)' ) i, indx(i), x(i)
  end do

  return
end
subroutine r8vec_index_delete_one_test ( )

!*****************************************************************************80
!
!! R8VEC_INDEX_DELETE_ONE_TEST tests R8VEC_INDEX_DELETE_ONE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 25

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDEX_DELETE_ONE_TEST'
  write ( *, '(a)' ) '  R8VEC_INDEX_DELETE_ONE deletes one copies of a'
  write ( *, '(a)' ) '  particular value.'

  xval = 8.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  xval = 7.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  seed = 123456789

  do i = 1, 20
    xval = r8_uniform_ab ( 0.0D+00, 20.0D+00, seed )
    xval = real ( nint ( xval ), kind = 8 )
    write ( *, '(4x,f6.2)' ) xval
    call r8vec_index_insert ( n, x, indx, xval )
  end do

  xval = 7.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  xval = 8.0D+00
  call r8vec_index_insert ( n, x, indx, xval )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) i, indx(i), x(i), x(indx(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Call R8VEC_INDEX_DELETE_ONE to delete one value of 8:'

  xval = 8.0D+00
  call r8vec_index_delete_one ( n, x, indx, xval, n, x, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) i, indx(i), x(i), x(indx(i))
  end do

  return
end
subroutine r8vec_index_insert_unique_test ( )

!*****************************************************************************80
!
!! R8VEC_INDEX_INSERT_UNIQUE_TEST tests R8VEC_INDEX_INSERT_UNIQUE.
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

  integer ( kind = 4 ), parameter :: n_max = 20

  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDEX_INSERT_UNIQUE_TEST'
  write ( *, '(a)' ) '  R8VEC_INDEX_INSERT_UNIQUE inserts unique values into an'
  write ( *, '(a)' ) '  index sorted array.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate some random values:'
  write ( *, '(a)' ) ''

  b = 0.0D+00
  c = real ( n_max, kind = 8 )
  seed = 123456789

  do i = 1, n_max
    xval = r8_uniform_ab ( b, c, seed )
    xval = real ( nint ( xval ), kind = 8 )
    write ( *, '(4x,f6.2)' ) xval
    call r8vec_index_insert_unique ( n, x, indx, xval )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) i, indx(i), x(i), x(indx(i))
  end do

  return
end
subroutine r8vec_index_order_test ( )

!*****************************************************************************80
!
!! R8VEC_INDEX_ORDER_TEST tests R8VEC_INDEX_ORDER.
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

  integer ( kind = 4 ), parameter :: n_max = 20

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDEX_ORDER_TEST'
  write ( *, '(a)' ) '  R8VEC_INDEX_ORDER sorts an index sorted array.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate some random values:'
  write ( *, '(a)' ) ''
  seed = 123456789

  do i = 1, N_MAX
    xval = r8_uniform_ab ( 0.0D+00, 20.0D+00, seed )
    xval = real ( nint ( xval ), kind = 8 )
    write ( *, '(4x,f6.2)' ) xval
    call r8vec_index_insert_unique ( n, x, indx, xval )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of unique entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) i, indx(i), x(i), x(indx(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now call R8VEC_INDEX_ORDER to carry out the sorting:'

  call r8vec_index_order ( n, x, indx )

  call r8vec_print ( n, x, '  X:' )

  return
end
subroutine r8vec_index_search_test ( )

!*****************************************************************************80
!
!! R8VEC_INDEX_SEARCH_TEST tests R8VEC_INDEX_SEARCH.
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

  integer ( kind = 4 ), parameter :: n_max = 20

  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) equal
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) xval

  n = 0

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDEX_SEARCH_TEST'
  write ( *, '(a)' ) '  R8VEC_INDEX_SEARCH searches for an entry '
  write ( *, '(a)' ) '  with a given value.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Generate some random values:'
  write ( *, '(a)' ) ''

  b = 0.0D+00
  c = real ( n_max, kind = 8 )
  seed = 123456789

  do i = 1, n_max
    xval = r8_uniform_ab ( b, c, seed )
    xval = real ( nint ( xval ), kind = 8 )
    write ( *, '(4x,f6.2)' ) xval
    call r8vec_index_insert_unique ( n, x, indx, xval )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Indexed list of entries:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) i, indx(i), x(i), x(indx(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Results of search for given XVAL:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  XVAL  Less Equal More'
  write ( *, '(a)' ) ''

  do i = 0, n_max
    xval = real ( i )
    call r8vec_index_search ( n, x, indx, xval, less, equal, more )
    write ( *, '(2x,f6.2,3x,i3,3x,i3,3x,i3)' ) xval, less, equal, more
  end do

  return
end
subroutine r8vec_index_sorted_range_test ( )

!*****************************************************************************80
!
!! R8VEC_INDEX_SORTED_RANGE_TEST tests R8VEC_INDEX_SORTED_RANGE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 September 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_hi
  integer ( kind = 4 ) i_lo
  integer ( kind = 4 ) indx(n)
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) r_lo
  real ( kind = 8 ) r_hi
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDEX_SORTED_RANGE_TEST'
  write ( *, '(a)' ) '  R8VEC_INDEX_SORTED_RANGE seeks the range I_LO:I_HI'
  write ( *, '(a)' ) '  of entries of sorted indexed R so that'
  write ( *, '(a)' ) '  R_LO <= R(INDX(I)) <= R_HI for I_LO <= I <= I_HI.'

  seed = 123456789

  do test = 1, 5

    call r8vec_uniform_01 ( n, seed, r )

    call r8vec_print ( n, r, '  Array' )

    call r8vec_sort_heap_index_a ( n, r, indx )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '     I  INDX    R(INDX(I))'
    write ( *, '(a)' ) ''
    do i = 1, n
      write ( *, '(2x,i4,2x,i4,2x,g14.6)' ) i, indx(i), r(indx(i))
    end do

    r_lo = r8_uniform_01 ( seed )
    r_hi = r8_uniform_01 ( seed )

    if ( r_hi < r_lo ) then
      t = r_lo
      r_lo = r_hi
      r_hi = t
    end if

    call r8vec_index_sorted_range ( n, r, indx, r_lo, r_hi, i_lo, i_hi )

    write ( *, '(a)' ) ''
    if ( i_hi < i_lo ) then
      write ( *, '(2x,a4,2x,6x,g14.6)' ) 'R_LO', r_lo
      write ( *, '(2x,a4,2x,6x,g14.6)' ) 'R_HI', r_hi
      write ( *, '(a)' ) '  Empty range in R.'
    else

      write ( *, '(2x,a4,2x,6x,g14.6)' ) 'R_LO', r_lo
      do i = i_lo, i_hi
        write ( *, '(2x,i4,2x,i4,2x,g14.6)' ) i, indx(i), r(indx(i))
      end do
      write ( *, '(2x,a4,2x,6x,g14.6)' ) 'R_HI', r_hi
    end if

  end do

  return
end
subroutine r8vec_indexed_heap_d_test ( )

!*****************************************************************************80
!
!! R8VEC_INDEXED_HEAP_D_TEST tests R8VEC_INDEXED_HEAP_D;
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

  real ( kind = 8 ) :: a(m) = (/ &
    101.0D+00, 102.0D+00, 103.0D+00, 104.0D+00, 105.0D+00, &
    106.0D+00, 107.0D+00, 108.0D+00, 109.0D+00, 110.0D+00, &
    111.0D+00, 112.0D+00, 113.0D+00, 114.0D+00, 115.0D+00, &
    116.0D+00, 117.0D+00, 118.0D+00, 119.0D+00, 120.0D+00 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: indx(n) = (/ &
    1, 11, 17, 5, 7, 13, 15, 3, 19, 9 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDEXED_HEAP_D_TEST'
  write ( *, '(a)' ) '  R8VEC_INDEXED_HEAP_D creates a descending heap'
  write ( *, '(a)' ) '  from an indexed R8VEC.'
!
!  Print before.
!
  call r8vec_print ( m, a, '  The data vector:' )
  call i4vec_print ( n, indx, '  The index vector:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX):'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
  end do
!
!  Create the heap.
!
  call r8vec_indexed_heap_d ( n, a, indx )
!
!  Print afterwards.  Only INDX should change.
!
  call r8vec_print ( m, a, '  The data vector (should NOT change):' )
  call i4vec_print ( n, indx, '  The index vector (may change):' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) is now a heap:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
  end do

  return
end
subroutine r8vec_indexed_heap_d_extract_test ( )

!*****************************************************************************80
!
!! R8VEC_INDEXED_HEAP_D_EXTRACT_TEST tests R8VEC_INDEXED_HEAP_D_EXTRACT.
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

  real ( kind = 8 ) a(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) indx_extract
  integer ( kind = 4 ) indx_insert
  integer ( kind = 4 ) indx_max
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDEXED_HEAP_D_EXTRACT_TEST'
  write ( *, '(a)' ) '  For an indexed R8VEC,'
  write ( *, '(a)' ) '  R8VEC_INDEXED_HEAP_D_EXTRACT extracts the maximum value;'
!
!  Set the data array.  To keep things easy, we will use the indicator vector.
!
  call r8vec_indicator1 ( m, a )
!
!  The index array will initially be a random subset of the numbers 1 to M,
!  in random order.
!
  n = 5
  indx(1:11) = (/ 9, 2, 8, 14, 5, 7, 15, 1, 19, 20, 3 /)

  call r8vec_print ( m, a, '  The data vector:' )
  call i4vec_print ( n, indx, '  The index vector:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX):'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
  end do
!
!  Create the descending heap.
!
  call r8vec_indexed_heap_d ( n, a, indx )

  call i4vec_print ( n, indx, '  The index vector after heaping:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after heaping:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
  end do
!
!  Insert five entries, and monitor the maximum.
!
  do i = 1, 5

    indx_insert = indx(n+1)

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  Inserting value ', a(indx_insert)

    call r8vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

    call r8vec_indexed_heap_d_max ( n, a, indx, indx_max )

    write ( *, '(a,g14.6)' ) '  Current maximum is ', a(indx_max)

  end do
  call r8vec_print ( m, a, '  The data vector after insertions:' )
  call i4vec_print ( n, indx, '  The index vector after insertions:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after insertions:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
  end do
!
!  Extract the first 5 largest elements.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now extract the maximum several times.'
  write ( *, '(a)' ) ''

  do i = 1, 5
    call r8vec_indexed_heap_d_extract ( n, a, indx, indx_extract )
    write ( *, '(a,i8,a,g14.6)' ) '  Extracting maximum element A(', &
      indx_extract,') = ', a(indx_extract)
  end do

  call r8vec_print ( m, a, '  The data vector after extractions:' )
  call i4vec_print ( n, indx, '  The index vector after extractions:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after extractions:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
  end do

  return
end
subroutine r8vec_indexed_heap_d_insert_test ( )

!*****************************************************************************80
!
!! R8VEC_INDEXED_HEAP_D_INSERT_TEST tests R8VEC_INDEXED_HEAP_D_INSERT.
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

  real ( kind = 8 ) a(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) indx_insert
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDEXED_HEAP_D_INSERT_TEST'
  write ( *, '(a)' ) '  For an indexed R8VEC,'
  write ( *, '(a)' ) '  R8VEC_INDEXED_HEAP_D_INSERT inserts a value into the heap.'
!
!  Set the data array.  To keep things easy, we will use the indicator vector.
!
  call r8vec_indicator1 ( m, a )
!
!  The index array will initially be a random subset of the numbers 1 to M,
!  in random order.
!
  n = 5
  indx(1:11) = (/ 9, 2, 8, 14, 5, 7, 15, 1, 19, 20, 3 /)

  call r8vec_print ( m, a, '  The data vector:' )
  call i4vec_print ( n, indx, '  The index vector:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX):'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
  end do
!
!  Create the descending heap.
!
  call r8vec_indexed_heap_d ( n, a, indx )

  call i4vec_print ( n, indx, '  The index vector after heaping:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after heaping:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
  end do
!
!  Insert five entries.
!
  do i = 1, 5

    indx_insert = indx(n+1)

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  Inserting value ', a(indx_insert)

    call r8vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

  end do

  call r8vec_print ( m, a, '  The data vector after insertions:' )
  call i4vec_print ( n, indx, '  The index vector after insertions:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after insertions:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
  end do

  return
end
subroutine r8vec_indexed_heap_d_max_test ( )

!*****************************************************************************80
!
!! R8VEC_INDEXED_HEAP_D_MAX_TEST tests R8VEC_INDEXED_HEAP_D_MAX.
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

  real ( kind = 8 ) a(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) indx_insert
  integer ( kind = 4 ) indx_max
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDEXED_HEAP_D_MAX_TEST'
  write ( *, '(a)' ) '  For an indexed R8VEC,'
  write ( *, '(a)' ) '  R8VEC_INDEXED_HEAP_D_MAX reports the maximum value.'
!
!  Set the data array.  To keep things easy, we will use the indicator vector.
!
  call r8vec_indicator1 ( m, a )
!
!  The index array will initially be a random subset of the numbers 1 to M,
!  in random order.
!
  n = 5
  indx(1:11) = (/ 9, 2, 8, 14, 5, 7, 15, 1, 19, 20, 3 /)

  call r8vec_print ( m, a, '  The data vector:' )
  call i4vec_print ( n, indx, '  The index vector:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX):'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
  end do
!
!  Create the descending heap.
!
  call r8vec_indexed_heap_d ( n, a, indx )

  call i4vec_print ( n, indx, '  The index vector after heaping:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after heaping:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
  end do
!
!  Insert five entries, and monitor the maximum.
!
  do i = 1, 5

    indx_insert = indx(n+1)

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  Inserting value ', a(indx_insert)

    call r8vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

    call r8vec_indexed_heap_d_max ( n, a, indx, indx_max )

    write ( *, '(a,g14.6)' ) '  Current maximum is ', a(indx_max)

  end do
  call r8vec_print ( m, a, '  The data vector after insertions:' )
  call i4vec_print ( n, indx, '  The index vector after insertions:' )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A(INDX) after insertions:'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
  end do

  return
end
subroutine r8vec_indicator0_test ( )

!*****************************************************************************80
!
!! R8VEC_INDICATOR0_TEST tests R8VEC_INDICATOR0.
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

  real ( kind = 8 ) a(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDICATOR0_TEST'
  write ( *, '(a)' ) '  R8VEC_INDICATOR0 returns an indicator vector.'

  call r8vec_indicator0 ( n, a )

  call r8vec_print ( n, a, '  The indicator0 vector:' )

  return
end
subroutine r8vec_indicator1_test ( )

!*****************************************************************************80
!
!! R8VEC_INDICATOR1_TEST tests R8VEC_INDICATOR1.
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

  real ( kind = 8 ) a(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_INDICATOR1_TEST'
  write ( *, '(a)' ) '  R8VEC_INDICATOR1 returns an indicator1 vector.'

  call r8vec_indicator1 ( n, a )

  call r8vec_print ( n, a, '  The indicator1 vector:' )

  return
end
subroutine r8vec_is_binary_test ( )

!*****************************************************************************80
!
!! R8VEC_IS_BINARY_TEST tests R8VEC_IS_BINARY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  logical r8vec_is_binary
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_IS_BINARY_TEST'
  write ( *, '(a)' ) '  R8VEC_IS_BINARY is TRUE if an R8VEC only contains'
  write ( *, '(a)' ) '  0 or 1 entries.'

  n = 3
  allocate ( x(1:n) )
  x = (/ 0.0D+00, 0.0D+00, 0.0D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_binary ( n, x ) ) then
    write ( *, '(a)' ) '  X is binary.'
  else
    write ( *, '(a)' ) '  X is NOT binary.'
  end if
  deallocate ( x )

  n = 3
  allocate ( x(1:n) )
  x = (/ 1.0D+00, 0.0D+00, 1.0D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_binary ( n, x ) ) then
    write ( *, '(a)' ) '  X is binary.'
  else
    write ( *, '(a)' ) '  X is NOT binary.'
  end if
  deallocate ( x )

  n = 3
  allocate ( x(1:n) )
  x = (/ 0.0D+00, 2.0D+00, 1.0D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_binary ( n, x ) ) then
    write ( *, '(a)' ) '  X is binary.'
  else
    write ( *, '(a)' ) '  X is NOT binary.'
  end if
  deallocate ( x )

  return
end
subroutine r8vec_is_distinct_test ( )

!*****************************************************************************80
!
!! R8VEC_IS_DISTINCT_TEST tests R8VEC_IS_DISTINCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  logical r8vec_is_distinct
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_IS_DISTINCT_TEST'
  write ( *, '(a)' ) '  R8VEC_IS_DISTINCT is TRUE if an R8VEC only contains'
  write ( *, '(a)' ) '  distinct entries.'

  n = 3
  allocate ( x(1:n) )
  x = (/ 0.0D+00, 1.0D+00, 3.0D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_distinct ( n, x ) ) then
    write ( *, '(a)' ) '  X is distinct.'
  else
    write ( *, '(a)' ) '  X is NOT distinct.'
  end if
  deallocate ( x )

  n = 3
  allocate ( x(1:n) )
  x = (/ 1.5D+00, 1.6D+00, 1.5D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_distinct ( n, x ) ) then
    write ( *, '(a)' ) '  X is distinct.'
  else
    write ( *, '(a)' ) '  X is NOT distinct.'
  end if
  deallocate ( x )

  n = 3
  allocate ( x(1:n) )
  x = (/ -1.0D+00, 1.0D+00, 10.0D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_distinct ( n, x ) ) then
    write ( *, '(a)' ) '  X is distinct.'
  else
    write ( *, '(a)' ) '  X is NOT distinct.'
  end if
  deallocate ( x )

  return
end
subroutine r8vec_is_integer_test ( )

!*****************************************************************************80
!
!! R8VEC_IS_INTEGER_TEST tests R8VEC_IS_INTEGER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  logical r8vec_is_integer
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_IS_INTEGER_TEST'
  write ( *, '(a)' ) '  R8VEC_IS_INTEGER is TRUE if an R8VEC only contains'
  write ( *, '(a)' ) '  integer entries.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Example 1: Obviously integer:'
  write ( *, '(a)' ) ''
  n = 6
  allocate ( x(1:n) )
  x = (/ 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 6.0D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_integer ( n, x ) ) then
    write ( *, '(a)' ) '  X is integer.'
  else
    write ( *, '(a)' ) '  X is NOT integer.'
  end if
  deallocate ( x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Obviously NOT integer:'
  write ( *, '(a)' ) ''
  n = 6
  allocate ( x(1:n) )
  x = (/ 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 6.5D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_integer ( n, x ) ) then
    write ( *, '(a)' ) '  X is integer.'
  else
    write ( *, '(a)' ) '  X is NOT integer.'
  end if
  deallocate ( x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Example 3: Not Integer, Not obvious:'
  write ( *, '(a)' ) ''
  n = 6
  allocate ( x(1:n) )
  x = (/ 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 6.0D+00 /)
  x(5) = x(5) + 0.0000001D+00
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_integer ( n, x ) ) then
    write ( *, '(a)' ) '  X is integer.'
  else
    write ( *, '(a)' ) '  X is NOT integer.'
  end if
  deallocate ( x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Example 4: Not Integer, Not obvious:'
  write ( *, '(a)' ) ''
  n = 6
  allocate ( x(1:n) )
  x = (/ 1.0D+00, 2.0D+00, 300000000.2D+00, 4.0D+00, 5.0D+00, 6.0D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_integer ( n, x ) ) then
    write ( *, '(a)' ) '  X is integer.'
  else
    write ( *, '(a)' ) '  X is NOT integer.'
  end if
  deallocate ( x )

  return
end
subroutine r8vec_legendre_test ( )

!*****************************************************************************80
!
!! R8VEC_LEGENDRE_TEST tests R8VEC_LEGENDRE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 June 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_LEGENDRE_TEST'
  write ( *, '(a)' ) '  R8VEC_LEGENDRE computes N Legendre points in [R1,R2].'

  r1 = -1.0D+00
  r2 = +1.0D+00
  n = 5
  allocate ( r(1:n) )

  call r8vec_legendre ( n, r1, r2, r )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4,a,g14.6,a,g14.6)' ) '  N = ', n, '  R1 = ', r1, '  R2 = ', r2

  call r8vec_print ( n, r, '  Legendre points:' )

  deallocate ( r )

  r1 =   0.0D+00
  r2 = +10.0D+00
  n = 7

  allocate ( r(1:n) )

  call r8vec_legendre ( n, r1, r2, r )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4,a,g14.6,a,g14.6)' ) '  N = ', n, '  R1 = ', r1, '  R2 = ', r2

  call r8vec_print ( n, r, '  Legendre points:' )

  deallocate ( r )

  return
end
subroutine r8vec_linspace_test ( )

!*****************************************************************************80
!
!! R8VEC_LINSPACE_TEST tests R8VEC_LINSPACE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_LINSPACE_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_LINSPACE: evenly spaced points between A and B;'

  a = 10.0D+00
  b = 20.0D+00

  call r8vec_linspace ( n, a, b, x )
  call r8vec_print ( n, x, '  r8vec_linspace ( 5, 10, 20 )' )

  return
end
subroutine r8vec_linspace2_test ( )

!*****************************************************************************80
!
!! R8VEC_LINSPACE2_TEST tests R8VEC_LINSPACE2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_LINSPACE2_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_LINSPACE2: R8VEC_LINSPACE, but no endpoints;'

  a = 10.0D+00
  b = 20.0D+00

  call r8vec_linspace2 ( n, a, b, x )
  call r8vec_print ( n, x, '  r8vec_linspace2 ( 5, 10, 20 )' )

  return
end
subroutine r8vec_max_test ( )

!*****************************************************************************80
!
!! R8VEC_MAX_TEST tests R8VEC_MAX.
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

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) aval
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8vec_max
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MAX_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_MAX:       maximum entry;'

  b = - real ( n, kind = 8 )
  c = real ( n, kind = 8 )

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  aval = r8vec_max ( n, a )
  write ( *, '(a,g14.6)' ) '  Maximum:                 ', aval

  return
end
subroutine r8vec_max_abs_index_test ( )

!*****************************************************************************80
!
!! R8VEC_MAX_ABS_INDEX_TEST tests R8VEC_MAX_ABS_INDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) ival
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MAX_ABS_INDEX_TEST'
  write ( *, '(a)' ) '  R8VEC_MAX_ABS_INDEX: index of entry of maximum absolute value;'

  r8_lo = - 10.0D+00
  r8_hi = + 10.0D+00

  seed = 123456789

  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call r8vec_max_abs_index ( n, a, ival )
  write ( *, '(a,i8)' ) '  Index of entry of maximum absolute value: ', ival

  return
end
subroutine r8vec_max_index_test ( )

!*****************************************************************************80
!
!! R8VEC_MAX_INDEX_TEST tests R8VEC_MAX_INDEX.
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

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MAX_INDEX_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_MAX_INDEX: index of maximum entry;'

  b = - real ( n, kind = 8 )
  c = real ( n, kind = 8 )

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call r8vec_max_index ( n, a, ival )
  write ( *, '(a,i8)' ) '  Maximum index:           ', ival

  return
end
subroutine r8vec_mean_test ( )

!*****************************************************************************80
!
!! R8VEC_MEAN_TEST tests R8VEC_MEAN.
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

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) mean
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MEAN_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_MEAN:      mean value;'

  b = - real ( n, kind = 8 )
  c = real ( n, kind = 8 )

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call r8vec_mean ( n, a, mean )
  write ( *, '(a,g14.6)' ) '  Mean:                     ', mean

  return
end
subroutine r8vec_mean_geometric_test ( )

!*****************************************************************************80
!
!! R8VEC_MEAN_GEOMETRIC_TEST tests R8VEC_MEAN_GEOMETRIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) mean_geometric
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MEAN_GEOMETRIC_TEST'
  write ( *, '(a)' ) '  R8VEC_MEAN_GEOMETRIC computes the geometric mean of'
  write ( *, '(a)' ) '  an R8VEC.'

  b = 0.0D+00
  c = 5.0D+00

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call r8vec_mean_geometric ( n, a, mean_geometric )
  write ( *, '(a,g14.6)' ) '  Geometric mean: ', mean_geometric

  return
end
subroutine r8vec_mean_running_test ( )

!*****************************************************************************80
!
!! R8VEC_MEAN_RUNNING_TEST tests R8VEC_MEAN_RUNNING.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)
  real ( kind = 8 ), allocatable :: s(:)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MEAN_RUNNING_TEST'
  write ( *, '(a)' ) &
    '  R8VEC_MEAN_RUNNING returns the running averages of an R8VEC.'

  n = 10
  a = -5.0D+00
  b = +10.0D+00
  seed = 123456789

  allocate ( r(1:n) )
  call r8vec_uniform_ab ( n, a, b, seed, r )

  call r8vec_print ( n, r, '  Random R8VEC:' )

  allocate ( s(1:n+1) )
  call r8vec_mean_running ( n, r, s )

  call r8vec_print ( n + 1, s, '  Running averages:' )

  deallocate ( r )
  deallocate ( s )

  return
end
subroutine r8vec_mean_update_test ( )

!*****************************************************************************80
!
!! R8VEC_MEAN_UPDATE_TEST tests R8VEC_MEAN_UPDATE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 November 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  real ( kind = 8 ) a(n_max)
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  real ( kind = 8 ) mean_n
  real ( kind = 8 ) mean_nm1
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MEAN_UPDATE_TEST'
  write ( *, '(a)' ) '  R8VEC_MEAN_UPDATE updates the mean of a vector'
  write ( *, '(a)' ) '  when one more element is added.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N      R               MEAN         MEAN_UPDATE'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, n_max

    r = r8_uniform_01 ( seed )
    a(i) = r
    call r8vec_mean ( i, a, mean )
    
    nm1 = i - 1
    mean_nm1 = mean_n
    call r8vec_mean_update ( nm1, mean_nm1, r, n, mean_n )

    write ( *, '(2x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) i, r, mean, mean_n

  end do

  return
end
subroutine r8vec_median_test ( )

!*****************************************************************************80
!
!! R8VEC_MEDIAN_TEST tests R8VEC_MEDIAN;
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

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) median
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MEDIAN_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_MEDIAN:    median value;'

  b = - real ( n, kind = 8 )
  c = real ( n, kind = 8 )

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call r8vec_median ( n, a, median )
  write ( *, '(a,g14.6)' ) '  Median:                   ', median

  return
end
subroutine r8vec_midspace_test ( )

!*****************************************************************************80
!
!! R8VEC_MIDSPACE_TEST tests R8VEC_MIDSPACE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MIDSPACE_TEST'
  write ( *, '(a)' ) '  R8VEC_MIDSPACE: evenly spaced midpoints between A and B'

  a = 10.0D+00
  b = 20.0D+00

  call r8vec_midspace ( n, a, b, x )
  call r8vec_print ( n, x, '  r8vec_midspace ( 5, 10, 20 )' )

  return
end
subroutine r8vec_min_test ( )

!*****************************************************************************80
!
!! R8VEC_MIN_TEST tests R8VEC_MIN.
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

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) aval
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8vec_min
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MIN_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_MIN:       minimum entry.'

  b = - real ( n, kind = 8 )
  c = real ( n, kind = 8 )

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  aval = r8vec_min ( n, a )
  write ( *, '(a,g14.6)' ) '  Minimum:                 ', aval

  return
end
subroutine r8vec_min_index_test ( )

!*****************************************************************************80
!
!! R8VEC_MIN_INDEX_TEST tests R8VEC_MIN_INDEX.
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

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MIN_INDEX_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_MIN_INDEX: index of minimum entry;'

  b = - real ( n, kind = 8 )
  c = real ( n, kind = 8 )

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  call r8vec_min_index ( n, a, ival )
  write ( *, '(a,i8)' ) '  Minimum index:           ', ival

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
subroutine r8vec_mirror_ab_next_test ( )

!*****************************************************************************80
!
!! R8VEC_MIRROR_AB_NEXT_TEST tests R8VEC_MIRROR_AB_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3

  real ( kind = 8 ) a(m)
  real ( kind = 8 ) b(m)
  logical done
  integer ( kind = 4 ) i
  character ( len = 5 ) title
  real ( kind = 8 ) x(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_MIRROR_AB_NEXT_TEST'
  write ( *, '(a)' ) '  R8VEC_MIRROR_AB_NEXT generates all versions of'
  write ( *, '(a)' ) '  of a real vector X mirrored by A and B.'

  a = (/ -0.5, -0.5, -0.5 /)
  x = (/  0.0,  0.0,  0.0 /)
  b = (/  0.5,  0.5,  0.5 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Case 1: 3x3x3 possibilities:'
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( m, a, '   A:' )
  call r8vec_transpose_print ( m, x, '   X:' )
  call r8vec_transpose_print ( m, b, '   B:' )
  write ( *, '(a)' ) ''

  i = 0
  done = .true.

  do

    call r8vec_mirror_ab_next ( m, a, b, x, done )

    if ( done ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Done.'
      exit
    end if

    i = i + 1
    write ( title, '(2x,i2,'':'')' ) i

    call r8vec_transpose_print ( m, x, title )

  end do

  a = (/ -0.5, -0.5, -0.5 /)
  x = (/  0.0,  0.5,  0.0 /)
  b = (/  0.5,  0.5,  0.5 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Case 2: 3x2x3 possibilities:'
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( m, a, '   A:' )
  call r8vec_transpose_print ( m, x, '   X:' )
  call r8vec_transpose_print ( m, b, '   B:' )
  write ( *, '(a)' ) ''

  i = 0
  done = .true.

  do

    call r8vec_mirror_ab_next ( m, a, b, x, done )

    if ( done ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Done.'
      exit
    end if

    i = i + 1
    write ( title, '(2x,i2,'':'')' ) i

    call r8vec_transpose_print ( m, x, title )

  end do

  a = (/  0.0, -0.5, -0.5 /)
  x = (/  0.0,  0.0,  0.0 /)
  b = (/  0.0,  0.5,  0.5 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Case 3: 1x3x3 possibilities:'
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( m, a, '   A:' )
  call r8vec_transpose_print ( m, x, '   X:' )
  call r8vec_transpose_print ( m, b, '   B:' )
  write ( *, '(a)' ) ''

  i = 0
  done = .true.

  do

    call r8vec_mirror_ab_next ( m, a, b, x, done )

    if ( done ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Done.'
      exit
    end if

    i = i + 1
    write ( title, '(2x,i2,'':'')' ) i

    call r8vec_transpose_print ( m, x, title )

  end do

  return
end
subroutine r8vec_nint_test ( )

!*****************************************************************************80
!
!! R8VEC_NINT_TEST tests R8VEC_NINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
 
  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_NINT_TEST'
  write ( *, '(a)' ) '  R8VEC_NINT rounds an R8VEC.'

  x1 = -5.0D+00
  x2 = +5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, x1, x2, seed, a )
  call r8vec_print ( n, a, '  Vector A:' )
  call r8vec_nint ( n, a )
  call r8vec_print ( n, a, '  Rounded vector A:' )

  return
end
subroutine r8vec_norm_test ( )

!*****************************************************************************80
!
!! R8VEC_NORM_TEST tests R8VEC_NORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8vec_norm
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_NORM_TEST'
  write ( *, '(a)' ) '  R8VEC_NORM computes the L2 norm of an R8VEC.'

  r8_lo = - 5.0D+00
  r8_hi = +10.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )

  call r8vec_print ( n, x, '  Vector X:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  R8VEC_NORM ( X ) = ', r8vec_norm ( n, x )

  return
end
subroutine r8vec_norm_affine_test ( )

!*****************************************************************************80
!
!! R8VEC_NORM_AFFINE_TEST tests R8VEC_NORM_AFFINE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  real ( kind = 8 ) r8vec_norm
  real ( kind = 8 ) r8vec_norm_affine
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_NORM_AFFINE_TEST'
  write ( *, '(a)' ) '  R8VEC_NORM_AFFINE computes the L2 norm of '
  write ( *, '(a)' ) '  the difference of two R8VECs.'

  r8_lo = - 5.0D+00
  r8_hi = +10.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
  z(1:n) = x(1:n) - y(1:n)

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  R8VEC_NORM_AFFINE(X,Y) = ', r8vec_norm_affine ( n, x, y )
  write ( *, '(a,g14.6)' ) '  R8VEC_NORM (X-Y):        ', r8vec_norm ( n, z )

  return
end
subroutine r8vec_norm_l0_test ( )

!*****************************************************************************80
!
!! R8VEC_NORM_L0_TEST tests R8VEC_NORM_L0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_hi
  real ( kind = 8 ) a_lo
  real ( kind = 8 ) r8vec_norm_l0
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_NORM_L0_TEST'
  write ( *, '(a)' ) '  R8VEC_NORM_L0 computes the L0 "norm" of an R8VEC.'

  a_lo = - 2.0D+00
  a_hi = + 2.0D+00

  seed = 123456789

  call r8vec_uniform_ab ( n, a_lo, a_hi, seed, a )

  call r8vec_nint ( n, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  write ( *, '(a,g14.6)' ) '  L0 norm:                 ', r8vec_norm_l0 ( n, a )

  return
end
subroutine r8vec_norm_l1_test ( )

!*****************************************************************************80
!
!! R8VEC_NORM_L1_TEST tests R8VEC_NORM_L1.
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

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8vec_norm_l1
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_NORM_L1_TEST'
  write ( *, '(a)' ) '  R8VEC_NORM_L1 computes the L1 norm of an R8VEC.'

  b = - real ( n, kind = 8 )
  c = real ( n, kind = 8 )

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  write ( *, '(a,g14.6)' ) '  L1 norm:                 ', r8vec_norm_l1 ( n, a )

  return
end
subroutine r8vec_norm_l2_test ( )

!*****************************************************************************80
!
!! R8VEC_NORM_L2_TEST tests R8VEC_NORM_L2.
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

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8vec_norm_l2
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_NORM_L2_TEST'
  write ( *, '(a)' ) '  R8VEC_NORM_L2 computes the L2 norm of an R8VEC.'

  b = - real ( n, kind = 8 )
  c = real ( n, kind = 8 )

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  write ( *, '(a,g14.6)' ) '  L2 norm:                 ', r8vec_norm_l2 ( n, a )

  return
end
subroutine r8vec_norm_li_test ( )

!*****************************************************************************80
!
!! R8VEC_NORM_LI_TEST tests R8VEC_NORM_LI.
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

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8vec_norm_li
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_NORM_LI_TEST'
  write ( *, '(a)' ) '  R8VEC_NORM_LI computes the Loo norm of an R8VEC.'

  b = - real ( n, kind = 8 )
  c = real ( n, kind = 8 )

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  write ( *, '(a,g14.6)' ) '  L-Infinity norm:         ', r8vec_norm_li ( n, a )

  return
end
subroutine r8vec_normal_01_test ( )

!*****************************************************************************80
!
!! R8VEC_NORMAL_01_TEST tests R8VEC_NORMAL_01.
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

  integer ( kind = 4 ), parameter :: n_max = 1000

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_mean
  real ( kind = 8 ) x_min
  real ( kind = 8 ) x_var

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_NORMAL_01_TEST'
  write ( *, '(a)' ) '  R8VEC_NORMAL_01 computes a vector of normally'
  write ( *, '(a)' ) '  distributed random numbers.'
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed
!
!  Test 1:
!  Simply call 5 times for 1 value, and print.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test #1: Call 5 times, 1 value each time.'
  write ( *, '(a)' ) ''

  n = 1
  do i = 1, 5
    call r8vec_normal_01 ( n, seed, x )
    write ( *, '(2x,i8,g14.6)' ) i, x(1)
  end do
!
!  Test 2:
!  Restore the random number seed, and repeat.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test #2: Restore the random number seed.'
  write ( *, '(a)' ) '  Call 5 times, 1 value each time.'
  write ( *, '(a)' ) '  The results should be identical.'
  write ( *, '(a)' ) ''

  seed = 123456789

  n = 1
  do i = 1, 5
    call r8vec_normal_01 ( n, seed, x )
    write ( *, '(2x,i8,g14.6)' ) i, x(1)
  end do
!
!  Test 3:
!  Restore the random number seed, compute all 5 values at once.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test #3: Restore the random number seed.'
  write ( *, '(a)' ) '  Call 1 time for 5 values.'
  write ( *, '(a)' ) '  The results should be identical.'
  write ( *, '(a)' ) ''

  seed = 123456789

  n = 5
  call r8vec_normal_01 ( n, seed, x )

  do i = 1, n
    write ( *, '(2x,i8,g14.6)' ) i, x(i)
  end do
!
!  Test 4:
!  Restore the random number seed, compute all 5 values at once.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test #4: Restore the random number seed.'
  write ( *, '(a)' ) '  Call for 2, 1, and 2 values.'
  write ( *, '(a)' ) '  The results should be identical.'
  write ( *, '(a)' ) ''

  seed = 123456789

  n = 2
  call r8vec_normal_01 ( n, seed, x )

  do i = 1, n
    write ( *, '(2x,i8,g14.6)' ) i, x(i)
  end do

  n = 1
  call r8vec_normal_01 ( n, seed, x )

  do i = 1, n
    write ( *, '(2x,i8,g14.6)' ) i, x(i)
  end do

  n = 2
  call r8vec_normal_01 ( n, seed, x )

  do i = 1, n
    write ( *, '(2x,i8,g14.6)' ) i, x(i)
  end do
!
!  Test 5:
!  Determine the minimum, maximum, mean and variance.
!
  n = n_max
  call r8vec_normal_01 ( n, seed, x )
  x_min = minval ( x(1:n) )
  x_max = maxval ( x(1:n) )
  x_mean = sum ( x(1:n) ) / real ( n, kind = 8 )
  x_var = sum ( ( x(1:n) - x_mean )**2 ) / real ( n - 1, kind = 8 )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Test #5:'
  write ( *, '(a,i12)' ) '  Number of samples was ', n
  write ( *, '(a,g14.6)' ) '  Minimum value was ', x_min
  write ( *, '(a,g14.6)' ) '  Maximum value was ', x_max
  write ( *, '(a,g14.6)' ) '  Average value was ', x_mean
  write ( *, '(a,g14.6)' ) '  Variance was      ', x_var
  write ( *, '(a,g14.6)' ) '  Expected average  ', 0.0D+00
  write ( *, '(a,g14.6)' ) '  Expected variance ', 1.0D+00

  return
end
subroutine r8vec_normal_ab_test ( )

!*****************************************************************************80
!
!! R8VEC_NORMAL_AB_TEST tests R8VEC_NORMAL_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) mu
  real ( kind = 8 ) r(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_NORMAL_AB_TEST'
  write ( *, '(a)' ) '  R8VEC_NORMAL_AB computes a vector of Normal AB values.'

  mu = 15.0D+00
  sigma = 0.25D+00
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  MU = ', mu
  write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
  write ( *, '(a,i12)' ) '  SEED = ', seed

  call r8vec_normal_ab ( n, mu, sigma, seed, r )

  call r8vec_print ( n, r, '  Vector of Normal AB values:' )
  
  return
end
subroutine r8vec_normalize_l1_test ( )

!*****************************************************************************80
!
!! R8VEC_NORMALIZE_L1_TEST tests R8VEC_NORMALIZE_L1.
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

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_NORMALIZE_L1_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_NORMALIZE_L1:  make unit sum;'

  b = - real ( n, kind = 8 )
  c = real ( n, kind = 8 )

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  call r8vec_normalize_l1 ( n, a )

  call r8vec_print ( n, a, '  After calling R8VEC_NORMALIZE_L1:' )

  return
end
subroutine r8vec_order_type_test ( )

!*****************************************************************************80
!
!! R8VEC_ORDER_TYPE_TEST tests R8VEC_ORDER_TYPE.
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

  integer ( kind = 4 ), parameter :: n = 4
  integer ( kind = 4 ), parameter :: test_num = 6

  integer ( kind = 4 ) j
  integer ( kind = 4 ) order
  integer ( kind = 4 ) test
  real ( kind = 8 ) x(n)
  real ( kind = 8 ), dimension ( n, test_num ) :: x_test = reshape ( (/ &
    1.0D+00, 3.0D+00, 2.0D+00, 4.0D+00, &
    2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, &
    1.0D+00, 2.0D+00, 2.0D+00, 4.0D+00, &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, &
    4.0D+00, 4.0D+00, 3.0D+00, 1.0D+00, &
    9.0D+00, 7.0D+00, 3.0D+00, 0.0D+00 /), (/ n, test_num /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_ORDER_TYPE_TEST'
  write ( *, '(a)' ) '  R8VEC_ORDER_TYPE classifies a R8VEC as'
  write ( *, '(a)' ) '  -1: no order'
  write ( *, '(a)' ) '   0: all equal;'
  write ( *, '(a)' ) '   1: ascending;'
  write ( *, '(a)' ) '   2: strictly ascending;'
  write ( *, '(a)' ) '   3: descending;'
  write ( *, '(a)' ) '   4: strictly descending.'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    x(1:n) = x_test(1:n,test)

    call r8vec_order_type ( n, x, order )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  The following vector has order type ', order
    write ( *, '(a)' ) ''
    do j = 1, n
      write ( *, '(i8,g14.6)' ) j, x(j)
    end do

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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_PERMUTE_TEST'
  write ( *, '(a)' ) '  R8VEC_PERMUTE permutes a R8VEC in place.'

  call r8vec_print ( n, x, '  Original array X[]' )

  call i4vec_print ( n, p, '  Permutation vector P[]' )

  call r8vec_permute ( n, p, x )

  call r8vec_print ( n, x, '  Permuted array X[P[]]:' )

  return
end
subroutine r8vec_permute_cyclic_test ( )

!*****************************************************************************80
!
!! R8VEC_PERMUTE_CYCLIC_TEST tests R8VEC_PERMUTE_CYCLIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) k
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_PERMUTE_CYCLIC_TEST'
  write ( *, '(a)' ) '  R8VEC_PERMUTE_CYCLIC performs a cyclic permutation'
  write ( *, '(a)' ) '  by K positions on an R8VEC.'

  k = 4
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  K = ', k

  call r8vec_indicator1 ( n, x )

  call r8vec_print ( n, x, '  Original array' )

  call r8vec_permute_cyclic ( n, k, x )

  call r8vec_print ( n, x, '  Array after cyclic permutation:' )

  return
end
subroutine r8vec_permute_uniform_test ( )

!*****************************************************************************80
!
!! R8VEC_PERMUTE_UNIFORM_TEST tests R8VEC_PERMUTE_UNIFORM.
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

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_PERMUTE_UNIFORM_TEST'
  write ( *, '(a)' ) '  R8VEC_PERMUTE_UNIFORM randomly reorders an R8VEC.'

  do i = 1, n
    a(i) = real ( 100 + i, kind = 8 )
  end do

  seed = 123456789

  call r8vec_print ( n, a, '  A, before permutation:' )

  call r8vec_permute_uniform ( n, a, seed )

  call r8vec_print ( n, a, '  A, after random permutation:' )

  return
end
subroutine r8vec_polarize_test ( )

!*****************************************************************************80
!
!! R8VEC_POLARIZE_TEST tests R8VEC_POLARIZE.
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

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ), dimension ( n ) :: a = (/ &
    1.0D+00, 2.0D+00,  3.0D+00 /)
  real ( kind = 8 ) a2(n)
  real ( kind = 8 ) a_normal(n)
  real ( kind = 8 ) a_parallel(n)
  real ( kind = 8 ) ap_norm
  real ( kind = 8 ), dimension ( n ) :: p = (/ &
    3.0D+00, 1.0D+00, -2.0D+00 /)
  real ( kind = 8 ) p_norm
  real ( kind = 8 ) pan
  real ( kind = 8 ) pap

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_POLARIZE_TEST'
  write ( *, '(a)' ) '  R8VEC_POLARIZE decomposes a vector into'
  write ( *, '(a)' ) '  components parallel and normal to a direction.'

  call r8vec_print ( n, a, '  Original vector:' )

  call r8vec_print ( n, p, '  Direction vector:' )

  call r8vec_polarize ( n, a, p, a_normal, a_parallel )

  call r8vec_print ( n, a_normal, '  Normal component:' )

  call r8vec_print ( n, a_parallel, '  Parallel component:' )

  pan = dot_product ( p(1:n), a_normal(1:n) )

  p_norm = sqrt ( sum ( p(1:n)**2 ) )
  ap_norm = sqrt ( sum ( a_parallel(1:n)**2 ) )

  pap = dot_product ( p(1:n), a_parallel(1:n) ) / ( p_norm * ap_norm )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) &
    '  Dot product of P and A_normal (should be 0) ', pan
  write ( *, '(a,g14.6)' ) &
    '  Cosine of angle between P and A_parallel (should be 1 or -1) ', pap

  a2(1:n) = a_normal(1:n) + a_parallel(1:n)

  call r8vec_print ( n, a2, '  Sum of components (should equal A):' )

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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_PRINT_TEST'
  write ( *, '(a)' ) '  R8VEC_PRINT prints an R8VEC.'

  call r8vec_print ( n, a, '  The R8VEC:' )

  return
end
subroutine r8vec_print_part_test ( )

!*****************************************************************************80
!
!! R8VEC_PRINT_PART_TEST tests R8VEC_PRINT_PART.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 100

  real ( kind = 8 ), dimension ( n ) :: a

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_PRINT_PART_TEST'
  write ( *, '(a)' ) '  R8VEC_PRINT_PART prints part of an R8VEC.'

  call r8vec_indicator1 ( n, a )

  call r8vec_print_part ( n, a, 10, 20, '  Entries 10:20 of the vector:' )

  return
end
subroutine r8vec_print_some_test ( )

!*****************************************************************************80
!
!! R8VEC_PRINT_SOME_TEST tests R8VEC_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 100

  real ( kind = 8 ), dimension ( n ) :: a
  integer ( kind = 4 ) max_print

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8VEC_PRINT_SOME prints some of an R8VEC.'

  call r8vec_indicator1 ( n, a )

  max_print = 10
  call r8vec_print_some ( n, a, max_print, '  No more than 10 lines of the vector:' )

  return
end
subroutine r8vec_reverse_test ( )

!*****************************************************************************80
!
!! R8VEC_REVERSE_TEST tests R8VEC_REVERSE.
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_REVERSE_TEST'
  write ( *, '(a)' ) '  R8VEC_REVERSE reverses a R8VEC.'

  call r8vec_indicator1 ( n, a )

  call r8vec_print ( n, a, '  Original array:' )

  call r8vec_reverse ( n, a )

  call r8vec_print ( n, a, '  Reversed array:' )

  a(1:n) = a(n:1:-1)

  call r8vec_print ( n, a, '  Re-reversed array using a(1:n) = a(n:1:-1)' )

  return
end
subroutine r8vec_rotate_test ( )

!*****************************************************************************80
!
!! R8VEC_ROTATE_TEST tests R8VEC_ROTATE.
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ), dimension ( n ) :: a = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /)
  integer ( kind = 4 ) m

  m = 2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_ROTATE_TEST'
  write ( *, '(a)' ) '  R8VEC_ROTATE rotates a R8VEC in place.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i8,a)' ) '  Rotate entries ', m, ' places to the right.'

  call r8vec_print ( n, a, '  Original array:' )

  call r8vec_rotate ( n, a, m )

  call r8vec_print ( n, a, '  Rotated array:' )

  return
end
subroutine r8vec_rsquared_test ( )

!*****************************************************************************80
!
!! R8VEC_RSQUARED_TEST tests R8VEC_RSQUARED.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 January 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11

  real ( kind = 8 ) r8vec_rsquared
  real ( kind = 8 ) rsquared
  real ( kind = 8 ), dimension ( n ) :: y_model = (/ &
     0.00D+00,  9.00D+00, 16.00D+00, 21.00D+00, 24.00D+00, &
    25.00D+00, 24.00D+00, 21.00D+00, 16.00D+00,  9.00D+00,  &
     0.00D+00 /)
  real ( kind = 8 ), dimension ( n ) :: y_data = (/ &
     0.00D+00,  9.58D+00, 16.76D+00, 21.52D+00, 24.38D+00, &
    24.97D+00, 22.90D+00, 20.45D+00, 12.40D+00,  7.65D+00, &
    -3.82D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_RSQUARED_TEST'
  write ( *, '(a)' ) '  R8VEC_RSQUARED evaluates the R^2 goodness-of-fit statistic.'

  call r8vec2_print ( n, y_data, y_model, "  Data and model:" );

  rsquared = r8vec_rsquared ( n, y_data, y_model );
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Computed R^2 is ', rsquared

  return
end
subroutine r8vec_rsquared_adjusted_test ( )

!*****************************************************************************80
!
!! R8VEC_RSQUARED_ADJUSTED_TEST tests R8VEC_RSQUARED_ADJUSTED.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11

  integer ( kind = 4 ), parameter :: degree = 2
  real ( kind = 8 ) r8vec_rsquared_adjusted
  real ( kind = 8 ) rsquared_adjusted
  real ( kind = 8 ), dimension ( n ) :: y_model = (/ &
     0.00D+00,  9.00D+00, 16.00D+00, 21.00D+00, 24.00D+00, &
    25.00D+00, 24.00D+00, 21.00D+00, 16.00D+00,  9.00D+00,  &
     0.00D+00 /)
  real ( kind = 8 ), dimension ( n ) :: y_data = (/ &
     0.00D+00,  9.58D+00, 16.76D+00, 21.52D+00, 24.38D+00, &
    24.97D+00, 22.90D+00, 20.45D+00, 12.40D+00,  7.65D+00, &
    -3.82D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_RSQUARED_ADJUSTED_TEST'
  write ( *, '(a)' ) '  R8VEC_RSQUARED_ADJUSTED evaluates the adjusted R^2 goodness-of-fit statistic.'

  call r8vec2_print ( n, y_data, y_model, "  Data and model:" )

  rsquared_adjusted = r8vec_rsquared_adjusted ( n, y_data, y_model, degree )
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,g14.6)' ) &
    '  Computed R^2 for degree ', degree, ' is ', rsquared_adjusted

  return
end
subroutine r8vec_scale_01_test ( )

!*****************************************************************************80
!
!! R8VEC_SCALE_01_TEST tests R8VEC_SCALE_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 October 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ) xs(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SCALE_01_TEST'
  write ( *, '(a)' ) '  R8VEC_SCALE_01 shifts and scales an R8VEC so that'
  write ( *, '(a)' ) '  it has min 0 and max 1'

  a = -5.0D+00
  b = 15.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, a, b, seed, x )
 
  call r8vec_print ( n, x, '  Vector X:' )
  call r8vec_mean ( n, x, mu )
  call r8vec_std ( n, x, sigma )
  xmax = maxval ( x )
  xmin = minval ( x )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  mean(X) = ', mu
  write ( *, '(a,g14.6)' ) '  std(X)  = ', sigma
  write ( *, '(a,g14.6)' ) '  max(X)  = ', xmax
  write ( *, '(a,g14.6)' ) '  min(X)  = ', xmin

  call r8vec_scale_01 ( n, x, xs )

  call r8vec_print ( n, xs, '  Vector XS:' )
  call r8vec_mean ( n, xs, mu )
  call r8vec_std ( n, xs, sigma )
  xmax = maxval ( xs )
  xmin = minval ( xs )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  mean(XS) = ', mu
  write ( *, '(a,g14.6)' ) '  std(XS)  = ', sigma
  write ( *, '(a,g14.6)' ) '  max(XS)  = ', xmax
  write ( *, '(a,g14.6)' ) '  min(XS)  = ', xmin

  return
end
subroutine r8vec_scale_ab_test ( )

!*****************************************************************************80
!
!! R8VEC_SCALE_AB_TEST tests R8VEC_SCALE_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 October 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ) xs(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SCALE_AB_TEST'
  write ( *, '(a)' ) '  R8VEC_SCALE_AB shifts and scales an R8VEC so that'
  write ( *, '(a)' ) '  it has min A and max B'

  a = -5.0D+00
  b = 15.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, a, b, seed, x )
 
  call r8vec_print ( n, x, '  Vector X:' )
  call r8vec_mean ( n, x, mu )
  call r8vec_std ( n, x, sigma )
  xmax = maxval ( x )
  xmin = minval ( x )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  mean(X) = ', mu
  write ( *, '(a,g14.6)' ) '  std(X)  = ', sigma
  write ( *, '(a,g14.6)' ) '  max(X)  = ', xmax
  write ( *, '(a,g14.6)' ) '  min(X)  = ', xmin

  a = -1.0D+00
  b = +1.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  Using interval [', a, ',', b, ']'

  call r8vec_scale_ab ( n, x, a, b, xs )

  call r8vec_print ( n, xs, '  Vector XS:' )
  call r8vec_mean ( n, xs, mu )
  call r8vec_std ( n, xs, sigma )
  xmax = maxval ( xs )
  xmin = minval ( xs )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  mean(XS) = ', mu
  write ( *, '(a,g14.6)' ) '  std(XS)  = ', sigma
  write ( *, '(a,g14.6)' ) '  max(XS)  = ', xmax
  write ( *, '(a,g14.6)' ) '  min(XS)  = ', xmin

  return
end
subroutine r8vec_search_binary_a_test ( )

!*****************************************************************************80
!
!! R8VEC_SEARCH_BINARY_A_TEST tests R8VEC_SEARCH_BINARY_A;
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

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) indx
  real ( kind = 8 ) search_val
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SEARCH_BINARY_A_TEST'
  write ( *, '(a)' ) '  For ascending order:'
  write ( *, '(a)' ) '  R8VEC_SEARCH_BINARY_A searches a sorted array;'
  write ( *, '(a)' ) ''
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  call r8vec_uniform_01 ( n, seed, a )

  search_val = a(1)

  call r8vec_sort_heap_a ( n, a )

  call r8vec_print ( n, a, '  Sorted vector A:' )
!
!  Now search the sorted array for a given value.
!
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Search the array for the value ', search_val

  call r8vec_search_binary_a ( n, a, search_val, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  SEARCH RESULT:'
  write ( *, '(a)' ) ''

  if ( 0 < indx ) then
    write ( *, '(a,i8)' ) '    The value occurs in index ', indx
  else
    write ( *, '(a)' ) '    The value does not occur in the array.'
  end if

  return
end
subroutine r8vec_sign3_running_test ( )

!*****************************************************************************80
!
!! R8VEC_SIGN3_RUNNING_TEST tests R8VEC_SIGN3_RUNNING.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)
  real ( kind = 8 ), allocatable :: s(:)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SIGN3_RUNNING_TEST'
  write ( *, '(a)' ) &
    '  R8VEC_SIGN3_RUNNING returns the running sign3 of an R8VEC.'

  n = 10
  a = -5.0D+00
  b = +10.0D+00
  seed = 123456789

  allocate ( r(1:n) )
  call r8vec_uniform_ab ( n, a, b, seed, r )

  call r8vec_print ( n, r, '  Random R8VEC:' )

  allocate ( s(1:n+1) )
  call r8vec_sign3_running ( n, r, s )

  call r8vec_print ( n + 1, s, '  Running sign3:' )

  deallocate ( r )
  deallocate ( s )

  return
end
subroutine r8vec_smooth_test ( )

!*****************************************************************************80
!
!! r8vec_smooth_test tests r8vec_smooth.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  character ( len = 80 ) label
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  real ( kind = 8 ) x(10)
  real ( kind = 8 ) z(10)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8vec_smooth_test'
  write ( *, '(a)' ) '  r8vec_smooth smooths an R8VEC.'

  n = 10
  do i = 1, n
    x(i) = real ( i, kind = 8 )
  end do
  call r8vec_print ( n, x, '  The vector X:' )
  s = 2
  call r8vec_smooth ( n, x, s, z )
  write ( label, '(a,i2)' ) '  Vector X using smoothing S = ', s
  call r8vec_print ( n, z, label )

  n = 10
  do i = 1, n
    x(i) = ( real ( i, kind = 8 ) ) ** 2
  end do
  call r8vec_print ( n, x, '  The vector X:' )
  s = 1
  call r8vec_smooth ( n, x, s, z )
  write ( label, '(a,i2)' ) '  Vector X using smoothing S = ', s
  call r8vec_print ( n, z, label )

  return
end
subroutine r8vec_softmax_test ( )

!*****************************************************************************80
!
!! R8VEC_SOFTMAX_TEST tests R8VEC_SOFTMAX.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: s(:)
  real ( kind = 8 ), allocatable :: x(:)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SOFTMAX_TEST'
  write ( *, '(a)' ) &
    '  R8VEC_SOFTMAX evaluates the softmax function of an R8VEC.'

  n = 10
  a = -3.0D+00
  b = +3.0D+00
  seed = 123456789

  allocate ( x(1:n) )
  call r8vec_normal_ab ( n, a, b, seed, x )

  allocate ( s(1:n) )
  call r8vec_softmax ( n, x, s )

  call r8vec2_print ( n, x, s, '  X, Softmax:' )

  deallocate ( s )
  deallocate ( x )

  return
end
subroutine r8vec_sort_bubble_a_test ( )

!*****************************************************************************80
!
!! R8VEC_SORT_BUBBLE_A_TEST tests R8VEC_SORT_BUBBLE_A.
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

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORT_BUBBLE_A_TEST'
  write ( *, '(a)' ) '  R8VEC_SORT_BUBBLE_A ascending sorts a R8VEC.'

  b = 0.0D+00
  c = 3.0D+00 * real ( n, kind = 8 )
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print_part ( n, a, 1, 10, '  Original array:' )

  call r8vec_sort_bubble_a ( n, a )

  call r8vec_print_part ( n, a, 1, 10, '  Ascending sorted array:' )

  return
end
subroutine r8vec_sort_heap_a_test ( )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_A_TEST tests R8VEC_SORT_HEAP_A.
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

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORT_HEAP_A_TEST'
  write ( *, '(a)' ) '  R8VEC_SORT_HEAP_A ascending sorts a R8VEC.'

  b = 0.0D+00
  c = 3.0D+00 * real ( n, kind = 8 )
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print_part ( n, a, 1, 10, '  Original array:' )

  call r8vec_sort_heap_a ( n, a )

  call r8vec_print_part ( n, a, 1, 10, '  Ascending sorted array:' )

  return
end
subroutine r8vec_sort_heap_d_test ( )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_D_TEST tests R8VEC_SORT_HEAP_D.
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

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORT_HEAP_D_TEST'
  write ( *, '(a)' ) '  R8VEC_SORT_HEAP_D descending sorts a R8VEC.'

  b = 0.0D+00
  c = 3.0D+00 * real ( n, kind = 8 )
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print_part ( n, a, 1, 10, '  Original array:' )

  call r8vec_sort_heap_d ( n, a )

  call r8vec_print_part ( n, a, 1, 10, '  Descending sorted array:' )

  return
end
subroutine r8vec_sort_heap_index_a_test ( )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_INDEX_A_TEST tests R8VEC_SORT_HEAP_INDEX_A.
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

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORT_HEAP_INDEX_A_TEST'
  write ( *, '(a)' ) '  R8VEC_SORT_HEAP_INDEX_A creates an ascending'
  write ( *, '(a)' ) '  sort index for a R8VEC.'

  b = 0.0D+00
  c = 3.0D+00 * real ( n, kind = 8 )
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print_part ( n, a, 1, 10, '  Unsorted array:' )

  call r8vec_sort_heap_index_a ( n, a, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  After indexed ascending sort:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, INDX(I), A(I)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2i8,g14.6)' ) i, indx(i), a(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now use the index array to carry out the'
  write ( *, '(a)' ) '  permutation implicitly.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  INDX(I), A(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(i8,g14.6)' ) indx(i), a(indx(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Call R8VEC_PERMUTE to carry out the permutation'
  write ( *, '(a)' ) '  explicitly.'

  call r8vec_permute ( n, indx, a )

  call r8vec_print ( n, a, '  I, A(I)' )

  return
end
subroutine r8vec_sort_heap_index_d_test ( )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_INDEX_D_TEST tests R8VEC_SORT_HEAP_INDEX_D.
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

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORT_HEAP_INDEX_D_TEST'
  write ( *, '(a)' ) '  R8VEC_SORT_HEAP_INDEX_D creates a descending'
  write ( *, '(a)' ) '  sort index for a R8VEC.'

  b = 0.0D+00
  c = 3.0D+00 * real ( n, kind = 8 )
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print_part ( n, a, 1, 10, '  Unsorted array:' )

  call r8vec_sort_heap_index_d ( n, a, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  After indexed descending sort:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, INDX(I), A(I)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2i8,g14.6)' ) i, indx(i), a(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now use the index array to carry out the'
  write ( *, '(a)' ) '  permutation implicitly.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  INDX(I), ARRAY(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(i8,g14.6)' ) indx(i), a(indx(i))
  end do

  return
end
subroutine r8vec_sort_heap_mask_a_test ( )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_MASK_A_TEST tests R8VEC_SORT_HEAP_MASK_A.
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

  integer ( kind = 4 ), parameter :: mask_num = 10
  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(mask_num)
  integer ( kind = 4 ), dimension ( mask_num ) :: mask = (/ &
    2, 4, 7, 8, 9, 12, 13, 16, 18, 19 /)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORT_HEAP_MASK_A_TEST'
  write ( *, '(a)' ) '  R8VEC_SORT_HEAP_MASK_A creates an ascending'
  write ( *, '(a)' ) '  sort index for a masked R8VEC.'

  b = 0.0D+00
  c = 3.0D+00 * real ( n, kind = 8 )
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Unsorted array:' )

  call i4vec_print ( mask_num, mask, '  The mask array:' )

  call r8vec_mask_print ( n, a, mask_num, mask, '  The masked unsorted array:' )

  call r8vec_sort_heap_mask_a ( n, a, mask_num, mask, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  After masked indexed ascending sort:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, INDX(I), MASK(INDX(I)), A(MASK(INDX(I)))'
  write ( *, '(a)' ) ''
  do i = 1, mask_num
    write ( *, '(3i8,g14.6)' ) i, indx(i), mask(indx(i)), a(mask(indx(i)))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Call I4VEC_PERMUTE to carry out the index permutation'
  write ( *, '(a)' ) '  explicitly on the MASK vector.'

  call i4vec_permute ( mask_num, indx, mask )
!
!  Essentially, INDX becomes the identity vector now.
!
  call i4vec_indicator1 ( mask_num, indx )

  call i4vec_print ( mask_num, mask, '  The reordered mask array:' )

  call r8vec_mask_print ( n, a, mask_num, mask, &
    '  The reordered masked sorted array:' )

  return
end
subroutine r8vec_sort_insert_a_test ( )

!*****************************************************************************80
!
!! R8VEC_SORT_INSERT_A_TEST tests R8VEC_SORT_INSERT_A.
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

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORT_INSERT_A_TEST'
  write ( *, '(a)' ) '  R8VEC_SORT_INSERT_A ascending sorts a R8VEC.'

  b = 0.0D+00
  c = 3.0D+00 * real ( n, kind = 8 )
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print_part ( n, a, 1, 10, '  Unsorted array:' )

  call r8vec_sort_insert_a ( n, a )

  call r8vec_print_part ( n, a, 1, 10, '  Sorted array:' )

  return
end
subroutine r8vec_sort_insert_index_a_test ( )

!*****************************************************************************80
!
!! R8VEC_SORT_INSERT_INDEX_A_TEST tests R8VEC_SORT_INSERT_INDEX_A.
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

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORT_INSERT_INDEX_A_TEST'
  write ( *, '(a)' ) '  R8VEC_SORT_INSERT_INDEX_A creates an ascending'
  write ( *, '(a)' ) '  sort index for a R8VEC.'

  b = 0.0D+00
  c = 3.0D+00 * real ( n, kind = 8 )
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print_part ( n, a, 1, 10, '  Unsorted array:' )

  call r8vec_sort_insert_index_a ( n, a, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  After indexed ascending sort:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, INDX(I), A(I)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2i8,g14.6)' ) i, indx(i), a(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now use the index array to carry out the'
  write ( *, '(a)' ) '  permutation implicitly.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, INDX(I), A(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(i8, i8,g14.6)' ) i, indx(i), a(indx(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Call R8VEC_PERMUTE to carry out the permutation'
  write ( *, '(a)' ) '  explicitly.'

  call r8vec_permute ( n, indx, a )

  call r8vec_print_part ( n, a, 1, 10, '  Permuted data' )

  return
end
subroutine r8vec_sort_quick_a_test ( )

!*****************************************************************************80
!
!! R8VEC_SORT_QUICK_A_TEST tests R8VEC_SORT_QUICK_A.
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

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORT_QUICK_A_TEST'
  write ( *, '(a)' ) '  R8VEC_SORT_QUICK_A sorts a R8VEC'
  write ( *, '(a)' ) '  using quick sort.'

  b = 0.0D+00
  c = 10.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Unsorted array:' )

  call r8vec_sort_quick_a ( n, a )

  call r8vec_print ( n, a, '  Sorted array:' )

  return
end
subroutine r8vec_sorted_merge_a_test ( )

!*****************************************************************************80
!
!! R8VEC_SORTED_MERGE_A_TEST tests R8VEC_SORTED_MERGE_A;
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

  integer ( kind = 4 ), parameter :: na = 10
  integer ( kind = 4 ), parameter :: nb = 10

  real ( kind = 8 ) a(na)
  real ( kind = 8 ) b(nb)
  real ( kind = 8 ) c(na+nb)
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORTED_MERGE_A_TEST'
  write ( *, '(a)' ) '  For ascending order:'
  write ( *, '(a)' ) '  R8VEC_SORTED_MERGE_A merges two sorted R8VEC''s;'
  write ( *, '(a)' ) ''
  write ( *, '(a,i12)' ) '  Using initial random number seed = ', seed

  call r8vec_uniform_01 ( na, seed, a )
  call r8vec_uniform_01 ( nb, seed, b )

  call r8vec_sort_heap_a ( na, a )

  call r8vec_sort_heap_a ( nb, b )

  call r8vec_print ( na, a, '  Sorted vector A:' )

  call r8vec_print ( nb, b, '  Sorted vector B:' )

  call r8vec_sorted_merge_a ( na, a, nb, b, nc, c )

  call r8vec_print ( nc, c, '  Merged vector C:' )

  return
end
subroutine r8vec_sorted_nearest_test ( )

!*****************************************************************************80
!
!! R8VEC_SORTED_NEAREST_TEST tests R8VEC_SORTED_NEAREST.
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

  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) r8vec_sorted_nearest
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xval

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORTED_NEAREST_TEST'
  write ( *, '(a)' ) '  R8VEC_SORTED_NEAREST finds the nearest entry'
  write ( *, '(a)' ) '  in a sorted R8VEC.'

  b = 0.0D+00
  c = 10.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, x )
  call r8vec_sort_heap_a ( n, x )

  call r8vec_print ( n, x, '  Sorted array:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     Test        Nearest'
  write ( *, '(a)' ) '     Value    Index   Value'
  write ( *, '(a)' ) ''
  do i = 1, 10

    xval = r8_uniform_ab ( b, c, seed )

    j = r8vec_sorted_nearest ( n, x, xval )

    write ( *, '(2x,f8.4,4x,i8,2x,f8.4)' ) xval, j, x(j)

  end do

  return
end
subroutine r8vec_sorted_range_test ( )

!*****************************************************************************80
!
!! R8VEC_SORTED_RANGE_TEST tests R8VEC_SORTED_RANGE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_hi
  integer ( kind = 4 ) i_lo
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) r_lo
  real ( kind = 8 ) r_hi
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORTED_RANGE_TEST'
  write ( *, '(a)' ) '  R8VEC_SORTED_RANGE seeks the range of indices'
  write ( *, '(a)' ) '  in a sorted vector R so that'
  write ( *, '(a)' ) '  R_LO <= R(I_LO:I_HI) <= R_HI.'

  seed = 123456789

  do test = 1, 5

    call r8vec_uniform_01 ( n, seed, r )

    call r8vec_sort_heap_a ( n, r )

    call r8vec_print ( n, r, '  Sorted array R:' )

    r_lo = r8_uniform_01 ( seed )
    r_hi = r8_uniform_01 ( seed )

    if ( r_hi < r_lo ) then
      t = r_lo
      r_lo = r_hi
      r_hi = t
    end if

    call r8vec_sorted_range ( n, r, r_lo, r_hi, i_lo, i_hi )

    write ( *, '(a)' ) ''
    if ( i_hi < i_lo ) then
      write ( *, '(2x,a4,2x,g14.6)' ) 'R_LO', r_lo
      write ( *, '(2x,a4,2x,g14.6)' ) 'R_HI', r_hi
      write ( *, '(2x,a)' ) '  Empty range in R.'
    else

      write ( *, '(2x,a4,2x,g14.6)' ) 'R_LO', r_lo
      do i = i_lo, i_hi
        write ( *, '(2x,i4,2x,g14.6)' ) i, r(i)
      end do
      write ( *, '(2x,a4,2x,g14.6)' ) 'R_HI', r_hi
    end if

  end do

  return
end
subroutine r8vec_sorted_split_test ( )

!*****************************************************************************80
!
!! R8VEC_SORTED_SPLIT_TEST tests R8VEC_SORTED_SPLIT.
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

  integer ( kind = 4 ), parameter :: n = 25

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i_gt
  integer ( kind = 4 ) i_lt
  integer ( kind = 4 ) seed
  real ( kind = 8 ) split

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORTED_SPLIT_TEST'
  write ( *, '(a)' ) '  R8VEC_SORTED_SPLIT splits a sorted R8VEC into'
  write ( *, '(a)' ) '  entries less than and greater than a'
  write ( *, '(a)' ) '  splitting value.'

  b = 0.0D+00
  c = 10.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  a(1:n) = real ( nint ( a(1:n) ), kind = 8 ) / 2.0D+00

  call r8vec_sort_heap_a ( n, a )

  split = 0.5D+00 * ( a(1) + a(n) )

  call r8vec_print ( n, a, '  The sorted array:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Splitting value is ', split

  call r8vec_sorted_split ( n, a, split, i_lt, i_gt )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Lower index I_LT = ', i_lt
  write ( *, '(a,i8)' ) '  Upper index I_GT = ', i_gt

  return
end
subroutine r8vec_sorted_undex_test ( )

!*****************************************************************************80
!
!! R8VEC_SORTED_UNDEX_TEST tests R8VEC_SORTED_UNDEX.
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
  real ( kind = 8 ) r8_epsilon
  real ( kind = 8 ) tol
  integer ( kind = 4 ), allocatable, dimension ( : ) :: undx
  integer ( kind = 4 ) x_unique_num
  real ( kind = 8 ), dimension ( x_num ) :: x_val = (/ &
    11.0, 11.0, 11.0, 22.0, 22.0, 33.0, 33.0, 55.0, 55.0 /)
  integer ( kind = 4 ) xdnu(x_num)
  real ( kind = 8 ), allocatable, dimension ( : ) :: xu_val

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORTED_UNDEX_TEST'
  write ( *, '(a)' ) '  R8VEC_SORTED_UNDEX produces index vectors which create a sorted'
  write ( *, '(a)' ) '  list of the unique elements of a sorted R8VEC,'
  write ( *, '(a)' ) '  and a map from the original vector to the (implicit)'
  write ( *, '(a)' ) '  vector of sorted unique elements.'

  call r8vec_print ( x_num, x_val, '  The vector X:' )

  tol = r8_epsilon ( )
  call r8vec_sorted_unique_count ( x_num, x_val, tol, x_unique_num )

  allocate ( undx(1:x_unique_num) )
  allocate ( xu_val(1:x_unique_num) )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Tolerance for equality is ', tol
  write ( *, '(a,i8)' ) '  Number of unique entries in X is ', x_unique_num

  call r8vec_sorted_undex ( x_num, x_val, x_unique_num, tol, undx, xdnu )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  UNDX can be used to list the unique elements of X'
  write ( *, '(a)' ) '  in sorted order.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  UNDX   X(UNDX)'
  write ( *, '(a)' ) ''
  do i = 1, x_unique_num
    write ( *, '(2x,i4,2x,i4,2x,f8.1)' ) i, undx(i), x_val(undx(i))
  end do

  xu_val(1:x_unique_num) = x_val(undx(1:x_unique_num))

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  UNDX can be used to created XU, a copy of X'
  write ( *, '(a)' ) '  containing only the unique elements, in sorted order.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  UNDX     XU(I)'
  write ( *, '(a)' ) ''
  do i = 1, x_unique_num
    write ( *, '(2x,i4,2x,i4,2x,f8.1)' ) i, undx(i), xu_val(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' )'  XDNU can be used to match each element of X with one of the'
  write ( *, '(a)' )'  unique elements'
  write ( *, '(a)' ) ''
  write ( *, '(a)' )'     I  XDNU    X(I)       XU(XDNU(I))'
  write ( *, '(a)' ) ''

  do i = 1, x_num
    write ( *, '(2x,i4,2x,i4,2x,f8.1,2x,f12.1)' ) &
      i, xdnu(i), x_val(i), xu_val(xdnu(i))
  end do

  deallocate ( undx )
  deallocate ( xu_val )

  return
end
subroutine r8vec_sorted_unique_test ( )

!*****************************************************************************80
!
!! R8VEC_SORTED_UNIQUE_TEST tests R8VEC_SORTED_UNIQUE;
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

  integer ( kind = 4 ), parameter :: n = 30

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) seed
  real ( kind = 8 ), parameter :: tol = 0.25D+00
  integer ( kind = 4 ) unique_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORTED_UNIQUE_TEST'
  write ( *, '(a)' ) '  R8VEC_SORTED_UNIQUE finds the unique entries in'
  write ( *, '(a)' ) '  a sorted R8VEC;'

  b = 0.0D+00
  c = real ( n, kind = 8 )
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  a(1:n) = real ( int ( a(1:n) ), kind = 8 )

  call r8vec_print_part ( n, a, 1, 10, '  Unsorted array:' )

  call r8vec_sort_heap_a ( n, a )

  call r8vec_sorted_unique ( n, a, tol, unique_num )

  call r8vec_print ( unique_num, a, '  Unique entries' )

  return
end
subroutine r8vec_sorted_unique_count_test ( )

!*****************************************************************************80
!
!! R8VEC_SORTED_UNIQUE_COUNT_TEST tests R8VEC_SORTED_UNIQUE_COUNT;
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

  integer ( kind = 4 ), parameter :: n = 30

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) unique_num
  integer ( kind = 4 ) seed
  real ( kind = 8 ), parameter :: tol = 0.25D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORTED_UNIQUE_COUNT_TEST'
  write ( *, '(a)' ) '  R8VEC_SORTED_UNIQUE_COUNT counts the unique entries'
  write ( *, '(a)' ) '  of a sorted R8VEC;'

  b = 0.0D+00
  c = real ( n, kind = 8 )
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  a(1:n) = real ( int ( a(1:n) ), kind = 8 )

  call r8vec_sorted_unique_count ( n, a, tol, unique_num )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Using a tolerance of ', tol
  write ( *, '(a,i8,a)' ) '  R8VEC_SORTED_UNIQUE_COUNT counts ', unique_num, &
    ' unique entries in A.'

  return
end
subroutine r8vec_sorted_unique_hist_test ( )

!*****************************************************************************80
!
!! R8VEC_SORTED_UNIQUE_HIST_TEST tests R8VEC_SORTED_UNIQUE_HIST.
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

  integer ( kind = 4 ), parameter :: unique_max = 30
  integer ( kind = 4 ), parameter :: n = 30

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) acount(unique_max)
  real ( kind = 8 ) auniq(unique_max)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) unique_num
  integer ( kind = 4 ) seed
  real ( kind = 8 ), parameter :: tol = 0.25D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SORTED_UNIQUE_HIST_TEST'
  write ( *, '(a)' ) '  R8VEC_SORTED_UNIQUE_HIST makes a histogram of'
  write ( *, '(a)' ) '  the unique entries in a sorted R8VEC.'

  b = 0.0D+00
  c = real ( n, kind = 8 )
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a,i12)' ) '  Using random number seed ', seed

  call r8vec_uniform_ab ( n, b, c, seed, a )

  a(1:n) = real ( int ( a(1:n) ), kind = 8 ) + 0.5D+00

  call r8vec_print ( n, a, '  Unsorted array:' )

  call r8vec_sort_bubble_a ( n, a )

  call r8vec_print ( n, a, '  Ascending sorted array:' )

  call r8vec_sorted_unique_hist ( n, a, tol, unique_max, unique_num, &
    auniq, acount )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8,a)' ) '  R8VEC_SORTED_UNIQUE_HIST counts ' , unique_num, &
    ' unique entries.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Value  Multiplicity'
  write ( *, '(a)' ) ''
  do i = 1, unique_num
    write ( *, '(2x,i8,2x,g14.6,2x,i8)' ) i, auniq(i), acount(i)
  end do

  return
end
subroutine r8vec_split_test ( )

!*****************************************************************************80
!
!! R8VEC_SPLIT_TEST tests R8VEC_SPLIT.
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

  integer ( kind = 4 ), parameter :: n = 25

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) isplit
  integer ( kind = 4 ) seed
  real ( kind = 8 ) split

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SPLIT_TEST'
  write ( *, '(a)' ) '  R8VEC SPLIT splits an R8VEC into'
  write ( *, '(a)' ) '  entries less than and greater than a'
  write ( *, '(a)' ) '  splitting value.'

  b = 0.0D+00
  c = 10.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  a(1:n) = real ( nint ( a(1:n) ), kind = 8 ) / 2.0D+00

  call r8vec_print ( n, a, '  The array:' )

  split = 0.5D+00 * ( a(1) + a(n) )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Splitting value is ', split

  call r8vec_split ( n, a, split, isplit )

  call r8vec_print ( n, a, '  The split array:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Array entries <= SPLIT up to index ', isplit

  return
end
subroutine r8vec_standardize_test ( )

!*****************************************************************************80
!
!! R8VEC_STANDARDIZE_TEST tests R8VEC_STANDARDIZE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mu
  real ( kind = 8 ) r8vec_max
  real ( kind = 8 ) r8vec_min
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ) xs(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_STANDARDIZE_TEST'
  write ( *, '(a)' ) '  R8VEC_STANDARDIZE shifts and scales an R8VEC so that'
  write ( *, '(a)' ) '  it has zero mean and unit standard deviation.'

  a = -5.0D+00
  b = 15.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, a, b, seed, x )
 
  call r8vec_print ( n, x, '  Vector X:' )
  call r8vec_mean ( n, x, mu )
  call r8vec_std_sample ( n, x, sigma )
  xmax = r8vec_max ( n, x )
  xmin = r8vec_min ( n, x )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  mean(X) = ', mu
  write ( *, '(a,g14.6)' ) '  std(X)  = ', sigma
  write ( *, '(a,g14.6)' ) '  max(X)  = ', xmax
  write ( *, '(a,g14.6)' ) '  min(X)  = ', xmin

  call r8vec_standardize ( n, x, xs )

  call r8vec_print ( n, xs, '  Vector XS:' )
  call r8vec_mean ( n, xs, mu )
  call r8vec_std_sample ( n, xs, sigma )
  xmax = r8vec_max ( n, xs )
  xmin = r8vec_min ( n, xs )
  write ( *, '(a,g14.6)' ) ''
  write ( *, '(a,g14.6)' ) '  mean(XS) = ', mu
  write ( *, '(a,g14.6)' ) '  std(XS)  = ', sigma
  write ( *, '(a,g14.6)' ) '  max(XS)  = ', xmax
  write ( *, '(a,g14.6)' ) '  min(XS)  = ', xmin

  return
end
subroutine r8vec_std_test ( )

!*****************************************************************************80
!
!! r8vec_std_test() tests r8vec_std().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  integer ( kind = 4 ) seed
  real ( kind = 8 ) std

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8vec_std_test():'
  write ( *, '(a)' ) '  r8vec_std() computes the standard deviation of an R8VEC.'

  r8_lo = - 5.0D+00
  r8_hi = + 5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, a )

  call r8vec_print ( n, a, '  Vector:' )

  call r8vec_std ( n, a, std )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  STD: ', std

  return
end
subroutine r8vec_std_sample_test ( )

!*****************************************************************************80
!
!! r8vec_std_sample_test() tests r8vec_std_sample().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  integer ( kind = 4 ) seed
  real ( kind = 8 ) std

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8vec_std_sample_test(0'
  write ( *, '(a)' ) '  r8vec_std_sample() computes the sample standard deviation'
  write ( *, '(a)' ) '  of an R8VEC.'

  r8_lo = - 5.0D+00
  r8_hi = + 5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, a )

  call r8vec_print ( n, a, '  Vector:' )

  call r8vec_std_sample ( n, a, std )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  STD: ', std

  return
end
subroutine r8vec_std_sample_update_test ( )

!*****************************************************************************80
!
!! r8vec_std_sample_update_test() tests r8vec_std_sample_update().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  real ( kind = 8 ) a(n_max)
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean_n
  real ( kind = 8 ) mean_nm1
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) std
  real ( kind = 8 ) std_n
  real ( kind = 8 ) std_nm1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8vec_std_sample_update_test()'
  write ( *, '(a)' ) '  r8vec_std_sample_update() updates sample standard deviation'
  write ( *, '(a)' ) '  of a vector when one more element is added.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N      R                STD          STD_UPDATE'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, n_max

    r = r8_uniform_01 ( seed )
    a(i) = r
    call r8vec_std_sample ( i, a, std )
    
    nm1 = i - 1
    mean_nm1 = mean_n
    std_nm1 = std_n

    call r8vec_std_sample_update ( nm1, mean_nm1, std_nm1, r, &
      n, mean_n, std_n )

    write ( *, '(2x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) i, r, std, std_n

  end do

  return
end
subroutine r8vec_std_update_test ( )

!*****************************************************************************80
!
!! r8vec_std_update_test() tests r8vec_std_update().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  real ( kind = 8 ) a(n_max)
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean_n
  real ( kind = 8 ) mean_nm1
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) std
  real ( kind = 8 ) std_n
  real ( kind = 8 ) std_nm1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8vec_std_update_test()'
  write ( *, '(a)' ) '  r8vec_std_update() updates the standard deviation of a'
  write ( *, '(a)' ) '  vector when one more element is added.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N      R                STD          STD_UPDATE'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, n_max

    r = r8_uniform_01 ( seed )
    a(i) = r
    call r8vec_std ( i, a, std )
    
    nm1 = i - 1
    mean_nm1 = mean_n
    std_nm1 = std_n

    call r8vec_std_update ( nm1, mean_nm1, std_nm1, r, &
      n, mean_n, std_n )

    write ( *, '(2x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) i, r, std, std_n

  end do

  return
end
subroutine r8vec_sum_test ( )

!*****************************************************************************80
!
!! R8VEC_SUM_TEST tests R8VEC_SUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 January 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8vec_sum
  integer ( kind = 4 ) seed
  real ( kind = 8 ) total

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SUM_TEST'
  write ( *, '(a)' ) '  R8VEC_SUM computes the sum of an R8VEC.'

  b = - 10.0D+00
  c = + 10.0D+00

  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a )

  call r8vec_print ( n, a, '  Input vector:' )

  write ( *, '(a)' ) ''

  total = r8vec_sum ( n, a )
  write ( *, '(a,g14.6)' ) '  SUM: ', total

  return
end
subroutine r8vec_sum_running_test ( )

!*****************************************************************************80
!
!! R8VEC_SUM_RUNNING_TEST tests R8VEC_SUM_RUNNING.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)
  real ( kind = 8 ), allocatable :: s(:)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SUM_RUNNING_TEST'
  write ( *, '(a)' ) '  R8VEC_SUM_RUNNING eturns the running sums of an R8VEC.'

  n = 10
  a = -5.0D+00
  b = +10.0D+00
  seed = 123456789

  allocate ( r(1:n) )
  call r8vec_uniform_ab ( n, a, b, seed, r )

  call r8vec_print ( n, r, '  R8VEC:' )

  allocate ( s(1:n+1) )
  call r8vec_sum_running ( n, r, s )

  call r8vec_print ( n + 1, s, '  Running sums:' )

  deallocate ( r )
  deallocate ( s )

  return
end
subroutine r8vec_transpose_print_test ( )

!*****************************************************************************80
!
!! R8VEC_TRANSPOSE_PRINT_TEST tests R8VEC_TRANSPOSE_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 12

  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_TRANSPOSE_PRINT_TEST'
  write ( *, '(a)' ) '  R8VEC_TRANSPOSE_PRINT prints an R8VEC "tranposed",'
  write ( *, '(a)' ) '  that is, placing multiple entries on a line.'

  x = (/ &
    1.1D+00, 2.02D+00, 30.33D+00, 444.44D+00, -0.005D+00, &
    6.6666666666D+00, 7777777.0D+00, 8.0D+00, 99.0D+00, 10.0D+00, &
    11.0D+00, 12.0D+00 /)

  call r8vec_transpose_print ( n, x, '  The vector X:' )

  return
end
subroutine r8vec_undex_test ( )

!*****************************************************************************80
!
!! R8VEC_UNDEX_TEST tests R8VEC_UNDEX.
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
  real ( kind = 8 ) r8_epsilon
  real ( kind = 8 ) tol
  integer ( kind = 4 ), allocatable, dimension ( : ) :: undx
  integer ( kind = 4 ) x_unique_num
  real ( kind = 8 ), dimension ( x_num ) :: x_val = (/ &
    33.0, 55.0, 11.0, 11.0, 55.0, 33.0, 22.0, 22.0, 11.0 /)
  integer ( kind = 4 ) xdnu(x_num)
  real ( kind = 8 ), allocatable, dimension ( : ) :: xu_val

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_UNDEX_TEST'
  write ( *, '(a)' ) '  R8VEC_UNDEX produces index vectors which create a sorted'
  write ( *, '(a)' ) '  list of the unique elements of an (unsorted) R8VEC,'
  write ( *, '(a)' ) '  and a map from the original vector to the (implicit)'
  write ( *, '(a)' ) '  vector of sorted unique elements.'

  call r8vec_print ( x_num, x_val, '  The vector X:' )

  tol = r8_epsilon ( )
  call r8vec_unique_count ( x_num, x_val, tol, x_unique_num )

  allocate ( undx(1:x_unique_num) )
  allocate ( xu_val(1:x_unique_num) )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Tolerance for equality is ', tol
  write ( *, '(a,i8)' ) '  Number of unique entries in X is ', x_unique_num

  call r8vec_undex ( x_num, x_val, x_unique_num, tol, undx, xdnu )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  UNDX can be used to list the unique elements of X'
  write ( *, '(a)' ) '  in sorted order.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  UNDX   X(UNDX)'
  write ( *, '(a)' ) ''
  do i = 1, x_unique_num
    write ( *, '(2x,i4,2x,i4,2x,f8.1)' ) i, undx(i), x_val(undx(i))
  end do

  xu_val(1:x_unique_num) = x_val(undx(1:x_unique_num))

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  UNDX can be used to created XU, a copy of X'
  write ( *, '(a)' ) '  containing only the unique elements, in sorted order.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  UNDX     XU(I)'
  write ( *, '(a)' ) ''
  do i = 1, x_unique_num
    write ( *, '(2x,i4,2x,i4,2x,f8.1)' ) i, undx(i), xu_val(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  XDNU can be used to match each element of X with one of the'
  write ( *, '(a)' ) '  unique elements'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I  XDNU    X(I)       XU(XDNU(I))'
  write ( *, '(a)' ) ''

  do i = 1, x_num
    write ( *, '(2x,i4,2x,i4,2x,f8.1,2x,f12.1)' ) &
      i, xdnu(i), x_val(i), xu_val(xdnu(i))
  end do

  deallocate ( undx )
  deallocate ( xu_val )

  return
end
subroutine r8vec_uniform_01_test ( )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01_TEST tests R8VEC_UNIFORM_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) r(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  R8VEC_UNIFORM_01 returns a random R8VEC '
  write ( *, '(a)' ) '  with entries in [0,1].'

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a,i12)' ) '  Input SEED = ', seed

  call r8vec_uniform_01 ( n, seed, r )

  call r8vec_print ( n, r, '  Random R8VEC:' )

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

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  R8VEC_UNIFORM_AB returns a random R8VEC '
  write ( *, '(a)' ) '  with entries in a given range [ A, B ]'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  For this problem:'
  write ( *, '(a,g14.6)' ) '  A = ', a
  write ( *, '(a,g14.6)' ) '  B = ', b

  seed = 123456789

  do test = 1, test_num

    write ( *, '(a)' ) ''
    write ( *, '(a,i12)' ) '  Input SEED = ', seed

    call r8vec_uniform_ab ( n, a, b, seed, r )

    call r8vec_print ( n, r, '  Random R8VEC:' )

  end do

  return
end
subroutine r8vec_uniform_unit_test ( )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_UNIT_TEST tests R8VEC_UNIFORM_UNIT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5

  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_UNIFORM_UNIT_TEST'
  write ( *, '(a)' ) '  R8VEC_UNIFORM_UNIT returns a random point on the'
  write ( *, '(a)' ) '  surface of the unit M sphere.'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 5

    call r8vec_uniform_unit ( m, seed, x )

    call r8vec_print ( m, x, '  Random unit vector:' )

  end do

  return
end
subroutine r8vec_variance_test ( )

!*****************************************************************************80
!
!! r8vec_variance_test() tests r8vec_variance().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  integer ( kind = 4 ) seed
  real ( kind = 8 ) variance

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8vec_variance_test()'
  write ( *, '(a)' ) '  r8vec_variance() computes the variance of an R8VEC.'

  r8_lo = - 5.0D+00
  r8_hi = + 5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, a )

  call r8vec_print ( n, a, '  Vector:' )

  call r8vec_variance ( n, a, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Variance: ', variance

  return
end
subroutine r8vec_variance_circular_test ( )

!*****************************************************************************80
!
!! R8VEC_VARIANCE_CIRCULAR_TEST tests R8VEC_VARIANCE_CIRCULAR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) seed
  real ( kind = 8 ) variance_circular
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_VARIANCE_CIRCULAR_TEST'
  write ( *, '(a)' ) '  R8VEC_VARIANCE_CIRCULAR computes the circular variance of an R8VEC.'

  a = - r8_pi
  b = + r8_pi
  seed = 123456789
  call r8vec_uniform_ab ( n, a, b, seed, x )

  call r8vec_print ( n, x, '  Uniform Vector in [-PI,+PI]:' )
  call r8vec_variance_circular ( n, x, variance_circular )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Circular variance: ', variance_circular

  a = 0.0D+00
  b = 1.0D+00
  seed = 123456789
  call r8vec_normal_ab ( n, a, b, seed, x )

  call r8vec_print ( n, x, '  Normal vector, mean 0, variance 1' )

  call r8vec_variance_circular ( n, x, variance_circular )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Circular variance: ', variance_circular

  return
end
subroutine r8vec_variance_sample_test ( )

!*****************************************************************************80
!
!! R8VEC_VARIANCE_SAMPLE_TEST tests R8VEC_VARIANCE_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8_hi
  real ( kind = 8 ) r8_lo
  integer ( kind = 4 ) seed
  real ( kind = 8 ) variance

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_VARIANCE_SAMPLE_TEST'
  write ( *, '(a)' ) '  R8VEC_VARIANCE_SAMPLE computes the sample variance of an R8VEC.'

  r8_lo = - 5.0D+00
  r8_hi = + 5.0D+00
  seed = 123456789
  call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, a )

  call r8vec_print ( n, a, '  Vector:' )

  call r8vec_variance_sample ( n, a, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Variance: ', variance

  return
end
subroutine r8vec_variance_sample_update_test ( )

!*****************************************************************************80
!
!! r8vec_variance_sample_update_test() tests r8vec_variance_sample_update().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  real ( kind = 8 ) a(n_max)
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean_n
  real ( kind = 8 ) mean_nm1
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) variance
  real ( kind = 8 ) variance_n
  real ( kind = 8 ) variance_nm1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8vec_variance_sample_update_test()'
  write ( *, '(a)' ) '  r8vec_variance_sample_update() updates the sample'
  write ( *, '(a)' ) '  variance of a vector when one more element is added.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N      R           VARIANCE     VARIANCE_UPDATE'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, n_max

    r = r8_uniform_01 ( seed )
    a(i) = r
    call r8vec_variance_sample ( i, a, variance )
    
    nm1 = i - 1
    mean_nm1 = mean_n
    variance_nm1 = variance_n

    call r8vec_variance_sample_update ( nm1, mean_nm1, variance_nm1, r, &
      n, mean_n, variance_n )

    write ( *, '(2x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) i, r, variance, variance_n

  end do

  return
end
subroutine r8vec_variance_update_test ( )

!*****************************************************************************80
!
!! r8vec_variance_update_test() tests r8vec_variance_update().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  real ( kind = 8 ) a(n_max)
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean_n
  real ( kind = 8 ) mean_nm1
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) variance
  real ( kind = 8 ) variance_n
  real ( kind = 8 ) variance_nm1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'r8vec_variance_update_test()'
  write ( *, '(a)' ) '  r8vec_variance_update() updates the variance of a vector'
  write ( *, '(a)' ) '  when one more element is added.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N      R           VARIANCE     VARIANCE_UPDATE'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, n_max

    r = r8_uniform_01 ( seed )
    a(i) = r
    call r8vec_variance ( i, a, variance )
    
    nm1 = i - 1
    mean_nm1 = mean_n
    variance_nm1 = variance_n

    call r8vec_variance_update ( nm1, mean_nm1, variance_nm1, r, &
      n, mean_n, variance_n )

    write ( *, '(2x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) i, r, variance, variance_n

  end do

  return
end
subroutine r8vec2_print_test ( )

!*****************************************************************************80
!
!! R8VEC2_PRINT_TEST tests R8VEC2_PRINT.
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

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) :: a(n) = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c(n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC2_PRINT_TEST'
  write ( *, '(a)' ) '  R8VEC2_PRINT prints a pair of R8VEC''s.'

  do i = 1, n
    b(i) = a(i) ** 2
    c(i) = sqrt ( a(i) )
  end do

  call r8vec2_print ( n, b, c, '  Squares and square roots:' )

  return
end
subroutine r8vec2_print_some_test ( )

!*****************************************************************************80
!
!! R8VEC2_PRINT_SOME_TEST tests R8VEC2_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:)
  real ( kind = 8 ), allocatable :: b(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: n = 100
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC2_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  R8VEC2_PRINT_SOME prints some of a pair of R8VEC''s.'

  allocate ( a(1:n) )
  allocate ( b(1:n) )

  do i = 1, n
    x = real ( i, kind = 8 )
    a(i) = x * x
    b(i) = sqrt ( x )
  end do

  call r8vec2_print_some ( n, a, b, 10, '  No more than 10 lines of two vectors:' )

  deallocate ( a )
  deallocate ( b )

  return
end
subroutine r8vec2_sort_a_test ( )

!*****************************************************************************80
!
!! R8VEC2_SORT_A_TEST tests R8VEC2_SORT_A.
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

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC2_SORT_A_TEST'
  write ( *, '(a)' ) '  R8VEC2_SORT_A ascending sorts a pair of R8VEC''s;'

  b = 1.0D+00
  c = 3.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, b,  c, seed, a1 )

  b = 5.0D+00
  c = 10.0D+00

  call r8vec_uniform_ab ( n, b, c, seed, a2 )

  a1(3) = a1(1)
  a2(3) = a2(1)

  a1(6) = a1(2)
  a2(6) = a2(2)

  a1(9) = a1(1)
  a2(9) = a2(1)

  call r8vec2_print ( n, a1, a2, '  The pair of arrays:' )

  call r8vec2_sort_a ( n, a1, a2 )

  call r8vec2_print ( n, a1, a2, '  Arrays after ascending sort:' )

  return
end
subroutine r8vec2_sort_d_test ( )

!*****************************************************************************80
!
!! R8VEC2_SORT_D_TEST tests R8VEC2_SORT_D.
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

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC2_SORT_D_TEST'
  write ( *, '(a)' ) '  R8VEC2_SORT_D descending sorts a pair of R8VEC''s.'

  b = 1.0D+00
  c = 3.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, b,  c, seed, a1 )

  b = 5.0D+00
  c = 10.0D+00

  call r8vec_uniform_ab ( n, b, c, seed, a2 )

  a1(3) = a1(1)
  a2(3) = a2(1)

  a1(6) = a1(2)
  a2(6) = a2(2)

  a1(9) = a1(1)
  a2(9) = a2(1)

  call r8vec2_print ( n, a1, a2, '  The pair of arrays:' )

  call r8vec2_sort_d ( n, a1, a2 )

  call r8vec2_print ( n, a1, a2, '  Arrays after descending sort:' )

  return
end
subroutine r8vec2_sort_heap_index_a_test ( )

!*****************************************************************************80
!
!! R8VEC2_SORT_HEAP_INDEX_A_TEST tests R8VEC2_SORT_HEAP_INDEX_A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC2_SORT_HEAP_INDEX_A_TEST'
  write ( *, '(a)' ) '  R8VEC2_SORT_HEAP_INDEX_A creates a sort index'
  write ( *, '(a)' ) '  for an (X,Y) array.'

  do i = 1, n

    x(i) = real ( i4_uniform_ab ( 0, n, seed ), kind = 8 ) / real ( n, kind = 8 )
    y(i) = real ( i4_uniform_ab ( 0, n, seed ), kind = 8 ) / real ( n, kind = 8 )

  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The unsorted array:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, X(I), Y(I)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i8,6x,2g14.6)' ) i, x(i), y(i)
  end do

  call r8vec2_sort_heap_index_a ( n, x, y, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  After sorting:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, INDX(I), X(I), Y(I)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2i8,2g14.6)' ) i, indx(i), x(i), y(i)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Now use the index array to carry out the'
  write ( *, '(a)' ) '  permutation implicitly.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, INDX(I), X(INDX(I)), Y(INDX(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2i8,2g14.6)' ) i, indx(i), x(indx(i)), y(indx(i))
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  R8VEC_PERMUTE carries out the permutation.'

  call r8vec_permute ( n, indx, x )
  call r8vec_permute ( n, indx, y )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I, X(I), Y(I)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i8,6x,2g14.6)' ) i, x(i), y(i)
  end do

  return
end
subroutine r8vec2_sorted_unique_test ( )

!*****************************************************************************80
!
!! R8VEC2_SORTED_UNIQUE_TEST tests R8VEC2_SORTED_UNIQUE.
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

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) unique_num
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC2_SORTED_UNIQUE_TEST'
  write ( *, '(a)' ) '  For a pair of R8VEC''s:'
  write ( *, '(a)' ) '  R8VEC2_SORTED_UNIQUE counts unique entries.'

  b = 1.0D+00
  c = 3.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a1 )

  b = 5.0D+00
  c = 10.0D+00

  call r8vec_uniform_ab ( n, b, c, seed, a2 )

  a1(3) = a1(1)
  a2(3) = a2(1)

  a1(6) = a1(2)
  a2(6) = a2(2)

  a1(9) = a1(1)
  a2(9) = a2(1)

  call r8vec2_print ( n, a1, a2, '  The pair of arrays:' )

  call r8vec2_sort_a ( n, a1, a2 )

  call r8vec2_print ( n, a1, a2, '  Arrays after ascending sort:' )

  call r8vec2_sorted_unique ( n, a1, a2, unique_num )

  call r8vec2_print ( unique_num, a1, a2, '  UNIQed array:' )

  return
end
subroutine r8vec2_sorted_unique_index_test ( )

!*****************************************************************************80
!
!! R8VEC2_SORTED_UNIQUE_INDEX_TEST tests R8VEC2_SORTED_UNIQUE_INDEX.
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

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) unique_num
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC2_SORTED_UNIQUE_INDEX_TEST'
  write ( *, '(a)' ) '  For a pair of R8VEC''s:'
  write ( *, '(a)' ) '  R8VEC2_SORTED_UNIQUE_INDEX indexes unique entries.'

  b = 1.0D+00
  c = 3.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a1 )

  b = 5.0D+00
  c = 10.0D+00

  call r8vec_uniform_ab ( n, b, c, seed, a2 )

  a1(3) = a1(1)
  a2(3) = a2(1)

  a1(6) = a1(2)
  a2(6) = a2(2)

  a1(9) = a1(1)
  a2(9) = a2(1)

  call r8vec2_sort_a ( n, a1, a2 )

  call r8vec2_print ( n, a1, a2, '  Sorted arrays:' )

  call r8vec2_sorted_unique_index ( n, a1, a2, unique_num, indx )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  The number of unique elements is ', unique_num

  call i4vec_print ( unique_num, indx, '  Index of Unique Elements:' )

  call r8vec_index_order ( unique_num, a1, indx )
  call r8vec_index_order ( unique_num, a2, indx )

  call r8vec2_print ( unique_num, a1, a2, '  After Indexed Nonunique Deletion.' )

  return
end
subroutine r8vec2_sum_max_index_test ( )

!*****************************************************************************80
!
!! R8VEC2_SUM_MAX_INDEX_TEST tests R8VEC2_SUM_MAX_INDEX.
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

  integer ( kind = 4 ), parameter :: n = 10

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC2_SUM_MAX_INDEX_TEST'
  write ( *, '(a)' ) '  For a pair of R8VEC''s:'
  write ( *, '(a)' ) '  R8VEC2_SUM_MAX_INDEX: index of the sum vector'
  write ( *, '(a)' ) '  with maximum value.'

  b = 0.0D+00
  c = 10.0D+00
  seed = 123456789

  call r8vec_uniform_ab ( n, b, c, seed, a1 )

  b = 0.0D+00
  c = 5.0D+00

  call r8vec_uniform_ab ( n, b, c, seed, a2 )

  call r8vec2_print ( n, a1, a2, '  The pair of vectors:' )

  call r8vec2_sum_max_index ( n, a1, a2, ival )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Index of maximum in A+B: ', ival

  return
end

