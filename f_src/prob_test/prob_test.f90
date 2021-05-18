program main

!*****************************************************************************80
!
!! MAIN is the main program for PROB_TEST.
!
!  Discussion:
!
!    PROB_TEST tests the PROB library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PROB_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Test the PROB library.'

  call angle_cdf_test ( )
  call angle_mean_test ( )
  call angle_pdf_test ( )

  call anglit_cdf_test ( )
  call anglit_sample_test ( )

  call arcsin_cdf_test ( )
  call arcsin_sample_test ( )

  call benford_cdf_test ( )
  call benford_pdf_test ( )

  call bernoulli_cdf_test ( )
  call bernoulli_sample_test ( )

  call bessel_i0_test ( )
  call bessel_i1_test ( )

  call beta_binomial_cdf_test ( )
  call beta_binomial_sample_test ( )

  call beta_cdf_test ( )
  call beta_inc_test ( )
  call beta_sample_test ( )

  call binomial_cdf_test ( )
  call binomial_sample_test ( )

  call birthday_cdf_test ( )
  call birthday_sample_test ( )

  call bradford_cdf_test ( )
  call bradford_sample_test ( )

  call buffon_box_pdf_test ( )
  call buffon_box_sample_test ( )

  call buffon_pdf_test ( )
  call buffon_sample_test ( )

  call burr_cdf_test ( )
  call burr_sample_test ( )

  call cardioid_cdf_test ( )
  call cardioid_sample_test ( )

  call cauchy_cdf_test ( )
  call cauchy_sample_test ( )

  call chebyshev1_cdf_test ( )
  call chebyshev1_sample_test ( )

  call chi_cdf_test ( )
  call chi_sample_test ( )

  call chi_square_cdf_test ( )
  call chi_square_sample_test ( )

  call chi_square_noncentral_sample_test ( )

  call circular_normal_01_sample_test ( )

  call circular_normal_sample_test ( )

  call cosine_cdf_test ( )
  call cosine_sample_test ( )

  call coupon_complete_pdf_test ( )
  call coupon_sample_test ( )

  call deranged_cdf_test ( )
  call deranged_sample_test ( )

  call dipole_cdf_test ( )
  call dipole_sample_test ( )

  call dirichlet_sample_test ( )
  call dirichlet_pdf_test ( )

  call dirichlet_mix_sample_test ( )
  call dirichlet_mix_pdf_test ( )

  call discrete_cdf_test ( )
  call discrete_sample_test ( )

  call disk_sample_test ( )

  call empirical_discrete_cdf_test ( )
  call empirical_discrete_sample_test ( )

  call english_letter_cdf_test ( )

  call english_sentence_length_cdf_test ( )
  call english_sentence_length_sample_test ( )

  call english_word_length_cdf_test ( )
  call english_word_length_sample_test ( )

  call erlang_cdf_test ( )
  call erlang_sample_test ( )

  call exponential_cdf_test ( )
  call exponential_sample_test ( )

  call exponential_01_cdf_test ( )
  call exponential_01_sample_test ( )

  call extreme_values_cdf_test ( )
  call extreme_values_sample_test ( )

  call f_cdf_test ( )
  call f_sample_test ( )

  call fermi_dirac_sample_test ( )

  call fisher_pdf_test ( )

  call fisk_cdf_test ( )
  call fisk_sample_test ( )

  call folded_normal_cdf_test ( )
  call folded_normal_sample_test ( )

  call frechet_cdf_test ( )
  call frechet_sample_test ( )

  call gamma_cdf_test ( )
  call gamma_sample_test ( )

  call genlogistic_cdf_test ( )
  call genlogistic_sample_test ( )

  call geometric_cdf_test ( )
  call geometric_sample_test ( )

  call gompertz_cdf_test ( )
  call gompertz_sample_test ( )

  call gumbel_cdf_test ( )
  call gumbel_sample_test ( )

  call half_normal_cdf_test ( )
  call half_normal_sample_test ( )

  call hypergeometric_cdf_test ( )
  call hypergeometric_sample_test ( )

  call i4_choose_test ( )
  call i4_choose_log_test ( )
  call i4_is_power_of_10_test ( )
  call i4_uniform_ab_test ( )
  call i4vec_uniform_ab_test ( )
  call i4vec_unique_count_test ( )

  call inverse_gaussian_cdf_test ( )
  call inverse_gaussian_sample_test ( )

  call laplace_cdf_test ( )
  call laplace_sample_test ( )

  call levy_cdf_test ( )

  call logistic_cdf_test ( )
  call logistic_sample_test ( )

  call log_normal_cdf_test ( )
  call log_normal_sample_test ( )

  call log_series_cdf_test ( )
  call log_series_sample_test ( )

  call log_uniform_cdf_test ( )
  call log_uniform_sample_test ( )

  call lorentz_cdf_test ( )
  call lorentz_sample_test ( )

  call maxwell_cdf_test ( )
  call maxwell_sample_test ( )

  call multinomial_coef_test ( )
  call multinomial_sample_test ( )
  call multinomial_pdf_test ( )

  call multinoulli_pdf_test ( )

  call nakagami_cdf_test ( )
  call nakagami_sample_test ( )

  call negative_binomial_cdf_test ( )
  call negative_binomial_sample_test ( )

  call normal_01_cdf_test ( )
  call normal_01_samples_test ( )

  call normal_cdf_test ( )
  call normal_samples_test ( )

  call normal_truncated_ab_cdf_test ( )
  call normal_truncated_ab_sample_test ( )

  call normal_truncated_a_cdf_test ( )
  call normal_truncated_a_sample_test ( )

  call normal_truncated_b_cdf_test ( )
  call normal_truncated_b_sample_test ( )

  call pareto_cdf_test ( )
  call pareto_sample_test ( )

  call pearson_05_pdf_test ( )

  call planck_pdf_test ( )
  call planck_sample_test ( )

  call poisson_cdf_test ( )
  call poisson_sample_test ( )

  call power_cdf_test ( )
  call power_sample_test ( )

  call quasigeometric_cdf_test ( )
  call quasigeometric_sample_test ( )

  call r8_beta_test ( )
  call r8_ceiling_test ( )
  call r8_error_f_test ( )
  call r8_factorial_test ( )
  call r8_gamma_inc_test ( )
  call r8_gamma_log_int_test ( )
  call r8_uniform_01_test ( )
  call r8_zeta_test ( )

  call rayleigh_cdf_test ( )
  call rayleigh_sample_test ( )

  call reciprocal_cdf_test ( )
  call reciprocal_sample_test ( )

  call runs_pdf_test ( )
  call runs_sample_test ( )

  call sech_cdf_test ( )
  call sech_sample_test ( )

  call semicircular_cdf_test ( )
  call semicircular_sample_test ( )

  call student_cdf_test ( )
  call student_sample_test ( )

  call student_noncentral_cdf_test ( )

  call tfn_test ( )

  call triangle_cdf_test ( )
  call triangle_sample_test ( )

  call triangular_cdf_test ( )
  call triangular_sample_test ( )

  call uniform_01_order_sample_test ( )

  call uniform_nsphere_sample_test ( )

  call uniform_01_cdf_test ( )
  call uniform_01_sample_test ( )

  call uniform_cdf_test ( )
  call uniform_sample_test ( )

  call uniform_discrete_cdf_test ( )
  call uniform_discrete_sample_test ( )

  call von_mises_cdf_test ( )
  call von_mises_sample_test ( )

  call weibull_cdf_test ( )
  call weibull_sample_test ( )

  call weibull_discrete_cdf_test ( )
  call weibull_discrete_sample_test ( )

  call zipf_cdf_test ( )
  call zipf_sample_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PROB_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine angle_cdf_test ( )

!*****************************************************************************80
!
!! ANGLE_CDF_TEST tests ANGLE_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) n
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANGLE_CDF_TEST'
  write ( *, '(a)' ) '  ANGLE_CDF evaluates the Angle CDF;'

  n = 5
  x = 0.50D+00

  call angle_cdf ( x, n, cdf )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter N = ', n
  write ( *, '(a,g14.6)' ) '  PDF argument X =  ', x
  write ( *, '(a,g14.6)' ) '  CDF value =       ', cdf

  return
end
subroutine angle_pdf_test ( )

!*****************************************************************************80
!
!! ANGLE_PDF_TEST tests ANGLE_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANGLE_PDF_TEST'
  write ( *, '(a)' ) '  ANGLE_PDF evaluates the Angle PDF;'

  n = 5
  x = 0.50D+00

  call angle_pdf ( x, n, pdf )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter N = ', n
  write ( *, '(a,g14.6)' ) '  PDF argument X =  ', x
  write ( *, '(a,g14.6)' ) '  PDF value =       ', pdf

  return
end
subroutine angle_mean_test ( )

!*****************************************************************************80
!
!! ANGLE_MEAN_TEST tests ANGLE_MEAN;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) mean
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANGLE_MEAN_TEST'
  write ( *, '(a)' ) '  ANGLE_MEAN computes the Angle mean;'

  n = 5
  call angle_mean ( n, mean )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter N = ', n
  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean

  return
end
subroutine anglit_cdf_test ( )

!*****************************************************************************80
!
!! ANGLIT_CDF_TEST tests ANGLIT_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANGLIT_CDF_TEST'
  write ( *, '(a)' ) '  ANGLIT_CDF evaluates the Anglit CDF;'
  write ( *, '(a)' ) '  ANGLIT_CDF_INV inverts the Anglit CDF.'
  write ( *, '(a)' ) '  ANGLIT_PDF evaluates the Anglit PDF;'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call anglit_sample ( seed, x )

    call anglit_pdf ( x, pdf )

    call anglit_cdf ( x, cdf )

    call anglit_cdf_inv ( cdf, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine anglit_sample_test ( )

!*****************************************************************************80
!
!! ANGLIT_SAMPLE_TEST tests ANGLIT_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANGLIT_SAMPLE_TEST'
  write ( *, '(a)' ) '  ANGLIT_MEAN computes the Anglit mean;'
  write ( *, '(a)' ) '  ANGLIT_SAMPLE samples the Anglit distribution;'
  write ( *, '(a)' ) '  ANGLIT_VARIANCE computes the Anglit variance.'

  call anglit_mean ( mean )
  call anglit_variance ( variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call anglit_sample ( seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine arcsin_cdf_test ( )

!*****************************************************************************80
!
!! ARCSIN_CDF_TEST tests ARCSIN_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  logical arcsin_check
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ARCSIN_CDF_TEST'
  write ( *, '(a)' ) '  ARCSIN_CDF evaluates the Arcsin CDF;'
  write ( *, '(a)' ) '  ARCSIN_CDF_INV inverts the Arcsin CDF.'
  write ( *, '(a)' ) '  ARCSIN_PDF evaluates the Arcsin PDF;'

  a = 1.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a

  if ( .not. arcsin_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ARCSIN_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call arcsin_sample ( a, seed, x )

    call arcsin_pdf ( x, a, pdf )

    call arcsin_cdf ( x, a, cdf )

    call arcsin_cdf_inv ( cdf, a, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine arcsin_sample_test ( )

!*****************************************************************************80
!
!! ARCSIN_SAMPLE_TEST tests ARCSIN_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  logical arcsin_check
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ARCSIN_SAMPLE_TEST'
  write ( *, '(a)' ) '  ARCSIN_MEAN computes the Arcsin mean;'
  write ( *, '(a)' ) '  ARCSIN_SAMPLE samples the Arcsin distribution;'
  write ( *, '(a)' ) '  ARCSIN_VARIANCE computes the Arcsin variance.'

  do i = 1, 2

    if ( i == 1 ) then
      a = 1.0D+00
    else if ( i == 2 ) then
      a = 16.0D+00
    end if

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a

    if ( .not. arcsin_check ( a ) ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ARCSIN_SAMPLE_TEST - Fatal error!'
      write ( *, '(a)' ) '  The parameters are not legal.'
      return
    end if

    call arcsin_mean ( a, mean )
    call arcsin_variance ( a, variance )

    write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean
    write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

    do j = 1, sample_num
      call arcsin_sample ( a, seed, x(j) )
    end do

    call r8vec_mean ( sample_num, x, mean )
    call r8vec_variance ( sample_num, x, variance )
    call r8vec_max ( sample_num, x, xmax )
    call r8vec_min ( sample_num, x, xmin )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
    write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
    write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
    write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
    write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  end do

  return
end
subroutine benford_cdf_test ( )

!*****************************************************************************80
!
!! BENFORD_CDF_TEST tests BENFORD_CDF.
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

  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf2
  integer ( kind = 4 ) n
  real ( kind = 8 ) pdf
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BENFORD_CDF_TEST'
  write ( *, '(a)' ) '  BENFORD_CDF evaluates the Benford CDF.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       N          CDF(N)          CDF(N) by summing'
  write ( *, '(a)' ) ''

  cdf2 = 0.0D+00
  do n = 1, 9
    call benford_cdf ( n, cdf )
    call benford_pdf ( n, pdf )
    cdf2 = cdf2 + pdf
    write ( *, '(2x,i6,2x,g14.6,2x,g14.6)' ) n, cdf, cdf2
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       N          CDF(N)          CDF(N) by summing'
  write ( *, '(a)' ) ''

  cdf2 = 0.0D+00
  do n = 10, 99
    call benford_cdf ( n, cdf )
    call benford_pdf ( n, pdf )
    cdf2 = cdf2 + pdf
    write ( *, '(2x,i6,2x,g14.6,2x,g14.6)' ) n, cdf, cdf2
  end do

  return
end
subroutine benford_pdf_test ( )

!*****************************************************************************80
!
!! BENFORD_PDF_TEST tests BENFORD_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) pdf

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BENFORD_PDF_TEST'
  write ( *, '(a)' ) '  BENFORD_PDF evaluates the Benford PDF.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       N       PDF(N)'
  write ( *, '(a)' ) ''

  do n = 1, 9
    call benford_pdf ( n, pdf )
    write ( *, '(2x,i6,2x,g14.6)' ) n, pdf
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       N       PDF(N)'
  write ( *, '(a)' ) ''

  do n = 10, 99
    call benford_pdf ( n, pdf )
    write ( *, '(2x,i6,2x,g14.6)' ) n, pdf
  end do

  return
end
subroutine bernoulli_cdf_test ( )

!*****************************************************************************80
!
!! BERNOULLI_CDF_TEST tests BERNOULLI_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  logical bernoulli_check
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BERNOULLI_CDF_TEST'
  write ( *, '(a)' ) '  BERNOULLI_CDF evaluates the Bernoulli CDF;'
  write ( *, '(a)' ) '  BERNOULLI_CDF_INV inverts the Bernoulli CDF.'
  write ( *, '(a)' ) '  BERNOULLI_PDF evaluates the Bernoulli PDF;'

  a = 0.75D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a

  if ( .not. bernoulli_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call bernoulli_sample ( a, seed, x )

    call bernoulli_pdf ( x, a, pdf )

    call bernoulli_cdf ( x, a, cdf )

    call bernoulli_cdf_inv ( cdf, a, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine bernoulli_sample_test ( )

!*****************************************************************************80
!
!! BERNOULLI_SAMPLE_TEST tests BERNOULLI_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  logical bernoulli_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BERNOULLI_SAMPLE_TEST'
  write ( *, '(a)' ) '  BERNOULLI_MEAN computes the Bernoulli mean;'
  write ( *, '(a)' ) '  BERNOULLI_SAMPLE samples the Bernoulli distribution;'
  write ( *, '(a)' ) '  BERNOULLI_VARIANCE computes the Bernoulli variance.'

  a = 0.75D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a

  if ( .not. bernoulli_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'BERNOULLI_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call bernoulli_mean ( a, mean )
  call bernoulli_variance ( a, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  do i = 1, sample_num
    call bernoulli_sample ( a, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine bessel_i0_test ( )

!*****************************************************************************80
!
!! BESSEL_I0_TEST tests BESSEL_I0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) bessel_i0
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BESSEL_I0_TEST:'
  write ( *, '(a)' ) '  BESSEL_I0 evaluates the Bessel I0 function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '          X            Exact                  BESSEL_I0(X)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bessel_i0_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = bessel_i0 ( x )

    write ( *, '(2x,f14.6,2x,g24.16,2x,g24.16)' ) x, fx, fx2

  end do

  return
end
subroutine bessel_i1_test ( )

!*****************************************************************************80
!
!! BESSEL_I1_TEST tests BESSEL_I1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) bessel_i1
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BESSEL_I1_TEST:'
  write ( *, '(a)' ) '  BESSEL_I1 evaluates the Bessel I1 function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '          X            Exact                  BESSEL_I1(X)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bessel_i1_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = bessel_i1 ( x )

    write ( *, '(2x,f14.6,2x,g24.16,2x,g24.16)' ) x, fx, fx2

  end do

  return
end
subroutine beta_cdf_test ( )

!*****************************************************************************80
!
!! BETA_CDF_TEST tests BETA_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 April 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical beta_check
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BETA_CDF_TEST'
  write ( *, '(a)' ) '  BETA_CDF evaluates the Beta CDF;'
  write ( *, '(a)' ) '  BETA_CDF_INV inverts the Beta CDF.'
  write ( *, '(a)' ) '  BETA_PDF evaluates the Beta PDF;'

  a = 12.0D+00
  b = 12.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. beta_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'BETA_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       A       B       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call beta_sample ( a, b, seed, x )

    call beta_pdf ( x, a, b, pdf )

    call beta_cdf ( x, a, b, cdf )

    call beta_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,6g14.6)' ) a, b, x, pdf, cdf, x2

  end do

  return
end
subroutine beta_inc_test ( )

!*****************************************************************************80
!
!! BETA_INC_TEST tests BETA_INC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) beta_inc
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BETA_INC_TEST:'
  write ( *, '(a)' ) '  BETA_INC evaluates the normalized incomplete Beta'
  write ( *, '(a)' ) '  function BETA_INC(A,B,X).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  A      B       X       Exact F       BETA_INC(A,B,X)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call beta_inc_values ( n_data, a, b, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = beta_inc ( a, b, x )

    write ( *, '(2x,3f8.4,2g14.6)' ) a, b, x, fx, fx2

  end do

  return
end
subroutine beta_sample_test ( )

!*****************************************************************************80
!
!! BETA_SAMPLE_TEST tests BETA_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical beta_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BETA_SAMPLE_TEST:'
  write ( *, '(a)' ) '  BETA_MEAN computes the Beta mean;'
  write ( *, '(a)' ) '  BETA_SAMPLE samples the Beta distribution;'
  write ( *, '(a)' ) '  BETA_VARIANCE computes the Beta variance.'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. beta_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'BETA_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call beta_mean ( a, b, mean )
  call beta_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  do i = 1, sample_num
    call beta_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine beta_binomial_cdf_test ( )

!*****************************************************************************80
!
!! BETA_BINOMIAL_CDF_TEST tests BETA_BINOMIAL_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical beta_binomial_check
  integer ( kind = 4 ) c
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BETA_BINOMIAL_CDF_TEST'
  write ( *, '(a)' ) '  BETA_BINOMIAL_CDF evaluates the Beta Binomial CDF;'
  write ( *, '(a)' ) '  BETA_BINOMIAL_CDF_INV inverts the Beta Binomial CDF.'
  write ( *, '(a)' ) '  BETA_BINOMIAL_PDF evaluates the Beta Binomial PDF;'

  a = 2.0D+00
  b = 3.0D+00
  c = 4

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b
  write ( *, '(a,i8)'    ) '  PDF parameter C = ', c

  if ( .not. beta_binomial_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'BETA_BINOMIAL_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call beta_binomial_sample ( a, b, c, seed, x )

    call beta_binomial_pdf ( x, a, b, c, pdf )

    call beta_binomial_cdf ( x, a, b, c, cdf )

    call beta_binomial_cdf_inv ( cdf, a, b, c, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine beta_binomial_sample_test ( )

!*****************************************************************************80
!
!! BETA_BINOMIAL_SAMPLE_TEST tests BETA_BINOMIAL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical beta_binomial_check
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BETA_BINOMIAL_SAMPLE_TEST'
  write ( *, '(a)' ) '  BETA_BINOMIAL_MEAN computes the Beta Binomial mean;'
  write ( *, '(a)' ) '  BETA_BINOMIAL_SAMPLE samples the Beta Binomial distribution;'
  write ( *, '(a)' ) '  BETA_BINOMIAL_VARIANCE computes the Beta Binomial variance.'

  a = 2.0D+00
  b = 3.0D+00
  c = 4

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b
  write ( *, '(a,i8)'    ) '  PDF parameter C = ', c

  if ( .not. beta_binomial_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'BETA_BINOMIAL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call beta_binomial_mean ( a, b, c, mean )
  call beta_binomial_variance ( a, b, c, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  do i = 1, sample_num
    call beta_binomial_sample ( a, b, c, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine binomial_cdf_test ( )

!*****************************************************************************80
!
!! BINOMIAL_CDF_TEST tests BINOMIAL_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  logical binomial_check
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BINOMIAL_CDF_TEST'
  write ( *, '(a)' ) '  BINOMIAL_CDF evaluates the Binomial CDF;'
  write ( *, '(a)' ) '  BINOMIAL_CDF_INV inverts the Binomial CDF.'
  write ( *, '(a)' ) '  BINOMIAL_PDF evaluates the Binomial PDF;'

  a = 5
  b = 0.65D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. binomial_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'BINOMIAL_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call binomial_sample ( a, b, seed, x )

    call binomial_pdf ( x, a, b, pdf )

    call binomial_cdf ( x, a, b, cdf )

    call binomial_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine binomial_sample_test ( )

!*****************************************************************************80
!
!! BINOMIAL_SAMPLE_TEST tests BINOMIAL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  logical binomial_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BINOMIAL_SAMPLE_TEST'
  write ( *, '(a)' ) '  BINOMIAL_MEAN computes the Binomial mean;'
  write ( *, '(a)' ) '  BINOMIAL_SAMPLE samples the Binomial distribution;'
  write ( *, '(a)' ) '  BINOMIAL_VARIANCE computes the Binomial variance.'

  a = 5
  b = 0.30D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. binomial_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'BINOMIAL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call binomial_mean ( a, b, mean )
  call binomial_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  do i = 1, sample_num
    call binomial_sample ( a, b, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine birthday_cdf_test ( )

!*****************************************************************************80
!
!! BIRTHDAY_CDF_TEST tests BIRTHDAY_CDF.
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

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2
  real ( kind = 8 ) pdf

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BIRTHDAY_CDF_TEST'
  write ( *, '(a)' ) '  BIRTHDAY_CDF evaluates the Birthday CDF;'
  write ( *, '(a)' ) '  BIRTHDAY_CDF_INV inverts the Birthday CDF.'
  write ( *, '(a)' ) '  BIRTHDAY_PDF evaluates the Birthday PDF;'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       N            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do n = 1, 30

    call birthday_pdf ( n, pdf )

    call birthday_cdf ( n, cdf )

    call birthday_cdf_inv ( cdf, n2 )

    write ( *, '(2x,i8,2x,g14.6,2x,g14.6,2x,i8)' ) n, pdf, cdf, n2

  end do

  return
end
subroutine birthday_sample_test ( )

!*****************************************************************************80
!
!! BIRTHDAY_SAMPLE_TEST tests BIRTHDAY_SAMPLE.
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

  integer ( kind = 4 ), parameter :: nsample = 10000

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) mean
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x(nsample)

  seed = 12345678

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BIRTHDAY_SAMPLE_TEST'
  write ( *, '(a)' ) '  BIRTHDAY_SAMPLE samples the Birthday distribution.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N            Mean           PDF'
  write ( *, '(a)' ) ''

  do n = 10, 40

    do i = 1, nsample
      call birthday_sample ( n, seed, x(i) )
    end do

    call i4vec_mean ( nsample, x, mean )
    call birthday_pdf ( n, pdf )

    write ( *, '(2x,i2,2x,g14.6,2x,g14.6)' ) n, mean, pdf

  end do

  return
end
subroutine bradford_cdf_test ( )

!*****************************************************************************80
!
!! BRADFORD_CDF_TEST tests BRADFORD_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical bradford_check
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BRADFORD_CDF_TEST'
  write ( *, '(a)' ) '  BRADFORD_CDF evaluates the Bradford CDF;'
  write ( *, '(a)' ) '  BRADFORD_CDF_INV inverts the Bradford CDF.'
  write ( *, '(a)' ) '  BRADFORD_PDF evaluates the Bradford PDF;'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C = ', c

  if ( .not. bradford_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'BRADFORD_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call bradford_sample ( a, b, c, seed, x )

    call bradford_pdf ( x, a, b, c, pdf )

    call bradford_cdf ( x, a, b, c, cdf )

    call bradford_cdf_inv ( cdf, a, b, c, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine bradford_sample_test ( )

!*****************************************************************************80
!
!! BRADFORD_SAMPLE_TEST tests BRADFORD_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical bradford_check
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BRADFORD_SAMPLE_TEST'
  write ( *, '(a)' ) '  BRADFORD_MEAN computes the Bradford mean;'
  write ( *, '(a)' ) '  BRADFORD_SAMPLE samples the Bradford distribution;'
  write ( *, '(a)' ) '  BRADFORD_VARIANCE computes Bradford the variance.'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C = ', c

  if ( .not. bradford_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'BRADFORD_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call bradford_mean ( a, b, c, mean )
  call bradford_variance ( a, b, c, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  do i = 1, sample_num
    call bradford_sample ( a, b, c, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine buffon_box_pdf_test ( )

!*****************************************************************************80
!
!! BUFFON_BOX_PDF_TEST tests BUFFON_BOX_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) l
  real ( kind = 8 ) pdf

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BUFFON_BOX_PDF_TEST'
  write ( *, '(a)' ) '  BUFFON_BOX_PDF evaluates the Buffon-Laplace PDF,'
  write ( *, '(a)' ) '  the probability that, on a grid of cells of width A'
  write ( *, '(a)' ) '  and height B, a needle of length L, dropped at random,'
  write ( *, '(a)' ) '  will cross at least one grid line.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      A         B         L        PDF'
  write ( *, '(a)' ) ''

  do i = 1, 5
    a = real ( i, kind = 8 )
    do j = 1, 5
      b = real ( j, kind = 8 )

      do k = 0, 5
        l = real ( k, kind = 8 ) * min ( a, b ) / 5.0D+00
        call buffon_box_pdf ( a, b, l, pdf )
        write ( *, '(2x,f8.4,2x,f8.4,2x,f8.4,2x,g14.6)' ) a, b, l, pdf
      end do

      write ( *, '(a)' ) ''

    end do

  end do

  return
end
subroutine buffon_box_sample_test ( )

!*****************************************************************************80
!
!! BUFFON_BOX_SAMPLE_TEST tests BUFFON_BOX_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 4

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) buffon_box_sample
  real ( kind = 8 ) err
  integer ( kind = 4 ) hits
  real ( kind = 8 ) l
  real ( kind = 8 ), parameter :: pi = 3.141592653589793238462643D+00
  real ( kind = 8 ) pi_est
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) trial_num
  integer ( kind = 4 ), dimension ( test_num ) :: trial_num_test = (/ &
    10, 100, 10000, 1000000 /)

  a = 1.0D+00
  b = 1.0D+00
  l = 1.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BUFFON_BOX_SAMPLE_TEST'
  write ( *, '(a)' ) '  BUFFON_BOX_SAMPLE simulates a Buffon-Laplace'
  write ( *, '(a)' ) '  needle dropping experiment.  On a grid of cells of '
  write ( *, '(a)' ) '  width A and height B, a needle of length L is dropped'
  write ( *, '(a)' ) '  at random.  We count the number of times it crosses'
  write ( *, '(a)' ) '  at least one grid line, and use this to estimate '
  write ( *, '(a)' ) '  the value of PI.'

  seed = 123456789

  call random_initialize ( seed )

  write ( *, '(a)' ) ''
  write ( *, '(a,f14.6)' ) '  Cell width A =    ', a
  write ( *, '(a,f14.6)' ) '  Cell height B =   ', b
  write ( *, '(a,f14.6)' ) '  Needle length L = ', l
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    Trials      Hits          Est(Pi)     Err'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    trial_num = trial_num_test(test)

    hits = buffon_box_sample ( a, b, l, trial_num, seed )

    if ( 0 < hits ) then
      pi_est = ( 2.0D+00 * l * ( a + b ) - l * l ) &
        * real ( trial_num, kind = 8 ) &
        / ( a * b * real ( hits, kind = 8 ) )
    else
      pi_est = huge ( pi_est )
    end if

    err = abs ( pi_est - pi )

    write ( *, '(2x,i8,2x,i8,2x,f14.6,2x,g14.6)' ) trial_num, hits, pi_est, err

  end do

  return
end
subroutine buffon_pdf_test ( )

!*****************************************************************************80
!
!! BUFFON_PDF_TEST tests BUFFON_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real ( kind = 8 ) l
  real ( kind = 8 ) pdf

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BUFFON_PDF_TEST'
  write ( *, '(a)' ) '  BUFFON_PDF evaluates the Buffon PDF,'
  write ( *, '(a)' ) '  the probability that, on a grid of cells of width A,'
  write ( *, '(a)' ) '  a needle of length L, dropped at random,'
  write ( *, '(a)' ) '  will cross at least one grid line.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      A         L        PDF'
  write ( *, '(a)' ) ''

  do i = 1, 5
    a = real ( i, kind = 8 )

    do k = 0, 5
      l = real ( k, kind = 8 ) * a / 5.0D+00
      call buffon_pdf ( a, l, pdf )
      write ( *, '(2x,f8.4,2x,f8.4,2x,g14.6)' ) a, l, pdf
    end do

    write ( *, '(a)' ) ''

  end do

  return
end
subroutine buffon_sample_test ( )

!*****************************************************************************80
!
!! BUFFON_SAMPLE_TEST tests BUFFON_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 4

  real ( kind = 8 ) a
  integer ( kind = 4 ) buffon_sample
  real ( kind = 8 ) err
  integer ( kind = 4 ) hits
  real ( kind = 8 ) l
  real ( kind = 8 ), parameter :: pi = 3.141592653589793238462643D+00
  real ( kind = 8 ) pi_est
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) trial_num
  integer ( kind = 4 ), dimension ( test_num ) :: trial_num_test = (/ &
    10, 100, 10000, 1000000 /)

  a = 1.0D+00
  l = 1.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BUFFON_SAMPLE_TEST'
  write ( *, '(a)' ) '  BUFFON_SAMPLE simulates a Buffon-Laplace'
  write ( *, '(a)' ) '  needle dropping experiment.  On a grid of cells of '
  write ( *, '(a)' ) '  width A, a needle of length L is dropped'
  write ( *, '(a)' ) '  at random.  We count the number of times it crosses'
  write ( *, '(a)' ) '  at least one grid line, and use this to estimate '
  write ( *, '(a)' ) '  the value of PI.'

  seed = 123456789

  call random_initialize ( seed )

  write ( *, '(a)' ) ''
  write ( *, '(a,f14.6)' ) '  Cell width A =    ', a
  write ( *, '(a,f14.6)' ) '  Needle length L = ', l
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    Trials      Hits          Est(Pi)     Err'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    trial_num = trial_num_test(test)

    hits = buffon_sample ( a, l, trial_num )

    if ( 0 < hits ) then
      pi_est = ( 2.0D+00 * l ) * real ( trial_num, kind = 8 ) &
        / ( a * real ( hits, kind = 8 ) )
    else
      pi_est = huge ( pi_est )
    end if

    err = abs ( pi_est - pi )

    write ( *, '(2x,i8,2x,i8,2x,f14.6,2x,g14.6)' ) trial_num, hits, pi_est, err

  end do

  return
end
subroutine burr_cdf_test ( )

!*****************************************************************************80
!
!! BURR_CDF_TEST tests BURR_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical burr_check
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) d
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BURR_CDF_TEST'
  write ( *, '(a)' ) '  BURR_CDF evaluates the Burr CDF;'
  write ( *, '(a)' ) '  BURR_CDF_INV inverts the Burr CDF.'
  write ( *, '(a)' ) '  BURR_PDF evaluates the Burr PDF;'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00
  d = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C = ', c
  write ( *, '(a,g14.6)' ) '  PDF parameter D = ', d

  if ( .not. burr_check ( a, b, c, d ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'BURR_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call burr_sample ( a, b, c, d, seed, x )

    call burr_pdf ( x, a, b, c, d, pdf )

    call burr_cdf ( x, a, b, c, d, cdf )

    call burr_cdf_inv ( cdf, a, b, c, d, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine burr_sample_test ( )

!*****************************************************************************80
!
!! BURR_SAMPLE_TEST tests BURR_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical burr_check
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BURR_SAMPLE_TEST'
  write ( *, '(a)' ) '  BURR_MEAN computes the Burr mean;'
  write ( *, '(a)' ) '  BURR_VARIANCE computes the Burr variance;'
  write ( *, '(a)' ) '  BURR_SAMPLE samples the Burr distribution;'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00
  d = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C = ', c
  write ( *, '(a,g14.6)' ) '  PDF parameter D = ', d

  if ( .not. burr_check ( a, b, c, d ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'BURR_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call burr_mean ( a, b, c, d, mean )
  call burr_variance ( a, b, c, d, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  do i = 1, sample_num
    call burr_sample ( a, b, c, d, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine cardioid_cdf_test ( )

!*****************************************************************************80
!
!! CARDIOID_CDF_TEST tests CARDIOID_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) :: a = 0.0D+00
  real ( kind = 8 ) :: b = 0.25D+00
  logical cardioid_check
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CARDIOID_CDF_TEST'
  write ( *, '(a)' ) '  CARDIOID_CDF evaluates the Cardioid CDF;'
  write ( *, '(a)' ) '  CARDIOID_CDF_INV inverts the Cardioid CDF.'
  write ( *, '(a)' ) '  CARDIOID_PDF evaluates the Cardioid PDF;'

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. cardioid_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CARDIOID_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call cardioid_sample ( a, b, seed, x )
    call cardioid_pdf ( x, a, b, pdf )
    call cardioid_cdf ( x, a, b, cdf )
    call cardioid_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine cardioid_sample_test ( )

!*****************************************************************************80
!
!! CARDIOID_SAMPLE_TEST tests CARDIOID_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) :: a = 0.0D+00
  real ( kind = 8 ) :: b = 0.25D+00
  logical cardioid_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CARDIOID_SAMPLE_TEST'
  write ( *, '(a)' ) '  CARDIOID_MEAN computes the Cardioid mean;'
  write ( *, '(a)' ) '  CARDIOID_SAMPLE samples the Cardioid distribution;'
  write ( *, '(a)' ) '  CARDIOID_VARIANCE computes the Cardioid variance.'

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. cardioid_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CARDIOID_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call cardioid_mean ( a, b, mean )
  call cardioid_variance ( a, b, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call cardioid_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine cauchy_cdf_test ( )

!*****************************************************************************80
!
!! CAUCHY_CDF_TEST tests CAUCHY_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical cauchy_check
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CAUCHY_CDF_TEST'
  write ( *, '(a)' ) '  CAUCHY_CDF evaluates the Cauchy CDF;'
  write ( *, '(a)' ) '  CAUCHY_CDF_INV inverts the Cauchy CDF.'
  write ( *, '(a)' ) '  CAUCHY_PDF evaluates the Cauchy PDF;'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. cauchy_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CAUCHY_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call cauchy_sample ( a, b, seed, x )

    call cauchy_pdf ( x, a, b, pdf )

    call cauchy_cdf ( x, a, b, cdf )

    call cauchy_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine cauchy_sample_test ( )

!*****************************************************************************80
!
!! CAUCHY_SAMPLE_TEST tests CAUCHY_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical cauchy_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CAUCHY_SAMPLE_TEST'
  write ( *, '(a)' ) '  CAUCHY_MEAN computes the Cauchy mean;'
  write ( *, '(a)' ) '  CAUCHY_VARIANCE computes the Cauchy variance;'
  write ( *, '(a)' ) '  CAUCHY_SAMPLE samples the Cauchy distribution.'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. cauchy_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CAUCHY_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call cauchy_mean ( a, b, mean )
  call cauchy_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  do i = 1, sample_num
    call cauchy_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine chebyshev1_cdf_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV1_CDF_TEST tests CHEBYSHEV1_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) chebyshev1_sample
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV1_CDF_TEST'
  write ( *, '(a)' ) '  CHEBYSHEV1_CDF evaluates the Chebyshev1 CDF;'
  write ( *, '(a)' ) '  CHEBYSHEV1_CDF_INV inverts the Chebyshev1 CDF.'
  write ( *, '(a)' ) '  CHEBYSHEV1_PDF evaluates the Chebyshev1 PDF;'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    x = chebyshev1_sample ( seed )

    call chebyshev1_pdf ( x, pdf )

    call chebyshev1_cdf ( x, cdf )

    call chebyshev1_cdf_inv ( cdf, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine chebyshev1_sample_test ( )

!*****************************************************************************80
!
!! CHEBYSHEV1_SAMPLE_TEST tests CHEBYSHEV1_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) chebyshev1_sample
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHEBYSHEV1_SAMPLE_TEST'
  write ( *, '(a)' ) '  CHEBYSHEV1_MEAN computes the Chebyshev1 mean;'
  write ( *, '(a)' ) '  CHEBYSHEV1_SAMPLE samples the Chebyshev1 distribution;'
  write ( *, '(a)' ) '  CHEBYSHEV1_VARIANCE computes the Chebyshev1 variance.'

  call chebyshev1_mean ( mean )
  call chebyshev1_variance ( variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =            ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =        ', variance

  do i = 1, sample_num
    x(i) = chebyshev1_sample ( seed )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine chi_cdf_test ( )

!*****************************************************************************80
!
!! CHI_CDF_TEST tests CHI_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  logical chi_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHI_CDF_TEST'
  write ( *, '(a)' ) '  CHI_CDF evaluates the Chi CDF.'
  write ( *, '(a)' ) '  CHI_CDF_INV inverts the Chi CDF.'
  write ( *, '(a)' ) '  CHI_PDF evaluates the Chi PDF.'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. chi_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CHI_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call chi_sample ( a, b, c, seed, x )

    call chi_pdf ( x, a, b, c, pdf )

    call chi_cdf ( x, a, b, c, cdf )

    call chi_cdf_inv ( cdf, a, b, c, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine chi_sample_test ( )

!*****************************************************************************80
!
!! CHI_SAMPLE_TEST tests CHI_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  logical chi_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHI_SAMPLE_TEST'
  write ( *, '(a)' ) '  CHI_MEAN computes the Chi mean;'
  write ( *, '(a)' ) '  CHI_VARIANCE computes the Chi variance;'
  write ( *, '(a)' ) '  CHI_SAMPLE samples the Chi distribution.'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C = ', c

  if ( .not. chi_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CHI_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call chi_mean ( a, b, c, mean )
  call chi_variance ( a, b, c, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  do i = 1, sample_num
    call chi_sample ( a, b, c, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine chi_square_cdf_test ( )

!*****************************************************************************80
!
!! CHI_SQUARE_CDF_TEST tests CHI_SQUARE_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  logical chi_square_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHI_SQUARE_CDF_TEST'
  write ( *, '(a)' ) '  CHI_SQUARE_CDF evaluates the Chi Square CDF;'
  write ( *, '(a)' ) '  CHI_SQUARE_CDF_INV inverts the Chi Square CDF.'
  write ( *, '(a)' ) '  CHI_SQUARE_PDF evaluates the Chi Square PDF;'

  a = 4.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a

  if ( .not. chi_square_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CHI_SQUARE_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call chi_square_sample ( a, seed, x )

    call chi_square_pdf ( x, a, pdf )

    call chi_square_cdf ( x, a, cdf )

    call chi_square_cdf_inv ( cdf, a, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine chi_square_sample_test ( )

!*****************************************************************************80
!
!! CHI_SQUARE_SAMPLE_TEST tests CHI_SQUARE_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  logical chi_square_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHI_SQUARE_SAMPLE_TEST'
  write ( *, '(a)' ) '  CHI_SQUARE_MEAN computes the Chi Square mean;'
  write ( *, '(a)' ) '  CHI_SQUARE_SAMPLE samples the Chi Square distribution;'
  write ( *, '(a)' ) '  CHI_SQUARE_VARIANCE computes the Chi Square variance.'

  a = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a

  if ( .not. chi_square_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CHI_SQUARE_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call chi_square_mean ( a, mean )
  call chi_square_variance ( a, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call chi_square_sample ( a, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine chi_square_noncentral_sample_test ( )

!*****************************************************************************80
!
!! CHI_SQUARE_NONCENTRAL_SAMPLE_TEST tests CHI_SQUARE_NONCENTRAL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical chi_square_noncentral_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CHI_SQUARE_NONCENTRAL_SAMPLE_TEST'
  write ( *, '(a)' ) '  CHI_SQUARE_NONCENTRAL_MEAN computes the Chi Square Noncentral mean.'
  write ( *, '(a)' ) '  CHI_SQUARE_NONCENTRAL_SAMPLE samples the Chi Square Noncentral distribution.'
  write ( *, '(a)' ) '  CHI_SQUARE_NONCENTRAL_VARIANCE computes the Chi Square Noncentral variance.'

  a = 3.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. chi_square_noncentral_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CHI_SQUARE_NONCENTRAL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call chi_square_noncentral_mean ( a, b, mean )
  call chi_square_noncentral_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  write ( *, '(a)' ) ''
  write ( *, '(a,i12)'   ) '  Initial seed =    ', seed

  do i = 1, sample_num
    call chi_square_noncentral_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a,i12)'   ) '  Final seed =      ', seed
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine circular_normal_01_sample_test ( )

!*****************************************************************************80
!
!! CIRCULAR_NORMAL_01_SAMPLE_TEST tests CIRCULAR_NORMAL_01_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) mean(2)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance(2)
  real ( kind = 8 ) x(2)
  real ( kind = 8 ) x_table(sample_num,2)
  real ( kind = 8 ) xmax(2)
  real ( kind = 8 ) xmin(2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CIRCULAR_NORMAL_01_SAMPLE_TEST'
  write ( *, '(a)' ) '  CIRCULAR_NORMAL_01_MEAN computes the Circular Normal 01 mean;'
  write ( *, '(a)' ) '  CIRCULAR_NORMAL_01_SAMPLE samples the Circular Normal 01 distribution;'
  write ( *, '(a)' ) '  CIRCULAR_NORMAL_01_VARIANCE computes the Circular Normal 01  variance.'

  call circular_normal_01_mean ( mean )
  call circular_normal_01_variance ( variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,2g14.6)' ) '  PDF means =               ', mean(1:2)
  write ( *, '(a,2g14.6)' ) '  PDF variances =           ', variance(1:2)

  do i = 1, sample_num
    call circular_normal_01_sample ( seed, x )
    x_table(i,1) = x(1)
    x_table(i,2) = x(2)
  end do

  do j = 1, 2
    call r8vec_mean ( sample_num, x_table(1,j), mean(j) )
    call r8vec_variance ( sample_num, x_table(1,j), variance(j) )
    call r8vec_max ( sample_num, x_table(1,j), xmax(j) )
    call r8vec_min ( sample_num, x_table(1,j), xmin(j) )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'     ) '  Sample size =     ', sample_num
  write ( *, '(a,2g14.6)' ) '  Sample mean =     ', mean(1:2)
  write ( *, '(a,2g14.6)' ) '  Sample variance = ', variance(1:2)
  write ( *, '(a,2g14.6)' ) '  Sample maximum =  ', xmax(1:2)
  write ( *, '(a,2g14.6)' ) '  Sample minimum =  ', xmin(1:2)

  return
end
subroutine circular_normal_sample_test ( )

!*****************************************************************************80
!
!! CIRCULAR_NORMAL_SAMPLE_TEST tests CIRCULAR_NORMAL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) mean(2)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance(2)
  real ( kind = 8 ) x(2)
  real ( kind = 8 ) x_table(sample_num,2)
  real ( kind = 8 ) xmax(2)
  real ( kind = 8 ) xmin(2)

  a(1) = 1.0D+00
  a(2) = 5.0D+00
  b = 0.75D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CIRCULAR_NORMAL_SAMPLE_TEST'
  write ( *, '(a)' ) '  CIRCULAR_NORMAL_MEAN computes the Circular Normal mean;'
  write ( *, '(a)' ) '  CIRCULAR_NORMAL_SAMPLE samples the Circular Normal distribution;'
  write ( *, '(a)' ) '  CIRCULAR_NORMAL_VARIANCE computes the Circular Normal variance.'

  call circular_normal_mean ( a, b, mean )
  call circular_normal_variance ( a, b, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,2g14.6)' ) '  PDF means =               ', mean(1:2)
  write ( *, '(a,2g14.6)' ) '  PDF variances =           ', variance(1:2)

  do i = 1, sample_num
    call circular_normal_sample ( a, b, seed, x )
    x_table(i,1) = x(1)
    x_table(i,2) = x(2)
  end do

  do j = 1, 2
    call r8vec_mean ( sample_num, x_table(1,j), mean(j) )
    call r8vec_variance ( sample_num, x_table(1,j), variance(j) )
    call r8vec_max ( sample_num, x_table(1,j), xmax(j) )
    call r8vec_min ( sample_num, x_table(1,j), xmin(j) )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'     ) '  Sample size =     ', sample_num
  write ( *, '(a,2g14.6)' ) '  Sample mean =     ', mean(1:2)
  write ( *, '(a,2g14.6)' ) '  Sample variance = ', variance(1:2)
  write ( *, '(a,2g14.6)' ) '  Sample maximum =  ', xmax(1:2)
  write ( *, '(a,2g14.6)' ) '  Sample minimum =  ', xmin(1:2)

  return
end
subroutine cosine_cdf_test ( )

!*****************************************************************************80
!
!! COSINE_CDF_TEST tests COSINE_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  logical cosine_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COSINE_CDF_TEST'
  write ( *, '(a)' ) '  COSINE_CDF evaluates the Cosine CDF.'
  write ( *, '(a)' ) '  COSINE_CDF_INV inverts the Cosine CDF.'
  write ( *, '(a)' ) '  COSINE_PDF evaluates the Cosine PDF.'

  a = 2.0D+00
  b = 1.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. cosine_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'COSINE_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call cosine_sample ( a, b, seed, x )

    call cosine_pdf ( x, a, b, pdf )

    call cosine_cdf ( x, a, b, cdf )

    call cosine_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine cosine_sample_test ( )

!*****************************************************************************80
!
!! COSINE_SAMPLE_TEST tests COSINE_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical cosine_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COSINE_SAMPLE_TEST'
  write ( *, '(a)' ) '  COSINE_MEAN computes the Cosine mean;'
  write ( *, '(a)' ) '  COSINE_SAMPLE samples the Cosine distribution;'
  write ( *, '(a)' ) '  COSINE_VARIANCE computes the Cosine variance.'

  a = 2.0D+00
  b = 1.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. cosine_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'COSINE_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call cosine_mean ( a, b, mean )
  call cosine_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call cosine_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine coupon_complete_pdf_test ( )

!*****************************************************************************80
!
!! COUPON_COMPLETE_PDF_TEST tests COUPON_COMPLETE_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) box_num
  real ( kind = 8 ) cdf
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) type_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COUPON_COMPLETE_PDF_TEST'
  write ( *, '(a)' ) '  COUPON_COMPLETE_PDF evaluates the coupon collector''s'
  write ( *, '(a)' ) '  complete collection pdf.'
  write ( *, '(a)' ) ''

  do type_num = 2, 4

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Number of coupon types is ', type_num
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '   BOX_NUM      PDF             CDF'
    write ( *, '(a)' ) ''
    cdf = 0.0D+00
    do box_num = 1, 20
      call coupon_complete_pdf ( type_num, box_num, pdf )
      cdf = cdf + pdf
      write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) box_num, pdf, cdf
    end do

  end do

  return
end
subroutine coupon_sample_test ( )

!*****************************************************************************80
!
!! COUPON_SAMPLE_TEST tests COUPON_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_trial = 10
  integer ( kind = 4 ), parameter :: max_type = 25

  real ( kind = 8 ) average
  integer ( kind = 4 ) coupon(max_type)
  real ( kind = 8 ) expect
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n_coupon
  integer ( kind = 4 ) n_type
  integer ( kind = 4 ) :: seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COUPON_SAMPLE_TEST'
  write ( *, '(a)' ) '  COUPON_SAMPLE samples the coupon PDF.'
  write ( *, '(a)' ) ''

  do n_type = 5, max_type, 5

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  Number of coupon types is ', n_type
    expect = real ( n_type, kind = 8 ) * log ( real ( n_type, kind = 8 ) )
    write ( *, '(a,g14.6)' ) '  Expected wait is about ', expect
    write ( *, '(a)' ) ''

    average = 0.0D+00
    do i = 1, n_trial
      call coupon_sample ( n_type, seed, coupon, n_coupon )
      write ( *, '(2i5)' ) i, n_coupon
      average = average + real ( n_coupon, kind = 8 )
    end do

    average = average / real ( n_trial, kind = 8 )
    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  Average wait was ', average

  end do

  return
end
subroutine deranged_cdf_test ( )

!*****************************************************************************80
!
!! DERANGED_CDF_TEST tests DERANGED_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) cdf
  logical deranged_check
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DERANGED_CDF_TEST'
  write ( *, '(a)' ) '  DERANGED_CDF evaluates the Deranged CDF;'
  write ( *, '(a)' ) '  DERANGED_CDF_INV inverts the Deranged CDF.'
  write ( *, '(a)' ) '  DERANGED_PDF evaluates the Deranged PDF;'

  a = 7

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter A = ', a

  if ( .not. deranged_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DERANGED_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do x = 0, a

    call deranged_pdf ( x, a, pdf )

    call deranged_cdf ( x, a, cdf )

    call deranged_cdf_inv ( cdf, a, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine deranged_sample_test ( )

!*****************************************************************************80
!
!! DERANGED_SAMPLE_TEST tests DERANGED_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) a
  logical deranged_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DERANGED_SAMPLE_TEST'
  write ( *, '(a)' ) '  DERANGED_MEAN computes the Deranged mean.'
  write ( *, '(a)' ) '  DERANGED_VARIANCE computes the Deranged variance.'
  write ( *, '(a)' ) '  DERANGED_SAMPLE samples the Deranged distribution.'

  a = 7

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter A =             ', a

  if ( .not. deranged_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DERANGED_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call deranged_mean ( a, mean )
  call deranged_variance ( a, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call deranged_sample ( a, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine dipole_cdf_test ( )

!*****************************************************************************80
!
!! DIPOLE_CDF_TEST tests DIPOLE_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 3

  real ( kind = 8 ) a
  real ( kind = 8 ) atest(test_num)
  real ( kind = 8 ) b
  real ( kind = 8 ) btest(test_num)
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_pi
  logical dipole_check
  integer ( kind = 4 ) i
  integer ( kind = 4 ) itest
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DIPOLE_CDF_TEST'
  write ( *, '(a)' ) '  DIPOLE_CDF evaluates the Dipole CDF.'
  write ( *, '(a)' ) '  DIPOLE_CDF_INV inverts the Dipole CDF.'
  write ( *, '(a)' ) '  DIPOLE_PDF evaluates the Dipole PDF.'

  atest(1) = 0.0D+00
  btest(1) = 1.0D+00
  atest(2) = r8_pi() / 4.0D+00
  btest(2) = 0.5D+00
  atest(3) = r8_pi() / 2.0D+00
  btest(3) = 0.0D+00

  do itest = 1, test_num

    a = atest(itest)
    b = btest(itest)

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
    write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

    if ( .not. dipole_check ( a, b ) ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'DIPOLE_CDF_TEST - Fatal error!'
      write ( *, '(a)' ) '  The parameters are not legal.'
      return
    end if

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
    write ( *, '(a)' ) ''

    do i = 1, 10

      call dipole_sample ( a, b, seed, x )

      call dipole_pdf ( x, a, b, pdf )

      call dipole_cdf ( x, a, b, cdf )

      call dipole_cdf_inv ( cdf, a, b, x2 )

      write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

    end do

  end do

  return
end
subroutine dipole_sample_test ( )

!*****************************************************************************80
!
!! DIPOLE_SAMPLE_TEST tests DIPOLE_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 10000
  integer ( kind = 4 ), parameter :: test_num = 3

  real ( kind = 8 ) a
  real ( kind = 8 ), dimension ( test_num ) :: a_test = (/ &
    0.0D+00, 0.785398163397448D+00, 1.57079632679490D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), dimension ( test_num ) :: b_test = (/ &
    1.0D+00, 0.5D+00, 0.0D+00 /)
  logical dipole_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DIPOLE_SAMPLE_TEST'
  write ( *, '(a)' ) '  DIPOLE_SAMPLE samples the Dipole distribution.'

  do test = 1, test_num

    a = a_test(test)
    b = b_test(test)

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
    write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

    if ( .not. dipole_check ( a, b ) ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'DIPOLE_SAMPLE_TEST - Fatal error!'
      write ( *, '(a)' ) '  The parameters are not legal.'
      return
    end if

    do i = 1, sample_num
      call dipole_sample ( a, b, seed, x(i) )
    end do

    call r8vec_mean ( sample_num, x, mean )
    call r8vec_variance ( sample_num, x, variance )
    call r8vec_max ( sample_num, x, xmax )
    call r8vec_min ( sample_num, x, xmin )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
    write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
    write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
    write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
    write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  end do

  return
end
subroutine dirichlet_sample_test ( )

!*****************************************************************************80
!
!! DIRICHLET_SAMPLE_TEST tests DIRICHLET_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3
  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a(n)
  logical dirichlet_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean(n)
  real ( kind = 8 ) m2(n,n)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance(n)
  real ( kind = 8 ) x(n,sample_num)
  real ( kind = 8 ) xmax(n)
  real ( kind = 8 ) xmin(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DIRICHLET_SAMPLE_TEST'
  write ( *, '(a)' ) '  DIRICHLET_SAMPLE samples the Dirichlet distribution;'
  write ( *, '(a)' ) '  DIRICHLET_MEAN computes the Dirichlet mean;'
  write ( *, '(a)' ) '  DIRICHLET_VARIANCE computes the Dirichlet variance.'

  a(1:n) = (/ 0.250D+00, 0.500D+00, 1.250D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of components N =        ', n

  call r8vec_print ( n, a, '  PDF parameters A:' )
  write ( *, '(a)'    ) '  PDF parameters A(1:N):'

  if ( .not. dirichlet_check ( n, a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DIRICHLET_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call dirichlet_mean ( n, a, mean )

  call dirichlet_variance ( n, a, variance )

  call r8vec_print ( n, mean, '  PDF mean:' )

  call r8vec_print ( n, variance, '  PDF variance:' )

  call dirichlet_moment2 ( n, a, m2 )

  call r8mat_print ( n, n, m2, '  Second moments:' )

  do i = 1, sample_num
    call dirichlet_sample ( n, a, seed, x(1,i) )
  end do

  call r8row_max ( n, sample_num, x, xmax )
  call r8row_min ( n, sample_num, x, xmin )
  call r8row_mean ( n, sample_num, x, mean )
  call r8row_variance ( n, sample_num, x, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Sample size = ', sample_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Observed Mean, Variance, Max, Min:'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,i8,4g14.6)' ) i, mean(i), variance(i), xmax(i), xmin(i)
  end do

  return
end
subroutine dirichlet_pdf_test ( )

!*****************************************************************************80
!
!! DIRICHLET_PDF_TEST tests DIRICHLET_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  real ( kind = 8 ) a(n)
  logical dirichlet_check
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DIRICHLET_PDF_TEST'
  write ( *, '(a)' ) '  DIRICHLET_PDF evaluates the Dirichlet PDF.'

  a(1:3) = (/ 0.250D+00, 0.500D+00, 1.250D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of components N =        ', n

  call r8vec_print ( n, a, '  PDF parameters A:' )

  if ( .not. dirichlet_check ( n, a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DIRICHLET_PDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  x(1:3) = (/ 0.500D+00, 0.125D+00, 0.375D+00 /)

  call r8vec_print ( n, x, '  PDF argument X: ' )

  call dirichlet_pdf ( x, n, a, pdf )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF value =           ', pdf

  return
end
subroutine dirichlet_mix_sample_test ( )

!*****************************************************************************80
!
!! DIRICHLET_MIX_SAMPLE_TEST tests DIRICHLET_MIX_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: comp_num = 2
  integer ( kind = 4 ), parameter :: elem_num = 3
  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a(elem_num,comp_num)
  integer ( kind = 4 ) comp
  real ( kind = 8 ) comp_weight(comp_num)
  logical dirichlet_mix_check
  integer ( kind = 4 ) elem_i
  integer ( kind = 4 ) j
  real ( kind = 8 ) mean(elem_num)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance(elem_num)
  real ( kind = 8 ) x(elem_num,sample_num)
  real ( kind = 8 ) xmax(elem_num)
  real ( kind = 8 ) xmin(elem_num)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DIRICHLET_MIX_SAMPLE_TEST'
  write ( *, '(a)' ) '  DIRICHLET_MIX_SAMPLE samples the Dirichlet Mix distribution;'
  write ( *, '(a)' ) '  DIRICHLET_MIX_MEAN computes the Dirichlet Mix mean;'

  a(1,1) = 0.250D+00
  a(2,1) = 0.500D+00
  a(3,1) = 1.250D+00

  a(1,2) = 1.500D+00
  a(2,2) = 0.500D+00
  a(3,2) = 2.000D+00

  comp_weight(1) = 1.0D+00
  comp_weight(2) = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Number of elements ELEM_NUM =   ', elem_num
  write ( *, '(a,i8)' ) '  Number of components COMP_NUM = ', comp_num
  call r8mat_print ( elem_num, comp_num, a, '  PDF parameters A(ELEM,COMP):' )
  call r8vec_print ( comp_num, comp_weight, '  Component weights' )

  if ( .not. dirichlet_mix_check ( comp_num, elem_num, a, comp_weight ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DIRICHLET_MIX_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call dirichlet_mix_mean ( comp_num, elem_num, a, comp_weight, mean )

  call r8vec_print ( elem_num, mean, '  PDF means: ' )

  do j = 1, sample_num
    call dirichlet_mix_sample ( comp_num, elem_num, a, &
      comp_weight, seed, comp, x(1,j) )
  end do

  call r8row_max ( elem_num, sample_num, x, xmax )

  call r8row_min ( elem_num, sample_num, x, xmin )

  call r8row_mean ( elem_num, sample_num, x, mean )

  call r8row_variance ( elem_num, sample_num, x, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Sample size = ', sample_num
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Observed Mean, Variance, Max, Min:'
  write ( *, '(a)' ) ''

  do elem_i = 1, elem_num
    write ( *, '(2x,i8,4g14.6)' ) elem_i, &
      mean(elem_i), variance(elem_i), xmax(elem_i), xmin(elem_i)
  end do

  return
end
subroutine dirichlet_mix_pdf_test ( )

!*****************************************************************************80
!
!! DIRICHLET_MIX_PDF_TEST tests DIRICHLET_MIX_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: comp_num = 2
  integer ( kind = 4 ), parameter :: elem_num = 3

  real ( kind = 8 ) a(elem_num,comp_num)
  real ( kind = 8 ) comp_weight(comp_num)
  logical dirichlet_mix_check
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x(elem_num)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DIRICHLET_MIX_PDF_TEST'
  write ( *, '(a)' ) '  DIRICHLET_MIX_PDF evaluates the Dirichlet Mix PDF.'

  a(1,1) = 0.250D+00
  a(2,1) = 0.500D+00
  a(3,1) = 1.250D+00

  a(1,2) = 1.500D+00
  a(2,2) = 0.500D+00
  a(3,2) = 2.000D+00

  comp_weight(1:2) = (/ 1.0D+00, 2.0D+00 /)

  write ( *, '(a,i8)' ) '  Number of elements ELEM_NUM =   ', elem_num
  write ( *, '(a,i8)' ) '  Number of components COMP_NUM = ', comp_num
  call r8mat_print ( elem_num, comp_num, a, '  PDF parameters A(ELEM,COMP):' )
  call r8vec_print ( comp_num, comp_weight, '  Component weights' )

  if ( .not. dirichlet_mix_check ( comp_num, elem_num, a, comp_weight ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DIRICHLET_MIX_PDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  x(1:3) = (/ 0.500D+00, 0.125D+00, 0.375D+00 /)

  call r8vec_print ( elem_num, x, '  PDF argument X: ' )

  call dirichlet_mix_pdf ( x, comp_num, elem_num, a, comp_weight, &
    pdf )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF value =           ', pdf

  return
end
subroutine discrete_cdf_test ( )

!*****************************************************************************80
!
!! DISCRETE_CDF_TEST tests DISCRETE_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: a = 6

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) cdf
  logical discrete_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DISCRETE_CDF_TEST'
  write ( *, '(a)' ) '  DISCRETE_CDF evaluates the Discrete CDF;'
  write ( *, '(a)' ) '  DISCRETE_CDF_INV inverts the Discrete CDF.'
  write ( *, '(a)' ) '  DISCRETE_PDF evaluates the Discrete PDF;'

  b(1:6) = (/ 1.0D+00, 2.0D+00, 6.0D+00, 2.0D+00, 4.0D+00, 1.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  PDF parameter A = ', a
  call r8vec_print ( a, b, '  PDF parameters B = ' )

  if ( .not. discrete_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DISCRETE_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call discrete_sample ( a, b, seed, x )

    call discrete_pdf ( x, a, b, pdf )

    call discrete_cdf ( x, a, b, cdf )

    call discrete_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine discrete_sample_test ( )

!*****************************************************************************80
!
!! DISCRETE_SAMPLE_TEST tests DISCRETE_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: a = 6
  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) b(a)
  logical discrete_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DISCRETE_SAMPLE_TEST'
  write ( *, '(a)' ) '  DISCRETE_MEAN computes the Discrete mean;'
  write ( *, '(a)' ) '  DISCRETE_SAMPLE samples the Discrete distribution;'
  write ( *, '(a)' ) '  DISCRETE_VARIANCE computes the Discrete variance.'

  b(1:6) = (/ 1.0D+00, 2.0D+00, 6.0D+00, 2.0D+00, 4.0D+00, 1.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  PDF parameter A =             ', a
  call r8vec_print ( a, b, '  PDF parameters B = ' )

  if ( .not. discrete_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DISCRETE_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call discrete_mean ( a, b, mean )
  call discrete_variance ( a, b, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call discrete_sample ( a, b, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine disk_sample_test ( )

!*****************************************************************************80
!
!! DISK_SAMPLE_TEST tests DISK_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) mean(2)
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x_table(sample_num,2)
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) xmax(2)
  real ( kind = 8 ) xmin(2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DISK_SAMPLE_TEST'
  write ( *, '(a)' ) '  DISK_MEAN returns the Disk mean.'
  write ( *, '(a)' ) '  DISK_SAMPLE samples the Disk distribution.'
  write ( *, '(a)' ) '  DISK_VARIANCE returns the Disk variance.'

  a = 10.0D+00
  b = 4.0D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  X coordinate of center is A = ', a
  write ( *, '(a,g14.6)' ) '  Y coordinate of center is B = ', b
  write ( *, '(a,g14.6)' ) '  Radius is C =                 ', c

  call disk_mean ( a, b, c, mean )
  call disk_variance ( a, b, c, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,2x,g14.6)'     ) '  Disk mean =     ', mean(1:2)
  write ( *, '(a,g14.6)' ) '  Disk variance = ', variance

  do i = 1, sample_num
    call disk_sample ( a, b, c, seed, x1, x2 )
    x_table(i,1) = x1
    x_table(i,2) = x2
  end do

  variance = sum ( ( x_table(1:sample_num,1) - a ) ** 2 &
                 + ( x_table(1:sample_num,2) - b ) ** 2 ) &
    / real ( sample_num, kind = 8 )

  do j = 1, 2
    call r8vec_mean ( sample_num, x_table(1,j), mean(j) )
    call r8vec_max ( sample_num, x_table(1,j), xmax(j) )
    call r8vec_min ( sample_num, x_table(1,j), xmin(j) )
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'     ) '  Sample size =     ', sample_num
  write ( *, '(a,2g14.6)' ) '  Sample mean =     ', mean(1:2)
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,2g14.6)' ) '  Sample maximum =  ', xmax(1:2)
  write ( *, '(a,2g14.6)' ) '  Sample minimum =  ', xmin(1:2)

  return
end
subroutine empirical_discrete_cdf_test ( )

!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_CDF_TEST tests EMPIRICAL_DISCRETE_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: a = 6

  real ( kind = 8 ), save, dimension ( a ) :: b = (/ &
    1.0D+00, 1.0D+00, 3.0D+00, 2.0D+00, 1.0D+00, 2.0D+00 /)
  real ( kind = 8 ), save, dimension ( a ) :: c = (/ &
    0.0D+00, 1.0D+00, 2.0D+00, 4.5D+00, 6.0D+00, 10.0D+00 /)
  real ( kind = 8 ) cdf
  logical empirical_discrete_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CDF_TEST'
  write ( *, '(a)' ) '  EMPIRICAL_DISCRETE_CDF evaluates the Empirical Discrete CDF;'
  write ( *, '(a)' ) '  EMPIRICAL_DISCRETE_CDF_INV inverts the Empirical Discrete CDF.'
  write ( *, '(a)' ) '  EMPIRICAL_DISCRETE_PDF evaluates the Empirical Discrete PDF;'

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter A =             ', a
  call r8vec_print ( a, b, '  PDF parameter B:' )
  call r8vec_print ( a, c, '  PDF parameter C:' )

  if ( .not. empirical_discrete_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call empirical_discrete_sample ( a, b, c, seed, x )

    call empirical_discrete_pdf ( x, a, b, c, pdf )

    call empirical_discrete_cdf ( x, a, b, c, cdf )

    call empirical_discrete_cdf_inv ( cdf, a, b, c, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine empirical_discrete_sample_test ( )

!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_SAMPLE_TEST tests EMPIRICAL_DISCRETE_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: a = 6
  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ), save, dimension ( a ) :: b = (/ &
    1.0D+00, 1.0D+00, 3.0D+00, 2.0D+00, 1.0D+00, 2.0D+00 /)
  real ( kind = 8 ), save, dimension ( a ) :: c = (/ &
    0.0D+00, 1.0D+00, 2.0D+00, 4.5D+00, 6.0D+00, 10.0D+00 /)
  logical empirical_discrete_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_SAMPLE_TEST'
  write ( *, '(a)' ) '  EMPIRICAL_DISCRETE_MEAN computes the Empirical Discrete mean;'
  write ( *, '(a)' ) '  EMPIRICAL_DISCRETE_SAMPLE samples the Empirical Discrete distribution;'
  write ( *, '(a)' ) '  EMPIRICAL_DISCRETE_VARIANCE computes the Empirical Discrete variance.'

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'   ) '  PDF parameter A =             ', a
  call r8vec_print ( a, b, '  PDF parameter B:' )
  call r8vec_print ( a, c, '  PDF parameter C:' )

  if ( .not. empirical_discrete_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call empirical_discrete_mean ( a, b, c, mean )
  call empirical_discrete_variance ( a, b, c, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call empirical_discrete_sample ( a, b, c, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine english_letter_cdf_test ( )

!*****************************************************************************80
!
!! ENGLISH_LETTER_CDF_TEST tests ENGLISH_LETTER_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character c
  character c2
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ENGLISH_LETTER_CDF_TEST'
  write ( *, '(a)' ) '  ENGLISH_LETTER_CDF evaluates the English Letter CDF;'
  write ( *, '(a)' ) '  ENGLISH_LETTER_CDF_INV inverts the English Letter CDF.'
  write ( *, '(a)' ) '  ENGLISH_LETTER_PDF evaluates the English Letter PDF;'

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   C              PDF             CDF    CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call english_letter_sample ( seed, c )

    call english_letter_pdf ( c, pdf )

    call english_letter_cdf ( c, cdf )

    call english_letter_cdf_inv ( cdf, c2 )

    write ( *, '(2x,a,2x,f14.6,2x,f14.6,2x,a)' ) &
      '"' // c // '"', pdf, cdf, '"' // c2 // '"'

  end do

  return
end
subroutine english_sentence_length_cdf_test ( )

!*****************************************************************************80
!
!! ENGLISH_SENTENCE_LENGTH_CDF_TEST tests ENGLISH_SENTENCE_LENGTH_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ENGLISH_SENTENCE_LENGTH_CDF_TEST'
  write ( *, '(a)' ) '  ENGLISH_SENTENCE_LENGTH_CDF evaluates the English Sentence Length CDF;'
  write ( *, '(a)' ) '  ENGLISH_SENTENCE_LENGTH_CDF_INV inverts the English Sentence Length CDF.'
  write ( *, '(a)' ) '  ENGLISH_SENTENCE_LENGTH_PDF evaluates the English Sentence Length PDF;'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call english_sentence_length_sample ( seed, x )

    call english_sentence_length_pdf ( x, pdf )

    call english_sentence_length_cdf ( x, cdf )

    call english_sentence_length_cdf_inv ( cdf, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine english_sentence_length_sample_test ( )

!*****************************************************************************80
!
!! ENGLISH_SENTENCE_LENGTH_SAMPLE_TEST tests ENGLISH_SENTENCE_LENGTH_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ENGLISH_SENTENCE_LENGTH_SAMPLE_TEST'
  write ( *, '(a)' ) '  ENGLISH_SENTENCE_LENGTH_MEAN computes the English Sentence Length mean;'
  write ( *, '(a)' ) '  ENGLISH_SENTENCE_LENGTH_SAMPLE samples the English Sentence Length distribution;'
  write ( *, '(a)' ) '  ENGLISH_SENTENCE_LENGTH_VARIANCE computes the English Sentence Length variance.'

  call english_sentence_length_mean ( mean )
  call english_sentence_length_variance ( variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call english_sentence_length_sample ( seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine english_word_length_cdf_test ( )

!*****************************************************************************80
!
!! ENGLISH_WORD_LENGTH_CDF_TEST tests ENGLISH_WORD_LENGTH_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ENGLISH_WORD_LENGTH_CDF_TEST'
  write ( *, '(a)' ) '  ENGLISH_WORD_LENGTH_CDF evaluates the English Word Length CDF;'
  write ( *, '(a)' ) '  ENGLISH_WORD_LENGTH_CDF_INV inverts the English Word Length CDF.'
  write ( *, '(a)' ) '  ENGLISH_WORD_LENGTH_PDF evaluates the English Word Length PDF;'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call english_word_length_sample ( seed, x )

    call english_word_length_pdf ( x, pdf )

    call english_word_length_cdf ( x, cdf )

    call english_word_length_cdf_inv ( cdf, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine english_word_length_sample_test ( )

!*****************************************************************************80
!
!! ENGLISH_WORD_LENGTH_SAMPLE_TEST tests ENGLISH_WORD_LENGTH_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ENGLISH_WORD_LENGTH_SAMPLE_TEST'
  write ( *, '(a)' ) '  ENGLISH_WORD_LENGTH_MEAN computes the English Word Length mean;'
  write ( *, '(a)' ) '  ENGLISH_WORD_LENGTH_SAMPLE samples the English Word Length distribution;'
  write ( *, '(a)' ) '  ENGLISH_WORD_LENGTH_VARIANCE computes the English Word Length variance.'

  call english_word_length_mean ( mean )
  call english_word_length_variance ( variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call english_word_length_sample ( seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine erlang_cdf_test ( )

!*****************************************************************************80
!
!! ERLANG_CDF_TEST tests ERLANG_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) c
  real ( kind = 8 ) cdf
  logical erlang_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ERLANG_CDF_TEST'
  write ( *, '(a)' ) '  ERLANG_CDF evaluates the Erlang CDF.'
  write ( *, '(a)' ) '  ERLANG_CDF_INV inverts the Erlang CDF.'
  write ( *, '(a)' ) '  ERLANG_PDF evaluates the Erlang PDF.'

  a = 1.0D+00
  b = 2.0D+00
  c = 3

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b
  write ( *, '(a,i8)'    ) '  PDF parameter C = ', c

  if ( .not. erlang_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ERLANG_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call erlang_sample ( a, b, c, seed, x )

    call erlang_pdf ( x, a, b, c, pdf )

    call erlang_cdf ( x, a, b, c, cdf )

    call erlang_cdf_inv ( cdf, a, b, c, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine erlang_sample_test ( )

!*****************************************************************************80
!
!! ERLANG_SAMPLE_TEST tests ERLANG_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) c
  logical erlang_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ERLANG_SAMPLE_TEST'
  write ( *, '(a)' ) '  ERLANG_MEAN computes the Erlang mean;'
  write ( *, '(a)' ) '  ERLANG_SAMPLE samples the Erlang distribution;'
  write ( *, '(a)' ) '  ERLANG_VARIANCE computes the Erlang variance.'

  a = 1.0D+00
  b = 2.0D+00
  c = 3

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =         ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =         ', b
  write ( *, '(a,i8)'    ) '  PDF parameter C =         ', c

  if ( .not. erlang_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ERLANG_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call erlang_mean ( a, b, c, mean )
  call erlang_variance ( a, b, c, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =              ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =          ', variance

  do i = 1, sample_num
    call erlang_sample ( a, b, c, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine exponential_01_cdf_test ( )

!*****************************************************************************80
!
!! EXPONENTIAL_01_CDF_TEST tests EXPONENTIAL_01_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EXPONENTIAL_01_CDF_TEST'
  write ( *, '(a)' ) '  EXPONENTIAL_01_CDF evaluates the Exponential 01 CDF.'
  write ( *, '(a)' ) '  EXPONENTIAL_01_CDF_INV inverts the Exponential 01 CDF.'
  write ( *, '(a)' ) '  EXPONENTIAL_01_PDF evaluates the Exponential 01 PDF.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call exponential_01_sample ( seed, x )

    call exponential_01_pdf ( x, pdf )

    call exponential_01_cdf ( x, cdf )

    call exponential_01_cdf_inv ( cdf, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine exponential_01_sample_test ( )

!*****************************************************************************80
!
!! EXPONENTIAL_01_SAMPLE_TEST tests EXPONENTIAL_01_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) mean
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EXPONENTIAL_01_SAMPLE_TEST'
  write ( *, '(a)' ) '  EXPONENTIAL_01_MEAN computes the Exponential 01 mean;'
  write ( *, '(a)' ) '  EXPONENTIAL_01_SAMPLE samples the Exponential 01 distribution;'
  write ( *, '(a)' ) '  EXPONENTIAL_01_VARIANCE computes the Exponential 01 variance.'

  call exponential_01_mean ( mean )
  call exponential_01_variance ( variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call exponential_01_sample ( seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine exponential_cdf_test ( )

!*****************************************************************************80
!
!! EXPONENTIAL_CDF_TEST tests EXPONENTIAL_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  logical exponential_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EXPONENTIAL_CDF_TEST'
  write ( *, '(a)' ) '  EXPONENTIAL_CDF evaluates the Exponential CDF.'
  write ( *, '(a)' ) '  EXPONENTIAL_CDF_INV inverts the Exponential CDF.'
  write ( *, '(a)' ) '  EXPONENTIAL_PDF evaluates the Exponential PDF.'

  a = 1.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =         ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =         ', b

  if ( .not. exponential_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'EXPONENTIAL_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call exponential_sample ( a, b, seed, x )

    call exponential_pdf ( x, a, b, pdf )

    call exponential_cdf ( x, a, b, cdf )

    call exponential_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine exponential_sample_test ( )

!*****************************************************************************80
!
!! EXPONENTIAL_SAMPLE_TEST tests EXPONENTIAL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical exponential_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EXPONENTIAL_SAMPLE_TEST'
  write ( *, '(a)' ) '  EXPONENTIAL_MEAN computes the Exponential mean;'
  write ( *, '(a)' ) '  EXPONENTIAL_SAMPLE samples the Exponential distribution;'
  write ( *, '(a)' ) '  EXPONENTIAL_VARIANCE computes the Exponential variance.'

  a = 1.0D+00
  b = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =       ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. exponential_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'EXPONENTIAL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call exponential_mean ( a, b, mean )
  call exponential_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call exponential_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine extreme_values_cdf_test ( )

!*****************************************************************************80
!
!! EXTREME_VALUES_CDF_TEST tests EXTREME_VALUES_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  logical extreme_values_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EXTREME_VALUES_CDF_TEST'
  write ( *, '(a)' ) '  EXTREME_VALUES_CDF evaluates the Extreme Values CDF;'
  write ( *, '(a)' ) '  EXTREME_VALUES_CDF_INV inverts the Extreme Values CDF.'
  write ( *, '(a)' ) '  EXTREME_VALUES_PDF evaluates the Extreme Values PDF;'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. extreme_values_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'EXTREME_VALUES_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call extreme_values_sample ( a, b, seed, x )

    call extreme_values_pdf ( x, a, b, pdf )

    call extreme_values_cdf ( x, a, b, cdf )

    call extreme_values_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine extreme_values_sample_test ( )

!*****************************************************************************80
!
!! EXTREME_VALUES_SAMPLE_TEST tests EXTREME_VALUES_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical extreme_values_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EXTREME_VALUES_SAMPLE_TEST'
  write ( *, '(a)' ) '  EXTREME_VALUES_MEAN computes the Extreme Values mean;'
  write ( *, '(a)' ) '  EXTREME_VALUES_SAMPLE samples the Extreme Values distribution;'
  write ( *, '(a)' ) '  EXTREME_VALUES_VARIANCE computes the Extreme Values variance.'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. extreme_values_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'EXTREME_VALUES_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call extreme_values_mean ( a, b, mean )
  call extreme_values_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  do i = 1, sample_num
    call extreme_values_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine f_cdf_test ( )

!*****************************************************************************80
!
!! F_CDF_TEST tests F_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  logical f_check
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'F_CDF_TEST'
  write ( *, '(a)' ) '  F_CDF evaluates the F CDF.'
  write ( *, '(a)' ) '  F_PDF evaluates the F PDF.'
  write ( *, '(a)' ) '  F_SAMPLE samples the F PDF.'

  m = 1
  n = 1

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter M = ', m
  write ( *, '(a,i8)'    ) '  PDF parameter N = ', n

  if ( .not. f_check ( m, n ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'F_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call f_sample ( m, n, seed, x )

    call f_pdf ( x, m, n, pdf )

    call f_cdf ( x, m, n, cdf )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, pdf, cdf

  end do

  return
end
subroutine f_sample_test ( )

!*****************************************************************************80
!
!! F_SAMPLE_TEST tests F_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  logical f_check
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  real ( kind = 8 ) mean
  integer ( kind = 4 ) n
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'F_SAMPLE_TEST'
  write ( *, '(a)' ) '  F_MEAN computes the F mean;'
  write ( *, '(a)' ) '  F_SAMPLE samples the F distribution;'
  write ( *, '(a)' ) '  F_VARIANCE computes the F variance.'

  m = 8
  n = 6

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter M =             ', m
  write ( *, '(a,i8)'    ) '  PDF parameter N =             ', n

  if ( .not. f_check ( m, n ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'F_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call f_mean ( m, n, mean )
  call f_variance ( m, n, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =          ', variance

  do i = 1, sample_num
    call f_sample ( m, n, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine fermi_dirac_sample_test ( )

!*****************************************************************************80
!
!! FERMI_DIRAC_SAMPLE_TEST tests FERMI_DIRAC_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 January 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 10000
  integer ( kind = 4 ), parameter :: test_num = 7

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ) u
  real ( kind = 8 ), dimension ( test_num ) :: u_test = (/ &
   1.0D+00, 2.0D+00, 4.0D+00, 8.0D+00, 16.0D+00, &
  32.0D+00, 1.0D+00  /)
  real ( kind = 8 ) v
  real ( kind = 8 ), dimension ( test_num ) :: v_test = (/ &
   1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, &
   1.0D+00, 0.25D+00  /)
  real ( kind = 8 ) variance
  real ( kind = 8 ) z(sample_num)
  real ( kind = 8 ) z_max
  real ( kind = 8 ) z_min

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FERMI_DIRAC_SAMPLE_TEST'
  write ( *, '(a)' ) '  FERMI_DIRAC_SAMPLE samples the Fermi Dirac distribution.'

  do test = 1, test_num

    u = u_test(test)
    v = v_test(test)
    seed = 123456789

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  U =          ', u
    write ( *, '(a,g14.6)' ) '  V =          ', v

    do i = 1, sample_num
      call fermi_dirac_sample ( u, v, seed, z(i) )
    end do

    z_max = maxval ( z(1:sample_num) )
    z_min = minval ( z(1:sample_num) )

    call r8vec_mean ( sample_num, z, mean )
    call r8vec_variance ( sample_num, z, variance )

    write ( *, '(a)' ) ''
    write ( *, '(a,i8)' ) '  SAMPLE_NUM =      ', sample_num
    write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
    write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
    write ( *, '(a,g14.6)' ) '  Maximum value =   ', z_max
    write ( *, '(a,g14.6)' ) '  Minimum value =   ', z_min

  end do

  return
end
subroutine fisher_pdf_test ( )

!*****************************************************************************80
!
!! FISHER_PDF_TEST tests FISHER_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10
  integer ( kind = 4 ), parameter :: test_num = 3

  integer ( kind = 4 ) j
  real ( kind = 8 ) kappa
  real ( kind = 8 ) mu(3)
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ) x(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FISHER_PDF_TEST'
  write ( *, '(a)' ) '  FISHER_PDF evaluates the Fisher PDF.'

  do test = 1, test_num

    if ( test == 1 ) then
      kappa = 0.0D+00
      mu = (/ 1.0D+00, 0.0D+00, 0.0D+00 /)
    else if ( test == 2 ) then
      kappa = 0.5D+00
      mu = (/ 1.0D+00, 0.0D+00, 0.0D+00 /)
    else if ( test == 3 ) then
      kappa = 10.0D+00
      mu = (/ 1.0D+00, 0.0D+00, 0.0D+00 /)
    end if

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  PDF parameters:'
    write ( *, '(a,g14.6)' ) '    Concentration parameter KAPPA =      ', kappa
    write ( *, '(a,3f8.4)' ) '    Direction MU(1:3) = ', mu(1:3)

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '      X                         PDF'
    write ( *, '(a)' ) ''

    seed = 123456789

    do j = 1, n

      call fisher_sample ( kappa, mu, 1, seed, x )

      call fisher_pdf ( x, kappa, mu, pdf )

      write ( *, '(2x,3f8.4,2x,g14.6)' ) x(1:3), pdf

    end do

  end do

  return
end
subroutine fisk_cdf_test ( )

!*****************************************************************************80
!
!! FISK_CDF_TEST tests FISK_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  logical fisk_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FISK_CDF_TEST'
  write ( *, '(a)' ) '  FISK_CDF evaluates the Fisk CDF;'
  write ( *, '(a)' ) '  FISK_CDF_INV inverts the Fisk CDF.'
  write ( *, '(a)' ) '  FISK_PDF evaluates the Fisk PDF;'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. fisk_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'FISK_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call fisk_sample ( a, b, c, seed, x )

    call fisk_pdf ( x, a, b, c, pdf )

    call fisk_cdf ( x, a, b, c, cdf )

    call fisk_cdf_inv ( cdf, a, b, c, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine fisk_sample_test ( )

!*****************************************************************************80
!
!! FISK_SAMPLE_TEST tests FISK_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  logical fisk_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FISK_SAMPLE_TEST'
  write ( *, '(a)' ) '  FISK_MEAN computes the Fisk mean;'
  write ( *, '(a)' ) '  FISK_SAMPLE samples the Fisk distribution;'
  write ( *, '(a)' ) '  FISK_VARIANCE computes the Fisk variance.'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. fisk_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'FISK_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call fisk_mean ( a, b, c, mean )
  call fisk_variance ( a, b, c, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call fisk_sample ( a, b, c, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine folded_normal_cdf_test ( )

!*****************************************************************************80
!
!! FOLDED_NORMAL_CDF_TEST tests FOLDED_NORMAL_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  logical folded_normal_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FOLDED_NORMAL_CDF_TEST'
  write ( *, '(a)' ) '  FOLDED_NORMAL_CDF evaluates the Folded Normal CDF.'
  write ( *, '(a)' ) '  FOLDED_NORMAL_CDF_INV inverts the Folded Normal CDF.'
  write ( *, '(a)' ) '  FOLDED_NORMAL_PDF evaluates the Folded Normal PDF.'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =         ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =         ', b

  if ( .not. folded_normal_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'FOLDED_NORMAL_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call folded_normal_sample ( a, b, seed, x )

    call folded_normal_pdf ( x, a, b, pdf )

    call folded_normal_cdf ( x, a, b, cdf )

    call folded_normal_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine folded_normal_sample_test ( )

!*****************************************************************************80
!
!! FOLDED_NORMAL_SAMPLE_TEST tests FOLDED_NORMAL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical folded_normal_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FOLDED_NORMAL_SAMPLE_TEST'
  write ( *, '(a)' ) '  FOLDED_NORMAL_MEAN computes the Folded Normal mean;'
  write ( *, '(a)' ) '  FOLDED_NORMAL_SAMPLE samples the Folded Normal distribution;'
  write ( *, '(a)' ) '  FOLDED_NORMAL_VARIANCE computes the Folded Normal variance.'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. folded_normal_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call folded_normal_mean ( a, b, mean )
  call folded_normal_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call folded_normal_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine frechet_cdf_test ( )

!*****************************************************************************80
!
!! FRECHET_CDF_TEST tests FRECHET_CDF.
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
!    02 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FRECHET_CDF_TEST'
  write ( *, '(a)' ) '  FRECHET_CDF evaluates the Frechet CDF;'
  write ( *, '(a)' ) '  FRECHET_CDF_INV inverts the Frechet CDF.'
  write ( *, '(a)' ) '  FRECHET_PDF evaluates the Frechet PDF;'

  alpha = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter ALPHA =         ', alpha

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call frechet_sample ( alpha, seed, x )

    call frechet_pdf ( x, alpha, pdf )

    call frechet_cdf ( x, alpha, cdf )

    call frechet_cdf_inv ( cdf, alpha, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine frechet_sample_test ( )

!*****************************************************************************80
!
!! FRECHET_SAMPLE_TEST tests FRECHET_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FRECHET_SAMPLE_TEST'
  write ( *, '(a)' ) '  FRECHET_MEAN computes the Frechet mean;'
  write ( *, '(a)' ) '  FRECHET_SAMPLE samples the Frechet distribution;'
  write ( *, '(a)' ) '  FRECHET_VARIANCE computes the Frechet variance.'

  alpha = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter ALPHA =         ', alpha

  call frechet_mean ( alpha, mean )
  call frechet_variance ( alpha, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call frechet_sample ( alpha, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine gamma_cdf_test ( )

!*****************************************************************************80
!
!! GAMMA_CDF_TEST tests GAMMA_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  logical gamma_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GAMMA_CDF_TEST'
  write ( *, '(a)' ) '  GAMMA_CDF evaluates the Gamma CDF.'
  write ( *, '(a)' ) '  GAMMA_PDF evaluates the Gamma PDF.'

  a = 1.0D+00
  b = 1.5D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. gamma_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GAMMA_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  X  PDF   CDF'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call gamma_sample ( a, b, c, seed, x )

    call gamma_cdf ( x, a, b, c, cdf )

    call gamma_pdf ( x, a, b, c, pdf )

    write ( *, '(2x,3g14.6)' ) x, pdf, cdf

  end do

  return
end
subroutine gamma_sample_test ( )

!*****************************************************************************80
!
!! GAMMA_SAMPLE_TEST tests GAMMA_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  logical gamma_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GAMMA_SAMPLE_TEST'
  write ( *, '(a)' ) '  GAMMA_MEAN computes the Gamma mean;'
  write ( *, '(a)' ) '  GAMMA_SAMPLE samples the Gamma distribution;'
  write ( *, '(a)' ) '  GAMMA_VARIANCE computes the Gamma variance.'

  a = 1.0D+00
  b = 3.0D+00
  c = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. gamma_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GAMMA_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call gamma_mean ( a, b, c, mean )
  call gamma_variance ( a, b, c, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call gamma_sample ( a, b, c, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine genlogistic_cdf_test ( )

!*****************************************************************************80
!
!! GENLOGISTIC_CDF_TEST tests GENLOGISTIC_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  logical genlogistic_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GENLOGISTIC_CDF_TEST'
  write ( *, '(a)' ) '  GENLOGISTIC_PDF evaluates the Genlogistic PDF.'
  write ( *, '(a)' ) '  GENLOGISTIC_CDF evaluates the Genlogistic CDF;'
  write ( *, '(a)' ) '  GENLOGISTIC_CDF_INV inverts the Genlogistic CDF.'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. genlogistic_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GENLOGISTIC_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call genlogistic_sample ( a, b, c, seed, x )

    call genlogistic_pdf ( x, a, b, c, pdf )

    call genlogistic_cdf ( x, a, b, c, cdf )

    call genlogistic_cdf_inv ( cdf, a, b, c, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine genlogistic_sample_test ( )

!*****************************************************************************80
!
!! GENLOGISTIC_SAMPLE_TEST tests GENLOGISTIC_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  logical genlogistic_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GENLOGISTIC_SAMPLE_TEST'
  write ( *, '(a)' ) '  GENLOGISTIC_MEAN computes the Genlogistic mean;'
  write ( *, '(a)' ) '  GENLOGISTIC_SAMPLE samples the Genlogistic distribution;'
  write ( *, '(a)' ) '  GENLOGISTIC_VARIANCE computes the Genlogistic variance.'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. genlogistic_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GENLOGISTIC_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call genlogistic_mean ( a, b, c, mean )
  call genlogistic_variance ( a, b, c, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call genlogistic_sample ( a, b, c, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine geometric_cdf_test ( )

!*****************************************************************************80
!
!! GEOMETRIC_CDF_TEST tests GEOMETRIC_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  logical geometric_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEOMETRIC_CDF_TEST'
  write ( *, '(a)' ) '  GEOMETRIC_CDF evaluates the Geometric CDF;'
  write ( *, '(a)' ) '  GEOMETRIC_CDF_INV inverts the Geometric CDF.'
  write ( *, '(a)' ) '  GEOMETRIC_PDF evaluates the Geometric PDF;'

  a = 0.25D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =      ', a

  if ( .not. geometric_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GEOMETRIC_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call geometric_sample ( a, seed, x )

    call geometric_pdf ( x, a, pdf )

    call geometric_cdf ( x, a, cdf )

    call geometric_cdf_inv ( cdf, a, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine geometric_sample_test ( )

!*****************************************************************************80
!
!! GEOMETRIC_SAMPLE_TEST tests GEOMETRIC_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  logical geometric_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GEOMETRIC_SAMPLE_TEST'
  write ( *, '(a)' ) '  GEOMETRIC_MEAN computes the Geometric mean;'
  write ( *, '(a)' ) '  GEOMETRIC_SAMPLE samples the Geometric distribution;'
  write ( *, '(a)' ) '  GEOMETRIC_VARIANCE computes the Geometric variance.'

  a = 0.25D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a

  if ( .not. geometric_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GEOMETRIC_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call geometric_mean ( a, mean )
  call geometric_variance ( a, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call geometric_sample ( a, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine gompertz_cdf_test ( )

!*****************************************************************************80
!
!! GOMPERTZ_CDF_TEST tests GOMPERTZ_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  logical gompertz_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GOMPERTZ_CDF_TEST'
  write ( *, '(a)' ) '  GOMPERTZ_CDF evaluates the Gompertz CDF;'
  write ( *, '(a)' ) '  GOMPERTZ_CDF_INV inverts the Gompertz CDF.'
  write ( *, '(a)' ) '  GOMPERTZ_PDF evaluates the Gompertz PDF;'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =       ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =       ', b

  if ( .not. gompertz_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'GOMPERTZ_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call gompertz_sample ( a, b, seed, x )

    call gompertz_pdf ( x, a, b, pdf )

    call gompertz_cdf ( x, a, b, cdf )

    call gompertz_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine gompertz_sample_test ( )

!*****************************************************************************80
!
!! GOMPERTZ_SAMPLE_TEST tests GOMPERTZ_SAMPLE;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical gompertz_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GOMPERTZ_SAMPLE_TEST'
  write ( *, '(a)' ) '  GOMPERTZ_SAMPLE samples the Gompertz distribution;'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =       ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =       ', b

  if ( .not. gompertz_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  do i = 1, sample_num
    call gompertz_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine gumbel_cdf_test ( )

!*****************************************************************************80
!
!! GUMBEL_CDF_TEST tests GUMBEL_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GUMBEL_CDF_TEST'
  write ( *, '(a)' ) '  GUMBEL_CDF evaluates the Gumbel CDF.'
  write ( *, '(a)' ) '  GUMBEL_CDF_INV inverts the Gumbel CDF.'
  write ( *, '(a)' ) '  GUMBEL_PDF evaluates the Gumbel PDF.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call gumbel_sample ( seed, x )

    call gumbel_pdf ( x, pdf )

    call gumbel_cdf ( x, cdf )

    call gumbel_cdf_inv ( cdf, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine gumbel_sample_test ( )

!*****************************************************************************80
!
!! GUMBEL_SAMPLE_TEST tests GUMBEL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GUMBEL_SAMPLE_TEST'
  write ( *, '(a)' ) '  GUMBEL_MEAN computes the Gumbel mean;'
  write ( *, '(a)' ) '  GUMBEL_SAMPLE samples the Gumbel distribution;'
  write ( *, '(a)' ) '  GUMBEL_VARIANCE computes the Gumbel variance.'

  call gumbel_mean ( mean )

  call gumbel_variance ( variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean      =               ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call gumbel_sample ( seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine half_normal_cdf_test ( )

!*****************************************************************************80
!
!! HALF_NORMAL_CDF_TEST tests HALF_NORMAL_CDF.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  logical half_normal_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HALF_NORMAL_CDF_TEST'
  write ( *, '(a)' ) '  HALF_NORMAL_CDF evaluates the Half Normal CDF.'
  write ( *, '(a)' ) '  HALF_NORMAL_CDF_INV inverts the Half Normal CDF.'
  write ( *, '(a)' ) '  HALF_NORMAL_PDF evaluates the Half Normal PDF.'

  a = 0.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =         ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =         ', b

  if ( .not. half_normal_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'HALF_NORMAL_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call half_normal_sample ( a, b, seed, x )

    call half_normal_pdf ( x, a, b, pdf )

    call half_normal_cdf ( x, a, b, cdf )

    call half_normal_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine half_normal_sample_test ( )

!*****************************************************************************80
!
!! HALF_NORMAL_SAMPLE_TEST tests HALF_NORMAL_SAMPLE.
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

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical half_normal_check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HALF_NORMAL_SAMPLE_TEST'
  write ( *, '(a)' ) '  HALF_NORMAL_MEAN computes the Half Normal mean;'
  write ( *, '(a)' ) '  HALF_NORMAL_SAMPLE samples the Half Normal distribution;'
  write ( *, '(a)' ) '  HALF_NORMAL_VARIANCE computes the Half Normal variance.'

  a = 0.0D+00
  b = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. half_normal_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'HALF_NORMAL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call half_normal_mean ( a, b, mean )
  call half_normal_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call half_normal_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine hypergeometric_cdf_test ( )

!*****************************************************************************80
!
!! HYPERGEOMETRIC_CDF_TEST tests HYPERGEOMETRIC_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  logical hypergeometric_check
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HYPERGEOMETRIC_CDF_TEST'
  write ( *, '(a)' ) '  HYPERGEOMETRIC_CDF evaluates the Hypergeometric CDF.'
  write ( *, '(a)' ) '  HYPERGEOMETRIC_PDF evaluates the Hypergeometric PDF.'

  x = 7

  n = 10
  m = 7
  l = 100

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Total number of balls =         ', l
  write ( *, '(a,i8)'    ) '  Number of white balls =         ', m
  write ( *, '(a,i8)'    ) '  Number of balls taken =         ', n

  if ( .not. hypergeometric_check ( n, m, l ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'HYPERGEOMETRIC_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call hypergeometric_pdf ( x, n, m, l, pdf )

  call hypergeometric_cdf ( x, n, m, l, cdf )

  write ( *, '(a,i8)'    ) '  PDF argument X =                ', x
  write ( *, '(a,g14.6)' ) '  PDF value =                   = ', pdf
  write ( *, '(a,g14.6)' ) '  CDF value =                   = ', cdf

  return
end
subroutine hypergeometric_sample_test ( )

!*****************************************************************************80
!
!! HYPERGEOMETRIC_SAMPLE_TEST tests HYPERGEOMETRIC_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  logical hypergeometric_check
  integer ( kind = 4 ) i
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  real ( kind = 8 ) mean
  integer ( kind = 4 ) n
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HYPERGEOMETRIC_SAMPLE_TEST'
  write ( *, '(a)' ) '  HYPERGEOMETRIC_MEAN computes the Hypergeometric mean;'
  write ( *, '(a)' ) '  HYPERGEOMETRIC_SAMPLE samples the Hypergeometric distribution;'
  write ( *, '(a)' ) '  HYPERGEOMETRIC_VARIANCE computes the Hypergeometric variance.'

  n = 10
  m = 7
  l = 100

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter N =             ', n
  write ( *, '(a,i8)'    ) '  PDF parameter M =             ', m
  write ( *, '(a,i8)'    ) '  PDF parameter L =             ', l

  if ( .not. hypergeometric_check ( n, m, l ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'HYPERGEOMETRIC_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call hypergeometric_mean ( n, m, l, mean )
  call hypergeometric_variance ( n, m, l, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call hypergeometric_sample ( n, m, l, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)' ) '  Sample minimum =  ', xmin

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
subroutine inverse_gaussian_cdf_test ( )

!*****************************************************************************80
!
!! INVERSE_GAUSSIAN_CDF_TEST tests INVERSE_GAUSSIAN_CDF.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  logical inverse_gaussian_check
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INVERSE_GAUSSIAN_CDF_TEST'
  write ( *, '(a)' ) '  INVERSE_GAUSSIAN_CDF evaluates the Inverse Gaussian CDF.'
  write ( *, '(a)' ) '  INVERSE_GAUSSIAN_PDF evaluates the Inverse Gaussian PDF.'

  a = 5.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. inverse_gaussian_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'INVERSE_GAUSSIAN_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call inverse_gaussian_sample ( a, b, seed, x )

    call inverse_gaussian_pdf ( x, a, b, pdf )

    call inverse_gaussian_cdf ( x, a, b, cdf )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf

  end do

  return
end
subroutine inverse_gaussian_sample_test ( )

!*****************************************************************************80
!
!! INVERSE_GAUSSIAN_SAMPLE_TEST tests INVERSE_GAUSSIAN_SAMPLE.
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

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  logical inverse_gaussian_check
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'INVERSE_GAUSSIAN_SAMPLE_TEST'
  write ( *, '(a)' ) '  INVERSE_GAUSSIAN_MEAN computes the Inverse Gaussian mean;'
  write ( *, '(a)' ) '  INVERSE_GAUSSIAN_SAMPLE samples the Inverse Gaussian distribution;'
  write ( *, '(a)' ) '  INVERSE_GAUSSIAN_VARIANCE computes the Inverse Gaussian variance.'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. inverse_gaussian_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'INVERSE_GAUSSIAN_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call inverse_gaussian_mean ( a, b, mean )
  call inverse_gaussian_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call inverse_gaussian_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine laplace_cdf_test ( )

!*****************************************************************************80
!
!! LAPLACE_CDF_TEST tests LAPLACE_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  logical laplace_check
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LAPLACE_CDF_TEST'
  write ( *, '(a)' ) '  LAPLACE_CDF evaluates the Laplace CDF;'
  write ( *, '(a)' ) '  LAPLACE_CDF_INV inverts the Laplace CDF.'
  write ( *, '(a)' ) '  LAPLACE_PDF evaluates the Laplace PDF;'

  a = 1.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. laplace_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAPLACE_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call laplace_sample ( a, b, seed, x )

    call laplace_pdf ( x, a, b, pdf )

    call laplace_cdf ( x, a, b, cdf )

    call laplace_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine laplace_sample_test ( )

!*****************************************************************************80
!
!! LAPLACE_SAMPLE_TEST tests LAPLACE_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  logical laplace_check
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LAPLACE_SAMPLE_TEST'
  write ( *, '(a)' ) '  LAPLACE_MEAN computes the Laplace mean;'
  write ( *, '(a)' ) '  LAPLACE_SAMPLE samples the Laplace distribution;'
  write ( *, '(a)' ) '  LAPLACE_VARIANCE computes the Laplace variance.'

  a = 1.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. laplace_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LAPLACE_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call laplace_mean ( a, b, mean )
  call laplace_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call laplace_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine levy_cdf_test ( )

!*****************************************************************************80
!
!! LEVY_CDF_TEST tests LEVY_CDF.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEVY_CDF_TEST'
  write ( *, '(a)' ) '  LEVY_CDF evaluates the Levy CDF;'
  write ( *, '(a)' ) '  LEVY_CDF_INV inverts the Levy CDF.'
  write ( *, '(a)' ) '  LEVY_PDF evaluates the Levy PDF;'

  a = 1.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    X              PDF           CDF          X2'
  write ( *, '(a)' ) ''

  do test = 1, test_num

    call levy_sample ( a, b, seed, x )

    call levy_pdf ( x, a, b, pdf )

    call levy_cdf ( x, a, b, cdf )

    call levy_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,g12.6,2x,g12.6,2x,g12.6,2x,g12.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine logistic_cdf_test ( )

!*****************************************************************************80
!
!! LOGISTIC_CDF_TEST tests LOGISTIC_CDF.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  logical logistic_check
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOGISTIC_CDF_TEST'
  write ( *, '(a)' ) '  LOGISTIC_CDF evaluates the Logistic CDF;'
  write ( *, '(a)' ) '  LOGISTIC_CDF_INV inverts the Logistic CDF.'
  write ( *, '(a)' ) '  LOGISTIC_PDF evaluates the Logistic PDF;'

  a = 1.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. logistic_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LOGISTIC_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call logistic_sample ( a, b, seed, x )

    call logistic_pdf ( x, a, b, pdf )

    call logistic_cdf ( x, a, b, cdf )

    call logistic_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine logistic_sample_test ( )

!*****************************************************************************80
!
!! LOGISTIC_SAMPLE_TEST tests LOGISTIC_SAMPLE.
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

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  logical logistic_check
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOGISTIC_SAMPLE_TEST'
  write ( *, '(a)' ) '  LOGISTIC_MEAN computes the Logistic mean;'
  write ( *, '(a)' ) '  LOGISTIC_SAMPLE samples the Logistic distribution;'
  write ( *, '(a)' ) '  LOGISTIC_VARIANCE computes the Logistic variance.'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. logistic_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LOGISTIC_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call logistic_mean ( a, b, mean )
  call logistic_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call logistic_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine log_normal_cdf_test ( )

!*****************************************************************************80
!
!! LOG_NORMAL_CDF_TEST tests LOG_NORMAL_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  logical log_normal_check
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOG_NORMAL_CDF_TEST'
  write ( *, '(a)' ) '  LOG_NORMAL_CDF evaluates the Log Normal CDF;'
  write ( *, '(a)' ) '  LOG_NORMAL_CDF_INV inverts the Log Normal CDF.'
  write ( *, '(a)' ) '  LOG_NORMAL_PDF evaluates the Log Normal PDF;'

  a = 10.0D+00
  b = 2.25D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. log_normal_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LOG_NORMAL_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call log_normal_sample ( a, b, seed, x )

    call log_normal_pdf ( x, a, b, pdf )

    call log_normal_cdf ( x, a, b, cdf )

    call log_normal_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine log_normal_sample_test ( )

!*****************************************************************************80
!
!! LOG_NORMAL_SAMPLE_TEST tests LOG_NORMAL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  logical log_normal_check
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOG_NORMAL_SAMPLE_TEST'
  write ( *, '(a)' ) '  LOG_NORMAL_MEAN computes the Log Normal mean;'
  write ( *, '(a)' ) '  LOG_NORMAL_SAMPLE samples the Log Normal distribution;'
  write ( *, '(a)' ) '  LOG_NORMAL_VARIANCE computes the Log Normal variance.'

  a = 1.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. log_normal_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LOG_NORMAL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call log_normal_mean ( a, b, mean )
  call log_normal_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call log_normal_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine log_series_cdf_test ( )

!*****************************************************************************80
!
!! LOG_SERIES_CDF_TEST tests LOG_SERIES_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  logical log_series_check
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOG_SERIES_CDF_TEST'
  write ( *, '(a)' ) '  LOG_SERIES_CDF evaluates the Log Series CDF;'
  write ( *, '(a)' ) '  LOG_SERIES_CDF_INV inverts the Log Series CDF.'
  write ( *, '(a)' ) '  LOG_SERIES_PDF evaluates the Log Series PDF;'

  a = 0.25D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =  ', a

  if ( .not. log_series_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LOG_SERIES_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call log_series_sample ( a, seed, x )

    call log_series_pdf ( x, a, pdf )

    call log_series_cdf ( x, a, cdf )

    call log_series_cdf_inv ( cdf, a, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine log_series_sample_test ( )

!*****************************************************************************80
!
!! LOG_SERIES_SAMPLE_TEST tests LOG_SERIES_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  logical log_series_check
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOG_SERIES_SAMPLE_TEST'
  write ( *, '(a)' ) '  LOG_SERIES_MEAN computes the Log Series mean;'
  write ( *, '(a)' ) '  LOG_SERIES_VARIANCE computes the Log Series variance;'
  write ( *, '(a)' ) '  LOG_SERIES_SAMPLE samples the Log Series distribution.'

  a = 0.25D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a

  if ( .not. log_series_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LOG_SERIES_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call log_series_mean ( a, mean )
  call log_series_variance ( a, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call log_series_sample ( a, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine log_uniform_cdf_test ( )

!*****************************************************************************80
!
!! LOG_UNIFORM_CDF_TEST tests LOG_UNIFORM_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  logical log_uniform_check
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOG_UNIFORM_CDF_TEST'
  write ( *, '(a)' ) '  LOG_UNIFORM_CDF evaluates the Log Uniform CDF;'
  write ( *, '(a)' ) '  LOG_UNIFORM_CDF_INV inverts the Log Uniform CDF.'
  write ( *, '(a)' ) '  LOG_UNIFORM_PDF evaluates the Log Uniform PDF;'

  a = 2.0D+00
  b = 20.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. log_uniform_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LOG_UNIFORM_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call log_uniform_sample ( a, b, seed, x )

    call log_uniform_pdf ( x, a, b, pdf )

    call log_uniform_cdf ( x, a, b, cdf )

    call log_uniform_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine log_uniform_sample_test ( )

!*****************************************************************************80
!
!! LOG_UNIFORM_SAMPLE_TEST tests LOG_UNIFORM_SAMPLE;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  logical log_uniform_check
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOG_UNIFORM_SAMPLE_TEST'
  write ( *, '(a)' ) '  LOG_UNIFORM_MEAN computes the Log Uniform mean;'
  write ( *, '(a)' ) '  LOG_UNIFORM_SAMPLE samples the Log Uniform distribution;'

  a = 2.0D+00
  b = 20.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. log_uniform_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LOG_UNIFORM_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call log_uniform_mean ( a, b, mean )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean

  do i = 1, sample_num
    call log_uniform_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine lorentz_cdf_test ( )

!*****************************************************************************80
!
!! LORENTZ_CDF_TEST tests LORENTZ_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LORENTZ_CDF_TEST'
  write ( *, '(a)' ) '  LORENTZ_CDF evaluates the Lorentz CDF;'
  write ( *, '(a)' ) '  LORENTZ_CDF_INV inverts the Lorentz CDF.'
  write ( *, '(a)' ) '  LORENTZ_PDF evaluates the Lorentz PDF;'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call lorentz_sample ( seed, x )

    call lorentz_pdf ( x, pdf )

    call lorentz_cdf ( x, cdf )

    call lorentz_cdf_inv ( cdf, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine lorentz_sample_test ( )

!*****************************************************************************80
!
!! LORENTZ_SAMPLE_TEST tests LORENTZ_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LORENTZ_SAMPLE_TEST'
  write ( *, '(a)' ) '  LORENTZ_MEAN computes the Lorentz mean;'
  write ( *, '(a)' ) '  LORENTZ_VARIANCE computes the Lorentz variance;'
  write ( *, '(a)' ) '  LORENTZ_SAMPLE samples the Lorentz distribution.'

  call lorentz_mean ( mean )
  call lorentz_variance ( variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call lorentz_sample ( seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine maxwell_cdf_test ( )

!*****************************************************************************80
!
!! MAXWELL_CDF_TEST tests MAXWELL_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  logical maxwell_check
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MAXWELL_CDF_TEST'
  write ( *, '(a)' ) '  MAXWELL_CDF evaluates the Maxwell CDF.'
  write ( *, '(a)' ) '  MAXWELL_CDF_INV inverts the Maxwell CDF.'
  write ( *, '(a)' ) '  MAXWELL_PDF evaluates the Maxwell PDF.'

  a = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =         ', a

  if ( .not. maxwell_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MAXWELL_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call maxwell_sample ( a, seed, x )

    call maxwell_pdf ( x, a, pdf )

    call maxwell_cdf ( x, a, cdf )

    call maxwell_cdf_inv ( cdf, a, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine maxwell_sample_test ( )

!*****************************************************************************80
!
!! MAXWELL_SAMPLE_TEST tests MAXWELL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  logical maxwell_check
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MAXWELL_SAMPLE_TEST'
  write ( *, '(a)' ) '  MAXWELL_MEAN computes the Maxwell mean;'
  write ( *, '(a)' ) '  MAXWELL_VARIANCE computes the Maxwell variance;'
  write ( *, '(a)' ) '  MAXWELL_SAMPLE samples the Maxwell distribution.'

  a = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a

  if ( .not. maxwell_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MAXWELL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call maxwell_mean ( a, mean )
  call maxwell_variance ( a, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', variance

  do i = 1, sample_num
    call maxwell_sample ( a, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine multinomial_coef_test ( )

!*****************************************************************************80
!
!! MULTINOMIAL_TEST tests MULTINOMIAL_COEF1, MULTINOMIAL_COEF2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2016
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
  integer ( kind = 4 ) ncomb2
  integer ( kind = 4 ) nfactor

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MULTINOMIAL_TEST'
  write ( *, '(a)' ) '  MULTINOMIAL_COEF1 computes multinomial'
  write ( *, '(a)' ) '  coefficients using the Gamma function;'
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

    call multinomial_coef1 ( nfactor, factor, ncomb1 )

    call multinomial_coef2 ( nfactor, factor, ncomb2 )

    write ( *, '(i4,i4,3x,i5,i5)' ) factor(1), factor(2), ncomb1, ncomb2

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

      call multinomial_coef2 ( nfactor, factor, ncomb2 )

      write ( *, '(i4,i4,i4,3x,i5,i5)' ) factor(1), factor(2), factor(3), &
        ncomb1, ncomb2

    end do

  end do

  return
end
subroutine multinomial_sample_test ( )

!*****************************************************************************80
!
!! MULTINOMIAL_SAMPLE_TEST tests MULTINOMIAL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: b = 3
  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) a
  real ( kind = 8 ) c(b)
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean(b)
  logical multinomial_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance(b)
  integer ( kind = 4 ) x(b,sample_num)
  integer ( kind = 4 ) xmax(b)
  integer ( kind = 4 ) xmin(b)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MULTINOMIAL_SAMPLE_TEST'
  write ( *, '(a)' ) '  MULTINOMIAL_MEAN computes the Multinomial mean;'
  write ( *, '(a)' ) '  MULTINOMIAL_SAMPLE samples the Multinomial distribution;'
  write ( *, '(a)' ) '  MULTINOMIAL_VARIANCE computes the Multinomial variance;'

  a = 5

  c(1:3) = (/ 0.125D+00, 0.500D+00, 0.375D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  PDF parameter A =             ', a
  write ( *, '(a,i8)' ) '  PDF parameter B =             ', b
  call r8vec_print ( b, c, '  PDF parameter C = ' )

  if ( .not. multinomial_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MULTINOMIAL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call multinomial_mean ( a, b, c, mean )
  call multinomial_variance ( a, b, c, variance )

  call r8vec_print ( b, mean, '  PDF means: ' )
  call r8vec_print ( b, variance, '  PDF variances:' )

  do i = 1, sample_num
    call multinomial_sample ( a, b, c, seed, x(1,i) )
  end do

  call i4row_max ( b, sample_num, x, xmax )
  call i4row_min ( b, sample_num, x, xmin )
  call i4row_mean ( b, sample_num, x, mean )
  call i4row_variance ( b, sample_num, x, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Sample size = ', sample_num
  write ( *, '(a)' ) '  Component Mean, Variance, Min, Max:'
  do i = 1, b
    write ( *, '(2x,i8,2g14.6,2i8)' ) i, mean(i), variance(i), xmin(i), xmax(i)
  end do

  return
end
subroutine multinomial_pdf_test ( )

!*****************************************************************************80
!
!! MULTINOMIAL_PDF_TEST tests MULTINOMIAL_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: b = 3

  integer ( kind = 4 ) a
  real ( kind = 8 ) c(b)
  integer ( kind = 4 ) i
  logical multinomial_check
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x(b)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MULTINOMIAL_PDF_TEST'
  write ( *, '(a)' ) '  MULTINOMIAL_PDF evaluates the Multinomial PDF.'
  a = 5

  c(1:3) = (/ 0.10D+00, 0.50D+00, 0.40D+00 /)

  write ( *, '(a,i8)' ) '  PDF parameter A = ', a
  write ( *, '(a,i8)' ) '  PDF parameter B = ', b
  call r8vec_print ( b, c, '  PDF parameter C:' )

  if ( .not. multinomial_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'MULTINOMIAL_PDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  x(1:3) = (/ 0, 2, 3 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  PDF argument X:'
  write ( *, '(a)' ) ''
  do i = 1, b
    write ( *, '(2x,i8)' ) x(i)
  end do

  call multinomial_pdf ( x, a, b, c, pdf )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF value =     ', pdf

  return
end
subroutine multinoulli_pdf_test ( )

!*****************************************************************************80
!
!! MULTINOULLLI_PDF_TEST tests MULTINOULLI_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) pdf
  integer ( kind = 4 ) seed
  real ( kind = 8 ) theta(n)
  real ( kind = 8 ) theta_sum
  integer ( kind = 4 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MULTINOULLI_PDF_TEST'
  write ( *, '(a)' ) '  MULTINOULLI_PDF evaluates the Multinoulli PDF.'

  seed = 123456789;
  call r8vec_uniform_01 ( n, seed, theta )
  theta_sum = sum ( theta(1:n) )
  theta(1:n) = theta(1:n) / theta_sum;

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   X     pdf(X)'
  write ( *, '(a)' ) ''
  do x = 0, n + 1
    call multinoulli_pdf ( x, n, theta, pdf )
    write ( *, '(2x,i2,2x,g14.6)' ) x, pdf
  end do

  return
end
subroutine nakagami_cdf_test ( )

!*****************************************************************************80
!
!! NAKAGAMI_CDF_TEST tests NAKAGAMI_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  logical nakagami_check
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NAKAGAMI_CDF_TEST'
  write ( *, '(a)' ) '  NAKAGAMI_CDF evaluates the Nakagami CDF;'
  write ( *, '(a)' ) '  NAKAGAMI_PDF evaluates the Nakagami PDF;'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. nakagami_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'NAKAGAMI_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    x = a + b + sqrt ( real ( i, kind = 8 ) / c / 10.0D+00 )

    call nakagami_pdf ( x, a, b, c, pdf )

    call nakagami_cdf ( x, a, b, c, cdf )

    call nakagami_cdf_inv ( cdf, a, b, c, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine nakagami_sample_test ( )

!*****************************************************************************80
!
!! NAKAGAMI_SAMPLE_TEST tests NAKAGAMI_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) mean
  logical nakagami_check
  real ( kind = 8 ) variance

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NAKAGAMI_SAMPLE_TEST'
  write ( *, '(a)' ) '  NAKAGAMI_MEAN computes the Nakagami mean;'
  write ( *, '(a)' ) '  NAKAGAMI_VARIANCE computes the Nakagami variance.'

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. nakagami_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'NAKAGAMI_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call nakagami_mean ( a, b, c, mean )
  call nakagami_variance ( a, b, c, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  return
end
subroutine negative_binomial_cdf_test ( )

!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_CDF_TEST tests NEGATIVE_BINOMIAL_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  logical negative_binomial_check
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NEGATIVE_BINOMIAL_CDF_TEST'
  write ( *, '(a)' ) '  NEGATIVE_BINOMIAL_CDF evaluates the Negative Binomial CDF.'
  write ( *, '(a)' ) '  NEGATIVE_BINOMIAL_CDF_INV inverts the Negative Binomial CDF.'
  write ( *, '(a)' ) '  NEGATIVE_BINOMIAL_PDF evaluates the Negative Binomial PDF.'

  a = 2
  b = 0.25D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. negative_binomial_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'NEGATIVE_BINOMIAL_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call negative_binomial_sample ( a, b, seed, x )

    call negative_binomial_pdf ( x, a, b, pdf )

    call negative_binomial_cdf ( x, a, b, cdf )

    call negative_binomial_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine negative_binomial_sample_test ( )

!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_SAMPLE_TEST tests NEGATIVE_BINOMIAL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  logical negative_binomial_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NEGATIVE_BINOMIAL_SAMPLE_TEST'
  write ( *, '(a)' ) '  NEGATIVE_BINOMIAL_MEAN computes the Negative Binomial mean;'
  write ( *, '(a)' ) '  NEGATIVE_BINOMIAL_SAMPLE samples the Negative Binomial distribution;'
  write ( *, '(a)' ) '  NEGATIVE_BINOMIAL_VARIANCE computes the Negative Binomial variance.'

  a = 2
  b = 0.75D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. negative_binomial_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'NEGATIVE_BINOMIAL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call negative_binomial_mean ( a, b, mean )
  call negative_binomial_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call negative_binomial_sample ( a, b, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine normal_01_cdf_test ( )

!*****************************************************************************80
!
!! NORMAL_01_CDF_TEST tests NORMAL_01_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMAL_01_CDF_TEST'
  write ( *, '(a)' ) '  NORMAL_01_CDF evaluates the Normal 01 CDF;'
  write ( *, '(a)' ) '  NORMAL_01_CDF_INV inverts the Normal 01 CDF.'
  write ( *, '(a)' ) '  NORMAL_01_PDF evaluates the Normal 01 PDF;'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call normal_01_sample ( seed, x )

    call normal_01_pdf ( x, pdf )

    call normal_01_cdf ( x, cdf )

    call normal_01_cdf_inv ( cdf, x2 )

    write ( *, '(2x,g24.16,2x,g14.6,2x,g14.6,2x,g24.16)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine normal_01_samples_test ( )

!*****************************************************************************80
!
!! NORMAL_01_SAMPLES_TEST tests NORMAL_01_SAMPLES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMAL_01_SAMPLES_TEST'
  write ( *, '(a)' ) '  NORMAL_01_MEAN computes the Normal 01 mean;'
  write ( *, '(a)' ) '  NORMAL_01_SAMPLES samples the Normal 01 PDF;'
  write ( *, '(a)' ) '  NORMAL_01_VARIANCE returns the Normal 01 variance.'

  call normal_01_mean ( mean )
  call normal_01_variance ( variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =     ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance = ', variance

  call normal_01_samples ( sample_num, seed, x )

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine normal_cdf_test ( )

!*****************************************************************************80
!
!! NORMAL_CDF_TEST tests NORMAL_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  logical normal_check
  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMAL_CDF_TEST'
  write ( *, '(a)' ) '  NORMAL_CDF evaluates the Normal CDF;'
  write ( *, '(a)' ) '  NORMAL_CDF_INV inverts the Normal CDF.'
  write ( *, '(a)' ) '  NORMAL_PDF evaluates the Normal PDF;'

  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  if ( .not. normal_check ( mu, sigma ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'NORMAL_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call normal_sample ( mu, sigma, seed, x )

    call normal_pdf ( x, mu, sigma, pdf )

    call normal_cdf ( x, mu, sigma, cdf )

    call normal_cdf_inv ( cdf, mu, sigma, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine normal_samples_test ( )

!*****************************************************************************80
!
!! NORMAL_SAMPLES_TEST tests NORMAL_SAMPLES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  logical normal_check
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) sigma
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMAL_SAMPLES_TEST'
  write ( *, '(a)' ) '  NORMAL_MEAN computes the Normal mean;'
  write ( *, '(a)' ) '  NORMAL_SAMPLES samples the Normal distribution;'
  write ( *, '(a)' ) '  NORMAL_VARIANCE returns the Normal variance.'

  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  if ( .not. normal_check ( mu, sigma ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'NORMAL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call normal_mean ( mu, sigma, mean )
  call normal_variance ( mu, sigma, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =            ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =        ', variance

  call normal_samples ( sample_num, mu, sigma, seed, x )

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine normal_truncated_ab_cdf_test ( )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_AB_CDF_TEST tests NORMAL_TRUNCATED_AB_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  real ( kind = 8 ) s
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  a = 50.0D+00
  b = 150.0D+00
  mu = 100.0D+00
  s = 25.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMAL_TRUNCATED_AB_CDF_TEST'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_AB_CDF evaluates the Normal Truncated AB CDF.'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_AB_CDF_INV inverts the Normal Truncated AB CDF.'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_AB_PDF evaluates the Normal Truncated AB PDF.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The "parent" normal distribution has'
  write ( *, '(a,g14.6)' ) '    mean =               ', mu
  write ( *, '(a,g14.6)' ) '    standard deviation = ', s
  write ( *, '(a)' ) '  The parent distribution is truncated to'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  the interval [', a, ',', b, ']'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call normal_truncated_ab_sample ( mu, s, a, b, seed, x )

    call normal_truncated_ab_pdf ( x, mu, s, a, b, pdf )

    call normal_truncated_ab_cdf ( x, mu, s, a, b, cdf )

    call normal_truncated_ab_cdf_inv ( cdf, mu, s, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine normal_truncated_ab_sample_test ( )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_AB_SAMPLE_TEST tests NORMAL_TRUNCATED_AB_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  real ( kind = 8 ) s
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  a = 50.0D+00
  b = 150.0D+00
  mu = 100.0D+00
  s = 25.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMAL_TRUNCATED_AB_SAMPLE_TEST'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_AB_MEAN computes the Normal Truncated AB mean;'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_AB_SAMPLE samples the Normal Truncated AB distribution;'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_AB_VARIANCE computes the Normal Truncated AB variance.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The "parent" normal distribution has'
  write ( *, '(a,g14.6)' ) '    mean =               ', mu
  write ( *, '(a,g14.6)' ) '    standard deviation = ', s
  write ( *, '(a)' ) '  The parent distribution is truncated to'
  write ( *, '(a,g14.6,a,g14.6,a)' ) '  the interval [', a, ',', b, ']'

  call normal_truncated_ab_mean ( mu, s, a, b, mean )

  call normal_truncated_ab_variance ( mu, s, a, b, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean      =               ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call normal_truncated_ab_sample ( mu, s, a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine normal_truncated_a_cdf_test ( )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_CDF_TEST tests NORMAL_TRUNCATED_A_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  real ( kind = 8 ) s
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  a = 50.0D+00
  mu = 100.0D+00
  s = 25.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMAL_TRUNCATED_A_CDF_TEST'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_A_CDF evaluates the Normal Truncated A CDF.'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_A_CDF_INV inverts the Normal Truncated A CDF.'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_A_PDF evaluates the Normal Truncated A PDF.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The "parent" normal distribution has'
  write ( *, '(a,g14.6)' ) '    mean =               ', mu
  write ( *, '(a,g14.6)' ) '    standard deviation = ', s
  write ( *, '(a)' ) '  The parent distribution is truncated to'
  write ( *, '(a,g14.6,a)' ) '  the interval [', a, ',+oo]'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call normal_truncated_a_sample ( mu, s, a, seed, x )

    call normal_truncated_a_pdf ( x, mu, s, a, pdf )

    call normal_truncated_a_cdf ( x, mu, s, a, cdf )

    call normal_truncated_a_cdf_inv ( cdf, mu, s, a, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine normal_truncated_a_sample_test ( )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_SAMPLE_TEST tests NORMAL_TRUNCATED_A_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  real ( kind = 8 ) s
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  a = 50.0D+00
  mu = 100.0D+00
  s = 25.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMAL_TRUNCATED_A_SAMPLE_TEST'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_A_MEAN computes the Normal Truncated A mean;'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_A_SAMPLE samples the Normal Truncated A distribution;'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_A_VARIANCE computes the Normal Truncated A variance.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The "parent" normal distribution has'
  write ( *, '(a,g14.6)' ) '    mean =               ', mu
  write ( *, '(a,g14.6)' ) '    standard deviation = ', s
  write ( *, '(a)' ) '  The parent distribution is truncated to'
  write ( *, '(a,g14.6,a)' ) '  the interval [', a, ',+oo]'

  call normal_truncated_a_mean ( mu, s, a, mean )

  call normal_truncated_a_variance ( mu, s, a, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean      =               ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call normal_truncated_a_sample ( mu, s, a, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine normal_truncated_b_cdf_test ( )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_CDF_TEST tests NORMAL_TRUNCATED_B_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  real ( kind = 8 ) s
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  b = 150.0D+00
  mu = 100.0D+00
  s = 25.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMAL_TRUNCATED_B_CDF_TEST'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_B_CDF evaluates the Normal Truncated B CDF.'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_B_CDF_INV inverts the Normal Truncated B CDF.'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_B_PDF evaluates the Normal Truncated B PDF.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The "parent" normal distribution has'
  write ( *, '(a,g14.6)' ) '    mean =               ', mu
  write ( *, '(a,g14.6)' ) '    standard deviation = ', s
  write ( *, '(a)' ) '  The parent distribution is truncated to'
  write ( *, '(a,g14.6,a)' ) '  the interval [-oo,', b, ']'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call normal_truncated_b_sample ( mu, s, b, seed, x )

    call normal_truncated_b_pdf ( x, mu, s, b, pdf )

    call normal_truncated_b_cdf ( x, mu, s, b, cdf )

    call normal_truncated_b_cdf_inv ( cdf, mu, s, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine normal_truncated_b_sample_test ( )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_SAMPLE_TEST tests NORMAL_TRUNCATED_B_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  real ( kind = 8 ) s
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  b = 150.0D+00
  mu = 100.0D+00
  s = 25.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMAL_TRUNCATED_B_SAMPLE_TEST'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_B_MEAN computes the Normal Truncated B mean;'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_B_SAMPLE samples the Normal Truncated B distribution;'
  write ( *, '(a)' ) '  NORMAL_TRUNCATED_B_VARIANCE computes the Normal Truncated B variance.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The "parent" normal distribution has'
  write ( *, '(a,g14.6)' ) '    mean =               ', mu
  write ( *, '(a,g14.6)' ) '    standard deviation = ', s
  write ( *, '(a)' ) '  The parent distribution is truncated to'
  write ( *, '(a,g14.6,a)' ) '  the interval [-oo,', b, ']'

  call normal_truncated_b_mean ( mu, s, b, mean )

  call normal_truncated_b_variance ( mu, s, b, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean      =               ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call normal_truncated_b_sample ( mu, s, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine pareto_cdf_test ( )

!*****************************************************************************80
!
!! PARETO_CDF_TEST tests PARETO_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  logical pareto_check
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PARETO_CDF_TEST'
  write ( *, '(a)' ) '  PARETO_CDF evaluates the Pareto CDF;'
  write ( *, '(a)' ) '  PARETO_CDF_INV inverts the Pareto CDF.'
  write ( *, '(a)' ) '  PARETO_PDF evaluates the Pareto PDF;'

  a = 0.5D+00
  b = 5.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. pareto_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'PARETO_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call pareto_sample ( a, b, seed, x )

    call pareto_pdf ( x, a, b, pdf )

    call pareto_cdf ( x, a, b, cdf )

    call pareto_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine pareto_sample_test

!*****************************************************************************80
!
!! PARETO_SAMPLE_TEST tests PARETO_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  logical pareto_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PARETO_SAMPLE_TEST'
  write ( *, '(a)' ) '  PARETO_MEAN computes the Pareto mean;'
  write ( *, '(a)' ) '  PARETO_SAMPLE samples the Pareto distribution;'
  write ( *, '(a)' ) '  PARETO_VARIANCE computes the Pareto variance.'

  a = 0.5D+00
  b = 5.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. pareto_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'PARETO_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call pareto_mean ( a, b, mean )
  call pareto_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call pareto_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine pearson_05_pdf_test ( )

!*****************************************************************************80
!
!! PEARSON_05_PDF_TEST tests PEARSON_05_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) pdf
  logical pearson_05_check
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PEARSON_05_PDF_TEST'
  write ( *, '(a)' ) '  PEARSON_05_PDF evaluates the Pearson 05 PDF.'

  x = 5.0D+00

  a = 1.0D+00
  b = 2.0D+00
  c = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C = ', c

  if ( .not. pearson_05_check ( a, b, c ) ) then

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'PEARSON_05_PDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call pearson_05_pdf ( x, a, b, c, pdf )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF argument X =  ', x
  write ( *, '(a,g14.6)' ) '  PDF value =       ', pdf

  return
end
subroutine planck_pdf_test ( )

!*****************************************************************************80
!
!! PLANCK_PDF_TEST tests PLANCK_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  logical planck_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PLANCK_PDF_TEST'
  write ( *, '(a)' ) '  PLANCK_PDF evaluates the Planck PDF.'
  write ( *, '(a)' ) '  PLANCK_SAMPLE samples the Planck PDF.'

  a = 2.0D+00;
  b = 3.0D+00;

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. planck_check ( a, b ) ) then

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'PLANCK_PDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call planck_sample ( a, b, seed, x )

    call planck_pdf ( x, a, b, pdf )

    write ( *, '(2x,2g14.6)' ) x, pdf

  end do

  return
end
subroutine planck_sample_test ( )

!*****************************************************************************80
!
!! PLANCK_SAMPLE_TEST tests PLANCK_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  logical planck_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PLANCK_SAMPLE_TEST'
  write ( *, '(a)' ) '  PLANCK_MEAN computes the Planck mean.'
  write ( *, '(a)' ) '  PLANCK_SAMPLE samples the Planck distribution.'
  write ( *, '(a)' ) '  PLANCK_VARIANCE computes the Planck variance.'
  write ( *, '(a)' ) ''

  a = 2.0D+00;
  b = 3.0D+00;

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. planck_check ( a, b ) ) then

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'PLANCK_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call planck_mean ( a, b, mean )
  call planck_variance ( a, b, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =     ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance = ', variance

  do i = 1, sample_num
    call planck_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine poisson_cdf_test ( )

!*****************************************************************************80
!
!! POISSON_CDF_TEST tests POISSON_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  logical poisson_check
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POISSON_CDF_TEST'
  write ( *, '(a)' ) '  POISSON_CDF evaluates the Poisson CDF,'
  write ( *, '(a)' ) '  POISSON_CDF_INV inverts the Poisson CDF.'
  write ( *, '(a)' ) '  POISSON_PDF evaluates the Poisson PDF.'

  a = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =  ', a

  if ( .not. poisson_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'POISSON_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call poisson_sample ( a, seed, x )

    call poisson_pdf ( x, a, pdf )

    call poisson_cdf ( x, a, cdf )

    call poisson_cdf_inv ( cdf, a, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine poisson_sample_test ( )

!*****************************************************************************80
!
!! POISSON_SAMPLE_TEST tests POISSON_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  logical poisson_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POISSON_SAMPLE_TEST'
  write ( *, '(a)' ) '  POISSON_MEAN computes the Poisson mean;'
  write ( *, '(a)' ) '  POISSON_SAMPLE samples the Poisson distribution;'
  write ( *, '(a)' ) '  POISSON_VARIANCE computes the Poisson variance.'

  a = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a

  if ( .not. poisson_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'POISSON_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call poisson_mean ( a, mean )
  call poisson_variance ( a, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call poisson_sample ( a, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine power_cdf_test ( )

!*****************************************************************************80
!
!! POWER_CDF_TEST tests POWER_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  logical power_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POWER_CDF_TEST'
  write ( *, '(a)' ) '  POWER_CDF evaluates the Power CDF;'
  write ( *, '(a)' ) '  POWER_CDF_INV inverts the Power CDF.'
  write ( *, '(a)' ) '  POWER_PDF evaluates the Power PDF;'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =       ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =       ', b

  if ( .not. power_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'POWER_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call power_sample ( a, b, seed, x )

    call power_pdf ( x, a, b, pdf )

    call power_cdf ( x, a, b, cdf )

    call power_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine power_sample_test ( )

!*****************************************************************************80
!
!! POWER_SAMPLE_TEST tests POWER_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  logical power_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POWER_SAMPLE_TEST'
  write ( *, '(a)' ) '  POWER_MEAN computes the Power mean;'
  write ( *, '(a)' ) '  POWER_SAMPLE samples the Power distribution;'
  write ( *, '(a)' ) '  POWER_VARIANCE computes the Power variance.'

  a = 2.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. power_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'POWER_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call power_mean ( a, b, mean )
  call power_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call power_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine quasigeometric_cdf_test ( )

!*****************************************************************************80
!
!! QUASIGEOMETRIC_CDF_TEST tests QUASIGEOMETRIC_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  logical quasigeometric_check
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUASIGEOMETRIC_CDF_TEST'
  write ( *, '(a)' ) '  QUASIGEOMETRIC_CDF evaluates the Quasigeometric CDF;'
  write ( *, '(a)' ) '  QUASIGEOMETRIC_CDF_INV inverts the Quasigeometric CDF.'
  write ( *, '(a)' ) '  QUASIGEOMETRIC_PDF evaluates the Quasigeometric PDF;'

  a = 0.4825D+00
  b = 0.5893D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =      ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =      ', b

  if ( .not. quasigeometric_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'QUASIGEOMETRIC_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call quasigeometric_sample ( a, b, seed, x )

    call quasigeometric_pdf ( x, a, b, pdf )

    call quasigeometric_cdf ( x, a, b, cdf )

    call quasigeometric_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine quasigeometric_sample_test ( )

!*****************************************************************************80
!
!! QUASIGEOMETRIC_SAMPLE_TEST tests QUASIGEOMETRIC_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  logical quasigeometric_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'QUASIGEOMETRIC_SAMPLE_TEST'
  write ( *, '(a)' ) '  QUASIGEOMETRIC_MEAN computes the Quasigeometric mean;'
  write ( *, '(a)' ) '  QUASIGEOMETRIC_SAMPLE samples the Quasigeometric distribution;'
  write ( *, '(a)' ) '  QUASIGEOMETRIC_VARIANCE computes the Quasigeometric variance.'

  a = 0.4825D+00
  b = 0.5893D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =      ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =      ', b

  if ( .not. quasigeometric_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'QUASIGEOMETRIC_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call quasigeometric_mean ( a, b, mean )
  call quasigeometric_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call quasigeometric_sample ( a, b, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine r8_beta_test ( )

!*****************************************************************************80
!
!! R8_BETA_TEST tests R8_BETA.
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

  real ( kind = 8 ) fxy1
  real ( kind = 8 ) fxy2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_beta
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BETA_TEST:'
  write ( *, '(a)' ) '  R8_BETA evaluates the Beta function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '          X               Y            BETA(X,Y)            R8_BETA(X,Y)'
  write ( *, '(a)' ) '                                       tabulated            computed'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call beta_values ( n_data, x, y, fxy1 )

    if ( n_data == 0 ) then
      exit
    end if

    fxy2 = r8_beta ( x, y )

    write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) x, y, fxy1, fxy2

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
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) r8_ceiling
  integer ( kind = 4 ) ival
  real ( kind = 8 ) rval

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CEILING_TEST'
  write ( *, '(a)' ) '  R8_CEILING rounds an R8 up.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X           R8_CEILING(X)'
  write ( *, '(a)' ) ''

  do i = -6, 6
    rval = real ( i, kind = 8 ) / 5.0D+00
    ival = r8_ceiling ( rval )
    write ( *, '(2x,g14.6,i8)' ) rval, ival
  end do

  return
end
subroutine r8_error_f_test ( )

!*****************************************************************************80
!
!! R8_ERROR_F_TEST tests R8_ERROR_F.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_error_f
  real ( kind = 8 ) r8_error_f_inverse
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ERROR_F_TEST'
  write ( *, '(a)' ) '  R8_ERROR_F evaluates the error function erf(x).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'X   -> Y = R8_ERROR_F(X) -> Z = R8_ERROR_F_INVERSE(Y)'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 20

    call normal_01_sample ( seed, x )
    y = r8_error_f ( x )
    z = r8_error_f_inverse ( y )
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, y, z

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
!    06 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) g
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_factorial

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_FACTORIAL_TEST'
  write ( *, '(a)' ) '  R8_FACTORIAL evaluates the factorial function.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       I    R8_FACTORIAL(I)'
  write ( *, '(a)' ) ''

  do i = 0, 20
    g = r8_factorial ( i )
    write ( *, '(2x,i8,2x,g14.6)' ) i, g
  end do

  return
end
subroutine r8_gamma_inc_test ( )

!*****************************************************************************80
!
!! R8_GAMMA_INC_TEST tests R8_GAMMA_INC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_gamma_inc
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_GAMMA_INC_TEST:'
  write ( *, '(a)' ) '  R8_GAMMA_INC evaluates the normalized incomplete Gamma'
  write ( *, '(a)' ) '  function P(A,X).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    A     X       Exact F      R8_GAMMA_INC(A,X)'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call gamma_inc_values ( n_data, a, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_gamma_inc ( a, x )

    write ( *, '(2x,2f8.4,2g14.6)' ) a, x, fx, fx2

  end do

  return
end
subroutine r8_gamma_log_int_test ( )

!*****************************************************************************80
!
!! R8_GAMMA_LOG_INT_TEST tests R8_GAMMA_LOG_INT;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) g
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_gamma_log_int

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_GAMMA_LOG_INT_TEST'
  write ( *, '(a)' ) '  R8_GAMMA_LOG_INT evaluates the logarithm of the'
  write ( *, '(a)' ) '  gamma function for integer argument.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       I    R8_GAMMA_LOG_INT(I)'
  write ( *, '(a)' ) ''

  do i = 1, 20
    g = r8_gamma_log_int ( i )
    write ( *, '(2x,i8,2x,g14.6)' ) i, g
  end do

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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  R8_UNIFORM_01 samples a uniform random'
  write ( *, '(a)' ) '  distribution in [0,1].'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Starting with seed = ', seed

  do i = 1, n
    x(i) = r8_uniform_01 ( seed )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  First few values:'
  write ( *, '(a)' ) ' '
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

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of values computed was N = ', n
  write ( *, '(a,g14.6)' ) '  Average value was ', mean
  write ( *, '(a,g14.6)' ) '  Minimum value was ', minval ( x(1:n) )
  write ( *, '(a,g14.6)' ) '  Maximum value was ', maxval ( x(1:n) )
  write ( *, '(a,g14.6)' ) '  Variance was ', variance

  return
end
subroutine r8_zeta_test ( )

!*****************************************************************************80
!
!! R8_ZETA_TEST tests R8_ZETA.
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

  integer ( kind = 4 ) i
  real ( kind = 8 ) p
  real ( kind = 8 ) r8_zeta
  real ( kind = 8 ) v

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ZETA_TEST'
  write ( *, '(a)' ) '  R8_ZETA estimates the Zeta function.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       P     R8_Zeta(P)'
  write ( *, '(a)' ) ''
  do i = 1, 25
    p = real ( i, kind = 8 )
    v = r8_zeta ( p )
    write ( *, '(2x,f6.0,2x,g14.6)' ) p, v
  end do

  write ( *, '(a)' ) ''
  do i = 0, 8
    p = 3.0 + real ( i, kind = 8 ) / 8.0D+00
    v = r8_zeta ( p );
    write ( *, '(2x,f6.0,2x,g14.6)' ) p, v
  end do

  return
end
subroutine rayleigh_cdf_test ( )

!*****************************************************************************80
!
!! RAYLEIGH_CDF_TEST tests RAYLEIGH_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  logical rayleigh_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAYLEIGH_CDF_TEST'
  write ( *, '(a)' ) '  RAYLEIGH_CDF evaluates the Rayleigh CDF;'
  write ( *, '(a)' ) '  RAYLEIGH_CDF_INV inverts the Rayleigh CDF.'
  write ( *, '(a)' ) '  RAYLEIGH_PDF evaluates the Rayleigh PDF;'

  a = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a

  if ( .not. rayleigh_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'RAYLEIGH_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call rayleigh_sample ( a, seed, x )

    call rayleigh_pdf ( x, a, pdf )

    call rayleigh_cdf ( x, a, cdf )

    call rayleigh_cdf_inv ( cdf, a, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine rayleigh_sample_test ( )

!*****************************************************************************80
!
!! RAYLEIGH_SAMPLE_TEST tests RAYLEIGH_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  logical rayleigh_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RAYLEIGH_SAMPLE_TEST'
  write ( *, '(a)' ) '  RAYLEIGH_MEAN computes the Rayleigh mean;'
  write ( *, '(a)' ) '  RAYLEIGH_SAMPLE samples the Rayleigh distribution;'
  write ( *, '(a)' ) '  RAYLEIGH_VARIANCE computes the Rayleigh variance.'

  a = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a

  if ( .not. rayleigh_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'RAYLEIGH_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call rayleigh_mean ( a, mean )
  call rayleigh_variance ( a, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call rayleigh_sample ( a, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine reciprocal_cdf_test ( )

!*****************************************************************************80
!
!! RECIPROCAL_CDF_TEST tests RECIPROCAL_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  logical reciprocal_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RECIPROCAL_CDF_TEST'
  write ( *, '(a)' ) '  RECIPROCAL_CDF evaluates the Reciprocal CDF.'
  write ( *, '(a)' ) '  RECIPROCAL_CDF_INV inverts the Reciprocal CDF.'
  write ( *, '(a)' ) '  RECIPROCAL_PDF evaluates the Reciprocal PDF.'

  a = 1.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =         ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =         ', b

  if ( .not. reciprocal_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'RECIPROCAL_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call reciprocal_sample ( a, b, seed, x )

    call reciprocal_pdf ( x, a, b, pdf )

    call reciprocal_cdf ( x, a, b, cdf )

    call reciprocal_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine reciprocal_sample_test ( )

!*****************************************************************************80
!
!! RECIPROCAL_SAMPLE_TEST tests RECIPROCAL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  logical reciprocal_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RECIPROCAL_SAMPLE_TEST'
  write ( *, '(a)' ) '  RECIPROCAL_MEAN computes the Reciprocal mean;'
  write ( *, '(a)' ) '  RECIPROCAL_SAMPLE samples the Reciprocal distribution;'
  write ( *, '(a)' ) '  RECIPROCAL_VARIANCE computes the Reciprocal variance.'

  a = 1.0D+00
  b = 3.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B = ', b

  if ( .not. reciprocal_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'RECIPROCAL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call reciprocal_mean ( a, b, mean )
  call reciprocal_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call reciprocal_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine runs_pdf_test ( )

!*****************************************************************************80
!
!! RUNS_PDF_TEST tests RUNS_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) pdf
  real ( kind = 8 ) pdf_total
  integer ( kind = 4 ) r

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RUNS_PDF_TEST'
  write ( *, '(a)' ) '  RUNS_PDF evaluates the Runs PDF;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  M is the number of symbols of one kind,'
  write ( *, '(a)' ) '  N is the number of symbols of the other kind,'
  write ( *, '(a)' ) '  R is the number of runs (sequences of one symbol)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         M         N         R      PDF'
  write ( *, '(a)' ) ''

  m = 6

  do n = 0, 8

    write ( *, '(a)' ) ''
    pdf_total = 0.0D+00

    do r = 1, 2 * min ( m, n ) + 2

      call runs_pdf ( m, n, r, pdf )
      write ( *, '(2x,i8,2x,i8,2x,i8,2x,g14.6)' ) m, n, r, pdf
      pdf_total = pdf_total + pdf

    end do

    write ( *, '(2x,i8,2x,8x,2x,8x,2x,g14.6)' ) m, pdf_total

  end do

  return
end
subroutine runs_sample_test ( )

!*****************************************************************************80
!
!! RUNS_SAMPLE_TEST tests RUNS_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  real ( kind = 8 ) mean
  integer ( kind = 4 ) n
  integer ( kind = 4 ) r(sample_num)
  integer ( kind = 4 ) rmax
  integer ( kind = 4 ) rmin
  integer ( kind = 4 ) seed
  real ( kind = 8 ) variance

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RUNS_SAMPLE_TEST'
  write ( *, '(a)' ) '  RUNS_MEAN computes the Runs mean;'
  write ( *, '(a)' ) '  RUNS_SAMPLE samples the Runs distribution;'
  write ( *, '(a)' ) '  RUNS_VARIANCE computes the Runs variance'

  m = 10
  n = 5

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter M = ', m
  write ( *, '(a,g14.6)' ) '  PDF parameter N = ', n

  call runs_mean ( m, n, mean )
  call runs_variance ( m, n, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  seed = 123456789

  do i = 1, sample_num
    call runs_sample ( m, n, seed, r(i) )
  end do

  call i4vec_mean ( sample_num, r, mean )
  call i4vec_variance ( sample_num, r, variance )
  call i4vec_max ( sample_num, r, rmax )
  call i4vec_min ( sample_num, r, rmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', rmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', rmin


  return
end
subroutine sech_cdf_test ( )

!*****************************************************************************80
!
!! SECH_CDF_TEST tests SECH_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  logical sech_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SECH_CDF_TEST'
  write ( *, '(a)' ) '  SECH_CDF evaluates the Sech CDF.'
  write ( *, '(a)' ) '  SECH_CDF_INV inverts the Sech CDF.'
  write ( *, '(a)' ) '  SECH_PDF evaluates the Sech PDF.'

  a = 3.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =         ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =         ', b

  if ( .not. sech_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SECH_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call sech_sample ( a, b, seed, x )

    call sech_pdf ( x, a, b, pdf )

    call sech_cdf ( x, a, b, cdf )

    call sech_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine sech_sample_test ( )

!*****************************************************************************80
!
!! SECH_SAMPLE_TEST tests SECH_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  logical sech_check
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SECH_SAMPLE_TEST'
  write ( *, '(a)' ) '  SECH_MEAN computes the Sech mean;'
  write ( *, '(a)' ) '  SECH_SAMPLE samples the Sech distribution;'
  write ( *, '(a)' ) '  SECH_VARIANCE computes the Sech variance.'

  a = 3.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. sech_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SECH_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call sech_mean ( a, b, mean )
  call sech_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call sech_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine semicircular_cdf_test ( )

!*****************************************************************************80
!
!! SEMICIRCULAR_CDF_TEST tests SEMICIRCULAR_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  logical semicircular_check
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SEMICIRCULAR_CDF_TEST'
  write ( *, '(a)' ) '  SEMICIRCULAR_CDF evaluates the Semicircular CDF.'
  write ( *, '(a)' ) '  SEMICIRCULAR_CDF_INV inverts the Semicircular CDF.'
  write ( *, '(a)' ) '  SEMICIRCULAR_PDF evaluates the Semicircular PDF.'

  a = 3.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =         ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =         ', b

  if ( .not. semicircular_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SEMICIRCULAR_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call semicircular_sample ( a, b, seed, x )

    call semicircular_pdf ( x, a, b, pdf )

    call semicircular_cdf ( x, a, b, cdf )

    call semicircular_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine semicircular_sample_test ( )

!*****************************************************************************80
!
!! SEMICIRCULAR_SAMPLE_TEST tests SEMICIRCULAR_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  logical semicircular_check
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SEMICIRCULAR_SAMPLE_TEST'
  write ( *, '(a)' ) '  SEMICIRCULAR_MEAN computes the Semicircular mean;'
  write ( *, '(a)' ) '  SEMICIRCULAR_SAMPLE samples the Semicircular distribution;'
  write ( *, '(a)' ) '  SEMICIRCULAR_VARIANCE computes the Semicircular variance.'

  a = 3.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. semicircular_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SEMICIRCULAR_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call semicircular_mean ( a, b, mean )
  call semicircular_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call semicircular_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine student_cdf_test ( )

!*****************************************************************************80
!
!! STUDENT_CDF_TEST tests STUDENT_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) seed
  logical student_check
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'STUDENT_CDF_TEST'
  write ( *, '(a)' ) '  STUDENT_CDF evaluates the Student CDF.'
  write ( *, '(a)' ) '  STUDENT_PDF evaluates the Student PDF.'
  write ( *, '(a)' ) '  STUDENT_SAMPLE samples the Student PDF.'

  a = 0.5D+00
  b = 2.0D+00
  c = 6.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =   ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =   ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =   ', c

  if ( .not. student_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'STUDENT_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10

    call student_sample ( a, b, c, seed, x )

    call student_pdf ( x, a, b, c, pdf )

    call student_cdf ( x, a, b, c, cdf )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, pdf, cdf

  end do

  return
end
subroutine student_sample_test ( )

!*****************************************************************************80
!
!! STUDENT_SAMPLE_TEST tests STUDENT_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  logical student_check
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'STUDENT_SAMPLE_TEST'
  write ( *, '(a)' ) '  STUDENT_MEAN computes the Student mean;'
  write ( *, '(a)' ) '  STUDENT_SAMPLE samples the Student distribution;'
  write ( *, '(a)' ) '  STUDENT_VARIANCE computes the Student variance.'

  a = 0.5D+00
  b = 2.0D+00
  c = 6.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. student_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'STUDENT_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call student_mean ( a, b, c, mean )
  call student_variance ( a, b, c, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call student_sample ( a, b, c, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine student_noncentral_cdf_test ( )

!*****************************************************************************80
!
!! STUDENT_NONCENTRAL_CDF_TEST tests STUDENT_NONCENTRAL_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) idf
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'STUDENT_NONCENTRAL_CDF_TEST'
  write ( *, '(a)' ) '  STUDENT_NONCENTRAL_CDF evaluates the Student Noncentral CDF;'

  x = 0.50D+00

  idf = 10
  b = 1.0D+00

  call student_noncentral_cdf ( x, idf, b, cdf )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF argument X =              ', x
  write ( *, '(a,i8)' ) '  PDF parameter IDF =           ', idf
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  CDF value =                   ', cdf

  return
end
subroutine tfn_test ( )

!*****************************************************************************80
!
!! TFN_TEST tests TFN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) h
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) t
  real ( kind = 8 ) t2
  real ( kind = 8 ) tfn

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TFN_TEST'
  write ( *, '(a)' ) '  TFN evaluates Owen''s T function;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '      H             A           T(H,A)      Exact'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call owen_values ( n_data, h, a, t )

    if ( n_data <= 0 ) then
      exit
    end if

    t2 = tfn ( h, a )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) h, a, t2, t

  end do

  return
end
subroutine triangle_cdf_test ( )

!*****************************************************************************80
!
!! TRIANGLE_CDF_TEST tests TRIANGLE_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  logical triangle_check
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_CDF_TEST'
  write ( *, '(a)' ) '  TRIANGLE_CDF evaluates the Triangle CDF;'
  write ( *, '(a)' ) '  TRIANGLE_CDF_INV inverts the Triangle CDF.'
  write ( *, '(a)' ) '  TRIANGLE_PDF evaluates the Triangle PDF;'

  a = 1.0D+00
  b = 3.0D+00
  c = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =      ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =      ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =      ', c

  if ( .not. triangle_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRIANGLE_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call triangle_sample ( a, b, c, seed, x )

    call triangle_pdf ( x, a, b, c, pdf )

    call triangle_cdf ( x, a, b, c, cdf )

    call triangle_cdf_inv ( cdf, a, b, c, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine triangle_sample_test ( )

!*****************************************************************************80
!
!! TRIANGLE_SAMPLE_TEST tests TRIANGLE_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  logical triangle_check
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_SAMPLE_TEST'
  write ( *, '(a)' ) '  TRIANGLE_MEAN returns the Triangle mean;'
  write ( *, '(a)' ) '  TRIANGLE_SAMPLE samples the Triangle distribution;'
  write ( *, '(a)' ) '  TRIANGLE_VARIANCE returns the Triangle variance;'

  a = 1.0D+00
  b = 3.0D+00
  c = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. triangle_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRIANGLE_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call triangle_mean ( a, b, c, mean )
  call triangle_variance ( a, b, c, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter MEAN =          ', mean
  write ( *, '(a,g14.6)' ) '  PDF parameter VARIANCE =      ', variance

  do i = 1, sample_num
    call triangle_sample ( a, b, c, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine triangular_cdf_test ( )

!*****************************************************************************80
!
!! TRIANGULAR_CDF_TEST tests TRIANGULAR_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  logical triangular_check
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGULAR_CDF_TEST'
  write ( *, '(a)' ) '  TRIANGULAR_CDF evaluates the Triangular CDF;'
  write ( *, '(a)' ) '  TRIANGULAR_CDF_INV inverts the Triangular CDF.'
  write ( *, '(a)' ) '  TRIANGULAR_PDF evaluates the Triangular PDF;'

  a = 1.0D+00
  b = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =      ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =      ', b

  if ( .not. triangular_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRIANGULAR_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call triangular_sample ( a, b, seed, x )

    call triangular_pdf ( x, a, b, pdf )

    call triangular_cdf ( x, a, b, cdf )

    call triangular_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine triangular_sample_test ( )

!*****************************************************************************80
!
!! TRIANGULAR_SAMPLE_TEST tests TRIANGULAR_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  logical triangular_check
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGULAR_SAMPLE_TEST'
  write ( *, '(a)' ) '  TRIANGULAR_MEAN computes the Triangular mean;'
  write ( *, '(a)' ) '  TRIANGULAR_SAMPLE samples the Triangular distribution;'
  write ( *, '(a)' ) '  TRIANGULAR_VARIANCE computes the Triangular variance.'

  a = 1.0D+00
  b = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. triangular_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'TRIANGULAR_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call triangular_mean ( a, b, mean )
  call triangular_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call triangular_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine uniform_01_order_sample_test ( )

!*****************************************************************************80
!
!! UNIFORM_01_ORDER_SAMPLE_TEST tests UNIFORM_01_ORDER_SAMPLE;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UNIFORM_01_ORDER_SAMPLE_TEST'
  write ( *, '(a)' ) '  UNIFORM_ORDER_SAMPLE samples the Uniform 01 Order distribution.'
  write ( *, '(a)' ) ''

  call uniform_01_order_sample ( n, seed, x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Ordered sample:'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,i8,g14.6)' ) i, x(i)
  end do

  return
end
subroutine uniform_nsphere_sample_test ( )

!*****************************************************************************80
!
!! UNIFORM_NSPHERE_SAMPLE_TEST tests UNIFORM_NSPHERE_SAMPLE;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) i
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UNIFORM_NSPHERE_SAMPLE_TEST'
  write ( *, '(a)' ) '  UNIFORM_NSPHERE_SAMPLE samples the Uniform Nsphere distribution.'

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)' ) '  Dimension N of sphere =       ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Points on the sphere:'
  write ( *, '(a)' ) ''

  do i = 1, 10
    call uniform_nsphere_sample ( n, seed, x )
    write ( *, '(2x,i8,3g14.6)' ) i, x(1:n)
  end do

  return
end
subroutine uniform_01_cdf_test ( )

!*****************************************************************************80
!
!! UNIFORM_01_CDF_TEST tests UNIFORM_01_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) uniform_01_sample
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UNIFORM_01_CDF_TEST'
  write ( *, '(a)' ) '  UNIFORM_01_CDF evaluates the Uniform 01 CDF;'
  write ( *, '(a)' ) '  UNIFORM_01_CDF_INV inverts the Uniform 01 CDF.'
  write ( *, '(a)' ) '  UNIFORM_01_PDF evaluates the Uniform 01 PDF;'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    x = uniform_01_sample ( seed )

    call uniform_01_pdf ( x, pdf )

    call uniform_01_cdf ( x, cdf )

    call uniform_01_cdf_inv ( cdf, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine uniform_01_sample_test ( )

!*****************************************************************************80
!
!! UNIFORM_01_SAMPLE_TEST tests UNIFORM_01_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) uniform_01_sample
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UNIFORM_01_SAMPLE_TEST'
  write ( *, '(a)' ) '  UNIFORM_01_MEAN computes the Uniform 01 mean;'
  write ( *, '(a)' ) '  UNIFORM_01_SAMPLE samples the Uniform 01 distribution;'
  write ( *, '(a)' ) '  UNIFORM_01_VARIANCE computes the Uniform 01 variance.'

  call uniform_01_mean ( mean )
  call uniform_01_variance ( variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =            ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =        ', variance

  do i = 1, sample_num
    x(i) = uniform_01_sample ( seed )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine uniform_cdf_test ( )

!*****************************************************************************80
!
!! UNIFORM_CDF_TEST tests UNIFORM_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  logical uniform_check
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UNIFORM_CDF_TEST'
  write ( *, '(a)' ) '  UNIFORM_CDF evaluates the Uniform CDF;'
  write ( *, '(a)' ) '  UNIFORM_CDF_INV inverts the Uniform CDF.'
  write ( *, '(a)' ) '  UNIFORM_PDF evaluates the Uniform PDF;'

  a = 1.0D+00
  b = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =      ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =      ', b

  if ( .not. uniform_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'UNIFORM_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call uniform_sample ( a, b, seed, x )

    call uniform_pdf ( x, a, b, pdf )

    call uniform_cdf ( x, a, b, cdf )

    call uniform_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine uniform_sample_test ( )

!*****************************************************************************80
!
!! UNIFORM_SAMPLE_TEST tests UNIFORM_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  logical uniform_check
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UNIFORM_SAMPLE_TEST'
  write ( *, '(a)' ) '  UNIFORM_MEAN computes the Uniform mean;'
  write ( *, '(a)' ) '  UNIFORM_SAMPLE samples the Uniform distribution;'
  write ( *, '(a)' ) '  UNIFORM_VARIANCE computes the Uniform variance.'

  a = 1.0D+00
  b = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. uniform_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'UNIFORM_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call uniform_mean ( a, b, mean )
  call uniform_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =        ', variance

  do i = 1, sample_num
    call uniform_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine uniform_discrete_cdf_test ( )

!*****************************************************************************80
!
!!  tests UNIFORM_DISCRETE_CDF.
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

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  logical uniform_discrete_check
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UNIFORM_DISCRETE_CDF_TEST'
  write ( *, '(a)' ) '  UNIFORM_DISCRETE_CDF evaluates the Uniform Discrete CDF;'
  write ( *, '(a)' ) '  UNIFORM_DISCRETE_CDF_INV inverts the Uniform Discrete CDF.'
  write ( *, '(a)' ) '  UNIFORM_DISCRETE_PDF evaluates the Uniform Discrete PDF;'

  a = 1
  b = 6

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter A =             ', a
  write ( *, '(a,i8)'    ) '  PDF parameter B =             ', b

  if ( .not. uniform_discrete_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'UNIFORM_DISCRETE_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call uniform_discrete_sample ( a, b, seed, x )

    call uniform_discrete_pdf ( x, a, b, pdf )

    call uniform_discrete_cdf ( x, a, b, cdf )

    call uniform_discrete_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine uniform_discrete_sample_test ( )

!*****************************************************************************80
!
!! UNIFORM_DISCRETE_SAMPLE_TEST tests UNIFORM_DISCRETE_SAMPLE.
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

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  logical uniform_discrete_check
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UNIFORM_DISCRETE_SAMPLE_TEST'
  write ( *, '(a)' ) '  UNIFORM_DISCRETE_MEAN computes the Uniform Discrete mean;'
  write ( *, '(a)' ) '  UNIFORM_DISCRETE_SAMPLE samples the Uniform Discrete distribution;'
  write ( *, '(a)' ) '  UNIFORM_DISCRETE_VARIANCE computes the Uniform Discrete variance.'

  a = 1
  b = 6

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  PDF parameter A =             ', a
  write ( *, '(a,i8)'    ) '  PDF parameter B =             ', b

  if ( .not. uniform_discrete_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'UNIFORM_DISCRETE_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call uniform_discrete_mean ( a, b, mean )
  call uniform_discrete_variance ( a, b, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call uniform_discrete_sample ( a, b, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine von_mises_cdf_test ( )

!*****************************************************************************80
!
!! VON_MISES_CDF_TEST tests VON_MISES_CDF.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  logical von_mises_check
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VON_MISES_CDF_TEST'
  write ( *, '(a)' ) '  VON_MISES_CDF evaluates the Von Mises CDF.'
  write ( *, '(a)' ) '  VON_MISES_CDF_INV inverts the Von Mises CDF.'
  write ( *, '(a)' ) '  VON_MISES_PDF evaluates the Von Mises PDF.'

  a = 1.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =      ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =      ', b

  if ( .not. von_mises_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'VON_MISES_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call von_mises_sample ( a, b, seed, x )

    call von_mises_pdf ( x, a, b, pdf )

    call von_mises_cdf ( x, a, b, cdf )

    call von_mises_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine von_mises_sample_test ( )

!*****************************************************************************80
!
!! VON_MISES_SAMPLE_TEST tests VON_MISES_SAMPLE.
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

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) circular_variance
  logical von_mises_check
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VON_MISES_SAMPLE_TEST'
  write ( *, '(a)' ) '  VON_MISES_MEAN computes the Von Mises mean;'
  write ( *, '(a)' ) '  VON_MISES_SAMPLE samples the Von Mises distribution.'
  write ( *, '(a)' ) &
    '  VON_MISES_CIRCULAR_VARIANCE computes the Von Mises circular_variance.'

  a = 1.0D+00
  b = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. von_mises_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'VON_MISES_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call von_mises_mean ( a, b, mean )
  call von_mises_circular_variance ( a, b, circular_variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF circular variance =       ', circular_variance

  do i = 1, sample_num
    call von_mises_sample ( a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_circular_variance ( sample_num, x, circular_variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =              ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =              ', mean
  write ( *, '(a,g14.6)' ) '  Sample circular variance = ', circular_variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =           ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =           ', xmin

  return
end
subroutine weibull_cdf_test ( )

!*****************************************************************************80
!
!! WEIBULL_CDF_TEST tests WEIBULL_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  logical weibull_check
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'WEIBULL_CDF_TEST'
  write ( *, '(a)' ) '  WEIBULL_CDF evaluates the Weibull CDF;'
  write ( *, '(a)' ) '  WEIBULL_CDF_INV inverts the Weibull CDF.'
  write ( *, '(a)' ) '  WEIBULL_PDF evaluates the Weibull PDF;'

  x = 3.0D+00

  a = 2.0D+00
  b = 3.0D+00
  c = 4.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. weibull_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'WEIBULL_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call weibull_sample ( a, b, c, seed, x )

    call weibull_pdf ( x, a, b, c, pdf )

    call weibull_cdf ( x, a, b, c, cdf )

    call weibull_cdf_inv ( cdf, a, b, c, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine weibull_sample_test ( )

!*****************************************************************************80
!
!! WEIBULL_SAMPLE_TEST tests WEIBULL_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  logical weibull_check
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'WEIBULL_SAMPLE_TEST'
  write ( *, '(a)' ) '  WEIBULL_MEAN computes the Weibull mean;'
  write ( *, '(a)' ) '  WEIBULL_SAMPLE samples the Weibull distribution;'
  write ( *, '(a)' ) '  WEIBULL_VARIANCE computes the Weibull variance.'

  a = 2.0D+00
  b = 3.0D+00
  c = 4.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =       ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter C =             ', c

  if ( .not. weibull_check ( a, b, c ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'WEIBULL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call weibull_mean ( a, b, c, mean )
  call weibull_variance ( a, b, c, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call weibull_sample ( a, b, c, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine weibull_discrete_cdf_test ( )

!*****************************************************************************80
!
!! WEIBULL_DISCRETE_CDF_TEST tests WEIBULL_DISCRETE_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  logical weibull_discrete_check
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'WEIBULL_DISCRETE_CDF_TEST'
  write ( *, '(a)' ) '  WEIBULL_DISCRETE_CDF evaluates the Weibull Discrete CDF;'
  write ( *, '(a)' ) '  WEIBULL_DISCRETE_CDF_INV inverts the Weibull Discrete CDF.'
  write ( *, '(a)' ) '  WEIBULL_DISCRETE_PDF evaluates the Weibull Discrete PDF;'

  a = 0.50D+00
  b = 1.5D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  if ( .not. weibull_discrete_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'WEIBULL_DISCRETE_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call weibull_discrete_sample ( a, b, seed, x )

    call weibull_discrete_pdf ( x, a, b, pdf )

    call weibull_discrete_cdf ( x, a, b, cdf )

    call weibull_discrete_cdf_inv ( cdf, a, b, x2 )

    write ( *, '(2x,i14,2g14.6,i14)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine weibull_discrete_sample_test ( )

!*****************************************************************************80
!
!! WEIBULL_DISCRETE_SAMPLE_TEST tests WEIBULL_DISCRETE_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  logical weibull_discrete_check
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'WEIBULL_DISCRETE_SAMPLE_TEST'
  write ( *, '(a)' ) '  WEIBULL_DISCRETE_SAMPLE samples the Weibull Discrete PDF.'

  a = 0.5D+00
  b = 1.5D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =     ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =     ', b

  if ( .not. weibull_discrete_check ( a, b ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'WEIBULL_DISCRETE_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  do i = 1, sample_num
    call weibull_discrete_sample ( a, b, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
subroutine zipf_cdf_test ( )

!*****************************************************************************80
!
!! ZIPF_CDF_TEST tests ZIPF_CDF.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2
  logical zipf_check

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ZIPF_CDF_TEST'
  write ( *, '(a)' ) '  ZIPF_CDF evaluates the Zipf CDF.'
  write ( *, '(a)' ) '  ZIPF_CDF_INV inverts the Zipf CDF.'
  write ( *, '(a)' ) '  ZIPF_PDF evaluates the Zipf PDF.'

  a = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A = ', a

  if ( .not. zipf_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ZIPF_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  X    PDF(X)       CDF(X)    CDF_INV(CDF)'
  write ( *, '(a)' ) ''

  do x = 1, 20

    call zipf_pdf ( x, a, pdf )
    call zipf_cdf ( x, a, cdf )
    call zipf_cdf_inv ( a, cdf, x2 )
    write ( *, '(2x,i8,2x,2g14.6,2x,i8)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine zipf_sample_test ( )

!*****************************************************************************80
!
!! ZIPF_SAMPLE_TEST tests ZIPF_SAMPLE.
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

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(sample_num)
  integer ( kind = 4 ) xmax
  integer ( kind = 4 ) xmin
  logical zipf_check

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ZIPF_SAMPLE_TEST'
  write ( *, '(a)' ) '  ZIPF_MEAN computes the mean of the Zipf distribution.'
  write ( *, '(a)' ) '  ZIPF_SAMPLE samples the Zipf distribution.'
  write ( *, '(a)' ) '  ZIPF_VARIANCE computes the variance of the Zipf distribution.'

  a = 4.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a

  if ( .not. zipf_check ( a ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ZIPF_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call zipf_mean ( a, mean )
  call zipf_variance ( a, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call zipf_sample ( a, seed, x(i) )
  end do

  call i4vec_mean ( sample_num, x, mean )
  call i4vec_variance ( sample_num, x, variance )
  call i4vec_max ( sample_num, x, xmax )
  call i4vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,i8)'    ) '  Sample maximum =  ', xmax
  write ( *, '(a,i8)'    ) '  Sample minimum =  ', xmin

  return
end
