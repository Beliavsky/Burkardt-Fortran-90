program main

!*****************************************************************************80
!
!! LOG_NORMAL_TRUNCATED_AB_TEST tests the LOG_NORMAL_TRUNCATED_AB library.
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

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOG_NORMAL_TRUNCATED_AB_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the LOG_NORMAL_TRUNCATED_AB library.'

  call log_normal_truncated_ab_cdf_test ( )
  call log_normal_truncated_ab_sample_test
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOG_NORMAL_TRUNCATED_AB_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine log_normal_truncated_ab_cdf_test ( )

!*****************************************************************************80
!
!! LOG_NORMAL_TRUNCATED_AB_CDF_TEST tests LOG_NORMAL_TRUNCATED_AB_CDF.
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
  real ( kind = 8 ) cdf
  logical ( kind = 4 ) check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOG_NORMAL_TRUNCATED_AB_CDF_TEST'
  write ( *, '(a)' ) '  LOG_NORMAL_TRUNCATED_AB_CDF evaluates the Lognormal Truncated AB CDF'
  write ( *, '(a)' ) '  LOG_NORMAL_TRUNCATED_AB_CDF_INV inverts the Lognormal Truncated AB CDF.'
  write ( *, '(a)' ) '  LOG_NORMAL_TRUNCATED_AB_PDF evaluates the Lognormal Truncated AB PDF'

  mu = 0.5D+00
  sigma = 3.0D+00
  a = exp ( mu )
  b = exp ( mu + 2.0D+00 * sigma )

  call log_normal_truncated_ab_check ( mu, sigma, a, b, check )

  if ( .not. check ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LOG_NORMAL_TRUNCATED_AB_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =            ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA =         ', sigma
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '              X             PDF             CDF         CDF_INV'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call log_normal_truncated_ab_sample ( mu, sigma, a, b, seed, x )

    call log_normal_truncated_ab_pdf ( x, mu, sigma, a, b, pdf )

    call log_normal_truncated_ab_cdf ( x, mu, sigma, a, b, cdf )

    call log_normal_truncated_ab_cdf_inv ( cdf, mu, sigma, a, b, x2 )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, pdf, cdf, x2

  end do

  return
end
subroutine log_normal_truncated_ab_sample_test ( )

!*****************************************************************************80
!
!! LOG_NORMAL_TRUNCATED_AB_SAMPLE_TEST tests LOG_NORMAL_TRUNCATED_AB_SAMPLE.
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

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical ( kind = 4 ) check
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LOG_NORMAL_TRUNCATED_AB_SAMPLE_TEST'
  write ( *, '(a)' ) '  LOG_NORMAL_TRUNCATED_AB_MEAN computes the Log Normal Truncated AB mean'
  write ( *, '(a)' ) '  LOG_NORMAL_TRUNCATED_AB_SAMPLE samples the Log Normal Truncated AB distribution'
  write ( *, '(a)' ) '  LOG_NORMAL_TRUNCATED_AB_VARIANCE computes the Log Normal Truncated AB variance.'

  mu = 0.5D+00
  sigma = 3.0D+00
  a = exp ( mu )
  b = exp ( mu + 2.0D+00 * sigma )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =            ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA =         ', sigma
  write ( *, '(a,g14.6)' ) '  PDF parameter A =             ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter B =             ', b

  call log_normal_truncated_ab_check ( mu, sigma, a, b, check )

  if ( .not. check ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LOG_NORMAL_TRUNCATED_AB_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call log_normal_truncated_ab_mean ( mu, sigma, a, b, mean )
  call log_normal_truncated_ab_variance ( mu, sigma, a, b, variance )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call log_normal_truncated_ab_sample ( mu, sigma, a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
