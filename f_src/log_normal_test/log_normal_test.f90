program main

!*****************************************************************************80
!
!! MAIN is the main program for LOG_NORMAL_TEST.
!
!  Discussion:
!
!    LOG_NORMAL_TEST tests the LOG_NORMAL library.
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
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LOG_NORMAL_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Test the LOG_NORMAL library.'

  call log_normal_cdf_test ( )
  call log_normal_sample_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LOG_NORMAL_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine log_normal_cdf_test ( )

!*****************************************************************************80
!
!! LOG_NORMAL_CDF_TEST tests LOG_NORMAL_CDF, LOG_NORMAL_CDF_INV, LOG_NORMAL_PDF.
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
  logical log_normal_check
  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LOG_NORMAL_CDF_TEST'
  write ( *, '(a)' ) '  LOG_NORMAL_CDF evaluates the Log Normal CDF;'
  write ( *, '(a)' ) '  LOG_NORMAL_CDF_INV inverts the Log Normal CDF.'
  write ( *, '(a)' ) '  LOG_NORMAL_PDF evaluates the Log Normal PDF;'

  mu = 10.0D+00
  sigma = 2.25D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  if ( .not. log_normal_check ( mu, sigma ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LOG_NORMAL_CDF_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X            PDF           CDF            CDF_INV'
  write ( *, '(a)' ) ' '

  do i = 1, 10

    call log_normal_sample ( mu, sigma, seed, x )

    call log_normal_pdf ( x, mu, sigma, pdf )

    call log_normal_cdf ( x, mu, sigma, cdf )

    call log_normal_cdf_inv ( cdf, mu, sigma, x2 )

    write ( *, '(2x,4g14.6)' ) x, pdf, cdf, x2

  end do

  return
end
subroutine log_normal_sample_test ( )

!*****************************************************************************80
!
!! LOG_NORMAL_SAMPLE_TEST tests LOG_NORMAL_MEAN, LOG_NORMAL_SAMPLE, LOG_NORMAL_VARIANCE.
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

  integer ( kind = 4 ) i
  logical log_normal_check
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  integer ( kind = 4 ) :: seed = 123456789
  real ( kind = 8 ) sigma
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LOG_NORMAL_SAMPLE_TEST'
  write ( *, '(a)' ) '  LOG_NORMAL_MEAN computes the Log Normal mean;'
  write ( *, '(a)' ) '  LOG_NORMAL_SAMPLE samples the Log Normal distribution;'
  write ( *, '(a)' ) '  LOG_NORMAL_VARIANCE computes the Log Normal variance.'

  mu = 1.0D+00
  sigma = 2.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  if ( .not. log_normal_check ( mu, sigma ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LOG_NORMAL_SAMPLE_TEST - Fatal error!'
    write ( *, '(a)' ) '  The parameters are not legal.'
    return
  end if

  call log_normal_mean ( mu, sigma, mean )
  call log_normal_variance ( mu, sigma, variance )

  write ( *, '(a,g14.6)' ) '  PDF mean =                    ', mean
  write ( *, '(a,g14.6)' ) '  PDF variance =                ', variance

  do i = 1, sample_num
    call log_normal_sample ( mu, sigma, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_variance ( sample_num, x, variance )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end

