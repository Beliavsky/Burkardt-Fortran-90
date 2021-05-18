program main

!*****************************************************************************80
!
!! MAIN is the main program for TRUNCATED_NORMAL_TEST.
!
!  Discussion:
!
!    TRUNCATED_NORMAL_TEST tests the TRUNCATED_NORMAL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Test the TRUNCATED_NORMAL library.'
!
!  Support and utility.
!
  call i4_uniform_ab_test ( )
  call r8_choose_test ( )
  call r8_factorial2_test ( )
  call r8_mop_test ( )
  call r8_uniform_01_test ( )
  call r8poly_print_test ( )
  call r8poly_value_horner_test ( )
  call r8vec_linspace_test ( )
  call r8vec_print_test ( )
!
!  Library functions.
!
  call normal_01_cdf_test ( )
  call normal_01_cdf_inv_test ( )
  call normal_01_mean_test ( )
  call normal_01_moment_test ( )
  call normal_01_pdf_test ( )
  call normal_01_sample_test ( )
  call normal_01_variance_test ( )

  call normal_ms_cdf_test ( )
  call normal_ms_cdf_inv_test ( )
  call normal_ms_mean_test ( )
  call normal_ms_moment_test ( )
  call normal_ms_moment_central_test ( )
  call normal_ms_moment_central_values_test ( )
  call normal_ms_pdf_test ( )
  call normal_ms_sample_test ( )
  call normal_ms_variance_test ( )

  call truncated_normal_a_cdf_test ( )
  call truncated_normal_a_cdf_inv_test ( )
  call truncated_normal_a_mean_test ( )
  call truncated_normal_a_moment_test ( )
  call truncated_normal_a_pdf_test ( )
  call truncated_normal_a_sample_test ( )
  call truncated_normal_a_variance_test ( )

  call truncated_normal_ab_cdf_test ( )
  call truncated_normal_ab_cdf_inv_test ( )
  call truncated_normal_ab_mean_test ( )
  call truncated_normal_ab_moment_test ( )
  call truncated_normal_ab_pdf_test ( )
  call truncated_normal_ab_sample_test ( )
  call truncated_normal_ab_variance_test ( )

  call truncated_normal_b_cdf_test ( )
  call truncated_normal_b_cdf_inv_test ( )
  call truncated_normal_b_mean_test ( )
  call truncated_normal_b_moment_test ( )
  call truncated_normal_b_pdf_test ( )
  call truncated_normal_b_sample_test ( )
  call truncated_normal_b_variance_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
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

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  I4_UNIFORM_AB computes pseudorandom values '
  write ( *, '(a)' ) '  in an interval [A,B].'

  a = -100
  b = 200
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
!    27 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf1
  real ( kind = 8 ) cdf2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_01_CDF_TEST'
  write ( *, '(a)' ) '  NORMAL_01_CDF inverts the CDF;'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X              CDF                       CDF'
  write ( *, '(a)' ) '                     (exact)                   (computed)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call normal_01_cdf_values ( n_data, x, cdf1 )

    if ( n_data == 0 ) then
      exit
    end if

    call normal_01_cdf ( x, cdf2 )

    write ( *, '(2x,g14.6,2x,g24.16,2x,g24.16)' ) x, cdf1, cdf2

  end do

  return
end
subroutine normal_01_cdf_inv_test ( )

!*****************************************************************************80
!
!! NORMAL_01_CDF_INV tests NORMAL_01_CDF_INV;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_01_CDF_INV_TEST'
  write ( *, '(a)' ) '  NORMAL_01_CDF_INV evaluates the CDF;'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      CDF             X                         X'
  write ( *, '(a)' ) '                     (exact)                   (computed)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call normal_01_cdf_values ( n_data, x1, cdf )

    if ( n_data == 0 ) then
      exit
    end if

    call normal_01_cdf_inv ( cdf, x2 )

    write ( *, '(2x,g14.6,2x,g24.16,2x,g24.16)' ) cdf, x1, x2

  end do

  return
end
subroutine normal_01_mean_test ( )

!*****************************************************************************80
!
!! NORMAL_01_MEAN_TEST tests NORMAL_01_MEAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_01_MEAN_TEST'
  write ( *, '(a)' ) '  NORMAL_01_MEAN computes the mean for tne Normal 01 PDF'

  call normal_01_mean ( mean )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean

  seed = 123456789
  do i = 1, sample_num
    call normal_01_sample ( seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine normal_01_moment_test ( )

!*****************************************************************************80
!
!! NORMAL_01_MOMENT_TEST tests NORMAL_01_MOMENT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) order
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_01_MOMENT_TEST'
  write ( *, '(a)' ) '  NORMAL_01_MOMENT returns the moments for tne Normal 01 PDF'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   Order      Moment'
  write ( *, '(a)' ) ''

  do order = 0, 10
    call normal_01_moment ( order, value )
    write ( *, '(2x,i6,2x,g14.6)' ) order, value
  end do

  return
end
subroutine normal_01_pdf_test ( )

!*****************************************************************************80
!
!! NORMAL_01_PDF_TEST tests NORMAL_01_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) pdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_01_PDF_TEST'
  write ( *, '(a)' ) '  NORMAL_01_PDF evaluates the Normal 01 PDF.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X            PDF'
  write ( *, '(a)' ) ' '

  do i = -20, 20

    x = real ( i ) / 10.0+00

    call normal_01_pdf ( x, pdf )

    write ( *, '(g14.6,2x,g14.6)' ) x, pdf

  end do

  return
end
subroutine normal_01_sample_test ( )

!*****************************************************************************80
!
!! NORMAL_01_SAMPLE_TEST tests NORMAL_01_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMAL_01_SAMPLE_TEST'
  write ( *, '(a)' ) '  NORMAL_01_SAMPLE returns samples from the normal'
  write ( *, '(a)' ) '  distribution with mean 0 and standard deviation 1.'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 10
    call normal_01_sample ( seed, x )
    write ( *, '(2x,i4,2x,g14.6)' ) i, x
  end do

  return
end
subroutine normal_01_variance_test ( )

!*****************************************************************************80
!
!! NORMAL_01_VARIANCE_TEST tests NORMAL_01_VARIANCE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_01_VARIANCE_TEST'
  write ( *, '(a)' ) '  NORMAL_01_VARIANCE returns the Normal 01 variance.'

  call normal_01_variance ( variance )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  seed = 123456789
  do i = 1, sample_num
    call normal_01_sample ( seed, x(i) )
  end do

  call r8vec_variance ( sample_num, x, variance )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance

  return
end
subroutine normal_ms_cdf_test ( )

!*****************************************************************************80
!
!! NORMAL_MS_CDF_TEST tests NORMAL_MS_CDF.
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

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_MS_CDF_TEST'
  write ( *, '(a)' ) '  NORMAL_MS_CDF evaluates the Normal MS CDF;'

  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X            CDF'
  write ( *, '(a)' ) ' '

  do i = -20, 20

    x = mu + sigma * real ( i, kind = 8 ) / 10.0D+00
    call normal_ms_cdf ( x, mu, sigma, cdf )
    write ( *, '(2x,g14.6,2x,g14.6)' ) x, cdf

  end do

  return
end
subroutine normal_ms_cdf_inv_test ( )

!*****************************************************************************80
!
!! NORMAL_MS_CDF_INV_TEST tests NORMAL_MS_CDF_INV.
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

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_MS_CDF_INV_TEST'
  write ( *, '(a)' ) '  NORMAL_MS_CDF_INV inverts the Normal MS CDF;'

  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X            CDF           CDF_INV'
  write ( *, '(a)' ) ' '

  do i = -20, 20

    x = mu + sigma * real ( i, kind = 8 ) / 10.0D+00
    call normal_ms_cdf ( x, mu, sigma, cdf )
    call normal_ms_cdf_inv ( cdf, mu, sigma, x2 )
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, cdf, x2

  end do

  return
end
subroutine normal_ms_mean_test ( )

!*****************************************************************************80
!
!! NORMAL_MS_MEAN_TEST tests NORMAL_MS_MEAN.
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

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_MS_MEAN_TEST'
  write ( *, '(a)' ) '  NORMAL_MS_MEAN computes the mean for tne Normal MS PDF'

  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  call normal_ms_mean ( mu, sigma, mean )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean

  seed = 123456789
  do i = 1, sample_num
    call normal_ms_sample ( mu, sigma, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine normal_ms_moment_test ( )

!*****************************************************************************80
!
!! NORMAL_MS_MOMENT_TEST tests NORMAL_MS_MOMENT.
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

  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  real ( kind = 8 ) sigma
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_MS_MOMENT_TEST'
  write ( *, '(a)' ) '  NORMAL_MS_MOMENT returns the moments for tne Normal MS PDF'

  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   Order      Moment'
  write ( *, '(a)' ) ''

  do order = 0, 10
    call normal_ms_moment ( order, mu, sigma, value )
    write ( *, '(2x,i6,2x,g14.6)' ) order, value
  end do

  return
end
subroutine normal_ms_moment_central_test ( )

!*****************************************************************************80
!
!! NORMAL_MS_MOMENT_CENTRAL_TEST tests NORMAL_MS_MOMENT_CENTRAL.
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

  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  real ( kind = 8 ) sigma
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_MS_MOMENT_CENTRAL_TEST'
  write ( *, '(a)' ) '  NORMAL_MS_MOMENT_CENTRAL returns central moments'
  write ( *, '(a)' ) '  for tne Normal MS PDF'

  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   Order      Moment'
  write ( *, '(a)' ) ''

  do order = 0, 10
    call normal_ms_moment_central ( order, mu, sigma, value )
    write ( *, '(2x,i6,2x,g14.6)' ) order, value
  end do

  return
end
subroutine normal_ms_moment_central_values_test ( )

!*****************************************************************************80
!
!! NORMAL_MS_MOMENT_CENTRAL_VALUES_TEST tests NORMAL_MS_MOMENT_CENTRAL_VALUES.
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

  real ( kind = 8 ) mu
  integer ( kind = 4 ) order
  real ( kind = 8 ) sigma
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_MS_MOMENT_CENTRAL_VALUES_TEST'
  write ( *, '(a)' ) '  NORMAL_MS_MOMENT_CENTRAL_VALUES returns values '
  write ( *, '(a)' ) '  of selected central moments for tne Normal MS PDF'

  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   Order      Moment'
  write ( *, '(a)' ) ''

  do order = 0, 10
    call normal_ms_moment_central_values ( order, mu, sigma, value )
    write ( *, '(2x,i6,2x,g14.6)' ) order, value
  end do

  return
end
subroutine normal_ms_pdf_test ( )

!*****************************************************************************80
!
!! NORMAL_MS_PDF_TEST tests NORMAL_MS_PDF.
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

  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_MS_PDF_TEST'
  write ( *, '(a)' ) '  NORMAL_MS_PDF evaluates the Normal MS PDF;'

  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X            PDF'
  write ( *, '(a)' ) ' '

  do i = -20, 20

    x = mu + sigma * real ( i, kind = 8 ) / 10.0D+00
    call normal_ms_pdf ( x, mu, sigma, pdf )
    write ( *, '(2x,4g14.6)' ) x, pdf

  end do

  return
end
subroutine normal_ms_sample_test ( )

!*****************************************************************************80
!
!! NORMAL_MS_SAMPLE_TEST tests NORMAL_MS_SAMPLE.
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

  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMAL_MS_SAMPLE_TEST'
  write ( *, '(a)' ) '  NORMAL_MS_SAMPLE returns samples the Normal MS PDF.'
  write ( *, '(a)' ) ''

  mu = 100.0D+00
  sigma = 15.0D+00
  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
  write ( *, '(a,i12)' ) '  SEED = ', seed
  write ( *, '(a)' ) ''

  do i = 1, 10
    call normal_ms_sample ( mu, sigma, seed, x )
    write ( *, '(2x,i4,2x,g14.6)' ) i, x
  end do

  return
end
subroutine normal_ms_variance_test ( )

!*****************************************************************************80
!
!! NORMAL_MS_VARIANCE_TEST tests NORMAL_MS_VARIANCE.
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

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'NORMAL_MS_VARIANCE_TEST'
  write ( *, '(a)' ) '  NORMAL_MS_VARIANCE returns the Normal MS variance.'

  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  call normal_ms_variance ( mu, sigma, variance )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  seed = 123456789
  do i = 1, sample_num
    call normal_ms_sample ( mu, sigma, seed, x(i) )
  end do

  call r8vec_variance ( sample_num, x, variance )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance

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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_FACTORIAL2_TEST'
  write ( *, '(a)' ) '  R8_FACTORIAL2 computes the double factorial function.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '    N                Exact' // &
    '                  Computed'
  write ( *, '(a)' ) ' '

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

  integer ( kind = 4 ), parameter :: n = 1000

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
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
!    02 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5

  real ( kind = 8 ), dimension ( 0 : m ) :: c = (/ &
    12.0D+00, -3.4D+00, 56.0D+00, 0.0D+00, 0.78D+00, 9.0D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8POLY_PRINT_TEST'
  write ( *, '(a)' ) '  R8POLY_PRINT prints an R8POLY.'

  call r8poly_print ( m, c, '  The R8POLY:' )

  return
end
subroutine r8poly_value_horner_test ( )

!*****************************************************************************80
!
!! R8POLY_VALUE_HORNER_TEST tests R8POLY_VALUE_HORNER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 16

  real ( kind = 8 ), dimension (0:m) :: c = (/ &
    24.0D+00, -50.0D+00, +35.0D+00, -10.0D+00, 1.0D+00 /)
  integer ( kind = 4 ) i
  real ( kind = 8 ) p
  real ( kind = 8 ) r8poly_value_horner
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8POLY_VALUE_HORNER_TEST'
  write ( *, '(a)' ) '  R8POLY_VALUE_HORNER evaluates a polynomial at'
  write ( *, '(a)' ) '  one point, using Horner''s method.'

  call r8poly_print ( m, c, '  The polynomial coefficients:' )

  x_lo = 0.0D+00
  x_hi = 5.0D+00
  call r8vec_linspace ( n, x_lo, x_hi, x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I    X    P(X)'
  write ( *, '(a)' ) ''

  do i = 1, n
    p = r8poly_value_horner ( m, c, x(i) )
    write ( *, '(2x,i2,2x,f8.4,2x,g14.6)' ) i, x(i), p
  end do

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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_LINSPACE_TEST'
  write ( *, '(a)' ) '  For a R8VEC:'
  write ( *, '(a)' ) '  R8VEC_LINSPACE: evenly spaced points between A and B;'

  a = 10.0D+00
  b = 20.0D+00

  call r8vec_linspace ( n, a, b, x )
  call r8vec_print ( n, x, '  r8vec_linspace ( 5, 10, 20 )' )

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
subroutine truncated_normal_a_cdf_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_CDF_TEST tests TRUNCATED_NORMAL_A_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf1
  real ( kind = 8 ) cdf2
  real ( kind = 8 ) mu
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_CDF_TEST:'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_A_CDF evaluates '
  write ( *, '(a)' ) '  the CDF of the lower Truncated Normal Distribution.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '      MU         S         A         X' // &
    '               CDF1                      CDF2'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call truncated_normal_a_cdf_values ( n_data, mu, sigma, a, x, cdf1 )

    if ( n_data == 0 ) then
      exit
    end if

    call truncated_normal_a_cdf ( x, mu, sigma, a, cdf2 )

    write ( *, &
      '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,g24.16,2x,g24.16)' ) &
      mu, sigma, a, x, cdf1, cdf2

  end do

  return
end
subroutine truncated_normal_a_cdf_inv_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_CDF_INV_TEST tests TRUNCATED_NORMAL_A_CDF_INV.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_CDF_INV_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_A_CDF_INV inverts '
  write ( *, '(a)' ) '  the lower Truncated Normal CDF;'

  a = 50.0D+00
  mu = 100.0D+00
  sigma = 15.0D+00
  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
  write ( *, '(a,i12)' ) '  SEED = ', seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X            CDF           CDF_INV'
  write ( *, '(a)' ) ' '

  do i = 1, 10
    call truncated_normal_a_sample ( mu, sigma, a, seed, x )
    call normal_ms_cdf ( x, mu, sigma, cdf )
    call normal_ms_cdf_inv ( cdf, mu, sigma, x2 )
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, cdf, x2
  end do

  return
end
subroutine truncated_normal_a_mean_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_MEAN_TEST tests TRUNCATED_NORMAL_A_MEAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 March 2015
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
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_MEAN_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_A_MEAN computes the mean'
  write ( *, '(a)' ) '  for tne Lower Truncated Normal PDF'

  a = 50.0D+00
  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  call truncated_normal_a_mean ( mu, sigma, a, mean )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean

  seed = 123456789
  do i = 1, sample_num
    call truncated_normal_a_sample ( mu, sigma, a, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine truncated_normal_a_moment_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_MOMENT_TEST tests TRUNCATED_NORMAL_A_MOMENT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 September 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), dimension ( 6 ) :: a_test = (/ &
    0.0D+00, -10.0D+00, 10.0D+00, -10.0D+00, +10.0D+00, -10.0D+00 /)
  real ( kind = 8 ) moment
  real ( kind = 8 ) mu
  real ( kind = 8 ), dimension ( 6 ) :: mu_test = (/ &
    0.0D+00,  0.0D+00,  0.0D+00,  0.0D+00, 0.0D+00, -5.0D+00 /)
  integer ( kind = 4 ) order
  real ( kind = 8 ) sigma
  real ( kind = 8 ), dimension ( 6 ) :: sigma_test = (/ &
    1.0D+00,  1.0D+00,  1.0D+00, 2.0D+00, 2.0D+00,  1.0D+00 /)
  integer ( kind = 4 ) test
  integer ( kind = 4 ) test_num

  test_num = 6
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_MOMENT_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_A_MOMENT evaluates the moments.'
  write ( *, '(a)' ) '  of the Lower Truncated Normal PDF:'

  do test = 1, test_num
    mu = mu_test(test)
    sigma = sigma_test(test)
    a = a_test(test)
    write ( *, '(a)' ) ''
    write ( *, '(a,i2,a,g14.6,a,g14.6,a,g14.6)' ) &
      '  Test = ', test, ' Mu = ', mu, ' Sigma = ', sigma, ' A = ', a
    write ( *, '(a)' ) ' Order  Moment'
    write ( *, '(a)' ) ''
    do order = 0, 8
      call truncated_normal_a_moment ( order, mu, sigma, a, moment )
      write ( *, '(2x,i2,2x,g14.6)' ) order, moment
    end do
  end do

  return
end
subroutine truncated_normal_a_pdf_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_PDF tests TRUNCATED_NORMAL_A_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) mu
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) pdf1
  real ( kind = 8 ) pdf2
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_PDF_TEST:'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_A_PDF evaluates '
  write ( *, '(a)' ) '  the PDF of the lower Truncated Normal Distribution.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '      MU         S         A         X' // &
    '               PDF1                      PDF2'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call truncated_normal_a_pdf_values ( n_data, mu, sigma, a, x, pdf1 )

    if ( n_data == 0 ) then
      exit
    end if

    call truncated_normal_a_pdf ( x, mu, sigma, a, pdf2 )

    write ( *, &
      '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,g24.16,2x,g24.16)' ) &
      mu, sigma, a, x, pdf1, pdf2

  end do

  return
end
subroutine truncated_normal_a_sample_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_SAMPLE_TEST tests TRUNCATED_NORMAL_A_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_SAMPLE_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_A_SAMPLE samples the '
  write ( *, '(a)' ) '  lower Truncated Normal PDF.'
  write ( *, '(a)' ) ''

  a = 50.0D+00
  mu = 100.0D+00
  sigma = 25.0D+00
  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Lower limit A =    ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
  write ( *, '(a,i12)' ) '  SEED = ', seed
  write ( *, '(a)' ) ''

  do i = 1, 10
    call truncated_normal_a_sample ( mu, sigma, a, seed, x )
    write ( *, '(2x,i4,2x,g14.6)' ) i, x
  end do

  return
end
subroutine truncated_normal_a_variance_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_A_VARIANCE_TEST tests TRUNCATED_NORMAL_A_VARIANCE.
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

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_VARIANCE_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_A_VARIANCE returns the variance'
  write ( *, '(a)' ) '  of the lower Truncated Normal distribution.'

  a = 50.0D+00
  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Lower limit A =    ', a
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  call truncated_normal_a_variance ( mu, sigma, a, variance )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  seed = 123456789
  do i = 1, sample_num
    call truncated_normal_a_sample ( mu, sigma, a, seed, x(i) )
  end do

  call r8vec_variance ( sample_num, x, variance )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance

  return
end
subroutine truncated_normal_ab_cdf_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_CDF_TEST tests TRUNCATED_NORMAL_AB_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 September 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf1
  real ( kind = 8 ) cdf2
  real ( kind = 8 ) mu
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_CDF_TEST:'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_AB_CDF evaluates'
  write ( *, '(a)' ) '  the CDF of the Truncated Normal Distribution.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '      MU         S         A         B        X' // &
    '               CDF1                      CDF2'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call truncated_normal_ab_cdf_values ( n_data, mu, sigma, a, b, x, cdf1 )

    if ( n_data == 0 ) then
      exit
    end if

    call truncated_normal_ab_cdf ( x, mu, sigma, a, b, cdf2 )

    write ( *, &
      '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,g14.6,2x,g24.16,2x,g24.16)' ) &
      mu, sigma, a, b, x, cdf1, cdf2

  end do

  return
end
subroutine truncated_normal_ab_cdf_inv_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_CDF_INV_TEST tests TRUNCATED_NORMAL_AB_CDF_INV.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_CDF_INV_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_AB_CDF_INV inverts '
  write ( *, '(a)' ) '  the Truncated Normal CDF;'

  a = 50.0D+00
  b = 150.0D+00
  mu = 100.0D+00
  sigma = 15.0D+00
  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
  write ( *, '(a,g14.6)' ) '  Upper limite B = ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
  write ( *, '(a,i12)' ) '  SEED = ', seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X            CDF           CDF_INV'
  write ( *, '(a)' ) ' '

  do i = 1, 10
    call truncated_normal_ab_sample ( mu, sigma, a, b, seed, x )
    call normal_ms_cdf ( x, mu, sigma, cdf )
    call normal_ms_cdf_inv ( cdf, mu, sigma, x2 )
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, cdf, x2
  end do

  return
end
subroutine truncated_normal_ab_mean_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_MEAN_TEST tests TRUNCATED_NORMAL_AB_MEAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 March 2015
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
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MEAN_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_AB_MEAN computes the mean'
  write ( *, '(a)' ) '  for the Truncated Normal PDF'

  a = 50.0D+00
  b = 150.0D+00
  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
  write ( *, '(a,g14.6)' ) '  Upper limit B = ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  call truncated_normal_ab_mean ( mu, sigma, a, b, mean )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean

  seed = 123456789
  do i = 1, sample_num
    call truncated_normal_ab_sample ( mu, sigma, a, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine truncated_normal_ab_moment_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_MOMENT_TEST tests TRUNCATED_NORMAL_AB_MOMENT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 September 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), dimension ( 9 ) :: a_test = (/ &
    -1.0D+00, 0.0D+00, -1.0D+00, -1.0D+00,  0.0D+00, &
     0.5D+00, -2.0D+00, -4.0D+00, 4.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), dimension ( 9 ) :: b_test = (/ &
    1.0D+00, 1.0D+00,  0.0D+00,  1.0D+00,  2.0D+00, &
    2.0D+00,  2.0D+00,  4.0D+00, 7.0D+00 /)
  real ( kind = 8 ) moment
  real ( kind = 8 ) mu
  real ( kind = 8 ), dimension ( 9 ) :: mu_test = (/ &
    0.0D+00, 0.0D+00,  0.0D+00,  0.0D+00,  1.0D+00, &
    0.0D+00,  0.0D+00,  0.0D+00, 5.0D+00 /)
  integer ( kind = 4 ) order
  real ( kind = 8 ) sigma
  real ( kind = 8 ), dimension ( 9 ) :: sigma_test = (/ &
    1.0D+00, 1.0D+00,  1.0D+00,  2.0D+00,  1.0D+00, &
    1.0D+00,  1.0D+00,  1.0D+00, 0.5D+00 /)
  integer ( kind = 4 ) test
  integer ( kind = 4 ) test_num

  test_num = 9
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT_TEST:'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_AB_MOMENT evaluates the moments'
  write ( *, '(a)' ) '  of the Truncated Normal PDF:'

  do test = 1, test_num
    mu = mu_test(test)
    sigma = sigma_test(test)
    a = a_test(test)
    b = b_test(test)
    write ( *, '(a)' ) ''
    write ( *, '(a,i2,a,g14.6,a,g14.6,a,g14.6,a,g14.6)' ) &
      '  Test = ', test, ' Mu = ', mu, ' Sigma = ', sigma, &
      ' A = ', a, ' B = ', b
    write ( *, '(a)' ) ' Order  Moment'
    write ( *, '(a)' ) ''
    do order = 0, 8
      call truncated_normal_ab_moment ( order, mu, sigma, a, b, moment )
      write ( *, '(2x,i2,2x,g14.6)' ) order, moment
    end do
  end do

  return
end
subroutine truncated_normal_ab_pdf_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_PDF tests TRUNCATED_NORMAL_AB_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 September 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mu
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) pdf1
  real ( kind = 8 ) pdf2
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_PDF_TEST:'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_AB_PDF evaluates '
  write ( *, '(a)' ) '  the PDF of the Truncated Normal Distribution.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '      MU         S         A         B        X' // &
    '               PDF1                      PDF2'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call truncated_normal_ab_pdf_values ( n_data, mu, sigma, a, b, x, pdf1 )

    if ( n_data == 0 ) then
      exit
    end if

    call truncated_normal_ab_pdf ( x, mu, sigma, a, b, pdf2 )

    write ( *, &
      '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,g14.6,2x,g24.16,2x,g24.16)' ) &
      mu, sigma, a, b, x, pdf1, pdf2

  end do

  return
end
subroutine truncated_normal_ab_sample_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_SAMPLE_TEST tests TRUNCATED_NORMAL_AB_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_SAMPLE_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_AB_SAMPLE samples the '
  write ( *, '(a)' ) '  Truncated Normal PDF.'
  write ( *, '(a)' ) ''

  a = 50.0D+00
  b = 150.0D+00
  mu = 100.0D+00
  sigma = 25.0D+00
  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Lower limit A =    ', a
  write ( *, '(a,g14.6)' ) '  Upper limit B =    ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
  write ( *, '(a,i12)' ) '  SEED = ', seed
  write ( *, '(a)' ) ''

  do i = 1, 10
    call truncated_normal_ab_sample ( mu, sigma, a, b, seed, x )
    write ( *, '(2x,i4,2x,g14.6)' ) i, x
  end do

  return
end
subroutine truncated_normal_ab_variance_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_AB_VARIANCE_TEST tests TRUNCATED_NORMAL_AB_VARIANCE.
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

  integer ( kind = 4 ), parameter :: sample_num = 1000

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_VARIANCE_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_AB_VARIANCE returns the variance'
  write ( *, '(a)' ) '  of the Truncated Normal distribution.'

  a = 50.0D+00
  b = 150.0D+00
  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
  write ( *, '(a,g14.6)' ) '  Upper limit B = ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  call truncated_normal_ab_variance ( mu, sigma, a, b, variance )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  seed = 123456789
  do i = 1, sample_num
    call truncated_normal_ab_sample ( mu, sigma, a, b, seed, x(i) )
  end do

  call r8vec_variance ( sample_num, x, variance )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance

  return
end
subroutine truncated_normal_b_cdf_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_CDF_TEST tests TRUNCATED_NORMAL_B_CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) cdf1
  real ( kind = 8 ) cdf2
  real ( kind = 8 ) mu
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMA_B_CDF_TEST:'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_B_CDF evaluates '
  write ( *, '(a)' ) '  the CDF of the upper Truncated Normal Distribution.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '      MU         S         B        X' // &
    '               CDF1                      CDF2'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call truncated_normal_b_cdf_values ( n_data, mu, sigma, b, x, cdf1 )

    if ( n_data == 0 ) then
      exit
    end if

    call truncated_normal_b_cdf ( x, mu, sigma, b, cdf2 )

    write ( *, &
      '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,g24.16,2x,g24.16)' ) &
      mu, sigma, b, x, cdf1, cdf2

  end do

  return
end
subroutine truncated_normal_b_cdf_inv_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_CDF_INV_TEST tests TRUNCATED_NORMAL_B_CDF_INV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 March 2015
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
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_CDF_INV_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_B_CDF_INV inverts '
  write ( *, '(a)' ) '  the Truncated Normal CDF;'

  b = 150.0D+00
  mu = 100.0D+00
  sigma = 15.0D+00
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Upper limite B = ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
  write ( *, '(a,i12)' ) '  SEED = ', seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X            CDF           CDF_INV'
  write ( *, '(a)' ) ' '

  do i = 1, 10
    call truncated_normal_b_sample ( mu, sigma, b, seed, x )
    call normal_ms_cdf ( x, mu, sigma, cdf )
    call normal_ms_cdf_inv ( cdf, mu, sigma, x2 )
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, cdf, x2
  end do

  return
end
subroutine truncated_normal_b_mean_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_MEAN_TEST tests TRUNCATED_NORMAL_B_MEAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 March 2015
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
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_MEAN_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_B_MEAN computes the mean'
  write ( *, '(a)' ) '  for the Upper Truncated Normal PDF'

  b = 150.0D+00
  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Upper limit B = ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  call truncated_normal_b_mean ( mu, sigma, b, mean )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean

  seed = 123456789
  do i = 1, sample_num
    call truncated_normal_b_sample ( mu, sigma, b, seed, x(i) )
  end do

  call r8vec_mean ( sample_num, x, mean )
  call r8vec_max ( sample_num, x, xmax )
  call r8vec_min ( sample_num, x, xmin )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end
subroutine truncated_normal_b_moment_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_MOMENT_TEST tests TRUNCATED_NORMAL_B_MOMENT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 September 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ), dimension ( 6 ) :: b_test = (/ &
    0.0D+00, 10.0D+00, -10.0D+00, 10.0D+00, -10.0D+00, 10.0D+00 /)
  real ( kind = 8 ) moment
  real ( kind = 8 ) mu
  real ( kind = 8 ), dimension ( 6 ) :: mu_test = (/ &
    0.0D+00,  0.0D+00,  0.0D+00,  0.0D+00, 0.0D+00, 5.0D+00 /)
  integer ( kind = 4 ) order
  real ( kind = 8 ) sigma
  real ( kind = 8 ), dimension ( 6 ) :: sigma_test = (/ &
    1.0D+00,  1.0D+00,  1.0D+00, 2.0D+00, 2.0D+00,  1.0D+00 /)
  integer ( kind = 4 ) test
  integer ( kind = 4 ) test_num

  test_num = 6
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_MOMENT_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_B_MOMENT evaluates the moments'
  write ( *, '(a)' ) '  for the Upper Truncated Normal PDF:'

  do test = 1, test_num
    mu = mu_test(test)
    sigma = sigma_test(test)
    b = b_test(test)
    write ( *, '(a)' ) ''
    write ( *, '(a,i2,a,g14.6,a,g14.6,a,g14.6)' ) &
      '  Test = ', test, ' Mu = ', mu, ' Sigma = ', sigma, ' B = ', b
    write ( *, '(a)' ) ' Order  Moment'
    write ( *, '(a)' ) ''
    do order = 0, 8
      call truncated_normal_b_moment ( order, mu, sigma, b, moment )
      write ( *, '(2x,i2,2x,g14.6)' ) order, moment
    end do
  end do

  return
end
subroutine truncated_normal_b_pdf_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_PDF_TEST tests TRUNCATED_NORMAL_B_PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) mu
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) pdf1
  real ( kind = 8 ) pdf2
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_PDF_TEST:'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_B_PDF evaluates the PDF'
  write ( *, '(a)' ) '  of the upper Truncated Normal Distribution.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '      MU         S         B        X' // &
    '               PDF1                      PDF2'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call truncated_normal_b_pdf_values ( n_data, mu, sigma, b, x, pdf1 )

    if ( n_data == 0 ) then
      exit
    end if

    call truncated_normal_b_pdf ( x, mu, sigma, b, pdf2 )

    write ( *, &
      '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,g24.16,2x,g24.16)' ) &
      mu, sigma, b, x, pdf1, pdf2

  end do

  return
end
subroutine truncated_normal_b_sample_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_SAMPLE_TEST tests TRUNCATED_NORMAL_B_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_SAMPLE_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_B_SAMPLE samples the '
  write ( *, '(a)' ) '  lower Truncated Normal PDF.'
  write ( *, '(a)' ) ''

  b = 150.0D+00
  mu = 100.0D+00
  sigma = 25.0D+00
  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Upper limit B =    ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
  write ( *, '(a,i12)' ) '  SEED = ', seed
  write ( *, '(a)' ) ''

  do i = 1, 10
    call truncated_normal_b_sample ( mu, sigma, b, seed, x )
    write ( *, '(2x,i4,2x,g14.6)' ) i, x
  end do

  return
end
subroutine truncated_normal_b_variance_test ( )

!*****************************************************************************80
!
!! TRUNCATED_NORMAL_B_VARIANCE_TEST tests TRUNCATED_NORMAL_B_VARIANCE.
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

  integer ( kind = 4 ), parameter :: sample_num = 1000


  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sigma
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_VARIANCE_TEST'
  write ( *, '(a)' ) '  TRUNCATED_NORMAL_B_VARIANCE returns the variance'
  write ( *, '(a)' ) '  of the upper Truncated Normal distribution.'

  b = 150.0D+00
  mu = 100.0D+00
  sigma = 15.0D+00

  write ( *, '(a)' ) ' '

  write ( *, '(a,g14.6)' ) '  Upper limit B = ', b
  write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
  write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

  call truncated_normal_b_variance ( mu, sigma, b, variance )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

  seed = 123456789
  do i = 1, sample_num
    call truncated_normal_b_sample ( mu, sigma, b, seed, x(i) )
  end do

  call r8vec_variance ( sample_num, x, variance )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance

  return
end
