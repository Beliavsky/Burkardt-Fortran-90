program main

!*****************************************************************************80
!
!! MAIN is the main program for FN_TEST.
!
!  Discussion:
!
!    FN_TEST tests the FN library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FN_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( * ,'(a)' ) '  Test the FN library.'

  call i4_mach_test ( )
  call r8_acos_test ( )
  call r8_acosh_test ( )
  call r8_ai_test ( )
  call r8_aid_test ( )
  call r8_aint_test ( )
  call r8_asin_test ( )
  call r8_asinh_test ( )
  call r8_atan_test ( )
  call r8_atan2_test ( )
  call r8_atanh_test ( )
  call r8_besi0_test ( )
  call r8_besi1_test ( )
  call r8_besj0_test ( )
  call r8_besj1_test ( )
  call r8_besk_test ( )
  call r8_besk0_test ( )
  call r8_besk1_test ( )
  call r8_besy0_test ( )
  call r8_besy1_test ( )
  call r8_beta_test ( )
  call r8_betai_test ( )
  call r8_bi_test ( )
  call r8_bid_test ( )
  call r8_binom_test ( )
  call r8_cbrt_test ( )
  call r8_chi_test ( )
  call r8_chu_test ( )
  call r8_ci_test ( )
  call r8_cin_test ( )
  call r8_cinh_test ( )
  call r8_cos_test ( )
  call r8_cos_deg_test ( )
  call r8_cosh_test ( )
  call r8_cot_test ( )
  call r8_csevl_test ( )
  call r8_dawson_test ( )
  call r8_e1_test ( )
  call r8_ei_test ( )
  call r8_erf_test ( )
  call r8_erfc_test ( )
  call r8_exp_test ( )
  call r8_fac_test ( )
  call r8_gamic_test ( )
  call r8_gamit_test ( )
  call r8_gaml_test ( )
  call r8_gamma_test ( )
  call r8_gamr_test ( )
  call r8_inits_test ( )
  call r8_int_test ( )
  call r8_lbeta_test ( )
  call r8_lgams_test ( )
  call r8_lgmc_test ( )
  call r8_li_test ( )
  call r8_lngam_test ( )
  call r8_lnrel_test ( )
  call r8_log_test ( )
  call r8_log10_test ( )
  call r8_mach_test ( )
  call r8_pak_test ( )
  call r8_poch_test ( )
  call r8_psi_test ( )
  call r8_rand_test ( )
  call r8_randgs_test ( )
  call r8_random_test ( )
  call r8_ren_test ( )
  call r8_shi_test ( )
  call r8_si_test ( )
  call r8_sin_test ( )
  call r8_sin_deg_test ( )
  call r8_sinh_test ( )
  call r8_spence_test ( )
  call r8_sqrt_test ( )
  call r8_tan_test ( )
  call r8_tanh_test ( )
  call r8_upak_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FN_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine i4_mach_test ( )

!*****************************************************************************80
!
!! I4_MACH_TEST tests I4_MACH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_mach

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_MACH_TEST:'
  write ( *, '(a)' ) '  I4_MACH evaluates integer machine numbers.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I4_MACH(1) = the standard input unit.'
  write ( *, '(a)' ) '  I4_MACH(2) = the standard output unit.'
  write ( *, '(a)' ) '  I4_MACH(3) = the standard punch unit.'
  write ( *, '(a)' ) '  I4_MACH(4) = the standard error message unit.'
  write ( *, '(a)' ) '  I4_MACH(5) = the number of bits per integer storage unit.'
  write ( *, '(a)' ) '  I4_MACH(6) = the number of characters per integer storage unit.'
  write ( *, '(a)' ) '  I4_MACH(7) = A, the base.'
  write ( *, '(a)' ) '  I4_MACH(8) = S, the number of base A digits.'
  write ( *, '(a)' ) '  I4_MACH(9) = A^S-1, the largest integer.'
  write ( *, '(a)' ) '  I4_MACH(10) = B, the base.'
  write ( *, '(a)' ) '  I4_MACH(11) = T, the number of single precision base B digits.'
  write ( *, '(a)' ) '  I4_MACH(12) = EMIN, the smallest single precision exponent E.'
  write ( *, '(a)' ) '  I4_MACH(13) = EMAX, the largest single precision exponent E.'
  write ( *, '(a)' ) '  I4_MACH(14) = T, the number of double precision base B digits.'
  write ( *, '(a)' ) '  I4_MACH(15) = EMIN, the smallest double precision exponent E.'
  write ( *, '(a)' ) '  I4_MACH(16) = EMAX, the largest double precision exponent E.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I     I4_MACH(I)'
  write ( *, '(a)' ) ''

  do i = 1, 16
    write ( *, '(2x,i4,2x,i12)' ) i, i4_mach ( i )
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
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_acos
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ACOS_TEST:'
  write ( *, '(a)' ) '  R8_ACOS evaluates the arc cosine.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X      ARCCOS(X)    R8_ACOS(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call arccos_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_acos ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

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
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_acosh
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ACOSH_TEST:'
  write ( *, '(a)' ) '  R8_ACOSH evaluates the inverse hyperbolic cosine.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X      ARCCOSH(X)  R8_ACOSH(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call arccosh_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_acosh ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_ai_test ( )

!*****************************************************************************80
!
!! R8_AI_TEST tests R8_AI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_ai
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_AI_TEST:'
  write ( *, '(a)' ) '  R8_AI evaluates the Airy function Ai'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X      AIRY_AI(X)  R8_AI(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call airy_ai_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_ai ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_aid_test ( )

!*****************************************************************************80
!
!! R8_AID_TEST tests R8_AID.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_aid
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_AID_TEST:'
  write ( *, '(a)' ) '  R8_AID evaluates the derivative of the Airy function Ai'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X     AIRY_AID(X)  R8_AID(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call airy_ai_prime_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_aid ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_aint_test ( )

!*****************************************************************************80
!
!! R8_AINT_TEST tests R8_AINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_aint
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_AINT_TEST:'
  write ( *, '(a)' ) '  R8_AINT evaluates the aint(X) function (round towards 0)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         AINT(X)  R8_AINT(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call int_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_aint ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' )  &
      x, fx1, fx2, abs ( fx1 - fx2 )

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
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_asin
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ASIN_TEST:'
  write ( *, '(a)' ) '  R8_ASIN evaluates the inverse sine.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X      ARCSIN(X)  R8_ASIN(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call arcsin_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_asin ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

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
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_asinh
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ASINH_TEST:'
  write ( *, '(a)' ) '  R8_ASINH evaluates the inverse hyperbolic sine function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X     ARCSINH(X)  R8_ASINH(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call arcsinh_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_asinh ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

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
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_atan
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ATAN_TEST:'
  write ( *, '(a)' ) '  R8_ATAN evaluates the arc tangent function'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X      ARCTAN(X)  R8_ATAN(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call arctan_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_atan ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_atan2_test ( )

!*****************************************************************************80
!
!! R8_ATAN2_TEST tests R8_ATAN2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_atan2
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ATAN2_TEST:'
  write ( *, '(a)' ) '  R8_ATAN2 evaluates the arc tangent function'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X               Y   ARCTAN2(Y,X)  R8_ATAN2(Y,X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call arctan2_values ( n_data, x, y, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_atan2 ( y, x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, y, fx1, fx2, abs ( fx1 - fx2 )

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
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_atanh
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ATANH_TEST:'
  write ( *, '(a)' ) '  R8_ATANH evaluates the arc hyperbolic tangent function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X     ARCTANH(X)  R8_ATANH(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call arctanh_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_atanh ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_besi0_test ( )

!*****************************************************************************80
!
!! R8_BESI0_TEST tests R8_BESI0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_besi0
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BESI0_TEST:'
  write ( *, '(a)' ) '  R8_BESI0 evaluates the Bessel I0(x) function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X       BESI0(X)  R8_BESI0(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bessel_i0_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_besi0 ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_besi1_test ( )

!*****************************************************************************80
!
!! R8_BESI1_TEST tests R8_BESI1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_besi1
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BESI1_TEST:'
  write ( *, '(a)' ) '  R8_BESI1 evaluates the Bessel I1(x) function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X       BESI1(X) R8_BESI1(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bessel_i1_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_besi1 ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_besj0_test ( )

!*****************************************************************************80
!
!! R8_BESJ0_TEST tests R8_BESJ0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_besj0
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BESJ0_TEST:'
  write ( *, '(a)' ) '  R8_BESJ0 evaluates the Bessel function J0(X).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X       BESJ0(X)  R8_BESJ0(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bessel_j0_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_besj0 ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_besj1_test ( )

!*****************************************************************************80
!
!! R8_BESJ1_TEST tests R8_BESJ1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_besj1
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BESJ1_TEST:'
  write ( *, '(a)' ) '  R8_BESJ1 evaluates the Bessel function J1(x)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X       BESJ1(X)  R8_BESJ1(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bessel_j1_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_besj1 ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_besk_test ( )

!*****************************************************************************80
!
!! R8_BESK_TEST R8_BESK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) nu
  real ( kind = 8 ) r8_besk
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BESK_TEST:'
  write ( *, '(a)' ) '  R8_BESK evaluates Bessel functions K(Nu,X)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '              NU             X       BESK(X)  R8_BESK(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bessel_kx_values ( n_data, nu, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_besk ( nu, x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      nu, x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_besk0_test ( )

!*****************************************************************************80
!
!! R8_BESK0_TEST tests R8_BESK0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_besk0
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BESK0_TEST:'
  write ( *, '(a)' ) '  R8_BESK0 evaluates Bessel functions K0(X)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X       BESK0(X)  R8_BESK0(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bessel_k0_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_besk0 ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_besk1_test ( )

!*****************************************************************************80
!
!! R8_BESK1_TEST tests R8_BESK1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_besk1
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BESK1_TEST:'
  write ( *, '(a)' ) '  R8_BESK1 evaluates Bessel functions K1(X)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X       BESK1(X)  R8_BESK1(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bessel_k1_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_besk1 ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_besy0_test ( )

!*****************************************************************************80
!
!! R8_BESY0_TEST tests R8_BESY0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_besy0
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BESY0_TEST:'
  write ( *, '(a)' ) '  R8_BESY0 evaluates Bessel functions Y0(X)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X       BESY0(X)  R8_BESY0(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bessel_y0_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_besy0 ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_besy1_test ( )

!*****************************************************************************80
!
!! R8_BESY1_TEST tests R8_BESY1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_besy1
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BESY1_TEST:'
  write ( *, '(a)' ) '  R8_BESY1 evaluates Bessel functions Y1(X)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X       BESY1(X)  R8_BESY1(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call bessel_y1_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_besy1 ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

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
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_beta

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BETA_TEST:'
  write ( *, '(a)' ) '  R8_BETA evaluates the Beta function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             A               B      BETA(A,B)  R8_BETA(A,B)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call beta_values ( n_data, a, b, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_beta ( a, b )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      a, b, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_betai_test ( )

!*****************************************************************************80
!
!! R8_BETAI_TEST tests R8_BETAI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_betai
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BETAI_TEST:'
  write ( *, '(a)' ) '  R8_BETAI evaluates the incomplete Beta function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X               A               B     ' // &
    'BETAI(A,B)  R8_BETAI(A,B)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call beta_inc_values ( n_data, a, b, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_betai ( x, a, b )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, a, b, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_bi_test ( )

!*****************************************************************************80
!
!! R8_BI_TEST tests R8_BI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_bi
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BI_TEST:'
  write ( *, '(a)' ) '  R8_BI evaluates the Airy function Bi(X)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X      AIRY_BI(X)  R8_BI(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call airy_bi_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_bi ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_bid_test ( )

!*****************************************************************************80
!
!! R8_BID_TEST tests R8_BID.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_bid
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BID_TEST:'
  write ( *, '(a)' ) '  R8_BID evaluates the derivative of the Airy function Bi(X)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X     AIRY_BID(X)  R8_BID(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call airy_bi_prime_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_bid ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_binom_test ( )

!*****************************************************************************80
!
!! R8_BINOM_TEST tests R8_BINOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  real ( kind = 8 ) diff
  integer ( kind = 4 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_binom

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_BINOM_TEST:'
  write ( *, '(a)' ) '  R8_BINOM evaluates the binomial coefficient.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             A               B     BINOM(A,B)  R8_BINOM(A,B)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call binomial_values ( n_data, a, b, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_binom ( a, b )

    write ( *, '(2x,i14,2x,i14,2x,i14,2x,g14.6,2x,g14.6)' ) &
      a, b, fx1, fx2, abs ( real ( fx1 ) - fx2 )

  end do

  return
end
subroutine r8_cbrt_test ( )

!*****************************************************************************80
!
!! R8_CBRT_TEST tests R8_CBRT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_cbrt
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CBRT_TEST:'
  write ( *, '(a)' ) '  R8_CBRT computes the cube root'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X        CBRT(X)  R8_CBRT(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call cbrt_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_cbrt ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_chu_test ( )

!*****************************************************************************80
!
!! R8_CHU_TEST tests R8_CHU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_chu
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CHU_TEST:'
  write ( *, '(a)' ) '  R8_CHU evaluates the hypergeometric U function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             A               B               X     CHU(A,B,X)' // &
    '         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call hypergeometric_u_values ( n_data, a, b, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_chu ( a, b, x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      a, b, x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_chi_test ( )

!*****************************************************************************80
!
!! R8_CHI_TEST tests R8_CHI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_chi
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CHI_TEST:'
  write ( *, '(a)' ) '  R8_CHI evaluates the hyperbolic cosine integral.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X          CHI(X)  R8_CHI(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call chi_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_chi ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_ci_test ( )

!*****************************************************************************80
!
!! R8_CI_TEST tests R8_CI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_ci
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CI_TEST:'
  write ( *, '(a)' ) '  R8_CI evaluates the cosine integral.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X           CI(X)  R8_CI(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call ci_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_ci ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_cin_test ( )

!*****************************************************************************80
!
!! R8_CIN_TEST tests R8_CIN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_cin
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CIN_TEST:'
  write ( *, '(a)' ) '  R8_CIN evaluates the alternate cosine integral.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X          CIN(X)'
  write ( *, '(a)' ) '                     R4_CIN(X)        Diff'
  write ( *, '(a)' ) '                     R8_CIN(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call cin_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_cin ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_cinh_test ( )

!*****************************************************************************80
!
!! R8_CINH_TEST tests R8_CINH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_cinh
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CINH_TEST:'
  write ( *, '(a)' ) '  R8_CINH evaluates the alternate hyperbolic cosine integral'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         CINH(X)  R8_CINH(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call cinh_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_cinh ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_cos_test ( )

!*****************************************************************************80
!
!! R8_COS_TEST tests R8_COS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_cos
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_COS_TEST:'
  write ( *, '(a)' ) '  R8_COS evaluates the cosine function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         COS(X)  R8_COS(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call cos_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_cos ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_cos_deg_test ( )

!*****************************************************************************80
!
!! R8_COS_DEG_TEST tests R8_COS_DEG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_cos_deg
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_COS_DEG_TEST:'
  write ( *, '(a)' ) '  R8_COS_DEG evaluates the cosine for degree arguments.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X     COS_DEG(X)'
  write ( *, '(a)' ) '                R4_COS_DEG(X)         Diff'
  write ( *, '(a)' ) '                R8_COS_DEG(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call cos_degree_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_cos_deg ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_cosh_test ( )

!*****************************************************************************80
!
!! R8_COSH_TEST tests R8_COSH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_cosh
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_COSH_TEST:'
  write ( *, '(a)' ) '  R8_COSH evaluates the hyperbolic cosine function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         COSH(X)  R8_COSH(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call cosh_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_cosh ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

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
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_cot
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_COT_TEST:'
  write ( *, '(a)' ) '  R8_COT evaluates the cotangent function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         COT(X)  R8_COT(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call cot_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_cot ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_csevl_test ( )

!*****************************************************************************80
!
!! R8_CSEVL_TEST tests R8_CSEVL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) err
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_csevl
  real ( kind = 8 ), parameter :: r8_pi =  3.141592653589793238462643383279503D+00
  real ( kind = 8 ), save :: expcs(15) = (/ &
   2.532131755504016D+00, &
   1.130318207984970D+00, &
   0.271495339534077D+00, &
   0.044336849848664D+00, &
   0.005474240442094D+00, &
   0.000542926311914D+00, &
   0.000044977322954D+00, &
   0.000003198436462D+00, &
   0.000000199212481D+00, &
   0.000000011036772D+00, &
   0.000000000550590D+00, &
   0.000000000024980D+00, &
   0.000000000001039D+00, &
   0.000000000000040D+00, &
   0.000000000000001D+00 /)
  real ( kind = 8 ) s
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_CSEVL_TEST:'
  write ( *, '(a)' ) '  R8_CSEVL evaluates a Chebyshev approximant'
  write ( *, '(a)' ) '  of N terms at a point X.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Here we use an approximant to the exponential function.'
  write ( *, '(a)' ) '  and average the absolute error at 21 points.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N    error'
  write ( *, '(a)' ) ''

  do n = 1, 12
    err = 0.0D+00
    do i = -10, 10
      x = real ( i, kind = 8 ) / 10.0D+00
      s = r8_csevl ( x, expcs, n )
      err = err + abs ( s - exp ( x ) )
    end do
    err = err / 21.0D+00
    write ( *, '(2x,i2,2x,g14.6)' ) n, err
  end do

  return
end
subroutine r8_dawson_test ( )

!*****************************************************************************80
!
!! R8_DAWSON_TEST tests R8_DAWSON.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_dawson
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_DAWSON_TEST:'
  write ( *, '(a)' ) '  R8_DAWSON evaluates Dawson''s integral.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X      DAWSON(X)  R8_DAWSON(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call dawson_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_dawson ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_e1_test ( )

!*****************************************************************************80
!
!! R8_E1_TEST tests R8_E1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_e1
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_E1_TEST:'
  write ( *, '(a)' ) '  R8_E1 evaluates the exponential integral E1(x).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X           E1(X)  R8_E1(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call e1_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_e1 ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_ei_test ( )

!*****************************************************************************80
!
!! R8_EI_TEST tests R8_EI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_ei
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_EI_TEST:'
  write ( *, '(a)' ) '  R8_EI evaluates the exponential integral Ei(X).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X           EI(X)  R8_EI(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call ei_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_ei ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_erf_test ( )

!*****************************************************************************80
!
!! R8_ERF_TEST tests R8_ERF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_erf
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ERF_TEST:'
  write ( *, '(a)' ) '  R8_ERF evaluates the error function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X          ERF(X)          R8_ERF(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call erf_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_erf ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_erfc_test ( )

!*****************************************************************************80
!
!! R8_ERFC_TEST tests R8_ERFC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_erfc
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_ERFC_TEST:'
  write ( *, '(a)' ) '  R8_ERFC evaluates the complementary error function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         ERFC(X)  R8_ERFC(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call erfc_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_erfc ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

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
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_exp
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_EXP_TEST:'
  write ( *, '(a)' ) '  R8_EXP evaluates the exponential function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X          EXP(X)  R8_EXP(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call exp_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_exp ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_fac_test ( )

!*****************************************************************************80
!
!! R8_FAC_TEST tests R8_FAC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_fac

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_FAC_TEST:'
  write ( *, '(a)' ) '  R8_FAC evaluates the factorial.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             N          FAC(N)     R8_FAC(N)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call r8_factorial_values ( n_data, n, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_fac ( n )

    write ( *, '(2x,i14,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      n, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_gamic_test ( )

!*****************************************************************************80
!
!! R8_GAMIC_TEST tests R8_GAMIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_gamic
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_GAMIC_TEST:'
  write ( *, '(a)' ) '  R8_GAMIC evaluates the incomplete Gamma function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             A               X     GAMIC(A,X)  R8_GAMIC(A,X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call gamma_inc_values ( n_data, a, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_gamic ( a, x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      a, x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_gamit_test ( )

!*****************************************************************************80
!
!! R8_GAMIT_TEST tests R8_GAMIT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_gamit
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_GAMIT_TEST:'
  write ( *, '(a)' ) '  R8_GAMIT evaluates Tricomi''s Incomplete Gamma.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             A               X     GAMIT(A,X)  R8_GAMIT(A,X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call gamma_inc_tricomi_values ( n_data, a, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_gamit ( a, x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' )  &
      a, x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_gaml_test ( )

!*****************************************************************************80
!
!! R8_GAML_TEST tests R8_GAML.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_GAML_TEST:'
  write ( *, '(a)' ) '  R8_GAML returns bounds for the argument of the gamma function.'

  call r8_gaml ( xmin, xmax )

  write ( *, '(a)' ) ''
  write ( *,'(a,g14.6)' ) '  Lower limit XMIN = ', xmin
  write ( *,'(a,g14.6)' ) '  Upper limit XMAX = ', xmax

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
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_gamma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_GAMMA_TEST:'
  write ( *, '(a)' ) '  R8_GAMMA evaluates the gamma function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X        GAMMA(X)  R8_GAMMA(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call gamma_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_gamma ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_gamr_test ( )

!*****************************************************************************80
!
!! R8_GAMR_TEST tests R8_GAMR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  real ( kind = 8 ) gx
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_gamr
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_GAMR_TEST:'
  write ( *, '(a)' ) '  R8_GAMR computes 1/Gamma(x).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X      1/GAMMA(X)  R8_GAMR(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call gamma_values ( n_data, x, gx )

    if ( n_data == 0 ) then
      exit
    end if

    fx1 = 1.0D+00 / gx
    fx2 = r8_gamr ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_inits_test ( )

!*****************************************************************************80
!
!! R8_INITS_TEST tests R8_INITS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) r8_inits
  real ( kind = 8 ), parameter :: sincs(15) = (/ &
    -0.374991154955873175839919279977323464D+00, &
    -0.181603155237250201863830316158004754D+00, &
    +0.005804709274598633559427341722857921D+00, &
    -0.000086954311779340757113212316353178D+00, &
    +0.000000754370148088851481006839927030D+00, &
    -0.000000004267129665055961107126829906D+00, &
    +0.000000000016980422945488168181824792D+00, &
    -0.000000000000050120578889961870929524D+00, &
    +0.000000000000000114101026680010675628D+00, &
    -0.000000000000000000206437504424783134D+00, &
    +0.000000000000000000000303969595918706D+00, &
    -0.000000000000000000000000371357734157D+00, &
    +0.000000000000000000000000000382486123D+00, &
    -0.000000000000000000000000000000336623D+00, &
    +0.000000000000000000000000000000000256D+00 /)
  real ( kind = 8 ) tol

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_INITS_TEST:'
  write ( *, '(a)' ) '  R8_INITS determines the Chebyshev interpolant degree'
  write ( *, '(a)' ) '  necessary to guarantee a desired accuracy level.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Here, we use a 15 term Chebyshev expansion for the'
  write ( *, '(a)' ) '  sine function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Accuracy    Terms Needed'
  write ( *, '(a)' ) ''

  tol = 1.0D+00
  do i = 1, 18
    n = r8_inits ( sincs, 15, tol )
    write ( *, '(2x,g14.6,2x,i4)' ) tol, n
    tol = tol / 10.0D+00
  end do

  return
end
subroutine r8_int_test ( )

!*****************************************************************************80
!
!! R8_INT_TEST tests R8_INT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_int
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_INT_TEST:'
  write ( *, '(a)' ) '  R8_INT evaluates the Int(X) function'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         INT(X)  R8_INT(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call int_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_int ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' )  &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_lbeta_test ( )

!*****************************************************************************80
!
!! R8_LBETA_TEST tests R8_LBETA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_lbeta

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_LBETA_TEST:'
  write ( *, '(a)' ) '  R8_LBETA evaluates the logarithm of the Beta function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             A               B     LBETA(A,B)  R8_LBETA(A,B)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call beta_log_values ( n_data, a, b, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_lbeta ( a, b )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      a, b, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_lgams_test ( )

!*****************************************************************************80
!
!! R8_LGAMS_TEST tests R8_LGAMS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) alngam
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) slngam
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_LGAMS_TEST:'
  write ( *, '(a)' ) '  R8_LGAMS evaluates the sign of Gamma(x) and'
  write ( *, '(a)' ) '  the logarithm of the absolute value of Gamma(x).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X        LNGAM(X)  Sign(Gamma(X)) ALNGAM        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call gamma_log_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    call r8_lgams ( x, fx2, slngam )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, slngam, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_lgmc_test ( )

!*****************************************************************************80
!
!! R8_LGMC_TEST tests R8_LGMC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  real ( kind = 8 ) gamma_log
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_lgmc
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_LGMC_TEST:'
  write ( *, '(a)' ) '  R8_LGMC evaluates the correction log gamma factor.'
  write ( *, '(a)' ) '  r8_lgmc(x) = log ( gamma ( x ) ) - log ( sqrt ( 2 * pi )'
  write ( *, '(a)' ) '    - ( x - 0.5 ) * log ( x ) + x'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X        LGMC(X)  R8_LGMC(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call gamma_log_values ( n_data, x, gamma_log )

    if ( n_data == 0 ) then
      exit
    end if
!
!  Function requires 10 <= x.
!
    if ( 10.0D+00 <= x ) then
      fx1 = gamma_log - log ( sqrt ( 2.0D+00 * r8_pi ) ) - ( x - 0.5D+00 ) * log ( x ) + x
      fx2 = r8_lgmc ( x )
      write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
        x, fx1, fx2, abs ( fx1 - fx2 )
    end if

  end do

  return
end
subroutine r8_li_test ( )

!*****************************************************************************80
!
!! R8_LI_TEST tests R8_LI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_li
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_LI_TEST:'
  write ( *, '(a)' ) '  R8_LI evaluates the logarithmic integral.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X           LI(X)  R8_LI(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call logarithmic_integral_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_li ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' )  &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_lngam_test ( )

!*****************************************************************************80
!
!! R8_LNGAM_TEST tests R8_LNGAM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_lngam
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_LNGAM_TEST:'
  write ( *, '(a)' ) '  R8_LNGAM evaluates the logarithm of the gamma function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X        LNGAM(X)  R8_LNGAM(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call gamma_log_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_lngam ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' )  &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_lnrel_test ( )

!*****************************************************************************80
!
!! R8_LNREL_TEST tests R8_LNREL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_lnrel
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_LNREL_TEST:'
  write ( *, '(a)' ) '  R8_LNREL evaluates LOG(X+1).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X        LOG(X+1)  R8_LNREL(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call log_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    x = x - 1.0D+00

    fx2 = r8_lnrel ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' )  &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_log_test ( )

!*****************************************************************************80
!
!! R8_LOG_TEST tests R8_LOG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_log
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_LOG_TEST:'
  write ( *, '(a)' ) '  R8_LOG evaluates the logarithm function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X          LOG(X)  R8_LOG(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call log_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_log ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' )  &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_log10_test ( )

!*****************************************************************************80
!
!! R8_LOG10_TEST tests R8_LOG10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_log10
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_LOG10_TEST:'
  write ( *, '(a)' ) '  R8_LOG10 evaluates the logarithm base 10 function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X        LOG10(X)  R8_LOG10(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call log10_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_log10 ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' )  &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_mach_test ( )

!*****************************************************************************80
!
!! R8_MACH_TEST tests R8_MACH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_mach

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_MACH_TEST:'
  write ( *, '(a)' ) '  R8_MACH evaluates double precision machine numbers.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  R8_MACH (1) = B^(EMIN-1), the smallest positive magnitude.'
  write ( *, '(a)' ) '  R8_MACH (2) = B^EMAX*(1 - B^(-T)), the largest magnitude.'
  write ( *, '(a)' ) '  R8_MACH (3) = B^(-T), the smallest relative spacing.'
  write ( *, '(a)' ) '  R8_MACH (4) = B^(1-T), the largest relative spacing.'
  write ( *, '(a)' ) '  R8_MACH (5) = LOG10(B)'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I     R8_MACH(I)'
  write ( *, '(a)' ) ''

  do i = 1, 5
    write ( *, '(2x,i4,2x,g14.6)' ) i, r8_mach ( i )
  end do

  return
end
subroutine r8_pak_test ( )

!*****************************************************************************80
!
!! R8_PAK_TEST tests R8_PAK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) :: n_test(11) = (/ &
    7, 8, 7, 7, 4, 0, -1, 0, 7, 2, 0 /)
  real ( kind = 8 ) r8_pak
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) :: y_test(11) = (/ &
    0.5D+00, &
    0.5D+00, &
   -0.5D+00, &
    0.75D+00, &
    0.9375D+00, &
    0.5D+00, &
    0.5D+00, &
    0.625D+00, &
    0.5048828125D+00, &
    0.7853981633974483D+00, &
    0.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_PAK_TEST:'
  write ( *, '(a)' ) '  R8_PAK converts a mantissa and base 2 exponent to an R8.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    Mantissa     Exponent         R8'
  write ( *, '(a)' ) ''

  do i = 1, 11

    y = y_test(i)
    n = n_test(i)

    x = r8_pak ( y, n )

    write ( *, '(2x,g24.16,2x,i8,2x,g24.16)' ) y, n, x

  end do

  return
end
subroutine r8_poch_test ( )

!*****************************************************************************80
!
!! R8_POCH_TEST tests R8_POCH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_poch
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_POCH_TEST:'
  write ( *, '(a)' ) '  R8_POCH evaluates the Pochhammer symbol.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             A               X      POCH(A,X)    R8_POCH(A,X)         Diff'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call r8_rise_values ( n_data, a, n, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    x = real ( n, kind = 8 )
    fx2 = r8_poch ( a, x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      a, x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_psi_test ( )
 
!*****************************************************************************80
!
!! R8_PSI_TEST tests R8_PSI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_psi
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_PSI_TEST:'
  write ( *, '(a)' ) '  R8_PSI evaluates the Psi function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         PSI(X)  R8_PSI(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call psi_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_psi ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_rand_test ( )

!*****************************************************************************80
!
!! R8_RAND_TEST tests R8_RAND.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) average
  integer ( kind = 4 ) i
  integer ( kind = 4 ), save, dimension ( 7 ) :: i_value = (/ &
    1, 2, 3, 4, 10, 100, 1000 /)
  integer ( kind = 4 ) ix0
  integer ( kind = 4 ) ix1
  integer ( kind = 4 ) k
  real ( kind = 8 ) r
  real ( kind = 8 ), save, dimension ( 7 ) :: r_value = (/ &
    0.0004127026D+00, &
    0.6750836372D+00, &
    0.1614754200D+00, &
    0.9086198807D+00, &
    0.5527787209D+00, &
    0.3600893021D+00, &
    0.2176990509D+00 /)
  real ( kind = 8 ) r8_rand
  real ( kind = 8 ) s
  real ( kind = 8 ) variance

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_RAND_TEST:'
  write ( *, '(a)' ) '  R8_RAND is a random number generator.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             I         R8_RAND        Expected'
  write ( *, '(a)' ) ''
!
!  Start the sequence with IX0 = IX1 = 0.
!
  ix0 = 0
  ix1 = 0
  s = 0.0D+00

  k = 1

  do i = 1, 1000

    r = r8_rand ( s, ix0, ix1 )

    if ( i == i_value(k) ) then
      write ( *, '(2x,i14,2x,g14.6,2x,g14.6)' ) i, r, r_value(k)
      k = k + 1
    end if

  end do
!
!  Restart the sequence with IX0 = IX1 = 0.
!
  ix0 = 0
  ix1 = 0
  s = 0.0D+00

  average = 0.0D+00
  do i = 1, 1000000
    r = r8_rand ( s, ix0, ix1 )
    average = average + r
  end do
  average = average / 1000000.0D+00
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,2x,g14.6)' ) '       Average =  ', average, 0.5D+00
!
!  Restart the sequence with IX0 = IX1 = 0.
!
  ix0 = 0
  ix1 = 0
  s = 0.0D+00

  variance = 0.0D+00
  do i = 1, 1000000
    r = r8_rand ( s, ix0, ix1 )
    variance = variance + ( r - average ) ** 2
  end do
  variance = variance / 1000000.0D+00
  write ( *, '(a,g14.6,2x,g14.6)' ) &
     '       Variance = ', variance, 1.0D+00 / 12.0D+00

  return
end
subroutine r8_randgs_test ( )

!*****************************************************************************80
!
!! R8_RANDGS_TEST tests R8_RANDGS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) m
  real ( kind = 8 ) m2
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_randgs
  real ( kind = 8 ) sd
  real ( kind = 8 ) sd2
  integer ( kind = 4 ) seed

  m = 3.0D+00
  sd = 2.0D+00
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_RANDGS_TEST:'
  write ( *, '(a)' ) '  R8_RANDGS is a normal random number generator.'
  write ( *, '(a,g14.6)' ) '  Mean = ', m
  write ( *, '(a,g14.6)' ) '  Standard Deviation = ', sd
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             I         R8_RANDGS'
  write ( *, '(a)' ) ''

  m2 = 0.0D+00
  sd2 = 0.0D+00

  do i = 1, 1000

    r = r8_randgs ( m, sd, seed )
    m2 = m2 + r
    sd2 = sd2 + ( r - m ) ** 2
    if ( i <= 10 ) then
      write ( *, '(2x,i14,2x,g14.6)' ) i, r
    end if

  end do

  m2 = m2 / 1000.0D+00
  sd2 = sqrt ( sd2 / 1000.0D+00 )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Sequence mean =  ', m2
  write ( *, '(a,g14.6)' ) '  Sequence standard deviation =  ', sd2

  return
end
subroutine r8_random_test ( )

!*****************************************************************************80
!
!! R8_RANDOM_TEST tests R8_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 32

  real ( kind = 8 ) average
  integer ( kind = 4 ) i
  integer ( kind = 4 ), save, dimension ( 7 ) :: i_value = (/ &
    1, 2, 3, 4, 10, 100, 1000 /)
  integer ( kind = 4 ) ix0
  integer ( kind = 4 ) ix1
  integer ( kind = 4 ) k
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_random
  real ( kind = 8 ) t(n+1)
  real ( kind = 8 ) variance

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_RANDOM_TEST:'
  write ( *, '(a)' ) '  R8_RANDOM is a random number generator.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             I         R8_RANDOM'
  write ( *, '(a)' ) ''
!
!  Initialize the generator.
!
  ix0 = 0
  ix1 = 0
  call r8_random_init ( n, t, ix0, ix1 )

  k = 1

  do i = 1, 1000

    r = r8_random ( n, t, ix0, ix1 )

    if ( i == i_value(k) ) then
      write ( *, '(2x,i14,2x,g14.6)' ) i, r
      k = k + 1
    end if

  end do
!
!  Reinitialize the generator.
!
  ix0 = 0
  ix1 = 0
  call r8_random_init ( n, t, ix0, ix1 )

  average = 0.0D+00
  do i = 1, 1000000
    r = r8_random ( n, t, ix0, ix1 )
    average = average + r
  end do
  average = average / 1000000.0D+00
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,2x,g14.6)' ) '       Average =  ', average, 0.5D+00
!
!  Reinitialize the generator.
!
  ix0 = 0
  ix1 = 0
  call r8_random_init ( n, t, ix0, ix1 )

  variance = 0.0D+00
  do i = 1, 1000000
    r = r8_random ( n, t, ix0, ix1 )
    variance = variance + ( r - average ) ** 2
  end do
  variance = variance / 1000000.0D+00
  write ( *, '(a,g14.6,2x,g14.6)' ) &
     '       Variance = ', variance, 1.0D+00 / 12.0D+00

  return
end
subroutine r8_ren_test ( )

!*****************************************************************************80
!
!! R8_REN_TEST tests R8_REN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) average
  integer ( kind = 4 ) i
  integer ( kind = 4 ), save, dimension ( 7 ) :: i_value = (/ &
    1, 2, 3, 4, 10, 100, 1000 /)
  integer ( kind = 4 ) k
  real ( kind = 8 ) r
  real ( kind = 8 ), save, dimension ( 7 ) :: r_value = (/ &
    0.470393D+00, &
    0.799066D+00, &
    0.883261D+00, &
    0.407667D+00, &
    0.955566D+00, &
    0.173576D+00, &
    0.121733D-01 /)  
  real ( kind = 8 ) r8_ren
  integer ( kind = 4 ) seed
  real ( kind = 8 ) variance

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_REN_TEST:'
  write ( *, '(a)' ) '  R8_REN is a random number generator.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             I         R8_REN         Expected'
  write ( *, '(a)' ) ''

  seed = 100001
  k = 1
  do i = 1, 1000

    r = r8_ren ( seed )

    if ( i == i_value(k) ) then
      write ( *, '(2x,i14,2x,g14.6,2x,g14.6)' ) i, r, r_value(k)
      k = k + 1
    end if

  end do

  seed = 100001
  average = 0.0D+00
  do i = 1, 1000000
    r = r8_ren ( seed )
    average = average + r
  end do
  average = average / 1000000.0D+00
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,2x,g14.6)' ) '       Average =  ', average, 0.5D+00

  seed = 100001
  variance = 0.0D+00
  do i = 1, 1000000
    r = r8_ren ( seed )
    variance = variance + ( r - average )**2
  end do
  variance = variance / 1000000.0D+00
  write ( *, '(a,g14.6,2x,g14.6)' ) &
     '       Variance = ', variance, 1.0D+00 / 12.0D+00

  return
end
subroutine r8_shi_test ( )

!*****************************************************************************80
!
!! R8_SHI_TEST tests R8_SHI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_shi
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SHI_TEST:'
  write ( *, '(a)' ) '  R8_SHI evaluates the hyperbolic sine integral.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X          SHI(X)  R8_SHI(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call shi_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_shi ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_si_test ( )

!*****************************************************************************80
!
!! R8_SI_TEST tests R8_SI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_si
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SI_TEST:'
  write ( *, '(a)' ) '  R8_SI evaluates the Sine integral.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X           SI(X)  R8_SI(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call si_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_si ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_sin_test ( )

!*****************************************************************************80
!
!! R8_SIN_TEST tests R8_SIN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_sin
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SIN_TEST:'
  write ( *, '(a)' ) '  R8_SIN evaluates the sine function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         SIN(X)  R8_SIN(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call sin_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_sin ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_sin_deg_test ( )

!*****************************************************************************80
!
!! R8_SIN_DEG_TEST tests R8_SIN_DEG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_sin_deg
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SIN_DEG_TEST:'
  write ( *, '(a)' ) '  R8_SIN_DEG evaluates the sine of a degree argument.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X     SIN_DEG(X)  R8_SIN_DEG(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call sin_degree_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_sin_deg ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_sinh_test ( )

!*****************************************************************************80
!
!! R8_SINH_TEST tests R8_SINH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_sinh
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SINH_TEST:'
  write ( *, '(a)' ) '  R8_SINH evaluates the hyperbolic sine function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         SINH(X)  R8_SINH(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call sinh_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_sinh ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_spence_test ( )

!*****************************************************************************80
!
!! R8_SPENCE_TEST tests R8_SPENCE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_spence
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SPENCE_TEST:'
  write ( *, '(a)' ) '  R8_SPENCE evaluates the dilogarithm function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X       SPENCE(X)  R8_SPENCE(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call dilogarithm_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_spence ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_sqrt_test ( )

!*****************************************************************************80
!
!! R8_SQRT_TEST tests R8_SQRT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_sqrt
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_SQRT_TEST:'
  write ( *, '(a)' ) '  R8_SQRT evaluates the square root function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         SQRT(X)  R8_SQRT(X)        Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call sqrt_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_sqrt ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_tan_test ( )

!*****************************************************************************80
!
!! R8_TAN_TEST tests R8_TAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_tan
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_TAN_TEST:'
  write ( *, '(a)' ) '  R8_TAN evaluates the tangent function'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         TAN(X)  R8_TAN(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call tan_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_tan ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_tanh_test ( )

!*****************************************************************************80
!
!! R8_TANH_TEST tests R8_TANH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_tanh
  real ( kind = 8 ) x

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_TANH_TEST:'
  write ( *, '(a)' ) '  R8_TANH evaluates the hyperbolic tangent function.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         TANH(X)  R8_TANH(X)         Diff'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call tanh_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_tanh ( x )

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, fx1, fx2, abs ( fx1 - fx2 )

  end do

  return
end
subroutine r8_upak_test ( )

!*****************************************************************************80
!
!! R8_UPAK_TEST tests R8_UPAK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) x
  real ( kind = 8 ) :: x_test(11) = (/ &
    64.0D+00, &
   128.0D+00, &
   -64.0D+00, &
    96.0D+00, &
    15.0D+00, &
    0.5D+00, &
    0.25D+00, &
    0.625D+00, &
   64.625D+00, &
    3.141592653589793D+00, &
    0.0D+00 /)
  real ( kind = 8 ) y

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_UPAK_TEST:'
  write ( *, '(a)' ) '  R8_UPAK converts an R8 to a mantissa and base 2 exponent.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '             X         Mantissa     Exponent'
  write ( *, '(a)' ) ''

  do i = 1, 11

    x = x_test(i)

    call r8_upak ( x, y, n )

    write ( *, '(2x,g14.6,2x,g24.16,2x,i8)' ) x, y, n

  end do

  return
end
