program main

!*****************************************************************************80
!
!! MAIN is the main program for C8LIB_TEST.
!
!  Discussion:
!
!    C8LIB_TEST tests C8LIB.
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

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8LIB_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test C8LIB.'

  call c8_abs_test ( )
  call c8_acos_test ( )
  call c8_acosh_test ( )
  call c8_add_test ( )
  call c8_arg_test ( )
  call c8_asin_test ( )
  call c8_asinh_test ( )
  call c8_atan_test ( )
  call c8_atanh_test ( )
  call c8_conj_test ( )
  call c8_cos_test ( )
  call c8_cosh_test ( )
  call c8_cube_root_test ( )
  call c8_div_test ( )
  call c8_div_r8_test ( )
  call c8_exp_test ( )
  call c8_imag_test ( )
  call c8_i_test ( )
  call c8_inv_test ( )
  call c8_le_l1_test ( )
  call c8_le_l2_test ( )
  call c8_le_li_test ( )
  call c8_log_test ( )
  call c8_mag_test ( )
  call c8_mul_test ( )
  call c8_nint_test ( )
  call c8_norm_l1_test ( )
  call c8_norm_l2_test ( )
  call c8_norm_li_test ( )
  call c8_normal_01_test ( )
  call c8_one_test ( )
  call c8_print_test ( )
  call c8_real_test ( )
  call c8_sin_test ( )
  call c8_sinh_test ( )
  call c8_sqrt_test ( )
  call c8_sub_test ( )
  call c8_tan_test ( )
  call c8_tanh_test ( )
  call c8_to_cartesian_test ( )
  call c8_to_polar_test ( )
  call c8_uniform_01_test ( )
  call c8_zero_test ( )

  call c8mat_identity_test ( )
  call c8mat_indicator_test ( )
  call c8mat_norm_fro_test ( )
  call c8mat_norm_l1_test ( )
  call c8mat_norm_li_test ( )
  call c8mat_uniform_01_test ( )

  call c8vec_indicator_test ( )
  call c8vec_nint_test ( )
  call c8vec_norm_l1_test ( )
  call c8vec_norm_l2_test ( )
  call c8vec_norm_li_test ( )
  call c8vec_print_test ( )
  call c8vec_print_part_test ( )
  call c8vec_sort_a_l1_test ( )
  call c8vec_sort_a_l2_test ( )
  call c8vec_sort_a_li_test ( )
  call c8vec_spiral_test ( )
  call c8vec_uniform_01_test ( )
  call c8vec_unity_test ( )

  call cartesian_to_c8_test ( )

  call polar_to_c8_test ( )

  call r8_atan_test ( )
  call r8_uniform_01_test ( )
  call r8poly2_root_test ( )
  call r8poly3_root_test ( )
  call r8poly4_root_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8LIB_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine c8_abs_test ( )

!*****************************************************************************80
!
!! C8_ABS_TEST tests C8_ABS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  real ( kind = 8 ) c8_abs
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  real ( kind = 8 ) r2
  real ( kind = 8 ) r3
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_ABS_TEST'
  write ( *, '(a)' ) '  C8_ABS computes the absolute value of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       C1=C8_UNIFORM_01          R2=C8_ABS(C1)             R3=ABS(C1)'
  write ( *, '(a)' ) '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    r2 = c8_abs ( c1 )
    r3 = abs ( c1 )
    write ( *, '(2x,2f12.6,2x,f12.6,12x,2x,f12.6)' ) c1, r2, r3
  end do

  return
end
subroutine c8_acos_test ( )

!*****************************************************************************80
!
!! C8_ACOS_TEST tests C8_ACOS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 November 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_acos
  complex ( kind = 8 ) c8_cos
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_ACOS_TEST'
  write ( *, '(a)' ) '  C8_ACOS computes the inverse cosine.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       C1=C8_UNIFORM_01          C2 = C8_ACOS(C1)           C3 = C8_COS(C2)'
  write ( *, '(a)' ) &
    '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a,a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    c2 = c8_acos ( c1 )
    c3 = c8_cos ( c2 )
    write ( *, '(2x,2f12.6,2x,2f12.6,2x,2f12.6)' ) c1, c2, c3
  end do

  return
end
subroutine c8_acosh_test ( )

!*****************************************************************************80
!
!! C8_ACOSH_TEST tests C8_ACOSH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 November 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_acosh
  complex ( kind = 8 ) c8_cosh
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_ACOSH_TEST'
  write ( *, '(a)' ) '  C8_ACOSH computes the inverse hyperbolic cosine.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       C1=C8_UNIFORM_01          C2 = C8_ACOSH(C1)           C3 = COSH(C2)'
  write ( *, '(a)' ) &
    '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a,a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    c2 = c8_acosh ( c1 )
    c3 = c8_cosh ( c2 )
    write ( *, '(2x,2f12.6,2x,2f12.6,2x,2f12.6)' ) c1, c2, c3
  end do

  return
end
subroutine c8_add_test ( )

!*****************************************************************************80
!
!! C8_ADD_TEST tests C8_ADD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_add
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_ADD_TEST'
  write ( *, '(a)' ) '  C8_ADD adds two C8s'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       C1=C8_UNIFORM_01          C2=C8_UNIFORM_01          C3 = C8_ADD(C1,C2)'
  write ( *, '(a)' ) &
    '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a,a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    c2 = c8_uniform_01 ( seed )
    c3 = c8_add ( c1, c2 )
    write ( *, '(2x,2f12.6,2x,2f12.6,2x,2f12.6)' ) c1, c2, c3
  end do

  return
end
subroutine c8_arg_test ( )

!*****************************************************************************80
!
!! C8_ARG_TEST tests C8_ARG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  real ( kind = 8 ) c8_arg
  complex ( kind = 8 ) c8_uniform_01
  real ( kind = 8 ) r2
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_ARG_TEST'
  write ( *, '(a)' ) '  C8_ARG computes the argument of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       C1=C8_UNIFORM_01          R2=C8_ARG(C1)'
  write ( *, '(a)' ) &
    '     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )

    r2 = c8_arg ( c1 )

    write ( *, '(2x,2f12.4,2x,f12.4)' ) c1, r2

  end do

  return
end
subroutine c8_asin_test ( )

!*****************************************************************************80
!
!! C8_ASIN_TEST tests C8_ASIN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_asin
  complex ( kind = 8 ) c8_sin
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_ASIN_TEST'
  write ( *, '(a)' ) '  C8_ASIN computes the inverse sine.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
     '       C1=C8_UNIFORM_01          C2 = C8_ASIN(C1)          C3 = C8_SIN(C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a,a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    c2 = c8_asin ( c1 )
    c3 = c8_sin ( c2 )
    write ( *, '(2x,2f12.6,2x,2f12.6,2x,2f12.6)' ) c1, c2, c3
  end do

  return
end
subroutine c8_asinh_test ( )

!*****************************************************************************80
!
!! C8_ASINH_TEST tests C8_ASINH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 November 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_asinh
  complex ( kind = 8 ) c8_sinh
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_ASINH_TEST'
  write ( *, '(a)' ) '  C8_ASINH computes the inverse hyperbolic sine.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
     '       C1=C8_UNIFORM_01          C2 = C8_ASINH(C1)         C3 = C8_SINH(C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a,a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    c2 = c8_asinh ( c1 )
    c3 = c8_sinh ( c2 )
    write ( *, '(2x,2f12.6,2x,2f12.6,2x,2f12.6)' ) c1, c2, c3
  end do

  return
end
subroutine c8_atan_test ( )

!*****************************************************************************80
!
!! C8_ATAN_TEST tests C8_ATAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_atan
  complex ( kind = 8 ) c8_tan
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_ATAN_TEST'
  write ( *, '(a)' ) '  C8_ATAN computes the inverse tangent.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
     '       C1=C8_UNIFORM_01          C2 = C8_ATAN(C1)          C3 = C8_TAN(C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a,a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    c2 = c8_atan ( c1 )
    c3 = c8_tan ( c2 )
    write ( *, '(2x,2f12.6,2x,2f12.6,2x,2f12.6)' ) c1, c2, c3
  end do

  return
end
subroutine c8_atanh_test ( )

!*****************************************************************************80
!
!! C8_ATANH_TEST tests C8_ATANH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 November 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_atanh
  complex ( kind = 8 ) c8_tanh
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_ATANH_TEST'
  write ( *, '(a)' ) '  C8_ATANH computes the inverse hyperbolic tangent.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
     '       C1=C8_UNIFORM_01          C2 = C8_ATANH(C1)         C3 = C8_TANH(C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a,a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    c2 = c8_atanh ( c1 )
    c3 = c8_tanh ( c2 )
    write ( *, '(2x,2f12.6,2x,2f12.6,2x,2f12.6)' ) c1, c2, c3
  end do

  return
end
subroutine c8_conj_test ( )

!*****************************************************************************80
!
!! C8_CONJ_TEST tests C8_CONJ.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_conj
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_CONJ_TEST'
  write ( *, '(a)' ) '  C8_CONJ computes the conjugate of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       C1=C8_UNIFORM_01          C2=C8_CONJ(C1)            C3=C8_CONJ(C2)'
  write ( *, '(a)' ) &
    '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_conj ( c1 )
    c3 = c8_conj ( c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  return
end
subroutine c8_cos_test ( )

!*****************************************************************************80
!
!! C8_COS_TEST tests C8_COS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_acos
  complex ( kind = 8 ) c8_cos
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_COS_TEST'
  write ( *, '(a)' ) '  C8_COS computes the cosine of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=C8_COS(C1)             C3=C8_ACOS(C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_cos ( c1 )
    c3 = c8_acos ( c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  return
end
subroutine c8_cosh_test ( )

!*****************************************************************************80
!
!! C8_COSH_TEST tests C8_COSH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_acosh
  complex ( kind = 8 ) c8_cosh
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_COSH_TEST'
  write ( *, '(a)' ) '  C8_COSH computes the hyperbolic cosine of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       C1=C8_UNIFORM_01          C2=C8_COSH(C1)          C3=C8_COSH(C2) '
  write ( *, '(a)' ) &
    '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_cosh ( c1 )
    c3 = c8_acosh ( c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  return
end
subroutine c8_cube_root_test ( )

!*****************************************************************************80
!
!! C8_CUBE_ROOT_TEST tests C8_CUBE_ROOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_cube_root
  complex ( kind = 8 ) c8_uniform_01
  real ( kind = 8 ) power
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_CUBE_ROOT_TEST'
  write ( *, '(a)' ) '  C8_CUBE_ROOT computes the principal cube root of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=C8_CUBE_ROOT(C1)       C3=C2*C2*C2'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_cube_root ( c1 )
    c3 = c2 * c2 * c2
    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=C1**(1.0/3.0)          C3=C2*C2*C2'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  power = 1.0D+00 / 3.0D+00

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c1**power
    c3 = c2 * c2 * c2
    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  return
end
subroutine c8_div_test ( )

!*****************************************************************************80
!
!! C8_DIV_TEST tests C8_DIV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c4
  complex ( kind = 8 ) c8_div
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_DIV_TEST'
  write ( *, '(a)' ) '  C8_DIV computes C3 = C1 / C2.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=C8_UNIFORM_01          C3=C8_DIV(C1,C2)              C4=C1/C2'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_uniform_01 ( seed )
    c3 = c8_div ( c1, c2 )
    c4 = c1 / c2

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3, c4

  end do

  return
end
subroutine c8_div_r8_test ( )

!*****************************************************************************80
!
!! C8_DIV_R8_TEST tests C8_DIV_R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c4
  complex ( kind = 8 ) c8_div_r8
  complex ( kind = 8 ) c8_uniform_01
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_DIV_R8_TEST'
  write ( *, '(a)' ) '  C8_DIV_R8 computes C3 = C1 / R2.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          R2=R8_UNIFORM_01          C3=C8_DIV(C1,C2)              C4=C1/C2'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    r2 = r8_uniform_01 ( seed )
    c3 = c8_div_r8 ( c1, r2 )
    c4 = c1 / r2

    write ( *, '(2x,2f12.4,2x,f12.4,12x,2x,2f12.4,2x,2f12.4)' ) c1, r2, c3, c4

  end do

  return
end
subroutine c8_exp_test ( )

!*****************************************************************************80
!
!! C8_EXP_TEST tests C8_EXP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_exp
  complex ( kind = 8 ) c8_log
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_EXP_TEST'
  write ( *, '(a)' ) '  C8_EXP computes exp ( Z ).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=C8_EXP(C1)             C3=C8_LOG(C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_exp ( c1 )
    c3 = c8_log ( c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  return
end
subroutine c8_i_test ( )

!*****************************************************************************80
!
!! C8_I_TEST tests C8_I.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c8_i

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_I_TEST'
  write ( *, '(a)' ) '  C8_I returns the imaginary unit.'
  write ( *, '(a)' ) ''

  c1 = c8_i ( )

  call c8_print ( c1, '  C1 = C8_I ( ) = ' )

  c2 = c1 * c1

  call c8_print ( c2, '  C2 = C1 * C1 = ' )

  return
end
subroutine c8_imag_test ( )

!*****************************************************************************80
!
!! C8_IMAG_TEST tests C8_IMAG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  real ( kind = 8 ) c8_imag
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  real ( kind = 8 ) r2
  real ( kind = 8 ) r3
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_IMAG_TEST'
  write ( *, '(a)' ) '  C8_IMAG computes the imaginary part of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       C1=C8_UNIFORM_01          R2=C8_IMAG(C1)             R3=IMAG(C1)'
  write ( *, '(a)' ) '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    r2 = c8_imag ( c1 )
    r3 = imag ( c1 )
    write ( *, '(2x,2f12.6,2x,f12.6,12x,2x,f12.6)' ) c1, r2, r3
  end do

  return
end
subroutine c8_inv_test ( )

!*****************************************************************************80
!
!! C8_INV_TEST tests C8_INV.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_inv
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_INV_TEST'
  write ( *, '(a)' ) '  C8_INV computes C2 = 1 / C1.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=C8_INV(C1)             C3=C8_INV(C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_inv ( c1 )
    c3 = c8_inv ( c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  return
end
subroutine c8_le_l1_test ( )

!*****************************************************************************80
!
!! C8_LE_L1_TEST tests C8_LE_L1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  logical ( kind = 4 ) c8_le_l1
  complex ( kind = 8 ) c8_uniform_01
  logical ( kind = 4 ) l3
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_LE_L1_TEST'
  write ( *, '(a)' ) '  C8_LE_L1 evalues (C1 <= C2) using the L1 norm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=C8_UNIFORM_01         L3=C8_LE_L1(C1,C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_uniform_01 ( seed )
    l3 = c8_le_l1 ( c1, c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,8x,l1)' ) c1, c2, l3

  end do

  return
end
subroutine c8_le_l2_test ( )

!*****************************************************************************80
!
!! C8_LE_L2_TEST tests C8_LE_L2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  logical ( kind = 4 ) c8_le_l2
  complex ( kind = 8 ) c8_uniform_01
  logical ( kind = 4 ) l3
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_LE_L2_TEST'
  write ( *, '(a)' ) '  C8_LE_L2 evalues (C1 <= C2) using the L2 norm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=C8_UNIFORM_01         L3=C8_LE_L2(C1,C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_uniform_01 ( seed )
    l3 = c8_le_l2 ( c1, c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,8x,l1)' ) c1, c2, l3

  end do

  return
end
subroutine c8_le_li_test ( )

!*****************************************************************************80
!
!! C8_LE_LI_TEST tests C8_LE_LI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  logical ( kind = 4 ) c8_le_li
  complex ( kind = 8 ) c8_uniform_01
  logical ( kind = 4 ) l3
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_LE_LI_TEST'
  write ( *, '(a)' ) '  C8_LE_LI evalues (C1 <= C2) using the Loo norm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=C8_UNIFORM_01         L3=C8_LE_LI(C1,C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_uniform_01 ( seed )
    l3 = c8_le_li ( c1, c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,8x,l1)' ) c1, c2, l3

  end do

  return
end
subroutine c8_log_test ( )

!*****************************************************************************80
!
!! C8_LOG_TEST tests C8_LOG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_exp
  complex ( kind = 8 ) c8_log
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_LOG_TEST'
  write ( *, '(a)' ) '  C8_LOG computes log ( Z ).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=C8_LOG(C1)             C3=C8_EXP(C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_log ( c1 )
    c3 = c8_exp ( c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  return
end
subroutine c8_mag_test ( )

!*****************************************************************************80
!
!! C8_MAG_TEST tests C8_MAG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  real ( kind = 8 ) c8_mag
  complex ( kind = 8 ) c8_uniform_01
  real ( kind = 8 ) r2
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_MAG_TEST'
  write ( *, '(a)' ) '  C8_MAG computes the magnitude of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       C1=C8_UNIFORM_01          R2=C8_MAG(C1)'
  write ( *, '(a)' ) &
    '     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    r2 = c8_mag ( c1 )

    write ( *, '(2x,2f12.4,2x,f12.4)' ) c1, r2

  end do

  return
end
subroutine c8_mul_test ( )

!*****************************************************************************80
!
!! C8_MUL_TEST tests C8_MUL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c4
  complex ( kind = 8 ) c8_mul
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_MUL_TEST'
  write ( *, '(a)' ) '  C8_MUL computes C3 = C1 * C2.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '        C1=C8_UNIFORM_01          C2=C8_UNIFORM_01' // &
                     '          C3=C8_MUL(C1,C2)          C4=C1*C2'
  write ( *, '(a)' ) '     ---------------------     ---------------------' // &
                     '     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_uniform_01 ( seed )
    c3 = c8_mul ( c1, c2 )
    c4 = c1 * c2

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3, c4

  end do

  return
end
subroutine c8_nint_test ( )

!*****************************************************************************80
!
!! C8_NINT_TEST tests C8_NINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 December 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c8_nint
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_NINT_TEST'
  write ( *, '(a)' ) '  C8_NINT computes the nearest integer to a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       C1=10*C8_UNIFORM_01      C2=C8_NINT(C1)'
  write ( *, '(a)' ) '     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = 10.0D+00 * c8_uniform_01 ( seed )
    c2 = c8_nint ( c1 )
    write ( *, '(2x,2f12.6,2x,2f12.6)' ) c1, c2
  end do

  return
end
subroutine c8_norm_l1_test ( )

!*****************************************************************************80
!
!! C8_NORM_L1_TEST tests C8_NORM_L1.
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

  complex ( kind = 8 ) c1
  real ( kind = 8 ) c8_norm_l1
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  real ( kind = 8 ) r2
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_NORM_L1_TEST'
  write ( *, '(a)' ) '  C8_NORM_L1 computes the L1 norm of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       C1=C8_UNIFORM_01          R2=C8_NORM_L1(C1)'
  write ( *, '(a)' ) '     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    r2 = c8_norm_l1 ( c1 )
    write ( *, '(2x,2f12.6,2x,f12.6,12x)' ) c1, r2
  end do

  return
end
subroutine c8_norm_l2_test ( )

!*****************************************************************************80
!
!! C8_NORM_L2_TEST tests C8_NORM_L2.
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

  complex ( kind = 8 ) c1
  real ( kind = 8 ) c8_norm_l2
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  real ( kind = 8 ) r2
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_NORM_L2_TEST'
  write ( *, '(a)' ) '  C8_NORM_L2 computes the L2 norm of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       C1=C8_UNIFORM_01          R2=C8_NORM_L2(C1)'
  write ( *, '(a)' ) '     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    r2 = c8_norm_l2 ( c1 )
    write ( *, '(2x,2f12.6,2x,f12.6,12x)' ) c1, r2
  end do

  return
end
subroutine c8_norm_li_test ( )

!*****************************************************************************80
!
!! C8_NORM_LI_TEST tests C8_NORM_LI.
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

  complex ( kind = 8 ) c1
  real ( kind = 8 ) c8_norm_li
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  real ( kind = 8 ) r2
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_NORM_LI_TEST'
  write ( *, '(a)' ) '  C8_NORM_LI computes the Loo norm of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       C1=C8_UNIFORM_01          R2=C8_NORM_LI(C1)'
  write ( *, '(a)' ) '     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    r2 = c8_norm_li ( c1 )
    write ( *, '(2x,2f12.6,2x,f12.6,12x)' ) c1, r2
  end do

  return
end
subroutine c8_normal_01_test ( )

!*****************************************************************************80
!
!! C8_NORMAL_01_TEST tests C8_NORMAL_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c8_normal_01
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 20

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_NORMAL_01_TEST'
  write ( *, '(a)' ) '  C8_NORMAL_01 generates unit pseudonormal C8s'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       C1=C8_NORMAL_01(SEED)'
  write ( *, '(a)' ) '     ---------------------'
  write ( *, '(a)' ) ' '

  do test = 1, test_num

    c1 = c8_normal_01 ( seed )
    write ( *, '(2x,2g14.6)' ) c1

  end do

  return
end
subroutine c8_one_test ( )

!*****************************************************************************80
!
!! C8_ONE_TEST tests C8_ONE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c8_one

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_ONE_TEST'
  write ( *, '(a)' ) '  C8_ONE returns 1 as a C8.'
  write ( *, '(a)' ) ''

  c1 = c8_one ( )

  call c8_print ( c1, '  C1 = C8_ONE ( ) = ' )

  c2 = c1 + c1

  call c8_print ( c2, '  C2 = C1 + C1 = ' )

  return
end
subroutine c8_print_test ( )

!*****************************************************************************80
!
!! C8_PRINT_TEST tests C8_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c4
  complex ( kind = 8 ) c5
  complex ( kind = 8 ) c6
  complex ( kind = 8 ) c7
  complex ( kind = 8 ) c8

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_PRINT_TEST'
  write ( *, '(a)' ) '  C8_PRINT prints a C8.'
  write ( *, '(a)' ) ' '

  c1 = cmplx (  0.0D+00,               0.0D+00,      kind = 8 )
  c2 = cmplx (  1.0D+00,               0.0D+00,      kind = 8 )
  c3 = cmplx (  3.141592653589793D+00, 0.0D+00,      kind = 8 )
  c4 = cmplx (  0.0D+00,               1.0D+00,      kind = 8 )
  c5 = cmplx (  1.0D+00,               2.0D+00,      kind = 8 )
  c6 = cmplx ( -12.34D+00,            56.78D+00,     kind = 8 )
  c7 = cmplx (  0.001D+00,             0.000002D+00, kind = 8 )
  c8 = cmplx (  3.0D+08,              -4.5D+09,      kind = 8 )

  call c8_print ( c1, '  Zero:' )
  call c8_print ( c2, '  One:' )
  call c8_print ( c3, '  Pi:' )
  call c8_print ( c4, '  i:' )
  call c8_print ( c5, '  1+2i:' )
  call c8_print ( c6, ' -12.34 + 56.78i:' )
  call c8_print ( c7, '  1E-3 + 2E-6i:' )
  call c8_print ( c8, '  3E8 - 4.5E9i:' )

  return
end
subroutine c8_real_test ( )

!*****************************************************************************80
!
!! C8_REAL_TEST tests C8_REAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  real ( kind = 8 ) c8_real
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  real ( kind = 8 ) r2
  real ( kind = 8 ) r3
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_REAL_TEST'
  write ( *, '(a)' ) '  C8_REAL computes the real part of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       C1=C8_UNIFORM_01          R2=C8_REAL(C1)             R3=REAL(C1)'
  write ( *, '(a)' ) '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    r2 = c8_real ( c1 )
    r3 = real ( c1, kind = 8 )
    write ( *, '(2x,2f12.6,2x,f12.6,12x,2x,f12.6)' ) c1, r2, r3
  end do

  return
end
subroutine c8_sin_test ( )

!*****************************************************************************80
!
!! C8_SIN_TEST tests C8_SIN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_asin
  complex ( kind = 8 ) c8_sin
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_SIN_TEST'
  write ( *, '(a)' ) '  C8_SIN computes the sine of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=C8_SIN(C1)             C3=C8_ASIN(C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_sin ( c1 )
    c3 = c8_asin ( c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  return
end
subroutine c8_sinh_test ( )

!*****************************************************************************80
!
!! C8_SINH_TEST tests C8_SINH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_asinh
  complex ( kind = 8 ) c8_sinh
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_SINH_TEST'
  write ( *, '(a)' ) '  C8_SINH computes the hyperbolic sine of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       C1=C8_UNIFORM_01          C2=C8_SINH(C1)          C3=C8_SINH(C2) '
  write ( *, '(a)' ) &
    '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_sinh ( c1 )
    c3 = c8_asinh ( c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  return
end
subroutine c8_sqrt_test ( )

!*****************************************************************************80
!
!! C8_SQRT_TEST tests C8_SQRT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_sqrt
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_SQRT_TEST'
  write ( *, '(a)' ) '  C8_SQRT computes the principal square root of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=C8_SQRT(C1)            C3=C2*C2'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_sqrt ( c1 )
    c3 = c2 * c2

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01          C2=SQRT(C1)               C3=C2*C2'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = sqrt ( c1 )
    c3 = c2 * c2

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  return
end
subroutine c8_sub_test ( )

!*****************************************************************************80
!
!! C8_SUB_TEST tests C8_SUB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c8_sub
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_SUB_TEST'
  write ( *, '(a)' ) '  C8_SUB subtracts two C8s'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       C1=C8_UNIFORM_01          C2=C8_UNIFORM_01          C3 = C8_SUB(C1,C2)'
  write ( *, '(a)' ) &
    '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a,a)' ) ' '

  seed = 123456789

  do i = 1, 10
    c1 = c8_uniform_01 ( seed )
    c2 = c8_uniform_01 ( seed )
    c3 = c8_sub ( c1, c2 )
    write ( *, '(2x,2f12.6,2x,2f12.6,2x,2f12.6)' ) c1, c2, c3
  end do

  return
end
subroutine c8_tan_test ( )

!*****************************************************************************80
!
!! C8_TAN_TEST tests C8_TAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_atan
  complex ( kind = 8 ) c8_tan
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_TAN_TEST'
  write ( *, '(a)' ) '  C8_TAN computes the tangent of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       C1=C8_UNIFORM_01          C2=C8_TAN(C1)             C3=C8_ATAN(C2)'
  write ( *, '(a)' ) &
    '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_tan ( c1 )
    c3 = c8_atan ( c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  return
end
subroutine c8_tanh_test ( )

!*****************************************************************************80
!
!! C8_TANH_TEST tests C8_TANH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_atanh
  complex ( kind = 8 ) c8_tanh
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_TANH_TEST'
  write ( *, '(a)' ) '  C8_TANH computes the tangent of a C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       C1=C8_UNIFORM_01          C2=C8_TANH(C1)           C3=C8_ATANH(C2)'
  write ( *, '(a)' ) &
    '     ---------------------     ---------------------    ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    c2 = c8_tanh ( c1 )
    c3 = c8_atanh ( c2 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, c2, c3

  end do

  return
end
subroutine c8_to_cartesian_test ( )

!*****************************************************************************80
!
!! C8_TO_CARTESIAN_TEST tests C8_TO_CARTESIAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10
  real ( kind = 8 ) x2
  real ( kind = 8 ) y2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_TO_CARTESIAN_TEST'
  write ( *, '(a)' ) '  C8_TO_CARTESIAN converts C8 to (X,Y).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01    (X2,Y2)=C8_TO_CARTESIAN(C1)     C3=CARTESIAN_TO_C8(X2,Y2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    call c8_to_cartesian ( c1, x2, y2 )
    call cartesian_to_c8 ( x2, y2, c3 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, x2, y2, c3

  end do

  return
end
subroutine c8_to_polar_test ( )

!*****************************************************************************80
!
!! C8_TO_POLAR_TEST tests C8_TO_POLAR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c3
  complex ( kind = 8 ) c8_uniform_01
  real ( kind = 8 ) r2
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t2
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_TO_POLAR_TEST'
  write ( *, '(a)' ) '  C8_TO_POLAR converts C8 to (R,T).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        C1=C8_UNIFORM_01       (R2,T2)=C8_TO_POLAR(C1)     C3=POLAR_TO_C8(R2,T2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )
    call c8_to_polar ( c1, r2, t2 )
    call polar_to_c8 ( r2, t2, c3 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) c1, r2, t2, c3

  end do

  return
end
subroutine c8_uniform_01_test ( )

!*****************************************************************************80
!
!! C8_UNIFORM_01_TEST tests C8_UNIFORM_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  C8_UNIFORM_01 returns a uniformly random "unit" C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       C1=C8_UNIFORM_01(SEED)'
  write ( *, '(a)' ) &
    '     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    c1 = c8_uniform_01 ( seed )

    write ( *, '(2x,2f12.4)' ) c1

  end do

  return
end
subroutine c8_zero_test ( )

!*****************************************************************************80
!
!! C8_ZERO_TEST tests C8_ZERO.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c8_zero

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8_ZERO_TEST'
  write ( *, '(a)' ) '  C8_ZERO returns 0 as a C8.'
  write ( *, '(a)' ) ''

  c1 = c8_zero ( )

  call c8_print ( c1, '  C1 = C8_ZERO ( ) = ' )

  return
end
subroutine c8mat_identity_test ( )

!*****************************************************************************80
!
!! C8MAT_IDENTITY_TEST tests C8MAT_IDENTITY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  complex ( kind = 8 ) a(n,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8MAT_IDENTITY_TEST'
  write ( *, '(a)' ) '  C8MAT_IDENTITY returns the complex identity matrix.'

  call c8mat_identity ( n, a )

  call c8mat_print ( n, n, a, '  The C8MAT_IDENTITY matrix:' )

  return
end
subroutine c8mat_indicator_test ( )

!*****************************************************************************80
!
!! C8MAT_INDICATOR_TEST tests C8MAT_INDICATOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  complex ( kind = 8 ) a(m,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8MAT_INDICATOR_TEST'
  write ( *, '(a)' ) '  C8MAT_INDICATOR returns the complex indicator matrix.'

  call c8mat_indicator ( m, n, a )

  call c8mat_print ( m, n, a, '  The C8MAT_INDICATOR matrix:' )

  return
end
subroutine c8mat_norm_fro_test ( )

!*****************************************************************************80
!
!! C8MAT_NORM_FRO_TEST tests C8MAT_NORM_FRO.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  complex ( kind = 8 ) a(m,n)
  real ( kind = 8 ) c8mat_norm_fro
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8MAT_NORM_FRO_TEST'
  write ( *, '(a)' ) '  C8MAT_NORM_FRO returns the Frobenius norm of a matrix.'

  call c8mat_indicator ( m, n, a )

  call c8mat_print ( m, n, a, '  The C8MAT_INDICATOR matrix:' )

  value = c8mat_norm_fro ( m, n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The Frobenius norm = ', value

  return
end
subroutine c8mat_norm_l1_test ( )

!*****************************************************************************80
!
!! C8MAT_NORM_L1_TEST tests C8MAT_NORM_L1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  complex ( kind = 8 ) a(m,n)
  real ( kind = 8 ) c8mat_norm_l1
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8MAT_NORM_L1_TEST'
  write ( *, '(a)' ) '  C8MAT_NORM_L1 returns the L1 norm of a matrix.'

  call c8mat_indicator ( m, n, a )

  call c8mat_print ( m, n, a, '  The C8MAT_INDICATOR matrix:' )

  value = c8mat_norm_l1 ( m, n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The L1 norm = ', value

  return
end
subroutine c8mat_norm_li_test ( )

!*****************************************************************************80
!
!! C8MAT_NORM_LI_TEST tests C8MAT_NORM_LI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  complex ( kind = 8 ) a(m,n)
  real ( kind = 8 ) c8mat_norm_li
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8MAT_NORM_LI_TEST'
  write ( *, '(a)' ) '  C8MAT_NORM_LI returns the Loo norm of a C8MAT.'

  call c8mat_indicator ( m, n, a )

  call c8mat_print ( m, n, a, '  The C8MAT_INDICATOR matrix:' )

  value = c8mat_norm_li ( m, n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The Loo norm = ', value

  return
end
subroutine c8mat_uniform_01_test ( )

!*****************************************************************************80
!
!! C8MAT_UNIFORM_01_TEST tests C8MAT_UNIFORM_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  complex ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8MAT_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  C8MAT_UNIFORM_01 computes a "random" complex matrix.'

  seed = 123456789

  call c8mat_uniform_01 ( m, n, seed, a )

  call c8mat_print ( m, n, a, '  The matrix:' )

  return
end
subroutine c8vec_indicator_test ( )

!*****************************************************************************80
!
!! C8VEC_INDICATOR_TEST tests C8VEC_INDICATOR;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 September 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  complex ( kind = 8 ) a(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_INDICATOR_TEST'
  write ( *, '(a)' ) '  C8VEC_INDICATOR sets A = (1-1i,2-2i,...,N-Ni)'

  call c8vec_indicator ( n, a )

  call c8vec_print ( n, a, '  The "indicator" vector:' )

  return
end
subroutine c8vec_nint_test ( )

!*****************************************************************************80
!
!! C8VEC_NINT_TEST tests C8VEC_NINT.
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

  integer ( kind = 4 ), parameter :: n = 5

  complex ( kind = 8 ) a(n)
  complex ( kind = 8 ) s
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_NINT_TEST'
  write ( *, '(a)' ) '  C8VEC_NINT rounds a complex vector.'

  seed = 123456789

  call c8vec_uniform_01 ( n, seed, a )

  s = complex ( 5.0D+00, 3.0D+00 )
  a(1:n) = s * a(1:n)

  call c8vec_print ( n, a, '  The initial vector:' )

  call c8vec_nint ( n, a )

  call c8vec_print ( n, a, '  The rounded vector:' )

  return
end
subroutine c8vec_norm_l1_test ( )

!*****************************************************************************80
!
!! C8VEC_NORM_L1_TEST tests C8VEC_NORM_L1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  complex ( kind = 8 ) a(n)
  real ( kind = 8 ) c8vec_norm_l1
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_NORM_L1_TEST'
  write ( *, '(a)' ) '  C8VEC_NORM_L1 returns the L1 norm of a C8VEC.'

  call c8vec_indicator ( n, a )

  call c8vec_print ( n, a, '  The C8VEC_INDICATOR vector:' )

  value = c8vec_norm_l1 ( n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The L1 norm = ', value

  return
end
subroutine c8vec_norm_l2_test ( )

!*****************************************************************************80
!
!! C8VEC_NORM_L2_TEST tests C8VEC_NORM_L2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  complex ( kind = 8 ) a(n)
  real ( kind = 8 ) c8vec_norm_l2
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_NORM_L2_TEST'
  write ( *, '(a)' ) '  C8VEC_NORM_L2 returns the L2 norm of a C8VEC.'

  call c8vec_indicator ( n, a )

  call c8vec_print ( n, a, '  The C8VEC_INDICATOR vector:' )

  value = c8vec_norm_l2 ( n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The L2 norm = ', value

  return
end
subroutine c8vec_norm_li_test ( )

!*****************************************************************************80
!
!! C8VEC_NORM_LI_TEST tests C8VEC_NORM_LI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  complex ( kind = 8 ) a(n)
  real ( kind = 8 ) c8vec_norm_li
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_NORM_LI_TEST'
  write ( *, '(a)' ) '  C8VEC_NORM_LI returns the Loo norm of a C8VEC.'

  call c8vec_indicator ( n, a )

  call c8vec_print ( n, a, '  The C8VEC_INDICATOR vector:' )

  value = c8vec_norm_li ( n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The Loo norm = ', value

  return
end
subroutine c8vec_print_test ( )

!*****************************************************************************80
!
!! C8VEC_PRINT_TEST tests C8VEC_PRINT.
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

  integer ( kind = 4 ), parameter :: n = 5

  complex ( kind = 8 ) a(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_PRINT_TEST'
  write ( *, '(a)' ) '  C8VEC_PRINT prints a C8VEC.'

  call c8vec_indicator ( n, a )

  call c8vec_print ( n, a, '  The C8VEC_INDICATOR vector:' )

  return
end
subroutine c8vec_print_part_test ( )

!*****************************************************************************80
!
!! C8VEC_PRINT_PART_TEST tests C8VEC_PRINT_PART.
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

  integer ( kind = 4 ), parameter :: n = 100

  complex ( kind = 8 ), dimension ( n ) :: a
  integer ( kind = 4 ) max_print

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_PRINT_PART_TEST'
  write ( *, '(a)' ) '  C8VEC_PRINT_PART prints part of a C8VEC.'

  call c8vec_indicator ( n, a )

  max_print = 10
  call c8vec_print_part ( n, a, max_print, '  Part of the C8VEC:' )

  return
end
subroutine c8vec_sort_a_l1_test ( )

!*****************************************************************************80
!
!! C8VEC_SORT_A_L1_TEST tests C8VEC_SORT_A_L1.
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

  complex ( kind = 8 ) a(n)
  real ( kind = 8 ) c8_norm_l1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_SORT_A_L1_TEST'
  write ( *, '(a)' ) '  C8VEC_SORT_A_L1 sorts a C8VEC by L1 norm.'

  seed = 123456789
  call c8vec_uniform_01 ( n, seed, a )

  call c8vec_print ( n, a, '  The unsorted vector' )

  call c8vec_sort_a_l1 ( n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I             A(I)                   L1(A(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i2,2x,2g14.6,2x,g14.6)' ) i, a(i), c8_norm_l1 ( a(i) )
  end do

  return
end
subroutine c8vec_sort_a_l2_test ( )

!*****************************************************************************80
!
!! C8VEC_SORT_A_L2_TEST tests C8VEC_SORT_A_L2.
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

  complex ( kind = 8 ) a(n)
  real ( kind = 8 ) c8_norm_l2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_SORT_A_L2_TEST'
  write ( *, '(a)' ) '  C8VEC_SORT_A_L2 sorts a C8VEC by L2 norm.'

  seed = 123456789
  call c8vec_uniform_01 ( n, seed, a )

  call c8vec_print ( n, a, '  The unsorted vector' )

  call c8vec_sort_a_l2 ( n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I             A(I)                   L2(A(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i2,2x,2g14.6,2x,g14.6)' ) i, a(i), c8_norm_l2 ( a(i) )
  end do

  return
end
subroutine c8vec_sort_a_li_test ( )

!*****************************************************************************80
!
!! C8VEC_SORT_A_LI_TEST tests C8VEC_SORT_A_LI.
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

  complex ( kind = 8 ) a(n)
  real ( kind = 8 ) c8_norm_li
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_SORT_A_LI_TEST'
  write ( *, '(a)' ) '  C8VEC_SORT_A_LI ascending sorts a C8VEC by Loo norm.'

  seed = 123456789
  call c8vec_uniform_01 ( n, seed, a )

  call c8vec_print ( n, a, '  The unsorted vector' )

  call c8vec_sort_a_li ( n, a )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I             A(I)                   Loo(A(I))'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i2,2x,2g14.6,2x,g14.6)' ) i, a(i), c8_norm_li ( a(i) )
  end do

  return
end
subroutine c8vec_spiral_test ( )

!*****************************************************************************80
!
!! C8VEC_SPIRAL_TEST tests C8VEC_SPIRAL;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 13

  complex ( kind = 8 ) c(n)
  complex ( kind = 8 ) c1
  complex ( kind = 8 ) c2
  integer ( kind = 4 ) m

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_SPIRAL_TEST'
  write ( *, '(a)' ) '  C8VEC_SPIRAL returns N points on a spiral'
  write ( *, '(a)' ) '  which includes M complete turns.'

  m = 1
  c1 = cmplx ( 5.0D+00, 0.0D+00, kind = 8 )
  c2 = cmplx ( 3.0D+00, 0.0D+00, kind = 8 )

  call c8vec_spiral ( n, m, c1, c2, c )

  call c8vec_print ( n, c, '  The spiral points:' )

  return
end
subroutine c8vec_uniform_01_test ( )

!*****************************************************************************80
!
!! C8VEC_UNIFORM_01_TEST tests C8VEC_UNIFORM_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 February 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  complex ( kind = 8 ) a(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  C8VEC_UNIFORM_01 computes a "random" complex vector.'

  seed = 123456789

  call c8vec_uniform_01 ( n, seed, a )

  call c8vec_print ( n, a, '  The C8VEC_UNIFORM_01 vector:' )

  return
end
subroutine c8vec_unity_test ( )

!*****************************************************************************80
!
!! C8VEC_UNITY_TEST tests C8VEC_UNITY;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 12

  complex ( kind = 8 ) a(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_UNITY_TEST'
  write ( *, '(a)' ) '  C8VEC_UNITY returns the N roots of unity'

  call c8vec_unity ( n, a )

  call c8vec_print ( n, a, '  The N roots of unity:' )

  return
end
subroutine cartesian_to_c8_test ( )

!*****************************************************************************80
!
!! CARTESIAN_TO_C8_TEST tests CARTESIAN_TO_C8.
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

  complex ( kind = 8 ) c2
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10
  real ( kind = 8 ) x1
  real ( kind = 8 ) x3
  real ( kind = 8 ) y1
  real ( kind = 8 ) y3

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CARTESIAN_TO_C8_TEST'
  write ( *, '(a)' ) '  CARTESIAN_TO_C8 converts (X,Y) to C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '      X1,Y1=R8_UNIFORM_01    C2=CARTESIAN_TO_C8(X1,Y1)     X3,Y3=C8_TO_CARTESIAN(C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    x1 = r8_uniform_01 ( seed )
    y1 = r8_uniform_01 ( seed )
    call cartesian_to_c8 ( x1, y1, c2 )
    call c8_to_cartesian ( c2, x3, y3 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) x1, y1, c2, x3, y3

  end do

  return
end
subroutine polar_to_c8_test ( )

!*****************************************************************************80
!
!! POLAR_TO_C8_TEST tests POLAR_TO_C8.
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

  complex ( kind = 8 ) c2
  real ( kind = 8 ) r1
  real ( kind = 8 ) r3
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t1
  real ( kind = 8 ) t3
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POLAR_TO_C8_TEST'
  write ( *, '(a)' ) '  POLAR_TO_C8 converts (R,T) to C8.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '      (R1,T1)=R8_UNIFORM_01()    C2=POLAR_TO_C8(R1,T1)    (R3,T3)=C8_TO_POLAR(C2)'
  write ( *, '(a)' ) &
     '     ---------------------     ---------------------     ---------------------'
  write ( *, '(a)' ) ' '

  seed = 123456789

  do test = 1, test_num

    r1 = r8_uniform_01 ( seed )
    t1 = 2.0D+00 * r8_pi * r8_uniform_01 ( seed )
    call polar_to_c8 ( r1, t1, c2 )
    call c8_to_polar ( c2, r3, t3 )

    write ( *, '(2x,2f12.4,2x,2f12.4,2x,2f12.4)' ) r1, t1, c2, r3, t3

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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_ATAN_TEST'
  write ( *, '(a)' ) '  R8_ATAN computes the arc-tangent given Y and X;'
  write ( *, '(a)' ) '  ATAN2 is the system version of this routine.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X               Y          ATAN2(Y,X)    R8_ATAN(Y,X)'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    x = xtest(test)
    y = ytest(test)
    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x, y, atan2 ( y, x ), r8_atan ( y, x )
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
subroutine r8poly2_root_test ( )

!*****************************************************************************80
!
!! R8POLY2_ROOT_TEST tests R8POLY2_ROOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 3

  real ( kind = 8 ) a
  real ( kind = 8 ), dimension(test_num) :: a_test = (/ &
    2.0D+00, 1.0D+00, 1.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), dimension(test_num) :: b_test = (/ &
    -2.0D+00, -20.0D+00, -2.0D+00 /)
  real ( kind = 8 ) c
  real ( kind = 8 ), dimension(test_num) :: c_test = (/ &
    -24.0D+00, 100.0D+00, 10.0D+00 /)
  complex ( kind = 8 ) r1
  complex ( kind = 8 ) r2
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8POLY2_ROOT_TEST'
  write ( *, '(a)' ) '  R8POLY2_ROOT finds quadratic equation roots.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         A         B         C     R1         R2'
  write ( *, '(a)' ) ' '

  do test = 1, test_num

    a = a_test(test)
    b = b_test(test)
    c = c_test(test)

    call r8poly2_root ( a, b, c, r1, r2 )

    write ( *, '(2x,3f8.1,4g14.6)' ) a, b, c, r1, r2

  end do

  return
end
subroutine r8poly3_root_test ( )

!*****************************************************************************80
!
!! R8POLY3_ROOT_TEST tests R8POLY3_ROOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 4

  real ( kind = 8 ) a
  real ( kind = 8 ), dimension(test_num) :: a_test = (/ &
    1.0D+00, 9.0D+00, 1.0D+00, 1.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), dimension(test_num) :: b_test = (/ &
    -6.0D+00, -36.0D+00, -5.0D+00, -8.0D+00  /)
  real ( kind = 8 ) c
  real ( kind = 8 ), dimension(test_num) :: c_test = (/ &
    11.0D+00, 54.0D+00, 8.0D+00, 25.0D+00  /)
  real ( kind = 8 ) d
  real ( kind = 8 ), dimension(test_num) :: d_test = (/ &
    -6.0D+00, -27.0D+00, -4.0D+00, -26.0D+00  /)
  complex ( kind = 8 ) r1
  complex ( kind = 8 ) r2
  complex ( kind = 8 ) r3
  integer ( kind = 4 ) test
!
!  1: Three distinct real roots, 1, 2, 3.
!  2: One repeated real root, 1.5, 1.5, 1.5.
!  3: Two real roots, one repeated, 1, 2, 2.
!  4: One real root, a complex conjugate pair, 2, 3+2I, 3-2I.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8POLY3_ROOT_TEST'
  write ( *, '(a)' ) '  R8POLY3_ROOT finds roots of cubic equations.'
  write ( *, '(a)' ) ' '

  do test = 1, test_num

    a = a_test(test)
    b = b_test(test)
    c = c_test(test)
    d = d_test(test)

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Polynomial coefficients A, B, C, D:'
    write ( *, '(a)' ) ' '
    write ( *, '(2x,4g14.6)' ) a, b, c, d

    call r8poly3_root ( a, b, c, d, r1, r2, r3 )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Roots:'
    write ( *, '(a)' ) ' '
    write ( *, '(2x,2g14.6)' ) r1
    write ( *, '(2x,2g14.6)' ) r2
    write ( *, '(2x,2g14.6)' ) r3

  end do

  return
end
subroutine r8poly4_root_test ( )

!*****************************************************************************80
!
!! R8POLY4_ROOT_TEST tests R8POLY4_ROOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 October 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 7

  real ( kind = 8 ) a
  real ( kind = 8 ), dimension(test_num) :: a_test = (/ &
    1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, &
    1.0D+00, 1.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), dimension(test_num) :: b_test = (/ &
    -10.0D+00, -5.0D+00, -22.0D+00, -16.0D+00, -20.0D+00, &
    2.0D+00, 0.0D+00 /)
  real ( kind = 8 ) c
  real ( kind = 8 ), dimension(test_num) :: c_test = (/ &
    35.0D+00, 1.0D+00, 141.0D+00, 72.0D+00, 150.0D+00, &
    1.0D+00, 13.0D+00 /)
  real ( kind = 8 ) d
  real ( kind = 8 ), dimension(test_num) :: d_test = (/ &
    -50.0D+00, 21.0D+00, -220.0D+00, -128.0D+00, -500.0D+00, &
    8.0D+00, 0.0D+00 /)
  real ( kind = 8 ) e
  real ( kind = 8 ), dimension(test_num) :: e_test = (/ &
    24.0D+00, -18.0D+00, +100.0D+00, 80.0D+00, 625.0D+00, &
    -12.0D+00, 36.0D+00 /)
  complex ( kind = 8 ) r1
  complex ( kind = 8 ) r2
  complex ( kind = 8 ) r3
  complex ( kind = 8 ) r4
  integer ( kind = 4 ) test
!
!  1: Four distinct real roots, 1, 2, 3, 4.
!  2: Three distinct real roots, 1, -2, 3, 3
!  3: Two distinct real roots, 1, 1, 10, 10.
!  4: Two distinct real roots, 2, 2, 2, 10
!  5: One real root, 5, 5, 5, 5
!  6: Two distinct real roots, one complex conjugate pair.
!  7: Two distinct complex conjugate pairs.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8POLY4_ROOT_TEST'
  write ( *, '(a)' ) '  R8POLY4_ROOT finds roots of quartic equations.'
  write ( *, '(a)' ) ' '

  do test = 1, test_num

    a = a_test(test)
    b = b_test(test)
    c = c_test(test)
    d = d_test(test)
    e = e_test(test)

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  A =', a
    write ( *, '(a,g14.6)' ) '  B =', b
    write ( *, '(a,g14.6)' ) '  C =', c
    write ( *, '(a,g14.6)' ) '  D =', d
    write ( *, '(a,g14.6)' ) '  E =', e

    call r8poly4_root ( a, b, c, d, e, r1, r2, r3, r4 )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Roots:'
    write ( *, '(a)' ) ' '
    write ( *, '(2x,2g14.6)' ) r1
    write ( *, '(2x,2g14.6)' ) r2
    write ( *, '(2x,2g14.6)' ) r3
    write ( *, '(2x,2g14.6)' ) r4

  end do

  return
end
